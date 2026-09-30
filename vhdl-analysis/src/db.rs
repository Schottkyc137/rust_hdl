use std::{
    cell::{Ref, RefCell, RefMut},
    collections::HashMap,
    hash::Hash,
};

use vhdl_syntax::{latin_1::Latin1String, parser::parse, syntax::DesignFileSyntax};

use crate::design::analyze_design;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
struct Rev(usize);

impl Rev {
    pub fn bump(&mut self) {
        self.0 += 1;
    }
}

#[derive(Debug)]
pub struct File {
    contents: Box<[u8]>,
    changed_at: Rev,
}

impl File {
    fn new(contents: Box<[u8]>, rev: Rev) -> File {
        File {
            contents,
            changed_at: rev,
        }
    }
}

#[derive(Debug, Clone)]
struct Deps(Vec<QueryKey>);

impl Deps {
    pub fn new() -> Deps {
        Deps(Vec::new())
    }

    pub fn push(&mut self, dep: QueryKey) {
        self.0.push(dep);
    }

    pub fn all_up_to_date(&self, db: &Db, verified_at: Rev) -> bool {
        self.0.iter().all(|&key| key.changed_at(db) <= verified_at)
    }
}

#[derive(Debug)]
struct Memo<T> {
    value: T,
    verified_at: Rev,
    changed_at: Rev,
    deps: Deps,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default, Hash)]
pub struct FileId(usize);

#[derive(Debug, Clone, PartialEq, Eq, Default, Hash)]
pub struct DesignUnit {
    primary_name: Latin1String,
    secondary_name: Option<Latin1String>,
}

impl DesignUnit {
    pub fn primary(name: Latin1String) -> DesignUnit {
        DesignUnit {
            primary_name: name,
            secondary_name: None,
        }
    }

    pub fn secondary(primary_name: Latin1String, secondary_name: Latin1String) -> DesignUnit {
        DesignUnit {
            primary_name,
            secondary_name: Some(secondary_name),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
enum QueryKey {
    File(FileId),
    Ast(FileId),
    DesignUnits(FileId),
}

impl QueryKey {
    fn changed_at(&self, db: &Db) -> Rev {
        match self {
            QueryKey::File(id) => db.files[id.0].changed_at,
            QueryKey::Ast(id) => {
                db.ensure(&db.parsed, *id, *self, |db| parse(&db.file(*id).contents).0);
                db.parsed.changed_at(&id)
            }
            QueryKey::DesignUnits(id) => {
                db.ensure(&db.file_units, *id, *self, |db| analyze_design(db, *id));
                db.file_units.changed_at(&id)
            }
        }
    }
}

#[derive(Debug, Clone)]
struct Frame {
    key: QueryKey,
    deps: Deps,
}

impl Frame {
    pub fn new(key: QueryKey) -> Frame {
        Frame {
            key,
            deps: Deps::new(),
        }
    }
}

#[derive(Debug)]
struct QueryMap<K, V>(RefCell<HashMap<K, Memo<V>>>);

impl<K: Hash + Eq, V> QueryMap<K, V> {
    fn borrow(&self) -> Ref<'_, HashMap<K, Memo<V>>> {
        self.0.borrow()
    }

    fn borrow_mut(&self) -> RefMut<'_, HashMap<K, Memo<V>>> {
        self.0.borrow_mut()
    }

    fn changed_at(&self, key: &K) -> Rev {
        self.borrow()[key].changed_at
    }

    fn is_up_to_date(&self, db: &Db, key: &K) -> bool {
        match self.borrow().get(&key) {
            Some(m) => m.verified_at == db.rev || m.deps.all_up_to_date(db, m.verified_at),
            None => false,
        }
    }
}

impl<K: Hash + Eq, V: Eq> QueryMap<K, V> {
    fn update(&self, key: K, value: V, rev: Rev, deps: Deps) {
        let mut t = self.borrow_mut();
        let changed_at = match t.get(&key) {
            Some(old) if old.value == value => old.changed_at,
            _ => rev,
        };
        t.insert(
            key,
            Memo {
                value: value,
                verified_at: rev,
                changed_at,
                deps,
            },
        );
    }
}

impl<K, V> QueryMap<K, V> {
    fn new() -> Self {
        QueryMap(RefCell::new(HashMap::new()))
    }
}

impl<K: Hash + Eq, V: Clone> QueryMap<K, V> {
    fn mark_verified(&self, key: &K, rev: Rev) -> V {
        let mut t = self.0.borrow_mut();
        let m = t.get_mut(key).unwrap();
        m.verified_at = rev;
        return m.value.clone();
    }
}

impl<K, V> Default for QueryMap<K, V> {
    fn default() -> Self {
        Self::new()
    }
}

/// Database
#[derive(Debug, Default)]
pub struct Db {
    rev: Rev,

    files: Vec<File>,

    parsed: QueryMap<FileId, DesignFileSyntax>,
    file_units: QueryMap<FileId, Vec<DesignUnit>>,

    stack: RefCell<Vec<Frame>>,
}

impl Db {
    pub fn new() -> Db {
        Db::default()
    }

    fn record(&self, key: QueryKey) {
        if let Some(frame) = self.stack.borrow_mut().last_mut() {
            frame.deps.push(key);
        }
    }

    /// Ensure a key is in the table.
    /// If it is not there, or it is out of date, recompute the key using the provided function.
    /// This also updates the table so subsequent calls don't re-compute when something changes
    fn ensure<K: Eq + Hash, V: Clone + Eq>(
        &self,
        table: &QueryMap<K, V>,
        key: K,
        qkey: QueryKey,
        compute: impl FnOnce(&Db) -> V,
    ) -> V {
        if table.is_up_to_date(self, &key) {
            return table.mark_verified(&key, self.rev);
        }

        self.check_no_cycles(&qkey);

        // Recompute
        self.push_frame(Frame::new(qkey));

        let value = compute(self);

        let frame = self.stack.borrow_mut().pop().unwrap();

        table.update(key, value.clone(), self.rev, frame.deps);
        value
    }

    /// Fetch a value from the table, creating it if it isn't there or up-to date
    fn fetch<K: Eq + Hash, V: Clone + Eq>(
        &self,
        table: &QueryMap<K, V>,
        key: K,
        qkey: QueryKey,
        compute: impl FnOnce(&Db) -> V,
    ) -> V {
        self.record(qkey);
        self.ensure(table, key, qkey, compute)
    }

    pub fn update_file_text(&mut self, id: FileId, text: Box<[u8]>) {
        let file = &mut self.files[id.0];
        self.rev.bump();
        file.contents = text;
        file.changed_at = self.rev;
    }

    pub fn add_file(&mut self, text: Box<[u8]>) -> FileId {
        self.rev.bump();
        let len = self.files.len();
        self.files.push(File::new(text, self.rev));
        FileId(len)
    }

    pub fn file(&self, id: FileId) -> &File {
        self.record(QueryKey::File(id));
        &self.files[id.0]
    }

    fn check_no_cycles(&self, key: &QueryKey) {
        if self.stack.borrow().iter().any(|f| &f.key == key) {
            panic!("cycle: {key:?}"); // later: return an error instead
        }
    }

    fn push_frame(&self, frame: Frame) {
        self.stack.borrow_mut().push(frame);
    }

    pub fn ast(&self, id: FileId) -> DesignFileSyntax {
        self.fetch(&self.parsed, id, QueryKey::Ast(id), |db| {
            parse(&db.file(id).contents).0
        })
    }

    pub fn design_units(&self, id: FileId) -> Vec<DesignUnit> {
        self.fetch(&self.file_units, id, QueryKey::DesignUnits(id), |db| {
            analyze_design(db, id)
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use vhdl_syntax::{
        latin_1::Latin1String,
        syntax::{
            valid_nodes::{ValidLibraryUnit, ValidPrimaryUnit},
            validate::valid_node::Valid,
        },
    };

    fn name(unit: Valid<DesignFileSyntax>) -> Latin1String {
        match unit
            .design_units()
            .next()
            .unwrap()
            .library_unit()
            .alternative()
        {
            ValidLibraryUnit::PrimaryUnit(primary) => match primary.alternative() {
                ValidPrimaryUnit::EntityDeclaration(entity) => entity
                    .entity_declaration_preamble()
                    .identifier_token()
                    .text()
                    .to_latin1_string(),
                ValidPrimaryUnit::ConfigurationDeclaration(_valid) => todo!(),
                ValidPrimaryUnit::PrimaryUnitPackageDeclaration(_valid) => todo!(),
                ValidPrimaryUnit::PackageInstantiationDeclarationPrimaryUnit(_valid) => todo!(),
                ValidPrimaryUnit::ContextDeclaration(_valid) => todo!(),
            },
            ValidLibraryUnit::SecondaryUnit(_valid) => todo!(),
        }
    }

    #[test]
    fn add_files() {
        let mut db = Db::new();
        let idx = db.add_file("entity foo is end foo;".as_bytes().into());

        let syntax = Valid::new(db.ast(idx)).unwrap();
        assert_eq!(name(syntax), Latin1String::from(b"foo"));

        db.update_file_text(idx, "entity bar is end bar;".as_bytes().into());
        let syntax = Valid::new(db.ast(idx)).unwrap();

        assert_eq!(name(syntax), Latin1String::from(b"bar"));
    }
}
