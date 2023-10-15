use crate::analysis::DesignRoot;
use crate::ast::visitor::{Visitor, VisitorResult};
use crate::ast::{
    AnyDesignUnit, AnyPrimaryUnit, AnySecondaryUnit, ComponentDeclaration, Declaration,
    EntityDeclaration, HasUnitId, InstantiationStatement, InterfaceDeclaration, MapAspect,
    ObjectClass, PackageDeclaration, SubprogramDeclaration, SubprogramDesignator, UnitKey,
    WithDecl, WithRef,
};
use crate::data::{ContentReader, Symbol};
use crate::syntax::Kind::*;
use crate::syntax::{Symbols, Token, TokenAccess, Tokenizer, Value};
use crate::{ast, EntityId, Position, Source};
use fnv::FnvHashMap;
use itertools::Itertools;
use std::collections::hash_map::Entry;
use std::collections::HashSet;
use std::default::Default;

#[derive(Debug, PartialEq, Clone)]
pub enum CompletionKind {
    Module,
    Constant,
    Signal,
    Variable,
    File,
    Type,
    Function,
    Operator,
    Attribute,
    Alias,
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompletionItemMode {
    Text,
    Snippet,
}

#[derive(Debug, PartialEq, Clone)]
pub struct CompletionItem {
    pub label: String,
    pub detail: String,
    pub kind: CompletionKind,
    pub mode: CompletionItemMode,
    pub entity: Option<EntityId>,
}

impl CompletionItem {
    pub fn text(
        label: String,
        detail: String,
        kind: CompletionKind,
        entity: Option<EntityId>,
    ) -> CompletionItem {
        CompletionItem {
            label,
            detail,
            kind,
            mode: CompletionItemMode::Text,
            entity,
        }
    }

    pub fn snippet(
        label: String,
        detail: String,
        kind: CompletionKind,
        entity: Option<EntityId>,
    ) -> CompletionItem {
        CompletionItem {
            label,
            detail,
            kind,
            mode: CompletionItemMode::Snippet,
            entity,
        }
    }

    pub fn from_decl<T>(
        decl: &WithDecl<T>,
        root: &DesignRoot,
        kind: CompletionKind,
    ) -> CompletionItem
    where
        T: ToString,
    {
        CompletionItem::text(
            decl.tree.to_string(),
            decl.decl
                .map(|id| root.get_ent(id).describe())
                .unwrap_or_default(),
            kind,
            decl.decl,
        )
    }

    pub fn from_ref<T>(
        reference: &WithRef<T>,
        root: &DesignRoot,
        kind: CompletionKind,
    ) -> CompletionItem
    where
        T: ToString,
    {
        CompletionItem::text(
            reference.item.to_string(),
            reference
                .reference
                .map(|id| root.get_ent(id).describe())
                .unwrap_or_default(),
            kind,
            reference.reference,
        )
    }
}

macro_rules! kind {
    ($kind: pat) => {
        Token { kind: $kind, .. }
    };
}

macro_rules! ident {
    ($bind: pat) => {
        Token {
            kind: Identifier,
            value: Value::Identifier($bind),
            ..
        }
    };
}

#[derive(Eq, PartialEq, Debug)]
enum MapAspectKind {
    Port,
    Generic,
}

/// Extracts the name of ports or generics from an AST for an entity with a certain ID.
/// The entity can be an `Entity`, `Component` or `Package`.
/// After walking the AST, the ports or generics are written to the `items` vector.
/// The `kind` member chooses whether to select ports or generics.
struct PortsOrGenericsExtractor<'a> {
    id: EntityId,
    items: &'a mut Vec<CompletionItem>,
    kind: MapAspectKind,
    root: &'a DesignRoot,
}

impl DesignRoot {
    fn extract_port_or_generic_names(
        &self,
        id: EntityId,
        items: &mut Vec<CompletionItem>,
        kind: MapAspectKind,
    ) {
        let mut searcher = PortsOrGenericsExtractor {
            id,
            items,
            kind,
            root: self,
        };
        self.walk(&mut searcher);
    }
}

impl<'a> Visitor for PortsOrGenericsExtractor<'a> {
    fn visit_component_declaration(
        &mut self,
        node: &ComponentDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if node.ident.decl != Some(self.id) {
            return VisitorResult::Skip;
        }
        if self.kind == MapAspectKind::Port {
            for port in &node.port_list {
                self.items.push(port.completable_name(self.root))
            }
        }
        if self.kind == MapAspectKind::Generic {
            for generic in &node.generic_list {
                self.items.push(generic.completable_name(self.root))
            }
        }
        VisitorResult::Stop
    }

    fn visit_entity_declaration(
        &mut self,
        node: &EntityDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if node.ident.decl != Some(self.id) {
            return VisitorResult::Skip;
        }
        if self.kind == MapAspectKind::Port {
            if let Some(ports) = &node.port_clause {
                for port in ports {
                    self.items.push(port.completable_name(self.root))
                }
            }
        }
        if self.kind == MapAspectKind::Generic {
            if let Some(generics) = &node.generic_clause {
                for generic in generics {
                    self.items.push(generic.completable_name(self.root))
                }
            }
        }
        VisitorResult::Stop
    }

    fn visit_package_declaration(
        &mut self,
        node: &PackageDeclaration,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if node.ident.decl != Some(self.id) {
            return VisitorResult::Skip;
        }
        if self.kind == MapAspectKind::Generic {
            if let Some(generics) = &node.generic_clause {
                for generic in generics {
                    self.items.push(generic.completable_name(self.root))
                }
            }
        }
        VisitorResult::Stop
    }
}

fn object_class_to_completion_kind(class: ObjectClass) -> CompletionKind {
    match class {
        ObjectClass::Signal => CompletionKind::Signal,
        ObjectClass::Constant => CompletionKind::Constant,
        ObjectClass::Variable | ObjectClass::SharedVariable => CompletionKind::Variable,
    }
}

impl InterfaceDeclaration {
    /// Returns completable names for an interface declarations.
    /// Example:
    /// `signal my_signal : natural := 5` => `my_signal`
    fn completable_name(&self, root: &DesignRoot) -> CompletionItem {
        match self {
            InterfaceDeclaration::Object(obj) => CompletionItem::from_decl(
                &obj.ident,
                root,
                object_class_to_completion_kind(obj.class),
            ),
            InterfaceDeclaration::File(file) => {
                CompletionItem::from_decl(&file.ident, root, CompletionKind::File)
            }
            InterfaceDeclaration::Type(typ) => {
                CompletionItem::from_decl(typ, root, CompletionKind::Type)
            }
            InterfaceDeclaration::Subprogram(decl, _) => {
                subprogram_declaration_to_completion_item(decl, root)
            }
            InterfaceDeclaration::Package(package) => {
                CompletionItem::from_decl(&package.ident, root, CompletionKind::Module)
            }
        }
    }
}

/// Visitor responsible for completions in selected AST elements
struct AutocompletionVisitor<'a> {
    root: &'a DesignRoot,
    cursor: Position,
    completions: &'a mut Vec<CompletionItem>,
}

impl<'a> AutocompletionVisitor<'a> {
    /// Loads completion options for the given map aspect.
    /// Returns `true`, when the cursor is inside the map aspect and the search should not continue.
    /// Returns `false` otherwise
    fn load_completions_for_map_aspect(
        &mut self,
        node: &InstantiationStatement,
        map: &MapAspect,
        ctx: &dyn TokenAccess,
        kind: MapAspectKind,
    ) -> bool {
        if !map.span(ctx).contains(self.cursor) {
            return false;
        }
        let formals_in_map: HashSet<String> =
            HashSet::from_iter(map.formals().map(|name| name.to_string().to_lowercase()));
        if let Some(ent) = node.entity_reference() {
            self.root
                .extract_port_or_generic_names(ent, self.completions, kind);
            self.completions
                .retain(|item| !formals_in_map.contains(&item.label.to_lowercase()));
            for completion in self.completions.iter_mut() {
                completion.label = format!("{} => $1,", completion.label);
                completion.mode = CompletionItemMode::Snippet
            }
        }
        true
    }
}

impl<'a> Visitor for AutocompletionVisitor<'a> {
    /// Visit an instantiation statement extracting completions for ports or generics.
    fn visit_instantiation_statement(
        &mut self,
        node: &InstantiationStatement,
        ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        if let Some(map) = &node.generic_map {
            if self.load_completions_for_map_aspect(node, map, ctx, MapAspectKind::Generic) {
                return VisitorResult::Stop;
            }
        }
        if let Some(map) = &node.port_map {
            if self.load_completions_for_map_aspect(node, map, ctx, MapAspectKind::Port) {
                return VisitorResult::Stop;
            }
        }
        VisitorResult::Skip
    }

    // preliminary optimizations: only visit architecture
    fn visit_any_primary_unit(
        &mut self,
        _node: &AnyPrimaryUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        VisitorResult::Skip
    }

    // preliminary optimizations: only visit architecture
    fn visit_any_secondary_unit(
        &mut self,
        node: &AnySecondaryUnit,
        _ctx: &dyn TokenAccess,
    ) -> VisitorResult {
        match node {
            AnySecondaryUnit::Architecture(_) => VisitorResult::Continue,
            AnySecondaryUnit::PackageBody(_) => VisitorResult::Skip,
        }
    }
}

fn subprogram_designator_to_completion_kind(desi: &SubprogramDesignator) -> CompletionKind {
    match desi {
        SubprogramDesignator::Identifier(_) => CompletionKind::Function,
        SubprogramDesignator::OperatorSymbol(_) => CompletionKind::Operator,
    }
}

/// Returns the completable string representation of a declaration
/// for example:
/// `let alias = parse_vhdl("alias my_alias is ...")`
/// `declaration_to_string(Declaration::Alias(alias)) == "my_alias"`
/// Returns `None` if the declaration has no string representation that can be used for completion
/// purposes.
fn subprogram_declaration_to_completion_item(
    decl: &SubprogramDeclaration,
    root: &DesignRoot,
) -> CompletionItem {
    match decl {
        SubprogramDeclaration::Procedure(proc) => {
            CompletionItem::from_decl(&proc.designator, root, CompletionKind::Function)
        }
        SubprogramDeclaration::Function(func) => CompletionItem::from_decl(
            &func.designator,
            root,
            subprogram_designator_to_completion_kind(&func.designator.tree.item),
        ),
    }
}

fn declaration_to_completion_item(decl: &Declaration, root: &DesignRoot) -> Option<CompletionItem> {
    match decl {
        Declaration::Object(o) => Some(CompletionItem::from_decl(
            &o.ident,
            root,
            object_class_to_completion_kind(o.class),
        )),
        Declaration::File(file) => Some(CompletionItem::from_decl(
            &file.ident,
            root,
            CompletionKind::File,
        )),
        Declaration::Type(typ) => Some(CompletionItem::from_decl(
            &typ.ident,
            root,
            CompletionKind::Type,
        )),
        Declaration::Component(comp) => Some(CompletionItem::from_decl(
            &comp.ident,
            root,
            CompletionKind::Module,
        )),
        Declaration::Attribute(attr) => match attr {
            ast::Attribute::Specification(spec) => Some(CompletionItem::from_ref(
                &spec.ident,
                root,
                CompletionKind::Attribute,
            )),
            ast::Attribute::Declaration(decl) => Some(CompletionItem::from_decl(
                &decl.ident,
                root,
                CompletionKind::Attribute,
            )),
        },
        Declaration::Alias(alias) => Some(CompletionItem::from_decl(
            &alias.designator,
            root,
            CompletionKind::Alias,
        )),
        Declaration::SubprogramDeclaration(decl) => {
            Some(subprogram_declaration_to_completion_item(decl, root))
        }
        Declaration::SubprogramBody(body) => Some(subprogram_declaration_to_completion_item(
            &body.specification,
            root,
        )),
        Declaration::Package(pkg) => Some(CompletionItem::from_decl(
            &pkg.ident,
            root,
            CompletionKind::Module,
        )),
        _ => None,
    }
}

/// Tokenizes `source` up to `cursor` but no further. The last token returned is the token
/// where the cursor currently resides or the token right before the cursor.
///
/// Examples:
///
/// input = "use ieee.std_logic_1|164.a"
///                              ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164}
///
/// input = "use ieee.std_logic_1164|.a"
///                                 ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164}
///
/// input = "use ieee.std_logic_1164.|a"
///                                  ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164, DOT}
/// input = "use ieee.std_logic_1164.a|"
///                                   ^ cursor position
/// `tokenize_input(input)` -> {USE, ieee, DOT, std_logic_1164, DOT, a}
///
/// On error, or if the source is empty, returns an empty vector.
fn tokenize_input(symbols: &Symbols, source: &Source, cursor: Position) -> Vec<Token> {
    let contents = source.contents();
    let mut tokenizer = Tokenizer::new(symbols, source, ContentReader::new(&contents));
    let mut tokens = Vec::new();
    loop {
        match tokenizer.pop() {
            Ok(Some(token)) => {
                if token.pos.start() >= cursor {
                    break;
                }
                tokens.push(token);
            }
            Ok(None) => break,
            Err(_) => return vec![],
        }
    }
    tokens
}

impl DesignRoot {
    /// helper function to list the name of all available libraries
    fn list_all_libraries(&self) -> Vec<CompletionItem> {
        self.available_libraries()
            .map(|k| {
                CompletionItem::text(
                    k.name_utf8(),
                    format!("library {}", k.name_utf8()),
                    CompletionKind::Module,
                    None,
                )
            })
            .collect()
    }

    /// List the name of all primary units for a given library.
    /// If the library is non-resolvable, list an empty vector
    fn list_primaries_for_lib(&self, lib: &Symbol) -> Vec<CompletionItem> {
        let Some(lib) = self.get_library_units(lib) else {
            return vec![];
        };
        lib.iter()
            .filter_map(|(key, unit)| match key {
                UnitKey::Primary(prim) => Some(CompletionItem::text(
                    prim.name_utf8(),
                    unit.describe(),
                    CompletionKind::Module,
                    unit.unit.get().and_then(|u| u.data().entity_id()),
                )),
                UnitKey::Secondary(_, _) => None,
            })
            .collect_vec()
    }

    /// Lists all available declarations for a primary unit inside a given library
    /// If the library does not exist or there is no primary unit with the given name for that library,
    /// return an empty vector
    fn list_available_declarations(
        &self,
        lib: &Symbol,
        primary_unit: &Symbol,
    ) -> Vec<CompletionItem> {
        let Some(lib) = self.get_library_units(lib) else {
            return vec![];
        };
        let Some(unit) = lib.get(&UnitKey::Primary(primary_unit.clone())) else {
            return vec![];
        };
        let unit = unit.unit.get();
        match unit.unwrap().to_owned() {
            AnyDesignUnit::Primary(AnyPrimaryUnit::Package(pkg)) => {
                let mut overloaded_items: FnvHashMap<String, Vec<CompletionItem>> =
                    FnvHashMap::default();
                for item in &pkg.decl {
                    let Some(completion_item) = declaration_to_completion_item(item, self) else {
                        continue;
                    };
                    match overloaded_items.entry(completion_item.label.clone()) {
                        Entry::Occupied(mut entry) => entry.get_mut().push(completion_item),
                        Entry::Vacant(entry) => {
                            let _ = entry.insert(vec![completion_item]);
                        }
                    }
                }
                let mut items = Vec::new();
                for (key, values) in overloaded_items.into_iter() {
                    if values.len() == 1 {
                        items.push(values[0].clone())
                    } else {
                        let item = CompletionItem::text(
                            key.clone(),
                            format!("+{} overloaded", values.len() - 1),
                            values[0].kind.clone(),
                            None,
                        );
                        items.push(item)
                    }
                }
                items.push(CompletionItem::text(
                    "all".to_string(),
                    "all".to_string(),
                    CompletionKind::Module,
                    None,
                ));
                items.sort_by(|a, b| a.label.cmp(&b.label));
                items
            }
            _ => Vec::default(),
        }
    }

    pub fn list_completion_options(
        &self,
        source: &Source,
        cursor: Position,
    ) -> Vec<CompletionItem> {
        let tokens = tokenize_input(&self.symbols, source, cursor);
        match &tokens[..] {
            [.., kind!(Library)] | [.., kind!(Use)] | [.., kind!(Use), kind!(Identifier)] => {
                self.list_all_libraries()
            }
            [.., kind!(Use), ident!(library), kind!(Dot)]
            | [.., kind!(Use), ident!(library), kind!(Dot), kind!(Identifier)] => {
                self.list_primaries_for_lib(library)
            }
            [.., kind!(Use), ident!(library), kind!(Dot), ident!(selected), kind!(Dot)]
            | [.., kind!(Use), ident!(library), kind!(Dot), ident!(selected), kind!(Dot), kind!(StringLiteral | Identifier)] => {
                self.list_available_declarations(library, selected)
            }
            _ => {
                let mut completions = vec![];
                let mut visitor = AutocompletionVisitor {
                    completions: &mut completions,
                    root: self,
                    cursor,
                };
                self.walk(&mut visitor);
                completions
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::analysis::completion::tokenize_input;
    use crate::analysis::tests::LibraryBuilder;
    use crate::syntax::test::Code;
    use assert_matches::assert_matches;

    #[test]
    fn tokenizing_an_empty_input() {
        let input = Code::new("");
        let tokens = tokenize_input(&input.symbols, input.source(), Position::new(0, 0));
        assert_eq!(tokens.len(), 0);
    }

    #[test]
    fn tokenizing_stops_at_the_cursors_position() {
        let input = Code::new("use ieee.std_logic_1164.all");
        let mut cursor = input.s1("std_logic_11").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        cursor = input.s1("std_logic_1164").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [kind!(Use), kind!(Identifier), kind!(Dot), kind!(Identifier)]
        );
        cursor = input.s1("std_logic_1164.").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [
                kind!(Use),
                kind!(Identifier),
                kind!(Dot),
                kind!(Identifier),
                kind!(Dot)
            ]
        );
        cursor = input.s1("std_logic_1164.all").pos().end();
        let tokens = tokenize_input(&input.symbols, input.source(), cursor);
        assert_matches!(
            tokens[..],
            [
                kind!(Use),
                kind!(Identifier),
                kind!(Dot),
                kind!(Identifier),
                kind!(Dot),
                kind!(All)
            ]
        );
    }

    #[test]
    pub fn completing_libraries() {
        let input = LibraryBuilder::new();
        let code = Code::new("library ");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("library ").pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert_eq!(options, root.list_all_libraries())
    }

    #[test]
    pub fn completing_primaries() {
        let (root, _) = LibraryBuilder::new().get_analyzed_root();
        let code = Code::new("use std.");
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        let expected_items = vec![
            CompletionItem {
                label: "textio".to_string(),
                entity: Some(root.find_textio_pkg().id),
                detail: "package 'textio'".to_string(),
                mode: CompletionItemMode::Text,
                kind: CompletionKind::Module,
            },
            CompletionItem {
                label: "standard".to_string(),
                entity: Some(root.find_standard_pkg().id),
                detail: "package 'standard'".to_string(),
                mode: CompletionItemMode::Text,
                kind: CompletionKind::Module,
            },
            CompletionItem {
                label: "env".to_string(),
                entity: Some(root.find_env_pkg().id),
                detail: "package 'env'".to_string(),
                mode: CompletionItemMode::Text,
                kind: CompletionKind::Module,
            },
        ];
        assert_eq!(options, expected_items);

        let code = Code::new("use std.t");
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        // Note that the filtering only happens at client side
        assert_eq!(options, expected_items);
    }

    #[test]
    pub fn completing_declarations() {
        let input = LibraryBuilder::new();
        let code = Code::new("use std.env.");
        let (root, _) = input.get_analyzed_root();
        let cursor = code.pos().end();
        let options = root.list_completion_options(code.source(), cursor);

        assert_eq!(
            options,
            [
                CompletionItem {
                    label: "all".to_string(),
                    detail: "all".to_string(),
                    entity: None,
                    mode: CompletionItemMode::Text,
                    kind: CompletionKind::Module
                },
                CompletionItem {
                    label: "finish".to_string(),
                    detail: "+1 overloaded".to_string(),
                    entity: None,
                    mode: CompletionItemMode::Text,
                    kind: CompletionKind::Function
                },
                CompletionItem {
                    label: "resolution_limit".to_string(),
                    detail: "function resolution_limit[return DELAY_LENGTH]".to_string(),
                    entity: Some(root.find_env_symbol("resolution_limit").id),
                    mode: CompletionItemMode::Text,
                    kind: CompletionKind::Function
                },
                CompletionItem {
                    label: "stop".to_string(),
                    detail: "+1 overloaded".to_string(),
                    entity: None,
                    mode: CompletionItemMode::Text,
                    kind: CompletionKind::Function
                },
            ]
        );
    }

    #[test]
    pub fn completing_instantiation_statement() {
        let mut input = LibraryBuilder::new();
        let code = input.code(
            "libname",
            "\
entity my_ent is
end entity my_ent;

architecture arch of my_ent is
    component comp is
    generic (
      A: natural := 5;
      B: integer
    );
    port (
      clk : in bit;
      rst : in bit;
      dout : out bit
    );
    end component comp;
    signal clk, rst: bit;
begin
    comp_inst: comp
    generic map (
        A => 2
    )
    port map (
        clk => clk
    );
end arch;
        ",
        );
        let (root, _) = input.get_analyzed_root();
        let cursor = code.s1("generic map (").pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert_eq!(
            options.iter().map(|it| &it.label).collect_vec(),
            vec!["B => $1,"]
        );

        let cursor = code.s1("port map (").pos().end();
        let options = root.list_completion_options(code.source(), cursor);
        assert_eq!(
            options.iter().map(|it| &it.label).collect_vec(),
            vec!["rst => $1,", "dout => $1,"]
        );
    }
}
