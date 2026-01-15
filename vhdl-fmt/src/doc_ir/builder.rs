use vhdl_syntax::syntax::node::SyntaxToken;

use crate::doc_ir::Doc;

pub struct DocBuilder {
    parents: Vec<usize>,
    children: Vec<Doc>,
}

impl DocBuilder {
    pub fn new() -> DocBuilder {
        DocBuilder {
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn push(&mut self, token: SyntaxToken) {
        self.children.push(Doc::Token(token));
    }

    pub fn hard_break(&mut self) {
        self.children.push(Doc::HardBreak);
    }

    pub fn indent(&mut self) {
        let len = self.children.len();
        self.parents.push(len);
    }

    pub fn dedent(&mut self) {
        let first_child = self.parents.pop().unwrap();
        let data = self.children.drain(first_child..).collect::<Vec<_>>();
        debug_assert!(
            !data.is_empty(),
            "Indented regions should not be empty as nodes should not be empty"
        );
        self.children.push(Doc::Indent(data));
    }

    pub fn build(mut self) -> Doc {
        // TODO: This does not work for an empty doc at the moment
        if self.children.len() == 1 {
            self.children.pop().unwrap()
        } else {
            Doc::Concat(self.children)
        }
    }
}
