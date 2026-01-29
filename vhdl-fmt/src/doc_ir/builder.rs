use vhdl_syntax::{syntax::node::SyntaxToken, tokens::Trivia};

use crate::doc_ir::{Doc, DocComment};

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

    pub fn push(&mut self, doc: Doc) {
        self.children.push(doc);
    }

    pub fn token(&mut self, token: SyntaxToken) {
        self.push(Doc::Token(token));
    }

    pub fn hard_break(&mut self) {
        self.push(Doc::HardBreak);
    }

    pub fn space(&mut self) {
        self.push(Doc::Space);
    }

    pub fn soft_break(&mut self) {
        self.push(Doc::SoftBreak);
    }

    pub fn comment(&mut self, comment: DocComment) {
        self.push(Doc::Comment(comment));
    }

    pub fn start_concat(&mut self) {
        let len = self.children.len();
        self.parents.push(len);
    }

    pub fn trivia(&mut self, trivia: Trivia) {
        self.push(Doc::Trivia(trivia));
    }

    pub fn end_concat(&mut self) {
        let first_child = self.parents.pop().unwrap();
        let data = self.children.drain(first_child..).collect::<Vec<_>>();
        debug_assert!(
            !data.is_empty(),
            "Indented regions should not be empty as nodes should not be empty"
        );
        self.push(Doc::Concat(data));
    }

    pub fn embed_in_group(&mut self) {
        let last = self.children.pop().unwrap();
        self.push(Doc::Group(Box::new(last)));
    }

    pub fn embed_in_indent(&mut self) {
        let last = self.children.pop().unwrap();
        self.push(Doc::Indent(Box::new(last)));
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
