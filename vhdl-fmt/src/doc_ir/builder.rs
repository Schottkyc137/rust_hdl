use vhdl_syntax::{syntax::node::SyntaxToken, tokens::Trivia};

use crate::doc_ir::{Doc, DocComment, Docs};

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum NodeKind {
    Concat,
    Indent,
    Group,
}

#[derive(Clone, Debug)]
pub enum Event {
    Push(Doc),
    Start(NodeKind),
    End(NodeKind),
}

pub struct DocBuilder {
    events: Vec<Event>,
}

impl DocBuilder {
    pub fn new() -> DocBuilder {
        DocBuilder { events: Vec::new() }
    }

    pub fn push(&mut self, doc: Doc) {
        self.events.push(Event::Push(doc));
    }

    pub fn token(&mut self, token: SyntaxToken) {
        self.push(Doc::Token(token));
    }

    #[allow(dead_code)]
    pub fn hard_break(&mut self) {
        self.push(Doc::HardBreak);
    }

    #[allow(dead_code)]
    pub fn space(&mut self) {
        self.push(Doc::Space);
    }

    pub fn soft_break(&mut self) {
        self.push(Doc::SoftBreak);
    }

    pub fn comment(&mut self, comment: DocComment) {
        self.push(Doc::Comment(comment));
    }

    pub fn trivia(&mut self, trivia: Trivia) {
        self.push(Doc::Trivia(trivia));
    }

    fn start(&mut self, kind: NodeKind) {
        self.events.push(Event::Start(kind));
    }

    fn end(&mut self, kind: NodeKind) {
        self.events.push(Event::End(kind));
    }

    pub fn start_concat(&mut self) {
        self.start(NodeKind::Concat);
    }

    pub fn start_group(&mut self) {
        self.start(NodeKind::Group);
    }

    pub fn start_indent(&mut self) {
        self.start(NodeKind::Indent);
    }

    pub fn end_concat(&mut self) {
        self.end(NodeKind::Concat);
    }

    pub fn end_group(&mut self) {
        self.end(NodeKind::Group);
    }

    pub fn end_indent(&mut self) {
        self.end(NodeKind::Indent);
    }

    pub fn build(self) -> Doc {
        let mut children = Vec::new();
        let mut parents = Vec::new();
        for event in self.events {
            match event {
                Event::Push(doc) => children.push(doc),
                Event::Start(_) => {
                    let len = children.len();
                    parents.push(len);
                }
                Event::End(node_kind) => {
                    // Canonicalize: Concat(Concat(X)) -> X
                    if node_kind == NodeKind::Concat && children.last().is_some_and(|doc| matches!(doc, Doc::Concat(_))) {
                        parents.pop();
                        continue;
                    }
                    let first_child = parents.pop().unwrap();
                    let data = children.drain(first_child..).collect::<Vec<_>>();
                    debug_assert!(
                        !data.is_empty(),
                        "Nodes should not be empty as SyntaxNodes should not be empty"
                    );
                    match node_kind {
                        NodeKind::Concat => children.push(Doc::Concat(Docs(data))),
                        NodeKind::Indent => children.push(Doc::Indent(Docs(data))),
                        NodeKind::Group => children.push(Doc::Group(Docs(data))),
                    }
                },
            }
        }
        assert!(children.len() == 1);
        let doc = children.pop().unwrap();
        println!("{doc:#?}");
        doc
    }
}
