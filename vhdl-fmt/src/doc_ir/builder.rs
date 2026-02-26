use vhdl_syntax::{syntax::node::SyntaxToken, tokens::Trivia};

use crate::doc_ir::{Doc, DocComment, Docs};

pub enum NodeKind {
    Concat,
    Indent,
    Group,
}

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

    pub fn blank_lines(&mut self, n: usize) {
        if n > 0 {
            self.push(Doc::BlankLines(n));
        }
    }

    pub fn aligned_spaces(&mut self, n: usize) {
        self.push(Doc::AlignedSpace(n));
    }

    pub fn token(&mut self, token: SyntaxToken) {
        self.push(Doc::Token(token));
    }

    pub fn hard_break(&mut self) {
        self.push(Doc::HardBreak);
    }

    pub fn space(&mut self) {
        self.push(Doc::Spaces(1));
    }

    pub fn soft_break(&mut self) {
        self.push(Doc::SoftBreak { flat_spaces: 1 });
    }

    pub fn comment(&mut self, comment: DocComment) {
        self.push(Doc::Comment(comment));
    }

    pub fn trailing_comment(&mut self, comment: DocComment) {
        self.push(Doc::TrailingComment(comment));
    }

    pub fn trivia(&mut self, trivia: Trivia) {
        if trivia.is_empty() {
            return;
        }
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
                Event::Push(doc) => {
                    children.push(doc);
                }
                Event::Start(_) => {
                    let len = children.len();
                    parents.push(len);
                }
                Event::End(node_kind) => {
                    let first_child = parents.pop().unwrap();
                    let mut data = children.drain(first_child..).collect::<Vec<_>>();
                    debug_assert!(
                        !data.is_empty(),
                        "Nodes should not be empty as SyntaxNodes should not be empty"
                    );
                    match node_kind {
                        NodeKind::Concat => {
                            // canonicalize: Concat([X]) == X
                            // This makes reading the Doc IR much easier
                            if data.len() == 1 {
                                children.push(data.pop().unwrap());
                            } else {
                                children.push(Doc::Concat(Docs(data)))
                            }
                        }
                        NodeKind::Indent => children.push(Doc::Indent(Docs(data))),
                        NodeKind::Group => children.push(Doc::Group(Docs(data))),
                    }
                }
            }
        }
        assert!(children.len() == 1);
        let doc = children.pop().unwrap();
        println!("{doc:#?}");
        doc
    }
}
