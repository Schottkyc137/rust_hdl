// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::error::{SyntaxErr, SyntaxErrKind};
use crate::syntax::child::Child;
use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::tokens::tokenizer::LexErr;
use crate::tokens::{Token, TokenKind};

enum Event {
    /// Start(Some): Start a node
    /// Start(None): record that a node could start here that is later overwritten
    Start(Option<NodeKind>),
    /// End a node
    End,
    /// Push a token with an optional associated lexer error
    Push(Token, Option<LexErr>),
    /// Precede (retroactively wrap) a node that was just finished with the given node kind
    Precede(NodeKind),
    /// Emit a syntax error that is associated to the last token or node pushed
    Error(SyntaxErrKind),
}

fn first_token(node: &GreenNode) -> &Token {
    match node.children().next().expect("empty nodes are dropped") {
        GreenChild::Token(token) => token.token(),
        GreenChild::Node(node) => first_token(node),
    }
}

/// Internal builder used to create nodes when parsing.
pub(crate) struct NodeBuilder {
    token_index: usize,
    events: Vec<Event>,
}

pub(crate) struct Marker {
    pos: usize,
    #[cfg(debug_assertions)]
    fused: bool,
}

impl Marker {
    pub fn new(pos: usize) -> Marker {
        Marker {
            pos,
            #[cfg(debug_assertions)]
            fused: true,
        }
    }

    pub fn defuse(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.fused = false;
        }
    }
}

impl Drop for Marker {
    fn drop(&mut self) {
        #[cfg(debug_assertions)]
        // Don't panic while another panic is unwinding: this aborts the process
        // and hides the original failure
        if self.fused && !std::thread::panicking() {
            panic!("marker dropped without set_unknown");
        }
    }
}

impl NodeBuilder {
    pub fn new() -> NodeBuilder {
        NodeBuilder {
            token_index: 0,
            events: Vec::new(),
        }
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }

    pub fn push(&mut self, token: Token, err: Option<LexErr>) {
        self.token_index += 1;
        self.push_event(Event::Push(token, err));
    }

    pub fn start_node(&mut self, kind: NodeKind) {
        self.push_event(Event::Start(Some(kind)));
    }

    pub fn end_node(&mut self) {
        self.push_event(Event::End);
    }

    pub fn end(self) -> (GreenNode, Vec<SyntaxErr>) {
        struct Parent {
            kind: NodeKind,
            // index of its first child
            first_child: usize,
            // number of errors recorded while started
            first_error: usize,
        }
        let mut parents: Vec<Parent> = Vec::new();
        let mut children: Vec<GreenChild> = Vec::new();
        let mut errors: Vec<SyntaxErr> = Vec::new();
        // Byte offset just past the last token's text, i.e. before the leading
        // trivia of the next one. This is where a missing token would go.
        let mut text_pos = 0usize;

        for event in self.events {
            match event {
                Event::Start(Some(kind)) => {
                    parents.push(Parent {
                        kind,
                        first_child: children.len(),
                        first_error: errors.len(),
                    });
                }
                Event::Start(None) => unreachable!("start_unknown without set_unknown"),
                Event::End => {
                    let Parent {
                        kind,
                        first_child,
                        first_error,
                    } = parents.pop().expect("end_node without start_node");
                    // Drop empty nodes and merge multiple errors into one
                    if children.len() == first_child {
                        if errors.len() > first_error {
                            errors.truncate(first_error);
                            let folded = SyntaxErrKind::Expected(
                                Child::<_, Box<[TokenKind]>>::Node(Box::new([kind])),
                            );
                            errors.push(SyntaxErr::new(text_pos..text_pos, folded));
                        }
                        continue;
                    }
                    let mut data = GreenNodeData::new(kind);
                    data.push_children(children.drain(first_child..));
                    children.push(GreenChild::Node(GreenNode::new(data)));
                }
                Event::Push(token, err) => {
                    if let Some(err) = err {
                        errors.push(SyntaxErr::from_lex_err(err, &token, text_pos));
                    }
                    text_pos += token.byte_len();
                    children.push(GreenChild::Token(GreenToken::new(token)));
                }
                Event::Precede(kind) => {
                    let first_child = parents.last().map_or(0, |parent| parent.first_child);
                    assert!(
                        children.len() > first_child,
                        "precede: nothing has been built in the current node"
                    );
                    assert!(
                        matches!(children.last(), Some(GreenChild::Node(_))),
                        "precede: the last child is a token, not a completed node"
                    );
                    parents.push(Parent {
                        kind,
                        first_child: children.len() - 1,
                        first_error: errors.len(),
                    });
                }
                Event::Error(kind) => match kind {
                    SyntaxErrKind::Expected(_) => {
                        errors.push(SyntaxErr::new(text_pos..text_pos, kind))
                    }
                    SyntaxErrKind::Unexpected(Child::Node(_)) => {
                        let Some(GreenChild::Node(node)) = children.last() else {
                            panic!("{kind:?} does not follow a node");
                        };
                        let start = text_pos - node.byte_len()
                            + first_token(node).leading_trivia().byte_len();
                        errors.push(SyntaxErr::new(start..text_pos, kind));
                    }
                    SyntaxErrKind::Unexpected(Child::Token(_)) => {
                        let Some(GreenChild::Token(token)) = children.last() else {
                            panic!("{kind:?} does not follow a token");
                        };
                        let start = text_pos - token.text().len();
                        errors.push(SyntaxErr::new(start..text_pos, kind));
                    }
                    SyntaxErrKind::Unterminated(_) => unreachable!("handled by push(Token)"),
                },
            }
        }

        assert_eq!(children.len(), 1, "expected exactly one root node");
        let root = match children.pop().unwrap() {
            GreenChild::Node(node) => node,
            GreenChild::Token(_) => panic!("the root must be a node, not a token"),
        };
        (root, errors)
    }

    /// Start an unknown node that is later patched using `set_unknown`
    pub fn start_unknown(&mut self) -> Marker {
        let marker = Marker::new(self.events.len());
        self.push_event(Event::Start(None));
        marker
    }

    pub fn set_unknown(&mut self, mut marker: Marker, kind: NodeKind) {
        self.events[marker.pos] = Event::Start(Some(kind));
        marker.defuse();
    }

    /// Record an error.
    /// The meaning of this is encoded in the `kind`:
    /// SyntaxErrKind::Unexpected is attached to the last token or node
    /// SyntaxErrKind::Expected is not attached and signifies that something was missing
    pub fn push_err(&mut self, kind: SyntaxErrKind) {
        self.push_event(Event::Error(kind));
    }

    /// Insert a new parent above the node that was just completed.
    pub fn precede(&mut self, kind: NodeKind) {
        self.push_event(Event::Precede(kind));
    }

    pub fn current_token_index(&self) -> usize {
        self.token_index
    }

    /// The node that was just parsed
    pub fn last_node(&self) -> Option<NodeKind> {
        let Some(Event::End) = self.events.last() else {
            return None;
        };
        let mut depth = 1usize;

        for event in self.events.iter().rev().skip(1) {
            match event {
                // `Precede` opens a node too, it is just written after its first child
                Event::Start(Some(kind)) | Event::Precede(kind) if depth == 1 => {
                    return Some(*kind)
                }
                Event::Start(_) | Event::Precede(_) => depth -= 1,
                Event::End => depth += 1,
                Event::Push(..) | Event::Error(_) => continue,
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::error::Span;
    use crate::syntax::child::ChildKind;
    use crate::tokens::trivia_piece::TriviaPiece;
    use crate::tokens::Trivia;

    const ROOT: NodeKind = NodeKind::DesignFile;
    const OUTER: NodeKind = NodeKind::DesignUnit;
    const INNER: NodeKind = NodeKind::Name;

    /// A token preceded by `spaces` spaces of leading trivia.
    fn tok(text: &'static [u8], spaces: usize) -> Token {
        let trivia = if spaces == 0 {
            Trivia::new()
        } else {
            Trivia::from([TriviaPiece::Spaces(spaces)])
        };
        Token::new(TokenKind::Identifier, text, trivia)
    }

    fn expected_token() -> SyntaxErrKind {
        SyntaxErrKind::Expected(Child::<Box<[NodeKind]>, _>::Token(Box::new([
            TokenKind::SemiColon,
        ])))
    }

    fn spans(errors: &[SyntaxErr]) -> Vec<Span> {
        errors.iter().map(|err| err.span().clone()).collect()
    }

    #[test]
    fn expected_is_zero_width_at_the_end_of_the_preceding_token() {
        // `ab cd`: the error sits between the two tokens and must hug `ab`
        // rather than point past the space.
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.push_err(expected_token());
        builder.push(tok(b"cd", 1), None);
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![2..2]);
    }

    #[test]
    fn expected_at_the_start_of_the_input_is_zero_width_at_zero() {
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push_err(expected_token());
        builder.push(tok(b"ab", 0), None);
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![0..0]);
    }

    #[test]
    fn unexpected_node_is_measured_without_its_leading_trivia() {
        // `  ab`: the node covers only the token's text, not the two spaces
        // that precede it.
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.start_node(INNER);
        builder.push(tok(b"ab", 2), None);
        builder.end_node();
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Node(INNER)));
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![2..4]);
    }

    #[test]
    fn separate_unexpected_runs_do_not_merge() {
        // A good token between two runs closes the first one.
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Token(
            TokenKind::Identifier,
        )));
        builder.push(tok(b"cd", 1), None);
        builder.push(tok(b"ef", 1), None);
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Token(
            TokenKind::Identifier,
        )));
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![0..2, 6..8]);
    }

    #[test]
    fn errors_in_an_empty_node_fold_into_one_naming_the_node() {
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start_node(INNER);
        builder.push_err(expected_token());
        builder.push_err(expected_token());
        builder.end_node();
        builder.end_node();

        let (root, errors) = builder.end();
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(errors[0].err(), SyntaxErrKind::Expected(Child::Node(kinds)) if **kinds == [INNER])
        );
        assert_eq!(*errors[0].span(), 2..2);
        // The empty node is not in the tree.
        assert!(root.children().count() == 1);
    }

    #[test]
    fn nested_empty_nodes_fold_to_the_outermost() {
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start_node(OUTER);
        builder.start_node(INNER);
        builder.push_err(expected_token());
        builder.end_node();
        builder.end_node();
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(errors.len(), 1);
        assert!(
            matches!(errors[0].err(), SyntaxErrKind::Expected(Child::Node(kinds)) if **kinds == [OUTER])
        );
    }

    #[test]
    fn a_node_that_holds_a_token_is_kept_with_its_errors_in_place() {
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.start_node(INNER);
        builder.push_err(expected_token());
        builder.push(tok(b"ab", 1), None);
        builder.end_node();
        builder.end_node();

        let (root, errors) = builder.end();
        assert_eq!(spans(&errors), vec![0..0]);
        assert!(matches!(
            root.children().next(),
            Some(GreenChild::Node(node)) if node.kind() == INNER
        ));
    }
}
