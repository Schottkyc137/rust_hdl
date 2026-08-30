// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::parser::error::{Span, SyntaxErr, SyntaxErrKind};
use crate::syntax::child::{Child, ChildKind};
use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::tokens::tokenizer::LexErr;
use crate::tokens::{Token, TokenKind};
use std::collections::VecDeque;

enum Event {
    Start(Option<NodeKind>),
    End,
    /// A token, together with the lexer error it carries, if any. A lexer error
    /// can address a piece of the token's leading trivia
    /// ([`LexErrPos::Trivia`](crate::tokens::tokenizer::LexErrPos::Trivia)), which
    /// no anchor could express, so it rides the token instead of becoming an
    /// [`Event::Error`] of its own.
    Push(Token, Option<LexErr>),
    Precede(NodeKind),
    /// An error, spanned by nothing: the parser states only *what* went wrong and
    /// the folding pass derives *where* from the kind. See [`Anchor`].
    // Boxed because errors are rare: stored inline, `SyntaxErrKind` would widen
    // every event in the stream, and there is one event per token.
    Error(Box<SyntaxErrKind>),
}

/// Where an error is measured, derived from its kind by [`Folder::emit_error`].
enum Anchor {
    /// Zero width at the current text position: something was expected here.
    Here,
    /// The extent of the node that closed just before this event.
    PrecedingNode,
}

/// The event stream after folding. Two invariants the raw stream lacks:
/// every [`FoldedEvent::Start`] has at least one token under it, and every
/// error either carries an [`Anchor`] or is delimited by an
/// [`FoldedEvent::ErrorStart`]/[`FoldedEvent::ErrorEnd`] pair.
enum FoldedEvent {
    Start(NodeKind),
    End,
    Push(Token, Option<LexErr>),
    Precede(NodeKind),
    Error(SyntaxErrKind, Anchor),
    /// Opens an error spanning several tokens. Emitted directly *after* the
    /// run's first token, so the span starts at that token's text.
    ErrorStart(SyntaxErrKind),
    /// Closes the open [`FoldedEvent::ErrorStart`] at the end of the last token
    /// of the run.
    ErrorEnd,
}

/// A node that has been started but has not yet seen a token, so it is not yet
/// known whether it survives. Errors reported inside it are held here: if no
/// token ever arrives the node is dropped and they collapse into a single
/// `Expected(Node(kind))`.
struct PendingNode {
    kind: NodeKind,
    errors: Vec<SyntaxErrKind>,
}

/// Folds the raw event stream into [`FoldedEvent`]s, dropping empty nodes and
/// merging the errors reported inside them.
///
/// Buffering is bounded by the distance to the next token — that is, by nesting
/// depth at the point of failure — not by the length of the stream.
struct Folder<I> {
    inner: I,
    out: VecDeque<FoldedEvent>,
    pending: Vec<PendingNode>,
    /// The token held back to see whether the error that reports it follows.
    /// A token inside a run must not close that run, so a token is only
    /// released once the next event says which side of the run it is on.
    held: Option<(Token, Option<LexErr>)>,
    /// Whether an `ErrorStart` is open and still absorbing `Unexpected` tokens.
    run_open: bool,
    /// The kind of the node dropped by the event just handled, if any. An
    /// `Unexpected(Node(k))` naming it refers to a node that no longer exists.
    just_dropped: Option<NodeKind>,
    drained: bool,
}

fn fold<I: Iterator<Item = Event>>(inner: I) -> Folder<I> {
    Folder {
        inner,
        out: VecDeque::new(),
        pending: Vec::new(),
        held: None,
        run_open: false,
        just_dropped: None,
        drained: false,
    }
}

impl<I: Iterator<Item = Event>> Folder<I> {
    /// Close an open run of unexpected tokens. Must run before the event that
    /// ends the run is emitted, so that the run's span stops at its last token.
    fn close_run(&mut self) {
        if self.run_open {
            self.out.push_back(FoldedEvent::ErrorEnd);
            self.run_open = false;
        }
    }

    /// Emit the nodes that were waiting to find out whether they survive, each
    /// followed by the errors reported inside it before its first token.
    fn flush_pending(&mut self) {
        for node in std::mem::take(&mut self.pending) {
            self.out.push_back(FoldedEvent::Start(node.kind));
            for kind in node.errors {
                self.emit_error(kind);
            }
        }
    }

    /// Release the held token, which also settles every node waiting on it.
    fn release_held(&mut self) {
        if let Some((token, err)) = self.held.take() {
            self.flush_pending();
            self.out.push_back(FoldedEvent::Push(token, err));
        }
    }

    /// Assign an anchor from the kind and queue the error.
    fn emit_error(&mut self, kind: SyntaxErrKind) {
        match &kind {
            // Each skipped token reports itself, so these arrive in runs; the
            // first opens the run and the rest are absorbed by it.
            SyntaxErrKind::Unexpected(ChildKind::Token(_)) => {
                if !self.run_open {
                    self.out.push_back(FoldedEvent::ErrorStart(kind));
                    self.run_open = true;
                }
            }
            SyntaxErrKind::Unexpected(ChildKind::Node(_)) => {
                self.out
                    .push_back(FoldedEvent::Error(kind, Anchor::PrecedingNode));
            }
            SyntaxErrKind::Expected(_) => {
                self.out.push_back(FoldedEvent::Error(kind, Anchor::Here));
            }
            SyntaxErrKind::Unterminated(_) => {
                unreachable!("lexer errors ride their token, they are never pushed as events")
            }
        }
    }

    fn handle(&mut self, event: Event) {
        // A held token reported by the error that follows it belongs to the
        // run, and extends it rather than ending it. The `ErrorStart` goes
        // *after* the token so the run's span starts at that token's text.
        if let Event::Error(kind) = event {
            if matches!(*kind, SyntaxErrKind::Unexpected(ChildKind::Token(_)))
                && self.held.is_some()
            {
                self.release_held();
                if !self.run_open {
                    self.out.push_back(FoldedEvent::ErrorStart(*kind));
                    self.run_open = true;
                }
                self.just_dropped = None;
                return;
            }
            return self.handle_other(Event::Error(kind));
        }
        self.handle_other(event)
    }

    fn handle_other(&mut self, event: Event) {
        // Any other event puts the held token outside the run, so the run ends
        // at the token before it.
        if self.held.is_some() {
            self.close_run();
            self.release_held();
        }
        let just_dropped = self.just_dropped.take();
        match event {
            Event::Start(Some(kind)) => {
                self.close_run();
                self.pending.push(PendingNode {
                    kind,
                    errors: Vec::new(),
                });
            }
            Event::Start(None) => unreachable!("start_unknown without set_unknown"),
            Event::Push(token, err) => self.held = Some((token, err)),
            Event::Precede(kind) => {
                self.close_run();
                self.flush_pending();
                self.out.push_back(FoldedEvent::Precede(kind));
            }
            Event::End => {
                self.close_run();
                let Some(node) = self.pending.pop() else {
                    self.out.push_back(FoldedEvent::End);
                    return;
                };
                // No token ever arrived, so the node is empty and is dropped.
                // Anything that went wrong inside it collapses into one error
                // naming the node, which reads better than the several
                // token-level errors that produced it.
                if !node.errors.is_empty() {
                    let folded =
                        SyntaxErrKind::Expected(Child::<_, Box<[TokenKind]>>::Node(Box::new([
                            node.kind,
                        ])));
                    match self.pending.last_mut() {
                        // Fold again at the next level out, so the outermost
                        // dropped node is the one reported.
                        Some(parent) => parent.errors.push(folded),
                        None => self.emit_error(folded),
                    }
                }
                self.just_dropped = Some(node.kind);
            }
            Event::Error(kind) => {
                let kind = *kind;
                // The node this names was folded away; reporting it would
                // measure an unrelated sibling.
                if let SyntaxErrKind::Unexpected(ChildKind::Node(named)) = &kind {
                    if just_dropped == Some(*named) {
                        self.just_dropped = just_dropped;
                        return;
                    }
                }
                // Inside a node that has no tokens yet: hold it, in case the
                // node turns out to be empty and the error folds into it.
                if let Some(node) = self.pending.last_mut() {
                    node.errors.push(kind);
                    return;
                }
                self.emit_error(kind);
            }
        }
    }
}

impl<I: Iterator<Item = Event>> Iterator for Folder<I> {
    type Item = FoldedEvent;

    fn next(&mut self) -> Option<FoldedEvent> {
        loop {
            if let Some(event) = self.out.pop_front() {
                return Some(event);
            }
            match self.inner.next() {
                Some(event) => self.handle(event),
                None if self.drained => return None,
                None => {
                    self.drained = true;
                    // The last token is outside any open run, same as if a
                    // further event had arrived.
                    self.close_run();
                    self.release_held();
                }
            }
        }
    }
}

/// The first token under `node`, for trimming a node's leading trivia off its
/// span. A folded node always has children, so the descent always terminates.
fn first_token(node: &GreenNode) -> &Token {
    match node.children().next().expect("a folded node has children") {
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
        self.events.push(Event::End);
    }

    pub fn end(self) -> (GreenNode, Vec<SyntaxErr>) {
        let mut parents: Vec<(NodeKind, usize)> = Vec::new();
        let mut children: Vec<GreenChild> = Vec::new();
        let mut errors: Vec<SyntaxErr> = Vec::new();
        // Byte offset just past the last token's text, i.e. before the leading
        // trivia of the next one. This is where a missing token would go.
        let mut text_pos = 0usize;
        // Text span of the last token, excluding its leading trivia.
        let mut last_token: Span = 0..0;
        // The open `ErrorStart`, with the text offset the run began at.
        let mut run: Option<(SyntaxErrKind, usize)> = None;

        for event in fold(self.events.into_iter()) {
            match event {
                FoldedEvent::Start(kind) => parents.push((kind, children.len())),
                FoldedEvent::End => {
                    let (kind, first_child) = parents.pop().expect("end_node without start_node");
                    let mut data = GreenNodeData::new(kind);
                    data.push_children(children.drain(first_child..));
                    debug_assert!(!data.is_empty(), "folding drops empty nodes");
                    children.push(GreenChild::Node(GreenNode::new(data)));
                }
                FoldedEvent::Push(token, err) => {
                    let text_start = text_pos + token.leading_trivia().byte_len();
                    if let Some(err) = err {
                        errors.push(SyntaxErr::from_lex_err(err, &token, text_pos));
                    }
                    last_token = text_start..text_start + token.text_len();
                    text_pos += token.byte_len();
                    children.push(GreenChild::Token(GreenToken::new(token)));
                }
                FoldedEvent::Precede(kind) => {
                    let first_child = parents.last().map_or(0, |&(_, first_child)| first_child);
                    assert!(
                        children.len() > first_child,
                        "precede: nothing has been built in the current node"
                    );
                    assert!(
                        matches!(children.last(), Some(GreenChild::Node(_))),
                        "precede: the last child is a token, not a completed node"
                    );
                    parents.push((kind, children.len() - 1));
                }
                FoldedEvent::Error(kind, Anchor::Here) => {
                    errors.push(SyntaxErr::new(text_pos..text_pos, kind));
                }
                FoldedEvent::Error(kind, Anchor::PrecedingNode) => {
                    let Some(GreenChild::Node(node)) = children.last() else {
                        panic!("{kind:?} does not follow a completed node");
                    };
                    // The kind names the node it is about, so it doubles as a
                    // check that the node measured is the node meant.
                    debug_assert!(
                        matches!(&kind, SyntaxErrKind::Unexpected(ChildKind::Node(named))
                            if *named == node.kind()),
                        "{kind:?} does not name the preceding {:?}",
                        node.kind()
                    );
                    // `byte_len` counts the node's leading trivia and `text_pos`
                    // does not count the next token's, so trimming the front
                    // needs the first token under the node.
                    let start =
                        text_pos - node.byte_len() + first_token(node).leading_trivia().byte_len();
                    errors.push(SyntaxErr::new(start..text_pos, kind));
                }
                FoldedEvent::ErrorStart(kind) => {
                    debug_assert!(run.is_none(), "nested ErrorStart");
                    run = Some((kind, last_token.start));
                }
                FoldedEvent::ErrorEnd => {
                    let (kind, start) = run.take().expect("ErrorEnd without ErrorStart");
                    errors.push(SyntaxErr::new(start..last_token.end, kind));
                }
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

    /// Record an error. Where it lands follows from its kind and from where it
    /// sits in the stream; see [`Anchor`] and [`Folder`].
    pub fn push_err(&mut self, kind: SyntaxErrKind) {
        self.push_event(Event::Error(Box::new(kind)));
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
    fn adjacent_unexpected_tokens_merge_into_one_span() {
        // `ab cd ef gh` with `cd` and `ef` unexpected: one diagnostic covering
        // both, not one per token.
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.push(tok(b"cd", 1), None);
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Token(
            TokenKind::Identifier,
        )));
        builder.push(tok(b"ef", 1), None);
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Token(
            TokenKind::Identifier,
        )));
        builder.push(tok(b"gh", 1), None);
        builder.end_node();

        let (_, errors) = builder.end();
        assert_eq!(spans(&errors), vec![3..8]);
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
    fn unexpected_node_naming_a_dropped_node_is_discarded() {
        // The node was empty, so it is not in the tree; the error that names
        // it would otherwise be measured against an unrelated sibling.
        let mut builder = NodeBuilder::new();
        builder.start_node(ROOT);
        builder.push(tok(b"ab", 0), None);
        builder.start_node(INNER);
        builder.end_node();
        builder.push_err(SyntaxErrKind::Unexpected(ChildKind::Node(INNER)));
        builder.end_node();

        let (_, errors) = builder.end();
        assert!(errors.is_empty(), "{errors:?}");
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
