// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::tokens::Token;

enum Event {
    Start(Option<NodeKind>),
    End,
    Push(Token),
    Precede(NodeKind),
}

/// Internal builder used to create nodes when parsing.
pub(crate) struct NodeBuilder {
    text_len: usize,
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
            text_len: 0,
            token_index: 0,
            events: Vec::new(),
        }
    }

    fn push_event(&mut self, event: Event) {
        self.events.push(event);
    }

    pub fn push(&mut self, token: Token) {
        self.token_index += 1;
        self.text_len += token.byte_len();
        self.push_event(Event::Push(token));
    }

    pub fn start_node(&mut self, kind: NodeKind) {
        self.push_event(Event::Start(Some(kind)));
    }

    pub fn end_node(&mut self) {
        self.events.push(Event::End);
    }

    pub fn end(self) -> GreenNode {
        let mut parents: Vec<(NodeKind, usize)> = Vec::new();
        let mut children: Vec<GreenChild> = Vec::new();

        for event in self.events {
            match event {
                Event::Start(Some(kind)) => parents.push((kind, children.len())),
                Event::Start(None) => unreachable!("start_unknown without set_unknown"),
                Event::End => {
                    let (kind, first_child) = parents.pop().expect("end_node without start_node");
                    let mut data = GreenNodeData::new(kind);
                    data.push_children(children.drain(first_child..));
                    if !data.is_empty() {
                        children.push(GreenChild::Node(GreenNode::new(data)));
                    }
                }
                Event::Push(token) => {
                    children.push(GreenChild::Token(GreenToken::new(token)));
                }
                // `set_unknown` overwrites the event, so an unpatched one never reaches this point
                Event::Precede(kind) => {
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
            }
        }

        assert_eq!(children.len(), 1, "expected exactly one root node");
        match children.pop().unwrap() {
            GreenChild::Node(node) => node,
            GreenChild::Token(_) => panic!("the root must be a node, not a token"),
        }
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

    /// Insert a new parent above the node that was just completed.
    pub fn precede(&mut self, kind: NodeKind) {
        self.push_event(Event::Precede(kind));
    }

    pub fn current_pos(&self) -> usize {
        self.text_len
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
                Event::Start(Some(kind)) | Event::Precede(kind) if depth == 1 => return Some(*kind),
                Event::Start(_) | Event::Precede(_) => depth -= 1,
                Event::End => depth += 1,
                Event::Push(_) => continue,
            }
        }

        None
    }

}
