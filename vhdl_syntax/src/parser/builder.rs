// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData, GreenToken};
use crate::syntax::node_kind::NodeKind;
use crate::tokens::Token;

/// Internal builder used to create nodes when parsing.
pub(crate) struct NodeBuilder {
    text_len: usize,
    token_index: usize,
    parents: Vec<(NodeKind, usize)>,
    children: Vec<GreenChild>,
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
            panic!("marker dropped without set_unknown or abandon");
        }
    }
}

impl NodeBuilder {
    pub fn new() -> NodeBuilder {
        NodeBuilder {
            text_len: 0,
            token_index: 0,
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn push(&mut self, token: Token) {
        let tok_text_len = token.byte_len();
        self.children
            .push(GreenChild::Token(GreenToken::new(token)));
        self.token_index += 1;
        self.text_len += tok_text_len;
    }

    pub fn start_node(&mut self, kind: NodeKind) {
        let len = self.children.len();
        self.parents.push((kind, len))
    }

    pub fn end_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let mut data = GreenNodeData::new(kind);
        data.push_children(self.children.drain(first_child..));
        // Do not push empty children
        // TODO: This is a required invariant, but enforcing it here is brittle.
        // Instead, we should move to a event-based API for parser <-> builder
        if !data.is_empty() {
            self.children.push(GreenChild::Node(GreenNode::new(data)));
        }
    }

    pub fn end(mut self) -> GreenNode {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            GreenChild::Node(node) => node,
            GreenChild::Token(_) => panic!(),
        }
    }

    /// Start an unknown node that is later patched using `set_unknown`
    pub fn start_unknown(&self) -> Marker {
        Marker::new(self.children.len())
    }

    pub fn set_unknown(&mut self, mut marker: Marker, kind: NodeKind) {
        assert!(
            marker.pos <= self.children.len(),
            "marker no longer valid, was end_node called early?"
        );

        if let Some(&(_, first_child)) = self.parents.last() {
            assert!(
                marker.pos >= first_child,
                "marker no longer valid, was it taken in an already-closed node?"
            );
        }

        self.parents.push((kind, marker.pos));
        marker.defuse();
    }

    #[allow(dead_code)]
    pub fn abandon(&mut self, mut marker: Marker) {
        // TODO: set tombstone when event-based
        marker.defuse();
    }

    /// Insert a new parent above the node that was just completed.
    pub fn precede(&mut self, kind: NodeKind) {
        let first_child = self
            .parents
            .last()
            .map_or(0, |&(_, first_child)| first_child);
        assert!(
            self.children.len() > first_child,
            "precede: nothing has been built in the current node"
        );
        assert!(
            matches!(self.children.last(), Some(GreenChild::Node(_))),
            "precede: the last child is a token, not a completed node"
        );
        self.parents.push((kind, self.children.len() - 1));
    }

    pub fn current_pos(&self) -> usize {
        self.text_len
    }

    pub fn current_token_index(&self) -> usize {
        self.token_index
    }

    /// The node that was just parsed
    pub fn last_node(&self) -> Option<NodeKind> {
        let first_child = self
            .parents
            .last()
            .map_or(0, |&(_, first_child)| first_child);
        if self.children.len() <= first_child {
            return None;
        }
        match self.children.last()? {
            GreenChild::Token(_) => None,
            GreenChild::Node(node) => Some(node.kind()),
        }
    }
}
