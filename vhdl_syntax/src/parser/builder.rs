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
    rel_offset: usize,
    text_len: usize,
    token_index: usize,
    parents: Vec<(NodeKind, usize)>,
    children: Vec<GreenChild>,
}

pub(crate) type Checkpoint = usize;

impl NodeBuilder {
    pub fn new() -> NodeBuilder {
        NodeBuilder {
            rel_offset: 0,
            text_len: 0,
            token_index: 0,
            parents: Vec::new(),
            children: Vec::new(),
        }
    }

    pub fn push(&mut self, token: Token) {
        let tok_text_len = token.byte_len();
        let offset = self.rel_offset;
        self.children
            .push(GreenChild::Token((offset, GreenToken::new(token))));
        self.rel_offset += tok_text_len;
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
        self.children
            .push(GreenChild::Node((0, GreenNode::new(data))));
    }

    /// Ends the current node and changes the kind.
    /// This is useful when the exact kind cannot be determined a priori (resp. this is hard).
    /// For instance, when parsing a type, the following production
    /// ```vhdl
    /// type foo
    /// ```
    /// could either be an incomplete type:
    /// ```vhdl
    /// type foo;
    /// ```
    /// or a regular type indication:
    /// ```vhdl
    /// type foo is ...
    /// ```
    pub fn end_node_with_kind(&mut self, kind: NodeKind) {
        let (_, first_child) = self.parents.pop().unwrap();
        let mut data = GreenNodeData::new(kind);
        data.push_children(self.children.drain(first_child..));
        self.children
            .push(GreenChild::Node((0, GreenNode::new(data))));
    }

    pub fn end(mut self) -> GreenNode {
        assert_eq!(self.children.len(), 1);
        match self.children.pop().unwrap() {
            GreenChild::Node((_, node)) => node,
            GreenChild::Token(_) => panic!(),
        }
    }

    pub fn checkpoint(&self) -> Checkpoint {
        self.children.len()
    }

    pub fn start_node_at(&mut self, checkpoint: Checkpoint, kind: NodeKind) {
        assert!(
            checkpoint <= self.children.len(),
            "checkpoint no longer valid, was finish_node called early?"
        );

        if let Some(&(_, first_child)) = self.parents.last() {
            assert!(
                checkpoint >= first_child,
                "checkpoint no longer valid, was an unmatched start_node_at called?"
            );
        }

        self.parents.push((kind, checkpoint));
    }

    pub fn current_pos(&self) -> usize {
        self.text_len
    }

    pub fn current_token_index(&self) -> usize {
        self.token_index
    }
}
