// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

use std::marker::PhantomData;

use crate::syntax::green::{GreenChild, GreenNode, GreenNodeData, GreenToken};
use crate::syntax::meta::Layout;
use crate::syntax::node::SyntaxNode;
use crate::syntax::AstNode;
use crate::tokens::Token;

pub struct RawNodeBuilder<T> {
    data: GreenNodeData,
    _marker: PhantomData<T>,
}

impl<T> RawNodeBuilder<T> {
    pub fn push_node<N: AstNode>(&mut self, node: N) {
        self.push(GreenChild::Node(node.raw().green().clone()));
    }

    pub fn push_token(&mut self, token: Token) {
        self.push(GreenChild::Token(GreenToken::new(token)));
    }

    fn push(&mut self, child: GreenChild) {
        self.data.push(child);
    }

    pub fn finish_untyped(self) -> SyntaxNode {
        assert!(!self.data.is_empty(), "Cannot build empty nodes");
        SyntaxNode::new_root(GreenNode::new(self.data))
    }
}

#[cfg(test)]
impl RawNodeBuilder<()> {
    pub fn new_untyped(kind: crate::syntax::NodeKind) -> RawNodeBuilder<()> {
        RawNodeBuilder {
            data: GreenNodeData::new(kind),
            _marker: PhantomData,
        }
    }
}

impl<T: AstNode> RawNodeBuilder<T> {
    pub fn new() -> RawNodeBuilder<T> {
        match T::META {
            Layout::Sequence(sequence) => RawNodeBuilder {
                data: GreenNodeData::new(sequence.kind),
                _marker: PhantomData,
            },
            Layout::List(list) => RawNodeBuilder {
                data: GreenNodeData::new(list.kind),
                _marker: PhantomData,
            },
            Layout::Choice(_) => unreachable!("Choice nodes cannot be built"),
        }
    }

    pub fn finish(self) -> T {
        T::cast_unchecked(self.finish_untyped())
    }
}
