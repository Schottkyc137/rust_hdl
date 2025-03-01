// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
use crate::syntax::generated::*;
use crate::syntax::node::{SyntaxNode, SyntaxToken};
use crate::syntax::node_kind::NodeKind;
use crate::syntax::AstNode;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
pub enum DesignatorSyntax {
    Identifier(SyntaxToken),
    StringLiteral(SyntaxToken),
    CharacterLiteral(SyntaxToken),
}
impl DesignatorSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Identifier => Some(DesignatorSyntax::Identifier(token)),
            StringLiteral => Some(DesignatorSyntax::StringLiteral(token)),
            CharacterLiteral => Some(DesignatorSyntax::CharacterLiteral(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            DesignatorSyntax::Identifier(inner) => inner.clone(),
            DesignatorSyntax::StringLiteral(inner) => inner.clone(),
            DesignatorSyntax::CharacterLiteral(inner) => inner.clone(),
        }
    }
}
pub enum NameSyntax {}
impl AstNode for NameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            _ => unreachable!(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct AbsolutePathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for AbsolutePathnameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AbsolutePathname => Some(AbsolutePathnameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AbsolutePathnameSyntax {
    pub fn dot_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == Dot)
    }
    pub fn partial_pathname(&self) -> Option<PartialPathnameSyntax> {
        self.0.children().find_map(PartialPathnameSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct PartialPathnameSyntax(pub(crate) SyntaxNode);
impl AstNode for PartialPathnameSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PartialPathname => Some(PartialPathnameSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PartialPathnameSyntax {}
