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
#[derive(Debug, Clone)]
pub struct EntityDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(EntityDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityDeclarationSyntax {
    pub fn entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Entity))
    }
    pub fn identifier_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == Identifier)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Is))
    }
    pub fn header(&self) -> Option<EntityHeaderSyntax> {
        self.0.children().find_map(EntityHeaderSyntax::cast)
    }
    pub fn begin_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Begin))
    }
    pub fn end_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::End))
    }
    pub fn final_entity_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Keyword(Kw::Entity))
            .nth(1)
    }
    pub fn final_identifier_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .filter(|token| token.kind() == Identifier)
            .nth(1)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == SemiColon)
    }
}
#[derive(Debug, Clone)]
pub struct EntityHeaderSyntax(pub(crate) SyntaxNode);
impl AstNode for EntityHeaderSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityHeader => Some(EntityHeaderSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl EntityHeaderSyntax {
    pub fn generic_clause(&self) -> Option<GenericClauseSyntax> {
        self.0.children().find_map(GenericClauseSyntax::cast)
    }
    pub fn port_clause(&self) -> Option<PortClauseSyntax> {
        self.0.children().find_map(PortClauseSyntax::cast)
    }
}
