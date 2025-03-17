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
pub struct AliasDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for AliasDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AliasDeclaration => Some(AliasDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AliasDeclarationSyntax {
    pub fn alias_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Alias))
    }
    pub fn designator(&self) -> Option<DesignatorSyntax> {
        self.0.tokens().find_map(DesignatorSyntax::cast)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == Colon)
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0.children().find_map(SubtypeIndicationSyntax::cast)
    }
    pub fn is_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Is))
    }
    pub fn name(&self) -> Option<NameSyntax> {
        self.0.children().find_map(NameSyntax::cast)
    }
    pub fn signature(&self) -> Option<SignatureSyntax> {
        self.0.children().find_map(SignatureSyntax::cast)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == SemiColon)
    }
}
