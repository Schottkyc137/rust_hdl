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
pub struct AccessTypeDefinitionSyntax(pub(crate) SyntaxNode);
impl AstNode for AccessTypeDefinitionSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::AccessTypeDefinition => Some(AccessTypeDefinitionSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl AccessTypeDefinitionSyntax {
    pub fn access_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Access))
    }
    pub fn subtype_indication(&self) -> Option<SubtypeIndicationSyntax> {
        self.0.children().find_map(SubtypeIndicationSyntax::cast)
    }
}
