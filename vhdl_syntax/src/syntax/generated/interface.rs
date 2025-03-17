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
pub struct GenericClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for GenericClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::GenericClause => Some(GenericClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl GenericClauseSyntax {
    pub fn generic_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Generic))
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == LeftPar)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0.children().find_map(InterfaceListSyntax::cast)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == RightPar)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == SemiColon)
    }
}
#[derive(Debug, Clone)]
pub struct PortClauseSyntax(pub(crate) SyntaxNode);
impl AstNode for PortClauseSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::PortClause => Some(PortClauseSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl PortClauseSyntax {
    pub fn port_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Port))
    }
    pub fn left_par_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == LeftPar)
    }
    pub fn interface_list(&self) -> Option<InterfaceListSyntax> {
        self.0.children().find_map(InterfaceListSyntax::cast)
    }
    pub fn right_par_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == RightPar)
    }
    pub fn semi_colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == SemiColon)
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceListSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceListSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceList => Some(InterfaceListSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceListSyntax {
    pub fn interface_declarations(
        &self,
    ) -> impl Iterator<Item = InterfaceDeclarationSyntax> + use<'_> {
        self.0
            .children()
            .filter_map(InterfaceDeclarationSyntax::cast)
    }
    pub fn semi_colon_tokens(&self) -> impl Iterator<Item = SyntaxToken> + use<'_> {
        self.0.tokens().filter(|tok| tok.kind() == SemiColon)
    }
}
pub enum InterfaceDeclarationSyntax {
    Object(InterfaceObjectDeclarationSyntax),
}
impl AstNode for InterfaceDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceObjectDeclaration => Some(InterfaceDeclarationSyntax::Object(
                InterfaceObjectDeclarationSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            InterfaceDeclarationSyntax::Object(inner) => inner.raw(),
        }
    }
}
#[derive(Debug, Clone)]
pub struct InterfaceObjectDeclarationSyntax(pub(crate) SyntaxNode);
impl AstNode for InterfaceObjectDeclarationSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::InterfaceObjectDeclaration => Some(InterfaceObjectDeclarationSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl InterfaceObjectDeclarationSyntax {
    pub fn interface_class(&self) -> Option<InterfaceClassSyntax> {
        self.0.tokens().find_map(InterfaceClassSyntax::cast)
    }
    pub fn identifier_list(&self) -> Option<IdentifierListSyntax> {
        self.0.children().find_map(IdentifierListSyntax::cast)
    }
    pub fn colon_token(&self) -> Option<SyntaxToken> {
        self.0.tokens().find(|token| token.kind() == Colon)
    }
    pub fn mode(&self) -> Option<ModeSyntax> {
        self.0.tokens().find_map(ModeSyntax::cast)
    }
    pub fn bus_token(&self) -> Option<SyntaxToken> {
        self.0
            .tokens()
            .find(|token| token.kind() == Keyword(Kw::Bus))
    }
}
pub enum InterfaceClassSyntax {
    Constant(SyntaxToken),
    Signal(SyntaxToken),
    Variable(SyntaxToken),
}
impl InterfaceClassSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::Constant) => Some(InterfaceClassSyntax::Constant(token)),
            Keyword(Kw::Signal) => Some(InterfaceClassSyntax::Signal(token)),
            Keyword(Kw::Variable) => Some(InterfaceClassSyntax::Variable(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            InterfaceClassSyntax::Constant(inner) => inner.clone(),
            InterfaceClassSyntax::Signal(inner) => inner.clone(),
            InterfaceClassSyntax::Variable(inner) => inner.clone(),
        }
    }
}
pub enum ModeSyntax {
    In(SyntaxToken),
    Out(SyntaxToken),
    Inout(SyntaxToken),
    Buffer(SyntaxToken),
    Linkage(SyntaxToken),
}
impl ModeSyntax {
    pub fn cast(token: SyntaxToken) -> Option<Self> {
        match token.kind() {
            Keyword(Kw::In) => Some(ModeSyntax::In(token)),
            Keyword(Kw::Out) => Some(ModeSyntax::Out(token)),
            Keyword(Kw::Inout) => Some(ModeSyntax::Inout(token)),
            Keyword(Kw::Buffer) => Some(ModeSyntax::Buffer(token)),
            Keyword(Kw::Linkage) => Some(ModeSyntax::Linkage(token)),
            _ => None,
        }
    }
    pub fn raw(&self) -> SyntaxToken {
        match self {
            ModeSyntax::In(inner) => inner.clone(),
            ModeSyntax::Out(inner) => inner.clone(),
            ModeSyntax::Inout(inner) => inner.clone(),
            ModeSyntax::Buffer(inner) => inner.clone(),
            ModeSyntax::Linkage(inner) => inner.clone(),
        }
    }
}
