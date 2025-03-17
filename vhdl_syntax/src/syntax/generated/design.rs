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
pub struct DesignFileSyntax(pub(crate) SyntaxNode);
impl AstNode for DesignFileSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DesignFile => Some(DesignFileSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DesignFileSyntax {
    pub fn design_units(&self) -> impl Iterator<Item = DesignUnitSyntax> + use<'_> {
        self.0.children().filter_map(DesignUnitSyntax::cast)
    }
}
#[derive(Debug, Clone)]
pub struct DesignUnitSyntax(pub(crate) SyntaxNode);
impl AstNode for DesignUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::DesignUnit => Some(DesignUnitSyntax(node)),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        self.0.clone()
    }
}
impl DesignUnitSyntax {
    pub fn context_clause(&self) -> Option<ContextClauseSyntax> {
        self.0.children().find_map(ContextClauseSyntax::cast)
    }
    pub fn library_units(&self) -> impl Iterator<Item = LibraryUnitSyntax> + use<'_> {
        self.0.children().filter_map(LibraryUnitSyntax::cast)
    }
}
pub enum LibraryUnitSyntax {
    Primary(PrimaryUnitSyntax),
}
impl AstNode for LibraryUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(LibraryUnitSyntax::Primary(
                PrimaryUnitSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            LibraryUnitSyntax::Primary(inner) => inner.raw(),
        }
    }
}
pub enum PrimaryUnitSyntax {
    Entity(EntityDeclarationSyntax),
}
impl AstNode for PrimaryUnitSyntax {
    fn cast(node: SyntaxNode) -> Option<Self> {
        match node.kind() {
            NodeKind::EntityDeclaration => Some(PrimaryUnitSyntax::Entity(
                EntityDeclarationSyntax::cast(node).unwrap(),
            )),
            _ => None,
        }
    }
    fn raw(&self) -> SyntaxNode {
        match self {
            PrimaryUnitSyntax::Entity(inner) => inner.raw(),
        }
    }
}
pub enum ContextClauseSyntax {}
impl AstNode for ContextClauseSyntax {
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
