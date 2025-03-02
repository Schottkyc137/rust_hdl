// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn type_declaration(&mut self) {
        self.start_node(TypeDeclaration);
        self.expect_kw(Kw::Type);
        self.identifier();
        if self.opt_token(SemiColon) {
            self.end_node_with_kind(IncompleteTypeDeclaration);
            return;
        }
        self.expect_kw(Kw::Is);
        self.type_definition();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn type_definition(&mut self) {
        match_next_token!(self,
            Keyword(Kw::Range) => {
                self.start_node(NumericTypeDefinition);
                self.range();
                if self.opt_token(Keyword(Kw::Units)) {
                    self.primary_unit_declaration();
                    while self.peek_token().is_some_and(|tok| tok != Keyword(Kw::End)) {
                        self.secondary_unit_declaration()
                    }
                    self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Units)]);
                    self.opt_identifier();
                    self.end_node_with_kind(PhysicalTypeDefinition);
                } else {
                    self.end_node();
                }
            },
            Keyword(Kw::Access) => {
                self.start_node(AccessTypeDefinition);
                self.skip();
                self.subtype_indication();
                self.end_node();
            },
            Keyword(Kw::Protected) => self.protected_type_definition(),
            Keyword(Kw::File) => self.file_type_definition(),
            Keyword(Kw::Array) => self.array_type_definitions(),
            Keyword(Kw::Record) => self.record_type_definition(),
            LeftPar => self.enumeration_type_definition()
        )
    }

    pub fn enumeration_type_definition(&mut self) {
        self.start_node(EnumerationTypeDeclaration);
        self.expect_token(LeftPar);
        self.separated_list(Parser::enumeration_literal, Comma);
        self.expect_token(RightPar);
        self.end_node();
    }

    pub fn enumeration_literal(&mut self) {
        self.expect_one_of_tokens([Identifier, CharacterLiteral])
    }

    pub fn protected_type_definition(&mut self) {
        self.start_node(ProtectedTypeDeclaration);
        self.expect_kw(Kw::Protected);
        let is_body = self.opt_token(Keyword(Kw::Body));
        self.declarative_part();
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Protected)]);
        if is_body {
            self.expect_token(Keyword(Kw::Body));
        }
        self.opt_identifier();
        if is_body {
            self.end_node_with_kind(ProtectedBody);
        } else {
            self.end_node();
        }
    }

    pub fn record_type_definition(&mut self) {
        self.start_node(ProtectedTypeDeclaration);
        self.expect_kw(Kw::Record);
        while let Some(tok) = self.peek_token() {
            match tok {
                Keyword(Kw::End) => break,
                _ => self.element_declaration(),
            }
        }
        self.expect_tokens([Keyword(Kw::End), Keyword(Kw::Record)]);
        self.opt_identifier();
        self.end_node();
    }

    pub fn element_declaration(&mut self) {
        self.start_node(ElementDeclaration);
        self.identifier_list();
        self.expect_token(Colon);
        self.element_subtype_definition();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn element_subtype_definition(&mut self) {
        self.subtype_indication();
    }

    pub fn file_type_definition(&mut self) {
        self.start_node(FileTypeDefinition);
        self.expect_tokens([Keyword(Kw::File), Keyword(Kw::Of)]);
        self.type_mark();
        self.end_node();
    }

    pub fn array_type_definitions(&mut self) {
        self.start_node(UnboundedArrayDefinition);
        self.expect_kw(Kw::Array);
        // TODO: bounded vs unbounded array type definition
        self.expect_token(LeftPar);
        self.separated_list(Parser::index_subtype_definition, Comma);
        self.expect_token(RightPar);
        self.expect_kw(Kw::Of);
        self.subtype_indication();
        self.end_node();
    }

    pub fn index_subtype_definition(&mut self) {
        self.start_node(IndexSubtypeDefinition);
        self.type_mark();
        self.expect_tokens([Keyword(Kw::Range), BOX]);
        self.end_node();
    }

    pub fn primary_unit_declaration(&mut self) {
        self.start_node(PrimaryUnitDeclaration);
        self.identifier();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn secondary_unit_declaration(&mut self) {
        self.start_node(SecondaryUnitDeclaration);
        self.identifier();
        self.expect_token(EQ);
        self.physical_literal();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn physical_literal(&mut self) {
        self.opt_token(AbstractLiteral);
        self.name();
    }

    pub fn subtype_declaration(&mut self) {
        self.start_node(SubtypeDeclaration);
        self.expect_kw(Kw::Subtype);
        self.identifier();
        self.expect_kw(Kw::Is);
        self.subtype_indication();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn discrete_range(&mut self) {
        // TODO: This is likely not all there is to it.
        // discrete_range ::= range | subtype_indication
        self.range();
    }
}
