// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::tokens::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub(crate) fn declarative_part(&mut self) {
        while let Some(token) = self.peek_token() {
            match token {
                Keyword(Kw::Begin | Kw::End) => break,
                Keyword(Kw::Type) => self.type_declaration(),
                Keyword(Kw::Subtype) => self.subtype_declaration(),
                Keyword(Kw::Component) => self.component_declaration(),
                Keyword(Kw::Impure | Kw::Pure | Kw::Function | Kw::Procedure) => {
                    self.subprogram_declaration_or_body()
                }
                Keyword(Kw::Package) => self.package_instantiation_declaration(),
                Keyword(Kw::For) => self.configuration_specification(),
                Keyword(Kw::File) => self.file_declaration(),
                Keyword(Kw::Shared | Kw::Variable) => self.variable_declaration(),
                Keyword(Kw::Constant) => self.constant_declaration(),
                Keyword(Kw::Signal) => self.signal_declaration(),
                Keyword(Kw::Attribute) => self.attribute(),
                Keyword(Kw::Use) => self.use_clause(),
                Keyword(Kw::Alias) => self.alias_declaration(),
                Keyword(Kw::View) => self.view_declaration(),
                _ => self.expect_tokens_err([Keyword(Kw::Type)]),
            }
        }
    }

    pub fn configuration_specification(&mut self) {
        todo!()
    }

    pub(crate) fn attribute(&mut self) {
        todo!()
    }

    pub fn view_declaration(&mut self) {
        todo!()
    }
}
