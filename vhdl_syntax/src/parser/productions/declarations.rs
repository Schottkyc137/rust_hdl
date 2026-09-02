// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::util::{choice_options, StallGuard};
use crate::parser::Parser;
use crate::syntax::meta::Layout;
use crate::syntax::NodeKind::{self, *};
use crate::tokens::TokenKind::*;
use crate::tokens::{Keyword as Kw, TokenKind};

pub(crate) fn is_start_of_declarative_part(token_kind: TokenKind) -> bool {
    matches!(
        token_kind,
        Keyword(
            Kw::Use
                | Kw::Type
                | Kw::Subtype
                | Kw::Shared
                | Kw::Constant
                | Kw::Signal
                | Kw::Variable
                | Kw::File
                | Kw::Component
                | Kw::Attribute
                | Kw::Alias
                | Kw::Impure
                | Kw::Pure
                | Kw::Function
                | Kw::Procedure
                | Kw::Package
                | Kw::For
                | Kw::View
                | Kw::Begin
        )
    )
}

impl Parser {
    pub(crate) fn declarations(&mut self, node_kind: NodeKind, layout: &Layout) {
        let allowed_nodes = choice_options(layout);
        self.node(node_kind, |p| {
            let mut guard = StallGuard::new();
            while guard.should_continue(p) {
                match p.peek_token() {
                    Keyword(Kw::Begin | Kw::End) | Eof => break,
                    Keyword(Kw::Type) => p.type_declaration(),
                    Keyword(Kw::Subtype) => p.subtype_declaration(),
                    Keyword(Kw::Component) => p.component_declaration(),
                    Keyword(Kw::Impure | Kw::Pure | Kw::Function | Kw::Procedure) => {
                        // TODO: Brittle
                        if p.next_nth_is(Keyword(Kw::New), 3) {
                            p.subprogram_instantiation_declaration();
                        } else {
                            p.subprogram_declaration_or_body()
                        }
                    }
                    Keyword(Kw::Package) => p.package_instantiation_declaration(),
                    Keyword(Kw::For) => p.configuration_specification(),
                    Keyword(Kw::File) => p.file_declaration(),
                    Keyword(Kw::Shared | Kw::Variable) => p.variable_declaration(),
                    Keyword(Kw::Constant) => p.constant_declaration(),
                    Keyword(Kw::Signal) => p.signal_declaration(),
                    Keyword(Kw::Attribute) => p.attribute_declaration_or_specification(),
                    Keyword(Kw::Use) => p.use_clause_declaration(),
                    Keyword(Kw::Alias) => p.alias_declaration(),
                    _ => {
                        p.expect_tokens_recover([
                            Keyword(Kw::Type),
                            Keyword(Kw::Subtype),
                            Keyword(Kw::Component),
                            Keyword(Kw::Impure),
                            Keyword(Kw::Pure),
                            Keyword(Kw::Function),
                            Keyword(Kw::Procedure),
                            Keyword(Kw::Package),
                            Keyword(Kw::For),
                            Keyword(Kw::File),
                            Keyword(Kw::Shared),
                            Keyword(Kw::Variable),
                            Keyword(Kw::Constant),
                            Keyword(Kw::Signal),
                            Keyword(Kw::Attribute),
                            Keyword(Kw::Use),
                            Keyword(Kw::Alias),
                        ]);
                        continue;
                    }
                }
                p.check_last_node_is_allowed(allowed_nodes);
            }
        });
    }

    pub fn use_clause_declaration(&mut self) {
        self.node(UseClauseDeclaration, |p| {
            p.use_clause();
        });
    }

    pub fn package_declaration(&mut self) {
        self.node(PackageDeclarationItem, |p| {
            p.package();
        });
    }

    pub fn configuration_specification(&mut self) {
        let unknown = self.start_unknown();
        self.node(ComponentConfigurationPreamble, |p| {
            p.expect_kw(Kw::For);
            p.component_specification();
        });
        self.binding_indication();
        self.expect_token(SemiColon);
        if self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
            let marker = unknown.resolve(self, NodeKind::CompoundConfigurationSpecification);
            while self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
                self.node(VerificationUnitBinding, |p| {
                    p.verification_unit_binding_indication();
                    p.expect_token(SemiColon);
                });
            }
            self.component_configuration_epilogue();
            marker.complete(self);
        } else {
            let marker = unknown.resolve(self, NodeKind::SimpleConfigurationSpecification);
            if self.next_is(Keyword(Kw::End)) {
                self.component_configuration_epilogue();
            }
            marker.complete(self);
        }
    }

    pub fn component_specification(&mut self) {
        self.node(NodeKind::ComponentSpecification, |p| {
            match_next_token!(p,
                Keyword(Kw::All) => {
                    p.skip_into_node(NodeKind::InstantiationListAll);
                },
                Keyword(Kw::Others) => {
                    p.skip_into_node(NodeKind::InstantiationListOthers);
                },
                Identifier => {
                    p.node(NodeKind::InstantiationListList, |p| {
                        p.skip();
                        while p.next_is(Comma) {
                            p.skip();
                            p.identifier();
                        }
                    });
                }
            );
            p.expect_token(Colon);
            p.name();
        });
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::to_test_text, Parser};

    #[test]
    fn package_instantiation() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_instantiation_declaration,
            "package ident is new lib.foo.bar;",
        ));
    }

    #[test]
    fn package_instantiation_generic_map() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_instantiation_declaration,
            "\
package ident is new lib.foo.bar
  generic map (
    foo => bar
  );",
        ));
    }
}
