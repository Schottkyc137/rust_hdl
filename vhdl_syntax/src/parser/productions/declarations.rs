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
                | Kw::Group
                | Kw::Disconnect
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
        self.start_node(node_kind);
        let mut guard = StallGuard::new();
        while guard.should_continue(self) {
            let start = self.builder.current_pos();
            match self.peek_token() {
                Keyword(Kw::Begin | Kw::End) | Eof => break,
                Keyword(Kw::Type) => self.type_declaration(),
                Keyword(Kw::Subtype) => self.subtype_declaration(),
                Keyword(Kw::Component) => self.component_declaration(),
                Keyword(Kw::Impure | Kw::Pure | Kw::Function | Kw::Procedure) => {
                    // TODO: Brittle
                    if self.next_nth_is(Keyword(Kw::New), 3) {
                        self.subprogram_instantiation_declaration();
                    } else {
                        self.subprogram_declaration_or_body()
                    }
                }
                Keyword(Kw::Package) => self.package_declarative_item(),
                Keyword(Kw::For) => self.configuration_specification(),
                Keyword(Kw::File) => self.file_declaration(),
                Keyword(Kw::Shared | Kw::Variable) => self.variable_declaration(),
                Keyword(Kw::Constant) => self.constant_declaration(),
                Keyword(Kw::Signal) => self.signal_declaration(),
                Keyword(Kw::Attribute) => self.attribute_declaration_or_specification(),
                Keyword(Kw::Use) => self.use_clause_declaration(),
                Keyword(Kw::Alias) => self.alias_declaration(),
                Keyword(Kw::Group) => self.group_declaration_or_template_declaration(),
                Keyword(Kw::Disconnect) => self.disconnection_specification(),
                _ => {
                    self.expect_tokens_recover([
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
                        Keyword(Kw::Group),
                        Keyword(Kw::Disconnect),
                    ]);
                    continue;
                }
            }
            self.check_last_node_is_allowed(start, allowed_nodes);
        }
        self.end_node();
    }

    pub fn use_clause_declaration(&mut self) {
        self.start_node(UseClauseDeclaration);
        self.use_clause();
        self.end_node();
    }

    pub fn package_declarative_item(&mut self) {
        if self.next_nth_is(Keyword(Kw::Body), 1) {
            self.package_body_declaration();
        } else if self.next_nth_is(Keyword(Kw::New), 3) {
            self.package_instantiation_declaration();
        } else {
            self.package_declaration();
        }
    }

    pub fn package_declaration(&mut self) {
        self.start_node(PackageDeclarationItem);
        self.package();
        self.end_node();
    }

    pub fn package_body_declaration(&mut self) {
        self.start_node(PackageBodyDeclaration);
        self.package_body();
        self.end_node();
    }

    pub fn disconnection_specification(&mut self) {
        self.start_node(DisconnectionSpecification);
        self.expect_kw(Kw::Disconnect);
        self.guarded_signal_specification();
        self.expect_kw(Kw::After);
        self.expression();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn guarded_signal_specification(&mut self) {
        self.start_node(GuardedSignalSpecification);
        self.signal_list();
        self.expect_token(Colon);
        self.type_mark();
        self.end_node();
    }

    pub fn signal_list(&mut self) {
        match_next_token!(self,
            Keyword(Kw::All) => {
                self.skip_into_node(SignalListAll);
            },
            Keyword(Kw::Others) => {
                self.skip_into_node(SignalListOthers);
            },
            Identifier => {
                self.separated_list(SignalListList, Parser::name, Comma);
            }
        );
    }

    pub fn configuration_specification(&mut self) {
        let checkpoint = self.checkpoint();
        self.start_node(ComponentConfigurationPreamble);
        self.expect_kw(Kw::For);
        self.component_specification();
        self.end_node();
        self.binding_indication();
        self.expect_token(SemiColon);
        if self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
            self.start_node_at(checkpoint, NodeKind::CompoundConfigurationSpecification);
            while self.next_is(Keyword(Kw::Use)) && self.next_nth_is(Keyword(Kw::Vunit), 1) {
                self.start_node(VerificationUnitBinding);
                self.verification_unit_binding_indication();
                self.expect_token(SemiColon);
                self.end_node();
            }
            self.component_configuration_epilogue();
            self.end_node();
        } else {
            self.start_node_at(checkpoint, NodeKind::SimpleConfigurationSpecification);
            if self.next_is(Keyword(Kw::End)) {
                self.component_configuration_epilogue();
            }
            self.end_node();
        }
    }

    pub fn component_specification(&mut self) {
        self.start_node(NodeKind::ComponentSpecification);
        match_next_token!(self,
            Keyword(Kw::All) => {
                self.skip_into_node(NodeKind::InstantiationListAll);
            },
            Keyword(Kw::Others) => {
                self.skip_into_node(NodeKind::InstantiationListOthers);
            },
            Identifier => {
                self.start_node(NodeKind::InstantiationListList);
                self.skip();
                while self.next_is(Comma) {
                    self.skip();
                    self.identifier();
                }
                self.end_node();
            }
        );
        self.expect_token(Colon);
        self.name();
        self.end_node();
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

    #[test]
    fn simple_disconnection_specification() {
        insta::assert_snapshot!(to_test_text(
            Parser::disconnection_specification,
            "disconnect s : bit after 10 ns;",
        ));
    }

    #[test]
    fn disconnection_specification_with_multiple_signals() {
        insta::assert_snapshot!(to_test_text(
            Parser::disconnection_specification,
            "disconnect s1, s2 : work.pkg.bit_t after 10 ns;",
        ));
    }

    #[test]
    fn disconnection_specification_others() {
        insta::assert_snapshot!(to_test_text(
            Parser::disconnection_specification,
            "disconnect others : bit after 10 ns;",
        ));
    }

    #[test]
    fn disconnection_specification_all() {
        insta::assert_snapshot!(to_test_text(
            Parser::disconnection_specification,
            "disconnect all : bit after 10 ns;",
        ));
    }

    #[test]
    fn nested_package_declaration() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_declarative_item,
            "package pkg is end package;",
        ));
    }

    #[test]
    fn nested_package_body() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_declarative_item,
            "package body pkg is end package body;",
        ));
    }

    #[test]
    fn nested_package_instantiation() {
        insta::assert_snapshot!(to_test_text(
            Parser::package_declarative_item,
            "package pkg is new work.gen_pkg generic map (x => y);",
        ));
    }

    #[test]
    fn disconnection_specification_is_not_allowed_in_a_package_body() {
        assert_recovery_snapshot!(
            "\
package body pkg is
  disconnect s : bit after 10 ns;
end package body;",
            Parser::package_body
        );
    }

    #[test]
    fn package_body_is_not_allowed_in_a_package_declaration() {
        assert_recovery_snapshot!(
            "\
package pkg is
  package body inner is
  end package body;
end package;",
            Parser::package
        );
    }
}
