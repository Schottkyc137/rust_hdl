// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::{Keyword, SemiColon};
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    fn any_sequential_statement(
        &mut self,
        kind: NodeKind,
        statement_inner: impl FnOnce(&mut Parser<T>),
    ) {
        self.start_node(kind);
        self.opt_label();
        statement_inner(self);
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn wait_statement(&mut self) {
        self.any_sequential_statement(WaitStatement, |parser| {
            parser.expect_kw(Kw::Wait);
            if parser.next_is(Keyword(Kw::On)) {
                parser.start_node(SensitivityClause);
                parser.skip();
                parser.name_list();
                parser.end_node();
            }
            if parser.next_is(Keyword(Kw::Until)) {
                parser.start_node(ConditionClause);
                parser.skip();
                parser.expression();
                parser.end_node();
            }
            if parser.next_is(Keyword(Kw::For)) {
                parser.start_node(TimeoutClause);
                parser.skip();
                parser.expression();
                parser.end_node();
            }
        });
    }

    pub fn assert_statement(&mut self) {
        self.any_sequential_statement(AssertStatement, |parser| {
            parser.expect_kw(Kw::Assert);
            parser.condition();
            if parser.next_is(Keyword(Kw::Report)) {
                parser.start_node(ReportExpression);
                parser.skip();
                parser.expression();
                parser.end_node();
            }
            if parser.next_is(Keyword(Kw::Severity)) {
                parser.start_node(SeverityExpression);
                parser.skip();
                parser.expression();
                parser.end_node();
            }
        });
    }

    pub fn report_statement(&mut self) {
        self.any_sequential_statement(ReportStatement, |parser| {
            parser.expect_kw(Kw::Report);
            parser.expression();
            if parser.next_is(Keyword(Kw::Severity)) {
                parser.start_node(SeverityExpression);
                parser.skip();
                parser.expression();
                parser.end_node();
            }
        });
    }

    fn next_or_exit_statement(&mut self, kw: crate::tokens::token_kind::Keyword, kind: NodeKind) {
        self.any_sequential_statement(kind, |parser| {
            parser.expect_kw(kw);
            parser.opt_identifier();
            if parser.next_is(Keyword(Kw::When)) {
                parser.start_node(WhenExpression);
                parser.skip();
                parser.expression();
                parser.end_node();
            }
        });
    }

    pub fn next_statement(&mut self) {
        self.next_or_exit_statement(Kw::Next, NextStatement);
    }

    pub fn exit_statement(&mut self) {
        self.next_or_exit_statement(Kw::Exit, ExitStatement);
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::check;
    use crate::parser::Parser;

    #[test]
    fn simple_wait_statement() {
        check(
            Parser::wait_statement,
            "wait;",
            "\
WaitStatement
  Keyword(Wait)
  SemiColon
        ",
        );
    }

    #[test]
    fn simple_wait_statement_with_label() {
        check(
            Parser::wait_statement,
            "foo: wait;",
            "\
WaitStatement
  Label
    Identifier 'foo'
    Colon
  Keyword(Wait)
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_sensitivity_list() {
        check(
            Parser::wait_statement,
            "wait on foo, bar;",
            "\
WaitStatement
  Keyword(Wait)
  SensitivityClause
    Keyword(On)
    NameList
      Name
        Identifier 'foo'
      Comma
      Name
        Identifier 'bar'
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_condition() {
        check(
            Parser::wait_statement,
            "wait until a = b;",
            "\
WaitStatement
  Keyword(Wait)
  ConditionClause
    Keyword(Until)
    BinaryExpression
      Name
        Identifier 'a'
      EQ
      Name
        Identifier 'b'
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_timeout() {
        check(
            Parser::wait_statement,
            "wait for 2 ns;",
            "\
WaitStatement
  Keyword(Wait)
  TimeoutClause
    Keyword(For)
    PhysicalLiteral
      AbstractLiteral '2'
      Name
        Identifier 'ns'
  SemiColon
        ",
        );
    }

    #[test]
    fn wait_statement_with_all_parts() {
        check(
            Parser::wait_statement,
            "wait on foo until bar for 2 ns;",
            "\
WaitStatement
  Keyword(Wait)
  SensitivityClause
    Keyword(On)
    NameList
      Name
        Identifier 'foo'
  ConditionClause
    Keyword(Until)
    Name
      Identifier 'bar'
  TimeoutClause
    Keyword(For)
    PhysicalLiteral
      AbstractLiteral '2'
      Name
        Identifier 'ns'
  SemiColon
        ",
        );
    }

    #[test]
    fn simple_assert() {
        check(
            Parser::assert_statement,
            "assert false;",
            "\
AssertStatement
  Keyword(Assert)
  Name
    Identifier 'false'
  SemiColon",
        )
    }

    #[test]
    fn full_assert() {
        check(
            Parser::assert_statement,
            "assert false report \"message\" severity error;",
            "\
AssertStatement
  Keyword(Assert)
  Name
    Identifier 'false'
  ReportExpression
    Keyword(Report)
    Literal
      StringLiteral '\"message\"'
  SeverityExpression
    Keyword(Severity)
    Name
      Identifier 'error'
  SemiColon",
        )
    }

    #[test]
    fn report_statement() {
        check(
            Parser::report_statement,
            "report \"message\" severity error;",
            "\
ReportStatement
  Keyword(Report)
  Literal
    StringLiteral '\"message\"'
  SeverityExpression
    Keyword(Severity)
    Name
      Identifier 'error'
  SemiColon",
        )
    }

    #[test]
    fn next_statement() {
        check(
            Parser::next_statement,
            "next;",
            "\
NextStatement
  Keyword(Next)
  SemiColon
        ",
        );
    }

    #[test]
    fn next_statement_loop_label() {
        check(
            Parser::next_statement,
            "next foo;",
            "\
NextStatement
  Keyword(Next)
  Identifier 'foo'
  SemiColon
        ",
        );
    }

    #[test]
    fn next_statement_condition() {
        check(
            Parser::next_statement,
            "next when condition;",
            "\
NextStatement
  Keyword(Next)
  WhenExpression
    Keyword(When)
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn next_statement_loop_label_condition() {
        check(
            Parser::next_statement,
            "next foo when condition;",
            "\
NextStatement
  Keyword(Next)
  Identifier 'foo'
  WhenExpression
    Keyword(When)
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement() {
        check(
            Parser::exit_statement,
            "exit;",
            "\
ExitStatement
  Keyword(Exit)
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement_loop_label() {
        check(
            Parser::exit_statement,
            "exit foo;",
            "\
ExitStatement
  Keyword(Exit)
  Identifier 'foo'
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement_condition() {
        check(
            Parser::exit_statement,
            "exit when condition;",
            "\
ExitStatement
  Keyword(Exit)
  WhenExpression
    Keyword(When)
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }

    #[test]
    fn exit_statement_loop_label_condition() {
        check(
            Parser::exit_statement,
            "exit foo when condition;",
            "\
ExitStatement
  Keyword(Exit)
  Identifier 'foo'
  WhenExpression
    Keyword(When)
    Name
      Identifier 'condition'
  SemiColon
        ",
        );
    }
}
