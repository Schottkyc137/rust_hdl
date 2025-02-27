// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::{Keyword, SemiColon};
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn wait_statement(&mut self) {
        self.start_node(WaitStatement);
        self.opt_label();
        self.expect_kw(Kw::Wait);
        if self.next_is(Keyword(Kw::On)) {
            self.start_node(SensitivityClause);
            self.skip();
            self.name_list();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Until)) {
            self.start_node(ConditionClause);
            self.skip();
            self.expression();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::For)) {
            self.start_node(TimeoutClause);
            self.skip();
            self.expression();
            self.end_node();
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn assert_statement(&mut self) {
        self.start_node(AssertStatement);
        self.opt_label();
        self.expect_kw(Kw::Assert);
        self.condition();
        if self.next_is(Keyword(Kw::Report)) {
            self.start_node(ReportExpression);
            self.skip();
            self.expression();
            self.end_node();
        }
        if self.next_is(Keyword(Kw::Severity)) {
            self.start_node(SeverityExpression);
            self.skip();
            self.expression();
            self.end_node();
        }
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn report_statement(&mut self) {
        self.start_node(ReportStatement);
        self.opt_label();
        self.expect_kw(Kw::Report);
        self.expression();
        if self.next_is(Keyword(Kw::Severity)) {
            self.start_node(SeverityExpression);
            self.skip();
            self.expression();
            self.end_node();
        }
        self.expect_token(SemiColon);
        self.end_node();
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
}
