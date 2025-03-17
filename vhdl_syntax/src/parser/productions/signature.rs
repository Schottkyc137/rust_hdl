// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::token_kind::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn signature(&mut self) {
        // LRM §4.5.3
        // signature ::= `[` [ name { `,` name } ] [ `return` name ] `]`;
        self.start_node(Signature);
        self.expect_token(LeftSquare);

        if !self.next_is_one_of([Keyword(Kw::Return), RightSquare]) {
            self.name_list();
        }

        if self.opt_token(Keyword(Kw::Return)) {
            self.name();
        }

        self.expect_token(RightSquare);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::{test_utils::check, Parser};

    #[test]
    fn parse_signature() {
        check(
            Parser::signature,
            "[natural, bit return unsigned]",
            "\
Signature
  LeftSquare
  NameList
    Name
      Identifier 'natural'
    Comma
    Name
      Identifier 'bit'
  Keyword(Return)
  Name
    Identifier 'unsigned'
  RightSquare
",
        );

        check(
            Parser::signature,
            "[]",
            "\
Signature
  LeftSquare
  RightSquare
",
        );

        check(
            Parser::signature,
            "[return ret_t]",
            "\
Signature
  LeftSquare
  Keyword(Return)
  Name
    Identifier 'ret_t'
  RightSquare
",
        );

        check(
            Parser::signature,
            "[arg1_t, arg2_t]",
            "\
Signature
  LeftSquare
  NameList
    Name
      Identifier 'arg1_t'
    Comma
    Name
      Identifier 'arg2_t'
  RightSquare
",
        );

        check(
            Parser::signature,
            "[arg1_t]",
            "\
Signature
  LeftSquare
  NameList
    Name
      Identifier 'arg1_t'
  RightSquare
",
        );
    }
}
