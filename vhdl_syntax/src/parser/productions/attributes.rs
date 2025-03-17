// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::match_next_token;
use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::*;
use crate::tokens::token_kind::Keyword as Kw;
use crate::tokens::TokenKind::*;
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn attribute_specification(&mut self) {
        self.start_node(AttributeSpecification);
        self.expect_kw(Kw::Attribute);
        self.identifier();
        self.expect_token(Keyword(Kw::Of));
        self.entity_specification();
        self.expect_token(Keyword(Kw::Is));
        self.expression();
        self.expect_token(SemiColon);
        self.end_node();
    }

    pub fn entity_specification(&mut self) {
        self.start_node(EntitySpecification);
        self.entity_name_list();
        self.expect_token(Colon);
        self.entity_class();
        self.end_node();
    }

    pub fn entity_name_list(&mut self) {
        self.start_node(EntityNameList);
        match_next_token!(self,
            Keyword(Kw::All) => self.skip(),
            Keyword(Kw::Others) => self.skip(),
            Identifier, StringLiteral, CharacterLiteral => {
                self.start_node(DesignatorList);
                self.separated_list(Parser::entity_designator, Comma);
                self.end_node();
            }
        );
        self.end_node();
    }

    pub fn entity_class(&mut self) {
        self.expect_one_of_tokens([
            Keyword(Kw::Entity),
            Keyword(Kw::Architecture),
            Keyword(Kw::Configuration),
            Keyword(Kw::Procedure),
            Keyword(Kw::Function),
            Keyword(Kw::Package),
            Keyword(Kw::Type),
            Keyword(Kw::Subtype),
            Keyword(Kw::Constant),
            Keyword(Kw::Signal),
            Keyword(Kw::Variable),
            Keyword(Kw::Component),
            Keyword(Kw::Label),
            Keyword(Kw::Literal),
            Keyword(Kw::Units),
            Keyword(Kw::Group),
            Keyword(Kw::File),
            Keyword(Kw::Property),
            Keyword(Kw::Sequence),
        ])
    }

    pub fn entity_designator(&mut self) {
        self.start_node(EntityDesignator);
        self.entity_tag();
        if self.peek_token() == Some(LeftSquare) {
            self.signature();
        }
        self.end_node();
    }

    pub fn entity_tag(&mut self) {
        self.expect_one_of_tokens([Identifier, CharacterLiteral, StringLiteral])
    }

    pub(crate) fn attribute(&mut self) {
        self.start_node(AttributeDeclaration);
        self.expect_kw(Kw::Attribute);
        self.identifier();
        self.expect_token(Colon);
        self.type_mark();
        self.expect_token(SemiColon);
        self.end_node();
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::test_utils::*;
    use crate::parser::Parser;

    #[test]
    fn parse_simple_attribute_declaration() {
        check(
            Parser::attribute,
            "attribute foo : lib.name;",
            "\
AttributeDeclaration
  Keyword(Attribute)
  Identifier 'foo'
  Colon
  Name
    Identifier 'lib'
    SelectedName
      Dot
      Identifier 'name'
  SemiColon",
        );
    }

    #[test]
    fn parse_simple_attribute_specification() {
        check(
            Parser::attribute_specification,
            "attribute attr_name of foo : signal is 0+1;",
            "\
AttributeSpecification
  Keyword(Attribute)
  Identifier 'attr_name'
  Keyword(Of)
  EntitySpecification
    EntityNameList
      DesignatorList
        EntityDesignator
          Identifier 'foo'
    Colon
    Keyword(Signal)
  Keyword(Is)
  BinaryExpression
    Literal
      AbstractLiteral '0'
    Plus
    Literal
      AbstractLiteral '1'
  SemiColon
",
        );
    }

    #[test]
    fn simple_attribute_specification_operator_symbol() {
        check(
            Parser::attribute_specification,
            "attribute attr_name of \"**\" : function is 0+1;",
            "\
AttributeSpecification
  Keyword(Attribute)
  Identifier 'attr_name'
  Keyword(Of)
  EntitySpecification
    EntityNameList
      DesignatorList
        EntityDesignator
          StringLiteral '\"**\"'
    Colon
    Keyword(Function)
  Keyword(Is)
  BinaryExpression
    Literal
      AbstractLiteral '0'
    Plus
    Literal
      AbstractLiteral '1'
  SemiColon
",
        );
    }

    #[test]
    fn attribute_specification_list() {
        check(
            Parser::attribute_specification,
            "attribute attr_name of foo, bar : signal is 0+1;",
            "\
AttributeSpecification
  Keyword(Attribute)
  Identifier 'attr_name'
  Keyword(Of)
  EntitySpecification
    EntityNameList
      DesignatorList
        EntityDesignator
          Identifier 'foo'
        Comma
        EntityDesignator
          Identifier 'bar'
    Colon
    Keyword(Signal)
  Keyword(Is)
  BinaryExpression
    Literal
      AbstractLiteral '0'
    Plus
    Literal
      AbstractLiteral '1'
  SemiColon
",
        );
    }

    #[test]
    fn attribute_specification_all() {
        check(
            Parser::attribute_specification,
            "attribute attr_name of all : signal is 0+1;",
            "\
AttributeSpecification
  Keyword(Attribute)
  Identifier 'attr_name'
  Keyword(Of)
  EntitySpecification
    EntityNameList
      Keyword(All)
    Colon
    Keyword(Signal)
  Keyword(Is)
  BinaryExpression
    Literal
      AbstractLiteral '0'
    Plus
    Literal
      AbstractLiteral '1'
  SemiColon
",
        );
    }

    #[test]
    fn attribute_specification_others() {
        check(
            Parser::attribute_specification,
            "attribute attr_name of others : signal is 0+1;",
            "\
AttributeSpecification
  Keyword(Attribute)
  Identifier 'attr_name'
  Keyword(Of)
  EntitySpecification
    EntityNameList
      Keyword(Others)
    Colon
    Keyword(Signal)
  Keyword(Is)
  BinaryExpression
    Literal
      AbstractLiteral '0'
    Plus
    Literal
      AbstractLiteral '1'
  SemiColon
",
        );
    }

    #[test]
    fn attribute_specification_with_signature() {
        check(
            Parser::attribute_specification,
            "attribute attr_name of foo[return natural] : function is 0+1;",
            "\
AttributeSpecification
  Keyword(Attribute)
  Identifier 'attr_name'
  Keyword(Of)
  EntitySpecification
    EntityNameList
      DesignatorList
        EntityDesignator
          Identifier 'foo'
          Signature
            LeftSquare
            Keyword(Return)
            Name
              Identifier 'natural'
            RightSquare
    Colon
    Keyword(Function)
  Keyword(Is)
  BinaryExpression
    Literal
      AbstractLiteral '0'
    Plus
    Literal
      AbstractLiteral '1'
  SemiColon
",
        );
    }
}
