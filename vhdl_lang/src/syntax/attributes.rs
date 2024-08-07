// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::ParseResult;
use super::expression::parse_expression;
use super::names::parse_type_mark;
use super::subprogram::parse_signature;
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::{
    Attribute, AttributeDeclaration, AttributeSpecification, Designator, EntityClass, EntityName,
    EntityTag, WithRef,
};
use crate::syntax::recover::expect_semicolon_or_last;
use vhdl_lang::syntax::parser::ParsingContext;

fn parse_entity_class(ctx: &mut ParsingContext<'_>) -> ParseResult<EntityClass> {
    Ok(expect_token!(ctx.stream, token,
        Entity => EntityClass::Entity,
        Architecture => EntityClass::Architecture,
        Configuration => EntityClass::Configuration,
        Procedure => EntityClass::Procedure,
        Function => EntityClass::Function,
        Package => EntityClass::Package,
        Type => EntityClass::Type,
        Subtype => EntityClass::Subtype,
        Constant => EntityClass::Constant,
        Signal => EntityClass::Signal,
        Variable => EntityClass::Variable,
        Component => EntityClass::Component,
        Label => EntityClass::Label,
        Literal => EntityClass::Literal,
        Units => EntityClass::Units,
        File => EntityClass::File
    ))
}

pub fn parse_entity_name_list(ctx: &mut ParsingContext<'_>) -> ParseResult<Vec<EntityName>> {
    Ok(expect_token!(ctx.stream, token, token_id,
        Identifier | StringLiteral => {
            let mut entity_name_list = Vec::new();
            let mut token = token;
            let mut token_id = token_id;
            loop {

                let designator = match token.kind {
                    Identifier => token.to_identifier_value(token_id)?.map_into(Designator::Identifier),
                    StringLiteral => token.to_operator_symbol(token_id)?.map_into(Designator::OperatorSymbol),
                    _ => unreachable!(""),
                };

                let signature = {
                    if ctx.stream.peek_kind() == Some(LeftSquare) {
                        Some(parse_signature(ctx)?)
                    } else {
                        None
                    }
                };

                entity_name_list.push(EntityName::Name(EntityTag {
                    designator: designator.map_into(WithRef::new),
                    signature,
                }));

                if ctx.stream.skip_if_kind(Comma) {
                    token_id = ctx.stream.get_current_token_id();
                    token = expect_token!(ctx.stream, token, Identifier | StringLiteral => token);
                } else {
                    break entity_name_list;
                }
            }
        },
        Others => {
            vec![EntityName::Others]
        },
        All => {
            vec![EntityName::All]
        }
    ))
}

pub fn parse_attribute(ctx: &mut ParsingContext<'_>) -> ParseResult<Vec<WithTokenSpan<Attribute>>> {
    let start_token = ctx.stream.expect_kind(Attribute)?;
    let ident = ctx.stream.expect_ident()?;
    Ok(expect_token!(ctx.stream, token,
        Colon => {
            let type_mark = parse_type_mark(ctx)?;
            let end_token = expect_semicolon_or_last(ctx);
            vec![WithTokenSpan::new(Attribute::Declaration(AttributeDeclaration {
                ident: ident.into(),
                type_mark,
            }), TokenSpan::new(start_token, end_token))]
        },
        Of => {
            let entity_names = parse_entity_name_list(ctx)?;
            let colon_token = ctx.stream.expect_kind(Colon)?;
            let entity_class = parse_entity_class(ctx)?;
            ctx.stream.expect_kind(Is)?;
            let expr = parse_expression(ctx)?;
            let end_token = expect_semicolon_or_last(ctx);

            entity_names
                .into_iter()
                .map(|entity_name| {
                WithTokenSpan::new(
                        Attribute::Specification(AttributeSpecification {
                            ident: WithRef::new(ident.clone()),
                            entity_name,
                            entity_class,
                            expr: expr.clone(),
                            colon_token
                        }),
                    TokenSpan::new(start_token, end_token)
                    )
                }).collect()
        }
    ))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;
    use pretty_assertions::assert_eq;

    #[test]
    fn parse_simple_attribute_declaration() {
        let code = Code::new("attribute foo : lib.name;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![WithTokenSpan::new(
                Attribute::Declaration(AttributeDeclaration {
                    ident: code.s1("foo").decl_ident(),
                    type_mark: code.s1("lib.name").type_mark()
                }),
                code.token_span()
            )]
        )
    }

    #[test]
    fn parse_simple_attribute_specification() {
        let code = Code::new("attribute attr_name of foo : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![WithTokenSpan::new(
                Attribute::Specification(AttributeSpecification {
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("foo").ref_designator(),
                        signature: None
                    }),
                    colon_token: code.s1(":").token(),
                    entity_class: EntityClass::Signal,
                    expr: code.s1("0+1").expr()
                }),
                code.token_span()
            )]
        )
    }

    #[test]
    fn parse_simple_attribute_specification_operator_symbol() {
        let code = Code::new("attribute attr_name of \"**\" : function is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![WithTokenSpan::new(
                Attribute::Specification(AttributeSpecification {
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("\"**\"").ref_designator(),
                        signature: None
                    }),
                    colon_token: code.s1(":").token(),
                    entity_class: EntityClass::Function,
                    expr: code.s1("0+1").expr()
                }),
                code.token_span()
            )]
        )
    }

    #[test]
    fn parse_attribute_specification_list() {
        let code = Code::new("attribute attr_name of foo, bar : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![
                WithTokenSpan::new(
                    Attribute::Specification(AttributeSpecification {
                        ident: WithRef::new(code.s1("attr_name").ident()),
                        entity_name: EntityName::Name(EntityTag {
                            designator: code.s1("foo").ref_designator(),
                            signature: None
                        }),
                        colon_token: code.s1(":").token(),
                        entity_class: EntityClass::Signal,
                        expr: code.s1("0+1").expr()
                    }),
                    code.token_span()
                ),
                WithTokenSpan::new(
                    Attribute::Specification(AttributeSpecification {
                        ident: WithRef::new(code.s1("attr_name").ident()),
                        entity_name: EntityName::Name(EntityTag {
                            designator: code.s1("bar").ref_designator(),
                            signature: None
                        }),
                        colon_token: code.s1(":").token(),
                        entity_class: EntityClass::Signal,
                        expr: code.s1("0+1").expr()
                    }),
                    code.token_span()
                )
            ]
        )
    }

    #[test]
    fn parse_attribute_specification_all() {
        let code = Code::new("attribute attr_name of all : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![WithTokenSpan::new(
                Attribute::Specification(AttributeSpecification {
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::All,
                    colon_token: code.s1(":").token(),
                    entity_class: EntityClass::Signal,
                    expr: code.s1("0+1").expr()
                }),
                code.token_span()
            )]
        )
    }

    #[test]
    fn parse_attribute_specification_others() {
        let code = Code::new("attribute attr_name of others : signal is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![WithTokenSpan::new(
                Attribute::Specification(AttributeSpecification {
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::Others,
                    colon_token: code.s1(":").token(),
                    entity_class: EntityClass::Signal,
                    expr: code.s1("0+1").expr()
                }),
                code.token_span()
            )]
        )
    }

    #[test]
    fn parse_attribute_specification_with_signature() {
        let code = Code::new("attribute attr_name of foo[return natural] : function is 0+1;");
        assert_eq!(
            code.with_stream(parse_attribute),
            vec![WithTokenSpan::new(
                Attribute::Specification(AttributeSpecification {
                    ident: WithRef::new(code.s1("attr_name").ident()),
                    entity_name: EntityName::Name(EntityTag {
                        designator: code.s1("foo").ref_designator(),
                        signature: Some(code.s1("[return natural]").signature())
                    }),
                    colon_token: code.s1(":").token(),
                    entity_class: EntityClass::Function,
                    expr: code.s1("0+1").expr()
                }),
                code.token_span()
            )]
        )
    }
}
