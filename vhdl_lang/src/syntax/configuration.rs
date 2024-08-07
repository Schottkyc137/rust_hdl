// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::common::check_end_identifier_mismatch;
use super::common::ParseResult;
use super::concurrent_statement::parse_generic_and_port_map;
use super::context::parse_use_clause;
use super::names::{parse_name, parse_selected_name};
use super::tokens::{Kind::*, TokenSpan};
use crate::ast::token_range::WithTokenSpan;
use crate::ast::*;
use crate::data::*;
use crate::syntax::recover::{expect_semicolon, expect_semicolon_or_last};
use vhdl_lang::syntax::parser::ParsingContext;
use vhdl_lang::TokenId;

/// LRM 7.3.2.2
pub(crate) fn parse_entity_aspect(ctx: &mut ParsingContext<'_>) -> ParseResult<EntityAspect> {
    let entity_aspect = expect_token!(
        ctx.stream,
        token,
        Open => EntityAspect::Open,
        Configuration => EntityAspect::Configuration(parse_selected_name(ctx)?),
        Entity => {
            let entity_name = parse_selected_name(ctx)?;
            let arch_name = {
                if ctx.stream.skip_if_kind(LeftPar) {
                    let ident = ctx.stream.expect_ident()?;
                    ctx.stream.expect_kind(RightPar)?;
                    Some(ident)
                } else {
                    None
                }
            };
            EntityAspect::Entity(entity_name, arch_name)
        }
    );
    Ok(entity_aspect)
}

fn parse_binding_indication_known_entity_aspect(
    ctx: &mut ParsingContext<'_>,
    entity_aspect: Option<EntityAspect>,
    start_token: TokenId,
) -> ParseResult<BindingIndication> {
    let (generic_map, port_map) = parse_generic_and_port_map(ctx)?;

    let semicolon_token = expect_semicolon_or_last(ctx);
    Ok(BindingIndication {
        entity_aspect,
        generic_map,
        port_map,
        span: TokenSpan::new(start_token, semicolon_token),
    })
}

/// LRM 7.3.2
fn parse_binding_indication(ctx: &mut ParsingContext<'_>) -> ParseResult<BindingIndication> {
    let (entity_aspect, token) = if let Some(use_token) = ctx.stream.pop_if_kind(Use) {
        (Some(parse_entity_aspect(ctx)?), use_token)
    } else {
        (None, ctx.stream.get_current_token_id())
    };
    parse_binding_indication_known_entity_aspect(ctx, entity_aspect, token)
}

fn parse_component_configuration_known_spec(
    ctx: &mut ParsingContext<'_>,
    spec: ComponentSpecification,
) -> ParseResult<ComponentConfiguration> {
    let (bind_ind, vunit_bind_inds) = peek_token!(
        ctx.stream,
        token,
        token_id,
        End => (None, Vec::new()),
        For => (None, Vec::new()),
        Use => {
            ctx.stream.skip();
            if ctx.stream.peek_kind() == Some(Vunit) {
                let vunit_bind_inds = parse_vunit_binding_indication_list_known_keyword(ctx, token_id)?;
                (None, vunit_bind_inds)
            } else {
                let aspect = parse_entity_aspect(ctx)?;
                let bind_ind = parse_binding_indication_known_entity_aspect(ctx, Some(aspect), token_id)?;

                if let Some(start_token) = ctx.stream.pop_if_kind(Use) {
                    (Some(bind_ind), parse_vunit_binding_indication_list_known_keyword(ctx, start_token)?)
                } else {
                    (Some(bind_ind), Vec::new())
                }
            }
        }
    );

    let block_config = expect_token!(
        ctx.stream,
        token,
        token_id,
        End => None,
        For => {
            let block_config = parse_block_configuration_known_keyword(ctx, token_id)?;
            ctx.stream.expect_kind(End)?;
            Some(block_config)
        }
    );

    ctx.stream.expect_kind(For)?;
    let semicolon_token = expect_semicolon_or_last(ctx);
    let start_token = spec.span.start_token;
    Ok(ComponentConfiguration {
        spec,
        bind_ind,
        vunit_bind_inds,
        block_config,
        span: TokenSpan::new(start_token, semicolon_token),
    })
}

enum ComponentSpecificationOrName {
    ComponentSpec(ComponentSpecification),
    Name(WithTokenSpan<Name>),
}

fn parse_component_specification_or_name(
    ctx: &mut ParsingContext<'_>,
    start_token: TokenId,
) -> ParseResult<ComponentSpecificationOrName> {
    peek_token!(
        ctx.stream, token,
        All => {
            ctx.stream.skip();
            let colon_token = ctx.stream.expect_kind(Colon)?;
            let component_name = parse_selected_name(ctx)?;
            let end_token = component_name.span.end_token;
            Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                instantiation_list: InstantiationList::All,
                component_name,
                colon_token,
                span: TokenSpan::new(start_token, end_token),
            }))

        },
        Others => {
            ctx.stream.skip();
            let colon_token = ctx.stream.expect_kind(Colon)?;
            let component_name = parse_selected_name(ctx)?;
            let end_token = component_name.span.end_token;
            Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                instantiation_list: InstantiationList::Others,
                component_name,
                colon_token,
                span: TokenSpan::new(start_token, end_token),
            }))
        },
        Identifier => {
            let name = parse_name(ctx)?;
            let sep_token = ctx.stream.peek_expect()?;
            match sep_token.kind {
                Colon => {
                    let colon_token = ctx.stream.get_current_token_id();
                    ctx.stream.skip();
                    let ident = to_simple_name(ctx.stream, name)?;
                    let component_name = parse_selected_name(ctx)?;
                    let end_token = component_name.span.end_token;
                    Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                        instantiation_list: InstantiationList::Labels(vec![ident]),
                        component_name,
                        colon_token,
                        span: TokenSpan::new(start_token, end_token),
                    }))
                }
                Comma => {
                    ctx.stream.skip();
                    let mut idents = vec![to_simple_name(ctx.stream, name)?];
                    let colon_token = loop {
                        idents.push(ctx.stream.expect_ident()?);
                        expect_token!(
                            ctx.stream,
                            next_token,
                            next_token_id,
                            Comma => {},
                            Colon => break next_token_id
                        );
                    };
                    let component_name = parse_selected_name(ctx)?;
                    let end_token = component_name.span.end_token;
                    Ok(ComponentSpecificationOrName::ComponentSpec(ComponentSpecification {
                        instantiation_list: InstantiationList::Labels(idents),
                        component_name,
                        colon_token,
                        span: TokenSpan::new(start_token, end_token),
                    }))
                }
                _ => Ok(ComponentSpecificationOrName::Name(name))
            }
        }
    )
}

fn parse_configuration_item_known_keyword(
    ctx: &mut ParsingContext<'_>,
    token: TokenId,
) -> ParseResult<ConfigurationItem> {
    match parse_component_specification_or_name(ctx, token)? {
        ComponentSpecificationOrName::ComponentSpec(component_spec) => {
            Ok(ConfigurationItem::Component(
                parse_component_configuration_known_spec(ctx, component_spec)?,
            ))
        }
        ComponentSpecificationOrName::Name(name) => Ok(ConfigurationItem::Block(
            parse_block_configuration_known_name(ctx, name, token)?,
        )),
    }
}

fn parse_block_configuration_known_name(
    ctx: &mut ParsingContext<'_>,
    name: WithTokenSpan<Name>,
    start_token: TokenId,
) -> ParseResult<BlockConfiguration> {
    let block_spec = name;
    // @TODO use clauses
    let use_clauses = Vec::new();
    let mut items = Vec::new();

    loop {
        expect_token!(
            ctx.stream,
            token,
            token_id,
            End => {
                break;
            },
            For => {
                items.push(parse_configuration_item_known_keyword(ctx, token_id)?);
            }
        );
    }
    ctx.stream.expect_kind(For)?;
    let semicolon = expect_semicolon_or_last(ctx);
    Ok(BlockConfiguration {
        block_spec,
        use_clauses,
        items,
        span: TokenSpan::new(start_token, semicolon),
    })
}

fn parse_block_configuration_known_keyword(
    ctx: &mut ParsingContext<'_>,
    token: TokenId,
) -> ParseResult<BlockConfiguration> {
    let name = parse_name(ctx)?;
    parse_block_configuration_known_name(ctx, name, token)
}

fn parse_vunit_binding_indication_list_known_keyword(
    ctx: &mut ParsingContext<'_>,
    start_token: TokenId,
) -> ParseResult<Vec<VUnitBindingIndication>> {
    let mut indications = Vec::new();
    loop {
        ctx.stream.expect_kind(Vunit)?;

        let mut vunit_list = Vec::new();

        let vunit_bind_ind = loop {
            vunit_list.push(parse_name(ctx)?);
            peek_token!(
                ctx.stream, token, token_id,
                Comma => {
                    ctx.stream.skip();
                },
                SemiColon => {
                    ctx.stream.skip();
                    break VUnitBindingIndication {
                        vunit_list,
                        span: TokenSpan::new(start_token, token_id)
                    };
                }
            );
        };

        indications.push(vunit_bind_ind);

        if !ctx.stream.skip_if_kind(Use) {
            break;
        }
    }
    Ok(indications)
}

/// LRM 3.4 Configuration declaration
pub fn parse_configuration_declaration(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ConfigurationDeclaration> {
    let start_token = ctx.stream.expect_kind(Configuration)?;
    let ident = WithDecl::new(ctx.stream.expect_ident()?);
    ctx.stream.expect_kind(Of)?;
    let entity_name = parse_selected_name(ctx)?;
    ctx.stream.expect_kind(Is)?;
    let mut decl = Vec::new();

    let vunit_bind_inds = loop {
        let token = ctx.stream.peek_expect()?;
        match token.kind {
            Use => {
                if ctx.stream.nth_kind_is(1, Vunit) {
                    let start_token = ctx.stream.get_current_token_id();
                    ctx.stream.skip();
                    break parse_vunit_binding_indication_list_known_keyword(ctx, start_token)?;
                }

                decl.push(parse_use_clause(ctx)?.map_into(Declaration::Use));
            }
            _ => break Vec::new(),
        }
    };

    let for_token = ctx.stream.expect_kind(For)?;
    let block_config = parse_block_configuration_known_keyword(ctx, for_token)?;

    let end_token = ctx.stream.expect_kind(End)?;
    ctx.stream.pop_if_kind(Configuration);
    let end_ident = ctx.stream.pop_optional_ident();
    let last_token = expect_semicolon_or_last(ctx);

    Ok(ConfigurationDeclaration {
        span: TokenSpan::new(start_token, last_token),
        context_clause: ContextClause::default(),
        end_ident_pos: check_end_identifier_mismatch(ctx, &ident.tree, end_ident),
        ident,
        entity_name,
        decl,
        vunit_bind_inds,
        block_config,
        end_token,
    })
}

/// LRM 7.3 Configuration Specification
pub fn parse_configuration_specification(
    ctx: &mut ParsingContext<'_>,
) -> ParseResult<ConfigurationSpecification> {
    let start_token = ctx.stream.expect_kind(For)?;
    match parse_component_specification_or_name(ctx, start_token)? {
        ComponentSpecificationOrName::ComponentSpec(spec) => {
            let bind_ind = parse_binding_indication(ctx)?;
            if let Some(use_token) = ctx.stream.pop_if_kind(Use) {
                let vunit_bind_inds =
                    parse_vunit_binding_indication_list_known_keyword(ctx, use_token)?;
                let end_token = ctx.stream.expect_kind(End)?;
                ctx.stream.expect_kind(For)?;
                let final_token = expect_semicolon_or_last(ctx);
                Ok(ConfigurationSpecification {
                    span: TokenSpan::new(start_token, final_token),
                    spec,
                    bind_ind,
                    vunit_bind_inds,
                    end_token: Some(end_token),
                })
            } else {
                let end_token = if let Some(token) = ctx.stream.pop_if_kind(End) {
                    ctx.stream.expect_kind(For)?;
                    expect_semicolon(ctx);
                    Some(token)
                } else {
                    None
                };
                let final_token = ctx.stream.get_last_token_id();
                Ok(ConfigurationSpecification {
                    span: TokenSpan::new(start_token, final_token),
                    spec,
                    bind_ind,
                    end_token,
                    vunit_bind_inds: Vec::new(),
                })
            }
        }
        ComponentSpecificationOrName::Name(name) => Err(Diagnostic::syntax_error(
            name.pos(ctx.stream),
            "Expected component specification",
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::syntax::test::Code;

    #[test]
    fn empty_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                    span: code.between("for", "end for;").token_span(),
                },
                end_token: code.s("end", 2).token(),
                end_ident_pos: None,
            }
        );
    }

    #[test]
    fn empty_configuration_variant() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                    span: code.between("for", "end for;").token_span(),
                },
                end_token: code.s("end", 2).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }
    #[test]
    fn configuration_use_clause() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use lib2.foo.bar;
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![
                    code.s1("use lib.foo.bar;")
                        .use_clause()
                        .map_into(Declaration::Use),
                    code.s1("use lib2.foo.bar;")
                        .use_clause()
                        .map_into(Declaration::Use)
                ],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                    span: code.between("for", "end for;").token_span(),
                },
                end_token: code.s("end", 2).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_vunit_binding_indication() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  use lib.foo.bar;
  use vunit baz.foobar;
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![code
                    .s1("use lib.foo.bar;")
                    .use_clause()
                    .map_into(Declaration::Use)],
                vunit_bind_inds: vec![VUnitBindingIndication {
                    vunit_list: vec![code.s1("baz.foobar").name()],
                    span: code.s1("use vunit baz.foobar;").token_span()
                }],
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                    span: code.between("for", "end for;").token_span(),
                },
                end_token: code.s("end", 2).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_block_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![],
                    span: code.between("for", "end for;").token_span(),
                },
                end_token: code.s("end", 2).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_nested_block_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for name(0 to 3)
    end for;
    for other_name
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![
                        ConfigurationItem::Block(BlockConfiguration {
                            block_spec: code.s1("name(0 to 3)").name(),
                            use_clauses: vec![],
                            items: vec![],
                            span: code
                                .s1("for name(0 to 3)
    end for;")
                                .token_span(),
                        }),
                        ConfigurationItem::Block(BlockConfiguration {
                            block_spec: code.s1("other_name").name(),
                            use_clauses: vec![],
                            items: vec![],
                            span: code
                                .s1("for other_name
    end for;")
                                .token_span(),
                        })
                    ],
                    span: code
                        .s1("for rtl(0)
    for name(0 to 3)
    end for;
    for other_name
    end for;
  end for;")
                        .token_span(),
                },
                end_token: code.s("end", 4).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_component_configuration_nested() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      for arch
      end for;
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![code
                                .s1("inst")
                                .ident()]),
                            colon_token: code.s1(":").token(),
                            component_name: code.s1("lib.pkg.comp").name(),
                            span: code.s1("for inst : lib.pkg.comp").token_span()
                        },
                        bind_ind: None,
                        vunit_bind_inds: Vec::new(),
                        block_config: Some(BlockConfiguration {
                            block_spec: code.s1("arch").name(),
                            use_clauses: vec![],
                            items: vec![],
                            span: code
                                .s1("for arch
      end for;")
                                .token_span(),
                        }),
                        span: code
                            .s1("for inst : lib.pkg.comp
      for arch
      end for;
    end for;")
                            .token_span()
                    })],
                    span: code
                        .s1("for rtl(0)
    for inst : lib.pkg.comp
      for arch
      end for;
    end for;
  end for;")
                        .token_span(),
                },
                end_token: code.s("end", 4).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_component_configuration_vunit_binding_indication() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      use entity work.bar;
      use vunit baz;
      for arch
      end for;
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![code
                                .s1("inst")
                                .ident()]),
                            colon_token: code.s1(":").token(),
                            component_name: code.s1("lib.pkg.comp").name(),
                            span: code.s1("for inst : lib.pkg.comp").token_span()
                        },
                        bind_ind: Some(BindingIndication {
                            entity_aspect: Some(EntityAspect::Entity(
                                code.s1("work.bar").name(),
                                None
                            )),
                            generic_map: None,
                            port_map: None,
                            span: code.s1("use entity work.bar;").token_span()
                        }),
                        vunit_bind_inds: vec![VUnitBindingIndication {
                            vunit_list: vec![code.s1("baz").name()],
                            span: code.s1("use vunit baz;").token_span()
                        }],
                        block_config: Some(BlockConfiguration {
                            block_spec: code.s1("arch").name(),
                            use_clauses: vec![],
                            items: vec![],
                            span: code
                                .s1("for arch
      end for;")
                                .token_span(),
                        }),
                        span: code
                            .s1("for inst : lib.pkg.comp
      use entity work.bar;
      use vunit baz;
      for arch
      end for;
    end for;")
                            .token_span(),
                    })],
                    span: code
                        .s1("for rtl(0)
    for inst : lib.pkg.comp
      use entity work.bar;
      use vunit baz;
      for arch
      end for;
    end for;
  end for;")
                        .token_span(),
                },
                end_token: code.s("end", 4).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_component_configuration_binding_indication() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
      use entity lib.use_name;
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![ConfigurationItem::Component(ComponentConfiguration {
                        spec: ComponentSpecification {
                            instantiation_list: InstantiationList::Labels(vec![code
                                .s1("inst")
                                .ident()]),
                            colon_token: code.s1(":").token(),
                            component_name: code.s1("lib.pkg.comp").name(),
                            span: code.s1("for inst : lib.pkg.comp").token_span()
                        },
                        bind_ind: Some(BindingIndication {
                            entity_aspect: Some(EntityAspect::Entity(
                                code.s1("lib.use_name").name(),
                                None
                            )),
                            generic_map: None,
                            port_map: None,
                            span: code.s1("use entity lib.use_name;").token_span()
                        }),
                        vunit_bind_inds: Vec::new(),
                        block_config: None,
                        span: code
                            .s1("for inst : lib.pkg.comp
      use entity lib.use_name;
    end for;")
                            .token_span(),
                    })],
                    span: code
                        .s1("for rtl(0)
    for inst : lib.pkg.comp
      use entity lib.use_name;
    end for;
  end for;")
                        .token_span(),
                },
                end_token: code.s("end", 3).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn configuration_component_configuration() {
        let code = Code::new(
            "\
configuration cfg of entity_name is
  for rtl(0)
    for inst : lib.pkg.comp
    end for;
    for inst1, inst2, inst3 : lib2.pkg.comp
    end for;
    for all : lib3.pkg.comp
    end for;
    for others : lib4.pkg.comp
    end for;
  end for;
end configuration cfg;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_declaration),
            ConfigurationDeclaration {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("cfg").decl_ident(),
                entity_name: code.s1("entity_name").name(),
                decl: vec![],
                vunit_bind_inds: Vec::new(),
                block_config: BlockConfiguration {
                    block_spec: code.s1("rtl(0)").name(),
                    use_clauses: vec![],
                    items: vec![
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Labels(vec![code
                                    .s1("inst")
                                    .ident()]),
                                colon_token: code.s(":", 1).token(),
                                component_name: code.s1("lib.pkg.comp").name(),
                                span: code.s1("for inst : lib.pkg.comp").token_span()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                            span: code
                                .s1("for inst : lib.pkg.comp
    end for;")
                                .token_span()
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Labels(vec![
                                    code.s1("inst1").ident(),
                                    code.s1("inst2").ident(),
                                    code.s1("inst3").ident()
                                ]),
                                colon_token: code.s(":", 2).token(),
                                component_name: code.s1("lib2.pkg.comp").name(),
                                span: code
                                    .s1("for inst1, inst2, inst3 : lib2.pkg.comp")
                                    .token_span()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                            span: code
                                .s1("for inst1, inst2, inst3 : lib2.pkg.comp
    end for;")
                                .token_span()
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::All,
                                component_name: code.s1("lib3.pkg.comp").name(),
                                colon_token: code.s(":", 3).token(),
                                span: code.s1("for all : lib3.pkg.comp").token_span()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                            span: code
                                .s1("for all : lib3.pkg.comp
    end for;")
                                .token_span()
                        }),
                        ConfigurationItem::Component(ComponentConfiguration {
                            spec: ComponentSpecification {
                                instantiation_list: InstantiationList::Others,
                                component_name: code.s1("lib4.pkg.comp").name(),
                                colon_token: code.s(":", 4).token(),
                                span: code.s1("for others : lib4.pkg.comp").token_span()
                            },
                            bind_ind: None,
                            vunit_bind_inds: Vec::new(),
                            block_config: None,
                            span: code
                                .s1("for others : lib4.pkg.comp
    end for;")
                                .token_span()
                        })
                    ],
                    span: code
                        .s1("for rtl(0)
    for inst : lib.pkg.comp
    end for;
    for inst1, inst2, inst3 : lib2.pkg.comp
    end for;
    for all : lib3.pkg.comp
    end for;
    for others : lib4.pkg.comp
    end for;
  end for;")
                        .token_span(),
                },
                end_token: code.s("end", 6).token(),
                end_ident_pos: Some(code.s("cfg", 2).token())
            }
        );
    }

    #[test]
    fn entity_entity_aspect_entity() {
        let code = Code::new("entity lib.foo.name");
        assert_eq!(
            code.with_stream(parse_entity_aspect),
            EntityAspect::Entity(code.s1("lib.foo.name").name(), None)
        );
    }

    #[test]
    fn entity_entity_aspect_entity_arch() {
        let code = Code::new("entity lib.foo.name(arch)");
        assert_eq!(
            code.with_stream(parse_entity_aspect),
            EntityAspect::Entity(
                code.s1("lib.foo.name").name(),
                Some(code.s1("arch").ident())
            )
        );
    }

    #[test]
    fn entity_entity_aspect_configuration() {
        let code = Code::new("configuration lib.foo.name");
        assert_eq!(
            code.with_stream(parse_entity_aspect),
            EntityAspect::Configuration(code.s1("lib.foo.name").name())
        );
    }

    #[test]
    fn entity_entity_aspect_open() {
        let code = Code::new("open");
        assert_eq!(code.with_stream(parse_entity_aspect), EntityAspect::Open);
    }

    #[test]
    fn simple_configuration_specification() {
        let code = Code::new("for all : lib.pkg.comp use entity work.foo(rtl);");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_specification),
            ConfigurationSpecification {
                span: code.token_span(),
                spec: ComponentSpecification {
                    instantiation_list: InstantiationList::All,
                    component_name: code.s1("lib.pkg.comp").name(),
                    colon_token: code.s1(":").token(),
                    span: code.s1("for all : lib.pkg.comp").token_span()
                },
                bind_ind: BindingIndication {
                    entity_aspect: Some(EntityAspect::Entity(
                        code.s1("work.foo").name(),
                        Some(code.s1("rtl").ident())
                    )),
                    generic_map: None,
                    port_map: None,
                    span: code.s1("use entity work.foo(rtl);").token_span()
                },
                vunit_bind_inds: Vec::new(),
                end_token: None,
            }
        );
    }

    #[test]
    fn simple_configuration_specification_end_for() {
        let code = Code::new("for all : lib.pkg.comp use entity work.foo(rtl); end for;");

        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_specification),
            ConfigurationSpecification {
                span: code.token_span(),
                spec: ComponentSpecification {
                    instantiation_list: InstantiationList::All,
                    component_name: code.s1("lib.pkg.comp").name(),
                    colon_token: code.s1(":").token(),
                    span: code.s1("for all : lib.pkg.comp").token_span()
                },
                bind_ind: BindingIndication {
                    entity_aspect: Some(EntityAspect::Entity(
                        code.s1("work.foo").name(),
                        Some(code.s1("rtl").ident())
                    )),
                    generic_map: None,
                    port_map: None,
                    span: code.s1("use entity work.foo(rtl);").token_span()
                },
                vunit_bind_inds: Vec::new(),
                end_token: Some(code.s1("end").token())
            }
        );
    }

    #[test]
    fn compound_configuration_specification() {
        let code = Code::new(
            "for all : lib.pkg.comp use entity work.foo(rtl); use vunit bar, baz; end for;",
        );

        assert_eq!(
            code.with_stream_no_diagnostics(parse_configuration_specification),
            ConfigurationSpecification {
                span: code.token_span(),
                spec: ComponentSpecification {
                    instantiation_list: InstantiationList::All,
                    component_name: code.s1("lib.pkg.comp").name(),
                    colon_token: code.s1(":").token(),
                    span: code.s1("for all : lib.pkg.comp").token_span()
                },
                bind_ind: BindingIndication {
                    entity_aspect: Some(EntityAspect::Entity(
                        code.s1("work.foo").name(),
                        Some(code.s1("rtl").ident())
                    )),
                    generic_map: None,
                    port_map: None,
                    span: code.s1("use entity work.foo(rtl);").token_span()
                },
                vunit_bind_inds: vec![VUnitBindingIndication {
                    vunit_list: vec![code.s1("bar").name(), code.s1("baz").name()],
                    span: code.s1("use vunit bar, baz;").token_span()
                }],
                end_token: Some(code.s1("end").token()),
            }
        );
    }
}
