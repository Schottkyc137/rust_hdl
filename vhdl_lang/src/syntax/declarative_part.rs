// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2018, Olof Kraigher olof.kraigher@gmail.com

use super::alias_declaration::parse_alias_declaration;
use super::attributes::parse_attribute;
use super::common::ParseResult;
use super::component_declaration::parse_component_declaration;
use super::configuration::parse_configuration_specification;
use super::context::parse_use_clause;
use super::names::parse_selected_name;
use super::object_declaration::{parse_file_declaration, parse_object_declaration};
use super::subprogram::parse_subprogram;
use super::tokens::{Kind::*, *};
use super::type_declaration::parse_type_declaration;
use crate::ast::{ContextClause, Declaration, PackageInstantiation};
use crate::data::DiagnosticHandler;
use crate::syntax::concurrent_statement::parse_map_aspect;
use crate::syntax::view::parse_mode_view_declaration;
use crate::VHDLStandard;

pub fn parse_package_instantiation(
    stream: &TokenStream,
    diagnsotics: &mut dyn DiagnosticHandler,
) -> ParseResult<PackageInstantiation> {
    let start_token = stream.expect_kind(Package)?;
    let ident = stream.expect_ident()?;
    stream.expect_kind(Is)?;
    stream.expect_kind(New)?;
    let package_name = parse_selected_name(stream)?;
    let generic_map = parse_map_aspect(stream, Generic, diagnsotics)?;
    let end_token = stream.expect_kind(SemiColon)?;

    Ok(PackageInstantiation {
        span: TokenSpan::new(start_token, end_token),
        context_clause: ContextClause::default(),
        ident: ident.into(),
        package_name,
        generic_map,
    })
}

pub fn is_declarative_part(stream: &TokenStream) -> ParseResult<bool> {
    Ok(matches!(
        stream.peek_expect()?.kind,
        Use | Type
            | Subtype
            | Shared
            | Constant
            | Signal
            | Variable
            | File
            | Component
            | Attribute
            | Alias
            | Impure
            | Pure
            | Function
            | Procedure
            | Package
            | For
            | View
            | Begin
    ))
}

pub fn parse_declarative_part(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
    standard: VHDLStandard,
) -> ParseResult<Vec<Declaration>> {
    let mut declarations: Vec<Declaration> = Vec::new();

    fn is_recover_token(kind: Kind) -> bool {
        matches!(
            kind,
            Type | Subtype
                | Component
                | Impure
                | Pure
                | Function
                | Procedure
                | Package
                | For
                | File
                | Shared
                | Constant
                | Signal
                | Variable
                | Attribute
                | View
                | Use
                | Alias
                | Begin
                | End
        )
    }

    while let Some(token) = stream.peek() {
        match token.kind {
            Begin | End => break,
            Type | Subtype | Component | Impure | Pure | Function | Procedure | Package | For => {
                let decl = match token.kind {
                    Type | Subtype => parse_type_declaration(stream, diagnostics, standard)
                        .map(Declaration::Type)?,
                    Component => parse_component_declaration(stream, diagnostics)
                        .map(Declaration::Component)?,
                    Impure | Pure | Function | Procedure => {
                        parse_subprogram(stream, diagnostics, standard)?
                    }
                    Package => parse_package_instantiation(stream, diagnostics)
                        .map(Declaration::Package)?,
                    For => parse_configuration_specification(stream, diagnostics)
                        .map(Declaration::Configuration)?,
                    _ => unreachable!(),
                };
                declarations.push(decl);
            }

            File | Shared | Constant | Signal | Variable | Attribute => {
                let decls: ParseResult<Vec<Declaration>> = match token.kind {
                    File => parse_file_declaration(stream)
                        .map(|decls| decls.into_iter().map(Declaration::File).collect()),
                    Shared | Constant | Signal | Variable => parse_object_declaration(stream)
                        .map(|decls| decls.into_iter().map(Declaration::Object).collect()),
                    Attribute => parse_attribute(stream)
                        .map(|decls| decls.into_iter().map(Declaration::Attribute).collect()),
                    _ => unreachable!(),
                };
                match decls.or_recover_until(stream, diagnostics, is_recover_token) {
                    Ok(ref mut decls) => declarations.append(decls),
                    Err(err) => {
                        diagnostics.push(err);
                        continue;
                    }
                }
            }

            Use | Alias => {
                let decl: ParseResult<Declaration> = match token.kind {
                    Use => parse_use_clause(stream, diagnostics).map(Declaration::Use),
                    Alias => parse_alias_declaration(stream).map(Declaration::Alias),
                    _ => unreachable!(),
                };
                match decl.or_recover_until(stream, diagnostics, is_recover_token) {
                    Ok(decl) => declarations.push(decl),
                    Err(err) => {
                        diagnostics.push(err);
                        continue;
                    }
                }
            }

            View => {
                match parse_mode_view_declaration(stream, diagnostics).or_recover_until(
                    stream,
                    diagnostics,
                    is_recover_token,
                ) {
                    Ok(decl) => declarations.push(Declaration::View(decl)),
                    Err(err) => {
                        diagnostics.push(err);
                        continue;
                    }
                }
            }

            _ => {
                use crate::VHDLStandard::*;
                let expected: &[Kind] = match standard {
                    VHDL2008 | VHDL1993 => &[
                        Type, Subtype, Component, Impure, Pure, Function, Procedure, Package, For,
                        File, Shared, Constant, Signal, Variable, Attribute, Use, Alias,
                    ],
                    VHDL2019 => &[
                        Type, Subtype, Component, Impure, Pure, Function, Procedure, Package, For,
                        File, Shared, Constant, Signal, Variable, Attribute, Use, Alias, View,
                    ],
                };
                diagnostics.push(token.kinds_error(expected));
                stream.skip_until(is_recover_token)?;
                continue;
            }
        }
    }

    Ok(declarations)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{ObjectClass, ObjectDeclaration};
    use crate::data::Diagnostic;
    use crate::syntax::test::{check_diagnostics, Code};
    use crate::VHDLStandard::{VHDL2008, VHDL2019};

    #[test]
    fn package_instantiation() {
        let code = Code::new(
            "\
package ident is new lib.foo.bar;
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_package_instantiation),
            PackageInstantiation {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("ident").decl_ident(),
                package_name: code.s1("lib.foo.bar").name(),
                generic_map: None
            }
        );
    }

    #[test]
    fn package_instantiation_generic_map() {
        let code = Code::new(
            "\
package ident is new lib.foo.bar
  generic map (
    foo => bar
  );
",
        );
        assert_eq!(
            code.with_stream_no_diagnostics(parse_package_instantiation),
            PackageInstantiation {
                span: code.token_span(),
                context_clause: ContextClause::default(),
                ident: code.s1("ident").decl_ident(),
                package_name: code.s1("lib.foo.bar").name(),
                generic_map: Some(
                    code.s1("generic map (
    foo => bar
  )")
                        .generic_map_aspect()
                )
            }
        );
    }

    #[test]
    fn parse_declarative_part_recover() {
        let code = Code::new(
            "\
var invalid: broken;
constant x: natural := 5;
",
        );
        let (decls, msgs) = code.with_partial_stream_diagnostics(|stream, diagnostics| {
            parse_declarative_part(stream, diagnostics, VHDL2008)
        });
        assert_eq!(
            decls,
            Ok(vec![Declaration::Object(ObjectDeclaration {
                span: code.s1_to_end("constant").token_span(),
                class: ObjectClass::Constant,
                ident: code.s1("x").decl_ident(),
                subtype_indication: code.s1("natural").subtype_indication(),
                expression: Some(code.s1("5").expr())
            })])
        );

        assert_eq!(
            msgs,
            vec![Diagnostic::syntax_error(
                code.s1("var").pos(),
                "Expected 'type', 'subtype', 'component', 'impure', 'pure', \
                 'function', 'procedure', 'package', 'for', 'file', \
                 'shared', 'constant', 'signal', 'variable', 'attribute', \
                 'use' or 'alias'"
            )]
        );
    }

    #[test]
    fn parse_declarative_part_error() {
        // Just checking that there is not an infinite loop
        let code = Code::new("invalid");
        let (decl, _) = code.with_partial_stream_diagnostics(|stream, diagnostics| {
            parse_declarative_part(stream, diagnostics, VHDL2008)
        });
        assert!(decl.is_err());
    }

    #[test]
    fn parse_declarative_part_vhdl2008_vs_vhdl2019() {
        let code = Code::new(
            "\
var not_a_var: broken;
",
        );
        let (_, diag) = code.with_partial_stream_diagnostics(|stream, diagnostics| {
            parse_declarative_part(stream, diagnostics, VHDL2008)
        });
        check_diagnostics(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1("var").pos(),
                "Expected 'type', 'subtype', 'component', 'impure', 'pure', \
                 'function', 'procedure', 'package', 'for', 'file', \
                 'shared', 'constant', 'signal', 'variable', 'attribute', \
                 'use' or 'alias'",
            )],
        );

        let code = Code::new(
            "\
var not_a_var: broken;
",
        );
        let (_, diag) = code.with_partial_stream_diagnostics(|stream, diagnostics| {
            parse_declarative_part(stream, diagnostics, VHDL2019)
        });
        check_diagnostics(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1("var").pos(),
                "Expected 'type', 'subtype', 'component', 'impure', 'pure', \
                 'function', 'procedure', 'package', 'for', 'file', \
                 'shared', 'constant', 'signal', 'variable', 'attribute', \
                 'use', 'alias' or 'view'",
            )],
        )
    }
}
