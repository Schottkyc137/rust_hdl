// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2023, Olof Kraigher olof.kraigher@gmail.com

use crate::ast::{IdentList, NameList, SeparatedList, WithRef};
use crate::data::{DiagnosticHandler, DiagnosticResult};
use crate::syntax::common::ParseResult;
use crate::syntax::names::parse_name;
use crate::syntax::Kind::Comma;
use crate::syntax::{kind_str, Kind, TokenAccess, TokenStream};
use crate::Diagnostic;

/// Skip extraneous tokens of kind `separator`.
/// When there are any extra tokens of that kind, mark all the positions of these tokens as erroneous
fn skip_extraneous_tokens(
    stream: &TokenStream,
    separator: Kind,
    diagnostics: &mut dyn DiagnosticHandler,
) {
    if let Some(separator_tok) = stream.pop_if_kind(separator) {
        let start_pos = stream.get_pos(separator_tok);
        let mut end_pos = start_pos;
        while let Some(separator_tok) = stream.pop_if_kind(separator) {
            end_pos = stream.get_pos(separator_tok)
        }
        diagnostics.push(Diagnostic::syntax_error(
            start_pos.combine(end_pos),
            format!("Extraneous '{}'", kind_str(separator)),
        ));
    }
}

/// Parses a list of the form
///   `element { separator element }`
/// where `element` is an AST element and `separator` is a token of some `ast::Kind`.
/// The returned list retains information of the whereabouts of the separator tokens.
pub fn parse_list_with_separator<F, T>(
    stream: &TokenStream,
    separator: Kind,
    diagnostics: &mut dyn DiagnosticHandler,
    parse_fn: F,
) -> DiagnosticResult<SeparatedList<T>>
where
    F: Fn(&TokenStream) -> ParseResult<T>,
{
    parse_list_with_separator_or_recover(stream, separator, diagnostics, parse_fn, None)
}

/// Same as `parse_list_with_separator`.
/// However, when supplied with a `recover_token` will skip until either the separator
/// or the recover token is found.
pub fn parse_list_with_separator_or_recover<F, T>(
    stream: &TokenStream,
    separator: Kind,
    diagnostics: &mut dyn DiagnosticHandler,
    parse_fn: F,
    recover_token: Option<Kind>,
) -> DiagnosticResult<SeparatedList<T>>
where
    F: Fn(&TokenStream) -> ParseResult<T>,
{
    let mut items = vec![];
    let mut tokens = vec![];
    loop {
        match parse_fn(stream) {
            Ok(item) => items.push(item),
            Err(err) => {
                if let Some(tok) = recover_token {
                    stream.skip_until(|kind| kind == separator || kind == tok)?;
                    diagnostics.push(err);
                } else {
                    return Err(err);
                }
            }
        }
        if let Some(separator_tok) = stream.pop_if_kind(separator) {
            skip_extraneous_tokens(stream, separator, diagnostics);
            tokens.push(separator_tok);
        } else {
            break;
        }
    }
    Ok(SeparatedList { items, tokens })
}

pub fn parse_name_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> DiagnosticResult<NameList> {
    parse_list_with_separator(stream, Comma, diagnostics, parse_name)
}

pub fn parse_ident_list(
    stream: &TokenStream,
    diagnostics: &mut dyn DiagnosticHandler,
) -> DiagnosticResult<IdentList> {
    parse_list_with_separator(stream, Comma, diagnostics, |stream| {
        stream.expect_ident().map(WithRef::new)
    })
}

#[cfg(test)]
mod test {
    use crate::ast::{IdentList, NameList, SeparatedList};
    use crate::syntax::names::parse_association_element;
    use crate::syntax::separated_list::{
        parse_ident_list, parse_list_with_separator_or_recover, parse_name_list,
    };
    use crate::syntax::test::Code;
    use crate::syntax::Kind;
    use crate::syntax::Kind::RightPar;
    use crate::Diagnostic;

    #[test]
    pub fn test_error_on_empty_list() {
        let code = Code::new("");
        let (res, diagnostics) = code.with_partial_stream_diagnostics(parse_ident_list);
        assert_eq!(
            res,
            Err(Diagnostic::syntax_error(code.eof_pos(), "Unexpected EOF"))
        );
        assert!(diagnostics.is_empty());
    }

    #[test]
    pub fn parse_single_element_list() {
        let code = Code::new("abc");
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_ident_list),
            IdentList::single(code.s1("abc").ident().into_ref())
        )
    }

    #[test]
    pub fn parse_list_with_multiple_elements() {
        let code = Code::new("abc, def, ghi");
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_ident_list),
            IdentList {
                items: vec![
                    code.s1("abc").ident().into_ref(),
                    code.s1("def").ident().into_ref(),
                    code.s1("ghi").ident().into_ref()
                ],
                tokens: vec![code.s(",", 1).token(), code.s(",", 2).token()]
            }
        )
    }

    #[test]
    fn parse_list_with_many_names() {
        let code = Code::new("work.foo, lib.bar.all");
        assert_eq!(
            code.parse_ok_no_diagnostics(parse_name_list),
            NameList {
                items: vec![code.s1("work.foo").name(), code.s1("lib.bar.all").name()],
                tokens: vec![code.s1(",").token()],
            }
        )
    }

    #[test]
    fn parse_extraneous_single_separators() {
        let code = Code::new("a,,b,c");
        let (res, diag) = code.with_stream_diagnostics(parse_ident_list);
        assert_eq!(
            res,
            IdentList {
                items: vec![
                    code.s1("a").ident().into_ref(),
                    code.s1("b").ident().into_ref(),
                    code.s1("c").ident().into_ref()
                ],
                tokens: vec![code.s(",", 1).token(), code.s(",", 3).token()]
            }
        );
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s(",", 2).pos(),
                "Extraneous ','"
            )]
        )
    }

    #[test]
    fn parse_extraneous_multiple_separators() {
        let code = Code::new("a,,,,b,c");
        let (res, diag) = code.with_stream_diagnostics(parse_ident_list);
        assert_eq!(
            res,
            IdentList {
                items: vec![
                    code.s1("a").ident().into_ref(),
                    code.s1("b").ident().into_ref(),
                    code.s1("c").ident().into_ref()
                ],
                tokens: vec![code.s(",", 1).token(), code.s(",", 5).token()]
            }
        );
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s(",,,", 2).pos(),
                "Extraneous ','"
            )]
        )
    }

    #[test]
    fn parse_recoverable_list() {
        let code = Code::new("a => b,c => d, e =>)");
        let (res, diag) = code.with_stream_diagnostics(|stream, diag| {
            let res = parse_list_with_separator_or_recover(
                stream,
                Kind::Comma,
                diag,
                parse_association_element,
                Some(RightPar),
            );
            stream.skip();
            res
        });
        assert_eq!(
            res,
            SeparatedList {
                items: vec![
                    code.s1("a => b").association_element(),
                    code.s1("c => d").association_element()
                ],
                tokens: vec![code.s(",", 1).token(), code.s(",", 2).token(),],
            }
        );
        assert_eq!(
            diag,
            vec![Diagnostic::syntax_error(
                code.s1(")"),
                "Expected {expression}"
            )]
        );
    }

    #[test]
    fn parse_list_with_erroneous_elements() {
        let code = Code::new("1,c,d");
        let mut diag: Vec<Diagnostic> = vec![];
        let diag = code
            .parse(|stream| parse_ident_list(stream, &mut diag))
            .expect_err("Should not parse OK");
        assert_eq!(
            diag,
            Diagnostic::syntax_error(code.s1("1"), "Expected '{identifier}'")
        );
    }
}
