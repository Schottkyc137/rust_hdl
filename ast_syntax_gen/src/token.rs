// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::node::SequenceNode;
use convert_case::{Case, Casing};
use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use serde_yml::{Mapping, Value};
use std::str::FromStr;

#[derive(PartialEq, Eq, Copy, Clone, Debug, strum::Display, strum::EnumString)]
pub enum TokenKind {
    /// A keyword, such as `entity`, `architecture` or `abs`.
    #[strum(disabled)]
    Keyword(Keyword),

    Plus,  // +
    Minus, // -

    EQ,  // =
    NE,  // /=
    LT,  // <
    LTE, // <=
    GT,  // >
    GTE, // >=

    QueEQ,  // ?=
    QueNE,  // ?/=
    QueLT,  // ?<
    QueLTE, // ?<=
    QueGT,  // ?>
    QueGTE, // ?>=
    Que,    // ?
    QueQue, // ??

    Times, // *
    Pow,   // **
    Div,   // /

    Tick,        // '
    LeftPar,     // (
    RightPar,    // )
    LeftSquare,  // [
    RightSquare, // ]
    SemiColon,   // ;
    Colon,       // :
    Bar,         // |
    Dot,         // .
    BOX,         // <>
    LtLt,        // <<
    GtGt,        // >>
    Circ,        // ^
    CommAt,      // @
    Concat,      // &
    Comma,       // ,
    ColonEq,     // :=
    RightArrow,  // =>

    Identifier,
    AbstractLiteral,
    StringLiteral,
    BitStringLiteral,
    CharacterLiteral,
    ToolDirective,

    // Erroneous input
    /// String, extended identifier or based integer without final quotation char
    Unterminated,

    /// Unknown input
    ///
    /// Produced, for example, when there is an unknown char or illegal bit string
    Unknown,
}

/// All available keywords in the latest (VHDL 2019) edition of VHDL
#[derive(PartialEq, Eq, Clone, Copy, Debug, strum::Display, strum::EnumString)]
pub enum Keyword {
    Abs,
    Access,
    After,
    Alias,
    All,
    And,
    Architecture,
    Array,
    Assert,
    Assume,
    Attribute,
    Begin,
    Block,
    Body,
    Buffer,
    Bus,
    Case,
    Component,
    Configuration,
    Constant,
    Context,
    Cover,
    Default,
    Disconnect,
    Downto,
    Else,
    Elsif,
    End,
    Entity,
    Exit,
    Fairness,
    File,
    For,
    Force,
    Function,
    Generate,
    Generic,
    Group,
    Guarded,
    If,
    Impure,
    In,
    Inertial,
    Inout,
    Is,
    Label,
    Library,
    Linkage,
    Literal,
    Loop,
    Map,
    Mod,
    Nand,
    New,
    Next,
    Nor,
    Not,
    Null,
    Of,
    On,
    Open,
    Or,
    Others,
    Out,
    Package,
    Parameter,
    Port,
    Postponed,
    Procedure,
    Process,
    Property,
    Protected,
    Private,
    Pure,
    Range,
    Record,
    Register,
    Reject,
    Release,
    Rem,
    Report,
    Restrict,
    Return,
    Rol,
    Ror,
    Select,
    Sequence,
    Severity,
    Signal,
    Shared,
    Sla,
    Sll,
    Sra,
    Srl,
    Strong,
    Subtype,
    Then,
    To,
    Transport,
    Type,
    Unaffected,
    Units,
    Until,
    Use,
    Variable,
    View,
    Vpgk,
    Vmode,
    Vprop,
    Vunit,
    Wait,
    When,
    While,
    With,
    Xnor,
    Xor,
}

impl TokenKind {
    pub fn build_expression(&self) -> proc_macro2::TokenStream {
        match self {
            TokenKind::Keyword(kw) => {
                let kw_name = format_ident!("{}", kw.to_string());
                quote! {
                    Keyword(Kw::#kw_name)
                }
            }
            _ => format_ident!("{}", self.to_string()).into_token_stream(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub name: String,
    /// the occurrence of this token, i.e., whether this is the
    /// 1st, second, third, e.t.c. token in the parent node.
    pub nth: usize,
    pub repeated: bool,
}

impl Token {
    pub fn from_mapping(mapping: &Mapping, parent: &SequenceNode) -> Option<Self> {
        let repeated = mapping
            .get("repeated")
            .unwrap_or(&Value::Bool(false))
            .as_bool()?;
        let kind = if let Some(token) = mapping.get("token") {
            let tok_name = token
                .as_str()
                .unwrap_or_else(|| panic!("{token:?} not a string"));
            TokenKind::from_str(tok_name)
                .unwrap_or_else(|err| panic!("'{tok_name}' is not a token: {err}"))
        } else if let Some(keyword) = mapping.get("keyword") {
            let kw_name = keyword
                .as_str()
                .unwrap_or_else(|| panic!("{keyword:?} not a string"));
            let kw = Keyword::from_str(kw_name)
                .unwrap_or_else(|err| panic!("{kw_name} not a token: {err}"));
            TokenKind::Keyword(kw)
        } else {
            panic!("{mapping:?} does not contain 'token' or 'keyword'");
        };
        let default_name = Value::String(kind.to_string().to_case(Case::Snake));
        let name_val = mapping.get("name").unwrap_or(&default_name);

        let name = name_val
            .as_str()
            .unwrap_or_else(|| panic!("{:?} not a ", name_val))
            .to_owned();
        let nth = parent.count_of_token_kind(kind);
        Some(Token {
            kind,
            repeated,
            nth,
            name,
        })
    }

    pub fn from_yaml(yaml: &Value, parent: &SequenceNode) -> Option<Self> {
        Self::from_mapping(yaml.as_mapping()?, parent)
    }

    pub fn from_keyword(kw: &str, parent: &SequenceNode) -> Option<Self> {
        let kind = TokenKind::Keyword(Keyword::from_str(kw).ok()?);
        Some(Self {
            kind,
            name: kw.to_lowercase(),
            repeated: false,
            nth: parent.count_of_token_kind(kind),
        })
    }

    pub fn build_getter(&self) -> TokenStream {
        let function_name = format_ident!("{}_token", self.name);
        let kind_ident = self.kind.build_expression();
        let nth = Literal::usize_unsuffixed(self.nth);
        if self.repeated {
            assert_eq!(self.nth, 1);
            quote! {
                pub fn #function_name(&self) -> impl Iterator<Item = SyntaxToken>  + use<'_> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == #kind_ident)
                }
            }
        } else {
            quote! {
                pub fn #function_name(&self) -> Option<SyntaxToken> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == #kind_ident)
                        .nth(#nth)
                }
            }
        }
    }
}
