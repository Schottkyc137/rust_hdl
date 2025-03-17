use crate::generate::Generate;
use crate::node_with_children::{ChildKind, InputChild, NodeWithChildren};
use crate::node_with_subnodes::{NodeWithSubNodes, SubNode};
use crate::node_with_tokens::{NodeWithSubTokens, SubToken};
use crate::rs_module::Module;
use convert_case::Case;
use convert_case::Casing;
use quote::quote;
use std::collections::HashMap;
use std::error::Error;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::process::Command;

macro_rules! keyword {
    ($name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Token(node_with_children::TokenKind::Keyword(
                stringify!($name).to_string(),
            )),
            name: format!("{}", stringify!($name)),
            fn_name: format!("{}_token", stringify!($name).to_case(Case::Snake)),
            repeated: false,
        }
    };
    ($name:ident, name = $alternate_name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Token(node_with_children::TokenKind::Keyword(
                stringify!($name).to_string(),
            )),
            name: format!("{}", stringify!($name)),
            fn_name: format!("{}_token", stringify!($alternate_name).to_case(Case::Snake)),
            repeated: false,
        }
    };
}

macro_rules! token {
    ($name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Token(node_with_children::TokenKind::Normal(
                stringify!($name).to_string(),
            )),
            name: format!("{}", stringify!($name)),
            fn_name: format!("{}_token", stringify!($name).to_case(Case::Snake)),
            repeated: false,
        }
    };
    ($name:ident, repeated) => {
        node_with_children::InputChild {
            kind: ChildKind::Token(node_with_children::TokenKind::Normal(
                stringify!($name).to_string(),
            )),
            name: format!("{}", stringify!($name)),
            fn_name: format!("{}_tokens", stringify!($name).to_case(Case::Snake)),
            repeated: true,
        }
    };
    ($name:ident, name = $alternate_name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Token(node_with_children::TokenKind::Normal(
                stringify!($name).to_string(),
            )),
            name: format!("{}", stringify!($name)),
            fn_name: format!("{}_token", stringify!($alternate_name).to_case(Case::Snake)),
            repeated: false,
        }
    };
}

macro_rules! subtoken {
    ($name:ident) => {
        node_with_tokens::SubToken {
            kind: node_with_children::TokenKind::Normal(stringify!($name).to_string()),
            name: format!("{}", stringify!($name)),
        }
    };
    ($name:ident, name=$alternate_name:ident) => {
        node_with_tokens::SubToken {
            kind: node_with_children::TokenKind::Normal(stringify!($name).to_string()),
            name: format!("{}", stringify!($alternate_name)),
        }
    };
}

macro_rules! subtoken_kw {
    ($name:ident) => {
        node_with_tokens::SubToken {
            kind: node_with_children::TokenKind::Keyword(stringify!($name).to_string()),
            name: format!("{}", stringify!($name)),
        }
    };
    ($name:ident, name=$alternate_name:ident) => {
        node_with_tokens::SubToken {
            kind: node_with_children::TokenKind::Keyword(stringify!($name).to_string()),
            name: format!("{}", stringify!($alternate_name)),
        }
    };
}

macro_rules! node {
    ($name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Node(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: stringify!($name).to_case(Case::Snake),
            repeated: false,
        }
    };
    ($name:ident repeated) => {
        node_with_children::InputChild {
            kind: ChildKind::Node(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: format!("{}s", stringify!($name)).to_case(Case::Snake),
            repeated: true,
        }
    };
    ($name:ident, name = $alternate_name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Node(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: stringify!($alternate_name).to_string(),
            repeated: false,
        }
    };
    ($name:ident, name = $alternate_name:ident repeated) => {
        node_with_children::InputChild {
            kind: ChildKind::Node(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: stringify!($alternate_name).to_string(),
            repeated: false,
        }
    };
}

macro_rules! token_node {
    ($name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Tokens(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: stringify!($name).to_case(Case::Snake),
            repeated: false,
        }
    };
    ($name:ident repeated) => {
        node_with_children::InputChild {
            kind: ChildKind::Tokens(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: format!("{}s", stringify!($name)).to_case(Case::Snake),
            repeated: true,
        }
    };
    ($name:ident, name = $alternate_name:ident) => {
        node_with_children::InputChild {
            kind: ChildKind::Tokens(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: stringify!($alternate_name).to_string(),
            repeated: false,
        }
    };
    ($name:ident, name = $alternate_name:ident repeated) => {
        node_with_children::InputChild {
            kind: ChildKind::Tokens(format!("{}Syntax", stringify!($name))),
            name: format!("{}Syntax", stringify!($name)),
            fn_name: stringify!($alternate_name).to_string(),
            repeated: false,
        }
    };
}

fn node_with_children<const N: usize>(
    name: impl Into<String>,
    children: [InputChild; N],
) -> Box<dyn Generate> {
    Box::new(NodeWithChildren::new(name, children))
}

fn node_with_tokens<const N: usize>(
    name: impl Into<String>,
    children: [SubToken; N],
) -> Box<dyn Generate> {
    Box::new(NodeWithSubTokens {
        name: name.into(),
        sub_tokens: children.into(),
    })
}

macro_rules! subnode {
    ($name:ident($kind:tt)) => {
        SubNode {
            name: stringify!($name).into(),
            kind: stringify!($kind).into(),
            node_kinds: vec![stringify!($kind).into()],
        }
    };
    ($($node_kind:ident)+ => $name:ident($kind:expr)) => {
        SubNode {
            name: stringify!($name).into(),
            kind: stringify!($kind).into(),
            node_kinds: vec![$(stringify!($node_kind).into()),+],
        }
    };
}

fn node_with_subnodes<const N: usize>(
    name: impl Into<String>,
    subnodes: [SubNode; N],
) -> Box<dyn Generate> {
    Box::new(NodeWithSubNodes {
        name: name.into(),
        sub_nodes: subnodes.into(),
    })
}

fn write_header<W: Write>(write: &mut W) -> Result<(), io::Error> {
    write!(
        write,
        "\
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
"
    )
}

mod generate;
mod node_with_children;
mod node_with_subnodes;
mod node_with_tokens;
mod rs_module;

fn main() -> Result<(), Box<dyn Error>> {
    let nodes = HashMap::from([
        (
            "alias_declaration",
            vec![node_with_children(
                "AliasDeclaration",
                [
                    keyword!(Alias),
                    token_node!(Designator),
                    token!(Colon),
                    node!(SubtypeIndication),
                    keyword!(Is),
                    node!(Name),
                    node!(Signature),
                    token!(SemiColon),
                ],
            )],
        ),
        (
            "names",
            vec![
                node_with_tokens(
                    "Designator",
                    [
                        subtoken!(Identifier),
                        subtoken!(StringLiteral),
                        subtoken!(CharacterLiteral),
                    ],
                ),
                node_with_subnodes("Name", []),
                node_with_children("AbsolutePathname", [token!(Dot), node!(PartialPathname)]),
                node_with_children(
                    "PartialPathname",
                    [
                        // TODO
                        ],
                ),
                node_with_children("Label", [token!(Identifier), token!(Colon)]),
                node_with_children("ExternalName", [token!(Identifier), token!(Colon)]),
            ],
        ),
        (
            "design",
            vec![
                node_with_children("DesignFile", [node!(DesignUnit repeated)]),
                node_with_children(
                    "DesignUnit",
                    [node!(ContextClause), node!(LibraryUnit repeated)],
                ),
                node_with_subnodes(
                    "LibraryUnit",
                    [
                        subnode!(EntityDeclaration => Primary(PrimaryUnit)),
                        // subnode!(Secondary(SecondaryUnit)),
                    ],
                ),
                node_with_subnodes(
                    "PrimaryUnit",
                    [
                        subnode!(Entity(EntityDeclaration)),
                        // subnode!(Configuration(ConfigurationDeclaration)),
                        // subnode!(Package(PackageDeclaration)),
                        // subnode!(PackageInstantiation(PackageInstantiationDeclaration)),
                        // subnode!(Context(ContextDeclaration)),
                    ],
                ),
                node_with_subnodes("ContextClause", []),
            ],
        ),
        (
            "entity",
            vec![
                node_with_children(
                    "EntityDeclaration",
                    [
                        keyword!(Entity),
                        token!(Identifier),
                        keyword!(Is),
                        node!(EntityHeader, name = header),
                        keyword!(Begin),
                        keyword!(End),
                        keyword!(Entity, name = final_entity),
                        token!(Identifier, name = final_identifier),
                        token!(SemiColon),
                    ],
                ),
                node_with_children("EntityHeader", [node!(GenericClause), node!(PortClause)]),
            ],
        ),
        ("signature", vec![node_with_children("Signature", [])]),
        ("subtype", vec![node_with_children("SubtypeIndication", [])]),
        (
            "interface",
            vec![
                node_with_children(
                    "GenericClause",
                    [
                        keyword!(Generic),
                        token!(LeftPar),
                        node!(InterfaceList),
                        token!(RightPar),
                        token!(SemiColon),
                    ],
                ),
                node_with_children(
                    "PortClause",
                    [
                        keyword!(Port),
                        token!(LeftPar),
                        node!(InterfaceList),
                        token!(RightPar),
                        token!(SemiColon),
                    ],
                ),
                node_with_children(
                    "InterfaceList",
                    [
                        node!(InterfaceDeclaration repeated),
                        token!(SemiColon, repeated),
                    ],
                ),
                node_with_subnodes(
                    "InterfaceDeclaration",
                    [subnode!(Object(InterfaceObjectDeclaration))],
                ),
                node_with_children(
                    "InterfaceObjectDeclaration",
                    [
                        token_node!(InterfaceClass),
                        node!(IdentifierList),
                        token!(Colon),
                        token_node!(Mode),
                        keyword!(Bus),
                    ],
                ),
                node_with_tokens(
                    "InterfaceClass",
                    [
                        subtoken_kw!(Constant),
                        subtoken_kw!(Signal),
                        subtoken_kw!(Variable),
                    ],
                ),
                node_with_tokens(
                    "Mode",
                    [
                        subtoken_kw!(In),
                        subtoken_kw!(Out),
                        subtoken_kw!(Inout),
                        subtoken_kw!(Buffer),
                        subtoken_kw!(Linkage),
                    ],
                ),
            ],
        ),
        (
            "identifier",
            vec![node_with_children(
                "IdentifierList",
                [token!(Identifier, repeated), token!(Comma, repeated)],
            )],
        ),
        (
            "types",
            vec![node_with_children(
                "AccessTypeDefinition",
                [keyword!(Access), node!(SubtypeIndication)],
            )],
        ),
    ]);

    let module = Module::new(nodes.keys().map(|name| name.to_string()).collect());

    let current_dir = std::env::current_dir()?;

    for (file_name, nodes) in nodes {
        let mut path = current_dir.clone();
        path.push(file_name);
        path.set_extension("rs");
        let mut file = OpenOptions::new()
            .write(true)
            .truncate(true)
            .create(true)
            .open(&path)
            .unwrap();
        write_header(&mut file)?;
        write!(
            file,
            "{}",
            quote! {
                use crate::syntax::generated::*;
                use crate::syntax::AstNode;
                use crate::syntax::node::{SyntaxNode, SyntaxToken};
                use crate::syntax::node_kind::NodeKind;
                use crate::tokens::TokenKind::*;
                use crate::tokens::Keyword as Kw;
            }
        )?;
        for node in nodes {
            write!(file, "{}", node.generate())?;
        }
        Command::new("rustfmt")
            .arg(path.to_string_lossy().as_ref())
            .spawn()?
            .wait()?;
    }
    let mut path = current_dir.clone();
    path.push("mod");
    path.set_extension("rs");
    let mut file = OpenOptions::new()
        .write(true)
        .truncate(true)
        .create(true)
        .open(&path)
        .unwrap();
    write_header(&mut file)?;
    write!(file, "{}", module.generate())?;
    Command::new("rustfmt")
        .arg(path.to_string_lossy().as_ref())
        .spawn()?
        .wait()?;
    Ok(())
}
