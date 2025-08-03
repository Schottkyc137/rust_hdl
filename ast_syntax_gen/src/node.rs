// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::serialize::NodeOrToken;
use crate::token::{Token, TokenKind};
use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenOrNode {
    Node(NodeRef),
    Token(Token),
}

impl From<TokenKind> for TokenOrNode {
    fn from(value: TokenKind) -> Self {
        TokenOrNode::Token(Token::from(value))
    }
}

impl From<NodeRef> for TokenOrNode {
    fn from(value: NodeRef) -> Self {
        TokenOrNode::Node(value)
    }
}

impl TokenOrNode {
    pub fn getter_name(&self) -> Ident {
        match self {
            TokenOrNode::Node(node) => node.getter_name(),
            TokenOrNode::Token(token) => token.getter_name(),
        }
    }
}

impl TokenOrNode {
    pub fn generate_rust_getter(&self) -> TokenStream {
        match self {
            TokenOrNode::Node(node) => node.build_getter(),
            TokenOrNode::Token(token) => token.build_getter(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeRef {
    pub kind: String,
    pub nth: usize,
    pub builtin: bool,
    pub repeated: bool,
    pub name: String,
}

impl From<String> for NodeRef {
    fn from(value: String) -> Self {
        NodeRef {
            kind: value.clone(),
            nth: 0,
            builtin: false,
            repeated: false,
            name: value,
        }
    }
}

impl NodeRef {
    fn getter_name(&self) -> Ident {
        format_ident!("{}", self.name.to_case(Case::Snake))
    }

    pub fn build_getter(&self) -> TokenStream {
        let fn_name = self.getter_name();
        let kind_name = format_ident!("{}Syntax", self.kind.to_string());
        let nth = Literal::usize_unsuffixed(self.nth);
        if self.repeated {
            assert_eq!(
                self.nth, 0,
                "node {self:?} is not at position 0 but is repeated"
            );
            quote! {
                pub fn #fn_name(&self) -> impl Iterator<Item = #kind_name>  + use<'_> {
                    self.0.children().filter_map(#kind_name::cast)
                }
            }
        } else {
            quote! {
                pub fn #fn_name(&self) -> Option<#kind_name> {
                    self.0.children().filter_map(#kind_name::cast).nth(#nth)
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Node {
    Items(SequenceNode),
    Choices(ChoiceNode),
    Alias(AliasNode),
}

impl Node {
    pub fn name(&self) -> String {
        match self {
            Node::Items(items) => items.name.clone(),
            Node::Choices(choices) => choices.name.clone(),
            Node::Alias(alias) => alias.name.clone(),
        }
    }
}

impl From<SequenceNode> for Node {
    fn from(value: SequenceNode) -> Self {
        Node::Items(value)
    }
}

impl From<ChoiceNode> for Node {
    fn from(value: ChoiceNode) -> Self {
        Node::Choices(value)
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SequenceNode {
    name: String,
    items: Vec<TokenOrNode>,
}

impl SequenceNode {
    pub fn new(name: impl Into<String>, items: Vec<TokenOrNode>) -> SequenceNode {
        SequenceNode {
            name: name.into().to_case(Case::UpperCamel),
            items,
        }
    }

    pub fn struct_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name.to_case(Case::UpperCamel))
    }

    pub fn generate_rust_struct(&self) -> TokenStream {
        let name = self.struct_name();
        quote! {
            #[derive(Debug, Clone)]
            pub struct #name(pub(crate) SyntaxNode);
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> TokenStream {
        let struct_name = self.struct_name();
        let node_kind = format_ident!("{}", self.name.to_case(Case::UpperCamel));
        quote! {
            impl AstNode for #struct_name {
                fn cast(node: SyntaxNode) -> Option<Self> {
                    match node.kind() {
                        NodeKind::#node_kind => Some(#struct_name(node)),
                        _ => None,
                    }
                }
                fn raw(&self) -> SyntaxNode {
                    self.0.clone()
                }
            }
        }
    }

    pub fn generate_rust_impl_getters(&self) -> proc_macro2::TokenStream {
        let items: TokenStream = self
            .items
            .iter()
            .map(|item| item.generate_rust_getter())
            .collect();
        let name = self.struct_name();
        quote! {
            impl #name {
                #items
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum NodesOrTokens {
    Nodes(Vec<NodeRef>),
    Tokens(Vec<Token>),
}

impl FromIterator<NodeRef> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = NodeRef>>(iter: T) -> Self {
        NodesOrTokens::Nodes(iter.into_iter().collect())
    }
}

impl FromIterator<Token> for NodesOrTokens {
    fn from_iter<T: IntoIterator<Item = Token>>(iter: T) -> Self {
        NodesOrTokens::Tokens(iter.into_iter().collect())
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct ChoiceNode {
    pub name: String,
    pub items: NodesOrTokens,
}

impl ChoiceNode {
    pub fn enum_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name.to_case(Case::UpperCamel))
    }

    fn enum_choices(&self) -> Vec<TokenStream> {
        match &self.items {
            NodesOrTokens::Nodes(nodes) => self.all_nodes_choices(nodes),
            NodesOrTokens::Tokens(tokens) => self.all_token_choices(tokens),
        }
    }

    fn all_nodes_choices(&self, items: &[NodeRef]) -> Vec<TokenStream> {
        items
            .iter()
            .map(|item| {
                let name = format_ident!("{}", item.name);
                let syntax_name = format_ident!("{}", item.name);
                quote! {
                    #name(#syntax_name)
                }
            })
            .collect()
    }

    fn all_token_choices(&self, items: &[Token]) -> Vec<proc_macro2::TokenStream> {
        items
            .into_iter()
            .map(|item| {
                let name = format_ident!("{}", item.name);
                quote! {
                    #name(SyntaxToken)
                }
            })
            .collect()
    }

    pub fn generate_rust_enum(&self) -> proc_macro2::TokenStream {
        let name = self.enum_name();
        let choices: TokenStream = self
            .enum_choices()
            .into_iter()
            .map(|x| quote! { #x , })
            .collect();
        quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #choices
            }
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> TokenStream {
        let name = self.enum_name();
        match &self.items {
            NodesOrTokens::Nodes(nodes) => {
                let cast_branches: TokenStream = nodes.iter().map(|item| {
                    let node_kind = format_ident!("{}", item.kind);
                    let enum_variant = format_ident!("{}Syntax", item.kind);
                    quote! {
                    NodeKind::#node_kind => Some(#name::#enum_variant(#node_kind::cast(node).unwrap())),
            }
                }).collect();
                let raw_branches: TokenStream = nodes
                    .iter()
                    .map(|item| {
                        let enum_variant = format_ident!("{}Syntax", item.kind);
                        quote! {
                                #name::#enum_variant(inner) => inner.raw(),
                        }
                    })
                    .collect();
                quote! {
                    impl AstNode for #name {
                        fn cast(node: SyntaxNode) -> Option<Self> {
                            match node.kind() {
                                #cast_branches
                                _ => None,
                            }
                        }
                        fn raw(&self) -> SyntaxNode {
                             match self {
                                #raw_branches
                            }
                        }
                    }
                }
            }
            NodesOrTokens::Tokens(tokens) => {
                let cast_branches: TokenStream = tokens
                    .iter()
                    .map(|item| {
                        let token_kind = item.kind.build_expression();
                        let enum_variant = format_ident!("{}", item.name);
                        quote! {
                            #token_kind => Some(#name::#enum_variant(token)),
                        }
                    })
                    .collect();
                let raw_branches: TokenStream = tokens
                    .iter()
                    .map(|item| {
                        let enum_variant = format_ident!("{}", item.name);
                        quote! {
                                #name::#enum_variant(token) => token.clone(),
                        }
                    })
                    .collect();
                quote! {
                    impl #name {
                        fn cast(token: SyntaxToken) -> Option<Self> {
                            match token.kind() {
                                #cast_branches
                                _ => None,
                            }
                        }
                        fn raw(&self) -> SyntaxToken {
                             match self {
                                #raw_branches
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct AliasNode {
    pub name: String,
    pub item: TokenOrNode,
}

impl AliasNode {
    pub fn generate_rust_alias(&self) -> TokenStream {
        let alias = format_ident!("{}", self.name.to_case(Case::UpperCamel));
        let target = match &self.item {
            TokenOrNode::Node(node) => format_ident!("{}", node.name.to_case(Case::UpperCamel)),
            TokenOrNode::Token(token) => format_ident!("{}", token.name.to_case(Case::UpperCamel)),
        };
        quote! {
            type #alias = #target;
        }
    }
}

impl Node {
    pub fn generate_rust_struct(&self) -> TokenStream {
        match self {
            Node::Items(items) => items.generate_rust_struct(),
            Node::Choices(choices) => choices.generate_rust_enum(),
            Node::Alias(alias) => alias.generate_rust_alias(),
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> TokenStream {
        match self {
            Node::Items(items) => items.generate_ast_node_rust_impl(),
            Node::Choices(choices) => choices.generate_ast_node_rust_impl(),
            Node::Alias(_) => quote! {},
        }
    }

    pub fn generate_rust_impl_getters(&self) -> TokenStream {
        match self {
            Node::Items(items) => items.generate_rust_impl_getters(),
            Node::Choices(_) => quote! {},
            Node::Alias(_) => quote! {},
        }
    }
}

#[derive(Debug, Default)]
pub struct Model {
    sections: HashMap<String, Vec<Node>>,
}

impl Model {
    pub fn push_node(&mut self, section: String, node: impl Into<Node>) {
        self.sections.entry(section).or_default().push(node.into())
    }

    pub fn into_sections(self) -> HashMap<String, Vec<Node>> {
        self.sections
    }

    pub fn check_no_duplicates(&self) {
        for (section, nodes) in &self.sections {
            for node in nodes {
                let mut seen = HashSet::new();
                match node {
                    Node::Items(seq_node) => {
                        for item in &seq_node.items {
                            if seen.contains(&item.getter_name()) {
                                panic!(
                                    "Duplicate node {} in node {} (section {})",
                                    item.getter_name(),
                                    node.name(),
                                    section
                                )
                            }
                            seen.insert(item.getter_name());
                        }
                    }
                    Node::Choices(choices_node) => match &choices_node.items {
                        NodesOrTokens::Nodes(nodes) => {
                            for item in nodes {
                                if seen.contains(&item.getter_name()) {
                                    panic!(
                                        "Duplicate node {} in node {}",
                                        item.getter_name(),
                                        node.name()
                                    )
                                }
                                seen.insert(item.getter_name());
                            }
                        }
                        NodesOrTokens::Tokens(tokens) => {
                            for item in tokens {
                                if seen.contains(&item.getter_name()) {
                                    panic!(
                                        "Duplicate node {} in node {}",
                                        item.getter_name(),
                                        node.name()
                                    )
                                }
                                seen.insert(item.getter_name());
                            }
                        }
                    },
                    Node::Alias(_) => {}
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::node::{Node, NodeRef, SequenceNode, TokenOrNode};
    use crate::token::{Keyword, Token, TokenKind};
    use quote::quote;

    #[test]
    fn test_node_generates_correct_struct() {
        let node = Node::Items(SequenceNode {
            name: "EntityDeclaration".to_owned(),
            items: vec![
                TokenOrNode::Token(Token {
                    name: "entity".to_owned(),
                    kind: TokenKind::Keyword(Keyword::Entity),
                    repeated: false,
                    nth: 0,
                }),
                TokenOrNode::Token(Token {
                    name: "identifier".to_owned(),
                    kind: TokenKind::Identifier,
                    repeated: false,
                    nth: 0,
                }),
                TokenOrNode::Node(NodeRef {
                    kind: "EntityHeader".to_owned(),
                    nth: 0,
                    repeated: false,
                    name: "entity_header".to_string(),
                    builtin: false,
                }),
            ],
        });

        assert_eq!(
            node.generate_rust_struct().to_string(),
            quote! {
                #[derive(Debug, Clone)]
                pub struct EntityDeclarationSyntax(pub(crate) SyntaxNode);
            }
            .to_string()
        );

        assert_eq!(
            node.generate_ast_node_rust_impl().to_string(),
            quote! {
                impl AstNode for EntityDeclarationSyntax {
                    fn cast(node: SyntaxNode) -> Option<Self> {
                        match node.kind() {
                            NodeKind::EntityDeclaration => Some(EntityDeclarationSyntax(node)),
                            _ => None,
                        }
                    }
                    fn raw(&self) -> SyntaxNode {
                        self.0.clone()
                    }
                }
            }
            .to_string()
        );

        assert_eq!(
            node.generate_rust_impl_getters().to_string(),
            quote! {
                pub fn entity_token(&self) -> Option<SyntaxToken> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == Keyword(Kw::Entity))
                        .nth(0)
                }
                pub fn identifier_token(&self) -> Option<SyntaxToken> {
                    self.0
                        .tokens()
                        .filter(|token| token.kind() == Identifier)
                        .nth(0)
                }
                pub fn entity_header(&self) -> Option<EntityHeaderSyntax> {
                    self.0.children().filter_map(EntityHeaderSyntax::cast).nth(0)
                }
            }
            .to_string()
        );
    }
}
