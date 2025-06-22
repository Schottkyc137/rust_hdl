// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2025, Lukas Scheller lukasscheller@icloud.com

use crate::token::{Token, TokenKind};
use convert_case::{Case, Casing};
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote};
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use serde_yml::{Mapping, Value};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TokenOrNode {
    Node(NodeRef),
    Token(Token),
}

impl TokenOrNode {
    pub fn generate_rust_getter(&self) -> proc_macro2::TokenStream {
        match self {
            TokenOrNode::Node(node) => node.build_getter(),
            TokenOrNode::Token(token) => token.build_getter(),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct NodeRef {
    kind: String,
    nth: usize,
    builtin: bool,
    repeated: bool,
    name: String,
}

impl NodeRef {
    pub fn from_mapping(mapping: &Mapping, parent: &SequenceNode) -> Option<Self> {
        let node = mapping
            .get("node")
            .unwrap_or_else(|| panic!("{mapping:?} does not contain 'node'"));
        let kind = node
            .as_str()
            .unwrap_or_else(|| panic!("{node:?} not a string"))
            .to_owned();
        let builtin = mapping
            .get("builtin")
            .unwrap_or(&Value::Bool(false))
            .as_bool()?;
        let repeated = mapping
            .get("repeated")
            .unwrap_or(&Value::Bool(false))
            .as_bool()?;
        let name = mapping
            .get("name")
            .unwrap_or(&Value::String(kind.to_string().to_case(Case::Snake)))
            .as_str()?
            .to_owned();
        let nth = parent.count_of_node(&kind);
        Some(NodeRef {
            kind,
            builtin,
            nth,
            repeated,
            name,
        })
    }

    pub fn from_yaml(yaml: &Value, parent: &SequenceNode) -> Option<Self> {
        Self::from_mapping(yaml.as_mapping()?, parent)
    }

    pub fn build_getter(&self) -> TokenStream {
        let fn_name = format_ident!("{}", self.name);
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
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct SequenceNode {
    name: String,
    items: Vec<TokenOrNode>,
}

impl SequenceNode {
    pub fn count_of_token_kind(&self, kind: TokenKind) -> usize {
        self.items
            .iter()
            .filter(|item| match item {
                TokenOrNode::Token(tok) => tok.kind == kind,
                _ => false,
            })
            .count()
    }

    pub fn count_of_node(&self, node: &str) -> usize {
        self.items
            .iter()
            .filter(|item| match item {
                TokenOrNode::Node(node_ref) => node_ref.kind == node,
                _ => false,
            })
            .count()
    }

    pub fn struct_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name.to_case(Case::UpperCamel))
    }

    pub fn generate_rust_struct(&self) -> proc_macro2::TokenStream {
        let name = self.struct_name();
        quote! {
            #[derive(Debug, Clone)]
            pub struct #name(pub(crate) SyntaxNode);
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> proc_macro2::TokenStream {
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
pub struct ChoiceNode {
    name: String,
    items: Vec<NodeRef>,
}

impl ChoiceNode {
    pub fn enum_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name.to_case(Case::UpperCamel))
    }

    fn enum_choices(&self) -> impl Iterator<Item = proc_macro2::TokenStream> + use<'_> {
        self.items.iter().map(|item| {
            let name = format_ident!("{}", item.name);
            let syntax_name = format_ident!("{}Syntax", item.name);
            quote! {
                #name(#syntax_name)
            }
        })
    }

    pub fn generate_rust_enum(&self) -> proc_macro2::TokenStream {
        let name = self.enum_name();
        let choices: TokenStream = self.enum_choices().map(|x| quote! { #x , }).collect();
        quote! {
            #[derive(Debug, Clone)]
            pub enum #name {
                #choices
            }
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> proc_macro2::TokenStream {
        let name = self.enum_name();
        let cast_branches: TokenStream = self.items.iter().map(|item| {
            let node_kind = format_ident!("{}", item.kind);
            let enum_variant = format_ident!("{}Syntax", item.kind);
            quote! {
                    NodeKind::#node_kind => Some(#name::#enum_variant(#node_kind::cast(node).unwrap())),
            }
        }).collect();
        let raw_branches: TokenStream = self
            .items
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
}

impl Node {
    pub fn from_key_value(name: String, mapping: &Mapping) -> Option<Self> {
        if let Some(seq) = mapping.get("sequence") {
            let mut node = SequenceNode {
                items: Vec::default(),
                name,
            };
            for element in seq
                .as_sequence()
                .unwrap_or_else(|| panic!("{:?} not a sequence", seq))
            {
                let mapping = element
                    .as_mapping()
                    .unwrap_or_else(|| panic!("{:?} not a mapping", element));
                if mapping.contains_key("node") {
                    node.items
                        .push(TokenOrNode::Node(NodeRef::from_mapping(mapping, &node)?));
                } else if mapping.contains_key("token") {
                    node.items
                        .push(TokenOrNode::Token(Token::from_mapping(mapping, &node)?));
                } else if mapping.contains_key("keyword") {
                    node.items.push(TokenOrNode::Token(Token::from_keyword(
                        mapping.get("keyword")?.as_str()?,
                        &node,
                    )?));
                } else {
                    panic!("Mapping {:?} not a node or token", mapping)
                }
            }
            Some(Node::Items(node))
        } else if let Some(choices) = mapping.get("choices") {
            let mut node = ChoiceNode {
                items: Vec::default(),
                name,
            };
            for element in choices
                .as_sequence()
                .unwrap_or_else(|| panic!("{:?} not a sequence", choices))
            {
                node.items.push(NodeRef::from_yaml(
                    element,
                    &SequenceNode {
                        name: "dummy".to_string(),
                        items: vec![],
                    },
                )?)
            }
            Some(Node::Choices(node))
        } else {
            panic!("{mapping:?} does not contain 'choices' or 'sequence'");
        }
    }

    pub fn generate_rust_struct(&self) -> proc_macro2::TokenStream {
        match self {
            Node::Items(items) => items.generate_rust_struct(),
            Node::Choices(choices) => choices.generate_rust_enum(),
        }
    }

    pub fn generate_ast_node_rust_impl(&self) -> proc_macro2::TokenStream {
        match self {
            Node::Items(items) => items.generate_ast_node_rust_impl(),
            Node::Choices(choices) => choices.generate_ast_node_rust_impl(),
        }
    }

    pub fn generate_rust_impl_getters(&self) -> proc_macro2::TokenStream {
        match self {
            Node::Items(items) => items.generate_rust_impl_getters(),
            Node::Choices(_) => quote! {},
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::node::{Node, NodeRef, SequenceNode, TokenOrNode};
    use crate::token::{Keyword, Token, TokenKind};
    use quote::quote;
    use std::error::Error;

    macro_rules! check_node_ref {
        ($name:ident, $str:literal, $exp:expr) => {
            #[test]
            fn $name() -> Result<(), Box<dyn Error>> {
                let yaml = $str;
                let node = NodeRef::from_yaml(
                    &serde_yml::from_str(yaml)?,
                    &SequenceNode {
                        items: Vec::new(),
                        name: "parent".to_owned(),
                    },
                );
                assert_eq!(node, Some($exp));
                Ok(())
            }
        };
    }

    check_node_ref!(
        node_ref_mapped_from_yaml,
        "node: EntityHeader",
        NodeRef {
            builtin: false,
            nth: 0,
            repeated: false,
            kind: "EntityHeader".to_owned(),
            name: "entity_header".to_owned()
        }
    );
    check_node_ref!(
        builtin_node_ref_from_yaml,
        "
node: EntityHeader
builtin: true
",
        NodeRef {
            builtin: true,
            nth: 0,
            repeated: false,
            kind: "EntityHeader".to_owned(),
            name: "entity_header".to_owned()
        }
    );

    #[test]
    fn simple_node_sequence() -> Result<(), Box<dyn Error>> {
        let yaml = r#"
sequence:
  - keyword: Entity
  - node: EntityHeader
"#;
        let node = Node::from_key_value("Foo".to_owned(), &serde_yml::from_str(yaml)?);
        assert_eq!(
            node,
            Some(Node::Items(SequenceNode {
                name: "Foo".to_owned(),
                items: vec![
                    TokenOrNode::Token(Token {
                        kind: TokenKind::Keyword(Keyword::Entity),
                        nth: 0,
                        repeated: false,
                        name: "entity".to_owned()
                    }),
                    TokenOrNode::Node(NodeRef {
                        kind: "EntityHeader".to_owned(),
                        nth: 0,
                        repeated: false,
                        builtin: false,
                        name: "entity_header".to_owned()
                    })
                ]
            }))
        );
        Ok(())
    }

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

trait Only<T> {
    fn only(&mut self) -> Option<T>;
}

impl<E, T> Only<T> for E
where
    E: ExactSizeIterator<Item = T>,
{
    fn only(&mut self) -> Option<T> {
        if self.len() != 1 {
            None
        } else {
            self.next()
        }
    }
}
