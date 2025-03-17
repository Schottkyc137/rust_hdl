//! A node with children.
//! Children of nodes are either sub-nodes or tokens.
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::Generate;
use proc_macro2::{Ident, Literal, TokenStream};
use quote::{format_ident, quote, ToTokens};
use std::collections::HashMap;
use std::ops::AddAssign;

pub struct NodeWithChildren {
    name: String,
    children: Vec<Child>,
}

pub struct InputChild {
    pub kind: ChildKind,
    pub name: String,
    pub fn_name: String,
    pub repeated: bool,
}

impl NodeWithChildren {
    pub fn new<const N: usize>(
        name: impl Into<String>,
        children: [InputChild; N],
    ) -> NodeWithChildren {
        let mut seen = HashMap::new();
        let mut actual_children = Vec::new();
        for child in children {
            seen.entry(child.kind.clone()).or_insert(0).add_assign(1);
            if seen[&child.kind] > 1 && child.repeated {
                panic!("repeated child {:?} occurred more than once", child.kind)
            }
            actual_children.push(Child {
                kind: child.kind.clone(),
                kind_name: child.name,
                fn_name: child.fn_name,
                occurrence: if child.repeated {
                    Occurrence::Repeated
                } else {
                    Occurrence::Nth(seen[&child.kind] - 1)
                },
            })
        }
        NodeWithChildren {
            name: name.into(),
            children: actual_children,
        }
    }
}

impl NodeWithChildren {
    fn struct_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name)
    }

    fn node_kind(&self) -> TokenStream {
        let kind_ident = format_ident!("{}", self.name);
        quote! { NodeKind::#kind_ident }
    }

    fn generate_ast_struct(&self) -> TokenStream {
        let struct_name = self.struct_name();
        let node_kind = self.node_kind();
        quote! {
            #[derive(Debug, Clone)]
            pub struct #struct_name(pub(crate) SyntaxNode);

            impl AstNode for #struct_name {
                fn cast(node: SyntaxNode) -> Option<Self> {
                    match node.kind() {
                        #node_kind => Some(#struct_name(node)),
                        _ => None,
                    }
                }

                fn raw(&self) -> SyntaxNode {
                    self.0.clone()
                }
            }
        }
    }

    fn generate_impls(&self) -> TokenStream {
        let struct_name = self.struct_name();
        let functions = self
            .children
            .iter()
            .map(|child| child.generate())
            .collect::<TokenStream>();
        quote! {
            impl #struct_name {
                #functions
            }
        }
    }
}

impl Generate for NodeWithChildren {
    fn generate(&self) -> TokenStream {
        let mut stream = self.generate_ast_struct();
        stream.extend(self.generate_impls());
        stream
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum TokenKind {
    Normal(String),
    Keyword(String),
}

impl TokenKind {
    pub fn generate(&self) -> TokenStream {
        match self {
            TokenKind::Normal(string) => format_ident!("{string}").into_token_stream(),
            TokenKind::Keyword(kw) => {
                let kw_ident = format_ident!("{kw}");
                quote! {
                    Keyword(Kw::#kw_ident)
                }
            }
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash, Debug)]
pub enum ChildKind {
    Node(String),
    Token(TokenKind),
    Tokens(String),
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Occurrence {
    Repeated,
    Nth(usize),
}

pub struct Child {
    pub kind: ChildKind,
    pub kind_name: String,
    pub fn_name: String,
    pub occurrence: Occurrence,
}

impl Child {
    fn fn_name(&self) -> Ident {
        format_ident!("{}", self.fn_name)
    }

    fn return_type(&self) -> Ident {
        if matches!(self.kind, ChildKind::Token(_)) {
            format_ident!("SyntaxToken")
        } else {
            format_ident!("{}", self.kind_name)
        }
    }

    fn function_impl(&self) -> TokenStream {
        match (self.occurrence, &self.kind) {
            (Occurrence::Nth(0), ChildKind::Token(kind)) => {
                let kind_name = kind.generate();
                quote! {
                    self.0.tokens().find(|token| token.kind() == #kind_name)
                }
            }
            (Occurrence::Nth(0), ChildKind::Node(kind)) => {
                let kind_name = format_ident!("{kind}");
                quote! {
                    self.0.children().find_map(#kind_name::cast)
                }
            }
            (Occurrence::Nth(0), ChildKind::Tokens(kind)) => {
                let kind_name = format_ident!("{kind}");
                quote! {
                    self.0.tokens().find_map(#kind_name::cast)
                }
            }
            (Occurrence::Nth(nth @ 1..), ChildKind::Token(kind)) => {
                let kind_name = kind.generate();
                let nth_lit = Literal::usize_unsuffixed(nth);
                quote! {
                    self.0.tokens().filter(|token| token.kind() == #kind_name).nth(#nth_lit)
                }
            }
            (Occurrence::Nth(nth @ 1..), ChildKind::Node(kind)) => {
                let kind_name = format_ident!("{kind}");
                let nth_lit = Literal::usize_unsuffixed(nth);
                quote! {
                    self.0.children().filter_map(#kind_name::cast).nth(#nth_lit)
                }
            }
            (Occurrence::Nth(nth @ 1..), ChildKind::Tokens(kind)) => {
                let kind_name = format_ident!("{kind}");
                let nth_lit = Literal::usize_unsuffixed(nth);
                quote! {
                    self.0.tokens().filter_map(#kind_name::cast).nth(#nth_lit)
                }
            }
            (Occurrence::Repeated, ChildKind::Node(kind)) => {
                let kind_name = format_ident!("{kind}");
                quote! {
                    self.0.children().filter_map(#kind_name::cast)
                }
            }
            (Occurrence::Repeated, ChildKind::Tokens(kind)) => {
                let kind_name = format_ident!("{kind}");
                quote! {
                    self.0.tokens().filter_map(#kind_name::cast)
                }
            }
            (Occurrence::Repeated, ChildKind::Token(kind)) => {
                let kind_name = kind.generate();
                quote! {
                    self.0.tokens().filter(|tok| tok.kind() == #kind_name)
                }
            }
        }
    }

    pub fn generate(&self) -> TokenStream {
        let fn_name = self.fn_name();
        let ret_type = self.return_type();
        let impl_stream = self.function_impl();
        if self.occurrence == Occurrence::Repeated {
            quote! {
                pub fn #fn_name(&self) -> impl Iterator<Item = #ret_type> + use<'_> {
                    #impl_stream
                }
            }
        } else {
            quote! {
                pub fn #fn_name(&self) -> Option<#ret_type> {
                    #impl_stream
                }
            }
        }
    }
}
