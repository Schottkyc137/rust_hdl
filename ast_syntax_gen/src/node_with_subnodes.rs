// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::Generate;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

pub struct SubNode {
    pub name: String,
    pub kind: String,
    pub node_kinds: Vec<String>,
}

impl SubNode {
    fn enum_name(&self) -> Ident {
        format_ident!("{}", self.name)
    }

    fn kind_name(&self) -> Ident {
        format_ident!("{}Syntax", self.kind)
    }

    fn node_kind(&self) -> impl Iterator<Item = Ident> + use<'_> {
        self.node_kinds.iter().map(|kind| format_ident!("{kind}"))
    }

    pub fn generate_enum_choice(&self) -> TokenStream {
        let name = self.enum_name();
        let kind = self.kind_name();
        quote! {
            #name(#kind),
        }
    }

    pub fn generate_cast_match_branch(&self, enum_name: Ident) -> TokenStream {
        let name = self.enum_name();
        let syntax_name = format_ident!("{}Syntax", self.kind);
        self.node_kind()
            .map(|kind| {
                quote! {
                    NodeKind::#kind => Some(#enum_name::#name(#syntax_name::cast(node).unwrap())),
                }
            })
            .collect()
    }

    pub fn generate_raw_match_branch(&self, enum_name: Ident) -> TokenStream {
        let name = self.enum_name();
        quote! {
            #enum_name::#name(inner) => inner.raw(),
        }
    }
}

pub struct NodeWithSubNodes {
    pub name: String,
    pub sub_nodes: Vec<SubNode>,
}

impl NodeWithSubNodes {
    fn enum_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name)
    }

    fn generate_ast_enum(&self) -> TokenStream {
        let enum_name = self.enum_name();
        let choices = self
            .sub_nodes
            .iter()
            .map(|node| node.generate_enum_choice())
            .collect::<TokenStream>();
        let match_branches = self
            .sub_nodes
            .iter()
            .map(|node| node.generate_cast_match_branch(enum_name.clone()))
            .collect::<TokenStream>();
        let raw_match_branches = if self.sub_nodes.is_empty() {
            quote! {_ => unreachable!()}
        } else {
            self.sub_nodes
                .iter()
                .map(|node| node.generate_raw_match_branch(enum_name.clone()))
                .collect::<TokenStream>()
        };

        quote! {
            pub enum #enum_name {
                #choices
            }

            impl AstNode for #enum_name {
                fn cast(node: SyntaxNode) -> Option<Self> {
                    match node.kind() {
                        #match_branches
                        _ => None,
                    }
                }

                fn raw(&self) -> SyntaxNode {
                    match self {
                        #raw_match_branches
                    }
                }
            }
        }
    }
}

impl Generate for NodeWithSubNodes {
    fn generate(&self) -> TokenStream {
        self.generate_ast_enum()
    }
}
