// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::Generate;
use crate::node_with_children::TokenKind;
use proc_macro2::{Ident, TokenStream};
use quote::{format_ident, quote};

pub struct SubToken {
    pub kind: TokenKind,
    pub name: String,
}

impl SubToken {
    fn choice_name(&self) -> Ident {
        format_ident!("{}", self.name)
    }

    fn generate_enum_choice(&self) -> TokenStream {
        let name = self.choice_name();
        quote! {
            #name(SyntaxToken),
        }
    }

    fn token_kind(&self) -> TokenStream {
        self.kind.generate()
    }

    fn generate_cast_match_branch(&self, enum_name: Ident) -> TokenStream {
        let name = self.choice_name();
        let token_kind = self.token_kind();
        quote! {
            #token_kind => Some(#enum_name::#name(token)),
        }
    }

    fn generate_raw_match_branch(&self, enum_name: Ident) -> TokenStream {
        let name = self.choice_name();
        quote! {
            #enum_name::#name(inner) => inner.clone(),
        }
    }
}

pub struct NodeWithSubTokens {
    pub name: String,
    pub sub_tokens: Vec<SubToken>,
}

impl NodeWithSubTokens {
    fn enum_name(&self) -> Ident {
        format_ident!("{}Syntax", self.name)
    }

    fn generate_ast_enum(&self) -> TokenStream {
        let enum_name = self.enum_name();
        let choices = self
            .sub_tokens
            .iter()
            .map(|node| node.generate_enum_choice())
            .collect::<TokenStream>();
        let match_branches = self
            .sub_tokens
            .iter()
            .map(|node| node.generate_cast_match_branch(enum_name.clone()))
            .collect::<TokenStream>();
        let raw_match_branches = self
            .sub_tokens
            .iter()
            .map(|node| node.generate_raw_match_branch(enum_name.clone()))
            .collect::<TokenStream>();
        quote! {
            pub enum #enum_name {
                #choices
            }

            impl #enum_name {
                pub fn cast(token: SyntaxToken) -> Option<Self> {
                    match token.kind() {
                        #match_branches
                        _ => None
                    }
                }

                pub fn raw(&self) -> SyntaxToken {
                    match self {
                        #raw_match_branches
                    }
                }
            }
        }
    }
}

impl Generate for NodeWithSubTokens {
    fn generate(&self) -> TokenStream {
        self.generate_ast_enum()
    }
}
