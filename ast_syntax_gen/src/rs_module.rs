// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::generate::Generate;
use proc_macro2::TokenStream;
use quote::{format_ident, quote};

pub struct Module {
    submodules: Vec<String>,
}

impl Module {
    pub fn new(submodules: Vec<String>) -> Module {
        Module { submodules }
    }
}

impl Generate for Module {
    fn generate(&self) -> TokenStream {
        self.submodules
            .iter()
            .map(|module| {
                let ident = format_ident!("{module}");
                quote! {
                    pub mod #ident;
                    pub use #ident::*;
                }
            })
            .collect::<TokenStream>()
    }
}
