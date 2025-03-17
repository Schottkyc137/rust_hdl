// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::Parser;
use crate::syntax::node_kind::NodeKind::PackageInstantiationDeclaration;
use crate::tokens::TokenKind::{Keyword, SemiColon};
use crate::tokens::TokenStream;

impl<T: TokenStream> Parser<T> {
    pub fn package_instantiation_declaration(&mut self) {
        self.start_node(PackageInstantiationDeclaration);
        self.expect_kw(crate::tokens::token_kind::Keyword::Package);
        self.identifier();
        self.expect_tokens([
            Keyword(crate::tokens::token_kind::Keyword::Is),
            Keyword(crate::tokens::token_kind::Keyword::New),
        ]);
        self.name();
        self.opt_generic_map_aspect();
        self.expect_token(SemiColon);
        self.end_node();
    }
}
