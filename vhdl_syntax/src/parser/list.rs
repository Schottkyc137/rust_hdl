// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use crate::parser::marker::CompletedMarker;
use crate::parser::Parser;
use crate::syntax::NodeKind;
use crate::tokens::TokenKind;

impl Parser {
    pub(crate) fn separated_list<T>(
        &mut self,
        node: NodeKind,
        element: impl Fn(&mut Parser) -> T,
        separator: TokenKind,
    ) -> CompletedMarker {
        self.node(node, |p| {
            element(p);
            while p.opt_token(separator) {
                element(p);
            }
        })
    }
}
