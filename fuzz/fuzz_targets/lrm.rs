// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2026, Lukas Scheller lukasscheller@icloud.com

//! Fuzzes the parser against the LRM

#![no_main]

use libfuzzer_sys::fuzz_target;
use rust_hdl_fuzz::{assert_parses, Design, Grammar, GrammarSource};
use std::sync::LazyLock;

/// The unmodified LRM grammar, `xtask/doc/vhdl-08.ungram`.
pub struct Lrm;

impl GrammarSource for Lrm {
    fn grammar() -> &'static Grammar {
        static PREPARED: LazyLock<Grammar> = LazyLock::new(|| {
            Grammar::new(
                "xtask/doc/vhdl-08.ungram",
                include_str!("../../xtask/doc/vhdl-08.ungram"),
                &[
                    ("Name", b"name"),
                    ("SubtypeIndication", b"subtype_indication"),
                ],
            )
        });
        &PREPARED
    }
}

fuzz_target!(|design: Design<Lrm>| assert_parses(design));
