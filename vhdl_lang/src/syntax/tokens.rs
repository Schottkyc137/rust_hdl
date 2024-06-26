// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) 2019, Olof Kraigher olof.kraigher@gmail.com

#[macro_use]
mod tokenizer;
/// Contains constant keywords for different versions of VHDL.
mod keywords;
mod tokenstream;

pub use tokenizer::*;
pub use tokenstream::*;
