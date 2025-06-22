use crate::node::Node;
use std::error::Error;
use std::io;
use std::io::prelude::*;

fn write_header<W: Write>(write: &mut W) -> Result<(), io::Error> {
    write!(
        write,
        "\
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com
"
    )
}

mod generate;
mod node;
mod node_with_children;
mod node_with_subnodes;
mod node_with_tokens;
mod rs_module;
mod serialize;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let mut current_dir = std::env::current_dir()?;

    current_dir.push("src");
    current_dir.push("syntax_definitions");

    for entry in std::fs::read_dir(&current_dir)? {
        let entry = entry?;
        let path = entry.path();
        if let Some(extension) = path.extension() {
            if extension == "yaml" {
                let file = std::fs::File::open(&path)?;
                let value: serde_yml::Value = serde_yml::from_reader(file)?;
                for (name, value) in value.as_mapping().unwrap() {
                    if let Some(sequence_node) = Node::from_key_value(
                        name.as_str().unwrap().to_owned(),
                        value.as_mapping().unwrap(),
                    ) {
                        println!("{}\n", sequence_node.generate_rust_struct());
                        println!("{}\n", sequence_node.generate_ast_node_rust_impl());
                        println!("{}\n", sequence_node.generate_rust_impl_getters());
                    }
                }
            }
        }
    }

    Ok(())
}
