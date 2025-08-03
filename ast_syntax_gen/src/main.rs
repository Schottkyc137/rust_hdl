use crate::node::Model;
use chrono::Datelike;
use std::error::Error;
use std::fs::OpenOptions;
use std::io;
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;
use std::process::Command;

fn write_header<W: Write>(write: &mut W) -> Result<(), io::Error> {
    let current_date = chrono::Utc::now();
    write!(
        write,
        "\
// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c) {}, Lukas Scheller lukasscheller@icloud.com
",
        current_date.year()
    )
}

mod node;
mod serialize;
mod serialized_to_model;
mod token;

fn main() -> Result<(), Box<dyn Error>> {
    let mut syntax_definitions = std::env::current_dir()?;

    syntax_definitions.push("src");
    syntax_definitions.push("syntax_definitions");

    let mut model = Model::default();

    for entry in std::fs::read_dir(&syntax_definitions)? {
        let entry = entry?;
        let path = entry.path();
        if let Some(extension) = path.extension() {
            if extension == "yaml" {
                let file = std::fs::File::open(&path)?;
                let nodes: crate::serialize::Nodes = serde_yml::from_reader(file)?;
                model.insert_ser_nodes(path.file_stem().unwrap().to_str().unwrap(), nodes);
            }
        }
    }

    model.check_no_duplicates();

    for (category, nodes) in model.into_sections() {
        let path = PathBuf::from(format!("{category}.rs"));
        let file = OpenOptions::new()
            .create(true)
            .truncate(true)
            .write(true)
            .open(&path)?;
        let mut writer = BufWriter::new(file);
        write_header(&mut writer)?;
        for node in nodes {
            writeln!(writer, "{}", node.generate_rust_struct())?;
            writeln!(writer, "{}", node.generate_ast_node_rust_impl())?;
            writeln!(writer, "{}", node.generate_rust_impl_getters())?;
        }
        Command::new("rustfmt").arg(path).spawn()?;
    }

    Ok(())
}
