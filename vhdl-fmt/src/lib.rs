mod align;
pub mod config;
mod doc_ir;
pub mod formatter;

use vhdl_syntax::syntax::node::SyntaxNode;

use crate::{config::Config, formatter::Formatter};

pub fn format(node: SyntaxNode) -> SyntaxNode {
    format_with_config(node, Config::default())
}

pub fn format_with_config(node: SyntaxNode, config: Config) -> SyntaxNode {
    let mut formatter = Formatter::new(config);
    formatter.format(node)
}
