mod align;
pub mod config;
mod doc_ir;
pub mod formatter;

use vhdl_syntax::syntax::node::SyntaxNode;

use crate::{config::Config, formatter::Formatter};

pub fn format(node: SyntaxNode) -> SyntaxNode {
    let mut formatter = Formatter::new(Config::default());
    formatter.format(node)
}
