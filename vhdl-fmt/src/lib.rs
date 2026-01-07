pub mod config;
pub mod formatter;
mod rule;
mod state;

use vhdl_syntax::syntax::node::SyntaxNode;

use crate::{config::Config, formatter::Formatter};

pub fn format(node: SyntaxNode) -> SyntaxNode {
    let mut formatter = Formatter::new(Config::default());
    formatter.format(node)
}
