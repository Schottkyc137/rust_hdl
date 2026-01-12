use std::mem::take;

use vhdl_syntax::tokens::Trivia;

pub enum RegionSeparator {
    /// Separate the next region with spaces
    Space,
    /// Separate the next region with newlines
    Newline,
}

pub struct State {
    indent: usize,
    pending_separator: Option<RegionSeparator>,
    // Trailing trivia from the previous token.
    previous_trailing_trivia: Option<Trivia>,
}

impl Default for State {
    fn default() -> Self {
        Self {
            indent: 0,
            pending_separator: None,
            previous_trailing_trivia: None,
        }
    }
}

impl State {
    pub fn new() -> State {
        State::default()
    }

    pub fn indent(&mut self) {
        self.indent += 1;
    }

    pub fn dedent(&mut self) {
        self.indent -= 1;
    }

    pub fn current_indent(&self) -> usize {
        self.indent
    }

    pub fn set_pending_separator(&mut self, sep: RegionSeparator) {
        self.pending_separator = Some(sep)
    }

    pub fn get_and_reset_pending_separator(&mut self) -> Option<RegionSeparator> {
        take(&mut self.pending_separator)
    }

    pub fn set_previous_trailing_trivia(&mut self, pending_trivia: Trivia) {
        self.previous_trailing_trivia = Some(pending_trivia);
    }

    pub fn take_previoud_trailing_trivia(&mut self) -> Option<Trivia> {
        take(&mut self.previous_trailing_trivia)
    }
}
