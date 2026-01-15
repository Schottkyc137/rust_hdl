use vhdl_syntax::tokens::TriviaPiece;

#[derive(Debug, Default, Eq, PartialEq, Clone)]
pub enum IndentStyle {
    #[default]
    Spaces,
    Tabs,
}

#[derive(Eq, PartialEq, Clone)]
pub struct Indentation {
    pub style: IndentStyle,
    pub width: usize,
}

impl Indentation {
    pub(crate) fn to_trivia(&self, level: usize) -> TriviaPiece {
        match self.style {
            IndentStyle::Spaces => TriviaPiece::Spaces(self.width * level),
            IndentStyle::Tabs => TriviaPiece::HorizontalTabs(self.width * level),
        }
    }
}

impl Default for Indentation {
    fn default() -> Self {
        Self {
            style: IndentStyle::Spaces,
            width: 4,
        }
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum NewlineStyle {
    #[default]
    LineFeed,
    CarriageReturnLineFeed,
}

#[derive(Eq, PartialEq, Clone)]
pub struct Config {
    pub newline_at_end: bool,
    pub indentationn: Indentation,
    pub newline_style: NewlineStyle,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            newline_at_end: true,
            indentationn: Indentation::default(),
            newline_style: NewlineStyle::default(),
        }
    }
}
