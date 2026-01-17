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

impl NewlineStyle {
    pub fn to_trivia(&self) -> TriviaPiece {
        self.to_trivia_n(1)
    }

    pub fn to_trivia_n(&self, n: usize) -> TriviaPiece {
        match self {
            NewlineStyle::LineFeed => TriviaPiece::LineFeeds(n),
            NewlineStyle::CarriageReturnLineFeed => TriviaPiece::CarriageReturnLineFeeds(n),
        }
    }
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
