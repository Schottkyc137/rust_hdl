use vhdl_syntax::tokens::TriviaPiece;

#[derive(Debug, Default)]
pub enum IndentStyle {
    #[default]
    Spaces,
    Tabs,
}

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

#[derive(Debug, Default)]
pub enum NewlineStyle {
    #[default]
    LineFeed,
    CarriageReturnLineFeed,
}

impl NewlineStyle {
    pub(crate) fn to_trivia(&self) -> TriviaPiece {
        match self {
            NewlineStyle::LineFeed => TriviaPiece::LineFeeds(1),
            NewlineStyle::CarriageReturnLineFeed => TriviaPiece::CarriageReturnLineFeeds(1),
        }
    }
}

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
