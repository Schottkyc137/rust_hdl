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

impl IndentStyle {
    pub fn to_trivia(&self, count: usize) -> TriviaPiece {
        match self {
            IndentStyle::Spaces => TriviaPiece::Spaces(count),
            IndentStyle::Tabs => TriviaPiece::HorizontalTabs(count),
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

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum UserBlankLinePolicy {
    /// Preserve user-supplied blank lines as-is (subject to formatter constraints).
    #[default]
    Preserve,
    // Future variants: Max(usize), Override(usize)
}

#[derive(Eq, PartialEq, Clone)]
pub struct Config {
    pub newline_at_end: bool,
    pub indentation: Indentation,
    pub newline_style: NewlineStyle,
    pub max_line_length: usize,
    pub blank_lines: UserBlankLinePolicy,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            newline_at_end: true,
            indentation: Indentation::default(),
            newline_style: NewlineStyle::default(),
            max_line_length: 80,
            blank_lines: UserBlankLinePolicy::default(),
        }
    }
}
