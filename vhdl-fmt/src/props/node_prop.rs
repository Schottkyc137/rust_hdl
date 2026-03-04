#[derive(Debug, Default)]
pub enum BreakKind {
    /// Not set, copy user trivia
    /// This is a temporary solution until the parser can deal with every syntax node
    #[default]
    Unset,
    /// Hard break
    Hard,
    /// Soft break. Either flat with N spaces or broken with a newline
    Soft { flat_spaces: usize },
    /// No break. This node should be formatted directly next to another
    Void,
    /// Break with N spaces
    Spaces(usize),
}

impl BreakKind {
    pub fn priority(&self) -> usize {
        match self {
            BreakKind::Unset => 0,
            BreakKind::Hard => 4,
            BreakKind::Soft { .. } => 3,
            BreakKind::Void => 1,
            BreakKind::Spaces(_) => 2,
        }
    }
}

#[derive(Debug, Default)]
pub struct NodeProp {
    /// Whether this node indents its children
    pub indents: bool,
    /// Whether this node groups its children (flat vs broken layout)
    pub groups: bool,
    /// Whether this node's first token is `(` and last token is `)`.
    pub parenthesized: bool,
    /// How to break before this node
    pub break_kind: BreakKind,
    /// How to break after this node
    pub break_kind_after: BreakKind,
    /// How to break the children of this node
    pub child_break_kind: BreakKind,
}

impl NodeProp {
    fn with_break_kind(break_kind: BreakKind) -> Self {
        Self {
            break_kind,
            ..Default::default()
        }
    }

    pub fn newline_before() -> Self {
        Self::with_break_kind(BreakKind::Hard)
    }

    pub fn spaces_before(n: usize) -> Self {
        Self::with_break_kind(BreakKind::Spaces(n))
    }

    pub fn soft_before(flat_spaces: usize) -> Self {
        Self::with_break_kind(BreakKind::Soft { flat_spaces })
    }

    pub fn nothing_before() -> Self {
        Self::with_break_kind(BreakKind::Void)
    }

    pub fn new() -> NodeProp {
        Self::default()
    }


    // Standard compound styles

    pub fn list() -> Self {
        Self {
            indents: true,
            groups: true,
            parenthesized: false,
            break_kind: BreakKind::Void,
            break_kind_after: BreakKind::Soft { flat_spaces: 0 },
            child_break_kind: BreakKind::Unset,
        }
    }

    // Modifiers

    pub fn indents(mut self) -> Self {
        self.indents = true;
        self
    }

    pub fn groups(mut self) -> Self {
        self.groups = true;
        self
    }

    pub fn parenthesized(mut self) -> Self {
        self.parenthesized = true;
        self
    }

    pub fn space_separated_children(mut self) -> Self {
        self.child_break_kind = BreakKind::Spaces(1);
        self
    }

    pub fn soft_break_after(mut self, flat_spaces: usize) -> Self {
        self.break_kind_after = BreakKind::Soft { flat_spaces };
        self
    }
}
