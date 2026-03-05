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

/// How this node positions itself relative to its siblings.
#[derive(Debug, Default)]
pub enum SelfLayout {
    #[default]
    Default,
    OwnLine,
    Joined,
    SpaceJoined,
}

/// How this node organises its children.
#[derive(Debug, Default, PartialEq, Eq)]
pub enum ChildLayout {
    #[default]
    Default,
    /// All elements should be separated by spaces.
    SpaceSeparated,
    /// A list of Items (nodes) that are separated by tokens.
    ItemList,
    /// children are enclosed in an open and closed parenthesis.
    /// Parentheses is abstract here and can include other delimiters
    /// like brackets (`[`, `]`) or external name start / ends (`<<`, `>>`)
    Parenthesized,
}

#[derive(Debug, Default)]
pub struct NodeProp {
    /// Whether this node indents its children
    pub indents: bool,
    /// Whether this node groups its children (flat vs broken layout)
    pub groups: bool,
    /// How this node positions itself relative to its siblings
    pub self_layout: SelfLayout,
    /// How this node organises its children
    pub child_layout: ChildLayout,
}

impl NodeProp {
    pub fn own_line() -> Self {
        Self {
            self_layout: SelfLayout::OwnLine,
            ..Default::default()
        }
    }

    pub fn joined() -> Self {
        Self {
            self_layout: SelfLayout::Joined,
            ..Default::default()
        }
    }

    pub fn space_joined() -> Self {
        Self {
            self_layout: SelfLayout::SpaceJoined,
            ..Default::default()
        }
    }

    pub fn new() -> NodeProp {
        Self::default()
    }

    // Standard compound styles

    pub fn list() -> Self {
        Self {
            indents: true,
            groups: true,
            self_layout: SelfLayout::Default,
            child_layout: ChildLayout::ItemList,
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
        self.child_layout = ChildLayout::Parenthesized;
        self
    }

    pub fn space_separated_children(mut self) -> Self {
        self.child_layout = ChildLayout::SpaceSeparated;
        self
    }
}
