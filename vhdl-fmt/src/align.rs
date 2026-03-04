use std::collections::HashMap;
use vhdl_syntax::{
    syntax::{
        NodeKind,
        node::{SyntaxElement, SyntaxNode, SyntaxToken},
        visitor::{PreorderWithTokens, WalkEvent},
    },
    tokens::TokenKind,
};

/// Maps tokens `text_pos` to the number of spaces that must precede it
/// so that colons within the same alignment group are in the same column.
#[derive(Debug, Clone, Default)]
pub struct AlignmentMap {
    inner: HashMap<usize, usize>,
}

impl AlignmentMap {
    pub fn new() -> AlignmentMap {
        AlignmentMap {
            inner: HashMap::new(),
        }
    }

    pub fn get(&self, token: &SyntaxToken) -> Option<usize> {
        self.inner.get(&token.text_pos()).copied()
    }

    pub fn insert(&mut self, token: SyntaxToken, spaces: usize) {
        self.inner.insert(token.text_pos(), spaces);
    }
}

/// Returns the TokenKind that a NodeKind should align to
fn get_alignment_target(kind: NodeKind) -> Option<TokenKind> {
    use vhdl_syntax::syntax::NodeKind as Nk;
    match kind {
        Nk::InterfaceList | Nk::RecordElementDeclarations => Some(TokenKind::Colon),
        Nk::AssociationList => Some(TokenKind::RightArrow),
        _ => None,
    }
}

/// For each node in `root` compute the alignment.
pub fn compute_alignment(root: &SyntaxNode) -> AlignmentMap {
    let mut map = AlignmentMap::new();
    for event in PreorderWithTokens::new(root.clone()) {
        if let WalkEvent::Enter(SyntaxElement::Node(node)) = event {
            if let Some(target) = get_alignment_target(node.kind()) {
                align_container(&node, target, &mut map);
            }
        }
    }
    map
}

/// Align all elements in the container to some target
fn align_container(container: &SyntaxNode, alignment_kind: TokenKind, map: &mut AlignmentMap) {
    // Collect (pre_colon_width, colon_text_pos) for each direct-child item.
    let items = container
        .children()
        .filter_map(|child| pre_alignment_info(&child, alignment_kind))
        .collect::<Vec<_>>();

    // Future: split `items` at group boundaries (blank line / comment in leading trivia
    // of the item's first token) and call apply_group_alignment for each sub-group.
    apply_group_alignment(&items, map);
}

/// Returns `(width_to_align, alignment_token)` for declaration-like nodes,
/// or `None` if the node has no direct descendant of `alignment_kind`.
fn pre_alignment_info(
    node: &SyntaxNode,
    alignment_kind: TokenKind,
) -> Option<(usize, SyntaxToken)> {
    let mut token_lens = 0usize;
    let mut alignment_token: Option<SyntaxToken> = None;

    for event in PreorderWithTokens::new(node.clone()) {
        if let WalkEvent::Enter(SyntaxElement::Token(token)) = event {
            if token.kind() == alignment_kind {
                alignment_token = Some(token);
                break;
            }
            token_lens += token.text().len();
        }
    }

    let token = alignment_token?;
    Some((token_lens, token))
}

fn apply_group_alignment(items: &[(usize, SyntaxToken)], map: &mut AlignmentMap) {
    let Some(max_width) = items.iter().map(|(w, _)| *w).max() else {
        return;
    };
    for (width, token) in items {
        // Pad each item so its colon lands at the same column as the widest item.
        let spaces = max_width - width;
        map.insert(token.clone(), spaces);
    }
}
