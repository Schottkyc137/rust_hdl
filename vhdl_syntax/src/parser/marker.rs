// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use std::num::NonZeroUsize;

use crate::{parser::Parser, syntax::NodeKind};

struct _Marker {
    pos: usize,
    #[cfg(debug_assertions)]
    fused: bool,
}

impl _Marker {
    fn new(pos: usize) -> _Marker {
        _Marker {
            pos,
            #[cfg(debug_assertions)]
            fused: true,
        }
    }

    fn defuse(&mut self) {
        #[cfg(debug_assertions)]
        {
            self.fused = false;
        }
    }
}

impl Drop for _Marker {
    fn drop(&mut self) {
        #[cfg(debug_assertions)]
        // Don't panic while another panic is unwinding: this aborts the process
        // and hides the original failure
        if self.fused && !std::thread::panicking() {
            panic!("node started but never completed");
        }
    }
}

/// A node that has been started but not yet finished.
#[must_use = "a started node must be completed"]
pub struct Marker {
    marker: _Marker,
    kind: NodeKind,
}

impl Marker {
    pub fn new(pos: usize, kind: NodeKind) -> Marker {
        Marker {
            marker: _Marker::new(pos),
            kind,
        }
    }

    fn pos(&self) -> usize {
        self.marker.pos
    }

    /// Finish the node. Everything pushed since the node was started becomes
    /// one of its children.
    pub fn complete(mut self, parser: &mut Parser) -> CompletedMarker {
        self.marker.defuse();
        parser.recovery.pop();
        parser.builder.end_node();
        CompletedMarker {
            start: self.marker.pos,
            kind: self.kind,
        }
    }
}

/// A node that has been started before its kind was known.
#[must_use = "an unknown node must be resolved"]
pub struct UnknownMarker(_Marker);

impl UnknownMarker {
    pub fn new(pos: usize) -> UnknownMarker {
        UnknownMarker(_Marker::new(pos))
    }

    /// Resolve the node's type
    pub fn resolve(self, parser: &mut Parser, kind: NodeKind) -> Marker {
        parser.builder.fix_node(self.0.pos, kind);
        parser.recovery.push(kind);
        Marker {
            marker: self.0,
            kind,
        }
    }

    /// Name the node's kind and finish it in one step.
    pub fn complete(self, parser: &mut Parser, kind: NodeKind) -> CompletedMarker {
        self.resolve(parser, kind).complete(parser)
    }
}

/// A node that has been finished. It can still gain a new parent, see [`Precede`].
#[derive(Debug, Clone, Copy)]
pub struct CompletedMarker {
    /// Index of the node's `Event::Start` in the event stream
    start: usize,
    kind: NodeKind,
}

impl CompletedMarker {
    pub fn kind(&self) -> NodeKind {
        self.kind
    }
}

pub trait Precede {
    fn precede(self, parser: &mut Parser, kind: NodeKind) -> Marker;
}

impl Precede for CompletedMarker {
    fn precede(self, parser: &mut Parser, kind: NodeKind) -> Marker {
        let marker = parser.start_node(kind);
        let distance =
            NonZeroUsize::new(marker.pos() - self.start).expect("a node cannot precede itself");
        parser.builder.fix_forward_parent(self.start, distance);
        marker
    }
}

impl Precede for Option<CompletedMarker> {
    fn precede(self, parser: &mut Parser, kind: NodeKind) -> Marker {
        match self {
            Some(marker) => marker.precede(parser, kind),
            None => parser.start_node(kind),
        }
    }
}
