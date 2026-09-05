// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2024, Lukas Scheller lukasscheller@icloud.com

use std::io::{self, Write};

/// A comment
///
/// Because VHDL allows comments to have any encoding,
/// this implementation makes no assumption as to that and is simply
/// backed by bytes. Utility methods exist to get the value with different
/// encodings.
#[derive(Clone, Eq, PartialEq, Debug)]
pub struct Comment {
    inner: Vec<u8>,
}

impl Comment {
    /// Creates a block comment with leading and trailing delimiters (`/*` and `*/`) already included
    /// 
    /// # Example
    /// 
    /// ```
    /// # use vhdl_syntax::tokens::trivia_piece::Comment;
    /// 
    /// let comment = Comment::block(b"Hello");
    /// assert_eq!(comment.as_bytes(), b"/*Hello*/");
    /// ```
    pub fn block(bytes: impl AsRef<[u8]>) -> Comment {
        let mut vec = Vec::new();
        vec.extend_from_slice(b"/*");
        vec.extend_from_slice(bytes.as_ref());
        vec.extend_from_slice(b"*/");
        Comment::from_raw(vec)
    }

    /// Creates a line comment, including the leading `--` separator
    /// 
    /// # Example
    /// 
    /// ```
    /// # use vhdl_syntax::tokens::trivia_piece::Comment;
    /// 
    /// let comment = Comment::line(b"Hello");
    /// assert_eq!(comment.as_bytes(), b"--Hello");
    /// ```
    pub fn line(bytes: impl AsRef<[u8]>) -> Comment {
        let mut vec = Vec::new();
        vec.extend_from_slice(b"--");
        vec.extend_from_slice(bytes.as_ref());
        Comment::from_raw(vec)
    }

    /// Creates a comment without leading or trailing delimiters, i.e.,
    /// to create a syntactically correct comment you must provide those yourself.
    /// Prefer [Comment::block] or [Comment::line] for safe alternatives.
    /// 
    /// # Example
    /// 
    /// ```
    /// # use vhdl_syntax::tokens::trivia_piece::Comment;
    /// 
    /// // from_raw requires supplying the the delimiters:
    /// assert_eq!(Comment::from_raw(b"--Hello"), Comment::line(b"Hello"));
    ///  assert_eq!(Comment::from_raw(b"/*World*/"), Comment::block(b"World"));
    /// 
    /// // from_raw allows creation of illegal or unterminated comments
    /// let illegal = Comment::from_raw(b"Hello");
    /// assert_eq!(illegal.as_bytes(), b"Hello");
    /// 
    /// let unterminated = Comment::from_raw(b"/* Hello");
    /// assert_eq!(unterminated.as_bytes(), b"/* Hello");
    /// ```
    pub fn from_raw(bytes: impl Into<Vec<u8>>) -> Comment {
        Comment {
            inner: bytes.into(),
        }
    }

    pub fn as_bytes(&self) -> &[u8] {
        &self.inner
    }

    pub fn byte_len(&self) -> usize {
        self.inner.len()
    }
}

/// Single trivia pieces that can be combined to form [Trivia](crate::tokens::Trivia) tokens.
#[derive(Clone, Eq, PartialEq, Debug)]
// ANCHOR: trivia-piece
pub enum TriviaPiece {
    /// Horizontal tab '\t' characters
    HorizontalTabs(usize),
    /// Vertical tab '\v' characters
    VerticalTabs(usize),
    /// Carriage return '\r' characters
    CarriageReturns(usize),
    /// Carriage return + line feed ("\r\n") pairs
    CarriageReturnLineFeeds(usize),
    /// newline '\n' characters
    LineFeeds(usize),
    /// form feed '\f' characters
    FormFeeds(usize),
    /// A line comment starting with a '--'
    LineComment(Comment),
    /// A block comment starting with a '/*' and ending with a '*/'
    BlockComment(Comment),
    /// Space ' ' characters
    Spaces(usize),
    /// Non breaking space characters
    NonBreakingSpaces(usize),
}
// ANCHOR_END: trivia-piece

impl TriviaPiece {
    /// Returns the length of this trivia piece.
    pub fn byte_len(&self) -> usize {
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) | VerticalTabs(n) | CarriageReturns(n) | LineFeeds(n)
            | FormFeeds(n) | Spaces(n) | NonBreakingSpaces(n) => *n,
            CarriageReturnLineFeeds(n) => *n * 2,
            LineComment(str) | BlockComment(str) => str.byte_len(),
        }
    }

    /// Returns `true` when this trivia piece represents a whitespace, newline or tab
    pub fn is_whitespace(&self) -> bool {
        self.is_newline() || self.is_space_or_tab()
    }

    /// Returns if this trivia represents a newline.
    pub fn is_newline(&self) -> bool {
        use TriviaPiece::*;
        matches!(
            self,
            CarriageReturns(_)
                | LineFeeds(_)
                | FormFeeds(_)
                | CarriageReturnLineFeeds(_)
                | VerticalTabs(_)
        )
    }

    /// Returns if this piece is a space or tab symbol, excluding vertical tabs
    pub fn is_space_or_tab(&self) -> bool {
        use TriviaPiece::*;
        matches!(self, Spaces(_) | HorizontalTabs(_) | NonBreakingSpaces(_))
    }

    /// Returns if this trivia piece is a block or line comment.
    pub fn is_comment(&self) -> bool {
        use TriviaPiece::*;
        matches!(self, BlockComment(_) | LineComment(_))
    }

    pub fn write_to(&self, writer: &mut impl Write) -> io::Result<()> {
        fn write_repeated(writer: &mut impl Write, el: &[u8], count: usize) -> io::Result<()> {
            for _ in 0..count {
                writer.write_all(el)?;
            }
            Ok(())
        }
        use TriviaPiece::*;
        match self {
            HorizontalTabs(n) => write_repeated(writer, b"\t", *n),
            VerticalTabs(n) => write_repeated(writer, &[0x0Bu8], *n),
            CarriageReturns(n) => write_repeated(writer, b"\r", *n),
            CarriageReturnLineFeeds(n) => write_repeated(writer, b"\r\n", *n),
            LineFeeds(n) => write_repeated(writer, b"\n", *n),
            FormFeeds(n) => write_repeated(writer, &[0x0Cu8], *n),
            LineComment(comment) | BlockComment(comment) => writer.write_all(comment.as_bytes()),
            Spaces(n) => write_repeated(writer, b" ", *n),
            NonBreakingSpaces(n) => write_repeated(writer, &[0xA0u8], *n),
        }
    }
}
