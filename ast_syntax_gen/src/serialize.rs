// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use serde::de::{MapAccess, Visitor};
use serde::{Deserialize, Deserializer, Serialize};
use std::fmt;

#[derive(Serialize, Debug)]
pub struct Node {
    pub name: String,
    pub contents: NodeContents,
}

#[derive(Debug, Default)]
pub struct Nodes(Vec<Node>);

impl From<Nodes> for Vec<Node> {
    fn from(value: Nodes) -> Self {
        value.0
    }
}

impl IntoIterator for Nodes {
    type Item = Node;
    type IntoIter = <Vec<Node> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

struct NodesVisitor;

impl<'de> Visitor<'de> for NodesVisitor {
    // The type that our Visitor is going to produce.
    type Value = Nodes;

    // Format a message stating what data this Visitor expects to receive.
    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("a node")
    }

    fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
    where
        A: MapAccess<'de>,
    {
        let mut res = Vec::with_capacity(map.size_hint().unwrap_or(0));
        while let Some((key, value)) = map.next_entry::<String, NodeContents>()? {
            res.push(Node {
                name: key,
                contents: value,
            })
        }
        Ok(Nodes(res))
    }
}

impl<'de> Deserialize<'de> for Nodes {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_map(NodesVisitor)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub enum NodeContents {
    Sequence(Vec<NodeOrToken>),
    Choice(Vec<NodeOrToken>),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
pub enum NodeOrToken {
    Node(NodeRef),
    Token(TokenRef),
    Keyword(KeywordRef),
}

#[derive(Serialize, Deserialize, Debug)]
pub struct NodeRef {
    pub node: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub terminated: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub repeated: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub parenthesized: bool,
}

impl NodeRef {
    pub fn name(&self) -> String {
        self.name.clone().unwrap_or_else(|| self.node.clone())
    }

    pub fn kind(&self) -> String {
        self.node.clone()
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct TokenRef {
    pub token: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub repeated: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub trailing: bool,
}

fn token_str_name(name: Option<String>, token: &str, trailing: bool) -> String {
    let base_name = name.clone().unwrap_or_else(|| token.to_owned());
    if trailing {
        format!("trailing_{base_name}")
    } else {
        base_name
    }
}

impl TokenRef {
    pub fn name(&self) -> String {
        token_str_name(self.name.clone(), &self.token, self.trailing)
    }
}

#[derive(Serialize, Deserialize, Debug)]
pub struct KeywordRef {
    pub keyword: String,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "std::ops::Not::not", default)]
    pub trailing: bool,
}

impl KeywordRef {
    pub fn name(&self) -> String {
        token_str_name(self.name.clone(), &self.keyword, self.trailing)
    }
}

#[test]
fn node() {
    let deserialized: Nodes = serde_yml::from_str(
        "\
Foo: !Sequence
  - node: A
  - token: B
  - node: T
    terminated: SemiColon
  - node: R
    repeated: true

Bar: !Choice
  - node: C
  - token: D
    ",
    )
    .unwrap();

    println!("{deserialized:?}")
}

#[test]
fn architecture_body() {
    let deserialized: Nodes = serde_yml::from_str(
        "\
ArchitectureBody: !Sequence
  - keyword: Architecture
  - token: Identifier
    name: name
  - keyword: Of
  - node: Name
    name: entity_name
  - keyword: Is
  - node: DeclarativePart
  - keyword: Begin
  - node: StatementPart
  - keyword: End
  - keyword: Architecture
  - token: Identifier
    name: final_name
  - token: SemiColon
    ",
    )
    .unwrap();

    println!("{deserialized:?}")
}
