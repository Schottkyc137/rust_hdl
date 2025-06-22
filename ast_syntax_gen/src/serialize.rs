// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this file,
// You can obtain one at http://mozilla.org/MPL/2.0/.
//
// Copyright (c)  2025, Lukas Scheller lukasscheller@icloud.com

use serde::{Deserialize, Serialize};
use serde_yml::to_string;
use std::vec;

#[derive(Serialize, Deserialize, Debug)]
struct Node {
    name: String,
    #[serde(flatten)]
    contents: NodeContents,
}

#[derive(Serialize, Deserialize, Debug)]
enum NodeContents {
    Sequence(Vec<NodeOrToken>),
    Choice(Vec<NodeOrToken>),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(untagged)]
enum NodeOrToken {
    Node(NodeRef),
    Token(TokenRef),
}

#[derive(Serialize, Deserialize, Debug)]
struct NodeRef {
    node: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct TokenRef {
    token: String,
}

#[derive(Serialize, Deserialize, Debug)]
struct KeywordRef {
    keyword: String,
}

#[test]
fn node() {
    let nodes = vec![Node {
        name: "Foo".to_owned(),
        contents: NodeContents::Sequence(vec![
            NodeOrToken::Node(NodeRef {
                node: "A".to_owned(),
            }),
            NodeOrToken::Token(TokenRef {
                token: "B".to_owned(),
            }),
        ]),
    }];

    println!("{}", to_string(&nodes).unwrap());

    let deserialized: Vec<Node> = serde_yml::from_str(
        "\
- Foo: !Sequence
  - node: A
  - token: B
- Bar: !Choice
  - node: C
  - token: D
    ",
    )
    .unwrap();

    println!("{:?}", deserialized)
}

#[test]
fn test2() {
    let yml: serde_yml::Value = serde_yml::from_str(
        "\
- Foo: !Sequence
  - node: A
  - token: B",
    )
    .unwrap();
    println!("{:?}", yml);
}
