use serde::{Deserialize, Serialize};
use sha3::Digest;
use std::{fmt::Display, str::FromStr};

/// Represents a SHA-256 digest of a node's children
#[derive(Debug, Default, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct NodeHash([u8; 32]);

impl NodeHash {
    pub fn inner(&self) -> [u8; 32] {
        self.0
    }
}

impl Display for NodeHash {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", hex::encode(self.0))
    }
}

impl From<&[u8]> for NodeHash {
    fn from(bytes: &[u8]) -> Self {
        let mut hash_bytes = [0; 32];
        hash_bytes.copy_from_slice(bytes);
        NodeHash(hash_bytes)
    }
}

impl From<&[u8; 32]> for NodeHash {
    fn from(bytes: &[u8; 32]) -> Self {
        let mut hash_bytes = [0; 32];
        hash_bytes.copy_from_slice(bytes);
        NodeHash(hash_bytes)
    }
}

impl FromStr for NodeHash {
    type Err = hex::FromHexError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let bytes = hex::decode(s)?;
        Ok(NodeHash::from(bytes.as_slice()))
    }
}

/// Represents a node's index in the tree
pub type NodeIndex = usize;

/// Represents the level and offset of a node in the tree respectively
pub type NodeCoords = (usize, usize);

#[derive(Debug, Default, Clone, Serialize, Deserialize, Eq, PartialEq)]
pub struct Node {
    pub index: NodeIndex,
    pub level: usize,
    pub offset: usize,
    pub is_leaf: bool,
    #[serde(with = "hash_serde")]
    pub hash: NodeHash,
    pub left: Option<Box<Node>>,
    pub right: Option<Box<Node>>,
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} ({}, {}): {}",
            self.index,
            self.level,
            self.offset,
            self.hash_str()
        )
    }
}

impl Node {
    pub fn new(
        index: NodeIndex,
        level: usize,
        offset: usize,
        left: Option<Box<Node>>,
        right: Option<Box<Node>>,
    ) -> Self {
        let hash = Self::hash_children(&left, &right);
        let is_leaf = left.is_none() && right.is_none();

        Node {
            index,
            level,
            offset,
            is_leaf,
            hash,
            left,
            right,
        }
    }

    pub fn new_with_hash(
        index: NodeIndex,
        level: usize,
        offset: usize,
        hash: NodeHash,
        left: Option<Box<Node>>,
        right: Option<Box<Node>>,
    ) -> Self {
        let is_leaf = left.is_none() && right.is_none();
        Node {
            index,
            level,
            offset,
            is_leaf,
            hash,
            left,
            right,
        }
    }

    /// Serializes the node into a byte vector
    pub fn as_bytes(&self) -> Vec<u8> {
        // TODO: consider logging the  error
        bincode::serialize(self).unwrap_or_default()
    }

    pub fn hash_children(left: &Option<Box<Node>>, right: &Option<Box<Node>>) -> NodeHash {
        let hash = match (left.as_ref(), right.as_ref()) {
            (Some(left), Some(right)) => {
                hash_values(vec![left.hash.clone(), right.hash.clone()])
                // let mut hasher = sha3::Sha3_256::new();
                // hasher.update(left.hash.inner());
                // hasher.update(right.hash.inner());
                // NodeHash::from(hasher.finalize().as_slice())
            }
            (Some(single_node), None) | (None, Some(single_node)) => {
                // let mut hasher = sha3::Sha3_256::new();
                // hasher.update(single_node.hash.inner());
                // NodeHash::from(hasher.finalize().as_slice())
                hash_values(vec![single_node.hash.clone()])
            }
            _ => NodeHash::default(),
        };

        hash
    }

    pub fn child_indices(&self) -> (Option<NodeIndex>, Option<NodeIndex>) {
        let (left_index, right_index) = match (self.left.as_ref(), self.right.as_ref()) {
            (Some(left), Some(right)) => (Some(left.index), Some(right.index)),
            (Some(single_node), None) => (Some(single_node.index), None),
            (None, Some(single_node)) => (None, Some(single_node.index)),
            _ => (None, None),
        };

        (left_index, right_index)
    }

    /// Returns a hex string representation of the node's hash
    pub fn hash_str(&self) -> String {
        self.hash.to_string()
    }

    /// Gets the parent's index
    pub fn parent_index(&self) -> NodeIndex {
        if self.index == 0 {
            0
        } else {
            (self.index - 1) / 2
        }
    }

    /// Gets the leftmost child's index
    pub fn left_child_index(&self) -> Option<NodeIndex> {
        Some(self.left.as_ref()?.index)
    }
}

pub fn hash_values(hashes: Vec<NodeHash>) -> NodeHash {
    if hashes.is_empty() {
        return NodeHash::default();
    }

    let mut hasher = sha3::Sha3_256::new();
    for hash in hashes {
        hasher.update(hash.inner());
    }

    NodeHash::from(hasher.finalize().as_slice())
}

mod hash_serde {
    use serde::{Deserialize, Deserializer, Serializer};

    use crate::prelude::NodeHash;

    pub fn serialize<S>(hash: &NodeHash, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&hash.to_string())
    }

    pub fn deserialize<'de, D>(d: D) -> Result<NodeHash, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(d)?;
        let node_hash_bytes = hex::decode(s).map_err(serde::de::Error::custom)?;
        Ok(NodeHash::from(node_hash_bytes.as_slice()))
    }
}
