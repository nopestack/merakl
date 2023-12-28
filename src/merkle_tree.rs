use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, VecDeque},
    fmt::Display,
};

use crate::{merkle_proof::MerkleProof, node::*};

#[derive(thiserror::Error, Debug, Clone, Serialize, Deserialize)]
pub enum MerkleTreeError {
    #[error("invalid proof")]
    InvalidProof,
    #[error("node {0} not found in tree")]
    NodeNotFound(NodeIndex),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleTree {
    root: Node,
    depth: usize,
}

impl MerkleTree {
    pub fn new(depth: usize, initial_leaf: NodeHash) -> Self {
        let leaf_count = 2usize.pow(depth as u32);

        let leaf_values = (1..=leaf_count)
            .map(|_| initial_leaf.clone())
            .collect::<Vec<NodeHash>>();

        let root = Self::build_tree_r(0, 0, 0, &leaf_values);

        MerkleTree { root, depth }
    }

    // Builds a tree recursively from a list of node hashes
    fn build_tree_r(index: usize, level: usize, offset: usize, nodes: &[NodeHash]) -> Node {
        match nodes {
            [node_hash] => Node::new_with_hash(index, level, offset, node_hash.clone(), None, None),
            _ => {
                let next_level = level + 1;
                let middle = nodes.len() / 2;

                let left_half = &nodes[..middle];
                let right_half = &nodes[middle..];

                // NOTE: such that the right subtree starts counting from the last index on the
                // left BFS style
                let right_offset = offset + middle;

                let left_index = 2 * index + 1;
                let right_index = 2 * index + 2;

                let left_node = Self::build_tree_r(left_index, next_level, offset, left_half);

                let right_node =
                    Self::build_tree_r(right_index, next_level, right_offset, right_half);

                Node::new(
                    index,
                    level,
                    offset,
                    Some(Box::new(left_node)),
                    Some(Box::new(right_node)),
                )
            }
        }
    }

    pub fn root_hash(&self) -> NodeHash {
        self.root.hash.clone()
    }

    pub fn root(&self) -> Node {
        self.root.clone()
    }

    pub fn root_node_ref(&self) -> &Node {
        &self.root
    }

    fn bfs_ref<F>(&self, criteria: F) -> Option<&Node>
    where
        F: Fn(&Node) -> bool,
    {
        let mut queue = VecDeque::new();
        queue.push_back(&self.root);

        while let Some(node) = queue.pop_back() {
            if criteria(node) {
                return Some(node);
            }

            if let Some(left) = node.left.as_ref() {
                queue.push_back(left);
            }

            if let Some(right) = node.right.as_ref() {
                queue.push_back(right);
            }
        }

        None
    }

    // TODO: figure out how to abstract the inner loop of bfs_ref and bfs_ref_mut
    fn bfs_ref_mut<F>(&mut self, criteria: F) -> Option<&mut Node>
    where
        F: Fn(&mut Node) -> bool,
    {
        let mut queue = VecDeque::new();
        queue.push_back(&mut self.root);

        while let Some(node) = queue.pop_back() {
            if criteria(node) {
                return Some(node);
            }

            if let Some(left) = node.left.as_mut() {
                queue.push_back(left);
            }

            if let Some(right) = node.right.as_mut() {
                queue.push_back(right);
            }
        }

        None
    }

    pub fn get_by_coords(&self, coords: NodeCoords) -> Option<&Node> {
        self.bfs_ref(|node| node.level == coords.0 && node.offset == coords.1)
    }

    pub fn get_index_by_coords(&self, coords: NodeCoords) -> Option<NodeIndex> {
        Some(self.get_by_coords(coords)?.index)
    }

    pub fn get(&self, index: NodeIndex) -> Option<&Node> {
        self.bfs_ref(|node| node.index == index)
    }

    pub fn get_mut(&mut self, index: NodeIndex) -> Option<&mut Node> {
        self.bfs_ref_mut(|node| node.index == index)
    }

    pub fn get_node_offset(&self, index: NodeIndex) -> Option<NodeCoords> {
        self.get(index).map(|node| (node.level, node.offset))
    }

    pub fn get_leaf_nodes(&self) -> Vec<Node> {
        self.iter().filter(|node| node.is_leaf).cloned().collect()
    }

    pub fn set(&mut self, index: NodeIndex, value: NodeHash) -> Result<NodeHash, MerkleTreeError> {
        let found = self
            .get_mut(index)
            .ok_or(MerkleTreeError::NodeNotFound(index))?;

        found.hash = value;

        let leaf_values = self
            .get_leaf_nodes()
            .into_iter()
            .map(|node| node.hash)
            .collect::<Vec<NodeHash>>();

        let new_root = Self::build_tree_r(0, 0, 0, leaf_values.as_slice());

        self.root = new_root;

        Ok(self.root.hash.clone())
    }

    /// Returns the sibling node of a given node
    pub fn get_sibling(&self, index: NodeIndex) -> Result<Option<Node>, MerkleTreeError> {
        let node = self
            .get(index)
            .ok_or(MerkleTreeError::NodeNotFound(index))?;

        let parent_index = node.parent_index();

        let parent_node = self
            .get(parent_index)
            .ok_or(MerkleTreeError::NodeNotFound(parent_index))?
            .clone();

        let (left_child_index, right_child_index) = parent_node.child_indices();

        match (left_child_index, right_child_index) {
            (Some(left_child_index), Some(right_child_index)) => {
                let sibling = if left_child_index == index {
                    self.get(right_child_index)
                } else {
                    self.get(left_child_index)
                };

                Ok(sibling.cloned())
            }
            // skip if node has no sibling
            _ => Ok(None),
        }
    }

    pub fn proof(&self, leaf_index: NodeIndex) -> Result<MerkleProof, MerkleTreeError> {
        let leaf = self
            .get(leaf_index)
            .ok_or(MerkleTreeError::NodeNotFound(leaf_index))?
            .to_owned();

        let mut hashes = vec![];

        let mut current_node = Some(&leaf);

        while let Some(node) = current_node {
            // NOTE: stop at root
            if node.hash == self.root_hash() {
                break;
            }

            let sibling = self.get_sibling(node.index)?;
            if let Some(sibling) = sibling {
                hashes.push(sibling.hash);
            }

            let parent_index = node.parent_index();

            let parent = self.get(parent_index);
            current_node = parent;
        }

        Ok(MerkleProof {
            root: self.root_hash(),
            hashes,
            leaf,
        })
    }

    pub fn verify_proof(&self, proof: MerkleProof) -> Result<NodeHash, MerkleTreeError> {
        // hash the last hash with the leaf hash
        // then hash the next hash with the resulting hash
        // repeat after all the sibling hashes run out
        // check if the final hash matches the root hash of the tree

        // pseudo code
        // acc = leaf.hash
        // hashes.reduce((acc, hash) => hash_children(acc, hash))

        let resulting_hash = proof.hashes.iter().fold(proof.leaf.hash, |acc, hash| {
            hash_values(vec![acc, hash.clone()])
        });

        Ok(resulting_hash)
    }

    pub fn iter(&self) -> MerkleTreeIter {
        let mut queue = VecDeque::new();
        let mut node_refs = VecDeque::new();

        queue.push_back(&self.root);

        while let Some(node) = queue.pop_front() {
            if let Some(left) = node.left.as_ref() {
                queue.push_back(left);
            }

            if let Some(right) = node.right.as_ref() {
                queue.push_back(right);
            }

            node_refs.push_back(node);
        }

        MerkleTreeIter { node_refs }
    }
}

impl Display for MerkleTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let serialized = serde_json::to_string_pretty(&self.root).map_err(|_| std::fmt::Error)?;
        write!(f, "{}", serialized)
    }
}

#[derive(Debug, Clone)]
pub struct MerkleTreeIter<'a> {
    node_refs: VecDeque<&'a Node>,
}

impl<'a> Iterator for MerkleTreeIter<'a> {
    type Item = &'a Node;

    fn next(&mut self) -> Option<Self::Item> {
        let next_node = self.node_refs.pop_front()?;

        Some(next_node)
    }
}
