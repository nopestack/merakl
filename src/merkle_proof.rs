use serde::{Deserialize, Serialize};

use crate::node::{Node, NodeHash};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MerkleProof {
    pub root: NodeHash,
    /// Represent the path from the leaf to the root
    pub hashes: Vec<NodeHash>,
    pub leaf: Node,
}
