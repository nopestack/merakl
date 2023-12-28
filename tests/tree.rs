use rand::{rngs::StdRng, Rng, RngCore, SeedableRng};

use merakl::prelude::*;

fn create_rng() -> StdRng {
    rand::rngs::StdRng::seed_from_u64(42)
}

fn generate_random_node_hash() -> NodeHash {
    let mut rng = create_rng();

    let mut bytes: [u8; 32] = [0; 32];

    rng.fill_bytes(&mut bytes);
    let hash_str = hex::encode(bytes);

    hash_str.parse().unwrap()
}

#[test]
fn build_tree_from_initial_values() {
    let initial_leaf = "abababababababababababababababababababababababababababababababab"
        .parse()
        .unwrap();

    let tree = MerkleTree::new(2, initial_leaf);

    let leaf_nodes = tree.iter().filter(|node| node.is_leaf).count();
    let non_leaf_nodes = tree.iter().filter(|node| !node.is_leaf).count();

    assert_ne!(tree.root_hash(), NodeHash::default());
    assert_eq!(leaf_nodes, 4);
    assert_eq!(non_leaf_nodes, 3);
}

#[test]
fn node_retrieval() {
    let initial_leaf = "abababababababababababababababababababababababababababababababab"
        .parse()
        .unwrap();

    let tree = MerkleTree::new(2, initial_leaf);

    // NOTE: sanity checks
    assert!(tree.get_by_coords((1, 0)).is_some());
    assert!(tree.get_by_coords((9, 9)).is_none());
    assert!(tree.get(5).is_some());
    assert!(tree.get(55).is_none());

    // NOTE: find a node by coords then find a node by its index and check if they are the same
    let found_by_coords = tree.get_by_coords((1, 0)).unwrap();
    let index_by_coords = tree.get_index_by_coords((1, 0)).unwrap();
    let found_by_index = tree.get(index_by_coords).unwrap();
    assert_eq!(found_by_coords, found_by_index);

    // NOTE: root has no parent
    let found_root = tree.get(0).unwrap();
    assert_eq!(found_root.parent_index(), 0);

    let found = tree.get(3).unwrap();
    assert_eq!(found.parent_index(), 1);

    // NOTE: node 3
    let sibling = tree.get_sibling(found.index).unwrap();

    for node in sibling {
        let sib_sibling = tree.get_sibling(node.index).unwrap();

        // NOTE: siblings should all be at the same level
        assert_eq!(node.level, 2);
    }
}

#[test]
fn set_leaf_value() {
    let initial_leaf = "0000000000000000000000000000000000000000000000000000000000000000"
        .parse()
        .unwrap();

    let mut tree = MerkleTree::new(2, initial_leaf);

    let old_root = tree.root_hash();
    let new_hash = generate_random_node_hash();

    let old_node = tree.get(3).unwrap().clone();

    let new_root = tree.set(3, new_hash.clone()).unwrap();
    let modified_node = tree.get(3).unwrap().clone();

    assert_eq!(modified_node.hash, new_hash);
    assert_ne!(old_node, modified_node);
    assert_ne!(new_root, old_root);
}

#[test]
fn generate_valid_proof() {
    let initial_leaf = "0000000000000000000000000000000000000000000000000000000000000000"
        .parse()
        .unwrap();

    let mut tree = MerkleTree::new(2, initial_leaf);
    let leaf = tree.get(5).unwrap().clone();

    tree.set(5, generate_random_node_hash()).unwrap();

    let proof = tree.proof(5).unwrap();

    let mut expected_hashes = vec![];

    let mut current_node = Some(&leaf);

    // TODO: consider impl'ing a helper method to traverse up the tree that accepts a visitor
    while let Some(node) = current_node {
        if node.hash == tree.root_hash() {
            break;
        }

        let sibling = tree.get_sibling(node.index).unwrap();
        if let Some(sibling) = sibling {
            expected_hashes.push(sibling.hash);
        }

        let parent_index = node.parent_index();

        let parent = tree.get(parent_index);
        current_node = parent;
    }

    assert_eq!(tree.root_hash(), proof.root);
    assert_eq!(proof.hashes.len(), expected_hashes.len());
    assert_eq!(proof.hashes, expected_hashes);
}

#[test]
fn verify_valid_proof() {
    let initial_leaf = "0000000000000000000000000000000000000000000000000000000000000000"
        .parse()
        .unwrap();

    let tree = MerkleTree::new(5, initial_leaf);

    let leaf_indices = tree
        .iter()
        .filter(|node| node.is_leaf)
        .map(|node| node.index)
        .collect::<Vec<NodeIndex>>();

    let mut rng = create_rng();

    // NOTE: grab a random leaf
    let index = rng.gen_range(0..leaf_indices.len());
    let leaf_index = leaf_indices[index];

    let proof = tree.proof(leaf_index).unwrap();

    // NOTE: check that the root hashes match after rebuilding the tree
    let proof_hash = tree.verify_proof(proof).unwrap();
    assert_eq!(proof_hash, tree.root_hash());
}
