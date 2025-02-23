mod base;
mod evaluation;
mod interpreter;
mod node;

#[cfg(feature = "bincode")]
mod serialization;

#[cfg(test)]
mod tests;

use crate::action_tree::*;
use crate::bunching::*;
use crate::interface::*;
use crate::mutex_like::*;
use crate::utility::*;
use std::collections::BTreeMap;
use std::mem::{self, MaybeUninit};

#[cfg(feature = "bincode")]
use bincode::{Decode, Encode};

#[cfg(feature = "rayon")]
use rayon::prelude::*;

#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
enum State {
    ConfigError = 0,
    #[default]
    Uninitialized = 1,
    TreeBuilt = 2,
    MemoryAllocated = 3,
    Solved = 4,
}

/// A struct representing a postflop game.
#[derive(Default)]
pub struct PostFlopGame {
    // state
    state: State,

    // postflop game configurations
    card_config: CardConfig,
    tree_config: TreeConfig,
    added_lines: Vec<Vec<Action>>,
    removed_lines: Vec<Vec<Action>>,
    action_root: Box<MutexLike<ActionTreeNode>>,

    // computed from configurations
    num_combinations: f64,
    initial_weights: [Vec<f32>; 2],
    private_cards: [Vec<(Card, Card)>; 2],
    same_hand_index: [Vec<u16>; 2],

    // indices in `private_cards` that do not conflict with the specified board cards
    valid_indices_flop: [Vec<u16>; 2],
    valid_indices_turn: Vec<[Vec<u16>; 2]>,
    valid_indices_river: Vec<[Vec<u16>; 2]>,

    // hand strength information: indices are stored in ascending strength order
    hand_strength: Vec<[Vec<StrengthItem>; 2]>,

    // isomorphism information
    // - `isomorphism_ref_*`: indices to which the eliminated events should refer
    // - `isomorphism_card_*`: list of cards eliminated by the isomorphism
    // - `isomorphism_swap_*`: list of hand index pairs that should be swapped when applying the
    //                         isomorphism with the specified suit
    isomorphism_ref_turn: Vec<u8>,
    isomorphism_card_turn: Vec<Card>,
    isomorphism_swap_turn: [SwapList; 4],
    isomorphism_ref_river: Vec<Vec<u8>>,
    isomorphism_card_river: [Vec<Card>; 4],
    isomorphism_swap_river: [[SwapList; 4]; 4],

    // bunching effect
    bunching_num_dead_cards: usize,
    bunching_num_combinations: f64,
    bunching_arena: Vec<f32>,
    bunching_strength: Vec<[Vec<u16>; 2]>,
    bunching_num_flop: [Vec<usize>; 2],
    bunching_num_turn: [Vec<Vec<usize>>; 2],
    bunching_num_river: [Vec<Vec<usize>>; 2],
    bunching_coef_flop: [Vec<usize>; 2],
    bunching_coef_turn: [Vec<Vec<usize>>; 2],

    // store options
    storage_mode: BoardState,
    target_storage_mode: BoardState,
    num_nodes: [u64; 3],
    is_compression_enabled: bool,
    num_storage: u64,
    num_storage_ip: u64,
    num_storage_chance: u64,
    misc_memory_usage: u64,

    // global storage
    // `storage*` are used as a global storage and are referenced by `PostFlopNode::storage*`.
    // Methods like `PostFlopNode::strategy` define how the storage is used.
    node_arena: Vec<MutexLike<PostFlopNode>>,
    storage1: Vec<u8>,
    storage2: Vec<u8>,
    storage_ip: Vec<u8>,
    storage_chance: Vec<u8>,
    locking_strategy: BTreeMap<usize, Vec<f32>>,

    // result interpreter
    action_history: Vec<usize>,
    node_history: Vec<usize>,
    is_normalized_weight_cached: bool,
    turn: Card,
    river: Card,
    turn_swapped_suit: Option<(u8, u8)>,
    turn_swap: Option<u8>,
    river_swap: Option<(u8, u8)>,
    total_bet_amount: [i32; 2],
    weights: [Vec<f32>; 2],
    normalized_weights: [Vec<f32>; 2],
    cfvalues_cache: [Vec<f32>; 2],
}

/// A struct representing a node in a postflop game tree.
///
/// The nodes must be stored as `Vec<MutexLike<PostFlopNode>>`.
#[derive(Debug, Clone, Copy)]
#[repr(C)]
pub struct PostFlopNode {
    prev_action: Action,
    player: u8,
    turn: Card,
    river: Card,
    is_locked: bool,
    amount: i32,
    children_offset: u32,
    num_children: u16,
    num_elements_ip: u16,
    num_elements: u32,
    scale1: f32,
    scale2: f32,
    scale3: f32,
    storage1: *mut u8, // strategy
    storage2: *mut u8, // regrets or cfvalues
    storage3: *mut u8, // IP cfvalues
}

unsafe impl Send for PostFlopNode {}
unsafe impl Sync for PostFlopNode {}

#[derive(Default)]
struct BuildTreeInfo {
    flop_index: usize,
    turn_index: usize,
    river_index: usize,
    num_storage: u64,
    num_storage_ip: u64,
    num_storage_chance: u64,
}

impl Game for PostFlopGame {
    type Node = PostFlopNode;

    #[inline]
    fn root(&self) -> MutexGuardLike<Self::Node> {
        self.node_arena[0].lock()
    }

    #[inline]
    fn num_private_hands(&self, player: usize) -> usize {
        self.private_cards[player].len()
    }

    #[inline]
    fn initial_weights(&self, player: usize) -> &[f32] {
        &self.initial_weights[player]
    }

    #[inline]
    fn evaluate(
        &self,
        result: &mut [MaybeUninit<f32>],
        node: &Self::Node,
        player: usize,
        cfreach: &[f32],
    ) {
        if self.bunching_num_dead_cards == 0 {
            self.evaluate_internal(result, node, player, cfreach);
        } else {
            self.evaluate_internal_bunching(result, node, player, cfreach);
        }
    }

    #[inline]
    fn chance_factor(&self, node: &Self::Node) -> usize {
        if node.turn == NOT_DEALT {
            if self.card_config.allowed_turn_cards.is_empty() {
                45 - self.bunching_num_dead_cards
            } else {
                self.card_config.allowed_turn_cards.len()
            }
        } else {
            if self.card_config.allowed_river_cards.is_empty() {
                44 - self.bunching_num_dead_cards
            } else {
                self.card_config.allowed_river_cards.len()
            }
        }
    }

    #[inline]
    fn is_solved(&self) -> bool {
        self.state == State::Solved
    }

    #[inline]
    fn set_solved(&mut self) {
        self.state = State::Solved;
        let history = self.action_history.clone();
        self.apply_history(&history);
    }

    #[inline]
    fn is_ready(&self) -> bool {
        self.state == State::MemoryAllocated && self.storage_mode == BoardState::River
    }

    #[inline]
    fn is_raked(&self) -> bool {
        self.tree_config.rake_rate > 0.0 && self.tree_config.rake_cap > 0.0
    }

    #[inline]
    fn isomorphic_chances(&self, node: &Self::Node) -> &[u8] {
        if node.turn == NOT_DEALT {
            &self.isomorphism_ref_turn
        } else {
            &self.isomorphism_ref_river[node.turn as usize]
        }
    }

    #[inline]
    fn isomorphic_swap(&self, node: &Self::Node, index: usize) -> &[Vec<(u16, u16)>; 2] {
        if node.turn == NOT_DEALT {
            &self.isomorphism_swap_turn[self.isomorphism_card_turn[index] as usize & 3]
        } else {
            &self.isomorphism_swap_river[node.turn as usize & 3]
                [self.isomorphism_card_river[node.turn as usize & 3][index] as usize & 3]
        }
    }

    #[inline]
    fn locking_strategy(&self, node: &Self::Node) -> &[f32] {
        if !node.is_locked {
            &[]
        } else {
            let index = self.node_index(node);
            self.locking_strategy.get(&index).unwrap()
        }
    }

    #[inline]
    fn is_compression_enabled(&self) -> bool {
        self.is_compression_enabled
    }
}

impl PostFlopGame {
    /// Creates a new empty [`PostFlopGame`].
    ///
    /// Use of this method is strongly discouraged because an instance created by this method is
    /// invalid until [`update_config`] is called.
    /// Please use [`with_config`] instead whenever possible.
    ///
    /// [`update_config`]: #method.update_config
    /// [`with_config`]: #method.with_config
    #[inline]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new [`PostFlopGame`] with the specified configuration.
    #[inline]
    pub fn with_config(card_config: CardConfig, action_tree: ActionTree) -> Result<Self, String> {
        let mut game = Self::new();
        game.update_config(card_config, action_tree)?;
        Ok(game)
    }

    /// Updates the game configuration. The solved result will be lost.
    #[inline]
    pub fn update_config(
        &mut self,
        card_config: CardConfig,
        action_tree: ActionTree,
    ) -> Result<(), String> {
        self.state = State::ConfigError;

        if !action_tree.invalid_terminals().is_empty() {
            return Err("Invalid terminal is found in action tree".to_string());
        }

        self.card_config = card_config;
        (
            self.tree_config,
            self.added_lines,
            self.removed_lines,
            self.action_root,
        ) = action_tree.eject();

        self.check_card_config()?;
        self.init_card_fields();
        self.init_root()?;

        self.state = State::TreeBuilt;

        self.init_interpreter();
        self.reset_bunching_effect();

        Ok(())
    }

    /// Sets the bunching effect configuration.
    ///
    /// **Warning**: Enabling the bunching effect will significantly slow down the solving process.
    /// Specifically, the computational complexity of the terminal evaluation will increase from
    /// *O*(#(OOP private hands) + #(IP private hands)) to *O*(#(OOP private hands) * #(IP private
    /// hands)).
    #[inline]
    pub fn set_bunching_effect(&mut self, bunching_data: &BunchingData) -> Result<(), String> {
        if self.state <= State::Uninitialized {
            return Err("Game is not successfully initialized".to_string());
        }

        if !bunching_data.is_ready() {
            return Err("Bunching configuration is not ready".to_string());
        }

        let mut flop_sorted = self.card_config.flop;
        flop_sorted.sort_unstable();
        if flop_sorted != bunching_data.flop() {
            return Err("Flop cards do not match".to_string());
        }

        self.reset_bunching_effect();
        self.set_bunching_effect_internal(bunching_data)?;

        Ok(())
    }

    /// Resets the bunching effect configuration. The current node will also be reset to the root.
    #[inline]
    pub fn reset_bunching_effect(&mut self) {
        self.bunching_num_dead_cards = 0;
        self.bunching_num_combinations = 0.0;
        self.bunching_arena = Vec::new();
        self.bunching_strength = Vec::new();
        self.bunching_num_flop = Default::default();
        self.bunching_num_turn = Default::default();
        self.bunching_num_river = Default::default();
        self.bunching_coef_flop = Default::default();
        self.bunching_coef_turn = Default::default();
        self.back_to_root();
    }

    /// Obtains the card configuration.
    #[inline]
    pub fn card_config(&self) -> &CardConfig {
        &self.card_config
    }

    /// Obtains the tree configuration.
    #[inline]
    pub fn tree_config(&self) -> &TreeConfig {
        &self.tree_config
    }

    /// Obtains the added lines.
    #[inline]
    pub fn added_lines(&self) -> &[Vec<Action>] {
        &self.added_lines
    }

    /// Obtains the removed lines.
    #[inline]
    pub fn removed_lines(&self) -> &[Vec<Action>] {
        &self.removed_lines
    }

    /// Returns the card list of private hands of the given player.
    ///
    /// The returned list contains only card pairs with positive weight, i.e., card pairs with zero
    /// weight are excluded. The returned list is sorted as follows:
    ///
    /// - Each card pair has IDs in `(low_id, high_id)` order.
    /// - Card pairs are sorted in the lexicographic order.
    #[inline]
    pub fn private_cards(&self, player: usize) -> &[(Card, Card)] {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        &self.private_cards[player]
    }

    /// Returns the estimated memory usage in bytes (uncompressed, compressed).
    #[inline]
    pub fn memory_usage(&self) -> (u64, u64) {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        let num_elements = 2 * self.num_storage + self.num_storage_ip + self.num_storage_chance;
        let uncompressed = 4 * num_elements + self.misc_memory_usage;
        let compressed = 2 * num_elements + self.misc_memory_usage;

        (uncompressed, compressed)
    }

    /// Returns the estimated additional memory usage in bytes when the bunching effect is enabled.
    #[inline]
    pub fn memory_usage_bunching(&self) -> u64 {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        self.memory_usage_bunching_internal()
    }

    /// Remove lines after building the `PostFlopGame` but before allocating memory.
    ///
    /// This allows the removal of chance-specific lines (e.g., remove overbets on board-pairing
    /// turns) which we cannot do while building an action tree.
    pub fn remove_lines(&mut self, lines: &[Vec<Action>]) -> Result<(), String> {
        if self.state <= State::Uninitialized {
            return Err("Game is not successfully initialized".to_string());
        } else if self.state >= State::MemoryAllocated {
            return Err("Game has already been allocated".to_string());
        }

        for line in lines {
            let mut root = self.root();
            let info = self.remove_line_recursive(&mut root, line)?;
            self.num_storage -= info.num_storage;
            self.num_storage_ip -= info.num_storage_ip;
            self.num_storage_chance -= info.num_storage_chance;
        }

        Ok(())
    }

    /// Returns whether the memory is allocated.
    ///
    /// If the memory is allocated, returns `Some(is_compression_enabled)`;
    /// otherwise, returns `None`.
    #[inline]
    pub fn is_memory_allocated(&self) -> Option<bool> {
        if self.state <= State::TreeBuilt {
            None
        } else {
            Some(self.is_compression_enabled)
        }
    }

    /// Allocates the memory.
    pub fn allocate_memory(&mut self, enable_compression: bool) {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        if self.state == State::MemoryAllocated
            && self.storage_mode == BoardState::River
            && self.is_compression_enabled == enable_compression
        {
            return;
        }

        let num_bytes = if enable_compression { 2 } else { 4 };
        if num_bytes * self.num_storage > isize::MAX as u64
            || num_bytes * self.num_storage_chance > isize::MAX as u64
        {
            panic!("Memory usage exceeds maximum size");
        }

        self.state = State::MemoryAllocated;
        self.is_compression_enabled = enable_compression;

        self.clear_storage();

        let storage_bytes = (num_bytes * self.num_storage) as usize;
        let storage_ip_bytes = (num_bytes * self.num_storage_ip) as usize;
        let storage_chance_bytes = (num_bytes * self.num_storage_chance) as usize;

        self.storage1 = vec![0; storage_bytes];
        self.storage2 = vec![0; storage_bytes];
        self.storage_ip = vec![0; storage_ip_bytes];
        self.storage_chance = vec![0; storage_chance_bytes];

        self.allocate_memory_nodes();

        self.storage_mode = BoardState::River;
        self.target_storage_mode = BoardState::River;
    }

    /// Checks the card configuration.
    pub(crate) fn check_card_config(&mut self) -> Result<(), String> {
        let config = &self.card_config;
        config.check_validity()?;

        let expected_state = match (config.turn != NOT_DEALT, config.river != NOT_DEALT) {
            (false, _) => BoardState::Flop,
            (true, false) => BoardState::Turn,
            (true, true) => BoardState::River,
        };

        if self.tree_config.initial_state != expected_state {
            return Err(format!(
                "Invalid initial state of `tree_config`: expected = {:?}, actual = {:?}",
                expected_state, self.tree_config.initial_state
            ));
        }

        if config.range[0].is_empty() {
            return Err("OOP range is empty".to_string());
        }

        if config.range[1].is_empty() {
            return Err("IP range is empty".to_string());
        }

        if !config.range[0].is_valid() {
            return Err("OOP range is invalid (loaded broken data?)".to_string());
        }

        if !config.range[1].is_valid() {
            return Err("IP range is invalid (loaded broken data?)".to_string());
        }

        self.init_hands();
        self.num_combinations = 0.0;

        for (&(c1, c2), &w1) in self.private_cards[0]
            .iter()
            .zip(self.initial_weights[0].iter())
        {
            let oop_mask: u64 = (1 << c1) | (1 << c2);
            for (&(c3, c4), &w2) in self.private_cards[1]
                .iter()
                .zip(self.initial_weights[1].iter())
            {
                let ip_mask: u64 = (1 << c3) | (1 << c4);
                if oop_mask & ip_mask == 0 {
                    self.num_combinations += w1 as f64 * w2 as f64;
                }
            }
        }

        if self.num_combinations == 0.0 {
            return Err("Valid card assignment does not exist".to_string());
        }

        Ok(())
    }

    /// Initializes fields `initial_weights` and `private_cards`.
    #[inline]
    fn init_hands(&mut self) {
        let config = &self.card_config;
        let (flop, turn, river) = (config.flop, config.turn, config.river);
        let range = &config.range;

        let mut board_mask: u64 = (1 << flop[0]) | (1 << flop[1]) | (1 << flop[2]);
        if turn != NOT_DEALT {
            board_mask |= 1 << turn;
        }
        if river != NOT_DEALT {
            board_mask |= 1 << river;
        }

        for player in 0..2 {
            let (hands, weights) = range[player].get_hands_weights(board_mask);
            self.initial_weights[player] = weights;
            self.private_cards[player] = hands;
        }
    }

    /// Initializes fields related to cards.
    pub(super) fn init_card_fields(&mut self) {
        for player in 0..2 {
            let same_hand_index = &mut self.same_hand_index[player];
            same_hand_index.clear();

            let player_hands = &self.private_cards[player];
            let opponent_hands = &self.private_cards[player ^ 1];
            for hand in player_hands {
                same_hand_index.push(
                    opponent_hands
                        .binary_search(hand)
                        .map_or(u16::MAX, |i| i as u16),
                );
            }
        }

        (
            self.valid_indices_flop,
            self.valid_indices_turn,
            self.valid_indices_river,
        ) = self.card_config.valid_indices(&self.private_cards);

        self.hand_strength = self.card_config.hand_strength(&self.private_cards);

        (
            self.isomorphism_ref_turn,
            self.isomorphism_card_turn,
            self.isomorphism_swap_turn,
            self.isomorphism_ref_river,
            self.isomorphism_card_river,
            self.isomorphism_swap_river,
        ) = self.card_config.isomorphism(&self.private_cards);
    }

    /// Initializes the root node of game tree.
    fn init_root(&mut self) -> Result<(), String> {
        let num_nodes = self.count_num_nodes();
        let total_num_nodes = num_nodes[0] + num_nodes[1] + num_nodes[2];

        if total_num_nodes > u32::MAX as u64
            || mem::size_of::<PostFlopNode>() as u64 * total_num_nodes > isize::MAX as u64
        {
            return Err("Too many nodes".to_string());
        }

        self.num_nodes = num_nodes;
        self.node_arena = (0..total_num_nodes)
            .map(|_| MutexLike::new(PostFlopNode::default()))
            .collect::<Vec<_>>();
        self.clear_storage();

        let mut info = BuildTreeInfo {
            turn_index: num_nodes[0] as usize,
            river_index: (num_nodes[0] + num_nodes[1]) as usize,
            ..Default::default()
        };

        match self.tree_config.initial_state {
            BoardState::Flop => info.flop_index += 1,
            BoardState::Turn => info.turn_index += 1,
            BoardState::River => info.river_index += 1,
        }

        let mut root = self.node_arena[0].lock();
        root.turn = self.card_config.turn;
        root.river = self.card_config.river;

        self.build_tree_recursive(0, &self.action_root.lock(), &mut info);

        self.num_storage = info.num_storage;
        self.num_storage_ip = info.num_storage_ip;
        self.num_storage_chance = info.num_storage_chance;
        self.misc_memory_usage = self.memory_usage_internal();

        Ok(())
    }

    /// Initializes the interpreter.
    #[inline]
    pub(super) fn init_interpreter(&mut self) {
        let vecs = [
            vec![0.0; self.num_private_hands(0)],
            vec![0.0; self.num_private_hands(1)],
        ];

        self.weights = vecs.clone();
        self.normalized_weights = vecs.clone();
        self.cfvalues_cache = vecs;
    }

    /// Clears the storage.
    #[inline]
    fn clear_storage(&mut self) {
        self.storage1 = Vec::new();
        self.storage2 = Vec::new();
        self.storage_ip = Vec::new();
        self.storage_chance = Vec::new();
    }

    /// Counts the number of nodes in the game tree.
    #[inline]
    fn count_num_nodes(&self) -> [u64; 3] {
        let (turn_coef, river_coef) = match (self.card_config.turn, self.card_config.river) {
            (NOT_DEALT, _) => {
                let allowed_turns = if self.card_config.allowed_turn_cards.is_empty() {
                    49 - self.isomorphism_card_turn.len()
                } else {
                    self.card_config.allowed_turn_cards.len()
                };
                let mut river_coef = 0;
                let flop = self.card_config.flop;
                let skip_cards = &self.isomorphism_card_turn;
                let flop_mask: u64 = (1 << flop[0]) | (1 << flop[1]) | (1 << flop[2]);
                let skip_mask: u64 = skip_cards.iter().map(|&card| 1 << card).sum();
                let turns = if self.card_config.allowed_turn_cards.is_empty() {
                    (0..52).collect::<Vec<_>>()
                } else {
                    self.card_config.allowed_turn_cards.clone()
                };
                for turn in turns {
                    if (1 << turn) & (flop_mask | skip_mask) == 0 {
                        let allowed_rivers = if self.card_config.allowed_river_cards.is_empty() {
                            48 - self.isomorphism_card_river[turn & 3].len()
                        } else {
                            self.card_config.allowed_river_cards.len()
                        };
                        river_coef += allowed_rivers;
                    }
                }
                (allowed_turns, river_coef)
            }
            (turn, NOT_DEALT) => {
                let allowed_rivers = if self.card_config.allowed_river_cards.is_empty() {
                    48 - self.isomorphism_card_river[turn as usize & 3].len()
                } else {
                    self.card_config.allowed_river_cards.len()
                };
                (1, allowed_rivers)
            }
            _ => (0, 1),
        };

        let num_action_nodes = count_num_action_nodes(&self.action_root.lock());

        [
            num_action_nodes[0],
            num_action_nodes[1] * turn_coef as u64,
            num_action_nodes[2] * river_coef as u64,
        ]
    }

    /// Computes the memory usage of this struct.
    #[inline]
    fn memory_usage_internal(&self) -> u64 {
        // untracked: tree_config, action_root

        let mut memory_usage = mem::size_of::<Self>() as u64;

        memory_usage += vec_memory_usage(&self.added_lines);
        memory_usage += vec_memory_usage(&self.removed_lines);
        for line in &self.added_lines {
            memory_usage += vec_memory_usage(line);
        }
        for line in &self.removed_lines {
            memory_usage += vec_memory_usage(line);
        }

        memory_usage += vec_memory_usage(&self.valid_indices_turn);
        memory_usage += vec_memory_usage(&self.valid_indices_river);
        memory_usage += vec_memory_usage(&self.hand_strength);
        memory_usage += vec_memory_usage(&self.isomorphism_ref_turn);
        memory_usage += vec_memory_usage(&self.isomorphism_card_turn);
        memory_usage += vec_memory_usage(&self.isomorphism_ref_river);

        for refs in &self.isomorphism_ref_river {
            memory_usage += vec_memory_usage(refs);
        }

        for cards in &self.isomorphism_card_river {
            memory_usage += vec_memory_usage(cards);
        }

        for player in 0..2 {
            memory_usage += vec_memory_usage(&self.initial_weights[player]);
            memory_usage += vec_memory_usage(&self.private_cards[player]);
            memory_usage += vec_memory_usage(&self.same_hand_index[player]);
            memory_usage += vec_memory_usage(&self.valid_indices_flop[player]);
            for indices in &self.valid_indices_turn {
                memory_usage += vec_memory_usage(&indices[player]);
            }
            for indices in &self.valid_indices_river {
                memory_usage += vec_memory_usage(&indices[player]);
            }
            for strength in &self.hand_strength {
                memory_usage += vec_memory_usage(&strength[player]);
            }
            for swap in &self.isomorphism_swap_turn {
                memory_usage += vec_memory_usage(&swap[player]);
            }
            for swap_list in &self.isomorphism_swap_river {
                for swap in swap_list {
                    memory_usage += vec_memory_usage(&swap[player]);
                }
            }
        }

        memory_usage += vec_memory_usage(&self.node_arena);

        memory_usage
    }

    /// Builds the game tree recursively.
    fn build_tree_recursive(
        &self,
        node_index: usize,
        action_node: &ActionTreeNode,
        info: &mut BuildTreeInfo,
    ) {
        let mut node = self.node_arena[node_index].lock();
        node.player = action_node.player;
        node.amount = action_node.amount;

        if node.is_terminal() {
            return;
        }

        if node.is_chance() {
            self.push_chances(node_index, info);
            for action_index in 0..node.num_actions() {
                let child_index = node_index + node.children_offset as usize + action_index;
                self.build_tree_recursive(child_index, &action_node.children[0].lock(), info);
            }
        } else {
            self.push_actions(node_index, action_node, info);
            for action_index in 0..node.num_actions() {
                let child_index = node_index + node.children_offset as usize + action_index;
                self.build_tree_recursive(
                    child_index,
                    &action_node.children[action_index].lock(),
                    info,
                );
            }
        }
    }

    /// Pushes the chance actions to the `node`.
    fn push_chances(&self, node_index: usize, info: &mut BuildTreeInfo) {
        let mut node = self.node_arena[node_index].lock();
        let flop = self.card_config.flop;
        let flop_mask: u64 = (1 << flop[0]) | (1 << flop[1]) | (1 << flop[2]);

        if node.turn == NOT_DEALT {
            let allowed = if self.card_config.allowed_turn_cards.is_empty() {
                let skip_mask: u64 = self.isomorphism_card_turn.iter().map(|&card| 1 << card).sum();
                (0..52)
                    .filter(|&c| (1 << c) & (flop_mask | skip_mask) == 0)
                    .collect::<Vec<_>>()
            } else {
                self.card_config.allowed_turn_cards.clone()
            };
            node.children_offset = (info.turn_index - node_index) as u32;
            for card in allowed {
                node.num_children += 1;
                let mut child = node.children().last().unwrap().lock();
                child.prev_action = Action::Chance(card);
                child.turn = card;
            }
            info.turn_index += node.num_children as usize;
        } else {
            let turn_mask = flop_mask | (1 << node.turn);
            let allowed = if self.card_config.allowed_river_cards.is_empty() {
                let skip_mask: u64 = self.isomorphism_card_river[node.turn as usize & 3]
                    .iter()
                    .map(|&card| 1 << card)
                    .sum();
                (0..52)
                    .filter(|&c| (1 << c) & (turn_mask | skip_mask) == 0)
                    .collect::<Vec<_>>()
            } else {
                self.card_config.allowed_river_cards.clone()
            };
            node.children_offset = (info.river_index - node_index) as u32;
            for card in allowed {
                node.num_children += 1;
                let mut child = node.children().last().unwrap().lock();
                child.prev_action = Action::Chance(card);
                child.turn = node.turn;
                child.river = card;
            }
            info.river_index += node.num_children as usize;
        }

        node.num_elements = node
            .cfvalue_storage_player()
            .map_or(0, |player| self.num_private_hands(player)) as u32;

        info.num_storage_chance += node.num_elements as u64;
    }

    /// Pushes the actions to the `node`.
    fn push_actions(
        &self,
        node_index: usize,
        action_node: &ActionTreeNode,
        info: &mut BuildTreeInfo,
    ) {
        let mut node = self.node_arena[node_index].lock();

        let street = match (node.turn, node.river) {
            (NOT_DEALT, _) => BoardState::Flop,
            (_, NOT_DEALT) => BoardState::Turn,
            _ => BoardState::River,
        };

        let base = match street {
            BoardState::Flop => &mut info.flop_index,
            BoardState::Turn => &mut info.turn_index,
            BoardState::River => &mut info.river_index,
        };

        node.children_offset = (*base - node_index) as u32;
        node.num_children = action_node.children.len() as u16;
        *base += node.num_children as usize;

        for (child, action) in node.children().iter().zip(action_node.actions.iter()) {
            let mut child = child.lock();
            child.prev_action = *action;
            child.turn = node.turn;
            child.river = node.river;
        }

        let num_private_hands = self.num_private_hands(node.player as usize);
        node.num_elements = (node.num_actions() * num_private_hands) as u32;
        node.num_elements_ip = match node.prev_action {
            Action::None | Action::Chance(_) => self.num_private_hands(PLAYER_IP as usize) as u16,
            _ => 0,
        };

        info.num_storage += node.num_elements as u64;
        info.num_storage_ip += node.num_elements_ip as u64;
    }

    fn set_bunching_effect_internal(&mut self, bunching_data: &BunchingData) -> Result<(), String> {
        let num_turn = self.valid_indices_turn.len();
        let num_river = self.valid_indices_river.len();
        let num_cards = num_turn + num_river;

        self.bunching_num_dead_cards = bunching_data.num_dead_cards();
        self.bunching_num_combinations = bunching_data.num_combinations();

        let mut arena_size = 0;
        arena_size += num_cards * self.num_private_hands(0) * self.num_private_hands(1);
        arena_size += num_cards;

        let mut strength_size = 0;
        if self.tree_config.initial_state == BoardState::Flop {
            strength_size = num_turn * num_river;
        } else if self.tree_config.initial_state == BoardState::Turn {
            strength_size = num_river;
        } else {
            strength_size = 1;
        }

        self.bunching_arena = Vec::with_capacity(arena_size);
        self.bunching_strength = Vec::with_capacity(strength_size);

        self.bunching_num_flop = [
            vec![0; self.num_private_hands(0)],
            vec![0; self.num_private_hands(1)],
        ];
        self.bunching_num_turn = [
            vec![vec![0; self.num_private_hands(0)]; num_turn],
            vec![vec![0; self.num_private_hands(1)]; num_turn],
        ];
        self.bunching_num_river = [
            vec![vec![0; self.num_private_hands(0)]; num_river],
            vec![vec![0; self.num_private_hands(1)]; num_river],
        ];
        self.bunching_coef_flop = [
            vec![0; self.num_private_hands(0)],
            vec![0; self.num_private_hands(1)],
        ];
        self.bunching_coef_turn = [
            vec![vec![0; self.num_private_hands(0)]; num_turn],
            vec![vec![0; self.num_private_hands(1)]; num_turn],
        ];

        let mut bunching_strength = Vec::with_capacity(strength_size);
        if self.tree_config.initial_state == BoardState::Flop {
            for i in 0..num_turn {
                for j in 0..num_river {
                    bunching_strength.push([
                        bunching_data.hand_strength(0, i, j),
                        bunching_data.hand_strength(1, i, j),
                    ]);
                }
            }
        } else if self.tree_config.initial_state == BoardState::Turn {
            for i in 0..num_river {
                bunching_strength.push([
                    bunching_data.hand_strength(0, 0, i),
                    bunching_data.hand_strength(1, 0, i),
                ]);
            }
        } else {
            bunching_strength.push([
                bunching_data.hand_strength(0, 0, 0),
                bunching_data.hand_strength(1, 0, 0),
            ]);
        }

        let mut bunching_arena = Vec::with_capacity(arena_size);
        let mut index = 0;

        for i in 0..num_cards {
            let (turn_idx, river_idx) = if self.tree_config.initial_state == BoardState::Flop {
                (i / num_river, i % num_river)
            } else if self.tree_config.initial_state == BoardState::Turn {
                (0, i)
            } else {
                (0, 0)
            };
            let num_combinations = bunching_data.num_combinations_turn_river(turn_idx, river_idx);
            bunching_arena.push(num_combinations);
            index += 1;

            for j in 0..self.num_private_hands(0) {
                for k in 0..self.num_private_hands(1) {
                    let combinations =
                        bunching_data.combinations(turn_idx, river_idx, j as u16, k as u16);
                    bunching_arena.push(combinations);
                    index += 1;

                    if combinations > 0.0 {
                        if self.tree_config.initial_state == BoardState::Flop {
                            if river_idx == 0 {
                                self.bunching_num_turn[0][turn_idx][j] += 1;
                                self.bunching_num_turn[1][turn_idx][k] += 1;
                            }
                            self.bunching_num_river[0][river_idx][j] += 1;
                            self.bunching_num_river[1][river_idx][k] += 1;
                        } else if self.tree_config.initial_state == BoardState::Turn {
                            if i == 0 {
                                self.bunching_num_turn[0][0][j] += 1;
                                self.bunching_num_turn[1][0][k] += 1;
                            }
                            self.bunching_num_river[0][i][j] += 1;
                            self.bunching_num_river[1][i][k] += 1;
                        } else {
                            self.bunching_num_river[0][0][j] += 1;
                            self.bunching_num_river[1][0][k] += 1;
                        }
                        if turn_idx == 0 && river_idx == 0 {
                            self.bunching_num_flop[0][j] += 1;
                            self.bunching_num_flop[1][k] += 1;
                        }
                    }
                }
            }
        }

        assert_eq!(index, arena_size);
        self.bunching_strength = bunching_strength;
        self.bunching_arena = bunching_arena;

        for player in 0..2 {
            for i in 0..self.num_private_hands(player) {
                self.bunching_coef_flop[player][i] = self.bunching_num_flop[player][i];
                for j in 0..num_turn {
                    self.bunching_coef_turn[player][j][i] =
                        self.bunching_num_turn[player][j][i] + self.bunching_num_flop[player][i];
                }
            }
        }

        self.back_to_root();
        Ok(())
    }

    #[inline]
    fn memory_usage_bunching_internal(&self) -> u64 {
        let mut memory_usage = vec_memory_usage(&self.bunching_arena);
        memory_usage += vec_memory_usage(&self.bunching_strength);

        for player in 0..2 {
            memory_usage += vec_memory_usage(&self.bunching_num_flop[player]);
            memory_usage += vec_memory_usage(&self.bunching_num_turn[player]);
            memory_usage += vec_memory_usage(&self.bunching_num_river[player]);
            memory_usage += vec_memory_usage(&self.bunching_coef_flop[player]);
            memory_usage += vec_memory_usage(&self.bunching_coef_turn[player]);

            for turn in &self.bunching_num_turn[player] {
                memory_usage += vec_memory_usage(turn);
            }
            for river in &self.bunching_num_river[player] {
                memory_usage += vec_memory_usage(river);
            }
            for turn in &self.bunching_coef_turn[player] {
                memory_usage += vec_memory_usage(turn);
            }
        }

        memory_usage
    }

    fn remove_line_recursive(
        &self,
        node: &mut MutexGuardLike<PostFlopNode>,
        line: &[Action],
    ) -> Result<BuildTreeInfo, String> {
        if line.is_empty() {
            return Err("Empty line".to_string());
        }

        let mut info = BuildTreeInfo::default();

        if node.is_terminal() {
            return Err("Invalid line: terminal reached too early".to_string());
        }

        let action = line[0];
        let child_index = node
            .children()
            .iter()
            .position(|c| c.lock().prev_action == action);

        let child_index = match child_index {
            Some(index) => index,
            None => {
                return Err(format!(
                    "Invalid line: action {:?} not found",
                    action_to_string(action)
                ))
            }
        };

        if node.is_chance() {
            info.num_storage_chance += node.num_elements as u64;
        } else {
            info.num_storage += node.num_elements as u64;
            info.num_storage_ip += node.num_elements_ip as u64;
        }

        if line.len() == 1 {
            let offset = node.children_offset as usize;
            let base_index = match self.tree_config.initial_state {
                BoardState::Flop => offset + info.flop_index,
                BoardState::Turn => offset + info.turn_index,
                BoardState::River => offset + info.river_index,
            };
            let child_base_index = base_index + child_index;

            let child = &mut node.children()[child_index].lock();
            let num_removed_nodes = match self.tree_config.initial_state {
                BoardState::Flop => {
                    let turn_coef = self.num_nodes[1] / (self.num_nodes[0] + 1);
                    let river_coef = self.num_nodes[2] / (self.num_nodes[1] + turn_coef);
                    1 + turn_coef + river_coef
                }
                BoardState::Turn => {
                    let river_coef = self.num_nodes[2] / (self.num_nodes[1] + 1);
                    1 + river_coef
                }
                BoardState::River => 1,
            };

            for i in (child_base_index + 1)..(child_base_index + num_removed_nodes as usize) {
                let removed_node = self.node_arena[i].lock();
                if removed_node.is_chance() {
                    info.num_storage_chance += removed_node.num_elements as u64;
                } else if !removed_node.is_terminal() {
                    info.num_storage += removed_node.num_elements as u64;
                    info.num_storage_ip += removed_node.num_elements_ip as u64;
                }
            }

            if node.num_children == 1 {
                return Err("Cannot remove the last child".to_string());
            }

            node.num_children -= 1;
            node.children().swap(child_index, node.num_children as usize);

            match self.tree_config.initial_state {
                BoardState::Flop => info.flop_index += 1,
                BoardState::Turn => info.turn_index += 1,
                BoardState::River => info.river_index += 1,
            }

            return Ok(info);
        }

        let child = &mut node.children()[child_index];
        let child_info = self.remove_line_recursive(&mut child.lock(), &line[1..])?;

        info.flop_index += child_info.flop_index;
        info.turn_index += child_info.turn_index;
        info.river_index += child_info.river_index;
        info.num_storage += child_info.num_storage;
        info.num_storage_ip += child_info.num_storage_ip;
        info.num_storage_chance += child_info.num_storage_chance;

        Ok(info)
    }

    fn allocate_memory_nodes(&mut self) {
        let mut offset = 0;
        let mut offset_ip = 0;
        let mut offset_chance = 0;

        let num_bytes = if self.is_compression_enabled { 2 } else { 4 };
        let storage1_ptr = self.storage1.as_mut_ptr();
        let storage2_ptr = self.storage2.as_mut_ptr();
        let storage_ip_ptr = self.storage_ip.as_mut_ptr();
        let storage_chance_ptr = self.storage_chance.as_mut_ptr();

        for node in &self.node_arena {
            let mut node = node.lock();
            if node.is_terminal() {
                continue;
            }

            if node.is_chance() {
                node.storage1 = unsafe { storage_chance_ptr.add(offset_chance) };
                offset_chance += num_bytes * node.num_elements as usize;
            } else {
                node.storage1 = unsafe { storage1_ptr.add(offset) };
                node.storage2 = unsafe { storage2_ptr.add(offset) };
                offset += num_bytes * node.num_elements as usize;

                if node.num_elements_ip > 0 {
                    node.storage3 = unsafe { storage_ip_ptr.add(offset_ip) };
                    offset_ip += num_bytes * node.num_elements_ip as usize;
                }
            }
        }

        for (index, strategy) in &self.locking_strategy {
            let mut node = self.node_arena[*index].lock();
            node.is_locked = true;

            let strategy_slice = if self.is_compression_enabled {
                node.strategy_compressed_mut()
            } else {
                node.strategy_mut()
            };

            let mut sum = 0.0;
            for &s in strategy {
                sum += s;
            }
            if !sum.is_finite() || sum <= 0.0 {
                panic!("Invalid locking strategy: sum must be positive and finite");
            }

            if strategy.len() != strategy_slice.len() {
                panic!(
                    "Invalid locking strategy: length mismatch (expected: {}, actual: {})",
                    strategy_slice.len(),
                    strategy.len()
                );
            }

            if self.is_compression_enabled {
                let scale = i16::MAX as f32 / sum;
                node.set_strategy_scale(scale);
                for (i, &s) in strategy.iter().enumerate() {
                    let val = (s * scale).round() as i16;
                    strategy_slice[i] = val as u16;
                }
            } else {
                let scale = 1.0 / sum;
                node.set_strategy_scale(scale);
                for (i, &s) in strategy.iter().enumerate() {
                    strategy_slice[i] = s * scale;
                }
            }
        }
    }
}