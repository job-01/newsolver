use super::*;
use crate::interface::*;
use crate::sliceop::*;
use crate::utility::*;

#[inline]
fn decode_signed_slice(slice: &[i16], scale: f32) -> Vec<f32> {
    let decoder = scale / i16::MAX as f32;
    slice.iter().map(|&x| x as f32 * decoder).collect()
}

impl PostFlopGame {
    /// Resets the current node to the root node and clears the history.
    #[inline]
    pub fn back_to_root(&mut self) {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        self.action_history.clear();
        self.node_history.clear();
        self.is_normalized_weight_cached = false;
        self.turn = self.card_config.turn;
        self.river = self.card_config.river;
        self.turn_swapped_suit = None;
        self.turn_swap = None;
        self.river_swap = None;
        self.total_bet_amount = [0, 0];

        self.weights[0].copy_from_slice(&self.initial_weights[0]);
        self.weights[1].copy_from_slice(&self.initial_weights[1]);
        self.assign_zero_weights();
    }

    /// Returns the action history of the current node.
    #[inline]
    pub fn history(&self) -> &[usize] {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        &self.action_history
    }

    /// Applies the specified history to reach a node.
    #[inline]
    pub fn apply_history(&mut self, history: &[usize]) {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        self.back_to_root();
        for &action in history {
            self.play(action);
        }
    }

    /// Returns whether the current node is a terminal node.
    #[inline]
    pub fn is_terminal_node(&self) -> bool {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        let node = self.node();
        node.is_terminal() || node.amount == self.tree_config.effective_stack
    }

    /// Returns whether the current node is a chance node.
    #[inline]
    pub fn is_chance_node(&self) -> bool {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        self.node().is_chance() && !self.is_terminal_node()
    }

    /// Returns the list of available actions in the current node.
    #[inline]
    pub fn available_actions(&self) -> Vec<Action> {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        if self.is_terminal_node() {
            Vec::new()
        } else {
            self.node()
                .children()
                .iter()
                .map(|c| c.lock().prev_action)
                .collect()
        }
    }

    /// Returns the bitmask of possible chance cards in the current node.
    ///
    /// Returns `0` if the current node is not a chance node.
    pub fn possible_cards(&self) -> u64 {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        if !self.is_chance_node() {
            return 0;
        }

        let flop_mask: u64 = self.card_config.flop.iter().map(|&c| 1 << c).sum();
        let board_mask = if self.turn != NOT_DEALT {
            flop_mask | (1 << self.turn)
        } else {
            flop_mask
        };
        let allowed = if self.turn == NOT_DEALT {
            if self.card_config.allowed_turn_cards.is_empty() {
                (0..52)
                    .filter(|&c| (1 << c) & flop_mask == 0)
                    .collect::<Vec<_>>()
            } else {
                self.card_config.allowed_turn_cards.clone()
            }
        } else {
            if self.card_config.allowed_river_cards.is_empty() {
                (0..52)
                    .filter(|&c| (1 << c) & board_mask == 0)
                    .collect::<Vec<_>>()
            } else {
                self.card_config.allowed_river_cards.clone()
            }
        };
        let mut mask = 0;
        for card in allowed {
            if !self.private_cards[0]
                .iter()
                .any(|&(c1, c2)| c1 == card || c2 == card)
                && !self.private_cards[1]
                    .iter()
                    .any(|&(c1, c2)| c1 == card || c2 == card)
            {
                mask |= 1 << card;
            }
        }
        mask
    }

    /// Returns the current player in the current node.
    #[inline]
    pub fn current_player(&self) -> usize {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        self.node().player()
    }

    /// Returns the current board cards.
    #[inline]
    pub fn current_board(&self) -> Vec<u8> {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        let mut ret = self.card_config.flop.to_vec();
        if self.turn != NOT_DEALT {
            ret.push(self.turn);
        }
        if self.river != NOT_DEALT {
            ret.push(self.river);
        }
        ret
    }

    /// Plays the specified action and updates the current node accordingly.
    pub fn play(&mut self, action: usize) {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if self.is_terminal_node() {
            panic!("Terminal node is not allowed");
        }

        if self.is_chance_node() {
            let is_turn = self.turn == NOT_DEALT;
            if self.storage_mode == BoardState::Flop
                || (!is_turn && self.storage_mode == BoardState::Turn)
            {
                panic!("Storage mode is not compatible");
            }
        }

        let node = self.node();
        let next_node = node.play(action);
        let next_action = next_node.prev_action;

        self.action_history.push(action);

        let node_index = if self.node_history.is_empty() {
            0
        } else {
            self.node_history.last().unwrap() + node.children_offset as usize + action
        };
        self.node_history.push(node_index);

        if next_node.is_chance() {
            if self.turn == NOT_DEALT {
                self.turn = next_action.unwrap_chance();
                let ref_idx = self.isomorphism_ref_turn[action];
                if ref_idx != action as u8 {
                    self.turn_swap = Some(ref_idx);
                    let card = self.isomorphism_card_turn[action];
                    let suit = card & 3;
                    self.turn_swapped_suit = Some((suit, self.isomorphism_card_turn[ref_idx as usize] & 3));
                    self.swap_weights(self.isomorphism_swap_turn[suit as usize]);
                }
            } else {
                self.river = next_action.unwrap_chance();
                let ref_idx = self.isomorphism_ref_river[self.turn as usize & 3][action];
                if ref_idx != action as u8 {
                    self.river_swap = Some((self.turn & 3, ref_idx));
                    let card = self.isomorphism_card_river[self.turn as usize & 3][action];
                    let suit = card & 3;
                    self.swap_weights(self.isomorphism_swap_river[self.turn as usize & 3][suit as usize]);
                }
            }
        } else {
            self.total_bet_amount[node.player()] = next_node.amount;
            self.update_weights(&next_node, action);
        }

        self.is_normalized_weight_cached = false;
    }

    /// Returns the total bet amount of the specified player.
    #[inline]
    pub fn total_bet_amount(&self, player: usize) -> i32 {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        self.total_bet_amount[player]
    }

    /// Locks the strategy of the current node with the specified strategy.
    pub fn lock_current_strategy(&mut self, strategy: &[f32]) {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if self.is_terminal_node() || self.is_chance_node() {
            panic!("Terminal or chance nodes cannot be locked");
        }

        let node = self.node();
        let num_elements = node.num_elements as usize;
        let num_actions = node.num_actions();

        if num_elements != num_actions * self.num_private_hands(node.player()) {
            panic!("Number of elements does not match");
        }

        if strategy.len() != num_elements {
            panic!("Strategy length does not match");
        }

        let mut sum = vec![0.0; self.num_private_hands(node.player())];
        for (i, &s) in strategy.iter().enumerate() {
            if !s.is_finite() || s < 0.0 {
                panic!("Strategy must be non-negative and finite");
            }
            sum[i / num_actions] += s;
        }

        if sum.iter().any(|&s| s <= 0.0) {
            panic!("Sum of strategy must be positive");
        }

        let strategy_slice = if self.is_compression_enabled {
            node.strategy_compressed_mut()
        } else {
            node.strategy_mut()
        };

        if self.is_compression_enabled {
            let scale_strategy = sum
                .iter()
                .map(|&s| if s > 0.0 { (i16::MAX as f32) / s } else { 0.0 })
                .collect::<Vec<_>>();
            node.set_strategy_scale(1.0);
            for (i, &s) in strategy.iter().enumerate() {
                let val = (s * scale_strategy[i / num_actions]).round() as i16;
                strategy_slice[i] = val as u16;
            }
        } else {
            let scale_strategy = sum
                .into_iter()
                .map(|s| if s > 0.0 { 1.0 / s } else { 0.0 })
                .collect::<Vec<_>>();
            node.set_strategy_scale(1.0);
            strategy_slice
                .iter_mut()
                .zip(strategy)
                .enumerate()
                .for_each(|(i, (dst, &src))| *dst = src * scale_strategy[i / num_actions]);
        }

        let curr_index = *self.node_history.last().unwrap_or(&0);
        self.locking_strategy.insert(curr_index, strategy.to_vec());
        node.is_locked = true;
    }

    /// Returns the strategy of the current node.
    #[inline]
    pub fn strategy(&self) -> Vec<f32> {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if self.is_terminal_node() || self.is_chance_node() {
            return Vec::new();
        }

        let node = self.node();
        if self.is_compression_enabled {
            decode_unsigned_slice(node.strategy_compressed(), node.strategy_scale())
        } else {
            let mut strategy = Vec::from(node.strategy());
            scale_slice(&mut strategy, node.strategy_scale());
            strategy
        }
    }

    /// Returns the current weights of private hands of the specified player.
    #[inline]
    pub fn current_weights(&self, player: usize) -> &[f32] {
        if self.state <= State::Uninitialized {
            panic!("Game is not successfully initialized");
        }

        &self.weights[player]
    }

    /// Computes and caches the normalized weights of private hands at the current node.
    #[inline]
    pub fn cache_normalized_weights(&mut self) {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if self.is_normalized_weight_cached {
            return;
        }

        self.normalized_weights[0].copy_from_slice(&self.weights[0]);
        self.normalized_weights[1].copy_from_slice(&self.weights[1]);

        let mut sum = [0.0; 2];
        for player in 0..2 {
            sum[player] = self.normalized_weights[player].iter().sum();
        }

        if sum[0] <= 0.0 || !sum[0].is_finite() || sum[1] <= 0.0 || !sum[1].is_finite() {
            panic!("Invalid weights: sum must be positive and finite");
        }

        scale_slice(&mut self.normalized_weights[0], 1.0 / sum[0]);
        scale_slice(&mut self.normalized_weights[1], 1.0 / sum[1]);
        self.is_normalized_weight_cached = true;
    }

    /// Returns the normalized weights of private hands of the specified player.
    #[inline]
    pub fn normalized_weights(&self, player: usize) -> &[f32] {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if !self.is_normalized_weight_cached {
            panic!("Normalized weights are not cached");
        }

        &self.normalized_weights[player]
    }

    /// Returns the equity of private hands of the specified player.
    pub fn equity(&self, player: usize) -> Vec<f32> {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if !self.is_terminal_node() {
            panic!("Equity is only available at terminal nodes");
        }

        let mut result = vec![MaybeUninit::<f32>::uninit(); self.num_private_hands(player)];
        self.evaluate(&mut result, &self.node(), player, &self.weights[player ^ 1]);
        result
            .into_iter()
            .map(|x| unsafe { x.assume_init() })
            .collect()
    }

    /// Returns the expected values of private hands of the specified player.
    pub fn expected_values(&self, player: usize) -> Vec<f32> {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if !self.is_terminal_node() {
            let node = self.node();
            if node.has_cfvalues_ip() {
                let cfvalues = if self.is_compression_enabled {
                    decode_signed_slice(node.cfvalues_ip_compressed(), node.cfvalue_ip_scale())
                } else {
                    let mut cfvalues = Vec::from(node.cfvalues_ip());
                    scale_slice(&mut cfvalues, node.cfvalue_ip_scale());
                    cfvalues
                };
                return if player == PLAYER_OOP { cfvalues } else { vec![0.0; cfvalues.len()] };
            }
            panic!("Expected values are only available at terminal nodes");
        }

        self.cfvalues_cache[player].copy_from_slice(&self.weights[player]);
        let mut result = vec![MaybeUninit::<f32>::uninit(); self.num_private_hands(player)];
        self.evaluate(&mut result, &self.node(), player, &self.cfvalues_cache[player]);
        result
            .into_iter()
            .map(|x| unsafe { x.assume_init() })
            .collect()
    }

    /// Returns the detailed expected values of private hands of the specified player.
    pub fn expected_values_detail(&self, player: usize) -> (Vec<f32>, Vec<f32>, Vec<f32>) {
        if self.state < State::MemoryAllocated {
            panic!("Memory is not allocated");
        }

        if !self.is_terminal_node() {
            panic!("Expected values are only available at terminal nodes");
        }

        let node = self.node();

        let mut ev_showdown_win = vec![0.0; self.num_private_hands(player)];
        let mut ev_showdown_lose = vec![0.0; self.num_private_hands(player)];
        let mut ev_fold = vec![0.0; self.num_private_hands(player)];

        let pot_size = self.tree_config.starting_pot + self.total_bet_amount[0] + self.total_bet_amount[1];
        let amount_to_call = (self.total_bet_amount[player ^ 1] - self.total_bet_amount[player]).max(0);
        let rake_factor = if self.is_raked() {
            let rake = (pot_size as f64 * self.tree_config.rake_rate).min(self.tree_config.rake_cap);
            1.0 - rake / pot_size as f64
        } else {
            1.0
        };

        if amount_to_call > 0 {
            let sign = if player == PLAYER_OOP { 1 } else { -1 };
            for (i, ev) in ev_fold.iter_mut().enumerate() {
                *ev = -sign as f32 * self.total_bet_amount[player] as f32;
            }
        } else {
            self.cfvalues_cache[player].copy_from_slice(&self.weights[player]);

            let mut result = vec![MaybeUninit::<f32>::uninit(); self.num_private_hands(player)];
            self.evaluate_terminal_showdown(&mut result, &node, player, &self.cfvalues_cache[player]);
            let result: Vec<f32> = result.into_iter().map(|x| unsafe { x.assume_init() }).collect();

            let pot_oop = self.total_bet_amount[PLAYER_OOP] as f32;
            let pot_ip = self.total_bet_amount[PLAYER_IP] as f32;

            if player == PLAYER_OOP {
                for (i, &r) in result.iter().enumerate() {
                    ev_showdown_win[i] = r.max(0.0) * rake_factor;
                    ev_showdown_lose[i] = r.min(0.0);
                }
            } else {
                for (i, &r) in result.iter().enumerate() {
                    ev_showdown_win[i] = (pot_oop + pot_ip - r).max(0.0) * rake_factor - pot_ip;
                    ev_showdown_lose[i] = (pot_oop + pot_ip - r).min(0.0) - pot_ip;
                }
            }
        }

        (ev_showdown_win, ev_showdown_lose, ev_fold)
    }

    #[inline]
    fn node(&self) -> MutexGuardLike<PostFlopNode> {
        let index = *self.node_history.last().unwrap_or(&0);
        self.node_arena[index].lock()
    }

    #[inline]
    fn node_index(&self, node: &PostFlopNode) -> usize {
        let this = self as *const Self;
        let node = node as *const PostFlopNode;
        let node_array = self.node_arena.as_ptr() as *const PostFlopNode;

        // This is safe because `node` is a reference to an element of `self.node_arena`
        unsafe { assert_eq!(this as usize % mem::align_of::<Self>(), 0); }
        unsafe { node.offset_from(node_array) as usize }
    }

    fn assign_zero_weights(&mut self) {
        let mut board_mask: u64 = (1 << self.card_config.flop[0])
            | (1 << self.card_config.flop[1])
            | (1 << self.card_config.flop[2]);
        if self.turn != NOT_DEALT {
            board_mask |= 1 << self.turn;
        }
        if self.river != NOT_DEALT {
            board_mask |= 1 << self.river;
        }

        for player in 0..2 {
            let hands = &self.private_cards[player];
            for (i, &(c1, c2)) in hands.iter().enumerate() {
                let mask = (1 << c1) | (1 << c2);
                if mask & board_mask != 0 {
                    self.weights[player][i] = 0.0;
                }
            }
        }
    }

    fn update_weights(&mut self, node: &MutexGuardLike<PostFlopNode>, action_idx: usize) {
        let acting_player = node.player();
        let strategy = if self.is_compression_enabled {
            decode_unsigned_slice(node.strategy_compressed(), node.strategy_scale())
        } else {
            let mut strategy = Vec::from(node.strategy());
            scale_slice(&mut strategy, node.strategy_scale());
            strategy
        };

        let num_private_hands = self.num_private_hands(acting_player);
        let num_actions = node.num_actions();

        for i in 0..num_private_hands {
            self.weights[acting_player][i] *= strategy[i * num_actions + action_idx];
        }
    }

    fn swap_weights(&mut self, swap_list: [Vec<(u16, u16)>; 2]) {
        for player in 0..2 {
            for &(i, j) in &swap_list[player] {
                self.weights[player].swap(i as usize, j as usize);
            }
        }
    }
}