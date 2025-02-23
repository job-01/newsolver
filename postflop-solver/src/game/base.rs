use super::*;
use crate::interface::*;
use crate::mutex_like::*;

#[cfg(feature = "bincode")]
use bincode::{Decode, Encode};

/// A node in the action tree.
///
/// The `player` field specifies the acting player (see [`PLAYER_OOP`] and [`PLAYER_IP`]).
/// When this node is terminal (i.e., fold or showdown), the `PLAYER_TERMINAL_FLAG` is set.
/// When this node is chance (i.e., turn/river deal), the `PLAYER_CHANCE_FLAG` is set.
///
/// The `amount` field specifies the total bet amount of the acting player.
///
/// The `num_chances` field specifies the number of chance actions when this node is chance.
/// This field is meaningful only immediately after the tree is built.
///
/// The `actions` field specifies the list of available actions.
///
/// The `children` field specifies the list of children nodes corresponding to each action.
#[repr(C)]
pub struct ActionTreeNode {
    player: u8,
    amount: i32,
    num_chances: usize,
    actions: Vec<Action>,
    children: Vec<MutexLike<ActionTreeNode>>,
}

/// A configuration of action tree.
///
/// The `initial_state` field specifies the initial street of the game.
///
/// The `starting_pot` field specifies the starting pot size.
///
/// The `effective_stack` field specifies the effective stack size.
///
/// The `rake_rate` field specifies the rake percentage (e.g. `0.05` means 5% rake).
///
/// The `rake_cap` field specifies the maximum rake amount.
///
/// The `*bet_sizes` fields specify the bet sizes for each street and player.
/// The first element is for OOP and the second element is for IP.
/// See [`BetSizeOptions`] for the specification of bet sizes.
///
/// # Examples
/// ```
/// use postflop_solver::*;
///
/// let config = TreeConfig {
///     initial_state: BoardState::Flop,
///     starting_pot: 100,
///     effective_stack: 1000,
///     rake_rate: 0.05,
///     rake_cap: 10.0,
///     flop_bet_sizes: [
///         BetSizeOptions::try_from(("50%", "100%")).unwrap(),
///         BetSizeOptions::try_from(("50%", "100%")).unwrap(),
///     ],
///     turn_bet_sizes: [
///         BetSizeOptions::try_from(("50%", "100%")).unwrap(),
///         BetSizeOptions::try_from(("50%", "100%")).unwrap(),
///     ],
///     river_bet_sizes: [
///         BetSizeOptions::try_from(("50%", "100%")).unwrap(),
///         BetSizeOptions::try_from(("50%", "100%")).unwrap(),
///     ],
/// };
/// ```
#[derive(Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub struct TreeConfig {
    pub initial_state: BoardState,
    pub starting_pot: i32,
    pub effective_stack: i32,
    pub rake_rate: f64,
    pub rake_cap: f64,
    pub flop_bet_sizes: [BetSizeOptions; 2],
    pub turn_bet_sizes: [BetSizeOptions; 2],
    pub river_bet_sizes: [BetSizeOptions; 2],
}

/// A struct representing an action tree.
///
/// A game tree consists of an action tree and chance nodes (turn/river deals).
/// This struct only represents the action tree part, and the chance nodes are handled by
/// [`PostFlopGame`].
///
/// See [`ActionTreeNode`] and [`TreeConfig`] for details.
///
/// # Examples
/// ```
/// use postflop_solver::*;
///
/// let config = TreeConfig {
///     initial_state: BoardState::Flop,
///     starting_pot: 100,
///     effective_stack: 1000,
///     ..Default::default()
/// };
/// let tree = ActionTree::new(config).unwrap();
/// ```
#[derive(Default)]
pub struct ActionTree {
    config: TreeConfig,
    added_lines: Vec<Vec<Action>>,
    removed_lines: Vec<Vec<Action>>,
    root: Box<MutexLike<ActionTreeNode>>,
}

impl ActionTree {
    /// Creates a new [`ActionTree`] with the specified configuration.
    pub fn new(config: TreeConfig) -> Result<Self, String> {
        let mut tree = Self {
            config,
            ..Default::default()
        };
        tree.build_tree_recursive(&mut tree.root.lock(), Action::None);
        Ok(tree)
    }

    /// Returns the list of invalid terminals.
    ///
    /// An invalid terminal is a terminal node that is not an all-in or a showdown.
    /// Such a terminal node is not allowed in the game tree because it violates the rule of poker.
    pub fn invalid_terminals(&self) -> Vec<Vec<Action>> {
        let mut invalid_terminals = Vec::new();
        self.check_invalid_terminal_recursive(
            &self.root.lock(),
            &mut Vec::new(),
            &mut invalid_terminals,
        );
        invalid_terminals
    }

    fn check_invalid_terminal_recursive(
        &self,
        node: &ActionTreeNode,
        history: &mut Vec<Action>,
        invalid_terminals: &mut Vec<Vec<Action>>,
    ) {
        if node.is_terminal() {
            if node.amount != self.config.effective_stack {
                invalid_terminals.push(history.clone());
            }
            return;
        }

        for (i, action) in node.actions.iter().enumerate() {
            history.push(*action);
            self.check_invalid_terminal_recursive(
                &node.children[i].lock(),
                history,
                invalid_terminals,
            );
            history.pop();
        }
    }

    /// Ejects the internal data.
    pub fn eject(
        self,
    ) -> (
        TreeConfig,
        Vec<Vec<Action>>,
        Vec<Vec<Action>>,
        Box<MutexLike<ActionTreeNode>>,
    ) {
        (self.config, self.added_lines, self.removed_lines, self.root)
    }

    fn build_tree_recursive(&mut self, node: &mut ActionTreeNode, prev_action: Action) {
        let amount = node.amount;
        let effective_stack = self.config.effective_stack;
        let starting_pot = self.config.starting_pot;

        node.player = match prev_action {
            Action::None => PLAYER_OOP,
            Action::Chance(_) => {
                PLAYER_CHANCE_FLAG | if node.player == PLAYER_OOP { PLAYER_IP } else { PLAYER_OOP }
            }
            _ => PLAYER_TERMINAL_FLAG | node.player,
        };

        if node.is_terminal() || amount == effective_stack {
            return;
        }

        if node.is_chance() {
            let mut num_chances = 0;
            let flop_mask: u64 = self.config.flop.iter().map(|&c| 1 << c).sum();
            if prev_action == Action::None { // Flop to Turn
                let allowed = if self.config.allowed_turn_cards.is_empty() {
                    (0..52).filter(|&c| (1 << c) & flop_mask == 0).collect::<Vec<_>>()
                } else {
                    self.config.allowed_turn_cards.clone()
                };
                for card in allowed {
                    node.push_action(Action::Chance(card));
                    num_chances += 1;
                }
            } else if let Action::Chance(turn_card) = prev_action { // Turn to River
                let turn_mask = flop_mask | (1 << turn_card);
                let allowed = if self.config.allowed_river_cards.is_empty() {
                    (0..52).filter(|&c| (1 << c) & turn_mask == 0).collect::<Vec<_>>()
                } else {
                    self.config.allowed_river_cards.clone()
                };
                for card in allowed {
                    node.push_action(Action::Chance(card));
                    num_chances += 1;
                }
            }
            node.num_chances = num_chances;
        } else {
            self.push_actions(node);
        }

        for child in &node.children {
            self.build_tree_recursive(&mut child.lock(), node.actions[node.children.len() - 1]);
        }
    }

    fn push_actions(&mut self, node: &mut ActionTreeNode) {
        let amount = node.amount;
        let effective_stack = self.config.effective_stack;
        let starting_pot = self.config.starting_pot;

        let pot = starting_pot + 2 * amount;
        let stack = effective_stack - amount;
        let player = node.player as usize;
        let prev_action = node.actions.last().copied().unwrap_or(Action::None);
        let street = match self.config.initial_state {
            BoardState::Flop => match prev_action {
                Action::None => BoardState::Flop,
                Action::Chance(_) => BoardState::Turn,
                _ => BoardState::River,
            },
            BoardState::Turn => match prev_action {
                Action::None => BoardState::Turn,
                _ => BoardState::River,
            },
            BoardState::River => BoardState::River,
        };

        let bet_sizes = match street {
            BoardState::Flop => &self.config.flop_bet_sizes[player],
            BoardState::Turn => &self.config.turn_bet_sizes[player],
            BoardState::River => &self.config.river_bet_sizes[player],
        };

        node.push_action(Action::Check);

        if prev_action == Action::Check {
            node.push_action(Action::Bet(bet_sizes.compute_bet_size(pot, stack)));
        }

        if stack > 0 {
            node.push_action(Action::AllIn(stack));
        }
    }
}

impl ActionTreeNode {
    #[inline]
    fn is_terminal(&self) -> bool {
        self.player & PLAYER_TERMINAL_FLAG != 0
    }

    #[inline]
    fn is_chance(&self) -> bool {
        self.player & PLAYER_CHANCE_FLAG != 0
    }

    #[inline]
    fn push_action(&mut self, action: Action) {
        self.actions.push(action);
        self.children.push(MutexLike::new(ActionTreeNode {
            player: self.player,
            amount: self.amount + action.amount(),
            ..Default::default()
        }));
    }
}

impl Default for ActionTreeNode {
    #[inline]
    fn default() -> Self {
        Self {
            player: 0,
            amount: 0,
            num_chances: 0,
            actions: Vec::new(),
            children: Vec::new(),
        }
    }
}

#[cfg(feature = "bincode")]
impl Encode for ActionTreeNode {
    fn encode<E: bincode::enc::Encoder>(
        &self,
        encoder: &mut E,
    ) -> Result<(), bincode::error::EncodeError> {
        self.player.encode(encoder)?;
        self.amount.encode(encoder)?;
        self.num_chances.encode(encoder)?;
        self.actions.encode(encoder)?;
        self.children.encode(encoder)?;
        Ok(())
    }
}

#[cfg(feature = "bincode")]
impl Decode for ActionTreeNode {
    fn decode<D: bincode::de::Decoder>(
        decoder: &mut D,
    ) -> Result<Self, bincode::error::DecodeError> {
        Ok(Self {
            player: Decode::decode(decoder)?,
            amount: Decode::decode(decoder)?,
            num_chances: Decode::decode(decoder)?,
            actions: Decode::decode(decoder)?,
            children: Decode::decode(decoder)?,
        })
    }
}

/// Counts the number of action nodes in each street.
pub fn count_num_action_nodes(root: &ActionTreeNode) -> [u64; 3] {
    let mut num_nodes = [0; 3];
    count_num_action_nodes_recursive(root, &mut num_nodes, BoardState::Flop);
    num_nodes
}

fn count_num_action_nodes_recursive(
    node: &ActionTreeNode,
    num_nodes: &mut [u64; 3],
    street: BoardState,
) {
    if node.is_terminal() || node.is_chance() {
        return;
    }

    match street {
        BoardState::Flop => num_nodes[0] += 1,
        BoardState::Turn => num_nodes[1] += 1,
        BoardState::River => num_nodes[2] += 1,
    }

    let next_street = if node.actions.iter().any(|&a| a == Action::Check) {
        match street {
            BoardState::Flop => BoardState::Turn,
            BoardState::Turn => BoardState::River,
            BoardState::River => BoardState::River,
        }
    } else {
        street
    };

    for child in &node.children {
        count_num_action_nodes_recursive(&child.lock(), num_nodes, next_street);
    }
}