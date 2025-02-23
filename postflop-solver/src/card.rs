use crate::hand::*;
use crate::range::*;
use once_cell::sync::Lazy;
use regex::Regex;
use std::collections::BTreeSet;
use std::fmt::Write;
use std::str::FromStr;

#[cfg(feature = "bincode")]
use bincode::{Decode, Encode};

pub type Card = u8;

pub const NOT_DEALT: Card = 255;

/// A configuration of cards.
///
/// This struct specifies the initial ranges of players and the board cards.
///
/// See [`Range`] for the specification of range strings.
///
/// # Examples
/// ```
/// use postflop_solver::*;
///
/// let config = CardConfig {
///     range: ["AKs,QQ+".parse().unwrap(), "AA,KK,QQ".parse().unwrap()],
///     flop: flop_from_str("Td9d6h").unwrap(),
///     ..Default::default()
/// };
/// ```
#[derive(Debug, Clone, Copy, PartialEq)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub struct CardConfig {
    pub range: [Range; 2],
    pub flop: [Card; 3],
    pub turn: Card,
    pub river: Card,
    pub allowed_turn_cards: Vec<Card>,
    pub allowed_river_cards: Vec<Card>,
}

/// A struct representing hand strength.
///
/// The `strength` field represents the strength of the hand (larger values means stronger hands).
/// The `index` field represents the index of the private hand in the list returned by
/// [`Range::get_hands_weights`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "bincode", derive(Decode, Encode))]
pub struct StrengthItem {
    pub strength: u16,
    pub index: u16,
}

type SwapList = [Vec<(u16, u16)>; 2];

impl Card {
    /// Returns the minimum of two cards.
    #[inline]
    pub fn min(card1: Card, card2: Card) -> Card {
        if card1 < card2 {
            card1
        } else {
            card2
        }
    }

    /// Returns the maximum of two cards.
    #[inline]
    pub fn max(card1: Card, card2: Card) -> Card {
        if card1 > card2 {
            card1
        } else {
            card2
        }
    }
}

impl Default for CardConfig {
    #[inline]
    fn default() -> Self {
        Self {
            range: [Range::new(), Range::new()],
            flop: [NOT_DEALT; 3],
            turn: NOT_DEALT,
            river: NOT_DEALT,
            allowed_turn_cards: Vec::new(),
            allowed_river_cards: Vec::new(),
        }
    }
}

/// Parses a comma-separated list of card strings into a vector of Cards.
///
/// # Examples
/// ```
/// use postflop_solver::*;
/// assert_eq!(parse_card_list("Qc,Kd,As"), Ok(vec![47, 45, 51]));
/// assert_eq!(parse_card_list(""), Ok(vec![]));
/// assert!(parse_card_list("Invalid").is_err());
/// ```
#[inline]
pub fn parse_card_list(s: &str) -> Result<Vec<Card>, String> {
    if s.trim().is_empty() {
        return Ok(Vec::new());
    }
    s.split(',')
        .map(|card_str| card_from_str(card_str.trim()))
        .collect()
}

impl CardConfig {
    /// Creates a new [`CardConfig`] from strings.
    ///
    /// # Examples
    /// ```
    /// use postflop_solver::*;
    ///
    /// let config = CardConfig::with_str_config(
    ///     "AKs,QQ+",
    ///     "AA,KK,QQ",
    ///     "Td9d6h",
    ///     Some("Qc,Kd"),  // Allowed turn cards
    ///     Some("As"),     // Allowed river cards
    /// ).unwrap();
    /// ```
    pub fn with_str_config(
        oop_range: &str,
        ip_range: &str,
        flop: &str,
        turn_cards: Option<&str>,
        river_cards: Option<&str>,
    ) -> Result<Self, String> {
        let mut config = Self {
            range: [oop_range.parse()?, ip_range.parse()?],
            flop: flop_from_str(flop)?,
            turn: NOT_DEALT,
            river: NOT_DEALT,
            allowed_turn_cards: turn_cards
                .map(parse_card_list)
                .transpose()?
                .unwrap_or_default(),
            allowed_river_cards: river_cards
                .map(parse_card_list)
                .transpose()?
                .unwrap_or_default(),
        };
        config.check_validity()?;
        Ok(config)
    }

    fn check_validity(&self) -> Result<(), String> {
        let mut board_cards = BTreeSet::new();
        for &card in &self.flop {
            if card == NOT_DEALT {
                return Err("Flop cards not initialized".to_string());
            }
            if card >= 52 {
                return Err(format!("Invalid flop card: {}", card));
            }
            if !board_cards.insert(card) {
                return Err("Duplicate flop card".to_string());
            }
        }
        if self.turn != NOT_DEALT {
            if self.turn >= 52 {
                return Err(format!("Invalid turn card: {}", self.turn));
            }
            if !board_cards.insert(self.turn) {
                return Err("Turn card overlaps with flop".to_string());
            }
        }
        if self.river != NOT_DEALT {
            if self.river >= 52 {
                return Err(format!("Invalid river card: {}", self.river));
            }
            if !board_cards.insert(self.river) {
                return Err("River card overlaps with flop or turn".to_string());
            }
            if self.turn == NOT_DEALT {
                return Err("River specified without turn".to_string());
            }
        }
        for &card in &self.allowed_turn_cards {
            if card >= 52 {
                return Err(format!("Invalid turn card: {}", card));
            }
            if board_cards.contains(&card) {
                return Err(format!(
                    "Allowed turn card {} overlaps with board",
                    card_to_string(card)?
                ));
            }
            board_cards.insert(card);
        }
        for &card in &self.allowed_river_cards {
            if card >= 52 {
                return Err(format!("Invalid river card: {}", card));
            }
            if board_cards.contains(&card) {
                return Err(format!(
                    "Allowed river card {} overlaps with board or turn cards",
                    card_to_string(card)?
                ));
            }
            board_cards.insert(card);
        }
        Ok(())
    }

    /// Computes valid hand indices for flop, turn, and river states.
    pub(super) fn valid_indices(
        &self,
        private_cards: &[(Card, Card); 2],
    ) -> ([Vec<u16>; 2], Vec<[Vec<u16>; 2]>, Vec<[Vec<u16>; 2]>) {
        let mut flop_mask: u64 = (1 << self.flop[0]) | (1 << self.flop[1]) | (1 << self.flop[2]);
        let turn_exists = self.turn != NOT_DEALT;
        let river_exists = self.river != NOT_DEALT;

        if turn_exists {
            flop_mask |= 1 << self.turn;
        }
        if river_exists {
            flop_mask |= 1 << self.river;
        }

        let mut valid_indices_flop = [Vec::new(), Vec::new()];
        let mut valid_indices_turn = Vec::new();
        let mut valid_indices_river = Vec::new();

        for player in 0..2 {
            let hands = &private_cards[player];
            let mut indices = Vec::with_capacity(hands.len());

            for (i, &(c1, c2)) in hands.iter().enumerate() {
                let mask: u64 = (1 << c1) | (1 << c2);
                if mask & flop_mask == 0 {
                    indices.push(i as u16);
                }
            }

            valid_indices_flop[player] = indices;
        }

        if !turn_exists {
            let allowed_turns = if self.allowed_turn_cards.is_empty() {
                let mut all_cards = Vec::with_capacity(52 - self.flop.len());
                for card in 0..52 {
                    if (1 << card) & flop_mask == 0 {
                        all_cards.push(card);
                    }
                }
                all_cards
            } else {
                self.allowed_turn_cards.clone()
            };

            for &turn in &allowed_turns {
                let mut indices = [Vec::new(), Vec::new()];
                let turn_mask = flop_mask | (1 << turn);

                for player in 0..2 {
                    let hands = &private_cards[player];
                    let mut player_indices = Vec::with_capacity(hands.len());

                    for (i, &(c1, c2)) in hands.iter().enumerate() {
                        let mask: u64 = (1 << c1) | (1 << c2);
                        if mask & turn_mask == 0 {
                            player_indices.push(i as u16);
                        }
                    }

                    indices[player] = player_indices;
                }

                valid_indices_turn.push(indices);
            }
        }

        if turn_exists && !river_exists {
            let allowed_rivers = if self.allowed_river_cards.is_empty() {
                let mut all_cards = Vec::with_capacity(52 - 4);
                for card in 0..52 {
                    if (1 << card) & flop_mask == 0 {
                        all_cards.push(card);
                    }
                }
                all_cards
            } else {
                self.allowed_river_cards.clone()
            };

            for &river in &allowed_rivers {
                let mut indices = [Vec::new(), Vec::new()];
                let river_mask = flop_mask | (1 << river);

                for player in 0..2 {
                    let hands = &private_cards[player];
                    let mut player_indices = Vec::with_capacity(hands.len());

                    for (i, &(c1, c2)) in hands.iter().enumerate() {
                        let mask: u64 = (1 << c1) | (1 << c2);
                        if mask & river_mask == 0 {
                            player_indices.push(i as u16);
                        }
                    }

                    indices[player] = player_indices;
                }

                valid_indices_river.push(indices);
            }
        }

        if river_exists {
            let mut indices = [Vec::new(), Vec::new()];
            for player in 0..2 {
                let hands = &private_cards[player];
                let mut player_indices = Vec::with_capacity(hands.len());

                for (i, &(c1, c2)) in hands.iter().enumerate() {
                    let mask: u64 = (1 << c1) | (1 << c2);
                    if mask & flop_mask == 0 {
                        player_indices.push(i as u16);
                    }
                }

                indices[player] = player_indices;
            }
            valid_indices_river.push(indices);
        }

        (valid_indices_flop, valid_indices_turn, valid_indices_river)
    }

    /// Computes hand strength for all possible turn/river combinations.
    pub(super) fn hand_strength(
        &self,
        private_cards: &[(Card, Card); 2],
    ) -> Vec<[Vec<StrengthItem>; 2]> {
        let turn_exists = self.turn != NOT_DEALT;
        let river_exists = self.river != NOT_DEALT;
        let mut hand_strength = Vec::new();

        let allowed_turns = if self.allowed_turn_cards.is_empty() {
            (0..52)
                .filter(|&c| !self.flop.contains(&c))
                .collect::<Vec<_>>()
        } else {
            self.allowed_turn_cards.clone()
        };
        let allowed_rivers = if self.allowed_river_cards.is_empty() {
            (0..52)
                .filter(|&c| {
                    !self.flop.contains(&c) && (self.turn == NOT_DEALT || c != self.turn)
                })
                .collect::<Vec<_>>()
        } else {
            self.allowed_river_cards.clone()
        };

        if !turn_exists {
            for &turn in &allowed_turns {
                for &river in &allowed_rivers {
                    if turn != river {
                        let strength = self.compute_hand_strength(turn, river, private_cards);
                        hand_strength.push(strength);
                    }
                }
            }
        } else if !river_exists {
            for &river in &allowed_rivers {
                if river != self.turn {
                    let strength = self.compute_hand_strength(self.turn, river, private_cards);
                    hand_strength.push(strength);
                }
            }
        } else {
            let strength = self.compute_hand_strength(self.turn, self.river, private_cards);
            hand_strength.push(strength);
        }

        hand_strength
    }

    fn compute_hand_strength(
        &self,
        turn: Card,
        river: Card,
        private_cards: &[(Card, Card); 2],
    ) -> [Vec<StrengthItem>; 2] {
        let board = [self.flop[0], self.flop[1], self.flop[2], turn, river];
        let mut strength = [Vec::new(), Vec::new()];

        for player in 0..2 {
            let hands = &private_cards[player];
            let mut player_strength = Vec::with_capacity(hands.len() + 2);
            player_strength.push(StrengthItem {
                strength: 0,
                index: u16::MAX,
            });

            for (i, &(c1, c2)) in hands.iter().enumerate() {
                let hand_mask: u64 = (1 << c1) | (1 << c2);
                let board_mask: u64 = board.iter().map(|&c| 1 << c).sum();
                if hand_mask & board_mask == 0 {
                    let strength_val = hand_strength(&[c1, c2], &board);
                    player_strength.push(StrengthItem {
                        strength: strength_val,
                        index: i as u16,
                    });
                }
            }

            player_strength.push(StrengthItem {
                strength: u16::MAX,
                index: u16::MAX,
            });
            player_strength.sort_unstable_by_key(|item| item.strength);
            strength[player] = player_strength;
        }

        strength
    }

    /// Computes isomorphic chance nodes for turn and river.
    pub(super) fn isomorphism(
        &self,
        private_cards: &[(Card, Card); 2],
    ) -> (
        Vec<u8>,
        Vec<Card>,
        [SwapList; 4],
        Vec<Vec<u8>>,
        [Vec<Card>; 4],
        [[SwapList; 4]; 4],
    ) {
        let turn_exists = self.turn != NOT_DEALT;
        let river_exists = self.river != NOT_DEALT;

        let mut isomorphism_ref_turn = Vec::new();
        let mut isomorphism_card_turn = Vec::new();
        let mut isomorphism_swap_turn = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
        let mut isomorphism_ref_river = Vec::new();
        let mut isomorphism_card_river = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
        let mut isomorphism_swap_river = [
            [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
            [Vec::new(), Vec::new(), Vec::new(), Vec::new()],
        ];

        let flop = &self.flop;
        let flop_suit_mask: u64 = (1 << (flop[0] & 3)) | (1 << (flop[1] & 3)) | (1 << (flop[2] & 3));
        let num_flop_suits = flop_suit_mask.count_ones();

        let allowed_turns = if self.allowed_turn_cards.is_empty() {
            (0..52)
                .filter(|&c| !flop.contains(&c))
                .collect::<Vec<_>>()
        } else {
            self.allowed_turn_cards.clone()
        };
        let allowed_rivers = if self.allowed_river_cards.is_empty() {
            (0..52)
                .filter(|&c| {
                    !flop.contains(&c) && (self.turn == NOT_DEALT || c != self.turn)
                })
                .collect::<Vec<_>>()
        } else {
            self.allowed_river_cards.clone()
        };

        if !turn_exists && num_flop_suits <= 2 {
            let mut ref_cards = Vec::new();
            let mut suit_map = [0; 4];

            for &turn in &allowed_turns {
                let suit = turn & 3;
                let num_suits = (flop_suit_mask | (1 << suit)).count_ones();
                if num_suits <= 2 {
                    let pos = ref_cards.iter().position(|&c| {
                        let ref_suit = c & 3;
                        self.range[0].is_suit_isomorphic(suit, ref_suit)
                            && self.range[1].is_suit_isomorphic(suit, ref_suit)
                    });

                    if let Some(pos) = pos {
                        isomorphism_ref_turn.push(pos as u8);
                        isomorphism_card_turn.push(turn);
                        suit_map[suit as usize] = ref_cards[pos] & 3;

                        for player in 0..2 {
                            let hands = &private_cards[player];
                            let mut swap_list = Vec::new();
                            for (i, &(c1, c2)) in hands.iter().enumerate() {
                                let c1_new = (c1 & !3) | suit_map[c1 & 3 as Card];
                                let c2_new = (c2 & !3) | suit_map[c2 & 3 as Card];
                                if c1 != c1_new || c2 != c2_new {
                                    let pos = hands
                                        .iter()
                                        .position(|&c| c == (c1_new, c2_new))
                                        .unwrap();
                                    swap_list.push((i as u16, pos as u16));
                                }
                            }
                            isomorphism_swap_turn[suit as usize][player] = swap_list;
                        }
                    } else {
                        ref_cards.push(turn);
                        isomorphism_ref_turn.push((ref_cards.len() - 1) as u8);
                        suit_map[suit as usize] = suit;
                    }
                } else {
                    isomorphism_ref_turn.push(isomorphism_ref_turn.len() as u8);
                }
            }
        }

        if turn_exists && !river_exists {
            let turn_suit = self.turn & 3;
            let num_suits = (flop_suit_mask | (1 << turn_suit)).count_ones();

            if num_suits <= 2 {
                let mut ref_cards = Vec::new();
                let mut suit_map = [0; 4];

                for &river in &allowed_rivers {
                    let suit = river & 3;
                    let num_suits =
                        (flop_suit_mask | (1 << turn_suit) | (1 << suit)).count_ones();
                    if num_suits <= 2 {
                        let pos = ref_cards.iter().position(|&c| {
                            let ref_suit = c & 3;
                            self.range[0].is_suit_isomorphic(suit, ref_suit)
                                && self.range[1].is_suit_isomorphic(suit, ref_suit)
                        });

                        if let Some(pos) = pos {
                            isomorphism_card_river[turn_suit as usize].push(river);
                            if isomorphism_ref_river.is_empty() {
                                isomorphism_ref_river.push(Vec::new());
                            }
                            isomorphism_ref_river
                                .last_mut()
                                .unwrap()
                                .push(pos as u8);

                            suit_map[suit as usize] = ref_cards[pos] & 3;

                            for player in 0..2 {
                                let hands = &private_cards[player];
                                let mut swap_list = Vec::new();
                                for (i, &(c1, c2)) in hands.iter().enumerate() {
                                    let c1_new = (c1 & !3) | suit_map[c1 & 3 as Card];
                                    let c2_new = (c2 & !3) | suit_map[c2 & 3 as Card];
                                    if c1 != c1_new || c2 != c2_new {
                                        let pos = hands
                                            .iter()
                                            .position(|&c| c == (c1_new, c2_new))
                                            .unwrap();
                                        swap_list.push((i as u16, pos as u16));
                                    }
                                }
                                isomorphism_swap_river[turn_suit as usize][suit as usize][player] =
                                    swap_list;
                            }
                        } else {
                            ref_cards.push(river);
                            if isomorphism_ref_river.is_empty() {
                                isomorphism_ref_river.push(Vec::new());
                            }
                            isomorphism_ref_river
                                .last_mut()
                                .unwrap()
                                .push((ref_cards.len() - 1) as u8);
                            suit_map[suit as usize] = suit;
                        }
                    } else if isomorphism_ref_river.is_empty() {
                        isomorphism_ref_river.push(Vec::new());
                    }
                }
            } else if isomorphism_ref_river.is_empty() {
                isomorphism_ref_river.push(Vec::new());
            }
        }

        (
            isomorphism_ref_turn,
            isomorphism_card_turn,
            isomorphism_swap_turn,
            isomorphism_ref_river,
            isomorphism_card_river,
            isomorphism_swap_river,
        )
    }
}

/// Computes the index of card pair in the `Range::data` field.
///
/// Undefined behavior if:
///   - `card1` or `card2` is not less than `52`
///   - `card1` is equal to `card2`
#[inline]
pub fn card_pair_to_index(card1: Card, card2: Card) -> usize {
    let min_card = Card::min(card1, card2);
    let max_card = Card::max(card1, card2);
    (max_card * (max_card - 1)) as usize / 2 + min_card as usize
}

/// Attempts to convert a rank character to a rank index.
///
/// `'A'` => `12`, `'K'` => `11`, ..., `'2'` => `0`.
#[inline]
fn char_to_rank(c: char) -> Result<u8, String> {
    match c {
        'A' | 'a' => Ok(12),
        'K' | 'k' => Ok(11),
        'Q' | 'q' => Ok(10),
        'J' | 'j' => Ok(9),
        'T' | 't' => Ok(8),
        '2'..='9' => Ok(c as u8 - b'2'),
        _ => Err(format!("Expected rank character: {c}")),
    }
}

/// Attempts to convert a suit character to a suit index.
///
/// `'c'` => `0`, `'d'` => `1`, `'h'` => `2`, `'s'` => `3`.
#[inline]
fn char_to_suit(c: char) -> Result<u8, String> {
    match c {
        'c' => Ok(0),
        'd' => Ok(1),
        'h' => Ok(2),
        's' => Ok(3),
        _ => Err(format!("Expected suit character: {c}")),
    }
}

/// Attempts to convert a rank index to a rank character.
///
/// `12` => `'A'`, `11` => `'K'`, ..., `0` => `'2'`.
#[inline]
fn rank_to_char(rank: u8) -> Result<char, String> {
    match rank {
        12 => Ok('A'),
        11 => Ok('K'),
        10 => Ok('Q'),
        9 => Ok('J'),
        8 => Ok('T'),
        0..=7 => Ok((rank + b'2') as char),
        _ => Err(format!("Invalid input: {rank}")),
    }
}

/// Attempts to convert a suit index to a suit character.
///
/// `0` => `'c'`, `1` => `'d'`, `2` => `'h'`, `3` => `'s'`.
#[inline]
fn suit_to_char(suit: u8) -> Result<char, String> {
    match suit {
        0 => Ok('c'),
        1 => Ok('d'),
        2 => Ok('h'),
        3 => Ok('s'),
        _ => Err(format!("Invalid input: {suit}")),
    }
}

/// Attempts to convert a card into a string.
///
/// # Examples
/// ```
/// use postflop_solver::card_to_string;
///
/// assert_eq!(card_to_string(0), Ok("2c".to_string()));
/// assert_eq!(card_to_string(5), Ok("3d".to_string()));
/// assert_eq!(card_to_string(10), Ok("4h".to_string()));
/// assert_eq!(card_to_string(51), Ok("As".to_string()));
/// assert!(card_to_string(52).is_err());
/// ```
#[inline]
pub fn card_to_string(card: Card) -> Result<String, String> {
    check_card(card)?;
    let rank = card >> 2;
    let suit = card & 3;
    Ok(format!("{}{}", rank_to_char(rank)?, suit_to_char(suit)?))
}

/// Attempts to convert hole cards into a string.
///
/// See [`Card`] for encoding of cards.
/// The card order in the input does not matter, but the output string is sorted in descending order
/// of card IDs.
///
/// # Examples
/// ```
/// use postflop_solver::hole_to_string;
///
/// assert_eq!(hole_to_string((0, 5)), Ok("3d2c".to_string()));
/// assert_eq!(hole_to_string((10, 51)), Ok("As4h".to_string()));
/// assert!(hole_to_string((52, 53)).is_err());
/// ```
#[inline]
pub fn hole_to_string(hole: (Card, Card)) -> Result<String, String> {
    let max_card = Card::max(hole.0, hole.1);
    let min_card = Card::min(hole.0, hole.1);
    Ok(format!(
        "{}{}",
        card_to_string(max_card)?,
        card_to_string(min_card)?
    ))
}

/// Attempts to convert a list of hole cards into a list of strings.
///
/// See [`Card`] for encoding of cards.
/// The card order of each pair in the input does not matter, but the output string of each pair is
/// sorted in descending order of card IDs.
///
/// # Examples
/// ```
/// use postflop_solver::holes_to_strings;
///
/// assert_eq!(
///     holes_to_strings(&[(0, 5), (10, 51)]),
///     Ok(vec!["3d2c".to_string(), "As4h".to_string()])
/// );
/// assert!(holes_to_strings(&[(52, 53)]).is_err());
/// ```
#[inline]
pub fn holes_to_strings(holes: &[(Card, Card)]) -> Result<Vec<String>, String> {
    holes.iter().map(|&hole| hole_to_string(hole)).collect()
}

/// Attempts to read the next card from a char iterator.
///
/// # Examples
/// ```
/// use postflop_solver::card_from_chars;
///
/// let mut chars = "2c3d4hAs".chars();
/// assert_eq!(card_from_chars(&mut chars), Ok(0));
/// assert_eq!(card_from_chars(&mut chars), Ok(5));
/// assert_eq!(card_from_chars(&mut chars), Ok(10));
/// assert_eq!(card_from_chars(&mut chars), Ok(51));
/// assert!(card_from_chars(&mut chars).is_err());
/// ```
#[inline]
pub fn card_from_chars<T: Iterator<Item = char>>(chars: &mut T) -> Result<Card, String> {
    let rank_char = chars.next().ok_or_else(|| "Unexpected end".to_string())?;
    let suit_char = chars.next().ok_or_else(|| "Unexpected end".to_string())?;

    let rank = char_to_rank(rank_char)?;
    let suit = char_to_suit(suit_char)?;

    Ok((rank << 2) | suit)
}

/// Attempts to convert a string into a card.
///
/// # Examples
/// ```
/// use postflop_solver::card_from_str;
///
/// assert_eq!(card_from_str("2c"), Ok(0));
/// assert_eq!(card_from_str("3d"), Ok(5));
/// assert_eq!(card_from_str("4h"), Ok(10));
/// assert_eq!(card_from_str("As"), Ok(51));
/// ```
#[inline]
pub fn card_from_str(s: &str) -> Result<Card, String> {
    let mut chars = s.chars();
    let result = card_from_chars(&mut chars)?;

    if chars.next().is_some() {
        return Err("Expected exactly two characters".to_string());
    }

    Ok(result)
}

/// Attempts to convert an optionally space-separated string into a sorted flop array.
///
/// # Examples
/// ```
/// use postflop_solver::flop_from_str;
///
/// assert_eq!(flop_from_str("2c3d4h"), Ok([0, 5, 10]));
/// assert_eq!(flop_from_str("As Ah Ks"), Ok([47, 50, 51]));
/// assert!(flop_from_str("2c3d4h5s").is_err());
/// ```
#[inline]
pub fn flop_from_str(s: &str) -> Result<[Card; 3], String> {
    let mut result = [0; 3];
    let mut chars = s.chars();

    result[0] = card_from_chars(&mut chars)?;
    result[1] = card_from_chars(&mut chars.by_ref().skip_while(|c| c.is_whitespace()))?;
    result[2] = card_from_chars(&mut chars.by_ref().skip_while(|c| c.is_whitespace()))?;

    if chars.next().is_some() {
        return Err("Expected exactly three cards".to_string());
    }

    result.sort_unstable();

    if result[0] == result[1] || result[1] == result[2] {
        return Err("Cards must be unique".to_string());
    }

    Ok(result)
}

/// Computes the index of card pair in the `Range::data` field.
///
/// See [`Card`] for encoding of cards.
///
/// Undefined behavior if:
///   - `card1` or `card2` is not less than `52`
///   - `card1` is equal to `card2`
#[inline]
pub fn index_to_card_pair(index: usize) -> (Card, Card) {
    let max_card = (((1 + 8 * index as u64) as f64).sqrt() as u64 + 1) / 2;
    let min_card = index as u64 - max_card * (max_card - 1) / 2;
    (min_card as Card, max_card as Card)
}

#[inline]
fn check_card(card: Card) -> Result<(), String> {
    if card < 52 {
        Ok(())
    } else {
        Err(format!("Invalid card: {card}"))
    }
}