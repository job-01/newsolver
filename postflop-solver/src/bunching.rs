use super::*;
use crate::range::*;
use crate::utility::*;

#[cfg(feature = "rayon")]
use rayon::prelude::*;

/// A struct representing bunching effect data.
///
/// Bunching effect refers to the effect that causes the number of combinations of each hand to
/// vary depending on the board cards and the range of players.
///
/// This struct provides precomputed data for such bunching effect.
///
/// **Caveat**: Currently, this struct only supports the flop state as the initial state.
#[derive(Default)]
pub struct BunchingData {
    flop: [Card; 3],
    ranges: [Range; 2],
    num_dead_cards: usize,
    num_combinations: f64,
    combinations: Vec<f32>,
    hand_strength: [Vec<u16>; 2],
}

impl BunchingData {
    /// Creates a new [`BunchingData`] instance with specified ranges and flop cards.
    pub fn new(ranges: &[Range; 2], flop: [Card; 3], dead_cards: Option<&[Card]>) -> Result<Self, String> {
        let mut dead_cards_set = BTreeSet::new();

        for &card in &flop {
            if card >= 52 {
                return Err(format!("Invalid flop card: {}", card));
            }
            if !dead_cards_set.insert(card) {
                return Err("Duplicate flop card".to_string());
            }
        }

        if let Some(cards) = dead_cards {
            for &card in cards {
                if card >= 52 {
                    return Err(format!("Invalid dead card: {}", card));
                }
                if !dead_cards_set.insert(card) {
                    return Err("Duplicate dead card".to_string());
                }
            }
        }

        Ok(Self {
            flop,
            ranges: ranges.clone(),
            num_dead_cards: dead_cards_set.len(),
            ..Default::default()
        })
    }

    /// Returns whether this instance is ready to use.
    #[inline]
    pub fn is_ready(&self) -> bool {
        !self.combinations.is_empty()
    }

    /// Returns the flop cards.
    #[inline]
    pub fn flop(&self) -> [Card; 3] {
        self.flop
    }

    /// Returns the number of dead cards.
    #[inline]
    pub fn num_dead_cards(&self) -> usize {
        self.num_dead_cards
    }

    /// Returns the number of combinations.
    #[inline]
    pub fn num_combinations(&self) -> f64 {
        self.num_combinations
    }

    /// Returns the number of combinations given turn/river indices and hand indices.
    #[inline]
    pub fn combinations(&self, turn_idx: usize, river_idx: usize, oop_idx: u16, ip_idx: u16) -> f32 {
        let num_river = self.num_river();
        let num_hands_oop = self.ranges[0].num_hands();
        let num_hands_ip = self.ranges[1].num_hands();

        let idx = (turn_idx * num_river + river_idx) * (num_hands_oop * num_hands_ip + 1)
            + oop_idx as usize * num_hands_ip
            + ip_idx as usize
            + 1;
        self.combinations[idx]
    }

    /// Returns the number of combinations given turn/river indices.
    #[inline]
    pub fn num_combinations_turn_river(&self, turn_idx: usize, river_idx: usize) -> f32 {
        let num_river = self.num_river();
        self.combinations[turn_idx * num_river + river_idx]
    }

    /// Returns the hand strength given turn/river indices and player.
    #[inline]
    pub fn hand_strength(&self, player: usize, turn_idx: usize, river_idx: usize) -> &[u16] {
        let num_river = self.num_river();
        let num_hands = self.ranges[player].num_hands();
        let idx = (turn_idx * num_river + river_idx) * num_hands;
        &self.hand_strength[player][idx..idx + num_hands]
    }

    /// Returns the number of remaining turn cards.
    #[inline]
    pub fn num_turn(&self) -> usize {
        // Note: This assumes access to card_config from PostFlopGame when used.
        // For standalone BunchingData, we assume full deck unless restricted externally.
        52 - self.num_dead_cards // Default to full deck minus dead cards
    }

    /// Returns the number of remaining river cards.
    #[inline]
    pub fn num_river(&self) -> usize {
        // Similarly, assumes full deck unless restricted externally
        51 - self.num_dead_cards // One less than turn due to turn card
    }

    /// Processes the bunching effect data without parallelism.
    pub fn process(&mut self) {
        self.process_internal(false);
    }

    /// Processes the bunching effect data with parallelism (requires `rayon` feature).
    pub fn process_parallel(&mut self) {
        self.process_internal(true);
    }

    fn process_internal(&mut self, enable_parallel: bool) {
        let dead_cards_mask: u64 = self.flop.iter().map(|&c| 1 << c).sum();
        
        // Use allowed cards if provided by card_config (assumed passed from PostFlopGame),
        // otherwise compute all possible turn/river cards
        let turn_cards = if let Some(card_config) = self.card_config { // Hypothetical access
            if !card_config.allowed_turn_cards.is_empty() {
                card_config.allowed_turn_cards.clone()
            } else {
                (0..52)
                    .filter(|&c| (1 << c) & dead_cards_mask == 0)
                    .collect::<Vec<_>>()
            }
        } else {
            (0..52)
                .filter(|&c| (1 << c) & dead_cards_mask == 0)
                .collect::<Vec<_>>()
        };

        let num_turn = turn_cards.len();
        let num_hands_oop = self.ranges[0].num_hands();
        let num_hands_ip = self.ranges[1].num_hands();

        let (oop_hands, oop_weights) = self.ranges[0].get_hands_weights(dead_cards_mask);
        let (ip_hands, ip_weights) = self.ranges[1].get_hands_weights(dead_cards_mask);

        let mut combinations = Vec::new();
        let mut hand_strength = [
            Vec::with_capacity(num_turn * num_hands_oop),
            Vec::with_capacity(num_turn * num_hands_ip),
        ];
        let mut num_combinations = 0.0;

        let process_iteration = |turn_idx: usize| {
            let turn = turn_cards[turn_idx];
            let turn_mask = dead_cards_mask | (1 << turn);

            let river_cards = if let Some(card_config) = self.card_config { // Hypothetical access
                if !card_config.allowed_river_cards.is_empty() {
                    card_config.allowed_river_cards.clone()
                } else {
                    (0..52)
                        .filter(|&c| (1 << c) & turn_mask == 0)
                        .collect::<Vec<_>>()
                }
            } else {
                (0..52)
                    .filter(|&c| (1 << c) & turn_mask == 0)
                    .collect::<Vec<_>>()
            };

            let num_river = river_cards.len();
            let mut combinations_local = Vec::with_capacity(num_river * (num_hands_oop * num_hands_ip + 1));
            let mut hand_strength_local = [
                Vec::with_capacity(num_river * num_hands_oop),
                Vec::with_capacity(num_river * num_hands_ip),
            ];
            let mut num_combinations_local = 0.0;

            for river_idx in 0..num_river {
                let river = river_cards[river_idx];
                let board = [self.flop[0], self.flop[1], self.flop[2], turn, river];
                let board_mask = turn_mask | (1 << river);

                let mut strength_oop = Vec::with_capacity(num_hands_oop + 2);
                let mut strength_ip = Vec::with_capacity(num_hands_ip + 2);
                strength_oop.push(0);
                strength_ip.push(0);

                combinations_local.push(0.0);

                for (i, &(c1, c2)) in oop_hands.iter().enumerate() {
                    let mask = (1 << c1) | (1 << c2);
                    if mask & board_mask == 0 {
                        let strength = hand_strength(&[c1, c2], &board);
                        strength_oop.push(strength);
                    }

                    for (j, &(c3, c4)) in ip_hands.iter().enumerate() {
                        let mask = (1 << c3) | (1 << c4);
                        if mask & board_mask == 0 && (mask & ((1 << c1) | (1 << c2))) == 0 {
                            let w = oop_weights[i] * ip_weights[j];
                            combinations_local.push(w);
                            num_combinations_local += w as f64;
                        } else {
                            combinations_local.push(0.0);
                        }
                    }
                }

                for (j, &(c3, c4)) in ip_hands.iter().enumerate() {
                    let mask = (1 << c3) | (1 << c4);
                    if mask & board_mask == 0 {
                        let strength = hand_strength(&[c3, c4], &board);
                        strength_ip.push(strength);
                    }
                }

                strength_oop.push(u16::MAX);
                strength_ip.push(u16::MAX);
                strength_oop.sort_unstable();
                strength_ip.sort_unstable();

                let mut strength_oop_iter = strength_oop.iter().peekable();
                let mut strength_ip_iter = strength_ip.iter().peekable();
                let mut prev_strength_oop = *strength_oop_iter.next().unwrap();
                let mut prev_strength_ip = *strength_ip_iter.next().unwrap();
                let mut accum_oop = 0;
                let mut accum_ip = 0;

                for &(c1, c2) in &oop_hands {
                    let mask = (1 << c1) | (1 << c2);
                    if mask & board_mask == 0 {
                        let strength = hand_strength(&[c1, c2], &board);
                        while let Some(&&s) = strength_oop_iter.peek() {
                            if s <= strength {
                                accum_oop += 1;
                                prev_strength_oop = s;
                                strength_oop_iter.next();
                            } else {
                                break;
                            }
                        }
                        hand_strength_local[0].push(if prev_strength_oop == strength { accum_oop } else { accum_oop + 1 });
                    }
                }

                for &(c3, c4) in &ip_hands {
                    let mask = (1 << c3) | (1 << c4);
                    if mask & board_mask == 0 {
                        let strength = hand_strength(&[c3, c4], &board);
                        while let Some(&&s) = strength_ip_iter.peek() {
                            if s <= strength {
                                accum_ip += 1;
                                prev_strength_ip = s;
                                strength_ip_iter.next();
                            } else {
                                break;
                            }
                        }
                        hand_strength_local[1].push(if prev_strength_ip == strength { accum_ip } else { accum_ip + 1 });
                    }
                }
            }

            (combinations_local, hand_strength_local, num_combinations_local)
        };

        if enable_parallel && num_turn >= 2 {
            #[cfg(feature = "rayon")]
            {
                let results: Vec<_> = (0..num_turn)
                    .into_par_iter()
                    .map(process_iteration)
                    .collect();

                for (combinations_local, hand_strength_local, num_combinations_local) in results {
                    combinations.extend(combinations_local);
                    hand_strength[0].extend(hand_strength_local[0]);
                    hand_strength[1].extend(hand_strength_local[1]);
                    num_combinations += num_combinations_local;
                }
            }
            #[cfg(not(feature = "rayon"))]
            {
                panic!("Parallel processing is not supported without the 'rayon' feature");
            }
        } else {
            for turn_idx in 0..num_turn {
                let (combinations_local, hand_strength_local, num_combinations_local) =
                    process_iteration(turn_idx);
                combinations.extend(combinations_local);
                hand_strength[0].extend(hand_strength_local[0]);
                hand_strength[1].extend(hand_strength_local[1]);
                num_combinations += num_combinations_local;
            }
        }

        self.combinations = combinations;
        self.hand_strength = hand_strength;
        self.num_combinations = num_combinations;
    }
}