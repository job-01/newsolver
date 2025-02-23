use super::*;
use crate::interface::*;
use crate::sliceop::*;
use std::mem::MaybeUninit;

#[cfg(feature = "rayon")]
use rayon::prelude::*;

impl PostFlopGame {
    pub(super) fn evaluate_internal(
        &self,
        result: &mut [MaybeUninit<f32>],
        node: &PostFlopNode,
        player: usize,
        cfreach: &[f32],
    ) {
        let opponent = player ^ 1;
        let pot_size = self.tree_config.starting_pot + node.amount * 2;
        let amount_to_call = (node.amount - self.total_bet_amount[player]).max(0);
        let rake_factor = if self.is_raked() {
            let rake = (pot_size as f64 * self.tree_config.rake_rate).min(self.tree_config.rake_cap);
            1.0 - rake / pot_size as f64
        } else {
            1.0
        };

        if amount_to_call > 0 {
            let sign = if player == PLAYER_OOP { 1 } else { -1 };
            #[cfg(not(feature = "rayon"))]
            for (r, &w) in result.iter_mut().zip(cfreach) {
                if w > 0.0 {
                    r.write(-sign as f32 * self.total_bet_amount[player] as f32);
                } else {
                    r.write(0.0);
                }
            }
            #[cfg(feature = "rayon")]
            result
                .par_iter_mut()
                .zip(cfreach.par_iter())
                .for_each(|(r, &w)| {
                    if w > 0.0 {
                        r.write(-sign as f32 * self.total_bet_amount[player] as f32);
                    } else {
                        r.write(0.0);
                    }
                });
        } else {
            self.evaluate_terminal_showdown(result, node, player, cfreach, rake_factor);
        }
    }

    pub(super) fn evaluate_terminal_showdown(
        &self,
        result: &mut [MaybeUninit<f32>],
        node: &PostFlopNode,
        player: usize,
        cfreach: &[f32],
        rake_factor: f32,
    ) {
        let opponent = player ^ 1;
        let strength = &self.hand_strength;
        let hands = &self.private_cards;
        let same_hand_index = &self.same_hand_index[player];
        let valid_indices = if node.river != NOT_DEALT {
            &self.valid_indices_river[0]
        } else if node.turn != NOT_DEALT {
            &self.valid_indices_river
        } else {
            &self.valid_indices_turn
        };

        let pot_oop = self.total_bet_amount[PLAYER_OOP] as f32;
        let pot_ip = self.total_bet_amount[PLAYER_IP] as f32;

        let num_cards = valid_indices.len();
        let num_hands = hands[player].len();

        let mut accum_ev = vec![0.0; num_hands];
        let mut accum_weight = vec![0.0; num_hands];

        #[cfg(not(feature = "rayon"))]
        for card_idx in 0..num_cards {
            let indices_player = &valid_indices[card_idx][player];
            let indices_opponent = &valid_indices[card_idx][opponent];
            let strength_player = &strength[card_idx][player];
            let strength_opponent = &strength[card_idx][opponent];
            let mut j = 0;

            for &i in indices_player {
                let mut ev = 0.0;
                let mut weight = 0.0;
                let strength_i = strength_player[i as usize + 1].strength;
                let same_hand_idx = same_hand_index[i as usize];

                while j < indices_opponent.len() {
                    let k = indices_opponent[j] as usize;
                    let strength_k = strength_opponent[k + 1].strength;

                    if strength_k >= strength_i {
                        break;
                    }

                    let w = cfreach[k];
                    if w > 0.0 {
                        ev -= w;
                        weight += w;
                    }

                    j += 1;
                }

                if same_hand_idx != u16::MAX && cfreach[same_hand_idx as usize] > 0.0 {
                    ev += 0.5 * cfreach[same_hand_idx as usize];
                }

                while j < indices_opponent.len() {
                    let k = indices_opponent[j] as usize;
                    let strength_k = strength_opponent[k + 1].strength;

                    if strength_k > strength_i {
                        let w = cfreach[k];
                        if w > 0.0 {
                            ev += w;
                            weight += w;
                        }
                    }

                    j += 1;
                }

                accum_ev[i as usize] += ev;
                accum_weight[i as usize] += weight;
            }
        }

        #[cfg(feature = "rayon")]
        {
            let (accum_ev_result, accum_weight_result): (Vec<_>, Vec<_>) = (0..num_cards)
                .into_par_iter()
                .map(|card_idx| {
                    let indices_player = &valid_indices[card_idx][player];
                    let indices_opponent = &valid_indices[card_idx][opponent];
                    let strength_player = &strength[card_idx][player];
                    let strength_opponent = &strength[card_idx][opponent];
                    let mut j = 0;

                    let mut accum_ev = vec![0.0; num_hands];
                    let mut accum_weight = vec![0.0; num_hands];

                    for &i in indices_player {
                        let mut ev = 0.0;
                        let mut weight = 0.0;
                        let strength_i = strength_player[i as usize + 1].strength;
                        let same_hand_idx = same_hand_index[i as usize];

                        while j < indices_opponent.len() {
                            let k = indices_opponent[j] as usize;
                            let strength_k = strength_opponent[k + 1].strength;

                            if strength_k >= strength_i {
                                break;
                            }

                            let w = cfreach[k];
                            if w > 0.0 {
                                ev -= w;
                                weight += w;
                            }

                            j += 1;
                        }

                        if same_hand_idx != u16::MAX && cfreach[same_hand_idx as usize] > 0.0 {
                            ev += 0.5 * cfreach[same_hand_idx as usize];
                        }

                        while j < indices_opponent.len() {
                            let k = indices_opponent[j] as usize;
                            let strength_k = strength_opponent[k + 1].strength;

                            if strength_k > strength_i {
                                let w = cfreach[k];
                                if w > 0.0 {
                                    ev += w;
                                    weight += w;
                                }
                            }

                            j += 1;
                        }

                        accum_ev[i as usize] = ev;
                        accum_weight[i as usize] = weight;
                    }

                    (accum_ev, accum_weight)
                })
                .reduce(
                    || (vec![0.0; num_hands], vec![0.0; num_hands]),
                    |(ev1, w1), (ev2, w2)| {
                        let mut ev = ev1;
                        let mut w = w1;
                        for i in 0..num_hands {
                            ev[i] += ev2[i];
                            w[i] += w2[i];
                        }
                        (ev, w)
                    },
                );

            for i in 0..num_hands {
                accum_ev[i] = accum_ev_result[i];
                accum_weight[i] = accum_weight_result[i];
            }
        }

        let total_weight: f32 = accum_weight.iter().sum();

        #[cfg(not(feature = "rayon"))]
        for (i, (r, &w)) in result.iter_mut().zip(cfreach).enumerate() {
            if w > 0.0 {
                let ev = if total_weight > 0.0 {
                    accum_ev[i] * (pot_oop + pot_ip) / total_weight
                } else {
                    0.0
                };
                if player == PLAYER_OOP {
                    r.write(ev * rake_factor);
                } else {
                    r.write((pot_oop + pot_ip - ev) * rake_factor - pot_ip);
                }
            } else {
                r.write(0.0);
            }
        }

        #[cfg(feature = "rayon")]
        result
            .par_iter_mut()
            .zip(cfreach.par_iter())
            .enumerate()
            .for_each(|(i, (r, &w))| {
                if w > 0.0 {
                    let ev = if total_weight > 0.0 {
                        accum_ev[i] * (pot_oop + pot_ip) / total_weight
                    } else {
                        0.0
                    };
                    if player == PLAYER_OOP {
                        r.write(ev * rake_factor);
                    } else {
                        r.write((pot_oop + pot_ip - ev) * rake_factor - pot_ip);
                    }
                } else {
                    r.write(0.0);
                }
            });
    }

    pub(super) fn evaluate_internal_bunching(
        &self,
        result: &mut [MaybeUninit<f32>],
        node: &PostFlopNode,
        player: usize,
        cfreach: &[f32],
    ) {
        let opponent = player ^ 1;
        let pot_size = self.tree_config.starting_pot + node.amount * 2;
        let amount_to_call = (node.amount - self.total_bet_amount[player]).max(0);
        let rake_factor = if self.is_raked() {
            let rake = (pot_size as f64 * self.tree_config.rake_rate).min(self.tree_config.rake_cap);
            1.0 - rake / pot_size as f64
        } else {
            1.0
        };

        let num_hands_player = self.num_private_hands(player);
        let num_hands_opponent = self.num_private_hands(opponent);
        let num_cards = self.valid_indices_river.len() + self.valid_indices_turn.len();

        let mut ev = vec![0.0; num_hands_player];
        let mut total_weight = 0.0;

        let mut idx = 0;
        if amount_to_call > 0 {
            let sign = if player == PLAYER_OOP { 1 } else { -1 };
            for i in 0..num_hands_player {
                if cfreach[i] > 0.0 {
                    ev[i] = -sign as f32 * self.total_bet_amount[player] as f32;
                }
            }
        } else {
            for card_idx in 0..num_cards {
                let num_combinations = self.bunching_arena[idx];
                idx += 1;

                for i in 0..num_hands_player {
                    for j in 0..num_hands_opponent {
                        let w = self.bunching_arena[idx] * cfreach[j];
                        if w > 0.0 {
                            let strength_player = self.bunching_strength[card_idx][player][i];
                            let strength_opponent = self.bunching_strength[card_idx][opponent][j];
                            let ev_contribution = if strength_player > strength_opponent {
                                w
                            } else if strength_player < strength_opponent {
                                -w
                            } else {
                                0.0
                            };
                            ev[i] += ev_contribution;
                            total_weight += w;
                        }
                        idx += 1;
                    }
                }
            }
        }

        let pot_oop = self.total_bet_amount[PLAYER_OOP] as f32;
        let pot_ip = self.total_bet_amount[PLAYER_IP] as f32;

        for (i, (r, &w)) in result.iter_mut().zip(cfreach).enumerate() {
            if w > 0.0 {
                let ev_i = if amount_to_call == 0 && total_weight > 0.0 {
                    ev[i] * (pot_oop + pot_ip) / total_weight
                } else {
                    ev[i]
                };
                if player == PLAYER_OOP {
                    r.write(ev_i * rake_factor);
                } else {
                    r.write((pot_oop + pot_ip - ev_i) * rake_factor - pot_ip);
                }
            } else {
                r.write(0.0);
            }
        }
    }
}