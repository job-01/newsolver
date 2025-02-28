use crate::range::*; //unused?
use postflop_solver::*;
use rayon::ThreadPool; //unused?
use serde::Serialize;
use std::sync::Mutex;
use crate::AppState;


// Define a simple state enum (adjust based on your needs)
#[derive(Debug)]
enum BoardState {
    Flop,
    Turn,
    River,
}

impl BoardState {
    pub fn from_card_count(count: usize) -> Self {
        match count {
            3 => BoardState::Flop,
            4 => BoardState::Turn,
            5 => BoardState::River,
            _ => panic!("Invalid card count: {}", count), // Or return a default/error
        }
    }
}

fn parse_board(board: &str) -> Result<(Vec<u8>, Option<u8>, Option<u8>), String> {
    let cards: Vec<&str> = board.split_whitespace().collect();
    if cards.len() < 3 || cards.len() > 5 {
        return Err(format!(
            "Invalid board: '{}'. Expected 3-5 cards (e.g., 'Ah Kh Qh' or 'Ah Kh Qh Kd As')",
            board
        ));
    }

    // Parse flop (always 3 cards)
    let flop: Vec<u8> = cards[0..3]
        .iter()
        .map(|&c| card_from_str(c))
        .collect::<Result<Vec<u8>, String>>()?;

    // Parse turn and river if present
    let turn = if cards.len() >= 4 {
        Some(card_from_str(cards[3])?)
    } else {
        None
    };
    let river = if cards.len() == 5 {
        Some(card_from_str(cards[4])?)
    } else {
        None
    };

    Ok((flop, turn, river))
}

fn card_from_str(card: &str) -> Result<u8, String> {
    let ranks = "23456789TJQKA";
    let suits = "cdhs";
    if card.len() != 2 {
        return Err(format!("Invalid card format: '{}'", card));
    }
    let rank = ranks
        .find(card.chars().nth(0).unwrap())
        .ok_or_else(|| format!("Invalid rank in card: '{}'", card))? as u8;
    let suit = suits
        .find(card.chars().nth(1).unwrap())
        .ok_or_else(|| format!("Invalid suit in card: '{}'", card))? as u8;
    Ok(rank * 4 + suit) // 0-51: 2c=0, As=51
}


#[inline]
fn decode_action(action: &str) -> Action {
    match action {
        "F" => Action::Fold,
        "X" => Action::Check,
        "C" => Action::Call,
        _ => {
            let mut chars = action.chars();
            let first_char = chars.next().unwrap();
            let amount = chars.as_str().parse().unwrap();
            match first_char {
                'B' => Action::Bet(amount),
                'R' => Action::Raise(amount),
                'A' => Action::AllIn(amount),
                _ => unreachable!(),
            }
        }
    }
}

#[inline]
fn action_usize(action: isize) -> usize {
    match action {
        -1 => usize::MAX,
        a => a as usize,
    }
}

#[inline]
fn round(value: f64) -> f64 {
    if value < 1.0 {
        (value * 1000000.0).round() / 1000000.0
    } else if value < 10.0 {
        (value * 100000.0).round() / 100000.0
    } else if value < 100.0 {
        (value * 10000.0).round() / 10000.0
    } else if value < 1000.0 {
        (value * 1000.0).round() / 1000.0
    } else if value < 10000.0 {
        (value * 100.0).round() / 100.0
    } else {
        (value * 10.0).round() / 10.0
    }
}

#[inline]
fn round_iter<'a>(iter: impl Iterator<Item = &'a f32> + 'a) -> impl Iterator<Item = f64> + 'a {
    iter.map(|&x| round(x as f64))
}

#[inline]
pub fn weighted_average(slice: &[f32], weights: &[f32]) -> f64 {
    let mut sum = 0.0;
    let mut weight_sum = 0.0;
    for (&value, &weight) in slice.iter().zip(weights.iter()) {
        sum += value as f64 * weight as f64;
        weight_sum += weight as f64;
    }
    sum / weight_sum
}

#[tauri::command(async)]
pub async fn game_init(
    state: tauri::State<'_, Mutex<AppState>>,
    oop_range: String,
    ip_range: String,
    board: String,
    starting_pot: i32,
    effective_stack: i32,
    rake_rate: f64,
    rake_cap: f64,
    donk_option: bool,
    oop_flop_bet: String,
    oop_flop_raise: String,
    oop_turn_bet: String,
    oop_turn_raise: String,
    oop_turn_donk: String,
    oop_river_bet: String,
    oop_river_raise: String,
    oop_river_donk: String,
    ip_flop_bet: String,
    ip_flop_raise: String,
    ip_turn_bet: String,
    ip_turn_raise: String,
    ip_river_bet: String,
    ip_river_raise: String,
    add_allin_threshold: f64,
    force_allin_threshold: f64,
    merging_threshold: f64,
    added_lines: String,
    removed_lines: String,
) -> Result<(), String> {
    let (turn, river, state) = match parse_board(&board) {
        Ok((flop, turn, river)) => {
            let total_cards = flop.len() + turn.is_some() as usize + river.is_some() as usize;
            (turn, river, BoardState::from_card_count(total_cards))
        }
        Err(e) => return Err(format!("Invalid board: {}", e)),
    };
    

    let ranges = &state.range_manager.lock().unwrap().0;
    let card_config = CardConfig {
        range: ranges[..2].try_into().unwrap(),
        flop: board[..3].try_into().unwrap(),
        turn,
        river,
    };

    let tree_config = TreeConfig {
        initial_state: state,
        starting_pot,
        effective_stack,
        rake_rate,
        rake_cap,
        flop_bet_sizes: [
            BetSizeOptions::try_from((oop_flop_bet.as_str(), oop_flop_raise.as_str())).unwrap(),
            BetSizeOptions::try_from((ip_flop_bet.as_str(), ip_flop_raise.as_str())).unwrap(),
        ],
        turn_bet_sizes: [
            BetSizeOptions::try_from((oop_turn_bet.as_str(), oop_turn_raise.as_str())).unwrap(),
            BetSizeOptions::try_from((ip_turn_bet.as_str(), ip_turn_raise.as_str())).unwrap(),
        ],
        river_bet_sizes: [
            BetSizeOptions::try_from((oop_river_bet.as_str(), oop_river_raise.as_str())).unwrap(),
            BetSizeOptions::try_from((ip_river_bet.as_str(), ip_river_raise.as_str())).unwrap(),
        ],
        turn_donk_sizes: match donk_option {
            false => None,
            true => DonkSizeOptions::try_from(oop_turn_donk.as_str()).ok(),
        },
        river_donk_sizes: match donk_option {
            false => None,
            true => DonkSizeOptions::try_from(oop_river_donk.as_str()).ok(),
        },
        add_allin_threshold,
        force_allin_threshold,
        merging_threshold,
    };

    let mut action_tree = ActionTree::new(tree_config).map_err(|e| e.to_string())?;

    if !added_lines.is_empty() {
        for added_line in added_lines.split(',') {
            let line = added_line
                .split(&['-', '|'][..])
                .map(decode_action)
                .collect::<Vec<_>>();
            if action_tree.add_line(&line).is_err() {
                return Err("Failed to add line (loaded broken tree?)".to_string());
            }
        }
    }

    if !removed_lines.is_empty() {
        for removed_line in removed_lines.split(',') {
            let line = removed_line
                .split(&['-', '|'][..])
                .map(decode_action)
                .collect::<Vec<_>>();
            if action_tree.remove_line(&line).is_err() {
                return Err("Failed to remove line (loaded broken tree?)".to_string());
            }
        }
    }

    let mut game = state.game.lock().unwrap();
    *game = PostFlopGame::with_config(card_config, action_tree).map_err(|e| e.to_string())?;

    // Apply allowed runouts if set
    if let Some(ref allowed) = *state.allowed_runouts.lock().unwrap() {
        let allowed_cards: Vec<u8> = allowed
            .iter()
            .map(|card| {
                let rank = "23456789TJQKA"
                    .find(card.chars().next().unwrap())
                    .unwrap_or(0);
                let suit = "♠♥♦♣"
                    .find(card.chars().nth(1).unwrap())
                    .unwrap_or(0);
                (rank * 4 + suit) as u8
            })
            .collect();
        println!("Converted allowed cards: {:?}", allowed_cards); // Debug

        let board_cards = game.board(); // Adjust based on postflop-solver API
        let mut solver_cards = Vec::new();
        for &card in &allowed_cards {
            if !board_cards.contains(&card) { // Exclude flop cards
                solver_cards.push(card);
            }
        }
        // Placeholder: Modify postflop-solver to add set_possible_runouts
        // game.set_possible_runouts(&solver_cards); // Uncomment when implemented
    }

    Ok(())
}

#[tauri::command]
pub fn game_private_cards(game_state: tauri::State<Mutex<PostFlopGame>>) -> [Vec<u16>; 2] {
    let game = game_state.lock().unwrap();
    let convert = |player: usize| {
        game.private_cards(player)
            .iter()
            .map(|&(c1, c2)| (c1 as u16) | (c2 as u16) << 8)
            .collect()
    };
    [convert(0), convert(1)]
}

#[tauri::command]
pub fn game_memory_usage(game_state: tauri::State<Mutex<PostFlopGame>>) -> (u64, u64) {
    let game = game_state.lock().unwrap();
    game.memory_usage()
}

#[tauri::command]
pub fn game_memory_usage_bunching(game_state: tauri::State<Mutex<PostFlopGame>>) -> u64 {
    let game = game_state.lock().unwrap();
    game.memory_usage_bunching()
}

#[tauri::command(async)]
pub async fn game_allocate_memory(
    game_state: tauri::State<'_, Mutex<PostFlopGame>>,
    enable_compression: bool,
) -> Result<(), String> {  // Change return type to Result<(), String>
    let mut game = game_state.lock().unwrap();
    game.allocate_memory(enable_compression);
    Ok(())  // Return Ok(()) to indicate success
}

#[tauri::command(async)]
pub async fn game_set_bunching(
    bunching_state: tauri::State<'_, Mutex<Option<BunchingData>>>,
    game_state: tauri::State<'_, Mutex<PostFlopGame>>,
) -> Result<(), String> {
    let bunching_data = bunching_state.lock().unwrap();
    let bunching_data = bunching_data.as_ref().unwrap();
    let mut game = game_state.lock().unwrap();
    game.set_bunching_effect(bunching_data).map_err(|e| e.to_string())
}

#[tauri::command(async)]
pub async fn game_solve_step(
    state: tauri::State<'_, Mutex<AppState>>,
    current_iteration: u32,
) -> Result<(), String> {
    let game = state.game.lock().unwrap();
    let pool = state.thread_pool.lock().unwrap();
    pool.install(|| solve_step(&*game, current_iteration));
}

#[tauri::command(async)]
pub async fn game_exploitability(
    state: tauri::State<'_, Mutex<AppState>>,
) -> Result<(), String> {
    let game = state.game.lock().unwrap();
    let pool = state.thread_pool.lock().unwrap();
    pool.install(|| compute_exploitability(&*game))
}

#[tauri::command(async)]
pub async fn game_finalize(
    state: tauri::State<'_, Mutex<AppState>>,
) -> Result<(), String> {
    let pool = state.thread_pool.lock().unwrap();
    pool.install(|| finalize(&mut *state.game.lock().unwrap()));
}

#[tauri::command]
pub fn game_apply_history(game_state: tauri::State<Mutex<PostFlopGame>>, history: Vec<usize>) {
    let mut game = game_state.lock().unwrap();
    game.apply_history(&history);
}

#[tauri::command]
pub fn game_total_bet_amount(
    game_state: tauri::State<Mutex<PostFlopGame>>,
    append: Vec<isize>,
) -> [i32; 2] {
    let mut game = game_state.lock().unwrap();
    if append.is_empty() {
        return game.total_bet_amount();
    }
    let history = game.history().to_vec();
    for &action in &append {
        game.play(action_usize(action));
    }
    let ret = game.total_bet_amount();
    game.apply_history(&history);
    ret
}

fn actions(game: &PostFlopGame) -> Vec<String> {
    if game.is_terminal_node() {
        vec!["terminal".to_string()]
    } else if game.is_chance_node() {
        vec!["chance".to_string()]
    } else {
        game.available_actions()
            .iter()
            .map(|&x| match x {
                Action::Fold => "Fold:0".to_string(),
                Action::Check => "Check:0".to_string(),
                Action::Call => "Call:0".to_string(),
                Action::Bet(amount) => format!("Bet:{amount}"),
                Action::Raise(amount) => format!("Raise:{amount}"),
                Action::AllIn(amount) => format!("Allin:{amount}"),
                _ => unreachable!(),
            })
            .collect()
    }
}

#[tauri::command]
pub fn game_actions_after(
    game_state: tauri::State<Mutex<PostFlopGame>>,
    append: Vec<isize>,
) -> Vec<String> {
    let mut game = game_state.lock().unwrap();
    if append.is_empty() {
        return actions(&game);
    }
    let history = game.history().to_vec();
    for &action in &append {
        game.play(action_usize(action));
    }
    let ret = actions(&game);
    game.apply_history(&history);
    ret
}

#[tauri::command]
pub fn game_possible_cards(game_state: tauri::State<Mutex<PostFlopGame>>) -> u64 {
    let game = game_state.lock().unwrap();
    game.possible_cards()
}

fn current_player(game: &PostFlopGame) -> String {
    if game.is_terminal_node() {
        "terminal".to_string()
    } else if game.is_chance_node() {
        "chance".to_string()
    } else if game.current_player() == 0 {
        "oop".to_string()
    } else {
        "ip".to_string()
    }
}

pub fn num_actions(game: &PostFlopGame) -> usize {
    match game.is_chance_node() {
        true => 0,
        false => game.available_actions().len(),
    }
}

#[derive(Serialize)]
pub struct GameResultsResponse {
    current_player: String,
    num_actions: usize,
    is_empty: i32,
    eqr_base: [i32; 2],
    weights: [Vec<f64>; 2],
    normalizer: [Vec<f64>; 2],
    equity: [Vec<f64>; 2],
    ev: [Vec<f64>; 2],
    eqr: [Vec<f64>; 2],
    strategy: Vec<f64>,
    action_ev: Vec<f64>,
}

#[tauri::command]
pub fn game_get_results(game_state: tauri::State<Mutex<PostFlopGame>>) -> GameResultsResponse {
    let mut game = game_state.lock().unwrap();

    let total_bet_amount = game.total_bet_amount();
    let pot_base = game.tree_config().starting_pot + total_bet_amount.iter().min().unwrap();
    let eqr_base = [
        pot_base + total_bet_amount[0],
        pot_base + total_bet_amount[1],
    ];

    let trunc = |&w: &f32| if w < 0.0005 { 0.0 } else { round(w as f64) };
    let weights = [
        game.weights(0).iter().map(trunc).collect::<Vec<_>>(),
        game.weights(1).iter().map(trunc).collect::<Vec<_>>(),
    ];

    let is_empty = |player: usize| weights[player].iter().all(|&w| w == 0.0);
    let is_empty_flag = is_empty(0) as i32 + 2 * is_empty(1) as i32;

    let mut normalizer = [Vec::new(), Vec::new()];
    let mut equity = [Vec::new(), Vec::new()];
    let mut ev = [Vec::new(), Vec::new()];
    let mut eqr = [Vec::new(), Vec::new()];

    if is_empty_flag > 0 {
        normalizer[0].extend(weights[0].iter());
        normalizer[1].extend(weights[1].iter());
    } else {
        game.cache_normalized_weights();

        normalizer[0].extend(round_iter(game.normalized_weights(0).iter()));
        normalizer[1].extend(round_iter(game.normalized_weights(1).iter()));

        let equity_raw = [game.equity(0), game.equity(1)];
        let ev_raw = [game.expected_values(0), game.expected_values(1)];

        equity[0].extend(round_iter(equity_raw[0].iter()));
        equity[1].extend(round_iter(equity_raw[1].iter()));
        ev[0].extend(round_iter(ev_raw[0].iter()));
        ev[1].extend(round_iter(ev_raw[1].iter()));

        for player in 0..2 {
            let pot = eqr_base[player] as f64;
            for (&eq, &ev) in equity_raw[player].iter().zip(ev_raw[player].iter()) {
                let (eq, ev) = (eq as f64, ev as f64);
                if eq < 5e-7 {
                    eqr[player].push(ev / 0.0);
                } else {
                    eqr[player].push(round(ev / (pot * eq)));
                }
            }
        }
    }

    let mut strategy = Vec::new();
    let mut action_ev = Vec::new();

    if !game.is_terminal_node() && !game.is_chance_node() {
        strategy.extend(round_iter(game.strategy().iter()));
        if is_empty_flag == 0 {
            action_ev.extend(round_iter(
                game.expected_values_detail(game.current_player()).iter(),
            ));
        }
    }

    GameResultsResponse {
        current_player: current_player(&game),
        num_actions: num_actions(&game),
        is_empty: is_empty_flag,
        eqr_base,
        weights,
        normalizer,
        equity,
        ev,
        eqr,
        strategy,
        action_ev,
    }
}

#[derive(Serialize)]
pub struct GameChanceReportsResponse {
    status: Vec<i32>,
    combos: [Vec<f64>; 2],
    equity: [Vec<f64>; 2],
    ev: [Vec<f64>; 2],
    eqr: [Vec<f64>; 2],
    strategy: Vec<f64>,
}

#[tauri::command]
pub fn game_get_chance_reports(
    game_state: tauri::State<Mutex<PostFlopGame>>,
    append: Vec<isize>,
    num_actions: usize,
) -> GameChanceReportsResponse {
    let mut game = game_state.lock().unwrap();
    let history = game.history().to_vec();

    let mut status = vec![0; 52]; // 0: not possible, 1: empty, 2: not empty
    let mut combos = [vec![0.0; 52], vec![0.0; 52]];
    let mut equity = [vec![0.0; 52], vec![0.0; 52]];
    let mut ev = [vec![0.0; 52], vec![0.0; 52]];
    let mut eqr = [vec![0.0; 52], vec![0.0; 52]];
    let mut strategy = vec![0.0; num_actions * 52];

    let possible_cards = game.possible_cards();
    for chance in 0..52 {
        if possible_cards & (1 << chance) == 0 {
            continue;
        }

        game.play(chance);
        for &action in &append[1..] {
            game.play(action_usize(action));
        }

        let trunc = |&w: &f32| if w < 0.0005 { 0.0 } else { w };
        let weights = [
            game.weights(0).iter().map(trunc).collect::<Vec<_>>(),
            game.weights(1).iter().map(trunc).collect::<Vec<_>>(),
        ];

        combos[0][chance] = round(weights[0].iter().fold(0.0, |acc, &w| acc + w as f64));
        combos[1][chance] = round(weights[1].iter().fold(0.0, |acc, &w| acc + w as f64));

        let is_empty = |player: usize| weights[player].iter().all(|&w| w == 0.0);
        let is_empty_flag = [is_empty(0), is_empty(1)];

        game.cache_normalized_weights();
        let normalizer = [game.normalized_weights(0), game.normalized_weights(1)];

        if !game.is_terminal_node() {
            let current_player = game.current_player();
            if !is_empty_flag[current_player] {
                let strategy_tmp = game.strategy();
                let num_hands = game.private_cards(current_player).len();
                let ws = if is_empty_flag[current_player ^ 1] {
                    &weights[current_player]
                } else {
                    normalizer[current_player]
                };
                for action in 0..num_actions {
                    let slice = &strategy_tmp[action * num_hands..(action + 1) * num_hands];
                    let strategy_summary = weighted_average(slice, ws);
                    strategy[action * 52 + chance] = round(strategy_summary);
                }
            }
        }

        if is_empty_flag[0] || is_empty_flag[1] {
            status[chance] = 1;
            game.apply_history(&history);
            continue;
        }

        status[chance] = 2;

        let total_bet_amount = game.total_bet_amount();
        let pot_base = game.tree_config().starting_pot + total_bet_amount.iter().min().unwrap();

        for player in 0..2 {
            let pot = (pot_base + total_bet_amount[player]) as f32;
            let equity_tmp = weighted_average(&game.equity(player), normalizer[player]);
            let ev_tmp = weighted_average(&game.expected_values(player), normalizer[player]);
            equity[player][chance] = round(equity_tmp);
            ev[player][chance] = round(ev_tmp);
            eqr[player][chance] = round(ev_tmp / (pot as f64 * equity_tmp));
        }

        game.apply_history(&history);
    }

    GameChanceReportsResponse {
        status,
        combos,
        equity,
        ev,
        eqr,
        strategy,
    }
}