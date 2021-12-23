type phase =
  | Claim
  | Deploy
  | Reinforce
  | Battle_SelectTerritory
  | Battle_SelectTarget
  | Battle_SelectAttackerCount
  | Battle_SelectDefenderCount
  | Battle_Resolving
  | Battle_Invade
  | Move_SelectTerritory
  | Move_SelectDestination
  | Move_Move
  | Over

type t = {
    players: Player.t array;
    our_player: int;
    mutable defeated_count: int;
    mutable current_player: int;
    mutable current_phase: phase;
    mutable selected_territory: int;
    mutable target_territory: int;
    mutable armies_to_deploy: int;
    mutable attacking_armies: int;
    mutable defending_armies: int;
    mutable territory_captured: bool;
    map: Map.t;
    owner: int array;
    armies: int array;
    cards: (int * int) array;
    mutable next_card: int;
    mutable traded_in_sets: int;
  }

let in_main_loop_phase game = match game.current_phase with
  | Claim | Deploy | Over -> false
  | _ -> true

let in_battle_phase game = match game.current_phase with
  | Battle_SelectTerritory
  | Battle_SelectTarget
  | Battle_SelectAttackerCount
  | Battle_SelectDefenderCount
  | Battle_Resolving
  | Battle_Invade
    -> true
  | _ -> false

let in_move_phase game = match game.current_phase with
  | Move_SelectTerritory
  | Move_SelectDestination
  | Move_Move
    -> true
  | _ -> false

let compute_reinforcements game player =
  let by_territories =
    Array.fold_left (fun c o -> if o = player then c + 1 else c) 0 game.owner
  in
  let by_continents =
    Array.fold_left (fun r (c : Map.continent) ->
        if Array.for_all (fun i -> game.owner.(i) = player) c.territories
        then r + c.reinforcement
        else r
      ) 0 game.map.continents
  in
  game.players.(player).reinforcements <- (max 3 (by_territories / 3)) + by_continents

let start_next_player_turn game =
  let player_count = Array.length game.players in
  let next_player =
    let rec loop = function
      | i when game.players.(i).defeated -> loop ((i + 1) mod player_count)
      | i -> i
    in
    loop ((game.current_player + 1) mod player_count)
  in
  game.current_player <- next_player;
  game.current_phase <- Reinforce;
  game.selected_territory <- -1;
  game.target_territory <- -1;
  game.armies_to_deploy <- game.players.(next_player).reinforcements
