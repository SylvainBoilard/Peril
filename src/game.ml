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
    mutable players: Color.name list;
    mutable current_player: Color.name;
    mutable current_phase: phase;
    mutable reinforcements: int;
    mutable selected_territory: int;
    mutable target_territory: int;
    mutable attacking_armies: int;
    mutable defending_armies: int;
    map: Map.t;
    owner: Color.name array;
    armies: int array;
  }

let compute_reinforcements game player =
  let territories_owned =
    Array.fold_left (fun c o -> if o = player then c + 1 else c) 0 game.owner
  in
  Array.fold_left (fun r (c : Map.continent) ->
      if Array.for_all (fun i -> game.owner.(i) = player) c.territories
      then r + c.reinforcement
      else r
    ) (max 3 (territories_owned / 3)) game.map.continents
