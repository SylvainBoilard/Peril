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
    map: Map.t;
    owner: Color.name array;
    armies: int array;
  }
