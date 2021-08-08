type phase =
  | Claim
  | Deploy
  | Reinforce
  | Battle
  | Move

type t = {
    mutable players: Color.name list;
    mutable current_player: Color.name;
    mutable current_phase: phase;
    map: Map.t;
    owner: Color.name array;
    armies: int array;
  }
