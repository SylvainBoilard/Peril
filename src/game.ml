type phase =
  | Claim
  | Deploy
  | Reinforce
  | Battle_SelectTerritory
  | Battle_SelectTarget
  | Battle_SelectAttackCount
  | Battle_SelectDefenceCount
  | Battle_Resolving
  | Battle_SelectInvadeCount
  | Move_SelectTerritory
  | Move_SelectDestination
  | Move_SelectCount

type t = {
    mutable players: Color.name list;
    mutable current_player: Color.name;
    mutable current_phase: phase;
    map: Map.t;
    owner: Color.name array;
    armies: int array;
  }
