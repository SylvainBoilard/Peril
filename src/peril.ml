open Utils
open Bigarray

let edition_mode = ref false
let selected_poi = ref Map.NoPOI

let attacker_count_selector = ref (Obj.magic () : Ui.army_count_selector)
let defender_count_selector = ref (Obj.magic () : Ui.army_count_selector)

let update_selected_territory territory (game : Game.t) render =
  assert (territory <> -1);
  if territory <> game.selected_territory then (
    game.selected_territory <- territory;
    let target_filter =
      if Game.in_battle_phase game
      then (fun i -> game.owner.(i) <> game.owner.(territory))
      else (fun i -> game.owner.(i) = game.owner.(territory))
    in
    let targets =
      Array.to_seq game.map.territories.(territory).adjacent
      |> Seq.filter_map (fun i ->
             if target_filter i
             then Some game.map.territories.(i).center
             else None)
      |> List.of_rev_seq
    in
    Render.update_dashed_buffers render game.map.territories.(territory).center targets
  )

let sort_dice points order offset len =
  let top_i = offset + len in
  let rec aux bot_i max_i max = function
    | i when i = top_i ->
       if max_i <> bot_i then (
         points.(max_i) <- points.(bot_i);
         points.(bot_i) <- max;
         let tmp = order.(max_i) in
         order.(max_i) <- order.(bot_i);
         order.(bot_i) <- tmp
       );
       if bot_i + 2 < top_i then
         aux (bot_i + 1) (bot_i + 1) points.(bot_i + 1) (bot_i + 2)
    | i when points.(i) > max -> aux bot_i i points.(i) (i + 1)
    | i -> aux bot_i max_i max (i + 1)
  in
  if len >= 2 then
    aux offset offset points.(offset) (offset + 1)

let need_to_animate_dice (game : Game.t) dice_order =
  game.attacking_armies <> game.defending_armies ||
    let dice_moves = ref false in
    for i = 0 to game.attacking_armies - 1 do
      if dice_order.(i) <> i || dice_order.(i + 3) <> i then
        dice_moves := true
    done;
    !dice_moves

let create_selector template (game : Game.t) attacker =
  let territory = if attacker then game.selected_territory else game.target_territory in
  let color_suite = game.players.(game.owner.(territory)).color_suite in
  let useable_armies = if attacker then game.armies.(territory) - 1 else game.armies.(territory) in
  let attacking_territory_center = game.map.territories.(game.selected_territory).center in
  let defending_territory_center = game.map.territories.(game.target_territory).center in
  let center = match attacking_territory_center.x < defending_territory_center.x, attacker with
    | true, true -> Vec2.(add attacking_territory_center { x = -0.192; y = 0.0 })
    | false, true -> Vec2.(add attacking_territory_center { x = 0.192; y = 0.0 })
    | true, false -> Vec2.(add defending_territory_center { x = 0.192; y = 0.0 })
    | false, false -> Vec2.(add defending_territory_center { x = -0.192; y = 0.0 })
  in
  Ui.make_army_count_selector_from_template template center color_suite useable_armies

let key_callback (game : Game.t) card_info_tooltip window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action, game.current_phase with
  | Escape, Press, _ -> setWindowShouldClose window true
  | F3, Press, (Claim | Deploy | Reinforce | Battle_SelectTerritory | Move_SelectTerritory | Over)
       when !selected_poi = NoPOI ->
     game.selected_territory <- -1;
     edition_mode := not !edition_mode
  | F5, Press, _ when !edition_mode ->
     Map.validate game.map;
     Map.save_to_xml_file game.map "maps/Earth.xml"
  | F6, Press, (Claim | Deploy) -> (* DEBUG: skip claim and deploy phases *)
     let territory_count = Array.length game.map.territories in
     let player_count = Array.length game.players in
     for i = 0 to territory_count - 1 do
       game.armies.(i) <- if i < 21 then 2 else 3;
       game.owner.(i) <- i mod player_count
     done;
     let random_state = Random.get_state () in
     Array.shuffle game.armies;
     Random.set_state random_state;
     Array.shuffle game.owner;
     for i = 0 to player_count - 1 do
       Game.compute_reinforcements game i
     done;
     game.current_player <- -1;
     Game.start_next_player_turn game
  | Space, Press, Battle_SelectTerritory ->
     if game.territory_captured then (
       game.players.(game.current_player).cards <- game.next_card :: game.players.(game.current_player).cards;
       if game.current_player = game.our_player then
         Ui.update_card_info_tooltip card_info_tooltip game.players.(game.our_player).cards game;
       game.next_card <- game.next_card + 1;
       game.territory_captured <- false
     );
     Game.set_current_phase game Move_SelectTerritory
  | Space, Press, Battle_Invade ->
     Game.set_current_phase game Battle_SelectTerritory
  | Space, Press, (Move_SelectTerritory | Move_Move) ->
     Game.start_next_player_turn game
  | _ -> ()

let mouse_button_callback
      (game : Game.t) render attacker_count_selector_template defender_count_selector_template
      window button pressed _(*modifiers*) =
  let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
  let cursor_coords = world_of_frame_coords cursor_pos in
  let clicked_territory = Map.find_territory_at_coords game.map cursor_coords in
  match button, pressed, game.current_phase with
  | 0, true, _ when !edition_mode ->
     if game.selected_territory = -1 then
       update_selected_territory clicked_territory game render
     else (
       let st = game.map.territories.(game.selected_territory) in
       selected_poi := Map.find_poi_of_shape_at_coords st.shape cursor_coords;
       match !selected_poi with
       | Corner n ->
          let cursor_pos = frame_of_world_coords st.shape.(n) in
          GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
          GLFW.setInputMode window Cursor Hidden
       | Edge (n, m) ->
          let new_shape =
            Array.init (Array.length st.shape + 1) (fun i ->
                if i <= n then
                  st.shape.(i)
                else if i = n + 1 then
                  Vec2.lerp st.shape.(n) st.shape.(m) 0.5
                else
                  st.shape.(i - 1)
              )
          in
          game.map.territories.(game.selected_territory) <- { st with shape = new_shape };
          selected_poi := Corner (n + 1);
          let cursor_pos = frame_of_world_coords new_shape.(n + 1) in
          GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
          GLFW.setInputMode window Cursor Hidden
       | NoPOI -> update_selected_territory clicked_territory game render
     )
  | 0, true, Claim ->
     if clicked_territory <> -1 && game.armies.(clicked_territory) = 0 then (
       game.armies.(clicked_territory) <- 1;
       game.owner.(clicked_territory) <- game.current_player;
       game.current_player <- (game.current_player + 1) mod Array.length game.players;
       if Array.for_all ((<>) 0) game.armies then
         Game.set_current_phase game Deploy
     )
  | 0, true, Deploy ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player then (
       let player_count = Array.length game.players in
       game.armies.(clicked_territory) <- game.armies.(clicked_territory) + 1;
       if Array.fold_left (+) 0 game.armies = 105 then (
         for i = 0 to player_count - 1 do
           Game.compute_reinforcements game i
         done;
         Game.start_next_player_turn game
       ) else (
         if game.current_player = game.our_player then
           Game.clear_highlighted_territories game;
         game.current_player <- (game.current_player + 1) mod Array.length game.players;
         if game.current_player = game.our_player then
           Game.update_highlighted_territories game;
       )
     )
  | 0, true, Reinforce ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player then (
       game.armies.(clicked_territory) <- game.armies.(clicked_territory) + 1;
       game.armies_to_deploy <- game.armies_to_deploy - 1;
       if game.armies_to_deploy = 0 then
         Game.set_current_phase game Battle_SelectTerritory
     )
  | 0, true, Battle_SelectTerritory ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player
        && Game.territory_can_attack game clicked_territory then (
       update_selected_territory clicked_territory game render;
       Game.set_current_phase game Battle_SelectTarget
     )
  | 0, true, Battle_SelectTarget ->
     if clicked_territory <> -1 then (
       if game.owner.(clicked_territory) = game.current_player then (
         if Game.territory_can_attack game clicked_territory then (
           update_selected_territory clicked_territory game render;
           Game.update_highlighted_territories game
         ) else
           Game.set_current_phase game Battle_SelectTerritory
       ) else if Array.mem clicked_territory game.map.territories.(game.selected_territory).adjacent then (
         game.target_territory <- clicked_territory;
         Game.set_current_phase game Battle_SelectAttackerCount;
         attacker_count_selector := create_selector attacker_count_selector_template game true;
         Render.update_dashed_buffers render
           game.map.territories.(game.selected_territory).center
           [game.map.territories.(clicked_territory).center]
       ) else
         Game.set_current_phase game Battle_SelectTerritory
     ) else
       Game.set_current_phase game Battle_SelectTerritory
  | 0, true, Battle_SelectAttackerCount ->
     let selector_center = !attacker_count_selector.center in
     let useable_armies = min 3 (game.armies.(game.selected_territory) - 1) in
     if cursor_coords.x < selector_center.x -. 0.128
        || cursor_coords.x > selector_center.x +. 0.128
        || cursor_coords.y < selector_center.y -. 0.384
        || cursor_coords.y > selector_center.y +. 0.384 then
       Game.set_current_phase game Battle_SelectTerritory
     else (
       let hovered_selector = Ui.find_hovered_selector !attacker_count_selector cursor_coords in
       if hovered_selector <> -1 && hovered_selector + 1 <= useable_armies then (
         !attacker_count_selector.activated <- hovered_selector;
         game.attacking_armies <- hovered_selector + 1;
         Game.set_current_phase game Battle_SelectDefenderCount;
         defender_count_selector := create_selector defender_count_selector_template game false
       )
     )
  | 0, true, Battle_SelectDefenderCount ->
     let useable_armies = min 2 game.armies.(game.target_territory) in
     let hovered_selector = Ui.find_hovered_selector !defender_count_selector cursor_coords in
     if hovered_selector <> -1 && hovered_selector + 1 <= useable_armies then (
       !defender_count_selector.activated <- hovered_selector;
       game.defending_armies <- hovered_selector + 1;
       Game.set_current_phase game Battle_Resolving
     )
  | 0, true, Move_SelectTerritory ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player
        && Game.territory_can_move game clicked_territory then (
       update_selected_territory clicked_territory game render;
       Game.set_current_phase game Move_SelectDestination
     )
  | 0, true, Move_SelectDestination ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player then (
       if Array.mem clicked_territory game.map.territories.(game.selected_territory).adjacent then (
         game.target_territory <- clicked_territory;
         Game.set_current_phase game Move_Move;
         Render.update_dashed_buffers render
           game.map.territories.(game.selected_territory).center
           [game.map.territories.(clicked_territory).center]
       ) else if Game.territory_can_move game clicked_territory then (
         update_selected_territory clicked_territory game render;
         Game.update_highlighted_territories game
       ) else
         Game.set_current_phase game Move_SelectTerritory
     ) else
       Game.set_current_phase game Move_SelectTerritory
  | 0, true, (Battle_Invade | Move_Move) ->
     if clicked_territory = game.target_territory && game.armies.(game.selected_territory) > 1 then (
       game.armies.(game.selected_territory) <- game.armies.(game.selected_territory) - 1;
       game.armies.(clicked_territory) <- game.armies.(clicked_territory) + 1
     )
  | 0, false, _ when !selected_poi <> NoPOI ->
     let st = game.map.territories.(game.selected_territory) in
     let new_center = compute_shape_barycenter st.shape in
     game.map.territories.(game.selected_territory) <- { st with center = new_center };
     selected_poi := NoPOI;
     GLFW.setInputMode window Cursor Normal
  | _ -> ()

let cursor_pos_callback (game : Game.t) _(*window*) x y =
  if !edition_mode then (
    let cursor_coords = world_of_frame_coords { x; y } in
    match !selected_poi with
    | Corner n when game.selected_territory <> -1 ->
       game.map.territories.(game.selected_territory).shape.(n) <- cursor_coords
    | _ -> ()
  )

let () =
  Random.self_init ();
  let map = Map.load_from_xml_file "maps/Earth.xml" in
  Map.validate map;
  GLFW.init ();
  GLFW.windowHint ClientApi OpenGLESApi;
  GLFW.windowHint ContextVersionMajor 2;
  GLFW.windowHint ContextVersionMinor 0;
  GLFW.windowHint Resizable false;
  let window = GLFW.createWindow 800 500 "Peril" () in
  GLFW.makeContextCurrent (Some window);
  GL.blendFunc SrcAlpha OneMinusSrcAlpha;
  let basic_shader = load_basic_shader () in
  let text_ctx = Text.init () in
  GL.releaseShaderCompiler ();
  let render = Render.make ("maps/" ^ map.background) in
  let text_font_serif = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" in
  let text_font_sans = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" in
  let territory_text = Text.create () in
  let fps_text, fps_outline = Text.create (), Text.create () in
  let status_text = Text.create () in
  let cartridge_text = Text.create () in
  let name_text, name_outline = Text.create (), Text.create () in
  let armies_text, armies_outline = Text.create (), Text.create () in
  let attacker_count_selector_template = Ui.make_army_count_selector_template 3 render.ui_texture Vec2.{ x = 0.0; y = 0.5 } in
  let defender_count_selector_template = Ui.make_army_count_selector_template 2 render.ui_texture Vec2.{ x = 0.0; y = 0.75 } in
  let battle_resolution = Ui.make_battle_resolution render.ui_texture in
  let card_info_tooltip = Ui.make_card_info_tooltip render.white_texture text_ctx text_font_sans in
  let game =
    let territory_count = Array.length map.territories in
    let cards_territories = Array.init territory_count Fun.id in
    let cards_armies = Array.init territory_count (fun i -> i mod 3) in
    Array.shuffle cards_territories;
    Array.shuffle cards_armies;
    { Game.players =
        [| Player.make "Roland" (Color.hsla_of_name Red);
           Player.make "Valérie" { (Color.hsla_of_name Green) with l = 0.4 };
           Player.make "Basile" { (Color.hsla_of_name Blue) with l = 0.6 } |];
      our_player = 0; defeated_count = 0; current_player = 0; current_phase = Claim;
      selected_territory = -1; target_territory = -1; armies_to_deploy = 0;
      attacking_armies = 0; defending_armies = 0; territory_captured = false;
      map;
      owner = Array.make territory_count (-1);
      armies = Array.make territory_count 0;
      highlight = Array.make territory_count false;
      cards = Array.map2 make_pair cards_territories cards_armies;
      next_card = 0; traded_in_sets = 0;
      highlight_anim = 0.0 }
  in
  let dice_points = Array.make 5 0 in
  let dice_order = Array.init 5 (fun i -> i mod 3) in
  let dice_sorted = ref false in
  let dashed_animation_time = ref 0.0 in
  GLFW.setKeyCallback window (Some (key_callback game card_info_tooltip)) |> ignore;
  GLFW.setMouseButtonCallback window (Some (mouse_button_callback game render attacker_count_selector_template defender_count_selector_template)) |> ignore;
  GLFW.setCursorPosCallback window (Some (cursor_pos_callback game)) |> ignore;
  let frame_time = ref 0.0 in
  let frame_time_count = ref 0 in
  let frame_start_time = ref (GLFW.getTime ()) in
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
    let cursor_coords = world_of_frame_coords cursor_pos in

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;
    GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;

    Render.draw_basic basic_shader render.background_texture render.background_buffer TriangleFan 0 4;

    if not !edition_mode then (
      if game.selected_territory <> -1 then (
        GL.uniform2f basic_shader.texture_coords_offset_location !dashed_animation_time 0.0;
        GL.lineWidth 3.0;
        Render.draw_basic
          basic_shader render.dashed_texture render.dashed_buffer
          ~elem_buffer:render.dashed_elem_buffer Lines 0 render.dashed_elem_count;
        GL.lineWidth 1.0;
        GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0
      );

      let highlight_outline_color =
        let t = max 0.0 (game.highlight_anim -. 4.0) in
        let l = 1.0 -. (t -. 0.5) *. (t -. 0.5) *. 4.0 in
        Color.(rgba_of_hsla { (hsla_of_name White) with l })
      in
      for i = 0 to Array.length map.territories - 1 do
        let armies_str = string_of_int game.armies.(i) in
        let owner = game.owner.(i) in
        let color =
          if owner <> -1
          then game.players.(owner).color_suite.normal
          else Color.rgba_of_name White
        in
        let outline_color =
          if game.highlight.(i)
          then highlight_outline_color
          else Color.rgba_of_name Black
        in
        Text.update text_ctx armies_text text_font_sans armies_str Regular 20 ~base_kerning:~-1 StreamDraw;
        Text.update text_ctx armies_outline text_font_sans armies_str Outline 20 ~base_kerning:~-1 StreamDraw;
        let offset = Vec2.{ x = float_of_int (armies_text.width / 2); y = -8.0 } in
        let pos = Vec2.(round (sub (frame_of_world_coords map.territories.(i).center) offset)) in
        Text.draw text_ctx armies_outline pos outline_color;
        Text.draw text_ctx armies_text pos color
      done;

      GL.useProgram basic_shader.program;
      GL.activeTexture 0;
      GL.uniform1i basic_shader.texture_location 0;
      GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;

      GL.enable Blend;
      begin match game.current_phase with
      | Battle_SelectAttackerCount ->
         !attacker_count_selector.anim_time <- Float.min 1.0 (!attacker_count_selector.anim_time +. 0.05);
         Ui.draw_army_count_selector basic_shader !attacker_count_selector cursor_coords
      | Battle_SelectDefenderCount ->
         !attacker_count_selector.anim_time <- Float.max 0.0 (!attacker_count_selector.anim_time -. 0.05);
         !defender_count_selector.anim_time <- Float.min 1.0 (!defender_count_selector.anim_time +. 0.05);
         Ui.draw_army_count_selector basic_shader !attacker_count_selector cursor_coords;
         Ui.draw_army_count_selector basic_shader !defender_count_selector cursor_coords;
      | Battle_Resolving ->
         battle_resolution.anim_time <- battle_resolution.anim_time +. 1.0 /. 60.0;
         Ui.draw_battle_resolution
           basic_shader battle_resolution dice_points dice_order game.attacking_armies game.defending_armies
      | _ -> ()
      end;
      GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
      GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
      GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0;

      Render.draw_game_info_sprites basic_shader render game;
      GL.disable Blend;
      let y_orig = -1.068 +. float_of_int (Array.length game.players) *. 0.136 in
      let row = ref game.defeated_count in
      for i = 0 to Array.length game.players - 1 do
        let player = game.players.((game.our_player + i) mod Array.length game.players) in
        if not player.defeated then (
          let y = y_orig -. 0.036 -. float_of_int !row *. 0.136 in
          let x = if player.reinforcements < 10 then -1.248 else -1.28 in
          Text.update text_ctx cartridge_text text_font_sans (string_of_int player.reinforcements) Regular 24 ~base_kerning:~-2 StreamDraw;
          Text.draw text_ctx cartridge_text (frame_of_world_coords Vec2.{ x; y }) (Color.rgba_of_name Black);
          Text.update text_ctx cartridge_text text_font_sans (string_of_int (List.length player.cards)) Regular 24 StreamDraw;
          Text.draw text_ctx cartridge_text (frame_of_world_coords Vec2.{ x = -1.44; y }) (Color.rgba_of_name Black);
          incr row
        )
      done;

      let y_orig = -1.004 +. float_of_int (Array.length game.players - game.defeated_count) *. 0.136 in
      if game.players.(game.our_player).cards <> []
         && -1.568 <= cursor_coords.x && cursor_coords.x < -1.44
         && y_orig -. 0.128 <= cursor_coords.y && cursor_coords.y < y_orig then (
        GL.useProgram basic_shader.program;
        GL.activeTexture 0;
        GL.uniform1i basic_shader.texture_location 0;
        GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
        GL.enable Blend;
        Ui.draw_card_info_tooltip basic_shader card_info_tooltip { x = -1.568; y = y_orig +. 0.008 };
        GL.disable Blend
      )
    ) else ( (* !edition_mode *)
      let vertex_count = Array.fold_left (fun c (t : Map.territory) -> c + Array.length t.shape) 0 map.territories in
      let border_data = Array1.create Float32 C_layout (vertex_count * 8) in
      let _, border_draws =
        Array.fold_left (fun (i, l) (t : Map.territory) ->
            let lum =
              if game.selected_territory <> -1 && game.map.territories.(game.selected_territory) == t
              then 1.0
              else 0.0
            in
            Array.fold_left (fun i (v : Vec2.t) ->
                let offset = i * 8 in
                border_data.{offset} <- v.x;
                border_data.{offset + 1} <- v.y;
                border_data.{offset + 2} <- 0.0;
                border_data.{offset + 3} <- 0.0;
                border_data.{offset + 4} <- lum;
                border_data.{offset + 5} <- lum;
                border_data.{offset + 6} <- lum;
                border_data.{offset + 7} <- 1.0;
                i + 1
              ) i t.shape, (i, Array.length t.shape) :: l
          ) (0, []) map.territories
      in
      GL.bindBuffer ArrayBuffer render.border_buffer;
      GL.bufferData ArrayBuffer border_data StreamDraw;
      Render.draw_basic_multi basic_shader render.white_texture render.border_buffer LineLoop border_draws;

      if game.selected_territory <> -1 && !selected_poi = NoPOI then (
        let selected_territory_shape = map.territories.(game.selected_territory).shape in
        let dot_coords = match Map.find_poi_of_shape_at_coords selected_territory_shape cursor_coords with
          | Corner n -> Some selected_territory_shape.(n)
          | Edge (n, m) -> Some (Vec2.lerp selected_territory_shape.(n) selected_territory_shape.(m) 0.5)
          | NoPOI -> None
        in
        match dot_coords with
        | Some coords ->
           GL.uniform2f basic_shader.vertex_coords_offset_location coords.x coords.y;
           GL.enable Blend;
           Render.draw_basic basic_shader render.dot_texture render.dot_buffer TriangleFan 0 4;
           GL.disable Blend;
           GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
        | None -> ()
      )
    );

    let name_color, name = match game.current_phase with
      | _ when !edition_mode -> Color.(rgba_of_name White), ""
      | Battle_SelectDefenderCount ->
         let player = game.players.(game.owner.(game.target_territory)) in
         player.color_suite.normal, player.name
      | Battle_Resolving -> Color.(rgba_of_name White), ""
      | _ ->
         let player = game.players.(game.current_player) in
         player.color_suite.normal, player.name
    in
    let status = match game.current_phase with
      | _ when !edition_mode -> "Edition"
      | Claim -> ", claim an empty territory"
      | Deploy ->
         Printf.sprintf ", deploy an army on one of your territories (you have %d remaining)"
           ((105 - Array.fold_left (+) (-2) game.armies) / 3)
      | Reinforce ->
         Printf.sprintf ", deploy an army on one of your territories (you have %d remaining)"
           game.armies_to_deploy
      | Battle_SelectTerritory -> ", select a territory from which to attack, or press Space to pass"
      | Battle_SelectTarget -> ", select the territory to attack"
      | Battle_SelectAttackerCount -> ", select the number of armies to attack with"
      | Battle_SelectDefenderCount -> ", select the number of armies to defend with"
      | Battle_Resolving -> "Battle rages..."
      | Battle_Invade -> ", invade the territory you captured with more armies, or press Space to pass"
      | Move_SelectTerritory -> ", select a territory from which to move armies, or press Space to pass"
      | Move_SelectDestination -> ", select the territory to send your armies to"
      | Move_Move -> ", move more armies to this territory, or press Space to pass"
      | Over -> " conquered the world!"
    in
    Text.update text_ctx name_outline text_font_serif name Outline 16 StreamDraw;
    Text.update text_ctx name_text text_font_serif name Regular 16 StreamDraw;
    Text.update text_ctx status_text text_font_serif status Regular 16 StreamDraw;
    Text.draw text_ctx name_outline { x = 10.0; y = 26.0 } (Color.rgba_of_name Black);
    Text.draw text_ctx name_text { x = 10.0; y = 26.0 } name_color;
    Text.draw text_ctx status_text { x = float_of_int (10 + name_text.width); y = 26.0 } (Color.rgba_of_name White);

    begin match Map.find_territory_at_coords map cursor_coords, game.selected_territory with
    | -1, -1 -> ()
    | -1, i | i, _ ->
       Text.update text_ctx territory_text text_font_serif map.territories.(i).name Regular 16 StreamDraw;
       let x = float_of_int (400 - territory_text.width / 2) in
       Text.draw text_ctx territory_text Vec2.{ x; y = 470.0 } (Color.rgba_of_name White)
    end;

    if game.current_player = game.our_player then (
      game.highlight_anim <- game.highlight_anim +. 1.0 /. 60.0;
      if game.highlight_anim >= 5.0 then
        game.highlight_anim <- game.highlight_anim -. 5.0
    );

    if game.selected_territory <> -1 then (
      if game.target_territory = -1 then
        dashed_animation_time := !dashed_animation_time +. 1.0 /. 120.0
      else
        dashed_animation_time := !dashed_animation_time +. 1.0 /. 40.0;
      if !dashed_animation_time >= 1.0 then
        dashed_animation_time := !dashed_animation_time -. 1.0
    );

    if game.current_phase = Battle_Resolving then (
      if battle_resolution.anim_time < 1.0 then (
        let t = sqrt battle_resolution.anim_time in
        for i = 0 to 4 do
          if Random.float 1.01 > t then
            dice_points.(i) <- Random.int 6
        done
      ) else if not !dice_sorted then (
        sort_dice dice_points dice_order 0 game.attacking_armies;
        sort_dice dice_points dice_order 3 game.defending_armies;
        if not (need_to_animate_dice game dice_order) then
          battle_resolution.anim_time <- 1.2;
        dice_sorted := true
      ) else if battle_resolution.anim_time >= 3.0 then (
        let min_armies = min game.attacking_armies game.defending_armies in
        let atk_i, def_i = game.selected_territory, game.target_territory in
        let atk_dead, def_dead = ref 0, ref 0 in
        for i = 0 to min_armies - 1 do
          if dice_points.(i) > dice_points.(i + 3)
          then incr def_dead
          else incr atk_dead
        done;
        game.armies.(def_i) <- game.armies.(def_i) - !def_dead;
        if game.armies.(def_i) = 0 then (
          let former_owner = game.owner.(def_i) in
          game.armies.(atk_i) <- game.armies.(atk_i) - game.attacking_armies;
          game.armies.(def_i) <- game.attacking_armies - !atk_dead;
          game.owner.(def_i) <- game.current_player;
          Game.compute_reinforcements game game.current_player;
          Game.compute_reinforcements game former_owner;
          if Array.for_all ((<>) former_owner) game.owner then (
            game.players.(former_owner).defeated <- true;
            game.players.(game.current_player).cards <-
              game.players.(former_owner).cards @ game.players.(game.current_player).cards;
            game.players.(former_owner).cards <- [];
            game.defeated_count <- game.defeated_count + 1
          );
          if Array.length game.players - 1 = game.defeated_count then
            Game.set_current_phase game Over
          else (
            game.territory_captured <- true;
            Game.set_current_phase game Battle_Invade
          )
        ) else (
          game.armies.(atk_i) <- game.armies.(atk_i) - !atk_dead;
          Game.set_current_phase game Battle_SelectTerritory
        );
        battle_resolution.anim_time <- 0.0;
        dice_sorted := false;
        for i = 0 to 4 do
          dice_order.(i) <- i mod 3
        done
      )
    );

    let fps_count = truncate (float_of_int !frame_time_count /. !frame_time +. 0.5) in
    let fps_str = Printf.sprintf "%d FPS" fps_count in
    Text.update text_ctx fps_outline text_font_sans fps_str Outline 10 StreamDraw;
    Text.update text_ctx fps_text text_font_sans fps_str Regular 10 StreamDraw;
    let fps_pos = Vec2.{ x = float_of_int (795 - fps_text.width); y = 15.0 } in
    let fps_color =
      if fps_count > 57 then
        Color.White
      else if fps_count > 50 then
        Color.Yellow
      else
        Color.Red
    in
    Text.draw text_ctx fps_outline fps_pos (Color.rgba_of_name Black);
    Text.draw text_ctx fps_text fps_pos (Color.rgba_of_name fps_color);

    GLFW.swapBuffers window;

    let frame_finish_time = GLFW.getTime () in
    if !frame_time_count < 10 then (
      incr frame_time_count;
      frame_time := !frame_time +. frame_finish_time -. !frame_start_time
    ) else
      frame_time := !frame_time *. 0.9 +. frame_finish_time -. !frame_start_time;
    frame_start_time := frame_finish_time
  done;
  Text.destroy territory_text;
  Text.destroy fps_text;
  Text.destroy fps_outline;
  Text.destroy status_text;
  Text.destroy cartridge_text;
  Text.destroy name_text;
  Text.destroy name_outline;
  Text.destroy armies_text;
  Text.destroy armies_outline;
  GLFW.destroyWindow window;
  GLFW.terminate ()
