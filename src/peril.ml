open Utils
open Bigarray

let edition_mode = ref false
let selected_poi = ref Map.NoPOI

let update_dashed_buffers (game : Game.t) vertex_buffer elem_buffer =
  let territory = game.map.territories.(game.selected_territory) in
  let adj_count = Array.length territory.adjacent in
  let center = territory.center in
  let vertex_data = Array1.create Float32 C_layout ((adj_count + 1) * 8) in
  let sub_0 = Array1.sub vertex_data 0 8 in
  [| center.x; center.y; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 |]
  |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub_0;
  Array.iteri (fun i a ->
      let t = game.map.territories.(a) in
      let sub = Array1.sub vertex_data ((i + 1) * 8) 8 in
      let target = t.center in
      let dist = Vec2.(mag (sub center target)) in
      [| target.x; target.y; dist *. -8.0; 0.0; 0.5; 0.5; 0.5; 1.0 |]
      |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub
    ) territory.adjacent;
  let elem_data = Array1.create Int16_unsigned C_layout (adj_count * 2) in
  for i = 0 to adj_count - 1 do
    let offset = i * 2 in
    elem_data.{offset} <- 0;
    elem_data.{offset + 1} <- i + 1
  done;
  GL.bindBuffer GL.ArrayBuffer vertex_buffer;
  GL.bufferData GL.ArrayBuffer vertex_data GL.DynamicDraw;
  GL.bindBuffer GL.ElementArrayBuffer elem_buffer;
  GL.bufferData GL.ElementArrayBuffer elem_data GL.DynamicDraw

let update_selected_territory territory (game : Game.t) dashed_vertex_buffer dashed_elem_buffer =
  if territory <> game.selected_territory then (
    game.selected_territory <- territory;
    if territory <> - 1 then
      update_dashed_buffers game dashed_vertex_buffer dashed_elem_buffer
  )

let select_dashed_draws (game : Game.t) =
  let adjacent = game.map.territories.(game.selected_territory).adjacent in
  if game.target_territory <> -1 then
    [ Array.find_offset ((=) game.target_territory) adjacent * 2, 2 ]
  else if game.current_phase = Battle_SelectTarget then
    Array.fold_left_i (fun i acc t ->
        if game.owner.(t) <> game.current_player
        then (i * 2, 2) :: acc
        else acc
      ) [] adjacent
  else (
    assert (game.current_phase = Move_SelectDestination);
    Array.fold_left_i (fun i acc t ->
        if game.owner.(t) = game.current_player
        then (i * 2, 2) :: acc
        else acc
      ) [] adjacent
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

let key_callback (game : Game.t) window key _(*scancode*) action _(*modifiers*) =
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
     for i = 0 to Array.length game.armies - 1 do
       game.armies.(i) <- if i < 21 then 2 else 3;
       game.owner.(i) <- List.(nth game.players (i mod length game.players))
     done;
     let random_state = Random.get_state () in
     Array.shuffle game.armies;
     Random.set_state random_state;
     Array.shuffle game.owner;
     game.current_player <- List.hd game.players;
     game.reinforcements <- Game.compute_reinforcements game game.current_player;
     game.current_phase <- Reinforce
  | Space, Press, Battle_SelectTerritory ->
     game.selected_territory <- -1;
     game.current_phase <- Move_SelectTerritory
  | Space, Press, Battle_Invade ->
     game.selected_territory <- -1;
     game.target_territory <- -1;
     game.current_phase <- Battle_SelectTerritory
  | Space, Press, (Move_SelectTerritory | Move_Move) ->
     game.selected_territory <- -1;
     game.target_territory <- -1;
     game.current_player <- List.find_next_loop ((=) game.current_player) game.players;
     game.reinforcements <- Game.compute_reinforcements game game.current_player;
     game.current_phase <- Reinforce
  | _ -> ()

let mouse_button_callback
      (game : Game.t) dashed_vertex_buffer dashed_elems_buffer
      window button pressed _(*modifiers*) =
  let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
  let cursor_coords = world_of_frame_coords cursor_pos in
  let clicked_territory = Map.find_territory_at_coords game.map cursor_coords in
  match button, pressed, game.current_phase with
  | 0, true, _ when !edition_mode ->
     if game.selected_territory = -1 then
       update_selected_territory clicked_territory game dashed_vertex_buffer dashed_elems_buffer
     else (
       let st = game.map.territories.(game.selected_territory) in
       selected_poi := Map.find_poi_of_shape_at_coords st.shape cursor_coords;
       match !selected_poi with
       | Corner n ->
          let cursor_pos = frame_of_world_coords st.shape.(n) in
          GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
          GLFW.setInputMode window GLFW.Cursor GLFW.Hidden
       | Edge (n, m) ->
          let new_shape =
            Array.init (Array.length st.shape + 1) (fun i ->
                if i <= n
                then st.shape.(i)
                else if i = n + 1
                then Vec2.lerp st.shape.(n) st.shape.(m) 0.5
                else st.shape.(i - 1)
              )
          in
          let new_center = compute_shape_barycenter new_shape in
          game.map.territories.(game.selected_territory) <- { st with shape = new_shape; center = new_center };
          selected_poi := Corner (n + 1);
          let cursor_pos = frame_of_world_coords new_shape.(n + 1) in
          GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
          GLFW.setInputMode window GLFW.Cursor GLFW.Hidden
       | NoPOI -> update_selected_territory clicked_territory game dashed_vertex_buffer dashed_elems_buffer
     )
  | 0, true, Claim ->
     if clicked_territory <> -1 && game.armies.(clicked_territory) = 0 then (
       game.armies.(clicked_territory) <- 1;
       game.owner.(clicked_territory) <- game.current_player;
       game.current_player <- List.find_next_loop ((=) game.current_player) game.players;
       if Array.for_all ((<>) 0) game.armies then
         game.current_phase <- Deploy
     )
  | 0, true, Deploy ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player then (
       game.armies.(clicked_territory) <- game.armies.(clicked_territory) + 1;
       game.current_player <- List.find_next_loop ((=) game.current_player) game.players;
       if Array.fold_left (+) 0 game.armies = 105 then (
         game.reinforcements <- Game.compute_reinforcements game game.current_player;
         game.current_phase <- Reinforce
       )
     )
  | 0, true, Reinforce ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player then (
       game.armies.(clicked_territory) <- game.armies.(clicked_territory) + 1;
       game.reinforcements <- game.reinforcements - 1;
       if game.reinforcements = 0 then
         game.current_phase <- Battle_SelectTerritory
     )
  | 0, true, Battle_SelectTerritory ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player && game.armies.(clicked_territory) > 1
        && Array.exists (fun i -> game.owner.(i) <> game.current_player) game.map.territories.(clicked_territory).adjacent then (
       update_selected_territory clicked_territory game dashed_vertex_buffer dashed_elems_buffer;
       game.current_phase <- Battle_SelectTarget
     ) else
       update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer
  | 0, true, Battle_SelectTarget ->
     if clicked_territory <> -1 then (
       if game.owner.(clicked_territory) = game.current_player then (
         if game.armies.(clicked_territory) > 1
            && Array.exists (fun i -> game.owner.(i) <> game.current_player) game.map.territories.(clicked_territory).adjacent then
           update_selected_territory clicked_territory game dashed_vertex_buffer dashed_elems_buffer
         else (
           update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer;
           game.current_phase <- Battle_SelectTerritory
         )
       ) else if Array.mem clicked_territory game.map.territories.(game.selected_territory).adjacent then (
         game.target_territory <- clicked_territory;
         game.current_phase <- Battle_SelectAttackerCount
       ) else (
         update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer;
         game.current_phase <- Battle_SelectTerritory
       )
     ) else (
       update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer;
       game.current_phase <- Battle_SelectTerritory
     )
  | 0, true, Battle_SelectAttackerCount ->
     let useable_armies = min 3 (game.armies.(game.selected_territory) - 1) in
     if cursor_coords.x < -0.5 || cursor_coords.x > 0.5 || cursor_coords.y < -0.5 || cursor_coords.y > 0.5 then (
       update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer;
       game.target_territory <- -1;
       game.current_phase <- Battle_SelectTerritory
     ) else
       for i = 1 to useable_armies do
         if Vec2.(sqr_mag (sub cursor_coords { x = 0.0; y = float_of_int (i - 2) *. 0.3 })) <= 0.128 *. 0.128 then (
           game.attacking_armies <- i;
           game.current_phase <- Battle_SelectDefenderCount
         )
       done
  | 0, true, Battle_SelectDefenderCount ->
     let useable_armies = min 2 game.armies.(game.target_territory) in
     for i = 1 to useable_armies do
       if Vec2.(sqr_mag (sub cursor_coords { x = 0.3; y = float_of_int i *. 0.3 -. 0.45 })) <= 0.128 *. 0.128 then (
         game.defending_armies <- i;
         game.current_phase <- Battle_Resolving
       )
     done
  | 0, true, Move_SelectTerritory ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player && game.armies.(clicked_territory) > 1
        && Array.exists (fun i -> game.owner.(i) = game.current_player) game.map.territories.(clicked_territory).adjacent then (
       update_selected_territory clicked_territory game dashed_vertex_buffer dashed_elems_buffer;
       game.current_phase <- Move_SelectDestination
     ) else
       update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer
  | 0, true, Move_SelectDestination ->
     if clicked_territory <> -1 && game.owner.(clicked_territory) = game.current_player then (
       if Array.mem clicked_territory game.map.territories.(game.selected_territory).adjacent then (
         game.target_territory <- clicked_territory;
         game.current_phase <- Move_Move
       ) else if game.armies.(clicked_territory) > 1
                 && Array.exists (fun i -> game.owner.(i) = game.current_player) game.map.territories.(clicked_territory).adjacent then
         update_selected_territory clicked_territory game dashed_vertex_buffer dashed_elems_buffer
       else (
         update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer;
         game.current_phase <- Move_SelectTerritory
       )
     ) else (
       update_selected_territory (-1) game dashed_vertex_buffer dashed_elems_buffer;
       game.current_phase <- Move_SelectTerritory
     )
  | 0, true, (Battle_Invade | Move_Move) ->
     if clicked_territory = game.target_territory && game.armies.(game.selected_territory) > 1 then (
       game.armies.(game.selected_territory) <- game.armies.(game.selected_territory) - 1;
       game.armies.(clicked_territory) <- game.armies.(clicked_territory) + 1
     )
  | 0, false, _ when !selected_poi <> NoPOI ->
     selected_poi := NoPOI;
     GLFW.setInputMode window GLFW.Cursor GLFW.Normal
  | _ -> ()

let cursor_pos_callback (game : Game.t) _(*window*) x y =
  if !edition_mode then (
    let cursor_coords = world_of_frame_coords { x; y } in
    match !selected_poi with
    | Corner n when game.selected_territory <> -1 ->
       game.map.territories.(game.selected_territory).shape.(n) <- cursor_coords
    | _ -> ()
  )

let load_background filename =
  let texture = load_texture filename in
  let buffer_data = [|
       1.6;  1.0;   1.0; 0.0;   1.0; 1.0; 1.0; 1.0;
      -1.6;  1.0;   0.0; 0.0;   1.0; 1.0; 1.0; 1.0;
      -1.6; -1.0;   0.0; 1.0;   1.0; 1.0; 1.0; 1.0;
       1.6; -1.0;   1.0; 1.0;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  texture, buffer

let load_dot () =
  let texture = load_texture "gfx/dot.png" in
  let buffer_data = [|
       0.016;  0.016;   1.0; 0.0;   1.0; 1.0; 1.0; 1.0;
      -0.016;  0.016;   0.0; 0.0;   1.0; 1.0; 1.0; 1.0;
      -0.016; -0.016;   0.0; 1.0;   1.0; 1.0; 1.0; 1.0;
       0.016; -0.016;   1.0; 1.0;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  texture, buffer

let load_pulse () =
  let shader = load_pulse_shader () in
  let texture = load_texture "gfx/pulse.png" in
  let buffer_data = [|
       0.128;  0.128;   1.0; 0.0;
      -0.128;  0.128;   0.0; 0.0;
      -0.128; -0.128;   0.0; 1.0;
       0.128; -0.128;   1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  shader, texture, buffer

let load_dice () =
  let texture = load_texture "gfx/dice.png" in
  let buffer_data = [|
       0.128;  0.128;   0.125; 0.0;   1.0; 1.0; 1.0; 1.0;
      -0.128;  0.128;   0.0  ; 0.0;   1.0; 1.0; 1.0; 1.0;
      -0.128; -0.128;   0.0  ; 0.5;   1.0; 1.0; 1.0; 1.0;
       0.128; -0.128;   0.125; 0.5;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  texture, buffer

let load_battle () =
  let texture = load_texture "gfx/battle.png" in
  let buffer_data = [|
       0.128;  0.128;   0.25; 0.0;   1.0; 1.0; 1.0; 1.0;
      -0.128;  0.128;   0.0 ; 0.0;   1.0; 1.0; 1.0; 1.0;
      -0.128; -0.128;   0.0 ; 0.5;   1.0; 1.0; 1.0; 1.0;
       0.128; -0.128;   0.25; 0.5;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  texture, buffer

let make_ui_background () =
  let buffer_data = [|
       0.5;  0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
      -0.5;  0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
      -0.5; -0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
       0.5; -0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  buffer

let () =
  let map = Map.load_from_xml_file "maps/Earth.xml" in
  Map.validate map;
  GLFW.init ();
  GLFW.windowHint GLFW.ClientApi GLFW.OpenGLESApi;
  GLFW.windowHint GLFW.ContextVersionMajor 2;
  GLFW.windowHint GLFW.ContextVersionMinor 0;
  GLFW.windowHint GLFW.Resizable false;
  let window = GLFW.createWindow 800 500 "Peril" () in
  GLFW.makeContextCurrent (Some window);
  GL.blendFunc GL.SrcAlpha GL.OneMinusSrcAlpha;
  let basic_shader = load_basic_shader () in
  let background_texture, background_buffer = load_background ("maps/" ^ map.background) in
  let white_texture = load_texture "gfx/pixel.png" in
  let border_buffer = GL.genBuffer () in
  let dot_texture, dot_buffer = load_dot () in
  let pulse_shader, pulse_texture, pulse_buffer = load_pulse () in
  let dashed_texture = load_texture "gfx/dashed.png" in
  let dashed_buffer = GL.genBuffer () in
  let dashed_elem_buffer = GL.genBuffer () in
  let dice_texture, dice_buffer = load_dice () in
  let battle_texture, battle_buffer = load_battle () in
  let arrow_buffer = GL.genBuffer () in
  let ui_background_buffer = make_ui_background () in
  let text_ctx = Text.init () in
  let text_font_serif = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" in
  let text_font_sans = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" in
  let name_text = Text.create () in
  let fps_text, fps_outline = Text.create (), Text.create () in
  let status_text = Text.create () in
  let armies_text, armies_outline = Text.create (), Text.create () in
  let game =
    { Game.players = [ Red; Green; Blue ]; current_player = Red; current_phase = Claim;
      reinforcements = 0; selected_territory = -1; target_territory = -1;
      attacking_armies = 0; defending_armies = 0;
      map;
      owner = Array.make (Array.length map.territories) Color.White;
      armies = Array.make (Array.length map.territories) 0 }
  in
  let dice_points = Array.make 5 0 in
  let dice_order = Array.init 5 (fun i -> i mod 3) in
  let dice_sorted = ref false in
  let pulse_animation_time = ref 0.0 in
  let dice_animation_time = ref 0.0 in
  let frame_time = ref 0.0 in
  let frame_time_count = ref 0 in
  let frame_start_time = ref (GLFW.getTime ()) in
  GLFW.setKeyCallback window (Some (key_callback game)) |> ignore;
  GLFW.setMouseButtonCallback window (Some (mouse_button_callback game dashed_buffer dashed_elem_buffer)) |> ignore;
  GLFW.setCursorPosCallback window (Some (cursor_pos_callback game)) |> ignore;
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
    let cursor_coords = world_of_frame_coords cursor_pos in

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;
    GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;

    draw_basic basic_shader background_texture background_buffer GL.TriangleFan 0 4;

    begin match Map.find_territory_at_coords map cursor_coords, game.selected_territory with
    | -1, -1 -> ()
    | -1, i | i, _ ->
       Text.update text_ctx name_text text_font_serif map.territories.(i).name Regular 16 GL.StreamDraw;
       let x = float_of_int (400 - name_text.width / 2) in
       Text.draw text_ctx name_text Vec2.{ x; y = 470.0 } (Color.rgba_of_name White)
    end;

    if game.selected_territory <> - 1 then (
      let coords = map.territories.(game.selected_territory).center in
      GL.useProgram pulse_shader.program;
      GL.activeTexture 0;
      GL.bindTexture GL.Texture2D pulse_texture;
      GL.uniform2f pulse_shader.vertex_coords_offset_location coords.x coords.y;
      GL.uniform1i pulse_shader.texture_location 0;
      GL.uniform4f pulse_shader.color_location 1.0 1.0 1.0 0.5;
      GL.uniform1f pulse_shader.time_location !pulse_animation_time;
      GL.enable GL.Blend;
      GL.bindBuffer GL.ArrayBuffer pulse_buffer;
      GL.vertexAttribPointer pulse_shader.vertex_coords_location 2 GL.Float false 16 0;
      GL.enableVertexAttribArray pulse_shader.vertex_coords_location;
      GL.vertexAttribPointer pulse_shader.vertex_texture_coords_location 2 GL.Float false 16 8;
      GL.enableVertexAttribArray pulse_shader.vertex_texture_coords_location;
      GL.drawArrays GL.TriangleFan 0 4;
      GL.disableVertexAttribArray pulse_shader.vertex_texture_coords_location;
      GL.disableVertexAttribArray pulse_shader.vertex_coords_location;
      GL.disable GL.Blend
    );

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;
    GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;

    if not !edition_mode then (
      if game.selected_territory <> -1 then (
        let dashed_draws = select_dashed_draws game in
        GL.uniform2f basic_shader.texture_coords_offset_location !pulse_animation_time 0.0;
        GL.enable GL.Blend;
        GL.lineWidth 3.0;
        draw_basic_multi
          basic_shader dashed_texture dashed_buffer
          ~elem_buffer:dashed_elem_buffer GL.Lines dashed_draws;
        GL.lineWidth 1.0;
        GL.disable GL.Blend;
        GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0
      );

      for i = 0 to Array.length map.territories - 1 do
        let armies_str = string_of_int game.armies.(i) in
        Text.update text_ctx armies_text text_font_sans armies_str Regular 20 GL.StreamDraw;
        Text.update text_ctx armies_outline text_font_sans armies_str Outline 20 GL.StreamDraw;
        let offset = Vec2.{ x = float_of_int (armies_text.width / 2); y = -8.0 } in
        let pos = Vec2.(round (sub (frame_of_world_coords map.territories.(i).center) offset)) in
        Text.draw text_ctx armies_outline pos (Color.rgba_of_name Black);
        Text.draw text_ctx armies_text pos (Color.rgba_of_name game.owner.(i))
      done;

      GL.useProgram basic_shader.program;
      GL.activeTexture 0;
      GL.uniform1i basic_shader.texture_location 0;
      GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;

      begin match game.current_phase with
      | Battle_SelectAttackerCount ->
         GL.enable GL.Blend;
         draw_basic basic_shader white_texture ui_background_buffer GL.TriangleFan 0 4;
         let c = Color.hsla_of_name game.owner.(game.selected_territory) in
         let useable_armies = min 3 (game.armies.(game.selected_territory) - 1) in
         for i = 0 to 2 do
           let c =
             Color.rgba_of_hsla @@
               if i + 1 > useable_armies then
                 { c with s = 0.0; l = 0.25 }
               else if Vec2.(sqr_mag (sub cursor_coords { x = 0.0; y = float_of_int (i - 1) *. 0.3 })) <= 0.128 *. 0.128 then
                 { c with l = 0.55 }
               else
                 { c with l = 0.45 }
           in
           GL.uniform4f basic_shader.ambient_color_location c.r c.g c.b 1.0;
           GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 (float_of_int (i - 1) *. 0.3);
           GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int i *. 0.25) 0.0;
           draw_basic basic_shader battle_texture battle_buffer GL.TriangleFan 0 4
         done;
         GL.disable GL.Blend;
         GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
         GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0;
         GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0
      | Battle_SelectDefenderCount ->
         GL.enable GL.Blend;
         draw_basic basic_shader white_texture ui_background_buffer GL.TriangleFan 0 4;
         let c = Color.rgba_of_name game.owner.(game.selected_territory) in
         GL.uniform4f basic_shader.ambient_color_location c.r c.g c.b c.a;
         GL.uniform2f basic_shader.vertex_coords_offset_location (-0.3) 0.0;
         GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int (game.attacking_armies - 1) *. 0.25) 0.0;
         draw_basic basic_shader battle_texture battle_buffer GL.TriangleFan 0 4;
         let c = Color.hsla_of_name game.owner.(game.target_territory) in
         let useable_armies = min 2 game.armies.(game.target_territory) in
         for i = 0 to 1 do
           let c =
             Color.rgba_of_hsla @@
               if i + 1 > useable_armies then
                 { c with s = 0.0; l = 0.25 }
               else if Vec2.(sqr_mag (sub cursor_coords { x = 0.3; y = float_of_int i *. 0.3 -. 0.15 })) <= 0.128 *. 0.128 then
                 { c with l = 0.55 }
               else
                 { c with l = 0.45 }
           in
           GL.uniform4f basic_shader.ambient_color_location c.r c.g c.b 1.0;
           GL.uniform2f basic_shader.vertex_coords_offset_location 0.3 (float_of_int i *. 0.3 -. 0.15);
           GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int i *. 0.25) 0.5;
           draw_basic basic_shader battle_texture battle_buffer GL.TriangleFan 0 4
         done;
         GL.disable GL.Blend;
         GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
         GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0;
         GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0
      | Battle_Resolving ->
         let dice_t = Float.clamp 0.0 1.0 ((!dice_animation_time -. 1.1) *. 2.0) in
         let dice_t_io = ease_in_out dice_t in
         let dice_t_i_f = ease_in (min 1.0 (dice_t *. 2.0)) in
         let arrow_t = Float.clamp 0.0 1.0 ((!dice_animation_time -. 1.3) *. 2.0) in
         let arrow_t_io = ease_in_out arrow_t in
         let arrow_t_i_f = ease_in (min 1.0 (arrow_t *. 3.0)) in
         let min_armies = min game.attacking_armies game.defending_armies in
         GL.enable GL.Blend;
         draw_basic basic_shader white_texture ui_background_buffer GL.TriangleFan 0 4;
         for i = 0 to min_armies - 1 do
           let x = Float.lerp (-0.172) 0.140 arrow_t_io in
           let y = float_of_int (i - 1) *. -0.3 in
           let buffer_data =
             Array1.of_array Float32 C_layout @@
               if dice_points.(i) > dice_points.(i + 3) then
                 [|   x         ; y +. 0.032;   0.78125; 0.0  ;   1.0; 1.0; 1.0; arrow_t_i_f;
                     -0.172     ; y +. 0.032;   0.75   ; 0.0  ;   1.0; 1.0; 1.0; arrow_t_i_f;
                     -0.172     ; y -. 0.032;   0.75   ; 0.125;   1.0; 1.0; 1.0; arrow_t_i_f;
                      x         ; y -. 0.032;   0.78125; 0.125;   1.0; 1.0; 1.0; arrow_t_i_f;
                      x +. 0.032; y -. 0.032;   0.8125 ; 0.125;   1.0; 1.0; 1.0; arrow_t_i_f;
                      x +. 0.032; y +. 0.032;   0.8125 ; 0.0  ;   1.0; 1.0; 1.0; arrow_t_i_f |]
               else
                 [| -.x         ; y +. 0.032;   0.78125; 0.125;   1.0; 1.0; 1.0; arrow_t_i_f;
                    -.x -. 0.032; y +. 0.032;   0.8125 ; 0.125;   1.0; 1.0; 1.0; arrow_t_i_f;
                    -.x -. 0.032; y -. 0.032;   0.8125 ; 0.25 ;   1.0; 1.0; 1.0; arrow_t_i_f;
                    -.x         ; y -. 0.032;   0.78125; 0.25 ;   1.0; 1.0; 1.0; arrow_t_i_f;
                      0.172     ; y -. 0.032;   0.75   ; 0.25 ;   1.0; 1.0; 1.0; arrow_t_i_f;
                      0.172     ; y +. 0.032;   0.75   ; 0.125;   1.0; 1.0; 1.0; arrow_t_i_f |]
           in
           GL.bindBuffer GL.ArrayBuffer arrow_buffer;
           GL.bufferData GL.ArrayBuffer buffer_data GL.DynamicDraw;
           draw_basic basic_shader battle_texture arrow_buffer GL.TriangleFan 0 6
         done;
         for i = 0 to game.attacking_armies - 1 do
           let o = dice_order.(i) in
           let y =
             if i < min_armies then (
               GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
               Float.lerp (float_of_int (o - 1)) (float_of_int (i - 1)) dice_t_io *. -0.3
             ) else (
               GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 (1.0 -. dice_t_i_f);
               Float.lerp (float_of_int (o - 1)) (float_of_int (o - 1) +. 0.5) dice_t_i_f *. -0.3
             )
           in
           GL.uniform2f basic_shader.vertex_coords_offset_location (-0.3) y;
           GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i) *. 0.125) 0.0;
           draw_basic basic_shader dice_texture dice_buffer GL.TriangleFan 0 4
         done;
         for i = 0 to game.defending_armies - 1 do
           let o = dice_order.(i + 3) in
           let y =
             if i < min_armies then (
               GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
               Float.lerp (float_of_int (o - 1)) (float_of_int (i - 1)) dice_t_io *. -0.3
             ) else (
               GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 (1.0 -. dice_t_i_f);
               Float.lerp (float_of_int (o - 1)) (float_of_int (o - 1) +. 0.5) dice_t_i_f *. -0.3
             )
           in
           GL.uniform2f basic_shader.vertex_coords_offset_location 0.3 y;
           GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i + 3) *. 0.125) 0.5;
           draw_basic basic_shader dice_texture dice_buffer GL.TriangleFan 0 4
         done;
         GL.disable GL.Blend;
         GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
         GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
         GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
      | _ -> ()
      end
    ) else (
      let vertex_count = Array.fold_left (fun c (t : Map.territory) -> c + Array.length t.shape) 0 map.territories in
      let border_data = Array1.create Float32 C_layout (vertex_count * 8) in
      let _, border_draws =
        Array.fold_left (fun (i, l) (t : Map.territory) ->
            Array.fold_left (fun i (v : Vec2.t) ->
                let offset = i * 8 in
                border_data.{offset} <- v.x;
                border_data.{offset + 1} <- v.y;
                border_data.{offset + 2} <- 0.0;
                border_data.{offset + 3} <- 0.0;
                border_data.{offset + 4} <- 0.0;
                border_data.{offset + 5} <- 0.0;
                border_data.{offset + 6} <- 0.0;
                border_data.{offset + 7} <- 1.0;
                i + 1
              ) i t.shape, (i, Array.length t.shape) :: l
          ) (0, []) map.territories
      in
      GL.bindBuffer GL.ArrayBuffer border_buffer;
      GL.bufferData GL.ArrayBuffer border_data GL.StreamDraw;
      draw_basic_multi basic_shader white_texture border_buffer GL.LineLoop border_draws;

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
           GL.enable GL.Blend;
           draw_basic basic_shader dot_texture dot_buffer GL.TriangleFan 0 4;
           GL.disable GL.Blend;
           GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
        | None -> ()
      )
    );

    let status = match game.current_phase with
      | _ when !edition_mode -> "Edition"
      | Claim ->
         Printf.sprintf "%s player, claim an empty territory"
           (Color.string_of_name game.current_player)
      | Deploy ->
         Printf.sprintf "%s player, deploy an army on one of your territories (you have %d remaining)"
           (Color.string_of_name game.current_player) ((105 - Array.fold_left (+) (-2) game.armies) / 3)
      | Reinforce ->
         Printf.sprintf "%s player, deploy an army on one of your territories (you have %d remaining)"
           (Color.string_of_name game.current_player) game.reinforcements
      | Battle_SelectTerritory ->
         Printf.sprintf "%s player, select a territory from which to attack, or press Space to pass"
           (Color.string_of_name game.current_player)
      | Battle_SelectTarget ->
         Printf.sprintf "%s player, select the territory to attack"
           (Color.string_of_name game.current_player)
      | Battle_SelectAttackerCount ->
         Printf.sprintf "%s player, select the number of armies to attack with"
           (Color.string_of_name game.current_player)
      | Battle_SelectDefenderCount ->
         Printf.sprintf "%s player, select the number of armies to defend with"
           (Color.string_of_name game.owner.(game.target_territory))
      | Battle_Resolving -> "Battle rages..."
      | Battle_Invade ->
         Printf.sprintf "%s player, invade the territory you captured with more armies, or press Space to pass"
           (Color.string_of_name game.current_player)
      | Move_SelectTerritory ->
         Printf.sprintf "%s player, select a territory from which to move armies, or press Space to pass"
           (Color.string_of_name game.current_player)
      | Move_SelectDestination ->
         Printf.sprintf "%s player, select the territory to send your armies to"
           (Color.string_of_name game.current_player)
      | Move_Move ->
         Printf.sprintf "%s player, move more armies to this territory, or press Space to pass"
           (Color.string_of_name game.current_player)
      | Over ->
         Printf.sprintf "%s player conquered the world!"
           (Color.string_of_name game.current_player)
    in
    Text.update text_ctx status_text text_font_serif (String.capitalize_ascii status) Regular 16 GL.StreamDraw;
    Text.draw text_ctx status_text { x = 10.0; y = 26.0 } (Color.rgba_of_name White);

    pulse_animation_time := !pulse_animation_time +. 1.0 /. 120.0;
    if !pulse_animation_time > 1.0 then pulse_animation_time := 0.0;

    if game.current_phase = Battle_Resolving then (
      dice_animation_time := !dice_animation_time +. 1.0 /. 60.0;
      if !dice_animation_time < 1.0 then (
        let t = sqrt !dice_animation_time in
        for i = 0 to 4 do
          if Random.float 1.01 > t then
            dice_points.(i) <- Random.int 6
        done
      ) else if !dice_animation_time >= 1.1 && not !dice_sorted then (
        sort_dice dice_points dice_order 0 game.attacking_armies;
        sort_dice dice_points dice_order 3 game.defending_armies;
        dice_sorted := true
      ) else if !dice_animation_time >= 3.0 then (
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
          if Array.for_all ((<>) former_owner) game.owner then
            game.players <- List.filter ((<>) former_owner) game.players;
          if List.length game.players = 1 then (
            game.selected_territory <- -1;
            game.target_territory <- -1;
            game.current_phase <- Over
          ) else
            game.current_phase <- Battle_Invade
        ) else (
          game.armies.(atk_i) <- game.armies.(atk_i) - !atk_dead;
          game.selected_territory <- -1;
          game.target_territory <- -1;
          game.current_phase <- Battle_SelectTerritory
        );
        dice_animation_time := 0.0;
        dice_sorted := false;
        for i = 0 to 4 do
          dice_order.(i) <- i mod 3
        done
      )
    );

    let fps_count = truncate (float_of_int !frame_time_count /. !frame_time +. 0.5) in
    let fps_str = Printf.sprintf "%d FPS" fps_count in
    Text.update text_ctx fps_outline text_font_sans fps_str Outline 10 GL.StreamDraw;
    Text.update text_ctx fps_text text_font_sans fps_str Regular 10 GL.StreamDraw;
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
  Text.destroy name_text;
  Text.destroy fps_text;
  Text.destroy fps_outline;
  Text.destroy status_text;
  Text.destroy armies_text;
  Text.destroy armies_outline;
  GLFW.destroyWindow window;
  GLFW.terminate ()
