open Utils
open Bigarray

let edition_mode = ref false
let selected_territory = ref None
let target_territory = ref None
let selected_poi = ref Map.NoPOI
let pulse_animation_time = ref 0.0
let dice_rolling = ref false
let dice_animation_time = ref 0.0
let reinforcements = ref 0

let update_dashed_buffers (map : Map.t) (territory : Map.territory) vertex_buffer elem_buffer =
  let adj_count = Array.length territory.adjacent in
  let center = territory.center in
  let vertex_data = Array1.create Float32 C_layout ((adj_count + 1) * 8) in
  let sub_0 = Array1.sub vertex_data 0 8 in
  [| center.x; center.y; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 |]
  |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub_0;
  Array.iteri (fun i a ->
      let t = map.territories.(a) in
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

let update_selected_territory territory map dashed_vertex_buffer dashed_elem_buffer =
  match territory, !selected_territory with
  | Some t, Some st when t = st -> ()
  | None, _ -> selected_territory := None
  | Some t, _ ->
     update_dashed_buffers map map.territories.(t) dashed_vertex_buffer dashed_elem_buffer;
     selected_territory := territory

let compute_reinforcements (game : Game.t) player =
  let territories_owned = ref 0 in
  for i = 0 to Array.length game.map.territories - 1 do
    if game.owner.(i) = player then incr territories_owned
  done;
  Array.fold_left (fun r (c : Map.continent) ->
      if Array.for_all (fun i -> game.owner.(i) = player) c.territories
      then r + c.reinforcement
      else r
    ) (max 3 (!territories_owned / 3)) game.map.continents

let key_callback (game : Game.t) window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | Escape, Press -> setWindowShouldClose window true
  | F3, Press when !selected_poi = NoPOI ->
     selected_territory := None;
     edition_mode := not !edition_mode
  | F5, Press when !edition_mode ->
     Map.validate game.map;
     Map.save_to_xml_file game.map "maps/Earth.xml"
  | Space, Press when game.current_phase = Battle_SelectTerritory ->
     selected_territory := None;
     game.current_phase <- Move_SelectTerritory
  | Space, Press when game.current_phase = Move_SelectTerritory ->
     selected_territory := None;
     game.current_player <- List.find_next_loop ((=) game.current_player) game.players;
     reinforcements := compute_reinforcements game game.current_player;
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
     begin match !selected_territory with
     | None -> update_selected_territory clicked_territory game.map dashed_vertex_buffer dashed_elems_buffer
     | Some st_i ->
        let st = game.map.territories.(st_i) in
        selected_poi := Map.find_poi_of_shape_at_coords st.shape cursor_coords;
        begin match !selected_poi with
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
           game.map.territories.(st_i) <- { st with shape = new_shape; center = new_center };
           selected_poi := Corner (n + 1);
           let cursor_pos = frame_of_world_coords new_shape.(n + 1) in
           GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
           GLFW.setInputMode window GLFW.Cursor GLFW.Hidden
        | NoPOI -> update_selected_territory clicked_territory game.map dashed_vertex_buffer dashed_elems_buffer
        end
     end
  | 0, true, Claim ->
     begin match clicked_territory with
     | Some ct_i ->
        if game.armies.(ct_i) = 0 then (
          game.armies.(ct_i) <- 1;
          game.owner.(ct_i) <- game.current_player;
          game.current_player <- List.find_next_loop ((=) game.current_player) game.players;
          if Array.for_all ((<>) 0) game.armies then
            game.current_phase <- Deploy
        )
     | None -> ()
     end
  | 0, true, Deploy ->
     begin match clicked_territory with
     | Some ct_i ->
        if game.owner.(ct_i) = game.current_player then (
          game.armies.(ct_i) <- game.armies.(ct_i) + 1;
          game.current_player <- List.find_next_loop ((=) game.current_player) game.players;
          if Array.fold_left (+) 0 game.armies = 105 then (
            reinforcements := compute_reinforcements game game.current_player;
            game.current_phase <- Reinforce
          )
        )
     | None -> ()
     end
  | 0, true, Reinforce ->
     begin match clicked_territory with
     | Some ct_i ->
        if game.owner.(ct_i) = game.current_player then (
          game.armies.(ct_i) <- game.armies.(ct_i) + 1;
          decr reinforcements;
          if !reinforcements = 0 then
            game.current_phase <- Battle_SelectTerritory
        )
     | None -> ()
     end
  | 0, true, Battle_SelectTerritory ->
     begin match clicked_territory with
     | Some ct_i when game.owner.(ct_i) = game.current_player ->
        update_selected_territory clicked_territory game.map dashed_vertex_buffer dashed_elems_buffer;
        game.current_phase <- Battle_SelectTarget
     | _ -> update_selected_territory None game.map dashed_vertex_buffer dashed_elems_buffer
     end
  | 0, true, Battle_SelectTarget ->
     begin match clicked_territory with
     | Some ct_i when game.owner.(ct_i) <> game.current_player ->
        target_territory := clicked_territory;
        game.current_phase <- Battle_SelectAttackCount
     | _ ->
        update_selected_territory clicked_territory game.map dashed_vertex_buffer dashed_elems_buffer;
        if clicked_territory = None then game.current_phase <- Battle_SelectTerritory
     end
  | 0, true, Move_SelectTerritory ->
     begin match clicked_territory with
     | Some ct_i when game.owner.(ct_i) = game.current_player ->
        update_selected_territory clicked_territory game.map dashed_vertex_buffer dashed_elems_buffer;
        game.current_phase <- Move_SelectDestination
     | _ -> update_selected_territory None game.map dashed_vertex_buffer dashed_elems_buffer
     end
  | 0, true, Move_SelectDestination ->
     begin match clicked_territory with
     | Some ct_i when game.owner.(ct_i) = game.current_player ->
        target_territory := clicked_territory;
        game.current_phase <- Move_SelectCount
     | _ ->
        update_selected_territory clicked_territory game.map dashed_vertex_buffer dashed_elems_buffer;
        if clicked_territory = None then game.current_phase <- Move_SelectTerritory
     end
  (* | 0, true, _ -> . *)
  | 0, false, _ when !selected_poi <> NoPOI ->
     selected_poi := NoPOI;
     GLFW.setInputMode window GLFW.Cursor GLFW.Normal
  | _ -> ()

let cursor_pos_callback (game : Game.t) _(*window*) x y =
  if !edition_mode then (
    let cursor_coords = world_of_frame_coords { x; y } in
    match !selected_territory, !selected_poi with
    | Some st_i, Corner n -> game.map.territories.(st_i).shape.(n) <- cursor_coords
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
       0.5;  0.5;   0.0; 0.0;   0.75; 0.75; 0.75; 0.5;
      -0.5;  0.5;   0.0; 0.0;   0.75; 0.75; 0.75; 0.5;
      -0.5; -0.5;   0.0; 0.0;   0.75; 0.75; 0.75; 0.5;
       0.5; -0.5;   0.0; 0.0;   0.75; 0.75; 0.75; 0.5;
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
  let ui_background_buffer = make_ui_background () in
  let text_ctx = Text.init () in
  let text_font_serif = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" in
  let text_font_sans = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" in
  let game =
    { Game.players = [ Red; Green; Blue ]; current_player = Red; current_phase = Claim; map;
      owner = Array.make (Array.length map.territories) Color.White;
      armies = Array.make (Array.length map.territories) 0 }
  in
  GLFW.setKeyCallback window (Some (key_callback game)) |> ignore;
  GLFW.setMouseButtonCallback window (Some (mouse_button_callback game dashed_buffer dashed_elem_buffer)) |> ignore;
  GLFW.setCursorPosCallback window (Some (cursor_pos_callback game)) |> ignore;
  let dice_points = Array.make 5 0 in
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
    let cursor_coords = world_of_frame_coords cursor_pos in

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;

    draw_basic basic_shader background_texture background_buffer GL.TriangleFan 0 4;

    begin match Map.find_territory_at_coords map cursor_coords, !selected_territory with
    | Some i, _ | None, Some i ->
       let name_text = Text.make text_ctx text_font_serif map.territories.(i).name Regular 16 in
       let x = float_of_int (400 - name_text.width / 2) in
       Text.draw text_ctx name_text Vec2.{ x; y = 470.0 } (Color.of_name Black);
       Text.destroy name_text
    | None, None -> ()
    end;

    begin match !selected_territory with
    | Some i ->
       let coords = map.territories.(i).center in
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
    | None -> ()
    end;

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;

    if not !edition_mode then (
      begin match !selected_territory with
      | Some i ->
         GL.uniform2f basic_shader.texture_coords_offset_location !pulse_animation_time 0.0;
         GL.enable GL.Blend;
         GL.lineWidth 3.0;
         draw_basic
           basic_shader dashed_texture dashed_buffer ~elem_buffer:dashed_elem_buffer
           GL.Lines 0 (Array.length map.territories.(i).adjacent * 2);
         GL.lineWidth 1.0;
         GL.disable GL.Blend;
         GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0
      | None -> ()
      end;

      for i = 0 to Array.length map.territories - 1 do
        let armies_str = string_of_int game.armies.(i) in
        let text = Text.make text_ctx text_font_sans armies_str Regular 20 in
        let outline = Text.make text_ctx text_font_sans armies_str Outline 20 in
        let offset = Vec2.{ x = float_of_int (text.width / 2); y = -8.0 } in
        let pos = Vec2.(sub (frame_of_world_coords map.territories.(i).center) offset) in
        Text.draw text_ctx outline pos (Color.of_name Black);
        Text.draw text_ctx text pos (Color.of_name game.owner.(i));
        Text.destroy text;
        Text.destroy outline
      done;

      if !dice_rolling then (
        GL.useProgram basic_shader.program;
        GL.activeTexture 0;
        GL.uniform1i basic_shader.texture_location 0;

        GL.enable GL.Blend;
        draw_basic basic_shader white_texture ui_background_buffer GL.TriangleFan 0 4;
        for i = 0 to 2 do
          GL.uniform2f basic_shader.vertex_coords_offset_location (-0.3) (float_of_int (i - 1) *. -0.3);
          GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i) *. 0.125) 0.0;
          draw_basic basic_shader dice_texture dice_buffer GL.TriangleFan 0 4
        done;
        for i = 0 to 1 do
          GL.uniform2f basic_shader.vertex_coords_offset_location 0.3 (float_of_int (i - 1) *. -0.3);
          GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i + 3) *. 0.125) 0.5;
          draw_basic basic_shader dice_texture dice_buffer GL.TriangleFan 0 4
        done;
        GL.disable GL.Blend;
        GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
        GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
      ) else if game.current_phase = Battle_SelectAttackCount
                || game.current_phase = Battle_SelectDefenceCount then (
        GL.useProgram basic_shader.program;
        GL.activeTexture 0;
        GL.uniform1i basic_shader.texture_location 0;

        GL.enable GL.Blend;
        draw_basic basic_shader white_texture ui_background_buffer GL.TriangleFan 0 4;
        for i = 0 to 2 do
          GL.uniform2f basic_shader.vertex_coords_offset_location (-0.3) (float_of_int (i - 1) *. -0.3);
          GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int i *. 0.25) 0.0;
          draw_basic basic_shader battle_texture battle_buffer GL.TriangleFan 0 4
        done;
        for i = 0 to 1 do
          GL.uniform2f basic_shader.vertex_coords_offset_location 0.3 (float_of_int (i - 1) *. -0.3);
          GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int i *. 0.25) 0.5;
          draw_basic basic_shader battle_texture battle_buffer GL.TriangleFan 0 4
        done;
        GL.disable GL.Blend;
        GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
        GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
      )
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

      if !selected_territory <> None && !selected_poi = NoPOI then (
        let selected_territory_shape = map.territories.(Option.get !selected_territory).shape in
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
           (Color.string_of_name game.current_player) !reinforcements
      | Battle_SelectTerritory ->
         Printf.sprintf "%s player, select a territory from which to attack, or press Space to pass"
           (Color.string_of_name game.current_player)
      | Battle_SelectTarget ->
         Printf.sprintf "%s player, select the territory to attack"
           (Color.string_of_name game.current_player)
      | Battle_SelectAttackCount ->
         Printf.sprintf "%s player, select the number of armies to attack with"
           (Color.string_of_name game.current_player)
      | Battle_SelectDefenceCount ->
         Printf.sprintf "<fixme> player, select the number of armies to defend with"
      | Move_SelectTerritory ->
         Printf.sprintf "%s player, select a territory from which to move armies, or press Space to pass"
           (Color.string_of_name game.current_player)
      | Move_SelectDestination ->
         Printf.sprintf "%s player, select the territory to send your armies to"
           (Color.string_of_name game.current_player)
      | Move_SelectCount ->
         Printf.sprintf "%s player, select the number of armies to move"
           (Color.string_of_name game.current_player)
      | _ -> "<todo>"
    in
    let status_text = Text.make text_ctx text_font_serif (String.capitalize_ascii status) Regular 16 in
    Text.draw text_ctx status_text { x = 10.0; y = 26.0 } (Color.of_name Black);
    Text.destroy status_text;

    pulse_animation_time := !pulse_animation_time +. 0.008;
    if !pulse_animation_time > 1.0 then pulse_animation_time := 0.0;

    if !dice_rolling && !dice_animation_time < 1.0 then (
      dice_animation_time := !dice_animation_time +. 0.0125;
      let t = sqrt !dice_animation_time in
      for i = 0 to 4 do
        if Random.float 1.01 > t then
          dice_points.(i) <- Random.int 6
      done
    );

    GLFW.swapBuffers window
  done;
  GLFW.destroyWindow window;
  GLFW.terminate ()
