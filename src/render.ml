open Utils
open Bigarray

type t = {
    white_texture: GL.texture;
    background_texture: GL.texture;
    background_buffer: GL.buffer;
    border_buffer: GL.buffer;
    dot_texture: GL.texture;
    dot_buffer: GL.buffer;
    dashed_texture: GL.texture;
    dashed_buffer: GL.buffer;
    dashed_elem_buffer: GL.buffer;
    mutable dashed_elem_count: int;
    ui_texture: GL.texture;
    game_state_buffer: GL.buffer;
    cartridge_buffer: GL.buffer;
  }

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
  GL.bindBuffer ArrayBuffer buffer;
  GL.bufferData ArrayBuffer buffer_data StaticDraw;
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
  GL.bindBuffer ArrayBuffer buffer;
  GL.bufferData ArrayBuffer buffer_data StaticDraw;
  texture, buffer

let load_game_state_buffer () =
  let buffer_data = [|
      0.128; 0.128;   0.5625; 0.625;   1.0; 1.0; 1.0; 1.0;
      0.0  ; 0.128;   0.5   ; 0.625;   1.0; 1.0; 1.0; 1.0;
      0.0  ; 0.0  ;   0.5   ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
      0.128; 0.0  ;   0.5625; 0.75 ;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer ArrayBuffer buffer;
  GL.bufferData ArrayBuffer buffer_data StaticDraw;
  buffer

let load_cartridge_buffer () =
  let gold = Color.rgba_of_hsla { h = 46.0; s = 0.9; l = 0.5; a = 1.0 } in
  let buffer_data = [|
      (* cartridge *)
      -1.184; 0.128;   0.390625; 0.625;   gold.r; gold.g; gold.b; 1.0;
      -1.6  ; 0.128;   0.375   ; 0.625;   gold.r; gold.g; gold.b; 1.0;
      -1.6  ; 0.0  ;   0.375   ; 0.75 ;   gold.r; gold.g; gold.b; 1.0;
      -1.184; 0.0  ;   0.390625; 0.75 ;   gold.r; gold.g; gold.b; 1.0;
      -1.152; 0.0  ;   0.40625 ; 0.75 ;   gold.r; gold.g; gold.b; 1.0;
      -1.152; 0.128;   0.40625 ; 0.625;   gold.r; gold.g; gold.b; 1.0;
      (* banner icon *)
      -1.264; 0.128;   0.5     ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
      -1.392; 0.128;   0.4375  ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
      -1.392; 0.0  ;   0.4375  ; 0.625;   0.0; 0.0; 0.0; 1.0;
      -1.264; 0.0  ;   0.5     ; 0.625;   0.0; 0.0; 0.0; 1.0;
      (* cards icon *)
      -1.44 ; 0.128;   0.5625  ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
      -1.568; 0.128;   0.5     ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
      -1.568; 0.0  ;   0.5     ; 0.625;   0.0; 0.0; 0.0; 1.0;
      -1.44 ; 0.0  ;   0.5625  ; 0.625;   0.0; 0.0; 0.0; 1.0;
      (* tab color *)
      -1.568; 0.128;   0.421875; 0.625;   1.0; 1.0; 1.0; 1.0;
      -1.6  ; 0.128;   0.40625 ; 0.625;   1.0; 1.0; 1.0; 1.0;
      -1.6  ; 0.0  ;   0.40625 ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
      -1.568; 0.0  ;   0.421875; 0.75 ;   1.0; 1.0; 1.0; 1.0;
      (* tab cartridge *)
      -1.568; 0.128;   0.4375  ; 0.625;   gold.r; gold.g; gold.b; 1.0;
      -1.6  ; 0.128;   0.421875; 0.625;   gold.r; gold.g; gold.b; 1.0;
      -1.6  ; 0.0  ;   0.421875; 0.75 ;   gold.r; gold.g; gold.b; 1.0;
      -1.568; 0.0  ;   0.4375  ; 0.75 ;   gold.r; gold.g; gold.b; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer ArrayBuffer buffer;
  GL.bufferData ArrayBuffer buffer_data StaticDraw;
  buffer

let make background_filename =
  let white_texture = load_texture "gfx/pixel.png" in
  let background_texture, background_buffer = load_background background_filename in
  let border_buffer = GL.genBuffer () in
  let dot_texture, dot_buffer = load_dot () in
  let dashed_texture = load_texture "gfx/dashed.png" in
  let dashed_buffer = GL.genBuffer () in
  let dashed_elem_buffer = GL.genBuffer () in
  let ui_texture = load_texture "gfx/ui.png" in
  let game_state_buffer = load_game_state_buffer () in
  let cartridge_buffer = load_cartridge_buffer () in
  { white_texture;
    background_texture; background_buffer;
    border_buffer;
    dot_texture; dot_buffer;
    dashed_texture; dashed_buffer; dashed_elem_buffer; dashed_elem_count = 0;
    ui_texture;
    game_state_buffer;
    cartridge_buffer }

let update_dashed_buffers render (origin : Vec2.t) targets =
  let target_count = List.length targets in
  let vertex_data = Array1.create Float32 C_layout ((target_count + 1) * 8) in
  let sub_0 = Array1.sub vertex_data 0 8 in
  [| origin.x; origin.y; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 |]
  |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub_0;
  List.iteri (fun i target ->
      let sub = Array1.sub vertex_data ((i + 1) * 8) 8 in
      let dist = Vec2.(mag (sub origin target)) in
      [| target.x; target.y; dist *. -8.0; 0.0; 0.5; 0.5; 0.5; 1.0 |]
      |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub
    ) targets;
  let elem_count = target_count * 2 in
  let elem_data = Array1.create Int16_unsigned C_layout elem_count in
  for i = 0 to target_count - 1 do
    let offset = i * 2 in
    elem_data.{offset} <- 0;
    elem_data.{offset + 1} <- i + 1
  done;
  render.dashed_elem_count <- elem_count;
  GL.bindBuffer ArrayBuffer render.dashed_buffer;
  GL.bufferData ArrayBuffer vertex_data DynamicDraw;
  GL.bindBuffer ElementArrayBuffer render.dashed_elem_buffer;
  GL.bufferData ElementArrayBuffer elem_data DynamicDraw

let draw_basic_prepare shader texture buffer =
  GL.bindTexture Texture2D texture;
  GL.bindBuffer ArrayBuffer buffer;
  GL.vertexAttribPointer shader.vertex_coords_location 2 Float false 32 0;
  GL.enableVertexAttribArray shader.vertex_coords_location;
  GL.vertexAttribPointer shader.vertex_texture_coords_location 2 Float false 32 8;
  GL.enableVertexAttribArray shader.vertex_texture_coords_location;
  GL.vertexAttribPointer shader.vertex_color_location 4 Float false 32 16;
  GL.enableVertexAttribArray shader.vertex_color_location

let draw_basic_teardown shader =
  GL.disableVertexAttribArray shader.vertex_color_location;
  GL.disableVertexAttribArray shader.vertex_texture_coords_location;
  GL.disableVertexAttribArray shader.vertex_coords_location

let draw_basic shader texture buffer ?elem_buffer mode first count =
  draw_basic_prepare shader texture buffer;
  begin match elem_buffer with
  | None -> GL.drawArrays mode first count
  | Some elem_buffer ->
     GL.bindBuffer ElementArrayBuffer elem_buffer;
     GL.drawElements mode count UnsignedShort (first * 2)
  end;
  draw_basic_teardown shader

let draw_basic_multi shader texture buffer ?elem_buffer mode list =
  draw_basic_prepare shader texture buffer;
  begin match elem_buffer with
  | None -> List.iter (fun (first, count) -> GL.drawArrays mode first count) list
  | Some elem_buffer ->
     GL.bindBuffer ElementArrayBuffer elem_buffer;
     List.iter (fun (first, count) -> GL.drawElements mode count UnsignedShort (first * 2)) list
  end;
  draw_basic_teardown shader

let draw_game_info_sprites basic_shader render (game : Game.t) =
  if Game.in_main_loop_phase game then (
    let y = -1.004 +. float_of_int (Array.length game.players - game.defeated_count) *. 0.136 in
    if Game.in_battle_phase game then
      GL.uniform2f basic_shader.texture_coords_offset_location 0.0625 0.0
    else if Game.in_move_phase game then
      GL.uniform2f basic_shader.texture_coords_offset_location 0.125 0.0;
    GL.uniform2f basic_shader.vertex_coords_offset_location (-1.6) y;
    draw_basic basic_shader render.ui_texture render.game_state_buffer TriangleFan 0 4;
    if game.territory_captured then (
      GL.uniform2f basic_shader.texture_coords_offset_location 0.1875 0.0;
      GL.uniform2f basic_shader.vertex_coords_offset_location (-1.472) y;
      draw_basic basic_shader render.ui_texture render.game_state_buffer TriangleFan 0 4
    );
    GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
    GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
  );
  let y_orig = -1.132 +. float_of_int (Array.length game.players) *. 0.136 in
  let row = ref game.defeated_count in
  for i = 0 to Array.length game.players - 1 do
    let player = game.players.((game.our_player + i) mod Array.length game.players) in
    if not player.defeated then (
      let pc, l =
        if game.current_player = i
        then player.color_suite.brighter, 1.0
        else player.color_suite.darker, 0.8
      in
      let y = y_orig -. float_of_int !row *. 0.136 in
      let visual_cards_count = min 3 (List.length player.cards) in
      let card_tx = 0.0625 *. float_of_int visual_cards_count in
      GL.uniform4f basic_shader.ambient_color_location l l l 1.0;
      GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 y;
      draw_basic_multi basic_shader render.ui_texture render.cartridge_buffer TriangleFan [0, 6; 6, 4];
      GL.uniform2f basic_shader.texture_coords_offset_location card_tx 0.0;
      draw_basic basic_shader render.ui_texture render.cartridge_buffer TriangleFan 10 4;
      GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0;
      GL.uniform4f basic_shader.ambient_color_location pc.r pc.g pc.b 1.0;
      draw_basic basic_shader render.ui_texture render.cartridge_buffer TriangleFan 14 4;
      GL.uniform4f basic_shader.ambient_color_location l l l 1.0;
      draw_basic basic_shader render.ui_texture render.cartridge_buffer TriangleFan 18 4;
      incr row
    )
  done;
  GL.uniform4f basic_shader.ambient_color_location 1.0 1.0 1.0 1.0;
  GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0
