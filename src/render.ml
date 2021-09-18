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
    ui_texture: GL.texture;
    dice_buffer: GL.buffer;
    battle_buffer: GL.buffer;
    arrow_buffer: GL.buffer;
    cartridge_buffer: GL.buffer;
    ui_background_buffer: GL.buffer;
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

let make_dice_buffer () =
  let buffer_data = [|
       0.128;  0.128;   0.125; 0.0 ;   1.0; 1.0; 1.0; 1.0;
      -0.128;  0.128;   0.0  ; 0.0 ;   1.0; 1.0; 1.0; 1.0;
      -0.128; -0.128;   0.0  ; 0.25;   1.0; 1.0; 1.0; 1.0;
       0.128; -0.128;   0.125; 0.25;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
  buffer

let make_battle_buffer () =
  let buffer_data = [|
       0.128;  0.128;   0.125; 0.5 ;   1.0; 1.0; 1.0; 1.0;
      -0.128;  0.128;   0.0  ; 0.5 ;   1.0; 1.0; 1.0; 1.0;
      -0.128; -0.128;   0.0  ; 0.75;   1.0; 1.0; 1.0; 1.0;
       0.128; -0.128;   0.125; 0.75;   1.0; 1.0; 1.0; 1.0;
    |] |> Array1.of_array Float32 C_layout
  in
  let buffer = GL.genBuffer () in
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StaticDraw;
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
  let dice_buffer = make_dice_buffer () in
  let battle_buffer = make_battle_buffer () in
  let arrow_buffer = GL.genBuffer () in
  let cartridge_buffer = GL.genBuffer () in
  let ui_background_buffer = GL.genBuffer () in
  { white_texture;
    background_texture; background_buffer;
    border_buffer;
    dot_texture; dot_buffer;
    dashed_texture; dashed_buffer; dashed_elem_buffer;
    ui_texture;
    dice_buffer; battle_buffer; arrow_buffer; cartridge_buffer;
    ui_background_buffer }

let update_dashed_buffers render territories selected_territory =
  let territory : Map.territory = territories.(selected_territory) in
  let adj_count = Array.length territory.adjacent in
  let center = territory.center in
  let vertex_data = Array1.create Float32 C_layout ((adj_count + 1) * 8) in
  let sub_0 = Array1.sub vertex_data 0 8 in
  [| center.x; center.y; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 |]
  |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub_0;
  Array.iteri (fun i a ->
      let t = territories.(a) in
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
  GL.bindBuffer GL.ArrayBuffer render.dashed_buffer;
  GL.bufferData GL.ArrayBuffer vertex_data GL.DynamicDraw;
  GL.bindBuffer GL.ElementArrayBuffer render.dashed_elem_buffer;
  GL.bufferData GL.ElementArrayBuffer elem_data GL.DynamicDraw

let select_dashed_draws f adjacent =
  let len = Array.length adjacent in
  let rec aux acc = function
    | i when i = len -> acc
    | i when f adjacent.(i) ->
       begin match acc with
       | (first, count) :: tl when (first + count) / 2 = i ->
          aux ((first, count + 2) :: tl) (i + 1)
       | _ -> aux ((i * 2, 2) :: acc) (i + 1)
       end
    | i -> aux acc (i + 1)
  in
  aux [] 0

let draw_basic_prepare shader texture buffer =
  GL.bindTexture GL.Texture2D texture;
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.vertexAttribPointer shader.vertex_coords_location 2 GL.Float false 32 0;
  GL.enableVertexAttribArray shader.vertex_coords_location;
  GL.vertexAttribPointer shader.vertex_texture_coords_location 2 GL.Float false 32 8;
  GL.enableVertexAttribArray shader.vertex_texture_coords_location;
  GL.vertexAttribPointer shader.vertex_color_location 4 GL.Float false 32 16;
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
     GL.bindBuffer GL.ElementArrayBuffer elem_buffer;
     GL.drawElements mode count GL.UnsignedShort (first * 2)
  end;
  draw_basic_teardown shader

let draw_basic_multi shader texture buffer ?elem_buffer mode list =
  draw_basic_prepare shader texture buffer;
  begin match elem_buffer with
  | None -> List.iter (fun (first, count) -> GL.drawArrays mode first count) list
  | Some elem_buffer ->
     GL.bindBuffer GL.ElementArrayBuffer elem_buffer;
     List.iter (fun (first, count) -> GL.drawElements mode count GL.UnsignedShort (first * 2)) list
  end;
  draw_basic_teardown shader

let draw_army_count_selectors
      basic_shader render selector_count max_count cursor_coords
      (color_suite : Color.suite) (base_coords : Vec2.t) (base_texture_coords : Vec2.t) =
  let buffer_data =
    let left, right = base_coords.x -. 0.128, base_coords.x +. 0.128 in
    let bot = base_coords.y -. 0.128 in
    let top = bot +. 0.256 *. float_of_int selector_count in
    [|
      right; top         ;   0.375; 0.75 ;   0.75; 0.75; 0.75; 0.5;
      left ; top         ;   0.25 ; 0.75 ;   0.75; 0.75; 0.75; 0.5;
      right; top -. 0.128;   0.375; 0.875;   0.75; 0.75; 0.75; 0.5;
      left ; top -. 0.128;   0.25 ; 0.875;   0.75; 0.75; 0.75; 0.5;
      right; bot +. 0.128;   0.375; 0.875;   0.75; 0.75; 0.75; 0.5;
      left ; bot +. 0.128;   0.25 ; 0.875;   0.75; 0.75; 0.75; 0.5;
      right; bot         ;   0.375; 1.0  ;   0.75; 0.75; 0.75; 0.5;
      left ; bot         ;   0.25 ; 1.0  ;   0.75; 0.75; 0.75; 0.5;
    |] |> Array1.of_array Float32 C_layout
  in
  GL.bindBuffer GL.ArrayBuffer render.ui_background_buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StreamDraw;
  draw_basic basic_shader render.ui_texture render.ui_background_buffer GL.TriangleStrip 0 8;
  for i = 0 to selector_count - 1 do
    let i_f = float_of_int i in
    let y = base_coords.y +. i_f *. 0.256 in
    let tx = base_texture_coords.x +. i_f *. 0.125 in
    let c =
      if i + 1 > max_count then
        color_suite.desaturated
      else if Vec2.(sqr_mag (sub cursor_coords { base_coords with y })) <= 0.112 *. 0.112 then
        color_suite.brighter
      else
        color_suite.darker
    in
    GL.uniform4f basic_shader.ambient_color_location c.r c.g c.b 1.0;
    GL.uniform2f basic_shader.vertex_coords_offset_location base_coords.x y;
    GL.uniform2f basic_shader.texture_coords_offset_location tx base_texture_coords.y;
    draw_basic basic_shader render.ui_texture render.battle_buffer GL.TriangleFan 0 4
  done

let draw_battle_resolution
      basic_shader render dice_t arrow_t dice_points dice_order
      attacking_armies defending_armies =
  let buffer_data = [|
      0.5;  0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
     -0.5;  0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
     -0.5; -0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
      0.5; -0.5;   0.0; 0.0;   0.5; 0.5; 0.5; 0.5;
    |] |> Array1.of_array Float32 C_layout
  in
  GL.bindBuffer GL.ArrayBuffer render.ui_background_buffer;
  GL.bufferData GL.ArrayBuffer buffer_data GL.StreamDraw;
  draw_basic basic_shader render.white_texture render.ui_background_buffer GL.TriangleFan 0 4;
  let dice_t_io = ease_in_out dice_t in
  let dice_t_i_f = ease_in (min 1.0 (dice_t *. 2.0)) in
  let arrow_t_io = ease_in_out arrow_t in
  let arrow_t_i_f = ease_in (min 1.0 (arrow_t *. 3.0)) in
  let min_armies = min attacking_armies defending_armies in
  for i = 0 to min_armies - 1 do
    let x = Float.lerp (-0.172) 0.140 arrow_t_io in
    let y = float_of_int (i - 1) *. -0.3 in
    let buffer_data =
      Array1.of_array Float32 C_layout @@
        if dice_points.(i) > dice_points.(i + 3) then
          [|   x         ; y +. 0.032;   0.390625; 0.5   ;   1.0; 1.0; 1.0; arrow_t_i_f;
              -0.172     ; y +. 0.032;   0.375   ; 0.5   ;   1.0; 1.0; 1.0; arrow_t_i_f;
              -0.172     ; y -. 0.032;   0.375   ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
               x         ; y -. 0.032;   0.390625; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
               x +. 0.032; y -. 0.032;   0.40625 ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
               x +. 0.032; y +. 0.032;   0.40625 ; 0.5   ;   1.0; 1.0; 1.0; arrow_t_i_f |]
        else
          [| -.x         ; y +. 0.032;   0.390625; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
             -.x -. 0.032; y +. 0.032;   0.40625 ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f;
             -.x -. 0.032; y -. 0.032;   0.40625 ; 0.625 ;   1.0; 1.0; 1.0; arrow_t_i_f;
             -.x         ; y -. 0.032;   0.390625; 0.625 ;   1.0; 1.0; 1.0; arrow_t_i_f;
               0.172     ; y -. 0.032;   0.375   ; 0.625 ;   1.0; 1.0; 1.0; arrow_t_i_f;
               0.172     ; y +. 0.032;   0.375   ; 0.5625;   1.0; 1.0; 1.0; arrow_t_i_f |]
    in
    GL.bindBuffer GL.ArrayBuffer render.arrow_buffer;
    GL.bufferData GL.ArrayBuffer buffer_data StreamDraw;
    draw_basic basic_shader render.ui_texture render.arrow_buffer GL.TriangleFan 0 6
  done;
  for i = 0 to attacking_armies - 1 do
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
    draw_basic basic_shader render.ui_texture render.dice_buffer GL.TriangleFan 0 4
  done;
  for i = 0 to defending_armies - 1 do
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
    GL.uniform2f basic_shader.texture_coords_offset_location (float_of_int dice_points.(i + 3) *. 0.125) 0.25;
    draw_basic basic_shader render.ui_texture render.dice_buffer GL.TriangleFan 0 4
  done

let draw_game_info_sprites basic_shader render (game : Game.t) =
  let y_orig = -1.068 +. float_of_int (Array.length game.players) *. 0.136 in
  if Game.in_main_loop_phase game then (
    let buffer_data =
      Array1.of_array Float32 C_layout @@
        [|
          -1.472; y_orig +. 0.192;   0.5625; 0.625;   1.0; 1.0; 1.0; 1.0;
          -1.6  ; y_orig +. 0.192;   0.5   ; 0.625;   1.0; 1.0; 1.0; 1.0;
          -1.6  ; y_orig +. 0.064;   0.5   ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
          -1.472; y_orig +. 0.064;   0.5625; 0.75 ;   1.0; 1.0; 1.0; 1.0;
        |]
    in
    if Game.in_battle_phase game then
      GL.uniform2f basic_shader.texture_coords_offset_location 0.0625 0.0
    else if Game.in_move_phase game then
      GL.uniform2f basic_shader.texture_coords_offset_location 0.125 0.0;
    GL.bindBuffer GL.ArrayBuffer render.cartridge_buffer;
    GL.bufferData GL.ArrayBuffer buffer_data StreamDraw;
    draw_basic basic_shader render.ui_texture render.cartridge_buffer GL.TriangleFan 0 4;
    if game.territory_captured then (
      GL.uniform2f basic_shader.texture_coords_offset_location 0.1875 0.0;
      GL.uniform2f basic_shader.vertex_coords_offset_location 0.128 0.0;
      draw_basic basic_shader render.ui_texture render.cartridge_buffer GL.TriangleFan 0 4;
      GL.uniform2f basic_shader.vertex_coords_offset_location 0.0 0.0;
    );
    GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0
  );
  let row = ref game.defeated_count in
  for i = 0 to Array.length game.players - 1 do
    let player = game.players.((game.current_player + i) mod Array.length game.players) in
    if not player.defeated then (
      let c = player.color_suite.normal in
      let x = -1.184 in
      let y = y_orig -. float_of_int !row *. 0.136 in
      let visual_cards_count = min 3 (List.length player.cards) in
      let card_tx = 0.5 +. 0.0625 *. float_of_int visual_cards_count in
      let buffer_data =
        Array1.of_array Float32 C_layout @@
          [|(* cartridge *)
            x         ; y +. 0.064;   0.390625; 0.625;   1.0; 1.0; 1.0; 1.0;
            x -. 0.416; y +. 0.064;   0.375   ; 0.625;   1.0; 1.0; 1.0; 1.0;
            x -. 0.416; y -. 0.064;   0.375   ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
            x         ; y -. 0.064;   0.390625; 0.75 ;   1.0; 1.0; 1.0; 1.0;
            x +. 0.032; y -. 0.064;   0.40625 ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
            x +. 0.032; y +. 0.064;   0.40625 ; 0.625;   1.0; 1.0; 1.0; 1.0;
            (* banner icon *)
            x -. 0.08 ; y +. 0.064;   0.5     ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
            x -. 0.208; y +. 0.064;   0.4375  ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
            x -. 0.208; y -. 0.064;   0.4375  ; 0.625;   0.0; 0.0; 0.0; 1.0;
            x -. 0.08 ; y -. 0.064;   0.5     ; 0.625;   0.0; 0.0; 0.0; 1.0;
            (* cards icon *)
            x -. 0.256; y +. 0.064;   card_tx +. 0.0625; 0.5  ;   0.0; 0.0; 0.0; 1.0;
            x -. 0.384; y +. 0.064;   card_tx          ; 0.5  ;   0.0; 0.0; 0.0; 1.0;
            x -. 0.384; y -. 0.064;   card_tx          ; 0.625;   0.0; 0.0; 0.0; 1.0;
            x -. 0.256; y -. 0.064;   card_tx +. 0.0625; 0.625;   0.0; 0.0; 0.0; 1.0;
            (* tab color *)
            -1.568    ; y +. 0.064;   0.421875; 0.625;   c.r; c.g; c.b; 1.0;
            -1.6      ; y +. 0.064;   0.40625 ; 0.625;   c.r; c.g; c.b; 1.0;
            -1.6      ; y -. 0.064;   0.40625 ; 0.75 ;   c.r; c.g; c.b; 1.0;
            -1.568    ; y -. 0.064;   0.421875; 0.75 ;   c.r; c.g; c.b; 1.0;
            (* tab cartridge *)
            -1.568    ; y +. 0.064;   0.4375  ; 0.625;   1.0; 1.0; 1.0; 1.0;
            -1.6      ; y +. 0.064;   0.421875; 0.625;   1.0; 1.0; 1.0; 1.0;
            -1.6      ; y -. 0.064;   0.421875; 0.75 ;   1.0; 1.0; 1.0; 1.0;
            -1.568    ; y -. 0.064;   0.4375  ; 0.75 ;   1.0; 1.0; 1.0; 1.0;
          |]
      in
      GL.bindBuffer GL.ArrayBuffer render.cartridge_buffer;
      GL.bufferData GL.ArrayBuffer buffer_data StreamDraw;
      draw_basic_multi basic_shader render.ui_texture render.cartridge_buffer GL.TriangleFan [0, 6; 6, 4; 10, 4; 14, 4; 18, 4];
      incr row
    )
  done
