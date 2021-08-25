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
    dice_texture: GL.texture;
    dice_buffer: GL.buffer;
    battle_texture: GL.texture;
    battle_buffer: GL.buffer;
    arrow_buffer: GL.buffer;
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

let make background_filename =
  let white_texture = load_texture "gfx/pixel.png" in
  let background_texture, background_buffer = load_background background_filename in
  let border_buffer = GL.genBuffer () in
  let dot_texture, dot_buffer = load_dot () in
  let dashed_texture = load_texture "gfx/dashed.png" in
  let dashed_buffer = GL.genBuffer () in
  let dashed_elem_buffer = GL.genBuffer () in
  let dice_texture, dice_buffer = load_dice () in
  let battle_texture, battle_buffer = load_battle () in
  let arrow_buffer = GL.genBuffer () in
  let ui_background_buffer = make_ui_background () in
  { white_texture;
    background_texture; background_buffer;
    border_buffer;
    dot_texture; dot_buffer;
    dashed_texture; dashed_buffer; dashed_elem_buffer;
    dice_texture; dice_buffer;
    battle_texture; battle_buffer;
    arrow_buffer;
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
