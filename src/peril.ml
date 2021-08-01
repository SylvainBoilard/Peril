open Utils
open Bigarray

let edition_mode = ref false
let selected_territory = ref (None : Map.territory option)
let selected_poi = ref Map.NoPOI
let animation_time = ref 0.0

let update_dashed_buffers (map : Map.t) (territory : Map.territory) vertex_buffer elem_buffer =
  let adj_count = List.length territory.adjacent in
  let center = territory.center in
  let vertex_data = Array1.create Float32 C_layout ((adj_count + 1) * 8) in
  let sub_0 = Array1.sub vertex_data 0 8 in
  [| center.x; center.y; 0.0; 0.0; 1.0; 1.0; 1.0; 1.0 |]
  |> Array1.of_array Float32 C_layout |> Fun.flip Array1.blit sub_0;
  List.iteri (fun i id ->
      let t =
        Array.find_sorted (fun (t : Map.territory) ->
            String.compare id t.id
          ) map.territories_by_id
      in
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
  if Option.is_some territory then
    update_dashed_buffers map (Option.get territory) dashed_vertex_buffer dashed_elem_buffer;
  selected_territory := territory

let key_callback map window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | Escape, Press -> setWindowShouldClose window true
  | F3, Press -> edition_mode := not !edition_mode
  | F5, Press when !edition_mode ->
     Map.validate map;
     Map.save_to_xml_file map "maps/Earth.xml"
  | _ -> ()

let mouse_button_callback
      (map : Map.t) dashed_vertex_buffer dashed_elems_buffer
      window button pressed _(*modifiers*) =
  let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
  let cursor_coords = world_of_frame_coords cursor_pos in
  match button, pressed with
  | 0, true when !edition_mode && Option.is_some !selected_territory ->
     let t = Option.get !selected_territory in
     selected_poi := Map.find_poi_of_shape_at_coords t.shape cursor_coords;
     begin match !selected_poi with
     | Corner n ->
        let cursor_pos = frame_of_world_coords t.shape.(n) in
        GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
        GLFW.setInputMode window GLFW.Cursor GLFW.Hidden
     | Edge (n, m) ->
        let new_shape =
          Array.init (Array.length t.shape + 1) (fun i ->
              if i <= n
              then t.shape.(i)
              else if i = n + 1
              then Vec2.lerp t.shape.(n) t.shape.(m) 0.5
              else t.shape.(i - 1)
            )
        in
        let new_center = compute_shape_barycenter new_shape in
        Array.iteri (fun i t' ->
            if t' == t then (
              map.territories.(i) <- { t with shape = new_shape; center = new_center };
              selected_territory := Some map.territories.(i);
              selected_poi := Corner (n + 1)
            )
          ) map.territories;
        let cursor_pos = frame_of_world_coords new_shape.(n + 1) in
        GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
        GLFW.setInputMode window GLFW.Cursor GLFW.Hidden
     | NoPOI ->
        let clicked_territory = Map.find_territory_at_coords map cursor_coords in
        update_selected_territory clicked_territory map dashed_vertex_buffer dashed_elems_buffer
     end
  | 0, true ->
     let clicked_territory = Map.find_territory_at_coords map cursor_coords in
     update_selected_territory clicked_territory map dashed_vertex_buffer dashed_elems_buffer
  | 0, false ->
     selected_poi := NoPOI;
     GLFW.setInputMode window GLFW.Cursor GLFW.Normal
  | _ -> ()

let cursor_pos_callback _(*window*) x y =
  if !edition_mode then (
    let cursor_coords = world_of_frame_coords { x; y } in
    match !selected_territory, !selected_poi with
    | Some t, Corner n -> t.shape.(n) <- cursor_coords
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
  let border_texture = load_texture "gfx/pixel.png" in
  let border_buffer = GL.genBuffer () in
  let dot_texture, dot_buffer = load_dot () in
  let pulse_shader, pulse_texture, pulse_buffer = load_pulse () in
  let dashed_texture = load_texture "gfx/dashed.png" in
  let dashed_buffer = GL.genBuffer () in
  let dashed_elem_buffer = GL.genBuffer () in
  let text_ctx = Text.init () in
  let text_font = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" in
  let edition_mode_text = Text.make text_ctx text_font "Edition" Regular in
  GLFW.setKeyCallback window (Some (key_callback map)) |> ignore;
  GLFW.setMouseButtonCallback window (Some (mouse_button_callback map dashed_buffer dashed_elem_buffer)) |> ignore;
  GLFW.setCursorPosCallback window (Some cursor_pos_callback) |> ignore;
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
    let cursor_coords = world_of_frame_coords cursor_pos in

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;

    draw_basic basic_shader background_texture background_buffer GL.TriangleFan 0 4;

    if not !edition_mode then (
      match !selected_territory with
      | Some territory ->
         GL.uniform2f basic_shader.texture_coords_offset_location !animation_time 0.0;
         GL.enable GL.Blend;
         GL.lineWidth 3.0;
         draw_basic
           basic_shader dashed_texture dashed_buffer ~elem_buffer:dashed_elem_buffer
           GL.Lines 0 (List.length territory.adjacent * 2);
         GL.lineWidth 1.0;
         GL.disable GL.Blend;
         GL.uniform2f basic_shader.texture_coords_offset_location 0.0 0.0
      | None -> ()
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
      draw_basic_multi basic_shader border_texture border_buffer GL.LineLoop border_draws;

      if !selected_territory <> None && !selected_poi = NoPOI then (
        let selected_territory_shape = (Option.get !selected_territory).shape in
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
      );

      Text.draw text_ctx edition_mode_text { x = 10.0; y = 26.0 } Color.(of_name Black)
    );

    begin match Map.find_territory_at_coords map cursor_coords, !selected_territory with
    | Some territory, _ | None, Some territory ->
       let name_text = Text.make text_ctx text_font territory.name Regular in
       let x = float_of_int (400 - name_text.width / 2) in
       Text.draw text_ctx name_text Vec2.{ x; y = 470.0 } Color.(of_name Black);
       Text.destroy name_text
    | None, None -> ()
    end;

    begin match !selected_territory with
    | Some territory ->
       let coords = territory.center in
       GL.useProgram pulse_shader.program;
       GL.activeTexture 0;
       GL.bindTexture GL.Texture2D pulse_texture;
       GL.uniform2f pulse_shader.vertex_coords_offset_location coords.x coords.y;
       GL.uniform1i pulse_shader.texture_location 0;
       GL.uniform4f pulse_shader.color_location 1.0 1.0 1.0 0.5;
       GL.uniform1f pulse_shader.time_location !animation_time;
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

    animation_time := !animation_time +. 0.008;
    if !animation_time > 1.0 then animation_time := 0.0;

    GLFW.swapBuffers window
  done;
  Text.destroy edition_mode_text;
  GLFW.destroyWindow window;
  GLFW.terminate ()
