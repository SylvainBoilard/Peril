open Utils
open Bigarray

let edition_mode = ref false
let selected_territory = ref (None : Map.territory option)
let selected_poi = ref Map.NoPOI

let key_callback map window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | Escape, Press -> setWindowShouldClose window true
  | F3, Press -> edition_mode := not !edition_mode
  | F5, Press when !edition_mode ->
     Map.validate map;
     Map.save_to_xml_file map "maps/Earth.xml"
  | _ -> ()

let mouse_button_callback (map : Map.t) window button pressed _(*modifiers*) =
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
        Array.iteri (fun i t' ->
            if t' == t then (
              map.territories.(i) <- { t with shape = new_shape };
              selected_territory := Some map.territories.(i);
              selected_poi := Corner (n + 1)
            )
          ) map.territories;
        let cursor_pos = frame_of_world_coords new_shape.(n + 1) in
        GLFW.setCursorPos window cursor_pos.x cursor_pos.y;
        GLFW.setInputMode window GLFW.Cursor GLFW.Hidden
     |  NoPOI -> selected_territory := Map.find_territory_at_coords map cursor_coords
     end
  | 0, true -> selected_territory := Map.find_territory_at_coords map cursor_coords
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

let draw_background shader texture buffer =
  GL.bindTexture GL.Texture2D texture;
  GL.bindBuffer GL.ArrayBuffer buffer;
  GL.vertexAttribPointer shader.vertex_coords_location 2 GL.Float false 32 0;
  GL.enableVertexAttribArray shader.vertex_coords_location;
  GL.vertexAttribPointer shader.vertex_texture_coords_location 2 GL.Float false 32 8;
  GL.enableVertexAttribArray shader.vertex_texture_coords_location;
  GL.vertexAttribPointer shader.vertex_color_location 4 GL.Float false 32 16;
  GL.enableVertexAttribArray shader.vertex_color_location;
  GL.drawArrays GL.TriangleFan 0 4;
  GL.disableVertexAttribArray shader.vertex_color_location;
  GL.disableVertexAttribArray shader.vertex_texture_coords_location;
  GL.disableVertexAttribArray shader.vertex_coords_location

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
  GLFW.setKeyCallback window (Some (key_callback map)) |> ignore;
  GLFW.setMouseButtonCallback window (Some (mouse_button_callback map)) |> ignore;
  GLFW.setCursorPosCallback window (Some cursor_pos_callback) |> ignore;
  let basic_shader = make_basic_shader () in
  let background_texture, background_buffer = load_background ("maps/" ^ map.background) in
  let territories_texture = make_white_pixel_texture () in
  let territories_buffer = GL.genBuffer () in
  let dot_texture = load_texture "gfx/dot.png" in
  let dot_buffer = GL.genBuffer () in
  let text_ctx = Text.init () in
  let text_font = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" in
  let edition_mode_text = Text.make text_ctx text_font "Edition" in
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
    let cursor_coords = world_of_frame_coords cursor_pos in

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;

    draw_background basic_shader background_texture background_buffer;

    if !edition_mode then (
      let vertices_count = Array.fold_left (fun c (t : Map.territory) -> c + Array.length t.shape) 0 map.territories in
      let territories_data = Array1.create Float32 C_layout (vertices_count * 6) in
      Array.fold_left (fun i (t : Map.territory) ->
          Array.fold_left (fun i (v : Vec2.t) ->
              let offset = i * 6 in
              territories_data.{offset} <- v.x;
              territories_data.{offset + 1} <- v.y;
              territories_data.{offset + 2} <- 0.0;
              territories_data.{offset + 3} <- 0.0;
              territories_data.{offset + 4} <- 0.0;
              territories_data.{offset + 5} <- 1.0;
              i + 1
            ) i t.shape
        ) 0 map.territories |> ignore;
      GL.bindBuffer GL.ArrayBuffer territories_buffer;
      GL.bufferData GL.ArrayBuffer territories_data GL.StreamDraw;
      GL.bindTexture GL.Texture2D territories_texture;
      GL.vertexAttribPointer basic_shader.vertex_coords_location 2 GL.Float false 24 0;
      GL.enableVertexAttribArray basic_shader.vertex_coords_location;
      GL.vertexAttribPointer basic_shader.vertex_color_location 4 GL.Float false 24 8;
      GL.enableVertexAttribArray basic_shader.vertex_color_location;
      Array.fold_left (fun i (t : Map.territory) ->
          let len = Array.length t.shape in
          GL.drawArrays GL.LineLoop i len;
          i + len
        ) 0 map.territories |> ignore;
      GL.disableVertexAttribArray basic_shader.vertex_color_location;
      GL.disableVertexAttribArray basic_shader.vertex_coords_location;

      if !selected_territory <> None && !selected_poi = NoPOI then (
        let selected_territory_shape = (Option.get !selected_territory).shape in
        let dot_coords = match Map.find_poi_of_shape_at_coords selected_territory_shape cursor_coords with
          | Corner n -> Some selected_territory_shape.(n)
          | Edge (n, m) -> Some (Vec2.lerp selected_territory_shape.(n) selected_territory_shape.(m) 0.5)
          | NoPOI -> None
        in
        match dot_coords with
        | Some coords ->
           let buffer_data = [|
               coords.x +. 0.016; coords.y +. 0.016;   1.0; 0.0;   1.0; 1.0; 1.0; 1.0;
               coords.x -. 0.016; coords.y +. 0.016;   0.0; 0.0;   1.0; 1.0; 1.0; 1.0;
               coords.x -. 0.016; coords.y -. 0.016;   0.0; 1.0;   1.0; 1.0; 1.0; 1.0;
               coords.x +. 0.016; coords.y -. 0.016;   1.0; 1.0;   1.0; 1.0; 1.0; 1.0;
             |] |> Array1.of_array Float32 C_layout
           in
           GL.enable GL.Blend;
           GL.blendFunc GL.SrcAlpha GL.OneMinusSrcAlpha;
           GL.bindBuffer GL.ArrayBuffer dot_buffer;
           GL.bufferData GL.ArrayBuffer buffer_data GL.StreamDraw;
           GL.bindTexture GL.Texture2D dot_texture;
           GL.vertexAttribPointer basic_shader.vertex_coords_location 2 GL.Float false 32 0;
           GL.enableVertexAttribArray basic_shader.vertex_coords_location;
           GL.vertexAttribPointer basic_shader.vertex_texture_coords_location 2 GL.Float false 32 8;
           GL.enableVertexAttribArray basic_shader.vertex_texture_coords_location;
           GL.vertexAttribPointer basic_shader.vertex_color_location 4 GL.Float false 32 16;
           GL.enableVertexAttribArray basic_shader.vertex_color_location;
           GL.drawArrays GL.TriangleFan 0 4;
           GL.disableVertexAttribArray basic_shader.vertex_color_location;
           GL.disableVertexAttribArray basic_shader.vertex_texture_coords_location;
           GL.disableVertexAttribArray basic_shader.vertex_coords_location;
           GL.disable GL.Blend
        | None -> ()
      );

      Text.draw text_ctx edition_mode_text { x = 10.0; y = 26.0 } 0.0 0.0 0.0
    );

    begin match Map.find_territory_at_coords map cursor_coords, !selected_territory with
    | Some territory, _ | None, Some territory ->
       let name_text = Text.make text_ctx text_font territory.name in
       let x = float_of_int (400 - name_text.width / 2) in
       Text.draw text_ctx name_text Vec2.{ x; y = 470.0} 0.0 0.0 0.0;
       Text.destroy name_text
    | None, None -> ()
    end;

    GLFW.swapBuffers window
  done;
  Text.destroy edition_mode_text;
  GLFW.destroyWindow window;
  GLFW.terminate ()
