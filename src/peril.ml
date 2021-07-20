open Bigarray

type basic_shader = {
    program: GL.program;
    vertex_coord_location: GL.attrib_location;
    vertex_color_location: GL.attrib_location;
    vertex_texture_coord_location: GL.attrib_location;
    texture_location: GL.uniform_location;
  }

type shape_poi = Corner of int | Edge of int * int | NoPOI

let edition_mode = ref false
let selected_territory = ref None
let selected_poi = ref NoPOI

let key_callback window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | Escape, Press -> setWindowShouldClose window true
  | F3, Press -> edition_mode := not !edition_mode
  | _ -> ()

let find_poi_of_shape_at_coord shape coord =
  let len = Array.length shape in
  let rec aux_edges = function
    | i when i + 1 = len ->
       if Vec2.(sqr_mag (sub (lerp shape.(i) shape.(0) 0.5) coord)) <= 0.001
       then Edge (i, 0)
       else NoPOI
    | i ->
       if Vec2.(sqr_mag (sub (lerp shape.(i) shape.(i + 1) 0.5) coord)) <= 0.001
       then Edge (i, i + 1)
       else aux_edges (i + 1)
  in
  let rec aux_corners = function
    | i when i = len -> aux_edges 0
    | i when Vec2.(sqr_mag (sub shape.(i) coord)) <= 0.001 -> Corner i
    | i -> aux_corners (i + 1)
  in
  aux_corners 0

let mouse_button_callback map window button pressed _(*modifiers*) =
  let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
  let cursor_coord = Utils.world_of_frame_coords cursor_pos in
  match button, pressed with
  | 0, true ->
     if !edition_mode && !selected_territory <> None then (
       selected_poi := find_poi_of_shape_at_coord (Option.get !selected_territory : Map.territory).shape cursor_coord;
       if !selected_poi = NoPOI then
         selected_territory := Map.find_territory_at_coord map cursor_coord
     ) else
       selected_territory := Map.find_territory_at_coord map cursor_coord
  | 0, false ->
     if !edition_mode && !selected_territory <> None then (
       begin match !selected_poi with
       | Corner n ->
          (Option.get !selected_territory).shape.(n) <- cursor_coord;
          Map.save_to_xml_file map "maps/Earth.xml"
       | _ -> ()
       end;
       selected_poi := NoPOI
     )
  | _ -> ()

let make_basic_shader () =
  let program = Utils.load_program "shaders" "basic" in
  let vertex_coord_location = GL.getAttribLocation program "VertexCoord" in
  let vertex_color_location = GL.getAttribLocation program "VertexColor" in
  let vertex_texture_coord_location = GL.getAttribLocation program "VertexTextureCoord" in
  let texture_location = GL.getUniformLocation program "Texture" in
  { program; vertex_coord_location; vertex_color_location; vertex_texture_coord_location; texture_location }

let () =
  let map = Map.load_from_xml_file "maps/Earth.xml" in
  GLFW.init ();
  GLFW.windowHint GLFW.ClientApi GLFW.OpenGLESApi;
  GLFW.windowHint GLFW.ContextVersionMajor 2;
  GLFW.windowHint GLFW.ContextVersionMinor 0;
  GLFW.windowHint GLFW.Resizable false;
  let window = GLFW.createWindow 800 500 "Peril" () in
  GLFW.makeContextCurrent (Some window);
  GLFW.setKeyCallback window (Some key_callback) |> ignore;
  GLFW.setMouseButtonCallback window (Some (mouse_button_callback map)) |> ignore;
  let basic_shader = make_basic_shader () in
  let background_texture = Utils.load_texture ("maps/" ^ map.background) in
  let background_buffer =
    let data = [|
         1.6;  1.0;   1.0; 0.0;   1.0; 1.0; 1.0; 1.0;
        -1.6;  1.0;   0.0; 0.0;   1.0; 1.0; 1.0; 1.0;
        -1.6; -1.0;   0.0; 1.0;   1.0; 1.0; 1.0; 1.0;
         1.6; -1.0;   1.0; 1.0;   1.0; 1.0; 1.0; 1.0;
      |] |> Array1.of_array Float32 C_layout
    in
    let buffer = GL.genBuffer () in
    GL.bindBuffer GL.ArrayBuffer buffer;
    GL.bufferData GL.ArrayBuffer data GL.StaticDraw;
    buffer
  in
  let territories_texture = Utils.make_white_pixel_texture () in
  let territories_buffer = GL.genBuffer () in
  let text_ctx = Text.init () in
  let text_font = Text.load_font "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf" in
  let edition_mode_text = Text.make text_ctx text_font "Edition" in
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();
    let cursor_pos = Vec2.of_tuple (GLFW.getCursorPos window) in
    let cursor_coord = Utils.world_of_frame_coords cursor_pos in

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1i basic_shader.texture_location 0;

    GL.bindTexture GL.Texture2D background_texture;
    GL.bindBuffer GL.ArrayBuffer background_buffer;
    GL.vertexAttribPointer basic_shader.vertex_coord_location 2 GL.Float false 32 0;
    GL.enableVertexAttribArray basic_shader.vertex_coord_location;
    GL.vertexAttribPointer basic_shader.vertex_texture_coord_location 2 GL.Float false 32 8;
    GL.enableVertexAttribArray basic_shader.vertex_texture_coord_location;
    GL.vertexAttribPointer basic_shader.vertex_color_location 4 GL.Float false 32 16;
    GL.enableVertexAttribArray basic_shader.vertex_color_location;
    GL.drawArrays GL.TriangleFan 0 4;
    GL.disableVertexAttribArray basic_shader.vertex_color_location;
    GL.disableVertexAttribArray basic_shader.vertex_texture_coord_location;
    GL.disableVertexAttribArray basic_shader.vertex_coord_location;

    if !edition_mode then (
      let territories_data = Array1.create Float32 C_layout (Array.length map.territories * 24) in
      Array.iteri (fun i (t : Map.territory) ->
          let offset = i * 24 in
          Array.iteri (fun j (v : Vec2.t) ->
              let offset = offset + j * 6 in
              territories_data.{offset} <- v.x;
              territories_data.{offset + 1} <- v.y;
              territories_data.{offset + 2} <- 0.0;
              territories_data.{offset + 3} <- 0.0;
              territories_data.{offset + 4} <- 0.0;
              territories_data.{offset + 5} <- 1.0
            ) t.shape
        ) map.territories;
      GL.bindBuffer GL.ArrayBuffer territories_buffer;
      GL.bufferData GL.ArrayBuffer territories_data GL.StreamDraw;
      GL.bindTexture GL.Texture2D territories_texture;
      GL.bindBuffer GL.ArrayBuffer territories_buffer;
      GL.vertexAttribPointer basic_shader.vertex_coord_location 2 GL.Float false 24 0;
      GL.enableVertexAttribArray basic_shader.vertex_coord_location;
      GL.vertexAttribPointer basic_shader.vertex_color_location 4 GL.Float false 24 8;
      GL.enableVertexAttribArray basic_shader.vertex_color_location;
      Array.iteri (fun i _ ->
          GL.drawArrays GL.LineLoop (i * 4) 4
        ) map.territories;
      GL.disableVertexAttribArray basic_shader.vertex_color_location;
      GL.disableVertexAttribArray basic_shader.vertex_coord_location;

      Text.draw text_ctx edition_mode_text { x = 10.0; y = 26.0 } 0.0 0.0 0.0
    );

    begin match Map.find_territory_at_coord map cursor_coord, !selected_territory with
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
