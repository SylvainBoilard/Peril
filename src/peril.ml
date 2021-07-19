open Bigarray

type basic_shader = {
    program: GL.program;
    vertex_coord_location: GL.attrib_location;
    vertex_color_location: GL.attrib_location;
    vertex_texture_coord_location: GL.attrib_location;
    texture_location: GL.uniform_location;
  }

let key_callback window key _(*scancode*) action _(*modifiers*) =
  let open GLFW in
  match key, action with
  | Escape, Press -> setWindowShouldClose window true
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
  let basic_shader = make_basic_shader () in
  let background_texture = Utils.load_texture ("maps/" ^ map.background) in
  let background_buffer =
    let data = [|
         1.6;  1.0;   1.0; 1.0;   1.0; 1.0; 1.0; 1.0;
        -1.6;  1.0;   0.0; 1.0;   1.0; 1.0; 1.0; 1.0;
        -1.6; -1.0;   0.0; 0.0;   1.0; 1.0; 1.0; 1.0;
         1.6; -1.0;   1.0; 0.0;   1.0; 1.0; 1.0; 1.0;
      |] |> Array1.of_array Float32 C_layout
    in
    let buffer = GL.genBuffer () in
    GL.bindBuffer GL.ArrayBuffer buffer;
    GL.bufferData GL.ArrayBuffer data GL.StaticDraw;
    buffer
  in
  let territories_texture = Utils.make_white_pixel_texture () in
  let territories_buffer =
    let territories_data = Array1.create Float32 C_layout (Array.length map.territories * 36) in
    Array.iteri (fun i (t : Map.territory) ->
        let offset = i * 36 in
        Array.iteri (fun j k ->
            let offset = offset + j * 6 in
            let v = t.shape.(k) in
            territories_data.{offset} <- v.x;
            territories_data.{offset + 1} <- v.y;
            territories_data.{offset + 2} <- t.color.r;
            territories_data.{offset + 3} <- t.color.g;
            territories_data.{offset + 4} <- t.color.b;
            territories_data.{offset + 5} <- 1.0
          ) [| 0; 1; 3; 3; 1; 2 |]
      ) map.territories;
    let buffer = GL.genBuffer () in
    GL.bindBuffer GL.ArrayBuffer buffer;
    GL.bufferData GL.ArrayBuffer territories_data GL.DynamicDraw;
    buffer
  in
  let territories_vertex_count = Array.length map.territories * 6 in
  while not (GLFW.windowShouldClose window) do
    GLFW.pollEvents ();

    GL.useProgram basic_shader.program;
    GL.activeTexture 0;
    GL.uniform1f basic_shader.texture_location 0.0;

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

    GL.bindTexture GL.Texture2D territories_texture;
    GL.bindBuffer GL.ArrayBuffer territories_buffer;
    GL.vertexAttribPointer basic_shader.vertex_coord_location 2 GL.Float false 24 0;
    GL.enableVertexAttribArray basic_shader.vertex_coord_location;
    GL.vertexAttribPointer basic_shader.vertex_color_location 4 GL.Float false 24 8;
    GL.enableVertexAttribArray basic_shader.vertex_color_location;
    GL.drawArrays GL.Triangles 0 territories_vertex_count;
    GL.disableVertexAttribArray basic_shader.vertex_color_location;
    GL.disableVertexAttribArray basic_shader.vertex_coord_location;

    GLFW.swapBuffers window
  done;
  GLFW.destroyWindow window;
  GLFW.terminate ()
