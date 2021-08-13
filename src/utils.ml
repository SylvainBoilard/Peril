let () = Random.self_init ()

module List =
  struct
    include List

    let find_next_loop f l =
      let rec aux = function
        | hd :: next :: _ when f hd -> next
        | hd :: [] when f hd -> List.hd l
        | _ :: tl -> aux tl
        | [] -> raise Not_found
      in
      aux l
  end

module Array =
  struct
    include Array

    let for_all_successive_pairs_loop f a =
      let len = length a in
      let rec aux = function
        | i when i + 1 = len -> f a.(i) a.(0)
        | i when f a.(i) a.(i + 1) -> aux (i + 1)
        | _ -> false
      in
      len < 2 || aux 0

    let find_offset f a =
      let len = length a in
      let rec aux = function
        | i when i = len -> raise Not_found
        | i when f a.(i) -> i
        | i -> aux (i + 1)
      in
      aux 0

    let of_rev_list = function
      | [] -> [||]
      | hd :: tl as l ->
         let len = List.length l in
         let a = make len hd in
         let rec aux i = function
           | [] -> a
           | hd :: tl -> unsafe_set a i hd; aux (i - 1) tl
         in
         aux (len - 2) tl

    let shuffle a =
      for i = length a - 1 downto 1 do
        let j = Random.int (i + 1) in
        let tmp = a.(i) in
        a.(i) <- a.(j);
        a.(j) <- tmp
      done
  end

let world_scale = Vec2.{ x = 250.0; y = -250.0 }
let world_offset = Vec2.{ x = 1.6; y = -1.0 }

let world_of_frame_coords coords =
  Vec2.(sub (div coords world_scale) world_offset)

let frame_of_world_coords coords =
  Vec2.(mult (add coords world_offset) world_scale)

let compute_shape_barycenter shape =
  Vec2.scale
    (Array.fold_left Vec2.add Vec2.zero shape)
    (1.0 /. float_of_int (Array.length shape))

let load_program directory name =
  let filename_prefix = Printf.sprintf "%s/%s" directory name in
  let load_shader kind ext =
    let shader = GL.createShader kind in
    begin
      try
        let file = open_in (filename_prefix ^ ext) in
        let shader_src = really_input_string file (in_channel_length file) in
        close_in_noerr file;
        GL.shaderSource shader shader_src;
        GL.compileShader shader;
        Option.iter
          (fun s ->
            Printf.eprintf "=== %s shader compilation log ===\n%!" (name ^ ext);
            Printf.eprintf "%s\n%!" s)
          (GL.getShaderInfoLog shader)
      with Sys_error str -> Printf.eprintf "%s\n%!" str
    end;
    shader
  in
  let vertex_shader = load_shader GL.VertexShader ".vert" in
  let fragment_shader = load_shader GL.FragmentShader ".frag" in
  let program = GL.createProgram () in
  GL.attachShader program vertex_shader;
  GL.attachShader program fragment_shader;
  GL.linkProgram program;
  Option.iter
    (fun s ->
      Printf.eprintf "=== %s program linkage log ===\n%!" name;
      Printf.eprintf "%s\n%!" s)
    (GL.getProgramInfoLog program);
  program

type basic_shader = {
    program: GL.program;
    vertex_coords_location: GL.attrib_location;
    vertex_texture_coords_location: GL.attrib_location;
    vertex_color_location: GL.attrib_location;
    texture_location: GL.uniform_location;
    vertex_coords_offset_location: GL.uniform_location;
    texture_coords_offset_location: GL.uniform_location;
    ambient_color_location: GL.uniform_location;
  }

let load_basic_shader () =
  let program = load_program "shaders" "basic" in
  let vertex_coords_location = GL.getAttribLocation program "VertexCoords" in
  let vertex_texture_coords_location = GL.getAttribLocation program "VertexTextureCoords" in
  let vertex_color_location = GL.getAttribLocation program "VertexColor" in
  let texture_location = GL.getUniformLocation program "Texture" in
  let vertex_coords_offset_location = GL.getUniformLocation program "VertexCoordsOffset" in
  let texture_coords_offset_location = GL.getUniformLocation program "TextureCoordsOffset" in
  let ambient_color_location = GL.getUniformLocation program "AmbientColor" in
  { program; vertex_coords_location; vertex_texture_coords_location; vertex_color_location;
    texture_location; vertex_coords_offset_location; texture_coords_offset_location; ambient_color_location }

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
     GL.drawElements mode count GL.UnsignedShort first
  end;
  draw_basic_teardown shader

let draw_basic_multi shader texture buffer ?elem_buffer mode list =
  draw_basic_prepare shader texture buffer;
  begin match elem_buffer with
  | None -> List.iter (fun (first, count) -> GL.drawArrays mode first count) list
  | Some elem_buffer ->
     GL.bindBuffer GL.ElementArrayBuffer elem_buffer;
     List.iter (fun (first, count) -> GL.drawElements mode count GL.UnsignedShort first) list
  end;
  draw_basic_teardown shader

type pulse_shader = {
    program: GL.program;
    vertex_coords_location: GL.attrib_location;
    vertex_texture_coords_location: GL.attrib_location;
    vertex_coords_offset_location: GL.uniform_location;
    texture_location: GL.uniform_location;
    color_location: GL.uniform_location;
    time_location: GL.uniform_location;
  }

let load_pulse_shader () =
  let program = load_program "shaders" "pulse" in
  let vertex_coords_location = GL.getAttribLocation program "VertexCoords" in
  let vertex_texture_coords_location = GL.getAttribLocation program "VertexTextureCoords" in
  let vertex_coords_offset_location = GL.getUniformLocation program "VertexCoordsOffset" in
  let texture_location = GL.getUniformLocation program "Texture" in
  let color_location = GL.getUniformLocation program "Color" in
  let time_location = GL.getUniformLocation program "Time" in
  { program; vertex_coords_location; vertex_texture_coords_location;
    vertex_coords_offset_location; texture_location; color_location; time_location }

let load_texture filename =
  let open Bigarray in
  let image = ImageLib_unix.openfile filename in
  let pixels = Array3.create Int8_unsigned C_layout image.height image.width 4 in
  begin match image.pixels with
  | Image.RGBA (Pix8 r, Pix8 g, Pix8 b, Pix8 a) ->
     for y = 0 to image.height - 1 do
       for x = 0 to image.width - 1 do
         let p = Array3.slice_left_1 pixels y x in
         p.{0} <- r.{x, y};
         p.{1} <- g.{x, y};
         p.{2} <- b.{x, y};
         p.{3} <- a.{x, y}
       done
     done;
  | _ -> failwith "load_texture: image format unsupported"
  end;
  let texture = GL.genTexture () in
  GL.bindTexture GL.Texture2D texture;
  GL.texParameter GL.Texture2D GL.MinFilter GL.Nearest;
  GL.texParameter GL.Texture2D GL.MagFilter GL.Nearest;
  GL.texImage2D GL.Texture2D 0 GL.RGBA image.width image.height 0 GL.RGBA GL.UnsignedByte;
  GL.texSubImage2D GL.Texture2D 0 0 0 GL.RGBA GL.UnsignedByte (Obj.magic pixels);
  texture
