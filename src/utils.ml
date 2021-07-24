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

    let find_opt f a =
      let len = length a in
      let rec aux = function
        | i when i = len -> None
        | i when f a.(i) -> Some a.(i)
        | i -> aux (i + 1)
      in
      aux 0
  end

type color = { r: float; g: float; b: float }

let world_of_frame_coords c =
  Vec2.{ x = c.x /. 250.0 -. 1.6; y = c.y /. -250.0 +. 1.0 }

let frame_of_world_coords c =
  Vec2.{ x = (c.x +. 1.6) *. 250.0; y = (c.y -. 1.0) *. -250.0 }

let color_of_hex str =
  Scanf.sscanf str "#%2x%2x%2x" (fun r g b ->
      { r = float_of_int r /. 255.0;
        g = float_of_int g /. 255.0;
        b = float_of_int b /. 255.0 })

let hex_of_color color =
  Printf.sprintf "#%02x%02x%02x"
    (truncate (color.r *. 255.0))
    (truncate (color.g *. 255.0))
    (truncate (color.b *. 255.0))

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
  }

let make_basic_shader () =
  let program = load_program "shaders" "basic" in
  let vertex_coords_location = GL.getAttribLocation program "VertexCoords" in
  let vertex_texture_coords_location = GL.getAttribLocation program "VertexTextureCoords" in
  let vertex_color_location = GL.getAttribLocation program "VertexColor" in
  let texture_location = GL.getUniformLocation program "Texture" in
  { program; vertex_coords_location; vertex_texture_coords_location; vertex_color_location; texture_location }

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
