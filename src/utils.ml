type color = { r: float; g: float; b: float }

let world_of_frame_coords c =
  Vec2.{ x = c.x /. 250.0 -. 1.6; y = c.y /. -250.0 +. 1.0 }

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

let make_white_pixel_texture () =
  let open Bigarray in
  let pixels = Array3.create Int8_unsigned C_layout 1 1 4 in
  Array3.fill pixels 255;
  let texture = GL.genTexture () in
  GL.bindTexture GL.Texture2D texture;
  GL.texParameter GL.Texture2D GL.MinFilter GL.Nearest;
  GL.texParameter GL.Texture2D GL.MagFilter GL.Nearest;
  GL.texImage2D GL.Texture2D 0 GL.RGBA 1 1 0 GL.RGBA GL.UnsignedByte;
  GL.texSubImage2D GL.Texture2D 0 0 0 GL.RGBA GL.UnsignedByte (Obj.magic pixels);
  texture
