type context = {
    program : GL.program;
    vertex_coords_location : GL.attrib_location;
    vertex_texture_coords_location : GL.attrib_location;
    view_offset_location : GL.uniform_location;
    face_texture_location : GL.uniform_location;
    color_location : GL.uniform_location;
    glyphs_texture : GL.texture;
    mutable glyph_cur_x : int;
    mutable glyph_cur_y : int;
    mutable glyph_max_h : int;
  }

type glyph_data = {
    bitmap : FreeType.Glyph.bitmap;
    tex_left : float;
    tex_right : float;
    tex_top : float;
    tex_bottom : float;
  }

type glyph_kind = Regular | Outline

external int_of_glyph_kind : glyph_kind -> int = "%identity"

let string_of_glyph_kind = function
  | Regular -> "glyph"
  | Outline -> "outline"

let glyph_key index kind size =
  assert (index < 1 lsl 21);
  assert (int_of_glyph_kind kind < 1 lsl 1);
  assert (size < 1 lsl (Sys.int_size - 22));
  index
  lor int_of_glyph_kind kind lsl 21
  lor size lsl 22

type font = {
    face : FreeType.Face.t;
    stroker : FreeType.Stroker.t;
    glyphs : (int, glyph_data) Hashtbl.t;
  }

type t = {
    data_buffer : GL.buffer;
    indices_buffer : GL.buffer;
    mutable elements_count : int;
    mutable width : int;
  }

let tex_size = 1024
let tex_size_f = float_of_int tex_size

let init () =
  let program = Utils.load_program "shaders" "text" in
  let glyphs_texture = GL.genTexture () in
  GL.bindTexture GL.Texture2D glyphs_texture;
  GL.texImage2D GL.Texture2D 0 GL.Alpha tex_size tex_size 0 GL.Alpha GL.UnsignedByte;
  GL.texParameter GL.Texture2D GL.MinFilter GL.Nearest;
  GL.texParameter GL.Texture2D GL.MagFilter GL.Nearest;
  FreeType.init ();
  { program;
    vertex_coords_location = GL.getAttribLocation program "VertexCoords";
    vertex_texture_coords_location = GL.getAttribLocation program "VertexTextureCoords";
    view_offset_location = GL.getUniformLocation program "ViewOffset";
    face_texture_location = GL.getUniformLocation program "FaceTexture";
    color_location = GL.getUniformLocation program "Color";
    glyphs_texture; glyph_cur_x = 0; glyph_cur_y = 0; glyph_max_h = 0 }

let load_font filename =
  let face = FreeType.Face.create filename 0 in
  let stroker = FreeType.Stroker.create () in
  FreeType.Stroker.(set stroker (1 lsl 6) Round Round 0);
  { face; stroker; glyphs = Hashtbl.create 128 }

let load_glyph ctx font index kind size =
  FreeType.Face.loadGlyph font.face index;
  let bitmap =
    let glyph = match kind with
      | Regular -> FreeType.Glyph.get font.face
      | Outline -> FreeType.Glyph.(stroke (get font.face) font.stroker true)
    in
    FreeType.Glyph.toBitmap glyph true
  in
  (*
  Printf.eprintf "Loaded %dpx %s #%d: %d (+%d), %d (-%d); +%d\n%!"
    size (string_of_glyph_kind kind) index
    bitmap.left bitmap.width bitmap.top bitmap.rows (bitmap.x_advance asr 16);
   *)
  if bitmap.x_advance land 0xffff <> 0 then
    Printf.eprintf "\027[1;33m%dpx glyph #%d has supplementary fractional advance of 0x0.%04x.\027[0m\n%!"
      size index (bitmap.x_advance land 0xffff);
  if ctx.glyph_cur_x + bitmap.width > tex_size then (
    ctx.glyph_cur_x <- 0;
    ctx.glyph_cur_y <- ctx.glyph_cur_y + ctx.glyph_max_h;
    ctx.glyph_max_h <- 0;
  );
  if ctx.glyph_cur_y + bitmap.rows > tex_size then
    Printf.eprintf "\027[1;31mNo room left for %dpx %s #%d in glyphs texture!\027[0m\n%!"
      size (string_of_glyph_kind kind) index;
  let glyph_data =
    { bitmap;
      tex_left = float_of_int ctx.glyph_cur_x /. tex_size_f;
      tex_right = float_of_int (ctx.glyph_cur_x + bitmap.width) /. tex_size_f;
      tex_top = float_of_int ctx.glyph_cur_y /. tex_size_f;
      tex_bottom = float_of_int (ctx.glyph_cur_y + bitmap.rows) /. tex_size_f }
  in
  Hashtbl.add font.glyphs (glyph_key index kind size) glyph_data;
  GL.pixelStorei GL.UnpackAlignment 1;
  GL.bindTexture GL.Texture2D ctx.glyphs_texture;
  GL.texSubImage2D GL.Texture2D 0 ctx.glyph_cur_x ctx.glyph_cur_y GL.Alpha GL.UnsignedByte bitmap.data;
  ctx.glyph_cur_x <- ctx.glyph_cur_x + bitmap.width;
  ctx.glyph_max_h <- max ctx.glyph_max_h bitmap.rows;
  glyph_data

let create () =
  { data_buffer = GL.genBuffer ();
    indices_buffer = GL.genBuffer ();
    elements_count = 0; width = 0 }

let update ctx text font str glyph_kind size ?(base_kerning=0) usage =
  FreeType.Face.setCharSize font.face 0 (size lsl 6) 72 72;
  let str_len = Uutf.String.fold_utf_8 (fun i _ _ -> succ i) 0 str in
  let text_data = Bigarray.(Array1.create Float32 C_layout (str_len * 16)) in
  let indices = Bigarray.(Array1.create Int16_unsigned C_layout (str_len * 6)) in
  let push_glyph x i p c =
    let glyph_data =
      try Hashtbl.find font.glyphs (glyph_key c glyph_kind size)
      with Not_found -> load_glyph ctx font c glyph_kind size
    in
    let kerning = base_kerning + FreeType.Face.getKerning font.face p c Default asr 6 in
    let left = float_of_int (x + kerning + glyph_data.bitmap.left) in
    let right = float_of_int (x + kerning + glyph_data.bitmap.left + glyph_data.bitmap.width) in
    let top = float_of_int glyph_data.bitmap.top in
    let bottom = float_of_int (glyph_data.bitmap.top - glyph_data.bitmap.rows) in
    let j = i * 16 in
    text_data.{j} <- right;
    text_data.{j + 1} <- bottom;
    text_data.{j + 2} <- glyph_data.tex_right;
    text_data.{j + 3} <- glyph_data.tex_bottom;
    text_data.{j + 4} <- right;
    text_data.{j + 5} <- top;
    text_data.{j + 6} <- glyph_data.tex_right;
    text_data.{j + 7} <- glyph_data.tex_top;
    text_data.{j + 8} <- left;
    text_data.{j + 9} <- bottom;
    text_data.{j + 10} <- glyph_data.tex_left;
    text_data.{j + 11} <- glyph_data.tex_bottom;
    text_data.{j + 12} <- left;
    text_data.{j + 13} <- top;
    text_data.{j + 14} <- glyph_data.tex_left;
    text_data.{j + 15} <- glyph_data.tex_top;
    let k = i * 6 in
    let l = i * 4 in
    indices.{k} <- l;
    indices.{k + 1} <- l + 1;
    indices.{k + 2} <- l + 2;
    indices.{k + 3} <- l + 2;
    indices.{k + 4} <- l + 1;
    indices.{k + 5} <- l + 3;
    x + kerning + glyph_data.bitmap.x_advance asr 16
  in
  let decoder = Uutf.decoder (`String str) in
  let rec loop x i p = match Uutf.decode decoder with
    | `Await -> assert false
    | `End -> x
    | `Malformed _ ->
       let c = FreeType.Face.getCharIndex font.face Uchar.(to_int rep) in
       loop (push_glyph x i p c) (i + 1) c
    | `Uchar u ->
       let c = FreeType.Face.getCharIndex font.face (Uchar.to_int u) in
       loop (push_glyph x i p c) (i + 1) c
  in
  text.width <- loop ~-base_kerning 0 0;
  text.elements_count <- str_len * 6;
  GL.bindBuffer GL.ArrayBuffer text.data_buffer;
  GL.bufferData GL.ArrayBuffer text_data usage;
  GL.bindBuffer GL.ElementArrayBuffer text.indices_buffer;
  GL.bufferData GL.ElementArrayBuffer indices usage

let destroy text =
  GL.deleteBuffer text.data_buffer;
  GL.deleteBuffer text.indices_buffer

let draw ctx text (position : Vec2.t) (color : Color.rgba) =
  GL.enable GL.Blend;
  GL.useProgram ctx.program;
  GL.uniform2f ctx.view_offset_location (400.0 -. position.x) (position.y -. 250.0);
  GL.uniform4f ctx.color_location color.r color.g color.b color.a;
  GL.activeTexture 0;
  GL.uniform1i ctx.face_texture_location 0;
  GL.bindTexture GL.Texture2D ctx.glyphs_texture;
  GL.bindBuffer GL.ArrayBuffer text.data_buffer;
  GL.vertexAttribPointer ctx.vertex_coords_location 2 GL.Float false 16 0;
  GL.enableVertexAttribArray ctx.vertex_coords_location;
  GL.vertexAttribPointer ctx.vertex_texture_coords_location 2 GL.Float false 16 8;
  GL.enableVertexAttribArray ctx.vertex_texture_coords_location;
  GL.bindBuffer GL.ElementArrayBuffer text.indices_buffer;
  GL.drawElements GL.Triangles text.elements_count GL.UnsignedShort 0;
  GL.disableVertexAttribArray ctx.vertex_texture_coords_location;
  GL.disableVertexAttribArray ctx.vertex_coords_location;
  GL.disable GL.Blend
