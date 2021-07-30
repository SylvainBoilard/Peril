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
    glyph : FreeType.glyph;
    tex_left : float;
    tex_right : float;
    tex_top : float;
    tex_bottom : float;
  }

type font = {
    face : FreeType.face;
    glyphs : (Uchar.t, glyph_data) Hashtbl.t;
  }

type t = {
    data_buffer : GL.buffer;
    indices_buffer : GL.buffer;
    elements_count : int;
    width : int;
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
  let face = FreeType.newFace filename 0 in
  FreeType.setCharSize face 0 (16 * 64) 72 72;
  { face; glyphs = Hashtbl.create 128 }

let load_glyph ctx font uchar =
  let index = FreeType.getCharIndex font.face (Uchar.to_int uchar) in
  let glyph = FreeType.loadGlyph font.face index in
  let utf8_buf = Buffer.create 6 in
  Buffer.add_utf_8_uchar utf8_buf uchar;
  (*
  Printf.eprintf "Loaded glyph #%d for char '%s' (code %d): %d (+%d), %d (-%d); +%d\n%!"
    index (Buffer.contents utf8_buf) (Uchar.to_int uchar) glyph.bitmap_left glyph.bitmap_width
    glyph.bitmap_top glyph.bitmap_rows (glyph.x_advance / 64);
   *)
  if glyph.x_advance mod 64 <> 0 then
    Printf.eprintf "\027[1;33mGlyph has supplementary fractional advance of %d/64.\027[0m\n%!" (glyph.x_advance mod 64);
  if ctx.glyph_cur_x + glyph.bitmap_width >= tex_size then (
    ctx.glyph_cur_x <- 0;
    ctx.glyph_cur_y <- ctx.glyph_cur_y + ctx.glyph_max_h;
    ctx.glyph_max_h <- 0
  );
  let glyph_data =
    { glyph;
      tex_left = float_of_int ctx.glyph_cur_x /. tex_size_f;
      tex_right = float_of_int (ctx.glyph_cur_x + glyph.bitmap_width) /. tex_size_f;
      tex_top = float_of_int ctx.glyph_cur_y /. tex_size_f;
      tex_bottom = float_of_int (ctx.glyph_cur_y + glyph.bitmap_rows) /. tex_size_f }
  in
  Hashtbl.add font.glyphs uchar glyph_data;
  GL.pixelStorei GL.UnpackAlignment 1;
  GL.bindTexture GL.Texture2D ctx.glyphs_texture;
  GL.texSubImage2D GL.Texture2D 0 ctx.glyph_cur_x ctx.glyph_cur_y GL.Alpha GL.UnsignedByte glyph.bitmap;
  ctx.glyph_cur_x <- ctx.glyph_cur_x + glyph.bitmap_width;
  ctx.glyph_max_h <- max ctx.glyph_max_h glyph.bitmap_rows;
  glyph_data

let make ctx font str =
  let data_buffer = GL.genBuffer () in
  let indices_buffer = GL.genBuffer () in
  let len = Uutf.String.fold_utf_8 (fun i _ _ -> succ i) 0 str in
  let text_data = Bigarray.(Array1.create Float32 C_layout (len * 16)) in
  let indices = Bigarray.(Array1.create Int16_unsigned C_layout (len * 6)) in
  let fill_text_data () =
    let push_uchar x i c =
      let glyph_data =
        try Hashtbl.find font.glyphs c
        with Not_found -> load_glyph ctx font c
      in
      let left = float_of_int (x + glyph_data.glyph.bitmap_left) in
      let right = float_of_int (x + glyph_data.glyph.bitmap_left + glyph_data.glyph.bitmap_width) in
      let top = float_of_int glyph_data.glyph.bitmap_top in
      let bottom = float_of_int (glyph_data.glyph.bitmap_top - glyph_data.glyph.bitmap_rows) in
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
      x + glyph_data.glyph.x_advance / 64
    in
    Uutf.String.fold_utf_8 (fun (x, i) _ -> function
        | `Uchar u -> push_uchar x i u, succ i
        | `Malformed _ -> push_uchar x i Uchar.rep, succ i
      ) (0, 0) str |> fst
  in
  let width = fill_text_data () in
  GL.bindBuffer GL.ArrayBuffer data_buffer;
  GL.bufferData GL.ArrayBuffer text_data GL.StaticDraw;
  GL.bindBuffer GL.ElementArrayBuffer indices_buffer;
  GL.bufferData GL.ElementArrayBuffer indices GL.StaticDraw;
  { data_buffer; indices_buffer; elements_count = len * 6; width }

let destroy text =
  GL.deleteBuffer text.data_buffer;
  GL.deleteBuffer text.indices_buffer

let draw ctx text (position : Vec2.t) r g b =
  GL.enable GL.Blend;
  GL.useProgram ctx.program;
  GL.uniform2f ctx.view_offset_location (400.0 -. position.x) (position.y -. 250.0);
  GL.uniform3f ctx.color_location r g b;
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
