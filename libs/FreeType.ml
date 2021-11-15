open Bigarray

external init : unit -> unit = "caml_FT_Init_FreeType"

module Face =
  struct
    type t [@@immediate]

    type kerning_mode = Default | Unfitted | Unscaled

    external create : filename:string -> face_index:int -> t
      = "caml_FT_New_Face"
    external setCharSize :
      face:t -> width:int -> height:int -> h_res:int -> v_res:int -> unit
      = "caml_FT_Set_Char_Size"
    external getCharIndex : face:t -> code:int -> int
      = "caml_FT_Get_Char_Index" [@@noalloc]
    external loadGlyph : face:t -> index:int -> unit = "caml_FT_Load_Glyph"
    external getKerning :
      face:t -> left:int -> right:int -> mode:kerning_mode -> int
      = "caml_FT_Get_Kerning"
    external getScaledHeight : face:t -> int = "caml_FT_Get_Scaled_Height"
  end

module Stroker =
  struct
    type t [@@immediate]

    type line_join = Round | Bevel | MiterVariable | MiterFixed
    type line_cap = Butt | Round | Square

    external create : unit -> t = "caml_FT_Stroker_New"
    external set :
      stroker:t -> radius:int -> line_cap:line_cap -> line_join:line_join
      -> miter_limit:int -> unit
      = "caml_FT_Stroker_Set" [@@noalloc]
  end

module Glyph =
  struct
    type t [@@immediate]

    type bitmap = {
        left : int;
        width : int;
        top : int;
        rows : int;
        x_advance : int;
        y_advance : int;
        data : (int, int8_unsigned_elt, c_layout) Array2.t;
      }

    external get : face:Face.t -> t = "caml_FT_Get_Glyph"
    external destroy : glyph:t -> unit = "caml_FT_Done_Glyph" [@@noalloc]
    external stroke : glyph:t -> stroker:Stroker.t -> destroy:bool -> t
      = "caml_FT_Glyph_Stroke"
    external toBitmap : glyph:t -> destroy:bool -> bitmap
      = "caml_FT_Glyph_To_Bitmap"
  end
