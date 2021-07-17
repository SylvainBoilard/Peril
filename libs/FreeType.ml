open Bigarray

type face [@@immediate]

type glyph = {
    bitmap_left : int;
    bitmap_width : int;
    bitmap_top : int;
    bitmap_rows : int;
    x_advance : int;
    y_advance : int;
    bitmap : (int, int8_unsigned_elt, c_layout) Array2.t;
  }

external init : unit -> unit = "caml_FT_Init_FreeType"
external newFace : filename:string -> face_index:int -> face
  = "caml_FT_New_Face"
external setCharSize :
  face:face -> width:int -> height:int -> h_res:int -> v_res:int -> unit
  = "caml_FT_Set_Char_Size"
external getCharIndex : face:face -> code:int -> int
  = "caml_FT_Get_Char_Index" [@@noalloc]
external loadGlyph : face:face -> index:int -> glyph = "caml_FT_Load_Glyph"
