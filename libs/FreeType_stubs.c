#include <ft2build.h>
#include FT_FREETYPE_H
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <assert.h>

#define CAMLvoid CAMLunused_start value unit CAMLunused_end

#define Val_cptr(p) (assert(((uintptr_t)(p) & 1) == 0), (value)(p) | 1)
#define Cptr_val(t, v) ((t)((v) & ~1))

static FT_Library ft_library;

CAMLprim value caml_FT_Init_FreeType(CAMLvoid)
{
    if (FT_Init_FreeType(&ft_library) != 0)
        caml_failwith("FreeType.init: failure.");
    return Val_unit;
}

CAMLprim value caml_FT_New_Face(value filename, value face_index)
{
    FT_Face face;
    int ret = FT_New_Face(
        ft_library, String_val(filename), Int_val(face_index), &face);

    if (ret != 0)
        caml_failwith("FreeType.newFace: failure.");
    return Val_cptr(face);
}

CAMLprim value caml_FT_Set_Char_Size(
    value face, value width, value height, value h_res, value v_res)
{
    int ret = FT_Set_Char_Size(
        Cptr_val(FT_Face, face), Int_val(width), Int_val(height),
        Int_val(h_res), Int_val(v_res));

    if (ret != 0)
        caml_failwith("FreeType.setCharSize: failure.");
    return Val_unit;
}

CAMLprim value caml_FT_Get_Char_Index(value face, value code)
{
    return Val_int(FT_Get_Char_Index(Cptr_val(FT_Face, face), Int_val(code)));
}

CAMLprim value caml_FT_Load_Glyph(value ml_face, value index)
{
    CAMLparam0();
    CAMLlocal1(glyph);
    FT_Face face = Cptr_val(FT_Face, ml_face);
    FT_GlyphSlot slot = face->glyph;

    if (FT_Load_Glyph(face, Int_val(index), FT_LOAD_RENDER) != 0)
        caml_failwith("FreeType.loadGlyph: failure.");
    glyph = caml_alloc_small(7, 0);
    Field(glyph, 0) = Val_int(slot->bitmap_left);
    Field(glyph, 1) = Val_int(slot->bitmap.width);
    Field(glyph, 2) = Val_int(slot->bitmap_top);
    Field(glyph, 3) = Val_int(slot->bitmap.rows);
    Field(glyph, 4) = Val_int(slot->advance.x);
    Field(glyph, 5) = Val_int(slot->advance.y);
    Field(glyph, 6) = Val_unit;
    Store_field(glyph, 6, caml_ba_alloc_dims(
        CAML_BA_CHAR | CAML_BA_C_LAYOUT, 2, slot->bitmap.buffer,
        (intnat)slot->bitmap.rows, (intnat)slot->bitmap.width));
    CAMLreturn(glyph);
}
