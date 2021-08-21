#include <ft2build.h>
#include FT_FREETYPE_H
#include FT_STROKER_H
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/bigarray.h>
#include <assert.h>

#define CAMLvoid CAMLunused_start value unit CAMLunused_end

static value Val_cptr(void* p)
{
    assert(((uintptr_t)p & 1) == 0);
    return (value)p | 1;
}

#define Cptr_val(t, v) ((t)((v) & ~1))

static FT_Library ft_library;

static const int ml_to_ft_kerning_mode[] = {
    FT_KERNING_DEFAULT,
    FT_KERNING_UNFITTED,
    FT_KERNING_UNSCALED
};

static const int ml_to_ft_line_join[] = {
    FT_STROKER_LINEJOIN_ROUND,
    FT_STROKER_LINEJOIN_BEVEL,
    FT_STROKER_LINEJOIN_MITER_VARIABLE,
    FT_STROKER_LINEJOIN_MITER_FIXED
};

static const int ml_to_ft_line_cap[] = {
    FT_STROKER_LINECAP_BUTT,
    FT_STROKER_LINECAP_ROUND,
    FT_STROKER_LINECAP_SQUARE
};

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
        caml_failwith("FreeType.Face.create: failure.");
    return Val_cptr(face);
}

CAMLprim value caml_FT_Set_Char_Size(
    value face, value width, value height, value h_res, value v_res)
{
    int ret = FT_Set_Char_Size(
        Cptr_val(FT_Face, face), Int_val(width), Int_val(height),
        Int_val(h_res), Int_val(v_res));

    if (ret != 0)
        caml_failwith("FreeType.Face.setCharSize: failure.");
    return Val_unit;
}

CAMLprim value caml_FT_Get_Char_Index(value face, value code)
{
    return Val_int(FT_Get_Char_Index(Cptr_val(FT_Face, face), Int_val(code)));
}

CAMLprim value caml_FT_Load_Glyph(value ml_face, value index)
{
    FT_Face face = Cptr_val(FT_Face, ml_face);

    if (FT_Load_Glyph(face, Int_val(index), FT_LOAD_DEFAULT) != 0)
        caml_failwith("FreeType.Face.loadGlyph: failure.");
    return Val_unit;
}

CAMLprim value caml_FT_Get_Kerning(
    value face, value left, value right, value mode)
{
    FT_Vector kerning;
    int ret = FT_Get_Kerning(
        Cptr_val(FT_Face, face), Int_val(left), Int_val(right),
        ml_to_ft_kerning_mode[Int_val(mode)], &kerning);

    if (ret != 0)
        caml_failwith("FreeType.Face.getKerning: failure.");
    return Val_int(kerning.x);
}

CAMLprim value caml_FT_Stroker_New(CAMLvoid)
{
    FT_Stroker stroker;
    if (FT_Stroker_New(ft_library, &stroker) != 0)
        caml_failwith("FreeType.Stroker.create: failure.");
    return Val_cptr(stroker);
}

CAMLprim value caml_FT_Stroker_Set(
    value ml_stroker, value radius, value line_cap, value line_join,
    value miter_limit)
{
    FT_Stroker stroker = Cptr_val(FT_Stroker, ml_stroker);
    FT_Stroker_Set(
        stroker, Int_val(radius), ml_to_ft_line_cap[Int_val(line_cap)],
        ml_to_ft_line_join[Int_val(line_join)], Int_val(miter_limit));
    return Val_unit;
}

CAMLprim value caml_FT_Get_Glyph(value ml_face)
{
    FT_Glyph glyph;
    if (FT_Get_Glyph(Cptr_val(FT_Face, ml_face)->glyph, &glyph) != 0)
        caml_failwith("FreeType.Glyph.get: failure.");
    return Val_cptr(glyph);
}

CAMLprim value caml_FT_Done_Glyph(value ml_glyph)
{
    FT_Done_Glyph(Cptr_val(FT_Glyph, ml_glyph));
    return Val_unit;
}

CAMLprim value caml_FT_Glyph_Stroke(
    value ml_glyph, value ml_stroker, value destroy)
{
    FT_Glyph glyph = Cptr_val(FT_Glyph, ml_glyph);
    FT_Stroker stroker = Cptr_val(FT_Stroker, ml_stroker);
    if (FT_Glyph_Stroke(&glyph, stroker, Bool_val(destroy)) != 0)
        caml_failwith("FreeType.Glyph.stroke: failure.");
    return Val_cptr(glyph);
}

CAMLprim value caml_FT_Glyph_To_Bitmap(value ml_glyph, value destroy)
{
    CAMLparam0();
    CAMLlocal1(bitmap);
    FT_Glyph glyph = Cptr_val(FT_Glyph, ml_glyph);
    int ret = FT_Glyph_To_Bitmap(
        &glyph, FT_RENDER_MODE_NORMAL, NULL, Bool_val(destroy));
    if (ret != 0)
        caml_failwith("FreeType.Glyph.toBitmap: failure.");
    FT_BitmapGlyph bitmap_glyph = (FT_BitmapGlyph)glyph;
    bitmap = caml_alloc_small(7, 0);
    Field(bitmap, 0) = Val_int(bitmap_glyph->left);
    Field(bitmap, 1) = Val_int(bitmap_glyph->bitmap.width);
    Field(bitmap, 2) = Val_int(bitmap_glyph->top);
    Field(bitmap, 3) = Val_int(bitmap_glyph->bitmap.rows);
    Field(bitmap, 4) = Val_int(bitmap_glyph->root.advance.x);
    Field(bitmap, 5) = Val_int(bitmap_glyph->root.advance.y);
    Field(bitmap, 6) = Val_unit;
    Store_field(bitmap, 6, caml_ba_alloc_dims(
        CAML_BA_CHAR | CAML_BA_C_LAYOUT, 2, NULL,
        (intnat)bitmap_glyph->bitmap.rows, (intnat)bitmap_glyph->bitmap.width));
    struct caml_ba_array* ba = Caml_ba_array_val(Field(bitmap, 6));
    memcpy(ba->data, bitmap_glyph->bitmap.buffer, caml_ba_byte_size(ba));
    FT_Done_Glyph(glyph);
    CAMLreturn(bitmap);
}
