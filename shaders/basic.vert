#version 100

attribute mediump vec2 VertexCoords;
attribute mediump vec2 VertexTextureCoords;
attribute lowp vec4 VertexColor;
uniform mediump vec2 TextureCoordsOffset;
varying mediump vec2 TextureCoords;
varying lowp vec4 Color;

const vec2 aspect_ratio = vec2(0.625, 1.0);

void main()
{
    gl_Position = vec4(VertexCoords * aspect_ratio, 0.0, 1.0);
    Color = VertexColor;
    TextureCoords = VertexTextureCoords + TextureCoordsOffset;
}
