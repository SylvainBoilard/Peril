#version 100

attribute mediump vec2 VertexCoord;
attribute mediump vec2 VertexTextureCoord;
attribute lowp vec4 VertexColor;
varying mediump vec2 TextureCoord;
varying lowp vec4 Color;

const vec2 aspect_ratio = vec2(0.625, -1.0);

void main()
{
    gl_Position = vec4(VertexCoord * aspect_ratio, 0.0, 1.0);
    Color = VertexColor;
    TextureCoord = VertexTextureCoord;
}
