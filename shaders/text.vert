#version 100

attribute mediump vec2 VertexCoords;
attribute mediump vec2 VertexTextureCoords;
uniform mediump vec2 ViewOffset;
varying mediump vec2 TextureCoords;

const vec2 view_scale = 2.0 / vec2(800.0, 500.0);
const vec2 view_offset = vec2(800.0, -500.0) / 2.0;

void main()
{
    gl_Position = vec4((VertexCoords - ViewOffset) * view_scale, 0.0, 1.0);
    TextureCoords = VertexTextureCoords;
}
