#version 100

attribute mediump vec2 VertexCoords;
attribute mediump vec2 VertexTextureCoords;
varying mediump vec2 TextureCoords;

const vec2 aspect_ratio = vec2(0.625, 1.0);

void main()
{
    gl_Position = vec4(VertexCoords * aspect_ratio, 0.0, 1.0);
    TextureCoords = VertexTextureCoords;
}
