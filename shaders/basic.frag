#version 100

uniform sampler2D Texture;
varying mediump vec2 TextureCoord;
varying lowp vec4 Color;

void main()
{
    gl_FragColor = Color * texture2D(Texture, TextureCoord);
}
