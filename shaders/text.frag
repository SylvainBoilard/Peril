#version 100

uniform lowp vec3 Color;
uniform sampler2D FaceTexture;
varying mediump vec2 TextureCoord;

void main()
{
    lowp float a = texture2D(FaceTexture, TextureCoord).a;
    gl_FragColor = vec4(Color, a);
}
