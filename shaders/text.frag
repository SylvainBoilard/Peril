#version 100

uniform lowp vec4 Color;
uniform sampler2D FaceTexture;
varying mediump vec2 TextureCoords;

void main()
{
    lowp float a = texture2D(FaceTexture, TextureCoords).a;
    gl_FragColor = vec4(Color.rgb, Color.a * a);
}
