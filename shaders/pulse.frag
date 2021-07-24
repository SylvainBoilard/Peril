#version 100

uniform sampler2D Texture;
uniform lowp vec4 Color;
uniform mediump float Time;
varying mediump vec2 TextureCoords;

void main()
{
    lowp vec4 t = texture2D(Texture, TextureCoords);
    mediump float d = t.r - Time;
    gl_FragColor = vec4(Color.rgb, Color.a * t.a * exp2(d * d * -96.0));
}
