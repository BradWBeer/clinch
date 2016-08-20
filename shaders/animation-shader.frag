#version 130

in vec2 v_tc;
in vec3 Normal0; 
in vec3 WorldPos0; 

uniform sampler2D ta;
uniform sampler2D td;
uniform sampler2D ts;

uniform mat4 gWVP;
uniform mat4 gWorld;

out vec4 fragColor;

void main() {
  fragColor = texture2D(ta, v_tc);
}
