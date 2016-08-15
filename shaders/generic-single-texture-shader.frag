#version 130

uniform sampler2D t1;

in vec2 v_tc1;

out vec4 fragColor;

void main() {
  fragColor = texture2D(t1, v_tc1);
}
