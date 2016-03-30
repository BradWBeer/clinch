#version 330

uniform mat4 P;
uniform mat4 M;
uniform sampler2D t1;

in vec3 v;
in vec2 tc1;

out vec2 v_tc1;

void main() {
  gl_Position = P * M * vec4(v, 1);
  v_tc1 = vec2(tc1.x, -tc1.y);
}
