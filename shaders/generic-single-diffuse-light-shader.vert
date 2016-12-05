#version 130

uniform   mat4 P;
uniform   mat4 M;
uniform   mat3 N;
uniform   sampler2D t1;

uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;

in vec3 v;
in vec3 n;
in vec4 c;
in vec2 tc1;

out vec3 _n;
out vec2 _tc1;
out vec4 _c;

out vec3 _ambientLight;
out vec3 _lightDirection;
out vec3 _lightIntensity;

void main() {
  gl_Position = P * M * vec4(v, 1);
  _n = normalize(N * n);
  _tc1 = tc1;
  _ambientLight = ambientLight;            
  _lightDirection = lightDirection;
  _lightIntensity = lightIntensity;
  _c = c;
}
