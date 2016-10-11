#version 330

uniform sampler2D t1;

in vec2 _tc1;
in vec3 _n;
in vec4 _c;

in vec3 _ambientLight;
in vec3 _lightDirection;
in vec3 _lightIntensity;

out vec4 fragColor;

vec3 diffuse(vec3 normal, vec3 direction, vec3 intensity) {
  
  float power = max( (dot(direction, normal) * -1), 0);
  return intensity * power; 
}

void main() {

  vec4 texColor = texture2D(t1, _tc1);   
  vec3 diffuse = diffuse(normalize(_n), _lightDirection, _lightIntensity);
  fragColor = texColor * _c * vec4(_ambientLight + diffuse, 1.0);
}
