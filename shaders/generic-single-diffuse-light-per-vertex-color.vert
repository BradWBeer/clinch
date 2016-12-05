#version 130

uniform   mat4 P;
uniform   mat4 M;
uniform   mat3 N;
uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform  vec3 lightIntensity;

in  vec3 v;
in  vec3 n;
in  vec3 vertexColor;

out vec4 fragmentColor;

vec3 diffuse(vec3 normal, vec3 direction, vec3 intensity) {
  
    float power = max( (dot(direction, normal) * -1), 0);
    return intensity * power; 
}

void main() {
    gl_Position = P * M * vec4(v, 1);

    vec3 _n = normalize(N * n);
    vec3 diffuse = diffuse(normalize(_n), lightDirection, lightIntensity);


    fragmentColor = vec4(vertexColor * (ambientLight + diffuse), 1.0);
}
