#version 130
// Vertex Shader Source Code

// A UNIFORM is single value which is passed to all programs in a run.
uniform mat4 P;
uniform mat4 M;
uniform vec4 color;
// An ATTRIBUTE is an array which gives one value for each vertex.
in vec3 v;

void main() {
    gl_Position = P * M * vec4(v, 1); 
}
