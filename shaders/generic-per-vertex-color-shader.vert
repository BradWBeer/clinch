#version 130
// Vertex Shader Source Code

// A UNIFORM is single value which is passed to all programs in a run.
uniform mat4 P;

// An ATTRIBUTE is an array which gives one value for each vertex.
in vec3 v;
in vec3 colors;
out vec3 _colors;

void main() {
    gl_Position = P * vec4(v, 1);
    _colors = colors;
}
