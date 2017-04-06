#version 130
// Fragment Shader Source Code

// fragment shader input
in vec3 _colors;

// This returns the fragment's color.
out vec4 fragColor;
void main() {

    // The triangle will just be white for now.
    fragColor = vec4(_colors, 1);
}
