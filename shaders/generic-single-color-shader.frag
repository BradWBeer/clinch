#version 130
// Fragment Shader Source Code
uniform vec4 color;

// This returns the fragment's color.
out vec4 fragColor;

void main() {
    
    // The triangle will just be white for now.
    fragColor = color;
}
