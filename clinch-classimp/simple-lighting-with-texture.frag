#version 120

uniform   sampler2D tex_file;
varying   vec4      vColor;
varying   vec2      vtc0;


void main() {

  gl_FragColor = texture2D(tex_file, vtc0) * vColor;

}
