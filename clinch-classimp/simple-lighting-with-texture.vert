// A by-vertex phong shader without specular highlights and coherent light source

// textureCoord   -> 2d texture coordinates
// texture01      -> a 2d sampler
// ambientLight   -> vec3 of ambient light color 
// lightDirection -> vec3 for the direction of the light source
// lightIntensity -> vec3 of intensity for the light source


#version 120

uniform   sampler2D tex_file;
attribute vec2      tc0;

uniform   vec3      ambientLight;
uniform   vec3      lightDirection;
uniform   vec3      diffuseLight;

uniform   vec3      clr_ambient;
uniform   vec3      clr_diffuse;
uniform   float     mat_opacity;


varying   vec4      vColor;
varying   vec2      vtc0;


void main() {
  gl_Position = ftransform();

  vec3 diffuse = max(dot(-lightDirection,
		    normalize(gl_NormalMatrix * gl_Normal)),
		0) * diffuseLight * clr_diffuse;

  vec3 ambient = ambientLight * clr_ambient;

  vColor = vec4(diffuse * ambient, mat_opacity);
  vtc0 = tc0;
}
