// A by-vertex phong shader without specular highlights and coherent light source

// color          -> a vec4 for surface color
// ambientLight   -> vec3 of ambient light color 
// lightDirection -> vec3 for the direction of the light source
// lightIntensity -> vec3 of intensity for the light source


#version 120

uniform   vec3      clr_ambient;
uniform   vec3      clr_diffuse;
uniform   float     mat_opacity;

uniform   vec3      lightDirection;
uniform   vec3      ambientLight;
uniform   vec3      diffuseLight;

varying   vec4      vColor;


void main() {
  
  gl_Position = ftransform();
  
  vec3 diffuse = max(dot(-lightDirection,
		       normalize(gl_NormalMatrix * gl_Normal)),
		   0) * diffuseLight;
  
  vColor = vec4(clr_ambient * ambientLight, mat_opacity) +
    vec4(clr_diffuse * diffuse, mat_opacity);  
}
