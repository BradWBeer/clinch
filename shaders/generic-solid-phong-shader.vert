#version 120

uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;
uniform   vec4 color;
varying   vec4 fragmentColor;

void main() {

  gl_Position = gl_ProjectionMatrix * gl_ModelViewMatrix * gl_Vertex;

  float power = max(dot(lightDirection,
			normalize(gl_NormalMatrix * gl_Normal)),
		    0);
  fragmentColor = vec4(color.rgb * (ambientLight + (lightIntensity * power)), color.a);
}
