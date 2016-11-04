#version 330


const int MAX_BONES = 100;
const int MAX_WEIGHTS = 4;

uniform   mat4 P;
uniform   mat4 M;
uniform   mat3 N;
uniform   sampler2D t1;

uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;

in vec3 v;
in vec3 n;
in vec4 c;
in vec2 tc1;

uniform   mat4 bones[MAX_BONES];
in int   boneIDs [MAX_WEIGHTS];
in float weights[MAX_WEIGHTS];


out vec3 _n;
out vec2 _tc1;
out vec4 _c;

out vec3 _ambientLight;
out vec3 _lightDirection;
out vec3 _lightIntensity;

void main() {

    int i;
    vec4 v4 = vec4(v, 1);
    mat4 boneTransform = mat4(0);

    for(i=0; i<MAX_WEIGHTS && weights[i]>0; i++) {
    
	boneTransform += bones[boneIDs[1]] * weights[1];
    }
    
    gl_Position = P * M * boneTransform * v4;
    _n = normalize(N * n);
    _tc1 = tc1;
    _ambientLight = ambientLight;            
    _lightDirection = lightDirection;
    _lightIntensity = lightIntensity;
    _c = c;
}
