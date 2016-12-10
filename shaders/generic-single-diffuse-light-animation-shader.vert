#version 130


const int MAX_BONES = 50;
const int MAX_WEIGHTS = 4;

uniform   mat4 P;
uniform   mat4 M;
uniform   mat3 N;
uniform   sampler2D t1;

uniform   mat4 bones[MAX_BONES];
uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;

in vec3 v;
in vec3 n;
in vec4 c;
in vec2 tc1;

in ivec4 boneIDs;
in vec4  weights;


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
    
    //boneTransform = bones[boneIDs.x]; 
    boneTransform += bones[boneIDs.x] * weights.x;
    boneTransform += bones[boneIDs.y] * weights.y;
    boneTransform += bones[boneIDs.z] * weights.z;
    boneTransform += bones[boneIDs.w] * weights.w;

    gl_Position = P * M * boneTransform * v4;
    _n = normalize(N * n);
    _tc1 = tc1;
    _ambientLight = ambientLight;            
    _lightDirection = lightDirection;
    _lightIntensity = lightIntensity;
    _c = c;
}
