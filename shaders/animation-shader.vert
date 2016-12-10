#version 130

const int MAX_BONES = 100;

uniform   mat4 P;
uniform   mat4 M;
uniform   sampler2D t1;

uniform   vec3 ambientLight;
uniform   vec3 lightDirection;
uniform   vec3 lightIntensity;

in vec3 v;
in vec3 n;
in vec4 c;
in vec2 tc1;

out vec3 _ambientLight;
out vec3 _lightDirection;
out vec3 _lightIntensity;

out vec3 _n;
out vec2 _tc1;
out vec4 _c;

// Animation specific input.
uniform sampler2D t1;
uniform mat4 gBones[MAX_BONES];
in ivec4 BoneIDs;
in vec4 Weights;

void main() {
  mat4 BoneTransform = gBones[BoneIDs[0]] * Weights[0];
  BoneTransform += gBones[BoneIDs[1]] * Weights[1];
  BoneTransform += gBones[BoneIDs[2]] * Weights[2];
  BoneTransform += gBones[BoneIDs[3]] * Weights[3];

  mat4 transform = P * M;
  _n = (worldTransform * BoneTransform * vec4(n, 0.0)).xyz;
  
  gl_Position = transform * vec4(v, 1);
  _n = normalize(N * n);
  _tc1 = tc1;
  _ambientLight = ambientLight;            
  _lightDirection = lightDirection;
  _lightIntensity = lightIntensity;
  _c = c;
}


void main()
{ 
    mat4 BoneTransform = gBones[BoneIDs[0]] * Weights[0];
    BoneTransform += gBones[BoneIDs[1]] * Weights[1];
    BoneTransform += gBones[BoneIDs[2]] * Weights[2];
    BoneTransform += gBones[BoneIDs[3]] * Weights[3];

    mat4 worldTransform = P*M;

    v_tc1 = vec2(tc1.x, -tc1.y);

    vec4 NormalL = BoneTransform * vec4(n, 0.0);
    v_n = (worldTransform * NormalL).xyz;
    v_p = (worldTransform * p).xyz; 
}

