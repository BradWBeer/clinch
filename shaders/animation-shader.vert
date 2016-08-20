#version 130 

const int MAX_BONES = 100;

in vec3 Position; 
in vec2 tc; 
in vec3 Normal; 
in ivec4 BoneIDs;
in vec4 Weights;

out vec2 v_tc;
out vec3 Normal0; 
out vec3 WorldPos0; 

uniform sampler2D ta;
uniform sampler2D td;
uniform sampler2D ts;

uniform mat4 gWVP;
uniform mat4 gWorld;
uniform mat4 gBones[MAX_BONES];

void main()
{ 
    mat4 BoneTransform = gBones[BoneIDs[0]] * Weights[0];
    BoneTransform += gBones[BoneIDs[1]] * Weights[1];
    BoneTransform += gBones[BoneIDs[2]] * Weights[2];
    BoneTransform += gBones[BoneIDs[3]] * Weights[3];

    vec4 PosL = BoneTransform * vec4(Position, 1.0);
    gl_Position = gWVP * PosL;
    v_tc = tc;
    vec4 NormalL = BoneTransform * vec4(Normal, 0.0);
    Normal0 = (gWorld * NormalL).xyz;
    WorldPos0 = (gWorld * PosL).xyz; 
}

