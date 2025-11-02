layout (std140) uniform ViewBlock {
    mat4 u_ModelViewMatrix;
    mat4 u_ModelViewProjectionMatrix;
    vec3 u_ViewOrigin;
    mat3 u_ViewAxis;
    ivec4 u_Viewport; // x, y, width, height
    vec2 u_ZRange;
    float u_MirrorSide;
};

layout (std140) uniform ShaderBlock {
    vec3 u_EntityOrigin;
    vec3 u_EntityDist;
    myhalf4 u_EntityColor;
    myhalf4 u_ConstColor;
    myhalf4 u_RGBGenFuncArgs, u_AlphaGenFuncArgs;
    vec4 u_TextureMatrix[2];
    myhalf u_ColorMod;
    float u_ShaderTime;
};

#define TextureMatrix2x3Mul(m2x3,tc) (vec2(dot((m2x3)[0].xy, (tc)), dot((m2x3)[0].zw, (tc))) + (m2x3)[1].xy)

layout (std140) uniform DiffuseLightBlock {
    vec3 u_LightDir;
    myhalf3 u_LightAmbient;
    myhalf3 u_LightDiffuse;
};

layout (std140) uniform DeluxeMapBlock {
    vec4 u_DeluxemapOffset[1]; // s-offset for v_LightmapTexCoord
    myhalf3 u_LightstyleColor[4];
};

uniform myhalf u_LightingIntensity;

layout (std140) uniform BlendMixBlock {
    myhalf2 u_BlendMix;
};

layout (std140) uniform TextureParamsBlock {
    vec4 u_TextureParams;
};

layout (std140) uniform OutlineBlock {
    float u_OutlineHeight;
    float u_OutlineCutOff;
};

#ifdef APPLY_DRAWFLAT
layout (std140) uniform DrawFlatBlock {
    myhalf3 u_WallColor;
    myhalf3 u_FloorColor;
};
#endif

// TODO: Guard by tcgen defines

layout (std140) uniform TexGenBlock {
    mat3 u_ReflectionTexMatrix;
    mat4 u_VectorTexMatrix;
};