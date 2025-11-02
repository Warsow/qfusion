layout (std140) uniform FogBlock {
    vec4 u_FogEyePlane, u_FogPlane;
    myhalf3 u_FogColor;
    vec2 u_FogScaleAndEyeDist;
};

#define FOG_TEXCOORD_STEP 1.0/256.0

#define FogDensity(coord) sqrt(clamp((coord)[0],0.0,1.0))*step(FOG_TEXCOORD_STEP,(coord)[1])

#define FOG_GEN_OUTPUT_COLOR
#include "fog_overload.glsl"

#undef FOG_GEN_OUTPUT_COLOR
#define FOG_GEN_OUTPUT_TEXCOORDS
#include "fog_overload.glsl"
