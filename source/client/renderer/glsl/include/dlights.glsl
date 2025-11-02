#if defined(NUM_DLIGHTS)

// TODO: Pass MAX_DLIGHTS properly
layout (std140) uniform DynamicLightBlock {
    myhalf3 u_DlightPosition[32];
    myhalf4 u_DlightDiffuseAndInvRadius[32];
    int u_NumDynamicLights;
};

#include "dlights_overload.glsl"

#define DLIGHTS_SURFACE_NORMAL_IN
#include "dlights_overload.glsl"

#endif
