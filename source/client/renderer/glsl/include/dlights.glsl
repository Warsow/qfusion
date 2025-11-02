#if defined(NUM_DLIGHTS)

layout (std140) uniform DynamicLightBlock {
    myhalf3 u_DlightPosition[MAX_DLIGHTS];
    myhalf4 u_DlightDiffuseAndInvRadius[MAX_DLIGHTS];
    int u_NumDynamicLights;
};

#include "dlights_overload.glsl"

#define DLIGHTS_SURFACE_NORMAL_IN
#include "dlights_overload.glsl"

#endif
