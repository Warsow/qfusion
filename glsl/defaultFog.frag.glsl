#include "include/common.glsl"
#include "include/uniforms.glsl"
#include "include/fog.glsl"

qf_varying vec2 v_FogCoord;

void main(void)
{
	float fogDensity = FogDensity(v_FogCoord);
	qf_FragColor = sRGBColor(vec4(LinearColor(u_FogColor), fogDensity));
}
