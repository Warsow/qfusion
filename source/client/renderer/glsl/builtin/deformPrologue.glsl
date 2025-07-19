#define QF_APPLY_DEFORMVERTS

#if defined(APPLY_AUTOSPRITE) || defined(APPLY_AUTOSPRITE2)
qf_attribute vec4 a_SpritePoint;
#else
#define a_SpritePoint vec4(0.0)
#endif

#if defined(APPLY_AUTOSPRITE2)
qf_attribute vec4 a_SpriteRightUpAxis;
#else
#define a_SpriteRightUpAxis vec4(0.0)
#endif

void QF_DeformVerts(inout vec4 Position, inout vec3 Normal, inout vec2 TexCoord)
{
float t = 0.0;
vec3 dist;
vec3 right, up, forward, newright;

#if defined(WAVE_SIN)