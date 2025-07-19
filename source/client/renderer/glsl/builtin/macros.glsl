#if !defined(myhalf)
#define myhalf float
#define myhalf2 vec2
#define myhalf3 vec3
#define myhalf4 vec4
#endif

#ifdef GL_ES
#define qf_lowp_float lowp float
#define qf_lowp_vec2 lowp vec2
#define qf_lowp_vec3 lowp vec3
#define qf_lowp_vec4 lowp vec4
#else
#define qf_lowp_float float
#define qf_lowp_vec2 vec2
#define qf_lowp_vec3 vec3
#define qf_lowp_vec4 vec4
#endif