#ifdef VERTEX_SHADER
out myhalf4 qf_FrontColor;
#define qf_varying out
#define qf_flat_varying flat out
#define qf_attribute in
#endif

#ifdef FRAGMENT_SHADER
in myhalf4 qf_FrontColor;
out myhalf4 qf_FragColor;
out myhalf4 qf_BrightColor;
#define qf_varying in
#define qf_flat_varying flat in
#endif

#define qf_texture texture
#define qf_textureCube texture
#define qf_textureLod textureLod
#define qf_textureArray texture
#define qf_texture3D texture
#define qf_textureOffset(a,b,c,d) textureOffset(a,b,ivec2(c,d))
#define qf_shadow texture