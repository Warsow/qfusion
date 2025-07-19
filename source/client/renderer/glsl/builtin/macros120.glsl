#define qf_varying varying
#define qf_flat_varying varying

#ifdef VERTEX_SHADER
#define qf_FrontColor gl_FrontColor
#define qf_attribute attribute
#endif

#ifdef FRAGMENT_SHADER
#define qf_FrontColor gl_Color
#define qf_FragColor gl_FragColor
#define qf_BrightColor gl_FragData[1]
#endif

#define qf_texture texture2D
#define qf_textureLod texture2DLod
#define qf_textureCube textureCube
#define qf_textureArray texture2DArray
#define qf_texture3D texture3D
#define qf_textureOffset(a,b,c,d) texture2DOffset(a,b,ivec2(c,d))
#define qf_shadow shadow2D