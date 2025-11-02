layout (std140) uniform DeformBuiltinBlock {
    vec3 u_QF_ViewOrigin;
    mat3 u_QF_ViewAxis;
    vec3 u_QF_EntityOrigin;
    float u_QF_ShaderTime;
    float u_QF_MirrorSide;
};