// We have to use these #ifdefs here because #defining prototypes
// of these functions to nothing results in a crash on Intel GPUs.

void QF_TransformVerts(inout vec4 Position, inout vec3 Normal, inout vec2 TexCoord)
{
#ifdef QF_NUM_BONE_INFLUENCES
	QF_VertexDualQuatsTransform(Position, Normal);
#endif
#ifdef QF_APPLY_DEFORMVERTS
	QF_DeformVerts(Position, Normal, TexCoord);
#endif
#ifdef APPLY_INSTANCED_TRANSFORMS
	QF_InstancedTransform(Position, Normal);
#endif
}

void QF_TransformVerts_Tangent(inout vec4 Position, inout vec3 Normal, inout vec3 Tangent, inout vec2 TexCoord)
{
#ifdef QF_NUM_BONE_INFLUENCES
	QF_VertexDualQuatsTransform_Tangent(Position, Normal, Tangent);
#endif
#ifdef QF_APPLY_DEFORMVERTS
	QF_DeformVerts(Position, Normal, TexCoord);
#endif
#ifdef APPLY_INSTANCED_TRANSFORMS
	QF_InstancedTransform(Position, Normal);
#endif
 }