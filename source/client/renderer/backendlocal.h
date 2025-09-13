/*
Copyright (C) 2011 Victor Luchits

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/
#ifndef R_BACKEND_LOCAL_H
#define R_BACKEND_LOCAL_H

#define MAX_STREAM_VBO_VERTS        8192
#define MAX_STREAM_VBO_ELEMENTS     MAX_STREAM_VBO_VERTS * 6
#define MAX_STREAM_VBO_TRIANGLES    MAX_STREAM_VBO_ELEMENTS / 3

#define MAX_DYNAMIC_DRAWS           2048

typedef struct {
	unsigned int numBones;
	dualquat_t dualQuats[MAX_GLSL_UNIFORM_BONES];
	unsigned int maxWeights;
} rbBonesData_t;

typedef struct {
	mesh_vbo_t *vbo;
	uint8_t *vertexData;
	VertElemSpan drawElements;
} rbDynamicStream_t;

typedef struct {
	const entity_t *entity;
	const shader_t *shader;
	const mfog_t *fog;
	const portalSurface_t *portalSurface;
	unsigned int shadowBits;
	vattribmask_t vattribs; // based on the fields above - cached to avoid rebinding
	int streamId;
	int primitive;
	vec2_t offset;
	int scissor[4];
	VertElemSpan drawElements;
} rbDynamicDraw_t;

class GLStateProxy;

typedef struct r_backend_s {
	GLStateProxy *glStateProxy;

	int64_t time;

	mat4_t cameraMatrix;
	mat4_t objectMatrix;
	mat4_t modelviewMatrix;
	mat4_t projectionMatrix;
	mat4_t modelviewProjectionMatrix;
	float zNear, zFar;

	int renderFlags;

	vec3_t cameraOrigin;
	mat3_t cameraAxis;

	const entity_t *currentEntity;
	modtype_t currentModelType;
	const mesh_vbo_t *currentMeshVBO;
	rbBonesData_t bonesData;
	const portalSurface_t *currentPortalSurface;

	// glUseProgram cache
	int currentProgram;
	int currentProgramObject;

	// RP_RegisterProgram cache
	int currentRegProgram;
	int currentRegProgramType;
	uint64_t currentRegProgramFeatures;

	struct {
		unsigned vertexDataSize;
		unsigned indexDataSize;
		mesh_vbo_t *vbo;
		void *vboData;
		void *iboData;
	} frameUploads[2];

	rbDynamicStream_t dynamicStreams[RB_VBO_NUM_STREAMS];
	rbDynamicDraw_t dynamicDraws[MAX_DYNAMIC_DRAWS];
	int numDynamicDraws;

	DrawCallData drawCallData;

	vattribmask_t currentVAttribs;

	int primitive;
	int currentVBOId;
	mesh_vbo_t *currentVBO;

	unsigned int currentDlightBits;
	unsigned int currentShadowBits;

	// shader state
	const shader_t *currentShader;
	double currentShaderTime;
	float currentShaderFrac;
	int currentShaderState;
	int shaderStateORmask, shaderStateANDmask;
	bool dirtyUniformState;
	bool doneDepthPass;
	int donePassesTotal;

	bool wireframe;

	const superLightStyle_t *superLightStyle;

	uint8_t entityColor[4];
	uint8_t entityOutlineColor[4];
	entity_t nullEnt;

	const mfog_t *fog, *texFog, *colorFog;

	bool greyscale;
	bool alphaHack;
	bool noDepthTest;
	bool noColorWrite;
	bool depthEqual;
	float hackedAlpha;

	float minLight;
	float hdrExposure;
	bool noWorldLight;
} rbackend_t;

extern rbackend_t rb;

void RB_DrawElementsReal( const DrawCallData &drawCallData );

#define RB_IsAlphaBlending( blendsrc,blenddst ) \
	( ( blendsrc ) == GLSTATE_SRCBLEND_SRC_ALPHA || ( blenddst ) == GLSTATE_DSTBLEND_SRC_ALPHA ) || \
	( ( blendsrc ) == GLSTATE_SRCBLEND_ONE_MINUS_SRC_ALPHA || ( blenddst ) == GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA )

// r_backend_program.c
void RB_InitShading( void );

void RB_DrawWireframeElements( const FrontendToBackendShared *fsh );
void RB_DrawShadedElements( const FrontendToBackendShared *fsh );

int RB_RegisterProgram( int type, const char *name, const DeformSig &deformSig,
						const deformv_t *deforms, int numDeforms, uint64_t features );
int RB_BindProgram( int program );
void RB_SetInstanceData( int numInstances, instancePoint_t *instances );

#endif // R_BACKEND_LOCAL_H
