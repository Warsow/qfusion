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

#define MAX_UNIFORM_BINDINGS        17

typedef struct {
	unsigned int numBones;
	dualquat_t dualQuats[MAX_GLSL_UNIFORM_BONES];
	unsigned int maxWeights;
} rbBonesData_t;

class GLStateProxy;

void *RB_GetTmpUniformBlock( unsigned binding, size_t blockSize );
void RB_CommitUniformBlock( unsigned binding, void *blockData, size_t blockSize );

typedef struct r_backend_s {
	struct {
		mesh_vbo_t *vbo;
		void *vboData;
		void *iboData;
		unsigned vboCapacityInVerts;
		unsigned vboCapacityInBytes;
		unsigned iboCapacityInElems;
	} vertexUploads[5];

	struct {
		GLuint id;
		void *lastUploadedData;
		void *scratchpadData;
		unsigned lastSize;
	} uniformUploads[MAX_UNIFORM_BINDINGS];

	// Either persistent during the entire frame or changes much less often than RB_BindShader() calls
	// TODO: Should it be split into truly-persistent and changing states?
	struct {
		entity_t nullEnt;
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

		float minLight;
		float hdrExposure;
		bool noWorldLight;

		// TODO: Should it be a tracked state? We don't really perform frequent switching to/from wireframe
		bool wireframe;

		unsigned shaderStateORmask;
		unsigned shaderStateANDmask;
	} globalState;

	// Gets modified only by RB_BindShader() call
	struct {
		const shader_t *currentShader;
		double currentShaderTime;
		float currentShaderFrac;

		const entity_t *currentEntity;
		modtype_t currentModelType;
		const portalSurface_t *currentPortalSurface;

		uint8_t entityColor[4];
		uint8_t entityOutlineColor[4];

		const mfog_t *fog, *texFog, *colorFog;

		bool greyscale;
		bool alphaHack;
		bool noDepthTest;
		bool noColorWrite;
		bool depthEqual;
		float hackedAlpha;
	} materialState;

	// Gets modified by RB_BindShader() and some consequent calls, including drawing passes
	struct {
		vattribmask_t currentVAttribs;
		unsigned currentDlightBits;
		unsigned currentShadowBits;
		rbBonesData_t bonesData;
		const superLightStyle_t *superLightStyle;
		unsigned currentShaderState;
		bool dirtyUniformState;
		bool doneDepthPass;
		int donePassesTotal;
	} drawState;

	struct {
		// TODO: Move to GLStateProxy?
		// glUseProgram cache
		int currentProgram;
		int currentProgramObject;

		// RP_RegisterProgram cache
		int currentRegProgram;
		int currentRegProgramType;
		uint64_t currentRegProgramFeatures;
	} programState;

	GLStateProxy *glState;
} rbackend_t;

extern rbackend_t rb;

void RB_DoDrawMeshVerts( const DrawMeshVertSpan *vertSpan, int primitive );

#define DRAWFLAT() \
	( ( rb.materialState.currentModelType == mod_brush ) && \
	( rb.globalState.renderFlags & ( RF_DRAWFLAT | RF_DRAWBRIGHT ) ) && \
	!( rb.materialState.currentShader->flags & SHADER_NODRAWFLAT ) )

int RB_RegisterProgram( int type, const shader_s *materialToGetDeforms, uint64_t features );
int RB_BindProgram( int program );

#endif // R_BACKEND_LOCAL_H
