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

#include "glstateproxy.h"

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
struct BackendState;

void *RB_GetTmpUniformBlock( BackendState *backendState, unsigned binding, size_t requestedBlockSize );
void RB_CommitUniformBlock( BackendState *backendState, unsigned binding, void *blockData, size_t blockSize );

struct RuntimeBackendState {
	GLuint programId { 0 };
};

struct BackendState {
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
	} global;

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
	} material;

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
	} draw;

	struct {
		int boundProgram;

		int cachedFastLookupProgram;
		int cachedFastLookupProgramType;
		uint64_t cachedFastLookupProgramFeatures;
	} program;

	struct {
		struct {
			unsigned sizeSoFar { 0 };
		} blockState[MAX_UNIFORM_BINDINGS];
	} uniform;

	GLStateProxy gl;
	BackendActionTape *const actionTape;

	BackendState( BackendActionTape *actionTape, int width, int height );
};

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
		uint8_t *buffer;
		uint8_t *lastResortScratchpad;
		unsigned blockSize;
		unsigned capacity;
	} uniformUploads[MAX_UNIFORM_BINDINGS];
} rbackend_t;

extern rbackend_t rb;

#define DRAWFLAT( state ) \
	( ( ( state )->material.currentModelType == mod_brush ) && \
	( ( state )->global.renderFlags & ( RF_DRAWFLAT | RF_DRAWBRIGHT ) ) && \
	!( ( state )->material.currentShader->flags & SHADER_NODRAWFLAT ) )

#endif // R_BACKEND_LOCAL_H
