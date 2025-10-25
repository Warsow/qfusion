/*
Copyright (C) 2002-2011 Victor Luchits

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

#include "local.h"
#include "program.h"
#include "frontend.h"
#include "backendlocal.h"
#include "glstateproxy.h"
#include "common/facilities/cvar.h"
#include <common/helpers/memspecbuilder.h>
#include <common/facilities/sysclock.h>

rbackend_t rb;

static void RB_RegisterStreamVBOs();

void RB_Init() {
	memset( &rb, 0, sizeof( rb ) );

	rb.glState = new GLStateProxy( glConfig.width, glConfig.height, glConfig.stencilBits );

	// create VBO's we're going to use for streamed data
	RB_RegisterStreamVBOs();
}

void RB_Shutdown() {
	for( auto &fru: rb.frameUploads ) {
		Q_free( fru.vboData );
		fru.vboData = nullptr;
		Q_free( fru.iboData );
		fru.iboData = nullptr;
	}
	delete rb.glState;
	rb.glState = nullptr;
}

void RB_BeginRegistration() {
	RB_RegisterStreamVBOs();
	RB_BindVBO( 0 );

	// unbind all texture targets on all TMUs
	rb.glState->unbindAllTextures();
}

void RB_EndRegistration() {
	RB_BindVBO( 0 );
}

void RB_SetTime( int64_t time ) {
	rb.globalState.time = time;
	rb.globalState.nullEnt.shaderTime = Sys_Milliseconds();
}

void RB_BeginFrame() {
	Vector4Set( rb.globalState.nullEnt.shaderRGBA, 1, 1, 1, 1 );
	rb.globalState.nullEnt.scale = 1;
	VectorClear( rb.globalState.nullEnt.origin );
	Matrix3_Identity( rb.globalState.nullEnt.axis );

	// start fresh each frame
	RB_SetShaderStateMask( ~0, 0 );
	RB_BindVBO( 0 );
	RB_FlushTextureCache();
}

void RB_EndFrame() {
}

void RB_FlushTextureCache( void ) {
	if( rb.glState ) {
		rb.glState->flushTextureCache();
	}
}

void RB_DepthRange( float depthmin, float depthmax ) {
	rb.glState->setDepthRange( depthmin, depthmax );
}

void RB_GetDepthRange( float* depthmin, float *depthmax ) {
	rb.glState->getDepthRange( depthmin, depthmax );
}

void RB_SaveDepthRange() {
	rb.glState->saveDepthRange();
}

void RB_RestoreDepthRange() {
	rb.glState->restoreDepthRange();
}

void RB_LoadCameraMatrix( const mat4_t m ) {
	Matrix4_Copy( m, rb.globalState.cameraMatrix );
}

void RB_LoadObjectMatrix( const mat4_t m ) {
	Matrix4_Copy( m, rb.globalState.objectMatrix );
	Matrix4_MultiplyFast( rb.globalState.cameraMatrix, m, rb.globalState.modelviewMatrix );
	Matrix4_Multiply( rb.globalState.projectionMatrix, rb.globalState.modelviewMatrix, rb.globalState.modelviewProjectionMatrix );
}

void RB_GetObjectMatrix( float *m ) {
	Matrix4_Copy( rb.globalState.objectMatrix, m );
}

void RB_LoadProjectionMatrix( const mat4_t m ) {
	Matrix4_Copy( m, rb.globalState.projectionMatrix );
	Matrix4_Multiply( m, rb.globalState.modelviewMatrix, rb.globalState.modelviewProjectionMatrix );
}

void RB_FlipFrontFace( void ) {
	rb.glState->flipFrontFace();
}

void RB_Scissor( int x, int y, int w, int h ) {
	rb.glState->setScissor( x, y, w, h );
}

void RB_GetScissor( int *x, int *y, int *w, int *h ) {
	rb.glState->getScissor( x, y, w, h );
}

void RB_Viewport( int x, int y, int w, int h ) {
	rb.glState->setViewport( x, y, w, h );
}

void RB_Clear( int bits, float r, float g, float b, float a ) {
	unsigned state = rb.glState->getState();

	if( bits & GL_DEPTH_BUFFER_BIT ) {
		state |= GLSTATE_DEPTHWRITE;
	}

	if( bits & GL_STENCIL_BUFFER_BIT ) {
		qglClearStencil( 128 );
	}

	if( bits & GL_COLOR_BUFFER_BIT ) {
		state = ( state & ~GLSTATE_NO_COLORWRITE ) | GLSTATE_ALPHAWRITE;
		qglClearColor( r, g, b, a );
	}

	rb.glState->setState( state );

	rb.glState->applyScissor();

	qglClear( bits );

	rb.glState->setDepthRange( 0.0f, 1.0f );
}

void RB_BindFrameBufferObject( RenderTargetComponents *components ) {
	// TODO: Resolve object lifetime problems/initialization order so we don't have to call it like this...
	GLStateProxy::bindFramebufferObject( rb.glState, components );
}

void RB_RegisterStreamVBOs() {
	for( auto &fru: rb.frameUploads ) {
		if( fru.vbo ) {
			R_TouchMeshVBO( fru.vbo );
		} else {
			const auto group = std::addressof( fru ) - rb.frameUploads;
			vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_COLOR0_BIT | VATTRIB_TEXCOORDS_BIT;
			if( group != UPLOAD_GROUP_2D_MESH && group != UPLOAD_GROUP_DEBUG_MESH ) {
				vattribs |= VATTRIB_NORMAL_BIT;
			}
			unsigned capacityInVerts;
			if( group != UPLOAD_GROUP_BATCHED_MESH_EXT && group != UPLOAD_GROUP_2D_MESH ) {
				capacityInVerts = ( 1 << 16 ) - 1;
			} else {
				// We don't need that much for sprites and 2D stuff
				capacityInVerts = 4096;
			}
			unsigned capacityInElems = 6 * capacityInVerts;
			// TODO: Allow to supplying capacity in bytes for heterogenous buffers
			fru.vbo = R_CreateMeshVBO( &rb, capacityInVerts, capacityInElems, 0, vattribs, VBO_TAG_STREAM, 0 );
			fru.vboData = Q_malloc( capacityInVerts * fru.vbo->layout.vertexSize );
			fru.iboData = Q_malloc( capacityInElems * sizeof( uint16_t ) );
			fru.vboCapacityInVerts = capacityInVerts;
			fru.vboCapacityInBytes = capacityInVerts * fru.vbo->layout.vertexSize;
			fru.iboCapacityInElems = capacityInElems;
		}
	}
}

mesh_vbo_s *RB_BindVBO( int id ) {
	mesh_vbo_t *vbo;
	if( id > 0 ) [[likely]] {
		vbo = R_GetVBOByIndex( id );
	} else if( id < 0 ) {
		const auto group = (unsigned)( -1 - id );
		assert( group < std::size( rb.frameUploads ) );
		vbo = rb.frameUploads[group].vbo;
	} else {
		vbo = nullptr;
	}

	if( vbo ) {
		rb.glState->bindVertexBuffer( vbo->vertexId );
		rb.glState->bindIndexBuffer( vbo->elemId );
	} else {
		rb.glState->bindVertexBuffer( 0 );
		rb.glState->bindIndexBuffer( 0 );
	}

	return vbo;
}

int RB_VBOIdForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	return -1 - (signed)group;
}

const VboSpanLayout *RB_VBOSpanLayoutForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	return &rb.frameUploads[group].vbo->layout;
}

unsigned RB_VboCapacityInVertexBytesForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	return rb.frameUploads[group].vboCapacityInBytes;
}

unsigned RB_VboCapacityInVerticesForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) && group != UPLOAD_GROUP_BATCHED_MESH_EXT );
	return rb.frameUploads[group].vboCapacityInVerts;
}

unsigned RB_VboCapacityInIndexElemsForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	return rb.frameUploads[group].iboCapacityInElems;
}

void R_BeginUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
}

void R_SetUploadedSubdataFromMeshUsingOffsets( unsigned group, unsigned baseVertex, unsigned verticesOffsetInBytes,
											   unsigned indicesOffsetInBytes, const mesh_t *mesh ) {
	assert( group < std::size( rb.frameUploads ) );
	if( mesh->numVerts && mesh->numElems ) {
		auto &fru = rb.frameUploads[group];

		auto *const destVertexData = (uint8_t *)fru.vboData + verticesOffsetInBytes;
		auto *const destIndexData  = (uint16_t *)( (uint8_t *)fru.iboData + indicesOffsetInBytes );

		R_FillVBOVertexDataBuffer( fru.vbo, &fru.vbo->layout, fru.vbo->layout.vertexAttribs, mesh, destVertexData );
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + baseVertex;
		}
	}
}

void R_SetUploadedSubdataFromMeshUsingLayout( unsigned group, unsigned baseVertex, const VboSpanLayout *layout,
											  unsigned indexOfFirstIndex, const mesh_t *mesh ) {
	assert( group < std::size( rb.frameUploads ) );
	if( mesh->numVerts && mesh->numElems ) {
		auto &fru = rb.frameUploads[group];

		R_FillVBOVertexDataBuffer( fru.vbo, layout, layout->vertexAttribs, mesh, fru.vboData );

		auto *const destIndexData  = ( (uint16_t *)fru.iboData ) + indexOfFirstIndex;
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + baseVertex;
		}
	}
}

void R_EndUploads( unsigned group, unsigned vertexDataSizeInBytes, unsigned indexDataSizeInBytes ) {
	assert( group < std::size( rb.frameUploads ) );
	if( vertexDataSizeInBytes && indexDataSizeInBytes ) {
		RB_BindVBO( RB_VBOIdForFrameUploads( group ) );
		const auto &fru = rb.frameUploads[group];

		qglBufferSubData( GL_ARRAY_BUFFER, 0, vertexDataSizeInBytes, fru.vboData );
		qglBufferSubData( GL_ELEMENT_ARRAY_BUFFER, 0, indexDataSizeInBytes, fru.iboData );
	}
}

void RB_DoDrawMeshVerts( const DrawMeshVertSpan *vertSpan, int primitive ) {
	// TODO: What's the purpose of v_drawElements
	if( !( v_drawElements.get() || rb.materialState.currentEntity == &rb.globalState.nullEnt ) ) [[unlikely]] {
		return;
	}

	rb.glState->applyScissor();

	if( const auto *mdSpan = std::get_if<MultiDrawElemSpan>( vertSpan ) ) {
		qglMultiDrawElements( primitive, mdSpan->counts, GL_UNSIGNED_SHORT, mdSpan->indices, mdSpan->numDraws );
	} else if( const auto *vertElemSpan = std::get_if<VertElemSpan>( vertSpan ) ) {
		const unsigned numVerts  = vertElemSpan->numVerts;
		const unsigned numElems  = vertElemSpan->numElems;
		const unsigned firstVert = vertElemSpan->firstVert;
		const unsigned firstElem = vertElemSpan->firstElem;

		qglDrawRangeElements( primitive, firstVert, firstVert + numVerts - 1, (int)numElems,
							  GL_UNSIGNED_SHORT, (GLvoid *)( firstElem * sizeof( elem_t ) ) );
	} else {
		assert( false );
	}
}

void RB_DrawMesh( const FrontendToBackendShared *fsh, int vboId, const VboSpanLayout *layout, const DrawMeshVertSpan *drawMeshVertSpan, int primitive ) {
	const mesh_vbo_s *vbo = RB_BindVBO( vboId );

	if( !layout ) [[likely]] {
		layout = &vbo->layout;
	}

	rb.drawState.currentVAttribs &= ~VATTRIB_INSTANCES_BITS;

	assert( rb.materialState.currentShader );

	rb.glState->enableVertexAttribs( rb.drawState.currentVAttribs, layout );

	if( rb.globalState.wireframe ) {
		RB_DrawWireframeMesh( fsh, drawMeshVertSpan, primitive );
	} else {
		RB_DrawShadedMesh( fsh, drawMeshVertSpan, primitive );
	}
}

void RB_SetCamera( const vec3_t cameraOrigin, const mat3_t cameraAxis ) {
	VectorCopy( cameraOrigin, rb.globalState.cameraOrigin );
	Matrix3_Copy( cameraAxis, rb.globalState.cameraAxis );
}

void RB_SetRenderFlags( int flags ) {
	rb.globalState.renderFlags = flags;
}

bool RB_EnableWireframe( bool enable ) {
	const bool oldVal = rb.globalState.wireframe;

	if( rb.globalState.wireframe != enable ) {
		rb.globalState.wireframe = enable;

		if( enable ) {
			RB_SetShaderStateMask( 0, GLSTATE_NO_DEPTH_TEST );
			qglPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
		} else {
			RB_SetShaderStateMask( ~0, 0 );
			qglPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
		}
	}

	return oldVal;
}

static void RB_UpdateVertexAttribs( void ) {
	vattribmask_t vattribs = rb.materialState.currentShader->vattribs;
	if( rb.drawState.superLightStyle ) {
		vattribs |= rb.drawState.superLightStyle->vattribs;
	}
	if( rb.drawState.bonesData.numBones ) {
		vattribs |= VATTRIB_BONES_BITS;
	}
	if( rb.materialState.currentEntity->outlineHeight != 0.0f ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( DRAWFLAT() ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( rb.drawState.currentShadowBits && ( rb.materialState.currentModelType == mod_brush ) ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	rb.drawState.currentVAttribs = vattribs;
}

void RB_BindShader( const entity_t *e, const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
					const shader_t *shader, const mfog_t *fog, const portalSurface_s *portalSurface ) {
	rb.materialState.currentShader = shader;
	rb.materialState.fog = fog;
	rb.materialState.texFog = rb.materialState.colorFog = NULL;

	rb.drawState.doneDepthPass = false;
	rb.drawState.dirtyUniformState = true;

	rb.materialState.currentEntity = e ? e : &rb.globalState.nullEnt;
	rb.materialState.currentModelType = rb.materialState.currentEntity->rtype == RT_MODEL &&
										rb.materialState.currentEntity->model ? rb.materialState.currentEntity->model->type : mod_bad;

	rb.drawState.currentDlightBits = 0;
	rb.drawState.currentShadowBits = 0;
	rb.drawState.superLightStyle = nullptr;

	rb.drawState.bonesData.numBones = 0;
	rb.drawState.bonesData.maxWeights = 0;

	rb.materialState.currentPortalSurface = portalSurface;

	if( !e ) {
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			rb.materialState.currentShaderTime = 1e-3 * (double)materialParams->shaderTime;
			rb.materialState.currentShaderFrac = materialParams->shaderFrac;
		} else {
			rb.materialState.currentShaderTime = 1e-3 * (double)rb.globalState.nullEnt.shaderTime;
			rb.materialState.currentShaderFrac = 0.0f;
		}
		rb.materialState.alphaHack = false;
		rb.materialState.greyscale = false;
		rb.materialState.noDepthTest = false;
		rb.materialState.noColorWrite =  false;
		rb.materialState.depthEqual = false;
	} else {
		Vector4Copy( rb.materialState.currentEntity->shaderRGBA, rb.materialState.entityColor );
		Vector4Copy( rb.materialState.currentEntity->outlineColor, rb.materialState.entityOutlineColor );
		int64_t givenShaderTime;
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			givenShaderTime = materialParams->shaderTime;
			rb.materialState.currentShaderFrac = materialParams->shaderFrac;
		} else {
			givenShaderTime = rb.materialState.currentEntity->shaderTime;
			rb.materialState.currentShaderFrac = 0.0f;
		}
		if( givenShaderTime > rb.globalState.time ) {
			rb.materialState.currentShaderTime = 0;
		} else {
			rb.materialState.currentShaderTime = 1e-3 * (double)( rb.globalState.time - givenShaderTime );
		}
		rb.materialState.alphaHack = e->renderfx & RF_ALPHAHACK ? true : false;
		rb.materialState.hackedAlpha = (float)e->shaderRGBA[3] / 255.0f;
		rb.materialState.greyscale = e->renderfx & RF_GREYSCALE ? true : false;
		rb.materialState.noDepthTest = e->renderfx & RF_NODEPTHTEST && e->rtype == RT_SPRITE ? true : false;
		rb.materialState.noColorWrite = e->renderfx & RF_NOCOLORWRITE ? true : false;
		rb.materialState.depthEqual = rb.materialState.alphaHack && ( e->renderfx & RF_WEAPONMODEL );
	}

	if( fog && fog->shader && !rb.materialState.noColorWrite ) {
		// should we fog the geometry with alpha texture or scale colors?
		if( !rb.materialState.alphaHack && Shader_UseTextureFog( shader ) ) {
			rb.materialState.texFog = fog;
		} else {
			// use scaling of colors
			rb.materialState.colorFog = fog;
		}
	}

	RB_UpdateVertexAttribs();
}

void RB_SetLightstyle( const superLightStyle_t *lightStyle ) {
	assert( rb.materialState.currentShader != NULL );
	rb.drawState.superLightStyle = lightStyle;
	rb.drawState.dirtyUniformState = true;

	RB_UpdateVertexAttribs();
}

void RB_SetDlightBits( unsigned int dlightBits ) {
	assert( rb.materialState.currentShader != NULL );
	rb.drawState.currentDlightBits = dlightBits;
	rb.drawState.dirtyUniformState = true;
}

void RB_SetBonesData( int numBones, dualquat_t *dualQuats, int maxWeights ) {
	assert( rb.materialState.currentShader != NULL );

	if( numBones > MAX_GLSL_UNIFORM_BONES ) {
		numBones = MAX_GLSL_UNIFORM_BONES;
	}
	if( maxWeights > 4 ) {
		maxWeights = 4;
	}

	rb.drawState.bonesData.numBones = numBones;
	memcpy( rb.drawState.bonesData.dualQuats, dualQuats, numBones * sizeof( *dualQuats ) );
	rb.drawState.bonesData.maxWeights = maxWeights;

	rb.drawState.dirtyUniformState = true;

	RB_UpdateVertexAttribs();
}

void RB_SetZClip( float zNear, float zFar ) {
	rb.globalState.zNear = zNear;
	rb.globalState.zFar = zFar;
}

void RB_SetLightParams( float minLight, bool noWorldLight, float hdrExposure ) {
	rb.globalState.minLight = minLight;
	rb.globalState.noWorldLight = noWorldLight;
	rb.globalState.hdrExposure = hdrExposure;
}

void RB_SetShaderStateMask( unsigned ANDmask, unsigned ORmask ) {
	rb.globalState.shaderStateANDmask = ANDmask;
	rb.globalState.shaderStateORmask  = ORmask;
}

int RB_RegisterProgram( int type, const shader_s *materialToGetDeforms, uint64_t features ) {
	const DeformSig &deformSig = materialToGetDeforms->deformSig;
	const deformv_t *deforms   = materialToGetDeforms->deforms;
	const unsigned numDeforms  = materialToGetDeforms->numdeforms;
	const bool noDeforms       = !numDeforms;

	if( rb.programState.currentRegProgramType == type && noDeforms && rb.programState.currentRegProgramFeatures == features ) {
		return rb.programState.currentRegProgram;
	}

	const int program = RP_RegisterProgram( type, nullptr, deformSig, deforms, numDeforms, features );
	if( noDeforms ) {
		rb.programState.currentRegProgram = program;
		rb.programState.currentRegProgramType = type;
		rb.programState.currentRegProgramFeatures = features;
	}

	return program;
}

int RB_BindProgram( int program ) {
	if( program == rb.programState.currentProgram ) {
		return rb.programState.currentProgramObject;
	}

	rb.programState.currentProgram = program;
	if( !program ) {
		rb.programState.currentProgramObject = 0;
		qglUseProgram( 0 );
		return 0;
	}

	const int object = RP_GetProgramObject( program );
	if( object ) {
		qglUseProgram( object );
	}

	rb.programState.currentProgramObject = object;
	rb.drawState.dirtyUniformState = true;
	return object;
}

void R_SubmitAliasSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceAlias_t *drawSurf ) {
	const maliasmesh_t *aliasmesh = drawSurf->mesh;

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 1u * aliasmesh->numverts,
		.firstElem = 0, .numElems = 3u * aliasmesh->numtris,
	};

	RB_DrawMesh( fsh, aliasmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
}

void R_SubmitSkeletalSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceSkeletal_t *drawSurf ) {
	const model_t *mod = drawSurf->model;
	const mskmodel_t *skmodel = ( const mskmodel_t * )mod->extradata;
	const mskmesh_t *skmesh = drawSurf->mesh;
	skmcacheentry_s *cache = nullptr;
	dualquat_t *bonePoseRelativeDQ = nullptr;

	skmodel = ( ( mskmodel_t * )mod->extradata );
	if( skmodel->numbones && skmodel->numframes > 0 ) {
		cache = R_GetSkeletalCache( e->number, mod->lodnum, fsh->sceneIndex );
	}

	if( cache ) {
		bonePoseRelativeDQ = R_GetSkeletalBones( cache );
	}

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 1u * skmesh->numverts,
		.firstElem = 0, .numElems = 3u * skmesh->numtris,
	};

	if( !cache || R_SkeletalRenderAsFrame0( cache ) ) {
		// fastpath: render static frame 0 as is
		if( skmesh->vbo ) {
			RB_DrawMesh( fsh, skmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
			return;
		}
	}

	if( bonePoseRelativeDQ && skmesh->vbo ) {
		// another fastpath: transform the initial pose on the GPU
		RB_SetBonesData( skmodel->numbones, bonePoseRelativeDQ, skmesh->maxWeights );
		RB_DrawMesh( fsh, skmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
		return;
	}
}

void R_SubmitBSPSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceBSP_t *drawSurf ) {
	const MergedBspSurface *mergedBspSurf = drawSurf->mergedBspSurf;

	assert( !mergedBspSurf->numInstances );

	RB_SetDlightBits( drawSurf->dlightBits );
	RB_SetLightstyle( mergedBspSurf->superLightStyle );

	const DrawMeshVertSpan &drawMeshVertSpan = drawSurf->mdSpan;

	RB_DrawMesh( fsh, mergedBspSurf->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
}

void R_SubmitNullSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * ) {
	assert( rsh.nullVBO != NULL );

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 6, .firstElem = 0, .numElems = 6,
	};

	RB_DrawMesh( fsh, rsh.nullVBO->index, nullptr, &drawMeshVertSpan, GL_LINES );
}

void R_SubmitDynamicMeshToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
									const mfog_t *fog, const portalSurface_t *portalSurface, const DynamicMeshDrawSurface *drawSurface ) {
	// Protect against the case when fillMeshBuffers() produces zero vertices
	if( drawSurface->actualNumVertices && drawSurface->actualNumIndices ) {
		const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
			.firstVert = drawSurface->verticesOffset,
			.numVerts  = drawSurface->actualNumVertices,
			.firstElem = drawSurface->indicesOffset,
			.numElems  = drawSurface->actualNumIndices,
		};

		RB_DrawMesh( fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH ), nullptr, &drawMeshVertSpan, GL_TRIANGLES );
	}
}

void R_SubmitBatchedSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e,
									const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
									const shader_t *shader, const mfog_t *fog,
									const portalSurface_t *portalSurface, unsigned vertElemSpanIndex ) {
	const VertElemSpan &vertElemSpan = fsh->batchedVertElemSpans[vertElemSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		RB_BindShader( e, overrideParams, paramsTable, shader, fog, nullptr );
		const DrawMeshVertSpan drawMeshVertSpan = vertElemSpan;
		RB_DrawMesh( fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH ), nullptr, &drawMeshVertSpan, GL_TRIANGLES );
	}
}

void R_SubmitBatchedSurfsToBackendExt( const FrontendToBackendShared *fsh, const entity_t *e,
									   const ShaderParams *, const ShaderParamsTable *paramsTable,
									   const shader_s *shader, const mfog_t *fog,
									   const portalSurface_t *portalSurface, unsigned vertElemSpanAndVboSpanIndex ) {
	const auto &[vertElemSpan, vboSpanLayout] = fsh->batchedVertElemAndVboSpans[vertElemSpanAndVboSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		RB_BindShader( e, nullptr, paramsTable, shader, fog, nullptr );
		const DrawMeshVertSpan drawMeshVertSpan = vertElemSpan;
		RB_DrawMesh( fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH_EXT ), &vboSpanLayout, &drawMeshVertSpan, GL_TRIANGLES );
	}
}