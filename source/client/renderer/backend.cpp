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
#include "backendactiontape.h"
#include "glstateproxy.h"
#include "common/facilities/cvar.h"
#include <common/helpers/memspecbuilder.h>
#include <common/facilities/sysclock.h>

rbackend_t rb;

static void RB_RegisterStreamVBOs();

BackendState::BackendState( BackendActionTape *actionTape_, int width, int height )
	: gl( actionTape_, width, height ), actionTape( actionTape_ ) {
	std::memset( &global, 0, sizeof( global ) );
	std::memset( &draw, 0, sizeof( draw ) );
	std::memset( &material, 0, sizeof( material ) );
	std::memset( &program, 0, sizeof( program ) );
}

void RB_Init() {
	memset( &rb, 0, sizeof( rb ) );

	// create VBO's we're going to use for streamed data
	RB_RegisterStreamVBOs();

	assert( qglGetError() == GL_NO_ERROR );

	unsigned sizeOfBlocks[MAX_UNIFORM_BINDINGS];
	RP_GetSizeOfUniformBlocks( sizeOfBlocks );

	// TODO: Vary it depending of actual kind of data
	constexpr unsigned kMaxBlocksForBinding = 1 << 14;

	for( unsigned i = 0; i < MAX_UNIFORM_BINDINGS; ++i ) {
		GLuint uboId = 0;
		qglGenBuffers( 1, &uboId );

		qglBindBuffer( GL_UNIFORM_BUFFER, uboId );

		const unsigned dataSize = sizeOfBlocks[i] * ( kMaxBlocksForBinding + 1 );
		assert( dataSize > 0 );

		// Zeroed by default
		auto *const data = (uint8_t *)Q_malloc( dataSize  );

		assert( uboId != 0 );
		rb.uniformUploads[i].id                   = uboId;
		rb.uniformUploads[i].blockSize            = sizeOfBlocks[i];
		rb.uniformUploads[i].capacity             = dataSize;
		rb.uniformUploads[i].buffer               = data;
		rb.uniformUploads[i].lastResortScratchpad = data + sizeOfBlocks[i] * kMaxBlocksForBinding;

		qglBufferData( GL_UNIFORM_BUFFER, sizeOfBlocks[i] * kMaxBlocksForBinding, nullptr, GL_DYNAMIC_DRAW );

		qglBindBuffer( GL_UNIFORM_BUFFER, 0 );

		if( qglGetError() != GL_NO_ERROR ) {
			Com_Error( ERR_FATAL, "Failed to setup a uniform buffer" );
		}
	}
}

void RB_Shutdown() {
	for( auto &vu: rb.vertexUploads ) {
		Q_free( vu.vboData );
		vu.vboData = nullptr;
		Q_free( vu.iboData );
		vu.iboData = nullptr;
	}
	for( auto &uu: rb.uniformUploads ) {
		qglDeleteBuffers( 1, &uu.id );
		Q_free( uu.buffer );
		uu.buffer = nullptr;
	}
}

void RB_SetDefaultGLState( int stencilBits ) {
	if( stencilBits ) {
		assert( stencilBits == 8 );
		qglStencilMask( ( GLuint ) ~0 );
		qglStencilFunc( GL_EQUAL, 128, 0xFF );
		qglStencilOp( GL_KEEP, GL_KEEP, GL_INCR );
	}

	qglDisable( GL_CULL_FACE );
	qglFrontFace( GL_CCW );
	qglDisable( GL_BLEND );
	qglDepthFunc( GL_LEQUAL );
	qglDepthMask( GL_FALSE );
	qglDisable( GL_POLYGON_OFFSET_FILL );
	qglPolygonOffset( -1.0f, 0.0f ); // units will be handled by RB_DepthOffset
	qglColorMask( GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE );
	qglEnable( GL_DEPTH_TEST );
	qglPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
	qglFrontFace( GL_CCW );
	qglEnable( GL_SCISSOR_TEST );
	// TODO: Pass as args
	qglScissor( 0, 0, glConfig.width, glConfig.height );

	for( int tmu = 0; tmu < MAX_TEXTURE_UNITS; ++tmu ) {
		qglActiveTexture( GL_TEXTURE0 + tmu );

		qglBindTexture( GL_TEXTURE_CUBE_MAP, 0 );
		qglBindTexture( GL_TEXTURE_2D_ARRAY, 0 );
		qglBindTexture( GL_TEXTURE_3D, 0 );
		qglBindTexture( GL_TEXTURE_2D, 0 );
	}

	qglActiveTexture( GL_TEXTURE0 );

	// TODO:?
	//qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, 0 );
	//qglBindBuffer( GL_ARRAY_BUFFER, 0 );

	qglUseProgram( 0 );
}

void RB_BindRenderTarget( RenderTargetComponents *components ) {
	if( components ) {
		RenderTarget            *const renderTarget           = components->renderTarget;
		RenderTargetTexture     *const oldAttachedTexture     = components->renderTarget->attachedTexture;
		RenderTargetDepthBuffer *const oldAttachedDepthBuffer = components->renderTarget->attachedDepthBuffer;
		RenderTargetTexture     *const newTexture             = components->texture;
		RenderTargetDepthBuffer *const newDepthBuffer         = components->depthBuffer;

		bool hasChanges = false;
		qglBindFramebuffer( GL_FRAMEBUFFER, renderTarget->fboId );
		if( oldAttachedTexture != newTexture ) {
			if( oldAttachedTexture ) {
				oldAttachedTexture->attachedToRenderTarget = nullptr;
			}
			if( RenderTarget *oldTarget = newTexture->attachedToRenderTarget ) {
				assert( oldTarget != renderTarget );
				// TODO: Do we have to bind it and call detach?
				oldTarget->attachedTexture = nullptr;
			}
			renderTarget->attachedTexture      = newTexture;
			newTexture->attachedToRenderTarget = renderTarget;
			qglFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, newTexture->texnum, 0 );
			hasChanges = true;
		}
		if( oldAttachedDepthBuffer != newDepthBuffer ) {
			if( oldAttachedDepthBuffer ) {
				oldAttachedDepthBuffer->attachedToRenderTarget = nullptr;
			}
			if( RenderTarget *oldTarget = newDepthBuffer->attachedToRenderTarget ) {
				assert( oldTarget != renderTarget );
				// TODO: Do we have to bind it and call detach?
				oldTarget->attachedDepthBuffer = nullptr;
			}
			renderTarget->attachedDepthBuffer      = newDepthBuffer;
			newDepthBuffer->attachedToRenderTarget = renderTarget;
			qglFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, newDepthBuffer->rboId );
			hasChanges = true;
		}
		if( hasChanges ) {
			// TODO: What to do in this case
			if( qglCheckFramebufferStatus( GL_FRAMEBUFFER ) != GL_FRAMEBUFFER_COMPLETE ) {
				// Just make sure that the status of attachments remains correct
				assert( renderTarget->attachedTexture == newTexture );
				assert( renderTarget->attachedDepthBuffer == newDepthBuffer );
				qglFramebufferTexture2D( GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0, GL_TEXTURE_2D, 0, 0 );
				qglFramebufferRenderbuffer( GL_FRAMEBUFFER, GL_DEPTH_ATTACHMENT, GL_RENDERBUFFER, 0 );
				renderTarget->attachedTexture          = nullptr;
				renderTarget->attachedDepthBuffer      = nullptr;
				newTexture->attachedToRenderTarget     = nullptr;
				newDepthBuffer->attachedToRenderTarget = nullptr;
			}
		}
	} else {
		qglBindFramebuffer( GL_FRAMEBUFFER, 0 );
	}
}

void RB_BeginRegistration() {
	RB_SetDefaultGLState( glConfig.stencilBits );

	RB_RegisterStreamVBOs();
	RB_BindVBO( nullptr, 0 );
}

void RB_EndRegistration() {
	RB_BindVBO( nullptr, 0 );
}

void RB_SetTime( BackendState *backendState, int64_t time ) {
	backendState->global.time = time;
	backendState->global.nullEnt.shaderTime = Sys_Milliseconds();
}

void RB_BeginUsingBackendState( BackendState *backendState ) {
	for( unsigned binding = 0; binding < MAX_UNIFORM_BINDINGS; ++binding ) {
		R_BeginUniformUploads( binding );
	}

	backendState->actionTape->append( []( RuntimeBackendState * ) { RB_SetDefaultGLState( glConfig.stencilBits ); } );

	Vector4Set( backendState->global.nullEnt.shaderRGBA, 1, 1, 1, 1 );
	backendState->global.nullEnt.scale = 1;
	VectorClear( backendState->global.nullEnt.origin );
	Matrix3_Identity( backendState->global.nullEnt.axis );

	// start fresh each frame
	RB_SetShaderStateMask( backendState, ~0, 0 );
	RB_BindVBO( backendState, 0 );
}

void RB_EndUsingBackendState( BackendState *backendState ) {
	backendState->actionTape->append( []( RuntimeBackendState * ) { qglUseProgram( 0 ); } );

	for( unsigned binding = 0; binding < MAX_UNIFORM_BINDINGS; ++binding ) {
		R_EndUniformUploads( binding, backendState->uniform.blockState[binding].sizeSoFar );
	}
}

void RB_DepthRange( BackendState *backendState, float depthmin, float depthmax ) {
	backendState->gl.setDepthRange( depthmin, depthmax );
}

void RB_GetDepthRange( BackendState *backendState, float *depthmin, float *depthmax ) {
	backendState->gl.getDepthRange( depthmin, depthmax );
}

void RB_SaveDepthRange( BackendState *backendState ) {
	backendState->gl.saveDepthRange();
}

void RB_RestoreDepthRange( BackendState *backendState ) {
	backendState->gl.restoreDepthRange();
}

void RB_LoadCameraMatrix( BackendState *backendState, const mat4_t m ) {
	Matrix4_Copy( m, backendState->global.cameraMatrix );
}

void RB_LoadObjectMatrix( BackendState *backendState, const mat4_t m ) {
	Matrix4_Copy( m, backendState->global.objectMatrix );
	Matrix4_MultiplyFast( backendState->global.cameraMatrix, m, backendState->global.modelviewMatrix );
	Matrix4_Multiply( backendState->global.projectionMatrix, backendState->global.modelviewMatrix, backendState->global.modelviewProjectionMatrix );
}

void RB_GetObjectMatrix( BackendState *backendState, float *m ) {
	Matrix4_Copy( backendState->global.objectMatrix, m );
}

void RB_LoadProjectionMatrix( BackendState *backendState, const mat4_t m ) {
	Matrix4_Copy( m, backendState->global.projectionMatrix );
	Matrix4_Multiply( m, backendState->global.modelviewMatrix, backendState->global.modelviewProjectionMatrix );
}

void RB_FlipFrontFace( BackendState *backendState ) {
	backendState->gl.flipFrontFace();
}

void RB_Scissor( BackendState *backendState, int x, int y, int w, int h ) {
	backendState->gl.setScissor( x, y, w, h );
}

void RB_GetScissor( BackendState *backendState, int *x, int *y, int *w, int *h ) {
	backendState->gl.getScissor( x, y, w, h );
}

void RB_Viewport( BackendState *backendState, int x, int y, int w, int h ) {
	backendState->gl.setViewport( x, y, w, h );
}

void RB_Clear( BackendState *backendState, int bits, float r, float g, float b, float a ) {
	unsigned state = backendState->gl.getState();

	if( bits & GL_DEPTH_BUFFER_BIT ) {
		state |= GLSTATE_DEPTHWRITE;
	}

	if( bits & GL_STENCIL_BUFFER_BIT ) {
		backendState->actionTape->clearStencil( 128 );
	}

	if( bits & GL_COLOR_BUFFER_BIT ) {
		state = ( state & ~GLSTATE_NO_COLORWRITE ) | GLSTATE_ALPHAWRITE;
		backendState->actionTape->clearColor( r, g, b, a );
	}

	backendState->gl.setState( state );

	backendState->gl.applyScissor();

	backendState->actionTape->clear( bits );

	backendState->gl.setDepthRange( 0.0f, 1.0f );
}

void RB_RegisterStreamVBOs() {
	for( auto &vu: rb.vertexUploads ) {
		// TODO: Allow to create explictly managed vertex buffers, so we don't have to touch auxiliary buffers
		if( vu.vbo ) {
			R_TouchMeshVBO( vu.vbo );
		} else {
			const auto group = std::addressof( vu ) - rb.vertexUploads;
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
			vu.vbo = R_CreateMeshVBO( &rb, capacityInVerts, capacityInElems, 0, vattribs, VBO_TAG_STREAM, 0 );
			vu.vboData = Q_malloc( capacityInVerts * vu.vbo->layout.vertexSize );
			vu.iboData = Q_malloc( capacityInElems * sizeof( uint16_t ) );
			vu.vboCapacityInVerts = capacityInVerts;
			vu.vboCapacityInBytes = capacityInVerts * vu.vbo->layout.vertexSize;
			vu.iboCapacityInElems = capacityInElems;
		}
	}
}

mesh_vbo_s *RB_BindVBO( BackendState *backendState, int id ) {
	mesh_vbo_t *vbo;
	if( id > 0 ) [[likely]] {
		vbo = R_GetVBOByIndex( id );
	} else if( id < 0 ) {
		const auto group = (unsigned)( -1 - id );
		assert( group < std::size( rb.vertexUploads ) );
		vbo = rb.vertexUploads[group].vbo;
	} else {
		vbo = nullptr;
	}

	const GLuint vertexId = vbo ? vbo->vertexId : 0;
	const GLuint elemId   = vbo ? vbo->elemId : 0;
	// TODO: Split the code path properly so we don't have to pass the null backend state
	if( backendState ) {
		backendState->gl.bindVertexBuffer( vertexId );
		backendState->gl.bindIndexBuffer( elemId );
	} else {
		qglBindBuffer( GL_ARRAY_BUFFER, vertexId );
		qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, elemId );
	}

	return vbo;
}

int RB_VBOIdForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.vertexUploads ) );
	return -1 - (signed)group;
}

const VboSpanLayout *RB_VBOSpanLayoutForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.vertexUploads ) );
	return &rb.vertexUploads[group].vbo->layout;
}

unsigned RB_VboCapacityInVertexBytesForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.vertexUploads ) );
	return rb.vertexUploads[group].vboCapacityInBytes;
}

unsigned RB_VboCapacityInVerticesForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.vertexUploads ) && group != UPLOAD_GROUP_BATCHED_MESH_EXT );
	return rb.vertexUploads[group].vboCapacityInVerts;
}

unsigned RB_VboCapacityInIndexElemsForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.vertexUploads ) );
	return rb.vertexUploads[group].iboCapacityInElems;
}

void R_BeginMeshUploads( unsigned group ) {
	assert( group < std::size( rb.vertexUploads ) );
}

void R_SetUploadedSubdataFromMeshUsingOffsets( unsigned group, unsigned baseVertex, unsigned verticesOffsetInBytes,
											   unsigned indicesOffsetInBytes, const mesh_t *mesh ) {
	assert( group < std::size( rb.vertexUploads ) );
	if( mesh->numVerts && mesh->numElems ) {
		auto &vu = rb.vertexUploads[group];

		auto *const destVertexData = (uint8_t *)vu.vboData + verticesOffsetInBytes;
		auto *const destIndexData  = (uint16_t *)((uint8_t *)vu.iboData + indicesOffsetInBytes );

		R_FillVBOVertexDataBuffer( vu.vbo, &vu.vbo->layout, vu.vbo->layout.vertexAttribs, mesh, destVertexData );
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + baseVertex;
		}
	}
}

void R_SetUploadedSubdataFromMeshUsingLayout( unsigned group, unsigned baseVertex, const VboSpanLayout *layout,
											  unsigned indexOfFirstIndex, const mesh_t *mesh ) {
	assert( group < std::size( rb.vertexUploads ) );
	if( mesh->numVerts && mesh->numElems ) {
		auto &vu = rb.vertexUploads[group];

		R_FillVBOVertexDataBuffer( vu.vbo, layout, layout->vertexAttribs, mesh, vu.vboData );

		auto *const destIndexData  = ( (uint16_t *)vu.iboData ) + indexOfFirstIndex;
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + baseVertex;
		}
	}
}

void R_EndMeshUploads( unsigned group, unsigned vertexDataSizeInBytes, unsigned indexDataSizeInBytes ) {
	assert( group < std::size( rb.vertexUploads ) );
	if( vertexDataSizeInBytes && indexDataSizeInBytes ) {
		RB_BindVBO( nullptr, RB_VBOIdForFrameUploads( group ) );
		const auto &vu = rb.vertexUploads[group];

		qglBufferSubData( GL_ARRAY_BUFFER, 0, vertexDataSizeInBytes, vu.vboData );
		qglBufferSubData( GL_ELEMENT_ARRAY_BUFFER, 0, indexDataSizeInBytes, vu.iboData );
	}
}

void R_BeginUniformUploads( unsigned binding ) {
	assert( binding < std::size( rb.uniformUploads ) );
}

void R_EndUniformUploads( unsigned binding, unsigned sizeInBytes ) {
	assert( binding < std::size( rb.uniformUploads ) );

	if( sizeInBytes > 0 ) {
		const auto &uu = rb.uniformUploads[binding];
		assert( sizeInBytes <= uu.capacity );
		assert( ( sizeInBytes % uu.blockSize ) == 0 );

		qglBindBuffer( GL_UNIFORM_BUFFER, uu.id );
		qglBufferSubData( GL_UNIFORM_BUFFER, 0, sizeInBytes, uu.buffer );
		// TODO: Avoid doing this
		qglBindBuffer( GL_UNIFORM_BUFFER, 0 );
	}
}

void *RB_GetTmpUniformBlock( BackendState *backendState, unsigned binding, size_t requestedBlockSize ) {
	assert( binding < std::size( rb.uniformUploads ) );
	assert( std::size( rb.uniformUploads ) == std::size( backendState->uniform.blockState ) );

	const auto *const globalState = &rb.uniformUploads[binding];
	assert( std::abs( (int)requestedBlockSize - (int)globalState->blockSize ) < 16 );

	auto *const stateOfBackendState = &backendState->uniform.blockState[binding];
	assert( ( stateOfBackendState->sizeSoFar % globalState->blockSize ) == 0 );

	void *result;
	if( stateOfBackendState->sizeSoFar + globalState->blockSize <= globalState->capacity ) [[likely]] {
		result = globalState->buffer + stateOfBackendState->sizeSoFar;
	} else {
		result = globalState->lastResortScratchpad;
	}

	std::memset( result, 0, requestedBlockSize );
	return result;
}

void RB_CommitUniformBlock( BackendState *backendState, unsigned binding, void *blockData, size_t submittedBlockSize ) {
	assert( binding < std::size( rb.uniformUploads ) );
	assert( std::size( rb.uniformUploads ) == std::size( backendState->uniform.blockState ) );

	const auto *globalState = &rb.uniformUploads[binding];
	assert( std::abs( (int)submittedBlockSize - (int)globalState->blockSize ) < 16 );

	bool updateBinding = false;

	auto *const stateOfBackendState = &backendState->uniform.blockState[binding];
	assert( ( stateOfBackendState->sizeSoFar % globalState->blockSize ) == 0 );

	// TODO: The initial offset is going to be > 0 if mulitple uploads are performed in parallel (and we use slices)
	if( stateOfBackendState->sizeSoFar > 0 ) [[likely]] {
		assert( stateOfBackendState->sizeSoFar >= globalState->blockSize );
		const uint8_t *prevData = globalState->buffer + ( stateOfBackendState->sizeSoFar - globalState->blockSize );
		if( std::memcmp( prevData, blockData, submittedBlockSize ) != 0 ) {
			if( stateOfBackendState->sizeSoFar + globalState->blockSize <= globalState->capacity ) {
				updateBinding = true;
			}
		}
	} else {
		updateBinding = true;
	}

	if( updateBinding ) {
		backendState->actionTape->bindBufferRange( GL_UNIFORM_BUFFER, binding, globalState->id,
												   stateOfBackendState->sizeSoFar, globalState->blockSize );
		stateOfBackendState->sizeSoFar += globalState->blockSize;
	}
}

void RB_DrawMesh( BackendState *backendState, const FrontendToBackendShared *fsh, int vboId, const VboSpanLayout *layout, const DrawMeshVertSpan *drawMeshVertSpan, int primitive ) {
	const mesh_vbo_s *vbo = RB_BindVBO( backendState, vboId );

	if( !layout ) [[likely]] {
		layout = &vbo->layout;
	}

	backendState->draw.currentVAttribs &= ~VATTRIB_INSTANCES_BITS;

	assert( backendState->material.currentShader );

	backendState->gl.enableVertexAttribs( backendState->draw.currentVAttribs, layout );

	if( backendState->global.wireframe ) {
		RB_DrawWireframeMesh( backendState, fsh, drawMeshVertSpan, primitive );
	} else {
		RB_DrawShadedMesh( backendState, fsh, drawMeshVertSpan, primitive );
	}
}

void RB_SetCamera( BackendState *backendState, const vec3_t cameraOrigin, const mat3_t cameraAxis ) {
	VectorCopy( cameraOrigin, backendState->global.cameraOrigin );
	Matrix3_Copy( cameraAxis, backendState->global.cameraAxis );
}

void RB_SetRenderFlags( BackendState *backendState, int flags ) {
	backendState->global.renderFlags = flags;
}

bool RB_EnableWireframe( BackendState *backendState, bool enable ) {
	const bool oldVal = backendState->global.wireframe;

	if( backendState->global.wireframe != enable ) {
		backendState->global.wireframe = enable;

		if( enable ) {
			RB_SetShaderStateMask( backendState, 0, GLSTATE_NO_DEPTH_TEST );
			backendState->actionTape->polygonMode( GL_FRONT_AND_BACK, GL_LINE );
		} else {
			RB_SetShaderStateMask( backendState, ~0, 0 );
			backendState->actionTape->polygonMode( GL_FRONT_AND_BACK, GL_FILL );
		}
	}

	return oldVal;
}

static void RB_UpdateVertexAttribs( BackendState *backendState ) {
	vattribmask_t vattribs = backendState->material.currentShader->vattribs;
	if( backendState->draw.superLightStyle ) {
		vattribs |= backendState->draw.superLightStyle->vattribs;
	}
	if( backendState->draw.bonesData.numBones ) {
		vattribs |= VATTRIB_BONES_BITS;
	}
	if( backendState->material.currentEntity->outlineHeight != 0.0f ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( DRAWFLAT( backendState ) ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	if( backendState->draw.currentShadowBits && ( backendState->material.currentModelType == mod_brush ) ) {
		vattribs |= VATTRIB_NORMAL_BIT;
	}
	backendState->draw.currentVAttribs = vattribs;
}

void RB_BindShader( BackendState *backendState, const entity_t *e, const ShaderParams *overrideParams,
					const ShaderParamsTable *paramsTable, const shader_t *shader,
					const mfog_t *fog, const portalSurface_s *portalSurface ) {
	backendState->material.currentShader = shader;
	backendState->material.fog = fog;
	backendState->material.texFog = backendState->material.colorFog = NULL;

	backendState->draw.doneDepthPass = false;
	backendState->draw.dirtyUniformState = true;

	backendState->material.currentEntity = e ? e : &backendState->global.nullEnt;

	backendState->material.currentModelType = backendState->material.currentEntity->rtype == RT_MODEL &&
		backendState->material.currentEntity->model ? backendState->material.currentEntity->model->type : mod_bad;

	backendState->draw.currentDlightBits = 0;
	backendState->draw.currentShadowBits = 0;
	backendState->draw.superLightStyle = nullptr;

	backendState->draw.bonesData.numBones = 0;
	backendState->draw.bonesData.maxWeights = 0;

	backendState->material.currentPortalSurface = portalSurface;

	if( !e ) {
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			backendState->material.currentShaderTime = 1e-3 * (double)materialParams->shaderTime;
			backendState->material.currentShaderFrac = materialParams->shaderFrac;
		} else {
			backendState->material.currentShaderTime = 1e-3 * (double)backendState->global.nullEnt.shaderTime;
			backendState->material.currentShaderFrac = 0.0f;
		}
		backendState->material.alphaHack = false;
		backendState->material.greyscale = false;
		backendState->material.noDepthTest = false;
		backendState->material.noColorWrite =  false;
		backendState->material.depthEqual = false;
	} else {
		Vector4Copy( backendState->material.currentEntity->shaderRGBA, backendState->material.entityColor );
		Vector4Copy( backendState->material.currentEntity->outlineColor, backendState->material.entityOutlineColor );
		int64_t givenShaderTime;
		if( const ShaderParams::Material *materialParams = ShaderParams::getMaterialParams( overrideParams, paramsTable ) ) {
			givenShaderTime = materialParams->shaderTime;
			backendState->material.currentShaderFrac = materialParams->shaderFrac;
		} else {
			givenShaderTime = backendState->material.currentEntity->shaderTime;
			backendState->material.currentShaderFrac = 0.0f;
		}
		if( givenShaderTime > backendState->global.time ) {
			backendState->material.currentShaderTime = 0;
		} else {
			backendState->material.currentShaderTime = 1e-3 * (double)( backendState->global.time - givenShaderTime );
		}
		backendState->material.alphaHack = e->renderfx & RF_ALPHAHACK ? true : false;
		backendState->material.hackedAlpha = (float)e->shaderRGBA[3] / 255.0f;
		backendState->material.greyscale = e->renderfx & RF_GREYSCALE ? true : false;
		backendState->material.noDepthTest = e->renderfx & RF_NODEPTHTEST && e->rtype == RT_SPRITE ? true : false;
		backendState->material.noColorWrite = e->renderfx & RF_NOCOLORWRITE ? true : false;
		backendState->material.depthEqual = backendState->material.alphaHack && ( e->renderfx & RF_WEAPONMODEL );
	}

	if( fog && fog->shader && !backendState->material.noColorWrite ) {
		// should we fog the geometry with alpha texture or scale colors?
		if( !backendState->material.alphaHack && Shader_UseTextureFog( shader ) ) {
			backendState->material.texFog = fog;
		} else {
			// use scaling of colors
			backendState->material.colorFog = fog;
		}
	}

	RB_UpdateVertexAttribs( backendState );
}

void RB_SetLightstyle( BackendState *backendState, const superLightStyle_t *lightStyle ) {
	assert( backendState->material.currentShader != NULL );
	backendState->draw.superLightStyle = lightStyle;
	backendState->draw.dirtyUniformState = true;

	RB_UpdateVertexAttribs( backendState );
}

void RB_SetDlightBits( BackendState *backendState, unsigned dlightBits ) {
	assert( backendState->material.currentShader != NULL );
	backendState->draw.currentDlightBits = dlightBits;
	backendState->draw.dirtyUniformState = true;
}

void RB_SetBonesData( BackendState *backendState, int numBones, dualquat_t *dualQuats, int maxWeights ) {
	assert( backendState->material.currentShader != NULL );

	if( numBones > MAX_GLSL_UNIFORM_BONES ) {
		numBones = MAX_GLSL_UNIFORM_BONES;
	}
	if( maxWeights > 4 ) {
		maxWeights = 4;
	}

	backendState->draw.bonesData.numBones = numBones;
	memcpy( backendState->draw.bonesData.dualQuats, dualQuats, numBones * sizeof( *dualQuats ) );
	backendState->draw.bonesData.maxWeights = maxWeights;

	backendState->draw.dirtyUniformState = true;

	RB_UpdateVertexAttribs( backendState );
}

void RB_SetZClip( BackendState *backendState, float zNear, float zFar ) {
	backendState->global.zNear = zNear;
	backendState->global.zFar = zFar;
}

void RB_SetLightParams( BackendState *backendState, float minLight, bool noWorldLight, float hdrExposure ) {
	backendState->global.minLight = minLight;
	backendState->global.noWorldLight = noWorldLight;
	backendState->global.hdrExposure = hdrExposure;
}

void RB_SetShaderStateMask( BackendState *backendState, unsigned ANDmask, unsigned ORmask ) {
	backendState->global.shaderStateANDmask = ANDmask;
	backendState->global.shaderStateORmask  = ORmask;
}

void R_SubmitAliasSurfToBackend( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceAlias_t *drawSurf ) {
	const maliasmesh_t *aliasmesh = drawSurf->mesh;

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 1u * aliasmesh->numverts,
		.firstElem = 0, .numElems = 3u * aliasmesh->numtris,
	};

	RB_DrawMesh( backendState, fsh, aliasmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
}

void R_SubmitSkeletalSurfToBackend( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceSkeletal_t *drawSurf ) {
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
			RB_DrawMesh( backendState, fsh, skmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
			return;
		}
	}

	if( bonePoseRelativeDQ && skmesh->vbo ) {
		// another fastpath: transform the initial pose on the GPU
		RB_SetBonesData( backendState, skmodel->numbones, bonePoseRelativeDQ, skmesh->maxWeights );
		RB_DrawMesh( backendState, fsh, skmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
		return;
	}
}

void R_SubmitBSPSurfToBackend( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceBSP_t *drawSurf ) {
	const MergedBspSurface *mergedBspSurf = drawSurf->mergedBspSurf;

	assert( !mergedBspSurf->numInstances );

	RB_SetDlightBits( backendState, drawSurf->dlightBits );
	RB_SetLightstyle( backendState, mergedBspSurf->superLightStyle );

	const DrawMeshVertSpan &drawMeshVertSpan = drawSurf->mdSpan;

	RB_DrawMesh( backendState, fsh, mergedBspSurf->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
}

void R_SubmitNullSurfToBackend( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * ) {
	assert( rsh.nullVBO != NULL );

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 6, .firstElem = 0, .numElems = 6,
	};

	RB_DrawMesh( backendState, fsh, rsh.nullVBO->index, nullptr, &drawMeshVertSpan, GL_LINES );
}

void R_SubmitDynamicMeshToBackend( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const DynamicMeshDrawSurface *drawSurface ) {
	// Protect against the case when fillMeshBuffers() produces zero vertices
	if( drawSurface->actualNumVertices && drawSurface->actualNumIndices ) {
		const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
			.firstVert = drawSurface->verticesOffset,
			.numVerts  = drawSurface->actualNumVertices,
			.firstElem = drawSurface->indicesOffset,
			.numElems  = drawSurface->actualNumIndices,
		};

		RB_DrawMesh( backendState, fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH ), nullptr, &drawMeshVertSpan, GL_TRIANGLES );
	}
}

void R_SubmitBatchedSurfsToBackend( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e,
									const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
									const shader_t *shader, const mfog_t *fog,
									const portalSurface_t *portalSurface, unsigned vertElemSpanIndex ) {
	const VertElemSpan &vertElemSpan = fsh->batchedVertElemSpans[vertElemSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		RB_BindShader( backendState, e, overrideParams, paramsTable, shader, fog, nullptr );
		const DrawMeshVertSpan drawMeshVertSpan = vertElemSpan;
		RB_DrawMesh( backendState, fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH ), nullptr, &drawMeshVertSpan, GL_TRIANGLES );
	}
}

void R_SubmitBatchedSurfsToBackendExt( BackendState *backendState, const FrontendToBackendShared *fsh, const entity_t *e,
									   const ShaderParams *, const ShaderParamsTable *paramsTable,
									   const shader_s *shader, const mfog_t *fog,
									   const portalSurface_t *portalSurface, unsigned vertElemSpanAndVboSpanIndex ) {
	const auto &[vertElemSpan, vboSpanLayout] = fsh->batchedVertElemAndVboSpans[vertElemSpanAndVboSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		RB_BindShader( backendState, e, nullptr, paramsTable, shader, fog, nullptr );
		const DrawMeshVertSpan drawMeshVertSpan = vertElemSpan;
		RB_DrawMesh( backendState, fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH_EXT ), &vboSpanLayout, &drawMeshVertSpan, GL_TRIANGLES );
	}
}