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

rbackend_t rb;

static void RB_RegisterStreamVBOs();

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

void RB_BeginUsingBackendState( SimulatedBackendState *backendState ) {
	for( unsigned binding = 0; binding < MAX_UNIFORM_BINDINGS; ++binding ) {
		R_BeginUniformUploads( binding );
	}

	// start fresh each frame
	backendState->setShaderStateMask( ~0, 0 );
	RB_BindVBO( backendState, 0 );
}

void RB_EndUsingBackendState( SimulatedBackendState *backendState ) {
	for( unsigned binding = 0; binding < MAX_UNIFORM_BINDINGS; ++binding ) {
		R_EndUniformUploads( binding, backendState->getCurrUniformDataSize( binding ) );
	}
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

mesh_vbo_s *RB_BindVBO( SimulatedBackendState *backendState, int id ) {
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

	// TODO: Split the code path properly so we don't have to pass the null backend state
	if( backendState ) {
		backendState->bindVbo( vbo );
	} else {
		qglBindBuffer( GL_ARRAY_BUFFER, vbo ? vbo->vertexId : 0 );
		qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo ? vbo->elemId : 0 );
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

void *RB_GetTmpUniformBlock( SimulatedBackendState *backendState, unsigned binding, size_t requestedBlockSize ) {
	assert( binding < std::size( rb.uniformUploads ) );

	const auto *const globalState = &rb.uniformUploads[binding];
	assert( std::abs( (int)requestedBlockSize - (int)globalState->blockSize ) < 16 );

	const unsigned sizeSoFar = backendState->getCurrUniformDataSize( binding );
	assert( ( sizeSoFar % globalState->blockSize ) == 0 );

	void *result;
	if( sizeSoFar + globalState->blockSize <= globalState->capacity ) [[likely]] {
		result = globalState->buffer + sizeSoFar;
	} else {
		result = globalState->lastResortScratchpad;
	}

	std::memset( result, 0, requestedBlockSize );
	return result;
}

void RB_CommitUniformBlock( SimulatedBackendState *backendState, unsigned binding, void *blockData, size_t submittedBlockSize ) {
	assert( binding < std::size( rb.uniformUploads ) );

	const auto *globalState = &rb.uniformUploads[binding];
	assert( std::abs( (int)submittedBlockSize - (int)globalState->blockSize ) < 16 );

	const unsigned sizeSoFar = backendState->getCurrUniformDataSize( binding );
	assert( ( sizeSoFar % globalState->blockSize ) == 0 );

	// TODO: The initial offset is going to be > 0 if mulitple uploads are performed in parallel (and we use slices)
	if( sizeSoFar > 0 ) [[likely]] {
		assert( sizeSoFar >= globalState->blockSize );
		const uint8_t *prevData = globalState->buffer + ( sizeSoFar - globalState->blockSize );
		if( std::memcmp( prevData, blockData, submittedBlockSize ) != 0 ) {
			if( sizeSoFar + globalState->blockSize <= globalState->capacity ) {
				backendState->registerUniformBlockUpdate( binding, globalState->id, globalState->blockSize );
			}
		}
	} else {
		backendState->registerUniformBlockUpdate( binding, globalState->id, globalState->blockSize );
	}
}

void R_SubmitAliasSurfToBackend( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceAlias_t *drawSurf ) {
	const maliasmesh_t *aliasmesh = drawSurf->mesh;

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 1u * aliasmesh->numverts,
		.firstElem = 0, .numElems = 3u * aliasmesh->numtris,
	};

	sbs->drawMesh( fsh, aliasmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
}

void R_SubmitSkeletalSurfToBackend( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceSkeletal_t *drawSurf ) {
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
			sbs->drawMesh( fsh, skmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
			return;
		}
	}

	if( bonePoseRelativeDQ && skmesh->vbo ) {
		// another fastpath: transform the initial pose on the GPU
		sbs->setBonesData( skmodel->numbones, bonePoseRelativeDQ, skmesh->maxWeights );
		sbs->drawMesh( fsh, skmesh->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
		return;
	}
}

void R_SubmitBSPSurfToBackend( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceBSP_t *drawSurf ) {
	const MergedBspSurface *mergedBspSurf = drawSurf->mergedBspSurf;

	assert( !mergedBspSurf->numInstances );

	sbs->setDlightBits( drawSurf->dlightBits );
	sbs->setLightstyle( mergedBspSurf->superLightStyle );

	const DrawMeshVertSpan &drawMeshVertSpan = drawSurf->mdSpan;

	sbs->drawMesh( fsh, mergedBspSurf->vbo->index, nullptr, &drawMeshVertSpan, GL_TRIANGLES );
}

void R_SubmitNullSurfToBackend( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * ) {
	assert( rsh.nullVBO );

	const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
		.firstVert = 0, .numVerts = 6, .firstElem = 0, .numElems = 6,
	};

	sbs->drawMesh( fsh, rsh.nullVBO->index, nullptr, &drawMeshVertSpan, GL_LINES );
}

void R_SubmitDynamicMeshToBackend( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const DynamicMeshDrawSurface *drawSurface ) {
	// Protect against the case when fillMeshBuffers() produces zero vertices
	if( drawSurface->actualNumVertices && drawSurface->actualNumIndices ) {
		const DrawMeshVertSpan drawMeshVertSpan = VertElemSpan {
			.firstVert = drawSurface->verticesOffset,
			.numVerts  = drawSurface->actualNumVertices,
			.firstElem = drawSurface->indicesOffset,
			.numElems  = drawSurface->actualNumIndices,
		};

		sbs->drawMesh( fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH ), nullptr, &drawMeshVertSpan, GL_TRIANGLES );
	}
}

void R_SubmitBatchedSurfsToBackend( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e,
									const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
									const shader_t *shader, const mfog_t *fog,
									const portalSurface_t *portalSurface, unsigned vertElemSpanIndex ) {
	const VertElemSpan &vertElemSpan = fsh->batchedVertElemSpans[vertElemSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		sbs->bindShader( e, overrideParams, paramsTable, shader, fog, nullptr );
		const DrawMeshVertSpan drawMeshVertSpan = vertElemSpan;
		sbs->drawMesh( fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH ), nullptr, &drawMeshVertSpan, GL_TRIANGLES );
	}
}

void R_SubmitBatchedSurfsToBackendExt( SimulatedBackendState *sbs, const FrontendToBackendShared *fsh, const entity_t *e,
									   const ShaderParams *, const ShaderParamsTable *paramsTable,
									   const shader_s *shader, const mfog_t *fog,
									   const portalSurface_t *portalSurface, unsigned vertElemSpanAndVboSpanIndex ) {
	const auto &[vertElemSpan, vboSpanLayout] = fsh->batchedVertElemAndVboSpans[vertElemSpanAndVboSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		sbs->bindShader( e, nullptr, paramsTable, shader, fog, nullptr );
		const DrawMeshVertSpan drawMeshVertSpan = vertElemSpan;
		sbs->drawMesh( fsh, RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH_EXT ), &vboSpanLayout, &drawMeshVertSpan, GL_TRIANGLES );
	}
}