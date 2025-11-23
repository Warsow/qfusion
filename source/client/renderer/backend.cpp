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
#include "backendstate.h"
#include "backendactiontape.h"
#include "glstateproxy.h"
#include "common/facilities/cvar.h"
#include <common/helpers/memspecbuilder.h>

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

static rbackend_t rb;

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

void RB_BeginRegistration() {
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