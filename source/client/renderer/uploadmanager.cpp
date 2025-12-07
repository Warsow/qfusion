/*
Copyright (C) 2002-2011 Victor Luchits
Copyright (C) 2025 vvk2212, Chasseur de bots

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

#include <common/helpers/scopeexitaction.h>
#include <utility>

#include "uploadmanager.h"
#include "backendstate.h"
#include "program.h"
#include "local.h"

UploadManager::UploadManager( BufferFactory *bufferFactory ) : m_bufferFactory( bufferFactory ) {
	[[maybe_unused]] wsw::ScopeExitAction cleanup( [this]() { this->destroy(); } );

	for( VertexStream &vertexStream: m_vertexStreams ) {
		const auto group = std::addressof( vertexStream ) - m_vertexStreams;
		vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_COLOR0_BIT | VATTRIB_TEXCOORDS_BIT;
		if( group != Mesh2D && group != DebugMesh ) {
			vattribs |= VATTRIB_NORMAL_BIT;
		}
		unsigned capacityInVerts;
		if( group != BatchedMeshExt && group != Mesh2D ) {
			capacityInVerts = ( 1 << 16 ) - 1;
		} else {
			// We don't need that much for sprites and 2D stuff
			capacityInVerts = ( group == Mesh2D ) ? ( 5 * 4096 ) : 4096;
		}
		assert( capacityInVerts > 0 );
		const size_t vertexDataSize = buildVertexLayoutForVattribs( &vertexStream.layout, vattribs, 0, capacityInVerts, 0 );
		const size_t indexDataSize  = sizeof( elem_t ) * 6 * capacityInVerts;
		vertexStream.vboData.reserve( vertexDataSize );
		vertexStream.iboData.reserve( indexDataSize );
		vertexStream.vboCapacityInVerts = capacityInVerts;
		std::optional<MeshBuffer> buffer = m_bufferFactory->createMeshBuffer( vertexDataSize, indexDataSize,
																			  BufferFactory::Usage::Dynamic );
		if( buffer ) {
			vertexStream.buffer = *buffer;
		} else {
			wsw::failWithRuntimeError( "Failed to create a stream mesh buffer" );
		}
	}

	unsigned sizeOfBlocks[MAX_UNIFORM_BINDINGS];
	RP_GetSizeOfUniformBlocks( sizeOfBlocks );

	// TODO: Vary it depending of actual kind of data
	constexpr unsigned kMaxBlocksForBinding = 1 << 14;

	assert( std::size( m_uniformStreams ) == MAX_UNIFORM_BINDINGS );
	for( unsigned i = 0; i < MAX_UNIFORM_BINDINGS; ++i ) {
		const unsigned uniformDataSize = sizeOfBlocks[i] * kMaxBlocksForBinding;
		assert( uniformDataSize > 0 );

		UniformStream &stream = m_uniformStreams[i];
		stream.data.reserve( uniformDataSize );
		stream.blockSize = sizeOfBlocks[i];

		if( std::optional<UniformBuffer> buffer = m_bufferFactory->createUniformBuffer( uniformDataSize ) ) {
			stream.buffer = *buffer;
		} else {
			wsw::failWithRuntimeError( "Failed to create a uniform buffer" );
		}
	}

	cleanup.cancel();
}

UploadManager::~UploadManager() {
	destroy();
}

void UploadManager::destroy() {
	for( VertexStream &vertexStream: m_vertexStreams ) {
		m_bufferFactory->destroyMeshBuffer( &vertexStream.buffer );
	}
	for( UniformStream &uniformStream: m_uniformStreams ) {
		m_bufferFactory->destroyUniformBuffer( &uniformStream.buffer );
	}
}

auto UploadManager::getMeshBuffer( UploadGroup group ) const -> const MeshBuffer *{
	assert( group < std::size( m_vertexStreams ) );
	return &m_vertexStreams[group].buffer;
}

auto UploadManager::getVboSpanLayout( UploadGroup group ) const -> const VboSpanLayout * {
	assert( group < std::size( m_vertexStreams ) );
	return &m_vertexStreams[group].layout;
}

auto UploadManager::getCapacityInBytes( UploadGroup group ) const -> unsigned {
	assert( group < std::size( m_vertexStreams ) );
	return m_vertexStreams[group].vboData.capacity();
}

auto UploadManager::getCapacityInVertices( UploadGroup group ) const -> unsigned {
	assert( group < std::size( m_vertexStreams ) && group != BatchedMeshExt );
	return m_vertexStreams[group].vboCapacityInVerts;
}

auto UploadManager::getCapacityInIndices( UploadGroup group ) const -> unsigned {
	assert( group < std::size( m_vertexStreams ) );
	return m_vertexStreams[group].iboData.capacity() / sizeof( elem_t );
}

void UploadManager::beginVertexUploads( UploadGroup group ) {
	assert( group < std::size( m_vertexStreams ) );
}

void UploadManager::setUploadedSubdataFromMeshUsingOffsets( UploadGroup group, unsigned baseVertex,
															unsigned verticesOffsetInBytes,
															unsigned indicesOffsetInBytes, const mesh_t *mesh ) {
	assert( group < std::size( m_vertexStreams ) );
	if( mesh->numVerts && mesh->numElems ) {
		VertexStream &streamState = m_vertexStreams[group];

		auto *const destVertexData = streamState.vboData.get() + verticesOffsetInBytes;
		auto *const destIndexData  = (uint16_t *)( streamState.iboData.get() + indicesOffsetInBytes );

		fillMeshVertexData( &streamState.layout, streamState.layout.vertexAttribs, mesh, destVertexData );
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + baseVertex;
		}
	}
}

void UploadManager::setUploadedSubdataFromMeshUsingLayout( UploadGroup group, unsigned baseVertex,
														   const VboSpanLayout *layout,
														   unsigned indexOfFirstIndex, const mesh_t *mesh ) {
	assert( group < std::size( m_vertexStreams ) );
	if( mesh->numVerts && mesh->numElems ) {
		VertexStream &streamState = m_vertexStreams[group];

		// Note: The data offset gets provided by the layout itself
		fillMeshVertexData( layout, layout->vertexAttribs, mesh, streamState.vboData.get() );

		auto *const destIndexData  = ( (uint16_t *)streamState.iboData.get() ) + indexOfFirstIndex;
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + baseVertex;
		}
	}
}

void UploadManager::endVertexUploads( UploadGroup group, unsigned vertexDataSizeInBytes, unsigned indexDataSizeInBytes ) {
	assert( group < std::size( m_vertexStreams ) );
	if( vertexDataSizeInBytes && indexDataSizeInBytes ) {
		VertexStream &streamState = m_vertexStreams[group];
		m_bufferFactory->uploadVertexData( &streamState.buffer, streamState.vboData.get(), vertexDataSizeInBytes );
		m_bufferFactory->uploadIndexData( &streamState.buffer, streamState.iboData.get(), indexDataSizeInBytes );
	}
}

void UploadManager::beginUniformUploads( unsigned binding ) {
	assert( binding < std::size( m_uniformStreams ) );
}

void UploadManager::endUniformUploads( unsigned binding, unsigned sizeInBytes ) {
	assert( binding < std::size( m_uniformStreams ) );

	if( sizeInBytes > 0 ) {
		UniformStream &streamState = m_uniformStreams[binding];
		assert( sizeInBytes <= streamState.data.capacity() );
		assert( ( sizeInBytes % streamState.blockSize ) == 0 );

		m_bufferFactory->uploadUniformData( &streamState.buffer, streamState.data.get(), sizeInBytes );
	}
}

auto UploadManager::allocUniformBlock( SimulatedBackendState *backendState, unsigned binding, size_t requestedBlockSize ) -> void * {
	assert( binding < std::size( m_uniformStreams ) );

	UniformStream *const streamState = &m_uniformStreams[binding];
	assert( std::abs( (int)requestedBlockSize - (int)streamState->blockSize ) < 16 );

	const unsigned sizeSoFar = backendState->getCurrUniformDataSize( binding );
	assert( ( sizeSoFar % streamState->blockSize ) == 0 );

	void *result;
	if( sizeSoFar + streamState->blockSize <= streamState->data.capacity() ) [[likely]] {
		result = streamState->data.get() + sizeSoFar;
	} else {
		result = streamState->lastResortScratchpad.reserveAndGet( streamState->blockSize );
	}

	std::memset( result, 0, requestedBlockSize );
	return result;
}

void UploadManager::commitUniformBlock( SimulatedBackendState *backendState, unsigned binding, void *blockData, size_t submittedBlockSize ) {
	assert( binding < std::size( m_uniformStreams ) );

	const UniformStream *const streamState = &m_uniformStreams[binding];
	assert( std::abs( (int)submittedBlockSize - (int)streamState->blockSize ) < 16 );

	const unsigned sizeSoFar = backendState->getCurrUniformDataSize( binding );
	assert( ( sizeSoFar % streamState->blockSize ) == 0 );

	// TODO: The initial offset is going to be > 0 if mulitple uploads are performed in parallel (and we use slices)
	if( sizeSoFar > 0 ) [[likely]] {
		assert( sizeSoFar >= streamState->blockSize );
		const uint8_t *prevData = streamState->data.get() + ( sizeSoFar - streamState->blockSize );
		if( std::memcmp( prevData, blockData, submittedBlockSize ) != 0 ) {
			if( sizeSoFar + streamState->blockSize <= streamState->data.capacity() ) {
				backendState->registerUniformBlockUpdate( binding, streamState->buffer.id, streamState->blockSize );
			}
		}
	} else {
		backendState->registerUniformBlockUpdate( binding, streamState->buffer.id, streamState->blockSize );
	}
}