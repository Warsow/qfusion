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
#include <atomic>
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
	constexpr unsigned kMaxBlocksForBinding = 1 << 12;

	assert( std::size( m_uniformStreams ) == MAX_UNIFORM_BINDINGS );
	for( unsigned binding = 0; binding < MAX_UNIFORM_BINDINGS; ++binding ) {
		const unsigned blockDataSize   = sizeOfBlocks[binding];
		const unsigned sliceDataSize   = blockDataSize * ( kMaxBlocksForBinding + 1 );
		const unsigned uniformDataSize = sliceDataSize * kMaxUniformSlices;
		assert( uniformDataSize > 0 );

		UniformStream *stream = &m_uniformStreams[binding];
		stream->data.reserve( uniformDataSize );
		stream->blockSize = sizeOfBlocks[binding];

		for( unsigned sliceId = 0; sliceId < kMaxUniformSlices; ++sliceId ) {
			stream->sliceLayoutsAndStates[sliceId].initialOffset = sliceId * sliceDataSize;
			stream->sliceLayoutsAndStates[sliceId].currentOffset = sliceId * sliceDataSize;
			// We exclude the last resort scratchpad from capacity
			// Note: The capacity could vary if we decide to reduce it for aux draws
			stream->sliceLayoutsAndStates[sliceId].capacity = sliceDataSize - blockDataSize;
		}

		if( std::optional<UniformBuffer> buffer = m_bufferFactory->createUniformBuffer( uniformDataSize ) ) {
			stream->buffer = *buffer;
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

auto UploadManager::acquireUniformSlice( unsigned category ) -> unsigned {
	// There should not be noticeable contention/effects of false sharing of the two counters
	const auto getAndIncrement = []( unsigned *const value ) -> unsigned {
		assert( ( (uintptr_t)value % alignof( std::atomic<unsigned> ) ) == 0 );
		return ( ( std::atomic<unsigned> *)value )->fetch_add( 1 );
	};

	unsigned sliceId;
	if( category == CameraUniforms ) {
		sliceId = getAndIncrement( &m_totalCameraUniformRequests ) % kMaxCameraSlices;
	} else {
		sliceId = kMaxCameraSlices + getAndIncrement( &m_totalAuxDrawUniformRequests ) % kMaxAuxDrawSlices;
	}
	assert( sliceId < kMaxUniformSlices );

	// TODO: Some streams are really unused for aux draws
	for( UniformStream &stream: m_uniformStreams ) {
		UniformStream::SliceLayoutAndState *layoutAndState = &stream.sliceLayoutsAndStates[sliceId];
		layoutAndState->currentOffset = layoutAndState->initialOffset;
	}

	return sliceId;
}

void UploadManager::commitUniformSlice( unsigned sliceId ) {
	assert( sliceId < kMaxUniformSlices );
	for( UniformStream &stream: m_uniformStreams ) {
		const UniformStream::SliceLayoutAndState &layoutAndState = stream.sliceLayoutsAndStates[sliceId];
		if( layoutAndState.currentOffset != layoutAndState.initialOffset ) {
			assert( layoutAndState.currentOffset > layoutAndState.initialOffset );
			const unsigned sizeInBytes = layoutAndState.currentOffset - layoutAndState.initialOffset;
			assert( ( sizeInBytes % stream.blockSize ) == 0 );
			assert( sizeInBytes <= layoutAndState.capacity );
			m_bufferFactory->uploadUniformData( &stream.buffer, stream.data.get() + layoutAndState.initialOffset,
												layoutAndState.initialOffset, sizeInBytes );
		}
	}
}

void UploadManager::getBufferAndRangeForBindingAndSlice( unsigned binding, unsigned sliceId, GLuint *bufferId,
														 unsigned *offset, unsigned *size ) const {
	assert( binding < MAX_UNIFORM_BINDINGS );
	assert( sliceId < kMaxUniformSlices );
	const UniformStream &stream = m_uniformStreams[binding];

	*bufferId = stream.buffer.id;
	*offset   = stream.sliceLayoutsAndStates[sliceId].initialOffset;
	*size     = stream.blockSize;
}

auto UploadManager::allocUniformBlock( SimulatedBackendState *backendState, unsigned binding, size_t requestedBlockSize ) -> void * {
	assert( binding < std::size( m_uniformStreams ) );

	UniformStream *const streamState = &m_uniformStreams[binding];
	assert( std::abs( (int)requestedBlockSize - (int)streamState->blockSize ) < 16 );

	const unsigned sliceId = backendState->getUniformSliceId();
	assert( sliceId < kMaxUniformSlices );

	const UniformStream::SliceLayoutAndState &sliceLayoutAndState = m_uniformStreams[binding].sliceLayoutsAndStates[sliceId];

	const unsigned initialOffset = sliceLayoutAndState.initialOffset;
	const unsigned currentOffset = sliceLayoutAndState.currentOffset;
	const unsigned sizeSoFar     = currentOffset - initialOffset;
	assert( ( sizeSoFar % streamState->blockSize ) == 0 );
	assert( ( sizeSoFar <= sliceLayoutAndState.capacity ) );
	(void)sizeSoFar;

	// Assume that the last resort scratchpad is located after just after the maximal valid block.
	// Note: It is not very good if we consider (persistent) data mapping, not copying
	void *result = streamState->data.get() + currentOffset;
	std::memset( result, 0, requestedBlockSize );
	return result;
}

void UploadManager::commitUniformBlock( SimulatedBackendState *backendState, unsigned binding, void *blockData, size_t submittedBlockSize ) {
	assert( binding < std::size( m_uniformStreams ) );

	const UniformStream *const streamState = &m_uniformStreams[binding];
	assert( std::abs( (int)submittedBlockSize - (int)streamState->blockSize ) < 16 );

	const unsigned sliceId = backendState->getUniformSliceId();
	assert( sliceId < kMaxUniformSlices );

	UniformStream::SliceLayoutAndState *sliceLayoutAndState = &m_uniformStreams[binding].sliceLayoutsAndStates[sliceId];

	const unsigned initialOffset  = sliceLayoutAndState->initialOffset;
	const unsigned sliceCapacity  = sliceLayoutAndState->capacity;
	unsigned *const currentOffset = &sliceLayoutAndState->currentOffset;
	const unsigned sizeSoFar      = *currentOffset - initialOffset;
	assert( ( initialOffset % streamState->blockSize ) == 0 );
	assert( ( sizeSoFar % streamState->blockSize ) == 0 );
	assert( ( sizeSoFar <= sliceCapacity ) );

	assert( blockData == streamState->data.get() + *currentOffset );
	if( sizeSoFar > 0 ) [[likely]] {
		const uint8_t *prevData = streamState->data.get() + *currentOffset - streamState->blockSize;
		if( std::memcmp( prevData, blockData, submittedBlockSize ) != 0 ) {
			if( sizeSoFar + streamState->blockSize <= sliceCapacity ) {
				backendState->bindUniformBlock( binding, streamState->buffer.id, *currentOffset, streamState->blockSize );
				*currentOffset += streamState->blockSize;
			}
		}
	} else {
		backendState->bindUniformBlock( binding, streamState->buffer.id, *currentOffset, streamState->blockSize );
		*currentOffset += streamState->blockSize;
	}
}