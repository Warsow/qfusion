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

#ifndef WSW_57b9f0ca_e4d3_40c7_931f_ab1c89596c76_H
#define WSW_57b9f0ca_e4d3_40c7_931f_ab1c89596c76_H

#include "buffermanagement.h"
#include "vattribs.h"
#include "local.h"

struct SimulatedBackendState;
struct UniformBlockOffsets;
struct mesh_s;

class UploadManager {
public:
	explicit UploadManager( BufferFactory *bufferFactory );
	~UploadManager();

	enum UploadGroup : unsigned {
		DynamicMesh,
		BatchedMesh,
		BatchedMeshExt,
		Mesh2D,
		DebugMesh,
	};

	[[nodiscard]]
	auto getMeshBuffer( UploadGroup group ) const -> const MeshBuffer *;
	[[nodiscard]]
	auto getVboSpanLayout( UploadGroup group ) const -> const VboSpanLayout *;
	[[nodiscard]]
	auto getCapacityInBytes( UploadGroup group ) const -> unsigned;
	[[nodiscard]]
	auto getCapacityInVertices( UploadGroup group ) const -> unsigned;
	[[nodiscard]]
	auto getCapacityInIndices( UploadGroup group ) const -> unsigned;

	void beginVertexUploads( UploadGroup group );
	void setUploadedSubdataFromMeshUsingOffsets( UploadGroup group, unsigned baseVertex, unsigned verticesOffsetInBytes,
												 unsigned indicesOffsetInBytes, const mesh_s *mesh );
	void setUploadedSubdataFromMeshUsingLayout( UploadGroup group, unsigned baseVertex, const VboSpanLayout *layout,
												unsigned indexOfFirstIndex, const mesh_s *mesh );
	void endVertexUploads( UploadGroup group, unsigned vertexDataSizeInBytes, unsigned indexDataSizeInBytes );

	// Note: UploadManager does not fully manage bindings, hence they're passed as unsigned, contrary to groups

	[[nodiscard]]
	auto allocUniformBlock( SimulatedBackendState *backendState, unsigned binding, size_t requestedBlockSize ) -> void *;
	void commitUniformBlock( SimulatedBackendState *backendState, unsigned binding, void *blockData, size_t blockSize );

	enum UniformUploadCategory : unsigned {
		CameraUniforms,
		AuxDrawUniforms,
	};

	[[nodiscard]]
	auto acquireUniformSlice( unsigned category ) -> unsigned;
	// TODO: Don't commit individually but specify a span of slices in order to
	// reduce buffer rebinding and, possibly, consolidate uploads
	void commitUniformSlice( unsigned sliceId );

	void getBufferAndRangeForBindingAndSlice( unsigned binding, unsigned sliceId, GLuint *bufferId,
											  unsigned *offset, unsigned *size ) const;

private:
	void destroy();

	BufferFactory *const m_bufferFactory;

	struct VertexStream {
		MeshBuffer buffer;
		VboSpanLayout layout;
		PodBuffer<uint8_t> vboData;
		PodBuffer<uint8_t> iboData;
		unsigned vboCapacityInVerts { 0 };
	};

	VertexStream m_vertexStreams[5];

	static constexpr unsigned kMaxCameraSlices  = MAX_REF_CAMERAS;
	static constexpr unsigned kMaxAuxDrawSlices = 16;
	static constexpr unsigned kMaxUniformSlices = kMaxCameraSlices + kMaxAuxDrawSlices;

	struct UniformStream {
		UniformBuffer buffer;
		PodBuffer<uint8_t> data;
		struct SliceLayoutAndState {
			unsigned initialOffset { 0 };
			unsigned currentOffset { 0 };
			unsigned capacity { 0 };
		};
		SliceLayoutAndState sliceLayoutsAndStates[kMaxUniformSlices];
		unsigned blockSize { 0 };
	};

	unsigned m_totalCameraUniformRequests { 0 };
	unsigned m_totalAuxDrawUniformRequests { 0 };

	UniformStream m_uniformStreams[MAX_UNIFORM_BINDINGS];
};



#endif
