/*
Copyright (C) 2011 Victor Luchits
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

#ifndef WSW_d121e83d_6bbb_4645_847b_7d7ce21ab86a_H
#define WSW_d121e83d_6bbb_4645_847b_7d7ce21ab86a_H

#include "glimp.h"
#include "vattribs.h"

#include <common/types/podbuffer.h>
#include <common/helpers/freelistallocator.h>

struct mesh_s;

struct MeshBuffer {
	GLuint vboId { 0 };
	GLuint iboId { 0 };
};

struct UniformBuffer {
	GLuint id { 0 };
};

class BufferFactory {
	friend class BufferCache;
public:
	enum class Usage { Static, Dynamic };

	[[nodiscard]]
	auto createMeshBuffer( size_t vertexDataSizeInBytes, size_t indexDataSizeInBytes, Usage usage )
		-> std::optional<MeshBuffer>;
	void destroyMeshBuffer( MeshBuffer *buffer );

	[[nodiscard]]
	auto createUniformBuffer( size_t dataSizeInBytes ) -> std::optional<UniformBuffer>;
	void destroyUniformBuffer( UniformBuffer *buffer );

	void uploadVertexData( MeshBuffer *buffer, const void *data, size_t dataSize );
	void uploadIndexData( MeshBuffer *buffer, const void *data, size_t dataSize );
	void uploadVertexData( MeshBuffer *buffer, const VboSpanLayout *layout, int vertsOffset, vattribmask_t vattribs, const mesh_s *mesh );
	void uploadIndexData( MeshBuffer *buffer, int vertsOffset, int elemsOffset, const mesh_s *mesh );
	void uploadInstancesData( MeshBuffer *buffer, const VboSpanLayout *layout, int instOffset, int numInstances, instancePoint_t *instances );
	void uploadUniformData( UniformBuffer *buffer, const void *data, int offset, size_t dataSize );
private:
	BufferFactory();
	~BufferFactory();

	void bindAndUpload( GLenum target, GLuint id, size_t offset, size_t dataSize, const void *data );

	PodBuffer<uint8_t> m_tmpDataBuffer;

	// TODO: Get rid of the global VAO
	GLuint m_globalVao { 0 };
};

class BufferCache {
public:
	BufferCache();
	~BufferCache();

	void touchMeshBuffer( MeshBuffer *buffer );
	void freeBuffersByTag( unsigned tag );
	void freeUnusedBuffers();

	[[nodiscard]]
	auto createMeshBuffer( unsigned numVerts, unsigned numElems, unsigned numInstances, vattribmask_t vattribs,
						   unsigned tag, vattribmask_t halfFloatVattribs ) -> MeshBuffer *;

	[[nodiscard]]
	auto getLayoutForBuffer( const MeshBuffer *buffer ) -> const VboSpanLayout *;

	[[nodiscard]]
	auto getUnderlyingFactory() -> BufferFactory * { return &m_factory; }
private:
	struct MeshBufferCacheEntry {
		MeshBufferCacheEntry *prev { nullptr };
		MeshBufferCacheEntry *next { nullptr };
		MeshBuffer buffer;
		VboSpanLayout layout;
		mutable int registrationSequence { 0 };
		unsigned tag { 0 };
	};

	template <typename Pred>
	void freeBuffersByCondition( Pred &&pred );

	[[nodiscard]]
	auto getEntryForMeshBuffer( const MeshBuffer *buffer ) -> const MeshBufferCacheEntry *;

	BufferFactory m_factory;
	wsw::HeapBasedFreelistAllocator m_allocator;
	MeshBufferCacheEntry *m_entriesHead { nullptr };

};

[[nodiscard]]
auto buildVertexLayoutForVattribs( VboSpanLayout *layout, vattribmask_t vattribs, vattribmask_t halfFloatVattribs,
								   int numVerts, int numInstances ) -> size_t;

// TODO: We don't check, should we?
[[maybe_unused]]
auto fillMeshVertexData( const VboSpanLayout *layout, vattribmask_t vattribs, const mesh_s *mesh, void *outData ) -> vattribmask_t;

auto getBufferCache() -> BufferCache *;

void R_InitVBO();
void R_ShutdownVBO();

#endif