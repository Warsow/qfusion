#ifndef WSW_d121e83d_6bbb_4645_847b_7d7ce21ab86a_H
#define WSW_d121e83d_6bbb_4645_847b_7d7ce21ab86a_H

#include "glimp.h"

#include <common/helpers/freelistallocator.h>

struct VboSpanLayout {
	vattribmask_t vertexAttribs;
	vattribmask_t halfFloatAttribs;

	unsigned baseOffset;
	unsigned instancesOffset;

	uint8_t vertexSize;

	uint8_t normalsOffset;
	uint8_t sVectorsOffset;
	uint8_t stOffset;
	uint8_t lmstOffset[( MAX_LIGHTMAPS + 1 ) / 2];
	uint8_t lmstSize[( MAX_LIGHTMAPS + 1 ) / 2];
	uint8_t lmlayersOffset[( MAX_LIGHTMAPS + 3 ) / 4];
	uint8_t colorsOffset[MAX_LIGHTMAPS];
	uint8_t bonesIndicesOffset;
	uint8_t bonesWeightsOffset;
	uint8_t spritePointsOffset;              // autosprite or autosprite2 centre + radius
};

struct MeshBuffer {
	GLuint vboId { 0 };
	GLuint iboId { 0 };
};

class BufferFactory {
public:
	[[nodiscard]]
	auto createVboAndIbo( size_t vertexDataSizeInBytes, size_t indexDataSizeInBytes )
	-> std::optional<std::pair<GLuint, GLuint>>;

	void destroyVboAndIbo( GLuint vbo, GLuint ibo );
};

class BufferCache {
	BufferCache();
	~BufferCache();
public:
	void touchMeshBuffer( MeshBuffer *buffer );
	void freeBuffersByTag( unsigned tag );
	void freeUnusedBuffers();
private:
	struct MeshBufferCacheEntry {
		MeshBuffer buffer;
		int registrationSequence { 0 };
		unsigned tag { 0 };
	};

	wsw::HeapBasedFreelistAllocator m_allocator;
	GLuint m_globalVao { 0 };
};



#endif