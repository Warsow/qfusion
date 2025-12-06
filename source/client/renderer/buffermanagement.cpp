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

#include "local.h"
#include "buffermanagement.h"

#include <common/helpers/half_float.h>
#include <common/helpers/links.h>

static BufferCache *g_instance;

void R_InitVBO() {
	assert( !g_instance );
	g_instance = new BufferCache;
}

void R_ShutdownVBO() {
	delete g_instance;
	g_instance = nullptr;
}

auto getBufferCache() -> BufferCache * {
	return g_instance;
}

BufferCache::BufferCache()
	: m_allocator( sizeof( MeshBufferCacheEntry ), 4 * 4096 ) {
}

BufferCache::~BufferCache() {
	freeBuffersByCondition( []( MeshBufferCacheEntry * ) { return true; } );
}

template <typename Pred>
void BufferCache::freeBuffersByCondition( Pred &&pred ) {
	bool unboundFirst = false;
	for( MeshBufferCacheEntry *entry = m_entriesHead, *next; entry; entry = next ) { next = entry->next;
		if( pred( entry ) ) {
			if( !unboundFirst ) {
				qglBindBuffer( GL_ARRAY_BUFFER, 0 );
				qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, 0 );
				unboundFirst = true;
			}
			wsw::unlink( entry, &m_entriesHead );
			m_factory.destroyMeshBuffer( &entry->buffer );
			entry->~MeshBufferCacheEntry();
			m_allocator.free( entry );
		}
	}
}

auto BufferCache::getEntryForMeshBuffer( const MeshBuffer *buffer ) -> const MeshBufferCacheEntry * {
	assert( m_allocator.mayOwn( buffer ) );
	return (const MeshBufferCacheEntry *)( (uintptr_t)buffer - offsetof( MeshBufferCacheEntry, buffer ) );
}

auto BufferCache::getLayoutForBuffer( const MeshBuffer *buffer ) -> const VboSpanLayout * {
	return &getEntryForMeshBuffer( buffer )->layout;
}

void BufferCache::touchMeshBuffer( MeshBuffer *buffer ) {
	getEntryForMeshBuffer( buffer )->registrationSequence = rsh.registrationSequence;
}

void BufferCache::freeBuffersByTag( unsigned tag ) {
	freeBuffersByCondition( [&]( const MeshBufferCacheEntry *entry ) { return entry->tag == tag; } );
}

void BufferCache::freeUnusedBuffers() {
	freeBuffersByCondition( []( const MeshBufferCacheEntry *entry ) {
		return entry->registrationSequence != rsh.registrationSequence;
	});
}

auto BufferCache::createMeshBuffer( unsigned numVerts, unsigned numElems, unsigned numInstances, vattribmask_t vattribs,
									unsigned tag, vattribmask_t halfFloatVattribs ) -> MeshBuffer * {
	if( void *const mem = m_allocator.allocOrNull() ) [[likely]] {
		if( !( halfFloatVattribs & VATTRIB_POSITION_BIT ) ) {
			halfFloatVattribs &= ~( VATTRIB_AUTOSPRITE_BIT );
		}

		halfFloatVattribs &= ~VATTRIB_COLORS_BITS;
		halfFloatVattribs &= ~VATTRIB_BONES_BITS;

		// TODO: convert quaternion component of instance_t to half-float
		// when uploading instances data
		halfFloatVattribs &= ~VATTRIB_INSTANCES_BITS;

		auto *newEntry = new( mem )MeshBufferCacheEntry;

		const unsigned indexDataSize  = numElems * sizeof( elem_t );
		const unsigned vertexDataSize = buildVertexLayoutForVattribs( &newEntry->layout, vattribs,
																	  halfFloatVattribs, numVerts, numInstances );

		const auto usage = tag != VBO_TAG_STREAM ? BufferFactory::Usage::Static : BufferFactory::Usage::Dynamic;
		if( std::optional<MeshBuffer> buffer = m_factory.createVboAndIbo( vertexDataSize, indexDataSize, usage ) ) {
			newEntry->buffer               = *buffer;
			newEntry->registrationSequence = rsh.registrationSequence;
			newEntry->tag                  = tag;

			wsw::link( newEntry, &m_entriesHead );
			return &newEntry->buffer;
		}

		newEntry->~MeshBufferCacheEntry();
		m_allocator.free( mem );
	}

	return nullptr;
}

BufferFactory::BufferFactory() {
	// TODO: Like we said, get rid of the global VAO
	while( qglGetError() != GL_NO_ERROR ) {}
	qglGenVertexArrays( 1, &m_globalVao );
	qglBindVertexArray( m_globalVao );
	if( qglGetError() != GL_NO_ERROR ) {
		wsw::failWithRuntimeError( "Failed to create the global VAO" );
	}
}

BufferFactory::~BufferFactory() {
	qglBindVertexArray( 0 );
	qglDeleteVertexArrays( 1, &m_globalVao );
}

auto BufferFactory::createVboAndIbo( size_t vertexDataSizeInBytes, size_t indexDataSizeInBytes, Usage usage )
	-> std::optional<MeshBuffer> {
	const GLenum glUsage = ( usage == Usage::Static ) ? GL_STATIC_DRAW : GL_DYNAMIC_DRAW;

	while( qglGetError() != GL_NO_ERROR ) {}

	GLuint buffers[2] { 0, 0 };
	qglGenBuffers( 2, buffers );
	if( qglGetError() != GL_NO_ERROR ) {
		return std::nullopt;
	}

	qglBindBuffer( GL_ARRAY_BUFFER, buffers[0] );
	qglBufferData( GL_ARRAY_BUFFER, vertexDataSizeInBytes, nullptr, glUsage );

	qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, buffers[1] );
	qglBufferData( GL_ELEMENT_ARRAY_BUFFER, indexDataSizeInBytes, nullptr, glUsage );

	if( qglGetError() != GL_NO_ERROR ) {
		qglDeleteBuffers( 2, buffers );
	}

	return std::optional( MeshBuffer { .vboId = buffers[0], .iboId = buffers[1] } );
}

void BufferFactory::destroyMeshBuffer( MeshBuffer *buffer ) {
	GLuint buffers[2];
	GLsizei numBuffers = 0;
	if( buffer->vboId ) {
		buffers[numBuffers++] = buffer->vboId;
		buffer->vboId = 0;
	}
	if( buffer->iboId ) {
		buffers[numBuffers++] = buffer->iboId;
		buffer->vboId = 0;
	}
	qglDeleteBuffers( numBuffers, buffers );
}

void BufferFactory::uploadVertexData( MeshBuffer *vbo, const VboSpanLayout *layout, int vertsOffset, vattribmask_t vattribs, const mesh_t *mesh ) {
	void *data = m_tmpDataBuffer.reserveAndGet( mesh->numVerts * layout->vertexSize );
	// TODO: Looks like harmless, check what's up anyway
	(void)fillMeshVertexData( layout, vattribs, mesh, data );
	qglBindBuffer( GL_ARRAY_BUFFER, vbo->vboId );
	qglBufferSubData( GL_ARRAY_BUFFER, vertsOffset * layout->vertexSize, mesh->numVerts * layout->vertexSize, data );
}

void BufferFactory::uploadIndexData( MeshBuffer *vbo, int vertsOffset, int elemsOffset, const mesh_t *mesh ) {
	elem_t *ielems = mesh->elems;
	if( vertsOffset ) {
		ielems = (elem_t *)m_tmpDataBuffer.reserveAndGet( mesh->numElems * sizeof( elem_t ) );
		for( unsigned i = 0; i < mesh->numElems; i++ ) {
			ielems[i] = vertsOffset + mesh->elems[i];
		}
	}

	qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo->iboId );
	qglBufferSubData( GL_ELEMENT_ARRAY_BUFFER, elemsOffset * sizeof( elem_t ),
					  mesh->numElems * sizeof( elem_t ), ielems );
}

void BufferFactory::uploadInstancesData( MeshBuffer *vbo, const VboSpanLayout *layout, int instOffset,
										 int numInstances, instancePoint_t *instances ) {
	if( layout->instancesOffset ) {
		qglBindBuffer( GL_ARRAY_BUFFER, vbo->vboId );
		qglBufferSubData( GL_ARRAY_BUFFER,
						  layout->instancesOffset + instOffset * sizeof( instancePoint_t ),
						  numInstances * sizeof( instancePoint_t ), instances );
	}
}

auto buildVertexLayoutForVattribs( VboSpanLayout *layout, vattribmask_t vattribs, vattribmask_t halfFloatVattribs,
								   int numVerts, int numInstances ) -> size_t {
	memset( layout, 0, sizeof( VboSpanLayout ) );

	// vertex data
	unsigned vertexSize = 0;

	vertexSize += FLOAT_VATTRIB_SIZE( VATTRIB_POSITION_BIT, halfFloatVattribs ) * 4;

	// normals data
	if( vattribs & VATTRIB_NORMAL_BIT ) {
		assert( !( vertexSize & 3 ) );
		layout->normalsOffset = vertexSize;
		vertexSize += FLOAT_VATTRIB_SIZE( VATTRIB_NORMAL_BIT, halfFloatVattribs ) * 4;
	}

	// s-vectors (tangent vectors)
	if( vattribs & VATTRIB_SVECTOR_BIT ) {
		assert( !( vertexSize & 3 ) );
		layout->sVectorsOffset = vertexSize;
		vertexSize += FLOAT_VATTRIB_SIZE( VATTRIB_SVECTOR_BIT, halfFloatVattribs ) * 4;
	}

	// texture coordinates
	if( vattribs & VATTRIB_TEXCOORDS_BIT ) {
		assert( !( vertexSize & 3 ) );
		layout->stOffset = vertexSize;
		vertexSize += FLOAT_VATTRIB_SIZE( VATTRIB_TEXCOORDS_BIT, halfFloatVattribs ) * 2;
	}

	// lightmap texture coordinates
	vattribbit_t lmattrbit = VATTRIB_LMCOORDS0_BIT;
	for( int i = 0; i < ( MAX_LIGHTMAPS + 1 ) / 2; i++ ) {
		if( !( vattribs & lmattrbit ) ) {
			break;
		}
		assert( !( vertexSize & 3 ) );
		layout->lmstOffset[i] = vertexSize;
		layout->lmstSize[i] = ( vattribs & ( lmattrbit << 1 ) ) ? 4 : 2;
		vertexSize += FLOAT_VATTRIB_SIZE( VATTRIB_LMCOORDS0_BIT, halfFloatVattribs ) * layout->lmstSize[i];
		lmattrbit = ( vattribbit_t )( ( vattribmask_t )lmattrbit << 2 );
	}

	// lightmap array texture layers
	for( int i = 0; i < ( MAX_LIGHTMAPS + 3 ) / 4; i++ ) {
		if( !( vattribs & ( VATTRIB_LMLAYERS0123_BIT << i ) ) ) {
			break;
		}
		assert( !( vertexSize & 3 ) );
		layout->lmlayersOffset[i] = vertexSize;
		vertexSize += sizeof( int );
	}

	// vertex colors
	if( vattribs & VATTRIB_COLOR0_BIT ) {
		assert( !( vertexSize & 3 ) );
		layout->colorsOffset[0] = vertexSize;
		vertexSize += sizeof( int );
	}

	// bones data for skeletal animation
	if( ( vattribs & VATTRIB_BONES_BITS ) == VATTRIB_BONES_BITS ) {
		assert( SKM_MAX_WEIGHTS == 4 );

		assert( !( vertexSize & 3 ) );
		layout->bonesIndicesOffset = vertexSize;
		vertexSize += sizeof( int );

		assert( !( vertexSize & 3 ) );
		layout->bonesWeightsOffset = vertexSize;
		vertexSize += sizeof( int );
	}

	// autosprites
	// FIXME: autosprite2 requires waaaay too much data for such a trivial
	// transformation..
	if( ( vattribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) {
		assert( !( vertexSize & 3 ) );
		layout->spritePointsOffset = vertexSize;
		vertexSize += FLOAT_VATTRIB_SIZE( VATTRIB_AUTOSPRITE_BIT, halfFloatVattribs ) * 4;
	}

	size_t dataSize = vertexSize * numVerts;

	// instances data
	if( ( ( vattribs & VATTRIB_INSTANCES_BITS ) == VATTRIB_INSTANCES_BITS ) && numInstances ) {
		assert( !( vertexSize & 3 ) );
		layout->instancesOffset = dataSize;
		dataSize += numInstances * sizeof( GLfloat ) * 8;
	}

	assert( vertexSize < std::numeric_limits<uint8_t>::max() );
	layout->vertexSize       = vertexSize;
	layout->vertexAttribs    = vattribs;
	layout->halfFloatAttribs = halfFloatVattribs;

	return dataSize;
}

template <typename InType, typename OutType, unsigned NumComponents>
[[nodiscard]]
static auto fillAttribData( vattribmask_t attrib, const void *inData, size_t outStride, size_t numVerts, void *outData ) -> vattribmask_t {
	assert( numVerts && NumComponents );
	if( inData ) {
		size_t vertNum = 0;
		do {
			auto *const __restrict out      = (OutType *)( (const uint8_t *)outData + vertNum * outStride );
			const auto *const __restrict in = ( (const InType *)inData ) + vertNum * NumComponents;
			assert( ( ( (uintptr_t)out ) % alignof( OutType ) ) == 0 );
			assert( ( ( (uintptr_t)in ) % alignof( InType ) ) == 0 );
			unsigned componentNum = 0;
			do {
				if constexpr( std::is_same_v<InType, float> && std::is_same_v<OutType, GLhalf> ) {
					out[componentNum] = Com_FloatToHalf( in[componentNum] );
				} else {
					out[componentNum] = in[componentNum];
				}
			} while( ++componentNum < NumComponents );
		} while( ++vertNum < numVerts );
		return 0;
	} else {
		return attrib;
	}
}

template <vattribmask_t Attrib, unsigned NumComponents>
[[nodiscard]]
static auto fillFloatAttribData( const void *inData, vattribmask_t hfa, size_t outStride, size_t numVerts, void *outData ) -> vattribmask_t {
	if( FLOAT_VATTRIB_GL_TYPE( Attrib, hfa ) == GL_HALF_FLOAT ) {
		return fillAttribData<float, GLhalf, NumComponents>( Attrib, inData, outStride, numVerts, outData );
	} else {
		return fillAttribData<float, float, NumComponents>( Attrib, inData, outStride, numVerts, outData );
	}
}

template <unsigned NumComponents>
[[nodiscard]]
static auto fillFloatAttribData( vattribbit_t attrib, const void *inData, vattribmask_t hfa, size_t outStride,
								 size_t numVerts, void *outData ) -> vattribmask_t {
	if( FLOAT_VATTRIB_GL_TYPE( attrib, hfa ) == GL_HALF_FLOAT ) {
		return fillAttribData<float, GLhalf, NumComponents>( attrib, inData, outStride, numVerts, outData );
	} else {
		return fillAttribData<float, float, NumComponents>( attrib, inData, outStride, numVerts, outData );
	}
}

static void fillAutosprite2Attribs( const mesh_t *mesh, const VboSpanLayout *layout, vattribmask_t hfa, uint8_t *data ) {
	// for autosprite2 also upload vertices that form the longest axis
	// the remaining vertex can be trivially computed in vertex shader

	unsigned numQuads;
	vec4_t *const verts = mesh->xyzArray;
	const elem_t *elems = mesh->elems;
	assert( ( mesh->elems && mesh->numElems ) || ( mesh->numVerts == 4 ) );
	if( mesh->elems && mesh->numElems ) {
		numQuads = mesh->numElems / 6;

		// protect against bogus autosprite2 meshes
		if( numQuads > mesh->numVerts / 4 ) {
			numQuads = mesh->numVerts / 4;
		}
	} else if( mesh->numVerts == 4 ) {
		// single quad as triangle fan
		numQuads = 1;
		elems    = kQuadIndices;
	} else {
		numQuads = 0;
	}

	const auto vertSize  = layout->vertexSize;
	size_t bufferOffset0 = layout->spritePointsOffset;
	size_t bufferOffset1 = layout->sVectorsOffset;
	for( unsigned i = 0; i < numQuads; i++ ) {
		// find the longest edge, the long edge and the short edge
		int longest_edge = -1, longer_edge = -1;
		float longest_dist = 0, longer_dist = 0;
		float d[3];
		vec3_t vd[3];
		const int edges[3][2] = { { 1, 0 }, { 2, 0 }, { 2, 1 } };
		for( int j = 0; j < 3; j++ ) {
			VectorSubtract( verts[elems[edges[j][0]]], verts[elems[edges[j][1]]], vd[j] );
			const float len = VectorLength( vd[j] );
			if( len != 0.0f ) {
				d[j] = len;
			} else {
				d[j] = 1.0f;
			}

			if( longest_edge == -1 || longest_dist < len ) {
				longer_dist = longest_dist;
				longer_edge = longest_edge;
				longest_dist = len;
				longest_edge = j;
			} else if( longer_dist < len ) {
				longer_dist = len;
				longer_edge = j;
			}
		}

		if( const int short_edge = 3 - ( longest_edge + longer_edge ); short_edge <= 2 ) {
			vec4_t centre[4], axes[4];

			// centre
			VectorAdd( verts[elems[edges[longest_edge][0]]], verts[elems[edges[longest_edge][1]]], centre[0] );
			VectorScale( centre[0], 0.5f, centre[0] );
			// radius
			centre[0][3] = d[longest_edge] * 0.5f; // unused
			// right axis, normalized
			VectorScale( vd[short_edge], 1.0 / d[short_edge], vd[short_edge] );
			// up axis, normalized
			VectorScale( vd[longer_edge], 1.0 / d[longer_edge], vd[longer_edge] );

			NormToLatLong( vd[short_edge], &axes[0][0] );
			NormToLatLong( vd[longer_edge], &axes[0][2] );

			for( unsigned j = 1; j < 4; j++ ) {
				Vector4Copy( centre[0], centre[j] );
				Vector4Copy( axes[0], axes[j] );
			}

			(void)fillFloatAttribData<VATTRIB_AUTOSPRITE_BIT, 4>( centre, hfa, vertSize, 4, data + bufferOffset0 );
			(void)fillFloatAttribData<VATTRIB_SVECTOR_BIT, 4>( axes, hfa, vertSize, 4, data + bufferOffset1 );

			bufferOffset0 += 4 * vertSize;
			bufferOffset1 += 4 * vertSize;
		}

		elems += 6;
	}
}

static void fillAutospriteAttribs( const mesh_s *mesh, const VboSpanLayout *layout, vattribmask_t hfa, uint8_t *data ) {
	vec4_t *verts;
	unsigned numQuads;
	if( mesh->xyzArray ) {
		verts = mesh->xyzArray;
		numQuads = mesh->numVerts / 4;
	} else {
		verts = nullptr;
		numQuads = 0;
	}

	const auto vertSize = layout->vertexSize;
	size_t bufferOffset = layout->spritePointsOffset;
	for( unsigned i = 0; i < numQuads; i++ ) {
		vec4_t centre[4];
		// centre
		for( unsigned j = 0; j < 3; j++ ) {
			centre[0][j] = ( verts[0][j] + verts[1][j] + verts[2][j] + verts[3][j] ) * 0.25f;
		}
		// radius
		centre[0][3] = Distance( verts[0], centre[0] ) * 0.707106f;     // 1.0f / sqrt(2)

		for( unsigned j = 1; j < 4; j++ ) {
			Vector4Copy( centre[0], centre[j] );
		}

		(void)fillFloatAttribData<VATTRIB_AUTOSPRITE_BIT, 4>( centre, hfa, vertSize, 4, data + bufferOffset );

		bufferOffset += 4 * vertSize;
		verts += 4;
	}
}

/*
* Generates required vertex data to be uploaded to the buffer.
*
* Vertex attributes masked by halfFloatVattribs will use half-precision floats
* to save memory, if GL_ARB_half_float_vertex is available. Note that if
* VATTRIB_POSITION_BIT is not set, it will also reset bits for other positional
* attributes such as autosprite pos and instance pos.
*/
auto fillMeshVertexData( const VboSpanLayout *layout, vattribmask_t vattribs, const mesh_t *mesh, void *outData ) -> vattribmask_t {
	vattribmask_t errMask   = 0;
	const unsigned numVerts = mesh->numVerts;
	const size_t vertSize   = layout->vertexSize;
	const vattribmask_t hfa = layout->halfFloatAttribs;
	uint8_t *const data     = (uint8_t *)outData + layout->baseOffset;

	// upload vertex xyz data
	if( vattribs & VATTRIB_POSITION_BIT ) {
		errMask |= fillFloatAttribData<VATTRIB_POSITION_BIT, 4>( mesh->xyzArray, hfa, vertSize, numVerts, data + 0 );
	}

	// upload normals data
	if( layout->normalsOffset && ( vattribs & VATTRIB_NORMAL_BIT ) ) {
		errMask |= fillFloatAttribData<VATTRIB_NORMAL_BIT, 4>( mesh->normalsArray, hfa, vertSize,
															   numVerts, data + layout->normalsOffset );
	}

	// upload tangent vectors
	if( layout->sVectorsOffset && ( ( vattribs & ( VATTRIB_SVECTOR_BIT | VATTRIB_AUTOSPRITE2_BIT ) ) == VATTRIB_SVECTOR_BIT ) ) {
		errMask |= fillFloatAttribData<VATTRIB_SVECTOR_BIT, 4>( mesh->sVectorsArray, hfa, vertSize,
																numVerts, data + layout->sVectorsOffset );
	}

	// upload texture coordinates
	if( layout->stOffset && ( vattribs & VATTRIB_TEXCOORDS_BIT ) ) {
		errMask |= fillFloatAttribData<VATTRIB_TEXCOORDS_BIT, 2>( mesh->stArray[0], hfa, vertSize,
																  numVerts, data + layout->stOffset );
	}

	// upload lightmap texture coordinates
	if( layout->lmstOffset[0] && ( vattribs & VATTRIB_LMCOORDS0_BIT ) ) {
		const auto type       = FLOAT_VATTRIB_GL_TYPE( VATTRIB_LMCOORDS0_BIT, hfa );
		const size_t lmstSize = ( ( type == GL_HALF_FLOAT ) ? 2 * sizeof( GLhalf ) : 2 * sizeof( float ) );

		unsigned lmattrbit = VATTRIB_LMCOORDS0_BIT;
		for( unsigned i = 0; i < ( MAX_LIGHTMAPS + 1 ) / 2; i++ ) {
			if( !( vattribs & lmattrbit ) ) {
				break;
			}
			assert( !( errMask & lmattrbit ) );
			errMask = fillFloatAttribData<2>( (vattribbit_t)lmattrbit, mesh->lmstArray[i * 2 + 0], hfa, vertSize,
											  numVerts, data + layout->lmstOffset[i] );
			if( errMask & lmattrbit ) {
				break;
			}
			lmattrbit <<= 1;
			if( vattribs & lmattrbit ) {
				assert( !( errMask & lmattrbit ) );
				errMask = fillFloatAttribData<2>( (vattribbit_t)lmattrbit, mesh->lmstArray[i * 2 + 1], hfa, vertSize,
												  numVerts, data + layout->lmstOffset[i] + lmstSize );
				if( errMask & lmattrbit ) {
					break;
				}
			}
			lmattrbit <<= 1;
		}
	}

	// upload lightmap array texture layers
	if( layout->lmlayersOffset[0] && ( vattribs & VATTRIB_LMLAYERS0123_BIT ) ) {
		vattribbit_t lmattrbit = VATTRIB_LMLAYERS0123_BIT;
		for( unsigned i = 0; i < ( MAX_LIGHTMAPS + 3 ) / 4; i++ ) {
			if( !( vattribs & lmattrbit ) ) {
				break;
			}
			assert( !( errMask & lmattrbit ) );
			errMask |= fillAttribData<int, int, 1>( lmattrbit, mesh->lmlayersArray[i], vertSize,
													numVerts, data + layout->lmlayersOffset[i] );
			if( errMask & lmattrbit ) {
				break;
			}
			lmattrbit = ( decltype( lmattrbit ) )( lmattrbit << 1 );
		}
	}

	// upload vertex colors (although indices > 0 are never used)
	if( layout->colorsOffset[0] && ( vattribs & VATTRIB_COLOR0_BIT ) ) {
		errMask |= fillAttribData<int, int, 1>( VATTRIB_COLOR0_BIT, mesh->colorsArray[0], vertSize,
												numVerts, data + layout->colorsOffset[0] );
	}

	// upload centre and radius for autosprites
	// this code assumes that the mesh has been properly pretransformed
	if( layout->spritePointsOffset && ( ( vattribs & VATTRIB_AUTOSPRITE2_BIT ) == VATTRIB_AUTOSPRITE2_BIT ) ) {
		fillAutosprite2Attribs( mesh, layout, hfa, data );
	} else if( layout->spritePointsOffset && ( ( vattribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) ) {
		fillAutospriteAttribs( mesh, layout, hfa, data );
	}

	if( vattribs & VATTRIB_BONES_BITS ) {
		if( layout->bonesIndicesOffset ) {
			errMask |= fillAttribData<int, int, 1>( VATTRIB_BONESINDICES_BIT, mesh->blendIndices, vertSize,
													numVerts, data + layout->bonesIndicesOffset );
		}
		if( layout->bonesWeightsOffset ) {
			errMask |= fillAttribData<int, int, 1>( VATTRIB_BONESWEIGHTS_BIT, mesh->blendWeights, vertSize,
													numVerts, data + layout->bonesWeightsOffset );
		}
	}

	return errMask;
}