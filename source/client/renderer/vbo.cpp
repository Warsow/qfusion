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
#include <common/types/podbuffer.h>

/*
=========================================================

VERTEX BUFFER OBJECTS

=========================================================
*/

typedef struct vbohandle_s {
	unsigned int index;
	mesh_vbo_t *vbo;
	struct vbohandle_s *prev, *next;
} vbohandle_t;

#define MAX_MESH_VERTEX_BUFFER_OBJECTS  0x8000

#define VBO_USAGE_FOR_TAG( tag ) \
	(GLenum)( ( tag ) == VBO_TAG_STREAM ? GL_DYNAMIC_DRAW : GL_STATIC_DRAW )

static mesh_vbo_t r_mesh_vbo[MAX_MESH_VERTEX_BUFFER_OBJECTS];

static vbohandle_t r_vbohandles[MAX_MESH_VERTEX_BUFFER_OBJECTS];
static vbohandle_t r_vbohandles_headnode, *r_free_vbohandles;

static int r_num_active_vbos;

static GLuint r_vao;

static PodBuffer<uint8_t> g_tmpVertexDataBuffer;
static PodBuffer<uint8_t> g_tmpIndexDataBuffer;

void R_InitVBO( void ) {
	int i;

	r_num_active_vbos = 0;

	memset( r_mesh_vbo, 0, sizeof( r_mesh_vbo ) );
	memset( r_vbohandles, 0, sizeof( r_vbohandles ) );

	// link vbo handles
	r_free_vbohandles = r_vbohandles;
	r_vbohandles_headnode.prev = &r_vbohandles_headnode;
	r_vbohandles_headnode.next = &r_vbohandles_headnode;
	for( i = 0; i < MAX_MESH_VERTEX_BUFFER_OBJECTS; i++ ) {
		r_vbohandles[i].index = i;
		r_vbohandles[i].vbo = &r_mesh_vbo[i];
	}
	for( i = 0; i < MAX_MESH_VERTEX_BUFFER_OBJECTS - 1; i++ ) {
		r_vbohandles[i].next = &r_vbohandles[i + 1];
	}

	qglGenVertexArrays( 1, &r_vao );
	qglBindVertexArray( r_vao );
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

mesh_vbo_t *R_CreateMeshVBO( int numVerts, int numElems, int numInstances,
							 vattribmask_t vattribs, vbo_tag_t tag, vattribmask_t halfFloatVattribs ) {
	GLuint vbo_id;
	vbohandle_t *vboh = NULL;
	mesh_vbo_t *vbo = NULL;
	GLenum usage = VBO_USAGE_FOR_TAG( tag );

	if( !r_free_vbohandles ) {
		return NULL;
	}

	if( !( halfFloatVattribs & VATTRIB_POSITION_BIT ) ) {
		halfFloatVattribs &= ~( VATTRIB_AUTOSPRITE_BIT );
	}

	halfFloatVattribs &= ~VATTRIB_COLORS_BITS;
	halfFloatVattribs &= ~VATTRIB_BONES_BITS;

	// TODO: convert quaternion component of instance_t to half-float
	// when uploading instances data
	halfFloatVattribs &= ~VATTRIB_INSTANCES_BITS;

	vboh = r_free_vbohandles;
	vbo = &r_mesh_vbo[vboh->index];
	memset( vbo, 0, sizeof( *vbo ) );

	const auto indexDataSize  = (GLsizeiptr)numElems * sizeof( elem_t );
	const auto vertexDataSize = (GLsizeiptr)buildVertexLayoutForVattribs( &vbo->layout, vattribs, halfFloatVattribs,
																		  numVerts, numInstances );

	// pre-allocate vertex buffer
	vbo_id = 0;
	qglGenBuffers( 1, &vbo_id );
	if( !vbo_id ) {
		goto error;
	}
	vbo->vertexId = vbo_id;

	qglBindBuffer( GL_ARRAY_BUFFER, vbo_id );
	qglBufferData( GL_ARRAY_BUFFER, vertexDataSize, nullptr, usage );
	if( qglGetError() == GL_OUT_OF_MEMORY ) {
		goto error;
	}

	// pre-allocate elements buffer
	vbo_id = 0;
	qglGenBuffers( 1, &vbo_id );
	if( !vbo_id ) {
		goto error;
	}
	vbo->elemId = vbo_id;

	qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo_id );
	qglBufferData( GL_ELEMENT_ARRAY_BUFFER, indexDataSize, nullptr, usage );
	if( qglGetError() == GL_OUT_OF_MEMORY ) {
		goto error;
	}

	r_free_vbohandles = vboh->next;

	// link to the list of active vbo handles
	vboh->prev = &r_vbohandles_headnode;
	vboh->next = r_vbohandles_headnode.next;
	vboh->next->prev = vboh;
	vboh->prev->next = vboh;

	r_num_active_vbos++;

	vbo->registrationSequence = rsh.registrationSequence;
	vbo->index = vboh->index + 1;
	vbo->tag = tag;

	return vbo;

error:
	if( vbo ) {
		R_ReleaseMeshVBO( vbo );
	}

	return NULL;
}

void R_TouchMeshVBO( mesh_vbo_t *vbo ) {
	vbo->registrationSequence = rsh.registrationSequence;
}

void R_ReleaseMeshVBO( mesh_vbo_t *vbo ) {
	GLuint vbo_id;

	assert( vbo != NULL );

	qglBindBuffer( GL_ARRAY_BUFFER, 0 );
	qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, 0 );

	if( vbo->vertexId ) {
		vbo_id = vbo->vertexId;
		qglDeleteBuffers( 1, &vbo_id );
	}

	if( vbo->elemId ) {
		vbo_id = vbo->elemId;
		qglDeleteBuffers( 1, &vbo_id );
	}

	if( vbo->index >= 1 && vbo->index <= MAX_MESH_VERTEX_BUFFER_OBJECTS ) {
		vbohandle_t *vboh = &r_vbohandles[vbo->index - 1];

		// remove from linked active list
		vboh->prev->next = vboh->next;
		vboh->next->prev = vboh->prev;

		// insert into linked free list
		vboh->next = r_free_vbohandles;
		r_free_vbohandles = vboh;

		r_num_active_vbos--;
	}

	memset( vbo, 0, sizeof( *vbo ) );
	vbo->tag = VBO_TAG_NONE;
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
	const elem_t trifanElems[6] { 0, 1, 2, 0, 2, 3 };
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
		elems    = trifanElems;
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
* R_FillVBOVertexDataBuffer
*
* Generates required vertex data to be uploaded to the buffer.
*
* Vertex attributes masked by halfFloatVattribs will use half-precision floats
* to save memory, if GL_ARB_half_float_vertex is available. Note that if
* VATTRIB_POSITION_BIT is not set, it will also reset bits for other positional
* attributes such as autosprite pos and instance pos.
*/
vattribmask_t R_FillVBOVertexDataBuffer( const VboSpanLayout *layout, vattribmask_t vattribs, const mesh_t *mesh, void *outData ) {
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

void R_UploadVBOVertexRawData( mesh_vbo_t *vbo, int vertsOffset, int numVerts, const void *data ) {
	qglBindBuffer( GL_ARRAY_BUFFER, vbo->vertexId );
	qglBufferSubData( GL_ARRAY_BUFFER, vertsOffset * vbo->layout.vertexSize, numVerts * vbo->layout.vertexSize, data );
}

vattribmask_t R_UploadVBOVertexData( mesh_vbo_t *vbo, int vertsOffset, vattribmask_t vattribs, const mesh_t *mesh ) {
	void *data = g_tmpVertexDataBuffer.reserveAndGet( mesh->numVerts * vbo->layout.vertexSize );
	vattribmask_t errMask = R_FillVBOVertexDataBuffer( &vbo->layout, vattribs, mesh, data );
	R_UploadVBOVertexRawData( vbo, vertsOffset, mesh->numVerts, data );
	return errMask;
}

void R_UploadVBOElemData( mesh_vbo_t *vbo, int vertsOffset, int elemsOffset, const mesh_t *mesh ) {
	elem_t *ielems = mesh->elems;
	if( vertsOffset ) {
		ielems = (elem_t *)g_tmpIndexDataBuffer.reserveAndGet( mesh->numElems * sizeof( elem_t ) );
		for( unsigned i = 0; i < mesh->numElems; i++ ) {
			ielems[i] = vertsOffset + mesh->elems[i];
		}
	}

	qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, vbo->elemId );
	qglBufferSubData( GL_ELEMENT_ARRAY_BUFFER, elemsOffset * sizeof( elem_t ),
						 mesh->numElems * sizeof( elem_t ), ielems );
}

vattribmask_t R_UploadVBOInstancesData( mesh_vbo_t *vbo, int instOffset, int numInstances, instancePoint_t *instances ) {
	vattribmask_t errMask = 0;

	assert( vbo != NULL );

	if( !vbo->vertexId ) {
		return 0;
	}

	if( !instances ) {
		errMask |= VATTRIB_INSTANCES_BITS;
	}

	if( errMask ) {
		return errMask;
	}

	if( vbo->layout.instancesOffset ) {
		qglBindBuffer( GL_ARRAY_BUFFER, vbo->vertexId );
		qglBufferSubData( GL_ARRAY_BUFFER,
							 vbo->layout.instancesOffset + instOffset * sizeof( instancePoint_t ),
							 numInstances * sizeof( instancePoint_t ), instances );
	}

	return 0;
}

void R_FreeVBOsByTag( vbo_tag_t tag ) {
	mesh_vbo_t *vbo;
	vbohandle_t *vboh, *next, *hnode;

	if( !r_num_active_vbos ) {
		return;
	}

	hnode = &r_vbohandles_headnode;
	for( vboh = hnode->prev; vboh != hnode; vboh = next ) {
		next = vboh->prev;
		vbo = &r_mesh_vbo[vboh->index];

		if( vbo->tag == tag ) {
			R_ReleaseMeshVBO( vbo );
		}
	}
}

void R_FreeUnusedVBOs( void ) {
	mesh_vbo_t *vbo;
	vbohandle_t *vboh, *next, *hnode;

	if( !r_num_active_vbos ) {
		return;
	}

	hnode = &r_vbohandles_headnode;
	for( vboh = hnode->prev; vboh != hnode; vboh = next ) {
		next = vboh->prev;
		vbo = &r_mesh_vbo[vboh->index];

		if( vbo->registrationSequence != rsh.registrationSequence ) {
			R_ReleaseMeshVBO( vbo );
		}
	}
}

void R_ShutdownVBO( void ) {
	mesh_vbo_t *vbo;
	vbohandle_t *vboh, *next, *hnode;

	if( !r_num_active_vbos ) {
		return;
	}

	hnode = &r_vbohandles_headnode;
	for( vboh = hnode->prev; vboh != hnode; vboh = next ) {
		next = vboh->prev;
		vbo = &r_mesh_vbo[vboh->index];

		R_ReleaseMeshVBO( vbo );
	}

	qglBindVertexArray( 0 );
	qglDeleteVertexArrays( 1, &r_vao );
	r_vao = 0;
}
