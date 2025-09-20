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
#include "glstateproxy.h"
#include <common/helpers/memspecbuilder.h>
#include <common/facilities/sysclock.h>

// Smaller buffer for 2D polygons. Also a workaround for some instances of a hardly explainable bug on Adreno
// that caused dynamic draws to slow everything down in some cases when normals are used with dynamic VBOs.
#define COMPACT_STREAM_VATTRIBS ( VATTRIB_POSITION_BIT | VATTRIB_COLOR0_BIT | VATTRIB_TEXCOORDS_BIT )
static elem_t dynamicStreamElems[RB_VBO_NUM_STREAMS][MAX_STREAM_VBO_ELEMENTS];

rbackend_t rb;

static void RB_RegisterStreamVBOs();

void RB_Init() {
	memset( &rb, 0, sizeof( rb ) );

	rb.glStateProxy = new GLStateProxy( glConfig.width, glConfig.height, glConfig.stencilBits );

	// initialize shading
	RB_InitShading();

	// create VBO's we're going to use for streamed data
	RB_RegisterStreamVBOs();
}

void RB_Shutdown() {
	for( auto &ds: rb.dynamicStreams ) {
		Q_free( ds.vertexData );
		ds.vertexData = nullptr;
	}
	for( auto &fru: rb.frameUploads ) {
		Q_free( fru.vboData );
		fru.vboData = nullptr;
		Q_free( fru.iboData );
		fru.iboData = nullptr;
	}
	delete rb.glStateProxy;
	rb.glStateProxy = nullptr;
}

void RB_BeginRegistration() {
	RB_RegisterStreamVBOs();
	RB_BindVBO( 0, 0 );

	// unbind all texture targets on all TMUs
	rb.glStateProxy->unbindAllTextures();
}

void RB_EndRegistration() {
	RB_BindVBO( 0, 0 );
}

void RB_SetTime( int64_t time ) {
	rb.time = time;
	rb.nullEnt.shaderTime = Sys_Milliseconds();
}

void RB_BeginFrame() {
	Vector4Set( rb.nullEnt.shaderRGBA, 1, 1, 1, 1 );
	rb.nullEnt.scale = 1;
	VectorClear( rb.nullEnt.origin );
	Matrix3_Identity( rb.nullEnt.axis );

	// start fresh each frame
	RB_SetShaderStateMask( ~0, 0 );
	RB_BindVBO( 0, 0 );
	RB_FlushTextureCache();
}

void RB_EndFrame() {
}

void RB_FlushTextureCache( void ) {
	if( rb.glStateProxy ) {
		rb.glStateProxy->flushTextureCache();
	}
}

void RB_DepthRange( float depthmin, float depthmax ) {
	rb.glStateProxy->setDepthRange( depthmin, depthmax );
}

void RB_GetDepthRange( float* depthmin, float *depthmax ) {
	rb.glStateProxy->getDepthRange( depthmin, depthmax );
}

void RB_SaveDepthRange() {
	rb.glStateProxy->saveDepthRange();
}

void RB_RestoreDepthRange() {
	rb.glStateProxy->restoreDepthRange();
}

void RB_LoadCameraMatrix( const mat4_t m ) {
	Matrix4_Copy( m, rb.cameraMatrix );
}

void RB_LoadObjectMatrix( const mat4_t m ) {
	Matrix4_Copy( m, rb.objectMatrix );
	Matrix4_MultiplyFast( rb.cameraMatrix, m, rb.modelviewMatrix );
	Matrix4_Multiply( rb.projectionMatrix, rb.modelviewMatrix, rb.modelviewProjectionMatrix );
}

void RB_LoadProjectionMatrix( const mat4_t m ) {
	Matrix4_Copy( m, rb.projectionMatrix );
	Matrix4_Multiply( m, rb.modelviewMatrix, rb.modelviewProjectionMatrix );
}

void RB_FlipFrontFace( void ) {
	rb.glStateProxy->flipFrontFace();
}

void RB_Scissor( int x, int y, int w, int h ) {
	rb.glStateProxy->setScissor( x, y, w, h );
}

void RB_GetScissor( int *x, int *y, int *w, int *h ) {
	rb.glStateProxy->getScissor( x, y, w, h );
}

void RB_Viewport( int x, int y, int w, int h ) {
	rb.glStateProxy->setViewport( x, y, w, h );
}

void RB_Clear( int bits, float r, float g, float b, float a ) {
	unsigned state = rb.glStateProxy->getState();

	if( bits & GL_DEPTH_BUFFER_BIT ) {
		state |= GLSTATE_DEPTHWRITE;
	}

	if( bits & GL_STENCIL_BUFFER_BIT ) {
		qglClearStencil( 128 );
	}

	if( bits & GL_COLOR_BUFFER_BIT ) {
		state = ( state & ~GLSTATE_NO_COLORWRITE ) | GLSTATE_ALPHAWRITE;
		qglClearColor( r, g, b, a );
	}

	rb.glStateProxy->setState( state );

	rb.glStateProxy->applyScissor();

	qglClear( bits );

	rb.glStateProxy->setDepthRange( 0.0f, 1.0f );
}

void RB_BindFrameBufferObject( RenderTargetComponents *components ) {
	// TODO: Resolve object lifetime problems/initialization order so we don't have to call it like this...
	GLStateProxy::bindFramebufferObject( rb.glStateProxy, components );
}

/*
* RB_RegisterStreamVBOs
*
* Allocate/keep alive dynamic vertex buffers object
* we'll steam the dynamic geometry into
*/
void RB_RegisterStreamVBOs() {
	vattribmask_t vattribs[RB_VBO_NUM_STREAMS] = {
		VATTRIBS_MASK &~VATTRIB_INSTANCES_BITS,
		COMPACT_STREAM_VATTRIBS
	};

	// allocate stream VBO's
	for( int i = 0; i < RB_VBO_NUM_STREAMS; i++ ) {
		rbDynamicStream_t *stream = &rb.dynamicStreams[i];
		if( stream->vbo ) {
			R_TouchMeshVBO( stream->vbo );
		} else {
			stream->vbo = R_CreateMeshVBO( &rb,
										   MAX_STREAM_VBO_VERTS, MAX_STREAM_VBO_ELEMENTS, 0,
										   vattribs[i], VBO_TAG_STREAM, 0 );
			stream->vertexData = (uint8_t *)Q_malloc( MAX_STREAM_VBO_VERTS * stream->vbo->vertexSize );
		}
	}

	for( auto &fru: rb.frameUploads ) {
		if( fru.vbo ) {
			R_TouchMeshVBO( fru.vbo );
		} else {
			constexpr vattribmask_t vattribs = VATTRIB_POSITION_BIT | VATTRIB_COLOR0_BIT | VATTRIB_TEXCOORDS_BIT;
			fru.vbo = R_CreateMeshVBO( &rb, MAX_UPLOAD_VBO_VERTICES, MAX_UPLOAD_VBO_INDICES, 0, vattribs, VBO_TAG_STREAM, 0 );
			fru.vboData = Q_malloc( MAX_UPLOAD_VBO_VERTICES * fru.vbo->vertexSize );
			fru.iboData = Q_malloc( MAX_UPLOAD_VBO_INDICES * sizeof( uint16_t ) );
		}
	}
}

void RB_BindVBO( int id, int primitive ) {
	rb.primitive = primitive;

	mesh_vbo_t *vbo;
	if( id < RB_VBO_NONE ) {
		if( id == RB_VBOIdForFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH ) ) {
			vbo = rb.frameUploads[UPLOAD_GROUP_DYNAMIC_MESH].vbo;
		} else if( id == RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH ) ) {
			vbo = rb.frameUploads[UPLOAD_GROUP_BATCHED_MESH].vbo;
		} else {
			vbo = rb.dynamicStreams[-id - 1].vbo;
		}
	} else if( id == RB_VBO_NONE ) {
		vbo = NULL;
	} else {
		vbo = R_GetVBOByIndex( id );
	}

	rb.currentVBOId = id;
	rb.currentVBO = vbo;
	if( vbo ) {
		rb.glStateProxy->bindVertexBuffer( vbo->vertexId );
		rb.glStateProxy->bindIndexBuffer( vbo->elemId );
	} else {
		rb.glStateProxy->bindVertexBuffer( 0 );
		rb.glStateProxy->bindIndexBuffer( 0 );
	}
}

int RB_VBOIdForFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	return std::numeric_limits<int>::min() + (int)group;
}

void R_BeginFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	rb.frameUploads[group].vertexDataSize = 0;
	rb.frameUploads[group].indexDataSize  = 0;
}

void R_SetFrameUploadMeshSubdata( unsigned group, unsigned verticesOffset, unsigned indicesOffset, const mesh_t *mesh ) {
	assert( group < std::size( rb.frameUploads ) );
	if( mesh->numVerts && mesh->numElems ) {
		auto &fru = rb.frameUploads[group];

		void *const destVertexData    = (uint8_t *)fru.vboData + verticesOffset * fru.vbo->vertexSize;
		uint16_t *const destIndexData = (uint16_t *)fru.iboData + indicesOffset;

		R_FillVBOVertexDataBuffer( fru.vbo, fru.vbo->vertexAttribs, mesh, destVertexData );
		for( unsigned i = 0; i < mesh->numElems; ++i ) {
			// TODO: Current frontend-enforced limitations are the sole protection from overflow
			// TODO: Use draw elements base vertex
			destIndexData[i] = mesh->elems[i] + verticesOffset;
		}

		fru.vertexDataSize = wsw::max( fru.vertexDataSize, verticesOffset + mesh->numVerts );
		fru.indexDataSize  = wsw::max( fru.indexDataSize, indicesOffset + mesh->numElems );
	}
}

void R_EndFrameUploads( unsigned group ) {
	assert( group < std::size( rb.frameUploads ) );
	if( auto &fru = rb.frameUploads[group]; fru.vertexDataSize && fru.indexDataSize ) {
		RB_BindVBO( RB_VBOIdForFrameUploads( group ), GL_TRIANGLES );

		qglBufferSubData( GL_ARRAY_BUFFER, 0, (GLsizeiptr)( fru.vbo->vertexSize * fru.vertexDataSize ), fru.vboData );
		qglBufferSubData( GL_ELEMENT_ARRAY_BUFFER, 0, (GLsizeiptr)( sizeof( uint16_t ) * fru.indexDataSize ), fru.iboData );

		fru.vertexDataSize = ~0u;
		fru.indexDataSize  = ~0u;
	}
}

static void R_CopyOffsetElements( const elem_t *inelems, int numElems, int vertsOffset, elem_t *outelems ) {
	for( int i = 0; i < numElems; i++, inelems++, outelems++ ) {
		*outelems = vertsOffset + *inelems;
	}
}

static void R_CopyOffsetTriangles( const elem_t *inelems, int numElems, int vertsOffset, elem_t *outelems ) {
	const int numTris = numElems / 3;
	for( int i = 0; i < numTris; i++, inelems += 3, outelems += 3 ) {
		outelems[0] = vertsOffset + inelems[0];
		outelems[1] = vertsOffset + inelems[1];
		outelems[2] = vertsOffset + inelems[2];
	}
}

static void R_BuildTrifanElements( int vertsOffset, int numVerts, elem_t *elems ) {
	for( int i = 2; i < numVerts; i++, elems += 3 ) {
		elems[0] = vertsOffset;
		elems[1] = vertsOffset + i - 1;
		elems[2] = vertsOffset + i;
	}
}

void RB_AddDynamicMesh( const entity_t *entity, const shader_t *shader,
						const struct mfog_s *fog, const struct portalSurface_s *portalSurface, unsigned shadowBits,
						const struct mesh_s *mesh, int primitive, float x_offset, float y_offset ) {

	// can't (and shouldn't because that would break batching) merge strip draw calls
	// (consider simply disabling merge later in this case if models with tristrips are added in the future, but that's slow)
	assert( ( primitive == GL_TRIANGLES ) || ( primitive == GL_LINES ) );

	bool trifan = false;
	int numVerts = mesh->numVerts, numElems = mesh->numElems;
	if( !numElems ) {
		numElems = ( wsw::max( numVerts, 2 ) - 2 ) * 3;
		trifan = true;
	}

	if( !numVerts || !numElems || ( numVerts > MAX_STREAM_VBO_VERTS ) || ( numElems > MAX_STREAM_VBO_ELEMENTS ) ) {
		return;
	}

	rbDynamicDraw_t *prev = nullptr;
	if( rb.numDynamicDraws ) {
		prev = &rb.dynamicDraws[rb.numDynamicDraws - 1];
	}

	int scissor[4];
	RB_GetScissor( &scissor[0], &scissor[1], &scissor[2], &scissor[3] );

	int streamId = RB_VBO_NONE;
	bool merge = false;
	if( prev ) {
		int prevRenderFX = 0, renderFX = 0;
		if( prev->entity ) {
			prevRenderFX = prev->entity->renderfx;
		}
		if( entity ) {
			renderFX = entity->renderfx;
		}
		if( ( ( shader->flags & SHADER_ENTITY_MERGABLE ) || ( prev->entity == entity ) ) && ( prevRenderFX == renderFX ) &&
			( prev->shader == shader ) && ( prev->fog == fog ) && ( prev->portalSurface == portalSurface ) &&
			( ( prev->shadowBits && shadowBits ) || ( !prev->shadowBits && !shadowBits ) ) ) {
			// don't rebind the shader to get the VBO in this case
			streamId = prev->streamId;
			if( ( prev->shadowBits == shadowBits ) && ( prev->primitive == primitive ) &&
				( prev->offset[0] == x_offset ) && ( prev->offset[1] == y_offset ) &&
				!memcmp( prev->scissor, scissor, sizeof( scissor ) ) ) {
				merge = true;
			}
		}
	}

	vattribmask_t vattribs;
	if( streamId == RB_VBO_NONE ) {
		RB_BindShader( entity, nullptr, nullptr, shader, fog );
		vattribs = rb.currentVAttribs;
		streamId = ( ( vattribs & ~COMPACT_STREAM_VATTRIBS ) ? RB_VBO_STREAM : RB_VBO_STREAM_COMPACT );
	} else {
		vattribs = prev->vattribs;
	}

	rbDynamicStream_t *const stream = &rb.dynamicStreams[-streamId - 1];

	if( ( !merge && ( ( rb.numDynamicDraws + 1 ) > MAX_DYNAMIC_DRAWS ) ) ||
		( ( stream->drawElements.firstVert + stream->drawElements.numVerts + numVerts ) > MAX_STREAM_VBO_VERTS ) ||
		( ( stream->drawElements.firstElem + stream->drawElements.numElems + numElems ) > MAX_STREAM_VBO_ELEMENTS ) ) {
		// wrap if overflows
		RB_FlushDynamicMeshes();

		stream->drawElements.firstVert = 0;
		stream->drawElements.numVerts = 0;
		stream->drawElements.firstElem = 0;
		stream->drawElements.numElems = 0;

		merge = false;
	}

	rbDynamicDraw_t *draw;
	if( merge ) {
		// merge continuous draw calls
		draw = prev;
		draw->drawElements.numVerts += numVerts;
		draw->drawElements.numElems += numElems;
	} else {
		draw = &rb.dynamicDraws[rb.numDynamicDraws++];
		draw->entity = entity;
		draw->shader = shader;
		draw->fog = fog;
		draw->portalSurface = portalSurface;
		draw->shadowBits = shadowBits;
		draw->vattribs = vattribs;
		draw->streamId = streamId;
		draw->primitive = primitive;
		draw->offset[0] = x_offset;
		draw->offset[1] = y_offset;
		memcpy( draw->scissor, scissor, sizeof( scissor ) );
		draw->drawElements.firstVert = stream->drawElements.firstVert + stream->drawElements.numVerts;
		draw->drawElements.numVerts = numVerts;
		draw->drawElements.firstElem = stream->drawElements.firstElem + stream->drawElements.numElems;
		draw->drawElements.numElems = numElems;
	}

	const int destVertOffset = stream->drawElements.firstVert + stream->drawElements.numVerts;
	R_FillVBOVertexDataBuffer( stream->vbo, vattribs, mesh,
							   stream->vertexData + destVertOffset * stream->vbo->vertexSize );

	elem_t *destElems = dynamicStreamElems[-streamId - 1] + stream->drawElements.firstElem + stream->drawElements.numElems;
	if( trifan ) {
		R_BuildTrifanElements( destVertOffset, numElems, destElems );
	} else {
		if( primitive == GL_TRIANGLES ) {
			R_CopyOffsetTriangles( mesh->elems, numElems, destVertOffset, destElems );
		} else {
			R_CopyOffsetElements( mesh->elems, numElems, destVertOffset, destElems );
		}
	}

	stream->drawElements.numVerts += numVerts;
	stream->drawElements.numElems += numElems;
}

void RB_FlushDynamicMeshes() {
	const int numDraws = rb.numDynamicDraws;
	if( !numDraws ) {
		return;
	}

	for( int i = 0; i < RB_VBO_NUM_STREAMS; i++ ) {
		rbDynamicStream_t *const stream = &rb.dynamicStreams[i];

		// R_UploadVBO* are going to rebind buffer arrays for upload
		// so update our local VBO state cache by calling RB_BindVBO
		RB_BindVBO( -i - 1, GL_TRIANGLES ); // dummy value for primitive here

		// because of firstVert, upload elems first
		if( stream->drawElements.numElems ) {
			mesh_t elemMesh;
			memset( &elemMesh, 0, sizeof( elemMesh ) );
			elemMesh.elems = dynamicStreamElems[i] + stream->drawElements.firstElem;
			elemMesh.numElems = stream->drawElements.numElems;
			R_UploadVBOElemData( stream->vbo, 0, stream->drawElements.firstElem, &elemMesh );
			stream->drawElements.firstElem += stream->drawElements.numElems;
			stream->drawElements.numElems = 0;
		}

		if( stream->drawElements.numVerts ) {
			R_UploadVBOVertexRawData( stream->vbo, stream->drawElements.firstVert, stream->drawElements.numVerts,
									  stream->vertexData + stream->drawElements.firstVert * stream->vbo->vertexSize );
			stream->drawElements.firstVert += stream->drawElements.numVerts;
			stream->drawElements.numVerts = 0;
		}
	}

	int sx, sy, sw, sh;
	RB_GetScissor( &sx, &sy, &sw, &sh );

	mat4_t m;
	Matrix4_Copy( rb.objectMatrix, m );
	const float transx = m[12];
	const float transy = m[13];

	float offsetx = 0.0f, offsety = 0.0f;
	for( int i = 0; i < numDraws; i++ ) {
		rbDynamicDraw_t *draw = rb.dynamicDraws + i;
		RB_BindShader( draw->entity, nullptr, nullptr, draw->shader, draw->fog );
		RB_BindVBO( draw->streamId, draw->primitive );
		RB_SetPortalSurface( draw->portalSurface );
		RB_Scissor( draw->scissor[0], draw->scissor[1], draw->scissor[2], draw->scissor[3] );

		// translate the mesh in 2D
		if( ( offsetx != draw->offset[0] ) || ( offsety != draw->offset[1] ) ) {
			offsetx = draw->offset[0];
			offsety = draw->offset[1];
			m[12] = transx + offsetx;
			m[13] = transy + offsety;
			RB_LoadObjectMatrix( m );
		}

		const auto &drawElements = draw->drawElements;
		RB_DrawMesh( nullptr, drawElements.firstVert, drawElements.numVerts, drawElements.firstElem, drawElements.numElems );
	}

	rb.numDynamicDraws = 0;

	RB_Scissor( sx, sy, sw, sh );

	// restore the original translation in the object matrix if it has been changed
	if( offsetx || offsety ) {
		m[12] = transx;
		m[13] = transy;
		RB_LoadObjectMatrix( m );
	}
}

void RB_DoDrawMeshVerts( const DrawMeshVertSpan &vertSpan ) {
	// TODO: What's the purpose of v_drawElements
	if( !( v_drawElements.get() || rb.currentEntity == &rb.nullEnt ) ) [[unlikely]] {
		return;
	}

	rb.glStateProxy->applyScissor();

	if( const auto *mdSpan = std::get_if<MultiDrawElemSpan>( &vertSpan ) ) {
		qglMultiDrawElements( rb.primitive, mdSpan->counts, GL_UNSIGNED_SHORT, mdSpan->indices, mdSpan->numDraws );
	} else if( const auto *vertElemSpan = std::get_if<VertElemSpan>( &vertSpan ) ) {
		const unsigned numVerts  = vertElemSpan->numVerts;
		const unsigned numElems  = vertElemSpan->numElems;
		const unsigned firstVert = vertElemSpan->firstVert;
		const unsigned firstElem = vertElemSpan->firstElem;

		qglDrawRangeElements( rb.primitive, firstVert, firstVert + numVerts - 1, (int)numElems,
							  GL_UNSIGNED_SHORT, (GLvoid *)( firstElem * sizeof( elem_t ) ) );
	} else {
		assert( false );
	}
}

void RB_DrawMesh( const FrontendToBackendShared *fsh, int firstVert, int numVerts, int firstElem, int numElems ) {
	RB_DrawMesh( fsh, VertElemSpan { .firstVert = (unsigned)firstVert, .numVerts = (unsigned)numVerts,
									 .firstElem = (unsigned)firstElem, .numElems = (unsigned)numElems } );
}

void RB_DrawMesh( const FrontendToBackendShared *fsh, const DrawMeshVertSpan &drawMeshVertSpan ) {
	rb.currentVAttribs &= ~VATTRIB_INSTANCES_BITS;

	rb.drawMeshVertSpan = drawMeshVertSpan;

	assert( rb.currentShader );

	rb.glStateProxy->enableVertexAttribs( rb.currentVAttribs, rb.currentVBO );

	if( rb.wireframe ) {
		RB_DrawWireframeMesh( fsh );
	} else {
		RB_DrawShadedMesh( fsh );
	}
}

void RB_SetCamera( const vec3_t cameraOrigin, const mat3_t cameraAxis ) {
	VectorCopy( cameraOrigin, rb.cameraOrigin );
	Matrix3_Copy( cameraAxis, rb.cameraAxis );
}

void RB_SetRenderFlags( int flags ) {
	rb.renderFlags = flags;
}

bool RB_EnableWireframe( bool enable ) {
	const bool oldVal = rb.wireframe;

	if( rb.wireframe != enable ) {
		rb.wireframe = enable;

		if( enable ) {
			RB_SetShaderStateMask( 0, GLSTATE_NO_DEPTH_TEST );
			qglPolygonMode( GL_FRONT_AND_BACK, GL_LINE );
		} else {
			RB_SetShaderStateMask( ~0, 0 );
			qglPolygonMode( GL_FRONT_AND_BACK, GL_FILL );
		}
	}

	return oldVal;
}

void R_SubmitAliasSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceAlias_t *drawSurf ) {
	const maliasmesh_t *aliasmesh = drawSurf->mesh;

	RB_BindVBO( aliasmesh->vbo->index, GL_TRIANGLES );
	RB_DrawMesh( fsh, 0, aliasmesh->numverts, 0, aliasmesh->numtris * 3 );
}

void R_SubmitSkeletalSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceSkeletal_t *drawSurf ) {
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

	if( !cache || R_SkeletalRenderAsFrame0( cache ) ) {
		// fastpath: render static frame 0 as is
		if( skmesh->vbo ) {
			RB_BindVBO( skmesh->vbo->index, GL_TRIANGLES );
			RB_DrawMesh( fsh, 0, skmesh->numverts, 0, skmesh->numtris * 3 );
			return;
		}
	}

	if( bonePoseRelativeDQ && skmesh->vbo ) {
		// another fastpath: transform the initial pose on the GPU
		RB_BindVBO( skmesh->vbo->index, GL_TRIANGLES );
		RB_SetBonesData( skmodel->numbones, bonePoseRelativeDQ, skmesh->maxWeights );
		RB_DrawMesh( fsh, 0, skmesh->numverts, 0, skmesh->numtris * 3 );
		return;
	}
}

void R_SubmitBSPSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceBSP_t *drawSurf ) {
	const MergedBspSurface *mergedBspSurf = drawSurf->mergedBspSurf;

	assert( !mergedBspSurf->numInstances );

	RB_BindVBO( mergedBspSurf->vbo->index, GL_TRIANGLES );
	RB_SetDlightBits( drawSurf->dlightBits );
	RB_SetLightstyle( mergedBspSurf->superLightStyle );

	RB_DrawMesh( fsh, drawSurf->mdSpan );
}

void R_SubmitNullSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * ) {
	assert( rsh.nullVBO != NULL );

	RB_BindVBO( rsh.nullVBO->index, GL_LINES );
	RB_DrawMesh( fsh, 0, 6, 0, 6 );
}

void R_SubmitDynamicMeshToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
									const mfog_t *fog, const portalSurface_t *portalSurface, const DynamicMeshDrawSurface *drawSurface ) {
	// Protect against the case when fillMeshBuffers() produces zero vertices
	if( drawSurface->actualNumVertices && drawSurface->actualNumIndices ) {
		RB_BindVBO( RB_VBOIdForFrameUploads( UPLOAD_GROUP_DYNAMIC_MESH ), GL_TRIANGLES );

		const VertElemSpan vertElemSpan {
			.firstVert = drawSurface->verticesOffset,
			.numVerts  = drawSurface->actualNumVertices,
			.firstElem = drawSurface->indicesOffset,
			.numElems  = drawSurface->actualNumIndices,
		};

		RB_DrawMesh( fsh, vertElemSpan );
	}
}

void R_SubmitBatchedSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e,
									const ShaderParams *overrideParams, const ShaderParamsTable *paramsTable,
									const shader_t *shader, const mfog_t *fog,
									const portalSurface_t *portalSurface, unsigned vertElemSpanIndex ) {
	const VertElemSpan &vertElemSpan = fsh->batchedVertElemSpans[vertElemSpanIndex];
	if( vertElemSpan.numVerts && vertElemSpan.numElems ) {
		RB_BindShader( e, overrideParams, paramsTable, shader, fog );
		RB_BindVBO( RB_VBOIdForFrameUploads( UPLOAD_GROUP_BATCHED_MESH ), GL_TRIANGLES );
		RB_DrawMesh( fsh, vertElemSpan );
	}
}

void R_SubmitSpriteSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e,
								   const ShaderParams *, const ShaderParamsTable *paramsTable,
								   const shader_s *shader, const mfog_t *fog,
								   const portalSurface_t *portalSurface, unsigned meshIndex ) {
	auto *mesh = (mesh_t *)( (uint8_t *)fsh->preparedSpriteMeshes + meshIndex * fsh->preparedSpriteMeshStride );
	RB_AddDynamicMesh( e, shader, fog, portalSurface, 0, mesh, GL_TRIANGLES, 0.0f, 0.0f );
}