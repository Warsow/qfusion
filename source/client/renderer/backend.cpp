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
#include <common/helpers/memspecbuilder.h>
#include <common/facilities/sysclock.h>

// Smaller buffer for 2D polygons. Also a workaround for some instances of a hardly explainable bug on Adreno
// that caused dynamic draws to slow everything down in some cases when normals are used with dynamic VBOs.
#define COMPACT_STREAM_VATTRIBS ( VATTRIB_POSITION_BIT | VATTRIB_COLOR0_BIT | VATTRIB_TEXCOORDS_BIT )
static elem_t dynamicStreamElems[RB_VBO_NUM_STREAMS][MAX_STREAM_VBO_ELEMENTS];

rbackend_t rb;

static void RB_SetGLDefaults();
static void RB_RegisterStreamVBOs();
static void RB_SelectTextureUnit( int tmu );

void RB_Init() {
	memset( &rb, 0, sizeof( rb ) );

	// set default OpenGL state
	RB_SetGLDefaults();
	rb.gl.scissor[2] = glConfig.width;
	rb.gl.scissor[3] = glConfig.height;

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
}

void RB_BeginRegistration() {
	RB_RegisterStreamVBOs();
	RB_BindVBO( 0, 0 );

	// unbind all texture targets on all TMUs
	for( int i = MAX_TEXTURE_UNITS - 1; i >= 0; i-- ) {
		RB_SelectTextureUnit( i );

		qglBindTexture( GL_TEXTURE_CUBE_MAP, 0 );
		if( glConfig.ext.texture_array ) {
			qglBindTexture( GL_TEXTURE_2D_ARRAY_EXT, 0 );
		}
		qglBindTexture( GL_TEXTURE_3D, 0 );
		qglBindTexture( GL_TEXTURE_2D, 0 );
	}

	RB_FlushTextureCache();
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

static void RB_SetGLDefaults( void ) {
	if( glConfig.stencilBits ) {
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
}

static void RB_SelectTextureUnit( int tmu ) {
	if( tmu != rb.gl.currentTMU ) {
		rb.gl.currentTMU = tmu;
		qglActiveTexture( tmu + GL_TEXTURE0 );
	}
}

void RB_FlushTextureCache( void ) {
	rb.gl.flushTextures = true;
}

void RB_BindImage( int tmu, const Texture *tex ) {
	assert( tex != NULL );
	assert( tex->texnum != 0 );

	if( rb.gl.flushTextures ) {
		rb.gl.flushTextures = false;
		memset( rb.gl.currentTextures, 0, sizeof( rb.gl.currentTextures ) );
	}

	const GLuint texnum = tex->texnum;
	if( rb.gl.currentTextures[tmu] != texnum ) {
		rb.gl.currentTextures[tmu] = texnum;
		RB_SelectTextureUnit( tmu );
		qglBindTexture( tex->target, tex->texnum );
	}
}

void RB_DepthRange( float depthmin, float depthmax ) {
	Q_clamp( depthmin, 0.0f, 1.0f );
	Q_clamp( depthmax, 0.0f, 1.0f );
	rb.gl.depthmin = depthmin;
	rb.gl.depthmax = depthmax;
	// depthmin == depthmax is a special case when a specific depth value is going to be written
	if( ( depthmin != depthmax ) && !rb.gl.depthoffset ) {
		depthmin += 4.0f / 65535.0f;
	}
	qglDepthRange( depthmin, depthmax );
}

void RB_GetDepthRange( float* depthmin, float *depthmax ) {
	*depthmin = rb.gl.depthmin;
	*depthmax = rb.gl.depthmax;
}

void RB_SaveDepthRange() {
	assert( !rb.gl.hasSavedDepth );
	rb.gl.savedDepthmin = rb.gl.depthmin;
	rb.gl.savedDepthmax = rb.gl.depthmax;
	rb.gl.hasSavedDepth = true;
}

void RB_RestoreDepthRange() {
	assert( rb.gl.hasSavedDepth );
	RB_DepthRange( rb.gl.savedDepthmin, rb.gl.savedDepthmax );
	rb.gl.hasSavedDepth = false;
}

void RB_DepthOffset( bool enable ) {
	float depthmin = rb.gl.depthmin;
	float depthmax = rb.gl.depthmax;
	rb.gl.depthoffset = enable;
	if( depthmin != depthmax ) {
		if( !enable ) {
			depthmin += 4.0f / 65535.0f;
		}
		qglDepthRange( depthmin, depthmax );
	}
}

void RB_ClearDepth( float depth ) {
	qglClearDepth( depth );
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

void RB_Cull( int cull ) {
	if( rb.gl.faceCull != cull ) {
		if( cull ) {
			if( !rb.gl.faceCull ) {
				qglEnable( GL_CULL_FACE );
			}
			qglCullFace( cull );
			rb.gl.faceCull = cull;
		} else {
			qglDisable( GL_CULL_FACE );
			rb.gl.faceCull = 0;
		}
	}
}

void RB_SetState( int state ) {
	const int diff = rb.gl.state ^ state;
	if( !diff ) {
		return;
	}

	if( diff & GLSTATE_BLEND_MASK ) {
		if( state & GLSTATE_BLEND_MASK ) {
			int blendsrc, blenddst;

			switch( state & GLSTATE_SRCBLEND_MASK ) {
				case GLSTATE_SRCBLEND_ZERO:
					blendsrc = GL_ZERO;
					break;
				case GLSTATE_SRCBLEND_DST_COLOR:
					blendsrc = GL_DST_COLOR;
					break;
				case GLSTATE_SRCBLEND_ONE_MINUS_DST_COLOR:
					blendsrc = GL_ONE_MINUS_DST_COLOR;
					break;
				case GLSTATE_SRCBLEND_SRC_ALPHA:
					blendsrc = GL_SRC_ALPHA;
					break;
				case GLSTATE_SRCBLEND_ONE_MINUS_SRC_ALPHA:
					blendsrc = GL_ONE_MINUS_SRC_ALPHA;
					break;
				case GLSTATE_SRCBLEND_DST_ALPHA:
					blendsrc = GL_DST_ALPHA;
					break;
				case GLSTATE_SRCBLEND_ONE_MINUS_DST_ALPHA:
					blendsrc = GL_ONE_MINUS_DST_ALPHA;
					break;
				default:
				case GLSTATE_SRCBLEND_ONE:
					blendsrc = GL_ONE;
					break;
			}

			switch( state & GLSTATE_DSTBLEND_MASK ) {
				case GLSTATE_DSTBLEND_ONE:
					blenddst = GL_ONE;
					break;
				case GLSTATE_DSTBLEND_SRC_COLOR:
					blenddst = GL_SRC_COLOR;
					break;
				case GLSTATE_DSTBLEND_ONE_MINUS_SRC_COLOR:
					blenddst = GL_ONE_MINUS_SRC_COLOR;
					break;
				case GLSTATE_DSTBLEND_SRC_ALPHA:
					blenddst = GL_SRC_ALPHA;
					break;
				case GLSTATE_DSTBLEND_ONE_MINUS_SRC_ALPHA:
					blenddst = GL_ONE_MINUS_SRC_ALPHA;
					break;
				case GLSTATE_DSTBLEND_DST_ALPHA:
					blenddst = GL_DST_ALPHA;
					break;
				case GLSTATE_DSTBLEND_ONE_MINUS_DST_ALPHA:
					blenddst = GL_ONE_MINUS_DST_ALPHA;
					break;
				default:
				case GLSTATE_DSTBLEND_ZERO:
					blenddst = GL_ZERO;
					break;
			}

			if( !( rb.gl.state & GLSTATE_BLEND_MASK ) ) {
				qglEnable( GL_BLEND );
			}

			qglBlendFuncSeparate( blendsrc, blenddst, GL_ONE, GL_ONE );
		} else {
			qglDisable( GL_BLEND );
		}
	}

	if( diff & ( GLSTATE_NO_COLORWRITE | GLSTATE_ALPHAWRITE ) ) {
		if( state & GLSTATE_NO_COLORWRITE ) {
			qglColorMask( GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE );
		} else {
			qglColorMask( GL_TRUE, GL_TRUE, GL_TRUE, ( state & GLSTATE_ALPHAWRITE ) ? GL_TRUE : GL_FALSE );
		}
	}

	if( diff & ( GLSTATE_DEPTHFUNC_EQ | GLSTATE_DEPTHFUNC_GT ) ) {
		if( state & GLSTATE_DEPTHFUNC_EQ ) {
			qglDepthFunc( GL_EQUAL );
		} else if( state & GLSTATE_DEPTHFUNC_GT ) {
			qglDepthFunc( GL_GREATER );
		} else {
			qglDepthFunc( GL_LEQUAL );
		}
	}

	if( diff & GLSTATE_DEPTHWRITE ) {
		if( state & GLSTATE_DEPTHWRITE ) {
			qglDepthMask( GL_TRUE );
		} else {
			qglDepthMask( GL_FALSE );
		}
	}

	if( diff & GLSTATE_NO_DEPTH_TEST ) {
		if( state & GLSTATE_NO_DEPTH_TEST ) {
			qglDisable( GL_DEPTH_TEST );
		} else {
			qglEnable( GL_DEPTH_TEST );
		}
	}

	if( diff & GLSTATE_OFFSET_FILL ) {
		if( state & GLSTATE_OFFSET_FILL ) {
			qglEnable( GL_POLYGON_OFFSET_FILL );
			RB_DepthOffset( true );
		} else {
			qglDisable( GL_POLYGON_OFFSET_FILL );
			RB_DepthOffset( false );
		}
	}

	if( diff & GLSTATE_STENCIL_TEST ) {
		if( glConfig.stencilBits ) {
			if( state & GLSTATE_STENCIL_TEST ) {
				qglEnable( GL_STENCIL_TEST );
			} else {
				qglDisable( GL_STENCIL_TEST );
			}
		}
	}

	if( diff & GLSTATE_ALPHATEST ) {
		if( glConfig.ext.multisample ) {
			if( state & GLSTATE_ALPHATEST ) {
				qglEnable( GL_SAMPLE_ALPHA_TO_COVERAGE );
			} else {
				qglDisable( GL_SAMPLE_ALPHA_TO_COVERAGE );
			}
		}
	}

	rb.gl.state = state;
}

void RB_FrontFace( bool front ) {
	qglFrontFace( front ? GL_CW : GL_CCW );
	rb.gl.frontFace = front;
}

void RB_FlipFrontFace( void ) {
	RB_FrontFace( !rb.gl.frontFace );
}

void RB_BindArrayBuffer( int buffer ) {
	if( buffer != rb.gl.currentArrayVBO ) {
		qglBindBuffer( GL_ARRAY_BUFFER, buffer );
		rb.gl.currentArrayVBO = buffer;
		rb.gl.lastVAttribs = 0;
	}
}

void RB_BindElementArrayBuffer( int buffer ) {
	if( buffer != rb.gl.currentElemArrayVBO ) {
		qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, buffer );
		rb.gl.currentElemArrayVBO = buffer;
	}
}

static void RB_EnableVertexAttrib( int index, bool enable ) {
	const unsigned bit = 1 << index;
	const unsigned diff = ( rb.gl.vertexAttribEnabled & bit ) ^ ( enable ? bit : 0 );
	if( diff ) {
		if( enable ) {
			rb.gl.vertexAttribEnabled |= bit;
			qglEnableVertexAttribArray( index );
		} else {
			rb.gl.vertexAttribEnabled &= ~bit;
			qglDisableVertexAttribArray( index );
		}
	}
}

void RB_Scissor( int x, int y, int w, int h ) {
	if( ( rb.gl.scissor[0] == x ) && ( rb.gl.scissor[1] == y ) &&
		( rb.gl.scissor[2] == w ) && ( rb.gl.scissor[3] == h ) ) {
		return;
	}

	rb.gl.scissor[0] = x;
	rb.gl.scissor[1] = y;
	rb.gl.scissor[2] = w;
	rb.gl.scissor[3] = h;
	rb.gl.scissorChanged = true;
}

void RB_GetScissor( int *x, int *y, int *w, int *h ) {
	if( x ) {
		*x = rb.gl.scissor[0];
	}
	if( y ) {
		*y = rb.gl.scissor[1];
	}
	if( w ) {
		*w = rb.gl.scissor[2];
	}
	if( h ) {
		*h = rb.gl.scissor[3];
	}
}

void RB_ApplyScissor( void ) {
	if( rb.gl.scissorChanged ) {
		rb.gl.scissorChanged = false;
		const int h = rb.gl.scissor[3];
		qglScissor( rb.gl.scissor[0], rb.gl.fbHeight - h - rb.gl.scissor[1], rb.gl.scissor[2], h );
	}
}

void RB_Viewport( int x, int y, int w, int h ) {
	rb.gl.viewport[0] = x;
	rb.gl.viewport[1] = y;
	rb.gl.viewport[2] = w;
	rb.gl.viewport[3] = h;
	qglViewport( x, rb.gl.fbHeight - h - y, w, h );
}

void RB_Clear( int bits, float r, float g, float b, float a ) {
	int state = rb.gl.state;

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

	RB_SetState( state );

	RB_ApplyScissor();

	qglClear( bits );

	RB_DepthRange( 0.0f, 1.0f );
}

void RB_BindFrameBufferObject( RenderTargetComponents *components ) {
	const int width  = components ? components->texture->width : glConfig.width;
	const int height = components ? components->texture->height : glConfig.height;

	if( rb.gl.fbHeight != height ) {
		rb.gl.scissorChanged = true;
	}

	rb.gl.fbWidth = width;
	rb.gl.fbHeight = height;

	// TODO: Track the currently bound FBO
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
	if( !vbo ) {
		RB_BindArrayBuffer( 0 );
		RB_BindElementArrayBuffer( 0 );
		return;
	}

	RB_BindArrayBuffer( vbo->vertexId );
	RB_BindElementArrayBuffer( vbo->elemId );
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
		RB_DrawElements( nullptr, drawElements.firstVert, drawElements.numVerts, drawElements.firstElem, drawElements.numElems );
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

static void RB_EnableVertexAttribs( void ) {
	const vattribmask_t vattribs = rb.currentVAttribs;
	const mesh_vbo_t *vbo = rb.currentVBO;
	const vattribmask_t hfa = vbo->halfFloatAttribs;

	assert( vattribs & VATTRIB_POSITION_BIT );

	if( ( vattribs == rb.gl.lastVAttribs ) && ( hfa == rb.gl.lastHalfFloatVAttribs ) ) {
		return;
	}

	rb.gl.lastVAttribs = vattribs;
	rb.gl.lastHalfFloatVAttribs = hfa;

	// xyz position
	RB_EnableVertexAttrib( VATTRIB_POSITION, true );
	qglVertexAttribPointer( VATTRIB_POSITION, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_POSITION_BIT, hfa ),
							   GL_FALSE, vbo->vertexSize, ( const GLvoid * )0 );

	// normal
	if( vattribs & VATTRIB_NORMAL_BIT ) {
		RB_EnableVertexAttrib( VATTRIB_NORMAL, true );
		qglVertexAttribPointer( VATTRIB_NORMAL, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_NORMAL_BIT, hfa ),
								   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->normalsOffset );
	} else {
		RB_EnableVertexAttrib( VATTRIB_NORMAL, false );
	}

	// s-vector
	if( vattribs & VATTRIB_SVECTOR_BIT ) {
		RB_EnableVertexAttrib( VATTRIB_SVECTOR, true );
		qglVertexAttribPointer( VATTRIB_SVECTOR, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_SVECTOR_BIT, hfa ),
								   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->sVectorsOffset );
	} else {
		RB_EnableVertexAttrib( VATTRIB_SVECTOR, false );
	}

	// color
	if( vattribs & VATTRIB_COLOR0_BIT ) {
		RB_EnableVertexAttrib( VATTRIB_COLOR0, true );
		qglVertexAttribPointer( VATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE,
								   GL_TRUE, vbo->vertexSize, (const GLvoid * )vbo->colorsOffset[0] );
	} else {
		RB_EnableVertexAttrib( VATTRIB_COLOR0, false );
	}

	// texture coordinates
	if( vattribs & VATTRIB_TEXCOORDS_BIT ) {
		RB_EnableVertexAttrib( VATTRIB_TEXCOORDS, true );
		qglVertexAttribPointer( VATTRIB_TEXCOORDS, 2, FLOAT_VATTRIB_GL_TYPE( VATTRIB_TEXCOORDS_BIT, hfa ),
								   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->stOffset );
	} else {
		RB_EnableVertexAttrib( VATTRIB_TEXCOORDS, false );
	}

	if( ( vattribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) {
		// submit sprite point
		RB_EnableVertexAttrib( VATTRIB_SPRITEPOINT, true );
		qglVertexAttribPointer( VATTRIB_SPRITEPOINT, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_AUTOSPRITE_BIT, hfa ),
								   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->spritePointsOffset );
	} else {
		RB_EnableVertexAttrib( VATTRIB_SPRITEPOINT, false );
	}

	// bones (skeletal models)
	if( ( vattribs & VATTRIB_BONES_BITS ) == VATTRIB_BONES_BITS ) {
		// submit indices
		RB_EnableVertexAttrib( VATTRIB_BONESINDICES, true );
		qglVertexAttribPointer( VATTRIB_BONESINDICES, 4, GL_UNSIGNED_BYTE,
								   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->bonesIndicesOffset );

		// submit weights
		RB_EnableVertexAttrib( VATTRIB_BONESWEIGHTS, true );
		qglVertexAttribPointer( VATTRIB_BONESWEIGHTS, 4, GL_UNSIGNED_BYTE,
								   GL_TRUE, vbo->vertexSize, ( const GLvoid * )vbo->bonesWeightsOffset );
	} else {
		// lightmap texture coordinates - aliasing bones, so not disabling bones
		int lmattr = VATTRIB_LMCOORDS01;
		int lmattrbit = VATTRIB_LMCOORDS0_BIT;

		for( int i = 0; i < ( MAX_LIGHTMAPS + 1 ) / 2; i++ ) {
			if( vattribs & lmattrbit ) {
				RB_EnableVertexAttrib( lmattr, true );
				qglVertexAttribPointer( lmattr, vbo->lmstSize[i],
										   FLOAT_VATTRIB_GL_TYPE( VATTRIB_LMCOORDS0_BIT, hfa ),
										   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->lmstOffset[i] );
			} else {
				RB_EnableVertexAttrib( lmattr, false );
			}

			lmattr++;
			lmattrbit <<= 2;
		}

		// lightmap array texture layers
		lmattr = VATTRIB_LMLAYERS0123;

		for( int i = 0; i < ( MAX_LIGHTMAPS + 3 ) / 4; i++ ) {
			if( vattribs & ( VATTRIB_LMLAYERS0123_BIT << i ) ) {
				RB_EnableVertexAttrib( lmattr, true );
				qglVertexAttribPointer( lmattr, 4, GL_UNSIGNED_BYTE,
										   GL_FALSE, vbo->vertexSize, ( const GLvoid * )vbo->lmlayersOffset[i] );
			} else {
				RB_EnableVertexAttrib( lmattr, false );
			}

			lmattr++;
		}
	}

	if( ( vattribs & VATTRIB_INSTANCES_BITS ) == VATTRIB_INSTANCES_BITS ) {
		RB_EnableVertexAttrib( VATTRIB_INSTANCE_QUAT, true );
		qglVertexAttribPointer( VATTRIB_INSTANCE_QUAT, 4, GL_FLOAT, GL_FALSE, 8 * sizeof( vec_t ),
								   ( const GLvoid * )vbo->instancesOffset );
		qglVertexAttribDivisor( VATTRIB_INSTANCE_QUAT, 1 );

		RB_EnableVertexAttrib( VATTRIB_INSTANCE_XYZS, true );
		qglVertexAttribPointer( VATTRIB_INSTANCE_XYZS, 4, GL_FLOAT, GL_FALSE, 8 * sizeof( vec_t ),
								   ( const GLvoid * )( vbo->instancesOffset + sizeof( vec_t ) * 4 ) );
		qglVertexAttribDivisor( VATTRIB_INSTANCE_XYZS, 1 );
	} else {
		RB_EnableVertexAttrib( VATTRIB_INSTANCE_QUAT, false );
		RB_EnableVertexAttrib( VATTRIB_INSTANCE_XYZS, false );
	}
}

void RB_DrawElementsReal( const DrawCallData &drawCallData ) {
	if( !( r_drawelements->integer || rb.currentEntity == &rb.nullEnt ) ) [[unlikely]] {
		return;
	}

	RB_ApplyScissor();

	if( const auto *mdSpan = std::get_if<MultiDrawElemSpan>( &drawCallData ) ) {
		qglMultiDrawElements( rb.primitive, mdSpan->counts, GL_UNSIGNED_SHORT, mdSpan->indices, mdSpan->numDraws );
	} else if( const auto *vertElemSpan = std::get_if<VertElemSpan>( &drawCallData ) ) {
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

static void RB_DrawElements_( const FrontendToBackendShared *fsh ) {
	assert( rb.currentShader );

	RB_EnableVertexAttribs();

	if( rb.wireframe ) {
		RB_DrawWireframeElements( fsh );
	} else {
		RB_DrawShadedElements( fsh );
	}
}

void RB_DrawElements( const FrontendToBackendShared *fsh, int firstVert, int numVerts, int firstElem, int numElems ) {
	RB_DrawElements( fsh, VertElemSpan { .firstVert = (unsigned)firstVert, .numVerts = (unsigned)numVerts,
										 .firstElem = (unsigned)firstElem, .numElems = (unsigned)numElems } );
}

void RB_DrawElements( const FrontendToBackendShared *fsh, const DrawCallData &drawCallData ) {
	rb.currentVAttribs &= ~VATTRIB_INSTANCES_BITS;

	rb.drawCallData = drawCallData;

	RB_DrawElements_( fsh );
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
	RB_DrawElements( fsh, 0, aliasmesh->numverts, 0, aliasmesh->numtris * 3 );
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
			RB_DrawElements( fsh, 0, skmesh->numverts, 0, skmesh->numtris * 3 );
			return;
		}
	}

	if( bonePoseRelativeDQ && skmesh->vbo ) {
		// another fastpath: transform the initial pose on the GPU
		RB_BindVBO( skmesh->vbo->index, GL_TRIANGLES );
		RB_SetBonesData( skmodel->numbones, bonePoseRelativeDQ, skmesh->maxWeights );
		RB_DrawElements( fsh, 0, skmesh->numverts, 0, skmesh->numtris * 3 );
		return;
	}
}

void R_SubmitBSPSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const drawSurfaceBSP_t *drawSurf ) {
	const MergedBspSurface *mergedBspSurf = drawSurf->mergedBspSurf;

	assert( !mergedBspSurf->numInstances );

	RB_BindVBO( mergedBspSurf->vbo->index, GL_TRIANGLES );
	RB_SetDlightBits( drawSurf->dlightBits );
	RB_SetLightstyle( mergedBspSurf->superLightStyle );

	RB_DrawElements( fsh, drawSurf->mdSpan );
}

void R_SubmitNullSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * ) {
	assert( rsh.nullVBO != NULL );

	RB_BindVBO( rsh.nullVBO->index, GL_LINES );
	RB_DrawElements( fsh, 0, 6, 0, 6 );
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

		RB_DrawElements( fsh, vertElemSpan );
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
		RB_DrawElements( fsh, vertElemSpan );
	}
}

void R_SubmitSpriteSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e,
								   const ShaderParams *, const ShaderParamsTable *paramsTable,
								   const shader_s *shader, const mfog_t *fog,
								   const portalSurface_t *portalSurface, unsigned meshIndex ) {
	auto *mesh = (mesh_t *)( (uint8_t *)fsh->preparedSpriteMeshes + meshIndex * fsh->preparedSpriteMeshStride );
	RB_AddDynamicMesh( e, shader, fog, portalSurface, 0, mesh, GL_TRIANGLES, 0.0f, 0.0f );
}