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
#include "backendlocal.h"
#include "../common/memspecbuilder.h"

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
	//rb.nullEnt.shaderTime = Sys_Milliseconds();
}

void RB_BeginFrame() {
	//Vector4Set( rb.nullEnt.shaderRGBA, 1, 1, 1, 1 );
	//rb.nullEnt.scale = 1;
	//VectorClear( rb.nullEnt.origin );
	//Matrix3_Identity( rb.nullEnt.axis );

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
	int h = rb.gl.scissor[3];
	if( rb.gl.scissorChanged ) {
		rb.gl.scissorChanged = false;
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
}

void RB_BindVBO( int id, int primitive ) {
	rb.primitive = primitive;

	mesh_vbo_t *vbo;
	if( id < RB_VBO_NONE ) {
		vbo = rb.dynamicStreams[-id - 1].vbo;
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

void RB_AddDynamicMesh( const ShaderParamsProvider *shaderParamsProvider, const shader_t *shader,
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
		if( const ShaderParamsProvider *prevParamsProvider = prev->shaderParamsProvider ) {
			if( const CommonParamsProvider *prevCommonParamsProvider = prevParamsProvider->commonParams ) {
				prevRenderFX = prevCommonParamsProvider->renderfx;
			}
		}
		if( shaderParamsProvider ) {
			if( const CommonParamsProvider *commonParamsProvider = shaderParamsProvider->commonParams ) {
				renderFX = commonParamsProvider->renderfx;
			}
		}
		if( ( ( shader->flags & SHADER_ENTITY_MERGABLE ) || ( prev->shaderParamsProvider == shaderParamsProvider ) ) && ( prevRenderFX == renderFX ) &&
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
		RB_BindShader( shaderParamsProvider, shader, fog );
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
		draw->shaderParamsProvider = shaderParamsProvider;
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
		draw->drawElements.numInstances = 0;
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
		RB_BindShader( draw->shaderParamsProvider, draw->shader, draw->fog );
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

void RB_DrawElementsReal( rbDrawElements_t *de ) {
	// TODO: We don't use null params provider, do we?
	if( !( r_drawelements->integer || /*rb.currentEntity == &rb.nullEnt*/ !rb.currentParamsProvider ) || !de ) {
		return;
	}

	RB_ApplyScissor();

	const int numVerts = de->numVerts;
	const int numElems = de->numElems;
	const int firstVert = de->firstVert;
	const int firstElem = de->firstElem;
	const int numInstances = de->numInstances;

	if( numInstances ) {
		// the instance data is contained in vertex attributes
		qglDrawElementsInstanced( rb.primitive, numElems, GL_UNSIGNED_SHORT,
								  (GLvoid *)( firstElem * sizeof( elem_t ) ), numInstances );
	} else {
		qglDrawRangeElements( rb.primitive, firstVert, firstVert + numVerts - 1, numElems,
							  GL_UNSIGNED_SHORT, (GLvoid *)( firstElem * sizeof( elem_t ) ) );
	}
}

static void RB_DrawElements_( const FrontendToBackendShared *fsh ) {
	if( rb.drawElements.numVerts && rb.drawElements.numElems ) [[likely]] {
		assert( rb.currentShader );

		RB_EnableVertexAttribs();

		if( rb.wireframe ) {
			RB_DrawWireframeElements( fsh );
		} else {
			RB_DrawShadedElements( fsh );
		}
	}
}

void RB_DrawElements( const FrontendToBackendShared *fsh, int firstVert, int numVerts, int firstElem, int numElems ) {
	rb.currentVAttribs &= ~VATTRIB_INSTANCES_BITS;

	rb.drawElements.numVerts = numVerts;
	rb.drawElements.numElems = numElems;
	rb.drawElements.firstVert = firstVert;
	rb.drawElements.firstElem = firstElem;
	rb.drawElements.numInstances = 0;

	RB_DrawElements_( fsh );
}

void RB_DrawElementsInstanced( const FrontendToBackendShared *fsh,
							   int firstVert, int numVerts, int firstElem, int numElems,
							   int numInstances, instancePoint_t *instances ) {
	if( numInstances ) {
		// currently not supporting dynamic instances
		// they will need a separate stream so they can be used with both static and dynamic geometry
		// (dynamic geometry will need changes to rbDynamicDraw_t)
		assert( rb.currentVBOId > RB_VBO_NONE );
		if( rb.currentVBOId > RB_VBO_NONE ) {
			rb.drawElements.numVerts = numVerts;
			rb.drawElements.numElems = numElems;
			rb.drawElements.firstVert = firstVert;
			rb.drawElements.firstElem = firstElem;
			rb.drawElements.numInstances = 0;

			if( rb.currentVBO->instancesOffset ) {
				// static VBO's must come with their own set of instance data
				rb.currentVAttribs |= VATTRIB_INSTANCES_BITS;
			}

			rb.drawElements.numInstances = numInstances;
			RB_DrawElements_( fsh );
		}
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
		cache = R_GetSkeletalCache( e->number, mod->lodnum );
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
	// shadowBits are shared for all rendering instances (normal view, portals, etc)
	const unsigned dlightBits = drawSurf->dlightBits;

	const unsigned numVerts = drawSurf->numSpanVerts;
	assert( numVerts );
	const unsigned numElems = drawSurf->numSpanElems;
	const unsigned firstVert = mergedBspSurf->firstVboVert + drawSurf->firstSpanVert;
	const unsigned firstElem = mergedBspSurf->firstVboElem + drawSurf->firstSpanElem;

	RB_BindVBO( mergedBspSurf->vbo->index, GL_TRIANGLES );

	RB_SetDlightBits( dlightBits );

	RB_SetLightstyle( mergedBspSurf->superLightStyle );

	if( mergedBspSurf->numInstances ) {
		RB_DrawElementsInstanced( fsh, firstVert, numVerts, firstElem, numElems, mergedBspSurf->numInstances, mergedBspSurf->instances );
	} else {
		RB_DrawElements( fsh, firstVert, numVerts, firstElem, numElems );
	}
}

void R_SubmitNullSurfToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog, const portalSurface_t *portalSurface, const void * ) {
	assert( rsh.nullVBO != NULL );

	RB_BindVBO( rsh.nullVBO->index, GL_LINES );
	RB_DrawElements( fsh, 0, 6, 0, 6 );
}

[[nodiscard]]
static auto findLightsThatAffectBounds( const Scene::DynamicLight *lights, std::span<const uint16_t> lightIndicesSpan,
										const float *mins, const float *maxs,
										uint16_t *affectingLightIndices ) -> unsigned {
	assert( mins[3] == 0.0f && maxs[3] == 1.0f );

	const uint16_t *lightIndices = lightIndicesSpan.data();
	const auto numLights         = (unsigned)lightIndicesSpan.size();

	unsigned lightIndexNum = 0;
	unsigned numAffectingLights = 0;
	do {
		const uint16_t lightIndex = lightIndices[lightIndexNum];
		const Scene::DynamicLight *light = lights + lightIndex;

		// TODO: Use SIMD explicitly without these redundant loads/shuffles
		const bool overlaps = BoundsIntersect( light->mins, light->maxs, mins, maxs );

		affectingLightIndices[numAffectingLights] = lightIndex;
		numAffectingLights += overlaps;
	} while( ++lightIndexNum < numLights );

	return numAffectingLights;
}

void R_SubmitSpriteSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
								   const mfog_t *fog, const portalSurface_t *portalSurface, std::span<const sortedDrawSurf_t> surfSpan ) {
	mesh_t mesh;

	for( size_t surfInSpan = 0; surfInSpan < surfSpan.size(); ++surfInSpan ) {

		vec3_t v_left, v_up;
		if( const float rotation = e->rotation; rotation != 0.0f ) {
			RotatePointAroundVector( v_left, &fsh->viewAxis[AXIS_FORWARD], &fsh->viewAxis[AXIS_RIGHT], rotation );
			CrossProduct( &fsh->viewAxis[AXIS_FORWARD], v_left, v_up );
		} else {
			VectorCopy( &fsh->viewAxis[AXIS_RIGHT], v_left );
			VectorCopy( &fsh->viewAxis[AXIS_UP], v_up );
		}

		if( fsh->renderFlags & RF_MIRRORVIEW ) {
			VectorInverse( v_left );
		}

		vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
		vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };

		vec3_t point;
		const float radius = e->radius * e->scale;
		VectorMA( e->origin, -radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[0] );
		VectorMA( point, -radius, v_left, xyz[3] );

		VectorMA( e->origin, radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[1] );
		VectorMA( point, -radius, v_left, xyz[2] );

		byte_vec4_t colors[4];
		for( unsigned i = 0; i < 4; i++ ) {
			VectorNegate( &fsh->viewAxis[AXIS_FORWARD], normals[i] );
			Vector4Copy( e->color, colors[i] );
		}

		elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
		vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

		mesh.elems = elems;
		mesh.numElems = 6;
		mesh.numVerts = 4;
		mesh.xyzArray = xyz;
		mesh.normalsArray = normals;
		mesh.lmstArray[0] = NULL;
		mesh.lmlayersArray[0] = NULL;
		mesh.stArray = texcoords;
		mesh.colorsArray[0] = colors;
		mesh.colorsArray[1] = NULL;
		mesh.sVectorsArray = NULL;

		RB_AddDynamicMesh( e, shader, fog, portalSurface, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
	}
}

void R_SubmitQuadPolysToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader, const mfog_t *fog,
								 const portalSurface_t *portalSurface, std::span<const sortedDrawSurf_t> surfSpan ) {
	uint16_t indices[6] { 0, 1, 2, 0, 2, 3 };

	vec4_t positions[4];
	byte_vec4_t colors[4];
	vec2_t texCoords[4];

	positions[0][3] = positions[1][3] = positions[2][3] = positions[3][3] = 1.0f;

	mesh_t mesh;
	std::memset( &mesh, 0, sizeof( mesh ) );

	mesh.elems    = indices;
	mesh.numElems = 6;
	mesh.numVerts = 4;
	mesh.xyzArray = positions;
	mesh.stArray  = texCoords;
	mesh.colorsArray[0] = colors;

	for( const sortedDrawSurf_t &sds: surfSpan ) {
		const auto *__restrict poly = (const QuadPoly *)sds.drawSurf;

		if( const auto *__restrict beamRules = std::get_if<QuadPoly::ViewAlignedBeamRules>( &poly->appearanceRules ) ) {
			assert( std::fabs( VectorLengthFast( beamRules->dir ) - 1.0f ) < 1.01f );
			vec3_t viewToOrigin, right;
			VectorSubtract( poly->origin, fsh->viewOrigin, viewToOrigin );
			CrossProduct( viewToOrigin, beamRules->dir, right );

			const float squareLength = VectorLengthSquared( right );
			if( squareLength > wsw::square( 0.001f ) ) [[likely]] {
				const float rcpLength = Q_RSqrt( squareLength );
				VectorScale( right, rcpLength, right );

				const float halfWidth = 0.5f * beamRules->width;

				vec3_t from, to;
				VectorMA( poly->origin, -poly->halfExtent, beamRules->dir, from );
				VectorMA( poly->origin, +poly->halfExtent, beamRules->dir, to );

				VectorMA( from, +halfWidth, right, positions[0] );
				VectorMA( from, -halfWidth, right, positions[1] );
				VectorMA( to, -halfWidth, right, positions[2] );
				VectorMA( to, +halfWidth, right, positions[3] );

				float stx = 1.0f;
				if( beamRules->tileLength > 0 ) {
					const float fullExtent = 2.0f * poly->halfExtent;
					stx = fullExtent * Q_Rcp( beamRules->tileLength );
				}

				Vector2Set( texCoords[0], 0.0f, 0.0f );
				Vector2Set( texCoords[1], 0.0f, 1.0f );
				Vector2Set( texCoords[2], stx, 1.0f );
				Vector2Set( texCoords[3], stx, 0.0f );

				const byte_vec4_t fromColorAsBytes {
					(uint8_t)( beamRules->fromColor[0] * 255 ),
					(uint8_t)( beamRules->fromColor[1] * 255 ),
					(uint8_t)( beamRules->fromColor[2] * 255 ),
					(uint8_t)( beamRules->fromColor[3] * 255 ),
				};
				const byte_vec4_t toColorAsBytes {
					(uint8_t)( beamRules->toColor[0] * 255 ),
					(uint8_t)( beamRules->toColor[1] * 255 ),
					(uint8_t)( beamRules->toColor[2] * 255 ),
					(uint8_t)( beamRules->toColor[3] * 255 ),
				};

				Vector4Copy( fromColorAsBytes, colors[0] );
				Vector4Copy( fromColorAsBytes, colors[1] );
				Vector4Copy( toColorAsBytes, colors[2] );
				Vector4Copy( toColorAsBytes, colors[3] );

				RB_AddDynamicMesh( e, shader, nullptr, nullptr, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
			}
		} else {
			vec3_t left, up;
			const float *color;

			if( const auto *orientedRules = std::get_if<QuadPoly::OrientedSpriteRules>( &poly->appearanceRules ) ) {
				color = orientedRules->color;
				VectorCopy( &orientedRules->axis[AXIS_RIGHT], left );
				VectorCopy( &orientedRules->axis[AXIS_UP], up );
			} else {
				color = std::get_if<QuadPoly::OrientedSpriteRules>( &poly->appearanceRules )->color;
				VectorCopy( &fsh->viewAxis[AXIS_RIGHT], left );
				VectorCopy( &fsh->viewAxis[AXIS_UP], up );
			}

			if( fsh->renderFlags & RF_MIRRORVIEW ) {
				VectorInverse( left );
			}

			vec3_t point;
			const float radius = poly->halfExtent;
			VectorMA( poly->origin, -radius, up, point );
			VectorMA( point, +radius, left, positions[0] );
			VectorMA( point, -radius, left, positions[3] );

			VectorMA( poly->origin, radius, up, point );
			VectorMA( point, +radius, left, positions[1] );
			VectorMA( point, -radius, left, positions[2] );

			Vector2Set( texCoords[0], 0.0f, 0.0f );
			Vector2Set( texCoords[1], 0.0f, 1.0f );
			Vector2Set( texCoords[2], 1.0f, 1.0f );
			Vector2Set( texCoords[3], 1.0f, 0.0f );

			colors[0][0] = ( uint8_t )( color[0] * 255 );
			colors[0][1] = ( uint8_t )( color[1] * 255 );
			colors[0][2] = ( uint8_t )( color[2] * 255 );
			colors[0][3] = ( uint8_t )( color[3] * 255 );

			Vector4Copy( colors[0], colors[1] );
			Vector4Copy( colors[0], colors[2] );
			Vector4Copy( colors[0], colors[3] );

			RB_AddDynamicMesh( e, shader, nullptr, nullptr, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
		}
	}
}

void R_SubmitDynamicMeshesToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
									const mfog_t *fog, const portalSurface_t *portalSurface, std::span<const sortedDrawSurf_t> surfSpan ) {
	// Maximum supported icosphere subdiv level
	// TODO check these values, share with the icosphere code
	// TODO we do not have to transfer icosphere indices every frame
	constexpr auto maxStorageVertices = 2562;
	constexpr auto maxStorageIndices  = 15360;

	// TODO: Point to the dynamic stream memory
	alignas( 16 ) vec4_t positions[maxStorageVertices];
	alignas( 16 ) vec4_t normals[maxStorageVertices];
	alignas( 16 ) vec2_t texCoords[maxStorageVertices];
	alignas( 16 ) byte_vec4_t colors[maxStorageVertices];
	alignas( 16 ) uint16_t indices[maxStorageIndices];

	uint16_t *affectingLightsStorage = nullptr;
	const bool hasAvailableLights    = r_dynamiclight->integer && !fsh->allVisibleLightIndices.empty();

	alignas( alignof( void * ) ) uint8_t scratchpad[64];
	for( const sortedDrawSurf_t &sds: surfSpan ) {
		const auto *__restrict mesh = (const DynamicMesh *)sds.drawSurf;

		// This call is more useful if dynamic stream memory is used directly
		// (we can flush the existing buffer if needed and write to now-free space).
		const auto maybeMaxRequirements = mesh->getStorageRequirements( fsh->viewOrigin, fsh->viewAxis, fsh->fovTangent,
																		fsh->cameraId, scratchpad );
		// Won't draw itself
		if( !maybeMaxRequirements ) {
			continue;
		}

		const auto [maxMeshVertices, maxMeshIndices] = *maybeMaxRequirements;
		if( maxMeshVertices > maxStorageVertices || maxMeshIndices > maxStorageIndices ) [[unlikely]] {
			continue;
		}

		std::span<const uint16_t> affectingLightIndices;
		if( mesh->applyVertexDynLight && hasAvailableLights ) {
			// Do alloca() once per loop
			if( !affectingLightsStorage ) [[unlikely]] {
				affectingLightsStorage = (uint16_t *)alloca( sizeof( uint16_t ) * fsh->allVisibleLightIndices.size() );
			}
			const auto numAffectingLights = findLightsThatAffectBounds( fsh->dynamicLights,
																		fsh->allVisibleLightIndices,
																		mesh->cullMins, mesh->cullMaxs,
																		affectingLightsStorage );
			affectingLightIndices = { affectingLightsStorage, numAffectingLights };
		}

		const auto [numPolyVertices, numPolyIndices] = mesh->fillMeshBuffers( fsh->viewOrigin, fsh->viewAxis,
																			  fsh->fovTangent,
																			  fsh->cameraId,
																			  fsh->dynamicLights, affectingLightIndices,
																			  scratchpad,
																			  positions, normals,
																			  texCoords, colors, indices );

		// TODO....
		RB_FlushDynamicMeshes();
		// TODO: Get rid of "mesh_", write to the dynamic stream memory directly

		mesh_t mesh_;
		memset( &mesh_, 0, sizeof( mesh_ ) );

		mesh_.elems          = indices;
		mesh_.numElems       = numPolyIndices;
		mesh_.numVerts       = numPolyVertices;
		mesh_.xyzArray       = positions;
		mesh_.stArray        = texCoords;
		mesh_.colorsArray[0] = colors;

		RB_AddDynamicMesh( e, shader, fog, portalSurface, 0, &mesh_, GL_TRIANGLES, 0.0f, 0.0f );
	}
}

static wsw_forceinline void calcAddedParticleLight( const float *__restrict particleOrigin,
													const Scene::DynamicLight *__restrict lights,
													std::span<const uint16_t> affectingLightIndices,
													float *__restrict addedLight ) {
	assert( !affectingLightIndices.empty() );

	size_t lightNum = 0;
	do {
		const Scene::DynamicLight *light = lights + affectingLightIndices[lightNum];
		const float squareDistance = DistanceSquared( light->origin, particleOrigin );
		// May go outside [0.0, 1.0] as we test against the bounding box of the entire aggregate
		float impactStrength = 1.0f - Q_Sqrt( squareDistance ) * Q_Rcp( light->maxRadius );
		// Just clamp so the code stays branchless
		impactStrength = wsw::clamp( impactStrength, 0.0f, 1.0f );
		VectorMA( addedLight, impactStrength, light->color, addedLight );
	} while( ++lightNum < affectingLightIndices.size() );
}

static void submitSpriteParticlesToBackend( const FrontendToBackendShared *fsh,
											const Scene::ParticlesAggregate *aggregate,
											const entity_t *entity,
											const shader_t *shader,
											std::span<const sortedDrawSurf_t> surfSpan,
											std::span<const uint16_t> affectingLightIndices ) {
	const auto *__restrict appearanceRules = &aggregate->appearanceRules;
	const auto *__restrict spriteRules     = std::get_if<Particle::SpriteRules>( &appearanceRules->geometryRules );
	const bool applyLight                  = !affectingLightIndices.empty();

	// TODO: Write directly to mapped buffers
	mesh_t mesh;
	elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
	vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
	vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
	byte_vec4_t colors[4];
	vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

	for( const sortedDrawSurf_t &sds: surfSpan ) {
		const auto *drawSurf = (const ParticleDrawSurface *)sds.drawSurf;

		// Ensure that the aggregate is the same
		assert( fsh->particleAggregates + drawSurf->aggregateIndex == aggregate );

		assert( drawSurf->particleIndex < aggregate->numParticles );
		const Particle *const __restrict particle = aggregate->particles + drawSurf->particleIndex;

		assert( particle->lifetimeFrac >= 0.0f && particle->lifetimeFrac <= 1.0f );

		assert( spriteRules->radius.mean > 0.0f );
		assert( spriteRules->radius.spread >= 0.0f );

		float signedFrac = Particle::kByteSpreadNormalizer * (float)particle->instanceRadiusSpreadFraction;
		float radius     = wsw::max( 0.0f, spriteRules->radius.mean + signedFrac * spriteRules->radius.spread );

		radius *= Particle::kScaleOfByteExtraScale * (float)particle->instanceRadiusExtraScale;

		if( spriteRules->sizeBehaviour != Particle::SizeNotChanging ) {
			radius *= calcSizeFracForLifetimeFrac( particle->lifetimeFrac, spriteRules->sizeBehaviour );
		}

		if( radius < 0.1f ) {
			continue;
		}

		vec3_t v_left, v_up;
		if( particle->rotationAngle != 0.0f ) {
			mat3_t axis;
			Matrix3_Rotate( fsh->viewAxis, particle->rotationAngle, &fsh->viewAxis[AXIS_FORWARD], axis );
			VectorCopy( &axis[AXIS_RIGHT], v_left );
			VectorCopy( &axis[AXIS_UP], v_up );
		} else {
			VectorCopy( &fsh->viewAxis[AXIS_RIGHT], v_left );
			VectorCopy( &fsh->viewAxis[AXIS_UP], v_up );
		}

		if( fsh->renderFlags & RF_MIRRORVIEW ) {
			VectorInverse( v_left );
		}

		vec3_t point;
		VectorMA( particle->origin, -radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[0] );
		VectorMA( point, -radius, v_left, xyz[3] );

		VectorMA( particle->origin, radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[1] );
		VectorMA( point, -radius, v_left, xyz[2] );

		vec4_t colorBuffer;
		const RgbaLifespan &colorLifespan = appearanceRules->colors[particle->instanceColorIndex];
		colorLifespan.getColorForLifetimeFrac( particle->lifetimeFrac, colorBuffer );

		if( applyLight ) {
			vec4_t addedLight { 0.0f, 0.0f, 0.0f, 1.0f };
			calcAddedParticleLight( particle->origin, fsh->dynamicLights, affectingLightIndices, addedLight );

			// TODO: Pass as a floating-point attribute to a GPU program?
			colorBuffer[0] = wsw::min( 1.0f, colorBuffer[0] + addedLight[0] );
			colorBuffer[1] = wsw::min( 1.0f, colorBuffer[1] + addedLight[1] );
			colorBuffer[2] = wsw::min( 1.0f, colorBuffer[2] + addedLight[2] );
		}

		Vector4Set( colors[0],
					(uint8_t)( 255 * colorBuffer[0] ),
					(uint8_t)( 255 * colorBuffer[1] ),
					(uint8_t)( 255 * colorBuffer[2] ),
					(uint8_t)( 255 * colorBuffer[3] ) );

		Vector4Copy( colors[0], colors[1] );
		Vector4Copy( colors[0], colors[2] );
		Vector4Copy( colors[0], colors[3] );

		// TODO: Write directly to mapped buffers
		memset( &mesh, 0, sizeof( mesh ) );
		mesh.numElems = 6;
		mesh.elems = elems;
		mesh.numVerts = 4;
		mesh.xyzArray = xyz;
		mesh.normalsArray = normals;
		mesh.stArray = texcoords;
		mesh.colorsArray[0] = colors;

		RB_AddDynamicMesh( entity, shader, nullptr, nullptr, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
	}
}

static void submitSparkParticlesToBackend( const FrontendToBackendShared *fsh,
										   const Scene::ParticlesAggregate *aggregate,
										   const entity_t *entity,
										   const shader_t *shader,
										   std::span<const sortedDrawSurf_t> surfSpan,
										   std::span<const uint16_t> affectingLightIndices ) {
	const auto *__restrict appearanceRules = &aggregate->appearanceRules;
	const auto *__restrict sparkRules      = std::get_if<Particle::SparkRules>( &appearanceRules->geometryRules );
	const bool applyLight                  = !affectingLightIndices.empty();

	// TODO: Write directly to mapped buffers
	mesh_t mesh;
	elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
	vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
	vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
	byte_vec4_t colors[4];
	vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

	for( const sortedDrawSurf_t &sds: surfSpan ) {
		const auto *drawSurf = (const ParticleDrawSurface *)sds.drawSurf;

		// Ensure that the aggregate is the same
		assert( fsh->particleAggregates + drawSurf->aggregateIndex == aggregate );

		assert( drawSurf->particleIndex < aggregate->numParticles );
		const Particle *const __restrict particle = aggregate->particles + drawSurf->particleIndex;

		assert( particle->lifetimeFrac >= 0.0f && particle->lifetimeFrac <= 1.0f );

		assert( sparkRules->length.mean >= 0.1f && sparkRules->width.mean >= 0.1f );
		assert( sparkRules->length.spread >= 0.0f && sparkRules->width.spread >= 0.0f );

		const float lengthSignedFrac = Particle::kByteSpreadNormalizer * (float)particle->instanceLengthSpreadFraction;
		const float widthSignedFrac  = Particle::kByteSpreadNormalizer * (float)particle->instanceWidthSpreadFraction;

		float length = wsw::max( 0.0f, sparkRules->length.mean + lengthSignedFrac * sparkRules->length.spread );
		float width  = wsw::max( 0.0f, sparkRules->width.mean + widthSignedFrac * sparkRules->width.spread );

		length *= Particle::kScaleOfByteExtraScale * (float)particle->instanceLengthExtraScale;
		width  *= Particle::kScaleOfByteExtraScale * (float)particle->instanceWidthExtraScale;

		const Particle::SizeBehaviour sizeBehaviour = sparkRules->sizeBehaviour;
		if( sizeBehaviour != Particle::SizeNotChanging ) {
			const float sizeFrac = calcSizeFracForLifetimeFrac( particle->lifetimeFrac, sizeBehaviour );
			if( sizeBehaviour != Particle::SizeBehaviour::Thinning && sizeBehaviour != Particle::SizeBehaviour::Thickening && sizeBehaviour != Particle::SizeBehaviour::ThickeningAndThinning ) {
				length *= sizeFrac;
			}
			width *= sizeFrac;
		}

		if( length < 0.1f || width < 0.1f ) {
			continue;
		}

		vec3_t particleDir;
		float fromFrac, toFrac;
		vec3_t visualVelocity;
		VectorAdd( particle->dynamicsVelocity, particle->artificialVelocity, visualVelocity );
		if( const float squareVisualSpeed = VectorLengthSquared( visualVelocity ); squareVisualSpeed > 1.0f ) [[likely]] {
			const float rcpVisualSpeed = Q_RSqrt( squareVisualSpeed );
			if( particle->rotationAngle == 0.0f ) [[likely]] {
				VectorScale( visualVelocity, rcpVisualSpeed, particleDir );
				fromFrac = 0.0f, toFrac = 1.0f;
			} else {
				vec3_t tmpParticleDir;
				VectorScale( visualVelocity, rcpVisualSpeed, tmpParticleDir );

				mat3_t rotationMatrix;
				const float *rotationAxis = kPredefinedDirs[particle->rotationAxisIndex];
				Matrix3_Rotate( axis_identity, particle->rotationAngle, rotationAxis, rotationMatrix );
				Matrix3_TransformVector( rotationMatrix, tmpParticleDir, particleDir );

				fromFrac = -0.5f, toFrac = +0.5f;
			}
		} else {
			continue;
		}

		assert( std::fabs( VectorLengthSquared( particleDir ) - 1.0f ) < 0.1f );

		// Reduce the viewDir-aligned part of the particleDir
		const float *const __restrict viewDir = &fsh->viewAxis[AXIS_FORWARD];
		assert( sparkRules->viewDirPartScale >= 0.0f && sparkRules->viewDirPartScale <= 1.0f );
		const float viewDirCutScale = ( 1.0f - sparkRules->viewDirPartScale ) * DotProduct( particleDir, viewDir );
		if( std::fabs( viewDirCutScale ) < 0.999f ) [[likely]] {
			VectorMA( particleDir, -viewDirCutScale, viewDir, particleDir );
			VectorNormalizeFast( particleDir );
		} else {
			continue;
		}

		vec3_t from, to, mid;
		VectorMA( particle->origin, fromFrac * length, particleDir, from );
		VectorMA( particle->origin, toFrac * length, particleDir, to );
		VectorAvg( from, to, mid );

		vec3_t viewToMid, right;
		VectorSubtract( mid, fsh->viewOrigin, viewToMid );
		CrossProduct( viewToMid, particleDir, right );
		if( const float squareLength = VectorLengthSquared( right ); squareLength > wsw::square( 0.001f ) ) [[likely]] {
			const float rcpLength = Q_RSqrt( squareLength );
			VectorScale( right, rcpLength, right );

			const float halfWidth = 0.5f * width;

			VectorMA( from, +halfWidth, right, xyz[0] );
			VectorMA( from, -halfWidth, right, xyz[1] );
			VectorMA( to, -halfWidth, right, xyz[2] );
			VectorMA( to, +halfWidth, right, xyz[3] );
		} else {
			continue;
		}

		vec4_t colorBuffer;
		const RgbaLifespan &colorLifespan = appearanceRules->colors[particle->instanceColorIndex];
		colorLifespan.getColorForLifetimeFrac( particle->lifetimeFrac, colorBuffer );

		if( applyLight ) {
			alignas( 16 ) vec4_t addedLight { 0.0f, 0.0f, 0.0f, 1.0f };
			calcAddedParticleLight( particle->origin, fsh->dynamicLights, affectingLightIndices, addedLight );

			// The clipping due to LDR limitations sucks...
			// TODO: Pass as a floating-point attribute to a GPU program?
			colorBuffer[0] = wsw::min( 1.0f, colorBuffer[0] + addedLight[0] );
			colorBuffer[1] = wsw::min( 1.0f, colorBuffer[1] + addedLight[1] );
			colorBuffer[2] = wsw::min( 1.0f, colorBuffer[2] + addedLight[2] );
		}

		Vector4Set( colors[0],
					(uint8_t)( 255 * colorBuffer[0] ),
					(uint8_t)( 255 * colorBuffer[1] ),
					(uint8_t)( 255 * colorBuffer[2] ),
					(uint8_t)( 255 * colorBuffer[3] ) );

		Vector4Copy( colors[0], colors[1] );
		Vector4Copy( colors[0], colors[2] );
		Vector4Copy( colors[0], colors[3] );

		// TODO: Write directly to mapped buffers
		memset( &mesh, 0, sizeof( mesh ) );
		mesh.numElems = 6;
		mesh.elems = elems;
		mesh.numVerts = 4;
		mesh.xyzArray = xyz;
		mesh.normalsArray = normals;
		mesh.stArray = texcoords;
		mesh.colorsArray[0] = colors;

		RB_AddDynamicMesh( entity, shader, nullptr, nullptr, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
	}
}

void R_SubmitParticleSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
									 const mfog_t *fog, const portalSurface_t *portalSurface, std::span<const sortedDrawSurf_t> surfSpan ) {
	assert( !surfSpan.empty() );

	const auto *const firstDrawSurf = (const ParticleDrawSurface *)surfSpan.front().drawSurf;
	const auto *const aggregate = fsh->particleAggregates + firstDrawSurf->aggregateIndex;
	// Less if the aggregate is visually split by some surfaces of other kinds
	assert( surfSpan.size() <= aggregate->numParticles );

	const Particle::AppearanceRules *const appearanceRules = &aggregate->appearanceRules;

	unsigned numAffectingLights     = 0;
	uint16_t *affectingLightIndices = nullptr;
	std::span<const uint16_t> lightIndicesSpan;
	if( appearanceRules->applyVertexDynLight && r_dynamiclight->integer && !fsh->allVisibleLightIndices.empty() ) {
		affectingLightIndices = (uint16_t *)alloca( sizeof( uint16_t ) * fsh->allVisibleLightIndices.size() );

		numAffectingLights = findLightsThatAffectBounds( fsh->dynamicLights, fsh->allVisibleLightIndices,
														 aggregate->mins, aggregate->maxs, affectingLightIndices );

		lightIndicesSpan = { affectingLightIndices, numAffectingLights };
	}

	if( std::holds_alternative<Particle::SpriteRules>( appearanceRules->geometryRules ) ) {
		submitSpriteParticlesToBackend( fsh, aggregate, e, shader, surfSpan, lightIndicesSpan );
	} else {
		submitSparkParticlesToBackend( fsh, aggregate, e, shader, surfSpan, lightIndicesSpan );
	}
}

void R_SubmitCoronaSurfsToBackend( const FrontendToBackendShared *fsh, const entity_t *e, const shader_t *shader,
								   const mfog_t *fog, const portalSurface_t *portalSurface, std::span<const sortedDrawSurf_t> surfSpan ) {
	mesh_t mesh;
	elem_t elems[6] = { 0, 1, 2, 0, 2, 3 };
	vec4_t xyz[4] = { {0,0,0,1}, {0,0,0,1}, {0,0,0,1}, {0,0,0,1} };
	vec4_t normals[4] = { {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0} };
	byte_vec4_t colors[4];
	vec2_t texcoords[4] = { {0, 1}, {0, 0}, {1,0}, {1,1} };

	vec3_t v_left, v_up;
	VectorCopy( &fsh->viewAxis[AXIS_RIGHT], v_left );
	VectorCopy( &fsh->viewAxis[AXIS_UP], v_up );

	if( fsh->renderFlags & RF_MIRRORVIEW ) {
		VectorInverse( v_left );
	}

	for( const sortedDrawSurf_t &sds: surfSpan ) {
		const auto *light = (const Scene::DynamicLight *)sds.drawSurf;

		assert( light && light->hasCoronaLight );

		const float radius = light->coronaRadius;

		vec3_t origin;
		VectorCopy( light->origin, origin );

		vec3_t point;
		VectorMA( origin, -radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[0] );
		VectorMA( point, -radius, v_left, xyz[3] );

		VectorMA( origin, radius, v_up, point );
		VectorMA( point, radius, v_left, xyz[1] );
		VectorMA( point, -radius, v_left, xyz[2] );

		Vector4Set( colors[0],
					bound( 0, light->color[0] * 96, 255 ),
					bound( 0, light->color[1] * 96, 255 ),
					bound( 0, light->color[2] * 96, 255 ),
					255 );

		Vector4Copy( colors[0], colors[1] );
		Vector4Copy( colors[0], colors[2] );
		Vector4Copy( colors[0], colors[3] );

		memset( &mesh, 0, sizeof( mesh ) );
		mesh.numElems = 6;
		mesh.elems = elems;
		mesh.numVerts = 4;
		mesh.xyzArray = xyz;
		mesh.normalsArray = normals;
		mesh.stArray = texcoords;
		mesh.colorsArray[0] = colors;

		RB_AddDynamicMesh( e, shader, fog, portalSurface, 0, &mesh, GL_TRIANGLES, 0.0f, 0.0f );
	}
}