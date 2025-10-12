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

#include "glstateproxy.h"
#include "local.h"

GLStateProxy::GLStateProxy( int initialWidth, int initialHeight, int stencilBits ) {
	if( stencilBits ) {
		assert( stencilBits == 8 );
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

	m_scissor[2] = initialWidth;
	m_scissor[3] = initialHeight;
	m_scissorChanged = true;

	unbindAllTextures();
}

void GLStateProxy::setViewport( int x, int y, int w, int h ) {
	Vector4Set( m_viewport, x, y, w, h );
	qglViewport( x, m_fbHeight - h - y, w, h );
}

void GLStateProxy::setScissor( int x, int y, int w, int h ) {
	if( m_scissor[0] != x || m_scissor[1] != y || m_scissor[2] != w || m_scissor[3] != h ) {
		Vector4Set( m_scissor, x, y, w, h );
		m_scissorChanged = true;
	}
}

void GLStateProxy::getScissor( int *x, int *y, int *w, int *h ) {
	if( x ) {
		*x = m_scissor[0];
	}
	if( y ) {
		*y = m_scissor[1];
	}
	if( w ) {
		*w = m_scissor[2];
	}
	if( h ) {
		*h = m_scissor[3];
	}
}

void GLStateProxy::applyScissor() {
	if( m_scissorChanged ) {
		m_scissorChanged = false;
		const int h = m_scissor[3];
		qglScissor( m_scissor[0], m_fbHeight - h - m_scissor[1], m_scissor[2], h );
	}
}

void GLStateProxy::setCull( int cull ) {
	if( m_faceCull != cull ) {
		if( cull ) {
			if( !m_faceCull ) {
				qglEnable( GL_CULL_FACE );
			}
			qglCullFace( cull );
			m_faceCull = cull;
		} else {
			qglDisable( GL_CULL_FACE );
			m_faceCull = 0;
		}
	}
}

void GLStateProxy::flipFrontFace() {
	m_frontFace = !m_frontFace;
	qglFrontFace( m_frontFace ? GL_CW : GL_CCW );
}

void GLStateProxy::setDepthRange( float depthMin, float depthMax ) {
	depthMin = wsw::clamp( depthMin, 0.0f, 1.0f );
	depthMax = wsw::clamp( depthMax, 0.0f, 1.0f );
	m_depthMin = depthMin;
	m_depthMax = depthMax;
	// depthmin == depthmax is a special case when a specific depth value is going to be written
	if( ( depthMin != depthMax ) && !m_depthOffset ) {
		depthMin += 4.0f / 65535.0f;
	}
	qglDepthRange( depthMin, depthMax );
}

void GLStateProxy::getDepthRange( float *depthMin, float *depthMax ) {
	*depthMin = m_depthMin;
	*depthMax = m_depthMax;
}

void GLStateProxy::saveDepthRange() {
	assert( !m_hasSavedDepth );
	m_savedDepthMin = m_depthMin;
	m_savedDepthMax = m_depthMax;
	m_hasSavedDepth = true;
}

void GLStateProxy::restoreDepthRange() {
	assert( m_hasSavedDepth );
	m_hasSavedDepth = false;
	setDepthRange( m_savedDepthMin, m_savedDepthMax );
}

void GLStateProxy::setDepthOffsetEnabled( bool enabled ) {
	float depthMin = m_depthMin;
	float depthMax = m_depthMax;
	m_depthOffset = enabled;
	if( depthMin != depthMax ) {
		if( !enabled ) {
			depthMin += 4.0f / 65535.0f;
		}
		qglDepthRange( depthMin, depthMax );
	}
}

void GLStateProxy::bindTexture( int multitextureNumber, const Texture *texture ) {
	assert( texture );
	assert( texture->texnum != 0 );

	if( m_flushTextures ) {
		m_flushTextures = false;
		memset( m_currentTextures, 0, sizeof( m_currentTextures ) );
	}

	const GLuint texnum = texture->texnum;
	if( m_currentTextures[multitextureNumber] != texnum ) {
		m_currentTextures[multitextureNumber] = texnum;
		selectActiveMultitexture( multitextureNumber );
		qglBindTexture( texture->target, texture->texnum );
	}
}

void GLStateProxy::selectActiveMultitexture( int multitextureNumber ) {
	if( m_currentTMU != multitextureNumber ) {
		m_currentTMU = multitextureNumber;
		qglActiveTexture( GL_TEXTURE0 + multitextureNumber );
	}
}

void GLStateProxy::unbindAllTextures() {
	for( int tmu = 0; tmu < MAX_TEXTURE_UNITS; ++tmu ) {
		selectActiveMultitexture( tmu );

		qglBindTexture( GL_TEXTURE_CUBE_MAP, 0 );
		qglBindTexture( GL_TEXTURE_2D_ARRAY, 0 );
		qglBindTexture( GL_TEXTURE_3D, 0 );
		qglBindTexture( GL_TEXTURE_2D, 0 );
	}

	flushTextureCache();
}

void GLStateProxy::bindVertexBuffer( GLuint buffer ) {
	if( m_currentVertexBuffer != buffer ) {
		qglBindBuffer( GL_ARRAY_BUFFER, buffer );
		m_currentVertexBuffer = buffer;
		m_lastVAttribs = 0;
	}
}

void GLStateProxy::bindIndexBuffer( GLuint buffer ) {
	if( m_currentIndexBuffer != buffer ) {
		qglBindBuffer( GL_ELEMENT_ARRAY_BUFFER, buffer );
		m_currentIndexBuffer = buffer;
	}
}

void GLStateProxy::setState( unsigned state ) {
	const unsigned diff = m_state ^ state;
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

			if( !( m_state & GLSTATE_BLEND_MASK ) ) {
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
			setDepthOffsetEnabled( true );
		} else {
			qglDisable( GL_POLYGON_OFFSET_FILL );
			setDepthOffsetEnabled( false );
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

	m_state = state;
}

void GLStateProxy::enableVertexAttrib( int index, bool enable ) {
	const unsigned bit = 1 << index;
	const unsigned diff = ( m_vertexAttribEnabled & bit ) ^ ( enable ? bit : 0 );
	if( diff ) {
		if( enable ) {
			m_vertexAttribEnabled |= bit;
			qglEnableVertexAttribArray( index );
		} else {
			m_vertexAttribEnabled &= ~bit;
			qglDisableVertexAttribArray( index );
		}
	}
}

void GLStateProxy::enableVertexAttribs( vattribmask_t vattribs, const VboSpanLayout *layout ) {
	assert( vattribs & VATTRIB_POSITION_BIT );
	const vattribmask_t hfa = layout->halfFloatAttribs;
	if( vattribs == m_lastVAttribs && hfa == m_lastHalfFloatVAttribs && layout->baseOffset == m_lastVboSpanOffset ) {
		return;
	}

	m_lastVAttribs = vattribs;
	m_lastHalfFloatVAttribs = hfa;
	m_lastVboSpanOffset = layout->baseOffset;

	// xyz position
	enableVertexAttrib( VATTRIB_POSITION, true );
	qglVertexAttribPointer( VATTRIB_POSITION, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_POSITION_BIT, hfa ),
							GL_FALSE, layout->vertexSize, ( const GLvoid * )( uintptr_t )layout->baseOffset );

	// normal
	if( vattribs & VATTRIB_NORMAL_BIT ) {
		enableVertexAttrib( VATTRIB_NORMAL, true );
		const auto *pointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->normalsOffset );
		qglVertexAttribPointer( VATTRIB_NORMAL, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_NORMAL_BIT, hfa ),
								GL_FALSE, layout->vertexSize, pointer );
	} else {
		enableVertexAttrib( VATTRIB_NORMAL, false );
	}

	// s-vector
	if( vattribs & VATTRIB_SVECTOR_BIT ) {
		enableVertexAttrib( VATTRIB_SVECTOR, true );
		const auto *pointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->sVectorsOffset );
		qglVertexAttribPointer( VATTRIB_SVECTOR, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_SVECTOR_BIT, hfa ),
								GL_FALSE, layout->vertexSize, pointer );
	} else {
		enableVertexAttrib( VATTRIB_SVECTOR, false );
	}

	// color
	if( vattribs & VATTRIB_COLOR0_BIT ) {
		enableVertexAttrib( VATTRIB_COLOR0, true );
		const auto *pointer = (const GLvoid * )(uintptr_t)( layout->baseOffset + layout->colorsOffset[0] );
		qglVertexAttribPointer( VATTRIB_COLOR0, 4, GL_UNSIGNED_BYTE, GL_TRUE, layout->vertexSize, pointer );
	} else {
		enableVertexAttrib( VATTRIB_COLOR0, false );
	}

	// texture coordinates
	if( vattribs & VATTRIB_TEXCOORDS_BIT ) {
		enableVertexAttrib( VATTRIB_TEXCOORDS, true );
		const auto *pointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->stOffset );
		qglVertexAttribPointer( VATTRIB_TEXCOORDS, 2, FLOAT_VATTRIB_GL_TYPE( VATTRIB_TEXCOORDS_BIT, hfa ),
								GL_FALSE, layout->vertexSize, pointer );
	} else {
		enableVertexAttrib( VATTRIB_TEXCOORDS, false );
	}

	if( ( vattribs & VATTRIB_AUTOSPRITE_BIT ) == VATTRIB_AUTOSPRITE_BIT ) {
		// submit sprite point
		enableVertexAttrib( VATTRIB_SPRITEPOINT, true );
		const auto *pointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->spritePointsOffset );
		qglVertexAttribPointer( VATTRIB_SPRITEPOINT, 4, FLOAT_VATTRIB_GL_TYPE( VATTRIB_AUTOSPRITE_BIT, hfa ),
								GL_FALSE, layout->vertexSize, pointer );
	} else {
		enableVertexAttrib( VATTRIB_SPRITEPOINT, false );
	}

	// bones (skeletal models)
	if( ( vattribs & VATTRIB_BONES_BITS ) == VATTRIB_BONES_BITS ) {
		// submit indices
		enableVertexAttrib( VATTRIB_BONESINDICES, true );
		const auto *indicesPointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->bonesIndicesOffset );
		qglVertexAttribPointer( VATTRIB_BONESINDICES, 4, GL_UNSIGNED_BYTE, GL_FALSE, layout->vertexSize, indicesPointer );

		// submit weights
		enableVertexAttrib( VATTRIB_BONESWEIGHTS, true );
		const auto *weightsPointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->bonesWeightsOffset );
		qglVertexAttribPointer( VATTRIB_BONESWEIGHTS, 4, GL_UNSIGNED_BYTE, GL_TRUE, layout->vertexSize, weightsPointer );
	} else {
		// lightmap texture coordinates - aliasing bones, so not disabling bones
		int lmattr = VATTRIB_LMCOORDS01;
		int lmattrbit = VATTRIB_LMCOORDS0_BIT;

		for( int i = 0; i < ( MAX_LIGHTMAPS + 1 ) / 2; i++ ) {
			if( vattribs & lmattrbit ) {
				enableVertexAttrib( lmattr, true );
				const auto *pointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->lmstOffset[i] );
				qglVertexAttribPointer( lmattr, layout->lmstSize[i],
										FLOAT_VATTRIB_GL_TYPE( VATTRIB_LMCOORDS0_BIT, hfa ),
										GL_FALSE, layout->vertexSize, pointer );
			} else {
				enableVertexAttrib( lmattr, false );
			}

			lmattr++;
			lmattrbit <<= 2;
		}

		// lightmap array texture layers
		lmattr = VATTRIB_LMLAYERS0123;

		for( int i = 0; i < ( MAX_LIGHTMAPS + 3 ) / 4; i++ ) {
			if( vattribs & ( VATTRIB_LMLAYERS0123_BIT << i ) ) {
				enableVertexAttrib( lmattr, true );
				const auto *pointer = ( const GLvoid * )(uintptr_t)( layout->baseOffset + layout->lmlayersOffset[i] );
				qglVertexAttribPointer( lmattr, 4, GL_UNSIGNED_BYTE, GL_FALSE, layout->vertexSize, pointer );
			} else {
				enableVertexAttrib( lmattr, false );
			}

			lmattr++;
		}
	}

	if( ( vattribs & VATTRIB_INSTANCES_BITS ) == VATTRIB_INSTANCES_BITS ) {
		enableVertexAttrib( VATTRIB_INSTANCE_QUAT, true );
		qglVertexAttribPointer( VATTRIB_INSTANCE_QUAT, 4, GL_FLOAT, GL_FALSE, 8 * sizeof( vec_t ),
								( const GLvoid * )(uintptr_t)layout->instancesOffset );
		qglVertexAttribDivisor( VATTRIB_INSTANCE_QUAT, 1 );

		enableVertexAttrib( VATTRIB_INSTANCE_XYZS, true );
		qglVertexAttribPointer( VATTRIB_INSTANCE_XYZS, 4, GL_FLOAT, GL_FALSE, 8 * sizeof( vec_t ),
								( const GLvoid * )(uintptr_t)( layout->instancesOffset + sizeof( vec_t ) * 4 ) );
		qglVertexAttribDivisor( VATTRIB_INSTANCE_XYZS, 1 );
	} else {
		enableVertexAttrib( VATTRIB_INSTANCE_QUAT, false );
		enableVertexAttrib( VATTRIB_INSTANCE_XYZS, false );
	}
}

void GLStateProxy::bindFramebufferObject( GLStateProxy *holder, RenderTargetComponents *components ) {
	const int width  = components ? components->texture->width : glConfig.width;
	const int height = components ? components->texture->height : glConfig.height;

	if( holder ) {
		if( holder->m_fbWidth != width || holder->m_fbHeight != height ) {
			holder->m_scissorChanged = true;
		}
		holder->m_fbWidth = width;
		holder->m_fbHeight = height;
	}

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