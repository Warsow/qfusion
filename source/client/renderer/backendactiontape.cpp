/*
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

#include "backendactiontape.h"
#include "backendlocal.h"
#include "program.h"
#include "local.h"

void BackendActionTape::enable( GLenum cap ) {
	append( [=]( RuntimeBackendState * ) { qglEnable( cap ); } );
}

void BackendActionTape::disable( GLenum cap ) {
	append( [=]( RuntimeBackendState * ) { qglDisable( cap ); } );
}

void BackendActionTape::viewport( GLint x, GLint y, GLsizei width, GLsizei height ) {
	append( [=]( RuntimeBackendState * ) { qglViewport( x, y, width, height ); } );
}

void BackendActionTape::scissor( GLint x, GLint y, GLsizei width, GLsizei height ) {
	append( [=]( RuntimeBackendState * ) { qglScissor( x, y, width, height ); } );
}

void BackendActionTape::colorMask( GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha ) {
	append( [=]( RuntimeBackendState * ) { qglColorMask( red, green, blue, alpha ); } );
}

void BackendActionTape::depthMask( GLboolean flag ) {
	append( [=]( RuntimeBackendState * ) { qglDepthMask( flag ); } );
}

void BackendActionTape::depthFunc( GLenum func ) {
	append( [=]( RuntimeBackendState * ) { qglDepthFunc( func ); } );
}

void BackendActionTape::blendFuncSeparate( GLenum srcRgb, GLenum dstRgb, GLenum srcAlpha, GLenum dstAlpha ) {
	append( [=]( RuntimeBackendState * ) { qglBlendFuncSeparate( srcRgb, dstRgb, srcAlpha, dstAlpha ); } );
}

void BackendActionTape::clearStencil( GLint value ) {
	append( [=]( RuntimeBackendState * ) { qglClearStencil( value ); } );
}

void BackendActionTape::clear( GLbitfield mask ) {
	append( [=]( RuntimeBackendState * ) { qglClear( mask ); } );
}

void BackendActionTape::clearColor( GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha ) {
	append( [=]( RuntimeBackendState * ) { qglClearColor( red, green, blue, alpha ); } );
}

void BackendActionTape::cullFace( GLenum mode ) {
	append( [=]( RuntimeBackendState * ) { qglCullFace( mode ); } );
}

void BackendActionTape::frontFace( GLenum mode ) {
	append( [=]( RuntimeBackendState * ) { qglFrontFace( mode ); } );
}

void BackendActionTape::depthRange( float nearVal, float farVal ) {
	append( [=]( RuntimeBackendState * ) { qglDepthRange( nearVal, farVal ); } );
}

void BackendActionTape::polygonMode( GLenum face, GLenum mode ) {
	append( [=]( RuntimeBackendState * ) { qglPolygonMode( face, mode ); } );
}

void BackendActionTape::activeTexture( GLenum texture ) {
	append( [=]( RuntimeBackendState * ) { qglActiveTexture( texture ); } );
}

void BackendActionTape::bindTexture( GLenum target, GLuint texture ) {
	append( [=]( RuntimeBackendState * ) { qglBindTexture( target, texture ); } );
}

void BackendActionTape::bindBuffer( GLenum target, GLuint buffer ) {
	append( [=]( RuntimeBackendState * ) { qglBindBuffer( target, buffer ); } );
}

void BackendActionTape::enableVertexAttrib( GLuint index ) {
	append( [=]( RuntimeBackendState * ) { qglEnableVertexAttribArray( index ); } );
}

void BackendActionTape::vertexAttribPointer( GLuint index, GLint size, GLenum type, GLboolean normalized,
											GLsizei stride, GLuint baseOffset, GLuint memberOffset ) {
	// Create an intermediate variable to avoid capturing both arguments
	const GLuint combinedOffset = baseOffset + memberOffset;
	append( [=]( RuntimeBackendState * ) {
		qglVertexAttribPointer( index, size, type, normalized, stride, (const GLvoid *)(uintptr_t)combinedOffset );
	});
}

void BackendActionTape::disableVertexAttrib( GLuint index ) {
	append( [=]( RuntimeBackendState * ) { qglDisableVertexAttribArray( index ); } );
}

void BackendActionTape::vertexAttribDivisor( GLuint index, GLuint divisor ) {
	append( [=]( RuntimeBackendState * ) { qglVertexAttribDivisor( index, divisor ); } );
}

void BackendActionTape::drawRangeElements( GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices ) {
	append( [=]( RuntimeBackendState * ) { qglDrawRangeElements( mode, start, end, count, type, indices ); } );
}

void BackendActionTape::multiDrawElements( GLenum mode, const GLsizei *count, GLenum type, const void *const *indices, GLsizei drawcount ) {
	append( [=]( RuntimeBackendState * ) { qglMultiDrawElements( mode, count, type, indices, drawcount ); } );
}

void BackendActionTape::bindRenderTarget( RenderTargetComponents *components ) {
	append( [=]( RuntimeBackendState * ) { RB_BindRenderTarget( components ); } );
}

void BackendActionTape::bindProgram( int program ) {
	const int id = RP_GetProgramObject( program );
	append( [=]( RuntimeBackendState *rbs ) {
		if( rbs->programId != id ) {
			rbs->programId = id;
			qglUseProgram( id );
		}
	});
}

void BackendActionTape::createAndBindProgram( int type, const shader_s *materialToGetDeforms, uint64_t features ) {
	append( [=]( RuntimeBackendState *rbs ) {
		const int program = RP_GetProgram( type, materialToGetDeforms, features );
		const int id      = RP_GetProgramObject( program );
		if( rbs->programId != id ) {
			rbs->programId = id;
			qglUseProgram( id );
		}
	});
}

void BackendActionTape::bindBufferRange( GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size ) {
	append( [=]( RuntimeBackendState * ) { qglBindBufferRange( target, index, buffer, offset, size ); } );
}