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

#ifndef WSW_97ca6e2b_90f8_4974_b7f4_a61949a8bfb9_H
#define WSW_97ca6e2b_90f8_4974_b7f4_a61949a8bfb9_H

#include <common/helpers/actiontape.h>
#include "local.h"

struct RuntimeBackendState;

class BackendActionTape : public wsw::ActionTape<RuntimeBackendState *> {
public:
	void enable( GLenum cap );
	void disable( GLenum cap );

	void viewport( GLint x, GLint y, GLsizei width, GLsizei height );
	void scissor( GLint x, GLint y, GLsizei width, GLsizei height );

	void colorMask( GLboolean red, GLboolean greed, GLboolean blue, GLboolean alpha );
	void depthMask( GLboolean flag );
	void depthFunc( GLenum func );
	void blendFuncSeparate( GLenum srcRgb, GLenum dstRgb, GLenum srcAlpha, GLenum dstAlpha );

	void clearStencil( GLint value );
	void clear( GLbitfield mask );
	void clearColor( GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha );

	void cullFace( GLenum mode );
	void frontFace( GLenum mode );
	void depthRange( float nearVal, float farVal );
	void polygonMode( GLenum face, GLenum mode );

	void activeTexture( GLenum texture );
	void bindTexture( GLenum target, GLuint texture );
	void bindBuffer( GLenum target, GLuint buffer );

	void enableVertexAttrib( GLuint index );
	void vertexAttribPointer( GLuint index, GLint size, GLenum type, GLboolean normalized, GLsizei stride, GLuint baseOffset, GLuint memberOffset );
	void disableVertexAttrib( GLuint index );
	void vertexAttribDivisor( GLuint index, GLuint divisor );

	void drawRangeElements( GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices );
	void multiDrawElements( GLenum mode, const GLsizei *count, GLenum type, const void *const *indices, GLsizei drawcount );

	void bindRenderTarget( RenderTargetComponents *components );

	void bindProgram( int program );
	void createAndBindProgram( int type, const shader_s *materialToGetDeforms, uint64_t features );

	void bindBufferRange( GLenum target, GLuint index, GLuint buffer, GLintptr offset, GLsizeiptr size );
};

#endif