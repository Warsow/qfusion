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

#ifndef WSW_e1cb64ea_0d69_4da8_9e68_acfbe33e9830_H
#define WSW_e1cb64ea_0d69_4da8_9e68_acfbe33e9830_H

#include <cstddef>
#include "vattribs.h"
#include "glimp.h"

struct mesh_vbo_s;
struct VboSpanLayout;
class RenderTargetComponents;
class BackendActionTape;
class Texture;

class GLStateProxy {
public:
	GLStateProxy( BackendActionTape *actionTape, int width, int height );

	[[nodiscard]]
	auto getState() const -> unsigned { return m_state; }

	void setState( unsigned state );

	void setCull( int cull );
	[[nodiscard]]
	auto getCull() const -> int { return m_faceCull; }

	void flipFrontFace();

	void setDepthRange( float depthMin, float depthMax );
	void getDepthRange( float *depthMin, float *depthMax );
	void saveDepthRange();
	void restoreDepthRange();
	void setDepthOffsetEnabled( bool enabled );

	void setScissor( int x, int y, int w, int h );
	void getScissor( int *x, int *y, int *w, int *h );
	void applyScissor();

	void setViewport( int x, int y, int w, int h );
	[[nodiscard]]
	auto getViewport() const -> const int * { return m_viewport; }

	void bindTexture( int multitextureNumber, const Texture *texture );

	void bindVertexBuffer( GLuint buffer );
	void bindIndexBuffer( GLuint buffer );

	void enableVertexAttribs( vattribmask_t attribs, const VboSpanLayout *layout );

	void drawRangeElements( GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, const void *indices );
	void multiDrawElements( GLenum mode, const GLsizei *count, GLenum type, const void *const *indices, GLsizei drawcount );

	void bindRenderTarget( RenderTargetComponents *components );

private:
	void selectActiveMultitexture( int multitextureNumber );
	// TODO: Convert to a template <bool>
	void enableVertexAttrib( int index, bool enable );

	unsigned m_state { 0 };

	GLuint m_currentVertexBuffer { 0 };
	GLuint m_currentIndexBuffer { 0 };

	int m_faceCull { 0 };
	bool m_frontFace { false };

	int m_viewport[4] { 0, 0, 0, 0 };
	int m_scissor[4] { 0, 0, 0, 0 };
	bool m_scissorChanged { false };

	unsigned m_vertexAttribEnabled { 0 };
	vattribmask_t m_lastVAttribs { 0 };
	vattribmask_t m_lastHalfFloatVAttribs { 0 };
	unsigned m_lastVboSpanOffset { 0 };

	int m_fbWidth { 0 };
	int m_fbHeight { 0 };

	float m_depthMin { 0.0f };
	float m_depthMax { 0.0f };
	float m_savedDepthMin { 0.0f };
	float m_savedDepthMax { 0.0f };
	bool m_hasSavedDepth { false };
	bool m_depthOffset { false };

	int m_currentTMU { 0 };
	unsigned m_currentTextures[MAX_TEXTURE_UNITS];

	BackendActionTape *const m_actionTape;
};

#endif