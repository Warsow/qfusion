/*
Copyright (C) 2012 Victor Luchits

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

#ifndef FTLIB_H
#define FTLIB_H

struct shader_s;
struct qfontface_s;

class Draw2DRequest;
class RenderSystem;

// font style flags
typedef enum {
	QFONT_STYLE_NONE            = 0,
	QFONT_STYLE_ITALIC          = ( 1 << 0 ),
	QFONT_STYLE_BOLD            = ( 1 << 1 ),
	QFONT_STYLE_MASK            = ( 1 << 2 ) - 1
} qfontstyle_t;

// font drawing flags
typedef enum {
	TEXTDRAWFLAG_NO_COLORS  = 1 << 0,   // draw color codes instead of applying them
	TEXTDRAWFLAG_KERNING    = 1 << 1
} textdrawflag_t;

bool FTLIB_Init( bool verbose );
void FTLIB_Shutdown( bool verbose );

struct qfontface_s *FTLIB_RegisterFont( RenderSystem *, const char *family, const char *fallback, int style, unsigned int size );
void FTLIB_TouchFont( RenderSystem *, struct qfontface_s *qfont );
void FTLIB_TouchAllFonts( RenderSystem * );
void FTLIB_PrecacheFonts( bool verbose );
void FTLIB_FreeFonts( bool verbose );

// drawing functions

size_t FTLIB_FontSize( struct qfontface_s *font );
size_t FTLIB_FontHeight( struct qfontface_s *font );
size_t FTLIB_StringWidth( RenderSystem *, const char *str, struct qfontface_s *font, size_t maxlen, int flags );
size_t FTLIB_StrlenForWidth( RenderSystem *, const char *str, struct qfontface_s *font, size_t maxwidth, int flags );
int FTLIB_FontUnderline( struct qfontface_s *font, int *thickness );
size_t FTLIB_FontAdvance( struct qfontface_s *font );
size_t FTLIB_FontXHeight( struct qfontface_s *font );
void FTLIB_DrawRawChar( RenderSystem *, Draw2DRequest *, int x, int y, wchar_t num, struct qfontface_s *font, const vec4_t color );
void FTLIB_DrawClampChar( RenderSystem *, Draw2DRequest *, int x, int y, wchar_t num, int xmin, int ymin, int xmax, int ymax, struct qfontface_s *font, const vec4_t color );
void FTLIB_DrawClampString( RenderSystem *, Draw2DRequest *, int x, int y, const char *str, int xmin, int ymin, int xmax, int ymax, struct qfontface_s *font, const vec4_t color, int flags );
size_t FTLIB_DrawRawString( RenderSystem *, Draw2DRequest *, int x, int y, const char *str, size_t maxwidth, int *width, struct qfontface_s *font, const vec4_t color, int flags );

#endif