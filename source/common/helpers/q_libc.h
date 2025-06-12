/*
Copyright (C) 1997-2001 Id Software, Inc.

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

#include <cstdlib>
#include <cstdarg>

#include <common/helpers/q_arch.h>

// This isn't exactly "libc" but well...

void Q_strncpyz( char *dest, const char *src, size_t size );
void Q_strncatz( char *dest, const char *src, size_t size );

int Q_vsnprintfz( char *dest, size_t size, const char *format, va_list argptr );

#ifndef _MSC_VER
int Q_snprintfz( char *dest, size_t size, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
int Q_snprintfz( char *dest, size_t size, _Printf_format_string_ const char *format, ... );
#endif

char *Q_strupr( char *s );
char *Q_strlwr( char *s );
const char *Q_strrstr( const char *s, const char *substr );
bool Q_isdigit( const char *str );
char *Q_trim( char *s );

/**
 * Converts the given null-terminated string to an URL encoded null-terminated string.
 * Only "unsafe" subset of characters are encoded.
 */
void Q_urlencode_unsafechars( const char *src, char *dst, size_t dst_size );
/**
 * Converts the given URL-encoded string to a null-terminated plain string. Returns
 * total (untruncated) length of the resulting string.
 */
size_t Q_urldecode( const char *src, char *dst, size_t dst_size );

void *Q_malloc( size_t size );
void *Q_realloc( void *buf, size_t newsize );
void Q_free( void *buf );
char *Q_strdup( const char *str );

int Com_GlobMatch( const char *pattern, const char *text, const bool casecmp );

// color string functions ("^1text" etc)
#define GRABCHAR_END    0
#define GRABCHAR_CHAR   1
#define GRABCHAR_COLOR  2
int Q_GrabCharFromColorString( const char **pstr, char *c, int *colorindex );
const char *COM_RemoveColorTokensExt( const char *str, bool draw );
#define COM_RemoveColorTokens( in ) COM_RemoveColorTokensExt( in,false )
int COM_SanitizeColorString( const char *str, char *buf, int bufsize, int maxprintablechars, int startcolor );
const char *Q_ColorStringTerminator( const char *str, int finalcolor );
int Q_ColorStrLastColor( int previous, const char *s, int maxlen );

const char *COM_RemoveJunkChars( const char *in );
int COM_ReadColorRGBString( const char *in );
int COM_ValidatePlayerColor( int rgbcolor );
bool COM_ValidateConfigstring( const char *string );

#ifdef IMPLEMENT_REMOVECOLORTOKENS

template <typename Buffer>
inline void removeColorTokens( Buffer *__restrict buffer ) {
	size_t readIndex = 0, writeIndex = 0;
	while( readIndex + 1 < buffer->size() ) {
		const char ch = ( *buffer )[readIndex];
		if( ch != '^' ) {
			( *buffer )[writeIndex++] = ch;
			readIndex += 1;
		} else {
			const char nextCh = ( *buffer )[readIndex + 1];
			if( (unsigned)( (int)nextCh - (int)'0' ) <= 9u ) {
				readIndex += 2;
			} else if( nextCh != '^' ) {
				// Treat a malformed ^-escape sequence as two regular characters
				( *buffer )[writeIndex++] = '^';
				readIndex += 1;
			} else {
				( *buffer )[writeIndex++] = '^';
				readIndex += 2;
			}
		}
	}
	if( readIndex != buffer->size() ) {
		( *buffer )[writeIndex++] = ( *buffer )[readIndex];
	}
	// Truncate
	buffer->erase( buffer->begin() + writeIndex );
}

#endif

size_t Q_WCharUtf8Length( wchar_t wc );
size_t Q_WCharToUtf8( wchar_t wc, char *dest, size_t bufsize );
char *Q_WCharToUtf8Char( wchar_t wc );
size_t Q_WCharToUtf8String( const wchar_t *ws, char *dest, size_t bufsize );
wchar_t Q_GrabWCharFromUtf8String( const char **pstr );
int Q_GrabWCharFromColorString( const char **pstr, wchar_t *wc, int *colorindex );
#define UTF8SYNC_LEFT 0
#define UTF8SYNC_RIGHT 1
int Q_Utf8SyncPos( const char *str, int pos, int dir );
void Q_FixTruncatedUtf8( char *str );
bool Q_IsBreakingSpace( const char *str );
bool Q_IsBreakingSpaceChar( wchar_t c );

float *tv( float x, float y, float z );
char *vtos( float v[3] );

#ifndef _MSC_VER
char *va( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
char *va_r( char *dst, size_t size, const char *format, ... ) __attribute__( ( format( printf, 3, 4 ) ) );
#else
char *va( _Printf_format_string_ const char *format, ... );
char *va_r( char *dst, size_t size, _Printf_format_string_ const char *format, ... );
#endif