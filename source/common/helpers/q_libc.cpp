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

#include <common/helpers/q_libc.h>
#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h> // fixme : needed for MAX_S_COLORS define
#include <common/helpers/exceptions.h>
#include <common/helpers/glob.h>
//#include <common/local.h>
#include <common/facilities/q_comref.h>
#include <common/facilities/messagestreams.h>

#include <climits>
#include <cctype>
#include <cstdio>
#include <cstring>

//============================================================================

/*
* TempVector
*
* This is just a convenience function
* for making temporary vectors for function calls
*/
float *tv( float x, float y, float z ) {
	thread_local static int index;
	thread_local static float vecs[8][3];
	float *v;

	// use an array so that multiple tempvectors won't collide
	// for a while
	v = vecs[index];
	index = ( index + 1 ) & 7;

	v[0] = x;
	v[1] = y;
	v[2] = z;

	return v;
}

/*
* VectorToString
*
* This is just a convenience function for printing vectors
*/
char *vtos( float v[3] ) {
	thread_local static int index;
	thread_local static char str[8][32];
	char *s;

	// use an array so that multiple vtos won't collide
	s = str[index];
	index = ( index + 1 ) & 7;

	Q_snprintfz( s, 32, "(%+6.3f %+6.3f %+6.3f)", v[0], v[1], v[2] );

	return s;
}

/*
* va_r
*
* does a varargs printf into a temp buffer, so I don't need to have
* varargs versions of all text functions.
*/
char *va_r( char *dest, size_t size, const char *format, ... ) {
	va_list argptr;
	va_start( argptr, format );
	Q_vsnprintfz( dest, size, format, argptr );
	va_end( argptr );
	return dest;
}

/*
* va
*
* does a varargs printf into a temp buffer, so I don't need to have
* varargs versions of all text functions.
*/
char *va( const char *format, ... ) {
	va_list argptr;
	thread_local static int str_index;
	thread_local static char string[8][2048];

	str_index = ( str_index + 1 ) & 7;
	va_start( argptr, format );
	Q_vsnprintfz( string[str_index], sizeof( string[0] ), format, argptr );
	va_end( argptr );

	return string[str_index];
}


/*
* Q_GrabCharFromColorString
*
* Parses a char or color escape sequence and advances (*pstr)
* "c" receives the character
* "colorindex", if not NULL, receives color indexes (0..10)
* Return values:
* GRABCHAR_END - end of string reached; *c will be '\0';  *colorindex is undefined
* GRABCHAR_CHAR - printable char parsed and saved to *c;  *colorindex is undefined
* GRABCHAR_COLOR - color escape parsed and saved to *colorindex;  *c is undefined
*/
int Q_GrabCharFromColorString( const char **pstr, char *c, int *colorindex ) {
	switch( **pstr ) {
		case '\0':
			*c = '\0';
			return GRABCHAR_END;

		case Q_COLOR_ESCAPE:
			if( ( *pstr )[1] >= '0' && ( *pstr )[1] < '0' + MAX_S_COLORS ) {
				if( colorindex ) {
					*colorindex = ColorIndex( ( *pstr )[1] );
				}
				( *pstr ) += 2; // skip the ^7
				return GRABCHAR_COLOR;
			} else if( ( *pstr )[1] == Q_COLOR_ESCAPE ) {
				*c = Q_COLOR_ESCAPE;
				( *pstr ) += 2; // skip the ^^
				return GRABCHAR_CHAR;
			}
		/* fall through */

		default:
			// normal char
			*c = **pstr;
			( *pstr )++;
			return GRABCHAR_CHAR;
	}
}

// Like Q_GrabCharFromColorString, but reads whole UTF-8 sequences
// and returns wide chars
int Q_GrabWCharFromColorString( const char **pstr, wchar_t *wc, int *colorindex ) {
	wchar_t num;

	num = Q_GrabWCharFromUtf8String( pstr );
	switch( num ) {
		case 0:
			*wc = 0;
			return GRABCHAR_END;

		case Q_COLOR_ESCAPE:
			if( **pstr >= '0' && **pstr < '0' + MAX_S_COLORS ) {
				if( colorindex ) {
					*colorindex = ColorIndex( **pstr );
				}
				( *pstr )++; // skip the color code
				return GRABCHAR_COLOR;
			} else if( **pstr == Q_COLOR_ESCAPE ) {
				*wc = Q_COLOR_ESCAPE;
				( *pstr )++; // skip the second ^
				return GRABCHAR_CHAR;
			}
		/* fall through */

		default:
			// normal char
			*wc = num;
			return GRABCHAR_CHAR;
	}
}


/*
* COM_RemoveColorTokensExt
*
* Remove color tokens from a string
* If "draw" is set, all printable ^^ and ^ will be become ^^ (e.g. ^a --> ^^a),
* so the result string may end up up to 1.5 times longer
* (only a final ^ really needs duplicating, it's just easier to do it for all)
*/
const char *COM_RemoveColorTokensExt( const char *str, bool draw ) {
	static char cleanString[MAX_STRING_CHARS];
	char *out = cleanString, *end = cleanString + sizeof( cleanString );
	const char *in = str;
	char c;
	int gc;

	while( out + 1 < end ) {
		gc = Q_GrabCharFromColorString( &in, &c, NULL );
		if( gc == GRABCHAR_CHAR ) {
			if( c == Q_COLOR_ESCAPE && draw ) {
				// write two tokens so ^^1 doesn't turn into ^1 which is a color code
				if( out + 2 == end ) {
					break;
				}
				*out++ = Q_COLOR_ESCAPE;
				*out++ = Q_COLOR_ESCAPE;
			} else {
				*out++ = c;
			}
		} else if( gc == GRABCHAR_COLOR ) {
			;
		} else if( gc == GRABCHAR_END ) {
			break;
		} else {
			assert( 0 );
		}
	}

	*out = '\0';
	return cleanString;
}

/*
* COM_SanitizeColorString
*
* Redundant color codes are removed: "^1^2text" ==> "^2text", "a^7" --> "a"
* Color codes preceding whitespace are moved to before the first non-whitespace
* char: "^1  a" ==> "  ^1a" (makes trimming spaces from the resulting string easier)
* A trailing ^ is duplicated: "a^" --> "a^^", to make sure we can easily append ^7
* (so the output may need 1 byte more than input)
* ----------------------------
* "bufsize" is size of output buffer including trailing zero, so strlen() of output
* string will be bufsize-1 at max
* "maxprintablechars" is how many printable (non-color-escape) characters
* to write at most. Use -1 for no limit.
* "startcolor" is the assumed color of the string if there are no color escapes
* E.g. if startcolor is 7, leading ^7 sequences will be dropped;
* if "startcolor" is -1, initial color is undefined, so "^7foo" will be written as is.
* ----------------------------
* Returns number of printable chars written
*/
int COM_SanitizeColorString( const char *str, char *buf, int bufsize, int maxprintablechars, int startcolor ) {
	char *out = buf, *end = buf + bufsize;
	const char *in = str;
	int oldcolor = startcolor, newcolor = startcolor;
	char c;
	int gc, colorindex;
	int c_printable = 0;

	if( maxprintablechars == -1 ) {
		maxprintablechars = INT_MAX;
	}

	while( out + 1 < end && c_printable < maxprintablechars ) {
		gc = Q_GrabCharFromColorString( &in, &c, &colorindex );

		if( gc == GRABCHAR_CHAR ) {
			bool emitcolor = ( newcolor != oldcolor && c != ' ' ) ? true : false;
			int numbytes = ( c == Q_COLOR_ESCAPE ) ? 2 : 1; // ^ will be duplicated
			if( emitcolor ) {
				numbytes += 2;
			}

			if( !( out + numbytes < end ) ) {
				break;  // no space to fit everything, so drop all

			}
			// emit the color escape if necessary
			if( emitcolor ) {
				*out++ = Q_COLOR_ESCAPE;
				*out++ = newcolor + '0';
				oldcolor = newcolor;
			}

			// emit the printable char
			*out++ = c;
			if( c == Q_COLOR_ESCAPE ) {
				*out++ = Q_COLOR_ESCAPE;
			}
			c_printable++;

		} else if( gc == GRABCHAR_COLOR ) {
			newcolor = colorindex;
		} else if( gc == GRABCHAR_END ) {
			break;
		} else {
			assert( 0 );
		}
	}
	*out = '\0';

	return c_printable;
}

/*
* Q_ColorStringTerminator
*
* Returns a color sequence to append to input string so that subsequent
* characters have desired color (we can't just append ^7 because the string
* may end in a ^, that would make the ^7 printable chars and color would stay)
* Initial color is assumed to be white
* Possible return values (assuming finalcolor is 7):
* "" if no color needs to be appended,
* "^7" or
* "^^7" if the string ends in an unterminated ^
*/
const char *Q_ColorStringTerminator( const char *str, int finalcolor ) {
	char c;
	int lastcolor = ColorIndex( COLOR_WHITE ), colorindex;
	const char *s = str;

	// see what color the string ends in
	while( 1 ) {
		int gc = Q_GrabCharFromColorString( &s, &c, &colorindex );
		if( gc == GRABCHAR_CHAR ) {
			;
		} else if( gc == GRABCHAR_COLOR ) {
			lastcolor = colorindex;
		} else if( gc == GRABCHAR_END ) {
			break;
		} else {
			assert( 0 );
		}
	}

	if( lastcolor == finalcolor ) {
		return "";
	} else {
		int escapecount = 0;
		static char buf[4];
		char *p = buf;

		// count up trailing ^'s
		while( --s >= str )
			if( *s == Q_COLOR_ESCAPE ) {
				escapecount++;
			} else {
				break;
			}

		if( escapecount & 1 ) {
			*p++ = Q_COLOR_ESCAPE;
		}
		*p++ = Q_COLOR_ESCAPE;
		*p++ = '0' + finalcolor;
		*p++ = '\0';

		return buf;
	}
}

/*
* Q_ColorStrLastColor
*
* Returns the last color in a string, or the previous color specified in the argument.
*/
int Q_ColorStrLastColor( int previous, const char *s, int maxlen ) {
	char c;
	const char *end = s;
	int lastcolor = previous, colorindex;

	if( maxlen > 0 ) {
		end += maxlen;
	}

	while( ( s < end ) || ( maxlen < 0 ) ) {
		int gc = Q_GrabCharFromColorString( &s, &c, &colorindex );
		if( gc == GRABCHAR_CHAR ) {
			;
		} else if( gc == GRABCHAR_COLOR ) {
			lastcolor = colorindex;
		} else if( gc == GRABCHAR_END ) {
			break;
		} else {
			assert( 0 );
		}
	}

	return lastcolor;
}


/*
* COM_RemoveJunkChars
*
* Remove junk chars from a string (created for autoaction filenames)
*/
const char *COM_RemoveJunkChars( const char *in ) {
	static char cleanString[MAX_STRING_CHARS];
	char *out = cleanString, *end = cleanString + sizeof( cleanString ) - 1;

	if( in ) {
		while( *in && ( out < end ) ) {
			if( isalpha( *in ) || isdigit( *in ) ) {
				// keep it
				*out = *in;
				in++;
				out++;
			} else if( *in == '<' || *in == '[' || *in == '{' ) {
				*out = '(';
				in++;
				out++;
			} else if( *in == '>' || *in == ']' || *in == '}' ) {
				*out = ')';
				in++;
				out++;
			} else if( *in == '.' || *in == '/' || *in == '_' ) {
				*out = '_';
				in++;
				out++;
			} else {
				// another char
				// skip it
				in++;
			}
		}
	}

	*out = '\0';
	return cleanString;
}

/*
* COM_ReadColorRGBString
*/
int COM_ReadColorRGBString( const char *in ) {
	int playerColor[3];
	if( in && in[0] ) {
		if( sscanf( in, "%3i %3i %3i", &playerColor[0], &playerColor[1], &playerColor[2] ) == 3 ) {
			return COLOR_RGB( playerColor[0], playerColor[1], playerColor[2] );
		}
	}
	return -1;
}

int COM_ValidatePlayerColor( int rgbcolor ) {
	int r, g, b;

	r = COLOR_R( rgbcolor );
	g = COLOR_G( rgbcolor );
	b = COLOR_B( rgbcolor );

	if( r >= 200 || g >= 200 || b >= 200 ) {
		return rgbcolor;
	}

	if( r + g >= 255 || g + b >= 255 || r + b >= 255 ) {
		return rgbcolor;
	}

	if( r + g + b >= 128 * 3 ) {
		return rgbcolor;
	}

	r = r > 127 ? 255 : 128 + r;
	g = g > 127 ? 255 : 128 + g;
	b = b > 127 ? 255 : 128 + b;
	return COLOR_RGB( r, g, b );
}

//============================================================================
//
//					LIBRARY REPLACEMENT FUNCTIONS
//
//============================================================================

/*
* Q_strncpyz
*/
void Q_strncpyz( char *dest, const char *src, size_t size ) {
#ifdef HAVE_STRLCPY
	strlcpy( dest, src, size );
#else
	if( size ) {
		while( --size && ( *dest++ = *src++ ) ) ;
		*dest = '\0';
	}
#endif
}

/*
* Q_strncatz
*/
void Q_strncatz( char *dest, const char *src, size_t size ) {
#ifdef HAVE_STRLCAT
	strlcat( dest, src, size );
#else
	if( size ) {
		while( --size && *dest++ ) ;
		if( size ) {
			dest--; size++;
			while( --size && ( *dest++ = *src++ ) ) ;
		}
		*dest = '\0';
	}
#endif
}

/*
* Q_vsnprintfz
*/
int Q_vsnprintfz( char *dest, size_t size, const char *format, va_list argptr ) {
	int len;

	assert( dest );
	assert( size );

	len = vsnprintf( dest, size, format, argptr );
	dest[size - 1] = 0;

	return len;
}

/*
* Q_snprintfz
*/
int Q_snprintfz( char *dest, size_t size, const char *format, ... ) {
	int len;
	va_list argptr;

	va_start( argptr, format );
	len = Q_vsnprintfz( dest, size, format, argptr );
	va_end( argptr );

	return len;
}

/*
* Q_strupr
*/
char *Q_strupr( char *s ) {
	char *p;

	if( s ) {
		for( p = s; *s; s++ )
			*s = toupper( *s );
		return p;
	}

	return NULL;
}

/*
* Q_strlwr
*/
char *Q_strlwr( char *s ) {
	char *p;

	if( s ) {
		for( p = s; *s; s++ )
			*s = tolower( *s );
		return p;
	}

	return NULL;
}

/*
* Q_strrstr
*/
const char *Q_strrstr( const char *s, const char *substr ) {
	const char *p;

	s = p = strstr( s, substr );
	while( s != NULL ) {
		p = s;
		s = strstr( s + 1, substr );
	}

	return p;
}

/*
* Q_trim
*/
#define IS_TRIMMED_CHAR( s ) ( ( s ) == ' ' || ( s ) == '\t' || ( s ) == '\r' || ( s ) == '\n' )
char *Q_trim( char *s ) {
	char *t = s;
	size_t len;

	// remove leading whitespace
	while( IS_TRIMMED_CHAR( *t ) ) t++;
	len = strlen( s ) - ( t - s );
	if( s != t ) {
		memmove( s, t, len + 1 );
	}

	// remove trailing whitespace
	while( len && IS_TRIMMED_CHAR( s[len - 1] ) )
		s[--len] = '\0';

	return s;
}

/*
* Q_WCharUtf8Length
*
* Returns the length of wchar_t encoded as UTF-8.
*/
size_t Q_WCharUtf8Length( wchar_t wc ) {
	unsigned int num = wc;

	if( !num ) {
		return 0;
	}
	if( num <= 0x7f ) {
		return 1;
	}
	if( num <= 0x7ff ) {
		return 2;
	}
	if( num <= 0xffff ) {
		return 3;
	}
	return 1; // 4-octet sequences are replaced with '?'
}

/*
* Q_WCharToUtf8
*
* Converts wchar_t to UTF-8 and returns the length of the written sequence.
*/
size_t Q_WCharToUtf8( wchar_t wc, char *dest, size_t bufsize ) {
	unsigned int num = wc;
	size_t ret = 0;

	if( num ) {
		if( num <= 0x7f ) {
			if( bufsize > 1 ) {
				*dest++ = num;
				ret = 1;
			}
		} else if( num <= 0x7ff ) {
			if( bufsize > 2 ) {
				*dest++ = 0xC0 | ( num >> 6 );
				*dest++ = 0x80 | ( num & 0x3f );
				ret = 2;
			}
		} else if( num <= 0xffff ) {
			if( bufsize > 3 ) {
				*dest++ = 0xE0 | ( num >> 12 );
				*dest++ = 0x80 | ( ( num & 0xfc0 ) >> 6 );
				*dest++ = 0x80 | ( num & 0x003f );
				ret = 3;
			}
		} else {
			// sorry, we don't support 4-octet sequences
			if( bufsize > 1 ) {
				*dest++ = '?';
				ret = 1;
			}
		}
	}

	if( bufsize > ret ) {
		*dest++ = '\0';
	}

	return ret;
}

/*
* Q_WCharToUtf8Char
*
* Converts wchar_t to UTF-8 using a temporary buffer.
*/
char *Q_WCharToUtf8Char( wchar_t wc ) {
	static char buf[5]; // longest valid utf-8 sequence is 4 bytes
	Q_WCharToUtf8( wc, buf, sizeof( buf ) );
	return buf;
}

/*
* Q_WCharToUtf8String
*
* Converts a wchar_t string (of the system native wchar size) to UTF-8.
* Returns the length of the written string.
*/
size_t Q_WCharToUtf8String( const wchar_t *ws, char *dest, size_t bufsize ) {
	size_t len = 0, utflen;

	if( !bufsize ) {
		return 0;
	}

	dest[0] = '\0';

	while( ( bufsize > 1 ) && *ws ) {
		utflen = Q_WCharToUtf8( *ws, dest, bufsize );
		if( !utflen ) {
			break;
		}

		ws++;
		dest += utflen;
		bufsize -= utflen;
		len += utflen;
	}

	return len;
}

/*
* Q_Utf8SyncPos
*
* For line editing: if we're in the middle of a UTF-8 sequence,
* skip left or right to the start of a UTF-8 sequence (or end of string)
* 'dir' should be UTF8SYNC_LEFT or UTF8SYNC_RIGHT
* Returns new position
* ------------------------------
* (To be pedantic, I must note that we may skip too many continuation chars
* in a malformed UTF-8 string.  But malformed UTF-8 isn't supposed to get
* into the console input line, and even if it does, we'll only be happy
* to delete it all with one BACKSPACE stroke)
*/
int Q_Utf8SyncPos( const char *str, int pos, int dir ) {
	// Skip until we hit an ASCII char (char & 0x80 == 0)
	// or the start of a utf-8 sequence (char & 0xC0 == 0xC0).
	if( dir == UTF8SYNC_LEFT ) {
		while( pos > 0 && !( ( str[pos] & 0x80 ) == 0 || ( str[pos] & 0x40 ) ) )
			pos--;
	} else {
		while( !( ( str[pos] & 0x80 ) == 0 || ( str[pos] & 0x40 ) ) )
			pos++;
	}

	return pos;
}

// returns a wide char, advances (*pstr) to next char (unless at end of string)
wchar_t Q_GrabWCharFromUtf8String( const char **pstr ) {
	int part, i;
	wchar_t val;
	const char *src = *pstr;

	if( !*src ) {
		return 0;
	}

	part = ( unsigned char )*src;
	src++;

	if( !( part & 0x80 ) ) { // 1 octet
		val = part;
	} else if( ( part & 0xE0 ) == 0xC0 ) {   // 2 octets
		val = ( part & 0x1F ) << 6;
		if( ( *src & 0xC0 ) != 0x80 ) {
			val = '?'; // incomplete 2-octet sequence (including unexpected '\0')
		} else {
			val |= *src & 0x3f;
			src++;
			if( val < 0x80 ) {
				val = '?';  // overlong utf-8 sequence
			}
		}
	} else if( ( part & 0xF0 ) == 0xE0 ) {   // 3 octets
		val = ( part & 0x0F ) << 12;
		if( ( *src & 0xC0 ) != 0x80 ) { // check 2nd octet
			val = '?';
		} else {
			val |= ( *src & 0x3f ) << 6;
			src++;
			if( ( *src & 0xC0 ) != 0x80 ) { // check 3rd octet
				val = '?';
			} else {
				val |= *src & 0x3f;
				src++;
				if( val < 0x800 ) {
					val = '?';  // overlong sequence
				}
			}
		}
	} else if( ( part & 0xF8 ) == 0xF0 ) {   // 4 octets
		// throw it away (it may be a valid sequence, we just don't support it)
		val = '?';
		for( i = 0; i < 4 && ( *src & 0xC0 ) == 0x80; i++ )
			src++;
	} else {
		val = '?';  // invalid utf-8 octet

	}
	*pstr = src;
	return val;
}

/*
* Q_FixTruncatedUtf8
*
* Terminates a UTF-8 string correctly if it's cut to a specific buffer length (for instance, when using strncpyz).
*/
void Q_FixTruncatedUtf8( char *str ) {
	size_t len = strlen( str );
	const char *temp;
	if( !len ) {
		return;
	}

	len = Q_Utf8SyncPos( str, len - 1, UTF8SYNC_LEFT );
	temp = str + len;
	if( ( *temp != '?' ) && ( Q_GrabWCharFromUtf8String( &temp ) == '?' ) ) {
		str[len] = '\0';
	}
}

/*
* Q_IsBreakingSpace
*/
bool Q_IsBreakingSpace( const char *str ) {
	const unsigned char *s = ( const unsigned char * )str;

	switch( s[0] ) {
		case ' ':
		case '\t':
			return true;
		case 0xe3:
			return ( s[1] == 0x80 ) && ( s[2] == 0x80 );
		case 0xe2:
			return ( s[1] == 0x80 ) && ( s[2] >= 0x80 ) && ( s[2] <= 0x8b );
	}

	return false;
}

/*
* Q_IsBreakingSpaceChar
*
* Same as IsBreakingSpace, but for a single character.
*/
bool Q_IsBreakingSpaceChar( wchar_t c ) {
	return ( c == ' ' ) || ( c == '\t' ) || ( c == 0x3000 ) || ( ( c >= 0x2000 ) && ( c <= 0x200b ) );
}

/*
* Q_isdigit
*/
bool Q_isdigit( const char *str ) {
	if( str && *str ) {
		while( isdigit( *str ) ) str++;
		if( !*str ) {
			return true;
		}
	}
	return false;
}

/*
* Q_urlencode_unsafechars
*/
void Q_urlencode_unsafechars( const char *src, char *dst, size_t dst_size ) {
	size_t i, n, len;

	assert( src );
	assert( dst );

	if( !src || !dst || !dst_size ) {
		return;
	}

	len = strlen( src );
	if( len >= dst_size ) {
		len = dst_size - 1;
	}

	// urlencode
	n = 0;
	for( i = 0; i < len && n < dst_size - 1; i++ ) {
		char c = src[i];

		if( c == ' ' || c == '#' || c == '%' ||
			c == '<' || c == '>' || c == '{' || c == '}' ||
			c == '|' || c == '\\' || c == '^' || c == '~' ||
			c == '[' || c == ']' ) {
			// urlencode
			if( n + 3 >= dst_size ) {
				// not enough space
				break;
			}

			dst[n  ] = '%';
			sprintf( &dst[n + 1], "%02x", (int)c );
			n += 3;
		} else {
			dst[n] = src[i];
			n++;
		}
	}
	dst[n] = '\0';
}

/*
* Q_urldecode
*/
#define hex2dec( x ) ( ( ( x ) <= '9' ? ( x ) - '0' : ( ( x ) <= 'F' ) ? ( x ) - 'A' + 10 : ( x ) - 'a' + 10 ) )
size_t Q_urldecode( const char *src, char *dst, size_t dst_size ) {
	char *dst_start = dst, *dst_end = dst + dst_size - 1;
	const char *src_end;

	if( !src || !dst || !dst_size ) {
		return 0;
	}

	src_end = src + strlen( src );
	while( src < src_end ) {
		if( dst == dst_end ) {
			break;
		}
		if( ( *src == '%' ) && ( src + 2 < src_end ) &&
			( isxdigit( src[1] ) && isxdigit( src[2] ) ) ) {
			*dst++ = ( hex2dec( src[1] ) << 4 ) + hex2dec( src[2] );
			src += 3;
		} else {
			*dst++ = *src++;
		}
	}

	*dst = '\0';
	return dst - dst_start;
}

//=====================================================================
//
//  INFO STRINGS
//
//=====================================================================


/*
* COM_ValidateConfigstring
*/
bool COM_ValidateConfigstring( const char *string ) {
	const char *p;
	bool opened = false;
	int parity = 0;

	if( !string ) {
		return false;
	}

	p = string;
	while( *p ) {
		if( *p == '\"' ) {
			if( opened ) {
				parity--;
				opened = false;
			} else {
				parity++;
				opened = true;
			}
		}
		p++;
	}

	if( parity != 0 ) {
		return false;
	}

	return true;
}



int Com_GlobMatch( const char *pattern, const char *text, const bool casecmp ) {
	return glob_match( pattern, text, casecmp );
}

void *Q_malloc( size_t size ) {
	// TODO: Ensure 16-byte alignment
	// Zero memory as lots of old stuff rely on the old mempool behaviour
	void *buf = std::calloc( size, 1 );

	if( !buf ) {
		wsw::failWithBadAlloc();
	}

	return buf;
}

void *Q_realloc( void *buf, size_t newsize ) {
	void *newbuf = realloc( buf, newsize );

	if( !newbuf && newsize ) {
		wsw::failWithBadAlloc();
	}

	// TODO: Zero memory too? There's no portable way of doing that

	return newbuf;
}

void Q_free( void *buf ) {
	std::free( buf );
}

char *Q_strdup( const char *str ) {
	auto len = strlen( str );
	auto *result = (char *)Q_malloc( len + 1 );
	memcpy( result, str, len + 1 );
	return result;
}