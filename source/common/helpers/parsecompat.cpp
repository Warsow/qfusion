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

#include "parsecompat.h"

#include <common/facilities/q_comref.h>

/*
* COM_Compress
*
* Parse a token out of a string
*/
int COM_Compress( char *data_p ) {
	char *in, *out;
	int c;
	bool newline = false, whitespace = false;

	in = out = data_p;
	if( in ) {
		while( ( c = *in ) != 0 ) {
			// skip double slash comments
			if( c == '/' && in[1] == '/' ) {
				while( *in && *in != '\n' ) {
					in++;
				}
				// skip /* */ comments
			} else if( c == '/' && in[1] == '*' ) {
				while( *in && ( *in != '*' || in[1] != '/' ) )
					in++;
				if( *in ) {
					in += 2;
				}
				// record when we hit a newline
			} else if( c == '\n' || c == '\r' ) {
				newline = true;
				in++;
				// record when we hit whitespace
			} else if( c == ' ' || c == '\t' ) {
				whitespace = true;
				in++;
				// an actual token
			} else {
				// if we have a pending newline, emit it (and it counts as whitespace)
				if( newline ) {
					*out++ = '\n';
					newline = false;
					whitespace = false;
				}
				if( whitespace ) {
					*out++ = ' ';
					whitespace = false;
				}

				// copy quoted strings unmolested
				if( c == '"' ) {
					*out++ = c;
					in++;
					while( 1 ) {
						c = *in;
						if( c && c != '"' ) {
							*out++ = c;
							in++;
						} else {
							break;
						}
					}
					if( c == '"' ) {
						*out++ = c;
						in++;
					}
				} else {
					*out = c;
					out++;
					in++;
				}
			}
		}
	}
	*out = 0;
	return out - data_p;
}

/*
* COM_ParseExt2_r
*
* Parse a token out of a string
*/
char *COM_ParseExt2_r( char *token, size_t token_size, const char **data_p, bool nl, bool sq ) {
	int c;
	unsigned len;
	const char *data;
	bool newlines = false;

	data = *data_p;
	len = 0;
	token[0] = 0;

	if( !data ) {
		*data_p = NULL;
		return token;
	}

	// skip whitespace
	skipwhite:
	while( (unsigned char)( c = *data ) <= ' ' ) {
		if( c == 0 ) {
			*data_p = NULL;
			return token;
		}
		if( c == '\n' ) {
			newlines = true;
		}
		data++;
	}

	if( newlines && !nl ) {
		*data_p = data;
		return token;
	}

	// skip // comments
	if( c == '/' && data[1] == '/' ) {
		data += 2;

		while( *data && *data != '\n' )
			data++;
		goto skipwhite;
	}

	// skip /* */ comments
	if( c == '/' && data[1] == '*' ) {
		data += 2;

		while( 1 ) {
			if( !*data ) {
				break;
			}
			if( *data != '*' || *( data + 1 ) != '/' ) {
				data++;
			} else {
				data += 2;
				break;
			}
		}
		goto skipwhite;
	}

	// handle quoted strings specially
	if( c == '\"' ) {
		if( sq ) {
			data++;
		}
		while( 1 ) {
			c = *data++;
			if( c == '\"' || !c ) {
				if( !c ) {
					data--;
				}

				if( ( len < token_size ) && ( !sq ) ) {
					token[len] = '\"';
					len++;
					//data++;
				}

				if( len == token_size ) {
					//Com_Printf ("Token exceeded %i chars, discarded.\n", (int)token_size);
					len = 0;
				}
				token[len] = 0;
				*data_p = data;
				return token;
			}
			if( len < token_size ) {
				token[len] = c;
				len++;
			}
		}
	}

	// parse a regular word
	do {
		if( len < token_size ) {
			token[len] = c;
			len++;
		}
		data++;
		c = *data;
	} while( (unsigned char)c > 32 );

	if( len == token_size ) {
		//Com_Printf ("Token exceeded %i chars, discarded.\n", (int)token_size);
		len = 0;
	}
	token[len] = 0;

	*data_p = data;
	return token;
}

static char com_token[MAX_TOKEN_CHARS];

/*
 * COM_ParseExt
 *
 * Parse a token out of a string
 */
char *COM_ParseExt2( const char **data_p, bool nl, bool sq ) {
	return COM_ParseExt2_r( com_token, MAX_TOKEN_CHARS, data_p, nl, sq );
}