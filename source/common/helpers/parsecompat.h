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

#ifndef WSW_97ef7d8e_6cde_474c_83c1_90969c0377c7_H
#define WSW_97ef7d8e_6cde_474c_83c1_90969c0377c7_H

#include <cstdlib>

int COM_Compress( char *data_p );

// data is an in/out parm, returns a parsed out token
char *COM_ParseExt2_r( char *token, size_t token_size, const char **data_p, bool nl, bool sq );
#define COM_ParseExt_r( token, token_size, data_p, nl ) COM_ParseExt2_r( token, token_size, (const char **)data_p, nl, true )
#define COM_Parse_r( token, token_size, data_p )   COM_ParseExt_r( token, token_size, data_p, true )

char *COM_ParseExt2( const char **data_p, bool nl, bool sq );
#define COM_ParseExt( data_p, nl ) COM_ParseExt2( (const char **)data_p, nl, true )
#define COM_Parse( data_p )   COM_ParseExt( data_p, true )

#endif