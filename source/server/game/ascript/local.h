/*
Copyright (C) 2008 German Garcia

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

#ifndef __QAS_LOCAL_H__
#define __QAS_LOCAL_H__

#define AS_USE_STLNAMES 1

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/q_shared.h>
#include <common/facilities/q_cvar.h>
#include "ascript.h"

#include <new>

#if defined ( _WIN32 ) || ( _WIN64 )
#include <string.h>
#endif

#define QAS_SECTIONS_SEPARATOR ';'
#define QAS_FILE_EXTENSION     ".as"

#define QAS_MemAlloc( pool, size ) ::calloc( size, 1 )
#define QAS_MemFree( mem ) ::free( mem )

#define QAS_Malloc( size ) QAS_MemAlloc( angelwrappool, size )
#define QAS_Free( data ) QAS_MemFree( data )

#define QAS_NEW( x )        new( QAS_Malloc( sizeof( x ) ) )( x )
#define QAS_DELETE( ptr,x ) {void *tmp = ptr; ( ptr )->~x(); QAS_Free( tmp );}

#define QAS_NEWARRAY( x,cnt )  (x*)QAS_Malloc( sizeof( x ) * cnt )
#define QAS_DELETEARRAY( ptr ) QAS_Free( ptr )

#endif // __QAS_LOCAL_H__
