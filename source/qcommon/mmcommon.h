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

// mm_common.h -- matchmaker definitions for client and server exe's (not modules)

#ifndef __MM_COMMON_H
#define __MM_COMMON_H

#include "wswcurl.h"
#include "mmquery.h"

// these are in milliseconds
#define MM_HEARTBEAT_INTERVAL   15 * 1000
#define MM_LOGOUT_TIMEOUT       3 * 1000

extern cvar_t *mm_url;

void MM_PasswordWrite( const char *user, const char *password );
// returns password as static string
const char *MM_PasswordRead( const char *user );

void MM_Init();
void MM_Shutdown();
void MM_Frame( int realmsec );

#endif
