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

#ifndef WSW_8003eb82_1a41_463b_805d_954a443e1819_H
#define WSW_8003eb82_1a41_463b_805d_954a443e1819_H

// common/library.c
typedef struct dllfunc_s { const char *name; void **funcPointer; } dllfunc_t;

void Com_UnloadLibrary( void **lib );
void *Com_LoadLibrary( const char *name, dllfunc_t *funcs ); // NULL-terminated array of functions
void *Com_LoadSysLibrary( const char *name, dllfunc_t *funcs ); // NULL-terminated array of functions
void *Com_LibraryProcAddress( void *lib, const char *name );

#endif