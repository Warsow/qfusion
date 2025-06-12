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

#ifndef WSW_7fa58676_4562_402a_ae41_7df2ef78e9b3_H
#define WSW_7fa58676_4562_402a_ae41_7df2ef78e9b3_H

#include <optional>

#define Q_CPU_FEATURE_SSE2    ( 0x1u )
#define Q_CPU_FEATURE_SSE41   ( 0x2u )
#define Q_CPU_FEATURE_SSE42   ( 0x4u )
#define Q_CPU_FEATURE_AVX     ( 0x8u )


#ifndef _MSC_VER
void Sys_Error( const char *error, ... ) __attribute__( ( format( printf, 1, 2 ) ) ) __attribute__( ( noreturn ) );
void Sys_Quit( void ) __attribute__( ( noreturn ) );
#else
__declspec( noreturn ) void Sys_Error( _Printf_format_string_ const char *error, ... );
__declspec( noreturn ) void Sys_Quit( void );
#endif

bool    Sys_IsBrowserAvailable( void );
void    Sys_OpenURLInBrowser( const char *url );

int Sys_GetPixelRatio();

char *Sys_GetClipboardData();
bool Sys_SetClipboardData( const char *data );
void Sys_FreeClipboardData( char *data );

void Sys_SendKeyEvents( void );
void Sys_AppActivate( void );

// TODO: Use a Java-like iterator
char **Sys_GetEnvironmentVariables();
void Sys_DeleteEnvironmentVariable( const char *name );

[[nodiscard]]
unsigned Sys_GetProcessorFeatures();
[[nodiscard]]
auto Sys_GetNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>>;


#endif