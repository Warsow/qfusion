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

#ifndef WSW_bac442f1_f64d_4f40_a306_2fe44f42499c_H
#define WSW_bac442f1_f64d_4f40_a306_2fe44f42499c_H

#include <common/facilities/messagestreams.h>

#include <optional>
#include <utility>

#define comDebug()   wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Debug ) ).getWriter()
#define comNotice()  wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Notice ) ).getWriter()
#define comWarning() wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Warning ) ).getWriter()
#define comError()   wsw::PendingOutputMessage( wsw::createMessageStream( wsw::MessageDomain::Common, wsw::MessageCategory::Error ) ).getWriter()

void Qcommon_Init( int argc, char **argv );
void Qcommon_Frame( unsigned realMsec, unsigned *gameMsec, float *extraTime );
void Qcommon_Shutdown( void );

void        Com_DeferQuit( void );

void        Com_SetClientState( int state );
void        Com_SetDemoPlaying( bool state );
void        Com_SetServerState( int state );

void        Com_BeginRedirect( int target, char *buffer, int buffersize,
							   void ( *flush )( int, const char*, const void* ), const void *extra );
void        Com_EndRedirect( void );
void        Com_DeferConsoleLogReopen( void );
void        Com_ReopenConsoleLog();
void        Com_CloseConsoleLog( bool lock, bool shutdown );

#endif