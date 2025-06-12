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

//
// console
//

#include <common/facilities/cmdargs.h>

#define NUM_CON_TIMES 4

// wsw : aiwa : global definition to activate case-sensitivity of console (1 == activated)
#define     CON_CASE_SENSITIVE 0

struct qfontface_s;

extern volatile bool con_initialized;

void Con_CheckResize( void );
void Con_Init( void );
void Con_Shutdown( void );
void Con_DrawConsole( unsigned width, unsigned height );
void Con_Print( const char *txt );
void Con_PrintSilent( const char *txt );
void Con_DrawNotify( unsigned width, unsigned height );
void Con_ClearNotify( void );
void Con_ToggleConsole_f( const CmdArgs & );
void Con_Close( void );

[[nodiscard]]
bool Con_HasKeyboardFocus();

[[nodiscard]]
bool Con_HandleKeyEvent( int key, bool down );
[[nodiscard]]
bool Con_HandleCharEvent( wchar_t key );

[[maybe_unused]]
uint64_t Con_SendCommonChatMessage( const wsw::StringView &text );
[[maybe_unused]]
uint64_t Con_SendTeamChatMessage( const wsw::StringView &text );
