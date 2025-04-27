/*
Copyright (C) 2015 SiPlus, Chasseur de bots

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

#include "../client/client.h"
#include "winquake.h"

void VID_SetProcessDPIAware( void );

void CL_Sys_Init( void ) {
	VID_SetProcessDPIAware();
}

void CL_Sys_Shutdown( void ) {
}

char **Sys_GetEnvironmentVariables() {
	return environ;
}

void Sys_DeleteEnvironmentVariable( const char *name ) {
	(void)_putenv_s( name, "" );
	assert( !getenv( name ) );
}
