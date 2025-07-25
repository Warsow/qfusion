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
#ifndef _WIN32
#error You should not be including this file on this platform
#endif

#ifndef __GLW_WIN_H__
#define __GLW_WIN_H__

#define WINDOW_CLASSNAME_SIZE   120

typedef struct {
	HINSTANCE hInstance;
	void *wndproc;

	char *applicationName;
	WCHAR *applicationNameW;
	HDC hDC;                    // handle to device context
	HWND hWnd;                  // handle to window
	HWND parenthWnd;            // handle to parent window
	HGLRC hGLRC;                // handle to GL rendering context
	int applicationIconResourceID;

	char windowClassName[WINDOW_CLASSNAME_SIZE];
	WCHAR windowClassNameW[WINDOW_CLASSNAME_SIZE];

	HINSTANCE hinstOpenGL;      // HINSTANCE for the OpenGL library

	int win_x, win_y;
	bool allowdisplaydepthchange;
} glwstate_t;

extern glwstate_t glw_state;

struct cvar_s;
extern cvar_s *vid_fullscreen;
extern cvar_s *vid_displayfrequency;

#endif
