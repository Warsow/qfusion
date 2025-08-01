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
// in_win.c -- windows mouse and joystick code
// 02/21/97 JCB Added extended DirectInput code to support external controllers.

#include <client/client.h>
#include <common/facilities/cvar.h>
#include "winquake.h"

//#ifdef __GNUC__
#define DIRECTINPUT_VERSION 0x0700 // Could use dx9, but older is more frequently used
//#else
//#define	DIRECTINPUT_VERSION 0x0800
//#endif

#include <dinput.h>
#include <xinput.h>

#define DINPUT_BUFFERSIZE           64 // http://www.esreality.com/?a=post&id=905276#pid905330
#define iDirectInputCreate( a, b, c, d ) pDirectInputCreate( a, b, c, d )

static HRESULT( WINAPI * pDirectInputCreate )( HINSTANCE hinst, DWORD dwVersion,
											   LPDIRECTINPUT * lplpDirectInput, LPUNKNOWN punkOuter );

// raw input specific defines
#define MAX_RI_DEVICE_SIZE 128
#define INIT_RIBUFFER_SIZE ( sizeof( RAWINPUTHEADER ) + sizeof( RAWMOUSE ) )

#define RI_RAWBUTTON_MASK 0x000003E0
#define RI_INVALID_POS    0x80000000

// raw input dynamic functions
typedef int ( WINAPI * pGetRawInputDeviceList )( OUT PRAWINPUTDEVICELIST pRawInputDeviceList, IN OUT PINT puiNumDevices, IN UINT cbSize );
typedef int ( WINAPI * pGetRawInputData )( IN HRAWINPUT hRawInput, IN UINT uiCommand, OUT LPVOID pData, IN OUT PINT pcbSize, IN UINT cbSizeHeader );
typedef int ( WINAPI * pGetRawInputDeviceInfoA )( IN HANDLE hDevice, IN UINT uiCommand, OUT LPVOID pData, IN OUT PINT pcbSize );
typedef BOOL ( WINAPI * pRegisterRawInputDevices )( IN PCRAWINPUTDEVICE pRawInputDevices, IN UINT uiNumDevices, IN UINT cbSize );

pGetRawInputDeviceList _GRIDL;
pGetRawInputData _GRID;
pGetRawInputDeviceInfoA _GRIDIA;
pRegisterRawInputDevices _RRID;

typedef struct {
	HANDLE rawinputhandle;          // raw input, identify particular mice

	int numbuttons;
	volatile int buttons;

	volatile int delta[2];
	int pos[2];
} rawmouse_t;

static rawmouse_t   *rawmice = NULL;
static int rawmicecount = 0;
static RAWINPUT     *raw = NULL;
static int ribuffersize = 0;
static bool rawinput_initialized = false;

static bool IN_RawInput_Init( void );
static void     IN_RawInput_Shutdown( void );
static int      IN_RawInput_Register( void );
static void     IN_RawInput_DeRegister( void );

extern int64_t sys_msg_time;

cvar_t *in_mouse;
cvar_t *in_grabinconsole;

bool in_appactive;

void Key_Event( int key, bool down, int64_t time ) {
	wsw::cl::KeyHandlingSystem::instance()->handleKeyEvent( key, down, time );
}

void Key_CharEvent( int key, wchar_t ch ) {
	wsw::cl::KeyHandlingSystem::instance()->handleCharEvent( key, ch );
}

void Key_MouseEvent( int key, bool down, int64_t time ) {
	wsw::cl::KeyHandlingSystem::instance()->handleMouseEvent( key, down, time );
}

/*
============================================================

MOUSE CONTROL

============================================================
*/

// used by win_vid.c
int mouse_buttons;
int mouse_wheel_type;

static int mouse_oldbuttonstate;
static POINT current_pos;
static int mx, my;
static bool mouseactive;    // false when not focus app
static bool restore_spi;
static bool mouseinitialized;
static int originalmouseparms[3], newmouseparms[3] = { 0, 0, 0 };
static bool mouseparmsvalid;
static unsigned int mstate_di;

static int window_center_x, window_center_y;
static RECT window_rect;

static LPDIRECTINPUT g_pdi;
static LPDIRECTINPUTDEVICE g_pMouse;

static HINSTANCE hInstDI;

static bool dinput_initialized;
static bool dinput_acquired;

// replacement for dxguid.lib which is not available in latest Windows SDKs for XP
static const GUID qGUID_XAxis =     { 0xa36d02e0, 0xc9f3, 0x11cf, { 0xbf, 0xc7, 0x44, 0x45, 0x53, 0x54, 0x00, 0x00 } };
static const GUID qGUID_YAxis =     { 0xa36d02e1, 0xc9f3, 0x11cf, { 0xbf, 0xc7, 0x44, 0x45, 0x53, 0x54, 0x00, 0x00 } };
static const GUID qGUID_ZAxis =     { 0xa36d02e2, 0xc9f3, 0x11cf, { 0xbf, 0xc7, 0x44, 0x45, 0x53, 0x54, 0x00, 0x00 } };
static const GUID qGUID_SysMouse =  { 0x6f1d2b60, 0xd5a0, 0x11cf, { 0xbf, 0xc7, 0x44, 0x45, 0x53, 0x54, 0x00, 0x00 } };

typedef struct MYDATA {
	LONG lX;                // X axis goes here
	LONG lY;                // Y axis goes here
	LONG lZ;                // Z axis goes here
	BYTE bButtonA;          // One button goes here
	BYTE bButtonB;          // Another button goes here
	BYTE bButtonC;          // Another button goes here
	BYTE bButtonD;          // Another button goes here
	BYTE bButtonE;          // Another button goes here
	BYTE bButtonF;          // Another button goes here
	BYTE bButtonG;          // Another button goes here
	BYTE bButtonH;          // Another button goes here
} MYDATA;

// This structure corresponds to c_dfDIMouse2 in dinput8.lib
// 0x80000000 is something undocumented but must be there, otherwise
// IDirectInputDevice_SetDataFormat may fail.
static DIOBJECTDATAFORMAT rgodf[] = {
	{ &qGUID_XAxis, FIELD_OFFSET( MYDATA, lX ), DIDFT_AXIS | DIDFT_ANYINSTANCE, 0, },
	{ &qGUID_YAxis, FIELD_OFFSET( MYDATA, lY ), DIDFT_AXIS | DIDFT_ANYINSTANCE, 0, },
	{ &qGUID_ZAxis, FIELD_OFFSET( MYDATA, lZ ), 0x80000000 | DIDFT_AXIS | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonA ), DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonB ), DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonC ), 0x80000000 | DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonD ), 0x80000000 | DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonE ), 0x80000000 | DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonF ), 0x80000000 | DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonG ), 0x80000000 | DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
	{ 0, FIELD_OFFSET( MYDATA, bButtonH ), 0x80000000 | DIDFT_BUTTON | DIDFT_ANYINSTANCE, 0, },
};

#define NUM_OBJECTS ( sizeof( rgodf ) / sizeof( rgodf[0] ) )

static DIDATAFORMAT df = {
	sizeof( DIDATAFORMAT ), // this structure
	sizeof( DIOBJECTDATAFORMAT ), // size of object data format
	DIDF_RELAXIS,           // absolute axis coordinates
	sizeof( MYDATA ),       // device data size
	NUM_OBJECTS,            // number of objects
	rgodf,                  // and here they are
};

/*
* IN_ActivateMouse
*
* Called when the window gains focus or changes in some way
*/
static void IN_ActivateMouse( void ) {
	int width, height;

	if( !mouseinitialized ) {
		return;
	}
	if( !in_mouse->integer ) {
		mouseactive = false;
		return;
	}
	if( mouseactive ) {
		return;
	}

	mouseactive = true;

	if( dinput_initialized ) {
		mstate_di = 0;
		if( g_pMouse ) {
			if( cl_hwnd ) {
				if( FAILED( IDirectInputDevice_SetCooperativeLevel( g_pMouse, cl_hwnd, DISCL_EXCLUSIVE | DISCL_FOREGROUND ) ) ) {
					Com_DPrintf( "Couldn't set DI coop level\n" );
					return;
				}
			}
			if( !dinput_acquired ) {
				IDirectInputDevice_Acquire( g_pMouse );
				dinput_acquired = true;
			}
		}
		return;
	}

	if( rawinput_initialized ) {
		if( IN_RawInput_Register() ) {
			Com_Printf( "Raw input: unable to register raw input, deinitializing\n" );
			IN_RawInput_Shutdown();
		}
	}

	mouse_oldbuttonstate = 0;

	if( mouseparmsvalid ) {
		restore_spi = SystemParametersInfo( SPI_SETMOUSE, 0, newmouseparms, 0 );
	}

	width = GetSystemMetrics( SM_CXSCREEN );
	height = GetSystemMetrics( SM_CYSCREEN );

	GetWindowRect( cl_hwnd, &window_rect );
	if( window_rect.left < 0 ) {
		window_rect.left = 0;
	}
	if( window_rect.top < 0 ) {
		window_rect.top = 0;
	}
	if( window_rect.right >= width ) {
		window_rect.right = width - 1;
	}
	if( window_rect.bottom >= height - 1 ) {
		window_rect.bottom = height - 1;
	}

	window_center_x = ( window_rect.right + window_rect.left ) / 2;
	window_center_y = ( window_rect.top + window_rect.bottom ) / 2;

	SetCursorPos( window_center_x, window_center_y );

	SetCapture( cl_hwnd );
	ClipCursor( &window_rect );
	while( ShowCursor( FALSE ) >= 0 ) ;
}


/*
* IN_DeactivateMouse
*
* Called when the window loses focus
*/
static void IN_DeactivateMouse( void ) {
	if( !mouseinitialized ) {
		return;
	}
	if( !mouseactive ) {
		return;
	}

	mouseactive = false;

	if( dinput_initialized ) {
		if( g_pMouse ) {
			if( dinput_acquired ) {
				IDirectInputDevice_Unacquire( g_pMouse );
				dinput_acquired = false;
			}
			if( cl_hwnd ) {
				if( FAILED( IDirectInputDevice_SetCooperativeLevel( g_pMouse, cl_hwnd, DISCL_EXCLUSIVE | DISCL_BACKGROUND ) ) ) {
					Com_DPrintf( "Couldn't set DI coop level\n" );
					return;
				}
			}
		}
		return;
	}

	if( rawinput_initialized > 0 ) {
		IN_RawInput_DeRegister();
	}

	if( restore_spi ) {
		SystemParametersInfo( SPI_SETMOUSE, 0, originalmouseparms, 0 );
	}

	ClipCursor( NULL );
	ReleaseCapture();
	while( ShowCursor( TRUE ) < 0 ) ;
}


/*
* IN_InitDInput
*/
static bool IN_InitDInput( void ) {
	HRESULT hr;
	DIPROPDWORD dipdw = {
		{
			sizeof( DIPROPDWORD ), // diph.dwSize
			sizeof( DIPROPHEADER ),     // diph.dwHeaderSize
			0,                  // diph.dwObj
			DIPH_DEVICE,        // diph.dwHow
		},
		DINPUT_BUFFERSIZE,      // dwData
	};

	if( !hInstDI ) {
		hInstDI = LoadLibrary( "dinput.dll" );

		if( hInstDI == NULL ) {
			Com_Printf( "Couldn't load dinput.dll\n" );
			return false;
		}
	}

	if( !pDirectInputCreate ) {
		pDirectInputCreate = (decltype( pDirectInputCreate ))(void *)GetProcAddress( hInstDI, "DirectInputCreateA" );

		if( !pDirectInputCreate ) {
			Com_Printf( "Couldn't get DI proc addr\n" );
			return false;
		}
	}

	// register with DirectInput and get an IDirectInput to play with
	hr = iDirectInputCreate( global_hInstance, DIRECTINPUT_VERSION, &g_pdi, NULL );

	if( FAILED( hr ) ) {
		Com_Printf( "DirectInputCreate failed\n" );
		return false;
	}

	// obtain an interface to the system mouse device
	hr = IDirectInput_CreateDevice( g_pdi, qGUID_SysMouse, &g_pMouse, NULL );

	if( FAILED( hr ) ) {
		Com_Printf( "Couldn't open DI mouse device\n" );
		return false;
	}

	// set the data format to "mouse format"
	hr = IDirectInputDevice_SetDataFormat( g_pMouse, &df );

	if( FAILED( hr ) ) {
		Com_Printf( "Couldn't set DI mouse format\n" );
		return false;
	}

	// set the cooperativity level
	hr = IDirectInputDevice_SetCooperativeLevel( g_pMouse, cl_hwnd,
												 DISCL_EXCLUSIVE | DISCL_FOREGROUND );

	if( FAILED( hr ) ) {
		Com_DPrintf( "Couldn't set DI coop level\n" );
		return false;
	}

	// set the buffer size to DINPUT_BUFFERSIZE elements
	// the buffer size is a DWORD property associated with the device
	hr = IDirectInputDevice_SetProperty( g_pMouse, DIPROP_BUFFERSIZE, &dipdw.diph );

	if( FAILED( hr ) ) {
		Com_DPrintf( "Couldn't set DI buffersize\n" );
		return false;
	}

	return true;
}

/*
* IN_ShutdownDInput
*/
static void IN_ShutdownDInput( void ) {
	if( g_pMouse ) {
		IDirectInputDevice_SetCooperativeLevel( g_pMouse, cl_hwnd, DISCL_NONEXCLUSIVE | DISCL_BACKGROUND );
		IDirectInputDevice_Release( g_pMouse );
	}

	if( g_pdi ) {
		IDirectInput_Release( g_pdi );
	}

	if( hInstDI ) {
		FreeLibrary( hInstDI );
	}

	g_pMouse = NULL;
	g_pdi = NULL;
	hInstDI = NULL;
	pDirectInputCreate = NULL;
}

/*
=========================================================================

RAW INPUT

=========================================================================
*/

/*
* IN_RawInput_Register
*/
int IN_RawInput_Register( void ) {
	// This function registers to receive the WM_INPUT messages
	RAWINPUTDEVICE Rid; // Register only for mouse messages from WM_INPUT

	// register to get wm_input messages
	Rid.usUsagePage = 0x01;
	Rid.usUsage = 0x02;
	Rid.dwFlags = RIDEV_CAPTUREMOUSE | RIDEV_NOLEGACY; // adds HID mouse and also ignores clicks out of the window and legacy mouse messages
	Rid.hwndTarget = cl_hwnd;

	// Register to receive the WM_INPUT message for any change in mouse (buttons, wheel, and movement will all generate the same message)
	if( !( *_RRID )( &Rid, 1, sizeof( Rid ) ) ) {
		return 1;
	}
	return 0;
}

/*
* IN_RawInput_DeRegister
*/
static void IN_RawInput_DeRegister( void ) {
	RAWINPUTDEVICE Rid;

	// deregister raw input
	Rid.usUsagePage = 0x01;
	Rid.usUsage = 0x02;
	Rid.dwFlags = RIDEV_REMOVE;
	Rid.hwndTarget = NULL;

	( *_RRID )( &Rid, 1, sizeof( Rid ) );
}

/*
* IN_RawInput_IsRDPMouse
*/
int IN_RawInput_IsRDPMouse( const char *cDeviceString ) {
	const char cRDPString[] = "\\??\\Root#RDP_MOU#";
	int i;

	if( strlen( cDeviceString ) < strlen( cRDPString ) ) {
		return 0;
	}

	for( i = strlen( cRDPString ) - 1; i >= 0; i-- ) {
		if( cDeviceString[i] != cRDPString[i] ) {
			return 0;
		}
	}
	return 1; // is RDP mouse
}

/*
* IN_RawInput_Init
*
* Returns false if rawinput is not available
*/
bool IN_RawInput_Init( void ) {
	int inputdevices, i, j, mtemp;
	PRAWINPUTDEVICELIST pRawInputDeviceList;
	char dname[MAX_RI_DEVICE_SIZE];
	HMODULE user32 = LoadLibrary( "user32.dll" );

	_GRIDL          = NULL;
	_GRID           = NULL;
	_GRIDIA         = NULL;
	_RRID           = NULL;

	rawmice         = NULL;
	rawmicecount    = 0;
	raw             = NULL;
	ribuffersize    = 0;

	if( !user32 ) {
		Com_Printf( "Raw input: unable to load user32.dll\n" );
		return false;
	}

	if( !( _RRID = ( pRegisterRawInputDevices )GetProcAddress( user32, "RegisterRawInputDevices" ) ) ) {
		Com_Printf( "Raw input: function RegisterRawInputDevices could not be registered\n" );
		return false;
	}

	if( !( _GRIDL = ( pGetRawInputDeviceList )GetProcAddress( user32, "GetRawInputDeviceList" ) ) ) {
		Com_Printf( "Raw input: function GetRawInputDeviceList could not be registered\n" );
		return false;
	}

	if( !( _GRIDIA = ( pGetRawInputDeviceInfoA )GetProcAddress( user32, "GetRawInputDeviceInfoA" ) ) ) {
		Com_Printf( "Raw input: function GetRawInputDeviceInfoA could not be registered\n" );
		return false;
	}

	if( !( _GRID = ( pGetRawInputData )GetProcAddress( user32, "GetRawInputData" ) ) ) {
		Com_Printf( "Raw input: function GetRawInputData could not be registered\n" );
		return false;
	}

	// 1st call to GetRawInputDeviceList: Pass NULL to get the number of devices.
	if( ( *_GRIDL )( NULL, &inputdevices, sizeof( RAWINPUTDEVICELIST ) ) != 0 ) {
		Com_Printf( "Raw input: unable to count raw input devices\n" );
		return false;
	}

	// Allocate the array to hold the DeviceList
	pRawInputDeviceList = (PRAWINPUTDEVICELIST)Q_malloc( sizeof( RAWINPUTDEVICELIST ) * inputdevices );

	// 2nd call to GetRawInputDeviceList: Pass the pointer to our DeviceList and GetRawInputDeviceList() will fill the array
	if( ( *_GRIDL )( pRawInputDeviceList, &inputdevices, sizeof( RAWINPUTDEVICELIST ) ) == -1 ) {
		Com_Printf( "Raw input: unable to get raw input device list\n" );
		return false;
	}

	// Loop through all devices and count the mice
	for( i = 0, mtemp = 0; i < inputdevices; i++ ) {
		if( pRawInputDeviceList[i].dwType == RIM_TYPEMOUSE ) {
			j = MAX_RI_DEVICE_SIZE;

			// Get the device name and use it to determine if it's the RDP Terminal Services virtual device.
			if( ( *_GRIDIA )( pRawInputDeviceList[i].hDevice, RIDI_DEVICENAME, dname, &j ) < 0 ) {
				dname[0] = 0;
			}

			if( IN_RawInput_IsRDPMouse( dname ) ) { // ignore rdp mouse
				continue;
			}

			// advance temp device count
			mtemp++;
		}
	}

	// exit out if no devices found
	if( !mtemp ) {
		Com_Printf( "Raw input: no usable device found\n" );
		return false;
	}

	// Loop again and bind devices
	rawmice = (rawmouse_t *)Q_malloc( sizeof( rawmouse_t ) * mtemp );
	for( i = 0; i < inputdevices; i++ ) {
		if( pRawInputDeviceList[i].dwType == RIM_TYPEMOUSE ) {
			j = MAX_RI_DEVICE_SIZE;

			// Get the device name and use it to determine if it's the RDP Terminal Services virtual device.
			if( ( *_GRIDIA )( pRawInputDeviceList[i].hDevice, RIDI_DEVICENAME, dname, &j ) < 0 ) {
				dname[0] = 0;
			}

			if( IN_RawInput_IsRDPMouse( dname ) ) { // ignore rdp mouse
				continue;
			}

			// print pretty message about the mouse
			dname[MAX_RI_DEVICE_SIZE - 1] = 0;
			for( mtemp = strlen( dname ); mtemp >= 0; mtemp-- ) {
				if( dname[mtemp] == '#' ) {
					dname[mtemp + 1] = 0;
					break;
				}
			}
			Com_Printf( "Raw input: [%i] %s\n", i, dname );

			// set handle
			rawmice[rawmicecount].rawinputhandle = pRawInputDeviceList[i].hDevice;
			rawmice[rawmicecount].numbuttons = 10;
			rawmice[rawmicecount].pos[0] = RI_INVALID_POS;
			rawmicecount++;
		}
	}

	// free the RAWINPUTDEVICELIST
	Q_free( pRawInputDeviceList );

	// alloc raw input buffer
	raw = (RAWINPUT *)Q_malloc( INIT_RIBUFFER_SIZE );
	ribuffersize = INIT_RIBUFFER_SIZE;

	return true;
}

/*
* IN_RawInput_Shutdown
*/
static void IN_RawInput_Shutdown( void ) {
	if( rawmicecount < 1 ) {
		return;
	}

	IN_RawInput_DeRegister();

	Q_free( rawmice );
	Q_free( raw );

	// dealloc mouse structure
	rawmicecount = 0;
}

/*
* IN_RawInput_MouseRead
*/
void IN_RawInput_MouseRead( HANDLE in_device_handle ) {
	int i = 0, tbuttons, j;
	int dwSize;

	if( !raw || !rawmice || rawmicecount < 1 ) {
		return; // no thx

	}
	// get raw input
	if( ( *_GRID )( (HRAWINPUT)in_device_handle, RID_INPUT, NULL, &dwSize, sizeof( RAWINPUTHEADER ) ) == -1 ) {
		Com_Printf( "Raw input: unable to add to get size of raw input header.\n" );
		return;
	}

	if( dwSize > ribuffersize ) {
		ribuffersize = dwSize;
		raw = (RAWINPUT *)Q_realloc( raw, dwSize );
	}

	if( ( *_GRID )( (HRAWINPUT)in_device_handle, RID_INPUT, raw, &dwSize, sizeof( RAWINPUTHEADER ) ) != dwSize ) {
		Com_Printf( "Raw input: unable to add to get raw input header.\n" );
		return;
	}

	// find mouse in our mouse list
	for( ; i < rawmicecount; i++ ) {
		if( rawmice[i].rawinputhandle == raw->header.hDevice ) {
			break;
		}
	}

	if( i == rawmicecount ) { // we're not tracking this mouse
		return;
	}

	// movement
	if( raw->data.mouse.usFlags & MOUSE_MOVE_ABSOLUTE ) {
		if( rawmice[i].pos[0] != RI_INVALID_POS ) {
			rawmice[i].delta[0] += raw->data.mouse.lLastX - rawmice[i].pos[0];
			rawmice[i].delta[1] += raw->data.mouse.lLastY - rawmice[i].pos[1];
		}
		rawmice[i].pos[0] = raw->data.mouse.lLastX;
		rawmice[i].pos[1] = raw->data.mouse.lLastY;
	} else {   // RELATIVE
		rawmice[i].delta[0] += raw->data.mouse.lLastX;
		rawmice[i].delta[1] += raw->data.mouse.lLastY;
		rawmice[i].pos[0] = RI_INVALID_POS;
	}

	// buttons
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_1_DOWN ) {
		Key_MouseEvent( K_MOUSE1, true, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_1_UP ) {
		Key_MouseEvent( K_MOUSE1, false, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_2_DOWN ) {
		Key_MouseEvent( K_MOUSE2, true, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_2_UP ) {
		Key_MouseEvent( K_MOUSE2, false, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_3_DOWN ) {
		Key_MouseEvent( K_MOUSE3, true, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_3_UP ) {
		Key_MouseEvent( K_MOUSE3, false, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_4_DOWN ) {
		Key_MouseEvent( K_MOUSE4, true, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_4_UP ) {
		Key_MouseEvent( K_MOUSE4, false, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_5_DOWN ) {
		Key_MouseEvent( K_MOUSE5, true, sys_msg_time );
	}
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_BUTTON_5_UP ) {
		Key_MouseEvent( K_MOUSE5, false, sys_msg_time );
	}

	// mouse wheel
	if( raw->data.mouse.usButtonFlags & RI_MOUSE_WHEEL ) {
		// if the current message has a mouse_wheel message
		if( (SHORT)raw->data.mouse.usButtonData > 0 ) {
			Key_Event( K_MWHEELUP, true, sys_msg_time );
			Key_Event( K_MWHEELUP, false, sys_msg_time );
		}

		if( (SHORT)raw->data.mouse.usButtonData < 0 ) {
			Key_Event( K_MWHEELDOWN, true, sys_msg_time );
			Key_Event( K_MWHEELDOWN, false, sys_msg_time );
		}
	}

	// extra buttons
	tbuttons = raw->data.mouse.ulRawButtons & RI_RAWBUTTON_MASK;
	for( j = 6; j < rawmice[i].numbuttons; j++ ) {
		if( ( tbuttons & ( 1 << j ) ) && !( rawmice[i].buttons & ( 1 << j ) ) ) {
			Key_MouseEvent( K_MOUSE1 + j, true, sys_msg_time );
		}

		if( !( tbuttons & ( 1 << j ) ) && ( rawmice[i].buttons & ( 1 << j ) ) ) {
			Key_MouseEvent( K_MOUSE1 + j, false, sys_msg_time );
		}
	}

	rawmice[i].buttons &= ~RI_RAWBUTTON_MASK;
	rawmice[i].buttons |= tbuttons;
}

/*
* IN_StartupMouse
*/
static void IN_StartupMouse( void ) {
	cvar_t *cv;

	cv = Cvar_Get( "in_initmouse", "1", CVAR_NOSET );
	if( !cv->integer ) {
		return;
	}

	dinput_initialized = false;
	rawinput_initialized = false;

	cv = Cvar_Get( "m_raw", "1", CVAR_ARCHIVE );
	if( cv->integer ) {
		rawinput_initialized = IN_RawInput_Init();
	}

	if( !rawinput_initialized ) {
		cv = Cvar_Get( "in_dinput", "0", CVAR_ARCHIVE );
		if( cv->integer ) {
			dinput_initialized = IN_InitDInput();
		}
	}

	if( rawinput_initialized ) {
		Com_Printf( "Raw input initialized with %i mice\n", rawmicecount );
	} else if( dinput_initialized ) {
		Com_Printf( "DirectInput initialized\n" );
	} else {
		Com_Printf( "DirectInput not initialized, using standard input\n" );
	}

	mouseinitialized = true;
	mouseparmsvalid = SystemParametersInfo( SPI_GETMOUSE, 0, originalmouseparms, 0 );
	mouse_buttons = 8;
	mouse_wheel_type = MWHEEL_UNKNOWN;
}

/*
* IN_MouseEvent
*/
void IN_MouseEvent( int mstate ) {
	int i;

	if( !mouseinitialized || dinput_initialized ) {
		return;
	}
	if( Con_HasKeyboardFocus() && !in_grabinconsole->integer ) {
		return;
	}

	// perform button actions
	for( i = 0; i < mouse_buttons; i++ ) {
		if( ( mstate & ( 1 << i ) ) &&
			!( mouse_oldbuttonstate & ( 1 << i ) ) ) {
			Key_MouseEvent( K_MOUSE1 + i, true, sys_msg_time );
		}

		if( !( mstate & ( 1 << i ) ) &&
			( mouse_oldbuttonstate & ( 1 << i ) ) ) {
			Key_MouseEvent( K_MOUSE1 + i, false, sys_msg_time );
		}
	}

	mouse_oldbuttonstate = mstate;
}

/*
* IN_GetMouseMovement
*/
void IN_GetMouseMovement( int *dx, int *dy ) {
	DIDEVICEOBJECTDATA od;
	DWORD dwElements;
	HRESULT hr;

	*dx = *dy = 0;
	
	if( !mouseactive ) {
		return;
	}

	if( rawinput_initialized ) {
		// probably not the right way...
		int i;

		mx = my = 0;

		for( i = 0; i < rawmicecount; i++ ) {
			mx += rawmice[i].delta[0];
			my += rawmice[i].delta[1];
			rawmice[i].delta[0] = rawmice[i].delta[1] = 0;
		}
	} else if( dinput_initialized ) {
		mx = 0;
		my = 0;

		for(;; ) {
			dwElements = 1;

			hr = IDirectInputDevice_GetDeviceData( g_pMouse,
												   sizeof( DIDEVICEOBJECTDATA ), &od, &dwElements, 0 );

			if( ( hr == DIERR_INPUTLOST ) || ( hr == DIERR_NOTACQUIRED ) ) {
				dinput_acquired = true;
				IDirectInputDevice_Acquire( g_pMouse );
				break;
			}

			// unable to read data or no data available
			if( FAILED( hr ) || dwElements == 0 ) {
				break;
			}

			sys_msg_time = od.dwTimeStamp;

			// look at the element to see what happened
			switch( od.dwOfs ) {
				case DIMOFS_X:
					mx += (int)od.dwData;
					break;

				case DIMOFS_Y:
					my += (int)od.dwData;
					break;

				case DIMOFS_Z:
					if( mouse_wheel_type != MWHEEL_WM ) {
						mouse_wheel_type = MWHEEL_DINPUT;
						if( (int)od.dwData > 0 ) {
							Key_Event( K_MWHEELUP, true, sys_msg_time );
							Key_Event( K_MWHEELUP, false, sys_msg_time );
						} else {
							Key_Event( K_MWHEELDOWN, true, sys_msg_time );
							Key_Event( K_MWHEELDOWN, false, sys_msg_time );
						}
					}
					break;

				case DIMOFS_BUTTON0:
				case DIMOFS_BUTTON1:
				case DIMOFS_BUTTON2:
				case DIMOFS_BUTTON3:
				case DIMOFS_BUTTON0 + 4:
				case DIMOFS_BUTTON0 + 5:
				case DIMOFS_BUTTON0 + 6:
				case DIMOFS_BUTTON0 + 7:
					if( od.dwData & 0x80 ) {
						mstate_di |= ( 1 << ( od.dwOfs - DIMOFS_BUTTON0 ) );
					} else {
						mstate_di &= ~( 1 << ( od.dwOfs - DIMOFS_BUTTON0 ) );
					}
					break;
			}
		}

		dinput_initialized = false; // FIXME...
		IN_MouseEvent( mstate_di );
		dinput_initialized = true;
	} else {
		// find mouse movement
		if( !GetCursorPos( &current_pos ) ) {
			return;
		}

		mx = current_pos.x - window_center_x;
		my = current_pos.y - window_center_y;

		// force the mouse to the center, so there's room to move
		if( mx || my ) {
			SetCursorPos( window_center_x, window_center_y );
		}
	}

	*dx = mx;
	*dy = my;
}

/*
* IN_Init
*/
void IN_Init( void ) {
	Com_Printf( "\n------- input initialization -------\n" );

	// mouse variables
	in_mouse        = Cvar_Get( "in_mouse", "1", CVAR_ARCHIVE );
	in_grabinconsole    = Cvar_Get( "in_grabinconsole", "0", CVAR_ARCHIVE );

	IN_StartupMouse();

	Com_Printf( "------------------------------------\n" );
}

/*
* IN_Shutdown
*/
void IN_Shutdown( void ) {
	IN_DeactivateMouse();

	if( rawinput_initialized ) {
		IN_RawInput_Shutdown();
	} else if( dinput_initialized ) {
		IN_ShutdownDInput();
	}

	dinput_acquired = dinput_initialized = false;
	rawinput_initialized = false;
}

/*
* IN_Restart
*/
void IN_Restart( const CmdArgs & ) {
	IN_Shutdown();
	IN_Init();
}

/*
* IN_Activate
*
* Called when the main window gains or loses focus.
* The window may have been destroyed and recreated
* between a deactivate and an activate.
*/
void IN_Activate( bool active ) {
	in_appactive = active;
	mouseactive = !active;  // force a new window check or turn off
}


/*
* IN_Frame
*
* Called every frame, even if not generating commands
*/
void IN_Frame( void ) {
	extern cvar_t *vid_fullscreen;

	if( !mouseinitialized ) {
		return;
	}

	if( vid_fullscreen ) {
		if( !vid_fullscreen->integer || cl_parent_hwnd ) {
			extern cvar_t *in_grabinconsole;

			// if we have a parent window (say, a browser plugin window) and
			// the window is not focused, deactivate the input
			if( cl_parent_hwnd && !AppFocused ) {
				if( in_appactive ) {
					IN_Activate( false );
				}
			} else if( in_grabinconsole->integer || !Con_HasKeyboardFocus() ) {
				if( !in_appactive && ActiveApp ) {
					IN_Activate( true );
				}
			} else {
				if( in_appactive ) {
					IN_Activate( false );
				}
			}
		} else {
			if( !ActiveApp && in_appactive ) {
				IN_Activate( false );
			} else if( ActiveApp && !in_appactive ) {
				IN_Activate( true );
			}
		}
	}

	if( !in_mouse || !in_appactive ) {
		IN_DeactivateMouse();
		return;
	}

	IN_ActivateMouse();
}

/*
* IN_Commands
*/
void IN_Commands( void ) {
}