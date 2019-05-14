/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.
Copyright (C) 2005 Stuart Dalton (badcdev@gmail.com)

This file is part of Quake III Arena source code.

Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Quake III Arena source code; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

// Dynamically loads OpenAL

#include "../gameshared/q_arch.h"
#include "../gameshared/q_shared.h"

#include "qal.h"

// Keep loading EFX procedures dynamically even for OpenAL SOFT static builds

#define EFX_INIT_UNKNOWN ( -1 )
#define EFX_INIT_FAILURE ( 0 )
#define EFX_INIT_SUCCESS ( +1 )

static int efx_init_status = EFX_INIT_UNKNOWN;

static void *GetEfxProcAddress( ALCdevice *device, const char *str ) {
	if( !qalcGetProcAddress ) {
		efx_init_status = EFX_INIT_FAILURE;
		return NULL;
	}
	void *rv = qalcGetProcAddress( device, str );
	if( !rv ) {
		efx_init_status = EFX_INIT_FAILURE;
	}
	return rv;
}

static void QAL_EFX_Init( ALCdevice *device ) {
	qalGenEffects = GetEfxProcAddress( device, "alGenEffects" );
	qalDeleteEffects = GetEfxProcAddress( device, "alDeleteEffects" );
	qalIsEffect = GetEfxProcAddress( device, "alIsEffect" );
	qalEffecti = GetEfxProcAddress( device, "alEffecti" );
	qalEffectiv = GetEfxProcAddress( device, "alEffectiv" );
	qalEffectf = GetEfxProcAddress( device, "alEffectf" );
	qalEffectfv = GetEfxProcAddress( device, "alEffectfv" );
	qalGetEffecti = GetEfxProcAddress( device, "alGetEffecti" );
	qalGetEffeciv = GetEfxProcAddress( device, "alGetEffectiv" );
	qalGetEffectf = GetEfxProcAddress( device, "alGetEffectf" );
	qalGetEffectfv = GetEfxProcAddress( device, "alGetEffectfv" );

	qalGenFilters = GetEfxProcAddress( device, "alGenFilters" );
	qalDeleteFilters = GetEfxProcAddress( device, "alDeleteFilters" );
	qalIsFilter = GetEfxProcAddress( device, "alIsFilter" );
	qalFilteri = GetEfxProcAddress( device, "alFilteri" );
	qalFilteriv = GetEfxProcAddress( device, "alFilteriv" );
	qalFilterf = GetEfxProcAddress( device, "alFilterf" );
	qalFilterfv = GetEfxProcAddress( device, "alFilterfv" );
	qalGetFilteri = GetEfxProcAddress( device, "alGetFilteri" );
	qalGetFilteriv = GetEfxProcAddress( device, "alGetFilteriv" );
	qalGetFilterf = GetEfxProcAddress( device, "alGetFilterf" );
	qalGetFilterfv = GetEfxProcAddress( device, "alGetFilterfv" );

	qalGenAuxiliaryEffectSlots = GetEfxProcAddress( device, "alGenAuxiliaryEffectSlots" );
	qalDeleteAuxiliaryEffectSlots = GetEfxProcAddress( device, "alDeleteAuxiliaryEffectSlots" );
	qalIsAuxiliaryEffectSlot = GetEfxProcAddress( device, "alIsAuxiliaryEffectSlot" );
	qalAuxiliaryEffectSloti = GetEfxProcAddress( device, "alAuxiliaryEffectSloti" );
	qalAuxiliaryEffectSlotiv = GetEfxProcAddress( device, "alAuxiliaryEffectSlotiv" );
	qalAuxiliaryEffectSlotf = GetEfxProcAddress( device, "alAuxiliaryEffectSlotf" );
	qalAuxiliaryEffectSlotfv = GetEfxProcAddress( device, "alAuxiliaryEffectSlotfv" );
	qalGetAuxiliaryEffectSloti = GetEfxProcAddress( device, "alGetAuxiliaryEffectSloti" );
	qalGetAuxiliaryEffectSlotiv = GetEfxProcAddress( device, "alGetAuxiliaryEffectSlotiv" );
	qalGetAuxiliaryEffectSlotf = GetEfxProcAddress( device, "alGetAuxiliaryEffectSlotf" );
	qalGetAuxiliaryEffectSlotfv = GetEfxProcAddress( device, "alGetAuxiliaryEffectSlotfv" );

	// If the status has not been set to "failure"
	if( efx_init_status != EFX_INIT_FAILURE ) {
		assert( efx_init_status == EFX_INIT_UNKNOWN );
		efx_init_status = EFX_INIT_SUCCESS;
	}
}

static void QAL_EFX_Shutdown() {
	qalGenEffects = NULL;
	qalDeleteEffects = NULL;
	qalIsEffect = NULL;
	qalEffecti = NULL;
	qalEffectiv = NULL;
	qalEffectf = NULL;
	qalEffectfv = NULL;
	qalGetEffecti = NULL;
	qalGetEffeciv = NULL;
	qalGetEffectf = NULL;
	qalGetEffectfv = NULL;

	qalGenFilters = NULL;
	qalDeleteFilters = NULL;
	qalIsFilter = NULL;
	qalFilteri = NULL;
	qalFilteriv = NULL;
	qalFilterf = NULL;
	qalFilterfv = NULL;
	qalGetFilteri = NULL;
	qalGetFilteriv = NULL;
	qalGetFilterf = NULL;
	qalGetFilterfv = NULL;

	qalGenAuxiliaryEffectSlots = NULL;
	qalDeleteAuxiliaryEffectSlots = NULL;
	qalIsAuxiliaryEffectSlot = NULL;
	qalAuxiliaryEffectSloti = NULL;
	qalAuxiliaryEffectSlotiv = NULL;
	qalAuxiliaryEffectSlotf = NULL;
	qalAuxiliaryEffectSlotfv = NULL;
	qalGetAuxiliaryEffectSloti = NULL;
	qalGetAuxiliaryEffectSlotiv = NULL;
	qalGetAuxiliaryEffectSlotf = NULL;
	qalGetAuxiliaryEffectSlotfv = NULL;

	efx_init_status = EFX_INIT_UNKNOWN;
}

// Always define symbols for loaded procedures

LPALGENEFFECTS qalGenEffects;
LPALDELETEEFFECTS qalDeleteEffects;
LPALISEFFECT qalIsEffect;
LPALEFFECTI qalEffecti;
LPALEFFECTIV qalEffectiv;
LPALEFFECTF qalEffectf;
LPALEFFECTFV qalEffectfv;
LPALGETEFFECTI qalGetEffecti;
LPALGETEFFECTIV qalGetEffeciv;
LPALGETEFFECTF qalGetEffectf;
LPALGETEFFECTFV qalGetEffectfv;

LPALGENFILTERS qalGenFilters;
LPALDELETEFILTERS qalDeleteFilters;
LPALISFILTER qalIsFilter;
LPALFILTERI qalFilteri;
LPALFILTERIV qalFilteriv;
LPALFILTERF qalFilterf;
LPALFILTERFV qalFilterfv;
LPALGETFILTERI qalGetFilteri;
LPALGETFILTERIV qalGetFilteriv;
LPALGETFILTERF qalGetFilterf;
LPALGETFILTERFV qalGetFilterfv;

LPALGENAUXILIARYEFFECTSLOTS qalGenAuxiliaryEffectSlots;
LPALDELETEAUXILIARYEFFECTSLOTS qalDeleteAuxiliaryEffectSlots;
LPALISAUXILIARYEFFECTSLOT qalIsAuxiliaryEffectSlot;
LPALAUXILIARYEFFECTSLOTI qalAuxiliaryEffectSloti;
LPALAUXILIARYEFFECTSLOTIV qalAuxiliaryEffectSlotiv;
LPALAUXILIARYEFFECTSLOTF qalAuxiliaryEffectSlotf;
LPALAUXILIARYEFFECTSLOTFV qalAuxiliaryEffectSlotfv;
LPALGETAUXILIARYEFFECTSLOTI qalGetAuxiliaryEffectSloti;
LPALGETAUXILIARYEFFECTSLOTIV qalGetAuxiliaryEffectSlotiv;
LPALGETAUXILIARYEFFECTSLOTF qalGetAuxiliaryEffectSlotf;
LPALGETAUXILIARYEFFECTSLOTFV qalGetAuxiliaryEffectSlotfv;

#ifdef OPENAL_RUNTIME

static bool alinit_fail = false;

LPALENABLE qalEnable;
LPALDISABLE qalDisable;
LPALISENABLED qalIsEnabled;
LPALGETSTRING qalGetString;
LPALGETBOOLEANV qalGetBooleanv;
LPALGETINTEGERV qalGetIntegerv;
LPALGETFLOATV qalGetFloatv;
LPALGETDOUBLEV qalGetDoublev;
LPALGETBOOLEAN qalGetBoolean;
LPALGETINTEGER qalGetInteger;
LPALGETFLOAT qalGetFloat;
LPALGETDOUBLE qalGetDouble;
LPALGETERROR qalGetError;
LPALISEXTENSIONPRESENT qalIsExtensionPresent;
LPALGETPROCADDRESS qalGetProcAddress;
LPALGETENUMVALUE qalGetEnumValue;
LPALLISTENERF qalListenerf;
LPALLISTENER3F qalListener3f;
LPALLISTENERFV qalListenerfv;
LPALLISTENERI qalListeneri;
LPALGETLISTENERF qalGetListenerf;
LPALGETLISTENER3F qalGetListener3f;
LPALGETLISTENERFV qalGetListenerfv;
LPALGETLISTENERI qalGetListeneri;
LPALGENSOURCES qalGenSources;
LPALDELETESOURCES qalDeleteSources;
LPALISSOURCE qalIsSource;
LPALSOURCEF qalSourcef;
LPALSOURCE3F qalSource3f;
LPALSOURCEFV qalSourcefv;
LPALSOURCEI qalSourcei;
LPALSOURCE3I qalSource3i;
LPALGETSOURCEF qalGetSourcef;
LPALGETSOURCE3F qalGetSource3f;
LPALGETSOURCEFV qalGetSourcefv;
LPALGETSOURCEI qalGetSourcei;
LPALSOURCEPLAYV qalSourcePlayv;
LPALSOURCESTOPV qalSourceStopv;
LPALSOURCEREWINDV qalSourceRewindv;
LPALSOURCEPAUSEV qalSourcePausev;
LPALSOURCEPLAY qalSourcePlay;
LPALSOURCESTOP qalSourceStop;
LPALSOURCEREWIND qalSourceRewind;
LPALSOURCEPAUSE qalSourcePause;
LPALSOURCEQUEUEBUFFERS qalSourceQueueBuffers;
LPALSOURCEUNQUEUEBUFFERS qalSourceUnqueueBuffers;
LPALGENBUFFERS qalGenBuffers;
LPALDELETEBUFFERS qalDeleteBuffers;
LPALISBUFFER qalIsBuffer;
LPALBUFFERDATA qalBufferData;
LPALGETBUFFERF qalGetBufferf;
LPALGETBUFFERI qalGetBufferi;
LPALDOPPLERFACTOR qalDopplerFactor;
LPALDOPPLERVELOCITY qalDopplerVelocity;
LPALSPEEDOFSOUND qalSpeedOfSound;
LPALDISTANCEMODEL qalDistanceModel;

LPALCCREATECONTEXT qalcCreateContext;
LPALCMAKECONTEXTCURRENT qalcMakeContextCurrent;
LPALCPROCESSCONTEXT qalcProcessContext;
LPALCSUSPENDCONTEXT qalcSuspendContext;
LPALCDESTROYCONTEXT qalcDestroyContext;
LPALCGETCURRENTCONTEXT qalcGetCurrentContext;
LPALCGETCONTEXTSDEVICE qalcGetContextsDevice;
LPALCOPENDEVICE qalcOpenDevice;
LPALCCLOSEDEVICE qalcCloseDevice;
LPALCGETERROR qalcGetError;
LPALCISEXTENSIONPRESENT qalcIsExtensionPresent;
LPALCGETPROCADDRESS qalcGetProcAddress;
LPALCGETENUMVALUE qalcGetEnumValue;
LPALCGETSTRING qalcGetString;
LPALCGETINTEGERV qalcGetIntegerv;

/*#if USE_SDL_VIDEO
#include "SDL.h"
#include "SDL_loadso.h"
#define OBJTYPE void *
#define OBJLOAD(x) SDL_LoadObject(x)
#define SYMLOAD(x,y) SDL_LoadFunction(x,y)
#define OBJFREE(x) SDL_UnloadObject(x)*/

#if defined _WIN32
#include <windows.h>
#define OBJTYPE HMODULE
#define OBJLOAD( x ) LoadLibrary( x )
#define SYMLOAD( x, y ) GetProcAddress( x, y )
#define OBJFREE( x ) FreeLibrary( x )

#else
#include <dlfcn.h>
#define OBJTYPE void *
#define OBJLOAD( x ) dlopen( x, RTLD_LAZY | RTLD_GLOBAL )
#define SYMLOAD( x, y ) dlsym( x, y )
#define OBJFREE( x ) dlclose( x )
#endif

#if !(defined _WIN32 || defined __ANDROID__)
#include <unistd.h>
#include <sys/types.h>
#endif

static OBJTYPE OpenALLib = NULL;

/*
* GPA
*/
static void *GPA( char *str ) {
	void *rv;

	rv = SYMLOAD( OpenALLib, str );
	if( !rv ) {
		Com_Printf( " Couldn't load symbol: %s\n", str );
		alinit_fail = true;
		return NULL;
	} else {
		//Com_DPrintf( " Loaded symbol: %s (0x%08X)\n", str, rv);
		return rv;
	}
}

/*
* QAL_Init
*/
bool QAL_Init( const char *libname, bool verbose ) {
	if( OpenALLib ) {
		return true;
	}

	if( verbose ) {
		Com_Printf( "Loading OpenAL library: %s\n", libname );
	}

	if( ( OpenALLib = OBJLOAD( libname ) ) == 0 ) {
#ifdef _WIN32
		return false;
#else
		char fn[2048];

		if( getcwd( fn, sizeof( fn ) ) == NULL ) {
			return false;
		}

		Q_strncatz( fn, "/", sizeof( fn ) );
		Q_strncatz( fn, libname, sizeof( fn ) );

		if( ( OpenALLib = OBJLOAD( fn ) ) == 0 ) {
			return false;
		}
#endif
	}

	alinit_fail = false;

	qalEnable = GPA( "alEnable" );
	qalDisable = GPA( "alDisable" );
	qalIsEnabled = GPA( "alIsEnabled" );
	qalGetString = GPA( "alGetString" );
	qalGetBooleanv = GPA( "alGetBooleanv" );
	qalGetIntegerv = GPA( "alGetIntegerv" );
	qalGetFloatv = GPA( "alGetFloatv" );
	qalGetDoublev = GPA( "alGetDoublev" );
	qalGetBoolean = GPA( "alGetBoolean" );
	qalGetInteger = GPA( "alGetInteger" );
	qalGetFloat = GPA( "alGetFloat" );
	qalGetDouble = GPA( "alGetDouble" );
	qalGetError = GPA( "alGetError" );
	qalIsExtensionPresent = GPA( "alIsExtensionPresent" );
	qalGetProcAddress = GPA( "alGetProcAddress" );
	qalGetEnumValue = GPA( "alGetEnumValue" );
	qalListenerf = GPA( "alListenerf" );
	qalListener3f = GPA( "alListener3f" );
	qalListenerfv = GPA( "alListenerfv" );
	qalListeneri = GPA( "alListeneri" );
	qalGetListenerf = GPA( "alGetListenerf" );
	qalGetListener3f = GPA( "alGetListener3f" );
	qalGetListenerfv = GPA( "alGetListenerfv" );
	qalGetListeneri = GPA( "alGetListeneri" );
	qalGenSources = GPA( "alGenSources" );
	qalDeleteSources = GPA( "alDeleteSources" );
	qalIsSource = GPA( "alIsSource" );
	qalSourcef = GPA( "alSourcef" );
	qalSource3f = GPA( "alSource3f" );
	qalSourcefv = GPA( "alSourcefv" );
	qalSourcei = GPA( "alSourcei" );
	qalSource3i = GPA( "alSource3i" );
	qalGetSourcef = GPA( "alGetSourcef" );
	qalGetSource3f = GPA( "alGetSource3f" );
	qalGetSourcefv = GPA( "alGetSourcefv" );
	qalGetSourcei = GPA( "alGetSourcei" );
	qalSourcePlayv = GPA( "alSourcePlayv" );
	qalSourceStopv = GPA( "alSourceStopv" );
	qalSourceRewindv = GPA( "alSourceRewindv" );
	qalSourcePausev = GPA( "alSourcePausev" );
	qalSourcePlay = GPA( "alSourcePlay" );
	qalSourceStop = GPA( "alSourceStop" );
	qalSourceRewind = GPA( "alSourceRewind" );
	qalSourcePause = GPA( "alSourcePause" );
	qalSourceQueueBuffers = GPA( "alSourceQueueBuffers" );
	qalSourceUnqueueBuffers = GPA( "alSourceUnqueueBuffers" );
	qalGenBuffers = GPA( "alGenBuffers" );
	qalDeleteBuffers = GPA( "alDeleteBuffers" );
	qalIsBuffer = GPA( "alIsBuffer" );
	qalBufferData = GPA( "alBufferData" );
	qalGetBufferf = GPA( "alGetBufferf" );
	qalGetBufferi = GPA( "alGetBufferi" );
	qalDopplerFactor = GPA( "alDopplerFactor" );
	qalDopplerVelocity = GPA( "alDopplerVelocity" );
	qalSpeedOfSound = GPA( "alSpeedOfSound" );
	qalDistanceModel = GPA( "alDistanceModel" );

	qalcCreateContext = GPA( "alcCreateContext" );
	qalcMakeContextCurrent = GPA( "alcMakeContextCurrent" );
	qalcProcessContext = GPA( "alcProcessContext" );
	qalcSuspendContext = GPA( "alcSuspendContext" );
	qalcDestroyContext = GPA( "alcDestroyContext" );
	qalcGetCurrentContext = GPA( "alcGetCurrentContext" );
	qalcGetContextsDevice = GPA( "alcGetContextsDevice" );
	qalcOpenDevice = GPA( "alcOpenDevice" );
	qalcCloseDevice = GPA( "alcCloseDevice" );
	qalcGetError = GPA( "alcGetError" );
	qalcIsExtensionPresent = GPA( "alcIsExtensionPresent" );
	qalcGetProcAddress = GPA( "alcGetProcAddress" );
	qalcGetEnumValue = GPA( "alcGetEnumValue" );
	qalcGetString = GPA( "alcGetString" );
	qalcGetIntegerv = GPA( "alcGetIntegerv" );

	if( alinit_fail ) {
		QAL_Shutdown();
		Com_Printf( " Error: One or more symbols not found.\n" );
		return false;
	}

	return true;
}

/*
* QAL_Shutdown
*/
void QAL_Shutdown( void ) {
	if( OpenALLib ) {
		OBJFREE( OpenALLib );
		OpenALLib = NULL;
	}

	QAL_EFX_Shutdown();

	qalEnable = NULL;
	qalDisable = NULL;
	qalIsEnabled = NULL;
	qalGetString = NULL;
	qalGetBooleanv = NULL;
	qalGetIntegerv = NULL;
	qalGetFloatv = NULL;
	qalGetDoublev = NULL;
	qalGetBoolean = NULL;
	qalGetInteger = NULL;
	qalGetFloat = NULL;
	qalGetDouble = NULL;
	qalGetError = NULL;
	qalIsExtensionPresent = NULL;
	qalGetProcAddress = NULL;
	qalGetEnumValue = NULL;
	qalListenerf = NULL;
	qalListener3f = NULL;
	qalListenerfv = NULL;
	qalListeneri = NULL;
	qalGetListenerf = NULL;
	qalGetListener3f = NULL;
	qalGetListenerfv = NULL;
	qalGetListeneri = NULL;
	qalGenSources = NULL;
	qalDeleteSources = NULL;
	qalIsSource = NULL;
	qalSourcef = NULL;
	qalSource3f = NULL;
	qalSourcefv = NULL;
	qalSourcei = NULL;
	qalGetSourcef = NULL;
	qalGetSource3f = NULL;
	qalGetSourcefv = NULL;
	qalGetSourcei = NULL;
	qalSourcePlayv = NULL;
	qalSourceStopv = NULL;
	qalSourceRewindv = NULL;
	qalSourcePausev = NULL;
	qalSourcePlay = NULL;
	qalSourceStop = NULL;
	qalSourceRewind = NULL;
	qalSourcePause = NULL;
	qalSourceQueueBuffers = NULL;
	qalSourceUnqueueBuffers = NULL;
	qalGenBuffers = NULL;
	qalDeleteBuffers = NULL;
	qalIsBuffer = NULL;
	qalBufferData = NULL;
	qalGetBufferf = NULL;
	qalGetBufferi = NULL;
	qalDopplerFactor = NULL;
	qalDopplerVelocity = NULL;
	qalSpeedOfSound = NULL;
	qalDistanceModel = NULL;

	qalcCreateContext = NULL;
	qalcMakeContextCurrent = NULL;
	qalcProcessContext = NULL;
	qalcSuspendContext = NULL;
	qalcDestroyContext = NULL;
	qalcGetCurrentContext = NULL;
	qalcGetContextsDevice = NULL;
	qalcOpenDevice = NULL;
	qalcCloseDevice = NULL;
	qalcGetError = NULL;
	qalcIsExtensionPresent = NULL;
	qalcGetProcAddress = NULL;
	qalcGetEnumValue = NULL;
	qalcGetString = NULL;
	qalcGetIntegerv = NULL;
}

bool QAL_Is_EFX_ExtensionSupported( ALCdevice *device ) {
	// Note: This function might be called from different threads,
	// but results of QAL_EFX_Init() should be idempotent,
	// so we get some excessive computations at startup in worst case.
	if( efx_init_status == EFX_INIT_UNKNOWN ) {
		QAL_EFX_Init( device );
	}

	return efx_init_status == EFX_INIT_SUCCESS;
}

bool QAL_Is_HRTF_ExtensionSupported( ALCdevice *device ) {
	if( !qalcGetProcAddress ) {
		return false;
	}

	// Just test whether these symbols are present.`
	// http://kcat.strangesoft.net/openal-extensions/SOFT_HRTF.txt
	return qalcGetProcAddress( device, "alcGetStringiSOFT" ) && qalcGetProcAddress( device, "alcResetDeviceSOFT" );
}

#else // OPENAL_RUNTIME is not defined

bool QAL_Init( const char *libname, bool verbose ) {
	return true;
}

void QAL_Shutdown( void ) {
#ifdef OPENAL_SOFT_STATIC
	// While procedures loading is performed on demand when a selected device is known,
	// this is a single place where they get unloaded
	// (this is not actually needed but let's do it for consistency)
	QAL_EFX_Shutdown();
#endif
}

bool QAL_Is_EFX_ExtensionSupported( ALCdevice *device ) {
#ifdef OPENAL_SOFT_STATIC
	// Must always be present for static OpenAL SOFT builds
	assert( qalcIsExtensionPresent( device, "ALC_EXT_EFX" ) );
	// Hovewer, keep loading extension procedures manually.
	// Same notes about thread-safety apply for this static version as for the regular one.
	if( efx_init_status == EFX_INIT_UNKNOWN ) {
		QAL_EFX_Init( device );
	}
	// Must not fail
	assert( efx_init_status == EFX_INIT_SUCCESS );
	return true;
#else
	// We have decided to disable EFX on Android.
	// Even if a third-party library supports these effects,
	// and, furthermore, if there is a hardware support for these effects,
	// there is no sufficient processing power for environment sampling.
	return false;
#endif
}

bool QAL_Is_HRTF_ExtensionSupported( ALCdevice *device ) {
#ifdef OPENAL_SOFT_STATIC
	// Must always be present for static OpenAL SOFT builds
	assert( qalcIsExtensionPresent( device, "ALC_SOFT_HRTF" ) );
	return true;
#else
	// Let's allow using/looking for OpenAL SOFT HRTF on Android
	// even if its unlikely we will ever run the game on this platform
	return qalcIsExtensionPresent( device, "ALC_SOFT_HRTF" );
#endif
}

#endif
