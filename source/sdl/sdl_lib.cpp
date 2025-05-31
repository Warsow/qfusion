#include <SDL.h>
#include <dlfcn.h>

#include <common/common.h>
#include <common/sys_library.h>

/*
* Sys_Library_Close
*/
bool Sys_Library_Close( void *lib ) {
	SDL_UnloadObject( lib );
	return true;
}

/*
* Sys_Library_GetFullName
*/
const char *Sys_Library_GetFullName( const char *name ) {
	return FS_AbsoluteNameForBaseFile( name );
}

/*
* Sys_Library_GetGameLibPath
*/
const char *Sys_Library_GetGameLibPath( const char *name, int64_t time, int randomizer ) {
	static char tempname[1024 * 10];
	Q_snprintfz( tempname, sizeof( tempname ), "%s/%s/tempmodules_%" PRIi64 "_%d_%d/%s", FS_RuntimeDirectory(), kDataDirectory.data(),
				 time, Sys_GetCurrentProcessId(), randomizer, name );
	return tempname;
}

/*
* Sys_Library_Open
*/
void *Sys_Library_Open( const char *name ) {
	return dlopen( name, RTLD_NOW );
}

/*
* Sys_Library_ProcAddress
*/
void *Sys_Library_ProcAddress( void *lib, const char *apifuncname ) {
	return dlsym( lib, apifuncname );
}

/*
* Sys_Library_ErrorString
*/
const char *Sys_Library_ErrorString( void ) {
	return dlerror();
}
