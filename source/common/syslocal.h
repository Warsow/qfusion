/*
Copyright (C) 2007 Pekka Lampila
Copyright (C) 2013 Victor Luchits

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

#ifndef __SYS_NET_H
#define __SYS_NET_H

#include <common/facilities/net.h>
#include <common/helpers/qthreads.h>

#include <optional>
#include <cstdio>

void    Sys_Init( int, char ** );

char    *Sys_ConsoleInput( void );
void    Sys_ConsoleOutput( char *string );

int     Sys_GetCurrentProcessId( void );

void        Sys_NET_Init( void );
void        Sys_NET_Shutdown( void );

net_error_t Sys_NET_GetLastError( void );

void        Sys_NET_SocketClose( socket_handle_t handle );
int         Sys_NET_SocketIoctl( socket_handle_t handle, long request, ioctl_param_t* param );

int64_t     Sys_NET_SendFile( socket_handle_t handle, int fileno, size_t offset, size_t count );

bool Sys_Library_Close( void *lib );
const char *Sys_Library_GetFullName( const char *name );
void *Sys_Library_Open( const char *name );
void *Sys_Library_ProcAddress( void *lib, const char *apifuncname );
const char *Sys_Library_ErrorString( void );

#define Q_THREADS_WAIT_INFINITE 0xFFFFFFFF

int Sys_Thread_Create( qthread_t **pthread, void *( *routine )( void* ), void *param );
void Sys_Thread_Join( qthread_t *thread );
void Sys_Thread_Yield( void );
uint64_t Sys_Thread_GetId();

int Sys_Mutex_Create( qmutex_t **pmutex );
void Sys_Mutex_Destroy( qmutex_t *mutex );
void Sys_Mutex_Lock( qmutex_t *mutex );
void Sys_Mutex_Unlock( qmutex_t *mutex );
int Sys_Atomic_Add( volatile int *value, int add, qmutex_t *mutex );
bool Sys_Atomic_CAS( volatile int *value, int oldval, int newval, qmutex_t *mutex );

int Sys_CondVar_Create( qcondvar_t **pcond );
void Sys_CondVar_Destroy( qcondvar_t *cond );
bool Sys_CondVar_Wait( qcondvar_t *cond, qmutex_t *mutex, unsigned int timeout_msec );
void Sys_CondVar_Wake( qcondvar_t *cond );

const char *Sys_FS_GetHomeDirectory( void );
const char *Sys_FS_GetCacheDirectory( void );
const char *Sys_FS_GetRuntimeDirectory( void );

bool    Sys_FS_RemoveDirectory( const char *path );
bool    Sys_FS_CreateDirectory( const char *path );

const char *Sys_FS_FindFirst( const char *path, unsigned musthave, unsigned canthave );
const char *Sys_FS_FindNext( unsigned musthave, unsigned canthave );
void        Sys_FS_FindClose( void );

void        *Sys_FS_LockFile( const char *path );
void        Sys_FS_UnlockFile( void *handle );

time_t      Sys_FS_FileMTime( const char *filename );

int         Sys_FS_FileNo( FILE *fp );

void        *Sys_FS_MMapFile( int fileno, size_t size, size_t offset, void **mapping, size_t *mapping_offset );
void        Sys_FS_UnMMapFile( void *mapping, void *data, size_t size, size_t mapping_offset );

void       Sys_Sleep( unsigned millis );

[[nodiscard]]
auto testProcessorFeatures() -> unsigned;
[[nodiscard]]
auto testNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>>;

#endif // __SYS_NET_H
