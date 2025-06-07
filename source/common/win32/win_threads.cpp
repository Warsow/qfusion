/*
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

#include <common/common.h>
#include <common/syslocal.h>
#include <windows.h>
#include <process.h>

struct qthread_s {
	HANDLE h;
};

struct qcondvar_s {
	CONDITION_VARIABLE c;
};

struct qmutex_s {
	CRITICAL_SECTION h;
};

int Sys_Mutex_Create( qmutex_t **pmutex ) {
	qmutex_t *mutex;

	mutex = ( qmutex_t * )Q_malloc( sizeof( *mutex ) );
	if( !mutex ) {
		return -1;
	}
	InitializeCriticalSection( &mutex->h );

	*pmutex = mutex;
	return 0;
}

void Sys_Mutex_Destroy( qmutex_t *mutex ) {
	if( !mutex ) {
		return;
	}
	DeleteCriticalSection( &mutex->h );
	Q_free( mutex );
}

void Sys_Mutex_Lock( qmutex_t *mutex ) {
	EnterCriticalSection( &mutex->h );
}

void Sys_Mutex_Unlock( qmutex_t *mutex ) {
	LeaveCriticalSection( &mutex->h );
}

int Sys_Thread_Create( qthread_t **pthread, void *( *routine )( void* ), void *param ) {
	qthread_t *thread;
	unsigned threadID;
	HANDLE h;

	h = (HANDLE)_beginthreadex( NULL, 0, ( unsigned( WINAPI * ) ( void * ) )routine, param, 0, &threadID );

	if( h == NULL ) {
		return GetLastError();
	}

	thread = ( qthread_t * )Q_malloc( sizeof( *thread ) );
	thread->h = h;
	*pthread = thread;
	return 0;
}

void Sys_Thread_Join( qthread_t *thread ) {
	if( thread ) {
		WaitForSingleObject( thread->h, INFINITE );
		CloseHandle( thread->h );
		free( thread );
	}
}

void Sys_Thread_Yield() {
	Sys_Sleep( 0 );
}

uint64_t Sys_Thread_GetId() {
	return GetCurrentThreadId();
}

int Sys_Atomic_Add( volatile int *value, int add, qmutex_t *mutex ) {
	return InterlockedExchangeAdd( (volatile LONG*)value, add );
}

bool Sys_Atomic_CAS( volatile int *value, int oldval, int newval, qmutex_t *mutex ) {
	return InterlockedCompareExchange( (volatile LONG*)value, newval, oldval ) == oldval;
}

int Sys_CondVar_Create( qcondvar_t **pcond ) {
	qcondvar_t *cond;

	if( !pcond ) {
		return -1;
	}

	cond = ( qcondvar_t * )Q_malloc( sizeof( *cond ) );
	InitializeConditionVariable( &( cond->c ) );

	*pcond = cond;

	return 0;
}

void Sys_CondVar_Destroy( qcondvar_t *cond ) {
	if( !cond ) {
		return;
	}

	Q_free( cond );
}

bool Sys_CondVar_Wait( qcondvar_t *cond, qmutex_t *mutex, unsigned int timeout_msec ) {
	if( !cond || !mutex ) {
		return false;
	}

	return SleepConditionVariableCS( &cond->c, &mutex->h, timeout_msec ) != 0;
}

void Sys_CondVar_Wake( qcondvar_t *cond ) {
	if( !cond ) {
		return;
	}
	WakeConditionVariable( &cond->c );
}