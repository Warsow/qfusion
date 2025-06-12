/*
Copyright (C) 2013 Victor Luchits
Copyright (C) 2024 Chasseur de Bots

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

#ifndef Q_THREADS_H
#define Q_THREADS_H

#include <cstdint>
#include <cstdlib>

//#define Q_THREADS_HAVE_CANCEL

struct qmutex_s;
typedef struct qmutex_s qmutex_t;

struct qthread_s;
typedef struct qthread_s qthread_t;

struct qcondvar_s;
typedef struct qcondvar_s qcondvar_t;

struct qbufPipe_s;
typedef struct qbufPipe_s qbufPipe_t;

qmutex_t *QMutex_Create( void );
void QMutex_Destroy( qmutex_t **pmutex );
void QMutex_Lock( qmutex_t *mutex );
void QMutex_Unlock( qmutex_t *mutex );

class TaskSystem;
class ProfilerHud;

namespace wsw {

[[noreturn]]
void failWithRuntimeError( const char * );

class Mutex {
	template <typename> friend class ScopedLock;
	friend class ::TaskSystem;
	friend class ::ProfilerHud;
public:
	Mutex( const Mutex & ) = delete;
	auto operator=( const Mutex & ) -> Mutex & = delete;
	Mutex( Mutex && ) = delete;
	auto operator=( Mutex && ) -> Mutex & = delete;

	Mutex() {
		if( !( m_underlying = QMutex_Create() ) ) [[unlikely]] {
			wsw::failWithRuntimeError( "Failed to create a mutex" );
		}
	}
	~Mutex() { QMutex_Destroy( &m_underlying ); }
private:
	void lock() { QMutex_Lock( m_underlying ); }
	void unlock() { QMutex_Unlock( m_underlying ); }

	qmutex_t *m_underlying;
};

template <typename T>
class ScopedLock {
public:
	ScopedLock( const ScopedLock<T> & ) = delete;
	auto operator=( const ScopedLock<T> & ) -> ScopedLock<T> & = delete;
	ScopedLock( ScopedLock<T> && ) = delete;
	auto operator=( ScopedLock<T> && ) -> ScopedLock & = delete;

	explicit ScopedLock( T *lockable ) : m_lockable( lockable ) { m_lockable->lock(); }
	~ScopedLock() { m_lockable->unlock(); }
private:
	T *m_lockable;
};

}

qcondvar_t *QCondVar_Create( void );
void QCondVar_Destroy( qcondvar_t **pcond );
bool QCondVar_Wait( qcondvar_t *cond, qmutex_t *mutex, unsigned int timeout_msec );
void QCondVar_Wake( qcondvar_t *cond );

qthread_t *QThread_Create( void *( *routine )( void* ), void *param );
void QThread_Join( qthread_t *thread );
int QThread_Cancel( qthread_t *thread );
void QThread_Yield( void );

qbufPipe_t *QBufPipe_Create( size_t bufSize, int flags );
void QBufPipe_Destroy( qbufPipe_t **pqueue );
void QBufPipe_Finish( qbufPipe_t *queue );

uint8_t *QBufPipe_AcquireWritableBytes( qbufPipe_t *queue, unsigned bytesToAcquire );
void QBufPipe_SubmitWrittenBytes( qbufPipe_t *queue, unsigned bytesToSubmit );

using PipeWaiterFn = int (*)( qbufPipe_t *, bool );

int QBufPipe_ReadCmds( qbufPipe_t *queue );
void QBufPipe_Wait( qbufPipe_t *queue, PipeWaiterFn waiterFn, unsigned timeout_msec );

#ifndef CHECK_CALLING_THREAD
#ifdef _DEBUG
#define CHECK_CALLING_THREAD
#endif
#endif

class CallingThreadChecker {
public:
#ifdef CHECK_CALLING_THREAD
	void markCurrentThreadForFurtherAccessChecks();
	void clearThreadForFurtherAccessChecks();
	void checkCallingThread( const char *file = __builtin_FILE(), int line = __builtin_LINE() ) const;
#else
	void markCurrentThreadForFurtherAccessChecks() {}
	void clearThreadForFurtherAccessChecks() {}
	void checkCallingThread( const char *file = nullptr, int line = 0 ) const {}
#endif
private:
#ifdef CHECK_CALLING_THREAD
	uint64_t m_threadId { 0 };
#endif
};

#endif // Q_THREADS_H
