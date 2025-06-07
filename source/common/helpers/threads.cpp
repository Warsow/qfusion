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
#include "pipeutils.h"
#include <common/syslocal.h>

/*
* QMutex_Create
*/
qmutex_t *QMutex_Create( void ) {
	int ret;
	qmutex_t *mutex;

	ret = Sys_Mutex_Create( &mutex );
	if( ret != 0 ) {
		Sys_Error( "QMutex_Create: failed with code %i", ret );
	}
	return mutex;
}

/*
* QMutex_Destroy
*/
void QMutex_Destroy( qmutex_t **pmutex ) {
	assert( pmutex != NULL );
	if( pmutex && *pmutex ) {
		Sys_Mutex_Destroy( *pmutex );
		*pmutex = NULL;
	}
}

/*
* QMutex_Lock
*/
void QMutex_Lock( qmutex_t *mutex ) {
	assert( mutex != NULL );
	Sys_Mutex_Lock( mutex );
}

/*
* QMutex_Unlock
*/
void QMutex_Unlock( qmutex_t *mutex ) {
	assert( mutex != NULL );
	Sys_Mutex_Unlock( mutex );
}

/*
* QCondVar_Create
*/
qcondvar_t *QCondVar_Create( void ) {
	int ret;
	qcondvar_t *cond;

	ret = Sys_CondVar_Create( &cond );
	if( ret != 0 ) {
		Sys_Error( "QCondVar_Create: failed with code %i", ret );
	}
	return cond;
}

/*
* QCondVar_Destroy
*/
void QCondVar_Destroy( qcondvar_t **pcond ) {
	assert( pcond != NULL );
	if( pcond && *pcond ) {
		Sys_CondVar_Destroy( *pcond );
		*pcond = NULL;
	}
}

/*
* QCondVar_Wait
*/
bool QCondVar_Wait( qcondvar_t *cond, qmutex_t *mutex, unsigned int timeout_msec ) {
	return Sys_CondVar_Wait( cond, mutex, timeout_msec );
}

/*
* QCondVar_Wake
*/
void QCondVar_Wake( qcondvar_t *cond ) {
	Sys_CondVar_Wake( cond );
}

/*
* QThread_Create
*/
qthread_t *QThread_Create( void *( *routine )( void* ), void *param ) {
	int ret;
	qthread_t *thread;

	ret = Sys_Thread_Create( &thread, routine, param );
	if( ret != 0 ) {
		Sys_Error( "QThread_Create: failed with code %i", ret );
	}
	return thread;
}

/*
* QThread_Join
*/
void QThread_Join( qthread_t *thread ) {
	Sys_Thread_Join( thread );
}

/*
* QThread_Yield
*/
void QThread_Yield( void ) {
	Sys_Thread_Yield();
}

// ============================================================================

struct qbufPipe_s {
	int blockWrite;
	volatile int terminated;
	unsigned write_pos;
	unsigned read_pos;
	volatile int cmdbuf_len;
	qmutex_t *cmdbuf_mutex;
	size_t bufSize;
	qcondvar_t *nonempty_condvar;
	qmutex_t *nonempty_mutex;
	char *buf;
};

/*
* QBufPipe_Create
*/
qbufPipe_t *QBufPipe_Create( size_t bufSize, int flags ) {
	qbufPipe_t *pipe = (qbufPipe_t *)malloc( sizeof( *pipe ) + bufSize );
	memset( pipe, 0, sizeof( *pipe ) );
	pipe->blockWrite = flags & 1;
	pipe->buf = (char *)( pipe + 1 );
	pipe->bufSize = bufSize;
	pipe->cmdbuf_mutex = QMutex_Create();
	pipe->nonempty_condvar = QCondVar_Create();
	pipe->nonempty_mutex = QMutex_Create();
	return pipe;
}

/*
* QBufPipe_Destroy
*/
void QBufPipe_Destroy( qbufPipe_t **ppipe ) {
	qbufPipe_t *pipe;

	assert( ppipe != NULL );
	if( !ppipe ) {
		return;
	}

	pipe = *ppipe;
	*ppipe = NULL;

	QMutex_Destroy( &pipe->cmdbuf_mutex );
	QMutex_Destroy( &pipe->nonempty_mutex );
	QCondVar_Destroy( &pipe->nonempty_condvar );
	free( pipe );
}

/*
* QBufPipe_Wake
*
* Signals the waiting thread to wake up.
*/
static void QBufPipe_Wake( qbufPipe_t *pipe ) {
	QCondVar_Wake( pipe->nonempty_condvar );
}

/*
* QBufPipe_Finish
*
* Blocks until the reader thread handles all commands
* or terminates with an error.
*/
void QBufPipe_Finish( qbufPipe_t *pipe ) {
	while( Sys_Atomic_CAS( &pipe->cmdbuf_len, 0, 0, pipe->cmdbuf_mutex ) == false && !pipe->terminated ) {
		QMutex_Lock( pipe->nonempty_mutex );
		QBufPipe_Wake( pipe );
		QMutex_Unlock( pipe->nonempty_mutex );
		QThread_Yield();
	}
}

/*
* QBufPipe_AllocCmd
*/
static void *QBufPipe_AllocCmd( qbufPipe_t *pipe, unsigned cmd_size ) {
	void *buf = &pipe->buf[pipe->write_pos];
	pipe->write_pos += cmd_size;
	return buf;
}

/*
* QBufPipe_BufLenAdd
*/
static void QBufPipe_BufLenAdd( qbufPipe_t *pipe, int val ) {
	Sys_Atomic_Add( &pipe->cmdbuf_len, val, pipe->cmdbuf_mutex );
}

struct RewindCmd final : public PipeCmd {
	[[nodiscard]]
	auto exec() -> unsigned override { return kResultRewind; }
};

static_assert( sizeof( RewindCmd ) <= PipeCmd::kAlignment );
static_assert( sizeof( RewindCmd ) <= sizeof( void * ) );
static_assert( alignof( RewindCmd ) <= PipeCmd::kAlignment );

static constexpr unsigned kMinCmdSize = PipeCmd::kAlignment;

/*
* Never allow the distance between the reader
* and the writer to grow beyond the size of the buffer.
*
* Note that there are race conditions here but in the worst case we're going
* to erroneously drop cmd's instead of stepping on the reader's toes.
*/
uint8_t *QBufPipe_AcquireWritableBytes( qbufPipe_t *pipe, unsigned bytesToAdvance ) {
	assert( !( bytesToAdvance % kMinCmdSize ) );

	if( !pipe ) [[unlikely]] {
		return nullptr;
	}

	assert( !( (uintptr_t)pipe->buf % kMinCmdSize ) );

	if( pipe->terminated ) [[unlikely]] {
		return nullptr;
	}

	assert( pipe->bufSize >= pipe->write_pos );
	if( pipe->bufSize < pipe->write_pos ) {
		pipe->write_pos = 0;
	}

	const unsigned write_remains = pipe->bufSize - pipe->write_pos;
	if( kMinCmdSize > write_remains ) {
		while( pipe->cmdbuf_len + bytesToAdvance + write_remains > pipe->bufSize ) {
			if( pipe->blockWrite ) {
				QThread_Yield();
			} else {
				return nullptr;
			}
		}

		// not enough space to enpipe even the reset cmd, rewind
		QBufPipe_BufLenAdd( pipe, write_remains ); // atomic
		pipe->write_pos = 0;
	} else if( bytesToAdvance > write_remains ) {
		while( pipe->cmdbuf_len + kMinCmdSize + bytesToAdvance + write_remains > pipe->bufSize ) {
			if( pipe->blockWrite ) {
				QThread_Yield();
			} else {
				return nullptr;
			}
		}

		// explicit pointer reset cmd
		auto *const allocatedPipeBytes = QBufPipe_AllocCmd( pipe, kMinCmdSize );
		new( allocatedPipeBytes )RewindCmd;

		QBufPipe_BufLenAdd( pipe, kMinCmdSize + write_remains ); // atomic
		pipe->write_pos = 0;
	} else {
		while( pipe->cmdbuf_len + bytesToAdvance > pipe->bufSize ) {
			if( pipe->blockWrite ) {
				QThread_Yield();
			} else {
				return nullptr;
			}
		}
	}

	return (uint8_t *)QBufPipe_AllocCmd( pipe, bytesToAdvance );
}

void QBufPipe_SubmitWrittenBytes( qbufPipe_t *pipe, unsigned bytesToSubmit ) {
	assert( !( bytesToSubmit % kMinCmdSize ) );

	QBufPipe_BufLenAdd( pipe, bytesToSubmit ); // atomic

	// wake the other thread waiting for signal
	QMutex_Lock( pipe->nonempty_mutex );
	QBufPipe_Wake( pipe );
	QMutex_Unlock( pipe->nonempty_mutex );
}

int QBufPipe_ReadCmds( qbufPipe_t *pipe ) {
	if( !pipe ) {
		return -1;
	}

	int numCmdsRead = 0;
	while( Sys_Atomic_CAS( &pipe->cmdbuf_len, 0, 0, pipe->cmdbuf_mutex ) == false && !pipe->terminated ) {
		assert( pipe->bufSize >= pipe->read_pos );
		if( pipe->bufSize < pipe->read_pos ) {
			pipe->read_pos = 0;
		}

		const int read_remains = pipe->bufSize - pipe->read_pos;
		if( read_remains < (int)kMinCmdSize ) {
			// implicit reset
			pipe->read_pos = 0;
			QBufPipe_BufLenAdd( pipe, -read_remains );
		}

		assert( ( ( (uintptr_t)( pipe->buf + pipe->read_pos ) ) % PipeCmd::kAlignment ) == 0 );

		PipeCmd *const cmd       = (PipeCmd *)( pipe->buf + pipe->read_pos );
		const unsigned cmdResult = cmd->exec();

		cmd->~PipeCmd();

		if( cmdResult == PipeCmd::kResultRewind ) {
			// this cmd is special
			pipe->read_pos = 0;
			static_assert( sizeof( RewindCmd ) <= kMinCmdSize );
			QBufPipe_BufLenAdd( pipe, -( (int)kMinCmdSize + read_remains ) ); // atomic
			continue;
		}

		numCmdsRead++;

		if( cmdResult == PipeCmd::kResultTerminate ) {
			pipe->terminated = 1;
			return -1;
		}

		if( (int)cmdResult > pipe->cmdbuf_len ) {
			assert( 0 );
			pipe->terminated = 1;
			return -1;
		}

		pipe->read_pos += cmdResult;
		QBufPipe_BufLenAdd( pipe, -( (int)cmdResult ) ); // atomic
	}

	return numCmdsRead;
}

/*
* QBufPipe_Wait
*/
void QBufPipe_Wait( qbufPipe_t *pipe, PipeWaiterFn waiterFn, unsigned timeout_msec ) {
	while( !pipe->terminated ) {
		bool timeout = false;

		while( Sys_Atomic_CAS( &pipe->cmdbuf_len, 0, 0, pipe->cmdbuf_mutex ) == true ) {
			QMutex_Lock( pipe->nonempty_mutex );

			timeout = QCondVar_Wait( pipe->nonempty_condvar, pipe->nonempty_mutex, timeout_msec ) == false;

			// don't hold the mutex, changes to cmdbuf_len are atomic anyway
			QMutex_Unlock( pipe->nonempty_mutex );
			break;
		}

		// we're guaranteed at this point that either cmdbuf_len is > 0
		// or that waiting on the condition variable has timed out
		if( waiterFn( pipe, timeout ) < 0 ) {
			// done
			return;
		}
	}
}

#ifdef CHECK_CALLING_THREAD

void CallingThreadChecker::markCurrentThreadForFurtherAccessChecks() {
	m_threadId = Sys_Thread_GetId();
}

void CallingThreadChecker::clearThreadForFurtherAccessChecks() {
	m_threadId = 0;
}

void CallingThreadChecker::checkCallingThread( const char *file, int line ) const {
	if( m_threadId ) {
		if( const uint64_t id = Sys_Thread_GetId(); id != m_threadId ) {
			// TODO: Print thread names (adding names to threads is a quite complicated topic so we ditch it for now)
			Com_Printf( "Illegal thread id %" PRIu64 ", only %" PRIu64 " is allowed to call this object", id, m_threadId );
			abort();
		}
	}
}

#endif