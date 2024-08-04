/*
Copyright (C) 2024 Chasseur de bots

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

#ifndef WSW_1592e913_d661_4bb3_bab4_1e7b665d1c0c_H
#define WSW_1592e913_d661_4bb3_bab4_1e7b665d1c0c_H

#include "taskhandle.h"

#include <cassert>
#include <cstdlib>
#include <utility>

class TaskSystem {
	friend struct TaskSystemImpl;

	struct CallableStorage {
		virtual ~CallableStorage() = default;
		virtual void moveSelfTo( void *mem ) = 0;
		virtual void call( unsigned workerIndex ) = 0;
	};
public:
	struct CtorArgs { size_t numExtraThreads; };
	explicit TaskSystem( CtorArgs &&args );
	~TaskSystem();
	// Returns a total number of threads which may execute, including the main (std::this_thread) thread
	[[nodiscard]]
	auto getNumberOfWorkers() const -> unsigned;

	enum Affinity : unsigned { AnyThread, OnlyThisThread };

	template <typename Callable>
	[[nodiscard]]
	auto add( Callable &&callable, std::initializer_list<TaskHandle> dependencies, Affinity affinity = AnyThread ) -> TaskHandle {
		return add( std::forward<Callable>( callable ), dependencies.begin(), dependencies.end(), affinity );
	}

	template <typename Callable>
	[[nodiscard]]
	auto add( Callable &&callable, const TaskHandle *dependenciesBegin, const TaskHandle *dependenciesEnd, Affinity affinity = AnyThread ) -> TaskHandle {
		// This is a specialized replacement for std::function which should not have place in this omnipresent header
		struct CallableStorageImpl final : public CallableStorage {
			CallableStorageImpl() = default;
			CallableStorageImpl( Callable &&c ) {
				new( m_buffer )Callable( std::forward<Callable>( c ) );
				m_hasValue = true;
			}
			~CallableStorageImpl() override {
				if( m_hasValue ) {
					( (Callable *)m_buffer )->~Callable();
				}
			}
			void moveSelfTo( void *mem ) override {
				auto *that = new( mem )CallableStorageImpl;
				assert( m_hasValue );
				auto *thisCallable = (Callable *)m_buffer;
				new( that->m_buffer )Callable( std::move( *thisCallable ) );
				that->m_hasValue = true;
				this->m_hasValue = false;
			}
			void call( unsigned workerIndex ) override {
				auto &thisCallable = *( (Callable *)m_buffer );
				thisCallable( workerIndex );
			}

			alignas( alignof( Callable ) )uint8_t m_buffer[sizeof( Callable )];
			bool m_hasValue { false };
		};

		// TODO: Use some kind of generalized scope guard
		struct Unlock {
			explicit Unlock( TaskSystemImpl *impl ) : m_impl( impl ) {}
			~Unlock() { releaseLock( m_impl ); }
			TaskSystemImpl *m_impl;
		};

		acquireLock( m_impl );

		[[maybe_unused]] volatile Unlock lock { m_impl };
		constexpr size_t alignment = alignof( CallableStorageImpl );
		constexpr size_t size      = sizeof( CallableStorageImpl );
		auto [mem, taskHandle]     = addEntryAndAllocCallableMem( affinity, dependenciesBegin, dependenciesEnd, alignment, size );
		assert( ( (uintptr_t)mem % alignment ) == 0 );
		new( mem )CallableStorageImpl( std::forward<Callable>( callable ) );

		return taskHandle;
	}

	void startExecution();
	[[nodiscard]] bool awaitCompletion();
private:
	enum CompletionStatus : unsigned { CompletionPending, CompletionSuccess, CompletionFailure };

	void clear();
	void awakeWorkers();

	[[nodiscard]]
	auto addEntryAndAllocCallableMem( Affinity affinity, const TaskHandle *dependenciesBegin, const TaskHandle *dependenciesEnd,
									  size_t alignment, size_t size ) -> std::pair<void *, TaskHandle>;

	static void acquireLock( struct TaskSystemImpl * );
	static void releaseLock( struct TaskSystemImpl * );

	static void threadLoopFunc( struct TaskSystemImpl *impl, unsigned threadNumber );
	[[nodiscard]]
	static bool threadExecTasks( struct TaskSystemImpl *impl, unsigned threadNumber );

	struct TaskSystemImpl *m_impl;
};

#endif