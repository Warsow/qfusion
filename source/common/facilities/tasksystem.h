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
#include <common/facilities/profiler.h>

class TaskSystem {
	friend struct TaskSystemImpl;
	friend class CoroTask;
	friend class TaskAwaiter;

	struct TapeCallable {
		virtual ~TapeCallable() = default;
		virtual void call( unsigned workerIndex, unsigned entryInstanceArg ) = 0;
	};

	struct TapeStateGuard {
		struct TaskSystemImpl *const impl;
		explicit TapeStateGuard( TaskSystemImpl *impl ) : impl( impl ) {
			beginTapeModification( impl );
		}
		~TapeStateGuard() {
			endTapeModification( impl, succeeded );
		}
		bool succeeded { false };
	};
public:
	struct CtorArgs {
		std::optional<wsw::ProfilingSystem::FrameGroup> profilingGroup;
		size_t numExtraThreads;
	};

	explicit TaskSystem( CtorArgs &&args );
	~TaskSystem();
	// Returns a total number of threads which may execute, including the main (std::this_thread) thread
	[[nodiscard]]
	auto getNumberOfWorkers() const -> unsigned;

	static constexpr unsigned kMaxTaskEntries = std::numeric_limits<int16_t>::max() - 1;

	enum Affinity : uint8_t { AnyThread = CoroTask::AnyThread, OnlyMainThread = CoroTask::OnlyMainThread };

	template <typename Callable>
	[[nodiscard]]
	auto add( std::initializer_list<TaskHandle> deps, Callable &&callable, Affinity affinity = AnyThread ) -> TaskHandle {
		return add( std::span<const TaskHandle> { deps.begin(), deps.end() }, std::forward<Callable>( callable ), affinity );
	}

	template <typename Callable>
	[[nodiscard]]
	auto add( std::span<const TaskHandle> deps, Callable &&callable, Affinity affinity = AnyThread ) -> TaskHandle {
		// This is a specialized replacement for std::function which should not have place in this omnipresent header
		struct TapeCallableImpl final : public TapeCallable {
			explicit TapeCallableImpl( Callable &&c ) : m_callable( std::forward<Callable>( c ) ) {}
			~TapeCallableImpl() override = default;
			void call( unsigned workerIndex, unsigned ) override {
				m_callable( workerIndex );
			}
			Callable m_callable;
		};

		[[maybe_unused]] volatile TapeStateGuard tapeStateGuard( m_impl );

		auto [mem, task] = addImpl( deps, alignof( TapeCallableImpl ), sizeof( TapeCallableImpl ), affinity );

		tapeStateGuard.succeeded = true;
		// Construct it last in nothrowing fashion
		static_assert( std::is_nothrow_move_constructible_v<Callable> );
		new( mem )TapeCallableImpl( std::forward<Callable>( callable ) );

		// The tape unlocks here
		return task;
	}

	template <typename CoroProducerFn>
	[[nodiscard]]
	auto addCoro( CoroProducerFn &&producerFn ) -> TaskHandle {
		[[maybe_unused]] volatile TapeStateGuard tapeStateGuard( m_impl );

		prepareForCoroFrameAllocation();

		CoroTask coroTask = producerFn();
		void *coroTaskMem = allocMemForCoroTask();

		tapeStateGuard.succeeded = true;

		// Move it to non-relocatable memory in a nonthrowing fashion
		static_assert( std::is_nothrow_move_constructible_v<CoroTask> );
		auto *resultCoro = new( coroTaskMem )CoroTask( std::move( coroTask ) );

		return resultCoro->m_handle.promise().m_task;
	}

	[[nodiscard]]
	auto awaiterOf( std::initializer_list<TaskHandle> tasks ) -> TaskAwaiter { return { this, { tasks.begin(), tasks.end() } }; }
	[[nodiscard]]
	auto awaiterOf( std::span<const TaskHandle> tasks ) -> TaskAwaiter { return { this, tasks }; }

	struct SingleTaskAwaiter : public TaskAwaiter {
		SingleTaskAwaiter( TaskSystem *taskSystem, TaskHandle taskHandle )
			: TaskAwaiter( taskSystem, { &m_taskHandle, 1 } ), m_taskHandle( taskHandle ) {}
		const TaskHandle m_taskHandle;
	};

	[[nodiscard]]
	auto awaiterOf( TaskHandle taskHandle ) -> SingleTaskAwaiter { return { this, taskHandle }; }

	template <typename Callable>
	[[nodiscard]]
	auto addForIndicesInRange( std::pair<unsigned, unsigned> indicesRange, std::initializer_list<TaskHandle> deps,
							   Callable &&callable, Affinity affinity = AnyThread ) -> TaskHandle {
		return addForIndicesInRange( indicesRange, deps.begin(), deps.end(), std::forward<Callable>( callable ), affinity );
	}

	template <typename Callable>
	[[nodiscard]]
	auto addForIndicesInRange( std::pair<unsigned, unsigned> indicesRange,
							   std::span<const TaskHandle> deps,
							   Callable &&callable, Affinity affinity = AnyThread ) -> TaskHandle {
		// This is a specialized replacement for std::function which should not have place in this omnipresent header
		struct TapeCallableImpl final : public TapeCallable {
			explicit TapeCallableImpl( Callable &&c ) : m_callable( std::forward<Callable>( c ) ) {}
			~TapeCallableImpl() override = default;
			void call( unsigned workerIndex, unsigned entryInstanceArg ) override {
				m_callable( workerIndex, entryInstanceArg );
			}
			Callable m_callable;
		};

		[[maybe_unused]] volatile TapeStateGuard tapeStateGuard( m_impl );

		auto [mem, task] = addForIndicesInRangeImpl( indicesRange, deps, alignof( TapeCallableImpl ),
													 sizeof( TapeCallableImpl ), affinity );

		tapeStateGuard.succeeded = true;
		// Construct it last in a nothrowing fashion
		static_assert( std::is_nothrow_move_constructible_v<Callable> );
		new( mem )TapeCallableImpl( std::forward<Callable>( callable ) );

		// The tape unlocks here
		return task;
	}

	template <typename Callable>
	[[nodiscard]]
	auto addForSubrangesInRange( std::pair<unsigned, unsigned> indicesRange, unsigned subrangeLength,
								 std::initializer_list<TaskHandle> deps,
								 Callable &&callable, Affinity affinity = AnyThread ) -> TaskHandle {
		return addForSubrangesInRange( indicesRange, subrangeLength, { deps.begin(), deps.end() },
									   std::forward<Callable>( callable ), affinity );
	}

	template <typename Callable>
	[[nodiscard]]
	auto addForSubrangesInRange( std::pair<unsigned, unsigned> indicesRange,
								 unsigned subrangeLength, std::span<const TaskHandle> deps,
								 Callable &&callable, Affinity affinity = AnyThread ) -> TaskHandle {
		// This is a specialized replacement for std::function which should not have place in this omnipresent header
		struct TapeCallableImpl final : public TapeCallable {
			explicit TapeCallableImpl( Callable &&c ) : m_callable( std::forward<Callable>( c ) ) {}
			~TapeCallableImpl() override = default;
			void call( unsigned workerIndex, unsigned entryInstanceArg ) override {
				const unsigned beginIndex = entryInstanceArg >> 16;
				const unsigned endIndex   = entryInstanceArg & 0xFFFF;
				m_callable( workerIndex, beginIndex, endIndex );
			}
			Callable m_callable;
		};

		[[maybe_unused]] volatile TapeStateGuard tapeStateGuard( m_impl );

		auto [mem, task] = addForSubrangesInRangeImpl( indicesRange, subrangeLength, deps,
													   alignof( TapeCallableImpl ), sizeof( TapeCallableImpl ), affinity );

		tapeStateGuard.succeeded = true;
		// Construct it last in a nothrowing fashion
		static_assert( std::is_nothrow_move_constructible_v<Callable> );
		new( mem )TapeCallableImpl( std::forward<Callable>( callable ) );

		// The tape unlocks here
		return task;
	}

	class ExecutionHandle {
		friend class TaskSystem;
		uintptr_t m_opaque { 0 };
	};

	// Caution! Don't think that this method is reentrant
	[[nodiscard]]
	auto startExecution( unsigned numAllowedExtraThreads = ~0u ) -> ExecutionHandle;

	[[nodiscard]] bool awaitCompletion( const ExecutionHandle &executionHandle );
private:
	enum CompletionStatus : unsigned { CompletionPending, CompletionSuccess, CompletionFailure };
	enum DependencyAddressMode : uint8_t { RangeOfEntries, RangeOfEntryHandles };

	void clear();
	void awakeWorkers( unsigned numThreadsToAwake );

	[[nodiscard]]
	auto addImpl( std::span<const TaskHandle> deps, size_t alignment, size_t size, Affinity affinity ) -> std::pair<void *, TaskHandle>;

	[[nodiscard]]
	auto addForIndicesInRangeImpl( std::pair<unsigned, unsigned> indicesRange, std::span<const TaskHandle> deps,
								   size_t alignment, size_t size, Affinity affinity ) -> std::pair<void *, TaskHandle>;

	[[nodiscard]]
	auto addForSubrangesInRangeImpl( std::pair<unsigned, unsigned> indicesRange,
									 unsigned subrangeLength, std::span<const TaskHandle> deps,
									 size_t alignment, size_t size, Affinity affinity ) -> std::pair<void *, TaskHandle>;

	[[nodiscard]]
	auto allocMemForCallable( size_t alignment, size_t size ) -> std::pair<void *, unsigned>;

	[[nodiscard]]
	auto allocMemForCoroTask() -> void *;

	[[nodiscard]]
	auto allocMemForCoroFrame( size_t size ) -> void *;

	void prepareForCoroFrameAllocation();

	[[nodiscard]]
	auto addResumeCoroTask( int tapeIndex, std::span<const TaskHandle> deps, std::coroutine_handle<CoroTask::promise_type> handle ) -> TaskHandle;

	[[nodiscard]]
	auto addRegularEntry( Affinity affinity, unsigned offsetOfCallable ) -> TaskHandle;

	auto addEntryToTape( int tapeIndex, Affinity affinity, unsigned offsetOfCallable ) -> TaskHandle;

	// A template workaround for working with implementation-private type
	template <typename Entry>
	static auto getEntryByHandle( struct TaskSystemImpl *impl, TaskHandle taskHandle ) -> Entry *;

	// Handles are allowed to point to any tape
	void addPolledDependenciesToEntry( TaskHandle taskHandle, const TaskHandle *depsBegin, const TaskHandle *depsEnd );
	// The range must point to the regular tape
	void addPushedDependentsToEntry( TaskHandle taskHandle, unsigned taskRangeBegin, unsigned taskRangeEnd );

	void setScanJump( TaskHandle taskHandle, unsigned scanJump );

	[[nodiscard]]
	auto addParallelAndJoinEntries( Affinity affinity, unsigned offsetOfCallable, unsigned numTasks )
		-> std::pair<std::pair<unsigned, unsigned>, TaskHandle>;

	void setupIotaInstanceArgs( std::pair<unsigned, unsigned> taskRange, unsigned startValue );
	void setupRangeInstanceArgs( std::pair<unsigned, unsigned> taskRange, unsigned startValue,
								 unsigned subrangeLength, unsigned totalWorkload );

	static void beginTapeModification( struct TaskSystemImpl * );
	static void endTapeModification( struct TaskSystemImpl *, bool succeeded );

	static void threadLoopFunc( struct TaskSystemImpl *impl, unsigned threadNumber,
								std::optional<wsw::ProfilingSystem::FrameGroup> profilingGroup );
	[[nodiscard]]
	static bool threadExecTasks( struct TaskSystemImpl *impl, unsigned threadNumber );

	struct TaskSystemImpl *m_impl;
};

#endif