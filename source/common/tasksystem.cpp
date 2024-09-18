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

#include "tasksystem.h"
#include "wswpodvector.h"
#include "wswalgorithm.h"
#include "qthreads.h"

#include <atomic>
#include <cassert>
#include <thread>
#include <vector>
#include <deque>
#include <optional>
#include <variant>
#include <span>
#include <barrier>

struct TaskSystemImpl {
	explicit TaskSystemImpl( unsigned numExtraThreads );
	~TaskSystemImpl();

	// Share it here as the impl has an access to private types
	struct ResumeCoroCallable final : public TaskSystem::TapeCallable {
		explicit ResumeCoroCallable( std::coroutine_handle<CoroTask::promise_type> handle ) : m_handle( handle ) {}
		void call( unsigned, unsigned ) override { m_handle.resume(); }
		std::coroutine_handle<CoroTask::promise_type> m_handle;
	};

	struct TaskEntry {
		enum Status : uint8_t { Pending, Busy, Completed };

		Status status { Pending };
		TaskSystem::DependencyAddressMode pushedDependentsAddressMode { TaskSystem::RangeOfEntries };
		TaskSystem::Affinity affinity { TaskSystem::AnyThread };

		// Let it crash if not set (~0u is a special value)
		unsigned offsetOfCallable { ~0u / 2 };

		volatile bool *dynamicCompletionStatusAddress { nullptr };

		unsigned extraScanJump { 0 };

		unsigned startOfRegularPolledDependencies { 0 }, endOfRegularPolledDependencies { 0 };
		unsigned startOfSignaledPolledDependencies { 0 }, endOfSignaledPolledDependencies { 0 };

		unsigned startOfPushedDependents { 0 }, endOfPushedDependents { 0 };

		unsigned numSatisfiedPushDependencies { 0 }, numTotalPushDependencies { 0 };

		unsigned instanceArg { 0 };
	};

	// In bytes
	static constexpr size_t kCapacityOfMemOfCallables = 256 * 1024;
	static constexpr size_t kCapacityOfMemOfCoroTasks = 128 * 1024;
	static constexpr size_t kMaxTaskEntries           = TaskSystem::kMaxTaskEntries;
	// Let us assume the average number of dependencies to be 16
	static constexpr size_t kMaxDependencyIndices     = 16 * kMaxTaskEntries;

	// Sanity checks
	// Note: We have decided that introduction of another tape should not lead to updating limits, as that tape is small
	static_assert( sizeof( TaskEntry ) * kMaxTaskEntries < 2 * 1024 * 1024 );
	static_assert( sizeof( uint16_t ) * kMaxDependencyIndices < 1024 * 1024 );

	// Note: All entries which get submitted between TaskSystem::startExecution() and TaskSystem::awaitCompletion()
	// stay in the same region of memory without relocation.

	// A separate storage for large callables.
	// Callables and task entries may reside in the same buffer, but it (as a late addition) complicates existing code.
	// Preventing use of std::function<> in public API is primary reason of using custom allocation of callables.
	std::unique_ptr<uint8_t[]> memOfCallables { new uint8_t[kCapacityOfMemOfCallables] };
	// Using a raw memory chunk for task entries (they aren't trivially constructible, hence they get created on demand)
	std::unique_ptr<uint8_t[]> memOfRegularTaskEntries { new uint8_t[sizeof( TaskEntry ) * kMaxTaskEntries] };
	// Ensure that it's signed so it can store opaque handles of tasks which are signed without hassle
	std::unique_ptr<int16_t[]> dependencyHandles { new int16_t[kMaxDependencyIndices] };

	std::unique_ptr<uint8_t[]> memOfCoroTasks { new uint8_t[kCapacityOfMemOfCoroTasks] };

	struct Tape {
		std::unique_ptr<uint8_t[]> memOfEntries { new uint8_t[sizeof( TaskEntry ) * kMaxTaskEntries] };
		unsigned startScanFrom { 0 };
		unsigned numEntriesSoFar { 0 };
	};

	Tape tapes[2];

	// In bytes
	unsigned sizeOfUsedMemOfCallables { 0 };
	unsigned numDependencyEntriesSoFar { 0 };

	unsigned savedSizeOfUsedMemOfCallables { 0 };
	unsigned savedNumDependencyEntriesSoFar { 0 };
	unsigned savedNumEntriesInTapesSoFar[2] { 0, 0 };

	unsigned sizeOfUsedMemOfCoroTasks { 0 };

	wsw::PodVector<uint16_t> tmpOffsetsOfCallables;

	std::barrier<decltype( []() noexcept {} )> barrier;

	std::vector<std::jthread> threads;
	std::deque<std::atomic_flag> signalingFlags;
	std::deque<std::atomic<unsigned>> completionStatuses;
	wsw::Mutex globalMutex;
	std::atomic<bool> isExecuting { false };
	std::atomic<bool> isShuttingDown { false };
	std::atomic<bool> awaitsCompletion { false };
};

TaskSystemImpl::TaskSystemImpl( unsigned numExtraThreads ) : barrier( numExtraThreads + 1 ) {
	signalingFlags.resize( numExtraThreads );
	completionStatuses.resize( numExtraThreads );

	// Spawn threads once we're done with vars
	for( unsigned threadNumber = 0; threadNumber < numExtraThreads; ++threadNumber ) {
		threads.emplace_back( std::jthread( TaskSystem::threadLoopFunc, this, threadNumber ) );
	}
}

TaskSystemImpl::~TaskSystemImpl() {
	// Make sure the parent system properly calls clear()
	assert( sizeOfUsedMemOfCallables == 0 );
}

TaskSystem::TaskSystem( CtorArgs &&args ) {
	m_impl = new TaskSystemImpl( args.numExtraThreads );
}

TaskSystem::~TaskSystem() {
	m_impl->isShuttingDown.store( true, std::memory_order_seq_cst );
	awakeWorkers();
	for( std::jthread &thread: m_impl->threads ) {
		thread.join();
	}
	clear();
	delete m_impl;
}

auto TaskSystem::getNumberOfWorkers() const -> unsigned {
	return m_impl->threads.size() + 1;
}

void TaskSystem::beginTapeModification( TaskSystemImpl *impl ) {
	impl->globalMutex.lock();
	impl->savedSizeOfUsedMemOfCallables  = impl->sizeOfUsedMemOfCallables;
	impl->savedNumDependencyEntriesSoFar = impl->numDependencyEntriesSoFar;
	impl->savedNumEntriesInTapesSoFar[0] = impl->tapes[0].numEntriesSoFar;
	impl->savedNumEntriesInTapesSoFar[1] = impl->tapes[1].numEntriesSoFar;
}

void TaskSystem::endTapeModification( TaskSystemImpl *impl, bool succeeded ) {
	if( !succeeded ) [[unlikely]] {
		// Callables must be constructed in a nonthrowing fashion, hence we just reset the offset
		impl->sizeOfUsedMemOfCallables  = impl->savedSizeOfUsedMemOfCallables;
		impl->numDependencyEntriesSoFar = impl->savedNumDependencyEntriesSoFar;
		impl->tapes[0].numEntriesSoFar  = impl->tapes[0].numEntriesSoFar;
		impl->tapes[1].numEntriesSoFar  = impl->tapes[1].numEntriesSoFar;
	}
	impl->globalMutex.unlock();
}

auto TaskSystem::allocMemForCoroTask() -> void * {
	// May happen in runtime. This has to be handled.
	if( m_impl->sizeOfUsedMemOfCoroTasks + sizeof( CoroTask ) > TaskSystemImpl::kCapacityOfMemOfCoroTasks ) {
		wsw::failWithRuntimeError( "The storage of coro tasks has been exhausted" );
	}
	void *result = m_impl->memOfCoroTasks.get() + m_impl->sizeOfUsedMemOfCoroTasks;
	assert( ( (uintptr_t)result % alignof( CoroTask ) ) == 0 );
	m_impl->sizeOfUsedMemOfCoroTasks += sizeof( CoroTask );
	return result;
}

auto TaskSystem::addResumeCoroTask( int tapeIndex, std::span<const TaskHandle> deps,
									std::coroutine_handle<CoroTask::promise_type> handle ) -> TaskHandle {
	auto [memForCallable, offsetOfCallable] = allocMemForCallable( alignof( TaskSystemImpl::ResumeCoroCallable ),
																   sizeof( TaskSystemImpl::ResumeCoroCallable ) );

	const auto affinity = (TaskSystem::Affinity)handle.promise().m_startInfo.affinity;

	TaskHandle result = addEntryToTape( tapeIndex, affinity, offsetOfCallable );
	addPolledDependenciesToEntry( result, deps.data(), deps.data() + deps.size() );

	// TODO: Won't be rolled back on failure if enclosing state guard rolls back,
	// but it seems to be harmless (not talking about being considered unreachable in practice)
	new( memForCallable )TaskSystemImpl::ResumeCoroCallable( handle );
	return result;
}

void CoroTask::promise_type::InitialSuspend::await_suspend( std::coroutine_handle<promise_type> h ) const noexcept {
	// Assumes the enclosing non-reentrant lock is held
	TaskHandle task = m_taskSystem->addResumeCoroTask( 1, h.promise().m_startInfo.initialDependencies, h );
	h.promise().m_task = task;

	auto *entry = TaskSystem::getEntryByHandle<TaskSystemImpl::TaskEntry>( m_taskSystem->m_impl, task );

	entry->dynamicCompletionStatusAddress = &h.promise().m_completed;
	assert( entry->status == TaskSystemImpl::TaskEntry::Pending );
	assert( !*entry->dynamicCompletionStatusAddress );
}

void TaskAwaiter::await_suspend( std::coroutine_handle<> h ) const {
	// Like regular add(), but uses the optimized callable
	auto typedHandle = std::coroutine_handle<CoroTask::promise_type>::from_address( h.address() );
	[[maybe_unused]] volatile TaskSystem::TapeStateGuard tapeStateGuard( m_taskSystem->m_impl );
	(void)m_taskSystem->addResumeCoroTask( 0, m_dependencies, typedHandle );
	tapeStateGuard.succeeded = true;
}

auto TaskSystem::allocMemForCallable( size_t alignment, size_t size ) -> std::pair<void *, unsigned> {
	assert( alignment && size );

	uintptr_t alignmentBytes   = 0;
	uintptr_t actualTopAddress = (uintptr_t)m_impl->memOfCallables.get() + m_impl->sizeOfUsedMemOfCallables;
	if( !( alignment & ( alignment - 1 ) ) ) [[likely]] {
		// TODO: Can be branchless
		if( auto rem = actualTopAddress & ( alignment - 1 ) ) {
			alignmentBytes += alignment - rem;
		}
	} else {
		if( auto rem = actualTopAddress % alignment ) {
			alignmentBytes += alignment - rem;
		}
	}

	// May happen in runtime. This has to be handled.
	if( m_impl->sizeOfUsedMemOfCallables + size + alignmentBytes > TaskSystemImpl::kCapacityOfMemOfCallables ) {
		wsw::failWithRuntimeError( "The storage of callables has been exhausted" );
	}

	const unsigned offsetOfCallable = m_impl->sizeOfUsedMemOfCallables + alignmentBytes;
	m_impl->sizeOfUsedMemOfCallables += size + alignmentBytes;

	void *const memForCallable = m_impl->memOfCallables.get() + offsetOfCallable;
	assert( ( (uintptr_t)memForCallable % alignment ) == 0 );
	return { memForCallable, offsetOfCallable };
}

auto TaskSystem::addRegularEntry( Affinity affinity, unsigned offsetOfCallable ) -> TaskHandle {
	return addEntryToTape( 0, affinity, offsetOfCallable );
}

auto TaskSystem::addEntryToTape( int tapeIndex, Affinity affinity, unsigned offsetOfCallable ) -> TaskHandle {
	assert( tapeIndex == 0 || tapeIndex == 1 );
	TaskSystemImpl::Tape *tape = &m_impl->tapes[tapeIndex];
	const int handleSign       = tapeIndex != 0 ? -1 : +1;

	// May happen in runtime. This has to be handled.
	if( tape->numEntriesSoFar + 1 >= TaskSystemImpl::kMaxTaskEntries ) [[unlikely]] {
		wsw::failWithRuntimeError( "Too many task entries" );
	}

	void *taskEntryMem = tape->memOfEntries.get() + sizeof( TaskSystemImpl::TaskEntry ) * tape->numEntriesSoFar;

	[[maybe_unused]] auto *entry = new( taskEntryMem )TaskSystemImpl::TaskEntry {
		.affinity         = affinity,
		.offsetOfCallable = offsetOfCallable,
	};
	tape->numEntriesSoFar++;

	TaskHandle resultHandle;
	static_assert( std::is_signed_v<decltype( TaskHandle::m_opaque )> );
	resultHandle.m_opaque = ( (decltype( TaskHandle::m_opaque ))tape->numEntriesSoFar ) * handleSign;
	return resultHandle;
}

template <typename Entry>
auto TaskSystem::getEntryByHandle( TaskSystemImpl *impl, TaskHandle taskHandle ) -> Entry * {
	TaskSystemImpl::Tape *tape;
	size_t index;
	if( taskHandle.m_opaque > 0 ) [[likely]] {
		index = (size_t)( taskHandle.m_opaque - 1 );
		tape  = &impl->tapes[0];
	} else if( taskHandle.m_opaque < 0 ) [[likely]] {
		index = (size_t)( ( -taskHandle.m_opaque ) - 1 );
		tape  = &impl->tapes[1];
	} else {
		wsw::failWithRuntimeError( "Attempt to get an entry by a null handle" );
	}
	assert( index < tape->numEntriesSoFar );
	return (TaskSystemImpl::TaskEntry *)tape->memOfEntries.get() + index;
}

void TaskSystem::addPolledDependenciesToEntry( TaskHandle taskHandle, const TaskHandle *depsBegin, const TaskHandle *depsEnd ) {
	assert( depsBegin <= depsEnd );

	// May happen in runtime. This has to be handled.
	const size_t newNumDependencyEntries = m_impl->numDependencyEntriesSoFar + ( depsEnd - depsBegin );
	if( newNumDependencyEntries > (size_t)TaskSystemImpl::kMaxDependencyIndices ) [[unlikely]] {
		wsw::failWithRuntimeError( "Too many dependencies" );
	}

	int16_t *const startOfRegularDependencies = m_impl->dependencyHandles.get() + m_impl->numDependencyEntriesSoFar;
	int16_t *const endOfSignaledDependencies  = startOfRegularDependencies + ( depsEnd - depsBegin );

	int16_t *regularDependenciesCursor  = startOfRegularDependencies;
	int16_t *signaledDependenciesCursor = endOfSignaledDependencies;

	const auto offsetOfRegularDependencies = (unsigned)m_impl->numDependencyEntriesSoFar;
	for( const TaskHandle *dependency = depsBegin; dependency < depsEnd; ++dependency ) {
		// It actually checks using internal assertions
		assert( getEntryByHandle<TaskSystemImpl::TaskEntry>( m_impl, *dependency ) );
		if( dependency->m_opaque > 0 ) [[likely]] {
			*regularDependenciesCursor = (int16_t)dependency->m_opaque;
			regularDependenciesCursor++;
		} else {
			signaledDependenciesCursor--;
			*signaledDependenciesCursor = (int16_t)dependency->m_opaque;
		}
	}

	const auto numRegularDependencies  = regularDependenciesCursor - startOfRegularDependencies;
	const auto numSignaledDependencies = endOfSignaledDependencies - signaledDependenciesCursor;
	assert( numRegularDependencies + numSignaledDependencies == depsEnd - depsBegin );
	assert( regularDependenciesCursor == signaledDependenciesCursor );
	m_impl->numDependencyEntriesSoFar += numRegularDependencies + numSignaledDependencies;

	auto *const entry = getEntryByHandle<TaskSystemImpl::TaskEntry>( m_impl, taskHandle );

	entry->startOfRegularPolledDependencies  = offsetOfRegularDependencies;
	entry->endOfRegularPolledDependencies    = offsetOfRegularDependencies + numRegularDependencies;
	entry->startOfSignaledPolledDependencies = offsetOfRegularDependencies + numRegularDependencies;
	entry->endOfSignaledPolledDependencies   = offsetOfRegularDependencies + numRegularDependencies + numSignaledDependencies;
}

void TaskSystem::addPushedDependentsToEntry( TaskHandle taskHandle, unsigned taskRangeBegin, unsigned taskRangeEnd ) {
	assert( taskRangeBegin <= taskRangeEnd );

	auto *const taskMemBase = (TaskSystemImpl::TaskEntry *)m_impl->tapes[0].memOfEntries.get();

	auto *const entry = getEntryByHandle<TaskSystemImpl::TaskEntry>( m_impl, taskHandle );
	entry->startOfPushedDependents     = taskRangeBegin;
	entry->endOfPushedDependents       = taskRangeEnd;
	entry->pushedDependentsAddressMode = DependencyAddressMode::RangeOfEntries;

	// Add an expected push dependency to each task in the range
	for( unsigned taskIndex = taskRangeBegin; taskIndex < taskRangeEnd; ++taskIndex ) {
		taskMemBase[taskIndex].numTotalPushDependencies++;
	}
}

void TaskSystem::setScanJump( TaskHandle taskHandle, unsigned scanJump ) {
	// Make sure it's a regular entry
	assert( taskHandle.m_opaque > 0 );
	auto *const entry = (TaskSystemImpl::TaskEntry *)m_impl->tapes[0].memOfEntries.get() + ( taskHandle.m_opaque - 1 );
	entry->extraScanJump = scanJump;
}

[[nodiscard]]
auto TaskSystem::addParallelAndJoinEntries( Affinity affinity, unsigned offsetOfCallable, unsigned numTasks )
	-> std::pair<std::pair<unsigned, unsigned>, TaskHandle> {
	TaskSystemImpl::Tape *const tape = &m_impl->tapes[0];

	// May happen in runtime. This has to be handled.
	if( tape->numEntriesSoFar + numTasks + 1 >= TaskSystemImpl::kMaxTaskEntries ) [[unlikely]] {
		wsw::failWithRuntimeError( "Too many task entries" );
	}

	const unsigned parRangeBegin = tape->numEntriesSoFar;
	const unsigned parRangeEnd   = tape->numEntriesSoFar + numTasks;

	auto *const taskMemBase = (TaskSystemImpl::TaskEntry *)tape->memOfEntries.get();
	for( unsigned parIndex = parRangeBegin; parIndex < parRangeEnd; ++parIndex ) {
		new( taskMemBase + parIndex )TaskSystemImpl::TaskEntry {
			.pushedDependentsAddressMode = DependencyAddressMode::RangeOfEntries,
			.affinity                    = affinity,
			.offsetOfCallable            = offsetOfCallable,
			.startOfPushedDependents     = parRangeEnd,
			.endOfPushedDependents       = parRangeEnd + 1,
		};
	}

	new( taskMemBase + parRangeEnd )TaskSystemImpl::TaskEntry {
		.affinity                 = affinity,
		.offsetOfCallable         = ~0u,
		.numTotalPushDependencies = numTasks,
	};

	tape->numEntriesSoFar += numTasks + 1;

	TaskHandle resultHandle;
	resultHandle.m_opaque = tape->numEntriesSoFar;
	return { { parRangeBegin, parRangeEnd }, resultHandle };
}

void TaskSystem::setupIotaInstanceArgs( std::pair<unsigned, unsigned> taskRange, unsigned startValue ) {
	TaskSystemImpl::Tape *const tape = &m_impl->tapes[0];
	const auto [taskRangeBegin, taskRangeEnd] = taskRange;
	assert( taskRangeBegin < taskRangeEnd && taskRangeEnd <= tape->numEntriesSoFar );

	for( unsigned taskIndex = taskRangeBegin; taskIndex < taskRangeEnd; ++taskIndex ) {
		auto *entry = (TaskSystemImpl::TaskEntry *)tape->memOfEntries.get() + taskIndex;
		entry->instanceArg = startValue + ( taskIndex - taskRangeBegin );
	}
}

void TaskSystem::setupRangeInstanceArgs( std::pair<unsigned, unsigned> taskRange, unsigned startValue,
										 unsigned subrangeLength, unsigned totalWorkload ) {
	TaskSystemImpl::Tape *const tape = &m_impl->tapes[0];
	const auto [taskRangeBegin, taskRangeEnd] = taskRange;
	assert( taskRangeBegin < taskRangeEnd && taskRangeEnd <= tape->numEntriesSoFar );
	assert( taskRangeEnd - taskRangeBegin <= totalWorkload );

	[[maybe_unused]] unsigned workloadLeft = totalWorkload;

	unsigned workloadStart = startValue;
	for( unsigned taskIndex = taskRangeBegin; taskIndex < taskRangeEnd; ++taskIndex ) {
		unsigned workloadEnd = workloadStart + subrangeLength;
		if( workloadEnd > totalWorkload + startValue ) [[unlikely]] {
			assert( taskIndex + 1 == taskRangeEnd );
			workloadEnd = totalWorkload + startValue;
		}

		auto *entry = (TaskSystemImpl::TaskEntry *)tape->memOfEntries.get() + taskIndex;
		assert( workloadStart < workloadEnd && workloadEnd <= 0xFFFFu );
		entry->instanceArg = ( workloadStart << 16 ) | workloadEnd;

		workloadLeft -= ( workloadEnd - workloadStart );
		workloadStart = workloadEnd;
	}

	assert( workloadLeft == 0 );
}

auto TaskSystem::addImpl( std::span<const TaskHandle> deps, size_t alignment, size_t size, Affinity affinity )
	-> std::pair<void *, TaskHandle> {
	auto [memForCallable, offsetOfCallable] = allocMemForCallable( alignment, size );

	TaskHandle result = addRegularEntry( affinity, offsetOfCallable );
	addPolledDependenciesToEntry( result, deps.data(), deps.data() + deps.size() );

	return { memForCallable, result };
}

auto TaskSystem::addForIndicesInRangeImpl( std::pair<unsigned int, unsigned int> indicesRange,
										   std::span<const TaskHandle> deps,
										   size_t alignment, size_t size, Affinity affinity )
	-> std::pair<void *, TaskHandle> {
	auto [indicesBegin, indicesEnd] = indicesRange;
	assert( indicesBegin <= indicesEnd );

	auto [memForCallable, offsetOfCallable] = allocMemForCallable( alignment, size );

	TaskHandle forkTask = addRegularEntry( affinity, ~0u );
	// Launches when all tasks in range [deps.begin(), deps.end()) are completed
	addPolledDependenciesToEntry( forkTask, deps.data(), deps.data() + deps.size() );

	// This call adds dependencies between parallel and join tasks as well
	auto [rangeOfParallelTasks, joinTask] = addParallelAndJoinEntries( affinity, offsetOfCallable, ( indicesEnd - indicesBegin ) );
	// Make parallel tasks depend on the fork task
	addPushedDependentsToEntry( forkTask, rangeOfParallelTasks.first, rangeOfParallelTasks.second );
	// If the forkTask has unsatisfied dependencies, allow jumping over its subsequent dependents
	setScanJump( forkTask, indicesEnd - indicesBegin );

	setupIotaInstanceArgs( rangeOfParallelTasks, indicesBegin );

	return { memForCallable, joinTask };
}

auto TaskSystem::addForSubrangesInRangeImpl( std::pair<unsigned, unsigned> indicesRange,
											 unsigned subrangeLength, std::span<const TaskHandle> deps,
											 size_t alignment, size_t size, Affinity affinity )
	-> std::pair<void *, TaskHandle> {
	assert( subrangeLength != 0 );
	const auto [indicesBegin, indicesEnd] = indicesRange;
	assert( indicesBegin <= indicesEnd );

	const auto workload = indicesEnd - indicesBegin;
	unsigned numTasks   = workload / subrangeLength;
	if( workload % subrangeLength ) {
		numTasks++;
	}

	auto [memForCallable, offsetOfCallable] = allocMemForCallable( alignment, size );

	TaskHandle forkTask = addRegularEntry( affinity, ~0u );
	// Launches when all tasks in range [deps.begin(), deps.end()) are completed
	addPolledDependenciesToEntry( forkTask, deps.data(), deps.data() + deps.size() );

	// This call adds dependencies between parallel and join tasks as well
	auto [rangeOfParallelTasks, joinTask] = addParallelAndJoinEntries( affinity, offsetOfCallable, numTasks );
	// Make parallel tasks depend on the fork task
	addPushedDependentsToEntry( forkTask, rangeOfParallelTasks.first, rangeOfParallelTasks.second );
	// If the forkTask has unsatisfied dependencies, allow jumping over its subsequent dependents
	setScanJump( forkTask, indicesEnd - indicesBegin );

	setupRangeInstanceArgs( rangeOfParallelTasks, indicesBegin, subrangeLength, workload );

	return { memForCallable, joinTask };
}

void TaskSystem::clear() {
	assert( !m_impl->isExecuting );

	// Callables may be shared for multiple task entries.
	// Tracking references to callables is way too bothersome.
	// Just prevent duplicated destructor calls which lead to crashing.
	m_impl->tmpOffsetsOfCallables.clear();

	// These are optimizations to the naive implementation.
	unsigned offsetOfLastDestroyedCallable     = ~0u;
	unsigned maxOffsetOfDestroyedCallableSoFar = 0;

	// Callables aren't trivially destructible
	// TODO: They can perfectly be in some cases (track this status)
	for( TaskSystemImpl::Tape &tape: m_impl->tapes ) {
		for( unsigned taskIndex = 0; taskIndex < tape.numEntriesSoFar; ++taskIndex ) {
			const unsigned offsetOfCallable = ( (TaskSystemImpl::TaskEntry *)tape.memOfEntries.get() )[taskIndex].offsetOfCallable;
			if( offsetOfCallable != ~0u ) {
				bool shouldDestroyIt = false;
				// If the list of processed offsets definitely does not contain the current offset
				if( maxOffsetOfDestroyedCallableSoFar < offsetOfCallable ) {
					maxOffsetOfDestroyedCallableSoFar = offsetOfCallable;
					shouldDestroyIt = true;
				} else {
					// If it is not the last destroyed callable (this is an optimization for sequence of tasks with the same callable)
					if( offsetOfLastDestroyedCallable != offsetOfCallable ) [[unlikely]] {
						// Perform a generic check using the slow path
						if( !wsw::contains( m_impl->tmpOffsetsOfCallables, offsetOfCallable ) ) {
							shouldDestroyIt = true;
						}
					}
				}

				if( shouldDestroyIt ) {
					assert( !wsw::contains( m_impl->tmpOffsetsOfCallables, offsetOfCallable ) );
					m_impl->tmpOffsetsOfCallables.push_back( offsetOfCallable );
					offsetOfLastDestroyedCallable = offsetOfCallable;

					assert( offsetOfCallable < m_impl->sizeOfUsedMemOfCallables );
					auto *callable = (TapeCallable *)( m_impl->memOfCallables.get() + offsetOfCallable );
					callable->~TapeCallable();
				}
			}
		}
		// Make sure we can just set count to zero for task entries
		static_assert( std::is_trivially_destructible_v<TaskSystemImpl::TaskEntry> );
		tape.numEntriesSoFar = 0;
		tape.startScanFrom   = 0;
	}

	m_impl->sizeOfUsedMemOfCallables  = 0;

	for( unsigned offset = 0; offset < m_impl->sizeOfUsedMemOfCoroTasks; offset += sizeof( CoroTask ) ) {
		( (CoroTask *)( m_impl->memOfCoroTasks.get() + offset ) )->~CoroTask();
	}

	m_impl->sizeOfUsedMemOfCoroTasks  = 0;
	m_impl->numDependencyEntriesSoFar = 0;
}

void TaskSystem::awakeWorkers() {
	// Awake all threads
	// TODO: Is there something smarter
	for( std::atomic_flag &flag: m_impl->signalingFlags ) {
		flag.test_and_set();
		flag.notify_one();
	}
}

void TaskSystem::startExecution() {
	assert( !m_impl->isExecuting );
	clear();

	std::fill( m_impl->completionStatuses.begin(), m_impl->completionStatuses.end(), CompletionPending );

	m_impl->isExecuting.store( true, std::memory_order_seq_cst );

	awakeWorkers();
}

bool TaskSystem::awaitCompletion() {
	assert( m_impl->isExecuting );

	// Run the part of workload in this thread as well
	bool succeeded = threadExecTasks( m_impl, ~0u );

	// Interrupt workers which may spin on this variable
	m_impl->awaitsCompletion.store( true, std::memory_order_seq_cst );

	// Await completion reply from workers
	m_impl->barrier.arrive_and_wait();

	if( succeeded ) {
		auto it = m_impl->completionStatuses.begin();
		for(; it != m_impl->completionStatuses.end(); ++it ) {
			const unsigned status = (*it).load( std::memory_order_relaxed );
			assert( status != CompletionPending );
			if( status != CompletionSuccess ) {
				succeeded = false;
				break;
			}
		}
	}

	m_impl->awaitsCompletion.store( false, std::memory_order_seq_cst );
	m_impl->isExecuting.store( false, std::memory_order_seq_cst );

	clear();

	return succeeded;
}

void TaskSystem::threadLoopFunc( TaskSystemImpl *__restrict impl, unsigned threadNumber ) {
	for(;; ) {
		impl->signalingFlags[threadNumber].wait( false );
		impl->signalingFlags[threadNumber].clear();

		if( impl->isShuttingDown.load( std::memory_order_seq_cst ) ) {
			break;
		}

		const auto status = threadExecTasks( impl, threadNumber ) ? CompletionSuccess : CompletionFailure;
		impl->completionStatuses[threadNumber].store( status, std::memory_order_seq_cst );
		(void)impl->barrier.arrive( 1 );
	}
}

bool TaskSystem::threadExecTasks( TaskSystemImpl *__restrict impl, unsigned threadNumber ) {
	assert( impl->isExecuting );

	const auto completeEntry = [=]( TaskSystemImpl::TaskEntry &entry ) -> void {
		if( entry.startOfPushedDependents < entry.endOfPushedDependents ) {
			unsigned dependentIndex = entry.startOfPushedDependents;
			if( entry.pushedDependentsAddressMode == DependencyAddressMode::RangeOfEntries ) {
				// This mode is only valid for dependencies from the regular tape
				auto *entries = (TaskSystemImpl::TaskEntry *)impl->tapes[0].memOfEntries.get();
				// Safe under mutex
				do {
					auto &dependentEntry = entries[dependentIndex];
					dependentEntry.numSatisfiedPushDependencies++;
					assert( dependentEntry.numSatisfiedPushDependencies <= dependentEntry.numTotalPushDependencies );
				} while( ++dependentIndex < entry.endOfPushedDependents );
			} else {
				const auto *dependencyHandles = impl->dependencyHandles.get();
				do {
					const TaskHandle dependentHandle { dependencyHandles[dependentIndex] };
					auto &dependentEntry = *getEntryByHandle<TaskSystemImpl::TaskEntry>( impl, dependentHandle );
					dependentEntry.numSatisfiedPushDependencies++;
					assert( dependentEntry.numSatisfiedPushDependencies <= dependentEntry.numTotalPushDependencies );
				} while( ++dependentIndex < entry.endOfPushedDependents );
			}
		}
		assert( !entry.dynamicCompletionStatusAddress || *entry.dynamicCompletionStatusAddress == true );
		// Safe under mutex.
		entry.status = TaskSystemImpl::TaskEntry::Completed;
	};

	const auto checkSatisfiedDependencies = [=]( const TaskSystemImpl::TaskEntry &__restrict entry ) {
		// TODO: These checks can be optimized by tracking indices/masks of already completed dependencies
		if( entry.startOfRegularPolledDependencies < entry.endOfRegularPolledDependencies ) {
			const int16_t *__restrict handles = impl->dependencyHandles.get();
			const auto *__restrict entries    = (TaskSystemImpl::TaskEntry *)impl->tapes[0].memOfEntries.get();
			unsigned dependencyIndex          = entry.startOfRegularPolledDependencies;
			do {
				auto &__restrict dependentEntry = entries[+handles[dependencyIndex] - 1];
				if( dependentEntry.status != TaskSystemImpl::TaskEntry::Completed ) {
					return false;
				}
			} while( ++dependencyIndex < entry.endOfRegularPolledDependencies );
		}
		if( entry.startOfSignaledPolledDependencies < entry.endOfSignaledPolledDependencies ) [[unlikely]] {
			const int16_t *__restrict handles = impl->dependencyHandles.get();
			const auto *__restrict entries    = (TaskSystemImpl::TaskEntry *)impl->tapes[1].memOfEntries.get();
			unsigned dependencyIndex          = entry.startOfSignaledPolledDependencies;
			do {
				auto &__restrict dependentEntry = entries[-handles[dependencyIndex] - 1];
				if( dependentEntry.status != TaskSystemImpl::TaskEntry::Completed ) {
					return false;
				}
			} while( ++dependencyIndex < entry.endOfSignaledPolledDependencies );
		}
		// Safe under mutex
		if( entry.numSatisfiedPushDependencies != entry.numTotalPushDependencies ) {
			assert( entry.numSatisfiedPushDependencies < entry.numTotalPushDependencies );
			return false;
		}
		return true;
	};

	try {
		std::optional<std::pair<size_t, unsigned>> pendingCompletionIndex;
		unsigned startFromTapeIndex = 0;
		for(;; ) {
			std::optional<std::pair<size_t, unsigned>> chosenIndex;
			bool isInNonPendingSpan = true;
			do {
				[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &impl->globalMutex );

				if( pendingCompletionIndex != std::nullopt ) {
					auto [indexInTape, indexOfTape] = *pendingCompletionIndex;
					completeEntry( ( (TaskSystemImpl::TaskEntry *)impl->tapes[indexOfTape].memOfEntries.get() )[indexInTape] );
					pendingCompletionIndex = std::nullopt;
				}

				// Alternate the initial tape index during each attempt of choosing a task, so another tape does not starve
				startFromTapeIndex = ( startFromTapeIndex + 1 ) % 2;
				// Scan both tapes
				for( unsigned turn = 0; turn < 2; ++turn ) {
					const unsigned tapeIndex         = ( startFromTapeIndex + turn ) % 2;
					TaskSystemImpl::Tape *const tape = &impl->tapes[tapeIndex];

					// Caution! This number may grow during execution outside of the lock scope.
					const size_t numEntries = tape->numEntriesSoFar;
					// This is similar to scanning the monotonic array-based heap in the AAS routing subsystem
					size_t entryIndex       = tape->startScanFrom;
					for( size_t entryScanJump; entryIndex < numEntries; entryIndex += entryScanJump ) {
						auto &entry       = ( (TaskSystemImpl::TaskEntry *)tape->memOfEntries.get() )[entryIndex];
						entryScanJump     = 1;
						const auto status = entry.status;
						assert( ( tapeIndex == 0 ) == ( entry.dynamicCompletionStatusAddress == nullptr ) );
						if( status == TaskSystemImpl::TaskEntry::Pending ) {
							if( isInNonPendingSpan ) {
								// Start scanning this tape later from this position
								tape->startScanFrom = entryIndex;
								isInNonPendingSpan  = false;
							}
							if( entry.affinity == AnyThread || threadNumber == ~0u ) {
								if( checkSatisfiedDependencies( entry ) ) {
									entry.status = TaskSystemImpl::TaskEntry::Busy;
									chosenIndex  = std::make_pair( entryIndex, tapeIndex );
									break;
								}
								// Apply it in the case of having unsatisfied dependencies
								entryScanJump += entry.extraScanJump;
							}
						} else if( tapeIndex == 1 && status == TaskSystemImpl::TaskEntry::Busy ) {
							if( isInNonPendingSpan ) {
								// Start scanning this tape later from this position
								tape->startScanFrom = entryIndex;
								isInNonPendingSpan  = false;
							}
							// Safe under mutex
							if( *entry.dynamicCompletionStatusAddress == true ) {
								completeEntry( entry );
							}
						}
						assert( entryScanJump > 0 );
					}
					if( chosenIndex != std::nullopt ) {
						break;
					}
				}
				// The global mutex unlocks here
			} while( false );

			if( chosenIndex != std::nullopt ) {
				auto [indexInTape, indexOfTape] = *chosenIndex;
				auto &chosenEntry = ( (TaskSystemImpl::TaskEntry *)impl->tapes[indexOfTape].memOfEntries.get() )[indexInTape];
				const unsigned offsetOfCallable = chosenEntry.offsetOfCallable;
				// If it's not an auxiliary entry without actual callable
				if( offsetOfCallable != ~0u ) [[likely]] {
					auto *callable = (TapeCallable *)( impl->memOfCallables.get() + offsetOfCallable );
					const unsigned workerIndex = threadNumber + 1;
					callable->call( workerIndex, chosenEntry.instanceArg );
				}
				if( !chosenEntry.dynamicCompletionStatusAddress ) [[likely]] {
					// Postpone completion so we don't have to lock twice
					pendingCompletionIndex = chosenIndex;
				}
			} else {
				// If all tasks are completed or busy
				if( isInNonPendingSpan ) {
					// If it's an actual worker thread
					if( threadNumber != ~0u ) {
						// It's fine if we do another loop attempt, so a relaxed load could be used here,
						// but seq_cst load should be less expensive than calling yield() as a consequence.
						if( impl->awaitsCompletion.load( std::memory_order_seq_cst ) ) {
							break;
						} else {
							// Await for dynamically submitted tasks.
							// Looks better than reusing await logic in threadLoopFunc().
							// Should be rarely reached if tuned right.
							std::this_thread::yield();
						}
					} else {
						// It's the main thread.
						// Interrupt the execution and let us set the awaitsCompletion flag.
						break;
					}
				}
			}
		}

		assert( pendingCompletionIndex == std::nullopt );
		assert( impl->isExecuting );
		return true;
	} catch( ... ) {
		assert( impl->isExecuting );
		return false;
	}
}