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

#include <atomic>
#include <cassert>
#include <thread>
#include <vector>
#include <deque>
#include <variant>
#include <span>
#include <mutex>
#include <barrier>

struct TaskSystemImpl {
	explicit TaskSystemImpl( unsigned numExtraThreads );
	~TaskSystemImpl();

	struct TaskEntry {
		enum Status : unsigned { Pending, Busy, Completed };
		unsigned status { Pending };
		unsigned offsetOfCallable { 0 };
		unsigned offsetOfDependencies { 0 };
		unsigned countOfDependencies { 0 };
		TaskSystem::Affinity affinity { TaskSystem::AnyThread };
	};

	// In bytes
	static constexpr size_t kCapacityOfMemOfCallables = 1 * 1024 * 1024;
	static constexpr size_t kMaxTaskEntries           = std::numeric_limits<int16_t>::max();
	// Let us assume the average number of dependencies to be 16
	static constexpr size_t kMaxDependencyEntries     = 16 * kMaxTaskEntries;

	// Sanity checks
	static_assert( sizeof( TaskEntry ) * kMaxTaskEntries < 1 * 1024 * 1024 );
	static_assert( sizeof( uint16_t ) * kMaxDependencyEntries < 1 * 1024 * 1024 );

	// Note: All entries which get submitted between TaskSystem::startExecution() and TaskSystem::awaitCompletion()
	// stay in the same region of memory without relocation.

	// A separate storage for large callables.
	// Callables and task entries may reside in the same buffer, but it (as a late addition) complicates existing code.
	// Preventing use of std::function<> in public API is primary reason of using custom allocation of callables.
	std::unique_ptr<uint8_t[]> memOfCallables { new uint8_t[kCapacityOfMemOfCallables] };
	// Using a raw memory chunk for task entries (they aren't trivially constructible, hence they get created on demand)
	std::unique_ptr<uint8_t[]> memOfTaskEntries { new uint8_t[sizeof( TaskEntry ) * kMaxTaskEntries] };
	std::unique_ptr<uint16_t[]> dependencyEntries { new uint16_t[kMaxDependencyEntries] };

	// In bytes
	unsigned sizeOfUsedMemOfCallables { 0 };
	unsigned numTaskEntriesSoFar { 0 };
	unsigned numDependencyEntriesSoFar { 0 };

	std::barrier<decltype( []() noexcept {} )> barrier;

	std::vector<std::jthread> threads;
	std::deque<std::atomic_flag> signalingFlags;
	std::deque<std::atomic<unsigned>> completionStatuses;
	std::atomic<unsigned> startScanFrom { 0 };
	std::mutex globalMutex;
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

void TaskSystem::acquireLock( TaskSystemImpl *impl ) {
	impl->globalMutex.lock();
}

void TaskSystem::releaseLock( TaskSystemImpl *impl ) {
	impl->globalMutex.unlock();
}

auto TaskSystem::addEntryAndAllocCallableMem( Affinity affinity, const TaskHandle *dependenciesBegin,
											  const TaskHandle *dependenciesEnd, size_t alignment, size_t size )
											  -> std::pair<void *, TaskHandle> {
	assert( dependenciesBegin <= dependenciesEnd );
	assert( alignment && size );

	// May happen in runtime. This has to be handled.

	if( m_impl->numTaskEntriesSoFar + 1 >= TaskSystemImpl::kMaxTaskEntries ) [[unlikely]] {
		throw std::runtime_error( "Too many task entries" );
	}
	const size_t newNumDependencyEntries = m_impl->numDependencyEntriesSoFar + ( dependenciesEnd - dependenciesBegin );
	if( newNumDependencyEntries > (size_t)TaskSystemImpl::kMaxTaskEntries ) [[unlikely]] {
		throw std::runtime_error( "Too many dependencies" );
	}

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

	if( m_impl->sizeOfUsedMemOfCallables + size + alignmentBytes > TaskSystemImpl::kCapacityOfMemOfCallables ) {
		throw std::runtime_error( "The storage of callables has been exhausted" );
	}

	const unsigned offsetOfCallable = m_impl->sizeOfUsedMemOfCallables + alignmentBytes;
	m_impl->sizeOfUsedMemOfCallables += size + alignmentBytes;

	const auto offsetOfDependencies = (unsigned)m_impl->numDependencyEntriesSoFar;
	const auto countOfDependencies  = (unsigned)( dependenciesEnd - dependenciesBegin );
	if( countOfDependencies ) [[likely]] {
		for( const TaskHandle *dependency = dependenciesBegin; dependency < dependenciesEnd; ++dependency ) {
			assert( dependency->m_opaque <= m_impl->numTaskEntriesSoFar );
			m_impl->dependencyEntries.get()[m_impl->numDependencyEntriesSoFar++] = dependency->m_opaque - 1;
		}
	}

	void *taskEntryMem = m_impl->memOfTaskEntries.get() + sizeof( TaskSystemImpl::TaskEntry ) * m_impl->numTaskEntriesSoFar;
	m_impl->numTaskEntriesSoFar++;
	new( taskEntryMem )TaskSystemImpl::TaskEntry {
		.offsetOfCallable     = offsetOfCallable,
		.offsetOfDependencies = offsetOfDependencies,
		.countOfDependencies  = countOfDependencies,
		.affinity             = affinity,
	};

	TaskHandle resultHandle;
	resultHandle.m_opaque = m_impl->numTaskEntriesSoFar;
	return { (void *)( m_impl->memOfCallables.get() + offsetOfCallable ), resultHandle };
}

void TaskSystem::clear() {
	assert( !m_impl->isExecuting );

	auto *const taskEntries = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get();

	// Callables aren't trivially destructible
	for( unsigned taskIndex = 0; taskIndex < m_impl->numTaskEntriesSoFar; ++taskIndex ) {
		auto *callable = (CallableStorage *)( m_impl->memOfCallables.get() + taskEntries[taskIndex].offsetOfCallable );
		callable->~CallableStorage();
	}
	m_impl->sizeOfUsedMemOfCallables  = 0;

	// Make sure we can just set count to zero for task entries
	static_assert( std::is_trivially_destructible_v<TaskSystemImpl::TaskEntry> );
	m_impl->numTaskEntriesSoFar       = 0;
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
	m_impl->startScanFrom.store( 0, std::memory_order_seq_cst );

	awakeWorkers();
}

bool TaskSystem::awaitCompletion() {
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
	try {
		auto *const __restrict entries = (TaskSystemImpl::TaskEntry *)impl->memOfTaskEntries.get();
		const size_t numEntries        = impl->numTaskEntriesSoFar;

		constexpr auto kIndexUnset    = (size_t)-1;
		size_t pendingCompletionIndex = kIndexUnset;
		for(;; ) {
			size_t chosenIndex      = kIndexUnset;
			bool isInNonPendingSpan = true;
			do {
				[[maybe_unused]] volatile std::scoped_lock<std::mutex> lock( impl->globalMutex );

				if( pendingCompletionIndex != kIndexUnset ) {
					TaskSystemImpl::TaskEntry &entry = entries[pendingCompletionIndex];
					// Safe under mutex
					entry.status = TaskSystemImpl::TaskEntry::Completed;
					pendingCompletionIndex = kIndexUnset;
				}

				// This is similar to scanning the monotonic array-based heap in the AAS routing subsystem
				size_t entryIndex = impl->startScanFrom.load( std::memory_order_relaxed );
				for(; entryIndex < numEntries; ++entryIndex ) {
					// Safe under mutex
					TaskSystemImpl::TaskEntry &entry = entries[entryIndex];
					const unsigned status            = entry.status;
					if( status == TaskSystemImpl::TaskEntry::Pending ) {
						if( isInNonPendingSpan ) {
							impl->startScanFrom.store( entryIndex, std::memory_order_relaxed );
							isInNonPendingSpan = false;
						}
						if( entry.affinity == AnyThread || threadNumber == ~0u ) {
							// TODO: These checks can be optimized by tracking indices/masks of already completed dependencies
							bool hasSatisfiedDependencies = true;
							unsigned dependencyIndex      = entry.offsetOfDependencies;
							for(; dependencyIndex < entry.countOfDependencies; ++dependencyIndex ) {
								// Safe under mutex
								if( entries[dependencyIndex].status == TaskSystemImpl::TaskEntry::Completed ) {
									hasSatisfiedDependencies = false;
									break;
								}
							}
							if( hasSatisfiedDependencies ) {
								entry.status = TaskSystemImpl::TaskEntry::Busy;
								chosenIndex  = entryIndex;
								break;
							}
						}
					}
				}
				// The global mutex unlocks here
			} while( false );

			if( chosenIndex != kIndexUnset ) {
				auto *callable = (CallableStorage *)( impl->memOfCallables.get() + entries[chosenIndex].offsetOfCallable );
				const unsigned workerIndex = threadNumber + 1;
				callable->call( workerIndex );
				// Postpone completion so we don't have to lock twice
				pendingCompletionIndex = chosenIndex;
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

		assert( pendingCompletionIndex == kIndexUnset );
		assert( impl->isExecuting );
		return true;
	} catch( ... ) {
		assert( impl->isExecuting );
		return false;
	}
}