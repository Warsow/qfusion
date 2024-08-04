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
		enum Status : uint8_t { Pending, Busy, Completed };

		Status status { Pending };
		TaskSystem::DependencyAddressMode polledDependenciesAddressMode { TaskSystem::RangeOfEntryIndices };
		TaskSystem::DependencyAddressMode pushedDependentsAddressMode { TaskSystem::RangeOfEntries };
		TaskSystem::Affinity affinity { TaskSystem::AnyThread };

		// Let it crash if not set (~0u is a special value)
		unsigned offsetOfCallable { ~0u / 2 };

		unsigned startOfPolledDependencies { 0 }, endOfPolledDependencies { 0 };
		unsigned startOfPushedDependents { 0 }, endOfPushedDependents { 0 };

		unsigned numSatisfiedPushDependencies { 0 }, numTotalPushDependencies { 0 };

		unsigned instanceArg { 0 };
	};

	// In bytes
	static constexpr size_t kCapacityOfMemOfCallables = 256 * 1024;
	static constexpr size_t kMaxTaskEntries           = TaskSystem::kMaxTaskEntries;
	// Let us assume the average number of dependencies to be 16
	static constexpr size_t kMaxDependencyIndices     = 16 * kMaxTaskEntries;

	// Sanity checks
	static_assert( sizeof( TaskEntry ) * kMaxTaskEntries < 2 * 1024 * 1024 );
	static_assert( sizeof( uint16_t ) * kMaxDependencyIndices < 1024 * 1024 );

	// Note: All entries which get submitted between TaskSystem::startExecution() and TaskSystem::awaitCompletion()
	// stay in the same region of memory without relocation.

	// A separate storage for large callables.
	// Callables and task entries may reside in the same buffer, but it (as a late addition) complicates existing code.
	// Preventing use of std::function<> in public API is primary reason of using custom allocation of callables.
	std::unique_ptr<uint8_t[]> memOfCallables { new uint8_t[kCapacityOfMemOfCallables] };
	// Using a raw memory chunk for task entries (they aren't trivially constructible, hence they get created on demand)
	std::unique_ptr<uint8_t[]> memOfTaskEntries { new uint8_t[sizeof( TaskEntry ) * kMaxTaskEntries] };
	std::unique_ptr<uint16_t[]> dependencyIndices {new uint16_t[kMaxDependencyIndices] };

	// In bytes
	unsigned sizeOfUsedMemOfCallables { 0 };
	unsigned numTaskEntriesSoFar { 0 };
	unsigned numDependencyEntriesSoFar { 0 };

	unsigned savedSizeOfUsedMemOfCallables { 0 };
	unsigned savedNumTaskEntriesSoFar { 0 };
	unsigned savedNumDependencyEntriesSoFar { 0 };

	wsw::PodVector<uint16_t> tmpOffsetsOfCallables;

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

void TaskSystem::beginTapeModification( TaskSystemImpl *impl ) {
	impl->globalMutex.lock();
	impl->savedSizeOfUsedMemOfCallables  = impl->sizeOfUsedMemOfCallables;
	impl->savedNumTaskEntriesSoFar       = impl->numTaskEntriesSoFar;
	impl->savedNumDependencyEntriesSoFar = impl->numDependencyEntriesSoFar;
}

void TaskSystem::endTapeModification( TaskSystemImpl *impl, bool succeeded ) {
	if( !succeeded ) [[unlikely]] {
		// Callables must be constructed in a nonthrowing fashion, hence we just reset the offset
		impl->sizeOfUsedMemOfCallables  = impl->savedSizeOfUsedMemOfCallables;
		impl->numTaskEntriesSoFar       = impl->savedNumTaskEntriesSoFar;
		impl->numDependencyEntriesSoFar = impl->savedNumDependencyEntriesSoFar;
	}
	impl->globalMutex.unlock();
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
		throw std::runtime_error( "The storage of callables has been exhausted" );
	}

	const unsigned offsetOfCallable = m_impl->sizeOfUsedMemOfCallables + alignmentBytes;
	m_impl->sizeOfUsedMemOfCallables += size + alignmentBytes;

	void *const memForCallable = m_impl->memOfCallables.get() + offsetOfCallable;
	assert( ( (uintptr_t)memForCallable % alignment ) == 0 );
	return { memForCallable, offsetOfCallable };
}

auto TaskSystem::addEntry( Affinity affinity, unsigned offsetOfCallable ) -> TaskHandle {
	// May happen in runtime. This has to be handled.
	if( m_impl->numTaskEntriesSoFar + 1 >= TaskSystemImpl::kMaxTaskEntries ) [[unlikely]] {
		throw std::runtime_error( "Too many task entries" );
	}

	void *taskEntryMem = m_impl->memOfTaskEntries.get() + sizeof( TaskSystemImpl::TaskEntry ) * m_impl->numTaskEntriesSoFar;
	m_impl->numTaskEntriesSoFar++;
	new( taskEntryMem )TaskSystemImpl::TaskEntry {
		.affinity         = affinity,
		.offsetOfCallable = offsetOfCallable,
	};

	TaskHandle resultHandle;
	resultHandle.m_opaque = m_impl->numTaskEntriesSoFar;
	return resultHandle;
}

void TaskSystem::addPolledDependenciesToEntry( TaskHandle taskHandle, const TaskHandle *depsBegin, const TaskHandle *depsEnd ) {
	assert( depsBegin <= depsEnd );

	// May happen in runtime. This has to be handled.
	const size_t newNumDependencyEntries = m_impl->numDependencyEntriesSoFar + ( depsEnd - depsBegin );
	if( newNumDependencyEntries > (size_t)TaskSystemImpl::kMaxDependencyIndices ) [[unlikely]] {
		throw std::runtime_error( "Too many dependencies" );
	}

	const auto offsetOfDependencies = (unsigned)m_impl->numDependencyEntriesSoFar;
	for( const TaskHandle *dependency = depsBegin; dependency < depsEnd; ++dependency ) {
		assert( dependency->m_opaque <= m_impl->numTaskEntriesSoFar );
		// TODO: Simplify this, lift it out of the loop
		m_impl->dependencyIndices.get()[m_impl->numDependencyEntriesSoFar++] = dependency->m_opaque - 1;
	}

	auto *const entry = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get() + ( taskHandle.m_opaque - 1 );
	entry->startOfPolledDependencies     = offsetOfDependencies;
	entry->endOfPolledDependencies       = offsetOfDependencies + ( depsEnd - depsBegin );
	entry->polledDependenciesAddressMode = DependencyAddressMode::RangeOfEntryIndices;
}

void TaskSystem::addPushedDependentsToEntry( TaskHandle taskHandle, unsigned taskRangeBegin, unsigned taskRangeEnd ) {
	assert( taskRangeBegin <= taskRangeEnd );

	auto *const taskMemBase = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get();

	auto *const entry = taskMemBase + ( taskHandle.m_opaque - 1 );
	entry->startOfPushedDependents     = taskRangeBegin;
	entry->endOfPushedDependents       = taskRangeEnd;
	entry->pushedDependentsAddressMode = DependencyAddressMode::RangeOfEntries;

	// Add an expected push dependency to each task in the range
	for( unsigned taskIndex = taskRangeBegin; taskIndex < taskRangeEnd; ++taskIndex ) {
		taskMemBase[taskIndex].numTotalPushDependencies++;
	}
}

[[nodiscard]]
auto TaskSystem::addParallelAndJoinEntries( Affinity affinity, unsigned offsetOfCallable, unsigned numTasks )
	-> std::pair<std::pair<unsigned, unsigned>, TaskHandle> {
	// May happen in runtime. This has to be handled.
	if( m_impl->numTaskEntriesSoFar + numTasks + 1 >= TaskSystemImpl::kMaxTaskEntries ) [[unlikely]] {
		throw std::runtime_error( "Too many task entries" );
	}

	const unsigned parRangeBegin = m_impl->numTaskEntriesSoFar;
	const unsigned parRangeEnd   = m_impl->numTaskEntriesSoFar + numTasks;

	auto *const taskMemBase = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get();
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

	m_impl->numTaskEntriesSoFar += numTasks + 1;

	TaskHandle resultHandle;
	resultHandle.m_opaque = m_impl->numTaskEntriesSoFar;
	return { { parRangeBegin, parRangeEnd }, resultHandle };
}

void TaskSystem::setupIotaInstanceArgs( std::pair<unsigned, unsigned> taskRange, unsigned startValue ) {
	const auto [taskRangeBegin, taskRangeEnd] = taskRange;
	assert( taskRangeBegin < taskRangeEnd && taskRangeEnd <= m_impl->numTaskEntriesSoFar );

	for( unsigned taskIndex = taskRangeBegin; taskIndex < taskRangeEnd; ++taskIndex ) {
		auto *entry = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get() + taskIndex;
		entry->instanceArg = startValue + ( taskIndex - taskRangeBegin );
	}
}

void TaskSystem::setupRangeInstanceArgs( std::pair<unsigned, unsigned> taskRange, unsigned startValue,
										 unsigned subrangeLength, unsigned totalWorkload ) {
	const auto [taskRangeBegin, taskRangeEnd] = taskRange;
	assert( taskRangeBegin < taskRangeEnd && taskRangeEnd <= m_impl->numTaskEntriesSoFar );
	assert( taskRangeEnd - taskRangeBegin <= totalWorkload );

	[[maybe_unused]] unsigned workloadLeft = totalWorkload;

	unsigned workloadStart = startValue;
	for( unsigned taskIndex = taskRangeBegin; taskIndex < taskRangeEnd; ++taskIndex ) {
		unsigned workloadEnd = workloadStart + subrangeLength;
		if( workloadEnd > totalWorkload + startValue ) [[unlikely]] {
			assert( taskIndex + 1 == taskRangeEnd );
			workloadEnd = totalWorkload + startValue;
		}

		auto *entry = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get() + taskIndex;
		assert( workloadStart < workloadEnd && workloadEnd <= 0xFFFFu );
		entry->instanceArg = ( workloadStart << 16 ) | workloadEnd;

		workloadLeft -= ( workloadEnd - workloadStart );
		workloadStart = workloadEnd;
	}

	assert( workloadLeft == 0 );
}

void TaskSystem::clear() {
	assert( !m_impl->isExecuting );

	auto *const taskEntries = (TaskSystemImpl::TaskEntry *)m_impl->memOfTaskEntries.get();

	// Callables may be shared for multiple task entries.
	// Tracking references to callables is way too bothersome.
	// Just prevent duplicated destructor calls which lead to crashing.
	m_impl->tmpOffsetsOfCallables.clear();

	// These are optimizations to the naive implementation.
	unsigned offsetOfLastDestroyedCallable     = ~0u;
	unsigned maxOffsetOfDestroyedCallableSoFar = 0;

	// Callables aren't trivially destructible
	// TODO: They can perfectly be in some cases (track this status)
	for( unsigned taskIndex = 0; taskIndex < m_impl->numTaskEntriesSoFar; ++taskIndex ) {
		const unsigned offsetOfCallable = taskEntries[taskIndex].offsetOfCallable;
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
					if( entry.startOfPushedDependents < entry.endOfPushedDependents ) {
						unsigned dependentIndex = entry.startOfPushedDependents;
						if( entry.pushedDependentsAddressMode == DependencyAddressMode::RangeOfEntries ) {
							// Safe under mutex
							do {
								auto &dependentEntry = entries[dependentIndex];
								dependentEntry.numSatisfiedPushDependencies++;
								assert( dependentEntry.numSatisfiedPushDependencies <= dependentEntry.numTotalPushDependencies );
							} while( ++dependentIndex < entry.endOfPushedDependents );
						} else {
							const auto *dependencyIndices = impl->dependencyIndices.get();
							do {
								auto &dependentEntry = entries[dependencyIndices[dependentIndex]];
								dependentEntry.numSatisfiedPushDependencies++;
								assert( dependentEntry.numSatisfiedPushDependencies <= dependentEntry.numTotalPushDependencies );
							} while( ++dependentIndex < entry.endOfPushedDependents );
						}
					}
					// Mark self as completed in all cases.
					// Safe under mutex.
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
							bool hasSatisfiedDependencies = true;
							// TODO: These checks can be optimized by tracking indices/masks of already completed dependencies
							if( entry.startOfPolledDependencies < entry.endOfPolledDependencies ) {
								unsigned dependencyIndex = entry.startOfPolledDependencies;
								if( entry.polledDependenciesAddressMode == DependencyAddressMode::RangeOfEntries ) {
									do {
										// Safe under mutex
										if( entries[dependencyIndex].status != TaskSystemImpl::TaskEntry::Completed ) {
											hasSatisfiedDependencies = false;
											break;
										}
									} while( ++dependencyIndex < entry.endOfPolledDependencies );
								} else {
									const auto *dependencIndices = impl->dependencyIndices.get();
									do {
										auto &dependentEntry = entries[dependencIndices[dependencyIndex]];
										if( dependentEntry.status != TaskSystemImpl::TaskEntry::Completed ) {
											hasSatisfiedDependencies = false;
											break;
										}
									} while( ++dependencyIndex < entry.endOfPolledDependencies );
								}
							}
							if( hasSatisfiedDependencies ) {
								// Safe under mutex
								if( entry.numSatisfiedPushDependencies != entry.numTotalPushDependencies ) {
									assert( entry.numSatisfiedPushDependencies < entry.numTotalPushDependencies );
									hasSatisfiedDependencies = false;
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
				const unsigned offsetOfCallable = entries[chosenIndex].offsetOfCallable;
				// If it's not an auxiliary entry without actual callable
				if( offsetOfCallable != ~0u ) [[likely]] {
					auto *callable = (TapeCallable *)( impl->memOfCallables.get() + offsetOfCallable );
					const unsigned workerIndex = threadNumber + 1;
					callable->call( workerIndex, entries[chosenIndex].instanceArg );
				}
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