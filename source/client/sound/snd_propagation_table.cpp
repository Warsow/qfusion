#include "snd_propagation.h"

#include <common/links.h>
#include <common/singletonholder.h>
#include <common/wswalgorithm.h>

#include <limits>
#include <random>
#include <vector>
#include <memory>

struct HeapEntry {
	float distance;
	float heapCost;
	int leafNum;

	HeapEntry( int leafNum_, float distance_, float heapCost_ )
		: distance( distance_ ), heapCost( heapCost_ ), leafNum( leafNum_ ) {}

	bool operator<( const HeapEntry &that ) const {
		// std:: algorithms use a max-heap
		return heapCost > that.heapCost;
	}
};

/**
 * A specialized updates heap optimized for path-finding usage patterns.
 */
class UpdatesHeap {
	HeapEntry *m_buffer;
	size_t m_size { 0 };
	size_t m_capacity { 1024 + 512 };
public:
	UpdatesHeap() { m_buffer = (HeapEntry *)std::malloc( sizeof( HeapEntry ) * m_capacity ); }

	~UpdatesHeap() { std::free( m_buffer ); }

	void Clear() { m_size = 0; }

	/**
	 * Constructs a new {@code HeapEntry} in place and adds it to the heap.
	 * @param leaf a forwarded parameter of {@code HeapEntry()} constructor.
	 * @param distance a forwarded parameter of {@code HeapEntry()} constructor.
	 * @param heapCost a forwarded parameter of {@code HeapEntry()} constructor.
	 */
	void Push( int leaf, float distance, float heapCost ) {
		new( m_buffer + m_size++ )HeapEntry( leaf, distance, heapCost );
		wsw::push_heap( m_buffer, m_buffer + m_size );
	}

	bool IsEmpty() const { return !m_size; }

	float BestDistance() const { return m_buffer[0].distance; }

	/**
	 * Pops a best heap entry.
	 * The returned value is valid until next {@code PrepareToAdd()} call.
	 * @return a reference to the newly popped entry.
	 */
	const HeapEntry &PopInPlace() {
		wsw::pop_heap( m_buffer, m_buffer + m_size );
		return m_buffer[--m_size];
	}

	/**
	 * Reserves buffer capacity for items that are about to be added.
	 * @param atMost a maximum number of newly added items.
	 */
	void ReserveForAddition( int atMost ) {
		if( m_size + atMost > m_capacity ) [[unlikely]] {
			const size_t newCapacity = ( 4 * ( m_size + atMost ) ) / 3;
			auto *newBuffer = (HeapEntry *)std::realloc( m_buffer, sizeof( HeapEntry ) * newCapacity );
			if( !newBuffer ) [[unlikely]] {
				wsw::failWithBadAlloc();
			}
			m_buffer   = newBuffer;
			m_capacity = newCapacity;
		}
	}
};

struct VertexBidirectionalUpdateStatus {
	float distance[2];
	int32_t parentLeaf[2];
	bool isVisited[2];
};

struct VertexFloodFillUpdateStatus {
	float distance;
	int32_t parentLeaf;
};

class BidirectionalPathFinder;

class PathReverseIterator {
	friend class BidirectionalPathFinder;
	BidirectionalPathFinder *const m_parent;
	int m_leafNum { std::numeric_limits<int>::min() };
	const int m_listIndex;

	PathReverseIterator( BidirectionalPathFinder *parent, int listIndex )
		: m_parent( parent ), m_listIndex( listIndex ) {}

	void ResetWithLeaf( int leafNum_ ) {
		assert( leafNum_ > 0 );
		this->m_leafNum = leafNum_;
	}
public:
	bool HasNext() const;
	void Next();
	int LeafNum() const { return m_leafNum; }
};

class FloodFillPathFinder {
	friend class CoarsePropagationThreadState;
	friend class CoarsePropagationBuilder;

	using VertexUpdateStatus = VertexFloodFillUpdateStatus;

	UpdatesHeap m_heap;
	PropagationGraphBuilder *m_graph;
	PodBufferHolder<VertexUpdateStatus> m_updateStatus;
	int m_lastFillLeafNum { -1 };

	void FloodFillForLeaf( int leafNum );

	int UnwindPath( int from, int to, int *directLeafNumsEnd, int *reverseLeafNumsBegin );

	float GetCertainPathDistance( int from, int to ) {
		assert( from == m_lastFillLeafNum );
		assert( std::isfinite( m_updateStatus.get()[to].distance ) );
		return m_updateStatus.get()[to].distance;
	}
public:
	explicit FloodFillPathFinder( PropagationGraphBuilder *graph ) : m_graph( graph ) {
		m_updateStatus.reserve( graph->NumLeafs() );
	}
};

class BidirectionalPathFinder {
	friend class PathReverseIterator;
	friend class PropagationTableBuilder;

	using IteratorType = PathReverseIterator;
	using VertexUpdateStatus = VertexBidirectionalUpdateStatus;

	/**
	 * An euclidean leaf-to-leaf distance table supplied by a parent
	 */
	const float *const m_euclideanDistanceTable;

	PropagationGraphBuilder *m_graph;

	PodBufferHolder<VertexUpdateStatus> m_updateStatus;

	UpdatesHeap m_heaps[2];

	IteratorType m_tmpDirectIterator { this, 0 };
	IteratorType m_tmpReverseIterator { this, 1 };

	float GetEuclideanDistance( int leaf1, int leaf2 ) const {
		return m_euclideanDistanceTable[leaf1 * m_graph->NumLeafs() + leaf2];
	}
public:
	explicit BidirectionalPathFinder( const float *euclideanDistanceTable, PropagationGraphBuilder *graph )
		: m_euclideanDistanceTable( euclideanDistanceTable )
		, m_graph( graph ) {
		m_updateStatus.reserve( graph->NumLeafs() );
	}

	/**
	 * Finds a path from a leaf to a leaf.
	 * The search algorithm is intended to be 2-directional
	 * and uniformly interleaving from direct to reverse propagation.
	 * Ordering of leaves does not really matter as the graph is undirected.
	 * @param fromLeaf a first leaf.
	 * @param toLeaf a second leaf.
	 * @param direct set to an address of a direct algorithm turn iterator on success.
	 * @param reverse set to an address of a reverse algorithm turn iterator on success.
	 * @return a best distance on success, an infinity on failure.
	 * @note returned iterators must be traversed backwards to get the algoritm turn start point (from- or to-leaf).
	 * These iterators are not assumed to contain complete direct/reverse path.
	 * They point to algorithm temporaries.
	 * However their combination is intended to represent an entire path.
	 * A last valid leaf during iteration matches {@code fromLeaf} and {@code toLeaf} accordingly.
	 */
	float FindPath( int fromLeaf, int toLeaf, IteratorType **direct, IteratorType **reverse );
};

inline bool PathReverseIterator::HasNext() const {
	return m_leafNum > 0 && m_parent->m_updateStatus.get()[m_leafNum].parentLeaf[m_listIndex] > 0;
}

inline void PathReverseIterator::Next() {
	assert( HasNext() );
	m_leafNum = m_parent->m_updateStatus.get()[m_leafNum].parentLeaf[m_listIndex];
}

float BidirectionalPathFinder::FindPath( int fromLeaf, int toLeaf, IteratorType **direct, IteratorType **reverse ) {
	// A-star hinting targets for each turn
	const int turnTargetLeaf[2] = { toLeaf, fromLeaf };

	for( int i = 0, end = m_graph->NumLeafs(); i < end; ++i ) {
		auto *status = m_updateStatus.get() + i;
		for( int turn = 0; turn < 2; ++turn ) {
			status->distance[turn] = std::numeric_limits<float>::infinity();
			status->parentLeaf[turn] = -1;
			status->isVisited[turn] = false;
		}
	}

	m_heaps[0].Clear();
	m_heaps[1].Clear();

	m_updateStatus.get()[fromLeaf].distance[0] = float( 0 );
	m_heaps[0].Push( fromLeaf, float( 0 ), GetEuclideanDistance( fromLeaf, toLeaf ) );
	m_updateStatus.get()[toLeaf].distance[1] = float( 0 );
	m_heaps[1].Push( toLeaf, float( 0 ), GetEuclideanDistance( toLeaf, fromLeaf ) );

	int bestLeaf = -1;
	auto bestDistanceSoFar = std::numeric_limits<float>::infinity();
	while( !m_heaps[0].IsEmpty() && !m_heaps[1].IsEmpty() ) {
		for( int turn = 0; turn < 2; ++turn ) {
			if( m_heaps[0].BestDistance() + m_heaps[1].BestDistance() >= bestDistanceSoFar ) {
				assert( bestLeaf > 0 );
				// Check whether this leaf has been really touched by direct and reverse algorithm turns
				assert( m_updateStatus.get()[bestLeaf].parentLeaf[0] >= 0 );
				assert( m_updateStatus.get()[bestLeaf].parentLeaf[1] >= 0 );
				m_tmpDirectIterator.ResetWithLeaf( bestLeaf );
				*direct = &m_tmpDirectIterator;
				m_tmpReverseIterator.ResetWithLeaf( bestLeaf );
				*reverse = &m_tmpReverseIterator;
				return bestDistanceSoFar;
			}

			auto *const activeHeap = &m_heaps[turn];

			const HeapEntry &entry = activeHeap->PopInPlace();
			// Save these values immediately as ReserveForAddition() call might make accessing the entry illegal.
			const int entryLeafNum = entry.leafNum;
			const double entryDistance = m_updateStatus.get()[entryLeafNum].distance[turn];

			m_updateStatus.get()[entryLeafNum].isVisited[turn] = true;

			// Now scan all adjacent vertices
			const auto *const adjacencyList = m_graph->AdjacencyList( entryLeafNum ) + 1;
			const auto listSize = adjacencyList[-1];
			activeHeap->ReserveForAddition( listSize );
			for( int i = 0; i < listSize; ++i ) {
				const auto leafNum = adjacencyList[i];
				auto *const status = m_updateStatus.get()+ leafNum;
				// We do not have to re-check already visited nodes for an euclidean heuristic
				if( status->isVisited[turn] ) {
					continue;
				}
				float edgeDistance = m_graph->EdgeDistance( entryLeafNum, leafNum );
				float relaxedDistance = edgeDistance + entryDistance;
				if( status->distance[turn] <= relaxedDistance ) {
					continue;
				}

				float otherDistance = status->distance[( turn + 1 ) & 1];
				if( otherDistance + relaxedDistance < bestDistanceSoFar ) {
					bestLeaf = leafNum;
					bestDistanceSoFar = otherDistance + relaxedDistance;
				}

				status->distance[turn] = relaxedDistance;
				status->parentLeaf[turn] = entryLeafNum;

				float euclideanDistance = GetEuclideanDistance( leafNum, turnTargetLeaf[turn] );
				activeHeap->Push( leafNum, relaxedDistance, relaxedDistance + euclideanDistance );
			}
		}
	}

	return std::numeric_limits<float>::infinity();
}

void FloodFillPathFinder::FloodFillForLeaf( int leafNum ) {
	// A-star hinting targets for each turn

	for( int i = 0, end = m_graph->NumLeafs(); i < end; ++i ) {
		auto *status = m_updateStatus.get() + i;
		status->distance = std::numeric_limits<float>::infinity();
		status->parentLeaf = -1;
	}

	m_heap.Clear();

	m_updateStatus.get()[leafNum].distance = 0.0f;
	m_heap.Push( leafNum, 0.0f, 0.0f );

	while( !m_heap.IsEmpty() ) {
		const HeapEntry &entry = m_heap.PopInPlace();
		// Save these values immediately as ReserveForAddition() call might make accessing the entry illegal.
		const int entryLeafNum = entry.leafNum;
		const double entryDistance = m_updateStatus.get()[entryLeafNum].distance;

		// Now scan all adjacent vertices
		const auto *const adjacencyList = m_graph->AdjacencyList( entryLeafNum ) + 1;
		const auto listSize = adjacencyList[-1];
		m_heap.ReserveForAddition( listSize );
		for( int i = 0; i < listSize; ++i ) {
			const auto leafNum = adjacencyList[i];
			auto *const status = &m_updateStatus.get()[leafNum];
			float edgeDistance = m_graph->EdgeDistance( entryLeafNum, leafNum );
			float relaxedDistance = edgeDistance + entryDistance;
			if( status->distance <= relaxedDistance ) {
				continue;
			}

			status->distance = relaxedDistance;
			status->parentLeaf = entryLeafNum;

			m_heap.Push( leafNum, relaxedDistance, relaxedDistance );
		}
	}

	m_lastFillLeafNum = leafNum;
}

int FloodFillPathFinder::UnwindPath( int from, int to, int *directLeafNumsEnd, int *reverseLeafNumsBegin ) {
	assert( from == m_lastFillLeafNum );

	int *directWritePtr = directLeafNumsEnd - 1;
	int *reverseWritePtr = reverseLeafNumsBegin;

	int vertexNum = to;
	for(;; ) {
		*directWritePtr-- = vertexNum;
		*reverseWritePtr++ = vertexNum;
		int parent = m_updateStatus.get()[vertexNum].parentLeaf;
		if( parent < 0 ) {
			break;
		}
		vertexNum = parent;
	}

	if( vertexNum != from ) {
		return -1;
	}

	ptrdiff_t diff = reverseWritePtr - reverseLeafNumsBegin;
	assert( diff > 0 );
	return (int)diff;
}

class PropagationTableBuilder;

class PropagationBuilderThreadState {
	friend class PropagationTableBuilder;
	friend class FinePropagationBuilder;
	friend class CoarsePropagationBuilder;
protected:
	using PropagationProps = PropagationTable::PropagationProps;
	using ParentBuilderType = PropagationTableBuilder;
	using GraphType = PropagationGraphBuilder;

	ParentBuilderType *const m_parent;
	PropagationProps *const m_table;
	GraphType m_graph;

	PodBufferHolder<int> m_tmpLeafNums;

	int m_total { -1 };
	int m_executed { 0 };
	int m_lastReportedProgress { 0 };
	int m_executedAtLastReport { 0 };

	explicit PropagationBuilderThreadState( ParentBuilderType *parent_, const PropagationGraphBuilder *referenceGraph );

	virtual void DoForRangeOfLeafs( int rangeBegin, int rangeEnd ) = 0;

	virtual ~PropagationBuilderThreadState() = default;

	void ComputePropsForPair( int leaf1, int leaf2 );

	void BuildInfluxDirForLeaf( float *allocatedDir, const int *leafsChain, int numLeafsInChain );

	void BuildInfluxDirForLeaf( float *allocatedDir, const int *leafsChainBegin, const int *leafsChainEnd ) {
		assert( leafsChainEnd > leafsChainBegin );
		// Sanity check
		assert( leafsChainEnd - leafsChainBegin < ( 1 << 20 ) );
		BuildInfluxDirForLeaf( allocatedDir, leafsChainBegin, (int)( leafsChainEnd - leafsChainBegin ) );
	}

	/**
	 * Builds a propagation path between given leaves.
	 * As the reachability presence and distance relations are symmetrical,
	 * an output is produced for direct and reverse path simultaneously.
	 * However influx dirs for first and second dir are not related at all
	 * (an influx dir for the second leaf is not an inversion of an influx dir for the first one).
	 * @param leaf1 a first leaf, must be distinct from the second one.
	 * @param leaf2 a second leaf, must be distinct from the first one.
	 * @param _1to2 a buffer for resulting propagation dir from first to second leaf.
	 * The result contains an average dir of sound flowing into second leaf while a source is placed at the first one.
	 * @param _2to1 a buffer for resulting propagation dir from second to first leaf.
	 * The result contains an average dir of sound flowing into first leaf while a source is placed at the second one.
	 * @param distance a distance of the best met temporary path returned as an out parameter.
	 * @return true if a path has been managed to be built successfully.
	 */
	virtual bool BuildPropagationPath( int leaf1, int leaf2, vec3_t _1to2, vec3_t _2to1, float *distance ) = 0;
};

class FinePropagationThreadState : public PropagationBuilderThreadState {
	friend class PropagationTableBuilder;
	friend class FinePropagationBuilder;
public:
	using PropagationProps = PropagationTable::PropagationProps;
	using IteratorType = PathReverseIterator;
	using ParentBuilderType = PropagationTableBuilder;
private:
	BidirectionalPathFinder m_pathFinder;

	void DoForRangeOfLeafs( int rangeBegin, int rangeEnd ) override;

	/**
	 * Unwinds a {PathReverseIterator} writing leaf numbers to a linear buffer.
	 * The buffer is assumed to be capable to store a leaves chain of maximum possible length for the current graph.
	 * @param iterator an iterator that represents intermediate results of path-finding.
	 * @param arrayEnd an end of the buffer range. Leaf numbers will be written before this address.
	 * @return a new range begin for the buffer (that is less than the {@code arrayEnd}
	 */
	int *Unwind( IteratorType *iterator, int *arrayEnd );

	/**
	 * Unwinds a {@code PathReverseIterator} writing leaf numbers to a linear buffer.
	 * The buffer is assumed to be capable to store a leaves chain of maximum possible length for the current graph.
	 * Scales graph edges defined by these leaf numbers at the same time.
	 * @param iterator an iterator that represents intermediate results of path-finding.
	 * @param arrayEnd an end of the buffer range. Leaf numbers will be written before this address.
	 * @param scale a weight scale for path edges
	 * @return a new range begin for the buffer (that is less than the {@code arrayEnd}
	 */
	int *UnwindScalingWeights( IteratorType *iterator, int *arrayEnd, float scale );

	bool BuildPropagationPath( int leaf1, int leaf2, vec3_t _1to2, vec3_t _2to1, float *distance ) override;
public:
	FinePropagationThreadState( ParentBuilderType *parent_, const float *euclideanDistanceTable, const PropagationGraphBuilder *referenceGraph )
		: PropagationBuilderThreadState( parent_, referenceGraph ), m_pathFinder( euclideanDistanceTable, &m_graph ) {}
};

static inline void ComputeLeafCenter( int leaf, vec3_t result ) {
	const vec3_t *bounds = S_GetLeafBounds( leaf );
	VectorSubtract( bounds[1], bounds[0], result );
	VectorScale( result, 0.5f, result );
	VectorAdd( bounds[0], result, result );
}

static inline float ComputeLeafToLeafDistance( int leaf1, int leaf2 ) {
	vec3_t center1, center2;
	ComputeLeafCenter( leaf1, center1 );
	ComputeLeafCenter( leaf2, center2 );
	return std::sqrt( DistanceSquared( center1, center2 ) );
}

static void BuildLeafEuclideanDistanceTable( float *table, int numLeafs ) {
	for( int i = 1; i < numLeafs; ++i ) {
		for( int j = i + 1; j < numLeafs; ++j ) {
			float distance = ComputeLeafToLeafDistance( i, j );
			table[i * numLeafs + j] = table[j * numLeafs + i] = (uint16_t)distance;
		}
	}
}

class CoarsePropagationThreadState : public PropagationBuilderThreadState {
	friend class CoarsePropagationBuilder;

	using ParentBuilderType = PropagationTableBuilder;
	using IteratorType = PathReverseIterator;
	using PathFinderType = FloodFillPathFinder;

	FloodFillPathFinder m_pathFinder;

	bool BuildPropagationPath( int leaf1, int leaf2, vec3_t _1to2, vec3_t _2to1, float *distance ) override;
public:
	explicit CoarsePropagationThreadState( ParentBuilderType *parent_, const PropagationGraphBuilder *referenceGraph )
		: PropagationBuilderThreadState( parent_, referenceGraph ), m_pathFinder( &m_graph ) {}

	void DoForRangeOfLeafs( int rangeBegin, int rangeEnd ) override;
};

class PropagationTableBuilder {
	friend class PropagationBuilderThreadState;
	friend class FinePropagationThreadState;
	friend class CoarsePropagationThreadState;
protected:

	PropagationGraphBuilder m_graphBuilder;

	PodBufferHolder<PropagationTable::PropagationProps> m_table;

	wsw::Mutex m_workloadMutex;
	int m_executedWorkload { 0 };
	int m_lastShownProgress { 0 };
	int m_totalWorkload { -1 };

	/**
	 * Adds a task progress to an overall progress.
	 * @param taskWorkloadDelta a number of workload units since last task progress report.
	 * A workload unit is a computation of {@code PropagationProps} for a pair of leafs.
	 * @note use this sparingly, only if "shown" progress of a task (progress percents) is changed.
	 * This is not that cheap to call.
	 */
	void AddTaskProgress( int taskWorkloadDelta );

	void ValidateJointResults();

#ifndef _MSC_VER
	void ValidationError( const char *format, ... )
		__attribute__( ( format( printf, 2, 3 ) ) ) __attribute__( ( noreturn ) );
#else
	__declspec( noreturn ) void ValidationError( _Printf_format_string_ const char *format, ... );
#endif

	virtual bool ExecComputations() = 0;
public:
	explicit PropagationTableBuilder( int actualNumLeafs, bool fastAndCoarse_ )
		: m_graphBuilder( actualNumLeafs, fastAndCoarse_ ) {}

	virtual ~PropagationTableBuilder() = default;

	bool Build();

	inline PodBufferHolder<PropagationTable::PropagationProps> ReleaseOwnership();
};

PropagationBuilderThreadState::PropagationBuilderThreadState( ParentBuilderType *parent, const PropagationGraphBuilder *referenceGraph )
	: m_parent( parent ), m_table( parent->m_table.get() )
	, m_graph( referenceGraph->NumLeafs(), referenceGraph->m_fastAndCoarse ) {
	// TODO: We used to share something...
	m_graph.m_distanceTable         = referenceGraph->m_distanceTable.makeADeepCopy();
	m_graph.m_dirsTable             = referenceGraph->m_dirsTable.makeADeepCopy();
	m_graph.m_adjacencyListsData    = referenceGraph->m_adjacencyListsData.makeADeepCopy();
	m_graph.m_adjacencyListsOffsets = referenceGraph->m_adjacencyListsOffsets.makeADeepCopy();

	m_tmpLeafNums.reserve( 2 * ( m_graph.NumLeafs() + 1 ) );
}

class FinePropagationBuilder : public PropagationTableBuilder {
	/**
	 * An euclidean distance table for leaves
	 * @todo using short values is sufficient for the majority of maps
	 */
	PodBufferHolder<float> m_euclideanDistanceTable;

	bool ExecComputations() override {
		TaskSystem taskSystem( { .numExtraThreads = S_SuggestNumExtraThreadsForComputations() } );
		// A workaround for non-movable,non-copyable types
		std::vector<std::shared_ptr<FinePropagationThreadState>> threadStatesForWorkers;
		for( unsigned i = 0; i < taskSystem.getNumberOfWorkers(); ++i ) {
			threadStatesForWorkers.emplace_back( std::make_shared<FinePropagationThreadState>( this, m_euclideanDistanceTable.get(), &m_graphBuilder ) );
		}
		const int numLeafs = m_graphBuilder.NumLeafs();
		// TODO: Let the task system manage it automatically?
		unsigned subrangeLength = 4;
		if( ( numLeafs / subrangeLength ) + 16 >= TaskSystem::kMaxTaskEntries ) {
			subrangeLength = ( numLeafs / TaskSystem::kMaxTaskEntries ) + 1;
		}
		auto fn = [=,&threadStatesForWorkers]( unsigned workerIndex, unsigned leafNumsBegin, unsigned leafNumsEnd ) {
			threadStatesForWorkers[workerIndex]->DoForRangeOfLeafs( leafNumsBegin, leafNumsEnd );
		};
		// Start early to test dynamic submission
		const TaskSystem::ExecutionHandle executionHandle = taskSystem.startExecution();
		(void)taskSystem.addForSubrangesInRange( { 1u, (unsigned)numLeafs }, subrangeLength, {}, std::move( fn ) );
		return taskSystem.awaitCompletion( executionHandle );
	}
public:
	explicit FinePropagationBuilder( int actualNumLeafs_ )
		: PropagationTableBuilder( actualNumLeafs_, false ) {
		m_euclideanDistanceTable.reserveZeroed( actualNumLeafs_ * actualNumLeafs_ );
		BuildLeafEuclideanDistanceTable( m_euclideanDistanceTable.get(), actualNumLeafs_ );
	}
};

class CoarsePropagationBuilder : public PropagationTableBuilder {
	bool ExecComputations() override {
		TaskSystem taskSystem( { .numExtraThreads = S_SuggestNumExtraThreadsForComputations() } );
		// A workaround for non-movable,non-copyable types
		std::vector<std::shared_ptr<CoarsePropagationThreadState>> threadStatesForWorkers;
		for( unsigned i = 0; i < taskSystem.getNumberOfWorkers(); ++i ) {
			threadStatesForWorkers.emplace_back( std::make_shared<CoarsePropagationThreadState>( this, &m_graphBuilder ) );
		}
		const int numLeafs = m_graphBuilder.NumLeafs();
		// TODO: Let the task system manage it automatically?
		unsigned subrangeLength = 4;
		if( ( numLeafs / subrangeLength ) + 16 >= TaskSystem::kMaxTaskEntries ) {
			subrangeLength = ( numLeafs / TaskSystem::kMaxTaskEntries ) + 1;
		}
		auto fn = [=,&threadStatesForWorkers]( unsigned workerIndex, unsigned leafNumsBegin, unsigned leafNumsEnd ) {
			threadStatesForWorkers[workerIndex]->DoForRangeOfLeafs( leafNumsBegin, leafNumsEnd );
		};
		// Start early to test dynamic submission
		const TaskSystem::ExecutionHandle executionHandle = taskSystem.startExecution();
		(void)taskSystem.addForSubrangesInRange( {1u, (unsigned)numLeafs }, subrangeLength, {}, std::move( fn ) );
		return taskSystem.awaitCompletion( executionHandle );
	}
public:
	explicit CoarsePropagationBuilder( int actualNumLeafs_ )
		: PropagationTableBuilder( actualNumLeafs_, true ) {}
};

PodBufferHolder<PropagationTable::PropagationProps> PropagationTableBuilder::ReleaseOwnership() {
	return std::move( m_table );
}

void PropagationTableBuilder::AddTaskProgress( int taskWorkloadDelta ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_workloadMutex );

	assert( taskWorkloadDelta > 0 );
	assert( m_totalWorkload > 0 && "The total workload value has not been set" );

	m_executedWorkload += taskWorkloadDelta;
	const auto newProgress = (int)( ( 100.0f * ( (float)m_executedWorkload / (float)m_totalWorkload ) ) );

	if( m_lastShownProgress != newProgress ) {
		m_lastShownProgress = newProgress;
		Com_Printf( "Computing a sound propagation table... %2d%%\n", newProgress );
	}
}

bool PropagationTableBuilder::Build() {
	if( !m_graphBuilder.Build() ) {
		return false;
	}

	const int numLeafs = m_graphBuilder.NumLeafs();
	// CBA to do a proper partitioning (we can't have more than 2^16 tasks)
	if( numLeafs >= std::numeric_limits<uint16_t>::max() ) {
		return false;
	}

	m_table.reserveZeroed( numLeafs * numLeafs );

	// Right now the computation host lifecycle should be limited only to scope where actual computations occur.

	// Set the total number of workload units
	// (a computation of props for a pair of leafs is a workload unit)
	this->m_totalWorkload = ( numLeafs - 1 ) * ( numLeafs - 2 ) / 2;

	if( !ExecComputations() ) {
		return false;
	}

#ifndef PUBLIC_BUILD
	ValidateJointResults();
#endif

	return true;
}

void PropagationTableBuilder::ValidateJointResults() {
	const int numLeafs = m_graphBuilder.NumLeafs();
	if( numLeafs <= 0 ) {
		ValidationError( "Illegal graph NumLeafs() %d", numLeafs );
	}
	const int actualNumLeafs = S_NumLeafs();
	if( numLeafs != actualNumLeafs ) {
		ValidationError( "graph NumLeafs() %d does not match actual map num leafs %d", numLeafs, actualNumLeafs );
	}

	for( int i = 1; i < numLeafs; ++i ) {
		for( int j = i + 1; j < numLeafs; ++j ) {
			const PropagationTable::PropagationProps &iToJ = m_table.get()[i * numLeafs + j];
			const PropagationTable::PropagationProps &jToI = m_table.get()[j * numLeafs + i];

			if( iToJ.HasDirectPath() ^ jToI.HasDirectPath() ) {
				ValidationError( "Direct path presence does not match for leaves %d, %d", i, j );
			}
			if( iToJ.HasDirectPath() ) {
				if( !std::isfinite( m_graphBuilder.EdgeDistance( i, j ) ) ) {
					ValidationError( "Graph distance is not finite for leaves %d, %d but props have direct path", i, j );
				}
				continue;
			}

			if( iToJ.HasIndirectPath() ^ jToI.HasIndirectPath() ) {
				ValidationError( "Indirect path presence does not match for leaves %d, %d", i, j );
			}
			if( std::isfinite( m_graphBuilder.EdgeDistance( i, j ) ) ) {
				ValidationError( "An edge in graph exists for leaves %d, %d but props do not have a direct path", i, j );
			}
			if( !iToJ.HasIndirectPath() ) {
				continue;
			}

			const float pathDistance = iToJ.GetDistance();
			if( !std::isfinite( pathDistance ) || pathDistance <= 0 ) {
				ValidationError( "Illegal propagation distance %f for pair (%d, %d)\n", pathDistance, i, j );
			}

			const auto reversePathDistance = jToI.GetDistance();
			if( reversePathDistance != pathDistance ) {
				const char *format = "Reverse path distance %f does not match direct one %f for leaves %d, %d";
				ValidationError( format, reversePathDistance, pathDistance, i, j );
			}

			// Just check whether these directories are normalized
			// (they are not the same and are not an inversion of each other)
			const char *dirTags[2] = { "direct", "reverse" };
			const PropagationTable::PropagationProps *propsRefs[2] = { &iToJ, &jToI };
			for( int k = 0; k < 2; ++k ) {
				vec3_t dir;
				propsRefs[k]->GetDir( dir );
				float length = std::sqrt( VectorLengthSquared( dir ) );
				if( std::abs( length - 1.0f ) > 0.1f ) {
					const char *format = "A dir %f %f %f for %s path between %d, %d is not normalized";
					ValidationError( format, dir[0], dir[1], dir[2], dirTags[k], i, j );
				}
			}
		}
	}
}

void PropagationTableBuilder::ValidationError( const char *format, ... ) {
	char buffer[1024];

	va_list va;
	va_start( va, format );
	Q_snprintfz( buffer, sizeof( buffer ), format, va );
	va_end( va );

	Com_Error( ERR_FATAL, "PropagationTableBuilder<?>::ValidateJointResults(): %s", buffer );
}

void FinePropagationThreadState::DoForRangeOfLeafs( int rangeBegin, int rangeEnd ) {
	assert( rangeBegin > 0 );
	assert( rangeEnd > rangeBegin );
	assert( rangeEnd <= m_graph.NumLeafs() );

	// The workload consists of a "rectangle" and a "triangle"
	// The rectangle width (along J - axis) is the range length
	// The rectangle height (along I - axis) is leafsRangeBegin - 1
	// Note that the first row of the table corresponds to a zero leaf and is skipped for processing.
	// Triangle legs have rangeLength size

	// -  -  -  -  -  -  -  -
	// -  *  o  o  o  X  X  X
	// -  o  *  o  o  X  X  X
	// -  o  o  *  o  X  X  X
	// -  o  o  o  *  X  X  X
	// -  o  o  o  o  *  X  X
	// -  o  o  o  o  o  *  X
	// -  o  o  o  o  o  o  *

	const int rangeLength = rangeEnd - rangeBegin;
	m_total = ( rangeBegin - 1 ) * rangeLength + ( rangeLength * ( rangeLength - 1 ) ) / 2;

	m_executed = 0;
	m_lastReportedProgress = 0;
	m_executedAtLastReport = 0;

	// Process "rectangular" part of the workload
	for( int i = 1; i < rangeBegin; ++i ) {
		for ( int j = rangeBegin; j < rangeEnd; ++j ) {
			this->ComputePropsForPair( i, j );
		}
	}

	// Process "triangular" part of the workload
	for( int i = rangeBegin; i < rangeEnd; ++i ) {
		for( int j = i + 1; j < rangeEnd; ++j ) {
			this->ComputePropsForPair( i, j );
		}
	}
}

void CoarsePropagationThreadState::DoForRangeOfLeafs( int rangeBegin, int rangeEnd ) {
	assert( rangeBegin > 0 );
	assert( rangeEnd > rangeBegin );
	assert( rangeEnd <= m_graph.NumLeafs() );

	const int rangeLength = rangeEnd - rangeBegin;
	m_total = ( rangeBegin - 1 ) * rangeLength + ( rangeLength * ( rangeLength - 1 ) ) / 2;

	m_executed = 0;
	m_lastReportedProgress = 0;
	m_executedAtLastReport = 0;

	// There should be leafsRangeEnd - leafsRangeBegin number of pathfinder calls
	for( int leafNum = rangeBegin; leafNum < rangeEnd; ++leafNum ) {
		// Use the Dijkstra algorithm to find path from leafNum to every other leaf
		m_pathFinder.FloodFillForLeaf( leafNum );
		// For every other leaf,
		// actually for the part of [leafsRangeBegin, leafsRangeEnd) x [0, leafsRangeEnd)
		// that belongs to the upper table triangle compute props
		// for a pair of cells below and above the table diagonal
		for( int thatLeaf = 1; thatLeaf < leafNum; ++thatLeaf ) {
			this->ComputePropsForPair( leafNum, thatLeaf );
		}
	}
}

void PropagationBuilderThreadState::ComputePropsForPair( int leaf1, int leaf2 ) {
	PropagationProps *const firstProps  = &m_table[leaf1 * m_graph.NumLeafs() + leaf2];
	PropagationProps *const secondProps = &m_table[leaf2 * m_graph.NumLeafs() + leaf1];

	if( m_graph.EdgeDistance( leaf1, leaf2 ) != std::numeric_limits<double>::infinity() ) {
		firstProps->SetHasDirectPath();
		secondProps->SetHasDirectPath();
	} else {
		vec3_t dir1, dir2;
		float distance;
		if( BuildPropagationPath( leaf1, leaf2, dir1, dir2, &distance ) ) {
			firstProps->SetIndirectPath( dir1, distance );
			secondProps->SetIndirectPath( dir2, distance );
		} else {
			firstProps->MarkAsFailed();
			secondProps->MarkAsFailed();
		}
	}

	m_executed++;
	const auto progress = (int)( 100 * ( (float)m_executed * Q_Rcp( (float)m_total ) ) );
	// We keep computing progress in percents to avoid confusion
	// but report only even values to reduce thread contention on AddTaskProgress()
	if( !( progress % 10 ) && progress != m_lastReportedProgress ) {
		const int taskWorkloadDelta = m_executed - m_executedAtLastReport;
		assert( taskWorkloadDelta > 0 );
		m_parent->AddTaskProgress( taskWorkloadDelta );
		m_lastReportedProgress = progress;
		m_executedAtLastReport = m_executed;
	}
}

/**
 * A helper for building a weighted sum of normalized vectors.
 */
class WeightedDirBuilder {
public:
	enum : int { MAX_DIRS = 5 };
private:
	vec3_t m_dirs[MAX_DIRS];
	float m_weights[MAX_DIRS];
	int m_numDirs { 0 };
public:
	/**
	 * Reserves a storage for a newly added vector.
	 * @param weight A weight that will be used for a final composition of accumulated data.
	 * @return a writable memory address that must be filled by the added vector.
	 */
	float *AllocDir( double weight ) {
		assert( m_numDirs < MAX_DIRS );
		assert( !std::isnan( weight ) );
		assert( weight >= 0.0 );
		assert( weight < std::numeric_limits<double>::infinity() );
		m_weights[m_numDirs] = (float)weight;
		return m_dirs[m_numDirs++];
	}

	/**
	 * Computes a weighted sum of accumulated normalized vectors.
	 * A resulting sum gets normalized.
	 * At least a single vector must be added before this call.
	 * @param dir a storage for a result.
	 */
	void BuildDir( vec3_t dir ) {
		VectorClear( dir );
		assert( m_numDirs );
		for( int i = 0; i < m_numDirs; ++i ) {
			VectorMA( dir, m_weights[i], m_dirs[i], dir );
		}
		VectorNormalize( dir );
		assert( std::abs( std::sqrt( VectorLengthSquared( dir ) ) - 1.0f ) < 0.1f );
	}
};

bool FinePropagationThreadState::BuildPropagationPath( int leaf1, int leaf2, vec3_t _1to2, vec3_t _2to1, float *distance ) {
	assert( leaf1 != leaf2 );

	WeightedDirBuilder _1to2Builder;
	WeightedDirBuilder _2to1Builder;

	IteratorType *directIterator;
	IteratorType *reverseIterator;

	// Save a copy of edge weights on demand.
	// Doing that is expensive.
	bool hasModifiedDistanceTable = false;

	double prevPathDistance = 0.0;
	double bestPathDistance = 0.0;
	int numAttempts = 0;
	// Increase quality in developer mode, so we can ship high-quality tables withing the game assets
	// Doing only a single attempt is chosen based on real tests
	// that show that these computations are extremely expensive
	// and could hang up a client computer for a hour (!).
	// Doing a single attempt also helps to avoid saving/restoring weights at all that is not cheap too.
	static_assert( WeightedDirBuilder::MAX_DIRS > 1, "Assumptions that doing only 1 attempt is faster are broken" );
	const int maxAttempts = WeightedDirBuilder::MAX_DIRS;
	// Do at most maxAttempts to find an alternative path
	for( ; numAttempts != maxAttempts; ++numAttempts ) {
		float newPathDistance = m_pathFinder.FindPath( leaf1, leaf2, &directIterator, &reverseIterator );
		// If the path cannot be (longer) found stop
		if( std::isinf( newPathDistance ) ) {
			break;
		}
		if( !directIterator->HasNext() || !reverseIterator->HasNext() ) {
			break;
		}
		if( !bestPathDistance ) {
			bestPathDistance = newPathDistance;
		}
		// Stop trying to find an alternative path if the new distance is much longer than the previous one
		if( prevPathDistance ) {
			if( newPathDistance > 1.1 * prevPathDistance && newPathDistance - prevPathDistance > 256.0 ) {
				break;
			}
		}

		prevPathDistance = newPathDistance;

		// tmpLeafNums are capacious enough to store slightly more than NumLeafs() * 2 elements
		int *const directLeafNumsEnd = m_tmpLeafNums.get() + m_graph.NumLeafs() + 1;
		int *directLeafNumsBegin;

		int *const reverseLeafNumsEnd = directLeafNumsEnd + m_graph.NumLeafs() + 1;
		int *reverseLeafNumsBegin;

		if( numAttempts + 1 != maxAttempts ) {
			if( !hasModifiedDistanceTable ) {
				m_graph.SaveDistanceTable();
				hasModifiedDistanceTable = true;
			}
			const auto lastDirectLeaf = directIterator->LeafNum();
			const auto firstReverseLeaf = reverseIterator->LeafNum();
			directLeafNumsBegin = UnwindScalingWeights( directIterator, directLeafNumsEnd, 3.0f );
			reverseLeafNumsBegin = UnwindScalingWeights( reverseIterator, reverseLeafNumsEnd, 3.0f );
			m_graph.ScaleEdgeDistance( lastDirectLeaf, firstReverseLeaf, 3.0f );
		} else {
			directLeafNumsBegin = this->Unwind( directIterator, directLeafNumsEnd );
			reverseLeafNumsBegin = this->Unwind( reverseIterator, reverseLeafNumsEnd );
		}

		const double attemptWeight = 1.0f / ( 1.0 + numAttempts );

		assert( *directLeafNumsBegin == leaf1 );
		// Direct leaf nums correspond to the head of the 1->2 path and yield 2->1 "propagation window"
		this->BuildInfluxDirForLeaf( _2to1Builder.AllocDir( attemptWeight ), directLeafNumsBegin, directLeafNumsEnd );

		assert( *reverseLeafNumsBegin == leaf2 );
		// Reverse leaf nums correspond to the tail of the 1->2 path and yield 1->2 "propagation window"
		this->BuildInfluxDirForLeaf( _1to2Builder.AllocDir( attemptWeight ), reverseLeafNumsBegin, reverseLeafNumsEnd );
	}

	if( hasModifiedDistanceTable ) {
		m_graph.RestoreDistanceTable();
	}

	if( !numAttempts ) {
		return false;
	}

	_1to2Builder.BuildDir( _1to2 );
	_2to1Builder.BuildDir( _2to1 );
	assert( bestPathDistance > 0 && std::isfinite( bestPathDistance ) );
	*distance = (float)bestPathDistance;
	assert( *distance > 0 && std::isfinite( *distance ) );
	return true;
}

bool CoarsePropagationThreadState::BuildPropagationPath( int leaf1, int leaf2, vec3_t _1to2, vec3_t _2to1, float *distance ) {
	// There's surely a sufficient room for the unwind buffer
	// tmpLeafNums are capacious enough to store slightly more than NumLeafs() * 2 elements
	int *const directLeafNumsEnd    = m_tmpLeafNums.get() + m_graph.NumLeafs() + 1;
	int *const reverseLeafNumsBegin = directLeafNumsEnd + 1;

	int numLeafsInChain = m_pathFinder.UnwindPath( leaf1, leaf2, directLeafNumsEnd, reverseLeafNumsBegin );
	// Looking forward to being able to use std::optional
	if( numLeafsInChain <= 0 ) {
		return false;
	}

	const int *directLeafNumsBegin = directLeafNumsEnd - numLeafsInChain;
	const int *reverseLeafNumsEnd = reverseLeafNumsBegin + numLeafsInChain;

	assert( directLeafNumsBegin[0] == reverseLeafNumsEnd[-1] );
	assert( directLeafNumsEnd[-1] == reverseLeafNumsBegin[0] );

	VectorClear( _1to2 );
	VectorClear( _2to1 );

	assert( *directLeafNumsBegin == leaf1 );
	// Direct leaf nums correspond to the head of the 1->2 path and yield 2->1 "propagation window"
	this->BuildInfluxDirForLeaf( _2to1, directLeafNumsBegin, directLeafNumsEnd );

	assert( *reverseLeafNumsBegin == leaf2 );
	// Reverse leaf nums correspond to the tail of the 1->2 path and yield 1->2 "propagation window"
	this->BuildInfluxDirForLeaf( _1to2, reverseLeafNumsBegin, reverseLeafNumsEnd );

	*distance = m_pathFinder.GetCertainPathDistance( leaf1, leaf2 );
	return true;
}

int *FinePropagationThreadState::Unwind( IteratorType *iterator, int *arrayEnd ) {
	int *arrayBegin = arrayEnd;
	// Traverse the direct iterator backwards
	int prevLeafNum = iterator->LeafNum();
	for(;; ) {
		*( --arrayBegin ) = prevLeafNum;
		iterator->Next();
		int nextLeafNum = iterator->LeafNum();
		prevLeafNum = nextLeafNum;
		if( !iterator->HasNext() ) {
			break;
		}
	}
	*( --arrayBegin ) = prevLeafNum;
	return arrayBegin;
}

int *FinePropagationThreadState::UnwindScalingWeights( IteratorType *iterator, int *arrayEnd, float scale ) {
	int *arrayBegin = arrayEnd;
	// Traverse the direct iterator backwards
	int prevLeafNum = iterator->LeafNum();
	for(;; ) {
		*( --arrayBegin ) = prevLeafNum;
		iterator->Next();
		int nextLeafNum = iterator->LeafNum();

		m_graph.ScaleEdgeDistance( prevLeafNum, nextLeafNum, scale );

		prevLeafNum = nextLeafNum;
		if( !iterator->HasNext() ) {
			break;
		}
	}
	*( --arrayBegin ) = prevLeafNum;
	return arrayBegin;
}

void PropagationBuilderThreadState::BuildInfluxDirForLeaf( float *allocatedDir, const int *leafsChain, int numLeafsInChain ) {
	assert( numLeafsInChain > 1 );
	const int maxTestedLeafs = wsw::min( numLeafsInChain, (int)WeightedDirBuilder::MAX_DIRS );

	WeightedDirBuilder builder;
	for( int i = 1; i < maxTestedLeafs; ++i ) {
		// The graph edge distance might be (temporarily) scaled.
		// However infinity values are preserved.
		if( std::isinf( m_graph.EdgeDistance( leafsChain[0], leafsChain[i] ) ) ) {
			// If there were added dirs, stop accumulating dirs
			if( i > 1 ) {
				break;
			}

			// Just return a dir from the first leaf to the next leaf without involving the dir builder
			// We do not even check visibility here as we have to provide some valid normalized dir
			// This should not be confusing to a listener as its very likely that secondary emission rays
			// can pass for the most part and can have much greater contribution to an actually used fake source dir.

			// Lets hope this happens rarely enough to avoid caching leaf centers
			vec3_t centers[2] {};
			for( int j = 0; j < 2; ++j ) {
				const vec3_t *const bounds = S_GetLeafBounds( leafsChain[i] );
				VectorSubtract( bounds[1], bounds[0], centers[i] );
				VectorScale( centers[i], 0.5f, centers[i] );
				VectorAdd( centers[i], bounds[0], centers[i] );
			}

			VectorSubtract( centers[1], centers[0], allocatedDir );
			VectorNormalize( allocatedDir );
			return;
		}

		// Continue accumulating dirs coming from other leafs to the first one.
		vec3_t dir { 0.0f, 0.0f, 0.0f };
		if( !m_graph.GetDirFromLeafToLeaf( leafsChain[i], leafsChain[0], dir ) ) {
			assert( 0 && "Should not be reached" );
		}

		// The dir must be present as there is a finite edge distance between leaves

		// We dropped using a distance from leaf to the first leaf
		// as a contribution weight as this distance may be scaled
		// and we do not longer cache leaf centers to compute a raw 3D distance
		float dirWeight = 1.0f - (float)i / (float)maxTestedLeafs;
		dirWeight *= dirWeight;
		// Avoid zero/very small weights diminishing dir contribution
		dirWeight += 0.25f;
		float *const dirToAdd = builder.AllocDir( dirWeight );
		VectorCopy( dir, dirToAdd );
	}

	// Build a result based on all accumulated dirs
	builder.BuildDir( allocatedDir );
}

class PropagationTableReader: public CachedComputationReader {
	bool ValidateTable( PropagationTable::PropagationProps *propsData, int actualNumLeafs );
public:
	PropagationTableReader( const PropagationTable *parent_, int fsFlags )
		: CachedComputationReader( parent_, fsFlags ) {}

	PodBufferHolder<PropagationTable::PropagationProps> ReadPropsTable( int actualNumLeafs );
};

class PropagationTableWriter: public CachedComputationWriter {
public:
	explicit PropagationTableWriter( const PropagationTable *parent_ )
		: CachedComputationWriter( parent_ ) {}

	bool WriteTable( const PropagationTable::PropagationProps *table, int numLeafs );
};

static SingletonHolder<PropagationTable> propagationTableHolder;

PropagationTable *PropagationTable::Instance() {
	return propagationTableHolder.instance();
}

void PropagationTable::Init() {
	propagationTableHolder.init();
}

void PropagationTable::Shutdown() {
	propagationTableHolder.shutdown();
}

inline void PropagationTable::PropagationProps::SetDir( const vec3_t dir ) {
	int byte = ::DirToByteFast( dir );
	assert( (unsigned)byte < std::numeric_limits<uint8_t>::max() - 1 );
	m_maybeDirByte = (uint8_t)byte;
}

inline void PropagationTable::PropagationProps::SetIndirectPath( const vec3_t dir, float distance ) {
	SetDistance( distance );
	SetDir( dir );
}

bool PropagationTable::TryReadFromFile( int fsFlags ) {
	PropagationTableReader reader( this, fsFlags );
	return ( this->m_table = reader.ReadPropsTable( NumLeafs() ) ).get() != nullptr;
}

bool PropagationTable::ComputeNewState( bool fastAndCoarse ) {
	if( fastAndCoarse ) {
		CoarsePropagationBuilder builder( NumLeafs() );
		if( builder.Build() ) {
			m_table = builder.ReleaseOwnership();
			return true;
		}
		return false;
	}

	FinePropagationBuilder builder( NumLeafs() );
	if( builder.Build() ) {
		m_table = builder.ReleaseOwnership();
		return true;
	}

	return false;
}

void PropagationTable::ProvideDummyData() {
	m_table.reserveZeroed( NumLeafs() * NumLeafs() );
}

bool PropagationTable::SaveToCache() {
	if( !NumLeafs() ) {
		return true;
	}

	PropagationTableWriter writer( this );
	return writer.WriteTable( this->m_table.get(), NumLeafs() );
}

bool PropagationTableReader::ValidateTable( PropagationTable::PropagationProps *propsData, int actualNumLeafs ) {
	const int maxByteValue = std::numeric_limits<uint8_t>::max();
	for( int i = 0, end = actualNumLeafs * actualNumLeafs; i < end; ++i ) {
		int dirByte = propsData[i].m_maybeDirByte;
		if( !::IsValidDirByte( dirByte ) ) {
			if( dirByte != maxByteValue && dirByte != maxByteValue - 1 ) {
				return false;
			}
		}
	}
	return true;
}

PodBufferHolder<PropagationTable::PropagationProps> PropagationTableReader::ReadPropsTable( int actualNumLeafs ) {
	// Sanity check
	assert( actualNumLeafs > 0 && actualNumLeafs < ( 1 << 20 ) );

	if( fsResult < 0 ) {
		return {};
	}

	int32_t savedNumLeafs;
	if( !ReadInt32( &savedNumLeafs ) ) {
		fsResult = -1;
		return {};
	}

	if( savedNumLeafs != actualNumLeafs ) {
		fsResult = -1;
		return {};
	}

	// TODO:... this is pretty bad..
	// Just return a view of the file data that is read and is kept in-memory.
	// An overhead of storing few extra strings at the beginning is insignificant.

	PodBufferHolder<PropagationTable::PropagationProps> result;
	result.reserve( actualNumLeafs * actualNumLeafs );
	if( Read( result.get(), sizeof( PropagationTable::PropagationProps ) * actualNumLeafs * actualNumLeafs ) ) {
		if( ValidateTable( result.get(), actualNumLeafs ) ) {
			return result;
		}
	}

	fsResult = -1;
	return {};
}

bool PropagationTableWriter::WriteTable( const PropagationTable::PropagationProps *table, int numLeafs ) {
	// Sanity check
	assert( numLeafs > 0 && numLeafs < ( 1 << 20 ) );

	if( fsResult < 0 ) {
		return false;
	}

	if( !WriteInt32( numLeafs ) ) {
		return false;
	}

	return Write( table, numLeafs * numLeafs * sizeof( PropagationTable::PropagationProps ) );
}