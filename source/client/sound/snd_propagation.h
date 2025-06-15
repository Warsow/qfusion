#ifndef QFUSION_SND_PROPAGATION_H
#define QFUSION_SND_PROPAGATION_H

#include "snd_local.h"
#include "snd_cached_computation.h"

#include <common/types/podbuffer.h>
#include <common/facilities/tasksystem.h>

class GraphLike {
	friend class CachedLeafsGraph;
	friend class CachedGraphReader;
	friend class CachedGraphWriter;
	friend class PropagationGraphBuilder;
protected:
	PodBuffer<int> m_adjacencyListsData;
	PodBuffer<int> m_adjacencyListsOffsets;
	PodBuffer<float> m_distanceTable;

	int m_numLeafs;

	explicit GraphLike( int numLeafs ): m_numLeafs( numLeafs ) {}
public:
	virtual ~GraphLike() = default;

	int NumLeafs() const { return m_numLeafs; }

	float EdgeDistance( int leaf1, int leaf2 ) const {
		assert( leaf1 > 0 && leaf1 < m_numLeafs );
		assert( leaf2 > 0 && leaf2 < m_numLeafs );
		return m_distanceTable.get()[leaf1 * m_numLeafs + leaf2];
	}

	const int *AdjacencyList( int leafNum ) const {
		assert( leafNum > 0 && leafNum < m_numLeafs );
		return m_adjacencyListsData.get() + m_adjacencyListsOffsets.get()[leafNum];
	}
};

class CachedLeafsGraph: public CachedComputation, public GraphLike {
	friend class PropagationTable;
	friend class CachedGraphReader;
	friend class CachedGraphWriter;
	template <typename> friend class SingletonHolder;
	friend class PropagationGraphBuilder;

	PodBuffer<uint8_t> m_dirsTable;

	void ResetExistingState() override;
	bool TryReadFromFile( int fsFlags ) override;
	bool ComputeNewState( bool fastAndCoarse ) override;
	void ProvideDummyData() override;
	bool SaveToCache() override;

	CachedLeafsGraph()
		: CachedComputation( "CachedLeafsGraph", ".graph", "CachedLeafsGraph@v1338" )
		, GraphLike( -1 ) {}
public:
	/**
	 * A helper that resolves ambiguous calls of {@code NumLeafs()} of both base classes.
	 */
	int NumLeafs() const { return ( (GraphLike *)this)->NumLeafs(); }

	static CachedLeafsGraph *Instance();
	static void Init();
	static void Shutdown();
};

class PropagationTable: public CachedComputation {
	friend class PropagationIOHelper;
	friend class PropagationTableReader;
	friend class PropagationTableWriter;
	friend class PropagationTableBuilder;
	friend class PropagationBuilderThreadState;
	friend class CoarsePropagationThreadState;
	friend class FinePropagationThreadState;
	friend class CachedLeafsGraph;
	template <typename> friend class SingletonHolder;

	struct alignas( 1 )PropagationProps {
		/**
		 * An index for {@code ByteToDir()} if is within {@code [0, MAXVERTEXNORMALS)} range.
		 */
		uint8_t m_maybeDirByte;
		/**
		 * An rough exponential encoding of an indirect path
		 */
		uint8_t m_distanceByte;

		bool HasDirectPath() const { return m_maybeDirByte == std::numeric_limits<uint8_t>::max(); }
		bool HasIndirectPath() const { return m_maybeDirByte < std::numeric_limits<uint8_t>::max() - 1; }
		void SetHasDirectPath() { m_maybeDirByte = std::numeric_limits<uint8_t>::max(); }
		void MarkAsFailed() { m_maybeDirByte = std::numeric_limits<uint8_t>::max() - 1; }

		inline void SetIndirectPath( const vec3_t dir, float distance );

		/**
		 * Implemented in the source as some things related to implementation should not be exposed right now
		 */
		inline void SetDir( const vec3_t dir );

		void GetDir( vec3_t dir ) const {
			assert( HasIndirectPath() );
			ByteToDir( m_maybeDirByte, dir );
		}

		void SetDistance( float distance ) {
			assert( distance > 0 );
			auto u = (unsigned)distance;
			// Limit the stored distance to 2^16 - 1
			clamp_high( u, ( 1u << 16u ) - 1 );
			// Store the distance using 256 units granularity
			u >>= 8;
			assert( u >= 0 && u < 256 );
			// Make sure that we do not lose the property of distance being positive.
			// Otherwise validation fails (while computations were perfect up to this).
			clamp_low( u, 1u );
			m_distanceByte = (uint8_t)u;
		}

		float GetDistance() const { return m_distanceByte * 256.0f; }
	};

	static_assert( alignof( PropagationProps ) == 1 );
	static_assert( sizeof( PropagationProps ) == 2 );

	PodBuffer<PropagationProps> m_table;

	const PropagationProps &GetProps( int fromLeafNum, int toLeafNum ) const {
		const auto numLeafs = NumLeafs();
		assert( numLeafs );
		assert( fromLeafNum > 0 && fromLeafNum < numLeafs );
		assert( toLeafNum > 0 && toLeafNum < numLeafs );
		return m_table.get()[numLeafs * fromLeafNum + toLeafNum];
	}

	void Clear() {
		m_table = PodBuffer<PropagationProps>();
	}

	void ResetExistingState() override {
		Clear();
	}

	bool TryReadFromFile( int fsFlags ) override;
	bool ComputeNewState( bool fastAndCoarse ) override;
	void ProvideDummyData() override;
	bool SaveToCache() override;
public:
	PropagationTable(): CachedComputation( "PropagationTable", ".table", "PropagationTable@v1338" ) {}

	~PropagationTable() override {
		Clear();
	}

	bool IsValid() const { return m_table.get() != nullptr; }

	/**
	 * Returns true if a direct (ray-like) path between these leaves exists.
	 * @note true results of {@code HasDirectPath()} and {@code HasIndirectPath} are mutually exclusive.
	 */
	bool HasDirectPath( int fromLeafNum, int toLeafNum ) const {
		return fromLeafNum == toLeafNum || GetProps( fromLeafNum, toLeafNum ).HasDirectPath();
	}

	/**
	 * Returns true if an indirect (maze-like) path between these leaves exists.
	 * @note true results of {@code HasDirectPath()} and {@code HasIndirectPath} are mutually exclusive.
	 */
	bool HasIndirectPath( int fromLeafNum, int toLeafNum ) const {
		return fromLeafNum != toLeafNum && GetProps( fromLeafNum, toLeafNum ).HasIndirectPath();
	}

	/**
	 * Returns propagation properties of an indirect (maze) path between these leaves.
	 * @param fromLeafNum a number of leaf where a real sound emitter origin is assumed to be located.
	 * @param toLeafNum a number of leaf where a listener origin is assumed to be located.
	 * @param dir an average direction of sound waves emitted by the source and ingoing to the listener leaf.
	 * @param distance a coarse estimation of distance that is covered by sound waves during propagation.
	 * @return true if an indirect path between given leaves exists (and there were propagation properties).
	 */
	bool GetIndirectPathProps( int fromLeafNum, int toLeafNum, vec3_t dir, float *distance ) const {
		if( fromLeafNum == toLeafNum ) {
			return false;
		}
		const auto &props = GetProps( fromLeafNum, toLeafNum );
		if( !props.HasIndirectPath() ) {
			return false;
		}
		props.GetDir( dir );
		*distance = props.GetDistance();
		return true;
	}

	static PropagationTable *Instance();
	static void Init();
	static void Shutdown();
};

class PropagationGraphBuilder: public GraphLike {
	friend class CachedLeafsGraph;
	friend class PropagationBuilderThreadState;
public:
	PropagationGraphBuilder( int numLeafs, bool fastAndCoarse ) : GraphLike( numLeafs ), m_fastAndCoarse( fastAndCoarse ) {}

	void SetEdgeDistance( int leaf1, int leaf2, float newDistance ) {
		// Template quirks: a member of a template base cannot be resolved in scope otherwise
		const int numLeafs  = this->m_numLeafs;
		auto *distanceTable = this->m_distanceTable.get();
		// The distance table must point at the scratchpad
		assert( leaf1 > 0 && leaf1 < numLeafs );
		assert( leaf2 > 0 && leaf2 < numLeafs );
		distanceTable[leaf1 * numLeafs + leaf2] = newDistance;
		distanceTable[leaf2 * numLeafs + leaf1] = newDistance;
	}

	void ScaleEdgeDistance( int leaf1, int leaf2, float scale ) {
		const int numLeafs        = this->m_numLeafs;
		auto *const distanceTable = this->m_distanceTable.get();
		assert( leaf1 > 0 && leaf1 < numLeafs );
		assert( leaf2 > 0 && leaf2 < numLeafs );
		distanceTable[leaf1 * numLeafs + leaf2] *= scale;
		distanceTable[leaf2 * numLeafs + leaf1] *= scale;
	}

	void SaveDistanceTable() {
		const float *src = m_distanceTable.get();
		float *const dst = m_distanceTableScratchpad.reserveAndGet( m_numLeafs * m_numLeafs );
		std::memcpy( dst, src, sizeof( float ) * m_numLeafs * m_numLeafs );
		std::swap( m_distanceTable, m_distanceTableScratchpad );
	}

	void RestoreDistanceTable() {
		std::swap( m_distanceTable, m_distanceTableScratchpad );
	}

	bool TryUsingGlobalGraph( GraphLike *target );

	/**
	 * Tries to build the graph data (or reuse data from the global graph).
	 */
	bool Build( GraphLike *target = nullptr );

	/**
	 * Returns an average propagation direction from leaf1 to leaf2.
	 * @param leaf1 a first leaf.
	 * @param leaf2 a second leaf.
	 * @param reuse a buffer for a valid result.
	 * @return a valid address of a direction vector on success, a null on failure
	 * @note a presence of a valid dir is symmetrical for any pair of leaves.
	 * @note a valid dir magnitude is symmetrical for any pair of leaves such that a valid dir exists for the pair.
	 */
	const float *GetDirFromLeafToLeaf( int leaf1, int leaf2, vec3_t reuse ) const;
private:

	PodBuffer<float> m_distanceTableScratchpad;
	PodBuffer<uint8_t> m_dirsTable;
	const bool m_fastAndCoarse;
};

#endif
