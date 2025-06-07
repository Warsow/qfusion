#include "snd_propagation.h"
#include <common/helpers/singletonholder.h>

#include <random>

/**
 * This is a helper for {@PropagationGraphBuilder::PrepareToBuild()} algorithms.
 * Putting these algorithms in a separate class serves two purposes:
 */
class LeafToLeafDirBuilder {
	trace_t trace;
	std::minstd_rand randomnessEngine;
	vec3_t leafCenters[2];
	vec3_t leafDimensions[2];
	vec3_t leafPoints[2];
	const int maxAdditionalAttempts;

	/**
	 * This is a helper that tries to pick a random point within a leaf bounds.
	 * A picking might fail for degenerate leaves of some kind.
	 * @param leafNum an actual leaf num in the collision world.
	 * @param storageIndex an index of internal storage for first or second leaf (0 or 1)
	 * @param topNodeHint a top node to start BSP traversal while testing point CM leaf num
	 * @return false if a point picking has failed. A dir building should be interrupted in this case.
	 */
	bool PrepareTestedPointForLeaf( int leafNum, int storageIndex, int topNodeHint );
public:
	explicit LeafToLeafDirBuilder( bool fastAndCoarse_, int numLeafs_ )
		: maxAdditionalAttempts( fastAndCoarse_ ? ( numLeafs_ > 2000 ? 16 : 32 ) : 256 ) {
		// Note: we actually are cutting off additional attempts for complex maps.
		// This is a hack to prevent computational explosion until more efficient algorithms are introduced
		// (switching from Dijkstra to bidirectional Dijkstra was good but not sufficient)
	}

	/**
	 * Tries to test propagation for a given leaves pair.
	 * @param leaf1 a first leaf.
	 * @param leaf2 a second leaf.
	 * @param resultDir a buffer for a propagation dir.
	 * @return an average distance for propagation between leaves on success, infinity on failure.
	 */
	float Build( int leaf1, int leaf2, vec3_t resultDir );
};

bool LeafToLeafDirBuilder::PrepareTestedPointForLeaf( int leafNum, int storageIndex, int topNodeHint ) {
	float *const point = leafPoints[storageIndex];
	const auto randomShift = (float)std::minstd_rand::min();
	const auto randomScale = 1.0f / ( std::minstd_rand::max() - randomShift );
	// Try 6 attempts to pick a random point within a leaf bounds
	for( int attemptNum = 0; attemptNum < 6; ++attemptNum ) {
		for( int i = 0; i < 3; ++i ) {
			point[i] = leafCenters[storageIndex][i];
			float random01 = randomScale * ( randomnessEngine() - randomShift );
			point[i] += -0.5f + random01 * leafDimensions[storageIndex][i];
		}
		if( S_PointLeafNum( point, topNodeHint ) == leafNum ) {
			return true;
		}
	}
	return false;
}

float LeafToLeafDirBuilder::Build( int leaf1, int leaf2, vec3_t resultDir ) {
	if( !S_LeafsInPVS( leaf1, leaf2 ) ) {
		return std::numeric_limits<float>::infinity();
	}

	const vec3_t *const leafBounds[2] = { S_GetLeafBounds( leaf1 ), S_GetLeafBounds( leaf2 ) };

	// Add a protection against bogus leaves
	if( VectorCompare( leafBounds[0][0], leafBounds[1][0] ) && VectorCompare( leafBounds[1][0], leafBounds[1][1] ) ) {
		return std::numeric_limits<float>::infinity();
	}

	vec3_t nodeHintBounds[2];
	ClearBounds( nodeHintBounds[0], nodeHintBounds[1] );
	for( int i = 0; i < 2; ++i ) {
		// Get dimensions
		VectorSubtract( leafBounds[i][1], leafBounds[i][0], leafCenters[i] );
		VectorCopy( leafCenters[i], leafDimensions[i] );
		// Get half-dimensions
		VectorScale( leafCenters[i], 0.5f, leafCenters[i] );
		// Add mins
		VectorAdd( leafCenters[i], leafBounds[i][0], leafCenters[i] );

		// Build bounds for top node hint
		AddPointToBounds( leafBounds[i][0], nodeHintBounds[0], nodeHintBounds[1] );
		AddPointToBounds( leafBounds[i][1], nodeHintBounds[0], nodeHintBounds[1] );
	}

	// Prepare for adding dir contributions
	VectorClear( resultDir );
	bool hasContributingDirs = false;

	const int topNodeHint = S_FindTopNodeForBox( nodeHintBounds[0], nodeHintBounds[1] );
	// Cast a ray from a leaf center to another leaf center.
	// Do not test whether these centers really belong to a leaf
	// (we remember this happening a lot for (almost) degenerate leaves while computing LeafPropsCache).
	S_Trace( &trace, leafCenters[0], leafCenters[1], vec3_origin, vec3_origin, MASK_SOLID, topNodeHint );
	if( trace.fraction == 1.0f ) {
		// Add center-to-center dir contribution.
		VectorSubtract( leafCenters[1], leafCenters[0], resultDir );
		float squareLength = VectorLengthSquared( resultDir );
		if( squareLength > 1 ) {
			// Give this dir a 3x greater weight
			float scale = 3.0f / std::sqrt( squareLength );
			VectorScale( resultDir, scale, resultDir );
			hasContributingDirs = true;
		}
	}

	for( int attemptNum = 0; attemptNum < this->maxAdditionalAttempts; ++attemptNum ) {
		int leaves[2] = { leaf1, leaf2 };
		for( int j = 0; j < 2; ++j ) {
			// For every leaf try picking random points within leaf bounds.
			// Stop doing attempts immediately on failure
			// (we are very likely have met another kind of a degenerate leaf).
			// TODO: We can try reusing picked leaf points from LeafPropsCache
			if( !this->PrepareTestedPointForLeaf( leaves[j], j, topNodeHint ) ) {
				goto done;
			}
		}

		S_Trace( &trace, leafPoints[0], leafPoints[1], vec3_origin, vec3_origin, MASK_SOLID, topNodeHint );
		if( trace.fraction != 1.0f ) {
			continue;
		}

		vec3_t tmp;
		// Try adding contribution of dir from first to second leaf
		VectorSubtract( leafPoints[1], leafPoints[0], tmp );
		const float squareDistance = VectorLengthSquared( tmp );
		if( squareDistance < 1 ) {
			continue;
		}

		const float scale = 1.0f / std::sqrt( squareDistance );
		VectorScale( tmp, scale, tmp );
		VectorAdd( tmp, resultDir, resultDir );
		hasContributingDirs = true;
	}

	done:
	if( !hasContributingDirs ) {
		return std::numeric_limits<float>::infinity();
	}

	// Must always produce a valid normalized dir once we have reached here
	VectorNormalize( resultDir );
	// Check normalization
	assert( std::abs( std::sqrt( VectorLengthSquared( resultDir ) ) - 1.0f ) < 0.2f );
	// Always return a plain distance between leaf centers.
	return std::sqrt( DistanceSquared( leafCenters[0], leafCenters[1] ) );
}

bool PropagationGraphBuilder::Build( GraphLike *target ) {
	// Should not be called for empty graphs
	assert( this->m_numLeafs > 0 );

	if( !target ) {
		target = this;
	}

	if( TryUsingGlobalGraph( target ) ) {
		return true;
	}

	int numTableCells = this->NumLeafs() * this->NumLeafs();
	// We can't allocate "backup" and "scratchpad" in the same chunk
	// as we want to be able to share the immutable table data ("backup")
	// Its still doable by providing a custom allocator.

	m_distanceTable.reserveZeroed( numTableCells );

	const int numLeafs = m_numLeafs;

	LeafToLeafDirBuilder dirBuilder( m_fastAndCoarse, numLeafs );

	const int dirsTableSide = numLeafs - 1;
	m_dirsTable.reserve( dirsTableSide * ( dirsTableSide - 1 ) / 2 );

	uint8_t *dirsDataPtr = m_dirsTable.get();
	float *distanceTable = m_distanceTable.get();

	for( int i = 1; i < numLeafs; ++i ) {
		for( int j = i + 1; j < numLeafs; ++j ) {
			vec3_t dir;
			const float distance = dirBuilder.Build( i, j, dir );
			distanceTable[i * numLeafs + j] = distance;
			distanceTable[j * numLeafs + i] = distance;
			if( !std::isfinite( distance ) ) {
				*dirsDataPtr++ = std::numeric_limits<uint8_t>::max();
				// Check immediately
				assert( !GetDirFromLeafToLeaf( i, j, dir ) );
				continue;
			}
			*dirsDataPtr++ = (uint8_t)::DirToByteFast( dir );
			// Check immediately
			assert( GetDirFromLeafToLeaf( i, j, dir ) );
		}
	}
	assert( dirsDataPtr - m_dirsTable.get() == dirsTableSide * ( dirsTableSide - 1 ) / 2 );

	size_t numAdjacencyElems = 0;
	for( int i = 1; i < numLeafs; ++i ) {
		int rowOffset = i * numLeafs;
		for( int j = 1; j < i; ++j ) {
			if( std::isfinite( distanceTable[rowOffset + j] ) ) {
				numAdjacencyElems++;
			}
		}
		for( int j = i + 1; j < numLeafs; ++j ) {
			if( std::isfinite( distanceTable[rowOffset + j] ) ) {
				numAdjacencyElems++;
			}
		}
	}

	// The additional cell for a leaf is for a size "prefix" of adjacency list
	numAdjacencyElems += numLeafs;

	m_adjacencyListsData.reserve( numAdjacencyElems );
	m_adjacencyListsOffsets.reserve( numLeafs );

	auto *const adjacencyListsData    = m_adjacencyListsData.get();
	auto *const adjacencyListsOffsets = m_adjacencyListsOffsets.get();

	int *dataPtr = adjacencyListsData;
	// Write a zero-length list for the zero leaf
	*dataPtr++ = 0;
	adjacencyListsOffsets[0] = 0;

	for( int i = 1; i < numLeafs; ++i ) {
		int rowOffset = i * numLeafs;
		// Save a position of the list length
		int *const listLengthRef = dataPtr++;
		for( int j = 1; j < i; ++j ) {
			if( std::isfinite( distanceTable[rowOffset + j] ) ) {
				*dataPtr++ = j;
			}
		}
		for( int j = i + 1; j < numLeafs; ++j ) {
			if( std::isfinite( distanceTable[rowOffset + j] ) ) {
				*dataPtr++ = j;
			}
		}
		adjacencyListsOffsets[i] = (int)( listLengthRef - adjacencyListsData );
		*listLengthRef = (int)( dataPtr - listLengthRef - 1 );
	}

	assert( numAdjacencyElems == (size_t)( dataPtr - adjacencyListsData ) );
	return true;
}

const float *PropagationGraphBuilder::GetDirFromLeafToLeaf( int leaf1, int leaf2, vec_t *reuse ) const {
	assert( leaf1 != leaf2 );
	const int numLeafs = this->NumLeafs();
	assert( leaf1 > 0 && leaf1 < numLeafs );
	assert( leaf2 > 0 && leaf2 < numLeafs );

	// We do not store dummy rows/columns for a zero leaf so we have to shift leaf numbers and table side
	leaf1--;
	leaf2--;
	const int tableSide = numLeafs - 1;
	assert( tableSide > 0 );

	// We store only data of the upper triangle

	// *  X  X  X  X
	// -  *  X  X  X
	// -  -  *  X  X
	// -  -  -  *  X
	// -  -  -  -  *

	// Let N be the table side
	// An offset for the 0-th row data is 0
	// An offset for the 1-st row data is N - 1
	// An offset for the 2-nd row data is ( N - 1 ) + ( N - 2 )
	// An offset for the 3-rd row data is ( N - 1 ) + ( N - 2 ) + ( N - 3 )
	// An offset for the 4-th row data is ( N - 1 ) + ( N - 2 ) + ( N - 3 ) + ( N - 4 )
	// ( N - 1 ) + ( N - 2 ) + ( N - 3 ) + ( N - 4 ) = 4 * N - ( 1 + 2 + 3 + 4 ) = 4 * N - 4 * ( 1 + 4 ) / 2
	// Let R be the row number
	// Thus row offset = R * N - R * ( 1 + R ) / 2

	// Use an anti-symmetrical property of the leaf-to-leaf dir relation while trying to access the lower triangle
	float sign = 1.0f;
	if( leaf1 > leaf2 ) {
		std::swap( leaf1, leaf2 );
		sign = -1.0f;
	}

	const int rowOffset = leaf1 * tableSide - ( leaf1 * ( 1 + leaf1 ) ) / 2;
	// We iterate over cells in the upper triangle starting from i + 1 for i-th row.
	// Remember that leaf1 corresponds to row index and leaf2 corresponds to column index.
	const int indexInRow = leaf2 - leaf1 - 1;
	const uint8_t dirByte = m_dirsTable.get()[rowOffset + indexInRow];
	if( dirByte != std::numeric_limits<uint8_t>::max() ) {
		::ByteToDir( dirByte, reuse );
		VectorScale( reuse, sign, reuse );
		return reuse;
	}
	return nullptr;
}

class CachedGraphReader: public CachedComputationReader {
public:
	CachedGraphReader( const CachedLeafsGraph *parent_, int fsFlags )
		: CachedComputationReader( parent_, fsFlags ) {}

	bool Read( CachedLeafsGraph *readObject );
};

class CachedGraphWriter: public CachedComputationWriter {
public:
	explicit CachedGraphWriter( const CachedLeafsGraph *parent_ )
		: CachedComputationWriter( parent_ ) {}

	bool Write( const CachedLeafsGraph *writtenObject );
};

static SingletonHolder<CachedLeafsGraph> leafsGraphHolder;

CachedLeafsGraph *CachedLeafsGraph::Instance() {
	return leafsGraphHolder.instance();
}

void CachedLeafsGraph::Init() {
	leafsGraphHolder.init();
}

void CachedLeafsGraph::Shutdown() {
	leafsGraphHolder.shutdown();
}

void CachedLeafsGraph::ResetExistingState() {
	m_distanceTable         = PodBufferHolder<float>();
	m_dirsTable             = PodBufferHolder<uint8_t>();
	m_adjacencyListsData    = PodBufferHolder<int>();
	m_adjacencyListsOffsets = PodBufferHolder<int>();
}

bool CachedLeafsGraph::TryReadFromFile( int fsFlags ) {
	CachedGraphReader reader( this, fsFlags );
	return reader.Read( this );
}

bool CachedLeafsGraph::ComputeNewState( bool fastAndCoarse_ ) {
	const int actualNumLeafs = CachedComputation::NumLeafs();
	// Always set the number of leafs for the graph even if we have not managed to build the graph.
	// The number of leafs in the CachedComputation will be always set by its EnsureValid() logic.
	// Hack... we have to resolve multiple inheritance ambiguity.
	( ( GraphLike *)this)->m_numLeafs = actualNumLeafs;

	PropagationGraphBuilder builder( actualNumLeafs, fastAndCoarse_ );
	// Specify "this" as a target to suppress an infinite recursion while trying to reuse the global graph
	if( !builder.Build( this ) ) {
		return false;
	}

	m_distanceTable         = std::move( builder.m_distanceTable );
	m_adjacencyListsOffsets = std::move( builder.m_adjacencyListsOffsets );
	m_adjacencyListsData    = std::move( builder.m_adjacencyListsData );
	m_dirsTable             = std::move( builder.m_dirsTable );

	return true;
}

void CachedLeafsGraph::ProvideDummyData() {
	m_distanceTable.reserveZeroed( 1 );
	m_dirsTable.reserveZeroed( 1 );
	m_adjacencyListsData.reserveZeroed( 1 );
	m_adjacencyListsOffsets.reserveZeroed( NumLeafs() );
}

bool CachedLeafsGraph::SaveToCache() {
	CachedGraphWriter writer( this );
	return writer.Write( this );
}

bool CachedGraphReader::Read( CachedLeafsGraph *readObject ) {
	if( fsResult < 0 ) {
		return false;
	}

	int32_t numLeafs;
	if( !ReadInt32( &numLeafs ) ) {
		return false;
	}

	// Sanity check
	if( numLeafs < 1 || numLeafs > ( 1 << 24 ) ) {
		return false;
	}

	int32_t listsDataSize = 0;
	if( !ReadInt32( &listsDataSize ) ) {
		return false;
	}

	const size_t numBytesForDistanceTable = numLeafs * numLeafs * sizeof( float );
	// Dummy rows/columns for zero leaf are not stored.
	// Only the upper triangle above the table diagonal is stored.
	const int dirsTableSide = numLeafs - 1;
	const int numStoredDirs = ( dirsTableSide * ( dirsTableSide - 1 ) ) / 2;
	const size_t numBytesForDirsTable = sizeof( uint8_t ) * numStoredDirs;
	const size_t numBytesForLists = ( listsDataSize + numLeafs ) * sizeof( int );
	if( BytesLeft() != numBytesForDistanceTable + numBytesForDirsTable + numBytesForLists ) {
		return false;
	}

	PodBufferHolder<float> distanceTableHolder;
	distanceTableHolder.reserve( numLeafs * numLeafs );
	if( !CachedComputationReader::Read( distanceTableHolder.get(), numBytesForDistanceTable ) ) {
		return false;
	}

	PodBufferHolder<uint8_t> dirsTableHolder;
	dirsTableHolder.reserve( numLeafs * numLeafs );
	if( !CachedComputationReader::Read( dirsTableHolder.get(), numBytesForDirsTable ) ) {
		return false;
	}

	// Validate dirs
	for( int i = 0; i < numStoredDirs; ++i ) {
		if( const uint8_t maybeDirByte = dirsTableHolder.get()[i]; !::IsValidDirByte( maybeDirByte ) ) {
			if( maybeDirByte != std::numeric_limits<uint8_t>::max() ) {
				return false;
			}
		}
	}

	PodBufferHolder<int> adjacencyListsData, adjacencyListsOffsets;
	adjacencyListsData.reserve( listsDataSize );
	adjacencyListsOffsets.reserve( numLeafs );

	if( !CachedComputationReader::Read( adjacencyListsData.get(), sizeof( int ) * listsDataSize ) ) {
		return false;
	}
	if( !CachedComputationReader::Read( adjacencyListsOffsets.get(), sizeof( int ) * numLeafs ) ) {
		return false;
	}

	int *const listsDataBegin = adjacencyListsData.get();
	int *const listsDataEnd   = adjacencyListsData.get() + listsDataSize;

	// Byte-swap and validate offsets
	int prevOffset = 0;
	auto *offsets = adjacencyListsOffsets.get();
	for( int i = 1; i < numLeafs; ++i ) {
		offsets[i] = LittleLong( offsets[i] );
		if( offsets[i] < 0 ) {
			return false;
		}
		if( offsets[i] >= listsDataSize ) {
			return false;
		}
		if( offsets[i] <= prevOffset ) {
			return false;
		}
		prevOffset = offsets[i];
	}

	// Byte-swap and validate lists
	// Start from the list for element #1
	// Retrieval data for a zero leaf is illegal anyway.
	const int *expectedNextListAddress = listsDataBegin + 1;
	for( int i = 1; i < numLeafs; ++i ) {
		// We have ensured this offset is valid
		int *list = listsDataBegin + offsets[i];
		// Check whether the list follows the previous list
		if( list != expectedNextListAddress ) {
			return false;
		}
		// Swap bytes in the memory, copy to a local variable only after that!
		*list = LittleLong( *list );
		// The first element of the list is it's size. Check it for sanity first.
		const int listSize = *list++;
		if( listSize < 0 || listSize > ( 1 << 24 ) ) {
			return false;
		}
		// Check whether accessing elements in range defined by this size is allowed.
		if( list + listSize > listsDataEnd ) {
			return false;
		}
		for( int j = 0; j < listSize; ++j ) {
			list[j] = LittleLong( list[j] );
			// Check whether its a valid non-zero leaf
			if( list[j] < 1 || list[j] >= numLeafs ) {
				return false;
			}
		}
		expectedNextListAddress = list + listSize;
	}

	for( int i = 0, end = numLeafs * numLeafs; i < end; ++i ) {
		distanceTableHolder.get()[i] = LittleLong( distanceTableHolder.get()[i] );
	}

	readObject->m_distanceTable         = std::move( distanceTableHolder );
	readObject->m_dirsTable             = std::move( dirsTableHolder );
	readObject->m_adjacencyListsData    = std::move( adjacencyListsData );
	readObject->m_adjacencyListsOffsets = std::move( adjacencyListsOffsets );
	( (GraphLike *)readObject )->m_numLeafs = numLeafs;

	return true;
}

bool CachedGraphWriter::Write( const CachedLeafsGraph *writtenObject ) {
	static_assert( sizeof( int32_t ) == sizeof( int ) );

	const int numLeafs = writtenObject->NumLeafs();
	if( !WriteInt32( numLeafs ) ) {
		return false;
	}

	const auto lastOffset = writtenObject->m_adjacencyListsOffsets.get()[numLeafs - 1];
	// Add the size of the last list (the first element in the array), +1 for the leading array size element
	const int listsDataSize     = lastOffset + writtenObject->m_adjacencyListsData.get()[lastOffset] + 1;

	if( !WriteInt32( listsDataSize ) ) {
		return false;
	}

	if( !CachedComputationWriter::Write( writtenObject->m_distanceTable.get(), numLeafs * numLeafs * sizeof( float ) ) ) {
		return false;
	}

	const int dirsTableSide   = numLeafs - 1;
	const int numStoredDirs   = ( dirsTableSide * ( dirsTableSide - 1 ) ) / 2;
	const size_t dirsDataSize = sizeof( uint8_t ) * numStoredDirs;

	if( !CachedComputationWriter::Write( writtenObject->m_dirsTable.get(), dirsDataSize ) ) {
		return false;
	}

	if( !CachedComputationWriter::Write( writtenObject->m_adjacencyListsData.get(), sizeof( int ) * listsDataSize ) ) {
		return false;
	}

	return CachedComputationWriter::Write( writtenObject->m_adjacencyListsOffsets.get(), sizeof( int ) * numLeafs );
}

bool PropagationGraphBuilder::TryUsingGlobalGraph( GraphLike *target ) {
	auto *const globalGraph = CachedLeafsGraph::Instance();
	// Can't be used for the global graph itself (falls into an infinite recursion)
	// WARNING! We have to force the desired type of the object first to avoid comparison of different pointers,
	// then erase the type to make it compiling. `this` differs in context of different base classes of an object.
	if( static_cast<const GraphLike *>( globalGraph ) == static_cast<const GraphLike *>( target ) ) {
		return false;
	}

	globalGraph->EnsureValid();
	if( !globalGraph->IsUsingValidData() ) {
		return false;
	}

	target->m_distanceTable         = globalGraph->m_distanceTable.makeADeepCopy();
	target->m_adjacencyListsData    = globalGraph->m_adjacencyListsData.makeADeepCopy();
	target->m_adjacencyListsOffsets = globalGraph->m_adjacencyListsOffsets.makeADeepCopy();
	if( auto *targetBuilder = dynamic_cast<PropagationGraphBuilder *>( target ) ) {
		targetBuilder->m_dirsTable = globalGraph->m_dirsTable.makeADeepCopy();
	}

	return true;
}