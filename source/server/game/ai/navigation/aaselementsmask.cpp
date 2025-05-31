#include "aasworld.h"
#include "aaselementsmask.h"

BitVector *AasElementsMask::areasMask = nullptr;
BitVector *AasElementsMask::facesMask = nullptr;

static wsw::StaticVector<BitVector, 2> bitVectorsHolder;
bool *AasElementsMask::tmpAreasVisRow = nullptr;
bool *AasElementsMask::blockedAreasTable = nullptr;

int AasElementsMask::numAreas = 0;

void AasElementsMask::Init( AiAasWorld *aasWorld ) {
	assert( bitVectorsHolder.empty() );
	const auto worldAreas = aasWorld->getAreas();
	const auto worldFaces = aasWorld->getFaces();

	assert( !worldAreas.empty() );
	assert( !worldFaces.empty() );

	// Every item corresponds to a single bit.
	// We can allocate only with a byte granularity so add one byte for every item.
	const size_t numAreasBytes = ( worldAreas.size() / 8 ) + 4u;
	areasMask = new( bitVectorsHolder.unsafe_grow_back() )BitVector( (uint8_t *)Q_malloc( numAreasBytes ), numAreasBytes );

	const size_t numFacesBytes = ( worldFaces.size() / 8 ) + 4u;
	facesMask = new( bitVectorsHolder.unsafe_grow_back() )BitVector( (uint8_t *)Q_malloc( numFacesBytes ), numFacesBytes );

	numAreas = (int)worldAreas.size();

	tmpAreasVisRow = (bool *)Q_malloc( sizeof( bool ) * numAreas * TMP_ROW_REDUNDANCY_SCALE );
	// Don't share these buffers even it looks doable.
	// It could lead to nasty reentrancy bugs especially considering that
	// both buffers are very likely to be used in blocked areas status determination.
	blockedAreasTable = (bool *)Q_malloc( sizeof( bool ) * numAreas );
}

void AasElementsMask::Shutdown() {
	for( BitVector &bv: bitVectorsHolder ) {
		Q_free( bv.words );
	}

	::bitVectorsHolder.clear();

	// Vectors do not manage the lifetime of supplied scratchpad but the level pool should take care of this
	areasMask = nullptr;
	facesMask = nullptr;

	Q_free( tmpAreasVisRow );
	tmpAreasVisRow = nullptr;

	Q_free( blockedAreasTable );
	blockedAreasTable = nullptr;
}