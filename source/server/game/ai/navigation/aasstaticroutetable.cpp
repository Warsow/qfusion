#include "aasstaticroutetable.h"
#include "aasroutecache.h"
#include "aasworld.h"
#include "../bot.h"
#include "../rewriteme.h"
#include <common/helpers/singletonholder.h>
#include <common/types/vector.h>
#include <cinttypes>

#ifdef WSW_USE_SSE2
#ifdef _MSC_VER
#include <intrin.h>
#else
#include <x86intrin.h>
#endif
#endif

static SingletonHolder<AasStaticRouteTable> g_instanceHolder;
bool AasStaticRouteTable::s_isAccessibleForRouteCache;

#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
bool AasStaticRouteTable::s_isCheckingMatchWithRouteCache;
#endif

void AasStaticRouteTable::init( const char *mapName ) {
	s_isAccessibleForRouteCache = false;
	g_instanceHolder.init( mapName );
	s_isAccessibleForRouteCache = true;
}

void AasStaticRouteTable::shutdown() {
	g_instanceHolder.shutdown();
	s_isAccessibleForRouteCache = false;
}

auto AasStaticRouteTable::instance() -> const AasStaticRouteTable * {
	return g_instanceHolder.instance();
}

AasStaticRouteTable::AasStaticRouteTable( const char *mapName ) {
	if( !AiAasWorld::instance()->isLoaded() ) {
		return;
	}

	wsw::StaticString<MAX_QPATH> filePath;
	filePath << wsw::StringView( "ai/" ) << wsw::StringView( mapName ) << wsw::StringView( ".routetable" );

	if( !loadFromFile( filePath.data() ) ) {
		if( compute() ) {
			if( !saveToFile( filePath.data() ) ) {
				aiError() << "Failed to save the static route table to" << filePath;
			} else {
				aiError() << "Saved the static route table to" << filePath << "successfully";
			}
		} else {
			aiError() << "Failed to compute the static route table";
		}
	} else {
		aiNotice() << "Loaded the static route table from" << filePath << "successfully";
	}
}

AasStaticRouteTable::~AasStaticRouteTable() {
	Q_free( m_dataForAllowedFlags.entries );
	Q_free( m_dataForAllowedFlags.areaNums );
	Q_free( m_walkingAreaNums );
	Q_free( m_walkingTravelTimes );

	Q_free( m_bufferSpans );
}

static constexpr const char *kFileTag   = "RouteTable";
static constexpr const int kFileVersion = 1341;

bool AasStaticRouteTable::loadFromFile( const char *filePath ) {
	AiPrecomputedFileReader reader( kFileTag, kFileVersion );
	if( reader.BeginReading( filePath ) != AiPrecomputedFileReader::SUCCESS ) {
		return false;
	}

	uint16_t *allowedNums = nullptr;
	AreaEntry *allowedEntries = nullptr;
	uint16_t *walkingNums = nullptr, *walkingTimes = nullptr;
	BufferSpansForFlags *spans = nullptr;

	uint32_t numAllowedNums = 0, numAllowedEntries = 0;
	uint32_t numWalkingNums = 0, numWalkingTimes = 0;
	uint32_t numSpans = 0;

	wsw::StaticVector<void *, 5> allocatedData;

	// TODO: The reader is defined in "rewriteme.h" for a reason...
	if( reader.ReadAsTypedBuffer( &allowedNums, &numAllowedNums ) ) {
		allocatedData.push_back( allowedNums );
	}
	if( reader.ReadAsTypedBuffer( &allowedEntries, &numAllowedEntries ) ) {
		allocatedData.push_back( allowedEntries );
	}
	if( reader.ReadAsTypedBuffer( &walkingNums, &numWalkingNums ) ) {
		allocatedData.push_back( walkingNums );
	}
	if( reader.ReadAsTypedBuffer( &walkingTimes, &numWalkingTimes ) ) {
		allocatedData.push_back( walkingTimes );
	}

	if( reader.ReadAsTypedBuffer( &spans, &numSpans ) ) {
		allocatedData.push_back( spans );
	}

	// The second condition is a minimal validation.
	// We should not really care about the latter once a proper checksum handling is implemented.
	if( !allocatedData.full() || ( numSpans != AiAasWorld::instance()->getAreas().size() ) ) {
		for( void *p : allocatedData ) {
			assert( p );
			Q_free( p );
		}
		return false;
	}

	m_dataForAllowedFlags.areaNums = allowedNums;
	m_dataForAllowedFlags.areaNumsDataSizeInElems = numAllowedNums;
	m_dataForAllowedFlags.entries  = allowedEntries;
	m_dataForAllowedFlags.entriesDataSizeInElems = numAllowedEntries;

	m_walkingAreaNums = walkingNums;
	m_walkingAreaNumsDataSizeInElems = numWalkingNums;
	m_walkingTravelTimes = walkingTimes;
	m_walkingTravelTimesDataSizeInElems = numWalkingTimes;

	m_bufferSpans = spans;
	m_numAreas    = numSpans;

	return true;
}

bool AasStaticRouteTable::saveToFile( const char *filePath ) {
	AiPrecomputedFileWriter writer( kFileTag, kFileVersion );
	if( !writer.BeginWriting( filePath ) ) {
		return false;
	}
	if( !writer.WriteTypedBuffer( m_dataForAllowedFlags.areaNums, m_dataForAllowedFlags.areaNumsDataSizeInElems ) ) {
		return false;
	}
	if( !writer.WriteTypedBuffer( m_dataForAllowedFlags.entries, m_dataForAllowedFlags.entriesDataSizeInElems ) ) {
		return false;
	}
	if( !writer.WriteTypedBuffer( m_walkingAreaNums, m_walkingAreaNumsDataSizeInElems ) ) {
		return false;
	}
	if( !writer.WriteTypedBuffer( m_walkingTravelTimes, m_walkingTravelTimesDataSizeInElems ) ) {
		return false;
	}
	if( !writer.WriteTypedBuffer( m_bufferSpans, m_numAreas ) ) {
		return false;
	}
	return true;
}

static constexpr auto kTravelFlagsWalking = TFL_WALK | TFL_AIR | TFL_WALKOFFLEDGE;

[[nodiscard]]
static auto calcTravelTimeWalkingOrFallingShort( AiAasRouteCache *routeCache, const AiAasWorld *aasWorld,
												 int fromAreaNum, int toAreaNum ) -> int {
	const auto *aasReaches = aasWorld->getReaches().data();

	int resultTravelTime = 0;
	// Prevent infinite looping (still happens for some maps)
	// TODO Check whether the bug is still there?
	int numHops = 0;
	for(;; ) {
		if( fromAreaNum == toAreaNum ) {
			return wsw::max( 1, resultTravelTime );
		}
		if( numHops++ == 1024 ) {
			assert( 0 );
			return 0;
		}
		int reachNum = 0;
		const int travelTime = routeCache->FindRoute( fromAreaNum, toAreaNum, kTravelFlagsWalking, &reachNum );
		if( !travelTime ) {
			assert( reachNum == 0 );
			return 0;
		}
		assert( reachNum > 0 );
		// Save the returned travel time once at start.
		if( !resultTravelTime ) {
			resultTravelTime = travelTime;
		}
		const auto &reach = aasReaches[reachNum];
		// Move to this area for the next iteration
		fromAreaNum = reach.areanum;
		// Check whether the travel type fits this function restrictions
		const int travelType = reach.traveltype & TRAVELTYPE_MASK;
		if( travelType == TRAVEL_WALK ) {
			continue;
		}
		if( travelType == TRAVEL_WALKOFFLEDGE ) {
			if( DistanceSquared( reach.start, reach.end ) < wsw::square( 0.8 * AI_JUMPABLE_HEIGHT ) ) {
				continue;
			}
		}
		return 0;
	}
}

// TODO: We can add functionality for taking the pointer out of the PodVector
template <typename T>
[[nodiscard]]
static wsw_forceinline auto makeHeapAllocCopy( const wsw::PodVector<T> &data ) -> T * {
	auto *const result = (T *)Q_malloc( sizeof( T ) * data.size() );
	std::memcpy( result, data.data(), sizeof( T ) * data.size() );
	return result;
}

template <typename T>
struct NumsAndEntriesBuilder {
	wsw::PodVector<uint16_t> nums;
	wsw::PodVector<T> entries;
	uint64_t totalNumEntriesInSpans { 0 };
	uint64_t maxNumEntriesInSpans { 0 };
	uint16_t minNum { 0 }, maxNum { 0 };
	size_t oldNumsSize { 0 }, oldEntriesSize { 0 };

	void beginSpan() {
		oldNumsSize    = nums.size();
		oldEntriesSize = entries.size();
		minNum         = std::numeric_limits<uint16_t>::max();
		maxNum         = 0;
	}

	void addNumAndEntry( uint16_t num, T &&entry ) {
		nums.push_back( num );
		entries.emplace_back( std::forward<T>( entry ) );
		minNum = wsw::min( minNum, num );
		maxNum = wsw::max( maxNum, num );
	}

	[[nodiscard]]
	auto endSpan() -> AasStaticRouteTable::BufferSpan {
		static_assert( sizeof( decltype( nums.front() ) ) == 2 );

		// Ensure that each nums span size is a multiple of 16 (so it is a multiple of a 2x16-byte SIMD vector)
		if( const size_t rem = nums.size() % 16 ) {
			nums.insert( nums.end(), 16 - rem, 0 );
		} else {
			// Ensure that we always can load at least 16 words of area data for comparison
			if( nums.size() == oldNumsSize ) [[unlikely]] {
				nums.insert( nums.end(), 16, 0 );
			}
		}

		const size_t numAreaNumsInSpan = nums.size() - oldNumsSize;
		const size_t numEntriesInSpan  = entries.size() - oldEntriesSize;
		assert( numAreaNumsInSpan <= std::numeric_limits<uint16_t>::max() );
		assert( numEntriesInSpan  <= std::numeric_limits<uint16_t>::max() );
		totalNumEntriesInSpans += numEntriesInSpan;
		maxNumEntriesInSpans = wsw::max( maxNumEntriesInSpans, numEntriesInSpan );

		return AasStaticRouteTable::BufferSpan {
			.areaNumsOffset    = (unsigned)oldNumsSize,
			.entriesOffset     = (unsigned)oldEntriesSize,
			.numAreaNumsInSpan = (uint16_t)( numAreaNumsInSpan ),
			.numEntriesInSpan  = (uint16_t)( numEntriesInSpan ),
			.minNum            = minNum,
			.maxNum            = maxNum,
		};
	}
};

bool AasStaticRouteTable::compute() {
	const auto *aasWorld = AiAasWorld::instance();
	if( !aasWorld->isLoaded() ) {
		return false;
	}

	assert( !s_isAccessibleForRouteCache );

	auto *const routeCache = AiAasRouteCache::Shared();
	const auto numAreas    = (uint16_t)aasWorld->getAreas().size();

	NumsAndEntriesBuilder<AreaEntry> allowedBuilder;
	NumsAndEntriesBuilder<uint16_t> walkingBuilder;
	wsw::PodVector<BufferSpansForFlags> spans;

	spans.reserve( numAreas );
	// Put dummy values for area 0, so we don't have to apply offsets to fromAreaNum during retrieval
	spans.emplace_back( BufferSpansForFlags() );

	unsigned lastDisplayedProgress = 0;
	const unsigned totalWorkSize   = wsw::square( numAreas - 1 );

	for( uint16_t fromAreaNum = 1; fromAreaNum < numAreas; ++fromAreaNum ) {
		const unsigned currWorkSize = ( fromAreaNum - 1 ) * ( numAreas - 1 );
		const auto currProgress     = (unsigned)std::floor( 100.0 * ( (double)currWorkSize / (double)totalWorkSize ) );
		if( lastDisplayedProgress != currProgress ) {
			lastDisplayedProgress = currProgress;
			Com_Printf( "Computing the static route table: %d%%\n", lastDisplayedProgress );
		}

		allowedBuilder.beginSpan();
		walkingBuilder.beginSpan();

		for( uint16_t toAreaNum = 1; toAreaNum < numAreas; ++toAreaNum ) {
			if( fromAreaNum != toAreaNum ) {
				// TODO: Add public getters to AiAasRouteCache that allow
				// a simultaneous retrieval (existing private ones are very poor)
				// TODO: We actually can read the entire Dijkstra's algorithm result for the given from area
				// (Area-by-area retrieval still works fast due to internal caching in the route cache)
				int reach = 0;
				if( const int time = routeCache->FindRoute( fromAreaNum, toAreaNum, Bot::ALLOWED_TRAVEL_FLAGS, &reach ) ) {
					allowedBuilder.addNumAndEntry( toAreaNum, AreaEntry {
						.reachNum = (uint16_t)reach, .travelTime = (uint16_t)time,
					});
				}
				if( const int time = calcTravelTimeWalkingOrFallingShort( routeCache, aasWorld,
																		  fromAreaNum, toAreaNum ) ) {
					assert( (unsigned)time <= (unsigned)std::numeric_limits<uint16_t>::max() );
					walkingBuilder.addNumAndEntry( toAreaNum, (uint16_t)time );
				}
			}
		}

		spans.emplace_back( BufferSpansForFlags {
			.allowed               = allowedBuilder.endSpan(),
			.walkingOrFallingShort = walkingBuilder.endSpan(),
		});
	}

	Com_Printf( "Num entries in spans   (for allowed flags): avg=%" PRIu64 ", max: %" PRIu64 "\n",
				allowedBuilder.totalNumEntriesInSpans / ( numAreas - 1 ), allowedBuilder.maxNumEntriesInSpans );
	Com_Printf( "Num entries in spans   (for walking flags): avg=%" PRIu64 ", max: %" PRIu64 "\n",
				walkingBuilder.totalNumEntriesInSpans / ( numAreas - 1 ), walkingBuilder.maxNumEntriesInSpans );

	// TODO: This is not exception-safe, use sane RAII buffers

	m_dataForAllowedFlags.areaNums = makeHeapAllocCopy( allowedBuilder.nums );
	m_dataForAllowedFlags.areaNumsDataSizeInElems = allowedBuilder.nums.size();
	m_dataForAllowedFlags.entries = makeHeapAllocCopy( allowedBuilder.entries );
	m_dataForAllowedFlags.entriesDataSizeInElems = allowedBuilder.entries.size();

	m_walkingAreaNums = makeHeapAllocCopy( walkingBuilder.nums );
	m_walkingAreaNumsDataSizeInElems = walkingBuilder.nums.size();
	m_walkingTravelTimes = makeHeapAllocCopy( walkingBuilder.entries );
	m_walkingTravelTimesDataSizeInElems = walkingBuilder.entries.size();

	m_bufferSpans = makeHeapAllocCopy( spans );
	m_numAreas    = numAreas;

#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
	// Check all computed results immediately
	for( uint16_t fromAreaNum = 1; fromAreaNum < numAreas; ++fromAreaNum ) {
		for( uint16_t toAreaNum = 1; toAreaNum < numAreas; ++toAreaNum ) {
			if( fromAreaNum != toAreaNum ) {
				// Just call these methods which have themselves internal checks
				(void)getAllowedRouteFromTo( fromAreaNum, toAreaNum );
				(void) getTravelTimeWalkingOrFallingShort( fromAreaNum, toAreaNum );
			}
		}
	}
#endif

	return true;
}


#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
static void checkMatchWithRouteCache( std::optional<std::pair<int, uint16_t>> tableResult,
									  int fromAreaNum, int toAreaNum, int travelFlags ) {
	auto *const aasWorld   = AiAasWorld::instance();
	auto *const routeCache = AiAasRouteCache::Shared();
	const int tableNum     = tableResult ? tableResult->first : 0;
	const int tableTime    = tableResult ? tableResult->second : 0;
	if( tableNum >= aasWorld->getReaches().size() ) abort();
	int dynamicNum = 0;
	const int dynamicTime  = routeCache->FindRoute( fromAreaNum, toAreaNum, travelFlags, &dynamicNum );
	if( tableNum != dynamicNum || tableTime != dynamicTime ) {
		Com_Printf( "From=%d to=%d travel flags=0x%x\n", fromAreaNum, toAreaNum, travelFlags );
		Com_Printf( "Table reach num=%d dynamic reach num=%d\n", tableNum, dynamicNum );
		Com_Printf( "Table time=%d dynamic time=%d\n", tableTime, dynamicTime );
		abort();
	} else {
		//Com_Printf( "Match with num=%d time=%d\n", tableNum, tableTime );
	}
}
#endif

[[nodiscard]]
auto AasStaticRouteTable::getAllowedRouteFromTo( int fromAreaNum, int toAreaNum ) const
	-> std::optional<std::pair<int, uint16_t>> {
	assert( fromAreaNum != toAreaNum );
	assert( (unsigned)( fromAreaNum - 1 ) < (unsigned)( m_numAreas - 1 ) );
	assert( (unsigned)( toAreaNum - 1 ) < (unsigned)( m_numAreas - 1 ) );

	auto result = getRouteFromTo( fromAreaNum, toAreaNum, m_bufferSpans[fromAreaNum].allowed, &m_dataForAllowedFlags );

#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
	const bool wasAccessible        = s_isAccessibleForRouteCache;
	s_isAccessibleForRouteCache     = false;
	s_isCheckingMatchWithRouteCache = true;
	checkMatchWithRouteCache( result, fromAreaNum, toAreaNum, Bot::ALLOWED_TRAVEL_FLAGS );
	s_isCheckingMatchWithRouteCache = false;
	s_isAccessibleForRouteCache     = wasAccessible;
#endif

	return result;
}

// TODO: Lift to the top level, merge with the same thing in the CM code
#ifndef _MSC_VER
#define wsw_ctz( x ) __builtin_ctz( x )
#else
__forceinline int wsw_ctz( int x ) {
	assert( x );
	unsigned long result = 0;
	_BitScanForward( &result, x );
	return result;
}
#endif

[[nodiscard]]
static auto findIndexOrAreaInAreaNumsArray( const uint16_t *areaNums, int areaNum,
											unsigned numAreasInSpan, unsigned numEntriesInSpan ) {
	assert( areaNum > 0 && areaNum <= std::numeric_limits<uint16_t>::max() );
	assert( numEntriesInSpan <= numAreasInSpan );

	(void)numAreasInSpan;
	(void)numEntriesInSpan;

#if !defined( WSW_USE_SSE2 ) || defined( CHECK_TABLE_MATCH_WITH_ROUTE_CACHE )
	unsigned genericIndex = 0;
	// Don't scan trailing zeroes in the generic version (that's why the loop is limited by numEntriesInSpan).
	for(; genericIndex < numEntriesInSpan; ++genericIndex ) {
		if( areaNums[genericIndex] == (uint16_t)areaNum ) {
			break;
		}
	}
#endif

#if defined( WSW_USE_SSE2 )
	constexpr unsigned kNumVectorsPerStep                = 2;
	constexpr unsigned kNumAreasPerVector                = sizeof( __m128i ) / sizeof( uint16_t );
	[[maybe_unused]] constexpr unsigned kNumAreasPerStep = kNumVectorsPerStep * kNumAreasPerVector;

	assert( numAreasInSpan < numEntriesInSpan + kNumAreasPerStep );
	assert( !( numAreasInSpan % kNumAreasPerStep ) );
	assert( numAreasInSpan >= kNumAreasPerStep );

	// Ensure the proper alignment as well
	assert( !( ( (uintptr_t)areaNums ) % 16 ) );

	// Return the same value in case of failing to find an area as the generic version does
	// (this is not obligatory for the function but is useful for the debug match check).
	unsigned sseIndex                = numEntriesInSpan;
	const auto *xmmAreaNumsPtr       = (const __m128i *)areaNums;
	const auto *const xmmAreaNumsEnd = (const __m128i *)( areaNums + numAreasInSpan );
	const __m128i xmmSearchValue     = _mm_set1_epi16( (int16_t)areaNum );
	do {
		const __m128i xmmCmp1    = _mm_cmpeq_epi16( _mm_load_si128( xmmAreaNumsPtr + 0 ), xmmSearchValue );
		const __m128i xmmCmp2    = _mm_cmpeq_epi16( _mm_load_si128( xmmAreaNumsPtr + 1 ), xmmSearchValue );
		// TODO: Should we alternatively combine XMM masks first?
		// The current version still have an advantage that it should not cross
		// integer/floating-point execution units on older processors though (?).
		const int cmpBitMask1    = _mm_movemask_epi8( xmmCmp1 );
		const int cmpBitMask2    = _mm_movemask_epi8( xmmCmp2 );
		if( ( cmpBitMask1 | cmpBitMask2 ) ) {
			// Combine comparison results like we have compared 256-bit vectors
			// _mm_movemask_epi8() result spans over 16 bits
			const int combinedBitMask         = ( cmpBitMask2 << 16 ) | cmpBitMask1;
			// Divide by two to convert the number of trailing zero byte sign bits to the number of trailing zero words
			const int matchingWordIndex       = wsw_ctz( combinedBitMask ) / 2;
			const ptrdiff_t currVectorOffset  = (const uint16_t *)xmmAreaNumsPtr - areaNums;
			sseIndex                          = (unsigned)( currVectorOffset + matchingWordIndex );
			break;
		}
		xmmAreaNumsPtr += kNumVectorsPerStep;
	} while( xmmAreaNumsPtr < xmmAreaNumsEnd );

	// TODO: The name of the defined switch is quite misleading in this case
#if defined( CHECK_TABLE_MATCH_WITH_ROUTE_CACHE )
	if( genericIndex != sseIndex ) {
		Com_Printf( "genericIndex=%d, sseIndex=%d, numEntriesInSpan=%d, numAreasInSpan=%d\n",
					genericIndex, sseIndex, numEntriesInSpan, numAreasInSpan );
		abort();
	}
#endif

	return sseIndex;
#else
	return genericIndex;
#endif
}

[[nodiscard]]
static inline bool isWithinInclusiveRange( int value, int minValueInclusive, int maxValueInclusive ) {
	return (unsigned)( value - minValueInclusive ) <= (unsigned)( maxValueInclusive - minValueInclusive );
}

auto AasStaticRouteTable::getRouteFromTo( int fromAreaNum, int toAreaNum, const BufferSpan &span,
										  const DataForTravelFlags *data )
										  -> std::optional<std::pair<int, uint16_t>> {
	// Check whether the toAreaNum is within the known range of area numbers in the span
	if( !isWithinInclusiveRange( toAreaNum, span.minNum, span.maxNum ) ) {
		return std::nullopt;
	}

	const uint16_t *areaNums   = data->areaNums + span.areaNumsOffset;
	const unsigned indexInSpan = findIndexOrAreaInAreaNumsArray( areaNums, toAreaNum,
																 span.numAreaNumsInSpan, span.numEntriesInSpan );

	if( indexInSpan < span.numEntriesInSpan ) {
		const AreaEntry &entry = data->entries[indexInSpan + span.entriesOffset];
		return std::pair( entry.reachNum, entry.travelTime );
	}

	return std::nullopt;
}

auto AasStaticRouteTable::getTravelTimeWalkingOrFallingShort( int fromAreaNum, int toAreaNum ) const
	-> std::optional<uint16_t> {
	if( fromAreaNum == toAreaNum ) {
		return 1;
	}
	assert( fromAreaNum != toAreaNum );
	assert( (unsigned)( fromAreaNum - 1 ) < (unsigned)( m_numAreas - 1 ) );
	assert( (unsigned)( toAreaNum - 1 ) < (unsigned)( m_numAreas - 1 ) );

	const BufferSpan &span = m_bufferSpans[fromAreaNum].walkingOrFallingShort;

	// Check whether the toAreaNum is within the known range of area numbers in the span
	if( !isWithinInclusiveRange( toAreaNum, span.minNum, span.maxNum ) ) {
#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
		if( calcTravelTimeWalkingOrFallingShort( AiAasRouteCache::Shared(), AiAasWorld::instance(), fromAreaNum, toAreaNum ) ) {
			Com_Printf( "From=%d to=%d unreachable by table data, but reachable by a dynamic computation\n",
						fromAreaNum, toAreaNum );
			abort();
		}
#endif
		return std::nullopt;
	}

	const uint16_t *areaNums   = m_walkingAreaNums + span.areaNumsOffset;
	const unsigned indexInSpan = findIndexOrAreaInAreaNumsArray( areaNums, toAreaNum,
																 span.numAreaNumsInSpan, span.numEntriesInSpan );

	if( indexInSpan < span.numEntriesInSpan ) {
		const uint16_t tableTime = m_walkingTravelTimes[indexInSpan + span.entriesOffset];
#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
		const uint16_t dynamicTime = calcTravelTimeWalkingOrFallingShort( AiAasRouteCache::Shared(), AiAasWorld::instance(),
																		  fromAreaNum, toAreaNum );
		if( tableTime != dynamicTime ) {
			Com_Printf( "From=%d to=%d table time=%d dynamic time=%d\n", fromAreaNum, toAreaNum, tableTime, dynamicTime );
			abort();
		}
#endif
		return tableTime;
	}

	return std::nullopt;
}