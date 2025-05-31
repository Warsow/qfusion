#ifndef WSW_42051f99_0ffd_43d2_9248_52a446d54421_H
#define WSW_42051f99_0ffd_43d2_9248_52a446d54421_H

#include <cassert>
#include <cstdint>
#include <utility>
#include <optional>

#if 0
#define CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
#endif

class AasStaticRouteTable {
	template <typename> friend class SingletonHolder;
	template <typename> friend struct NumsAndEntriesBuilder;

public:
	[[nodiscard]]
	static auto instance() -> const AasStaticRouteTable *;

	static void init( const char *mapName );
	static void shutdown();

	[[nodiscard]]
	auto getAllowedRouteFromTo( int fromAreaNum, int toAreaNum ) const -> std::optional<std::pair<int, uint16_t>>;
	[[nodiscard]]
	auto getTravelTimeWalkingOrFallingShort( int fromAreaNum, int toAreaNum ) const -> std::optional<uint16_t>;

	static bool s_isAccessibleForRouteCache;
#ifdef CHECK_TABLE_MATCH_WITH_ROUTE_CACHE
	static bool s_isCheckingMatchWithRouteCache;
#endif
private:
	explicit AasStaticRouteTable( const char *mapName );
	~AasStaticRouteTable();

	[[nodiscard]]
	bool loadFromFile( const char *filePath );
	[[nodiscard]]
	bool compute();
	[[nodiscard]]
	bool saveToFile( const char *filePath );

	struct BufferSpan;
	struct DataForTravelFlags;

	[[nodiscard]]
	static auto getRouteFromTo( int fromAreaNum, int toAreaNum, const BufferSpan &span, const DataForTravelFlags *data )
		-> std::optional<std::pair<int, uint16_t>>;

	struct alignas( 4 ) AreaEntry {
		// All current maps, including popular custom maps, fit uint16_t limitations.
		uint16_t reachNum;
		uint16_t travelTime;
	};

	struct alignas( 4 ) BufferSpan {
		unsigned areaNumsOffset { 0 };
		unsigned entriesOffset { 0 };
		uint16_t numAreaNumsInSpan { 0 };
		uint16_t numEntriesInSpan { 0 };
		uint16_t minNum { 0 }, maxNum { 0 };
	};

	// Store side-by-side in memory as we often query both
	struct BufferSpansForFlags {
		BufferSpan allowed;
		// Note: entries are just short unsigned integers in this case
		BufferSpan walkingOrFallingShort;
	};

	struct DataForTravelFlags {
		// TODO: Use our custom RAII buffers (using std::unique_ptr should be avoided)
		AreaEntry *entries { nullptr };
		uint16_t *areaNums { nullptr };
		unsigned entriesDataSizeInElems { 0 };
		unsigned areaNumsDataSizeInElems { 0 };
	};

	// We do not longer split flags to allowed/preferred, only allowed flags are left
	DataForTravelFlags m_dataForAllowedFlags;

	uint16_t *m_walkingAreaNums { nullptr };
	uint16_t *m_walkingTravelTimes { nullptr };
	unsigned m_walkingTravelTimesDataSizeInElems { 0 };
	unsigned m_walkingAreaNumsDataSizeInElems { 0 };

	BufferSpansForFlags *m_bufferSpans { nullptr };
	unsigned m_numAreas { 0 };
};

#endif