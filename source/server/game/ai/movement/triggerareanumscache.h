#ifndef WSW_27e919d8_dea3_4667_b05f_118b4ade67d0_H
#define WSW_27e919d8_dea3_4667_b05f_118b4ade67d0_H

#include <common/q_shared.h>
#include <common/wswbasicmath.h>
#include <common/wswpodvector.h>

#include <bitset>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <unordered_map>
#include <utility>
#include <span>

class TriggerAreaNumsCache {
public:
	class ClassTriggerNums {
		friend class TriggerAreaNumsCache;

		using SpanOfNums = std::pair<uint8_t, uint8_t>;

		[[nodiscard]]
		auto getFirstOf( const SpanOfNums &span ) const -> std::optional<uint16_t> {
			return span.second ? std::optional( m_data[span.first] ) : std::nullopt;
		}

		SpanOfNums m_jumppadNumsSpan;
		SpanOfNums m_teleporterNumsSpan;
		SpanOfNums m_platformNumsSpan;
		uint16_t m_data[15];
		uint16_t m_size { 0 };

		[[nodiscard]]
		auto addNums( const uint16_t *nums, unsigned numOfNums ) -> std::pair<uint8_t, uint8_t> {
			numOfNums = wsw::min<unsigned>( numOfNums, std::size( m_data ) - m_size );
			const auto result = std::make_pair( (uint8_t)m_size, (uint8_t)numOfNums );
			std::memcpy( m_data + m_size, nums, sizeof( nums[0] ) * numOfNums );
			m_size += numOfNums;
			assert( m_size <= std::size( m_data ) && m_size < std::numeric_limits<uint8_t>::max() );
			return result;
		}
	public:
		[[nodiscard]]
		auto getFirstJummpadNum() const -> std::optional<uint16_t> { return getFirstOf( m_jumppadNumsSpan ); }
		[[nodiscard]]
		auto getFirstTeleporterNum() const -> std::optional<uint16_t> { return getFirstOf( m_teleporterNumsSpan ); }
		[[nodiscard]]
		auto getFirstPlatformNum() const -> std::optional<uint16_t> { return getFirstOf( m_platformNumsSpan ); }
	};
private:
	mutable int m_areaNums[MAX_EDICTS] {};
	mutable std::unordered_map<int, ClassTriggerNums> m_triggersForArea;
	mutable std::unordered_map<int, wsw::PodVector<int>> m_jumppadTargetAreaNums;
	// Don't insert junk nodes to indicate tested but empty areas, use this bitset instead
	mutable std::bitset<std::numeric_limits<uint16_t>::max()> m_testedTriggersForArea;
	// TODO: Fuse with the set above into a 2-bit set
	mutable std::bitset<std::numeric_limits<uint16_t>::max()> m_hasTriggersForArea;

	static void findJumppadTargetAreaNums( const struct edict_s *jumppadEntity, int jumppadAreaNum, wsw::PodVector<int> *targetAreaNums );
public:
	void clear();
	[[nodiscard]]
	auto getAreaNum( int entNum ) const -> int;
	[[nodiscard]]
	auto getTriggersForArea( int areaNum ) const -> const ClassTriggerNums *;
	[[nodiscard]]
	auto getJumppadAreaNumAndTargetAreaNums( int entNum ) const -> std::pair<int, std::span<const int>>;
};

extern TriggerAreaNumsCache triggerAreaNumsCache;

#endif