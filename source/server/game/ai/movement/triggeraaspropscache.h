#ifndef WSW_27e919d8_dea3_4667_b05f_118b4ade67d0_H
#define WSW_27e919d8_dea3_4667_b05f_118b4ade67d0_H

#include <common/facilities/q_comref.h>
#include <common/helpers/wswbasicmath.h>
#include <common/types/podvector.h>

#include <bitset>
#include <cstdlib>
#include <cstring>
#include <optional>
#include <unordered_map>
#include <utility>
#include <span>

class TriggerAasPropsCache {
public:
	void reload();

	[[nodiscard]]
	auto getTriggerEntNumForTeleportReach( int reachNum ) const -> std::optional<int>;
	[[nodiscard]]
	auto getTriggerEntNumForJumppadReach( int reachNum ) const -> std::optional<int>;
	[[nodiscard]]
	auto getTriggerEntNumForElevatorReach( int reachNum ) const -> std::optional<int>;

	auto getJumppadTargetAreas( int entNum ) const -> std::span<const uint16_t>;
	auto getElevatorTargetAreas( int entNum ) const -> std::span<const uint16_t>;
private:
	mutable std::unordered_map<int, wsw::PodVector<uint16_t>> m_jumppadTargetAreas;
	mutable std::unordered_map<int, wsw::PodVector<uint16_t>> m_elevatorTargetAreas;

	mutable std::unordered_map<int, int> m_triggerEntNumsForJumppadReach;
	mutable std::unordered_map<int, int> m_triggerEntNumsForTeleportReach;
	mutable std::unordered_map<int, int> m_triggerEntNumsForElevatorReach;
};

extern TriggerAasPropsCache triggerAasPropsCache;

#endif