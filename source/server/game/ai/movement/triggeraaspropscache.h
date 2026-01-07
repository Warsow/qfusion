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
	void clear();

	[[nodiscard]]
	auto getTriggerEntNumForTeleportReach( int reachNum ) const -> std::optional<int>;
	[[nodiscard]]
	auto getTriggerEntNumForJumppadReach( int reachNum ) const -> std::optional<int>;
	[[nodiscard]]
	auto getTriggerEntNumForElevatorReach( int reachNum ) const -> std::optional<int>;
private:
	mutable std::unordered_map<int, int> m_triggerAreaNumsForJumppadReach;
	mutable std::unordered_map<int, int> m_triggerAreaNumsForTeleportReach;
	mutable std::unordered_map<int, int> m_triggerAreaNumsForElevatorReach;
};

extern TriggerAasPropsCache triggerAasPropsCache;

#endif