#ifndef WSW_45f23f69_2bed_4677_803a_e1e13c49a4cb_H
#define WSW_45f23f69_2bed_4677_803a_e1e13c49a4cb_H

#include "../ailocal.h"

class Bot;
class TrackedEnemy;

class PathBlockingTracker {
	friend class BotAwarenessModule;

	Bot *const bot;
	bool didClearAtLastUpdate { false };

	explicit PathBlockingTracker( Bot *bot_ ): bot( bot_ ) {}

	inline void ClearBlockedAreas();

	bool IsAPotentialBlocker( const TrackedEnemy *enemy, float damageToKillBot, int botBestWeaponTier ) const;

	void Update();
};

#endif
