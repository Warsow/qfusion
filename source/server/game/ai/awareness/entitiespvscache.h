#ifndef WSW_f719e41f_2abc_4ba1_8089_c43c90fb8d9c_H
#define WSW_f719e41f_2abc_4ba1_8089_c43c90fb8d9c_H

#include "../component.h"

class EntitiesPvsCache {
	// 2 bits per each other entity
	static constexpr unsigned ENTITY_DATA_STRIDE = 2 * (MAX_EDICTS / 32);
	// MAX_EDICTS strings per each entity
	mutable uint32_t visStrings[MAX_EDICTS][ENTITY_DATA_STRIDE];

	static bool AreInPvsUncached( const edict_t *ent1, const edict_t *ent2 );

	static EntitiesPvsCache instance;
public:
	static EntitiesPvsCache *Instance() { return &instance; }

	// We could avoid explicit clearing of the cache each frame by marking each entry by the computation timestamp.
	// This approach is convenient and is widely for bot perception caches.
	// However we have to switch to the explicit cleaning in this case
	// to prevent excessive memory usage and cache misses.
	void Update() {
		if( ( level.framenum % 4 ) == 0 ) {
			memset( &visStrings[0][0], 0, sizeof( visStrings ) );
		}
	}

	bool AreInPvs( const edict_t *ent1, const edict_t *ent2 ) const;
};

#endif
