#ifndef WSW_f4175f6d_87ed_4203_82d7_b598e2124068_H
#define WSW_f4175f6d_87ed_4203_82d7_b598e2124068_H

#include "entitiespvscache.h"
#include <common/wswstaticvector.h>
#include <common/wswsortbyfield.h>

[[nodiscard]]
static inline bool isGenericEntityInPvs( const edict_t *self, const edict_t *ent ) {
	return EntitiesPvsCache::Instance()->AreInPvs( self, ent );
}

struct EntAndDistance {
	int entNum;
	float distance;
};

static const unsigned MAX_NONCLIENT_ENTITIES = MAX_EDICTS - MAX_CLIENTS;
typedef wsw::StaticVector<uint16_t, MAX_NONCLIENT_ENTITIES> EntNumsVector;
typedef wsw::StaticVector<EntAndDistance, MAX_NONCLIENT_ENTITIES> EntsAndDistancesVector;

// Returns the number of executed trace calls
template<unsigned N, unsigned M, typename PvsFunc, typename VisFunc>
[[maybe_unused]]
auto visCheckRawEnts( wsw::StaticVector<EntAndDistance, N> *rawEnts,
					  wsw::StaticVector<uint16_t, M> *prunedEnts,
					  const edict_t *botEnt, unsigned visFuncCallsQuotum,
					  PvsFunc pvsFunc, VisFunc visFunc ) -> unsigned {
	// This allows for a nicer code at call sites
	if( !visFuncCallsQuotum || rawEnts->empty() ) {
		return 0;
	}

	// Sort all entities by distance to the bot.
	// Then select not more than visEntsLimit nearest entities in PVS, then call visFunc().
	// It may cause data loss (far entities that may have higher logical priority),
	// but in a common good case (when there are few visible entities) it preserves data,
	// and in the worst case mentioned above it does not act weird from player POV and prevents server hang up.

	wsw::sortByField( rawEnts->data(), rawEnts->data() + rawEnts->size(), &EntAndDistance::distance );

	wsw::StaticVector<uint16_t, M> entsInPvs;

	const unsigned limit      = wsw::min( wsw::min( rawEnts->size(), prunedEnts->capacity() ), visFuncCallsQuotum );
	const edict_t *gameEdicts = game.edicts;
	for( unsigned rawEntIndex = 0; rawEntIndex < rawEnts->size(); ++rawEntIndex ) {
		const auto entNum = (uint16_t)( ( *rawEnts )[rawEntIndex].entNum );
		if( pvsFunc( botEnt, gameEdicts + entNum ) ) {
			entsInPvs.push_back( entNum );
			if( entsInPvs.size() == limit ) [[unlikely]] {
				break;
			}
		}
	}

	prunedEnts->clear();
	for( const uint16_t entNum: entsInPvs ) {
		if( visFunc( botEnt, gameEdicts + entNum ) ) {
			prunedEnts->push_back( entNum );
		}
	}

	assert( entsInPvs.size() <= limit );
	return entsInPvs.size();
}

#endif
