#ifndef WSW_9aafc0da_a31d_468a_855d_638b0fdfd95b_H
#define WSW_9aafc0da_a31d_468a_855d_638b0fdfd95b_H

#include "../ailocal.h"
#include "goalentities.h"
#include <common/types/staticvector.h>

struct SelectedNavEntity {
	int64_t timeoutAt;
	const NavEntity *navEntity;
	unsigned instanceId;
	float cost;
	float pickupGoalWeight;

	[[nodiscard]]
	bool isSame( const std::optional<SelectedNavEntity> &maybeThat ) const {
		return maybeThat && instanceId == maybeThat->instanceId;
	}

	[[nodiscard]]
	static auto nextInstanceId() -> unsigned { return ++s_nextInstanceId; }
private:
	static inline unsigned s_nextInstanceId { 0 };
};

class BotItemsSelector {
	const Bot *const bot;

	int64_t disabledForSelectionUntil[MAX_EDICTS];

	float internalEntityWeights[MAX_EDICTS];
	float overriddenEntityWeights[MAX_EDICTS];

	// For each item contains a goal weight that would a corresponding AI pickup goal have.
	float internalPickupGoalWeights[MAX_EDICTS];

	float GetEntityWeight( int entNum ) const {
		float overriddenEntityWeight = overriddenEntityWeights[entNum];
		if( overriddenEntityWeight != 0 ) {
			return overriddenEntityWeight;
		}
		return internalEntityWeights[entNum];
	}

	float GetGoalWeight( int entNum ) const {
		float overriddenEntityWeight = overriddenEntityWeights[entNum];
		// Make goal weight based on overridden entity weight
		if( overriddenEntityWeight != 0 ) {
			// High weight items would have 2.0f goal weight
			return 2.0f * Q_Sqrt( wsw::max( overriddenEntityWeight, 10.0f ) * Q_Rcp( 10.0f ) );
		}
		return internalPickupGoalWeights[entNum];
	}

	void UpdateInternalItemAndGoalWeights();

	struct ItemAndGoalWeights {
		float itemWeight;
		float goalWeight;

		ItemAndGoalWeights( float itemWeight_, float goalWeight_ )
			: itemWeight( itemWeight_ ), goalWeight( goalWeight_ ) {}
	};

	ItemAndGoalWeights ComputeItemWeights( const gsitem_t *item ) const;
	ItemAndGoalWeights ComputeWeaponWeights( const gsitem_t *item ) const;
	ItemAndGoalWeights ComputeAmmoWeights( const gsitem_t *item ) const;
	ItemAndGoalWeights ComputeArmorWeights( const gsitem_t *item ) const;
	ItemAndGoalWeights ComputeHealthWeights( const gsitem_t *item ) const;
	ItemAndGoalWeights ComputePowerupWeights( const gsitem_t *item ) const;

#ifndef _MSC_VER
	void Debug( const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) );
#else
	void Debug( _Printf_format_string_ const char *format, ... );
#endif

	SelectedNavEntity Select( const NavEntity *navEntity, float cost, unsigned timeout ) {
		return { .timeoutAt = level.time + timeout, .navEntity = navEntity,
				 .instanceId = SelectedNavEntity::nextInstanceId(),
				 .cost = cost, .pickupGoalWeight = GetGoalWeight( navEntity->Id() ) };
	}

	bool IsShortRangeReachable( const NavEntity *navEntity, const int *fromAreaNums, int numFromAreas ) const;
public:
	explicit BotItemsSelector( const Bot *bot_ ) : bot( bot_ ) {
		// We zero only this array as its content does not get cleared in SuggestGoalEntity() calls
		memset( disabledForSelectionUntil, 0, sizeof( disabledForSelectionUntil ) );
	}

	void ClearOverriddenEntityWeights() {
		memset( overriddenEntityWeights, 0, sizeof( overriddenEntityWeights ) );
	}

	// This weight overrides internal one computed by this brain itself.
	void OverrideEntityWeight( const edict_t *ent, float weight ) {
		overriddenEntityWeights[ENTNUM( const_cast<edict_t*>( ent ) )] = weight;
	}

	void MarkAsDisabled( const NavEntity &navEntity, unsigned millis ) {
		disabledForSelectionUntil[navEntity.Id()] = level.time + millis;
	}

	bool IsTopTierItem( const NavTarget *navTarget ) const {
		return navTarget && navTarget->IsTopTierItem( overriddenEntityWeights );
	}

	std::optional<SelectedNavEntity> SuggestGoalNavEntity( const NavEntity *currSelectedNavEntity );
};

#endif
