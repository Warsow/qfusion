#ifndef WSW_bb314e6a_1000_44f6_8404_85e663052b82_H
#define WSW_bb314e6a_1000_44f6_8404_85e663052b82_H

#include "weaponselector.h"
#include "../awareness/selectedenemy.h"

struct AimParams {
	vec3_t fireOrigin;
	vec3_t fireTarget;
	float suggestedCoordError;
};

class BotFireTargetCache {
	struct CachedFireTarget {
		Vec3 origin { 0, 0, 0 };
		unsigned selectedEnemyInstanceId { 0 };
		unsigned selectedWeaponsInstanceId { 0 };
		int64_t invalidAt { 0 };

		bool IsValidFor( const SelectedEnemy &selectedEnemy, const SelectedWeapons &selectedWeapons ) const {
			return selectedEnemy.InstanceId() == selectedEnemyInstanceId &&
				   selectedWeapons.InstanceId() == selectedWeaponsInstanceId &&
				   invalidAt > level.time;
		}

		void CacheFor( const SelectedEnemy &selectedEnemy,
					   const SelectedWeapons &selectedWeapons,
					   const vec3_t origin_ ) {
			this->origin.set( origin_ );
			selectedEnemyInstanceId = selectedEnemy.InstanceId();
			selectedWeaponsInstanceId = selectedWeapons.InstanceId();
			invalidAt = level.time + 64;
		}
	};

	const Bot *const bot;

	CachedFireTarget cachedFireTarget;

	void SetupCoarseFireTarget( const SelectedEnemy &selectedEnemy,
								const GenericFireDef &fireDef,
								vec3_t fire_origin, vec3_t target );

	void AdjustPredictionExplosiveAimTypeParams( const SelectedEnemy &selectedEnemy,
												 const SelectedWeapons &selectedWeapons,
												 const GenericFireDef &fireDef,
												 AimParams *aimParams );

	void AdjustPredictionAimTypeParams( const SelectedEnemy &selectedEnemy,
										const SelectedWeapons &selectedWeapons,
										const GenericFireDef &fireDef,
										AimParams *aimParams );

	void AdjustDropAimTypeParams( const SelectedEnemy &selectedEnemy,
								  const SelectedWeapons &selectedWeapons,
								  const GenericFireDef &fireDef,
								  AimParams *aimParams );

	void AdjustInstantAimTypeParams( const SelectedEnemy &selectedEnemy,
									 const SelectedWeapons &selectedWeapons,
									 const GenericFireDef &fireDef,
									 AimParams *aimParams );

	void AdjustForShootableEnvironment( const SelectedEnemy &selectedEnemy, float splashRadius, AimParams *aimParams );

	void GetPredictedTargetOrigin( const SelectedEnemy &selectedEnemy,
								   const SelectedWeapons &selectedWeapons,
								   float projectileSpeed,
								   AimParams *aimParams );

	void PredictProjectileShot( const SelectedEnemy &selectedEnemy,
								float projectileSpeed,
								AimParams *aimParams,
								bool applyTargetGravity );

public:
	explicit BotFireTargetCache( const Bot *bot_ ): bot( bot_ ) {}

	void AdjustAimParams( const SelectedEnemy &selectedEnemy, const SelectedWeapons &selectedWeapons,
						  const GenericFireDef &fireDef, AimParams *aimParams );
};

#endif
