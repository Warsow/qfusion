#ifndef WSW_98953973_42bd_4ce9_a3ab_72ce908bd1c3_H
#define WSW_98953973_42bd_4ce9_a3ab_72ce908bd1c3_H

#include "../ailocal.h"
#include <common/types/staticvector.h>
#include "firetargetcache.h"

class Bot;
class BotInput;

class WeaponsToSelect {
	int builtin { WEAP_NONE };
	int script { -1 };
	bool preferBuiltin { true };
	WeaponsToSelect( int builtin_, int script_, bool preferBuiltin_ )
		: builtin( builtin_ ), script( script_ ), preferBuiltin( preferBuiltin_ ) {}
public:
	[[nodiscard]]
	auto getScriptWeapon() const -> std::optional<int> {
		return script >= 0 ? std::make_optional( script ) : std::nullopt;
	}
	[[nodiscard]]
	auto getBuiltinWeapon() const -> int {
		return builtin;
	}
	[[nodiscard]]
	auto shouldPreferBuiltin() const -> std::optional<bool> {
		return script >= 0 ? std::make_optional( preferBuiltin ) : std::nullopt;
	}
	static auto bultinOnly( int builtin ) -> WeaponsToSelect {
		assert( builtin > 0 );
		return WeaponsToSelect( builtin, -1, true );
	}
	static auto builtinAndPrimaryScript( int builtin, int script ) -> WeaponsToSelect {
		assert( builtin > 0 && script >= 0 );
		return WeaponsToSelect( builtin, script, false );
	}
	static auto builtinAndSecondaryScript( int builtin, int script ) -> WeaponsToSelect {
		assert( builtin > 0 && script >= 0 );
		return WeaponsToSelect( builtin, script, true );
	}
};

class BotWeaponsUsageModule {
	friend class BotWeaponSelector;

	static constexpr unsigned MAX_SCRIPT_WEAPONS = 3;

	Bot *const bot;
	wsw::StaticVector<AiScriptWeaponDef, MAX_SCRIPT_WEAPONS> scriptWeaponDefs;
	wsw::StaticVector<int, MAX_SCRIPT_WEAPONS> scriptWeaponCooldown;

	BotFireTargetCache builtinFireTargetCache;
	BotFireTargetCache scriptFireTargetCache;

	BotWeaponSelector weaponSelector;
	SelectedWeapons selectedWeapons;

	// Vary angular speed depending of enemy tracking duration
	// (make sure bots finish their targets)
	struct alignas( 4 )EnemyTrackingSpeedHolder {
		Int64Align4 trackingEnemySince;
		unsigned selectedEnemyInstanceId;
		unsigned selectedWeaponsInstanceId;

		EnemyTrackingSpeedHolder()
			: trackingEnemySince( 0 ), selectedEnemyInstanceId( 0 ), selectedWeaponsInstanceId( 0 ) {}

		float UpdateAndGet( const SelectedEnemy &selectedEnemy,
							const SelectedWeapons &selectedWeapons,
							float skill ) {
			int64_t levelTime = level.time;
			if( selectedEnemy.InstanceId() != selectedEnemyInstanceId ||
				selectedWeapons.InstanceId() != selectedWeaponsInstanceId  ) {
				trackingEnemySince = levelTime;
			}
			return 0.75f + skill * 1.25f * BoundedFraction( levelTime - trackingEnemySince, 1000.0f );
		}
	};

	EnemyTrackingSpeedHolder enemyTrackingSpeedHolder;

	// Returns true if current look angle worth pressing attack
	bool CheckShot( const AimParams &aimParams, const BotInput *input, const GenericFireDef &fireDef );

	bool TryTraceShot( trace_t *tr, const Vec3 &newLookDir,
					   const AimParams &aimParams,
					   const GenericFireDef &fireDef );

	bool CheckSplashTeamDamage( const vec3_t hitOrigin, const AimParams &aimParams, const GenericFireDef &fireDef );

	// A helper to determine whether continuous-fire weapons should be fired even if there is an obstacle in-front.
	// Should be called if a TryTraceShot() call has set non-unit fraction.
	bool IsShotBlockedBySolidWall( trace_t *tr,
								   float distanceThreshold,
								   const AimParams &aimParams,
								   const GenericFireDef &fireDef );

	void LookAtEnemy( float coordError, const vec3_t fire_origin, vec3_t target, BotInput *input );
	void PressAttack( const GenericFireDef *fireDef, const GenericFireDef *builtinFireDef,
					  const GenericFireDef *scriptFireDef, BotInput *input );


	class AimingRandomHolder {
		int64_t valuesTimeoutAt[3] = { 0, 0, 0 };
		float values[3] = { 0.5f, 0.5f, 0.5f };

	public:
		inline float GetCoordRandom( int coordNum ) {
			if( valuesTimeoutAt[coordNum] <= level.time ) {
				values[coordNum] = random();
				valuesTimeoutAt[coordNum] = level.time + 128 + From0UpToMax( 256, random() );
			}
			return values[coordNum];
		}
	};

	AimingRandomHolder aimingRandomHolder;

	void SetSelectedWeapons( const WeaponsToSelect &weaponsToSelect, unsigned timeoutPeriod );
public:
	explicit BotWeaponsUsageModule( Bot *bot_ );

	void UpdateScriptWeaponsStatus();
	void Frame( const WorldState &cachedWorldState );
	void Think( const WorldState &cachedWorldState );
	void TryFire( BotInput *input );

	inline const SelectedWeapons &GetSelectedWeapons() const { return selectedWeapons; }
};

#endif
