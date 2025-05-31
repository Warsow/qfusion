#ifndef WSW_8d375276_69da_46a5_93ab_4c3dfa3e6f9a_H
#define WSW_8d375276_69da_46a5_93ab_4c3dfa3e6f9a_H

#include "../awareness/enemiestracker.h"
#include <optional>
#include <utility>

class WorldState;

class GenericFireDef
{
	// Allow SelectedWeapons to use the default constructor
	friend class SelectedWeapons;

	float projectileSpeed;
	float splashRadius;
	ai_weapon_aim_type aimType;
	short weaponNum;
	bool isBuiltin;

	GenericFireDef()
		: projectileSpeed( 0 ),
		splashRadius( 0 ),
		aimType( AI_WEAPON_AIM_TYPE_INSTANT_HIT ),
		weaponNum( -1 ),
		isBuiltin( false ) {}

public:
	GenericFireDef( int weaponNum_, const firedef_t *builtinFireDef ) {
		this->projectileSpeed = builtinFireDef->speed;
		this->splashRadius = builtinFireDef->splash_radius;
		this->aimType = BuiltinWeaponAimType( weaponNum_, builtinFireDef->fire_mode );
		this->weaponNum = (short)weaponNum_;
		this->isBuiltin = true;
	}

	GenericFireDef( int weaponNum_, const AiScriptWeaponDef *scriptWeaponDef ) {
		this->projectileSpeed = scriptWeaponDef->projectileSpeed;
		this->splashRadius = scriptWeaponDef->splashRadius;
		this->aimType = scriptWeaponDef->aimType;
		this->weaponNum = (short)weaponNum_;
		this->isBuiltin = false;
	}

	inline int WeaponNum() const { return weaponNum; }
	inline bool IsBuiltin() const { return isBuiltin; }

	inline ai_weapon_aim_type AimType() const { return aimType; }
	inline float ProjectileSpeed() const { return projectileSpeed; }
	inline float SplashRadius() const { return splashRadius; }
	inline bool IsContinuousFire() const { return isBuiltin; }
};

class SelectedWeapons
{
	friend class BotWeaponsUsageModule;
	friend class BotWeaponSelector;
	friend class Bot;

	GenericFireDef builtinFireDef;
	GenericFireDef scriptFireDef;

	int64_t timeoutAt;
	unsigned instanceId;

	bool preferBuiltinWeapon;
	bool hasSelectedBuiltinWeapon;
	bool hasSelectedScriptWeapon;

	SelectedWeapons()
		: timeoutAt( 0 ),
		instanceId( 0 ),
		preferBuiltinWeapon( true ),
		hasSelectedBuiltinWeapon( false ),
		hasSelectedScriptWeapon( false ) {}

public:
	inline const GenericFireDef *BuiltinFireDef() const {
		return hasSelectedBuiltinWeapon ? &builtinFireDef : nullptr;
	}
	inline const GenericFireDef *ScriptFireDef() const {
		return hasSelectedScriptWeapon ? &scriptFireDef : nullptr;
	}
	inline int BuiltinWeaponNum() const {
		return hasSelectedBuiltinWeapon ? builtinFireDef.WeaponNum() : -1;
	}
	inline int ScriptWeaponNum() const {
		return hasSelectedScriptWeapon ? scriptFireDef.WeaponNum() : -1;
	}
	inline unsigned InstanceId() const { return instanceId; }
	inline bool AreValid() const { return timeoutAt > level.time; }
	inline void Invalidate() { timeoutAt = level.time; }
	inline int64_t TimeoutAt() const { return timeoutAt; }
	inline bool PreferBuiltinWeapon() const { return preferBuiltinWeapon; }
};

class Bot;
class WeaponsToSelect;

class BotWeaponSelector {
	Bot *const bot;

	float weaponChoiceRandom { 0.5f };
	int64_t weaponChoiceRandomTimeoutAt { 0 };

	int64_t nextFastWeaponSwitchActionCheckAt { 0 };
	const unsigned weaponChoicePeriod;

public:
	BotWeaponSelector( Bot *bot_, unsigned weaponChoicePeriod_ )
		: bot( bot_ ), weaponChoicePeriod( weaponChoicePeriod_ ) {}

	void Frame();
	void Think();

private:
	[[nodiscard]]
	bool checkFastWeaponSwitchAction();

	[[nodiscard]]
	bool hasWeakOrStrong( int weapon ) const;

	void selectWeapon();

	[[nodiscard]]
	auto suggestSniperRangeWeapon( float distanceToEnemy ) -> std::optional<int>;
	[[nodiscard]]
	auto suggestFarRangeWeapon( float distanceToEnemy ) -> std::optional<int>;
	[[nodiscard]]
	auto suggestMiddleRangeWeapon( float distanceToEnemy ) -> std::optional<int>;
	[[nodiscard]]
	auto suggestCloseRangeWeapon( float distanceToEnemy ) -> std::optional<int>;

	[[nodiscard]]
	auto suggestFarOrSniperPositionalCombatWeapon( bool hasEB, bool hasMG ) -> std::optional<int>;

	[[nodiscard]]
	auto suggestInstagibWeapon( float distanceToEnemy ) -> std::optional<int>;
	[[nodiscard]]
	auto suggestFinishWeapon() -> std::optional<int>;

	[[nodiscard]]
	auto suggestScriptWeapon( float distanceToEnemy ) -> std::optional<std::pair<int, int>>;

	void setSelectedWeapons( const WeaponsToSelect &weaponsToSelect, unsigned timeoutPeriod );
};

#endif
