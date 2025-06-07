#ifndef WSW_3da5c088_b618_40ab_885f_273c60bffd82_H
#define WSW_3da5c088_b618_40ab_885f_273c60bffd82_H

#include "../component.h"
#include <common/helpers/freelistallocator.h>
#include <common/types/staticdeque.h>
#include <common/types/staticvector.h>
#include "../vec3.h"
#include <common/facilities/q_comref.h>
#include <limits>

inline bool HasQuad( const edict_t *ent ) {
	return ent && ent->r.client && ent->r.client->ps.inventory[POWERUP_QUAD];
}

inline bool HasShell( const edict_t *ent ) {
	return ent && ent->r.client && ent->r.client->ps.inventory[POWERUP_SHELL];
}

inline bool HasPowerups( const edict_t *ent ) {
	if( !ent || !ent->r.client ) {
		return false;
	}
	return ent->r.client->ps.inventory[POWERUP_QUAD] && ent->r.client->ps.inventory[POWERUP_SHELL];
}

inline bool IsCarrier( const edict_t *ent ) {
	return ent && ent->r.client && ent->s.effects & EF_CARRIER;
}

float DamageToKill( const edict_t *ent, float armorProtection, float armorDegradation );

float DamageToKill( float health, float armor, float armorProtection, float armorDegradation );

class EnemiesTracker;

class TrackedEnemy {
	friend class EnemiesTracker;
	const EnemiesTracker *const m_parent { nullptr };
public:
	enum class HitFlags: int {
		NONE = 0,
		RAIL = 1,
		ROCKET = 2,
		SHAFT = 4,
		ALL = 7
	};

	TrackedEnemy *prev { nullptr }, *next { nullptr };

	[[nodiscard]]
	auto NextInTrackedList() const -> const TrackedEnemy * { return next; };
private:
	float m_lastSelectionWeight { 0.0f };
	float m_avgPositiveWeightSoFar { 0.0f };
	float m_maxPositiveWeightSoFar { 0.0f };
	unsigned m_positiveWeightsCount { 0 };

	const int64_t m_registeredAt { 0 };

	// Same as front() of lastSeenTimestamps, used for faster access
	int64_t m_lastSeenAt { 0 };

	// Same as front() of lastSeenOrigins, used for faster access
	Vec3 m_lastSeenOrigin { 0, 0, 0 };
	// Same as front() of lastSeenVelocities, used for faster access
	Vec3 m_lastSeenVelocity { 0, 0, 0 };

	// Some intermediates that should be cached for consequent MightBlockArea() flags
	mutable int64_t m_lookDirComputedAt { 0 };
	mutable int64_t m_weaponHitFlagsComputedAt { 0 };

	mutable HitFlags m_cachedWeaponHitFlags { 0 };
	mutable float m_cachedWeaponHitKillDamage { 0.0f };

	mutable vec3_t m_lookDir { 0.0f, 0.0f, 1.0f };

	HitFlags ComputeCheckForWeaponHitFlags( float damageToKillTarget ) const;
public:
	const edict_t *const m_ent;

	TrackedEnemy( const EnemiesTracker *parent, const edict_t *ent, int64_t registeredAt )
		: m_parent( parent ), m_registeredAt( registeredAt ), m_ent( ent ) {}

	static constexpr unsigned MAX_TRACKED_SNAPSHOTS = 16;

	void Clear();
	void OnViewed( const float *overrideEntityOrigin = nullptr );

	inline const char *Nick() const {
		if( !m_ent ) {
			return "???";
		}
		return m_ent->r.client ? m_ent->r.client->netname.data() : m_ent->classname;
	}

	int EntNum() const { return m_ent->s.number; }

	bool HasQuad() const { return ::HasQuad( m_ent ); }
	bool HasShell() const { return ::HasShell( m_ent ); }
	bool HasPowerups() const { return ::HasPowerups( m_ent ); }
	bool IsCarrier() const { return ::IsCarrier( m_ent ); }

	template<int Weapon>
	int AmmoReadyToFireCount() const {
		if( auto *client = m_ent->r.client ) {
			if( const int *inventory = client->ps.inventory; inventory[Weapon] ) {
				constexpr int indexShift = Weapon - WEAP_GUNBLADE;
				return inventory[AMMO_GUNBLADE + indexShift] + inventory[AMMO_WEAK_GUNBLADE + indexShift];
			}
		}
		return 0;
	}

	int ShellsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_RIOTGUN>(); }
	int GrenadesReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_GRENADELAUNCHER>(); }
	int RocketsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_ROCKETLAUNCHER>(); }
	int PlasmasReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_PLASMAGUN>(); }
	int BulletsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_MACHINEGUN>(); }
	int LasersReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_LASERGUN>(); }
	int BoltsReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_ELECTROBOLT>(); }
	int WavesReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_SHOCKWAVE>(); }
	int InstasReadyToFireCount() const { return AmmoReadyToFireCount<WEAP_INSTAGUN>(); }

	bool IsShootableCurrWeapon( int weapon ) const;
	bool IsShootableCurrOrPendingWeapon( int weapon ) const;

	bool TriesToKeepUnderXhair( const float *origin ) const;

	inline unsigned FireDelay() const {
		return m_ent->r.client ? m_ent->r.client->ps.stats[STAT_WEAPON_TIME] : 0u;
	}

	int64_t LastSeenAt() const { return m_lastSeenAt; }
	Vec3 LastSeenOrigin() const { return m_lastSeenOrigin; }
	Vec3 LastSeenVelocity() const { return m_lastSeenVelocity; }

	int64_t LastAttackedByTime() const;
	float TotalInflictedDamage() const;

	bool IsValid() const {
		return m_ent && !( G_ISGHOSTING( m_ent ) );
	}

	float AvgWeight() const { return m_avgPositiveWeightSoFar; }
	float MaxWeight() const { return m_maxPositiveWeightSoFar; }

	Vec3 LookDir() const;

	HitFlags GetCheckForWeaponHitFlags( float damageToKillTarget ) const;

	inline Vec3 Angles() const { return Vec3( m_ent->s.angles ); }

	class alignas ( 4 )Snapshot {
		Int64Align4 timestamp;
		int16_t packedOrigin[3];
		int16_t packedVelocity[3];
		int16_t angles[2];
	public:
		Snapshot( const vec3_t origin_, const vec3_t velocity_, const vec3_t angles_, int64_t timestamp_ ) {
			this->timestamp = timestamp_;
			SetPacked4uVec( origin_, this->packedOrigin );
			SetPacked4uVec( velocity_, this->packedVelocity );
			angles[0] = (int16_t)angles_[PITCH];
			angles[1] = (int16_t)angles_[YAW];
		}

		int64_t Timestamp() const { return timestamp; }
		Vec3 Origin() const { return GetUnpacked4uVec( packedOrigin ); }
		Vec3 Velocity() const { return GetUnpacked4uVec( packedVelocity ); }

		Vec3 Angles() const {
			vec3_t result;
			result[PITCH] = angles[0];
			result[YAW] = angles[1];
			result[ROLL] = 0;
			return Vec3( result );
		}
	};

	using SnapshotsQueue = wsw::StaticDeque<Snapshot, MAX_TRACKED_SNAPSHOTS>;
	SnapshotsQueue m_lastSeenSnapshots;
};

class AttackStats {
	friend class EnemiesTracker;

	// Very close to 8 game seconds
	static constexpr unsigned kMaxKeptFrames = 64 * 8;

	static_assert( wsw::isPowerOf2( kMaxKeptFrames ), "Should be a power of 2 for fast modulo computation" );

	// Damage is saturated up to 255 units.
	// Storing greater values not only does not make sense, but leads to non-efficient memory usage/cache access.
	uint8_t m_frameDamages[kMaxKeptFrames] {};

	unsigned m_frameIndex { 0 };
	unsigned m_totalAttacks { 0 };
	int64_t m_lastDamageAt { 0 };
	int64_t m_lastTouchAt { 0 };
	int m_totalDamage { 0 };
	const int m_entNum { -1 };

public:
	AttackStats *prev { nullptr }, *next { nullptr };

	explicit AttackStats( int entNum ) : m_entNum( entNum ) {}

	// Call it once in a game frame
	void Frame() {
		const uint8_t overwrittenDamage = m_frameDamages[m_frameIndex];
		m_frameIndex = ( m_frameIndex + 1 ) % kMaxKeptFrames;
		m_totalDamage -= overwrittenDamage;
		m_frameDamages[m_frameIndex] = 0;

		if( overwrittenDamage > 0 ) {
			m_totalAttacks--;
		}
	}

	// Call it after Frame() in the same frame
	void OnDamage( float damage ) {
		if( damage > 0.0f ) {
			m_frameDamages[m_frameIndex] = (uint8_t)( wsw::clamp( damage, 1.0f, 255.0f ) );
			m_totalDamage += m_frameDamages[m_frameIndex];
			m_totalAttacks++;
			m_lastDamageAt = level.time;
		}
	}

	// Call it after Frame() in the same frame if damage is not registered
	// but you want to mark frame as a frame of activity anyway
	void Touch() { m_lastTouchAt = level.time; }

	[[nodiscard]]
	int64_t LastActivityAt() const { return wsw::max( m_lastDamageAt, m_lastTouchAt ); }
};

class BotAwarenessModule;

class EnemiesTracker : public AiComponent {
	friend class TrackedEnemy;
	friend class BotAwarenessModule;
	friend class AiSquad;

	static constexpr unsigned kAttackerTimeout = 7500;
	static constexpr unsigned kTargetTimeout   = 5000;
private:
	Bot *const m_bot;
	BotAwarenessModule *const m_module;

	// An i-th element corresponds to i-th entity
	TrackedEnemy *m_entityToEnemyTable[MAX_EDICTS] {};

	AttackStats *m_entityToAttackerStatsTable[MAX_EDICTS] {};
	AttackStats *m_entityToTargetStatsTable[MAX_EDICTS] {};

	// List heads for tracked and active enemies lists
	TrackedEnemy *m_trackedEnemiesHead { nullptr };

	AttackStats *m_trackedAttackerStatsHead { nullptr };
	AttackStats *m_trackedTargetStatsHead { nullptr };

	wsw::HeapBasedFreelistAllocator m_trackedEnemiesAllocator { sizeof( TrackedEnemy ), MAX_EDICTS };
	wsw::HeapBasedFreelistAllocator m_attackerStatsAllocator { sizeof( AttackStats ), MAX_EDICTS };
	wsw::HeapBasedFreelistAllocator m_targetStatsAllocator { sizeof( AttackStats ), MAX_EDICTS };

	void RemoveEnemy( TrackedEnemy *enemy );

	void UpdateEnemyWeight( TrackedEnemy *enemy, int64_t reactionTime );

	virtual float ComputeRawEnemyWeight( const edict_t *enemy );

	/**
	 * Modifies weight for an enemy that has been an attacker of the bot recently.
	 * @param enemy an enemy underlying entity (for a newly added or updated enemy)
	 * @param weightSoFar a weight of the enemy computed to the moment of this call
	 * @return a modified value of the enemy weight (can't be less than the supplied one)
	 */
	virtual float ModifyWeightForAttacker( const edict_t *enemy, float weightSoFar );

	/**
	 * Modifies weight for an enemy that has been hit by the bot recently.
	 * @param enemy an enemy underlying entity (for a newly added or updated enemy)
	 * @param weightSoFar a weight of the enemy computed to the moment of this call
	 * @return a modified value of the enemy weight (can't be less than the supplied one)
	 */
	virtual float ModifyWeightForHitTarget( const edict_t *enemy, float weightSoFar );

	/**
	 * Modifies weight for an enemy based on a "kill enemy/be killed by enemy" damage ratio
	 * @param enemy an enemy underlying entity (for a newly added or updated enemy)
	 * @param weightSoFar a weight of the enemy computed to the moment of this call
	 * @return a modified value of the enemy weight (can't be less that the supplied one)
	 */
	virtual float ModifyWeightForDamageRatio( const edict_t *enemy, float weightSoFar );

	AttackStats * EnqueueAttacker( const edict_t *attacker, int damage );

protected:
public:
	EnemiesTracker( Bot *bot, BotAwarenessModule *module ) : m_bot( bot ), m_module( module ) {}
	~EnemiesTracker() override;

	[[nodiscard]]
	auto TrackedEnemiesHead() const -> const TrackedEnemy * { return m_trackedEnemiesHead; }

	void Update();

	void OnEnemyViewed( const edict_t *enemy );
	void OnEnemyOriginGuessed( const edict_t *enemy,
							   unsigned minMillisSinceLastSeen,
							   const float *guessedOrigin = nullptr );

	const TrackedEnemy *ChooseVisibleEnemy();
	const TrackedEnemy *ChooseLostOrHiddenEnemy( std::optional<unsigned> timeout = std::nullopt );

	void OnPain( const edict_t *bot, const edict_t *enemy, float kick, int damage );
	void OnEnemyDamaged( const edict_t *bot, const edict_t *target, int damage );

	void EnqueueTarget( const edict_t *target );

	// Returns zero if ent not found
	int64_t LastAttackedByTime( const edict_t *ent ) const;
	int64_t LastTargetTime( const edict_t *ent ) const;

	float TotalDamageInflictedBy( const edict_t *ent ) const;
};

inline int64_t TrackedEnemy::LastAttackedByTime() const { return m_parent->LastAttackedByTime( m_ent ); }
inline float TrackedEnemy::TotalInflictedDamage() const { return m_parent->TotalDamageInflictedBy( m_ent ); }

#endif
