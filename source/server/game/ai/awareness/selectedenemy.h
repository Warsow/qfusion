#ifndef WSW_0457e810_33e9_4523_9ae9_ad62853bf134_H
#define WSW_0457e810_33e9_4523_9ae9_ad62853bf134_H

#include "enemiestracker.h"

class SelectedEnemy {
	friend class Bot;
	friend class BotAwarenessModule;

	const Bot *m_bot;
	const TrackedEnemy *m_enemy;
	int64_t m_timeoutAt { 0 };
	unsigned m_instanceId { 0 };

	// TODO: Just use pragmas?

	struct alignas( 4 ) FrameCachedFloat {
		mutable Int64Align4 computedAt { -1 };
		mutable float value { std::numeric_limits<float>::infinity() };
	};

	struct alignas( 4 ) FrameCachedBool {
		mutable Int64Align4 computedAt { -1 };
		mutable bool value { false };
	};

	mutable FrameCachedFloat m_maxThreatFactor;
	mutable FrameCachedBool m_canEnemiesHit;
	mutable FrameCachedBool m_couldHitIfTurns;
	mutable FrameCachedFloat m_botViewDirDotToEnemyDir;
	mutable FrameCachedFloat m_enemyViewDirDotToBotDir;
	mutable FrameCachedBool m_aboutToHitEBorIG;
	mutable FrameCachedBool m_aboutToHitLGorPG;
	mutable FrameCachedBool m_aboutToHitRLorSW;
	mutable FrameCachedBool m_arePotentiallyHittable;

	bool IsValid() const { return m_enemy->IsValid() && m_timeoutAt > level.time; }

	bool ShouldInvalidate() const { return !IsValid(); }

	void CheckValid( const char *function ) const {
#ifdef _DEBUG
		if( IsValid() ) {
			return;
		}

		char tag[64];
		AI_FailWith( va_r( tag, sizeof( tag ), "SelectedEnemy::%s()", tag ), "Selected enemies are invalid\n" );
#endif
	}

	explicit SelectedEnemy( const Bot *bot, const TrackedEnemy *enemy, int64_t timeoutAt, unsigned instanceId )
		: m_bot( bot ), m_enemy( enemy ), m_timeoutAt( timeoutAt ), m_instanceId( instanceId ) {}

	bool TestAboutToHitEBorIG( int64_t levelTime ) const;
	bool TestAboutToHitLGorPG( int64_t levelTime ) const;
	bool TestAboutToHitRLorSW( int64_t levelTime ) const;

	bool IsAboutToHit( FrameCachedBool *cached, bool ( SelectedEnemy::*testHit )( int64_t ) const ) const;

	bool TestIsPotentiallyHittable() const;
public:
	float GetBotViewDirDotToEnemyDir() const;
	float GetEnemyViewDirDotToBotDir() const;

	unsigned InstanceId() const { return m_instanceId; }

	bool IsBasedOn( const edict_t *ent ) const {
		return m_enemy->m_ent == ent;
	}

	bool IsBasedOn( const TrackedEnemy *enemy ) const {
		return m_enemy == enemy;
	}

	Vec3 LastSeenOrigin() const {
		return m_enemy->LastSeenOrigin();
	}

	Vec3 ActualOrigin() const {
		CheckValid( "ActualOrigin" );
		return Vec3( m_enemy->m_ent->s.origin );
	}

	Vec3 LastSeenVelocity() const {
		CheckValid( "LastSeenVelocity" );
		return m_enemy->LastSeenVelocity();
	}

	int64_t LastSeenAt() const {
		CheckValid( "LastSeenAt" );
		return m_enemy->LastSeenAt();
	}

	Vec3 ActualVelocity() const {
		CheckValid( "ActualVelocity" );
		return Vec3( m_enemy->m_ent->velocity );
	}

	Vec3 LookDir() const;

	float DamageToKill() const;

	int PendingWeapon() const;

	unsigned FireDelay() const {
		CheckValid( "FireDelay" );
		return m_enemy->FireDelay();
	}

	const TrackedEnemy *GetTrackedEnemy() const {
		CheckValid( "GetTrackedEnemy" );
		return m_enemy;
	}

	bool IsStaticSpot() const {
		return Ent()->r.client == nullptr;
	}

	const edict_t *Ent() const {
		CheckValid( "Ent" );
		return m_enemy->m_ent;
	}

	const edict_t *TraceKey() const {
		CheckValid( "TraceKey" );
		return m_enemy->m_ent;
	}

	bool OnGround() const {
		CheckValid( "OnGround" );
		return m_enemy->m_ent->groundentity != nullptr;
	}

	bool HasQuad() const {
		CheckValid( "HasQuad" );
		return m_enemy->HasQuad();
	}

	bool IsACarrier() const {
		CheckValid( "HasCarrier" );
		return m_enemy->IsCarrier();
	}

	float TotalInflictedDamage() const {
		CheckValid( "TotalInflictedDamage" );
		return m_enemy->TotalInflictedDamage();
	}

	// Checks whether a bot can potentially hit enemies from its origin if it adjusts view angles properly
	bool IsPotentiallyHittable() const;

	bool EnemyCanHit() const;
	static bool TestCanHit( const edict_t *attacker, const edict_t *victim, float viewDot );

	bool EnemyCanBeHit() const;
	bool CouldBeHitIfBotTurns() const;

	[[nodiscard]]
	bool IsAboutToHitEBorIG() const {
		return IsAboutToHit( &m_aboutToHitEBorIG, &SelectedEnemy::TestAboutToHitEBorIG );
	}

	[[nodiscard]]
	bool IsAboutToHitRLorSW() const {
		return IsAboutToHit( &m_aboutToHitRLorSW, &SelectedEnemy::TestAboutToHitRLorSW );
	}

	[[nodiscard]]
	bool IsAboutToHitLGorPG() const {
		return IsAboutToHit( &m_aboutToHitLGorPG, &SelectedEnemy::TestAboutToHitLGorPG );
	}

	bool IsThreatening() const {
		CheckValid( "IsThreatening" );
		return MaxThreatFactor() > 0.9f;
	}

	float MaxThreatFactor() const;
	static float ComputeThreatFactor( const Bot *bot, const edict_t *ent,
									  const SelectedEnemy *useForCacheLookup = nullptr );
};

#endif
