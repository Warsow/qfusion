#include "enemiestracker.h"
#include "../bot.h"

constexpr float MAX_ENEMY_WEIGHT = 5.0f;

float DamageToKill( const edict_t *ent, float armorProtection, float armorDegradation ) {
	if( !ent || !ent->takedamage ) {
		return std::numeric_limits<float>::infinity();
	}

	if( !ent->r.client ) {
		return ent->health;
	}

	float health = ent->r.client->ps.stats[STAT_HEALTH];
	float armor = ent->r.client->ps.stats[STAT_ARMOR];

	return DamageToKill( health, armor, armorProtection, armorDegradation );
}

float DamageToKill( float health, float armor, float armorProtection, float armorDegradation ) {
	if( armor <= 0.0f ) {
		return health;
	}
	if( armorProtection == 1.0f ) {
		return std::numeric_limits<float>::infinity();
	}

	if( armorDegradation != 0 ) {
		float damageToWipeArmor = armor / armorDegradation;
		float healthDamageToWipeArmor = damageToWipeArmor * ( 1.0f - armorProtection );

		if( healthDamageToWipeArmor < health ) {
			return damageToWipeArmor + ( health - healthDamageToWipeArmor );
		}

		return health / ( 1.0f - armorProtection );
	}

	return health / ( 1.0f - armorProtection );
}

void TrackedEnemy::OnViewed( const float *overrideEntityOrigin ) {
	if( m_lastSeenSnapshots.size() == MAX_TRACKED_SNAPSHOTS ) {
		m_lastSeenSnapshots.pop_front();
	}

	// Put the likely case first
	const float *origin = !overrideEntityOrigin ? m_ent->s.origin : overrideEntityOrigin;
	// Set members for faster access
	VectorCopy( origin, m_lastSeenOrigin.Data() );
	VectorCopy( m_ent->velocity, m_lastSeenVelocity.Data() );
	m_lastSeenAt = level.time;
	// Store in a queue then for history
	m_lastSeenSnapshots.emplace_back( Snapshot( m_ent->s.origin, m_ent->velocity, m_ent->s.angles, level.time ) );
}

Vec3 TrackedEnemy::LookDir() const {
	const auto levelTime = level.time;
	if( m_lookDirComputedAt != levelTime ) {
		m_lookDirComputedAt = levelTime;
		if ( const Bot *bot = m_ent->bot ) {
			bot->EntityPhysicsState()->ForwardDir().CopyTo( m_lookDir );
		} else {
			AngleVectors( m_ent->s.angles, m_lookDir, nullptr, nullptr );
		}
	}
	return Vec3( m_lookDir );
}

static inline bool HasAmmoForWeapon( const Client *client, int weapon ) {
	assert( weapon >= WEAP_NONE && weapon < WEAP_TOTAL );
	const auto *inventory = client->ps.inventory;
	constexpr int shifts[2] = { (int)AMMO_GUNBLADE - (int)WEAP_GUNBLADE, (int)AMMO_WEAK_GUNBLADE - (int)WEAP_GUNBLADE };
	static_assert( shifts[0] > 0 && shifts[1] > 0, "" );
	return ( inventory[weapon + shifts[0]] | inventory[weapon + shifts[1]] ) != 0;
}

bool TrackedEnemy::IsShootableCurrWeapon( int weapon ) const {
	const auto *client = m_ent->r.client;
	if( !client ) {
		return false;
	}

	const auto *playerStats = client->ps.stats;
	if( playerStats[STAT_WEAPON] != weapon ) {
		return false;
	}

	return HasAmmoForWeapon( client, weapon );
}

bool TrackedEnemy::IsShootableCurrOrPendingWeapon( int weapon ) const {
	const auto *client = m_ent->r.client;
	if( !client ) {
		return false;
	}

	const auto *playerStats = client->ps.stats;
	const bool isCurrentWeapon = playerStats[STAT_WEAPON] == weapon;
	const bool isPendingWeapon = playerStats[STAT_PENDING_WEAPON] == weapon;
	if( !( isCurrentWeapon | isPendingWeapon ) ) {
		return false;
	}

	return HasAmmoForWeapon( client, weapon );
}

bool TrackedEnemy::TriesToKeepUnderXhair( const float *origin ) const {
	float lastDot = -1.0f - 0.01f;
	float bestDot = -1.0f - 0.01f;
	float prevDot = -1.0f - 0.01f;
	const auto levelTime = level.time;
	bool isMonotonicallyIncreasing = true;
	for( const auto &snapshot: m_lastSeenSnapshots ) {
		if( levelTime - snapshot.Timestamp() > 500 ) {
			continue;
		}

		prevDot = lastDot;

		Vec3 toOriginDir( snapshot.Origin() );
		toOriginDir.Z() += playerbox_stand_viewheight;
		toOriginDir -= origin;
		float squareDistance = toOriginDir.SquaredLength();
		if( squareDistance < 1 ) {
			lastDot = bestDot = 1.0f;
			continue;
		}

		toOriginDir *= -1.0f * Q_RSqrt( squareDistance );
		vec3_t lookDir;
		AngleVectors( snapshot.Angles().Data(), lookDir, nullptr, nullptr );

		const float dot = toOriginDir.Dot( lookDir );
		if( dot > bestDot ) {
			// Return immediately in this case
			if( dot > 0.995f ) {
				return true;
			}
			bestDot = dot;
		}
		if( isMonotonicallyIncreasing ) {
			if( dot <= lastDot ) {
				isMonotonicallyIncreasing = false;
			}
		}
		lastDot = dot;
	}

	if( lastDot > prevDot && lastDot > 0.99f ) {
		return true;
	}

	if( isMonotonicallyIncreasing && bestDot > 0.95f ) {
		return true;
	}

	return false;
}

TrackedEnemy::HitFlags TrackedEnemy::GetCheckForWeaponHitFlags( float damageToKillTarget ) const {
	auto levelTime = level.time;

	if( m_weaponHitFlagsComputedAt != levelTime || m_cachedWeaponHitKillDamage != damageToKillTarget ) {
		m_weaponHitFlagsComputedAt  = levelTime;
		m_cachedWeaponHitKillDamage = damageToKillTarget;
		m_cachedWeaponHitFlags      = ComputeCheckForWeaponHitFlags( damageToKillTarget );
	}

	return m_cachedWeaponHitFlags;
}

TrackedEnemy::HitFlags TrackedEnemy::ComputeCheckForWeaponHitFlags( float damageToKillTarget ) const {
	if( !m_ent->r.client ) {
		return HitFlags::NONE;
	}

	int flags = 0;
	if( damageToKillTarget < 150 ) {
		if( RocketsReadyToFireCount() || WavesReadyToFireCount()) {
			flags |= (int)HitFlags::ROCKET;
		}
	}

	if( InstasReadyToFireCount() ) {
		flags |= (int)HitFlags::RAIL;
	} else if( ( HasQuad() || damageToKillTarget < 140 ) && BoltsReadyToFireCount() ) {
		flags |= (int)HitFlags::RAIL;
	}

	if( LasersReadyToFireCount() && damageToKillTarget < 80 ) {
		flags |= (int)HitFlags::SHAFT;
	} else if( BulletsReadyToFireCount() && ( ( HasQuad() || damageToKillTarget < 30 ) ) ) {
		flags |= (int)HitFlags::SHAFT;
	}

	return (HitFlags)flags;
}

EnemiesTracker::~EnemiesTracker() noexcept {
	for( AttackStats *stats = m_trackedAttackerStatsHead, *next; stats; stats = next ) { next = stats->next;
		stats->~AttackStats();
		m_attackerStatsAllocator.free( stats );
	}
	for( AttackStats *stats = m_trackedTargetStatsHead, *next; stats; stats = next ) { next = stats->next;
		stats->~AttackStats();
		m_targetStatsAllocator.free( stats );
	}
	for( TrackedEnemy *enemy = m_trackedEnemiesHead, *next; enemy; enemy = next ) { next = enemy->next;
		enemy->~TrackedEnemy();
		m_trackedEnemiesAllocator.free( enemy );
	}
}

void EnemiesTracker::Update() {
	const int64_t levelTime = level.time;

	for( AttackStats *stats = m_trackedAttackerStatsHead, *next; stats; stats = next ) { next = stats->next;
		stats->Frame();
		if( stats->LastActivityAt() + kAttackerTimeout <= levelTime ) {
			m_entityToAttackerStatsTable[stats->m_entNum] = nullptr;
			wsw::unlink( stats, &m_trackedAttackerStatsHead );
			stats->~AttackStats();
		}
	}

	for( AttackStats *stats = m_trackedTargetStatsHead, *next; stats; stats = next ) { next = stats->next;
		stats->Frame();
		if( stats->LastActivityAt() + kTargetTimeout <= levelTime ) {
			m_entityToTargetStatsTable[stats->m_entNum] = nullptr;
			wsw::unlink( stats, &m_trackedTargetStatsHead );
			stats->~AttackStats();
		}
	}

	// Process urgent events.
	for( TrackedEnemy *enemy = m_trackedEnemiesHead, *next; enemy; enemy = next ) { next = enemy->next;
		// If the enemy cannot be longer valid
		if( G_ISGHOSTING( enemy->m_ent ) ) {
			Debug( "The enemy %s is ghosting, should be forgot\n", enemy->Nick() );
			RemoveEnemy( enemy );
		} else if( enemy->m_ent->s.teleported ) {
			// Update origin immediately by the destination
			if( levelTime - enemy->m_lastSeenAt < 64 ) {
				enemy->OnViewed();
			}
		}
	}

	const auto reactionTime = (int64_t)wsw::min( 48.0f, 320.0f * ( 1.0f - m_bot->Skill() ) );

	if( m_bot->PermitsDistributedUpdateThisFrame() ) {
		TrackedEnemy *nextEnemy = nullptr;
		for( TrackedEnemy *enemy = m_trackedEnemiesHead; enemy; enemy = nextEnemy ) { nextEnemy = enemy->next;
			// Remove not seen yet enemies
			if( const auto diff = (int)( levelTime - enemy->LastSeenAt() ); diff > 15000 ) {
				Debug( "has not seen %s for %d ms, should forget this enemy\n", enemy->Nick(), diff );
				RemoveEnemy( enemy );
				continue;
			}

			// Do not forget, just skip
			if( enemy->m_ent->flags & ( FL_NOTARGET | FL_BUSY ) ) {
				continue;
			}
			// Skip during reaction time
			if( enemy->m_registeredAt + reactionTime > levelTime ) {
				continue;
			}

			UpdateEnemyWeight( enemy, reactionTime );
		}
	}
}

float EnemiesTracker::ModifyWeightForAttacker( const edict_t *enemy, float weightSoFar ) {
	// Don't engage in fights
	if( m_bot->WillRetreat() || m_bot->IsNavTargetATopTierItem() ) {
		return weightSoFar;
	}
	if( int64_t time = LastAttackedByTime( enemy ) ) {
		// TODO: Add weight for poor attackers (by total damage / attack attempts ratio)
		return weightSoFar + 1.5f * ( 1.0f - BoundedFraction( level.time - time, kAttackerTimeout ) );
	}
	return weightSoFar;
}

float EnemiesTracker::ModifyWeightForHitTarget( const edict_t *enemy, float weightSoFar ) {
	// Don't engage in fights
	if( m_bot->WillRetreat() || m_bot->IsNavTargetATopTierItem() ) {
		return weightSoFar;
	}
	if( int64_t time = LastTargetTime( enemy ) ) {
		// TODO: Add weight for targets that are well hit by bot
		return weightSoFar + 1.5f * ( 1.0f - BoundedFraction( level.time - time, kTargetTimeout ) );
	}
	return weightSoFar;
}

float EnemiesTracker::ModifyWeightForDamageRatio( const edict_t *enemy, float weightSoFar ) {
	constexpr float maxDamageToKill = 350.0f;

	const edict_t *botEnt   = game.edicts + m_bot->EntNum();
	float damageToKillEnemy = DamageToKill( enemy, g_armor_protection->value, g_armor_degradation->value );
	if( ::HasQuad( botEnt ) ) {
		damageToKillEnemy /= QUAD_DAMAGE_SCALE;
	}
	if( ::HasShell( enemy ) ) {
		damageToKillEnemy *= QUAD_DAMAGE_SCALE;
	}

	const float damageToBeKilled = DamageToKill( botEnt, g_armor_protection->value, g_armor_degradation->value );
	return weightSoFar + ( damageToBeKilled - damageToKillEnemy ) * ( 1.0f / maxDamageToKill );
}

float EnemiesTracker::ComputeRawEnemyWeight( const edict_t *enemy ) {
	if( !enemy || G_ISGHOSTING( enemy ) ) {
		return 0.0;
	}

	float weight = 0.5f;
	if( !enemy->r.client ) {
		weight = enemy->aiIntrinsicEnemyWeight;
		if( weight <= 0.0f ) {
			return 0.0f;
		}
	}

	weight = ModifyWeightForAttacker( enemy, weight );
	weight = ModifyWeightForHitTarget( enemy, weight );

	// Should we keep this hardcoded?
	if( ::IsCarrier( enemy ) ) {
		weight += 2.0f;
	}

	weight = ModifyWeightForDamageRatio( enemy, weight );
	Q_clamp( weight, 0.0f, MAX_ENEMY_WEIGHT );
	return weight;
}

void EnemiesTracker::OnPain( const edict_t *botEnt, const edict_t *enemy, float kick, int damage ) {
	const AttackStats *attackByGivenEnemyStats = EnqueueAttacker( enemy, damage );

	bool detectNewThreat = true;
	if( botEnt->bot->IsPrimaryAimEnemy( enemy ) ) {
		detectNewThreat = false;
		const AttackStats *attackByCurrEnemyStats = nullptr;
		for( const AttackStats *stats = m_trackedAttackerStatsHead; stats; stats = stats->next ) {
			if( botEnt->bot->IsPrimaryAimEnemy( game.edicts + stats->m_entNum ) ) {
				attackByCurrEnemyStats = stats;
				break;
			}
		}
		// If the current enemy did not inflict any damage
		// or this attacker hits harder than current one, there is a new threat
		if( !attackByCurrEnemyStats || attackByCurrEnemyStats->m_totalDamage < attackByGivenEnemyStats->m_totalDamage ) {
			detectNewThreat = true;
		}
	}

	if( detectNewThreat ) {
		m_module->OnHurtByNewThreat( enemy, this );
	}
}

int64_t EnemiesTracker::LastAttackedByTime( const edict_t *ent ) const {
	if( const AttackStats *stats = m_entityToAttackerStatsTable[ENTNUM( ent )] ) {
		return stats->m_lastDamageAt;
	}

	return 0;
}

int64_t EnemiesTracker::LastTargetTime( const edict_t *ent ) const {
	if( const AttackStats *stats = m_entityToTargetStatsTable[ENTNUM( ent ) ] ) {
		return stats->m_lastTouchAt;
	}

	return 0;
}

float EnemiesTracker::TotalDamageInflictedBy( const edict_t *ent ) const {
	if( const AttackStats *stats = m_entityToTargetStatsTable[ENTNUM( ent )] ) {
		return (float)stats->m_totalDamage;
	}

	return 0;
}

AttackStats *EnemiesTracker::EnqueueAttacker( const edict_t *attacker, int damage ) {
	const int entNum   = ENTNUM( attacker );
	AttackStats *stats = m_entityToAttackerStatsTable[entNum];
	if( !stats ) {
		stats = new( m_attackerStatsAllocator.allocOrThrow() )AttackStats( entNum );
		m_entityToAttackerStatsTable[entNum] = stats;
	}

	stats->OnDamage( damage );
	return stats;
}

void EnemiesTracker::EnqueueTarget( const edict_t *target ) {
	const int entNum   = ENTNUM( target );
	AttackStats *stats = m_entityToTargetStatsTable[entNum];
	if( !stats ) {
		stats = new( m_targetStatsAllocator.allocOrThrow() )AttackStats( entNum );
		m_entityToTargetStatsTable[entNum] = stats;
	}

	stats->Touch();
}

void EnemiesTracker::OnEnemyDamaged( const edict_t *bot, const edict_t *target, int damage ) {
	// TODO: Is it appropriate to not make a distinction between enemies
	// we aim at intentionally and enemies we happen to hurt?

	const int entNum   = ENTNUM( target );
	AttackStats *stats = m_entityToTargetStatsTable[entNum];
	if( !stats ) {
		stats = new( m_targetStatsAllocator.allocOrThrow() )AttackStats( entNum );
		m_entityToTargetStatsTable[entNum] = stats;
	}

	stats->OnDamage( damage );
}

void EnemiesTracker::UpdateEnemyWeight( TrackedEnemy *enemy, int64_t reactionTime ) {
	// Explicitly limit effective reaction time to a time quantum between Think() calls
	// This method gets called before all enemies are viewed.
	// For seen enemy registration actual weights of known enemies are mandatory
	// (enemies may get evicted based on their weights and weight of a just seen enemy).
	if( level.time - enemy->LastSeenAt() > wsw::max<int64_t>( 64, reactionTime ) ) {
		enemy->m_lastSelectionWeight = 0;
		return;
	}

	enemy->m_lastSelectionWeight = ComputeRawEnemyWeight( enemy->m_ent );
	if( enemy->m_maxPositiveWeightSoFar < enemy->m_lastSelectionWeight ) {
		enemy->m_maxPositiveWeightSoFar = enemy->m_lastSelectionWeight;
	}
	if( enemy->m_lastSelectionWeight > 0 ) {
		float weightsSum = enemy->m_avgPositiveWeightSoFar * (float)enemy->m_positiveWeightsCount;
		weightsSum += enemy->m_lastSelectionWeight;
		enemy->m_positiveWeightsCount++;
		enemy->m_avgPositiveWeightSoFar = weightsSum * Q_Rcp( (float)enemy->m_positiveWeightsCount );
	}
}

const TrackedEnemy *EnemiesTracker::ChooseVisibleEnemy() {
	vec3_t forward;
	const edict_t *botEnt = game.edicts + m_bot->EntNum();
	AngleVectors( botEnt->s.angles, forward, nullptr, nullptr );

	// Until these bounds distance factor scales linearly
	constexpr float distanceBounds = 3500.0f;

	TrackedEnemy *bestEnemy = nullptr;
	float bestEnemyScore    = 0.0f;
	for( TrackedEnemy *enemy = m_trackedEnemiesHead; enemy; enemy = enemy->next ) {
		if( !enemy->IsValid() ) {
			continue;
		}
		// Not seen in this frame enemies have zero weight
		if( enemy->m_lastSelectionWeight <= 0.0f ) {
			continue;
		}

		Vec3 botToEnemy = Vec3( botEnt->s.origin ) - enemy->LastSeenOrigin();
		if( const auto maybeDistance = botToEnemy.normalizeFast( { .minAcceptableLength = 48.0f } ) ) {
			// For far enemies distance factor is lower
			const float distanceFactor = 1.0f - 0.7f * BoundedFraction( *maybeDistance, distanceBounds );
			// Should affect the score only a bit (otherwise bot will miss a dangerous enemy that he is not looking at).
			const float directionFactor = 0.7f + 0.3f * botToEnemy.Dot( forward );

			const float currScore = enemy->m_lastSelectionWeight * distanceFactor * directionFactor;
			if( bestEnemyScore < currScore ) {
				bestEnemyScore = currScore;
				bestEnemy      = enemy;
			}
		} else {
			bestEnemyScore = std::numeric_limits<float>::max();
			bestEnemy      = enemy;
		}
	}

	if( bestEnemy ) {
		EnqueueTarget( bestEnemy->m_ent );
	}

	return bestEnemy;
}

const TrackedEnemy *EnemiesTracker::ChooseLostOrHiddenEnemy( std::optional<unsigned> timeout ) {
	if( m_bot->Skill() < 0.33f ) {
		return nullptr;
	}

	vec3_t forward;
	const edict_t *botEnt = game.edicts + m_bot->EntNum();
	AngleVectors( botEnt->s.angles, forward, nullptr, nullptr );

	unsigned timeoutToUse = 5000;
	if( timeout && *timeout < timeoutToUse ) {
		timeoutToUse = *timeout;
	}

	float bestScore = 0.0f;
	const TrackedEnemy *bestEnemy = nullptr;
	for( TrackedEnemy *enemy = m_trackedEnemiesHead; enemy; enemy = enemy->next ) {
		if( !enemy->IsValid() ) {
			continue;
		}

		// If it has been weighted for selection (and thus was considered visible)
		if( enemy->m_lastSelectionWeight >= 0.0f ) {
			continue;
		}

		float directionFactor = 0.5f, distanceFactor = 1.0f;
		Vec3 botToSpotDirection = enemy->LastSeenOrigin() - botEnt->s.origin;
		if( const auto maybeDistance = botToSpotDirection.normalizeFast( { .minAcceptableLength = 48.0f } ) ) {
			directionFactor = 0.3f + 0.7f * botToSpotDirection.Dot( forward );
			distanceFactor = 1.0f - 0.9f * BoundedFraction( *maybeDistance, 2000.0f );
		}

		float timeFactor = 1.0f - BoundedFraction( level.time - enemy->LastSeenAt(), timeoutToUse );

		float currScore = ( 0.5f * ( enemy->m_maxPositiveWeightSoFar + enemy->m_avgPositiveWeightSoFar ) );
		currScore *= directionFactor * distanceFactor * timeFactor;
		if( currScore > bestScore ) {
			bestScore = currScore;
			bestEnemy = enemy;
		}
	}

	return bestEnemy;
}

void EnemiesTracker::OnEnemyViewed( const edict_t *ent ) {
	TrackedEnemy *enemy = m_entityToEnemyTable[ENTNUM( ent )];
	if( enemy && enemy->IsValid() ) {
		enemy->OnViewed();
	} else {
		void *mem;
		if( enemy ) {
			wsw::unlink( enemy, &m_trackedEnemiesHead );
			enemy->~TrackedEnemy();
			mem = enemy;
		} else {
			mem = m_trackedEnemiesAllocator.allocOrThrow();
		}
		auto *newlyLinkedEnemy = new( mem )TrackedEnemy( this, ent, level.time );
		wsw::link( newlyLinkedEnemy, &m_trackedEnemiesHead );
		newlyLinkedEnemy->OnViewed();
	}
}

void EnemiesTracker::OnEnemyOriginGuessed( const edict_t *ent, unsigned minMillisSinceLastSeen,
										   const float *guessedOrigin ) {
	TrackedEnemy *enemy = m_entityToEnemyTable[ENTNUM( ent )];
	if( enemy && enemy->IsValid() ) {
		// If there is already an Enemy record containing an entity,
		// check whether this record timed out enough to be overwritten.
		if( enemy->m_lastSeenAt + minMillisSinceLastSeen <= level.time ) {
			enemy->OnViewed( guessedOrigin );
		}
	} else {
		void *mem;
		if( enemy ) {
			wsw::unlink( enemy, &m_trackedEnemiesHead );
			enemy->~TrackedEnemy();
			mem = enemy;
		} else {
			mem = m_trackedEnemiesAllocator.allocOrThrow();
		}
		auto *newlyLinkedEnemy = new( mem )TrackedEnemy( this, ent, level.time );
		wsw::link( newlyLinkedEnemy, &m_trackedEnemiesHead );
		newlyLinkedEnemy->OnViewed( guessedOrigin );
	}
}

void EnemiesTracker::RemoveEnemy( TrackedEnemy *enemy ) {
	m_module->OnEnemyRemoved( enemy );

	wsw::unlink( enemy, &m_trackedEnemiesHead );
	enemy->~TrackedEnemy();
	m_trackedEnemiesAllocator.free( enemy );
}