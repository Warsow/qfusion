#include "selectedenemy.h"
#include "entitiespvscache.h"
#include "../bot.h"

Vec3 SelectedEnemy::LookDir() const {
	CheckValid( "LookDir" );

	if( const auto *bot = m_enemy->m_ent->bot ) {
		return bot->EntityPhysicsState()->ForwardDir();
	}

	vec3_t lookDir;
	AngleVectors( m_enemy->m_ent->s.angles, lookDir, nullptr, nullptr );
	return Vec3( lookDir );
}

float SelectedEnemy::DamageToKill() const {
	CheckValid( "DamageToKill" );

	float damageToKill = ::DamageToKill( m_enemy->m_ent, g_armor_protection->value, g_armor_degradation->value );
	if( m_enemy->HasShell() ) {
		damageToKill *= QUAD_DAMAGE_SCALE;
	}

	return damageToKill;
}

int SelectedEnemy::PendingWeapon() const {
	if( const auto *ent = m_enemy->m_ent ) {
		if( const auto *client = ent->r.client ) {
			return client->ps.stats[STAT_PENDING_WEAPON];
		}
	}

	return -1;
}

float SelectedEnemy::MaxThreatFactor() const {
	const auto levelTime = level.time;
	if( m_maxThreatFactor.computedAt != levelTime ) {
		m_maxThreatFactor.computedAt = levelTime;

		m_maxThreatFactor.value = ComputeThreatFactor( m_bot, m_enemy->m_ent, this );
		if( level.time - m_enemy->LastAttackedByTime() < 1000 ) {
			m_maxThreatFactor.value = Q_Sqrt( m_maxThreatFactor.value );
		}
	}
	return m_maxThreatFactor.value;
}

float SelectedEnemy::ComputeThreatFactor( const Bot *bot, const edict_t *ent, const SelectedEnemy *useForCacheLookup ) {
	if( !ent ) {
		return 0.0f;
	}

	// Try cutting off further expensive calls by doing this cheap test first
	if( const auto *client = ent->r.client ) {
		// Can't shoot soon.
		if( client->ps.stats[STAT_WEAPON_TIME] > 800 ) {
			return 0.0f;
		}
	}

	Vec3 enemyToBotDir( Vec3( bot->Origin() ) - ent->s.origin );
	if( !enemyToBotDir.normalizeFast() ) {
		return 1.0f;
	}

	float dot;
	if( useForCacheLookup ) {
		// Try reusing this value that is very likely to be cached
		dot = useForCacheLookup->GetEnemyViewDirDotToBotDir();
	} else {
		vec3_t enemyLookDir;
		AngleVectors( ent->s.angles, enemyLookDir, nullptr, nullptr );
		dot = enemyToBotDir.dot( enemyLookDir );
	}

	// Check whether the enemy is itself a bot.
	// Check whether the bot is an tracked/selected enemy of other bot?
	// This however would make other bots way too special.
	// The code should work fine for all kind of enemies.
	if( Bot *entBot = ent->bot ) {
		if( dot < entBot->FovDotFactor() ) {
			return 0.0f;
		}
	} else if( ent->r.client && dot < 0.2f ) {
		// There is no threat if the bot is not in fov for a client (but not for a turret for example)
		return 0.0f;
	}

	if( !EntitiesPvsCache::Instance()->AreInPvs( ent, game.edicts + bot->EntNum() ) ) {
		return 0.0f;
	}

	if( ent->s.effects & ( EF_QUAD | EF_CARRIER ) ) {
		return 1.0f;
	}

	if( const auto *hazard = bot->PrimaryHazard() ) {
		if( hazard->attacker == ent ) {
			return 0.5f + 0.5f * BoundedFraction( hazard->damage, 75 );
		}
	}

	// Its guaranteed that the enemy cannot hit
	if( dot < 0.7f ) {
		return 0.5f * dot;
	}

	float result = dot;
	// If the enemy belongs to these "selected enemies", try using a probably cached value of the "can hit" test.
	// Otherwise perform a computation (there is no cache for enemies not belonging to this selection)
	if( useForCacheLookup ) {
		if( !useForCacheLookup->EnemyCanHit() ) {
			result *= 0.5f;
		}
	} else if( !TestCanHit( ent, game.edicts + bot->EntNum(), dot ) ) {
		result *= 0.5f;
	}

	return Q_Sqrt( result );
}

bool SelectedEnemy::IsPotentiallyHittable() const {
	CheckValid( "IsPotentiallyHittable" );

	const auto levelTime = level.time;
	if( m_arePotentiallyHittable.computedAt != levelTime ) {
		m_arePotentiallyHittable.computedAt = levelTime;

		m_arePotentiallyHittable.value = TestIsPotentiallyHittable();
	}

	return m_arePotentiallyHittable.value;
}

bool SelectedEnemy::TestIsPotentiallyHittable() const {
	const auto *__restrict self = game.edicts + m_bot->EntNum();

	if( GetBotViewDirDotToEnemyDir() > 0.7f ) {
		return false;
	}

	if( !EntitiesPvsCache::Instance()->AreInPvs( self, m_enemy->m_ent ) ) {
		return false;
	}

	trace_t trace;
	Vec3 viewPoint( self->s.origin );
	viewPoint.z() += self->viewheight;
	SolidWorldTrace( &trace, viewPoint.data(), m_enemy->m_ent->s.origin );
	return trace.fraction == 1.0f;
}

bool SelectedEnemy::EnemyCanHit() const {
	CheckValid( "EnemyCanHit" );

	const auto levelTime = level.time;
	if( m_canEnemiesHit.computedAt != levelTime ) {
		m_canEnemiesHit.computedAt = levelTime;

		m_canEnemiesHit.value = TestCanHit( m_enemy->m_ent, game.edicts + m_bot->EntNum(), GetEnemyViewDirDotToBotDir() );
	}

	return m_canEnemiesHit.value;
}

bool SelectedEnemy::TestCanHit( const edict_t *attacker, const edict_t *victim, float viewDot ) {
	if( !( attacker && victim ) ) {
		return false;
	}

	if( viewDot < 0.7f ) {
		return false;
	}

	if( !EntitiesPvsCache::Instance()->AreInPvs( attacker, victim ) ) {
		return false;
	}

	auto *targetEnt = const_cast<edict_t *>( attacker );
	trace_t trace;
	auto *enemyEnt = const_cast<edict_t *>( victim );
	Vec3 traceStart( enemyEnt->s.origin );
	traceStart.z() += enemyEnt->viewheight;

	G_Trace( &trace, traceStart.data(), nullptr, nullptr, targetEnt->s.origin, enemyEnt, MASK_AISOLID );
	if( trace.fraction != 1.0f && game.edicts + trace.ent == targetEnt ) {
		return true;
	}

	// If there is a distinct chest point (we call it chest since it is usually on chest position)
	if( std::abs( targetEnt->viewheight ) > 8 ) {
		Vec3 targetPoint( targetEnt->s.origin );
		targetPoint.z() += targetEnt->viewheight;
		G_Trace( &trace, traceStart.data(), nullptr, nullptr, targetPoint.data(), enemyEnt, MASK_AISOLID );
		if( trace.fraction != 1.0f && game.edicts + trace.ent == targetEnt ) {
			return true;
		}
	}

	// Don't waste cycles on further tests (as it used to be).
	// This test is for getting a coarse info anyway.

	return false;
}

bool SelectedEnemy::CouldBeHitIfBotTurns() const {
	CheckValid( "CouldBeHitIfBotTurns" );

	const auto levelTime = level.time;
	if( m_couldHitIfTurns.computedAt != levelTime ) {
		m_couldHitIfTurns.computedAt = levelTime;

		m_couldHitIfTurns.value = TestCanHit( game.edicts + m_bot->EntNum(), m_enemy->m_ent, 1.0f );
	}

	return m_couldHitIfTurns.value;
}

bool SelectedEnemy::EnemyCanBeHit() const {
	// Check whether it could be possibly hit from bot origin and the bot is looking at it
	return CouldBeHitIfBotTurns() && GetBotViewDirDotToEnemyDir() > m_bot->FovDotFactor();
}

float SelectedEnemy::GetBotViewDirDotToEnemyDir() const {
	const auto levelTime = level.time;
	if( m_botViewDirDotToEnemyDir.computedAt != levelTime ) {
		m_botViewDirDotToEnemyDir.computedAt = levelTime;

		const float viewHeight = playerbox_stand_viewheight;
		Vec3 botViewDir( m_bot->EntityPhysicsState()->ForwardDir() );
		Vec3 botToEnemyDir( m_enemy->LastSeenOrigin());
		botToEnemyDir.z() -= viewHeight;
		botToEnemyDir -= m_bot->Origin();
		if( botToEnemyDir.normalizeFast() ) {
			m_botViewDirDotToEnemyDir.value = botViewDir.dot( botToEnemyDir );
		} else {
			m_botViewDirDotToEnemyDir.value = 1.0f;
		}
	}

	return m_botViewDirDotToEnemyDir.value;
}

float SelectedEnemy::GetEnemyViewDirDotToBotDir() const {
	const auto levelTime = level.time;
	if( m_enemyViewDirDotToBotDir.computedAt != levelTime ) {
		m_enemyViewDirDotToBotDir.computedAt = levelTime;

		const float viewHeight = playerbox_stand_viewheight;
		Vec3 enemyToBotDir( m_bot->Origin() );
		enemyToBotDir.z() -= viewHeight;
		enemyToBotDir -= m_enemy->LastSeenOrigin();
		if( enemyToBotDir.normalizeFast() ) {
			m_enemyViewDirDotToBotDir.value = m_enemy->LookDir().dot( enemyToBotDir );
		} else {
			m_enemyViewDirDotToBotDir.value = 1.0f;
		}
	}

	return m_enemyViewDirDotToBotDir.value;
}

bool SelectedEnemy::IsAboutToHit( FrameCachedBool *cached, bool ( SelectedEnemy::*testHit )( int64_t ) const ) const {
	const auto levelTime = level.time;
	if( cached->computedAt != levelTime ) {
		cached->computedAt = levelTime;
		cached->value = ( this->*testHit )( levelTime );
	}
	return cached->value;
}

bool SelectedEnemy::TestAboutToHitEBorIG( int64_t levelTime ) const {
	if( !m_enemy->IsShootableCurrOrPendingWeapon( WEAP_ELECTROBOLT ) ) {
		if( !m_enemy->IsShootableCurrOrPendingWeapon( WEAP_INSTAGUN ) ) {
			return false;
		}
	}

	// We can dodge at the last movement, so wait until there is 1/3 of a second to make a shot
	if( m_enemy->FireDelay() > 333 ) {
		return false;
	}

	const Vec3 enemyOrigin( m_enemy->LastSeenOrigin() );
	const float distance = Q_Sqrt( enemyOrigin.squareDistanceTo( m_bot->Origin() ) );

	float dotThreshold = 0.95f;
	// Check whether the enemy is really holding the weapon (so its not the mere pending one)
	if( m_enemy->IsShootableCurrWeapon( WEAP_ELECTROBOLT ) || m_enemy->IsShootableCurrWeapon( WEAP_INSTAGUN ) ) {
		// Apply a lower dot threshold if the enemy is really holding the weapon
		dotThreshold = 0.90f;
	}

	// Is not going to put crosshair right now
	// TODO: Check past view dots and derive direction?
	// Note: raise the dot threshold for distant enemies
	if( GetEnemyViewDirDotToBotDir() < dotThreshold + 0.03f * BoundedFraction( distance, 2500.0f ) ) {
		return false;
	}

	const float squareSpeed = m_enemy->LastSeenVelocity().squareLength();
	// Hitting at this speed is unlikely
	if( squareSpeed > wsw::square( 650.0f ) ) {
		return false;
	}

	if( const auto *const client = m_enemy->m_ent->r.client ) {
		// If not zooming
		if( !client->ps.stats[PM_STAT_ZOOMTIME] ) {
			// It's unlikely to hit at this distance
			if( distance > 1250.0f ) {
				return false;
			}
		} else {
			// It's hard to hit having a substantial speed while zooming
			if( squareSpeed > wsw::square( 400.0f ) ) {
				return false;
			}
		}
	}

	// Expensive tests go last

	if( !EntitiesPvsCache::Instance()->AreInPvs( game.edicts + m_bot->EntNum(), m_enemy->m_ent ) ) {
		return false;
	}

	trace_t trace;
	Vec3 traceStart( enemyOrigin );
	traceStart.z() += playerbox_stand_viewheight;
	SolidWorldTrace( &trace, traceStart.data(), m_bot->Origin() );
	if( trace.fraction != 1.0f ) {
		return false;
	}

	return true;
}

bool SelectedEnemy::TestAboutToHitLGorPG( int64_t levelTime ) const {
	const float *const __restrict botOrigin = m_bot->Origin();
	const Vec3 enemyOrigin( m_enemy->LastSeenOrigin() );

	// Skip enemies that are out of LG range. (Consider PG to be inefficient outside of this range too)
	const float squareDistance = enemyOrigin.squareDistanceTo( botOrigin );
	if( squareDistance > wsw::square( kLasergunRange ) ) {
		return false;
	}

	if( !m_enemy->IsShootableCurrOrPendingWeapon( WEAP_LASERGUN ) ) {
		if( !m_enemy->IsShootableCurrOrPendingWeapon( WEAP_PLASMAGUN ) ) {
			return false;
		}
	}

	// Check whether this PG can be matched against LG
	const auto *ent = m_enemy->m_ent;
	if( const Bot *thatBot = ent->bot ) {
		// Raise the skip distance threshold for hard bots
		if( Q_Sqrt( squareDistance ) > 384.0f + 512.0f * thatBot->Skill() ) {
			return false;
		}
	} else {
		// Check whether PG is usable at this ping
		const auto ping = -(float)ent->r.client->timeDelta;
		// Make sure we got timeDelta sign right
		assert( ping >= 0.0f );
		if( ping >= 100 ) {
			return false;
		}
		const float pingFactor = 1e-2f * ping;
		assert( pingFactor >= 0.0f && pingFactor < 1.0f );
		// Skip if the client is fairly far to adjust PG tracking for this ping.
		// Lower the skip distance threshold for high-ping clients.
		if( Q_Sqrt( squareDistance ) > 768.0f - 384.0f * pingFactor ) {
			return false;
		}
	}

	// We can start dodging at the last moment, are not going to be hit hard
	if( m_enemy->FireDelay() > 333 ) {
		return false;
	}

	float viewDotThreshold = 0.97f;
	// Check whether the enemy is really holding the weapon
	if( m_enemy->IsShootableCurrWeapon( WEAP_LASERGUN ) || m_enemy->IsShootableCurrWeapon( WEAP_PLASMAGUN ) ) {
		// Apply a lower dot threshold if the enemy is really holding the weapon
		viewDotThreshold = 0.90f;
	}

	if( GetEnemyViewDirDotToBotDir() < viewDotThreshold ) {
		return false;
	}


	// It's better to avoid fighting vs LG using dodging on ground and flee away
	// if the bot is in a "nofall" area and is running away from an enemy
	// TODO: The decision shouldn't be made here, move it to the planning module
	bool skipIfKnockBackWontMakeWorse = false;
	const auto &physicsState = m_bot->EntityPhysicsState();
	const float botSpeed2D = physicsState->Speed2D();
	float speedFactor = 0.0f;
	Vec3 botVelocity2DDir( m_bot->EntityPhysicsState()->Velocity() );
	// Hack! We assume WillRetreat() flag really produces retreating.
	if( botSpeed2D > 300.0f || ( m_bot->WillRetreat() && botSpeed2D > 1 ) ) {
		int botAreaNums[2] { 0, 0 };
		const auto *const aasWorld = AiAasWorld::instance();
		const auto aasAreaSettings = aasWorld->getAreaSettings();
		const int numBotAreas = physicsState->PrepareRoutingStartAreas( botAreaNums );
		for( int i = 0; i < numBotAreas; ++i ) {
			const auto flags = aasAreaSettings[botAreaNums[i]].areaflags;
			// If there are grounded areas they must be NOFALL
			if( !( flags & AREA_GROUNDED ) ) {
				continue;
			}
			if( !( flags & AREA_NOFALL ) ) {
				break;
			}

			// Actually make a dir on demand
			botVelocity2DDir.z() = 0;
			botVelocity2DDir *= Q_Rcp( botSpeed2D );

			// Check whether we're going to hit an obstacle on knockback
			speedFactor = Q_Sqrt( wsw::min( botSpeed2D, 1000.0f ) * 1e-3f );
			Vec3 testedPoint( Vec3( botOrigin ) + ( ( 64.0f + 96.0f * speedFactor ) * botVelocity2DDir ) );
			edict_t *self = game.edicts + m_bot->EntNum();
			trace_t trace;
			// Let's check against other players as well to prevent blocking of teammates
			G_Trace( &trace, self->s.origin, nullptr, nullptr, testedPoint.data(), self, MASK_PLAYERSOLID );
			if( trace.fraction != 1.0f ) {
				break;
			}

			// Check whether we're not going to have worse travel time to target
			const int targetAreaNum = m_bot->NavTargetAasAreaNum();
			const int testedAreaNum = aasWorld->findAreaNum( testedPoint );
			int currTravelTime = m_bot->RouteCache()->FindRoute( botAreaNums, numBotAreas, targetAreaNum, m_bot->TravelFlags() );
			// Can't say much in this case
			if( !currTravelTime ) {
				break;
			}
			int testedTravelTime = m_bot->RouteCache()->FindRoute( testedAreaNum, targetAreaNum, m_bot->TravelFlags() );
			// If the nav target is going to become unreachable or the travel time is worse
			if( !testedTravelTime || testedTravelTime > currTravelTime ) {
				break;
			}

			skipIfKnockBackWontMakeWorse = true;
			break;
		}
	}

	if( skipIfKnockBackWontMakeWorse ) {
		assert( speedFactor >= 0.0f && speedFactor <= 1.0f );
		// Make the skip distance depend of the bot speed.
		// If the speed is fairly large we can jump/bunny-hop back even being close.
		float distanceThreshold = 64.0f + 256.0f * ( 1.0f - speedFactor );
		if( squareDistance > distanceThreshold * distanceThreshold ) {
			// The look dir cache is maintained by TrackedEnemy itself.
			// Besides it this is a quite rarely executed code path

			// If the knockback is going to assist a leap back
			if( m_enemy->LookDir().dot( botVelocity2DDir ) > 0.90f - 0.20f * speedFactor ) {
				return false;
			}
		}
	}

	if( !EntitiesPvsCache::Instance()->AreInPvs( game.edicts + m_bot->EntNum(), m_enemy->m_ent ) ) {
		return false;
	}

	// Check whether the enemy really tries to track the bot
	if( !m_enemy->TriesToKeepUnderXhair( botOrigin ) ) {
		return false;
	}

	trace_t trace;
	Vec3 traceStart( enemyOrigin );
	traceStart.z() += playerbox_stand_viewheight;
	SolidWorldTrace( &trace, traceStart.data(), botOrigin );
	if( trace.fraction != 1.0f ) {
		for( float deltaZ: { playerbox_stand_maxs[2] - 2.0f, playerbox_stand_mins[2] + 2.0f } ) {
			Vec3 traceEnd( botOrigin[0], botOrigin[1], botOrigin[2] + deltaZ );
			SolidWorldTrace( &trace, traceStart.data(), traceEnd.data() );
			if( trace.fraction == 1.0f ) {
				return true;
			}
		}
	}

	return false;
}

bool SelectedEnemy::TestAboutToHitRLorSW( int64_t levelTime ) const {
	if( !m_enemy->IsShootableCurrOrPendingWeapon( WEAP_ROCKETLAUNCHER ) ) {
		if( !m_enemy->IsShootableCurrOrPendingWeapon( WEAP_SHOCKWAVE ) ) {
			return false;
		}
	}

	const float *const __restrict botOrigin = m_bot->Origin();

	float distanceThreshold = 512.0f;
	// Ideally should check the bot environment too
	const float deltaZ = m_enemy->LastSeenOrigin().z() - botOrigin[2];
	if( deltaZ > 16.0f ) {
		distanceThreshold += 2.0f * BoundedFraction( deltaZ, 128.0f );
	} else if( deltaZ < -16.0f ) {
		distanceThreshold -= BoundedFraction( deltaZ, 128.0f );
	}

	const float squareDistance = m_enemy->LastSeenOrigin().squareDistanceTo( botOrigin );
	if( squareDistance > distanceThreshold * distanceThreshold ) {
		return false;
	}

	const float distance = Q_Sqrt( squareDistance );
	const float distanceFraction = BoundedFraction( distance, distanceThreshold );
	// Do not wait for an actual shot on a short distance.
	// Its impossible to dodge on a short distance due to damage splash.
	// If the distance is close to zero 750 millis of reloading left must be used for making a dodge.
	if( m_enemy->FireDelay() > (unsigned)( 750 - ( ( 750 - 333 ) * distanceFraction ) ) ) {
		return false;
	}

	float dotThreshold = 0.5f;
	// Check whether an enemy is really holding the weapon
	if( m_enemy->IsShootableCurrWeapon( WEAP_ROCKETLAUNCHER ) || m_enemy->IsShootableCurrWeapon( WEAP_SHOCKWAVE ) ) {
		// Apply a lower dot threshold if the enemy is really holding the weapon
		dotThreshold = 0.25f;
	}

	// Is not going to put crosshair right now
	if( GetEnemyViewDirDotToBotDir() < dotThreshold + 0.4f * distanceFraction ) {
		return false;
	}

	if( !EntitiesPvsCache::Instance()->AreInPvs( game.edicts + m_bot->EntNum(), m_enemy->m_ent ) ) {
		return false;
	}

	trace_t trace;
	// TODO: Check view dot and derive direction?
	Vec3 enemyViewOrigin( m_enemy->LastSeenOrigin() );
	enemyViewOrigin.z() += playerbox_stand_viewheight;
	SolidWorldTrace( &trace, enemyViewOrigin.data(), botOrigin );
	if( trace.fraction == 1.0f ) {
		return true;
	}

	// A coarse environment test, check whether there are hittable environment elements
	// around the bot that are visible for the enemy
	for( int x = -1; x <= 1; x += 2 ) {
		for( int y = -1; y <= 1; y += 2 ) {
			Vec3 sidePoint( botOrigin );
			sidePoint.x() += 64.0f * x;
			sidePoint.y() += 64.0f * y;
			SolidWorldTrace( &trace, botOrigin, sidePoint.data() );
			if( trace.fraction == 1.0f || ( trace.surfFlags & SURF_NOIMPACT ) ) {
				continue;
			}
			const Vec3 oldImpact( trace.endpos );
			// Notice the order: we trace a ray from enemy to impact point to avoid having to offset start point
			SolidWorldTrace( &trace, enemyViewOrigin.data(), oldImpact.data() );
			if( trace.fraction == 1.0f || oldImpact.squareDistanceTo( trace.endpos ) < 8 * 8 ) {
				return true;
			}
		}
	}

	return false;
}