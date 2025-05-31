#include "awarenessmodule.h"
#include "../manager.h"
#include "../teamplay/squadbasedteam.h"
#include "../bot.h"

BotAwarenessModule::BotAwarenessModule( Bot *bot_ )
	: bot( bot_ )
	, targetChoicePeriod( (unsigned)( 1500 - 500 * bot_->Skill() ) )
	, reactionTime( 320u - (unsigned)( 300 * bot_->Skill() ) )
	, alertTracker( bot_ )
	, hazardsDetector( bot_ )
	, hazardsSelector( bot_ )
	, eventsTracker( bot_ )
	, keptInFovPointTracker( bot_, this )
	, pathBlockingTracker( bot_ )
	, enemiesTracker( bot_, this ) {}

void BotAwarenessModule::OnAttachedToSquad( AiSquad *squad_ ) {
}

void BotAwarenessModule::OnDetachedFromSquad( AiSquad *squad_ ) {
	bot->m_selectedEnemy = std::nullopt;
	bot->m_lostEnemy     = std::nullopt;
}

void BotAwarenessModule::OnEnemyOriginGuessed( const edict_t *enemy, unsigned minMillisSinceLastSeen, const float *guessedOrigin ) {
	enemiesTracker.OnEnemyOriginGuessed( enemy, minMillisSinceLastSeen, guessedOrigin );
}

void BotAwarenessModule::OnPain( const edict_t *enemy, float kick, int damage ) {
	const edict_t *self = game.edicts + bot->EntNum();
	enemiesTracker.OnPain( self, enemy, kick, damage );
}

void BotAwarenessModule::OnEnemyDamaged( const edict_t *target, int damage ) {
	const edict_t *self = game.edicts + bot->EntNum();
	enemiesTracker.OnEnemyDamaged( self, target, damage );
}

const TrackedEnemy *BotAwarenessModule::ChooseLostOrHiddenEnemy( unsigned timeout ) {
	return enemiesTracker.ChooseLostOrHiddenEnemy( timeout );
}

void BotAwarenessModule::InvalidateSelectedEnemiesIfNeeded() {
	// Check each frame. Don't let non-empty std::optionals contain timed out values.
	if( bot->m_selectedEnemy && bot->m_selectedEnemy->ShouldInvalidate() ) {
		bot->m_selectedEnemy = std::nullopt;
	}
	if( bot->m_lostEnemy && bot->m_lostEnemy->ShouldInvalidate() ) {
		bot->m_lostEnemy = std::nullopt;
	}
}

void BotAwarenessModule::Update() {
	// TODO: Make the control flow clear
	InvalidateSelectedEnemiesIfNeeded();

	enemiesTracker.Update();
	eventsTracker.Update();

	InvalidateSelectedEnemiesIfNeeded();

	if( bot->PermitsDistributedUpdateThisFrame() ) {
		RegisterVisibleEnemies();
		CheckForNewHazards();

		if( bot->m_selectedEnemy ) {
			if( level.time - bot->m_selectedEnemy->LastSeenAt() > wsw::min( 64u, reactionTime ) ) {
				bot->m_selectedEnemy = std::nullopt;
			}
		}
		if( !bot->m_selectedEnemy ) {
			UpdateSelectedEnemy();
			shouldUpdateBlockedAreasStatus = true;
		}

		// Calling this also makes sense if the "update" flag has been retained from previous frames
		UpdateBlockedAreasStatus();

		keptInFovPointTracker.update();

		TryTriggerPlanningForNewHazard();
	}
}

void BotAwarenessModule::UpdateSelectedEnemy() {
	bot->m_selectedEnemy = std::nullopt;
	bot->m_lostEnemy     = std::nullopt;

	[[maybe_unused]] float visibleEnemyWeight  = 0.0f;
	[[maybe_unused]] const unsigned instanceId = selectedEnemyInstanceId++;
	[[maybe_unused]] const int64_t timeoutAt   = level.time + targetChoicePeriod;
	if( const TrackedEnemy *visibleEnemy = enemiesTracker.ChooseVisibleEnemy() ) {
		bot->m_selectedEnemy = SelectedEnemy( bot, visibleEnemy, timeoutAt, instanceId );
		visibleEnemyWeight = 0.5f * ( visibleEnemy->AvgWeight() + visibleEnemy->MaxWeight() );
	}

	if( const TrackedEnemy *lostEnemy = enemiesTracker.ChooseLostOrHiddenEnemy() ) {
		float lostEnemyWeight = 0.5f * ( lostEnemy->AvgWeight() + lostEnemy->MaxWeight() );
		// If there is a lost or hidden enemy of greater weight, store it
		if( lostEnemyWeight > visibleEnemyWeight ) {
			// Share the instance id with the visible enemy
			bot->m_lostEnemy = SelectedEnemy( bot, lostEnemy, timeoutAt, instanceId );
		}
	}
}

bool BotAwarenessModule::HurtEvent::IsValidFor( const Bot *bot_ ) const {
	if( level.time - lastHitTimestamp > 350 ) {
		return false;
	}

	// Check whether the inflictor entity is no longer valid

	if( !inflictor->r.inuse ) {
		return false;
	}

	if( !inflictor->r.client && inflictor->aiIntrinsicEnemyWeight <= 0 ) {
		return false;
	}

	if( G_ISGHOSTING( inflictor ) ) {
		return false;
	}

	const Vec3 lookDir( bot_->EntityPhysicsState()->ForwardDir() );
	Vec3 toThreat( Vec3( inflictor->s.origin ) - bot_->Origin() );
	if( toThreat.normalizeFast() ) [[likely]] {
		return toThreat.Dot( lookDir ) < bot_->FovDotFactor();
	}

	// Assume that we can't turn to this direction in order to react to threat as it is not defined.
	return false;
}

void BotAwarenessModule::TryTriggerPlanningForNewHazard() {
	if( bot->Skill() <= 0.33f ) {
		return;
	}

	const Hazard *hazard = hazardsSelector.PrimaryHazard();
	if( !hazard ) {
		return;
	}

	// Trying to do urgent replanning based on more sophisticated formulae was a bad idea.
	// The bot has inertia and cannot change dodge direction so fast,
	// and it just lead to no actual dodging performed since the actual mean dodge vector is about zero.

	if( !triggeredPlanningHazard.IsValid() ) {
		triggeredPlanningHazard = *hazard;
		bot->ForcePlanBuilding();
	}
}

void BotAwarenessModule::OnHurtByNewThreat( const edict_t *newThreat, const AiComponent *threatDetector ) {
	// Reject threats detected by bot brain if there is active squad.
	// Otherwise there may be two calls for a single or different threats
	// detected by squad and the bot brain enemy pool itself.
	if( bot->IsInSquad() && threatDetector == &this->enemiesTracker ) {
		return;
	}

	bool hadValidThreat = hurtEvent.IsValidFor( bot );
	float totalInflictedDamage = enemiesTracker.TotalDamageInflictedBy( newThreat );
	if( hadValidThreat ) {
		// The active threat is more dangerous than a new one
		if( hurtEvent.totalDamage > totalInflictedDamage ) {
			return;
		}
		// The active threat has the same inflictor
		if( hurtEvent.inflictor == newThreat ) {
			// Just update the existing threat
			hurtEvent.totalDamage = totalInflictedDamage;
			hurtEvent.lastHitTimestamp = level.time;
			return;
		}
	}

	Vec3 botLookDir( bot->EntityPhysicsState()->ForwardDir() );
	Vec3 toEnemyDir = Vec3( newThreat->s.origin ) - bot->Origin();
	const float squareDistance = toEnemyDir.SquaredLength();
	if( squareDistance < 1 ) {
		return;
	}

	const float invDistance = Q_RSqrt( squareDistance );
	toEnemyDir *= invDistance;
	// TODO: Check against the actual bot fov
	if( toEnemyDir.Dot( botLookDir ) >= 0 ) {
		return;
	}

	// Try guessing the enemy origin
	toEnemyDir.X() += -0.25f + 0.50f * random();
	toEnemyDir.Y() += -0.10f + 0.20f * random();
	if( !toEnemyDir.normalizeFast() ) [[unlikely]] {
		return;
	}

	hurtEvent.inflictor = newThreat;
	hurtEvent.lastHitTimestamp = level.time;
	hurtEvent.possibleOrigin = Q_Rcp( invDistance ) * toEnemyDir + bot->Origin();
	hurtEvent.totalDamage = totalInflictedDamage;
	// Force replanning on new threat
	if( !hadValidThreat ) {
		bot->ForcePlanBuilding();
	}
}

void BotAwarenessModule::OnEnemyRemoved( const TrackedEnemy *enemy ) {
	bool wereChanges = false;
	if( bot->m_selectedEnemy && bot->m_selectedEnemy->IsBasedOn( enemy ) ) {
		bot->m_selectedEnemy = std::nullopt;
		wereChanges          = true;
	}
	if( bot->m_lostEnemy && bot->m_lostEnemy->IsBasedOn( enemy ) ) {
		bot->m_lostEnemy = std::nullopt;
		wereChanges      = true;
	}
	if( wereChanges ) {
		bot->ForcePlanBuilding();
	}
}

void BotAwarenessModule::UpdateBlockedAreasStatus() {
	if( !shouldUpdateBlockedAreasStatus ) {
		return;
	}

	if( !bot->TryGetExpensiveThinkCallQuota() ) {
		return;
	}

	pathBlockingTracker.Update();
	shouldUpdateBlockedAreasStatus = false;
}

static bool IsEnemyVisible( const edict_t *self, const edict_t *enemyEnt ) {
	trace_t trace;
	edict_t *const gameEdicts = game.edicts;
	edict_t *ignore = gameEdicts + ENTNUM( self );

	Vec3 traceStart( self->s.origin );
	traceStart.Z() += self->viewheight;
	Vec3 traceEnd( enemyEnt->s.origin );

	G_Trace( &trace, traceStart.Data(), nullptr, nullptr, traceEnd.Data(), ignore, MASK_OPAQUE );
	if( trace.fraction == 1.0f || gameEdicts + trace.ent == enemyEnt ) {
		return true;
	}

	vec3_t dims;
	if( enemyEnt->r.client ) {
		// We're sure clients in-game have quite large and well-formed hitboxes, so no dimensions test is required.
		// However we have a much more important test to do.
		// If this point usually corresponding to an enemy chest/weapon is not
		// considered visible for a bot but is really visible, the bot behavior looks weird.
		// That's why this special test is added.

		// If the view height makes a considerable spatial distinction
		if( abs( enemyEnt->viewheight ) > 8 ) {
			traceEnd.Z() += enemyEnt->viewheight;
			G_Trace( &trace, traceStart.Data(), nullptr, nullptr, traceEnd.Data(), ignore, MASK_OPAQUE );
			if( trace.fraction == 1.0f || gameEdicts + trace.ent == enemyEnt ) {
				return true;
			}
		}

		// We have deferred dimensions computations to a point after trace call.
		for( int i = 0; i < 3; ++i ) {
			dims[i] = enemyEnt->r.maxs[i] - enemyEnt->r.mins[i];
		}
	}
	else {
		for( int i = 0; i < 3; ++i ) {
			dims[i] = enemyEnt->r.maxs[i] - enemyEnt->r.mins[i];
		}
		// Prevent further testing in degenerate case (there might be non-player enemies).
		if( !dims[0] || !dims[1] || !dims[2] ) {
			return false;
		}
		if( wsw::max( dims[0], wsw::max( dims[1], dims[2] ) ) < 8 ) {
			return false;
		}
	}

	// Try testing 4 corners of enemy projection onto bot's "view".
	// It is much less expensive that testing all 8 corners of the hitbox.

	Vec3 enemyToBotDir( self->s.origin );
	enemyToBotDir -= enemyEnt->s.origin;
	if( !enemyToBotDir.normalizeFast() ) [[unlikely]] {
		return true;
	}

	vec3_t right, up;
	MakeNormalVectors( enemyToBotDir.Data(), right, up );

	// Add some inner margin to the hitbox (a real model is less than it and the computations are coarse).
	const float sideOffset = ( 0.8f * wsw::min( dims[0], dims[1] ) ) / 2;
	float zOffset[2] = { enemyEnt->r.maxs[2] - 0.1f * dims[2], enemyEnt->r.mins[2] + 0.1f * dims[2] };
	// Switch the side from left to right
	for( int i = -1; i <= 1; i += 2 ) {
		// Switch Z offset
		for( int j = 0; j < 2; j++ ) {
			// traceEnd = Vec3( enemyEnt->s.origin ) + i * sideOffset * right;
			traceEnd.Set( right );
			traceEnd *= i * sideOffset;
			traceEnd += enemyEnt->s.origin;
			traceEnd.Z() += zOffset[j];
			G_Trace( &trace, traceStart.Data(), nullptr, nullptr, traceEnd.Data(), ignore, MASK_OPAQUE );
			if( trace.fraction == 1.0f || gameEdicts + trace.ent == enemyEnt ) {
				return true;
			}
		}
	}

	return false;
}

void BotAwarenessModule::RegisterVisibleEnemies() {
	if( GS_MatchState( *ggs ) == MATCH_STATE_COUNTDOWN ) {
		return;
	}

	// Compute look dir before loop
	const Vec3 lookDir    = bot->EntityPhysicsState()->ForwardDir();
	const float dotFactor = bot->FovDotFactor();
	const edict_t *botEnt = game.edicts + bot->EntNum();

	// Note: non-client entities also may be candidate targets.
	wsw::StaticVector<EntAndDistance, MAX_EDICTS> candidateTargets;

	const edict_t *const gameEdicts = game.edicts;
	const int numGameEntities       = game.numentities;
	for( int entNum = 1; entNum < numGameEntities; ++entNum ) {
		const edict_t *ent = gameEdicts + entNum;
		if( !bot->IsDefinitelyNotAFeasibleEnemy( ent ) ) {
			// Reject targets quickly by fov
			Vec3 toTarget( Vec3( ent->s.origin ) - bot->Origin() );
			const float squareDistance = toTarget.SquaredLength();

			if( squareDistance > 1.0f || squareDistance < wsw::square( ent->aiVisibilityDistance ) ) [[likely]] {
				const float rcpDistance = Q_RSqrt( squareDistance );
				toTarget *= rcpDistance;
				if( toTarget.Dot( lookDir ) >= dotFactor ) {
					candidateTargets.emplace_back( { .entNum = entNum, .distance = squareDistance * rcpDistance } );
				}
			}
		}
	}

	const auto pvsFunc             = isGenericEntityInPvs;
	const auto visFunc             = IsEnemyVisible;
	const auto visCheckCallsQuotum = MAX_CLIENTS;
	wsw::StaticVector<uint16_t, MAX_CLIENTS> visibleTargets;
	visCheckRawEnts( &candidateTargets, &visibleTargets, botEnt, visCheckCallsQuotum, pvsFunc, visFunc );

	for( const auto entNum: visibleTargets ) {
		enemiesTracker.OnEnemyViewed( gameEdicts + entNum );
	}

	alertTracker.CheckAlertSpots( visibleTargets );
}

void BotAwarenessModule::CheckForNewHazards() {
	// This call returns a value if the primary hazard is valid
	// TODO: This is totally wrong!
	if( PrimaryHazard() != nullptr ) {
		return;
	}

	hazardsSelector.BeginUpdate();

	hazardsDetector.Exec();

	if( const EntNumsVector &v = hazardsDetector.visibleDangerousRockets; !v.empty() ) {
		hazardsSelector.FindProjectileHazards( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( hazardsDetector.visibleOtherRockets );
	}

	if( const EntNumsVector &v = hazardsDetector.visibleDangerousWaves; !v.empty() ) {
		hazardsSelector.FindWaveHazards( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( hazardsDetector.visibleOtherWaves );
	}

	if( const EntNumsVector &v = hazardsDetector.visibleDangerousBlasts; !v.empty() ) {
		hazardsSelector.FindProjectileHazards( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( hazardsDetector.visibleOtherBlasts );
	}

	if( const EntNumsVector &v = hazardsDetector.visibleDangerousGrenades; !v.empty() ) {
		hazardsSelector.FindProjectileHazards( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( hazardsDetector.visibleOtherGrenades );
	}

	if( const EntNumsVector &v = hazardsDetector.visibleDangerousPlasmas; !v.empty() ) {
		// The detection is quite expensive, allow intentional failing at it (and thus rejecting quickly)
		constexpr float failureChance = 0.5f;
		hazardsSelector.FindPlasmaHazards( v );
		eventsTracker.TryGuessingProjectileOwnersOrigins( v, failureChance );
		eventsTracker.TryGuessingProjectileOwnersOrigins( hazardsDetector.visibleOtherPlasmas, failureChance );
	}

	if( const EntNumsVector &v = hazardsDetector.visibleDangerousLasers; !v.empty() ) {
		hazardsSelector.FindLaserHazards( v );
		eventsTracker.TryGuessingBeamOwnersOrigins( v );
		eventsTracker.TryGuessingBeamOwnersOrigins( hazardsDetector.visibleOtherLasers );
	}

	hazardsSelector.EndUpdate();
}