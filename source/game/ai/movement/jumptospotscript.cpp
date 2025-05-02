#include "jumptospotscript.h"
#include "movementlocal.h"
#include "fallbackaction.h"
#include "environmenttracecache.h"
#include "bestjumpablespotdetector.h"
#include "../manager.h"
#include "../../../common/wswalgorithm.h"

void JumpToSpotScript::Activate( const vec3_t startOrigin_,
								 const vec3_t targetOrigin_,
								 unsigned timeout,
								 float reachRadius_,
								 float startAirAccelFrac_,
								 float endAirAccelFrac_,
								 float jumpBoostSpeed_ ) {
	VectorCopy( targetOrigin_, this->targetOrigin );
	VectorCopy( startOrigin_, this->startOrigin );
	this->timeout = timeout + 150u;
	this->reachRadius = reachRadius_;
	Q_clamp( startAirAccelFrac_, 0.0f, 1.0f );
	Q_clamp( endAirAccelFrac_, 0.0f, 1.0f );
	this->startAirAccelFrac = startAirAccelFrac_;
	this->endAirAccelFrac = endAirAccelFrac_;
	this->jumpBoostSpeed = jumpBoostSpeed_;
	this->hasAppliedJumpBoost = false;

	// Check whether there is no significant difference in start and target height
	// (velocity correction looks weird in that case)
	this->allowCorrection = false;
	Vec3 jumpVec( targetOrigin );
	jumpVec -= startOrigin;
	if( fabsf( jumpVec.Z() ) / sqrtf( jumpVec.X() * jumpVec.X() + jumpVec.Y() * jumpVec.Y() ) < 0.2f ) {
		this->allowCorrection = true;
	}

	MovementScript::Activate();
}

bool JumpToSpotScript::TryDeactivate( PredictionContext *context ) {
	assert( status == PENDING );

	// If the fallback is still active, invalidate it
	if ( level.time - activatedAt > wsw::max( 250u, timeout ) ) {
		return DeactivateWithStatus( INVALID );
	}

	// If the fallback movement has just started, skip tests
	if( level.time - activatedAt < wsw::min( 250u, timeout ) ) {
		return false;
	}

	const AiEntityPhysicsState *entityPhysicsState;
	if( context ) {
		entityPhysicsState = &context->movementState->entityPhysicsState;
	} else {
		entityPhysicsState = bot->EntityPhysicsState();
	}

	// Wait for landing
	if( !entityPhysicsState->GroundEntity() ) {
		return false;
	}

	// Wait until the target is reached (prevent deactivation if we still are at the start point)
	if( DistanceSquared( entityPhysicsState->Origin(), targetOrigin ) < reachRadius * reachRadius ) {
		return false;
	}

	const auto aasAreaSettings = AiAasWorld::instance()->getAreaSettings();

	int currAreaNums[2] = { 0, 0 };
	const int numCurrAreas = entityPhysicsState->PrepareRoutingStartAreas( currAreaNums );

	// First, check whether we have entered any area with disabled flags
	for( int i = 0; i < numCurrAreas; ++i ) {
		const auto &areaSettings = aasAreaSettings[currAreaNums[i]];
		if( ( areaSettings.contents & undesiredAasContents ) || ( areaSettings.areaflags & undesiredAasFlags ) ) {
			return DeactivateWithStatus( INVALID );
		}
	}

	// Second, check whether we have entered some area with satisfying flags
	for( int i = 0; i < numCurrAreas; ++i ) {
		const auto &areaSettings = aasAreaSettings[currAreaNums[i]];
		if( ( areaSettings.contents & desiredAasContents ) || ( areaSettings.areaflags & desiredAasFlags ) ) {
			return DeactivateWithStatus( COMPLETED );
		}
	}

	return false;
}

void JumpToSpotScript::SetupMovement( PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	// Start Z is rather important, don't use entity origin as-is
	Vec3 toTargetDir( entityPhysicsState.Origin() );
	toTargetDir.Z() += game.edicts[bot->EntNum()].viewheight;
	toTargetDir -= targetOrigin;
	toTargetDir *= -1.0f;

	if( !toTargetDir.normalize() ) {
		return;
	}

	int forwardMovement = 1;
	float viewDot = toTargetDir.Dot( entityPhysicsState.ForwardDir() );
	if( viewDot < 0 ) {
		toTargetDir *= -1.0f;
		forwardMovement = -1;
	}

	botInput->SetIntendedLookDir( toTargetDir, true );

	// Note: we do not check only 2D dot product but the dot product in all dimensions intentionally
	// (a bot often has to look exactly at the spot that might be above).
	// Setting exact view angles is important
	if( fabsf( viewDot ) < 0.99f ) {
		// This huge value is important, otherwise bot falls down in some cases
		botInput->SetTurnSpeedMultiplier( 15.0f );
		return;
	}

	botInput->SetForwardMovement( forwardMovement );

	if( !entityPhysicsState.GroundEntity() ) {
		if( !hasAppliedJumpBoost ) {
			hasAppliedJumpBoost = true;
			// Avoid weird-looking behavior, only boost jumping in case when bot has just started a jump
			if( jumpBoostSpeed > 0 && entityPhysicsState.Velocity()[2] > context->GetJumpSpeed() - 30 ) {
				Vec3 modifiedVelocity( entityPhysicsState.Velocity() );
				modifiedVelocity.Z() += jumpBoostSpeed;
				context->record->SetModifiedVelocity( modifiedVelocity );
				return;
			}
		}

		float jumpDistance2D = sqrtf( Distance2DSquared( this->startOrigin, this->targetOrigin ) );
		if( jumpDistance2D < 16.0f ) {
			return;
		}

		float distanceToTarget2D = sqrtf( Distance2DSquared( entityPhysicsState.Origin(), this->targetOrigin ) );
		float distanceFrac = distanceToTarget2D / jumpDistance2D;

		float accelFrac = startAirAccelFrac + distanceFrac * ( endAirAccelFrac - startAirAccelFrac );
		if( accelFrac > 0 ) {
			clamp_high( accelFrac, 1.0f );
			context->CheatingAccelerate( accelFrac );
		}

		if( allowCorrection && entityPhysicsState.Speed2D() > 100 ) {
			// If the movement has been inverted
			if( forwardMovement < 0 ) {
				// Restore the real to target dir that has been inverted
				toTargetDir *= -1.0f;
			}

			// Check whether a correction should be really applied
			// Otherwise it looks weird (a bot starts fly ghosting near the target like having a jetpack).
			Vec3 toTargetDir2D( targetOrigin );
			toTargetDir2D -= entityPhysicsState.Origin();
			toTargetDir2D.Z() = 0;
			toTargetDir2D *= 1.0f / distanceToTarget2D;

			Vec3 velocity2D( entityPhysicsState.Velocity() );
			velocity2D.Z() = 0;
			velocity2D *= 1.0f / entityPhysicsState.Speed2D();

			float velocity2DDotToTargetDir2D = velocity2D.Dot( toTargetDir2D );
			if( velocity2DDotToTargetDir2D < 0.9f ) {
				context->CheatingCorrectVelocity( velocity2DDotToTargetDir2D, toTargetDir2D );
			}
		}

		// Crouch in-air to reduce chances of hitting an obstacle
		botInput->SetUpMovement( -1 );
		return;
	}

	// Wait for accelerating on ground, except there is an obstacle or a gap
	if( entityPhysicsState.Speed2D() < context->GetRunSpeed() - 30 ) {
		// Check whether there is a gap in front of the bot
		trace_t trace;
		Vec3 traceStart( entityPhysicsState.ForwardDir() );
		traceStart *= forwardMovement;
		traceStart *= 24.0f;
		traceStart += entityPhysicsState.Origin();
		Vec3 traceEnd( traceStart );
		traceEnd.Z() -= 40.0f;
		edict_t *ignore = game.edicts + bot->EntNum();
		G_Trace( &trace, traceStart.Data(), nullptr, nullptr, traceEnd.Data(), ignore, MASK_PLAYERSOLID );
		// If there is no gap or hazard in front of the bot, wait for accelerating on ground
		if( trace.fraction != 1.0f && !( trace.contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_NODROP ) ) ) {
			return;
		}

		// We're unsure if the bot could reach the needed speed for jumping from the ledge.
		// Try using a cheating acceleration
		Vec3 forwardDir( entityPhysicsState.ForwardDir() );
		// Check whether the bot is not leaning too hard to avoid weird-looking movement
		if( fabsf( forwardDir.Z() ) < 0.3f ) {
			if( DistanceSquared( entityPhysicsState.Origin(), targetOrigin ) > wsw::square( 48 ) ) {
				forwardDir.Z() = 0;
				if( forwardDir.normalizeFast() && toTargetDir.Dot( forwardDir ) > 0.9f ) {
					Vec3 modifiedVelocity( forwardDir );
					modifiedVelocity *= context->GetRunSpeed();
					context->record->SetModifiedVelocity( modifiedVelocity );
				}
			}
		}
	}

	// Jump in all other cases
	botInput->SetUpMovement( 1 );
}

class BestAreaCenterJumpableSpotDetector: public BestRegularJumpableSpotDetector {
	wsw::StaticVector<SpotAndScore, 64> spotsHeap;
	void GetCandidateSpots( SpotAndScore **begin, SpotAndScore **end ) override;
	std::span<const int> GetBoxAreas( int *areasBuffer, int maxNumAreas  );
	void FillCandidateSpotsUsingRoutingTest( std::span<const int> boxAreaNums );
	void FillCandidateSpotsWithoutRoutingTest( std::span<const int> boxAreaNums );
	inline bool TestAreaSettings( const aas_areasettings_t &areaSettings );
public:
	BestAreaCenterJumpableSpotDetector() {
		SetTestSpotAreaNums( true );
	}
};

static BestAreaCenterJumpableSpotDetector bestAreaCenterJumpableSpotDetector;

inline bool BestAreaCenterJumpableSpotDetector::TestAreaSettings( const aas_areasettings_t &areaSettings ) {
	if( !( areaSettings.areaflags & ( AREA_GROUNDED ) ) ) {
		return false;
	}
	if( areaSettings.areaflags & ( AREA_DISABLED | AREA_JUNK ) ) {
		return false;
	}
	if( areaSettings.contents & ( AREACONTENTS_WATER | AREACONTENTS_LAVA | AREACONTENTS_SLIME | AREACONTENTS_DONOTENTER ) ) {
		return false;
	}
	return true;
}

std::span<const int> BestAreaCenterJumpableSpotDetector::GetBoxAreas( int *boxAreaNums, int maxAreaNums ) {
	Vec3 boxMins( -128, -128, -32 );
	Vec3 boxMaxs( +128, +128, +64 );
	boxMins += startOrigin;
	boxMaxs += startOrigin;
	return aasWorld->findAreasInBox( boxMins, boxMaxs, boxAreaNums, maxAreaNums );
}

void BestAreaCenterJumpableSpotDetector::GetCandidateSpots( SpotAndScore **begin, SpotAndScore **end ) {
	spotsHeap.clear();

	int boxAreaNumsBuffer[64];
	const auto boxAreaNums = GetBoxAreas( boxAreaNumsBuffer, 64 );
	if( routeCache ) {
		FillCandidateSpotsUsingRoutingTest( boxAreaNums );
	} else {
		FillCandidateSpotsWithoutRoutingTest( boxAreaNums );
	}

	wsw::make_heap( spotsHeap.begin(), spotsHeap.end() );

	*begin = spotsHeap.begin();
	*end = spotsHeap.end();
}

void BestAreaCenterJumpableSpotDetector::FillCandidateSpotsUsingRoutingTest( std::span<const int> boxAreaNums ) {
	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const float areaPointZOffset = 1.0f - playerbox_stand_mins[2];

	for( const int areaNum: boxAreaNums ) {
		if( !TestAreaSettings( aasAreaSettings[areaNum] ) ) {
			continue;
		}

		const auto &area = aasAreas[areaNum];
		Vec3 areaPoint( area.center );
		areaPoint.Z() = area.mins[2] + areaPointZOffset;
		if( areaPoint.SquareDistanceTo( startOrigin ) < wsw::square( 64.0f ) ) {
			continue;
		}
		int travelTime = routeCache->FindRoute( areaNum, navTargetAreaNum, travelFlags );
		if( !travelTime || travelTime > startTravelTimeToTarget ) {
			continue;
		}
		// Otherwise no results are produced
		areaPoint.Z() += playerbox_stand_maxs[2];
		// Use the negated travel time as a score for the max-heap (closest to target spot gets evicted first)
		new( spotsHeap.unsafe_grow_back() )SpotAndScore( areaPoint.Data(), -travelTime, areaNum, -1 );
	}
}

void BestAreaCenterJumpableSpotDetector::FillCandidateSpotsWithoutRoutingTest( std::span<const int> boxAreaNums ) {
	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const float areaPointZOffset = 1.0f - playerbox_stand_mins[2];

	for( const int areaNum: boxAreaNums ) {
		if( !TestAreaSettings( aasAreaSettings[areaNum] ) ) {
			continue;
		}

		const auto &area = aasAreas[areaNum];
		Vec3 areaPoint( area.center );
		areaPoint.Z() = area.mins[2] + areaPointZOffset;
		float squareDistance = areaPoint.SquareDistanceTo( startOrigin );
		areaPoint.Z() += playerbox_stand_maxs[2];
		// Farthest spots should get evicted first
		new( spotsHeap.unsafe_grow_back() )SpotAndScore( areaPoint.Data(), squareDistance, areaNum, -1 );
	}
}

MovementScript *FallbackAction::TryFindJumpToSpotFallback( PredictionContext *context, bool testTravelTime ) {
	// Cut off these extremely expensive computations
	if( !bot->TryGetVitalComputationQuota() ) {
		return nullptr;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *const fallback = &m_subsystem->jumpToSpotScript;

	auto *const areaDetector = &::bestAreaCenterJumpableSpotDetector;

	// Prepare auxiliary travel time data
	if( testTravelTime ) {
		int navTargetAreaNum = context->NavTargetAasAreaNum();
		if( !navTargetAreaNum ) {
			return nullptr;
		}
		int startTravelTimeToTarget = context->TravelTimeToNavTarget();
		if( !startTravelTimeToTarget ) {
			return nullptr;
		}
		areaDetector->AddRoutingParams( bot->RouteCache(), bot->TravelFlags(), navTargetAreaNum, startTravelTimeToTarget );
	}

	unsigned jumpTravelTime;
	areaDetector->SetJumpPhysicsProps( context->GetRunSpeed(), context->GetJumpSpeed() );
	if( const auto *spot = areaDetector->Exec( entityPhysicsState.Origin(), &jumpTravelTime ) ) {
		fallback->Activate( entityPhysicsState.Origin(), spot->origin, jumpTravelTime );
		return fallback;
	}

	return nullptr;
}

// Can't be defined in the header due to accesing a Bot field
MovementScript *FallbackAction::TryFindJumpAdvancingToTargetFallback( PredictionContext *context ) {
	// Let the bot lose its speed first
	if( bot->MillisInBlockedState() < 100 ) {
		return nullptr;
	}

	return TryFindJumpToSpotFallback( context, true );
}

MovementScript *FallbackAction::TryFindJumpLikeReachFallback( PredictionContext *context,
																		const aas_reachability_t &nextReach ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	// BSPC generates lots of junk jump reachabilities from small steps to small steps, stairs/ramps boundaries, etc
	// Check whether all areas from start and end are grounded and are of the same height,
	// and run instead of jumping in this case

	// Check only if a reachability does not go noticeably upwards
	if( nextReach.end[2] < nextReach.start[2] + 16.0f ) {
		Vec3 reachVec( nextReach.start );
		reachVec -= nextReach.end;
		float squareReachLength = reachVec.SquaredLength();
		// If a reachability is rather short
		if( squareReachLength < wsw::square( 72.0f ) ) {
			// If there is no significant sloppiness
			if( fabsf( reachVec.Z() ) / sqrtf( wsw::square( reachVec.X() ) + wsw::square( reachVec.Y() ) ) < 0.3f ) {
				const auto *aasWorld = AiAasWorld::instance();
				const auto aasAreas = aasWorld->getAreas();
				const auto aasAreaSettings = aasWorld->getAreaSettings();

				int areaNumsBuffer[32];
				areaNumsBuffer[0] = 0;
				const auto tracedAreaNums = aasWorld->traceAreas( nextReach.start, nextReach.end, areaNumsBuffer, 32 );
				const float startAreaZ = aasAreas[tracedAreaNums[0]].mins[2];
				size_t i = 1;
				for(; i < tracedAreaNums.size(); ++i ) {
					const int areaNum = tracedAreaNums[i];
					// Stop on a non-grounded area
					if( !( aasAreaSettings[areaNum].areaflags & AREA_GROUNDED ) ) {
						break;
					}
					// Force jumping over lava/slime/water
					// TODO: not sure about jumppads, if would ever have an intention to pass jumppad without touching it
					constexpr auto undesiredContents = AREACONTENTS_LAVA | AREACONTENTS_SLIME | AREACONTENTS_WATER;
					if( aasAreaSettings[areaNum].contents & undesiredContents ) {
						break;
					}
					// Stop if there is a significant height difference
					if( fabsf( aasAreas[areaNum].mins[2] - startAreaZ ) > 12.0f ) {
						break;
					}
				}

				// All areas pass the walkability test, use walking to a node that seems to be really close
				if( i == tracedAreaNums.size() ) {
					auto *fallback = &m_subsystem->useWalkableNodeScript;
					Vec3 target( nextReach.end );
					target.Z() += 1.0f - playerbox_stand_mins[2];
					fallback->Activate( target.Data(), 24.0f, AiAasWorld::instance()->findAreaNum( target ), 500u );
					return fallback;
				}
			}
		}
	}

	AiTrajectoryPredictor predictor;
	predictor.SetStepMillis( 128 );
	predictor.SetNumSteps( 8 );
	predictor.SetEnterAreaProps( 0, AREACONTENTS_LAVA | AREACONTENTS_SLIME | AREACONTENTS_DONOTENTER );
	predictor.SetEnterAreaNum( nextReach.areanum );
	predictor.SetColliderBounds( vec3_origin, playerbox_stand_maxs );
	predictor.SetEntitiesCollisionProps( true, bot->EntNum() );
	predictor.AddStopEventFlags( AiTrajectoryPredictor::HIT_SOLID | AiTrajectoryPredictor::HIT_ENTITY );

	int numAttempts;
	float startSpeed2D;

	const float *attemptsZBoosts;
	const float *startAirAccelFracs;
	const float *endAirAccelFracs;

	const float jumpAttemptsZBoosts[] = { 0.0f, 10.0f };
	// Not used in prediction but should fit results for assumed start 2D speed
	const float jumpStartAirAccelFracs[] = { 0.0f, 0.5f };
	const float jumpEndAirAccelFracs[] = { 0.0f, 0.0f };

	const float strafejumpAttemptsZBoosts[] = { 0.0f, 15.0f, 40.0f };
	// Not used in prediction but should fit results for assumed start 2D speed
	const float strafejumpStartAirAccelFracs[] = { 0.9f, 1.0f, 1.0f };
	const float strafejumpEndAirAccelFracs[] = { 0.5f, 0.9f, 1.0f };

	if( ( nextReach.traveltype & TRAVELTYPE_MASK ) == TRAVEL_STRAFEJUMP ) {
		// Approximate applied acceleration as having 470 units of starting speed
		// That's what BSPC assumes for generating strafejumping reachabilities
		startSpeed2D = 470.0;
		numAttempts = sizeof( strafejumpAttemptsZBoosts ) / sizeof( *strafejumpAttemptsZBoosts );
		attemptsZBoosts = strafejumpAttemptsZBoosts;
		startAirAccelFracs = strafejumpStartAirAccelFracs;
		endAirAccelFracs = strafejumpEndAirAccelFracs;
	} else {
		startSpeed2D = context->GetRunSpeed();
		numAttempts = sizeof( jumpAttemptsZBoosts ) / sizeof( *jumpAttemptsZBoosts );
		attemptsZBoosts = jumpAttemptsZBoosts;
		startAirAccelFracs = jumpStartAirAccelFracs;
		endAirAccelFracs = jumpEndAirAccelFracs;
	}

	Vec3 startVelocity( nextReach.end );
	startVelocity -= entityPhysicsState.Origin();
	startVelocity.Z() = 0;
	if( !startVelocity.normalize() ) {
		return nullptr;
	}

	startVelocity *= startSpeed2D;

	const float defaultJumpSpeed = context->GetJumpSpeed();

	const auto *routeCache = bot->RouteCache();
	int navTargetAreaNum = context->NavTargetAasAreaNum();
	// Note: we don't stop on the first feasible travel time here and below
	int travelTimeFromReachArea = routeCache->FindRoute( nextReach.areanum, navTargetAreaNum, bot->TravelFlags() );
	if( !travelTimeFromReachArea ) {
		return nullptr;
	}

	AiTrajectoryPredictor::Results predictionResults;

	vec3_t jumpTarget;

	int i = 0;
	for(; i < numAttempts; ++i ) {
		startVelocity.Z() = defaultJumpSpeed + attemptsZBoosts[i];

		if( i ) {
			// Results are cleared by default
			predictionResults.Clear();
		}

		auto stopEvents = predictor.Run( startVelocity.Data(), entityPhysicsState.Origin(), &predictionResults );
		// A trajectory have entered an undesired contents
		if( stopEvents & AiTrajectoryPredictor::ENTER_AREA_CONTENTS ) {
			continue;
		}

		// A trajectory has hit the target area
		if( stopEvents & AiTrajectoryPredictor::ENTER_AREA_NUM ) {
			VectorCopy( nextReach.end, jumpTarget );
			break;
		}

		if( !( stopEvents & AiTrajectoryPredictor::HIT_SOLID ) ) {
			continue;
		}

		if( !ISWALKABLEPLANE( &predictionResults.trace->plane ) ) {
			continue;
		}

		if( predictionResults.trace->contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_WATER | CONTENTS_NODROP ) ) {
			continue;
		}

		const int landingArea = predictionResults.lastAreaNum;
		int travelTimeFromLandingArea = routeCache->FindRoute( landingArea, navTargetAreaNum, bot->TravelFlags() );

		// Note: thats why we are using best travel time among allowed and preferred travel flags
		// (there is a suspicion that many feasible areas might be cut off by the following test otherwise).
		// If the travel time is significantly worse than travel time from reach area
		if( !travelTimeFromLandingArea || travelTimeFromLandingArea - 20 > travelTimeFromReachArea ) {
			continue;
		}

		VectorCopy( predictionResults.origin, jumpTarget );
		break;
	}

	// All attempts have failed
	if( i == numAttempts ) {
		return nullptr;
	}

	jumpTarget[2] += 1.0f - playerbox_stand_mins[2] + game.edicts[bot->EntNum()].viewheight;
	auto *fallback = &m_subsystem->jumpToSpotScript;
	fallback->Activate( entityPhysicsState.Origin(), jumpTarget, predictionResults.millisAhead,
						32.0f, startAirAccelFracs[i], endAirAccelFracs[i], attemptsZBoosts[i] );
	return fallback;
}

class BestConnectedToHubAreasJumpableSpotDetector: public BestRegularJumpableSpotDetector {
	wsw::StaticVector<SpotAndScore, 256> spotsHeap;
	void GetCandidateSpots( SpotAndScore **begin, SpotAndScore **end ) override;
public:
	float searchRadius;
	int currAreaNums[2];
	int numCurrAreas;

	BestConnectedToHubAreasJumpableSpotDetector() {
		SetTestSpotAreaNums( true );
	}
};

static BestConnectedToHubAreasJumpableSpotDetector bestConnectedToHubAreasJumpableSpotDetector;

MovementScript *FallbackAction::TryFindLostNavTargetFallback( PredictionContext *context ) {
	Assert( !context->NavTargetAasAreaNum() );

	// This code is extremely expensive, prevent frametime spikes
	if( !bot->TryGetVitalComputationQuota() ) {
		return nullptr;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *detector = &::bestConnectedToHubAreasJumpableSpotDetector;
	detector->searchRadius = 48.0f + 512.0f * BoundedFraction( bot->MillisInBlockedState(), 2000 );
	detector->numCurrAreas = entityPhysicsState.PrepareRoutingStartAreas( detector->currAreaNums );
	detector->SetJumpPhysicsProps( context->GetRunSpeed(), context->GetJumpSpeed() );
	unsigned millis;
	if( const auto *spot = detector->Exec( entityPhysicsState.Origin(), &millis ) ) {
		auto *fallback = &m_subsystem->jumpToSpotScript;
		// TODO: Compute and set a correct timeout instead of this magic number
		fallback->Activate( entityPhysicsState.Origin(), spot->origin, millis, 32.0f );
		return fallback;
	}

	return nullptr;
}

void BestConnectedToHubAreasJumpableSpotDetector::GetCandidateSpots( SpotAndScore **begin, SpotAndScore **end ) {
	spotsHeap.clear();

	const auto *aasWorld = AiAasWorld::instance();
	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto *aiManager = AiManager::Instance();

	Vec3 boxMins( -searchRadius, -searchRadius, -32.0f - 0.33f * searchRadius );
	Vec3 boxMaxs( +searchRadius, +searchRadius, +24.0f + 0.15f * searchRadius );
	boxMins += startOrigin;
	boxMaxs += startOrigin;

	int areasBuffer[256];
	const auto boxAreaNums = AiAasWorld::instance()->findAreasInBox( boxMins, boxMaxs, areasBuffer, 256 );
	for( const int areaNum: boxAreaNums ) {
		const auto &areaSettings = aasAreaSettings[areaNum];
		if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
			continue;
		}
		if( areaSettings.areaflags & AREA_DISABLED ) {
			continue;
		}
		if( areaSettings.contents & ( AREACONTENTS_LAVA | AREACONTENTS_SLIME | AREACONTENTS_DONOTENTER ) ) {
			continue;
		}
		if( areaNum == currAreaNums[0] || areaNum == currAreaNums[1] ) {
			continue;
		}

		const auto &area = aasAreas[areaNum];
		Vec3 areaPoint( area.center );
		areaPoint.Z() = area.mins[2] + 4.0f - playerbox_stand_mins[2];
		float squareDistance = areaPoint.SquareDistanceTo( startOrigin );
		if( squareDistance < wsw::square( 48.0f ) ) {
			continue;
		}

		if( !aiManager->IsAreaReachableFromHubAreas( areaNum ) ) {
			continue;
		}

		// Use the distance as a spot score in a max-heap.
		// Farthest spots should get evicted first
		new( spotsHeap.unsafe_grow_back() )SpotAndScore( areaPoint.Data(), squareDistance, areaNum, -1 );
	}

	// FindBestJumpableSpot assumes candidates to be a max-heap
	wsw::make_heap( spotsHeap.begin(), spotsHeap.end() );
	*begin = spotsHeap.begin();
	*end = spotsHeap.end();
}