#include "landonsavedareasaction.h"
#include "movementlocal.h"
#include "triggerareanumscache.h"

float LandOnSavedAreasAction::SaveJumppadLandingAreas( const edict_t *jumppadEntity ) {
	savedLandingAreas.clear();

	const auto [jumppadAreaNum, jumppadTargetAreaNums] = triggerAreaNumsCache.getJumppadAreaNumAndTargetAreaNums( jumppadEntity->s.number );
	assert( jumppadAreaNum > 0 );

	const auto *aasWorld        = AiAasWorld::instance();
	const auto *routeCache      = bot->RouteCache();
	const auto *aasAreas        = aasWorld->getAreas().data();
	const auto *aasAreaSettings = aasWorld->getAreaSettings().data();

	int nextTravelTime    = 0;
	int nextReachAreaNum  = 0;
	int navTargetAreaNum  = bot->NavTargetAasAreaNum();
	if( navTargetAreaNum ) {
		int reachNum   = 0;
		nextTravelTime = routeCache->FindRoute( jumppadAreaNum, navTargetAreaNum, bot->TravelFlags(), &reachNum );
		if( nextTravelTime ) {
			if( reachNum ) {
				nextReachAreaNum = aasWorld->getReaches()[reachNum].areanum;
			} else {
				// If it ends directly in the target area (wtf?)
				savedLandingAreas.push_back( navTargetAreaNum );
				return aasWorld->getAreas()[navTargetAreaNum].mins[2];
			}
		}
	}

	// Filter raw nearby areas
	FilteredAreas filteredAreas;
	for( const int areaNum: jumppadTargetAreaNums ) {
		// Skip tests for the next area
		if( areaNum == nextReachAreaNum ) {
			continue;
		}

		float score = 1.0f;
		if( navTargetAreaNum ) {
			const int travelTime = routeCache->FindRoute( areaNum, navTargetAreaNum, bot->TravelFlags() );
			// If the nav target is not reachable from the box area or
			// it leads to a greater travel time than the jumppad target area
			if( !travelTime || travelTime >= nextTravelTime ) {
				continue;
			}
			// The score is greater if it shortens travel time greater
			score = (float)nextTravelTime / (float)travelTime;
		}

		// Apply penalty for ledge areas (prevent falling just after landing)
		if( aasAreaSettings[areaNum].areaflags & AREA_LEDGE ) {
			score *= 0.75f;
		}
		if( aasAreaSettings[areaNum].areaflags & AREA_JUNK ) {
			score *= 0.33f;
		}

		filteredAreas.emplace_back( AreaAndScore( areaNum, score ) );
		if( filteredAreas.full() ) {
			break;
		}
	}

	// Sort filtered areas so best areas are first
	wsw::sortByFieldDescending( filteredAreas.begin(), filteredAreas.end(), &AreaAndScore::score );

	savedLandingAreas.clear();

	for( unsigned i = 0, end = wsw::min( filteredAreas.size(), savedLandingAreas.capacity() ); i < end; ++i ) {
		savedLandingAreas.push_back( filteredAreas[i].areaNum );
	}

	// Always add the target area (with the lowest priority)
	if( nextReachAreaNum ) {
		if( savedLandingAreas.full() ) {
			savedLandingAreas.pop_back();
		}
		savedLandingAreas.push_back( nextReachAreaNum );
	}

	if( !savedLandingAreas.empty() ) {
		float minAreaZ = std::numeric_limits<float>::max();
		for( int areaNum: savedLandingAreas ) {
			minAreaZ = wsw::min( minAreaZ, aasAreas[areaNum].mins[2] );
		}
		return minAreaZ;
	}

	// Force starting landing attempts (almost) immediately
	return std::numeric_limits<float>::lowest();
}

void LandOnSavedAreasAction::BeforePlanning() {
	BaseAction::BeforePlanning();
	currAreaIndex = -1;
	totalTestedAreas = 0;

	this->savedLandingAreas.clear();
	for( int areaNum: m_subsystem->savedLandingAreas )
		this->savedLandingAreas.push_back( areaNum );

	m_subsystem->savedLandingAreas.clear();
}

void LandOnSavedAreasAction::AfterPlanning() {
	BaseAction::AfterPlanning();
	if( this->isDisabledForPlanning ) {
		return;
	}
	if( this->savedLandingAreas.empty() ) {
		return;
	}

	m_subsystem->savedLandingAreas.clear();
	for( int areaNum: this->savedLandingAreas )
		m_subsystem->savedLandingAreas.push_back( areaNum );
}

bool LandOnSavedAreasAction::TryLandingStepOnArea( int areaNum, PredictionContext *context ) {
	auto *botInput = &context->record->botInput;
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const float *origin = entityPhysicsState.Origin();

	const auto &area = AiAasWorld::instance()->getAreas()[areaNum];
	Vec3 areaPoint( area.center );
	// Lower area point to a bottom of area. Area mins/maxs are absolute.
	areaPoint.Z() = area.mins[2] + ( -playerbox_stand_mins[2] );

	Vec3 intendedLookDir( Vec3( areaPoint ) - origin );
	if( !intendedLookDir.normalizeFast() ) {
		return false;
	}

	botInput->Clear();
	botInput->isUcmdSet = true;

	botInput->SetIntendedLookDir( intendedLookDir, true );

	// Disallow any input rotation while landing, it relies on a side aircontrol.
	botInput->SetAllowedRotationMask( InputRotation::NONE );

	// While we do not use forwardbunny, there is still a little air control
	// from forward key, even without PMFEAT_AIRCONTROL feature
	if( entityPhysicsState.ForwardDir().Dot( intendedLookDir ) > 0.7f ) {
		botInput->SetForwardMovement( +1 );
		if( entityPhysicsState.Speed2D() > 1.0f ) {
			Vec3 velocity2DDir( entityPhysicsState.Velocity()[0], entityPhysicsState.Velocity()[1], 0.0f );
			velocity2DDir *= Q_Rcp( entityPhysicsState.Speed2D() );
			if( velocity2DDir.Dot( intendedLookDir ) > 0.3f ) {
				context->CheatingAccelerate( 1.0f );
			} else {
				context->CheatingCorrectVelocity( areaPoint );
			}
		} else {
			Vec3 velocity( entityPhysicsState.Velocity() );
			velocity += 5.0f * intendedLookDir;
			context->record->SetModifiedVelocity( velocity );
		}
	} else {
		botInput->SetTurnSpeedMultiplier( 5.0f );
	}

	return true;
}

void LandOnSavedAreasAction::PlanPredictionStep( PredictionContext *context ) {
	if( !GenericCheckIsActionEnabled( context ) ) {
		return;
	}

	// This list might be empty if all nearby areas have been disabled (e.g. as blocked by enemy).
	if( savedLandingAreas.empty() ) {
		Debug( "Cannot apply action: the saved landing areas list is empty\n" );
		this->isDisabledForPlanning = true;
		context->cannotApplyAction = true;
		return;
	}

	// If there the current tested area is set
	if( currAreaIndex >= 0 ) {
		Assert( (int)savedLandingAreas.size() > currAreaIndex );
		// Continue testing this area
		if( TryLandingStepOnArea( savedLandingAreas[currAreaIndex], context ) ) {
			return;
		}

		// Schedule next saved area for testing
		const char *format = "Landing on area %d/%d has failed, roll back to initial landing state for next area\n";
		Debug( format, currAreaIndex, savedLandingAreas.size() );
		currAreaIndex = -1;
		totalTestedAreas++;
		// Force rolling back to savepoint
		context->SetPendingRollback();
		// (the method execution implicitly will be continued on the code below outside this condition on next call)
		return;
	}

	// There is not current tested area set, try choose one that fit
	for(; totalTestedAreas < savedLandingAreas.size(); totalTestedAreas++ ) {
		// Test each area left using a-priori feasibility of an area
		if( TryLandingStepOnArea( savedLandingAreas[totalTestedAreas], context ) ) {
			// Set the area as current
			currAreaIndex = totalTestedAreas;
			// Create a savepoint
			context->savepointTopOfStackIndex = context->topOfStackIndex;
			// (the method execution will be implicitly continue on the code inside the condition above on next call)
			Debug( "Area %d/%d has been chosen for landing tests\n", currAreaIndex, savedLandingAreas.size() );
			return;
		}
	}

	// All areas have been tested, and there is no suitable area for landing
	Debug( "Warning: An area suitable for landing has not been found\n" );

	// Just look at the target
	const auto &movementState = context->movementState;
	Vec3 toTargetDir( movementState->entityPhysicsState.Origin() );
	if( movementState->weaponJumpMovementState.IsActive() ) {
		toTargetDir -= movementState->weaponJumpMovementState.JumpTarget();
	} else if( movementState->jumppadMovementState.IsActive() ) {
		toTargetDir -= movementState->jumppadMovementState.JumpTarget();
	} else {
		AI_FailWith( "LandOnSavedAreasAction::PlanPredictionStep()", "Neither jumppad nor weapon jump states is active" );
	}

	toTargetDir *= -1;
	if( !toTargetDir.normalizeFast() ) {
		toTargetDir.Set( 0.0f, 0.0f, -1.0f );
	}

	auto *botInput = &context->record->botInput;
	botInput->SetIntendedLookDir( toTargetDir );

	// Use the simplest but the most reliable kind of movement in this case
	float dotForward = toTargetDir.Dot( movementState->entityPhysicsState.ForwardDir() );
	if( dotForward < 0.7f ) {
		botInput->SetTurnSpeedMultiplier( 5.0f );
	} else {
		botInput->SetForwardMovement( 1 );
	}

	// Disallow any input rotation while landing, it relies on a side aircontrol.
	botInput->SetAllowedRotationMask( InputRotation::NONE );

	botInput->isUcmdSet = true;

	// Do not predict ahead.
	context->isCompleted = true;
}

void LandOnSavedAreasAction::CheckPredictionStepResults( PredictionContext *context ) {
	BaseAction::CheckPredictionStepResults( context );
	// If movement step failed, make sure that the next area (if any) will be tested after rollback
	if( context->cannotApplyAction ) {
		totalTestedAreas++;
		currAreaIndex = -1;
		return;
	}

	if( context->isCompleted ) {
		return;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( !entityPhysicsState.GroundEntity() ) {
		// Check whether to continue prediction still makes sense
		if( context->topOfStackIndex + 1 == PredictionContext::MAX_PREDICTED_STATES ) {
			// Stop wasting CPU cycles on this. Also prevent overflow of the prediction stack
			// leading to inability of restarting the action for testing a next area (if any).
			currAreaIndex = -1;
			totalTestedAreas++;
			context->SetPendingRollback();
		}
		return;
	}

	// Check which area bot has landed in
	Assert( currAreaIndex >= 0 && currAreaIndex == (int)totalTestedAreas && currAreaIndex < (int)savedLandingAreas.size() );
	const int targetAreaNum = savedLandingAreas[currAreaIndex];
	int currAreaNums[2] = { 0, 0 };
	const int numCurrAreas = entityPhysicsState.PrepareRoutingStartAreas( currAreaNums );
	// If the bot is in the target area
	if( currAreaNums[0] == targetAreaNum || currAreaNums[1] == targetAreaNum ) {
		Debug( "A prediction step has lead to touching a ground in the target landing area, should stop planning\n" );
		context->isCompleted = true;
		return;
	}

	const auto *aasWorld = AiAasWorld::instance();
	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaFloorClusterNums = aasWorld->areaFloorClusterNums();
	// If the target area is in some floor cluster
	if( int targetFloorClusterNum = aasAreaFloorClusterNums[targetAreaNum] ) {
		int i = 0;
		for(; i < numCurrAreas; ++i ) {
			if( aasAreaFloorClusterNums[currAreaNums[i]] == targetFloorClusterNum ) {
				break;
			}
		}
		// Some of the current areas is in the same cluster
		if( i != numCurrAreas ) {
			Debug( "A prediction step has lead to touching a ground in the floor cluster of the landing area\n" );
			context->isCompleted = true;
			return;
		}
	} else {
		// Check whether the target area is reachable from the current area by walking and seems to be straight-walkable
		int bestTravelTime = std::numeric_limits<int>::max();
		const auto *routeCache = bot->RouteCache();
		for( int i = 0; i < numCurrAreas; ++i ) {
			int travelFlags = TFL_WALK | TFL_WALKOFFLEDGE | TFL_AIR;
			if( int travelTime = routeCache->FindRoute( currAreaNums[i], targetAreaNum, travelFlags ) ) {
				bestTravelTime = wsw::min( travelTime, bestTravelTime );
			}
		}
		// If the target area is short-range reachable by walking (in 150 seconds^-2)
		if( bestTravelTime < 150 ) {
			Vec3 testedTargetPoint( aasAreas[targetAreaNum].center );
			// We are sure the target area is grounded
			testedTargetPoint.Z() = aasAreas[targetAreaNum].mins[2] + 1.0f - playerbox_stand_mins[2];
			// Add a unit offset from ground
			Vec3 currPoint( entityPhysicsState.Origin() );
			currPoint.Z() += 1.0f;
			// We have to check against entities in this case
			trace_t trace;
			const auto *ignore = game.edicts + bot->EntNum();
			const float *mins = playerbox_stand_mins;
			const float *maxs = playerbox_stand_maxs;
			G_Trace( &trace, currPoint.Data(), mins, maxs, testedTargetPoint.Data(), ignore, MASK_PLAYERSOLID );
			if( trace.fraction == 1.0f ) {
				Debug( "A prediction step has lead to touching a ground in a short-range neighbour area of the target area\n" );
				context->isCompleted = true;
				return;
			}
		}
	}

	Debug( "A prediction step has lead to touching a ground in an unexpected area\n" );
	context->SetPendingRollback();
	// Make sure that the next area (if any) will be tested after rolling back
	totalTestedAreas++;
	currAreaIndex = -1;
}