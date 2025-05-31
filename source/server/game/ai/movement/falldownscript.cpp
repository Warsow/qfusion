#include "falldownscript.h"
#include "movementlocal.h"

bool FallDownScript::TryDeactivate( PredictionContext *context ) {
	assert( status == PENDING );

	if( level.time - activatedAt > timeout ) {
		status = INVALID;
		return true;
	}

	const AiEntityPhysicsState *entityPhysicsState;
	if( context ) {
		entityPhysicsState = &context->movementState->entityPhysicsState;
	} else {
		entityPhysicsState = bot->EntityPhysicsState();
	}

	// Wait for touching any ground
	if( !entityPhysicsState->GroundEntity() ) {
		return false;
	}

	if( DistanceSquared( entityPhysicsState->Origin(), targetOrigin ) > reachRadius * reachRadius ) {
		return false;
	}

	return entityPhysicsState->Origin()[2] < targetOrigin[2];
}

void FallDownScript::SetupMovement( PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	Vec3 toTargetDir( entityPhysicsState.Origin() );
	toTargetDir.Z() += game.edicts[bot->EntNum()].viewheight;
	toTargetDir -= entityPhysicsState.GroundEntity() ? startOrigin : targetOrigin;
	toTargetDir *= -1.0f;
	if( !toTargetDir.normalize() ) {
		return;
	}

	botInput->isUcmdSet = true;

	if( entityPhysicsState.GroundEntity() ) {
		// Keep looking at this point.
		// Otherwise bot spinning in front of enemy looks weird.
		if( const std::optional<Vec3> &keptInFovPoint = bot->GetKeptInFovPoint() ) {
			Vec3 toPointVec( *keptInFovPoint );
			toPointVec -= entityPhysicsState.Origin();
			botInput->SetIntendedLookDir( toPointVec, false );
			int keyMoves[2];
			context->TraceCache().makeKeyMovesToTarget( context, toTargetDir, keyMoves );
			botInput->SetForwardMovement( keyMoves[0] );
			botInput->SetRightMovement( keyMoves[1] );
			botInput->canOverrideLookVec = true;
			return;
		}

		botInput->SetIntendedLookDir( toTargetDir, true );
		const float dot = toTargetDir.Dot( entityPhysicsState.ForwardDir() );
		if( dot < 0.9f ) {
			botInput->SetTurnSpeedMultiplier( 15.0f );
		} else {
			botInput->SetForwardMovement( 1 );
			if( dot < 0.99f ) {
				botInput->SetWalkButton( true );
			}
		}
		return;
	}

	// We're falling and might miss the target
	Vec3 velocityDir( entityPhysicsState.Velocity() );
	if( entityPhysicsState.Speed() < 1 ) {
		return;
	}

	velocityDir *= Q_Rcp( entityPhysicsState.Speed() );

	// If the velocity fairly conforms the direction to target
	if( velocityDir.Dot( toTargetDir ) > 0.95f ) {
		botInput->SetIntendedLookDir( toTargetDir );
		return;
	}

	// Stop looking down and try gain some side velocity using the slight forward air-control
	toTargetDir.Z() = 0;
	if( !toTargetDir.normalize() ) {
		return;
	}

	botInput->SetIntendedLookDir( toTargetDir, false );

	if( entityPhysicsState.ForwardDir().Dot( toTargetDir ) < 0.95f ) {
		botInput->SetTurnSpeedMultiplier( 15.0f );
	} else {
		botInput->SetForwardMovement( 1 );
		// Prevent weird-looking accelerated falling
		if( entityPhysicsState.Velocity()[2] > -25.0f ) {
			context->CheatingAccelerate( 0.5f );
		}
	}
}

MovementScript *FallbackAction::TryFindWalkOffLedgeReachFallback( PredictionContext *context,
																			const aas_reachability_t &nextReach ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	// If the falling distance is really low, treat is just as walking to a node
	const float squareFallingHeight = DistanceSquared( nextReach.start, nextReach.end );
	if( squareFallingHeight < wsw::square( 40.0f ) ) {
		auto *script = &m_subsystem->useWalkableNodeScript;
		float squareDistance = DistanceSquared( entityPhysicsState.Origin(), nextReach.start );
		unsigned timeout = 100 + (unsigned)( 1000.0f * sqrtf( squareDistance ) / context->GetRunSpeed() );
		Vec3 target( nextReach.start );
		target.Z() += 1.0f - playerbox_stand_mins[2];
		script->Activate( target.Data(), 16.0f, nextReach.areanum, timeout );
		return script;
	}

	const int targetAreaNum = nextReach.areanum;
	const auto &targetArea = AiAasWorld::instance()->getAreas()[targetAreaNum];

	auto *script = &m_subsystem->fallDownScript;
	// Set target not to the reach. end but to the center of the target area (a reach end is often at ledge)
	// Setting the proper Z (should be greater than an origin of bot standing at destination) is important!
	Vec3 targetOrigin( targetArea.center[0], targetArea.center[1], targetArea.mins[2] + 4.0f - playerbox_stand_mins[2] );
	// Compute the proper timeout
	float distanceToReach = sqrtf( DistanceSquared( entityPhysicsState.Origin(), nextReach.start ) );
	unsigned travelTimeToLedgeMillis = (unsigned)( 1000.0f * distanceToReach / context->GetRunSpeed() );
	unsigned fallingTimeMillis = (unsigned)( 1000.0f * sqrtf( 2.0f * sqrtf( squareFallingHeight ) / level.gravity ) );
	script->Activate( nextReach.start, targetOrigin.Data(), travelTimeToLedgeMillis + fallingTimeMillis + 250, 24.0f );
	return script;
}
