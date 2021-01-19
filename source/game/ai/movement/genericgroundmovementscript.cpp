#include "genericgroundmovementscript.h"
#include "movementlocal.h"
#include "../combat/tacticalspotsregistry.h"

/*
bool GenericGroundMovementScript::TryDeactivate( Context *context ) {
	// This code is useful for all children and should be called first in all overridden implementations

	// Deactivate the action with success if the bot has touched a trigger
	// (was the bot targeting at it does not matter)

	if( level.time - bot->LastTriggerTouchTime() < 64 ) {
		// Consider the completion successful
		status = COMPLETED;
		return true;
	}

	if( level.time - bot->LastKnockbackAt() < 64 ) {
		// Consider the action failed
		status = INVALID;
		return true;
	}

	return false;
}

bool GenericGroundMovementScript::ShouldSkipTests( Context *context ) {
	if( context ) {
		return !context->movementState->entityPhysicsState.GroundEntity();
	}

	return !game.edicts[bot->EntNum()].groundentity;
}

int GenericGroundMovementScript::GetCurrBotAreas( int *areaNums, Context *context ) {
	if( context ) {
		return context->movementState->entityPhysicsState.PrepareRoutingStartAreas( areaNums );
	}

	return bot->EntityPhysicsState()->PrepareRoutingStartAreas( areaNums );
}

bool GenericGroundMovementScript::SetupForKeptPointInFov( MovementPredictionContext *context,
														  const float *steeringTarget,
														  const float *keptInFovPoint ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	Vec3 intendedMoveDir( steeringTarget );
	intendedMoveDir -= entityPhysicsState.Origin();
	const float distanceToTarget = intendedMoveDir.Normalize();
	int keyMoves[2];
	// While using makeRandomizedKeyMovesToTarget() is desirable,
	// we do not use movement prediction and thus it should be avoided due to possible bot mistakes.
	context->TraceCache().makeKeyMovesToTarget( context, intendedMoveDir, keyMoves );
	// The call above does not guarantee producing at least a single pressed direction key
	if( !( keyMoves[0] | keyMoves[1] ) ) {
		return false;
	}

	Vec3 intendedLookVec( keptInFovPoint );
	intendedLookVec -= entityPhysicsState.Origin();
	botInput->SetIntendedLookDir( intendedLookVec, false );
	botInput->canOverrideLookVec = true;
	botInput->isUcmdSet = true;
	botInput->SetForwardMovement( keyMoves[0] );
	botInput->SetRightMovement( keyMoves[1] );

	if( !this->allowDashing ) {
		return true;
	}

	// Check whether various bot state parameters allow dashing.
	// Return with successfully set up state for kept-in-fov point anyway.

	if( !entityPhysicsState.GroundEntity() ) {
		return true;
	}

	if( ( bot->ShouldBeSilent() || bot->ShouldMoveCarefully() ) ) {
		return true;
	}

	// Check whether it's safe to dash (or jump)
	// 1) there should be only a single direction defined
	// 2) the target should be relatively far from the bot
	// 3) the target should conform to the direction key well

	if( !( keyMoves[0] ^ keyMoves[1] ) ) {
		return true;
	}

	bool canUseDash = true;
	const auto *pmStats = context->currPlayerState->pmove.stats;
	// Try jumping in case whether the bot has a relatively high speed and it conforms to the intended direction well
	if( !( ( pmStats[PM_STAT_FEATURES] & PMFEAT_DASH ) && !pmStats[PM_STAT_DASHTIME] ) ) {
		// Check whether jumping is allowed
		if ( !allowRunning || !( ( pmStats[PM_STAT_FEATURES] & PMFEAT_JUMP ) ) ) {
			return true;
		}
		// There's no point in trying to keep speed if it's this low
		if( entityPhysicsState.Speed2D() <= context->GetRunSpeed() ) {
			return true;
		}
		// Check whether the bot becomes volatile for an aiming enemy having jumped
		if( bot->ShouldSkinBunnyInFavorOfCombatMovement() ) {
			return true;
		}
		const auto *aasWorld = AiAasWorld::Instance();
		// Try keeping speed only in NOFALL areas that belong to some floor cluster
		const int currGroundedAreaNum = context->CurrGroundedAasAreaNum();
		if( !aasWorld->AreaFloorClusterNums()[currGroundedAreaNum] ) {
			return true;
		}
		if( !( aasWorld->AreaSettings()[currGroundedAreaNum].areaflags & AREA_NOFALL ) ) {
			return true;
		}
		canUseDash = false;
	}

	// Use a larger distance threshold if we're going to jump
	if( distanceToTarget < ( canUseDash ? 72.0f : 96.0f ) ) {
		return true;
	}

	vec3_t keyDir;
	if( keyMoves[0] ) {
		entityPhysicsState.ForwardDir().CopyTo( keyDir );
		VectorScale( keyDir, keyMoves[0], keyDir );
	} else {
		entityPhysicsState.RightDir().CopyTo( keyDir );
		VectorScale( keyDir, keyMoves[1], keyDir );
	}

	if( canUseDash ) {
		botInput->SetSpecialButton( true );
		return true;
	}

	const float *velocity = entityPhysicsState.Velocity();
	if ( intendedMoveDir.Dot( keyDir ) > 0.9f && DotProduct( velocity, keyDir ) > 0.9f ) {
		botInput->SetUpMovement( 1 );
	}

	return true;
}

void GenericGroundMovementScript::SetupMovement( Context *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;
	const auto &miscTactics = bot->GetMiscTactics();
	const auto *pmStats = context->currPlayerState->pmove.stats;

	vec3_t steeringTarget;
	// Call an overridden by a child method
	this->GetSteeringTarget( steeringTarget );
#if 0
	AITools_DrawColorLine( entityPhysicsState.Origin(), steeringTarget, DebugColor(), 0 );
#endif

	// Try to keep looking at this point if it is present and use 4 direction keys.
	// Even if there might be some faults, turning from (potential) enemies looks poor.
	if( const float *keptInFovPoint = bot->GetKeptInFovPoint() ) {
		if( SetupForKeptPointInFov( context, steeringTarget, keptInFovPoint ) ) {
			return;
		}
	}

	Vec3 intendedLookDir( steeringTarget );
	intendedLookDir -= entityPhysicsState.Origin();

	const float squareDistanceToTarget = intendedLookDir.SquaredLength();
	intendedLookDir.Z() *= Z_NO_BEND_SCALE;
	intendedLookDir.Normalize();

	botInput->SetIntendedLookDir( intendedLookDir, true );

	// Set 1.0f as a default value to prevent blocking in some cases
	float intendedDotActual = 1.0f;
	// We should operate on vectors in 2D plane, otherwise we get dot product match rather selfdom.
	Vec3 intendedLookDir2D( intendedLookDir.X(), intendedLookDir.Y(), 0.0f );
	if( intendedLookDir2D.SquaredLength() > 0.001f ) {
		intendedLookDir2D.Normalize();
		Vec3 forward2DDir( entityPhysicsState.ForwardDir() );
		forward2DDir.Z() = 0;
		if( forward2DDir.SquaredLength() > 0.001f ) {
			forward2DDir.Normalize();
			intendedDotActual = intendedLookDir2D.Dot( forward2DDir );
		}
	}

	const auto isInNofallArea = AiAasWorld::Instance()->AreaSettings()[context->CurrAasAreaNum()].areaflags & AREA_NOFALL;

	if( !entityPhysicsState.GroundEntity() ) {
		if( intendedDotActual > 0.95f ) {
			if( allowAirAccel && isInNofallArea && squareDistanceToTarget > SQUARE( 96.0f ) ) {
				context->CheatingAccelerate( 0.5f );
			}
			return;
		} else if( intendedDotActual < 0 ) {
			return;
		}

		if( !( pmStats[PM_STAT_FEATURES] & PMFEAT_AIRCONTROL ) ) {
			return;
		}

		context->CheatingCorrectVelocity( steeringTarget );
		return;
	}

	if( intendedDotActual < 0.7f ) {
		botInput->SetForwardMovement( 0 );
		botInput->SetTurnSpeedMultiplier( 5.0f );
		return;
	}

	botInput->SetForwardMovement( 1 );
	botInput->SetWalkButton( true );
	if( allowRunning ) {
		assert( intendedDotActual >= 0.7f );
		// Map [0.7, 1.0] to [0.0, 1.0]
		float frac = ( intendedDotActual - 0.7f ) / ( 1.0f - 0.7f );
		assert( frac >= 0.0f && frac <= 1.001f );
		// Raise threshold for a low `frac`
		float threshold = 16.0f + ( isInNofallArea ? 16.0f : 32.0f ) * ( 1.0f - frac );
		if( squareDistanceToTarget > SQUARE( threshold ) ) {
			botInput->SetWalkButton( false );
		}
	}

	if( allowCrouchSliding ) {
		if( ShouldCrouchSlideNow( context ) || ShouldPrepareForCrouchSliding( context ) ) {
			botInput->SetUpMovement( -1 );
			return;
		}
	}

	// Try setting a forward dash, the only kind of non-ground movement allowed in this sub-action
	if( miscTactics.shouldBeSilent || miscTactics.shouldMoveCarefully ) {
		return;
	}

	if( !allowDashing || !( pmStats[PM_STAT_FEATURES] & PMFEAT_DASH ) ) {
		return;
	}

	if( intendedDotActual < 0.95f ) {
		return;
	}

	if( squareDistanceToTarget < SQUARE( 72.0f ) ) {
		return;
	}

	// Use dash where possible
	if( !pmStats[PM_STAT_DASHTIME] ) {
		botInput->SetSpecialButton( true );
		return;
	}

	// Try keeping a high speed under certain conditions by jumping
	if( !( pmStats[PM_STAT_FEATURES] & PMFEAT_JUMP ) ) {
		return;
	}

	if( bot->Skill() <= 0.33f ) {
		return;
	}

	const float speed2D = entityPhysicsState.Speed2D();
	if( speed2D < 0.5f * ( context->GetRunSpeed() + context->GetDashSpeed() ) ) {
		return;
	}

	if( !isInNofallArea && ( squareDistanceToTarget < SQUARE( 128.0f ) || speed2D > 550.0f ) ) {
		return;
	}

	Vec3 velocityDir( entityPhysicsState.Velocity() );
	velocityDir *= Q_Rcp( entityPhysicsState.Speed() );
	if( velocityDir.Dot( intendedLookDir ) < ( isInNofallArea ? 0.90f : 0.99f ) ) {
		return;
	}

	botInput->SetUpMovement( +1 );
}

bool GenericGroundMovementScript::TestActualWalkability( int targetAreaNum, const vec3_t targetOrigin, Context *context ) {
	const AiEntityPhysicsState *entityPhysicsState;
	if( context ) {
		entityPhysicsState = &context->movementState->entityPhysicsState;
	} else {
		entityPhysicsState = bot->EntityPhysicsState();
	}

	const auto &routeCache = bot->RouteCache();
	int fromAreaNums[2];
	const int numFromAreas = entityPhysicsState->PrepareRoutingStartAreas( fromAreaNums );
	int travelTimeToTarget = 0;
	for( int j = 0; j < numFromAreas; ++j ) {
		travelTimeToTarget = routeCache->TravelTimeToGoalArea( fromAreaNums[j], targetAreaNum, TRAVEL_FLAGS );
		if( travelTimeToTarget && travelTimeToTarget <= 300 ) {
			break;
		}
	}

	// If the target is not reachable or the travel time is way too large now
	if( !travelTimeToTarget || travelTimeToTarget > 300 ) {
		return false;
	}

	// Test whether the spot can be still considered walkable by results of a coarse trace test

	// TODO: Use floor cluster raycasting too (if the target is in the same floor cluster)?
	// TODO: Use nav mesh first to cut off expensive trace computations?
	// There would be problems though:
	// Floor cluster raycasting is limited and still requires a final trace test
	// Nav mesh raycasting is not reliable due to poor quality of produced nav meshes.

	trace_t trace;
	vec3_t traceMins, traceMaxs;
	TacticalSpotsRegistry::GetSpotsWalkabilityTraceBounds( traceMins, traceMaxs );
	float *start = const_cast<float *>( entityPhysicsState->Origin() );
	float *end = const_cast<float *>( targetOrigin );
	edict_t *skip = game.edicts + bot->EntNum();
	// Allow hitting triggers (for "walk to a trigger" fallbacks)
	// Otherwise a trace hits at a solid brush behind a trigger and we witness a false negative.
	G_Trace( &trace, start, traceMins, traceMaxs, end, skip, MASK_PLAYERSOLID | CONTENTS_TRIGGER );
	if( trace.fraction != 1.0f ) {
		if( game.edicts[trace.ent].r.solid != SOLID_TRIGGER ) {
			return false;
		}
	}

	return true;
}*/