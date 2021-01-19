#include "fallbackmovementaction.h"
#include "movementscript.h"
#include "movementlocal.h"
#include "bestjumpablespotdetector.h"
#include "../combat/tacticalspotsregistry.h"
#include "../manager.h"
#include "../trajectorypredictor.h"

/*
void FallbackMovementAction::PlanPredictionStep( Context *context ) {
	bool handledSpecialMovement = false;
	if( auto *fallback = module->activeMovementScript ) {
		fallback->SetupMovement( context );
		handledSpecialMovement = true;
	} else if( context->IsInNavTargetArea() ) {
		SetupNavTargetAreaMovement( context );
		handledSpecialMovement = true;
	}

	// If we have saved a path that is not perfect but good enough during attempts for bunny-hopping prediction
	if( !context->goodEnoughPath.empty() || !context->lastResortPath.empty() ) {
		const char *tag;
		MovementPredictionContext::PredictedPath *path;
		if( !context->goodEnoughPath.empty() ) {
			path = &context->goodEnoughPath;
			tag = "a good enough";
		} else {
			path = &context->lastResortPath;
			tag = "a last resort";
		}

		assert( !path->empty() );

		context->predictedMovementActions.clear();
		for( const auto &pathElem : *path ) {
			context->predictedMovementActions.push_back( pathElem );
		}

		context->goodEnoughPath.clear();
		context->lastResortPath.clear();

		context->isCompleted = true;
		// Prevent saving the current (fallback) action on stack... TODO: this should be done more explicitly
		context->isTruncated = true;
		Debug( "Using %s path built during this planning session\n", tag );
		Debug( "The good enough path starts with %s\n", context->predictedMovementActions.front().action->Name() );
		return;
	}

	auto *botInput = &context->record->botInput;
	if( handledSpecialMovement ) {
		botInput->SetAllowedRotationMask( BotInputRotation::NONE );
	} else {
		const auto &entityPhysicsState = context->movementState->entityPhysicsState;
		if( !entityPhysicsState.GroundEntity() ) {
			// Fallback path movement is the last hope action, wait for landing
			SetupLostNavTargetMovement( context );
		} else if( auto *fallback = TryFindMovementFallback( context ) ) {
			module->activeMovementScript = fallback;
			fallback->SetupMovement( context );
			handledSpecialMovement = true;
			botInput->SetAllowedRotationMask( BotInputRotation::NONE );
		} else {
			// This often leads to bot blocking and suicide. TODO: Invesigate what else can be done.
			botInput->Clear();
			if( const float *keptInFovPoint = bot->GetKeptInFovPoint() ) {
				Vec3 intendedLookVec( keptInFovPoint );
				intendedLookVec -= entityPhysicsState.Origin();
				botInput->SetIntendedLookDir( intendedLookVec, false );
			}
		}
	}

	botInput->isUcmdSet = true;
	Debug( "Planning is complete: the action should never be predicted ahead\n" );
	context->isCompleted = true;
}

void FallbackMovementAction::SetupNavTargetAreaMovement( Context *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	Vec3 intendedLookDir( context->NavTargetOrigin() );
	intendedLookDir -= entityPhysicsState.Origin();
	intendedLookDir.NormalizeFast();
	botInput->SetIntendedLookDir( intendedLookDir, true );

	if( entityPhysicsState.GroundEntity() ) {
		botInput->SetForwardMovement( true );
		if( bot->ShouldMoveCarefully() ) {
			botInput->SetWalkButton( true );
		} else if( context->IsCloseToNavTarget() ) {
			botInput->SetWalkButton( true );
		}
	} else {
		// Try applying QW-like aircontrol
		float dotForward = intendedLookDir.Dot( entityPhysicsState.ForwardDir() );
		if( dotForward > 0 ) {
			float dotRight = intendedLookDir.Dot( entityPhysicsState.RightDir() );
			if( dotRight > 0.3f ) {
				botInput->SetRightMovement( +1 );
			} else if( dotRight < -0.3f ) {
				botInput->SetRightMovement( -1 );
			}
		}
	}
}

void FallbackMovementAction::SetupLostNavTargetMovement( Context *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	// Looks like the nav target is lost due to being high above the ground

	// If there is a substantial 2D speed, looks like the bot is jumping over a gap
	if( entityPhysicsState.Speed2D() > context->GetRunSpeed() - 50 ) {
		// Keep looking in the velocity direction
		botInput->SetIntendedLookDir( entityPhysicsState.Velocity(), false );
		return;
	}

	if( !entityPhysicsState.IsHighAboveGround() ) {
		// Keep looking in the velocity direction
		if( entityPhysicsState.SquareSpeed() > 1 ) {
			botInput->SetIntendedLookDir( entityPhysicsState.Velocity(), false );
			return;
		}
	}

	// Keep looking in the current direction
	botInput->SetIntendedLookDir( entityPhysicsState.ForwardDir(), true );
}

MovementScript *FallbackMovementAction::TryFindMovementFallback( Context *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	// First check for being in lava
	// TODO: Inspect why waterType does not work as intended
	if( entityPhysicsState.waterLevel >= 1 ) {
		const auto *aasAreaSettings = AiAasWorld::Instance()->AreaSettings();
		int currAreaNums[2] = { 0, 0 };
		if( int numCurrAreas = entityPhysicsState.PrepareRoutingStartAreas( currAreaNums ) ) {
			int i = 0;
			// Try check whether there is really lava here
			for( ; i < numCurrAreas; ++i ) {
				if( aasAreaSettings[currAreaNums[i]].contents & ( AREACONTENTS_LAVA | AREACONTENTS_SLIME ) ) {
					break;
				}
			}
			// Start checking for jumping fallback only after that (do not fail with double computations!)
			if( i != numCurrAreas ) {
				if( auto *fallback = TryFindJumpFromLavaFallback( context ) ) {
					return fallback;
				}
			}
		}
	}

	// All the following checks require a valid nav target
	if( !context->NavTargetAasAreaNum() ) {
		if( bot->MillisInBlockedState() > 500 ) {
			if( auto *fallback = TryFindLostNavTargetFallback( context ) ) {
				return fallback;
			}
		}
		return nullptr;
	}

	// Check if the bot is standing on a ramp
	if( entityPhysicsState.GroundEntity() && entityPhysicsState.GetGroundNormalZ() < 0.999f ) {
		if( int groundedAreaNum = context->CurrGroundedAasAreaNum() ) {
			if( AiAasWorld::Instance()->AreaSettings()[groundedAreaNum].areaflags & AREA_INCLINED_FLOOR ) {
				if( auto *fallback = TryFindRampFallback( context, groundedAreaNum ) ) {
					return fallback;
				}
			}
		}
	}

	if( auto *fallback = TryFindAasBasedFallback( context ) ) {
		return fallback;
	}

	// Check for stairs
	if( auto *fallback = TryFindStairsFallback( context ) ) {
		return fallback;
	}

	// It is not unusual to see tiny ramp-like areas to the both sides of stairs.
	// Try using these ramp areas as directions for fallback movement.
	if( auto *fallback = TryFindNearbyRampAreasFallback( context ) ) {
		return fallback;
	}

	if( auto *fallback = TryFindWalkableTriggerFallback( context ) ) {
		return fallback;
	}

	if( auto *fallback = TryNodeBasedFallbacksLeft( context ) ) {
		// Check whether its really a node based fallback
		auto *const nodeBasedFallback = &module->useWalkableNodeScript;
		if( fallback == nodeBasedFallback ) {
			const vec3_t &origin = nodeBasedFallback->NodeOrigin();
			const int areaNum = nodeBasedFallback->NodeAreaNum();
			if( auto *jumpFallback = TryShortcutOtherFallbackByJumping( context, origin, areaNum ) ) {
				return jumpFallback;
			}
		}
		return fallback;
	}

	if( auto *fallback = TryFindJumpAdvancingToTargetFallback( context ) ) {
		return fallback;
	}

	return nullptr;
}

MovementScript *FallbackMovementAction::TryNodeBasedFallbacksLeft( Context *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	const unsigned millisInBlockedState = bot->MillisInBlockedState();
	if( millisInBlockedState < 500 ) {
		return nullptr;
	}

	// Try using the nav target as a fallback movement target
	Assert( context->NavTargetAasAreaNum() );
	auto *nodeFallback = &module->useWalkableNodeScript;
	if( context->NavTargetOrigin().SquareDistanceTo( entityPhysicsState.Origin() ) < SQUARE( 384.0f ) ) {
		Vec3 target( context->NavTargetOrigin() );
		target.Z() += -playerbox_stand_mins[2];
		nodeFallback->Activate( target.Data(), 32.0f, context->NavTargetAasAreaNum() );
		nodeFallback->TryDeactivate( context );
		if( nodeFallback->IsActive() ) {
			return nodeFallback;
		}
	}

	if( millisInBlockedState < 750 ) {
		return nullptr;
	}

	vec3_t areaPoint;
	int areaNum;
	if( context->sameFloorClusterAreasCache.GetClosestToTargetPoint( context, areaPoint, &areaNum ) ) {
		nodeFallback->Activate( areaPoint, 48.0f, areaNum );
		return nodeFallback;
	}

	if( millisInBlockedState > 1500 ) {
		// Notify the nav target selection code
		bot->OnMovementToNavTargetBlocked();
	}

	return nullptr;
}

MovementScript *FallbackMovementAction::TryFindAasBasedFallback( Context *context ) {
	const int nextReachNum = context->NextReachNum();
	if( !nextReachNum ) {
		return nullptr;
	}

	const auto &nextReach = AiAasWorld::Instance()->Reachabilities()[nextReachNum];
	const int traveltype = nextReach.traveltype & TRAVELTYPE_MASK;

	if( traveltype == TRAVEL_WALK ) {
		return TryFindWalkReachFallback( context, nextReach );
	}

	if( traveltype == TRAVEL_JUMPPAD || traveltype == TRAVEL_TELEPORT || traveltype == TRAVEL_ELEVATOR ) {
		// Always follow these reachabilities
		auto *fallback = &module->useWalkableNodeScript;
		// Note: We have to add several units to the target Z, otherwise a collision test
		// on next frame is very likely to immediately deactivate it
		fallback->Activate( ( Vec3( 0, 0, -playerbox_stand_mins[2] ) + nextReach.start ).Data(), 16.0f );
		return fallback;
	}

	if( traveltype == TRAVEL_WALKOFFLEDGE ) {
		return TryFindWalkOffLedgeReachFallback( context, nextReach );
	}

	if( traveltype == TRAVEL_JUMP || traveltype == TRAVEL_STRAFEJUMP ) {
		// This means we try jumping directly to the reach. target the current position
		if( auto *script = TryFindJumpLikeReachFallback( context, nextReach ) ) {
			return script;
		}
		// Try walking to the reach start otherwise
		return TryFindWalkReachFallback( context, nextReach );
	}

	// The only possible fallback left
	auto *fallback = &module->jumpOverBarrierScript;
	if( traveltype == TRAVEL_BARRIERJUMP || traveltype == TRAVEL_WATERJUMP ) {
		fallback->Activate( nextReach.start, nextReach.end );
		return fallback;
	}

	// Disallow WJ attempts for TRAVEL_DOUBLEJUMP reachabilities
	if( traveltype == TRAVEL_DOUBLEJUMP ) {
		fallback->Activate( nextReach.start, nextReach.end, false );
		return fallback;
	}

	return nullptr;
}*/


