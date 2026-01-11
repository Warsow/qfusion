#include "../bot.h"
#include "movementsubsystem.h"
#include "movementlocal.h"
#include "triggeraaspropscache.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/scopeexitaction.h>

MovementSubsystem::MovementSubsystem( Bot *bot_ )
	: bot( bot_ )
	, weaponJumpAttemptsRateLimiter( 2 )
	, sameFloorClusterAreasCache( bot_ )
	, nextFloorClusterAreasCache( bot_ ) {
	//movementState.Reset();
}

bool MovementSubsystem::CanChangeWeapons() const {
	/*
	auto &weaponJumpState = movementState.weaponJumpMovementState;
	if( weaponJumpState.IsActive() ) {
		return weaponJumpState.hasTriggeredWeaponJump;
	}*/
	const int64_t levelTime = level.time;
	// If there were no recent failed weapon jump attempts
	if( levelTime - lastWeaponJumpTriggeringFailedAt > 512 ) {
		return true;
	}
	// Hack... make a copy of the rate limiter (it's cheap) to avoid modifying its state
	RateLimiter limiter( this->weaponJumpAttemptsRateLimiter );
	// Check whether the rate limiter would allow next weapon jumping attempt soon and disable switching in this case
	return !limiter.TryAcquire( levelTime + 384 );
}

auto MovementSubsystem::findTriggerReachNumForScriptActivation( int entNum, int desiredTravelType,
																const CachedLastNearbyTriggerReach &cached ) -> int {
	if( cached.entNum == entNum && cached.reachNum > 0 && ( level.time - cached.touchedAt ) < 1000 ) {
		return cached.reachNum;
	}
	// Try inspecting the direct reachability from the current area
	// TODO: Should we update the cache?
	return findNextReachNumForTravelType( desiredTravelType, 1 );
}

// Note: We don't put a distance limit because the limit on the distance to a reachability start
// works unexpectedly for very large triggers (you're about to touch it and still are far from the reach start point).
// The limit of hops is theoretically prone to a similar issue, but we have opted to use a sufficiently large value.
auto MovementSubsystem::findNextReachNumForTravelType( int desiredTravelType, int hopLimit ) -> int {
	assert( hopLimit >= 1 );
	if( const int navTargetAreaNum = bot->NavTargetAasAreaNum() ) {
		int startAreaNums[2] { 0, 0 };
		const int numStartAreas = bot->EntityPhysicsState()->PrepareRoutingStartAreas( startAreaNums );
		for( int startAreaIndex = 0; startAreaIndex < numStartAreas; ++startAreaIndex ) {
			if( startAreaIndex == navTargetAreaNum ) {
				return 0;
			}
		}

		int desiredReachNum = 0;
		int genericReachNum = 0;

		int bestDesiredReachTravelTime = std::numeric_limits<int>::max();
		int bestGenericTravelTime      = std::numeric_limits<int>::max();

		const auto aasReaches  = AiAasWorld::instance()->getReaches();
		const auto *routeCache = bot->RouteCache();
		const auto travelFlags = bot->TravelFlags();

		// Note: We loop over areas instead of passing span of areas to the routing call
		// as we're specifically interested in desiredTravelType reachabilities
		for( int startAreaIndex = 0; startAreaIndex < numStartAreas; ++startAreaIndex ) {
			int reachNum = 0;
			if( const int travelTime = routeCache->FindRoute( startAreaNums[startAreaIndex], navTargetAreaNum,
															  travelFlags, &reachNum ) ) {
				if( aasReaches[reachNum].traveltype == desiredTravelType ) {
					if( bestDesiredReachTravelTime > travelTime ) {
						bestDesiredReachTravelTime = travelTime;
						desiredReachNum            = reachNum;
					}
				} else {
					if( bestGenericTravelTime > travelTime ) {
						bestGenericTravelTime = travelTime;
						genericReachNum       = reachNum;
					}
				}
			}
		}
		if( desiredReachNum ) {
			return desiredReachNum;
		}
		if( genericReachNum ) {
			int fromAreaNum = aasReaches[genericReachNum].areanum;
			for( int hopNum = 1; hopNum < hopLimit; ++hopNum ) {
				// We've reached the nav target without using a reachability of the desired travel type
				if( fromAreaNum == navTargetAreaNum ) {
					return 0;
				}
				int reachNum = 0;
				if( routeCache->FindRoute( fromAreaNum, navTargetAreaNum, travelFlags, &reachNum ) ) {
					const auto &reach = aasReaches[reachNum];
					if( reach.traveltype == desiredTravelType ) {
						return reachNum;
					}
					fromAreaNum = reach.areanum;
				} else {
					return 0;
				}
			}
		}
	}
	return 0;
}

void MovementSubsystem::ActivateJumppadState( const edict_t *jumppadEnt ) {
	const int entNum   = ENTNUM( jumppadEnt );
	const int reachNum = findTriggerReachNumForScriptActivation( entNum, TRAVEL_JUMPPAD, lastNearbyJumppadReach );
	jumppadScript.setTarget( entNum, reachNum );
	setActiveScript( &jumppadScript );
}

void MovementSubsystem::ActivateElevatorState( const edict_t *triggerEnt ) {
	if( activeScript != &elevatorScript ) {
		const int entNum   = ENTNUM( triggerEnt );
		const int reachNum = findTriggerReachNumForScriptActivation( entNum, TRAVEL_ELEVATOR, lastNearbyElevatorReach );
		elevatorScript.setTarget( entNum, reachNum );
		setActiveScript( &elevatorScript );
	}
}

bool MovementSubsystem::CanInterruptMovement() const {
	if( activeScript == &jumppadScript || activeScript == &elevatorScript ) {
		return false;
	}
	/*
	if( movementState.jumppadMovementState.IsActive() ) {
		return false;
	}
	if( movementState.flyUntilLandingMovementState.IsActive() ) {
		return false;
	}
	if( movementState.weaponJumpMovementState.IsActive() ) {
		return false;
	}*/

	const edict_t *self = game.edicts + bot->EntNum();
	// False if the bot is standing on a platform and it has not switched to the TOP state
	return !( self->groundentity && self->groundentity->use == Use_Plat && self->groundentity->moveinfo.state != STATE_TOP );
}


void MovementSubsystem::Frame( BotInput *input ) {
	if( const int reachNum = findNextReachNumForTravelType( TRAVEL_JUMPPAD, 24 ) ) {
		lastNearbyJumppadReach.reachNum  = reachNum;
		lastNearbyJumppadReach.touchedAt = level.time;
		lastNearbyJumppadReach.entNum    = ::triggerAasPropsCache.getTriggerEntNumForJumppadReach( reachNum ).value_or( 0 );
	}
	if( const int reachNum = findNextReachNumForTravelType( TRAVEL_ELEVATOR, 24 ) ) {
		lastNearbyElevatorReach.reachNum  = reachNum;
		lastNearbyElevatorReach.touchedAt = level.time;
		lastNearbyElevatorReach.entNum    = ::triggerAasPropsCache.getTriggerEntNumForElevatorReach( reachNum ).value_or( 0 );
	}

	if( activeScript ) {
		if( activeScript->getTimeoutAt() <= level.time ) {
			setActiveScript( nullptr );
		}
	}

	if( const auto *groundEntity = movementState.entityPhysicsState.GroundEntity() ) {
		if( groundEntity->use == Use_Plat ) {
			if( activeScript != &elevatorScript ) {
				setActiveScript( &elevatorScript );
				const int entNum   = ENTNUM( groundEntity->enemy );
				const int reachNum = findTriggerReachNumForScriptActivation( entNum, TRAVEL_ELEVATOR, lastNearbyElevatorReach );
				elevatorScript.setTarget( entNum, reachNum );
			}
		}
	}

	if( activeScript ) {
		if( !produceBotInput( activeScript, input ) ) {
			setActiveScript( nullptr );
		}
	}

	const bool shouldSelectNewScript = !activeScript;
	if( shouldSelectNewScript ) {
		if( bot->Skill() >= 0.33f ) {
			if( produceBotInput( &bunnyHopScript, input ) ) {
				setActiveScript( &bunnyHopScript );
			}
		}
		if( !activeScript ) {
			// TODO: Check for immediate recovery in case of a dangerous failure
			if( movementState.entityPhysicsState.GroundEntity() ) {
				setActiveScript( findFallbackScript( input ) );
			} else {
				int beaconColor = 0;
				// Other scripts should recover on their own (should be sufficiently robust)
				if( prevActiveScript == &walkToPointScript || prevActiveScript == &bunnyHopScript ) {
					if( produceBotInput( &waitForLandingRelaxedScript, input ) ) {
						beaconColor = COLOR_RGB( 0, 192, 0 );
						setActiveScript( &waitForLandingRelaxedScript );
					} else if( produceBotInput( &landToPreventFallingScript, input ) ) {
						beaconColor = COLOR_RGB( 0, 0, 255 );
						setActiveScript( &landToPreventFallingScript );
					}
				}
				if( !activeScript ) {
					beaconColor = COLOR_RGB( 192, 0, 0 );
					input->SetIntendedLookDir( movementState.entityPhysicsState.ForwardDir() );
					input->ClearMovementDirections();
					input->isUcmdSet          = true;
					input->canOverrideLookVec = true;
					input->canOverridePitch   = true;
				}
				if( beaconColor ) {
					Vec3 v1( movementState.entityPhysicsState.Origin() );
					Vec3 v2( Vec3( 0, 0, 72 ) + v1 );
					AITools_DrawColorLine( v1.data(), v2.data(), beaconColor, 0 );
				}
			}
		}
	}

	if( shouldSelectNewScript && activeScript ) {
		// Make sure the script bumps its own timeout properly
		assert( activeScript->getTimeoutAt() > level.time );
	}
}

bool MovementSubsystem::produceBotInput( MovementScript *script, BotInput *input ) {
	testedScript = script;
	bool result  = script->produceBotInput( input );
	testedScript = nullptr;
	return result;
}

void MovementSubsystem::setActiveScript( MovementScript *script ) {
	if( activeScript ) {
		prevActiveScript = activeScript;
	}
	activeScript = script;
}

auto MovementSubsystem::findFallbackScript( BotInput *input ) -> MovementScript * {
	if( const int navTargetAreaNum = bot->NavTargetAasAreaNum() ) {
		const auto *const aasWorld = AiAasWorld::instance();

		// Make sure that there's no areas below
		assert( movementState.entityPhysicsState.GroundEntity() );

		// We have to retrieve all areas in the box of the bot.
		// It's the correct approach for navigation.
		// Currently, it is not performed during prediction for performance reasons.

		int botAreas[32];
		int numBotAreas = (int)aasWorld->findAreasInBox( bot->self->r.absmin, bot->self->r.absmax,
														 botAreas, sizeof( botAreas ) ).size();

		// TODO: Is it needed?
		for( const int areaNum: { movementState.entityPhysicsState.CurrAasAreaNum(),
								  movementState.entityPhysicsState.DroppedToFloorAasAreaNum() } ) {
			if( areaNum && numBotAreas < (int)std::size( botAreas ) ) {
				if( !wsw::contains( botAreas, botAreas + numBotAreas, areaNum ) ) {
					botAreas[numBotAreas++] = areaNum;
				}
			}
		}

		if( wsw::contains( botAreas, botAreas + numBotAreas, navTargetAreaNum ) ) {
			walkToPointScript.setTargetPoint( bot->NavTargetOrigin() );
			if( produceBotInput( &walkToPointScript, input ) ) {
				return &walkToPointScript;
			}
		} else {
			int reachNum = 0;
			if( bot->RouteCache()->FindRoute( botAreas, numBotAreas, navTargetAreaNum, bot->TravelFlags(), &reachNum ) ) {
				const auto &reach = aasWorld->getReaches()[reachNum];
				if( reach.traveltype == TRAVEL_WALK || reach.traveltype == TRAVEL_JUMPPAD ||
					reach.traveltype == TRAVEL_TELEPORT || reach.traveltype == TRAVEL_ELEVATOR ) {
					walkToPointScript.setTargetPoint( Vec3( reach.start ) );
					if( produceBotInput( &walkToPointScript, input ) ) {
						return &walkToPointScript;
					}
				} else if( reach.traveltype == TRAVEL_WALKOFFLEDGE ) {
					if( DistanceSquared( reach.start, reach.end ) <= wsw::square( AI_JUMPABLE_HEIGHT ) ) {
						walkToPointScript.setTargetPoint( Vec3( reach.end ) );
						if( produceBotInput( &walkToPointScript, input ) ) {
							return &walkToPointScript;
						}
					}
					traverseWalkOffLedgeReachScript.setTargetReachNum( reachNum );
					if( produceBotInput( &traverseWalkOffLedgeReachScript, input ) ) {
						return &traverseWalkOffLedgeReachScript;
					}
				} else if( reach.traveltype == TRAVEL_JUMP || reach.traveltype == TRAVEL_STRAFEJUMP ) {
					traverseJumpReachScript.setTargetReachNum( reachNum );
					if( produceBotInput( &traverseJumpReachScript, input ) ) {
						return &traverseJumpReachScript;
					}
				} else if( reach.traveltype == TRAVEL_BARRIERJUMP ) {
					traverseBarrierJumpReachScript.setTargetReachNum( reachNum );
					if( produceBotInput( &traverseBarrierJumpReachScript, input ) ) {
						return &traverseBarrierJumpReachScript;
					}
				}
			}
		}
	}

	return nullptr;
}

void MovementSubsystem::ApplyPendingTurnToLookAtPoint( BotInput *botInput, PredictionContext *context ) {
	if( pendingLookAtPointState.timeoutAt < level.time ) {
		return;
	}

	AiEntityPhysicsState *entityPhysicsState_;
	unsigned frameTime;
	if( context ) {
		entityPhysicsState_ = &context->movementState->entityPhysicsState;
		frameTime = context->predictionStepMillis;
	} else {
		entityPhysicsState_ = &movementState.entityPhysicsState;
		frameTime = game.frametime;
	}


	const AiPendingLookAtPoint &pendingLookAtPoint = pendingLookAtPointState.pendingLookAtPoint;
	Vec3 toPointDir( pendingLookAtPoint.Origin() );
	toPointDir -= entityPhysicsState_->Origin();
	if( !toPointDir.normalizeFast() ) {
		return;
	}

	botInput->SetIntendedLookDir( toPointDir, true );
	botInput->isLookDirSet = true;

	float turnSpeedMultiplier = pendingLookAtPoint.TurnSpeedMultiplier();
	Vec3 newAngles = bot->GetNewViewAngles( entityPhysicsState_->Angles().data(), toPointDir, frameTime, turnSpeedMultiplier );
	botInput->SetAlreadyComputedAngles( newAngles );

	botInput->canOverrideLookVec = false;
	botInput->canOverridePitch = false;
}

static const char *lastNoLookDirAction = "";
static const char *lastNoUcmdAction = "";

void MovementSubsystem::ApplyInput( BotInput *input, PredictionContext *context ) {
	// While these conditions can hold from time to time and that's not a bug
	// it's better to eliminate these cases eventually completely.
	if( !input->isLookDirSet ) {
		if( context ) {
			if( strcmp( lastNoLookDirAction, context->ActiveActionName() ) != 0 ) {
				aiWarning() << "FIXME " << wsw::StringView( context->ActiveActionName() ) << " No look dir set";
				lastNoLookDirAction = context->ActiveActionName();
			}
		} else {
			aiWarning() << "FIXME: No look dir set";
		}
		return;
	}
	if( !input->isUcmdSet ) {
		if( context ) {
			if( strcmp( lastNoUcmdAction, context->ActiveActionName() ) != 0 ) {
				aiWarning() << "FIXME" << wsw::StringView( context->ActiveActionName() ) << "No ucmd set";
				lastNoUcmdAction = context->ActiveActionName();
			}
		} else {
			aiWarning() << "FIXME: No ucmd set";
		}
		return;
	}

	if( context ) {
		auto *entityPhysicsState_ = &context->movementState->entityPhysicsState;
		if( !input->hasAlreadyComputedAngles ) {
			Vec3 newAngles( bot->GetNewViewAngles( entityPhysicsState_->Angles().data(), input->IntendedLookDir(),
												   context->predictionStepMillis, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		// There's no need to modify entityPhysicsState right now
		// as we are going to update entityPhysicsState by PMove() results.
		// TODO: Split the method for null context and non-null context
		// TODO: Inline this part into context->NextMovementStep()
	} else {
		edict_t *self = game.edicts + bot->EntNum();
		if( !input->hasAlreadyComputedAngles ) {
			Vec3 newAngles( bot->GetNewViewAngles( self->s.angles, input->IntendedLookDir(),
												   game.frametime, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		input->AlreadyComputedAngles().copyTo( self->s.angles );
	}
}

PredictionContext::PredictionContext( MovementSubsystem *subsystem, PredictedPath *predictedMovementActions_ )
	: bot( subsystem->bot )
	, m_subsystem( subsystem )
	, predictedMovementActions( predictedMovementActions_ )
	, movementState( nullptr )
	, record( nullptr )
	, totalMillisAhead( 0 )
	, predictionStepMillis( 0 )
	, oldStepMillis( 0 )
	, topOfStackIndex( 0 ) {}