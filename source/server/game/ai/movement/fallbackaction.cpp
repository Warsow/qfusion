#include "fallbackaction.h"
#include "movementscript.h"
#include "movementlocal.h"
#include "bestjumpablespotdetector.h"
#include "../combat/tacticalspotsregistry.h"
#include "../navigation/aasstaticroutetable.h"
#include "../manager.h"
#include "../ailocal.h"
#include "../trajectorypredictor.h"
#include "../classifiedentitiescache.h"

void FallbackAction::PlanPredictionStep( PredictionContext *context ) {
	bool handledSpecialMovement = false;
	if( auto *fallback = m_subsystem->activeMovementScript ) {
		fallback->SetupMovement( context );
		handledSpecialMovement = true;
	} else if( context->IsInNavTargetArea() ) {
		SetupNavTargetAreaMovement( context );
		handledSpecialMovement = true;
	}

	// If we have saved a path that is not perfect but good enough during attempts for bunny-hopping prediction
	if( !context->goodEnoughPath.empty() || !context->lastResortPath.empty() ) {
		const char *tag;
		PredictionContext::PredictedPath *path;
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
		botInput->SetAllowedRotationMask( InputRotation::NONE );
	} else {
		const auto &entityPhysicsState = context->movementState->entityPhysicsState;
		if( !entityPhysicsState.GroundEntity() && CanWaitForLanding( context ) ) {
			// Fallback path movement is the last hope action, wait for landing
			SetupLostNavTargetMovement( context );
		} else if( auto *fallback = TryFindMovementFallback( context ) ) {
			m_subsystem->activeMovementScript = fallback;
			fallback->SetupMovement( context );
			handledSpecialMovement = true;
			botInput->SetAllowedRotationMask( InputRotation::NONE );
		} else {
			// This often leads to bot blocking and suicide. TODO: Invesigate what else can be done.
			botInput->Clear();
			if( const std::optional<Vec3> &keptInFovPoint = bot->GetKeptInFovPoint() ) {
				Vec3 intendedLookVec( *keptInFovPoint );
				intendedLookVec -= entityPhysicsState.Origin();
				botInput->SetIntendedLookDir( intendedLookVec, false );
			} else {
				botInput->SetIntendedLookDir( entityPhysicsState.ForwardDir() );
			}
			botInput->canOverrideLookVec = true;
			botInput->canOverridePitch = true;
		}
	}

	botInput->isUcmdSet = true;
	Debug( "Planning is complete: the action should never be predicted ahead\n" );
	context->isCompleted = true;
}

bool FallbackAction::CanWaitForLanding( PredictionContext *context ) {
	const int navTargetAreaNum = context->NavTargetAasAreaNum();
	if( !navTargetAreaNum ) {
		return false;
	}

	// Switch to picking the target immediately
	if( context->IsInNavTargetArea() ) {
		return false;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const auto *const routeCache = bot->RouteCache();
	const auto *const routeTable = AasStaticRouteTable::instance();
	const int travelFlags = bot->TravelFlags();

	int fromAreaNums[2] { 0, 0 };
	const int numFromAreas = entityPhysicsState.PrepareRoutingStartAreas( fromAreaNums );
	const int startTravelTime = routeCache->FindRoute( fromAreaNums, numFromAreas, navTargetAreaNum, travelFlags );
	if( !startTravelTime ) {
		return false;
	}

	AiTrajectoryPredictor predictor;
	predictor.AddStopEventFlags( AiTrajectoryPredictor::HIT_SOLID );
	predictor.AddStopEventFlags( AiTrajectoryPredictor::HIT_ENTITY );
	predictor.AddStopEventFlags( AiTrajectoryPredictor::HIT_LIQUID );
	Vec3 mins( playerbox_stand_mins );
	mins.Z() += 12.0f;
	predictor.SetColliderBounds( mins.Data(), playerbox_stand_maxs );
	predictor.SetStepMillis( 67 );
	predictor.SetNumSteps( 16 );
	AiTrajectoryPredictor::Results results;
	predictor.SetEntitiesCollisionProps( true, bot->EntNum() );
	(void)predictor.Run( entityPhysicsState.Velocity(), entityPhysicsState.Origin(), &results );

	const auto *const aasWorld = AiAasWorld::instance();
	int resultAreaNum = aasWorld->findAreaNum( results.origin );
	if( !resultAreaNum ) {
		// WTF?
		results.origin[2] += 8.0f;
		resultAreaNum = aasWorld->findAreaNum( results.origin );
		if( !resultAreaNum ) {
			return false;
		}
	}

	// Nothing is going to be changed
	if( resultAreaNum == entityPhysicsState.CurrAasAreaNum() ) {
		return true;
	}

	// The nav target area is going to be reached
	if( resultAreaNum == navTargetAreaNum ) {
		return true;
	}

	// Lower restrictions for landing in the same floor cluster
	if( const auto resultFloorClusterNum = aasWorld->floorClusterNum( resultAreaNum ) ) {
		for( int i = 0; i < numFromAreas; ++i ) {
			if( fromAreaNums[i] && aasWorld->floorClusterNum( fromAreaNums[i] ) == resultFloorClusterNum ) {
				return true;
			}
		}
		if( aasWorld->floorClusterNum( navTargetAreaNum ) == resultFloorClusterNum ) {
			return true;
		}
	}

	const int endTravelTime = routeCache->FindRoute( resultAreaNum, navTargetAreaNum, travelFlags );
	if( !endTravelTime ) {
		return false;
	}

	// Consider an advancement to be a success
	if( startTravelTime > endTravelTime ) {
		return true;
	}

	// Permit having a slightly greater travel time if we can return quickly by walking in the worst case
	for( int i = 0; i < numFromAreas; ++i ) {
		// TODO: Check whether we can lift this test to the beginning of checks as it's quite cheap now
		const auto backTravelTime = routeTable->getTravelTimeWalkingOrFallingShort( resultAreaNum, fromAreaNums[i] );
		if( backTravelTime && *backTravelTime < 100 ) {
			return true;
		}
	}

	return false;
}

void FallbackAction::SetupNavTargetAreaMovement( PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	Vec3 intendedLookDir( context->NavTargetOrigin() );
	intendedLookDir -= entityPhysicsState.Origin();
	if( !intendedLookDir.normalizeFast() ) {
		return;
	}

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

	botInput->isUcmdSet = true;
	botInput->canOverrideUcmd = true;
	botInput->canOverrideLookVec = true;
}

void FallbackAction::SetupLostNavTargetMovement( PredictionContext *context ) {
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

MovementScript *FallbackAction::TryFindMovementFallback( PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	// First check for being in lava
	// TODO: Inspect why waterType does not work as intended
	if( entityPhysicsState.waterLevel >= 1 ) {
		const auto aasAreaSettings = AiAasWorld::instance()->getAreaSettings();
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
		if( bot->MillisInBlockedState() > 1250 ) {
			if( auto *fallback = TryFindLostNavTargetFallback( context ) ) {
				return fallback;
			}
		}
		return nullptr;
	}

	// Check if the bot is standing on a ramp
	if( entityPhysicsState.GroundEntity() && entityPhysicsState.GetGroundNormalZ() < 0.999f ) {
		if( int groundedAreaNum = context->CurrGroundedAasAreaNum() ) {
			if( AiAasWorld::instance()->getAreaSettings()[groundedAreaNum].areaflags & AREA_INCLINED_FLOOR ) {
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
		return fallback;
	}

	if( auto *fallback = TryFindJumpAdvancingToTargetFallback( context ) ) {
		return fallback;
	}

	return nullptr;
}

MovementScript *FallbackAction::TryNodeBasedFallbacksLeft( PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	const unsigned millisInBlockedState = bot->MillisInBlockedState();
	if( millisInBlockedState < 500 ) {
		return nullptr;
	}

	// Try using the nav target as a fallback movement target
	Assert( context->NavTargetAasAreaNum() );
	auto *nodeFallback = &m_subsystem->useWalkableNodeScript;
	if( context->NavTargetOrigin().SquareDistanceTo( entityPhysicsState.Origin() ) < wsw::square( 384.0f ) ) {
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

MovementScript *FallbackAction::TryFindAasBasedFallback( PredictionContext *context ) {
	const int nextReachNum = context->NextReachNum();
	if( !nextReachNum ) {
		return nullptr;
	}

	const auto &nextReach = AiAasWorld::instance()->getReaches()[nextReachNum];
	const int traveltype = nextReach.traveltype & TRAVELTYPE_MASK;

	if( traveltype == TRAVEL_WALK ) {
		return TryFindWalkReachFallback( context, nextReach );
	}

	if( traveltype == TRAVEL_JUMPPAD || traveltype == TRAVEL_TELEPORT || traveltype == TRAVEL_ELEVATOR ) {
		auto *fallback = &m_subsystem->useWalkableNodeScript;
		bool didPlatformSpecificHandling = false;
		if( traveltype == TRAVEL_ELEVATOR ) {
			const edict_t *foundPlatform = nullptr;
			// Q3 be_aas_optimize.c: "for TRAVEL_ELEVATOR the facenum is the model number of elevator"
			for( const auto triggerEntNum: wsw::ai::ClassifiedEntitiesCache::instance()->getAllPersistentMapPlatformTriggers() ) {
				const edict_t *trigger = game.edicts + triggerEntNum;
				const edict_t *platform = trigger->enemy;
				assert( platform && platform->use == Use_Plat );
				if( platform->s.modelindex == (unsigned)nextReach.facenum ) {
					foundPlatform = platform;
					break;
				}
			}
			if( foundPlatform && ( foundPlatform->moveinfo.state == STATE_TOP || foundPlatform->moveinfo.state == STATE_BOTTOM ) ) {
				// Figure out what platform state (TOP or BOTTOM) is closer to the reach start
				const float startDeltaZ = std::fabs( foundPlatform->moveinfo.start_origin[2] - nextReach.start[2] );
				const float endDeltaZ   = std::fabs( foundPlatform->moveinfo.end_origin[2] - nextReach.start[2] );
				if( startDeltaZ < endDeltaZ ) {
					if( foundPlatform->moveinfo.state == STATE_TOP ) {
						didPlatformSpecificHandling = true;
					}
				} else {
					if( foundPlatform->moveinfo.state == STATE_BOTTOM ) {
						didPlatformSpecificHandling = true;
					}
				}
				if( didPlatformSpecificHandling ) {
					Vec3 targetOrigin( getTriggerOrigin( foundPlatform ) );
					targetOrigin.Z() = nextReach.start[2] - playerbox_stand_mins[2];
					fallback->Activate( targetOrigin.Data(), 16.0f );
				}
			}
		}

		if( !didPlatformSpecificHandling ) {
			Vec3 targetOrigin( nextReach.start );
			// Note: We have to add several units to the target Z, otherwise a collision test
			// on next frame is very likely to immediately deactivate it
			targetOrigin.Z() -= playerbox_stand_mins[2];
			fallback->Activate( targetOrigin.Data(), 16.0f );
		}

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
	auto *fallback = &m_subsystem->jumpOverBarrierScript;
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
}


