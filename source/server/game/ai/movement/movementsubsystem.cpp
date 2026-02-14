#include "../bot.h"
#include "movementsubsystem.h"
#include "movementlocal.h"
#include "triggeraaspropscache.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/scopeexitaction.h>

MovementSubsystem::MovementSubsystem( Bot *bot_ )
	: m_bot( bot_ )
	, m_weaponJumpAttemptsRateLimiter( 2 )
	, m_sameFloorClusterAreasCache( bot_ )
	, m_nextFloorClusterAreasCache( bot_ ) {
	//movementState.Reset();
}

bool MovementSubsystem::canChangeWeapons() const {
	/*
	auto &weaponJumpState = movementState.weaponJumpMovementState;
	if( weaponJumpState.IsActive() ) {
		return weaponJumpState.hasTriggeredWeaponJump;
	}*/
	const int64_t levelTime = level.time;
	// If there were no recent failed weapon jump attempts
	if( levelTime - m_lastWeaponJumpTriggeringFailedAt > 512 ) {
		return true;
	}
	// Hack... make a copy of the rate limiter (it's cheap) to avoid modifying its state
	RateLimiter limiter( this->m_weaponJumpAttemptsRateLimiter );
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
	if( const int navTargetAreaNum = m_bot->NavTargetAasAreaNum() ) {
		int startAreaNums[2] { 0, 0 };
		const int numStartAreas = m_bot->EntityPhysicsState()->PrepareRoutingStartAreas( startAreaNums );
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
		const auto *routeCache = m_bot->RouteCache();
		const auto travelFlags = m_bot->TravelFlags();

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

void MovementSubsystem::activateJumppadState( const edict_t *jumppadEnt ) {
	const int entNum   = ENTNUM( jumppadEnt );
	const int reachNum = findTriggerReachNumForScriptActivation( entNum, TRAVEL_JUMPPAD, m_lastNearbyJumppadReach );
	m_jumppadScript.setTarget( entNum, reachNum );
	setActiveScript( &m_jumppadScript );
}

void MovementSubsystem::activateElevatorState( const edict_t *triggerEnt ) {
	if( m_activeScript != &m_elevatorScript ) {
		const int entNum   = ENTNUM( triggerEnt );
		const int reachNum = findTriggerReachNumForScriptActivation( entNum, TRAVEL_ELEVATOR, m_lastNearbyElevatorReach );
		m_elevatorScript.setTarget( entNum, reachNum );
		setActiveScript( &m_elevatorScript );
	}
}

bool MovementSubsystem::canInterruptMovement() const {
	if( m_activeScript == &m_jumppadScript || m_activeScript == &m_elevatorScript ) {
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

	const edict_t *self = game.edicts + m_bot->EntNum();
	// False if the bot is standing on a platform and it has not switched to the TOP state
	return !( self->groundentity && self->groundentity->use == Use_Plat && self->groundentity->moveinfo.state != STATE_TOP );
}


void MovementSubsystem::frame( BotInput *input ) {
	if( const int reachNum = findNextReachNumForTravelType( TRAVEL_JUMPPAD, 24 ) ) {
		m_lastNearbyJumppadReach.reachNum  = reachNum;
		m_lastNearbyJumppadReach.touchedAt = level.time;
		m_lastNearbyJumppadReach.entNum    = ::triggerAasPropsCache.getTriggerEntNumForJumppadReach( reachNum ).value_or( 0 );
	}
	if( const int reachNum = findNextReachNumForTravelType( TRAVEL_ELEVATOR, 24 ) ) {
		m_lastNearbyElevatorReach.reachNum  = reachNum;
		m_lastNearbyElevatorReach.touchedAt = level.time;
		m_lastNearbyElevatorReach.entNum    = ::triggerAasPropsCache.getTriggerEntNumForElevatorReach( reachNum ).value_or( 0 );
	}

	const bool hadActiveScript = m_activeScript != nullptr;
	if( m_activeScript ) {
		if( m_activeScript->getTimeoutAt() <= level.time ) {
			setActiveScript( nullptr );
		}
	}

	if( const auto *groundEntity = m_movementState.entityPhysicsState.GroundEntity() ) {
		if( groundEntity->use == Use_Plat ) {
			if( m_activeScript != &m_elevatorScript ) {
				setActiveScript( &m_elevatorScript );
				const int entNum   = ENTNUM( groundEntity->enemy );
				const int reachNum = findTriggerReachNumForScriptActivation( entNum, TRAVEL_ELEVATOR, m_lastNearbyElevatorReach );
				m_elevatorScript.setTarget( entNum, reachNum );
			}
		}
	}

	if( m_activeScript ) {
		if( !produceBotInput( m_activeScript, input ) ) {
			setActiveScript( nullptr );
		}
	}

	const bool shouldSelectNewScript = !m_activeScript;
	if( shouldSelectNewScript ) {
		if( m_bot->Skill() >= 0.33f ) {
			if( produceBotInput( &m_bunnyHopScript, input ) ) {
				setActiveScript( &m_bunnyHopScript );
			}
		}
		if( !m_activeScript ) {
			// TODO: Check for immediate recovery in case of a dangerous failure
			if( m_movementState.entityPhysicsState.GroundEntity() ) {
				setActiveScript( findFallbackScript( input ) );
				if( !m_activeScript ) {
					if( hadActiveScript ) {
						m_noScriptOnGroundSinceLevelTime     = level.time;
						m_noScriptOnGroundSinceLevelFramenum = level.framenum;
					} else {
						setActiveScript( findLastResortGroundScript( input ) );
					}
				}
			} else {
				int beaconColor = 0;
				// Other scripts should recover on their own (should be sufficiently robust)
				if( m_prevActiveScript == &m_walkToPointScript || m_prevActiveScript == &m_bunnyHopScript ) {
					if( produceBotInput( &m_waitForLandingRelaxedScript, input ) ) {
						beaconColor = COLOR_RGB( 0, 192, 0 );
						setActiveScript( &m_waitForLandingRelaxedScript );
					} else if( produceBotInput( &m_landToPreventFallingScript, input ) ) {
						beaconColor = COLOR_RGB( 0, 0, 255 );
						setActiveScript( &m_landToPreventFallingScript );
					}
				}
				if( !m_activeScript ) {
					beaconColor = COLOR_RGB( 192, 0, 0 );
					input->SetIntendedLookDir( m_movementState.entityPhysicsState.ForwardDir() );
					input->ClearMovementDirections();
					input->isUcmdSet          = true;
					input->canOverrideLookVec = true;
					input->canOverridePitch   = true;
				}
				if( beaconColor ) {
					Vec3 v1( m_movementState.entityPhysicsState.Origin() );
					Vec3 v2( Vec3( 0, 0, 72 ) + v1 );
					AITools_DrawColorLine( v1.data(), v2.data(), beaconColor, 0 );
				}
			}
		}
	}

	if( shouldSelectNewScript && m_activeScript ) {
		// Make sure the script bumps its own timeout properly
		assert( m_activeScript->getTimeoutAt() > level.time );
	}

	if( m_activeScript && m_movementState.entityPhysicsState.GroundEntity() ) {
		m_noScriptOnGroundSinceLevelTime     = level.time + 1;
		m_noScriptOnGroundSinceLevelFramenum = level.framenum + 1;
	}
}

bool MovementSubsystem::produceBotInput( MovementScript *script, BotInput *input ) {
	m_testedScript = script;
	bool result    = script->produceBotInput( input );
	m_testedScript = nullptr;
	return result;
}

void MovementSubsystem::setActiveScript( MovementScript *script ) {
	if( m_activeScript ) {
		m_prevActiveScript = m_activeScript;
	}
	m_activeScript = script;
}

auto MovementSubsystem::findFallbackScript( BotInput *input ) -> MovementScript * {
	if( const int navTargetAreaNum = m_bot->NavTargetAasAreaNum() ) {
		const auto *const aasWorld     = AiAasWorld::instance();
		const auto &entityPhysicsState = m_movementState.entityPhysicsState;

		// Make sure that there's no areas below
		assert( entityPhysicsState.GroundEntity() );

		// We have to retrieve all areas in the box of the bot.
		// It's the correct approach for navigation.
		// Currently, it is not performed during prediction for performance reasons.

		int botAreas[32];
		int numBotAreas = (int)aasWorld->findAreasInBox( m_bot->self->r.absmin, m_bot->self->r.absmax,
														 botAreas, std::size( botAreas ) ).size();

		// TODO: Is this needed?
		for( const int areaNum: { entityPhysicsState.CurrAasAreaNum(),
								  entityPhysicsState.DroppedToFloorAasAreaNum() } ) {
			if( areaNum && numBotAreas < (int)std::size( botAreas ) ) {
				if( !wsw::contains( botAreas, botAreas + numBotAreas, areaNum ) ) {
					botAreas[numBotAreas++] = areaNum;
				}
			}
		}

		if( wsw::contains( botAreas, botAreas + numBotAreas, navTargetAreaNum ) ) {
			m_walkToPointScript.setTargetPoint( m_bot->NavTargetOrigin() );
			if( produceBotInput( &m_walkToPointScript, input ) ) {
				return &m_walkToPointScript;
			}
			m_jumpToPointScript.setTargetPoint( m_bot->NavTargetOrigin() );
			if( produceBotInput( &m_jumpToPointScript, input ) ) {
				return &m_jumpToPointScript;
			}
		} else {
			// TODO: It is not necessarily the grounded area num
			const int currGroundedAreaNum = entityPhysicsState.CurrAasAreaNum();
			// TODO: Test all bot areas?
			if( entityPhysicsState.GetGroundNormalZ() < 0.99f ) {
				// TODO: Check AREA_GROUNDED (is it needed?)
				if( aasWorld->getAreaSettings()[currGroundedAreaNum].areaflags & AREA_INCLINED_FLOOR ) {
					if( const auto maybeReachNumAndTravelTime = findRampClusterExitReachNumAndTravelTime( entityPhysicsState, m_bot ) ) {
						const auto &reach    = aasWorld->getReaches()[maybeReachNumAndTravelTime->first];
						const auto &exitArea = aasWorld->getAreas()[reach.areanum];
						wsw::StaticVector<Vec3, 2> testedPoints;
						testedPoints.push_back( Vec3( exitArea.center[0], exitArea.center[1], exitArea.mins[2] + 32.0f ) );
						testedPoints.push_back( Vec3( reach.end ) );
						// TODO: Allow testing multiple points at once
						for( const Vec3 &testedPoint: testedPoints ) {
							m_walkToPointScript.setTargetPoint( testedPoint, maybeReachNumAndTravelTime->second );
							if( produceBotInput( &m_walkToPointScript, input ) ) {
								return &m_walkToPointScript;
							}
						}
						// TODO: Do as a last resort when all other (regular reach traversal) scripts fail?
						for( const Vec3 &testedPoint: testedPoints ) {
							m_jumpToPointScript.setTargetPoint( testedPoint, reach.areanum );
							if( produceBotInput( &m_jumpToPointScript, input ) ) {
								return &m_jumpToPointScript;
							}
						}
					}
				}
			}
			if( const int stairsClusterNum = aasWorld->stairsClusterNum( currGroundedAreaNum ) ) {
				int exitStairsAreaNum = 0, exitReachNum = 0, exitTravelTime = 0;
				if( findBestStairsExitProps( entityPhysicsState, stairsClusterNum, m_bot,
											 &exitStairsAreaNum, &exitReachNum, &exitTravelTime ) ) {
					wsw::StaticVector<Vec3, 2> testedPoints;
					// If it does not match the nav target area
					if( exitReachNum ) {
						const auto &area = aasWorld->getAreas()[aasWorld->getReaches()[exitReachNum].areanum];
						testedPoints.push_back( Vec3( area.center[0], area.center[1], area.mins[2] + 32.0f ) );
					}
					const auto &area = aasWorld->getAreas()[exitStairsAreaNum];
					testedPoints.push_back( Vec3( area.center[0], area.center[1], area.mins[2] + 32.0f ) );
					for( const Vec3 &testedPoint: testedPoints ) {
						// TODO: Allow testing multiple points at once
						m_walkToPointScript.setTargetPoint( testedPoint, exitTravelTime );
						if( produceBotInput( &m_walkToPointScript, input ) ) {
							return &m_walkToPointScript;
						}
					}
					// TODO: Do as a last resort when all other (regular reach traversal) scripts fail?
					for( const Vec3 &testedPoint: testedPoints ) {
						m_jumpToPointScript.setTargetPoint( testedPoint, exitStairsAreaNum );
						if( produceBotInput( &m_jumpToPointScript, input ) ) {
							return &m_jumpToPointScript;
						}
					}
				}
			}

			int reachNum           = 0;
			const auto *routeCache = m_bot->RouteCache();
			const auto travelFlags = m_bot->TravelFlags();
			if( routeCache->FindRoute( botAreas, numBotAreas, navTargetAreaNum, travelFlags, &reachNum ) ) {
				const auto &reach = aasWorld->getReaches()[reachNum];
				if( reach.traveltype == TRAVEL_WALK ) {
					int travelTimeFromPoint = 0;
					// Note: We don't care of travel time inside area (that's what the current routing system does)
					// Note: We don't really need to check this condition but it slightly reduces CPU load in target areas
					if( reach.areanum != navTargetAreaNum ) {
						travelTimeFromPoint = routeCache->FindRoute( reach.areanum, navTargetAreaNum, travelFlags );
					}
					const auto &nextArea = aasWorld->getAreas()[reach.areanum];
					Vec3 nextAreaPoint( nextArea.center[0], nextArea.center[1], nextArea.mins[2] + 32.0f );
					// Note: The travel time value is equal for the area center and the reach end, as pointed above
					m_walkToPointScript.setTargetPoint( nextAreaPoint, travelTimeFromPoint );
					if( produceBotInput( &m_walkToPointScript, input ) ) {
						return &m_walkToPointScript;
					}
					// TODO: Allow testing multiple points at once
					m_walkToPointScript.setTargetPoint( Vec3( reach.end ), travelTimeFromPoint );
					if( produceBotInput( &m_walkToPointScript, input ) ) {
						return &m_walkToPointScript;
					}
					// Switch to jumping as a last resort
					m_jumpToPointScript.setTargetPoint( Vec3( reach.end ) );
					if( produceBotInput( &m_jumpToPointScript, input ) ) {
						return &m_jumpToPointScript;
					}
				} else if( reach.traveltype == TRAVEL_JUMPPAD ||
					reach.traveltype == TRAVEL_TELEPORT || reach.traveltype == TRAVEL_ELEVATOR ) {
					m_walkToPointScript.setTargetPoint( Vec3( reach.start ) );
					if( produceBotInput( &m_walkToPointScript, input ) ) {
						return &m_walkToPointScript;
					}
				} else if( reach.traveltype == TRAVEL_WALKOFFLEDGE ) {
					if( DistanceSquared( reach.start, reach.end ) <= wsw::square( AI_JUMPABLE_HEIGHT ) ) {
						m_walkToPointScript.setTargetPoint( Vec3( reach.end ) );
						if( produceBotInput( &m_walkToPointScript, input ) ) {
							return &m_walkToPointScript;
						}
					}
					m_traverseWalkOffLedgeReachScript.setTargetReachNum( reachNum );
					if( produceBotInput( &m_traverseWalkOffLedgeReachScript, input ) ) {
						return &m_traverseWalkOffLedgeReachScript;
					}
				} else if( reach.traveltype == TRAVEL_JUMP || reach.traveltype == TRAVEL_STRAFEJUMP ) {
					m_traverseJumpReachScript.setTargetReachNum( reachNum );
					if( produceBotInput( &m_traverseJumpReachScript, input ) ) {
						return &m_traverseJumpReachScript;
					}
				} else if( reach.traveltype == TRAVEL_BARRIERJUMP ) {
					m_traverseBarrierJumpReachScript.setTargetReachNum( reachNum );
					if( produceBotInput( &m_traverseBarrierJumpReachScript, input ) ) {
						return &m_traverseBarrierJumpReachScript;
					}
				}
			}
		}
	}

	return nullptr;
}

auto MovementSubsystem::findLastResortGroundScript( BotInput *input ) -> MovementScript * {
	constexpr auto minDelta = 500;
	constexpr auto maxDelta = ( 2 * Bot::BLOCKED_TIMEOUT ) / 3;
	static_assert( minDelta < maxDelta );
	// If we don't manage to unblock till we reach maxDelta, just let the bot respawn
	if( const auto delta = level.time - m_noScriptOnGroundSinceLevelTime; delta > minDelta && delta < maxDelta ) {
		if( ( level.framenum % MAX_CLIENTS ) == ( m_bot->EntNum() % MAX_CLIENTS ) ) {
			m_singleFrameSideStepScript.setAttemptOffset( m_noScriptOnGroundSinceLevelFramenum + delta );
			(void)produceBotInput( &m_singleFrameSideStepScript, input );
			// Intentionally non returning anything (don't let the single-frame script become active)
		}
	}
	return nullptr;
}

void MovementSubsystem::applyPendingTurnToLookAtPoint( BotInput *botInput, PredictionContext *context ) {
	if( m_pendingLookAtPointState.timeoutAt < level.time ) {
		return;
	}

	AiEntityPhysicsState *entityPhysicsState_;
	unsigned frameTime;
	if( context ) {
		entityPhysicsState_ = &context->movementState->entityPhysicsState;
		frameTime = context->predictionStepMillis;
	} else {
		entityPhysicsState_ = &m_movementState.entityPhysicsState;
		frameTime = game.frametime;
	}


	const AiPendingLookAtPoint &pendingLookAtPoint = m_pendingLookAtPointState.pendingLookAtPoint;
	Vec3 toPointDir( pendingLookAtPoint.Origin() );
	toPointDir -= entityPhysicsState_->Origin();
	if( !toPointDir.normalizeFast() ) {
		return;
	}

	botInput->SetIntendedLookDir( toPointDir, true );
	botInput->isLookDirSet = true;

	float turnSpeedMultiplier = pendingLookAtPoint.TurnSpeedMultiplier();
	Vec3 newAngles = m_bot->GetNewViewAngles( entityPhysicsState_->Angles().data(), toPointDir, frameTime, turnSpeedMultiplier );
	botInput->SetAlreadyComputedAngles( newAngles );

	botInput->canOverrideLookVec = false;
	botInput->canOverridePitch = false;
}

static const char *lastNoLookDirAction = "";
static const char *lastNoUcmdAction = "";

void MovementSubsystem::applyInput( BotInput *input, PredictionContext *context ) {
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
			Vec3 newAngles( m_bot->GetNewViewAngles( entityPhysicsState_->Angles().data(), input->IntendedLookDir(),
													 context->predictionStepMillis, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		// There's no need to modify entityPhysicsState right now
		// as we are going to update entityPhysicsState by PMove() results.
		// TODO: Split the method for null context and non-null context
		// TODO: Inline this part into context->NextMovementStep()
	} else {
		edict_t *self = game.edicts + m_bot->EntNum();
		if( !input->hasAlreadyComputedAngles ) {
			Vec3 newAngles( m_bot->GetNewViewAngles( self->s.angles, input->IntendedLookDir(),
													 game.frametime, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		input->AlreadyComputedAngles().copyTo( self->s.angles );
	}
}