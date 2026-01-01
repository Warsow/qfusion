#include "bunnyhopaction.h"
#include "movementlocal.h"

bool BunnyHopAction::GenericCheckIsActionEnabled( PredictionContext *context ) {
	if( !BaseAction::GenericCheckIsActionEnabled( context ) ) {
		return false;
	}

	// TODO: Get rid of disabledForApplicationFrameIndex
	if( this->disabledForApplicationFrameIndex != context->topOfStackIndex ) {
		return true;
	}

	assert( context->topOfStackIndex == 0 );
	Debug( "Cannot apply action: the action has been disabled for application on frame %d\n", context->topOfStackIndex );
	this->isDisabledForPlanning = true;
	return false;
}

bool BunnyHopAction::CheckCommonBunnyHopPreconditions( PredictionContext *context ) {
	int currAasAreaNum = context->CurrAasAreaNum();
	if( !currAasAreaNum ) {
		Debug( "Cannot apply action: curr AAS area num is undefined\n" );
		context->SetPendingRollback();
		return false;
	}

	int navTargetAasAreaNum = context->NavTargetAasAreaNum();
	if( !navTargetAasAreaNum ) {
		Debug( "Cannot apply action: nav target AAS area num is undefined\n" );
		context->SetPendingRollback();
		return false;
	}

	// Cannot find a next reachability in chain while it should exist
	// (looks like the bot is too high above the ground)
	if( !context->IsInNavTargetArea() && !context->NextReachNum() ) {
		Debug( "Cannot apply action: next reachability is undefined and bot is not in the nav target area\n" );
		// This might be another router woe as many rejected trajectories seem legit.
		// We have decided to save the trajectory if there was an advancement applying a huge penalty.
		if( minTravelTimeToNavTargetSoFar && minTravelTimeToNavTargetSoFar < travelTimeAtSequenceStart ) {
			context->SaveLastResortPath( sequencePathPenalty );
		}
		context->SetPendingRollback();
		return false;
	}

	if( !( context->currMinimalPlayerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_JUMP ) ) {
		Debug( "Cannot apply action: bot does not have the jump movement feature\n" );
		context->SetPendingRollback();
		this->isDisabledForPlanning = true;
		return false;
	}

	if( bot->ShouldBeSilent() ) {
		Debug( "Cannot apply action: bot should be silent\n" );
		context->SetPendingRollback();
		this->isDisabledForPlanning = true;
		return false;
	}

	return true;
}

void BunnyHopAction::SetupCommonBunnyHopInput( PredictionContext *context ) {
	const auto *pmoveStats = context->currMinimalPlayerState->pmove.stats;

	auto *botInput = &context->record->botInput;
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	botInput->SetForwardMovement( 1 );
	botInput->canOverrideLookVec = true;
	botInput->canOverridePitch   = true;

	if( ( pmoveStats[PM_STAT_FEATURES] & PMFEAT_DASH ) && !pmoveStats[PM_STAT_DASHTIME] ) {
		bool shouldDash = false;
		if( entityPhysicsState.Speed() < context->GetDashSpeed() && entityPhysicsState.GroundEntity() ) {
			// Prevent dashing into obstacles
			auto &traceCache = context->TraceCache();
			auto query( EnvironmentTraceCache::Query::front() );
			traceCache.testForQuery( context, query );
			if( traceCache.resultForQuery( query ).trace.fraction == 1.0f ) {
				shouldDash = true;
			}
		}

		if( shouldDash ) {
			botInput->SetSpecialButton( true );
			botInput->SetUpMovement( 0 );
			// Predict dash precisely
			context->predictionStepMillis = context->DefaultFrameTime();
		} else {
			botInput->SetUpMovement( 1 );
		}
	} else {
		if( entityPhysicsState.Speed() < context->GetRunSpeed() ) {
			botInput->SetUpMovement( 0 );
		} else {
			botInput->SetUpMovement( 1 );
		}
	}
}

bool BunnyHopAction::SetupBunnyHopping( const Vec3 &intendedLookVec, PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	Vec3 toTargetDir2D( intendedLookVec );
	botInput->SetIntendedLookDir( intendedLookVec );
	botInput->isUcmdSet = true;
	toTargetDir2D.Z() = 0;

	Vec3 velocityDir2D( entityPhysicsState.Velocity() );
	velocityDir2D.Z() = 0;

	float squareSpeed2D = entityPhysicsState.SquareSpeed2D();
	float toTargetDir2DSqLen = toTargetDir2D.SquaredLength();

	if( squareSpeed2D > 1.0f ) {
		SetupCommonBunnyHopInput( context );

		velocityDir2D *= 1.0f / entityPhysicsState.Speed2D();

		if( toTargetDir2DSqLen > 0.1f ) {
			toTargetDir2D *= Q_RSqrt( toTargetDir2DSqLen );
			float velocityDir2DDotToTargetDir2D = velocityDir2D.Dot( toTargetDir2D );
			if( velocityDir2DDotToTargetDir2D > 0.0f ) {
				// Apply a full acceleration at the initial trajectory part.
				// A reached dot threshold is the only extra condition.
				// The action activation rate is still relatively low
				// and the resulting velocity gain accumulated over real game frames is moderate.
				// Make sure we use the maximal acceleration possible for first frames
				// switching to the default fraction to simulate an actual resulting trajectory.
				if( velocityDir2DDotToTargetDir2D > 0.7f && context->totalMillisAhead <= 64 ) {
					context->CheatingAccelerate( 1.0f );
				} else {
					context->CheatingAccelerate( velocityDir2DDotToTargetDir2D );
				}
			}
			if( velocityDir2DDotToTargetDir2D < STRAIGHT_MOVEMENT_DOT_THRESHOLD ) {
				// Apply a path penalty for aircontrol abuse
				if( velocityDir2DDotToTargetDir2D < 0 ) {
					EnsurePathPenalty( 1000 );
				}
				context->CheatingCorrectVelocity( velocityDir2DDotToTargetDir2D, toTargetDir2D );
			}
		}
	}
	// Looks like the bot is in air falling vertically
	else if( !entityPhysicsState.GroundEntity() ) {
		// Release keys to allow full control over view in air without affecting movement
		if( bot->ShouldAttack() && CanFlyAboveGroundRelaxed( context ) ) {
			botInput->ClearMovementDirections();
			botInput->canOverrideLookVec = true;
		}
		return true;
	} else {
		SetupCommonBunnyHopInput( context );
		return true;
	}

	if( bot->ShouldAttack() && CanFlyAboveGroundRelaxed( context ) ) {
		botInput->ClearMovementDirections();
		botInput->canOverrideLookVec = true;
	}

	// Skip dash and WJ near triggers and nav targets to prevent missing a trigger/nav target
	const int nextReachNum = context->NextReachNum();
	if( !nextReachNum ) {
		// Preconditions check must not allow bunnying outside of nav target area having an empty reach. chain
		Assert( context->IsInNavTargetArea() );
		botInput->SetSpecialButton( false );
		botInput->canOverrideLookVec = false;
		botInput->canOverridePitch = false;
		return true;
	}

	switch( AiAasWorld::instance()->getReaches()[nextReachNum].traveltype ) {
		case TRAVEL_TELEPORT:
		case TRAVEL_JUMPPAD:
		case TRAVEL_ELEVATOR:
		case TRAVEL_LADDER:
		case TRAVEL_BARRIERJUMP:
			botInput->SetSpecialButton( false );
			botInput->canOverrideLookVec = false;
			botInput->canOverridePitch = true;
			return true;
		default:
			if( context->IsCloseToNavTarget() ) {
				botInput->SetSpecialButton( false );
				botInput->canOverrideLookVec = false;
				botInput->canOverridePitch = false;
				return true;
			}
	}

	if( ShouldPrepareForCrouchSliding( context, 8.0f ) ) {
		botInput->SetUpMovement( -1 );
		context->predictionStepMillis = context->DefaultFrameTime();
	}

	TrySetWalljump( context, velocityDir2D, toTargetDir2D );
	return true;
}

bool BunnyHopAction::CanFlyAboveGroundRelaxed( const PredictionContext *context ) const {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		return false;
	}

	float desiredHeightOverGround = 0.3f * AI_JUMPABLE_HEIGHT;
	return entityPhysicsState.HeightOverGround() >= desiredHeightOverGround;
}

void BunnyHopAction::TrySetWalljump( PredictionContext *context, const Vec3 &velocity2DDir, const Vec3 &intendedLookDir2D ) {
	if( !CanSetWalljump( context, velocity2DDir, intendedLookDir2D ) ) {
		return;
	}

	auto *botInput = &context->record->botInput;
	botInput->ClearMovementDirections();
	botInput->SetSpecialButton( true );
	// Predict a frame precisely for walljumps
	context->predictionStepMillis = context->DefaultFrameTime();
}

bool BunnyHopAction::CanSetWalljump( PredictionContext *context, const Vec3 &velocity2DDir, const Vec3 &intended2DLookDir ) const {
	const short *pmoveStats = context->currMinimalPlayerState->pmove.stats;
	if( !( pmoveStats[PM_STAT_FEATURES] & PMFEAT_WALLJUMP ) ) {
		return false;
	}

	if( pmoveStats[PM_STAT_WJTIME] ) {
		return false;
	}

	if( pmoveStats[PM_STAT_STUN] ) {
		return false;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		return false;
	}

	if( entityPhysicsState.HeightOverGround() < 8.0f && entityPhysicsState.Velocity()[2] <= 0 ) {
		return false;
	}

	// The 2D speed is too low for walljumping
	if( entityPhysicsState.Speed2D() < 400 ) {
		return false;
	}

	return velocity2DDir.Dot( entityPhysicsState.ForwardDir() ) > 0.7f && velocity2DDir.Dot( intended2DLookDir ) > 0.7f;
}

bool BunnyHopAction::CheckStepSpeedGainOrLoss( PredictionContext *context ) {
	const auto &newEntityPhysicsState = context->movementState->entityPhysicsState;
	const auto &oldEntityPhysicsState = context->PhysicsStateBeforeStep();

	// Test for a huge speed loss in case of hitting of an obstacle
	const float *oldVelocity = oldEntityPhysicsState.Velocity();
	const float *newVelocity = newEntityPhysicsState.Velocity();
	const float oldSquare2DSpeed = oldEntityPhysicsState.SquareSpeed2D();
	const float newSquare2DSpeed = newEntityPhysicsState.SquareSpeed2D();

	// Check for unintended bouncing back (starting from some speed threshold)
	if( oldSquare2DSpeed > wsw::square( 100.0f ) ) {
		if( newSquare2DSpeed > wsw::square( 1.0f ) ) {
			Vec3 oldVelocity2DDir( oldVelocity[0], oldVelocity[1], 0 );
			// TODO: Cache the inverse speed
			oldVelocity2DDir *= Q_Rcp( oldEntityPhysicsState.Speed2D() );
			Vec3 newVelocity2DDir( newVelocity[0], newVelocity[1], 0 );
			newVelocity2DDir *= Q_Rcp( newEntityPhysicsState.Speed2D() );
			if( oldVelocity2DDir.Dot( newVelocity2DDir ) < 0.3f ) {
				Debug( "A prediction step has lead to an unintended bouncing back\n" );
				return false;
			}
		} else {
			Debug( "A prediction step has lead to close to zero 2D speed while it was significant\n" );
			return false;
		}
	}

	// Check for regular speed loss
	const float oldSpeed = oldEntityPhysicsState.Speed();
	const float newSpeed = newEntityPhysicsState.Speed();

	Assert( context->predictionStepMillis );
	float actualSpeedGainPerSecond = ( newSpeed - oldSpeed ) / ( 0.001f * context->predictionStepMillis );
	if( actualSpeedGainPerSecond >= minDesiredSpeedGainPerSecond || context->IsInNavTargetArea() ) {
		// Reset speed loss timer
		currentSpeedLossSequentialMillis = 0;
		return true;
	}

	const char *format = "Actual speed gain per second %.3f is lower than the desired one %.3f\n";
	Debug( "oldSpeed: %.1f, newSpeed: %1.f, speed gain per second: %.1f\n", oldSpeed, newSpeed, actualSpeedGainPerSecond );
	Debug( format, actualSpeedGainPerSecond, minDesiredSpeedGainPerSecond );

	currentSpeedLossSequentialMillis += context->predictionStepMillis;
	if( tolerableSpeedLossSequentialMillis > currentSpeedLossSequentialMillis ) {
		return true;
	}

	// Let actually interrupt it if the new speed is less than this threshold.
	// Otherwise many trajectories that look feasible get rejected.
	// We should not however completely eliminate this interruption
	// as sometimes it prevents bumping in obstacles pretty well.
	const float speed2D = newEntityPhysicsState.Speed2D();
	const float threshold = 0.5f * ( context->GetRunSpeed() + context->GetDashSpeed() );
	if( speed2D >= threshold ) {
		return true;
	}

	// If the area is not a "skip collision" area
	if( !( AiAasWorld::instance()->getAreaSettings()[context->CurrAasAreaNum()].areaflags & AREA_SKIP_COLLISION_MASK ) ) {
		const float frac = ( threshold - speed2D ) * Q_Rcp( threshold );
		EnsurePathPenalty( (unsigned)( 100 + 3000 * Q_Sqrt( frac ) ) );
	}

	return true;
}

bool BunnyHopAction::WasOnGroundThisFrame( const PredictionContext *context ) const {
	return context->movementState->entityPhysicsState.GroundEntity() || context->frameEvents.hasJumped;
}

bool BunnyHopAction::TryHandlingWorseTravelTimeToTarget( PredictionContext *context,
														 int currTravelTimeToTarget,
														 int groundedAreaNum ) {
	constexpr const char *format = "A prediction step has lead to increased travel time to nav target\n";
	// Convert minTravelTimeToNavTargetSoFar to millis to have the same units for comparison
	int maxTolerableTravelTimeMillis = 10 * minTravelTimeToNavTargetSoFar;
	maxTolerableTravelTimeMillis += tolerableWalkableIncreasedTravelTimeMillis;

	// Convert currTravelTime from seconds^-2 to millis to have the same units for comparison
	if( 10 * currTravelTimeToTarget > maxTolerableTravelTimeMillis ) {
		Debug( format );
		return false;
	}

	EnsurePathPenalty( 200 );

	// Can't say much in this case. Continue prediction.
	if( !groundedAreaNum || !minTravelTimeAreaNumSoFar ) {
		return true;
	}

	const auto *aasWorld = AiAasWorld::instance();

	// Allow further prediction if we're still in the same floor cluster
	if( const int clusterNum = aasWorld->floorClusterNum( minTravelTimeAreaNumSoFar ) ) {
		if( clusterNum == aasWorld->floorClusterNum( groundedAreaNum ) ) {
			return true;
		}
	}

	// Allow further prediction if we're in a NOFALL area.
	if( aasWorld->getAreaSettings()[groundedAreaNum].areaflags & AREA_NOFALL ) {
		const auto aasAreas = aasWorld->getAreas();
		// Delta Z relative to the best area so far must be positive
		if( aasAreas[groundedAreaNum].mins[2] > aasAreas[minTravelTimeAreaNumSoFar].mins[2] ) {
			EnsurePathPenalty( 250 );
			return true;
		}
		// Allow negative Z while being in a stairs cluster
		if( aasWorld->stairsClusterNum( groundedAreaNum ) ) {
			EnsurePathPenalty( 350 );
			return true;
		}
	}

	// Disallow moving into an area if the min travel time area cannot be reached by walking from the area.
	// Use a simple reverse reach. test instead of router calls (that turned out to be expensive/non-scalable).
	if( CheckDirectReachWalkingOrFallingShort( groundedAreaNum, minTravelTimeAreaNumSoFar ) ) {
		return true;
	}

	EnsurePathPenalty( 3000 );
	return true;
}

bool BunnyHopAction::CheckDirectReachWalkingOrFallingShort( int fromAreaNum, int toAreaNum ) {
	const auto *aasWorld = AiAasWorld::instance();
	const auto aasReaches = aasWorld->getReaches();
	const auto &areaSettings = aasWorld->getAreaSettings()[fromAreaNum];

	// Limit number of tested rev. reach.
	// TODO: Add and use reverse reach. table for this and many other purposes
	int maxReachNum = areaSettings.firstreachablearea + wsw::min( areaSettings.numreachableareas, 16 );
	for( int revReachNum = areaSettings.firstreachablearea; revReachNum != maxReachNum; revReachNum++ ) {
		const auto &reach = aasReaches[revReachNum];
		if( reach.areanum != toAreaNum ) {
			continue;
		}
		const auto travelType = reach.traveltype & TRAVELTYPE_MASK;
		if( travelType == TRAVEL_WALK ) {
			EnsurePathPenalty( 300 );
			return true;
		}
		if( travelType == TRAVEL_WALKOFFLEDGE ) {
			// Make sure the fall distance is insufficient
			if( reach.start[2] - reach.end[2] < 64.0f ) {
				EnsurePathPenalty( 400 );
				return true;
			}
		}
		// We've found a rev. reach. (even if it did not pass additional tests). Avoid doing further tests.
		break;
	}

	return false;
}

bool BunnyHopAction::TryHandlingUnreachableTarget( PredictionContext *context ) {
	currentUnreachableTargetSequentialMillis += context->predictionStepMillis;
	if( currentUnreachableTargetSequentialMillis < tolerableUnreachableTargetSequentialMillis ) {
		return true;
	}

	Debug( "A prediction step has lead to undefined travel time to the nav target\n" );
	return false;
}

bool BunnyHopAction::CheckNavTargetAreaTransition( PredictionContext *context ) {
	if( !context->IsInNavTargetArea() ) {
		// If the bot has left the nav target area
		if( hasEnteredNavTargetArea ) {
			if( !hasTouchedNavTarget ) {
				Debug( "The bot has left the nav target area without touching the nav target\n" );
				return false;
			}
			// Otherwise just save the action for next frame.
			// We do not want to fall in a gap after picking a nav target.
		}
		return true;
	}

	hasEnteredNavTargetArea = true;
	if( HasTouchedNavEntityThisFrame( context ) ) {
		hasTouchedNavTarget = true;
	}

	if( hasTouchedNavTarget ) {
		return true;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	Vec3 toTargetDir( context->NavTargetOrigin() );
	toTargetDir -= entityPhysicsState.Origin();
	if( !toTargetDir.normalizeFast() ) {
		return true;
	}

	Vec3 velocityDir( entityPhysicsState.Velocity() );
	velocityDir *= Q_Rcp( entityPhysicsState.Speed() );
	if( velocityDir.Dot( toTargetDir ) > 0.7f ) {
		return true;
	}

	Debug( "The bot is very likely going to miss the nav target\n" );
	return false;
}

bool BunnyHopAction::HasMadeAnAdvancementPriorToLanding( PredictionContext *context, int currTravelTimeToTarget ) {
	assert( currTravelTimeToTarget );

	// If there was a definite advancement from the initial position
	if( currTravelTimeToTarget < travelTimeAtSequenceStart ) {
		return true;
	}

	// Any feasible travel time would be an advancement in this case
	if( !travelTimeAtSequenceStart ) {
		return true;
	}

	if( currTravelTimeToTarget > travelTimeAtSequenceStart ) {
		return false;
	}

	// Try finding a target point in the same area
	Vec3 targetPoint( 0, 0, 0 );
	std::optional<float> initial2DDistance;
	if( reachAtSequenceStart ) {
		if( const auto reachNum = context->NextReachNum(); reachNum == reachAtSequenceStart ) {
			targetPoint.Set( AiAasWorld::instance()->getReaches()[reachNum].start );
			initial2DDistance = distanceToReachAtStart;
		}
	} else if( context->IsInNavTargetArea() ) {
		targetPoint = context->NavTargetOrigin();
		initial2DDistance = distanceInNavTargetAreaAtStart;
	}

	if( initial2DDistance == std::nullopt ) {
		return false;
	}

	constexpr const float min2DAdvancementToTarget = 72.0f;
	// If the area was way too small to track advancement within its bounds
	if( *initial2DDistance < min2DAdvancementToTarget ) {
		return false;
	}

	const auto &newEntityPhysicsState = context->movementState->entityPhysicsState;
	const float distance2DToTarget = targetPoint.FastDistance2DTo( newEntityPhysicsState.Origin() );
	// If the advancement was insufficient
	if( distance2DToTarget + min2DAdvancementToTarget > initial2DDistance ) {
		return false;
	}

	// Make sure we can normalize the velocity dir.
	// Consider the dir confirming in the case of a small velocity.
	if( newEntityPhysicsState.Speed() < 100.0f ) {
		return true;
	}

	if( distance2DToTarget > 12.0f ) {
		Vec3 dirToReach( targetPoint - newEntityPhysicsState.Origin() );
		dirToReach.normalizeFastOrThrow();

		Vec3 velocityDir( newEntityPhysicsState.Velocity() );
		velocityDir *= Q_Rcp( newEntityPhysicsState.Speed() );
		constexpr const float maxFracDistance = min2DAdvancementToTarget, invMaxFracDistance = 1.0f / maxFracDistance;
		const float distance2DFrac = invMaxFracDistance * wsw::min( maxFracDistance, distance2DToTarget );
		assert( distance2DFrac >= -0.01 && distance2DFrac <= 1.01f );
		// Require a better velocity conformance for landing closer to the target
		const float dotThreshold = 0.9f - 0.2f * distance2DFrac;
		if( velocityDir.Dot( dirToReach ) > dotThreshold ) {
			return true;
		}
	}

	return false;
}

void BunnyHopAction::CheckPredictionStepResults( PredictionContext *context ) {
	BaseAction::CheckPredictionStepResults( context );
	if( context->shouldRollback || context->isCompleted ) {
		return;
	}

	if( !CheckStepSpeedGainOrLoss( context ) ) {
		context->SetPendingRollback();
		return;
	}

	if( !CheckNavTargetAreaTransition( context ) ) {
		context->SetPendingRollback();
		return;
	}

	// This entity physics state has been modified after prediction step
	const auto &newEntityPhysicsState = context->movementState->entityPhysicsState;

	const int currTravelTimeToTarget = context->TravelTimeToNavTarget();
	if( !currTravelTimeToTarget ) {
		if( !TryHandlingUnreachableTarget( context ) ) {
			context->SetPendingRollback();
		}
		return;
	}

	// Reset unreachable target timer
	currentUnreachableTargetSequentialMillis = 0;

	const float squareDistanceFromStart = originAtSequenceStart.SquareDistanceTo( newEntityPhysicsState.Origin() );
	const int groundedAreaNum = context->CurrGroundedAasAreaNum();
	if( currTravelTimeToTarget <= minTravelTimeToNavTargetSoFar ) {
		minTravelTimeToNavTargetSoFar = currTravelTimeToTarget;
		minTravelTimeAreaNumSoFar = context->CurrAasAreaNum();
	} else {
		if( !TryHandlingWorseTravelTimeToTarget( context, currTravelTimeToTarget, groundedAreaNum ) ) {
			context->SetPendingRollback();
			return;
		}
	}

	if( squareDistanceFromStart < wsw::square( 64 ) ) {
		if( SequenceDuration( context ) < 384 ) {
			return;
		}

		// Prevent wasting CPU cycles on further prediction
		Debug( "The bot still has not covered 64 units yet in 384 millis\n" );
		context->SetPendingRollback();
		return;
	}

	if( WasOnGroundThisFrame( context ) ) {
		if( HasMadeAnAdvancementPriorToLanding( context, currTravelTimeToTarget ) ) {
			// If we're currently at the best position
			if( currTravelTimeToTarget == minTravelTimeToNavTargetSoFar ) {
				// Check for completion if we have already made a hop before
				if( hopsCounter ) {
					// Try a "direct" completion if we've landed some sufficient units ahead of the last hop origin
					if( latchedHopOrigin.SquareDistance2DTo( newEntityPhysicsState.Origin() ) > wsw::square( 72 ) ) {
						if( !sequencePathPenalty && hopsCounter == 2 ) {
							context->isCompleted = true;
							return;
						}
					}
				} else {
					// Set the latched hop state if it's needed
					if( !hasALatchedHop ) {
						hasALatchedHop = true;
						latchedHopOrigin.Set( newEntityPhysicsState.Origin() );
						// Save an "good enough" path that is going to be used if the direct completion fails
						unsigned advancement = 0;
						if( travelTimeAtSequenceStart ) {
							advancement = travelTimeAtSequenceStart - currTravelTimeToTarget;
						}
						if( hopsCounter == 0 ) {
							// Save a "last resort" path if we are about to mark the first hop
							context->SaveLastResortPath( sequencePathPenalty );
						} else {
							// Save a "good enough" path if we are about to mark the second hop
							context->SaveGoodEnoughPath( advancement, sequencePathPenalty );
						}
					}
				}
			}
		}
	} else {
		if( !didTheLatchedHop ) {
			if( hasALatchedHop ) {
				didTheLatchedHop = true;
				hopsCounter++;
			}
		} else {
			// Don't waste further cycles (the completion condition won't hold).
			if( hopsCounter && sequencePathPenalty ) {
				context->SetPendingRollback();
				return;
			}
		}
	}

	// Check whether to continue prediction still makes sense
	constexpr unsigned naturalLimit = PredictionContext::MAX_PREDICTED_STATES;
	unsigned stackGrowthLimit;
	if( hopsCounter > 1 ) {
		stackGrowthLimit = ( 7 * naturalLimit ) / 8;
	} else if( hopsCounter ) {
		stackGrowthLimit = ( 5 * naturalLimit ) / 6;
	} else {
		stackGrowthLimit = ( 3 * naturalLimit ) / 4;
	}
	if( context->topOfStackIndex < stackGrowthLimit ) {
		return;
	}

	// Stop wasting CPU cycles on this. Also prevent overflow of the prediction stack
	// leading to inability of restarting the action for testing a next direction (if any).
	context->SetPendingRollback();
}

void BunnyHopAction::OnApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::OnApplicationSequenceStarted( context );

	minTravelTimeToNavTargetSoFar = std::numeric_limits<int>::max();
	minTravelTimeAreaNumSoFar = 0;

	travelTimeAtSequenceStart = 0;
	reachAtSequenceStart = 0;

	latchedHopOrigin.Set( 0, 0, 0 );

	sequencePathPenalty = 0;

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	originAtSequenceStart.Set( entityPhysicsState.Origin() );

	distanceToReachAtStart = std::numeric_limits<float>::infinity();
	distanceInNavTargetAreaAtStart = std::numeric_limits<float>::infinity();

	if( context->NavTargetAasAreaNum() ) {
		int reachNum, travelTime;
		context->NextReachNumAndTravelTimeToNavTarget( &reachNum, &travelTime );
		if( travelTime ) {
			minTravelTimeToNavTargetSoFar = travelTime;
			travelTimeAtSequenceStart = travelTime;
			reachAtSequenceStart = reachNum;
			if( reachNum ) {
				const auto &reach = AiAasWorld::instance()->getReaches()[reachNum];
				distanceToReachAtStart = originAtSequenceStart.Distance2DTo( reach.start );
			} else {
				distanceInNavTargetAreaAtStart = originAtSequenceStart.Distance2DTo( context->NavTargetOrigin() );
			}
		}
	}

	currentSpeedLossSequentialMillis = 0;
	currentUnreachableTargetSequentialMillis = 0;

	hasEnteredNavTargetArea = false;
	hasTouchedNavTarget = false;

	hasALatchedHop = false;
	didTheLatchedHop = false;
	hopsCounter = 0;
}

void BunnyHopAction::OnApplicationSequenceStopped( PredictionContext *context,
												   SequenceStopReason reason,
												   unsigned stoppedAtFrameIndex ) {
	BaseAction::OnApplicationSequenceStopped( context, reason, stoppedAtFrameIndex );

	if( reason != FAILED ) {
		if( reason != DISABLED ) {
			this->disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
		}
		return;
	}

	// If the action has been disabled due to prediction stack overflow
	if( this->isDisabledForPlanning ) {
		return;
	}

	// Disable applying this action after rolling back to the savepoint
	this->disabledForApplicationFrameIndex = 0;
}

void BunnyHopAction::BeforePlanning() {
	BaseAction::BeforePlanning();
	this->disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
}