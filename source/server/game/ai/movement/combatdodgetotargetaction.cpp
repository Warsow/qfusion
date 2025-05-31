#include "combatdodgetotargetaction.h"
#include "movementlocal.h"

[[nodiscard]]
static inline auto makeMoveDir( const float *__restrict fractions, const Vec3 &__restrict forwardDir,
								const Vec3 &__restrict rightDir ) {
	const Vec3 forwardPart( forwardDir * fractions[0] );
	const Vec3 rightPart( rightDir * fractions[1] );
	Vec3 moveDir( forwardPart + rightPart );
	moveDir.Z() *= Z_NO_BEND_SCALE;
	const float moveDirSquareLen = moveDir.SquaredLength();
	moveDir *= Q_RSqrt( moveDirSquareLen );
	assert( std::fabs( moveDir.LengthFast() - 1.0f ) < 0.01f );
	return moveDir;
}

void CombatDodgeSemiRandomlyToTargetAction::UpdateKeyMoveDirs( PredictionContext *context ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *combatMovementState = &context->movementState->keyMoveDirsState;
	Assert( !combatMovementState->IsActive() );

	vec3_t closestFloorPoint;
	std::optional<Vec3> maybeTarget;
	const float *__restrict botOrigin = entityPhysicsState.Origin();
	if( context->sameFloorClusterAreasCache.GetClosestToTargetPoint( context, closestFloorPoint ) ) {
		if( Distance2DSquared( botOrigin, closestFloorPoint ) > wsw::square( 1.0f ) ) {
			maybeTarget = Vec3( closestFloorPoint );
		}
	} else if( const int nextReachNum = context->NextReachNum() ) {
		const auto &__restrict nextReach = AiAasWorld::instance()->getReaches()[nextReachNum];
		// This check is not just a normalization check but also is a logical one (switch to end if close to start)
		if( Distance2DSquared( botOrigin, nextReach.start ) > wsw::square( 16.0f ) ) {
			maybeTarget = Vec3( nextReach.start );
		} else if( Distance2DSquared( botOrigin, nextReach.end ) > wsw::square( 1.0f ) ) {
			maybeTarget = Vec3( nextReach.end );
		}
	} else if( context->NavTargetAasAreaNum() ) {
		Vec3 navTargetOrigin( context->NavTargetOrigin() );
		if( navTargetOrigin.SquareDistance2DTo( botOrigin ) > wsw::square( 1.0f ) ) {
			maybeTarget = navTargetOrigin;
		}
	}

	constexpr auto kMaxSideDirs = std::size( kSideDirFractions );

	bool hasDefinedMoveDir = false;
	// Slightly randomize (use fully random dirs in a half of cases).
	// This remains biased towards the target as random dirs in the
	// another half of cases still could conform to the desired direction.
	if( maybeTarget && ( random() > 0.5f ) ) {
		Vec3 desiredMoveDir( Vec3( *maybeTarget ) - botOrigin );
		desiredMoveDir.Z() *= Z_NO_BEND_SCALE;
		const float desiredDirSquareLen = desiredMoveDir.SquaredLength();
		if( desiredDirSquareLen > wsw::square( 12.0f ) ) {
			const Vec3 forwardDir( entityPhysicsState.ForwardDir() ), rightDir( entityPhysicsState.RightDir() );
			hasDefinedMoveDir = true;

			desiredMoveDir *= Q_RSqrt( desiredDirSquareLen );
			assert( std::fabs( desiredMoveDir.LengthFast() - 1.0f ) < 0.01f );

			struct DirAndScore {
				unsigned dir; float score;
				[[nodiscard]]
				bool operator<( const DirAndScore &that ) const { return score > that.score; }
			} dirsAndScores[kMaxSideDirs];
			assert( std::size( dirsAndScores ) == std::size( dirIndices ) );

			for( unsigned i = 0; i < kMaxSideDirs; ++i ) {
				const Vec3 moveDir( makeMoveDir( kSideDirFractions[i], forwardDir, rightDir ) );
				dirsAndScores[i] = { i, desiredMoveDir.Dot( moveDir ) };
			}

			// Give a preference to dirs with larger scores
			wsw::sortByFieldDescending( std::begin( dirsAndScores ), std::end( dirsAndScores ), &DirAndScore::score );
			for( unsigned i = 0; i < kMaxSideDirs; ++i ) {
				this->dirIndices[i] = dirsAndScores[i].dir;
			}
		}
	}
	if( !hasDefinedMoveDir ) {
		const std::optional<SelectedEnemy> &selectedEnemy = bot->GetSelectedEnemy();
		if( selectedEnemy && selectedEnemy->IsThreatening() ) {
			const Vec3 forwardDir( entityPhysicsState.ForwardDir() ), rightDir( entityPhysicsState.RightDir() );

			Vec3 enemyLookDir( selectedEnemy->LookDir() );
			enemyLookDir.Z() *= Z_NO_BEND_SCALE;
			enemyLookDir.normalizeFastOrThrow();

			// We don't want a fully deterministic choice by the dot product (so best dirs are always first).
			// We select randomly using weights giving greater weights to better dirs.

			float totalWeightsSum = 0.0f;
			// A single value is sufficient but using a pair is more clear
			std::pair<float, float> segments[kMaxSideDirs];

			constexpr float kMaxWeight = 1.0f, kMinWeight = 0.25f;
			for( unsigned i = 0; i < kMaxSideDirs; ++i ) {
				const Vec3 moveDir( makeMoveDir( kSideDirFractions[i], forwardDir, rightDir ) );
				// Give orthogonal directions a priority
				const float frac = 1.0f - std::fabs( enemyLookDir.Dot( moveDir ) );
				assert( frac >= -0.01f && frac <= 1.01f );
				const float weight = kMinWeight + ( kMaxWeight - kMinWeight ) * frac;
				assert( weight > 0.1f );
				const float oldSum = totalWeightsSum;
				totalWeightsSum += weight;
				segments[i] = { oldSum, totalWeightsSum };
			}

			// Now pick directions randomly one by one, better dirs got a greater chance
			unsigned numPickedDirs = 0;
			unsigned alreadyPickedMask = 0;
			assert( totalWeightsSum > kMinWeight * kMaxSideDirs && totalWeightsSum > 0.1f );
			while( numPickedDirs != kMaxSideDirs ) {
				// Add a small epsilon to ensure it's within bounds
				const float r = ( totalWeightsSum - 0.001f ) * random();
				assert( r >= 0.0f && r < totalWeightsSum );
				// We've picked a random value within [0, totalWeightsSum) bounds.
				// Find a direction that corresponds to this value.
				for( unsigned i = 0, maskBit = 1; i < kMaxSideDirs; ++i, maskBit <<= 1 ) {
					if( !( alreadyPickedMask & maskBit ) ) {
						assert( segments[i].first < segments[i].second );
						if( r >= segments[i].first && r <= segments[i].second ) {
							this->dirIndices[numPickedDirs++] = i;
							alreadyPickedMask |= maskBit;
							break;
						}
					}
				}
			}
			assert( alreadyPickedMask == ( 1u << kMaxSideDirs ) - 1 );
		} else {
			// Poor man's shuffle (we don't have an STL-compatible RNG).
			// This is acceptable to randomize directions between frames.
			const auto randomOffset = (unsigned)( level.framenum );
			for( unsigned i = 0; i < std::size( this->dirIndices ); ++i ) {
				// TODO: Fix the RNG interface/the RNG in general, use a proper shuffle.
				this->dirIndices[( i + randomOffset ) % std::size( this->dirIndices )] = i;
			}
		}
	}

	const auto *directions = kSideDirSigns[this->dirIndices[0]];
	combatMovementState->Activate( directions[0], directions[1], dirsTimeout );
}

void CombatDodgeSemiRandomlyToTargetAction::PlanPredictionStep( PredictionContext *context ) {
	auto *botInput = &context->record->botInput;
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	botInput->canOverrideLookVec = true;
	botInput->shouldOverrideLookVec = true;
	botInput->isUcmdSet = true;

	if( attemptNum == maxAttempts ) {
		if( IsAllowedToFail() ) {
			Debug( "All attempts have failed. Switching to the fallback/dummy action\n" );
			Assert( this->allowFailureUsingThatAsNextAction );
			Assert( this->allowFailureUsingThatAsNextAction != this );
			this->DisableWithAlternative( context, this->allowFailureUsingThatAsNextAction );
			return;
		}
		Debug( "Attempts count has reached its limit. Should stop planning\n" );
		// There is no fallback action since this action is a default one for combat state.
		botInput->SetForwardMovement( 0 );
		botInput->SetRightMovement( 0 );
		botInput->SetUpMovement( bot->IsCombatCrouchingAllowed() ? -1 : +1 );
		context->isCompleted = true;
	}

	// If there are "selected enemies", this look dir will be overridden
	// using more appropriate value by aiming subsystem
	// but still has to be provided for movement prediction.
	// Otherwise the bot will be looking at "kept in fov" point.
	botInput->SetIntendedLookDir( lookDir, true );

	const short *const pmStats = context->currMinimalPlayerState->pmove.stats;
	if( entityPhysicsState.GroundEntity() ) {
		assert( !botInput->IsSpecialButtonSet() && !botInput->UpMovement() );
		assert( !botInput->ForwardMovement() && !botInput->RightMovement() );
		if( isCombatDashingAllowed && !pmStats[PM_STAT_DASHTIME] && ( pmStats[PM_STAT_FEATURES] & PMFEAT_DASH ) ) {
			if( const float threshold = context->GetDashSpeed() - 10; entityPhysicsState.Speed2D() < threshold ) {
				botInput->SetSpecialButton( true );
				context->predictionStepMillis = context->DefaultFrameTime();
			}
		}
		auto *const combatMovementState = &context->movementState->keyMoveDirsState;
		// If dashing is allowed but cannot be performed, try preserving the speed by jumping
		// if the current velocity direction conforms the desired one for this prediction attempt.
		if( isCombatDashingAllowed && !botInput->IsSpecialButtonSet() && ( pmStats[PM_STAT_FEATURES] & PMFEAT_JUMP ) ) {
			if( const float threshold = context->GetDashSpeed(); entityPhysicsState.Speed2D() > threshold ) {
				Vec3 keyMoveDir( 0, 0, 0 );
				keyMoveDir += (float)combatMovementState->ForwardMove() * entityPhysicsState.ForwardDir();
				keyMoveDir += (float)combatMovementState->RightMove() * entityPhysicsState.RightDir();
				if( const auto squareKeyDirLen = keyMoveDir.SquaredLength(); squareKeyDirLen > wsw::square( 0.5f ) ) {
					keyMoveDir *= Q_RSqrt( squareKeyDirLen );
					assert( std::fabs( keyMoveDir.LengthFast() - 1.0f ) < 0.01f );
					Vec3 velocity2DDir( entityPhysicsState.Velocity() );
					velocity2DDir.Z() = 0;
					velocity2DDir *= Q_Rcp( entityPhysicsState.Speed2D() );
					assert( std::fabs( velocity2DDir.LengthFast() - 1.0f ) < 0.01f );
					if( velocity2DDir.Dot( keyMoveDir ) > 0.87f ) {
						const auto floorAreaNum = entityPhysicsState.DroppedToFloorAasAreaNum();
						// Restrict to NOFALL areas for now
						// TODO: Use another prediction attempt with the same direction if jumping fails
						if( AiAasWorld::instance()->getAreaSettings()[floorAreaNum].areaflags & AREA_NOFALL ) {
							botInput->SetUpMovement( 1 );
						}
					}
				}
			}
		}
		if( !botInput->UpMovement() ) {
			botInput->SetForwardMovement( combatMovementState->ForwardMove() );
			botInput->SetRightMovement( combatMovementState->RightMove() );
		}
		// Set at least a single key or button while on ground (forward/right move keys might both be zero)
		if( !botInput->ForwardMovement() && !botInput->RightMovement() && !botInput->UpMovement() ) {
			if( !botInput->IsSpecialButtonSet() ) {
				botInput->SetUpMovement( isCompatCrouchingAllowed ? -1 : +1 );
			}
		}
	} else {
		if( ( pmStats[PM_STAT_FEATURES] & PMFEAT_WALLJUMP ) && !pmStats[PM_STAT_WJTIME] && !pmStats[PM_STAT_STUN] ) {
			botInput->SetSpecialButton( true );
			context->predictionStepMillis = context->DefaultFrameTime();
		}

		const float skill = bot->Skill();
		Assert( skill >= 0.0f && skill <= 1.0f );
		const float runSpeed = context->GetRunSpeed();
		if( !botInput->IsSpecialButtonSet() && ( entityPhysicsState.Speed2D() < runSpeed * ( 1.0f + skill ) ) ) {
			const float accelFrac = skill;
			context->CheatingAccelerate( accelFrac );
		}
	}
}

void CombatDodgeSemiRandomlyToTargetAction::CheckPredictionStepResults( PredictionContext *context ) {
	BaseAction::CheckPredictionStepResults( context );
	if( context->cannotApplyAction || context->isCompleted ) {
		return;
	}

	if( !this->bestTravelTimeSoFar ) {
		if( IsAllowedToFail() ) {
			Debug( "The initial travel time was undefined\n" );
			context->SetPendingRollback();
			return;
		}
	}

	const int newTravelTimeToTarget = context->TravelTimeToNavTarget();
	// If there is no definite current travel time to target
	if( !newTravelTimeToTarget ) {
		// If there was a definite initial/best travel time to target
		if( this->bestTravelTimeSoFar ) {
			Debug( "A prediction step has lead to an undefined travel time to the nav target\n" );
			context->SetPendingRollback();
			return;
		}
	}

	const int currGroundedAreaNum = context->CurrGroundedAasAreaNum();
	if( newTravelTimeToTarget <= this->bestTravelTimeSoFar ) {
		this->bestTravelTimeSoFar = newTravelTimeToTarget;
		this->bestFloorClusterSoFar = AiAasWorld::instance()->floorClusterNum( currGroundedAreaNum );
	} else {
		// If this flag is set, rollback immediately.
		// We need to be sure the action leads to advancing to the nav target.
		// Otherwise a reliable fallback action should be used.
		if( IsAllowedToFail() ) {
			Debug( "A prediction step has lead to an increased travel time to the nav target\n" );
			context->SetPendingRollback();
			return;
		}

		if( newTravelTimeToTarget > this->bestTravelTimeSoFar + 50 ) {
			bool rollback = true;
			// If we're still in the best floor cluster, use more lenient increased travel time threshold
			if( AiAasWorld::instance()->floorClusterNum( currGroundedAreaNum ) == bestFloorClusterSoFar ) {
				if( newTravelTimeToTarget < this->bestTravelTimeSoFar + 100 ) {
					rollback = false;
				}
			}
			if( rollback ) {
				Debug( "A prediction step has lead to an increased travel time to the nav target\n" );
				context->SetPendingRollback();
				return;
			}
		}
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	// Check for prediction termination...
	if( !entityPhysicsState.GroundEntity() || this->SequenceDuration( context ) < dirsTimeout ) {
		context->SaveSuggestedActionForNextFrame( this );
		return;
	}

	float minDistance = 16.0f;
	if( IsAllowedToFail() ) {
		// Using "combat dodging" over fallback movement is unjustified if the resulting speed is this low
		if( entityPhysicsState.Speed2D() < context->GetRunSpeed() ) {
			Debug( "The 2D speed is way too low and does not justify using this combat action over fallback one\n" );
			context->SetPendingRollback();
			return;
		}
		minDistance = 24.0f;
	}

	// Check for blocking
	if( originAtSequenceStart.SquareDistance2DTo( entityPhysicsState.Origin() ) < wsw::square( minDistance ) ) {
		Debug( "The total covered distance since the sequence start is too low\n" );
		context->SetPendingRollback();
		return;
	}

	context->isCompleted = true;
}

void CombatDodgeSemiRandomlyToTargetAction::OnApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::OnApplicationSequenceStarted( context );

	this->bestTravelTimeSoFar = context->TravelTimeToNavTarget();
	this->bestFloorClusterSoFar = 0;
	if( int clusterNum = AiAasWorld::instance()->floorClusterNum( context->CurrGroundedAasAreaNum() ) ) {
		this->bestFloorClusterSoFar = clusterNum;
	}

	auto *const moveDirsState = &context->movementState->keyMoveDirsState;

	// The look dir gets reset in OnBeforePlanning() only once during planning.
	// We obviously should use the same look dir for every attempt
	// (for every action application sequence).
	// Only pressed buttons and their randomness vary during attempts.
	if( !lookDir ) {
		Assert( !context->topOfStackIndex );
		// There are currently 3 call sites where this movement action gets activated.
		// 1) PredictionContext::SuggestAnyAction()
		// The action gets selected if there are valid "selected enemies"
		// and if the bot should attack and should keep crosshair on enemies.
		// 2) PredictionContext::SuggestAnyAction()
		// If the previous condition does not hold but there is a valid "kept in fov point"
		// and the bot has a nav target and should not "rush headless"
		// (so a combat semi-random dodging keeping the "point" in fov
		// usually to be ready to fire is used for movement to nav target)
		// 3) WalkCarefullyAction::PlanPredictionStep()
		// That action checks whether a bot should "walk carefully"
		// and usually switches to a first bunnying action of proposed bunnying actions
		// if conditions of "walking carefully" action are not met.
		// But if the bot logic reports the bot should skip bunnying and favor combat movement
		// (e.g. to do an urgent dodge) this combat movement action gets activated.
		// There might be no predefined look dir in this case and thus we should keep existing look dir.

		// We try to select a look dir if it is available according to situation priority
		bool hasDefinedLookDir = false;
		if( bot->ShouldKeepXhairOnEnemy() && bot->GetSelectedEnemy() != std::nullopt ) {
			bot->GetSelectedEnemy()->LastSeenOrigin().CopyTo( tmpDir );
			hasDefinedLookDir = true;
		} else if( const std::optional<Vec3> &keptInFovPoint = bot->GetKeptInFovPoint() ) {
			keptInFovPoint->CopyTo( tmpDir );
			hasDefinedLookDir = true;
		}

		const auto &entityPhysicsState = context->movementState->entityPhysicsState;
		if( hasDefinedLookDir ) {
			VectorSubtract( tmpDir, entityPhysicsState.Origin(), tmpDir );
			VectorNormalize( tmpDir );
		} else {
			// Just keep existing look dir
			entityPhysicsState.ForwardDir().CopyTo( tmpDir );
		}
		lookDir = tmpDir;

		// Check whether directions have timed out only once per planning frame
		if( !moveDirsState->IsActive() ) {
			UpdateKeyMoveDirs( context );
			hasUpdatedMoveDirsAtFirstAttempt = true;
		}
	}

	// We have failed the first attempt using the kept dirs.
	// Deactivate the dirs state and try all new dirs.
	if( attemptNum == 1 && !hasUpdatedMoveDirsAtFirstAttempt ) {
		Assert( moveDirsState->IsActive() );
		moveDirsState->Deactivate();
		UpdateKeyMoveDirs( context );
		// Consider the first attempt wasted
		maxAttempts += 1;
	}

	unsigned dirIndex = ~0u;
	if( hasUpdatedMoveDirsAtFirstAttempt ) {
		if( attemptNum != maxAttempts ) {
			dirIndex = dirIndices[attemptNum];
		}
	} else {
		if( attemptNum && attemptNum != maxAttempts ) {
			dirIndex = dirIndices[attemptNum - 1];
		}
	}

	if( dirIndex < std::size( kSideDirSigns ) ) {
		const auto *signs = kSideDirSigns[dirIndex];
		moveDirsState->forwardMove = (int8_t)signs[0];
		moveDirsState->rightMove = (int8_t)signs[1];
	}
}

void CombatDodgeSemiRandomlyToTargetAction::OnApplicationSequenceStopped( PredictionContext *context,
																		  SequenceStopReason stopReason,
																		  unsigned stoppedAtFrameIndex ) {
	BaseAction::OnApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );
	if( stopReason != FAILED ) {
		attemptNum = 0;
		return;
	}

	attemptNum++;
	Assert( attemptNum <= maxAttempts );
	// Could've been set on prediction stack overflow
	this->isDisabledForPlanning = false;
}

void CombatDodgeSemiRandomlyToTargetAction::BeforePlanning() {
	BaseAction::BeforePlanning();

	const float skill = bot->Skill();

	this->lookDir = nullptr;
	this->attemptNum = 0;
	this->hasUpdatedMoveDirsAtFirstAttempt = false;
	this->isCombatDashingAllowed = ( skill >= 0.33f ) && bot->IsCombatDashingAllowed();
	this->isCompatCrouchingAllowed = bot->IsCombatCrouchingAllowed();
	this->maxAttempts = std::size( kSideDirSigns );
	this->allowFailureUsingThatAsNextAction = nullptr;

	constexpr float limit = 0.66f;
	this->dirsTimeout = 400u;
	// Make easy bots change directions slower
	if( skill < limit ) {
		const float frac = skill * ( 1.0f / limit );
		assert( frac >= 0.0f && frac <= 1.0f );
		this->dirsTimeout += (unsigned)( 300.0f * ( 1.0f - frac ) );
	}
}