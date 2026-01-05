/*
Copyright (C) 2026 vvk2212, Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#include "fallbackscriptsandactions.h"
#include "movementsubsystem.h"
#include "movementlocal.h"
#include <common/helpers/algorithm.h>

[[nodiscard]]
static bool contactsTargetPoint( const AiEntityPhysicsState &entityPhysicsState, const Vec3 &targetPoint ) {
	const Vec3 botMins( Vec3( playerbox_stand_mins ) + entityPhysicsState.Origin() );
	const Vec3 botMaxs( Vec3( playerbox_stand_maxs ) + entityPhysicsState.Origin() );
	return BoundsAndSphereIntersect( botMins.Data(), botMaxs.Data(), targetPoint.Data(), 1.0f );
}

void JumpToPointAction::beforePlanning() {
	BaseAction::beforePlanning();
	m_isDisabledForPlanning = false;
	m_attemptNum            = 0;
}

void JumpToPointAction::afterPlanning() {
	BaseAction::afterPlanning();
	m_isDisabledForPlanning = false;
	m_attemptNum            = 0;
}

void JumpToPointAction::onApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::onApplicationSequenceStarted( context );
	m_startPoint.Set( context->movementState->entityPhysicsState.Origin() );
	m_hasJumped = false;
}

void JumpToPointAction::onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
													  unsigned stoppedAtFrameIndex ) {
	BaseAction::onApplicationSequenceStopped( context, sequenceStopReason, stoppedAtFrameIndex );
	if( sequenceStopReason == FAILED ) {
		if( m_attemptNum < kMaxAttempts ) {
			++m_attemptNum;
		} else {
			m_isDisabledForPlanning = true;
		}
	}
}

auto JumpToPointAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( m_isDisabledForPlanning ) {
		return PredictionResult::Abort;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *const botInput           = &context->record->botInput;

	Vec3 viewOrigin( entityPhysicsState.Origin() );
	viewOrigin.Z() += playerbox_stand_viewheight;

	Vec3 intendedLookDir( ( Vec3( 0, 0, 24 ) + m_targetPoint ) - viewOrigin );
	if( !intendedLookDir.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
		intendedLookDir = Vec3( 0, 0, 1 );
	}

	botInput->SetIntendedLookDir( intendedLookDir, true );

	if( entityPhysicsState.GroundEntity() ) {
		if( !m_hasJumped ) {
			if( intendedLookDir.Dot( entityPhysicsState.ForwardDir() ) > 0.95f ) {
				botInput->SetForwardMovement( 1 );
				botInput->SetUpMovement( 1 );
				// TODO: Vary for reachability kind
				context->record->SetModifiedVelocity( ( 20.0f + context->GetDashSpeed() ) * intendedLookDir );
				// TODO: Detect jumps via events
				m_hasJumped = true;
			} else {
				botInput->SetTurnSpeedMultiplier( 3.0f );
			}
		}
		context->predictionStepMillis = context->DefaultFrameTime();
	} else {
		// Otherwise, wait for landing on the initial position first
		if( m_hasJumped ) {
			if( intendedLookDir.Dot( entityPhysicsState.ForwardDir() ) > 0.7f ) {
				botInput->SetForwardMovement( 1 );
				if( m_attemptNum > 0 ) {
					const float accelFrac = Q_Sqrt( (float)m_attemptNum * ( 1.0f / kMaxAttempts ) );
					context->CheatingAccelerate( accelFrac );
				}
				context->CheatingCorrectVelocity( m_targetPoint );
			} else {
				botInput->SetTurnSpeedMultiplier( 3.0f );
			}
		}
	}

	botInput->isUcmdSet = true;

	return PredictionResult::Continue;
}

[[nodiscard]]
static bool handleImperfectLanding( PredictionContext *context, const Vec3 &targetPoint, int targetAreaNum ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	int currAreaNums[2] { 0, 0 };
	const int numCurrAreas = entityPhysicsState.PrepareRoutingStartAreas( currAreaNums );
	if( !numCurrAreas ) [[unlikely]] {
		return false;
	}

	if( wsw::contains( currAreaNums, currAreaNums + numCurrAreas, targetAreaNum ) ) {
		context->SaveGoodEnoughPath( 1, (unsigned)targetPoint.FastDistanceTo( entityPhysicsState.Origin() ) );
		return true;
	}

	const auto *const aasWorld = AiAasWorld::instance();
	if( const auto targetClusterNum = aasWorld->floorClusterNum( targetAreaNum ) ) {
		for( int i = 0; i < numCurrAreas; ++i ) {
			if( targetClusterNum == aasWorld->floorClusterNum( currAreaNums[i] ) ) {
				context->SaveLastResortPath( (unsigned)targetPoint.FastDistanceTo( entityPhysicsState.Origin() ) );
				return true;
			}
		}
	}

	const int travelTime = context->RouteCache()->FindRoute( currAreaNums, numCurrAreas, targetAreaNum,
															 TFL_WALK | TFL_AIR | TFL_WALKOFFLEDGE );
	if( travelTime > 0 && travelTime < 100 ) {
		// Convert the penalty to msec from sec^-2
		context->SaveLastResortPath( 1000 + 10 * travelTime );
		return true;
	}

	return false;
}

auto JumpToPointAction::checkPredictionStepResults( PredictionContext *context ) -> PredictionResult {
	if( const auto result = BaseAction::checkPredictionStepResults( context ); result != PredictionResult::Continue ) {
		context->ShowBuiltPlanPath( true );
		return result;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		if( m_targetPoint.SquareDistanceTo( entityPhysicsState.Origin() ) <
			m_startPoint.SquareDistanceTo( entityPhysicsState.Origin() ) ) {
			if( contactsTargetPoint( entityPhysicsState, m_targetPoint ) ) {
				return PredictionResult::Complete;
			}
			if( m_targetAreaNum ) {
				if( !handleImperfectLanding( context, m_targetPoint, m_targetAreaNum ) ) {
					context->ShowBuiltPlanPath();
				}
			} else {
				context->ShowBuiltPlanPath();
			}
			return PredictionResult::Restart;
		}
	}

	if( context->topOfStackIndex + 2 < MAX_PREDICTED_STATES ) {
		return PredictionResult::Continue;
	}

	context->ShowBuiltPlanPath();
	return PredictionResult::Restart;
}

void SwitchingActionsForStateScript::selectActiveAction( BaseAction *action ) {
	if( !m_predictedMovementActions.empty() ) {
		// TODO: Remove action from fields of the entry, store it separately for the entire predicted tape
		if( m_predictedMovementActions.front().action != action ) {
			m_predictedMovementActions.clear();
		}
	}
	m_storageOfActionPtrs[0] = action;
}

bool TraverseJumpReachScript::produceBotInput( BotInput *input ) {
	const auto *const aasWorld     = AiAasWorld::instance();
	const auto &targetReach        = aasWorld->getReaches()[m_targetReachNum];
	const auto &entityPhysicsState = m_subsystem->getMovementState().entityPhysicsState;

	Vec3 reachDir( Vec3( targetReach.end ) - Vec3( targetReach.start ) );
	reachDir.normalizeFastOrThrow();
	vec3_t realReachStart;
	VectorMA( targetReach.start, -16.0f, reachDir.Data(), realReachStart );

	const bool shouldReachStartPoint = !contactsTargetPoint( entityPhysicsState, Vec3( realReachStart ) ) &&
		DistanceSquared( targetReach.end, entityPhysicsState.Origin() ) > DistanceSquared( targetReach.end, realReachStart );

	for( int turn = 0; turn < 2; ++turn ) {
		if( shouldReachStartPoint == ( turn == 0 ) ) {
			m_walkToPointAction.setTargetPoint( Vec3( realReachStart ) );
			// Some reachabilities start in air... we rely on the turn loop in such cases
			m_walkToPointAction.setAllowToReachThePointInAir( true );
			selectActiveAction( &m_walkToPointAction );
		} else {
			m_jumpToPointAction.setTarget( Vec3( targetReach.end ), targetReach.areanum );
			selectActiveAction( &m_jumpToPointAction );
		}
		if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
			m_timeoutAt = level.time + 3000;
			return true;
		}
	}

	return false;
}

void WalkToPointAction::beforePlanning() {
	BaseAction::beforePlanning();
	m_isDisabledForPlanning = false;
	if( !m_bot->ShouldBeSilent() ) {
		const auto pmoveFeatures = m_bot->PlayerState()->pmove.stats[PM_STAT_FEATURES];
		m_isDashingAllowed       = ( pmoveFeatures & PMFEAT_DASH ) != 0;
		m_isJumpingAllowed       = ( pmoveFeatures & PMFEAT_JUMP ) != 0;
		m_minDistanceFromTargetToDash = kBaseMinDistanceFromTargetToDash;
		m_minDistanceFromTargetToJump = kBaseMinDistanceFromTargetToJump;
	} else {
		m_isJumpingAllowed = false;
		m_isDashingAllowed = false;
	}
}

void WalkToPointAction::bumpDashDistance() {
	m_minDistanceFromTargetToDash += kBaseMinDistanceFromTargetToDash;
	if( m_minDistanceFromTargetToDash >= m_distanceFromStartToTarget ) {
		m_isDashingAllowed = false;
	}
}

void WalkToPointAction::bumpJumpDistance() {
	m_minDistanceFromTargetToJump += kBaseMinDistanceFromTargetToJump;
	if( m_minDistanceFromTargetToJump >= m_distanceFromStartToTarget ) {
		m_isJumpingAllowed = false;
	}
}

void WalkToPointAction::afterPlanning() {
	BaseAction::afterPlanning();
	m_isDisabledForPlanning = false;
}

void WalkToPointAction::onApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::onApplicationSequenceStarted( context );
	m_distanceFromStartToTarget = m_targetPoint.FastDistance2DTo( context->movementState->entityPhysicsState.Origin() );
	m_hasDashedOrWalljumped     = false;
	m_hasJumped                 = false;
}

void WalkToPointAction::onApplicationSequenceStopped( PredictionContext *context, SequenceStopReason sequenceStopReason,
													  unsigned stoppedAtFrameIndex ) {
	BaseAction::onApplicationSequenceStopped( context, sequenceStopReason, stoppedAtFrameIndex );
	if( sequenceStopReason == FAILED ) {
		// Note: Looks like sometimes we're unable to detect events
		if( m_hasJumped && m_isJumpingAllowed ) {
			bumpJumpDistance();
		} else if( m_hasDashedOrWalljumped && m_isDashingAllowed ) {
			bumpDashDistance();
		} else if( m_isJumpingAllowed ) {
			bumpJumpDistance();
		} else if( m_isDashingAllowed ) {
			bumpDashDistance();
		} else {
			m_isDisabledForPlanning = true;
		}
	}
}

[[nodiscard]]
auto WalkToPointAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( m_isDisabledForPlanning ) {
		return PredictionResult::Abort;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const auto *pmoveStats         = context->currMinimalPlayerState->pmove.stats;
	auto *const botInput           = &context->record->botInput;

	Vec3 viewOrigin( entityPhysicsState.Origin() );
	viewOrigin.Z() += playerbox_stand_viewheight;

	Vec3 dirToTargetPoint( Vec3( m_targetPoint ) - viewOrigin );
	dirToTargetPoint.Z() = 0;
	const std::optional<float> maybeDistance2D = dirToTargetPoint.normalizeFast( { .minAcceptableLength = 1.0f } );
	if( !maybeDistance2D ) {
		return PredictionResult::Restart;
	}

	if( std::optional<Vec3> keptInFovPoint = m_subsystem->bot->GetKeptInFovPoint() ) {
		if( keptInFovPoint->SquareDistance2DTo( entityPhysicsState.Origin() ) > wsw::square( 1.0f ) ) {
			Vec3 lookVec( Vec3( entityPhysicsState.Origin() ) - *keptInFovPoint );
			lookVec.Z() *= Z_NO_BEND_SCALE;

			if( entityPhysicsState.GroundEntity() ) {
				int keyMoves[2] { 0, 0 };
				context->TraceCache().makeKeyMovesToTarget( context, dirToTargetPoint, keyMoves );
				if( keyMoves[0] | keyMoves[1] ) {
					botInput->SetForwardMovement( keyMoves[0] );
					botInput->SetRightMovement( keyMoves[1] );
					if( *maybeDistance2D > m_walkProximityThreshold ) {
						if( std::abs( keyMoves[0] ) + std::abs( keyMoves[1] ) == 1 ) {
							if( m_isDashingAllowed && *maybeDistance2D >= m_minDistanceFromTargetToDash ) {
								if( entityPhysicsState.Speed2D() < context->GetDashSpeed() ) {
									botInput->SetSpecialButton( true );
								}
							} else if( m_isJumpingAllowed && *maybeDistance2D >= m_minDistanceFromTargetToJump ) {
								if( entityPhysicsState.Speed2D() > context->GetRunSpeed() - 1.0f ) {
									botInput->SetUpMovement( 1 );
								}
							}
						}
					}
					botInput->SetIntendedLookDir( lookVec );
					botInput->isUcmdSet          = true;
					botInput->canOverrideLookVec = true;
					botInput->canOverridePitch   = true;
				}
			} else {
				botInput->SetIntendedLookDir( lookVec );
				botInput->isUcmdSet          = true;
				botInput->canOverrideLookVec = true;
				botInput->canOverridePitch   = true;
			}
		}
	}

	if( !botInput->isUcmdSet ) {
		botInput->SetIntendedLookDir( dirToTargetPoint, true );
		botInput->isUcmdSet = true;
		if( dirToTargetPoint.Dot( entityPhysicsState.ForwardDir() ) > 0.95f ) {
			botInput->SetForwardMovement( 1 );
			if( *maybeDistance2D > m_walkProximityThreshold ) {
				if( entityPhysicsState.GroundEntity() ) {
					if( m_isDashingAllowed && !pmoveStats[PM_STAT_DASHTIME] ) {
						if( *maybeDistance2D >= m_minDistanceFromTargetToDash ) {
							if( entityPhysicsState.Speed2D() < context->GetDashSpeed() ) {
								botInput->SetSpecialButton( true );
							}
						}
					} else if( m_isJumpingAllowed ) {
						if( *maybeDistance2D >= m_minDistanceFromTargetToJump ) {
							if( entityPhysicsState.Speed2D() > context->GetRunSpeed() - 1.0f ) {
								Vec3 velocityDir( entityPhysicsState.Velocity() );
								velocityDir *= Q_Rcp( entityPhysicsState.Speed() );
								// TODO: dirToTargetPoint is a 2D dir
								if( velocityDir.Dot( dirToTargetPoint ) > 0.9f ) {
									botInput->SetUpMovement( 1 );
								}
							}
						}
					}
				}
			} else {
				botInput->SetWalkButton( true );
			}
		}
		if( m_subsystem->bot->ShouldAttack() ) {
			if( !entityPhysicsState.GroundEntity() ) {
				botInput->ClearMovementDirections();
				botInput->ClearButtons();
				botInput->canOverrideUcmd  = true;
				botInput->canOverridePitch = true;
			}
		}
	}

	return PredictionResult::Continue;
}

[[nodiscard]]
auto WalkToPointAction::checkPredictionStepResults( PredictionContext *context ) -> PredictionResult {
	m_hasDashedOrWalljumped = context->frameEvents.hasDashed || context->frameEvents.hasWalljumped;
	m_hasJumped             = context->frameEvents.hasJumped || context->frameEvents.hasDoubleJumped;

	if( const auto result = BaseAction::checkPredictionStepResults( context ); result != PredictionResult::Continue ) {
		return result;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	// TODO: Put limitations on 2D speed (it is important for WALKOFFLEDGE)
	if( entityPhysicsState.GroundEntity() || m_allowToReachThePointInAir ) {
		if( entityPhysicsState.Speed2D() <= m_maxAllowed2DSpeedAtTargetPoint ) {
			if( contactsTargetPoint( entityPhysicsState, m_targetPoint ) ) {
				return PredictionResult::Complete;
			}
		}
	}

	const float squareDistance2D = m_targetPoint.FastDistance2DTo( entityPhysicsState.Origin() );
	// Stop wasting CPU cycles if we are definitely not going to hit the target point
	if( squareDistance2D > wsw::square( m_distanceFromStartToTarget + 48.0f ) ) {
		return PredictionResult::Restart;
	}

	if( context->topOfStackIndex < MAX_PREDICTED_STATES - 2 ) {
		return PredictionResult::Continue;
	}

	return PredictionResult::Restart;
}

bool WalkToPointScript::produceBotInput( BotInput *input ) {
	// TODO: Invalidate explicitly?
	if( contactsTargetPoint( m_subsystem->getMovementState().entityPhysicsState, m_targetPoint ) ) {
		return false;
	}

	m_walkToPointAction.setTargetPoint( m_targetPoint );

	if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
		const auto lastRecordTimestamp = m_predictedMovementActions.back().timestamp;
		// game.realtime is the baseline for predicted entries
		assert( lastRecordTimestamp >= game.realtime );
		// TODO: Should we specify timeout in realtime as well?
		m_timeoutAt = level.time + ( lastRecordTimestamp - game.realtime ) + 1;
		return true;
	}

	return false;
}

void LandOnPointAction::beforePlanning() {
	BaseAction::beforePlanning();
	m_isDisabledForPlanning      = false;
	m_tryUsingDirectionKeys      = false;
	m_tryUsingCheatingCorrection = false;
	m_attemptNum                 = 0;
}

void LandOnPointAction::afterPlanning() {
	BaseAction::afterPlanning();
	m_isDisabledForPlanning = false;
}

void LandOnPointAction::onApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::onApplicationSequenceStarted( context );
	m_tryUsingDirectionKeys      = ( m_attemptNum & 1 ) != 0;
	m_tryUsingCheatingCorrection = ( m_attemptNum & 2 ) != 0;
}

void LandOnPointAction::onApplicationSequenceStopped( PredictionContext *context,
													  SequenceStopReason sequenceStopReason,
													  unsigned stoppedAtFrameIndex ) {
	BaseAction::onApplicationSequenceStopped( context, sequenceStopReason, stoppedAtFrameIndex );
	if( sequenceStopReason == FAILED ) {
		if( m_attemptNum < 3 ) {
			m_attemptNum++;
		} else {
			m_isDisabledForPlanning = true;
		}
	}
}

auto LandOnPointAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( m_isDisabledForPlanning ) {
		return PredictionResult::Abort;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *const botInput           = &context->record->botInput;

	Vec3 viewOrigin( entityPhysicsState.Origin() );
	viewOrigin.Z() += playerbox_stand_viewheight;

	Vec3 intendedLookDir3D( m_targetPoint - viewOrigin );
	if( intendedLookDir3D.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
		Vec3 intendedLookDir2D( intendedLookDir3D.X(), intendedLookDir3D.Y(), 0.0f );
		if( m_tryUsingDirectionKeys ) {
			constexpr float dotThreshold = 0.9f;
			if( intendedLookDir2D.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
				const float intendedDotForward = intendedLookDir2D.Dot( entityPhysicsState.ForwardDir() );
				if( std::fabs( intendedDotForward ) > dotThreshold ) {
					botInput->SetForwardMovement( intendedDotForward > 0.0f ? +1 : -1 );
					botInput->SetIntendedLookDir( intendedLookDir2D );
				} else {
					const float intendedDotRight = intendedLookDir2D.Dot( entityPhysicsState.RightDir() );
					if( std::fabs( intendedDotRight ) > dotThreshold ) {
						botInput->SetRightMovement( intendedDotRight > 0.0f ? +1 : -1 );
						botInput->SetIntendedLookDir( intendedLookDir2D );
					}
				}
			}
		}
		if( !( botInput->ForwardMovement() | botInput->RightMovement() ) ) {
			botInput->SetIntendedLookDir( intendedLookDir3D, true );
			if( intendedLookDir3D.Dot( entityPhysicsState.ForwardDir() ) > 0.3f ) {
				botInput->SetForwardMovement( 1 );
			} else {
				botInput->SetTurnSpeedMultiplier( 2.0f );
			}
		}
	} else {
		botInput->SetIntendedLookDir( Vec3( 0.0f, 0.0f, -1.0f ) );
	}

	if( m_tryUsingCheatingCorrection ) {
		context->CheatingCorrectVelocity( m_targetPoint );
	}

	assert( botInput->isLookDirSet );
	botInput->isUcmdSet = true;

	if( context->topOfStackIndex < 4 ) {
		context->predictionStepMillis = 16;
	} else if( context->topOfStackIndex < 8 ) {
		context->predictionStepMillis = 4 * 16;
	} else if( context->topOfStackIndex < 16 ) {
		context->predictionStepMillis = 8 * 16;
	} else {
		context->predictionStepMillis = 12 * 16;
	}

	return PredictionResult::Continue;
}

auto LandOnPointAction::checkPredictionStepResults( PredictionContext *context ) -> PredictionResult {
	if( const auto result = BaseAction::checkPredictionStepResults( context ); result != PredictionResult::Continue ) {
		context->ShowBuiltPlanPath( false );
		return result;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		if( contactsTargetPoint( entityPhysicsState, m_targetPoint ) ) {
			return PredictionResult::Complete;
		}
		if( m_targetAreaNum ) {
			if( !handleImperfectLanding( context, m_targetPoint, m_targetAreaNum ) ) {
				context->ShowBuiltPlanPath( false );
			}
		} else {
			context->ShowBuiltPlanPath( false );
		}
		return PredictionResult::Restart;
	} else {
		if( context->topOfStackIndex + 2 < MAX_PREDICTED_STATES ) {
			return PredictionResult::Continue;
		}
		context->ShowBuiltPlanPath( false );
		return PredictionResult::Restart;
	}
}

bool TraverseWalkOffLedgeReachScript::produceBotInput( BotInput *input ) {
	const auto *const aasWorld     = AiAasWorld::instance();
	const auto &entityPhysicsState = m_subsystem->getMovementState().entityPhysicsState;
	const auto &targetReach        = aasWorld->getReaches()[m_targetReachNum];

	bool shouldReachStartPoint = false;
	if( entityPhysicsState.Origin()[2] + 24 >= targetReach.start[2] ) {
		if( !contactsTargetPoint( entityPhysicsState, Vec3( targetReach.start ) ) ) {
			shouldReachStartPoint = true;
		}
	}

	for( int turn = 0; turn < 2; ++turn ) {
		// Do the opposite on the second turn (we're unsure what action to select in boundary cases)
		if( shouldReachStartPoint == ( turn == 0 ) ) {
			m_walkToPointAction.setTargetPoint( Vec3( targetReach.start ) );
			m_walkToPointAction.setAllowToReachThePointInAir( true );
			m_walkToPointAction.setWalkProximityThreshold( 24.0f );
			selectActiveAction( &m_walkToPointAction );
		} else {
			m_landOnPointAction.setTarget( Vec3( targetReach.end ), targetReach.areanum );
			selectActiveAction( &m_landOnPointAction );
		}
		if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
			m_timeoutAt = level.time + 3000;
			return true;
		}
	}

	return false;
}

void ClimbOntoBarrierAction::beforePlanning() {
	BaseAction::beforePlanning();
	m_isDisabledForPlanning = false;
}

void ClimbOntoBarrierAction::afterPlanning() {
	BaseAction::afterPlanning();
	m_isDisabledForPlanning = false;
}

void ClimbOntoBarrierAction::onApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::onApplicationSequenceStarted( context );
}

void ClimbOntoBarrierAction::onApplicationSequenceStopped( PredictionContext *context,
														   SequenceStopReason sequenceStopReason,
														   unsigned stoppedAtFrameIndex ) {
	BaseAction::onApplicationSequenceStopped( context, sequenceStopReason, stoppedAtFrameIndex );
	if( sequenceStopReason == FAILED ) {
		m_isDisabledForPlanning = true;
	}
}

auto ClimbOntoBarrierAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( m_isDisabledForPlanning ) {
		return PredictionResult::Abort;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *const botInput           = &context->record->botInput;
	const auto *aasWorld           = AiAasWorld::instance();
	const auto &targetReach        = aasWorld->getReaches()[m_targetReachNum];

	if( entityPhysicsState.GroundEntity() ) {
		const float feetZ = entityPhysicsState.Origin()[2] + playerbox_stand_mins[2];
		// If we are closer to start
		if( std::fabs( targetReach.start[2] - feetZ ) < std::fabs( targetReach.end[2] - feetZ ) ) {
			if( contactsTargetPoint( entityPhysicsState, Vec3( targetReach.start ) ) ) {
				botInput->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
				// TODO: Ignore the speed limitation if we are facing the reachability in an optimal position
				if( entityPhysicsState.Speed2D() < 10.0f ) {
					if( entityPhysicsState.ForwardDir().Z() > 0.9f ) {
						botInput->SetUpMovement( 1 );
					} else {
						botInput->SetTurnSpeedMultiplier( 5.0f );
					}
				} else {
					botInput->SetTurnSpeedMultiplier( 5.0f );
				}
			} else {
				return PredictionResult::Restart;
			}
		} else {
			// If we appear to be standing on the barrier
			Vec3 viewOrigin( entityPhysicsState.Origin() );
			viewOrigin.Z() += playerbox_stand_viewheight;

			Vec3 intendedLookDir( Vec3( aasWorld->getAreas()[targetReach.areanum].center ) - viewOrigin );
			if( intendedLookDir.normalizeFast() ) {
				botInput->SetIntendedLookDir( intendedLookDir, true );
				if( intendedLookDir.Dot( entityPhysicsState.ForwardDir() ) > 0.9f ) {
					botInput->SetForwardMovement( 1 );
				}
			} else {
				return PredictionResult::Restart;
			}
		}
	} else {
		Vec3 viewOrigin( entityPhysicsState.Origin() );
		viewOrigin.Z() += playerbox_stand_viewheight;

		Vec3 intendedLookDir( viewOrigin );
		const auto &targetArea = aasWorld->getAreas()[targetReach.areanum];
		intendedLookDir -= targetArea.center;
		if( !intendedLookDir.normalizeFast() ) {
			intendedLookDir = viewOrigin - Vec3( targetArea.center[0], targetArea.center[1], targetArea.mins[2] );
			intendedLookDir.normalizeFastOrThrow();
		}

		intendedLookDir *= -1;
		if( intendedLookDir.Dot( entityPhysicsState.ForwardDir() ) > 0.9f ) {
			botInput->SetForwardMovement( 1 );
		} else {
			botInput->SetTurnSpeedMultiplier( 10.0f );
		}
		intendedLookDir.Z() *= 2.0f;
		botInput->SetIntendedLookDir( intendedLookDir, false );
		botInput->SetUpMovement( 1 );
	}

	botInput->isUcmdSet = true;

	context->predictionStepMillis = context->DefaultFrameTime();

	return PredictionResult::Continue;
}

auto ClimbOntoBarrierAction::checkPredictionStepResults( PredictionContext *context ) -> PredictionResult {
	if( const auto result = BaseAction::checkPredictionStepResults( context ); result != PredictionResult::Continue ) {
		return result;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const auto &targetReach        = AiAasWorld::instance()->getReaches()[m_targetReachNum];

	if( entityPhysicsState.GroundEntity() ) {
		const float feetZ = entityPhysicsState.Origin()[2] + playerbox_stand_mins[2];
		if( std::fabs( targetReach.end[2] - feetZ ) < std::fabs( targetReach.start[2] - feetZ ) ) {
			if( entityPhysicsState.CurrAasAreaNum() == targetReach.areanum ||
				entityPhysicsState.DroppedToFloorAasAreaNum() == targetReach.areanum ) {
				return PredictionResult::Complete;
			}
			// TODO: Test against target area center
			if( !handleImperfectLanding( context, Vec3( targetReach.end ), targetReach.areanum ) ) {
				context->ShowBuiltPlanPath();
			}
			return PredictionResult::Restart;
		}
	}

	if( context->topOfStackIndex + 2 < MAX_PREDICTED_STATES ) {
		return PredictionResult::Continue;
	}

	context->ShowBuiltPlanPath();
	return PredictionResult::Restart;
}

bool TraverseBarrierJumpReachScript::produceBotInput( BotInput *input ) {
	const auto *aasWorld           = AiAasWorld::instance();
	const auto &entityPhysicsState = m_subsystem->getMovementState().entityPhysicsState;
	const auto &targetReach        = aasWorld->getReaches()[m_targetReachNum];

	const bool shouldReachStartPoint = !contactsTargetPoint( entityPhysicsState, Vec3( targetReach.start ) );
	if( shouldReachStartPoint ) {
		m_walkToPointAction.setTargetPoint( Vec3( targetReach.start ) );
		selectActiveAction( &m_walkToPointAction );
	} else {
		m_climbOntoBarrierAction.setTargetReachNum( m_targetReachNum );
		selectActiveAction( &m_climbOntoBarrierAction );
	}

	if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
		m_timeoutAt = level.time + 3000;
		return true;
	}

	return false;
}