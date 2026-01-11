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
#include "triggeraaspropscache.h"
#include <common/helpers/algorithm.h>

[[nodiscard]]
static bool contactsTargetPoint( const AiEntityPhysicsState &entityPhysicsState, const Vec3 &targetPoint ) {
	const Vec3 botMins( Vec3( playerbox_stand_mins ) + entityPhysicsState.Origin() );
	const Vec3 botMaxs( Vec3( playerbox_stand_maxs ) + entityPhysicsState.Origin() );
	return BoundsAndSphereIntersect( botMins.data(), botMaxs.data(), targetPoint.data(), 1.0f );
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
	m_startPoint.set( context->movementState->entityPhysicsState.Origin() );
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
	viewOrigin.z() += playerbox_stand_viewheight;

	Vec3 intendedLookDir( ( Vec3( 0, 0, 24 ) + m_targetPoint ) - viewOrigin );
	if( !intendedLookDir.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
		intendedLookDir = Vec3( 0, 0, 1 );
	}

	botInput->SetIntendedLookDir( intendedLookDir, true );

	if( entityPhysicsState.GroundEntity() ) {
		if( !m_hasJumped ) {
			if( intendedLookDir.dot( entityPhysicsState.ForwardDir() ) > 0.95f ) {
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
			if( intendedLookDir.dot( entityPhysicsState.ForwardDir() ) > 0.7f ) {
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
		context->SaveGoodEnoughPath( 1, (unsigned)targetPoint.fastDistanceTo( entityPhysicsState.Origin() ) );
		return true;
	}

	const auto *const aasWorld = AiAasWorld::instance();
	if( const auto targetClusterNum = aasWorld->floorClusterNum( targetAreaNum ) ) {
		for( int i = 0; i < numCurrAreas; ++i ) {
			if( targetClusterNum == aasWorld->floorClusterNum( currAreaNums[i] ) ) {
				context->SaveLastResortPath( (unsigned)targetPoint.fastDistanceTo( entityPhysicsState.Origin() ) );
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
		if( m_targetPoint.squareDistanceTo( entityPhysicsState.Origin() ) <
			m_startPoint.squareDistanceTo( entityPhysicsState.Origin() ) ) {
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
	VectorMA( targetReach.start, -16.0f, reachDir.data(), realReachStart );

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
	m_distanceFromStartToTarget = m_targetPoint.fastDistance2DTo( context->movementState->entityPhysicsState.Origin() );
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
	viewOrigin.z() += playerbox_stand_viewheight;

	Vec3 dirToTargetPoint( Vec3( m_targetPoint ) - viewOrigin );
	dirToTargetPoint.z() = 0;
	const std::optional<float> maybeDistance2D = dirToTargetPoint.normalizeFast( { .minAcceptableLength = 1.0f } );
	if( !maybeDistance2D ) {
		return PredictionResult::Restart;
	}

	if( std::optional<Vec3> keptInFovPoint = m_subsystem->bot->GetKeptInFovPoint() ) {
		if( keptInFovPoint->squareDistance2DTo( entityPhysicsState.Origin() ) > wsw::square( 1.0f ) ) {
			Vec3 lookVec( Vec3( entityPhysicsState.Origin() ) - *keptInFovPoint );
			lookVec.z() *= Z_NO_BEND_SCALE;

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
		if( dirToTargetPoint.dot( entityPhysicsState.ForwardDir() ) > 0.95f ) {
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
								if( velocityDir.dot( dirToTargetPoint ) > 0.9f ) {
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

	const float squareDistance2D = m_targetPoint.fastDistance2DTo( entityPhysicsState.Origin() );
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
	viewOrigin.z() += playerbox_stand_viewheight;

	Vec3 intendedLookDir3D( m_targetPoint - viewOrigin );
	if( intendedLookDir3D.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
		Vec3 intendedLookDir2D( intendedLookDir3D.x(), intendedLookDir3D.y(), 0.0f );
		if( m_tryUsingDirectionKeys ) {
			constexpr float dotThreshold = 0.9f;
			if( intendedLookDir2D.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
				const float intendedDotForward = intendedLookDir2D.dot( entityPhysicsState.ForwardDir() );
				if( std::fabs( intendedDotForward ) > dotThreshold ) {
					botInput->SetForwardMovement( intendedDotForward > 0.0f ? +1 : -1 );
					botInput->SetIntendedLookDir( intendedLookDir2D );
				} else {
					const float intendedDotRight = intendedLookDir2D.dot( entityPhysicsState.RightDir() );
					if( std::fabs( intendedDotRight ) > dotThreshold ) {
						botInput->SetRightMovement( intendedDotRight > 0.0f ? +1 : -1 );
						botInput->SetIntendedLookDir( intendedLookDir2D );
					}
				}
			}
		}
		if( !( botInput->ForwardMovement() | botInput->RightMovement() ) ) {
			botInput->SetIntendedLookDir( intendedLookDir3D, true );
			if( intendedLookDir3D.dot( entityPhysicsState.ForwardDir() ) > 0.3f ) {
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
					if( entityPhysicsState.ForwardDir().z() > 0.9f ) {
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
			viewOrigin.z() += playerbox_stand_viewheight;

			Vec3 intendedLookDir( Vec3( aasWorld->getAreas()[targetReach.areanum].center ) - viewOrigin );
			if( intendedLookDir.normalizeFast() ) {
				botInput->SetIntendedLookDir( intendedLookDir, true );
				if( intendedLookDir.dot( entityPhysicsState.ForwardDir() ) > 0.9f ) {
					botInput->SetForwardMovement( 1 );
				}
			} else {
				return PredictionResult::Restart;
			}
		}
	} else {
		Vec3 viewOrigin( entityPhysicsState.Origin() );
		viewOrigin.z() += playerbox_stand_viewheight;

		Vec3 intendedLookDir( viewOrigin );
		const auto &targetArea = aasWorld->getAreas()[targetReach.areanum];
		intendedLookDir -= targetArea.center;
		if( !intendedLookDir.normalizeFast() ) {
			intendedLookDir = viewOrigin - Vec3( targetArea.center[0], targetArea.center[1], targetArea.mins[2] );
			intendedLookDir.normalizeFastOrThrow();
		}

		intendedLookDir *= -1;
		if( intendedLookDir.dot( entityPhysicsState.ForwardDir() ) > 0.9f ) {
			botInput->SetForwardMovement( 1 );
		} else {
			botInput->SetTurnSpeedMultiplier( 10.0f );
		}
		intendedLookDir.z() *= 2.0f;
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

bool JumppadScript::produceBotInput( BotInput *input ) {
	const auto &entityPhysicsState = *m_subsystem->bot->EntityPhysicsState();
	// TODO: Is the jumppad entity considered to be "ground entity"?
	// TODO: Is entityPhysicsState up-to-date when we activate this script?
	if( entityPhysicsState.GroundEntity() ) {
		return false;
	}

	const auto *const aasWorld = AiAasWorld::instance();

	[[maybe_unused]]
	const float *const triggerTargetOrigin = game.edicts[m_triggerEntNum].target_ent->s.origin;

	if( m_targetReachNum > 0 ) {
		const auto &targetReach = aasWorld->getReaches()[m_targetReachNum];
		const auto &targetArea  = aasWorld->getAreas()[targetReach.areanum];
		// If the bot should be landing (the feet are above the target area or the origin is above the reach end point)
		if( entityPhysicsState.Origin()[2] + playerbox_stand_mins[2] > targetArea.mins[2] ||
			entityPhysicsState.Origin()[2] > targetReach.end[2] ) {
			// Prefer the target area even if we have failed on a previous attempt
			if( m_lastGoodLandingAreaNum == targetReach.areanum ) {
				if( reuseCachedPathForLastGoodArea( input ) ) {
					return true;
				}
			}
			if( tryLandingOnArea( targetReach.areanum, input ) ) {
				return true;
			}
			// If we're still flying upwards
			if( entityPhysicsState.Velocity()[2] > 0 ) {
				// Try to continue controlled flight
				if( setupFreeflyMovement( input, triggerTargetOrigin, entityPhysicsState ) ) {
					return true;
				}
				// Just wait for starting falling
				input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
				input->isUcmdSet = true;
				return true;
			} else {
				// Try landing on arbitrary areas
				std::span<const uint16_t> jumppadTargetAreas = ::triggerAasPropsCache.getJumppadTargetAreas( m_triggerEntNum );
				if( tryLandingOnAreas( jumppadTargetAreas, targetReach.areanum, input ) ) {
					return true;
				}
				// Just wait for irrecoverable falling
				input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
				input->isUcmdSet = true;
				return true;
			}
			return false;
		} else {
			return setupNonLandingMovement( input, triggerTargetOrigin, entityPhysicsState );
		}
	} else {
		// Note: In case of no target areas we fall back to "non-landing" movement
		float minTargetAreaHeight = std::numeric_limits<float>::max();
		for( const int areaNum : ::triggerAasPropsCache.getJumppadTargetAreas( m_triggerEntNum ) ) {
			minTargetAreaHeight = wsw::min( minTargetAreaHeight, aasWorld->getAreas()[areaNum].mins[2] );
		}
		if( entityPhysicsState.Origin()[2] + playerbox_stand_mins[2] > minTargetAreaHeight ) {
			if( reuseCachedPathForLastGoodArea( input ) ) {
				return true;
			}
			if( m_lastGoodLandingAreaNum > 0 ) {
				if( tryLandingOnArea( m_lastGoodLandingAreaNum, input ) ) {
					return true;
				}
			}
			std::span<const uint16_t> jumppadTargetAreas = ::triggerAasPropsCache.getJumppadTargetAreas( m_triggerEntNum );
			if( tryLandingOnAreas( jumppadTargetAreas, m_lastGoodLandingAreaNum, input ) ) {
				return true;
			}
			input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
			input->isUcmdSet = true;
			return true;
		} else {
			return setupNonLandingMovement( input, triggerTargetOrigin, entityPhysicsState );
		}
	}
}

bool JumppadScript::setupNonLandingMovement( BotInput *input, const float *triggerTargetOrigin,
											 const AiEntityPhysicsState &entityPhysicsState ) {
	if( entityPhysicsState.Velocity()[2] > 0 ) {
		if( setupFreeflyMovement( input, triggerTargetOrigin, entityPhysicsState ) ) {
			return true;
		}
		// Just wait for irrecoverable falling
		input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
		input->isUcmdSet = true;
		return true;
	} else {
		if( setupRestartTriggerMovement( input, entityPhysicsState ) ) {
			return true;
		}
		// Just wait for falling somewhere (TODO?)
		input->SetIntendedLookDir( Vec3( 0, 0, -1 ), true );
		input->isUcmdSet = true;
		return true;
	}
}

bool JumppadScript::reuseCachedPathForLastGoodArea( BotInput *input ) {
	if( !m_predictedMovementActions.empty() ) {
		if( m_predictedMovementActions.front().action == &m_landOnPointAction ) {
			if( m_lastGoodLandingAreaNum == m_landOnPointAction.getTargetAreaNum() ) {
				MovementActionRecord record {};
				if( getCachedActionAndRecordForCurrTime( &record ) ) {
					m_predictedMovementActions.front().action->execActionRecord( &record, input );
					return true;
				}
			}
		}
	}
	return false;
}

bool JumppadScript::tryLandingOnArea( int areaNum, BotInput *input ) {
	const auto &area = AiAasWorld::instance()->getAreas()[areaNum];
	Vec3 areaPoint( area.center[0], area.center[1], area.mins[2] + 32.0f );

	m_landOnPointAction.setTarget( areaPoint, areaNum );
	m_predictedMovementActions.clear();
	selectActiveAction( &m_landOnPointAction );

	if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
		m_lastGoodLandingAreaNum = areaNum;
		return true;
	}

	return false;
}

bool JumppadScript::tryLandingOnAreas( std::span<const uint16_t> areaNums, int skipAreaNum, BotInput *input ) {
	for( const int areaNum: areaNums ) {
		if( areaNum != skipAreaNum ) [[likely]] {
			if( tryLandingOnArea( areaNum, input ) ) {
				return true;
			}
		}
	}
	return false;
}

bool JumppadScript::setupFreeflyMovement( BotInput *input, const float *targetTriggerOrigin,
										  const AiEntityPhysicsState &entityPhysicsState ) {

	// TODO: Check distance of origin to the reach segment

	Vec3 viewOrigin( entityPhysicsState.Origin() );
	viewOrigin.z() += playerbox_stand_viewheight;
	if( const auto maybePoint = m_subsystem->bot->GetKeptInFovPoint() ) {
		Vec3 intendedLookDir( *maybePoint - viewOrigin );
		if( intendedLookDir.normalizeFast() ) {
			input->SetIntendedLookDir( intendedLookDir, true );
		} else {
			input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
		}
	} else {
		Vec3 intendedLookDir( Vec3( targetTriggerOrigin ) - viewOrigin );
		if( intendedLookDir.normalizeFast() ) {
			input->SetIntendedLookDir( intendedLookDir, true );
		} else {
			input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
		}
	}

	input->isUcmdSet          = true;
	input->canOverridePitch   = true;
	input->canOverrideLookVec = true;
	return true;
}

bool JumppadScript::setupRestartTriggerMovement( BotInput *input, const AiEntityPhysicsState &entityPhysicsState ) {
	Vec3 viewOrigin( entityPhysicsState.Origin() );
	viewOrigin.z() += playerbox_stand_viewheight;

	const auto *trigger = &game.edicts[m_triggerEntNum];
	const Vec3 triggerOrigin( 0.5f * ( Vec3( trigger->r.absmin ) + Vec3( trigger->r.absmax ) ) );

	Vec3 intendedLookDir( triggerOrigin - viewOrigin );
	if( intendedLookDir.normalizeFast() ) {
		input->SetIntendedLookDir( intendedLookDir, true );
	} else {
		input->SetIntendedLookDir( Vec3( 0, 0, -1 ), true );
	}

	input->isUcmdSet        = true;
	input->canOverridePitch = true;
	input->canOverrideUcmd  = true;
	return true;
}

bool ElevatorScript::produceBotInput( BotInput *input ) {
	const auto &entityPhysicsState = m_subsystem->getMovementState().entityPhysicsState;
	const edict_t *platformEntity  = nullptr;
	if( const auto *groundEntity = entityPhysicsState.GroundEntity() ) {
		if( groundEntity->use == Use_Plat ) {
			platformEntity = groundEntity;
		}
	} else {
		trace_t trace;
		// TODO: Limit trace bounds
		Vec3 traceEnd( Vec3( 0, 0, -99999 ) + entityPhysicsState.Origin() );
		G_Trace( &trace, entityPhysicsState.Origin(), nullptr, nullptr, traceEnd.data(),
				 game.edicts + m_subsystem->bot->EntNum(), MASK_SOLID );
		if( trace.fraction != 1.0f && !trace.allsolid && !trace.startsolid ) {
			const edict_t *entity = game.edicts + trace.ent;
			if( entity->use == Use_Plat ) {
				platformEntity = entity;
			}
		}
	}
	if( !platformEntity ) {
		return false;
	}
	if( platformEntity->moveinfo.state == STATE_TOP ) {
		return setupExitPlatformMovement( input, platformEntity, entityPhysicsState );
	} else {
		return setupRidePlatformMovement( input, platformEntity, entityPhysicsState );
	}
}

bool ElevatorScript::setupExitPlatformMovement( BotInput *input, const edict_t *platformEntity,
												const AiEntityPhysicsState &entityPhysicsState ) {
	BaseAction *appropriateAction;
	if( entityPhysicsState.GroundEntity() ) {
		assert( entityPhysicsState.GroundEntity() == platformEntity );
		appropriateAction = &m_walkToPointAction;
	} else {
		appropriateAction = &m_landOnPointAction;
	}

	const auto *const aasWorld = AiAasWorld::instance();
	if( m_targetReachNum > 0 ) {
		const auto &targetReach = aasWorld->getReaches()[m_targetReachNum];
		// Prefer the target area even if we've failed
		if( m_lastGoodExitAreaNum == targetReach.areanum ) {
			if( reuseCachedPathForLastGoodResult( input, appropriateAction ) ) {
				return true;
			}
		}
		if( tryMovingToArea( appropriateAction, targetReach.areanum, input ) ) {
			return true;
		}
		wsw::StaticVector<int, 2> areasToSkip;
		areasToSkip.push_back( targetReach.areanum );
		if( m_lastGoodExitAreaNum > 0 && m_lastGoodExitAreaNum != targetReach.areanum ) {
			if( tryMovingToArea( appropriateAction, m_lastGoodExitAreaNum, input ) ) {
				return true;
			}
			areasToSkip.push_back( m_lastGoodExitAreaNum );
		}
		std::span<const uint16_t> areasToTest = ::triggerAasPropsCache.getElevatorTargetAreas( m_triggerEntNum );
		if( tryMovingToAreas( appropriateAction, areasToTest, areasToSkip, input ) ) {
			return true;
		}
		if( entityPhysicsState.GroundEntity() ) {
			const Vec3 reachEnd( targetReach.end );
			m_predictedMovementActions.clear();
			selectActiveAction( &m_walkToPointAction );
			m_walkToPointAction.setTargetPoint( reachEnd );
			if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
				m_lastGoodExitAreaNum = -1;
				m_lastGoodExitOrigin  = reachEnd;
				return true;
			}
			Vec3 viewOrigin( entityPhysicsState.Origin() );
			viewOrigin.z() += playerbox_stand_viewheight;
			Vec3 intendedLookDir( reachEnd - viewOrigin );
			if( intendedLookDir.normalizeFast() ) {
				input->SetIntendedLookDir( intendedLookDir, true );
				input->isUcmdSet = true;
				if( intendedLookDir.dot( entityPhysicsState.ForwardDir() ) > 0.9f ) {
					input->SetForwardMovement( 1 );
				}
				return true;
			}
			return false;
		} else {
			// Try landing
			input->SetIntendedLookDir( Vec3( 0, 0, -1 ), true );
			input->isUcmdSet          = true;
			input->canOverrideLookVec = true;
			input->canOverridePitch   = true;
			return true;
		}
	} else {
		if( reuseCachedPathForLastGoodResult( input, appropriateAction ) ) {
			return true;
		}
		wsw::StaticVector<int, 1> areasToSkip;
		if( m_lastGoodExitAreaNum > 0 ) {
			if( tryMovingToArea( appropriateAction, m_lastGoodExitAreaNum, input ) ) {
				return true;
			}
			areasToSkip.push_back( m_lastGoodExitAreaNum );
		}
		std::span<const uint16_t> areasToTest = ::triggerAasPropsCache.getElevatorTargetAreas( m_triggerEntNum );
		if( tryMovingToAreas( appropriateAction, areasToTest, areasToSkip, input ) ) {
			return true;
		}
	}

	return false;
}

bool ElevatorScript::setupRidePlatformMovement( BotInput *input, const edict_t *platformEntity,
												const AiEntityPhysicsState &entityPhysicsState ) {
	if( entityPhysicsState.GroundEntity() ) {
		assert( entityPhysicsState.GroundEntity() == platformEntity );
		if( platformEntity->moveinfo.state == STATE_BOTTOM ) {
			const Vec3 botMins( Vec3( playerbox_stand_mins ) + entityPhysicsState.Origin() );
			const Vec3 botMaxs( Vec3( playerbox_stand_maxs ) + entityPhysicsState.Origin() );
			const edict_t *const triggerEntity = platformEntity->enemy;
			assert( triggerEntity == game.edicts + m_triggerEntNum );
			if( !GClip_EntityContact( botMins.data(), botMaxs.data(), triggerEntity ) ) {
				// TODO: Account for kept in fov point
				Vec3 viewOrigin( entityPhysicsState.Origin() );
				viewOrigin.z() += playerbox_stand_viewheight;
				Vec3 triggerOrigin( 0.5f * ( Vec3( triggerEntity->r.absmin ) + Vec3( triggerEntity->r.absmax ) ) );
				Vec3 intendedLookDir( triggerOrigin - viewOrigin );
				if( intendedLookDir.normalizeFast() ) {
					input->SetIntendedLookDir( intendedLookDir, true );
					if( intendedLookDir.dot( entityPhysicsState.ForwardDir() ) > 0.9f ) {
						input->SetForwardMovement( true );
					} else {
						input->SetTurnSpeedMultiplier( 3.0f );
					}
					input->isUcmdSet = true;
				} else {
					aiWarning() << "Failed to normalize the dir to the target trigger";
				}
			}
		}
		if( !input->isLookDirSet ) {
			input->SetIntendedLookDir( Vec3( 0, 0, 1 ), true );
			input->isUcmdSet          = true;
			input->canOverrideLookVec = true;
			input->canOverridePitch   = true;
		}
	} else {
		Vec3 viewOrigin( entityPhysicsState.Origin() );
		viewOrigin.z() += playerbox_stand_viewheight;
		if( const std::optional<Vec3> maybeKeptInFovPoint = m_subsystem->bot->GetKeptInFovPoint() ) {
			Vec3 intendedLookDir( *maybeKeptInFovPoint - viewOrigin );
			if( intendedLookDir.normalizeFast() ) {
				input->SetIntendedLookDir( intendedLookDir, true );
			}
		}
		if( !input->isLookDirSet && m_targetReachNum > 0 ) {
			Vec3 intendedLookDir( Vec3( AiAasWorld::instance()->getReaches()[m_targetReachNum].end ) - viewOrigin );
			if( intendedLookDir.normalizeFast() ) {
				input->SetIntendedLookDir( intendedLookDir, true );
			}
		}
		if( !input->isLookDirSet ) {
			input->SetIntendedLookDir( Vec3( 0, 0, -1 ), true );
		}
		input->isUcmdSet          = true;
		input->canOverrideLookVec = true;
		input->canOverridePitch   = true;
	}

	return true;
}

// TODO: Split it into two subroutines?
bool ElevatorScript::reuseCachedPathForLastGoodResult( BotInput *input, BaseAction *appropriateAction ) {
	assert( appropriateAction == &m_walkToPointAction || appropriateAction == &m_landOnPointAction );
	if( !m_predictedMovementActions.empty() ) {
		if( m_predictedMovementActions.front().action == appropriateAction ) {
			if( appropriateAction == &m_landOnPointAction ) {
				if( m_lastGoodExitAreaNum == m_landOnPointAction.getTargetAreaNum() ) {
					MovementActionRecord record {};
					if( getCachedActionAndRecordForCurrTime( &record ) ) {
						m_landOnPointAction.execActionRecord( &record, input );
						return true;
					}
				}
			} else {
				// If we have been using this origin
				// TODO: Handle land->walk transitions
				if( m_lastGoodExitOrigin == m_walkToPointAction.getTargetPoint() ) {
					MovementActionRecord record {};
					if( getCachedActionAndRecordForCurrTime( &record ) ) {
						m_walkToPointAction.execActionRecord( &record, input );
						return true;
					}
				}
			}
		}
	}
	return false;
}

bool ElevatorScript::tryMovingToArea( BaseAction *appropriateAction, int areaNum, BotInput *input ) {
	assert( appropriateAction == &m_walkToPointAction || appropriateAction == &m_landOnPointAction );
	m_predictedMovementActions.clear();
	selectActiveAction( appropriateAction );
	const auto &area = AiAasWorld::instance()->getAreas()[areaNum];
	Vec3 targetPoint( area.center[0], area.center[1], area.mins[2] + 32.0f );
	if( appropriateAction == &m_walkToPointAction ) {
		m_walkToPointAction.setTargetPoint( targetPoint );
		if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
			m_lastGoodExitAreaNum = areaNum;
			m_lastGoodExitOrigin  = targetPoint;
			return true;
		}
	} else {
		m_landOnPointAction.setTarget( targetPoint, areaNum );
		if( PredictingAndCachingMovementScript::produceBotInput( input ) ) {
			m_lastGoodExitAreaNum = areaNum;
			// TODO: std::optional?
			m_lastGoodExitOrigin  = Vec3( 0, 0, 0 );
			return true;
		}
	}
	return false;
}

bool ElevatorScript::tryMovingToAreas( BaseAction *appropriateAction, std::span<const uint16_t> areasToTest,
									   std::span<int> areasToSkip, BotInput *input ) {
	for( const int areaNum: areasToTest ) {
		if( !wsw::contains( areasToSkip, areaNum ) ) [[likely]] {
			if( tryMovingToArea( appropriateAction, areaNum, input ) ) {
				return true;
			}
		}
	}
	return false;
}

void WaitForLandingRelaxedAction::onApplicationSequenceStarted( PredictionContext *context ) {
	BaseAction::onApplicationSequenceStarted( context );
	m_travelTimeAtSequenceStart = context->TravelTimeToNavTarget();
}

void WaitForLandingRelaxedAction::onApplicationSequenceStopped( PredictionContext *context,
																SequenceStopReason sequenceStopReason,
																unsigned stoppedAtFrameIndex ) {
	BaseAction::onApplicationSequenceStopped( context, sequenceStopReason, stoppedAtFrameIndex );
	if( sequenceStopReason != SUCCEEDED ) {
		m_isDisabledForPlanning = true;
	}
}

[[nodiscard]]
auto WaitForLandingRelaxedAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( m_isDisabledForPlanning ) {
		return PredictionResult::Abort;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *const botInput           = &context->record->botInput;

	if( const std::optional<Vec3> maybeKeptInFovPoint = m_bot->GetKeptInFovPoint() ) {
		Vec3 viewOrigin( entityPhysicsState.Origin() );
		viewOrigin.z() += playerbox_stand_viewheight;
		Vec3 intendedLookDir( *maybeKeptInFovPoint - viewOrigin );
		if( intendedLookDir.normalizeFast() ) {
			botInput->SetIntendedLookDir( intendedLookDir, true );
		}
	}

	if( !botInput->isLookDirSet ) {
		botInput->SetIntendedLookDir( entityPhysicsState.ForwardDir(), true );
	}

	botInput->canOverridePitch = true;
	botInput->canOverrideUcmd  = true;
	botInput->isUcmdSet        = true;

	return PredictionResult::Continue;
}

[[nodiscard]]
auto WaitForLandingRelaxedAction::checkPredictionStepResults( PredictionContext *context ) -> PredictionResult {
	if( const auto result = BaseAction::checkPredictionStepResults( context ); result != PredictionResult::Continue ) {
		return result;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	// This condition accounts for landing in lava/slime as well
	// TODO: Does the base method account for this?
	if( entityPhysicsState.waterLevel > 1 ) {
		return PredictionResult::Abort;
	}

	if( entityPhysicsState.GroundEntity() ) {
		if( m_travelTimeAtSequenceStart ) {
			const int currTravelTimeToTarget = context->TravelTimeToNavTarget();
			if( !currTravelTimeToTarget ) {
				return PredictionResult::Abort;
			}
			if( currTravelTimeToTarget > m_travelTimeAtSequenceStart ) {
				return PredictionResult::Abort;
			}
		}
		return PredictionResult::Complete;
	} else {
		// Try detecting bumping into obstactles
		const auto &oldEntityPhysicsState = context->PhysicsStateBeforeStep();
		if( oldEntityPhysicsState.Speed2D() > 100 && entityPhysicsState.Speed2D() < 50 ) {
			return PredictionResult::Abort;
		}
	}

	if( context->topOfStackIndex + 2 < MAX_PREDICTED_STATES ) {
		return PredictionResult::Continue;
	}

	return PredictionResult::Abort;
}

bool WaitForLandingRelaxedScript::produceBotInput( BotInput *input ) {
	const auto &entityPhysicsState = m_subsystem->getMovementState().entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		return false;
	}

	return produceNonCachedInputUsingAction( &m_waitForLandingRelaxedAction, input );
}

bool LandToPreventFallingScript::produceBotInput( BotInput *input ) {
	const auto &entityPhysicsState = m_subsystem->getMovementState().entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		return false;
	}

	Bot *const bot = m_subsystem->bot;
	if( const int navTargetAreaNum = bot->NavTargetAasAreaNum() ) {
		struct Walker : public ReachChainWalker {
			Walker( const AiAasRouteCache *routeCache_, int targetAreaNum_, const float *currOrigin_ ) :
				ReachChainWalker( routeCache_, targetAreaNum_ ), currOrigin( currOrigin_ ) {}
			bool Accept( int reachNum, const aas_reachability_t &reach, int travelTime ) override {
				if( !collectedPointsAndAreas.full() && numHops < 16 ) {
					const auto &area = AiAasWorld::instance()->getAreas()[reach.areanum];
					// TODO: Check whether we can really land using basic physics
					constexpr float squareDistanceThreshold = wsw::square( 512.0f );
					if( area.mins[2] < currOrigin.z() ) {
						if( currOrigin.squareDistance2DTo( reach.start ) < squareDistanceThreshold ) {
							collectedPointsAndAreas.push_back( { Vec3( reach.start ), reach.areanum } );
						}
						if( !collectedPointsAndAreas.full() ) {
							const Vec3 areaPoint( area.center[0], area.center[1], area.mins[2] + 32 );
							if( currOrigin.squareDistance2DTo( areaPoint ) < squareDistanceThreshold ) {
								collectedPointsAndAreas.push_back( { areaPoint, reach.areanum } );
							}
						}
					}
					numHops++;
					return true;
				}
				return false;
			}
			wsw::StaticVector<std::pair<Vec3, int>, 20> collectedPointsAndAreas;
			unsigned numHops { 0 };
			Vec3 currOrigin;
		};

		Walker walker( bot->RouteCache(), bot->TravelFlags(), entityPhysicsState.Origin() );
		walker.SetAreaNums( entityPhysicsState, navTargetAreaNum );
		if( walker.Exec() ) {
			for( const auto &[areaPoint, areaNum]: walker.collectedPointsAndAreas ) {
				m_landOnPointAction.setTarget( areaPoint, areaNum );
				// TODO: Let the land action be capable of handling multiple points using restarts
				if( produceNonCachedInputUsingAction( &m_landOnPointAction, input ) ) {
					return true;
				}
			}
		}
	}

	if( const int areaNum = entityPhysicsState.DroppedToFloorAasAreaNum() ) {
		// TODO: Is this check needed?
		if( AiAasWorld::instance()->getAreaSettings()[areaNum].areaflags & AREA_GROUNDED ) {
			const auto &area = AiAasWorld::instance()->getAreas()[areaNum];
			// Helps with the quad ramp @ wdm6
			m_landOnPointAction.setTarget( Vec3( area.center[0], area.center[1], area.mins[2] + 32 ), areaNum );
			if( produceNonCachedInputUsingAction( &m_landOnPointAction, input ) ) {
				return true;
			}
		}
	}

	return false;
}