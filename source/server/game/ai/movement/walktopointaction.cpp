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

#include "walktopointaction.h"
#include "movementsubsystem.h"
#include "movementlocal.h"

static constexpr float kBaseMinDistanceFromTargetToDash = 108.0f;
static constexpr float kBaseMinDistanceFromTargetToJump = 96.0f;

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

	Vec3 dirToTargetPoint( Vec3( m_targetPoint ) - entityPhysicsState.Origin() );
	dirToTargetPoint.Z() = 0;
	const std::optional<float> maybeDistance2D = dirToTargetPoint.normalizeFast( { .minAcceptableLength = 1.0f } );
	if( !maybeDistance2D ) {
		return PredictionResult::Restart;
	}

	if( std::optional<Vec3> keptInFovPoint = m_subsystem->bot->GetKeptInFovPoint() ) {
		if( keptInFovPoint->SquareDistance2DTo( entityPhysicsState.Origin() ) > wsw::square( 1.0f ) ) {
			Vec3 lookVec( Vec3( entityPhysicsState.Origin() ) - *keptInFovPoint );
			lookVec.Z() *= 0.33f;

			if( entityPhysicsState.GroundEntity() ) {
				int keyMoves[2] { 0, 0 };
				context->TraceCache().makeKeyMovesToTarget( context, dirToTargetPoint, keyMoves );
				if( keyMoves[0] | keyMoves[1] ) {
					botInput->SetForwardMovement( keyMoves[0] );
					botInput->SetRightMovement( keyMoves[1] );
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
			if( entityPhysicsState.GroundEntity() ) {
				if( m_isDashingAllowed && !pmoveStats[PM_STAT_DASHTIME] && *maybeDistance2D >= m_minDistanceFromTargetToDash ) {
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
	if( entityPhysicsState.GroundEntity() ) {
		const Vec3 currMins( Vec3( playerbox_stand_mins ) + entityPhysicsState.Origin() );
		const Vec3 currMaxs( Vec3( playerbox_stand_maxs ) + entityPhysicsState.Origin() );
		// TODO: PointWithinBounds ()
		if( BoundsAndSphereIntersect( currMins.Data(), currMaxs.Data(), m_targetPoint.Data(), 1.0f ) ) {
			return PredictionResult::Complete;
		}
	} else {
		if( entityPhysicsState.IsHighAboveGround() ) {
			Debug( "The step has lead to being way too high above ground" );
			return PredictionResult::Restart;
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