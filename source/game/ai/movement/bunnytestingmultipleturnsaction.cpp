#include "bunnytestingmultipleturnsaction.h"
#include "movementlocal.h"
#include "movementmodule.h"

const float BunnyTestingMultipleTurnsAction::kAngularSpeed[kMaxAngles] = {
	30.0f, 60.0f, 90.0f, 150.0f, 210.0f
};

void BunnyTestingMultipleTurnsAction::PlanPredictionStep( MovementPredictionContext *context ) {
	// This action is the first applied action as it is specialized
	// and falls back to other bunnying actions if it cannot be applied.
	if( !GenericCheckIsActionEnabled( context ) ) {
		return;
	}

	if( !CheckCommonBunnyHopPreconditions( context ) ) {
		return;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	vec3_t lookDir;
	if( context->totalMillisAhead ) {
		if( hasWalljumped && entityPhysicsState.Speed() > 1 ) {
			Vec3 velocityDir( entityPhysicsState.Velocity() );
			velocityDir *= 1.0f / entityPhysicsState.Speed();
			velocityDir.CopyTo( lookDir );
		} else {
			if( context->frameEvents.hasWalljumped ) {
				// Keep rotating the look dir if a walljump happened at the very beginning of the path
				if( originAtSequenceStart.SquareDistance2DTo( entityPhysicsState.Origin() ) > SQUARE( 32 ) ) {
					hasWalljumped = true;
				}
			}

			float timeLike = 0.001f * context->totalMillisAhead;
			static_assert( kMaxAttempts == 2 * kMaxAngles );
			const float sign = attemptNum % 2 ? +1.0f : -1.0f;
			timeLike = timeLike < 1.0 ? Q_Sqrt( Q_Sqrt( timeLike ) ) : 1.0f;
			const float angle = ( sign * kAngularSpeed[attemptNum / 2] ) * timeLike;

			mat3_t m;
			Matrix3_Rotate( axis_identity, angle, 0.0f, 0.0f, 1.0f, m );
			Matrix3_TransformVector( m, initialDir.Data(), lookDir );
		}
	} else {
		Vec3 forwardDir( entityPhysicsState.ForwardDir() );
		if( !attemptNum ) {
			// Save the initial look dir for this bot and game frame
			forwardDir.CopyTo( initialDir );
		}
		forwardDir.CopyTo( lookDir );
	}

	if( !SetupBunnyHopping( Vec3( lookDir ), context ) ) {
		return;
	}
}

void BunnyTestingMultipleTurnsAction::OnApplicationSequenceStopped( MovementPredictionContext *context,
																	SequenceStopReason stopReason,
																	unsigned stoppedAtFrameIndex ) {
	BunnyHopAction::OnApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );
	if( stopReason != FAILED ) {
		return;
	}

	attemptNum++;
	if( attemptNum == kMaxAttempts ) {
		return;
	}

	// Allow the action application after the context rollback to savepoint
	disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
	// Ensure this action will be used after rollback
	context->SaveSuggestedActionForNextFrame( this );
}