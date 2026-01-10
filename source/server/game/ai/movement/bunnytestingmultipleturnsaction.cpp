#include "bunnytestingmultipleturnsaction.h"
#include "movementlocal.h"
#include "movementsubsystem.h"

static constexpr float kMinAngularSpeed = 120.0f;
static constexpr float kMaxAngularSpeed = 270.0f;
static constexpr float kAngularSpeedRange = kMaxAngularSpeed - kMinAngularSpeed;

const float BunnyTestingMultipleTurnsAction::kAngularSpeed[kMaxAngles] = {
	kMinAngularSpeed, kMinAngularSpeed + 0.33f * kAngularSpeedRange,
	kMinAngularSpeed + 0.66f * kAngularSpeedRange, kMaxAngularSpeed
};

auto BunnyTestingMultipleTurnsAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	// This action is the first applied action as it is specialized
	// and falls back to other bunnying actions if it cannot be applied.
	if( const auto result = genericCheckIsActionEnabled( context ); result != PredictionResult::Continue ) {
		return result;
	}

	if( const auto result = checkCommonBunnyHopPreconditions( context ); result != PredictionResult::Continue ) {
		return result;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	vec3_t lookDir;
	if( context->totalMillisAhead ) {
		if( m_hasWalljumped && entityPhysicsState.Speed() > 1 ) {
			Vec3 velocityDir( entityPhysicsState.Velocity() );
			velocityDir *= 1.0f / entityPhysicsState.Speed();
			velocityDir.copyTo( lookDir );
		} else {
			if( context->frameEvents.hasWalljumped ) {
				// Keep rotating the look dir if a walljump happened at the very beginning of the path
				if( m_originAtSequenceStart.squareDistance2DTo( entityPhysicsState.Origin() ) > wsw::square( 32 ) ) {
					m_hasWalljumped = true;
				}
			}

			static_assert( kMaxAttempts == 2 * kMaxAngles );
			const float sign = ( m_attemptNum % 2 ) ? +1.0f : -1.0f;

			const float attemptAngularSpeed = kAngularSpeed[m_attemptNum / 2];
			constexpr const float invAngularSpeedRange = 1.0f / ( kMaxAngularSpeed - kMinAngularSpeed );
			// Defines how close the angular speed is to the max angular speed
			const float fracOfMaxSpeed = ( attemptAngularSpeed - kMinAngularSpeed ) * invAngularSpeedRange;

			const float timeSeconds = 0.001f * (float)context->totalMillisAhead;
			// Hack, scale the time prior to checks (this yields better results)
			float timeLike = 0.75f * timeSeconds;
			if( timeLike < 1.0f ) {
				// Change the angle slower for larger resulting turns
				timeLike = std::pow( timeLike, 0.5f + 0.5f * fracOfMaxSpeed );
			}

			mat3_t m;
			const float angle = ( sign * attemptAngularSpeed ) * timeLike;
			Matrix3_Rotate( axis_identity, angle, 0.0f, 0.0f, 1.0f, m );
			Matrix3_TransformVector( m, m_initialDir.data(), lookDir );
		}
	} else {
		Vec3 forwardDir( entityPhysicsState.ForwardDir() );
		if( !m_attemptNum ) {
			// Save the initial look dir for this bot and game frame
			forwardDir.copyTo( m_initialDir );
		}
		forwardDir.copyTo( lookDir );
	}

	if( !setupBunnyHopping( Vec3( lookDir ), context ) ) {
		return PredictionResult::Restart;
	}

	return PredictionResult::Continue;
}

void BunnyTestingMultipleTurnsAction::onApplicationSequenceStopped( PredictionContext *context,
																	SequenceStopReason stopReason,
																	unsigned stoppedAtFrameIndex ) {
	BunnyHopAction::onApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );
	if( stopReason != FAILED ) {
		return;
	}

	m_attemptNum++;
	if( m_attemptNum == kMaxAttempts ) {
		return;
	}

	// Allow the action application after the context rollback to savepoint
	m_disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
}