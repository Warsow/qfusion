/*
Copyright (C) 2017-2025 vvk2212, Chasseur de bots

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

#include "movementscript.h"
#include "movementsubsystem.h"
#include "baseaction.h"
// TODO: Use absolute path?
#include "../bot.h"

#define Assert( expr ) assert( expr )

void PredictingMovementScript::onInterceptedPredictedEvent( int ev, int parm ) {
	m_activeContext->OnInterceptedPredictedEvent( ev, parm );
}

void PredictingMovementScript::onInterceptedPMoveTouchTriggers( pmove_t *pm, vec3_t const previousOrigin ) {
	m_activeContext->OnInterceptedPMoveTouchTriggers( pm, previousOrigin );
}

void PredictingAndCachingMovementScript::Debug( const char *format, ... ) const {
	va_list va;
	va_start( va, format );
	AI_Debugv( "PredictingAndCachingMovementScript", format, va );
	va_end( va );
}

bool PredictingAndCachingMovementScript::produceBotInput( BotInput *input ) {
	MovementActionRecord movementActionRecord;
	if( BaseAction *action = getCachedActionAndRecordForCurrTime( &movementActionRecord ) ) {
		action->ExecActionRecord( &movementActionRecord, input );
		return true;
	}

	assert( !m_movementActions.empty() );
	m_predictedMovementActions.clear();

	PredictionContext context( m_subsystem, &m_predictedMovementActions );
	m_activeContext = &context;
	const bool succeeded = context.BuildPlan( m_movementActions );
	m_activeContext = nullptr;

	if( succeeded ) {
		assert( !m_predictedMovementActions.empty() );
		BaseAction *action = getCachedActionAndRecordForCurrTime( &movementActionRecord );
		assert( action );
		action->ExecActionRecord( &movementActionRecord, input );
		return true;
	}

	return false;
}

auto PredictingAndCachingMovementScript::getCachedActionAndRecordForCurrTime( MovementActionRecord *record_ ) -> BaseAction * {
	const int64_t realTime = game.realtime;
	PredictedMovementAction *prevPredictedAction = nullptr;
	PredictedMovementAction *nextPredictedAction = nullptr;
	for( PredictedMovementAction &predictedAction: m_predictedMovementActions ) {
		if( predictedAction.timestamp >= realTime ) {
			nextPredictedAction = &predictedAction;
			break;
		}
		prevPredictedAction = &predictedAction;
	}

	if( !nextPredictedAction ) {
		Debug( "Cannot use predicted movement action: next one (its timestamp is not in the past) cannot be found\n" );
		return nullptr;
	}

	if( !prevPredictedAction ) {
		// If there were no activated actions, the next state must be recently computed for current level time.
		Assert( nextPredictedAction->timestamp == realTime );
		// These assertions have already spotted a bug
		const auto *self = game.edicts + m_subsystem->bot->EntNum();
		Assert( VectorCompare( nextPredictedAction->entityPhysicsState.Origin(), self->s.origin ) );
		Assert( VectorCompare( nextPredictedAction->entityPhysicsState.Velocity(), self->velocity ) );
		// If there is a modified velocity, it will be copied with this record and then applied
		*record_ = nextPredictedAction->record;
		Debug( "Using just computed predicted movement action %s\n", nextPredictedAction->action->Name() );
		return nextPredictedAction->action;
	}

	Assert( prevPredictedAction->timestamp + prevPredictedAction->stepMillis == nextPredictedAction->timestamp );

	return tryCheckingAndLerpingActions( prevPredictedAction, nextPredictedAction, record_ );
}

auto PredictingAndCachingMovementScript::tryCheckingAndLerpingActions( PredictedMovementAction *prevAction,
																	   PredictedMovementAction *nextAction,
																	   MovementActionRecord *record_ ) -> BaseAction * {
	const int64_t realTime = game.realtime;

	// Check whether predicted action is valid for an actual bot entity physics state
	auto checkLerpFrac = (float)( realTime - prevAction->timestamp );
	checkLerpFrac /= (float)( nextAction->timestamp - prevAction->timestamp );
	Assert( checkLerpFrac > 0.0f && checkLerpFrac <= 1.0f );

	const char *format =
		"Prev predicted action timestamp is " PRId64 ", "
		"next predicted action is " PRId64 ", real time is %" PRId64 "\n";

	Debug( format, prevAction->timestamp, nextAction->timestamp, level.time );
	Debug( "Should interpolate entity physics state using fraction %f\n", checkLerpFrac );

	// Prevent cache invalidation on each frame if bot is being hit by a continuous fire weapon and knocked back.
	// Perform misprediction test only on the 3rd frame after a last knockback timestamp.
	if( level.time - m_subsystem->bot->LastKnockbackAt() <= 32 ) {
		return lerpActionRecords( prevAction, nextAction, record_ );
	}

	if( !checkPredictedOrigin( prevAction, nextAction, checkLerpFrac ) ) {
		return nullptr;
	}

	if( !checkPredictedVelocity( prevAction, nextAction, checkLerpFrac ) ) {
		return nullptr;
	}

	if( !checkPredictedAngles( prevAction, nextAction, checkLerpFrac ) ) {
		return nullptr;
	}

	return lerpActionRecords( prevAction, nextAction, record_ );
}

bool PredictingAndCachingMovementScript::checkPredictedOrigin( PredictedMovementAction *prevAction,
															   PredictedMovementAction *nextAction,
															   float checkLerpFrac ) {
	const auto &prevPhysicsState = prevAction->entityPhysicsState;
	const auto &nextPhysicsState = nextAction->entityPhysicsState;

	vec3_t expectedOrigin;
	VectorLerp( prevPhysicsState.Origin(), checkLerpFrac, nextPhysicsState.Origin(), expectedOrigin );
	float squareDistanceMismatch = DistanceSquared( m_subsystem->bot->Origin(), expectedOrigin );
	if( squareDistanceMismatch < 3.0f * 3.0f ) {
		return true;
	}

	float distanceMismatch = Q_Sqrt( squareDistanceMismatch );
	const char *format_ = "Cannot use predicted movement action: distance mismatch %f is too high for lerp frac %f\n";
	Debug( format_, distanceMismatch, checkLerpFrac );
	return false;
}

bool PredictingAndCachingMovementScript::checkPredictedVelocity( PredictedMovementAction *prevAction,
																 PredictedMovementAction *nextAction,
																 float checkLerpFrac ) {
	const auto &prevPhysicsState = prevAction->entityPhysicsState;
	const auto &nextPhysicsState = nextAction->entityPhysicsState;

	float expectedSpeed = ( 1.0f - checkLerpFrac ) * prevPhysicsState.Speed() + checkLerpFrac * nextPhysicsState.Speed();
	float actualSpeed = m_subsystem->bot->EntityPhysicsState()->Speed();
	float mismatch = std::abs( actualSpeed - expectedSpeed );

	if( mismatch > 5.0f ) {
		Debug( "Expected speed: %.1f, actual speed: %.1f, speed mismatch: %.1f\n", expectedSpeed, actualSpeed, mismatch );
		Debug( "Cannot use predicted movement action: speed mismatch is too high\n" );
		return false;
	}

	if( actualSpeed < 30.0f ) {
		return true;
	}

	vec3_t expectedVelocity;
	VectorLerp( prevPhysicsState.Velocity(), checkLerpFrac, nextPhysicsState.Velocity(), expectedVelocity );
	Vec3 expectedVelocityDir( expectedVelocity );
	expectedVelocityDir.normalizeFastOrThrow();
	Vec3 actualVelocityDir( m_subsystem->bot->Velocity() );
	actualVelocityDir.normalizeFastOrThrow();

	float cosine = expectedVelocityDir.Dot( actualVelocityDir );
	static const float MIN_COSINE = std::cos( (float) DEG2RAD( 3.0f ) );
	if( cosine > MIN_COSINE ) {
		return true;
	}

	Debug( "An angle between expected and actual velocities is %f degrees\n", RAD2DEG( std::acos( cosine ) ) );
	Debug( "Cannot use predicted movement action:  expected and actual velocity directions differ significantly\n" );
	return false;
}

bool PredictingAndCachingMovementScript::checkPredictedAngles( PredictedMovementAction *prevAction,
															   PredictedMovementAction *nextAction,
															   float checkLerpFrac ) {
	// TODO: This does not seem to be viable
	if( nextAction->record.botInput.canOverrideLookVec ) {
		return true;
	}

	Vec3 prevStateAngles( prevAction->entityPhysicsState.Angles() );
	Vec3 nextStateAngles( nextAction->entityPhysicsState.Angles() );

	vec3_t expectedAngles;
	for( int i : { YAW, ROLL } ) {
		expectedAngles[i] = LerpAngle( prevStateAngles.Data()[i], nextStateAngles.Data()[i], checkLerpFrac );
	}

	if( !nextAction->record.botInput.canOverridePitch ) {
		expectedAngles[PITCH] = LerpAngle( prevStateAngles.Data()[PITCH], nextStateAngles.Data()[PITCH], checkLerpFrac );
	} else {
		expectedAngles[PITCH] = game.edicts[m_subsystem->bot->EntNum()].s.angles[PITCH];
	}

	vec3_t expectedLookDir;
	AngleVectors( expectedAngles, expectedLookDir, nullptr, nullptr );
	float cosine = m_subsystem->bot->EntityPhysicsState()->ForwardDir().Dot( expectedLookDir );
	static const float MIN_COSINE = std::cos( (float)DEG2RAD( 3.0f ) );
	if( cosine > MIN_COSINE ) {
		return true;
	}

	Debug( "An angle between and actual look directions is %f degrees\n", RAD2DEG( std::acos( cosine ) ) );
	Debug( "Cannot use predicted movement action: expected and actual look directions differ significantly\n" );
	return false;
}

auto PredictingAndCachingMovementScript::lerpActionRecords( PredictedMovementAction *prevAction,
															PredictedMovementAction *nextAction,
															MovementActionRecord *record_ ) -> BaseAction * {
	const int64_t realTime = game.realtime;

	// If next predicted state is likely to be completed next frame, use its input as-is (except the velocity)
	if( nextAction->timestamp - realTime <= game.frametime ) {
		*record_ = nextAction->record;
		// Apply modified velocity only once for an exact timestamp
		if( nextAction->timestamp != realTime ) {
			record_->hasModifiedVelocity = false;
		}
		return nextAction->action;
	}

	const float inputLerpFrac = game.frametime / ( (float)( nextAction->timestamp - realTime ) );
	Assert( inputLerpFrac > 0 && inputLerpFrac <= 1.0f );
	// If next predicted time is likely to be pending next frame again, interpolate input for a single frame ahead
	*record_ = nextAction->record;
	// Prevent applying a modified velocity from the new state
	record_->hasModifiedVelocity = false;

	if( !record_->botInput.canOverrideLookVec ) {
		Vec3 actualLookDir( m_subsystem->bot->EntityPhysicsState()->ForwardDir() );
		Vec3 intendedLookVec( record_->botInput.IntendedLookDir() );
		VectorLerp( actualLookDir.Data(), inputLerpFrac, intendedLookVec.Data(), intendedLookVec.Data() );
		record_->botInput.SetIntendedLookDir( intendedLookVec );
	}

	auto prevRotationMask = (unsigned)prevAction->record.botInput.allowedRotationMask;
	auto nextRotationMask = (unsigned)nextAction->record.botInput.allowedRotationMask;
	record_->botInput.allowedRotationMask = (InputRotation)( prevRotationMask & nextRotationMask );

	return nextAction->action;
}
