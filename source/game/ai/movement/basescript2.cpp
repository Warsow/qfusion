#include "basescript2.h"

BaseScript2::BaseScript2( BotMovementModule *module )
	: m_module( module )
	, m_cachedPath( &module->m_sharedCachedPath ) {}

[[nodiscard]]
auto BaseScript2::getActionAndRecordForCurrGameState( MovementActionRecord *record ) -> BaseMovementAction * {
	BaseMovementAction *action = getCachedActionAndRecordForCurrTime( record );
	if( action ) {
		return action;
	}

	m_cachedPath->clear();
	MovementPredictionContext context( m_module->bot, this );
	if( context.buildPlan() ) {
		for( const auto &pathElem: context.predictedMovementActions ) {
			m_cachedPath->push_back( pathElem );
		}
	} else {
		if( context.goodEnoughPath.empty() && context.lastResortPath.empty() ) {
			return nullptr;
		}

		const char *tag;
		MovementPredictionContext::PredictedPath *path;
		if( !context.goodEnoughPath.empty() ) {
			path = &context.goodEnoughPath;
			tag = "a good enough";
		} else {
			path = &context.lastResortPath;
			tag = "a last resort";
		}

		assert( !path->empty() );

		for( const auto &pathElem : *path ) {
			m_cachedPath->push_back( pathElem );
		}

		debug( "Using %s path built during this planning session\n", tag );
		debug( "The good enough path starts with %s\n", path->front().action->Name() );
	}

	// The first predicted action should not have any offset from the current time
	assert( m_cachedPath->front().timestamp == 0 );
	[[maybe_unused]] int64_t prevTime = -1;
	for( auto &predictedAction: *m_cachedPath ) {
		// Check whether this value contains only positive relative offset from the current time
		assert( (uint64_t)predictedAction.timestamp < 30000 );
		assert( predictedAction.action );
		// Sanity checks
		//printf( "ts: %lld pt %lld\n", (long long)predictedAction.timestamp, (long long)prevTime );
		//assert( predictedAction.timestamp > prevTime );
		prevTime = predictedAction.timestamp;
		// Convert to a timestamp based on the real time
		predictedAction.timestamp += game.realtime;
	}

	// TODO: Make "display path" code functioning !!!!!!!!!
#if 0
	// Enabling this should be accompanied by setting sv_pps 62 to prevent running out of entities
	context->showBuiltPlanPath();
#endif
	action = getCachedActionAndRecordForCurrTime( record );
#if 0
	AITools_DrawColorLine( bot->Origin(), ( Vec3( 0, 0, 48 ) + bot->Origin() ).Data(), action->debugColor(), 0 );
#endif
	return action;
}

void BaseScript2::debug( const char *format, ... ) const {
#if ( defined( ENABLE_MOVEMENT_DEBUG_OUTPUT ) || defined( CHECK_INFINITE_NEXT_STEP_LOOPS ) )
	// Check if there is an already detected error in this case and perform output only it the condition passes
#if !defined( ENABLE_MOVEMENT_DEBUG_OUTPUT )
	if( ::nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD ) {
		return;
	}
#endif

	char tag[64];
	Q_snprintfz( tag, 64, "^6MovementPredictionContext(%s)", m_module->bot->Tag() );

	va_list va;
	va_start( va, format );
	AI_Debugv( tag, format, va );
	va_end( va );
#endif
}

auto BaseScript2::getCachedActionAndRecordForCurrTime( MovementActionRecord *record_ ) -> BaseMovementAction * {
	const int64_t realTime = game.realtime;
	PathElem *prevPredictedAction = nullptr;
	PathElem *nextPredictedAction = nullptr;
	for( PathElem &predictedAction: *m_cachedPath ) {
		if( predictedAction.timestamp >= realTime ) {
			nextPredictedAction = &predictedAction;
			break;
		}
		prevPredictedAction = &predictedAction;
	}

	if( !nextPredictedAction ) {
		debug( "Cannot use predicted movement action: next one (its timestamp is not in the past) cannot be found\n" );
		return nullptr;
	}

	if( !prevPredictedAction ) {
		// If there were no activated actions, the next state must be recently computed for current level time.
		assert( nextPredictedAction->timestamp == realTime );
		// These assertions have already spotted a bug
		const auto *self = game.edicts + m_module->bot->EntNum();
		assert( VectorCompare( nextPredictedAction->entityPhysicsState.Origin(), self->s.origin ) );
		assert( VectorCompare( nextPredictedAction->entityPhysicsState.Velocity(), self->velocity ) );
		// If there is a modified velocity, it will be copied with this record and then applied
		*record_ = nextPredictedAction->record;
		debug( "Using just computed predicted movement action %s\n", nextPredictedAction->action->Name() );
		return nextPredictedAction->action;
	}

	assert( prevPredictedAction->timestamp + prevPredictedAction->stepMillis == nextPredictedAction->timestamp );

	// Fail prediction if both previous and next predicted movement states mask mismatch the current movement state
	// TODOOOOOOOOOOOOoo
	/*
	const auto &actualMovementState = module->movementState;
	if( !actualMovementState.TestActualStatesForExpectedMask( prevPredictedAction->movementStatesMask, bot ) ) {
		if( !actualMovementState.TestActualStatesForExpectedMask( nextPredictedAction->movementStatesMask, bot ) ) {
			return nullptr;
		}
	}*/

	return tryCheckAndLerpActions( prevPredictedAction, nextPredictedAction, record_ );
}

auto BaseScript2::tryCheckAndLerpActions( PathElem *prevAction,
										  PathElem *nextAction,
										  MovementActionRecord *record_ ) -> BaseMovementAction * {
	const int64_t realTime = game.realtime;

	// Check whether predicted action is valid for an actual bot entity physics state
	auto checkLerpFrac = (float)( realTime - prevAction->timestamp );
	checkLerpFrac *= 1.0f / (float)( nextAction->timestamp - prevAction->timestamp );
	assert( checkLerpFrac > 0 && checkLerpFrac <= 1.0f );

	const char *format =
		"Prev predicted action timestamp is " PRId64 ", "
		"next predicted action is " PRId64 ", real time is %" PRId64 "\n";

	debug( format, prevAction->timestamp, nextAction->timestamp, level.time );
	debug( "Should interpolate entity physics state using fraction %f\n", checkLerpFrac );

	// Prevent cache invalidation on each frame if bot is being hit by a continuous fire weapon and knocked back.
	// Perform misprediction test only on the 3rd frame after a last knockback timestamp.
	if( level.time - m_module->bot->lastKnockbackAt <= 32 ) {
		return lerpActionRecords( prevAction, nextAction, record_ );
	}

	if( checkPredictedOrigin( prevAction, nextAction, checkLerpFrac ) ) {
		if( checkPredictedVelocity( prevAction, nextAction, checkLerpFrac ) ) {
			if( !checkPredictedAngles( prevAction, nextAction, checkLerpFrac ) ) {
				return lerpActionRecords( prevAction, nextAction, record_ );
			}
		}
	}

	return nullptr;
}

bool BaseScript2::checkPredictedOrigin( PathElem *prevAction, PathElem *nextAction, float checkLerpFrac ) {
	const auto &prevPhysicsState = prevAction->entityPhysicsState;
	const auto &nextPhysicsState = nextAction->entityPhysicsState;

	vec3_t expectedOrigin;
	VectorLerp( prevPhysicsState.Origin(), checkLerpFrac, nextPhysicsState.Origin(), expectedOrigin );
	float squareDistanceMismatch = DistanceSquared( m_module->bot->Origin(), expectedOrigin );
	if( squareDistanceMismatch < 3.0f * 3.0f ) {
		return true;
	}

	float distanceMismatch = Q_Sqrt( squareDistanceMismatch );
	const char *format_ = "Cannot use predicted movement action: distance mismatch %f is too high for lerp frac %f\n";
	debug( format_, distanceMismatch, checkLerpFrac );
	return false;
}

bool BaseScript2::checkPredictedVelocity( PathElem *prevAction, PathElem *nextAction, float checkLerpFrac ) {
	const auto &prevPhysicsState = prevAction->entityPhysicsState;
	const auto &nextPhysicsState = nextAction->entityPhysicsState;

	const Bot *bot = m_module->bot;

	float expectedSpeed = ( 1.0f - checkLerpFrac ) * prevPhysicsState.Speed() + checkLerpFrac * nextPhysicsState.Speed();
	float actualSpeed = bot->EntityPhysicsState()->Speed();
	float mismatch = std::abs( actualSpeed - expectedSpeed );

	if( mismatch > 5.0f ) {
		debug( "Expected speed: %.1f, actual speed: %.1f, speed mismatch: %.1f\n", expectedSpeed, actualSpeed, mismatch );
		debug( "Cannot use predicted movement action: speed mismatch is too high\n" );
		return false;
	}

	if( actualSpeed < 30.0f ) {
		return true;
	}

	vec3_t expectedVelocity;
	VectorLerp( prevPhysicsState.Velocity(), checkLerpFrac, nextPhysicsState.Velocity(), expectedVelocity );
	Vec3 expectedVelocityDir( expectedVelocity );
	expectedVelocityDir.Normalize();
	Vec3 actualVelocityDir( bot->Velocity() );
	actualVelocityDir.Normalize();

	float cosine = expectedVelocityDir.Dot( actualVelocityDir );
	static const float MIN_COSINE = std::cos( (float) DEG2RAD( 3.0f ) );
	if( cosine > MIN_COSINE ) {
		return true;
	}

	debug( "An angle between expected and actual velocities is %f degrees\n", RAD2DEG( std::acos( cosine ) ) );
	debug( "Cannot use predicted movement action:  expected and actual velocity directions differ significantly\n" );
	return false;
}

bool BaseScript2::checkPredictedAngles( PathElem *prevAction, PathElem *nextAction, float checkLerpFrac ) {
	if( nextAction->record.botInput.canOverrideLookVec ) {
		return true;
	}

	const Bot *bot = m_module->bot;

	Vec3 prevStateAngles( prevAction->entityPhysicsState.Angles() );
	Vec3 nextStateAngles( nextAction->entityPhysicsState.Angles() );

	vec3_t expectedAngles;
	for( int i : { YAW, ROLL } ) {
		expectedAngles[i] = LerpAngle( prevStateAngles.Data()[i], nextStateAngles.Data()[i], checkLerpFrac );
	}

	if( !nextAction->record.botInput.canOverridePitch ) {
		expectedAngles[PITCH] = LerpAngle( prevStateAngles.Data()[PITCH], nextStateAngles.Data()[PITCH], checkLerpFrac );
	} else {
		expectedAngles[PITCH] = game.edicts[bot->EntNum()].s.angles[PITCH];
	}

	vec3_t expectedLookDir;
	AngleVectors( expectedAngles, expectedLookDir, nullptr, nullptr );
	float cosine = bot->EntityPhysicsState()->ForwardDir().Dot( expectedLookDir );
	static const float MIN_COSINE = std::cos( (float)DEG2RAD( 3.0f ) );
	if( cosine > MIN_COSINE ) {
		return true;
	}

	debug( "An angle between and actual look directions is %f degrees\n", RAD2DEG( std::acos( cosine ) ) );
	debug( "Cannot use predicted movement action: expected and actual look directions differ significantly\n" );
	return false;
}

auto BaseScript2::lerpActionRecords( PathElem *prevAction, PathElem *nextAction, MovementActionRecord *record_ ) -> BaseMovementAction * {
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

	const float inputLerpFrac = (float)game.frametime / ( (float)( nextAction->timestamp - realTime ) );
	assert( inputLerpFrac > 0 && inputLerpFrac <= 1.0f );
	// If next predicted time is likely to be pending next frame again, interpolate input for a single frame ahead
	*record_ = nextAction->record;
	// Prevent applying a modified velocity from the new state
	record_->hasModifiedVelocity = false;

	if( !record_->botInput.canOverrideLookVec ) {
		Vec3 actualLookDir( m_module->bot->EntityPhysicsState()->ForwardDir() );
		Vec3 intendedLookVec( record_->botInput.IntendedLookDir() );
		VectorLerp( actualLookDir.Data(), inputLerpFrac, intendedLookVec.Data(), intendedLookVec.Data() );
		record_->botInput.SetIntendedLookDir( intendedLookVec );
	}

	auto prevRotationMask = (unsigned)prevAction->record.botInput.allowedRotationMask;
	auto nextRotationMask = (unsigned)nextAction->record.botInput.allowedRotationMask;
	record_->botInput.allowedRotationMask = (BotInputRotation)( prevRotationMask & nextRotationMask );

	return nextAction->action;
}
