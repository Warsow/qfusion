#include "../bot.h"
#include "movementmodule.h"
#include "movementlocal.h"
#include "environmenttracecache.h"
#include "bestjumpablespotdetector.h"
#include "movementscript.h"
#include "basescript2.h"

BotMovementModule::BotMovementModule( Bot *bot_ )
	: bot( bot_ )
	, m_sameFloorClusterAreasCache( bot_ )
	, m_nextFloorClusterAreasCache( bot_ )
	, weaponJumpAttemptsRateLimiter( 2 )
	/*
	, fallbackMovementAction( this )
	, handleTriggeredJumppadAction( this )
	, landOnSavedAreasAction( this )
	, ridePlatformAction( this )
	, swimMovementAction( this )
	, flyUntilLandingAction( this )
	, campASpotMovementAction( this )
	, bunnyToStairsOrRampExitAction( this )
	, bunnyTestingNextReachDirsAction( this )
	, bunnyToBestVisibleReachAction( this )
	, bunnyToBestFloorClusterPointAction( this )
	, bunnyTestingMultipleTurnsAction( this )
	, combatDodgeSemiRandomlyToTargetAction( this )
	, scheduleWeaponJumpAction( this )
	, tryTriggerWeaponJumpAction( this )
	, correctWeaponJumpAction( this )
	, predictionContext( this )
	, useWalkableNodeScript( bot_, this )
	, useRampExitScript( bot_, this )
	, useStairsExitScript( bot_, this )
	, useWalkableTriggerScript( bot_, this )
	, jumpToSpotScript( bot_, this )
	, fallDownScript( bot_, this )
	, jumpOverBarrierScript( bot_, this ) */{
	movementState.Reset();

	//m_scripts.push_back( new BunnyHopScript2( this ) );
	m_scripts.push_back( new CombatScript2( this ) );
}

bool BotMovementModule::CanChangeWeapons() const {
	auto &weaponJumpState = movementState.weaponJumpMovementState;
	if( weaponJumpState.IsActive() ) {
		return weaponJumpState.hasTriggeredWeaponJump;
	}
	const int64_t levelTime = level.time;
	// If there were no recent failed weapon jump attempts
	if( levelTime - lastWeaponJumpTriggeringFailedAt > 512 ) {
		return true;
	}
	// Hack... make a copy of the rate limiter (it's cheap) to avoid modifying its state
	RateLimiter limiter( this->weaponJumpAttemptsRateLimiter );
	// Check whether the rate limiter would allow next weapon jumping attempt soon and disable switching in this case
	return !limiter.TryAcquire( levelTime + 384 );
}

bool BotMovementModule::CanInterruptMovement() const {
	if( movementState.jumppadMovementState.IsActive() ) {
		return false;
	}
	if( movementState.flyUntilLandingMovementState.IsActive() ) {
		return false;
	}
	if( movementState.weaponJumpMovementState.IsActive() ) {
		return false;
	}

	const edict_t *self = game.edicts + bot->EntNum();
	// False if the bot is standing on a platform and it has not switched to the TOP state
	return !( self->groundentity && self->groundentity->use == Use_Plat && self->groundentity->moveinfo.state != STATE_TOP );
}

void BotMovementModule::Frame( BotInput *input ) {
	CheckBlockingDueToInputRotation();

	ApplyPendingTurnToLookAtPoint( input );

	movementState.Frame( game.frametime );

	const edict_t *self = game.edicts + bot->EntNum();
	movementState.TryDeactivateContainedStates( self, nullptr );

	BaseMovementAction *action = nullptr;
	MovementActionRecord movementActionRecord;
	if( m_activeScript ) {
		action = m_activeScript->getActionAndRecordForCurrGameState( &movementActionRecord );
	}

	if( !action ) {
		const auto *const oldScript = m_activeScript;
		m_activeScript = nullptr;
		for( BaseScript2 *script : m_scripts ) {
			if( script != oldScript ) {
				action = script->getActionAndRecordForCurrGameState( &movementActionRecord );
				if( action ) {
					m_activeScript = script;
					break;
				}
			}
		}
	}

	// TODO: Assert action
	if( action ) {
		action->ExecActionRecord( &movementActionRecord, input, nullptr );
	}

	CheckGroundPlatform();
}

void BotMovementModule::CheckGroundPlatform() {
	const edict_t *self = game.edicts + bot->EntNum();
	if( !self->groundentity ) {
		return;
	}

	// Reset saved platform areas after touching a solid world ground
	if( self->groundentity == world ) {
		savedPlatformAreas.clear();
		return;
	}

	if( self->groundentity->use != Use_Plat ) {
		return;
	}

	if( self->groundentity->moveinfo.state != STATE_BOTTOM ) {
		return;
	}

	// TODO!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	// Let the script save exit areas!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	//ridePlatformAction.TrySaveExitAreas( nullptr, self->groundentity );
}

void BotMovementModule::CheckBlockingDueToInputRotation() {
	if( movementState.campingSpotState.IsActive() ) {
		return;
	}
	if( movementState.inputRotation == BotInputRotation::NONE ) {
		return;
	}

	const edict_t *self = game.edicts + bot->EntNum();

	if( !self->groundentity ) {
		return;
	}

	float threshold = self->r.client->ps.stats[PM_STAT_MAXSPEED] - 30.0f;
	if( threshold < 0 ) {
		threshold = DEFAULT_PLAYERSPEED - 30.0f;
	}

	if( self->velocity[0] * self->velocity[0] + self->velocity[1] * self->velocity[1] > threshold * threshold ) {
		nextRotateInputAttemptAt = 0;
		inputRotationBlockingTimer = 0;
		lastInputRotationFailureAt = 0;
		return;
	}

	inputRotationBlockingTimer += game.frametime;
	if( inputRotationBlockingTimer < 200 ) {
		return;
	}

	int64_t millisSinceLastFailure = level.time - lastInputRotationFailureAt;
	assert( millisSinceLastFailure >= 0 );
	if( millisSinceLastFailure >= 10000 ) {
		nextRotateInputAttemptAt = level.time + 400;
	} else {
		nextRotateInputAttemptAt = level.time + 2000 - 400 * ( millisSinceLastFailure / 2500 );
		assert( nextRotateInputAttemptAt > level.time + 400 );
	}
	lastInputRotationFailureAt = level.time;
}

void BotMovementModule::ApplyPendingTurnToLookAtPoint( BotInput *botInput, MovementPredictionContext *context ) {
	BotPendingLookAtPointState *pendingLookAtPointState;
	AiEntityPhysicsState *entityPhysicsState_;
	unsigned frameTime;
	if( context ) {
		pendingLookAtPointState = &context->movementState->pendingLookAtPointState;
		entityPhysicsState_ = &context->movementState->entityPhysicsState;
		frameTime = context->predictionStepMillis;
	} else {
		pendingLookAtPointState = &movementState.pendingLookAtPointState;
		entityPhysicsState_ = &movementState.entityPhysicsState;
		frameTime = game.frametime;
	}

	if( !pendingLookAtPointState->IsActive() ) {
		return;
	}

	const AiPendingLookAtPoint &pendingLookAtPoint = pendingLookAtPointState->pendingLookAtPoint;
	Vec3 toPointDir( pendingLookAtPoint.Origin() );
	toPointDir -= entityPhysicsState_->Origin();
	toPointDir.NormalizeFast();

	botInput->SetIntendedLookDir( toPointDir, true );
	botInput->isLookDirSet = true;

	float turnSpeedMultiplier = pendingLookAtPoint.TurnSpeedMultiplier();
	Vec3 newAngles = bot->GetNewViewAngles( entityPhysicsState_->Angles().Data(), toPointDir, frameTime, turnSpeedMultiplier );
	botInput->SetAlreadyComputedAngles( newAngles );

	botInput->canOverrideLookVec = false;
	botInput->canOverridePitch = false;
}

static const char *lastNoLookDirAction = "";
static const char *lastNoUcmdAction = "";

void BotMovementModule::ApplyInput( BotInput *input, MovementPredictionContext *context ) {
	constexpr const char *tag = "BotMovementModule::ApplyInput";
	// While these conditions can hold from time to time and that's not a bug
	// it's better to eliminate these cases eventually completely.
	if( !input->isLookDirSet ) {
		if( context ) {
			if( strcmp( lastNoLookDirAction, context->ActiveActionName() ) != 0 ) {
				Com_Printf( S_COLOR_RED "%s FIXME %s: No look dir set\n", tag, context->ActiveActionName() );
				lastNoLookDirAction = context->ActiveActionName();
			}
		} else {
			Com_Printf( S_COLOR_RED "%s FIXME: No look dir set\n", tag );
		}
		return;
	}
	if( !input->isUcmdSet ) {
		if( context ) {
			if( strcmp( lastNoUcmdAction, context->ActiveActionName() ) != 0 ) {
				Com_Printf( S_COLOR_RED "%s: FIXME %s: No ucmd set\n", tag, context->ActiveActionName() );
				lastNoUcmdAction = context->ActiveActionName();
			}
		} else {
			Com_Printf( S_COLOR_RED "%s: FIXME: No ucmd set\n", tag );
		}
		return;
	}

	if( context ) {
		auto *entityPhysicsState_ = &context->movementState->entityPhysicsState;
		if( !input->hasAlreadyComputedAngles ) {
			TryRotateInput( input, context );
			Vec3 newAngles( bot->GetNewViewAngles( entityPhysicsState_->Angles().Data(), input->IntendedLookDir(),
												   context->predictionStepMillis, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		entityPhysicsState_->SetAngles( input->AlreadyComputedAngles() );
	} else {
		edict_t *self = game.edicts + bot->EntNum();
		if( !input->hasAlreadyComputedAngles ) {
			TryRotateInput( input, context );
			Vec3 newAngles( bot->GetNewViewAngles( self->s.angles, input->IntendedLookDir(),
												   game.frametime, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		input->AlreadyComputedAngles().CopyTo( self->s.angles );
	}
}

bool BotMovementModule::TryRotateInput( BotInput *input, MovementPredictionContext *context ) {

	const float *botOrigin;
	BotInputRotation *prevRotation;

	if( context ) {
		botOrigin = context->movementState->entityPhysicsState.Origin();
		prevRotation = &context->movementState->inputRotation;
	} else {
		botOrigin = bot->Origin();
		prevRotation = &movementState.inputRotation;
	}

	const float *const keptInFovPoint = bot->GetKeptInFovPoint();
	if( !keptInFovPoint || nextRotateInputAttemptAt > level.time ) {
		*prevRotation = BotInputRotation::NONE;
		return false;
	}

	// Cut off an expensive PVS call early
	if( input->IsRotationAllowed( BotInputRotation::ALL_KINDS_MASK ) ) {
		// We do not utilize PVS cache since it might produce different results for predicted and actual bot origin
		if( !trap_inPVS( keptInFovPoint, botOrigin ) ) {
			*prevRotation = BotInputRotation::NONE;
			return false;
		}
	}

	Vec3 selfToPoint( keptInFovPoint );
	selfToPoint -= botOrigin;
	selfToPoint.NormalizeFast();

	if( input->IsRotationAllowed( BotInputRotation::BACK ) ) {
		float backDotThreshold = ( *prevRotation == BotInputRotation::BACK ) ? -0.3f : -0.5f;
		if( selfToPoint.Dot( input->IntendedLookDir() ) < backDotThreshold ) {
			*prevRotation = BotInputRotation::BACK;
			InvertInput( input, context );
			return true;
		}
	}

	if( input->IsRotationAllowed( BotInputRotation::SIDE_KINDS_MASK ) ) {
		vec3_t intendedRightDir, intendedUpDir;
		MakeNormalVectors( input->IntendedLookDir().Data(), intendedRightDir, intendedUpDir );
		const float dotRight = selfToPoint.Dot( intendedRightDir );

		if( input->IsRotationAllowed( BotInputRotation::RIGHT ) ) {
			const float rightDotThreshold = ( *prevRotation == BotInputRotation::RIGHT ) ? 0.6f : 0.7f;
			if( dotRight > rightDotThreshold ) {
				*prevRotation = BotInputRotation::RIGHT;
				TurnInputToSide( intendedRightDir, +1, input, context );
				return true;
			}
		}

		if( input->IsRotationAllowed( BotInputRotation::LEFT ) ) {
			const float leftDotThreshold = ( *prevRotation == BotInputRotation::LEFT ) ? -0.6f : -0.7f;
			if( dotRight < leftDotThreshold ) {
				*prevRotation = BotInputRotation::LEFT;
				TurnInputToSide( intendedRightDir, -1, input, context );
				return true;
			}
		}
	}

	*prevRotation = BotInputRotation::NONE;
	return false;
}

static inline void SetupInputForTransition( BotInput *input, const edict_t *groundEntity, const vec3_t intendedForwardDir ) {
	// If actual input is not inverted, release keys/clear special button while starting a transition
	float intendedDotForward = input->IntendedLookDir().Dot( intendedForwardDir );
	if( intendedDotForward < 0 ) {
		if( groundEntity ) {
			input->SetSpecialButton( false );
		}
		input->ClearMovementDirections();
		input->SetTurnSpeedMultiplier( 2.0f - 5.0f * intendedDotForward );
	} else if( intendedDotForward < 0.3f ) {
		if( groundEntity ) {
			input->SetSpecialButton( false );
		}
		input->SetTurnSpeedMultiplier( 2.0f );
	}
}

void BotMovementModule::InvertInput( BotInput *input, MovementPredictionContext *context ) {
	input->SetForwardMovement( -input->ForwardMovement() );
	input->SetRightMovement( -input->RightMovement() );

	input->SetIntendedLookDir( -input->IntendedLookDir(), true );

	const edict_t *groundEntity;
	vec3_t forwardDir;
	if( context ) {
		context->movementState->entityPhysicsState.ForwardDir().CopyTo( forwardDir );
		groundEntity = context->movementState->entityPhysicsState.GroundEntity();
	} else {
		movementState.entityPhysicsState.ForwardDir().CopyTo( forwardDir );
		groundEntity = game.edicts[bot->EntNum()].groundentity;
	}

	SetupInputForTransition( input, groundEntity, forwardDir );

	// Prevent doing a forward dash if all direction keys are clear.

	if( !input->IsSpecialButtonSet() || !groundEntity ) {
		return;
	}

	if( input->ForwardMovement() || input->RightMovement() ) {
		return;
	}

	input->SetForwardMovement( -1 );
}

void BotMovementModule::TurnInputToSide( vec3_t sideDir, int sign, BotInput *input, MovementPredictionContext *context ) {
	VectorScale( sideDir, sign, sideDir );

	const edict_t *groundEntity;
	vec3_t forwardDir;
	if( context ) {
		context->movementState->entityPhysicsState.ForwardDir().CopyTo( forwardDir );
		groundEntity = context->movementState->entityPhysicsState.GroundEntity();
	} else {
		movementState.entityPhysicsState.ForwardDir().CopyTo( forwardDir );
		groundEntity = game.edicts[bot->EntNum()].groundentity;
	}

	// Rotate input
	input->SetIntendedLookDir( sideDir, true );

	// If flying, release side keys to prevent unintended aircontrol usage
	if( !groundEntity ) {
		input->SetForwardMovement( 0 );
		input->SetRightMovement( 0 );
	} else {
		int oldForwardMovement = input->ForwardMovement();
		int oldRightMovement = input->RightMovement();
		input->SetForwardMovement( sign * oldRightMovement );
		input->SetRightMovement( sign * oldForwardMovement );
		input->SetSpecialButton( false );
	}

	SetupInputForTransition( input, groundEntity, sideDir );
}

MovementPredictionContext::MovementPredictionContext( Bot *bot_, BaseScript2 *script_ )
	: bot( bot_ )
	, m_script( script_ )
	, movementState( nullptr )
	, record( nullptr )
	, oldPlayerState( nullptr )
	, currPlayerState( nullptr )
	, actionSuggestedByAction( nullptr )
	, activeAction( nullptr )
	, totalMillisAhead( 0 )
	, predictionStepMillis( 0 )
	, oldStepMillis( 0 )
	, topOfStackIndex( 0 )
	, savepointTopOfStackIndex( 0 )
	, sequenceStopReason( SequenceStopReason::SUCCEEDED )
	, isCompleted( false )
	, cannotApplyAction( false )
	, shouldRollback( false ) {}

MovementPredictionContext::HitWhileRunningTestResult MovementPredictionContext::MayHitWhileRunning() {
	if( const auto *cachedResult = mayHitWhileRunningCachesStack.GetCached() ) {
		return *cachedResult;
	}

	const auto &selectedEnemies = bot->GetSelectedEnemies();
	if( !selectedEnemies.AreValid() ) {
		mayHitWhileRunningCachesStack.SetCachedValue( HitWhileRunningTestResult::Failure() );
		// TODO: What if we use Success()?
		return HitWhileRunningTestResult::Failure();
	}

	const auto &entityPhysicsState = movementState->entityPhysicsState;
	Vec3 botLookDir( entityPhysicsState.ForwardDir() );

	Vec3 botToEnemyDir( selectedEnemies.LastSeenOrigin() );
	botToEnemyDir -= entityPhysicsState.Origin();
	// We are sure it has non-zero length (enemies collide with the bot)
	botToEnemyDir.NormalizeFast();

	// Check whether the bot may hit while running
	if( botToEnemyDir.Dot( botLookDir ) > STRAIGHT_MOVEMENT_DOT_THRESHOLD ) {
		HitWhileRunningTestResult result;
		result.canHitAsIs = true;
		result.mayHitOverridingPitch = true;
		mayHitWhileRunningCachesStack.SetCachedValue( result );
		return result;
	}

	// Check whether we can change pitch
	botLookDir.Z() = botToEnemyDir.Z();
	// Normalize again
	float lookDirSquareLength = botLookDir.SquaredLength();
	if( lookDirSquareLength < 0.01f ) {
		mayHitWhileRunningCachesStack.SetCachedValue( HitWhileRunningTestResult::Failure() );
		return HitWhileRunningTestResult::Failure();
	}

	botLookDir *= Q_RSqrt( lookDirSquareLength );
	if( botToEnemyDir.Dot( botLookDir ) > STRAIGHT_MOVEMENT_DOT_THRESHOLD ) {
		HitWhileRunningTestResult result;
		result.canHitAsIs = false;
		result.mayHitOverridingPitch = true;
		mayHitWhileRunningCachesStack.SetCachedValue( result );
		return result;
	}

	mayHitWhileRunningCachesStack.SetCachedValue( HitWhileRunningTestResult::Failure() );
	return HitWhileRunningTestResult::Failure();
}