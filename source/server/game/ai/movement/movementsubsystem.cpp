#include "../bot.h"
#include "movementsubsystem.h"
#include "movementlocal.h"
#include "environmenttracecache.h"
#include "bestjumpablespotdetector.h"
#include "movementscript.h"

MovementSubsystem::MovementSubsystem( Bot *bot_ )
	: bot( bot_ )
	, weaponJumpAttemptsRateLimiter( 2 )
	, fallbackMovementAction( this )
	, handleTriggeredJumppadAction( this )
	, landOnSavedAreasAction( this )
	, ridePlatformAction( this )
	, swimMovementAction( this )
	, flyUntilLandingAction( this )
	, campASpotMovementAction( this )
	, bunnyToStairsOrRampExitAction( this )
	, bunnyFollowingReachChainAction( this )
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
	, jumpOverBarrierScript( bot_, this ) {
	movementState.Reset();
}

bool MovementSubsystem::CanChangeWeapons() const {
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

bool MovementSubsystem::CanInterruptMovement() const {
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

void MovementSubsystem::Frame( BotInput *input ) {
	CheckBlockingDueToInputRotation();

	ApplyPendingTurnToLookAtPoint( input );

	movementState.Frame( game.frametime );

	const edict_t *self = game.edicts + bot->EntNum();
	movementState.TryDeactivateContainedStates( self, nullptr );

	if( activeMovementScript && activeMovementScript->TryDeactivate( nullptr ) ) {
		activeMovementScript = nullptr;
	}

	MovementActionRecord movementActionRecord;
	BaseAction *movementAction = predictionContext.GetActionAndRecordForCurrTime( &movementActionRecord );

	movementAction->ExecActionRecord( &movementActionRecord, input, nullptr );
}

void MovementSubsystem::CheckBlockingDueToInputRotation() {
	if( movementState.campingSpotState.IsActive() ) {
		return;
	}
	if( movementState.inputRotation == InputRotation::NONE ) {
		return;
	}

	const edict_t *self = game.edicts + bot->EntNum();

	if( !self->groundentity ) {
		return;
	}

	float threshold = self->r.client->ps.stats[PM_STAT_MAXSPEED] - 30.0f;
	if( threshold < 0 ) {
		threshold = GS_DefaultPlayerSpeed( *ggs ) - 30.0f;
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

void MovementSubsystem::ApplyPendingTurnToLookAtPoint( BotInput *botInput, PredictionContext *context ) {
	PendingLookAtPointState *pendingLookAtPointState;
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
	if( !toPointDir.normalizeFast() ) {
		return;
	}

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

void MovementSubsystem::ApplyInput( BotInput *input, PredictionContext *context ) {
	// While these conditions can hold from time to time and that's not a bug
	// it's better to eliminate these cases eventually completely.
	if( !input->isLookDirSet ) {
		if( context ) {
			if( strcmp( lastNoLookDirAction, context->ActiveActionName() ) != 0 ) {
				aiWarning() << "FIXME " << wsw::StringView( context->ActiveActionName() ) << " No look dir set";
				lastNoLookDirAction = context->ActiveActionName();
			}
		} else {
			aiWarning() << "FIXME: No look dir set";
		}
		return;
	}
	if( !input->isUcmdSet ) {
		if( context ) {
			if( strcmp( lastNoUcmdAction, context->ActiveActionName() ) != 0 ) {
				aiWarning() << "FIXME" << wsw::StringView( context->ActiveActionName() ) << "No ucmd set";
				lastNoUcmdAction = context->ActiveActionName();
			}
		} else {
			aiWarning() << "FIXME: No ucmd set";
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
		// There's no need to modify entityPhysicsState right now
		// as we are going to update entityPhysicsState by PMove() results.
		// TODO: Split the method for null context and non-null context
		// TODO: Inline this part into context->NextMovementStep()
	} else {
		edict_t *self = game.edicts + bot->EntNum();
		if( !input->hasAlreadyComputedAngles ) {
			// TODO: We don't have to rotate if it has been already rotated during prediction.
			// Unfortunately, we can't make the distinction in the current codebase state.
			// TryRotateInput() checks should prevent from a double rotation.
			TryRotateInput( input, context );
			Vec3 newAngles( bot->GetNewViewAngles( self->s.angles, input->IntendedLookDir(),
												   game.frametime, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		input->AlreadyComputedAngles().CopyTo( self->s.angles );
	}
}

bool MovementSubsystem::TryRotateInput( BotInput *input, PredictionContext *context ) {

	const float *botOrigin;
	InputRotation *prevRotation;

	if( context ) {
		botOrigin = context->movementState->entityPhysicsState.Origin();
		prevRotation = &context->movementState->inputRotation;
	} else {
		botOrigin = bot->Origin();
		prevRotation = &movementState.inputRotation;
	}

	const std::optional<Vec3> &keptInFovPoint = bot->GetKeptInFovPoint();
	if( !keptInFovPoint || nextRotateInputAttemptAt > level.time ) {
		*prevRotation = InputRotation::NONE;
		return false;
	}

	Vec3 selfToPoint( *keptInFovPoint );
	selfToPoint -= botOrigin;
	if( !selfToPoint.normalizeFast() ) {
		*prevRotation = InputRotation::NONE;
		return false;
	}

	if( input->IsRotationAllowed( InputRotation::BACK ) ) {
		float backDotThreshold = ( *prevRotation == InputRotation::BACK ) ? -0.3f : -0.5f;
		if( selfToPoint.Dot( input->IntendedLookDir() ) < backDotThreshold ) {
			*prevRotation = InputRotation::BACK;
			InvertInput( input, context );
			return true;
		}
	}

	if( input->IsRotationAllowed( InputRotation::SIDE_KINDS_MASK ) ) {
		vec3_t intendedRightDir, intendedUpDir;
		MakeNormalVectors( input->IntendedLookDir().Data(), intendedRightDir, intendedUpDir );
		const float dotRight = selfToPoint.Dot( intendedRightDir );

		if( input->IsRotationAllowed( InputRotation::RIGHT ) ) {
			const float rightDotThreshold = ( *prevRotation == InputRotation::RIGHT ) ? 0.6f : 0.7f;
			if( dotRight > rightDotThreshold ) {
				*prevRotation = InputRotation::RIGHT;
				TurnInputToSide( intendedRightDir, +1, input, context );
				return true;
			}
		}

		if( input->IsRotationAllowed( InputRotation::LEFT ) ) {
			const float leftDotThreshold = ( *prevRotation == InputRotation::LEFT ) ? -0.6f : -0.7f;
			if( dotRight < leftDotThreshold ) {
				*prevRotation = InputRotation::LEFT;
				TurnInputToSide( intendedRightDir, -1, input, context );
				return true;
			}
		}
	}

	*prevRotation = InputRotation::NONE;
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

void MovementSubsystem::InvertInput( BotInput *input, PredictionContext *context ) {
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

void MovementSubsystem::TurnInputToSide( vec3_t sideDir, int sign, BotInput *input, PredictionContext *context ) {
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

PredictionContext::PredictionContext( MovementSubsystem *subsystem )
	: bot( subsystem->bot )
	, m_subsystem( subsystem )
	, sameFloorClusterAreasCache( m_subsystem->bot )
	, nextFloorClusterAreasCache( m_subsystem->bot )
	, movementState( nullptr )
	, record( nullptr )
	, totalMillisAhead( 0 )
	, predictionStepMillis( 0 )
	, oldStepMillis( 0 )
	, topOfStackIndex( 0 )
	, savepointTopOfStackIndex( 0 )
	, sequenceStopReason( SequenceStopReason::SUCCEEDED )
	, isCompleted( false )
	, cannotApplyAction( false )
	, shouldRollback( false ) {}