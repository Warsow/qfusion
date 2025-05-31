#include "campaspotaction.h"
#include "movementlocal.h"

bool CampASpotMovementAction::TryUpdateKeyMoveDirs( PredictionContext *context ) {
	auto *campingSpotState = &context->movementState->campingSpotState;
	if( campingSpotState->AreKeyMoveDirsValid() ) {
		return false;
	}

	int keyMoves[2];
	Vec3 botToSpotDir( campingSpotState->Origin() );
	botToSpotDir -= context->movementState->entityPhysicsState.Origin();
	if( !botToSpotDir.normalize() ) {
		return false;
	}

	context->TraceCache().makeRandomizedKeyMovesToTarget( context, botToSpotDir, keyMoves );
	campingSpotState->SetKeyMoveDirs( keyMoves[0], keyMoves[1] );
	return true;
}

Vec3 CampASpotMovementAction::GetUpdatedPendingLookDir( PredictionContext *context ) {
	auto *const lookAtPointState = &context->movementState->pendingLookAtPointState;
	const std::optional<Vec3> &keptInFovPoint = bot->GetKeptInFovPoint();
	const auto &campingSpotState = context->movementState->campingSpotState;

	// The "kept in fov point" is stronger than any intrinsic camping spot state direction
	if( keptInFovPoint ) {
		// Create an intrinsic "pending look at point" that has a default suggested turn speed multiplier
		AiPendingLookAtPoint lookAtPoint( AiPendingLookAtPoint( *keptInFovPoint, 1.0f ) );
		lookAtPointState->Activate( lookAtPoint, 100 );
	} else if( !lookAtPointState->IsActive() ) {
		AiPendingLookAtPoint lookAtPoint( campingSpotState.GetOrUpdateRandomLookAtPoint() );
		lookAtPointState->Activate( lookAtPoint, 750 );
	}

	Vec3 intendedLookDir( lookAtPointState->pendingLookAtPoint.Origin() );
	intendedLookDir -= context->movementState->entityPhysicsState.Origin();
	if( !intendedLookDir.normalize() ) {
		intendedLookDir = context->movementState->entityPhysicsState.ForwardDir();
	}

	return intendedLookDir;
}

void CampASpotMovementAction::PlanPredictionStep( PredictionContext *context ) {
	auto *const defaultAction = context->SuggestDefaultAction();
	if( !GenericCheckIsActionEnabled( context, defaultAction ) ) {
		return;
	}

	if( this->disabledForApplicationFrameIndex == context->topOfStackIndex ) {
		context->sequenceStopReason = DISABLED;
		context->cannotApplyAction = true;
		context->actionSuggestedByAction = defaultAction;
		return;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *campingSpotState = &context->movementState->campingSpotState;
	auto *botInput = &context->record->botInput;

	context->SetDefaultBotInput();
	context->record->botInput.canOverrideLookVec = true;

	const Vec3 spotOrigin( campingSpotState->Origin() );
	const float distance = spotOrigin.Distance2DTo( entityPhysicsState.Origin() );

	// Set to the actual look dir by default
	Vec3 intendedLookDir( entityPhysicsState.ForwardDir() );

	// A "pending look at point" and aiming for attacking are mutually exclusive for reasons described below.
	// Check whether we should prefer attacking.
	if( bot->GetSelectedEnemy() != std::nullopt ) {
		if( bot->ShouldAttack() && bot->ShouldKeepXhairOnEnemy() ) {
			// Disallow setting already computed angles procuded by the pending look at point
			context->movementState->pendingLookAtPointState.Deactivate();
		}
	} else {
		intendedLookDir = GetUpdatedPendingLookDir( context );
	}

	botInput->SetIntendedLookDir( intendedLookDir, true );

	// Reset movement keys waiting for alinging view in the proper direction
	if( intendedLookDir.Dot( entityPhysicsState.ForwardDir() ) < 0.85f ) {
		botInput->ClearMovementDirections();
		botInput->SetWalkButton( true );
		// Use a coarse prediction in this case
		context->predictionStepMillis = 3 * context->DefaultFrameTime();
		return;
	}

	// We need a fairly precise prediction while moving on ground
	context->predictionStepMillis = context->DefaultFrameTime();

	// Keep actual look dir as-is, adjust position by keys only
	botInput->SetIntendedLookDir( intendedLookDir, true );

	// If the bot is within the spot radius, random strafing is allowed
	if( distance / campingSpotState->Radius() < 1.0f ) {
		// If there was no move dirs update and the bot is withing the spot radius, use lesser prediction precision
		if( !TryUpdateKeyMoveDirs( context ) ) {
			context->predictionStepMillis = 2 * context->DefaultFrameTime();
		}

		// All move dirs might be falsely considered blocked (while not really being blocked) in some environments
		// Check whether at least a single movement direction is set
		if( campingSpotState->ForwardMove() || campingSpotState->RightMove() ) {
			botInput->SetForwardMovement( campingSpotState->ForwardMove() );
			botInput->SetRightMovement( campingSpotState->RightMove() );
			botInput->SetWalkButton( random() > campingSpotState->Alertness() * 0.75f );
			return;
		}
	}

	// If the bot is outside of the spot radius or there is no defined movement directions

	Vec3 botToSpotDir( spotOrigin );
	botToSpotDir -= entityPhysicsState.Origin();
	if( !botToSpotDir.normalize() ) {
		return;
	}

	DirToKeyInput( botToSpotDir, intendedLookDir.Data(), entityPhysicsState.RightDir().Data(), botInput );

	botInput->SetWalkButton( random() > campingSpotState->Alertness() * 0.75f );
}

void CampASpotMovementAction::CheckPredictionStepResults( PredictionContext *context ) {
	BaseAction::CheckPredictionStepResults( context );
	if( context->cannotApplyAction || context->isCompleted ) {
		return;
	}

	Vec3 origin( context->movementState->entityPhysicsState.Origin() );
	const auto &campingSpotState = context->movementState->campingSpotState;
	if( !campingSpotState.IsActive() ) {
		Debug( "A prediction step has lead to camping spot state deactivation (the bot is too far from its origin)\n" );
		context->SetPendingRollback();
		return;
	}

	const auto &newEntityPhysicsState = context->movementState->entityPhysicsState;
	const auto &oldEntityPhysicsState = context->PhysicsStateBeforeStep();
	const float radius = campingSpotState.Radius();
	Vec3 spotOrigin( campingSpotState.Origin() );

	const float oldSquareDistanceToOrigin = spotOrigin.SquareDistance2DTo( oldEntityPhysicsState.Origin() );
	const float newSquareDistanceToOrigin = spotOrigin.SquareDistance2DTo( newEntityPhysicsState.Origin() );
	if( oldSquareDistanceToOrigin > wsw::square( 1.3f * radius ) ) {
		if( newSquareDistanceToOrigin > oldSquareDistanceToOrigin ) {
			Debug( "A prediction step has lead to even greater distance to the spot origin while bot should return to it\n" );
			context->SetPendingRollback();
			return;
		}
	}

	// Wait for landing
	if( !newEntityPhysicsState.GroundEntity() ) {
		return;
	}

	if( newSquareDistanceToOrigin < wsw::square( radius ) ) {
		const unsigned sequenceDuration = this->SequenceDuration( context );
		const unsigned completionMillisThreshold = (unsigned) ( 512 * ( 1.0f - 0.5f * campingSpotState.Alertness() ) );
		if( sequenceDuration > completionMillisThreshold ) {
			Debug( "Bot is close to the spot origin and there is enough predicted data ahead\n" );
			context->isCompleted = true;
			return;
		}
	}
}

void CampASpotMovementAction::OnApplicationSequenceStopped( PredictionContext *context,
														    SequenceStopReason stopReason,
														    unsigned stoppedAtFrameIndex ) {
	BaseAction::OnApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );

	if( stopReason == DISABLED ) {
		return;
	}

	if( stopReason == FAILED ) {
		disabledForApplicationFrameIndex = context->savepointTopOfStackIndex;
		return;
	}

	disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
}
