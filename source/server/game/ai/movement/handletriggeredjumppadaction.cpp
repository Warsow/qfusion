#include "baseaction.h"
#include "movementlocal.h"

void HandleTriggeredJumppadAction::PlanPredictionStep( PredictionContext *context ) {
	if( !GenericCheckIsActionEnabled( context ) ) {
		return;
	}

	auto *jumppadMovementState = &context->movementState->jumppadMovementState;
	Assert( jumppadMovementState->IsActive() );

	if( jumppadMovementState->hasEnteredJumppad ) {
		context->cannotApplyAction = true;
		Debug( "The bot has already processed jumppad trigger touch in the given context state, fly until landing\n" );
		return;
	}

	jumppadMovementState->hasEnteredJumppad = true;

	auto *botInput = &context->record->botInput;
	botInput->Clear();

	const edict_t *jumppadEntity = jumppadMovementState->JumppadEntity();
	float startLandingAtZ = m_subsystem->landOnSavedAreasAction.SaveJumppadLandingAreas( jumppadEntity );
	Vec3 lookTarget( jumppadMovementState->JumpTarget() );
	// Let the bot start looking at the first area
	// TODO: This action should be combined with finding landing action so we can check the choice result
	if( const auto &savedAreas = m_subsystem->landOnSavedAreasAction.savedLandingAreas; !savedAreas.empty() ) {
		const aas_area_t &area = AiAasWorld::instance()->getAreas()[savedAreas.front()];
		lookTarget.Set( 0.5f * ( Vec3( area.mins ) + Vec3( area.maxs ) ) );
		lookTarget.Z() = area.mins[2];
	}
	context->movementState->flyUntilLandingMovementState.ActivateWithZLevelThreshold( lookTarget.Data(), startLandingAtZ );
	// Stop prediction (jumppad triggers are not simulated by Exec() code)
	context->isCompleted = true;
}
