#include "../bot.h"
#include "movementsubsystem.h"
#include "movementlocal.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/scopeexitaction.h>

MovementSubsystem::MovementSubsystem( Bot *bot_ )
	: bot( bot_ )
	, weaponJumpAttemptsRateLimiter( 2 )
	, sameFloorClusterAreasCache( bot_ )
	, nextFloorClusterAreasCache( bot_ ) {
	//movementState.Reset();
}

bool MovementSubsystem::CanChangeWeapons() const {
	/*
	auto &weaponJumpState = movementState.weaponJumpMovementState;
	if( weaponJumpState.IsActive() ) {
		return weaponJumpState.hasTriggeredWeaponJump;
	}*/
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
	/*
	if( movementState.jumppadMovementState.IsActive() ) {
		return false;
	}
	if( movementState.flyUntilLandingMovementState.IsActive() ) {
		return false;
	}
	if( movementState.weaponJumpMovementState.IsActive() ) {
		return false;
	}*/

	const edict_t *self = game.edicts + bot->EntNum();
	// False if the bot is standing on a platform and it has not switched to the TOP state
	return !( self->groundentity && self->groundentity->use == Use_Plat && self->groundentity->moveinfo.state != STATE_TOP );
}

void MovementSubsystem::Frame( BotInput *input ) {
	// TODO:!!!!!!!!!!!!
	// ApplyPendingTurnToLookAtPoint( input );

	if( activeScript ) {
		if( activeScript->getTimeoutAt() <= level.time ) {
			activeScript = nullptr;
		}
	}

	if( activeScript ) {
		if( !activeScript->produceBotInput( input ) ) {
			activeScript = nullptr;
		}
	}

	const bool shouldSelectNewScript = !activeScript;
	if( shouldSelectNewScript ) {
		if( bot->Skill() >= 0.33f ) {
			activeScript = &bunnyHopScript;
			if( !bunnyHopScript.produceBotInput( input ) ) {
				activeScript = nullptr;
			}
		}
		if( !activeScript ) {
			// TODO: Check for immediate recovery in case of a dangerous failure
			if( movementState.entityPhysicsState.GroundEntity() ) {
				activeScript = findFallbackScript( input );
			} else {
				input->SetIntendedLookDir( movementState.entityPhysicsState.ForwardDir() );
				input->ClearMovementDirections();
				input->isUcmdSet = true;
			}
		}
	}

	if( shouldSelectNewScript && activeScript ) {
		// Make sure the script bumps its own timeout properly
		assert( activeScript->getTimeoutAt() > level.time );
	}
}

auto MovementSubsystem::findFallbackScript( BotInput *input ) -> MovementScript * {
	MovementScript *const oldScript = activeScript;
	[[maybe_unused]] volatile wsw::ScopeExitAction restoreOldScript( [&, this]() { activeScript = oldScript; } );

	if( const int navTargetAreaNum = bot->NavTargetAasAreaNum() ) {
		const auto *const aasWorld = AiAasWorld::instance();

		// Make sure that there's no areas below
		assert( movementState.entityPhysicsState.GroundEntity() );

		// We have to retrieve all areas in the box of the bot.
		// It's the correct approach for navigation.
		// Currently, it is not performed during prediction for performance reasons.

		int botAreas[32];
		int numBotAreas = (int)aasWorld->findAreasInBox( bot->self->r.absmin, bot->self->r.absmax,
														 botAreas, sizeof( botAreas ) ).size();

		// TODO: Is it needed?
		for( const int areaNum: { movementState.entityPhysicsState.CurrAasAreaNum(),
								  movementState.entityPhysicsState.DroppedToFloorAasAreaNum() } ) {
			if( areaNum && numBotAreas < (int)std::size( botAreas ) ) {
				if( !wsw::contains( botAreas, botAreas + numBotAreas, areaNum ) ) {
					botAreas[numBotAreas++] = areaNum;
				}
			}
		}

		if( wsw::contains( botAreas, botAreas + numBotAreas, navTargetAreaNum ) ) {
			walkToPointScript.setTargetPoint( bot->NavTargetOrigin() );
			activeScript = &walkToPointScript;
			if( activeScript->produceBotInput( input ) ) {
				return &walkToPointScript;
			}
		} else {
			const auto *routeCache = bot->RouteCache();
			const int travelFlags  = bot->TravelFlags();

			int reachNum = 0;
			if( routeCache->FindRoute( botAreas, numBotAreas, navTargetAreaNum, travelFlags, &reachNum ) ) {
				const auto &reach = aasWorld->getReaches()[reachNum];
				walkToPointScript.setTargetPoint( Vec3( reach.start ) );
				activeScript = &walkToPointScript;
				if( activeScript->produceBotInput( input ) ) {
					return &walkToPointScript;
				}
			}
		}
	}

	return nullptr;
}

void MovementSubsystem::ApplyPendingTurnToLookAtPoint( BotInput *botInput, PredictionContext *context ) {
	if( pendingLookAtPointState.timeoutAt < level.time ) {
		return;
	}

	AiEntityPhysicsState *entityPhysicsState_;
	unsigned frameTime;
	if( context ) {
		entityPhysicsState_ = &context->movementState->entityPhysicsState;
		frameTime = context->predictionStepMillis;
	} else {
		entityPhysicsState_ = &movementState.entityPhysicsState;
		frameTime = game.frametime;
	}


	const AiPendingLookAtPoint &pendingLookAtPoint = pendingLookAtPointState.pendingLookAtPoint;
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
			Vec3 newAngles( bot->GetNewViewAngles( self->s.angles, input->IntendedLookDir(),
												   game.frametime, input->TurnSpeedMultiplier() ) );
			input->SetAlreadyComputedAngles( newAngles );
		}
		input->AlreadyComputedAngles().CopyTo( self->s.angles );
	}
}

PredictionContext::PredictionContext( MovementSubsystem *subsystem, PredictedPath *predictedMovementActions_ )
	: bot( subsystem->bot )
	, m_subsystem( subsystem )
	, predictedMovementActions( predictedMovementActions_ )
	, movementState( nullptr )
	, record( nullptr )
	, totalMillisAhead( 0 )
	, predictionStepMillis( 0 )
	, oldStepMillis( 0 )
	, topOfStackIndex( 0 ) {}