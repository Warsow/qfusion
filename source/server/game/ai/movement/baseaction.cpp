#include "baseaction.h"
#include "movementlocal.h"

void BaseAction::Debug( const char *format, ... ) const {
#if ( defined( ENABLE_MOVEMENT_DEBUG_OUTPUT ) || defined( CHECK_INFINITE_NEXT_STEP_LOOPS ) )
	// Check if there is an already detected error in this case and perform output only it the condition passes
#if !defined( ENABLE_MOVEMENT_DEBUG_OUTPUT )
	if( ::nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD ) {
		return;
	}
#endif

	char tag[128];
	Q_snprintfz( tag, 128, "^5%s(%s)", this->Name(), Nick( game.edicts + bot->EntNum() ) );

	va_list va;
	va_start( va, format );
	AI_Debugv( tag, format, va );
	va_end( va );
#endif
}

void BaseAction::ExecActionRecord( const MovementActionRecord *record, BotInput *inputWillBeUsed, PredictionContext *context ) {
	Assert( inputWillBeUsed );
	// TODO: Discover why we still need to do that for pending look at points
	// while the pending look at points seemingly gets applied in SimulateMockBotFrame()
	if( inputWillBeUsed->hasAlreadyComputedAngles ) {
		Vec3 angles( inputWillBeUsed->AlreadyComputedAngles() );
		*inputWillBeUsed = record->botInput;
		inputWillBeUsed->SetAlreadyComputedAngles( angles );
	} else {
		*inputWillBeUsed = record->botInput;
	}

	if( context ) {
		if( record->hasModifiedVelocity ) {
			context->movementState->entityPhysicsState.SetVelocity( record->ModifiedVelocity() );
		}

		// Pending weapon must have been set in PlanPredictionStep()
		// (in planning context it is defined by record->pendingWeapon, pendingWeaponsStack.back()).
		if( record->pendingWeapon >= WEAP_NONE ) {
			//Assert(record->pendingWeapon == context->PendingWeapon());
		}
		return;
	}

	edict_t *const self = game.edicts + bot->EntNum();

	if( record->hasModifiedVelocity ) {
		record->ModifiedVelocity().CopyTo( self->velocity );
	}

	if( record->pendingWeapon != -1 ) {
		self->r.client->ps.stats[STAT_PENDING_WEAPON] = record->pendingWeapon;
	}
}

void BaseAction::CheckPredictionStepResults( PredictionContext *context ) {
	// These flags might be set by ExecActionRecord(). Skip checks in this case.
	if( context->cannotApplyAction || context->isCompleted ) {
		return;
	}

	const auto &newEntityPhysicsState = context->movementState->entityPhysicsState;
	const auto &oldEntityPhysicsState = context->PhysicsStateBeforeStep();

	// This is a default basic test that suits many relatively simple actions
	// Forbid movement from regular contents to "bad" contents
	// (if old contents are "bad" too, a movement step is considered legal)
	// Note: we do not check any points between these two ones,
	// and this can lead to missing "bad contents" for large prediction time step

	constexpr auto badContents = CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_DONOTENTER;
	if( newEntityPhysicsState.waterType & badContents ) {
		if( !( oldEntityPhysicsState.waterType & badContents ) ) {
			if( newEntityPhysicsState.waterType & CONTENTS_LAVA ) {
				Debug( "A prediction step has lead to entering CONTENTS_LAVA point\n" );
			} else if( newEntityPhysicsState.waterType & CONTENTS_SLIME ) {
				Debug( "A prediction step has lead to entering CONTENTS_SLIME point\n" );
			} else {
				Debug( "A prediction step has lead to entering CONTENTS_DONOTENTER point\n" );
			}

			context->SetPendingRollback();
			return;
		}
	}

	if( stopPredictionOnEnteringWater && newEntityPhysicsState.waterLevel > 1 ) {
		Debug( "A prediction step has lead to entering water, should stop planning\n" );
		context->isCompleted = true;
		return;
	}

	// Check AAS areas in the same way
	int oldAasAreaNum = oldEntityPhysicsState.CurrAasAreaNum();
	int newAasAreaNum = newEntityPhysicsState.CurrAasAreaNum();
	if( newAasAreaNum != oldAasAreaNum ) {
		const auto &aasAreaSettings = AiAasWorld::instance()->getAreaSettings();
		const auto &currAreaSettings = aasAreaSettings[newAasAreaNum];
		const auto &prevAreaSettings = aasAreaSettings[oldAasAreaNum];

		if( currAreaSettings.areaflags & AREA_DISABLED ) {
			if( !( prevAreaSettings.areaflags & AREA_DISABLED ) ) {
				Debug( "A prediction step has lead to entering an AREA_DISABLED AAS area\n" );
				context->SetPendingRollback();
				return;
			}
		}

		if( currAreaSettings.contents & AREACONTENTS_DONOTENTER ) {
			if( !( prevAreaSettings.contents & AREACONTENTS_DONOTENTER ) ) {
				Debug( "A prediction step has lead to entering an AREACONTENTS_DONOTENTER AAS area\n" );
				context->SetPendingRollback();
				return;
			}
		}
	}

	if( this->stopPredictionOnTouchingJumppad ) {
		if( const uint16_t touchedTriggerNum = context->frameEvents.touchedJumppadEntNum ) {
			if( touchedTriggerNum == context->m_jumppadPathTriggerNum ) {
				Debug( "A prediction step has lead to touching the jumppad, should stop planning\n" );
				context->isCompleted = true;
			} else {
				Debug( "A prediction step has lead to touching a (wrong) jumppad, rolling back\n" );
				context->SetPendingRollback();
			}
			return;
		}
	}
	if( this->stopPredictionOnTouchingTeleporter ) {
		if( const uint16_t touchedTriggerNum = context->frameEvents.touchedTeleporterEntNum ) {
			if( touchedTriggerNum == context->m_teleporterPathTriggerNum ) {
				Debug( "A prediction step has lead to touching the teleporter, should stop planning\n" );
				context->isCompleted = true;
			} else {
				Debug( "A prediction step has lead to touching a (wrong) teleporter, rolling back\n" );
				context->SetPendingRollback();
			}
		}
		return;
	}
	if( this->stopPredictionOnTouchingPlatform ) {
		if( const uint16_t touchedPlatformNum = context->frameEvents.touchedPlatformEntNum ) {
			if( touchedPlatformNum == context->m_platformPathTriggerNum ) {
				Debug( "A prediction step has lead to touching the platform, should stop planning\n" );
				context->isCompleted = true;
			} else {
				Debug( "A prediction step has lead to touching a (wrong) platform, rolling back\n" );
				context->SetPendingRollback();
			}
		}
		return;
	}

	if( this->stopPredictionOnTouchingNavEntity ) {
		if( HasTouchedNavEntityThisFrame( context ) ) {
			Debug( "A prediction step has lead to touching the nav entity, should stop planning\n" );
			context->isCompleted = true;
			return;
		}
	}

	if( bot->ShouldRushHeadless() ) {
		return;
	}

	if( this->failPredictionOnEnteringHazardImpactZone ) {
		if( const auto *hazard = bot->PrimaryHazard() ) {
			if( hazard->SupportsImpactTests() ) {
				// Check the new origin condition first to cut off early
				if( hazard->HasImpactOnPoint( newEntityPhysicsState.Origin() ) ) {
					if( !hazard->HasImpactOnPoint( oldEntityPhysicsState.Origin() ) ) {
						Debug( "A prediction step has lead to entering a hazard influence zone, should rollback\n" );
						context->SetPendingRollback();
						return;
					}
				}
			}
		}
	}

	// If misc tactics flag "rush headless" is set, areas occupied by enemies are never excluded from routing
	const auto *routeCache = bot->RouteCache();
	// Check the new origin condition first to cut off early
	if( routeCache->AreaDisabled( newAasAreaNum ) ) {
		if( !routeCache->AreaDisabled( oldAasAreaNum ) ) {
			Debug( "A prediction step has lead to entering a disabled for routing area, should rollback\n" );
			return;
		}
	}
}

bool BaseAction::HasTouchedNavEntityThisFrame( PredictionContext *context ) {
	const edict_t *gameEdicts = game.edicts;
	const uint16_t *ents = context->frameEvents.otherTouchedTriggerEnts;
	for( int i = 0, end = context->frameEvents.numOtherTouchedTriggers; i < end; ++i ) {
		const edict_t *ent = gameEdicts + ents[i];
		if( bot->IsNavTargetBasedOnEntity( ent ) ) {
			return true;
		}
	}
	return false;
}

void BaseAction::BeforePlanning() {
	bot = m_subsystem->bot;
	isDisabledForPlanning = false;
	sequenceStartFrameIndex = std::numeric_limits<unsigned>::max();
	sequenceEndFrameIndex = std::numeric_limits<unsigned>::max();
	thisFrameCMShapeList = nullptr;
}

void BaseAction::OnApplicationSequenceStarted( PredictionContext *context ) {
	Debug( "OnApplicationSequenceStarted(context): context->topOfStackIndex=%d\n", context->topOfStackIndex );

	constexpr auto invalidValue = std::numeric_limits<unsigned>::max();
	Assert( sequenceStartFrameIndex == invalidValue );
	sequenceEndFrameIndex = invalidValue;
	sequenceStartFrameIndex = context->topOfStackIndex;
	originAtSequenceStart.Set( context->movementState->entityPhysicsState.Origin() );
	thisFrameCMShapeList = nullptr;
}

void BaseAction::OnApplicationSequenceStopped( PredictionContext *context,
													   SequenceStopReason reason,
													   unsigned stoppedAtFrameIndex ) {
	constexpr auto invalidValue = std::numeric_limits<unsigned>::max();
	Assert( sequenceStartFrameIndex != invalidValue );
	Assert( sequenceEndFrameIndex == invalidValue );
	Assert( sequenceStartFrameIndex <= stoppedAtFrameIndex );
	sequenceStartFrameIndex = invalidValue;
	sequenceEndFrameIndex = stoppedAtFrameIndex;

	const char *format = "OnApplicationSequenceStopped(context, %s, %d): context->topOfStackIndex=%d\n";
	switch( reason ) {
		case UNSPECIFIED:
			// Should not be reached
			Assert( false );
			break;
		case SUCCEEDED:
			Debug( format, "succeeded", stoppedAtFrameIndex, context->topOfStackIndex );
			context->MarkSavepoint( this, stoppedAtFrameIndex + 1 );
			break;
		case SWITCHED:
			Debug( format, "switched", stoppedAtFrameIndex, context->topOfStackIndex );
			context->MarkSavepoint( this, stoppedAtFrameIndex );
			break;
		case DISABLED:
			Debug( format, "disabled", stoppedAtFrameIndex, context->topOfStackIndex );
			context->MarkSavepoint( this, stoppedAtFrameIndex );
			break;
		case FAILED:
			Debug( format, "failed", stoppedAtFrameIndex, context->topOfStackIndex );
			break;
	}
}

unsigned BaseAction::SequenceDuration( const PredictionContext *context ) const {
	unsigned millisAheadAtSequenceStart = context->MillisAheadForFrameStart( sequenceStartFrameIndex );
	// TODO: Ensure that the method gets called only after prediction step in some way
	// (We need a valid and actual prediction step millis)
	Assert( context->predictionStepMillis );
	Assert( context->predictionStepMillis % 16 == 0 );
	Assert( context->totalMillisAhead + context->predictionStepMillis > millisAheadAtSequenceStart );
	return context->totalMillisAhead + context->predictionStepMillis - millisAheadAtSequenceStart;
}