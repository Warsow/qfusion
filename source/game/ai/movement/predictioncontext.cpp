#include "predictioncontext.h"
#include "movementlocal.h"
#include "../frameentitiescache.h"
#include "basescript2.h"

void MovementPredictionContext::NextReachNumAndTravelTimeToNavTarget( int *reachNum, int *travelTimeToNavTarget ) {
	*reachNum = 0;
	*travelTimeToNavTarget = 0;

	// Do NOT use cached reachability chain for the frame (if any).
	// It might be invalid after movement step, and the route cache does caching itself pretty well.

	const int navTargetAreaNum = NavTargetAasAreaNum();
	if( !navTargetAreaNum ) {
		return;
	}

	const auto *routeCache = bot->RouteCache();

	int fromAreaNums[2];
	int numFromAreas = movementState->entityPhysicsState.PrepareRoutingStartAreas( fromAreaNums );
	if( int travelTime = routeCache->PreferredRouteToGoalArea( fromAreaNums, numFromAreas, navTargetAreaNum, reachNum ) ) {
		*travelTimeToNavTarget = travelTime;
	}
}

void MovementPredictionContext::ShowBuiltPlanPath( bool useActionsColor ) const {
	for( unsigned i = 0, j = 1; j < predictedMovementActions.size(); ++i, ++j ) {

		int color = 0;
		if( useActionsColor ) {
			color = predictedMovementActions[i].action->DebugColor();
		} else {
			switch( i % 3 ) {
				case 0: color = COLOR_RGB( 192, 0, 0 ); break;
				case 1: color = COLOR_RGB( 0, 192, 0 ); break;
				case 2: color = COLOR_RGB( 0, 0, 192 ); break;
			}
		}
		const float *v1 = predictedMovementActions[i].entityPhysicsState.Origin();
		const float *v2 = predictedMovementActions[j].entityPhysicsState.Origin();
		AITools_DrawColorLine( v1, v2, color, 0 );
	}
}

static MovementPredictionContext *currPredictionContext;
static const CMShapeList *pmoveShapeList;

static void Intercepted_PredictedEvent( int entNum, int ev, int parm ) {
	currPredictionContext->OnInterceptedPredictedEvent( ev, parm );
}

static void Intercepted_PMoveTouchTriggers( pmove_t *pm, const vec3_t previous_origin ) {
	currPredictionContext->OnInterceptedPMoveTouchTriggers( pm, previous_origin );
}

static void Intercepted_Trace( trace_t *t, const vec3_t start, const vec3_t mins,
							   const vec3_t maxs, const vec3_t end,
							   int ignore, int contentmask, int timeDelta ) {
	// TODO: Check whether contentmask is compatible
	GAME_IMPORT.CM_ClipToShapeList( pmoveShapeList, t, start, end, mins, maxs, contentmask );
}

static int Intercepted_PointContents( const vec3_t p, int timeDelta ) {
	int topNodeHint = ::collisionTopNodeCache.getTopNode( p, p, !currPredictionContext->topOfStackIndex );
	return trap_CM_TransformedPointContents( p, nullptr, nullptr, nullptr, topNodeHint );
}

void MovementPredictionContext::OnInterceptedPredictedEvent( int ev, int parm ) {
	switch( ev ) {
		case EV_JUMP:
			this->frameEvents.hasJumped = true;
			break;
		case EV_DOUBLEJUMP:
			this->frameEvents.hasDoubleJumped = true;
			break;
		case EV_DASH:
			this->frameEvents.hasDashed = true;
			break;
		case EV_WALLJUMP:
			this->frameEvents.hasWalljumped = true;
			break;
		case EV_FALL:
			this->frameEvents.hasTakenFallDamage = true;
			break;
		default: // Shut up an analyzer
			break;
	}
}

void MovementPredictionContext::OnInterceptedPMoveTouchTriggers( pmove_t *pm, vec3_t const previousOrigin ) {
	// expand the search bounds to include the space between the previous and current origin
	vec3_t mins, maxs;
	for( int i = 0; i < 3; i++ ) {
		if( previousOrigin[i] < pm->playerState->pmove.origin[i] ) {
			mins[i] = previousOrigin[i] + pm->maxs[i];
			if( mins[i] > pm->playerState->pmove.origin[i] + pm->mins[i] ) {
				mins[i] = pm->playerState->pmove.origin[i] + pm->mins[i];
			}
			maxs[i] = pm->playerState->pmove.origin[i] + pm->maxs[i];
		} else {
			mins[i] = pm->playerState->pmove.origin[i] + pm->mins[i];
			maxs[i] = previousOrigin[i] + pm->mins[i];
			if( maxs[i] < pm->playerState->pmove.origin[i] + pm->maxs[i] ) {
				maxs[i] = pm->playerState->pmove.origin[i] + pm->maxs[i];
			}
		}
	}

	// Make a local copy of the reference for faster access (avoid accessing shared library relocation table).
	edict_t *const gameEdicts = game.edicts;

	nearbyTriggersCache.ensureValidForBounds( mins, maxs );

	for( int i = 0; i < nearbyTriggersCache.numJumppadEnts; ++i ) {
		if( GClip_EntityContact( mins, maxs, gameEdicts + nearbyTriggersCache.jumppadEntNums[i] ) ) {
			frameEvents.hasTouchedJumppad = true;
			break;
		}
	}

	for( int i = 0; i < nearbyTriggersCache.numTeleportEnts; ++i ) {
		if( GClip_EntityContact( mins, maxs, gameEdicts + nearbyTriggersCache.teleportEntNums[i] ) ) {
			frameEvents.hasTouchedTeleporter = true;
			break;
		}
	}

	for( int i = 0; i < nearbyTriggersCache.numPlatformEnts; ++i ) {
		if( GClip_EntityContact( mins, maxs, gameEdicts + nearbyTriggersCache.platformEntNums[i] ) ) {
			frameEvents.hasTouchedPlatform = true;
			break;
		}
	}

	if ( nearbyTriggersCache.numOtherEnts <= FrameEvents::MAX_TOUCHED_OTHER_TRIGGERS ) {
		for( int i = 0; i < nearbyTriggersCache.numOtherEnts; ++i ) {
			uint16_t entNum = nearbyTriggersCache.otherEntNums[i];
			if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
				frameEvents.otherTouchedTriggerEnts[frameEvents.numOtherTouchedTriggers++] = entNum;
			}
		}
	} else {
		for( int i = 0; i < nearbyTriggersCache.numOtherEnts; ++i ) {
			uint16_t entNum = nearbyTriggersCache.otherEntNums[i];
			if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
				frameEvents.otherTouchedTriggerEnts[frameEvents.numOtherTouchedTriggers++] = entNum;
				if( frameEvents.numOtherTouchedTriggers == FrameEvents::MAX_TOUCHED_OTHER_TRIGGERS ) {
					break;
				}
			}
		}
	}
}

void MovementPredictionContext::SaveGoodEnoughPath( unsigned advancement, unsigned penaltyMillis ) {
	if( penaltyMillis ) {
		if( penaltyMillis >= goodEnoughPathPenalty ) {
			return;
		}
	} else {
		if( advancement <= goodEnoughPathAdvancement ) {
			return;
		}
	}

	// Sanity check
	if( predictedMovementActions.size() < 4 ) {
		return;
	}

	goodEnoughPathPenalty = penaltyMillis;
	goodEnoughPathAdvancement = advancement;
	goodEnoughPath.clear();
	for( const auto &pathElem: predictedMovementActions ) {
		new( goodEnoughPath.unsafe_grow_back() )PredictedMovementAction( pathElem );
	}
}

void MovementPredictionContext::SaveLastResortPath( unsigned penaltyMillis ) {
	if( lastResortPathPenalty <= penaltyMillis ) {
		return;
	}

	// Sanity check
	if( predictedMovementActions.size() < 4 ) {
		return;
	}

	lastResortPathPenalty = penaltyMillis;
	lastResortPath.clear();
	for( const auto &pathElem: predictedMovementActions ) {
		new( lastResortPath.unsafe_grow_back() )PredictedMovementAction( pathElem );
	}
}

void MovementPredictionContext::SetupStackForStep() {
	PredictedMovementAction *topOfStack;
	if( topOfStackIndex > 0 ) {
		Assert( predictedMovementActions.size() );
		Assert( botMovementStatesStack.size() == predictedMovementActions.size() );
		Assert( playerStatesStack.size() == predictedMovementActions.size() );

		Assert( mayHitWhileRunningCachesStack.Size() == predictedMovementActions.size() );
		Assert( environmentTestResultsStack.size() == predictedMovementActions.size() );

		// topOfStackIndex already points to a needed array element in case of rolling back
		const auto &belowTopOfStack = predictedMovementActions[topOfStackIndex - 1];
		// For case of rolling back to savepoint we have to truncate grew stacks to it
		// The only exception is rolling back to the same top of stack.
		if( this->shouldRollback ) {
			Assert( this->topOfStackIndex <= predictedMovementActions.size() );
			predictedMovementActions.truncate( topOfStackIndex );
			botMovementStatesStack.truncate( topOfStackIndex );
			playerStatesStack.truncate( topOfStackIndex );

			mayHitWhileRunningCachesStack.PopToSize( topOfStackIndex );
			environmentTestResultsStack.truncate( topOfStackIndex );
		} else {
			// For case of growing stack topOfStackIndex must point at the first
			// 'illegal' yet free element at top of the stack
			Assert( predictedMovementActions.size() == topOfStackIndex );
		}

		topOfStack = new( predictedMovementActions.unsafe_grow_back() )PredictedMovementAction( belowTopOfStack );

		// Push a copy of previous player state onto top of the stack
		oldPlayerState = &playerStatesStack.back();
		playerStatesStack.push_back( *oldPlayerState );
		currPlayerState = &playerStatesStack.back();
		// Push a copy of previous movement state onto top of the stack
		botMovementStatesStack.push_back( botMovementStatesStack.back() );

		oldStepMillis = belowTopOfStack.stepMillis;
		Assert( belowTopOfStack.stepMillis > 0 );
		totalMillisAhead = (unsigned)( belowTopOfStack.timestamp ) + belowTopOfStack.stepMillis;
	} else {
		predictedMovementActions.clear();
		botMovementStatesStack.clear();
		playerStatesStack.clear();

		mayHitWhileRunningCachesStack.PopToSize( 0 );
		environmentTestResultsStack.clear();

		const edict_t *self = game.edicts + bot->EntNum();

		topOfStack = new( predictedMovementActions.unsafe_grow_back() )PredictedMovementAction;
		// Push the actual bot player state onto top of the stack
		oldPlayerState = &self->r.client->ps;
		playerStatesStack.push_back( *oldPlayerState );
		currPlayerState = &playerStatesStack.back();
		// Push the actual bot movement state onto top of the stack
		botMovementStatesStack.push_back( m_script->m_module->movementState );

		oldStepMillis = game.frametime;
		totalMillisAhead = 0;
	}
	// Check whether topOfStackIndex really points at the last element of the array
	Assert( predictedMovementActions.size() == topOfStackIndex + 1 );

	movementState = &botMovementStatesStack.back();
	// Provide a predicted movement state for Ai base class
	bot->entityPhysicsState = &movementState->entityPhysicsState;

	// Set the current action record
	this->record = &topOfStack->record;
	this->record->pendingWeapon = -1;

	Assert( mayHitWhileRunningCachesStack.Size() + 1 == predictedMovementActions.size() );
	// The different method is used (there is no copy/move constructors for the template type)
	mayHitWhileRunningCachesStack.PushDummyNonCachedValue();
	new ( environmentTestResultsStack.unsafe_grow_back() )EnvironmentTraceCache;

	this->shouldRollback = false;

	// Save a movement state BEFORE movement step
	topOfStack->entityPhysicsState = this->movementState->entityPhysicsState;
	topOfStack->movementStatesMask = this->movementState->GetContainedStatesMask();
}

/*
inline BaseMovementAction *MovementPredictionContext::SuggestAnyAction() {
	if( BaseMovementAction *action = this->SuggestSuitableAction() ) {
		return action;
	}

	auto *const combatDodgeAction = &module->combatDodgeSemiRandomlyToTargetAction;

	// If no action has been suggested, use a default/dummy one.
	// We have to check the combat action since it might be disabled due to planning stack overflow.
	if( bot->ShouldKeepXhairOnEnemy() ) {
		const auto &selectedEnemies = bot->GetSelectedEnemies();
		if( selectedEnemies.AreValid() && selectedEnemies.ArePotentiallyHittable() ) {
			if( !combatDodgeAction->IsDisabledForPlanning() ) {
				return combatDodgeAction;
			}
		}
	}
	if( bot->GetKeptInFovPoint() && !bot->ShouldRushHeadless() && bot->NavTargetAasAreaNum() ) {
		if( !combatDodgeAction->IsDisabledForPlanning() ) {
			// The fallback movement action produces fairly reliable results.
			// However the fallback movement looks poor.
			// Try using this "combat dodge action" using the "kept in fov" point as an enemy
			// and apply stricter success/termination checks.
			// If this combat action fails, the fallback action gets control.
			combatDodgeAction->allowFailureUsingThatAsNextAction = &module->fallbackMovementAction;
			return combatDodgeAction;
		}
	}

	return &module->fallbackMovementAction;
}*/

/*
BaseMovementAction *MovementPredictionContext::SuggestDefaultAction() {
	// Do not even try using (accelerated) bunnying for easy bots.
	// They however will still still perform various jumps,
	// even during regular roaming on plain surfaces (thats what movement fallbacks do).
	// Ramp/stairs areas and areas not in floor clusters are exceptions
	// (these kinds of areas are still troublesome for bot movement).
	auto *const defaultBunnyHopAction = &module->bunnyToStairsOrRampExitAction;
	auto *const combatMovementAction = &module->combatDodgeSemiRandomlyToTargetAction;
	auto *const savedCombatNextAction = combatMovementAction->allowFailureUsingThatAsNextAction;
	BaseMovementAction *suggestedAction = defaultBunnyHopAction;
	if( bot->ShouldSkinBunnyInFavorOfCombatMovement() ) {
		// Do not try bunnying first and start from this combat action directly
		if( !combatMovementAction->IsDisabledForPlanning() ) {
			combatMovementAction->allowFailureUsingThatAsNextAction = defaultBunnyHopAction;
			suggestedAction = combatMovementAction;
		}
	} else if( bot->Skill() < 0.33f ) {
		const auto *aasWorld = AiAasWorld::Instance();
		const int currGroundedAreaNum = CurrGroundedAasAreaNum();
		// If the current area is not a ramp-like area
		if( !( aasWorld->AreaSettings()[currGroundedAreaNum].areaflags & AREA_INCLINED_FLOOR ) ) {
			// If the current area is not in a stairs cluster
			if( !( aasWorld->AreaStairsClusterNums()[currGroundedAreaNum] ) ) {
				// If the current area is in a floor cluster
				if( aasWorld->AreaFloorClusterNums()[currGroundedAreaNum ] ) {
					// Use a basic movement for easy bots
					suggestedAction = &module->fallbackMovementAction;
				}
			}
		}
	}
	return suggestedAction;
}*/

/*
BaseMovementAction *MovementPredictionContext::SuggestSuitableAction() {
	Assert( !this->actionSuggestedByAction );

	const auto &entityPhysicsState = this->movementState->entityPhysicsState;

	if( entityPhysicsState.waterLevel > 1 ) {
		return &module->swimMovementAction;
	}

	if( movementState->weaponJumpMovementState.IsActive() ) {
		if( movementState->weaponJumpMovementState.hasCorrectedWeaponJump ) {
			if( movementState->flyUntilLandingMovementState.IsActive() ) {
				return &module->flyUntilLandingAction;
			}
			return &module->landOnSavedAreasAction;
		}
		if( movementState->weaponJumpMovementState.hasTriggeredWeaponJump ) {
			return &module->correctWeaponJumpAction;
		}
		return &module->tryTriggerWeaponJumpAction;
	}

	if( movementState->jumppadMovementState.hasTouchedJumppad ) {
		if( movementState->jumppadMovementState.hasEnteredJumppad ) {
			if( movementState->flyUntilLandingMovementState.IsActive() ) {
				if( movementState->flyUntilLandingMovementState.CheckForLanding( this ) ) {
					return &module->landOnSavedAreasAction;
				}

				return &module->flyUntilLandingAction;
			}
			// Fly until landing movement state has been deactivate,
			// switch to bunnying (and, implicitly, to a dummy action if it fails)
			return SuggestDefaultAction();
		}
		return &module->handleTriggeredJumppadAction;
	}

	if( const edict_t *groundEntity = entityPhysicsState.GroundEntity() ) {
		if( groundEntity->use == Use_Plat ) {
			// (prevent blocking if touching platform but not actually triggering it like @ wdm1 GA)
			const auto &mins = groundEntity->r.absmin;
			const auto &maxs = groundEntity->r.absmax;
			if( mins[0] <= entityPhysicsState.Origin()[0] && maxs[0] >= entityPhysicsState.Origin()[0] ) {
				if( mins[1] <= entityPhysicsState.Origin()[1] && maxs[1] >= entityPhysicsState.Origin()[1] ) {
					return &module->ridePlatformAction;
				}
			}
		}
	}

	if( movementState->campingSpotState.IsActive() ) {
		return &module->campASpotMovementAction;
	}

	// The dummy movement action handles escaping using the movement fallback
	if( module->activeMovementScript ) {
		return &module->fallbackMovementAction;
	}

	if( topOfStackIndex > 0 ) {
		return SuggestDefaultAction();
	}

	return &module->scheduleWeaponJumpAction;
}*/

bool MovementPredictionContext::TestNextAction() {
	if( !NavTargetAasAreaNum() ) {
		return false;
	}

	if( activeActionNum >= m_script->m_actions.size() ) {
		return false;
	}

	activeAction = m_script->m_actions[activeActionNum];
	bool startNewSequence = true;

	topOfStackIndex = 0;

	for(;; ) {
		SetupStackForStep();
		// Reset prediction step millis time.
		// Actions might set their custom step value (otherwise it will be set to a default one).
		this->predictionStepMillis = 0;

		this->cannotApplyAction = false;
		this->record->Clear();
		if( startNewSequence ) {
			this->activeAction->OnApplicationSequenceStarted( this );
			startNewSequence = false;
		}

		Debug( "About to call action->PlanPredictionStep() for %s at ToS frame %d\n", activeAction->Name(), topOfStackIndex );
		activeAction->PlanPredictionStep( this );

		if( this->shouldRollback ) {
			// Stop an action application sequence manually with a failure.
			this->activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::FAILED, (unsigned)-1 );
			Debug( "Prediction step failed after action->PlanPredictionStep() call for %s\n", activeAction->Name() );
			topOfStackIndex = 0;
			predictedMovementActions.clear();
			startNewSequence = true;
			continue;
		}

		if( this->cannotApplyAction ) {
			this->topOfStackIndex = 0;
			predictedMovementActions.clear();
			return true;
		}

		if( this->isCompleted ) {
			constexpr const char *format = "Movement prediction is completed on %s, ToS frame %d, %d millis ahead\n";
			Debug( format, activeAction->Name(), this->topOfStackIndex, this->totalMillisAhead );
			// Stop an action application sequence manually with a success.
			activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::SUCCEEDED, this->topOfStackIndex );
			if( !this->isTruncated ) {
				SavePathElem( activeAction );
			}
			return false;
		}

		// If prediction step millis time has not been set, set it to a default value
		if( !this->predictionStepMillis ) {
			this->predictionStepMillis = DefaultFrameTime();
			if( this->topOfStackIndex >= 4 ) {
				this->predictionStepMillis *= ( this->topOfStackIndex < MAX_PREDICTED_STATES / 2 ) ? 3 : 6;
			}
		}

		NextMovementStep();
		activeAction->CheckPredictionStepResults( this );

		if( this->shouldRollback ) {
			this->activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::FAILED, (unsigned)-1 );
			topOfStackIndex = 0;
			predictedMovementActions.clear();
			startNewSequence = true;
			continue;
		}

		// If movement planning is completed, there is no need to do a next step
		if( this->isCompleted ) {
			constexpr const char *format = "Movement prediction is completed on %s, ToS frame %d, %d millis ahead\n";
			Debug( format, activeAction->Name(), this->topOfStackIndex, this->totalMillisAhead );
			if( !this->isTruncated ) {
				SavePathElem( activeAction );
			}

			// Stop action application sequence manually with a success.
			// Prevent duplicated OnApplicationSequenceStopped() call
			// (it might have been done in action->CheckPredictionStepResults() for this->activeAction)
			this->activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::SUCCEEDED, topOfStackIndex );
			// Stop planning by returning false
			return false;
		}

		// Check whether next prediction step is possible
		if( this->CanGrowStackForNextStep() ) {
			SavePathElem( activeAction );
			continue;
		}

		// Disable this action for further planning (it has lead to stack overflow)
		activeAction->isDisabledForPlanning = true;
		topOfStackIndex = 0;
		predictedMovementActions.clear();
		Debug( "Stack overflow on action %s, this action will be disabled for further planning\n", activeAction->Name() );
		return true;
	}
}

bool MovementPredictionContext::NextPredictionStep() {
	SetupStackForStep();

	// Reset prediction step millis time.
	// Actions might set their custom step value (otherwise it will be set to a default one).
	this->predictionStepMillis = 0;
#ifdef CHECK_ACTION_SUGGESTION_LOOPS
	Assert( m_script->m_actions.size() < 32 );
	uint32_t testedActionsMask = 0;
	wsw::StaticVector<BaseMovementAction *, 32> testedActionsList;
#endif

	BaseMovementAction *rollback = nullptr;

	// Get an initial suggested a-priori action !!!!!!!!!!!!!!!!!!!!! TODO: !!!!!!!!!!!!!!!!!!!!!!!!!! What if its the action suggested by action
	unsigned actionNum = activeAction ? activeAction->ActionNum() : 0;
	BaseMovementAction *action = activeAction;
	if( !activeAction ) {
		if( this->actionSuggestedByAction ) {
			action = this->actionSuggestedByAction;
		} else {
			action = m_script->m_actions[actionNum];
		}
	}

#ifdef CHECK_ACTION_SUGGESTION_LOOPS
	testedActionsMask |= ( 1 << action->ActionNum() );
	testedActionsList.push_back( action );
#endif

	this->sequenceStopReason = UNSPECIFIED;
	for(;; ) {
		this->cannotApplyAction = false;
		// Prevent reusing record from the switched on the current frame action
		this->record->Clear();
		if( this->activeAction != action ) {
			// If there was an active previous action, stop its application sequence.
			if( this->activeAction ) {
				unsigned stoppedAtFrameIndex = topOfStackIndex;

				// Never pass the UNSPECIFIED reason to the OnApplicationSequenceStopped() call
				if( sequenceStopReason == UNSPECIFIED ) {
					sequenceStopReason = SWITCHED;
				}

				this->activeAction->OnApplicationSequenceStopped( this, sequenceStopReason, stoppedAtFrameIndex );
			}

			sequenceStopReason = UNSPECIFIED;

			this->activeAction = action;
			// Start the action application sequence
			this->activeAction->OnApplicationSequenceStarted( this );
		}

		Debug( "About to call action->PlanPredictionStep() for %s at ToS frame %d\n", action->Name(), topOfStackIndex );
		action->PlanPredictionStep( this );
		// Check for rolling back necessity (an action application chain has lead to an illegal state)
		if( this->shouldRollback ) {
			// Stop an action application sequence manually with a failure.
			this->activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::FAILED, (unsigned)-1 );
			// An action can be suggested again after rolling back on the next prediction step.
			// Force calling action->OnApplicationSequenceStarted() on the next prediction step.
			this->actionSuggestedByAction = this->activeAction;
			this->activeAction = nullptr;
			Debug( "Prediction step failed after action->PlanPredictionStep() call for %s\n", action->Name() );
			this->RollbackToSavepoint();
			// Continue planning by returning true (the stack will be restored to a savepoint index)
			return true;
		}

		if( this->cannotApplyAction ) {
			if( actionNum + 1 == m_script->m_actions.size() ) {
				predictedMovementActions.clear();
				return false;
			}
			action = m_script->m_actions[++actionNum];
			continue;
		}

		// Movement prediction is completed
		if( this->isCompleted ) {
			constexpr const char *format = "Movement prediction is completed on %s, ToS frame %d, %d millis ahead\n";
			Debug( format, action->Name(), this->topOfStackIndex, this->totalMillisAhead );
			// Stop an action application sequence manually with a success.
			action->OnApplicationSequenceStopped( this, BaseMovementAction::SUCCEEDED, this->topOfStackIndex );
			// Save the predicted movement action
			// Note: this condition is put outside since it is valid only once per a BuildPlan() call
			// and the method is called every prediction frame (up to hundreds of times per a bot per a game frame)
			if( !this->isTruncated ) {
				this->SavePathElem( action );
			}
			// Stop planning by returning false
			return false;
		}

		// An action can be applied, stop testing suitable actions
		break;
	}

	Assert( action == this->activeAction );

	// If prediction step millis time has not been set, set it to a default value
	if( !this->predictionStepMillis ) {
		this->predictionStepMillis = DefaultFrameTime();
		if( this->topOfStackIndex >= 4 ) {
			this->predictionStepMillis *= ( this->topOfStackIndex < MAX_PREDICTED_STATES / 2 ) ? 3 : 6;
		}
	}

	NextMovementStep();

	action->CheckPredictionStepResults( this );
	// If results check has been passed
	if( !this->shouldRollback ) {
		// If movement planning is completed, there is no need to do a next step
		if( this->isCompleted ) {
			constexpr const char *format = "Movement prediction is completed on %s, ToS frame %d, %d millis ahead\n";
			Debug( format, action->Name(), this->topOfStackIndex, this->totalMillisAhead );
			if( !this->isTruncated ) {
				SavePathElem( action );
			}
			// Stop action application sequence manually with a success.
			// Prevent duplicated OnApplicationSequenceStopped() call
			// (it might have been done in action->CheckPredictionStepResults() for this->activeAction)
			if( this->activeAction ) {
				this->activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::SUCCEEDED, topOfStackIndex );
			}
			// Stop planning by returning false
			return false;
		}

		// Check whether next prediction step is possible
		if( this->CanGrowStackForNextStep() ) {
			SavePathElem( action );
			// Continue planning by returning true
			return true;
		}

		// Disable this action for further planning (it has lead to stack overflow)
		action->isDisabledForPlanning = true;
		Debug( "Stack overflow on action %s, this action will be disabled for further planning\n", action->Name() );
		this->SetPendingRollback();
	}

	constexpr const char *format = "Prediction step failed for %s after calling action->CheckPredictionStepResults()\n";
	Debug( format, action->Name() );

	// An active action might have been already reset in action->CheckPredictionStepResults()
	if( this->activeAction ) {
		// Stop action application sequence with a failure manually.
		this->activeAction->OnApplicationSequenceStopped( this, BaseMovementAction::FAILED, (unsigned)-1 );
	}
	// An action can be suggested again after rolling back on the next prediction step.
	// Force calling action->OnApplicationSequenceStarted() on the next prediction step.
	this->activeAction = nullptr;

	this->RollbackToSavepoint();
	// Continue planning by returning true
	return true;
}

bool MovementPredictionContext::buildPlan() {
	for( auto *action: m_script->m_actions ) {
		action->BeforePlanning();
	}

	// Intercept these calls implicitly performed by PMove()
	const auto general_PMoveTouchTriggers = module_PMoveTouchTriggers;
	const auto general_PredictedEvent = module_PredictedEvent;

	module_PMoveTouchTriggers = &Intercepted_PMoveTouchTriggers;
	module_PredictedEvent = &Intercepted_PredictedEvent;

	edict_t *const self = game.edicts + bot->EntNum();

	// We used to modify real entity state every prediction frame so this was an initial state backup.
	// Modifications are no longer performed. This is still useful for validation purposes.

	const Vec3 origin( self->s.origin );
	const Vec3 velocity( self->velocity );
	const Vec3 angles( self->s.angles );
	const int viewHeight = self->viewheight;
	const Vec3 mins( self->r.mins );
	const Vec3 maxs( self->r.maxs );
	const int waterLevel = self->waterlevel;
	const int waterType = self->watertype;
	const edict_t *const groundEntity = self->groundentity;
	const int groundEntityLinkCount = self->groundentity_linkcount;

	const auto savedPlayerState = self->r.client->ps;
	const auto savedPMove = self->r.client->old_pmove;

	Assert( self->bot->entityPhysicsState == &m_script->m_module->movementState.entityPhysicsState );
	// Save current entity physics state (it will be modified even for a single prediction step)
	const auto savedMovementState = m_script->m_module->movementState;

	// Remember to reset these values before each planning session
	this->totalMillisAhead = 0;
	this->savepointTopOfStackIndex = 0;
	this->topOfStackIndex = 0;
	this->activeAction = nullptr;
	this->actionSuggestedByAction = nullptr;
	this->sequenceStopReason = UNSPECIFIED;
	this->isCompleted = false;
	this->isTruncated = false;
	this->shouldRollback = false;

	this->goodEnoughPath.clear();
	this->goodEnoughPathPenalty = std::numeric_limits<unsigned>::max();
	this->goodEnoughPathAdvancement = 0;

	this->lastResortPath.clear();
	this->lastResortPathPenalty = std::numeric_limits<unsigned>::max();

#ifndef CHECK_INFINITE_NEXT_STEP_LOOPS
	for(;; ) {
		if( !NextPredictionStep() ) {
			break;
		}
	}
#else
	::nextStepIterationsCounter = 0;
	this->activeActionNum = 0;
	for(;; ) {
		if( !TestNextAction() ) {
			break;
		}
		this->activeActionNum++;
		++nextStepIterationsCounter;
		if( nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD ) {
			continue;
		}
		// An verbose output has been enabled at this stage
		if( nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD + 200 ) {
			continue;
		}
		constexpr const char *message =
			"MovementPredictionContext::BuildPlan(): "
			"an infinite NextPredictionStep() loop has been detected. "
			"Check the server console output of last 200 steps\n";
		G_Error( "%s", message );
	}
#endif

	// Ensure that the entity state is not modified by any remnants of old code that used to do that
	Assert( VectorCompare( origin.Data(),  self->s.origin ) );
	Assert( VectorCompare( velocity.Data(), self->velocity ) );
	Assert( VectorCompare( angles.Data(), self->s.angles ) );
	Assert( self->viewheight == viewHeight );
	Assert( VectorCompare( mins.Data(), self->r.mins ) );
	Assert( VectorCompare( maxs.Data(), self->r.maxs ) );
	Assert( self->waterlevel == waterLevel );
	Assert( self->watertype == waterType );
	Assert( self->groundentity == groundEntity );
	Assert( self->groundentity_linkcount == groundEntityLinkCount );

	// It's fine even if there are structure member gaps as memcpy is really used by a compiler.
	// These checks are supposed to be turned off in production builds anyway
	Assert( !std::memcmp( &self->r.client->ps, &savedPlayerState, sizeof( savedPlayerState ) ) );
	Assert( !std::memcmp( &self->r.client->old_pmove, &savedPMove, sizeof( savedPMove ) ) );

	for( auto *action: m_script->m_actions ) {
		action->AfterPlanning();
	}

	// TODO: This assumes that a plan is not empty!

	// Set first predicted movement state as the current bot movement state
	m_script->m_module->movementState = savedMovementState;
	// Restore the current entity physics state reference in Ai subclass
	self->bot->entityPhysicsState = &m_script->m_module->movementState.entityPhysicsState;
	// These assertions helped to find an annoying bug during development
	Assert( VectorCompare( self->s.origin, self->bot->entityPhysicsState->Origin() ) );
	Assert( VectorCompare( self->velocity, self->bot->entityPhysicsState->Velocity() ) );

	module_PMoveTouchTriggers = general_PMoveTouchTriggers;
	module_PredictedEvent = general_PredictedEvent;

	return !predictedMovementActions.empty();
}

void MovementPredictionContext::NextMovementStep() {
	auto *botInput = &this->record->botInput;
	auto *entityPhysicsState = &movementState->entityPhysicsState;

	// Make sure we're modify botInput/entityPhysicsState before copying to ucmd

	// Corresponds to Bot::Think();
	m_script->m_module->ApplyPendingTurnToLookAtPoint( botInput, this );
	// Corresponds to module->Frame();
	this->activeAction->ExecActionRecord( this->record, botInput, this );
	// Corresponds to Bot::Think();
	m_script->m_module->ApplyInput( botInput, this );

	// ExecActionRecord() call in SimulateMockBotFrame() might fail or complete the planning execution early.
	// Do not call PMove() in these cases
	if( this->cannotApplyAction || this->isCompleted ) {
		return;
	}

	const edict_t *self = game.edicts + bot->EntNum();

	// Prepare for PMove()
	currPlayerState->POVnum = (unsigned)ENTNUM( self );
	currPlayerState->playerNum = (unsigned)PLAYERNUM( self );

	VectorCopy( entityPhysicsState->Origin(), currPlayerState->pmove.origin );
	VectorCopy( entityPhysicsState->Velocity(), currPlayerState->pmove.velocity );
	Vec3 angles( entityPhysicsState->Angles() );
	angles.CopyTo( currPlayerState->viewangles );

	currPlayerState->pmove.gravity = (int)level.gravity;
	currPlayerState->pmove.pm_type = PM_NORMAL;



	pmove_t pm;
	// TODO: Eliminate this call?
	memset( &pm, 0, sizeof( pmove_t ) );

	pm.playerState = currPlayerState;
	botInput->CopyToUcmd( &pm.cmd );

	for( int i = 0; i < 3; i++ )
		pm.cmd.angles[i] = (short)ANGLE2SHORT( angles.Data()[i] ) - currPlayerState->pmove.delta_angles[i];

	VectorSet( currPlayerState->pmove.delta_angles, 0, 0, 0 );

	// Check for unsigned value wrapping
	Assert( this->predictionStepMillis && this->predictionStepMillis < 100 );
	Assert( this->predictionStepMillis % 16 == 0 );
	pm.cmd.msec = (uint8_t)this->predictionStepMillis;
	pm.cmd.serverTimeStamp = game.serverTime + this->totalMillisAhead;

	this->frameEvents.Clear();

	// Try using an already retrieved list if possible
	// (this saves some excessive bounds comparison)
	if( auto *shapeList = activeAction->thisFrameCMShapeList ) {
		pmoveShapeList = shapeList;
		// Prevent further reuse
		activeAction->thisFrameCMShapeList = nullptr;
	} else {
		pmoveShapeList = TraceCache().getShapeListForPMoveCollision( this );
	}

	// The naive solution of supplying a dummy trace function
	// (that yields a zeroed output with fraction = 1) does not work.
	// An actual logic tied to this flag has to be added in Pmove() for each module_Trace() call.
	pm.skipCollision = !pmoveShapeList;

	::currPredictionContext = this;

	// We currently test collisions only against a solid world on each movement step and the corresponding PMove() call.
	// Touching trigger entities is handled by Intercepted_PMoveTouchTriggers(), also we use AAS sampling for it.
	// Actions that involve touching trigger entities currently are never predicted ahead.
	// If an action really needs to test against entities, a corresponding prediction step flag
	// should be added and this interception of the module_Trace() should be skipped if the flag is set.

	// Save the G_GS_Trace() pointer
	auto oldModuleTrace = module_Trace;
	module_Trace = Intercepted_Trace;

	// Do not test entities contents for same reasons
	// Save the G_PointContents4D() pointer
	auto oldModulePointContents = module_PointContents;
	module_PointContents = Intercepted_PointContents;

	Pmove( &pm );

	// Restore the G_GS_Trace() pointer
	module_Trace = oldModuleTrace;
	// Restore the G_PointContents4D() pointer
	module_PointContents = oldModulePointContents;

	// Update the entity physics state that is going to be used in the next prediction frame
	entityPhysicsState->UpdateFromPMove( &pm );
	// Update the entire movement state that is going to be used in the next prediction frame
	this->movementState->Frame( this->predictionStepMillis );
	this->movementState->TryDeactivateContainedStates( self, this );
}

#ifdef CHECK_INFINITE_NEXT_STEP_LOOPS
int nextStepIterationsCounter;
#endif

void MovementPredictionContext::Debug( const char *format, ... ) const {
#if ( defined( ENABLE_MOVEMENT_DEBUG_OUTPUT ) || defined( CHECK_INFINITE_NEXT_STEP_LOOPS ) )
	// Check if there is an already detected error in this case and perform output only it the condition passes
#if !defined( ENABLE_MOVEMENT_DEBUG_OUTPUT )
	if( ::nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD ) {
		return;
	}
#endif

	char tag[64];
	Q_snprintfz( tag, 64, "^6MovementPredictionContext(%s)", bot->Tag() );

	va_list va;
	va_start( va, format );
	AI_Debugv( tag, format, va );
	va_end( va );
#endif
}

void MovementPredictionContext::CheatingAccelerate( float frac ) {
	if( bot->ShouldMoveCarefully() ) {
		return;
	}

	const auto &entityPhysicsState = this->movementState->entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		return;
	}

	// If the 2D velocity direction is not defined
	if( entityPhysicsState.Speed2D() < 1 ) {
		return;
	}

	const float speed = entityPhysicsState.Speed();
	const float speedThreshold = this->GetRunSpeed() - 15.0f;
	// Respect player class speed properties
	const float groundSpeedScale = speedThreshold / ( DEFAULT_PLAYERSPEED_STANDARD - 15.0f );

	// Avoid division by zero and logic errors
	if( speed < speedThreshold ) {
		return;
	}

	if( frac <= 0.0f ) {
		return;
	}

	const float maxSpeedGainPerSecond = 250.0f;
	const float minSpeedGainPerSecond = 75.0f;
	float speedGainPerSecond;
	// If speed = pivotSpeed speedGainPerSecond remains the same
	const float pivotSpeed = 550.0f * groundSpeedScale;
	const float constantAccelSpeed = 900.0f;
	if( speed > pivotSpeed ) {
		speedGainPerSecond = minSpeedGainPerSecond;
		// In this case speedGainPerSecond slowly decreases to minSpeedGainPerSecond
		// on speed = constantAccelSpeed or greater
		if( speed <= constantAccelSpeed ) {
			float speedFrac = BoundedFraction( speed - pivotSpeed, constantAccelSpeed - pivotSpeed );
			Assert( speedFrac >= 0.0f && speedFrac <= 1.0f );
			speedGainPerSecond += ( maxSpeedGainPerSecond - minSpeedGainPerSecond ) * ( 1.0f - speedFrac );
		}
	} else {
		// In this case speedGainPerSecond might be 2x as greater than maxSpeedGainPerSecond
		Assert( speedThreshold < pivotSpeed );
		float speedFrac = BoundedFraction( speed - speedThreshold, pivotSpeed - speedThreshold );
		Assert( speedFrac >= 0.0f && speedFrac <= 1.0f );
		speedGainPerSecond = maxSpeedGainPerSecond * ( 1.0f + ( 1.0f - speedFrac ) );
		// Also modify the frac to ensure the bot accelerates fast to the pivotSpeed in all cases
		// (the real applied frac is frac^(1/4))
		frac = Q_Sqrt( frac );
	}

	speedGainPerSecond *= groundSpeedScale;
	// (CheatingAccelerate() is called for several kinds of fallback movement that are assumed to be reliable).
	// Do not lower speed gain per second in this case (fallback movement should be reliable).
	// A caller should set an appropriate frac if CheatingAccelerate() is used for other kinds of movement.

	clamp_high( frac, 1.0f );
	frac = Q_Sqrt( frac );

	Vec3 newVelocity( entityPhysicsState.Velocity() );
	// Preserve the old velocity Z.
	// Note the old cheating acceleration code did not do that intentionally.
	// This used to produce very spectacular bot movement but it leads to
	// an increased rate of movement rejection by the prediction system
	// once much more stricter buynnying tests were implemented.
	float oldZ = newVelocity.Z();
	// Convert boost direction to Z
	newVelocity.Z() = 0;
	// Normalize velocity boost direction
	newVelocity *= 1.0f / entityPhysicsState.Speed2D();
	// Make velocity boost vector
	newVelocity *= ( frac * speedGainPerSecond ) * ( 0.001f * this->oldStepMillis );
	// Add velocity boost to the entity velocity in the given physics state
	newVelocity += entityPhysicsState.Velocity();
	// Keep old velocity Z
	newVelocity.Z() = oldZ;

	record->SetModifiedVelocity( newVelocity );
}

void MovementPredictionContext::CheatingCorrectVelocity( const vec3_t target ) {
	const auto &entityPhysicsState = this->movementState->entityPhysicsState;

	Vec3 toTargetDir2D( target );
	toTargetDir2D -= entityPhysicsState.Origin();
	toTargetDir2D.Z() = 0;
	toTargetDir2D.NormalizeFast();

	Vec3 velocity2DDir( entityPhysicsState.Velocity() );
	velocity2DDir.Z() = 0;
	velocity2DDir *= 1.0f / entityPhysicsState.Speed2D();

	CheatingCorrectVelocity( velocity2DDir.Dot( toTargetDir2D ), toTargetDir2D );
}

void MovementPredictionContext::CheatingCorrectVelocity( float velocity2DDirDotToTarget2DDir, const Vec3 &toTargetDir2D ) {
	// Respect player class movement limitations
	if( !( this->currPlayerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_AIRCONTROL ) ) {
		return;
	}

	const auto &entityPhysicsState = this->movementState->entityPhysicsState;
	const float speed = entityPhysicsState.Speed();
	if( speed < 100 ) {
		return;
	}

	// Check whether the direction to the target is normalized
	Assert( toTargetDir2D.LengthFast() > 0.99f && toTargetDir2D.LengthFast() < 1.01f );

	Vec3 newVelocity( entityPhysicsState.Velocity() );
	// Normalize current velocity direction
	newVelocity *= 1.0f / speed;
	// Modify velocity direction
	newVelocity += 0.06f * toTargetDir2D;
	// Normalize velocity direction again after modification
	newVelocity.Normalize();
	// Restore velocity magnitude
	newVelocity *= speed;

	record->SetModifiedVelocity( newVelocity );
}