#include "predictioncontext.h"
#include "movementlocal.h"
#include "triggerareanumscache.h"
#include "../classifiedentitiescache.h"

void PredictionContext::NextReachNumAndTravelTimeToNavTarget( int *reachNum, int *travelTimeToNavTarget ) {
	*reachNum = 0;
	*travelTimeToNavTarget = 0;

	// Do NOT use cached reachability chain for the frame (if any).
	// It might be invalid after movement step, and the route cache does caching itself pretty well.

	const int navTargetAreaNum = NavTargetAasAreaNum();
	if( !navTargetAreaNum ) {
		return;
	}

	const auto *routeCache = bot->RouteCache();
	const int travelFlags  = bot->TravelFlags();

	int fromAreaNums[2];
	int numFromAreas = movementState->entityPhysicsState.PrepareRoutingStartAreas( fromAreaNums );
	if( int travelTime = routeCache->FindRoute( fromAreaNums, numFromAreas, navTargetAreaNum, travelFlags, reachNum ) ) {
		*travelTimeToNavTarget = travelTime;
	}
}

void PredictionContext::ShowBuiltPlanPath( bool useActionsColor ) const {
	for( unsigned i = 0, j = 1; j < predictedMovementActions->size(); ++i, ++j ) {

		int color = 0;
		if( useActionsColor ) {
			color = ( *predictedMovementActions )[i].action->DebugColor();
		} else {
			switch( i % 3 ) {
				case 0: color = COLOR_RGB( 192, 0, 0 ); break;
				case 1: color = COLOR_RGB( 0, 192, 0 ); break;
				case 2: color = COLOR_RGB( 0, 0, 192 ); break;
			}
		}
		const float *v1 = ( *predictedMovementActions )[i].entityPhysicsState.Origin();
		const float *v2 = ( *predictedMovementActions )[j].entityPhysicsState.Origin();
		AITools_DrawColorLine( v1, v2, color, 0 );
	}
}

static void Intercepted_PredictedEvent( int entNum, int ev, int parm ) {
	game.edicts[entNum].bot->OnInterceptedPredictedEvent( ev, parm );
}

static void Intercepted_PMoveTouchTriggers( pmove_t *pm, const vec3_t previous_origin ) {
	game.edicts[pm->playerState->playerNum + 1].bot->OnInterceptedPMoveTouchTriggers( pm, previous_origin );
}

static PredictionContext *currPredictionContext;

static const CMShapeList *pmoveShapeList;
static bool pmoveShouldTestContents;

static void Intercepted_Trace( trace_t *t, const vec3_t start, const vec3_t mins,
							   const vec3_t maxs, const vec3_t end,
							   int ignore, int contentmask, int timeDelta ) {
	// TODO: Check whether contentmask is compatible
	SV_ClipToShapeList( pmoveShapeList, t, start, end, mins, maxs, contentmask );
	if( !currPredictionContext->m_platformTriggerEntNumsToUseDuringPrediction.empty() ) [[unlikely]] {
		if( t->fraction > 0.0f ) [[likely]] {
			auto *const cache = &currPredictionContext->nearbyTriggersCache;
			// Note: We don't expand actually checked bounds as it does some expansion on its own and mins/maxs are small
			cache->ensureValidForBounds( start, end );
			const float *clipMins = mins ? mins : vec3_origin;
			const float *clipMaxs = maxs ? maxs : vec3_origin;
			for( unsigned platformIndex = 0; platformIndex < cache->numPlatformSolidEnts; ++platformIndex ) {
				const auto *const platform = game.edicts + cache->platformSolidEntNums[platformIndex];
				if( ISBRUSHMODEL( platform->s.modelindex ) ) [[likely]] {
					struct cmodel_s *cmodel = SV_InlineModel( (int)platform->s.modelindex );
					trace_t t2;
					SV_TransformedBoxTrace( &t2, start, end, clipMins, clipMaxs, cmodel, contentmask,
												 platform->s.origin, platform->s.angles );
					if( t2.fraction < t->fraction ) {
						*t = t2;
						if( t2.fraction == 0.0f ) {
							break;
						}
					}
				}
			}
		}
	}
}

static int Intercepted_PointContents( const vec3_t p, int timeDelta ) {
	if( pmoveShouldTestContents ) [[unlikely]] {
		int topNodeHint = ::collisionTopNodeCache.getTopNode( p, p, !currPredictionContext->topOfStackIndex );
		return SV_TransformedPointContents( p, nullptr, nullptr, nullptr, topNodeHint );
	}
	return 0;
}

void PredictionContext::OnInterceptedPredictedEvent( int ev, int parm ) {
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

void PredictionContext::OnInterceptedPMoveTouchTriggers( pmove_t *pm, vec3_t const previousOrigin ) {
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

	for( unsigned i = 0; i < nearbyTriggersCache.numJumppadEnts; ++i ) {
		const uint16_t entNum = nearbyTriggersCache.jumppadEntNums[i];
		if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
			frameEvents.touchedJumppadEntNum = entNum;
			break;
		}
	}

	for( unsigned i = 0; i < nearbyTriggersCache.numTeleportEnts; ++i ) {
		const uint16_t entNum = nearbyTriggersCache.teleportEntNums[i];
		if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
			frameEvents.touchedTeleporterEntNum = entNum;
			break;
		}
	}

	for( unsigned i = 0; i < nearbyTriggersCache.numPlatformTriggerEnts; ++i ) {
		const uint16_t entNum = nearbyTriggersCache.platformTriggerEntNums[i];
		if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
			frameEvents.touchedPlatformEntNum = entNum;
			break;
		}
	}

	if ( nearbyTriggersCache.numOtherEnts <= FrameEvents::MAX_TOUCHED_OTHER_TRIGGERS ) {
		for( unsigned i = 0; i < nearbyTriggersCache.numOtherEnts; ++i ) {
			const uint16_t entNum = nearbyTriggersCache.otherEntNums[i];
			if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
				frameEvents.otherTouchedTriggerEnts[frameEvents.numOtherTouchedTriggers++] = entNum;
			}
		}
	} else {
		for( unsigned i = 0; i < nearbyTriggersCache.numOtherEnts; ++i ) {
			const uint16_t entNum = nearbyTriggersCache.otherEntNums[i];
			if( GClip_EntityContact( mins, maxs, gameEdicts + entNum ) ) {
				frameEvents.otherTouchedTriggerEnts[frameEvents.numOtherTouchedTriggers++] = entNum;
				if( frameEvents.numOtherTouchedTriggers == FrameEvents::MAX_TOUCHED_OTHER_TRIGGERS ) {
					break;
				}
			}
		}
	}
}

void PredictionContext::SaveGoodEnoughPath( unsigned advancement, unsigned penaltyMillis ) {
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
	if( predictedMovementActions->size() < 4 ) {
		return;
	}

	goodEnoughPathPenalty = penaltyMillis;
	goodEnoughPathAdvancement = advancement;
	goodEnoughPath.clear();
	for( const auto &pathElem: *predictedMovementActions ) {
		new( goodEnoughPath.unsafe_grow_back() )PredictedMovementAction( pathElem );
	}
}

void PredictionContext::SaveLastResortPath( unsigned penaltyMillis ) {
	if( lastResortPathPenalty <= penaltyMillis ) {
		return;
	}

	// Sanity check
	if( predictedMovementActions->size() < 4 ) {
		return;
	}

	lastResortPathPenalty = penaltyMillis;
	lastResortPath.clear();
	for( const auto &pathElem: *predictedMovementActions ) {
		new( lastResortPath.unsafe_grow_back() )PredictedMovementAction( pathElem );
	}
}

void PredictionContext::SetupStackForStep() {
	PredictedMovementAction *topOfStack;
	if( topOfStackIndex > 0 ) {
		Assert( predictedMovementActions->size() );
		Assert( botMovementStatesStack.size() == predictedMovementActions->size() );
		Assert( playerStatesStack.size() == predictedMovementActions->size() );

		Assert( defaultBotInputsCachesStack.Size() == predictedMovementActions->size() );
		Assert( environmentTestResultsStack.size() == predictedMovementActions->size() );

		// topOfStackIndex already points to a needed array element in case of rolling back
		const auto &belowTopOfStack = ( *predictedMovementActions )[topOfStackIndex - 1];
		// For case of rolling back to savepoint we have to truncate grew stacks to it
		// The only exception is rolling back to the same top of stack.
		if( this->shouldRollback ) {
			Assert( this->topOfStackIndex <= predictedMovementActions->size() );
			predictedMovementActions->truncate( topOfStackIndex );
			botMovementStatesStack.truncate( topOfStackIndex );
			playerStatesStack.truncate( topOfStackIndex );

			defaultBotInputsCachesStack.PopToSize( topOfStackIndex );
			environmentTestResultsStack.truncate( topOfStackIndex );
		} else {
			// For case of growing stack topOfStackIndex must point at the first
			// 'illegal' yet free element at top of the stack
			Assert( predictedMovementActions->size() == topOfStackIndex );
		}

		topOfStack = new( predictedMovementActions->unsafe_grow_back() )PredictedMovementAction( belowTopOfStack );

		// Push a copy of previous player state onto top of the stack
		oldMinimalPlayerState = std::addressof( playerStatesStack.back() );

		playerStatesStack.push_back( playerStatesStack.back() );
		currMinimalPlayerState = std::addressof( playerStatesStack.back() );

		// Push a copy of previous movement state onto top of the stack
		botMovementStatesStack.push_back( botMovementStatesStack.back() );

		oldStepMillis = belowTopOfStack.stepMillis;
		Assert( belowTopOfStack.stepMillis > 0 );
		totalMillisAhead = (unsigned)( belowTopOfStack.timestamp ) + belowTopOfStack.stepMillis;
	} else {
		predictedMovementActions->clear();
		botMovementStatesStack.clear();
		playerStatesStack.clear();

		defaultBotInputsCachesStack.PopToSize( 0 );
		environmentTestResultsStack.clear();

		topOfStack = new( predictedMovementActions->unsafe_grow_back() )PredictedMovementAction;

		// Push the actual bot player state onto top of the stack
		oldMinimalPlayerState = &minimalPlayerStateForFrame0;

		currMinimalPlayerState  = playerStatesStack.unsafe_grow_back();
		*currMinimalPlayerState = minimalPlayerStateForFrame0;

		// Push the actual bot movement state onto top of the stack
		botMovementStatesStack.push_back( m_subsystem->movementState );

		oldStepMillis = game.frametime;
		totalMillisAhead = 0;
	}
	// Check whether topOfStackIndex really points at the last element of the array
	Assert( predictedMovementActions->size() == topOfStackIndex + 1 );

	movementState = &botMovementStatesStack.back();
	// Provide a predicted movement state for Ai base class
	bot->entityPhysicsState = &movementState->entityPhysicsState;

	// Set the current action record
	this->record = &topOfStack->record;
	this->record->pendingWeapon = -1;

	// Check caches size, a cache size must match the stack size after addition of a single placeholder element.
	Assert( defaultBotInputsCachesStack.Size() + 1 == predictedMovementActions->size() );
	// Then put placeholders for non-cached yet values onto top of caches stack
	defaultBotInputsCachesStack.PushDummyNonCachedValue();
	new ( environmentTestResultsStack.unsafe_grow_back() )EnvironmentTraceCache;

	this->shouldRollback = false;

	// Save a movement state BEFORE movement step
	topOfStack->entityPhysicsState = this->movementState->entityPhysicsState;
}

bool PredictionContext::NextPredictionStep( BaseAction *action, bool *hasStartedSequence ) {
	SetupStackForStep();

	assert( !this->shouldRollback );

	// Reset prediction step millis time.
	// Actions might set their custom step value (otherwise it will be set to a default one).
	this->predictionStepMillis = 0;
	// Prevent reusing record from the switched on the current frame action
	this->record->Clear();

	if( !*hasStartedSequence ) {
		*hasStartedSequence = true;
		action->OnApplicationSequenceStarted( this );
	}

	Debug( "About to call action->PlanPredictionStep() for %s at ToS frame %d\n", action->Name(), topOfStackIndex );
	action->PlanPredictionStep( this );

	if( action->isDisabledForPlanning ) {
		// Reset the rollback flag
		if( this->shouldRollback ) {
			this->RollbackToSavepoint();
		}
		return false;
	}

	// Check for rolling back necessity (an action application chain has lead to an illegal state)
	if( this->shouldRollback ) {
		// Stop an action application sequence manually with a failure.
		action->OnApplicationSequenceStopped( this, BaseAction::FAILED, (unsigned)-1 );
		*hasStartedSequence = false;
		Debug( "Prediction step failed after action->PlanPredictionStep() call for %s\n", action->Name() );
		this->RollbackToSavepoint();
		// Continue planning by returning true (the stack will be restored to a savepoint index)
		return true;
	}

	// Movement prediction is completed
	// TODO: Are we going to allow early completion?
	if( this->isCompleted ) {
		constexpr const char *format = "Movement prediction is completed on %s, ToS frame %d, %d millis ahead\n";
		Debug( format, action->Name(), this->topOfStackIndex, this->totalMillisAhead );
		// Stop an action application sequence manually with a success.
		action->OnApplicationSequenceStopped( this, BaseAction::SUCCEEDED, this->topOfStackIndex );
		SaveActionOnStack( action );
		// Stop planning by returning false
		return false;
	}

	// If prediction step millis time has not been set, set it to a default value
	if( !this->predictionStepMillis ) {
		this->predictionStepMillis = DefaultFrameTime();
		if( this->topOfStackIndex >= 4 ) {
			this->predictionStepMillis *= ( this->topOfStackIndex < MAX_PREDICTED_STATES / 2 ) ? 3 : 6;
		}
	}

	NextMovementStep( action );

	action->CheckPredictionStepResults( this );
	// If results check has been passed
	if( !this->shouldRollback ) {
		// If movement planning is completed, there is no need to do a next step
		if( this->isCompleted ) {
			constexpr const char *format = "Movement prediction is completed on %s, ToS frame %d, %d millis ahead\n";
			Debug( format, action->Name(), this->topOfStackIndex, this->totalMillisAhead );
			SaveActionOnStack( action );
			// Stop action application sequence manually with a success.
			// Prevent duplicated OnApplicationSequenceStopped() call
			// (it might have been done in action->CheckPredictionStepResults() for this->activeAction)
			action->OnApplicationSequenceStopped( this, BaseAction::SUCCEEDED, topOfStackIndex );
			*hasStartedSequence = false;
			// Stop planning by returning false
			return false;
		}

		// Check whether next prediction step is possible
		if( this->CanGrowStackForNextStep() ) {
			SaveActionOnStack( action );
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

	action->OnApplicationSequenceStopped( this, BaseAction::FAILED, (unsigned)-1 );
	*hasStartedSequence = false;

	this->RollbackToSavepoint();
	// Continue planning by returning true
	return true;
}

void PredictionContext::SavePathTriggerNums() {
	m_jumppadPathTriggerNum = m_teleporterPathTriggerNum = m_platformPathTriggerNum = 0;

	const int targetAreaNum = bot->NavTargetAasAreaNum();
	if( !targetAreaNum ) {
		return;
	}

	int startAreaNums[2] { 0, 0 };
	const int numStartAreas = bot->entityPhysicsState->PrepareRoutingStartAreas( startAreaNums );
	if( startAreaNums[0] == targetAreaNum || startAreaNums[1] == targetAreaNum ) {
		return;
	}

	const auto aasReaches = AiAasWorld::instance()->getReaches();
	const auto *const __restrict routeCache = bot->RouteCache();
	const auto *const __restrict botOrigin = bot->Origin();
	const int travelFlags = bot->TravelFlags();

	int reachAreaNum = 0;
	enum MetTriggerFlags : unsigned { Teleporter = 0x1, Jumppad = 0x2, Platform = 0x4 };
	unsigned metTriggerBits = 0;
	// Don't inspect the whole reach chain to target for performance/logical reasons, and also protect from routing bugs
	for( int i = 0; i < 32; ++i ) {
		int travelTime, reachNum = 0;
		if( !reachAreaNum ) {
			travelTime = routeCache->FindRoute( startAreaNums, numStartAreas, targetAreaNum, travelFlags, &reachNum );
		} else {
			travelTime = routeCache->FindRoute( reachAreaNum, targetAreaNum, travelFlags, &reachNum );
		}
		if( !travelTime || !reachNum ) {
			break;
		}
		const auto &__restrict reach = aasReaches[reachNum];
		reachAreaNum = reach.areanum;
		if( reachAreaNum == targetAreaNum ) {
			break;
		}
		// Another cutoff
		if( DistanceSquared( botOrigin, reach.start ) > wsw::square( 768 ) ) {
			break;
		}
		if( const auto *const __restrict classTriggerNums = triggerAreaNumsCache.getTriggersForArea( reachAreaNum ) ) {
			if( !m_teleporterPathTriggerNum ) {
				if( const std::optional<uint16_t> maybeTeleporterNum = classTriggerNums->getFirstTeleporterNum() ) {
					m_teleporterPathTriggerNum = *maybeTeleporterNum;
					metTriggerBits |= Teleporter;
				}
			}
			if( !m_jumppadPathTriggerNum ) {
				if( const std::optional<uint16_t> maybeJumppadNum = classTriggerNums->getFirstJummpadNum() ) {
					m_jumppadPathTriggerNum = *maybeJumppadNum;
					metTriggerBits |= Jumppad;
				}
			}
			if( !m_platformPathTriggerNum ) {
				if( const std::optional<uint16_t> maybePlatformNum = classTriggerNums->getFirstPlatformNum() ) {
					m_platformPathTriggerNum = *maybePlatformNum;
					metTriggerBits |= Platform;
				}
			}
			// Interrupt at this
			// TODO: Don't even try testing for kinds of triggers that are not even present on the map
			if( metTriggerBits == ( Teleporter | Jumppad | Platform ) ) {
				break;
			}
		}
	}
}

// TODO: Make a non-template wrapper for fixed vectors so we don't have to instantiate templates for different size
template <unsigned N>
static void collectNearbyTriggers( wsw::StaticVector<uint16_t, N> *__restrict dest,
								   const float *__restrict botOrigin,
								   std::span<const uint16_t> entNums ) {
	const auto *const __restrict gameEdicts = game.edicts;

	dest->clear();

	for( const uint16_t entNum: entNums ) {
		const edict_t *const trigger = gameEdicts + entNum;
		if( BoundsAndSphereIntersect( trigger->r.absmin, trigger->r.absmax, botOrigin, 768.0f ) ) {
			dest->push_back( entNum );
			// TODO: Add an initial pass to select nearest/best in this case?
			if( dest->full() ) [[unlikely]] {
				break;
			}
		}
	}
}

void PredictionContext::SaveNearbyEntities() {
	const float *const origin = bot->Origin();
	const auto *const  cache  = wsw::ai::ClassifiedEntitiesCache::instance();

	collectNearbyTriggers( &m_jumppadEntNumsToUseDuringPrediction, origin, cache->getAllPersistentMapJumppads() );
	collectNearbyTriggers( &m_teleporterEntNumsToUseDuringPrediction, origin, cache->getAllPersistentMapTeleporters() );
	collectNearbyTriggers( &m_otherTriggerEntNumsToUseDuringPrediction, origin, cache->getAllOtherTriggersInThisFrame() );

	m_platformTriggerEntNumsToUseDuringPrediction.clear();
	for( const uint16_t entNum: cache->getAllPersistentMapPlatformTriggers() ) {
		bool shouldAddThisTrigger = false;
		const edict_t *const trigger = game.edicts + entNum;
		if( BoundsAndSphereIntersect( trigger->r.absmin, trigger->r.absmax, origin, 768.0f ) ) {
			shouldAddThisTrigger = true;
		} else {
			const edict_t *platform = trigger->enemy;
			assert( platform && platform->use == Use_Plat );
			if( BoundsAndSphereIntersect( platform->r.absmin, platform->r.absmax, origin, 768.0f ) ) {
				shouldAddThisTrigger = true;
			}
		}
		if( shouldAddThisTrigger ) {
			m_platformTriggerEntNumsToUseDuringPrediction.push_back( entNum );
			if( m_platformTriggerEntNumsToUseDuringPrediction.full() ) [[unlikely]] {
				break;
			}
		}
	}

	nearbyTriggersCache.context = this;
}

bool PredictionContext::BuildPlan( std::span<BaseAction *> actionsToUse ) {
	assert( !actionsToUse.empty() );

	// Intercept these calls implicitly performed by PMove()
	const auto general_PMoveTouchTriggers = ggs->PMoveTouchTriggers;
	const auto general_PredictedEvent = ggs->PredictedEvent;

	ggs->PMoveTouchTriggers = &Intercepted_PMoveTouchTriggers;
	ggs->PredictedEvent = &Intercepted_PredictedEvent;

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

	// TODO: We don't modify these two, do we?
	const auto savedPlayerState = self->r.client->ps;
	const auto savedPMove = self->r.client->old_pmove;

	Assert( self->bot->entityPhysicsState == &m_subsystem->movementState.entityPhysicsState );
	// Save current entity physics state (it will be modified even for a single prediction step)
	const AiEntityPhysicsState currEntityPhysicsState = m_subsystem->movementState.entityPhysicsState;

	// Get modified every NextMovementFrame() call
	this->playerStateForPmove = self->r.client->ps;

	// Kept unmodified during plan building
	this->minimalPlayerStateForFrame0.pmove      = playerStateForPmove.pmove;
	this->minimalPlayerStateForFrame0.viewheight = playerStateForPmove.viewheight;

	// Remember to reset these values before each planning session
	this->totalMillisAhead = 0;
	this->topOfStackIndex  = 0;
	this->isCompleted      = false;
	this->shouldRollback   = false;

	this->goodEnoughPath.clear();
	this->goodEnoughPathPenalty = std::numeric_limits<unsigned>::max();
	this->goodEnoughPathAdvancement = 0;

	this->lastResortPath.clear();
	this->lastResortPathPenalty = std::numeric_limits<unsigned>::max();

	SavePathTriggerNums();
	SaveNearbyEntities();

	bool succeeded = false;
	for( BaseAction *action: actionsToUse ) {
		// Millis are set appropritately to the top-of-stack index
		assert( this->topOfStackIndex == 0 );
		assert( !this->isCompleted );
		assert( !this->shouldRollback );
		TryBuildingPlanUsingAction( action );
		if( isCompleted ) {
			succeeded = true;
			break;
		}
	}

	if( !succeeded ) {
		PredictedPath *fallbackPath = nullptr;
		const char *fallbackDesc    = nullptr;
		if( !goodEnoughPath.empty() ) {
			fallbackPath = &goodEnoughPath;
			fallbackDesc = "good enough";
		} else if( !lastResortPath.empty() ) {
			fallbackPath = &lastResortPath;
			fallbackDesc = "last resort";
		}
		if( fallbackPath ) {
			Debug( "Using a %s path as a fallback\n", fallbackDesc );
			predictedMovementActions->clear();
			for( const auto &pathElem : *fallbackPath ) {
				predictedMovementActions->push_back( pathElem );
			}
			succeeded = true;
		}
	}

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

	// Set first predicted movement state as the current bot movement state
	m_subsystem->movementState = botMovementStatesStack[0];
	// Even the first predicted movement state usually has modified physics state, restore it to a saved value
	m_subsystem->movementState.entityPhysicsState = currEntityPhysicsState;
	// Restore the current entity physics state reference in Ai subclass
	self->bot->entityPhysicsState = &m_subsystem->movementState.entityPhysicsState;
	// These assertions helped to find an annoying bug during development
	Assert( VectorCompare( self->s.origin, self->bot->entityPhysicsState->Origin() ) );
	Assert( VectorCompare( self->velocity, self->bot->entityPhysicsState->Velocity() ) );

	ggs->PMoveTouchTriggers = general_PMoveTouchTriggers;
	ggs->PredictedEvent = general_PredictedEvent;

	if( succeeded ) {
		// The first predicted action should not have any offset from the current time
		assert( predictedMovementActions->front().timestamp == 0 );
		for( PredictedMovementAction &predictedAction: *predictedMovementActions ) {
			assert( predictedAction.action );
			// Check whether this value contains only positive relative offset from the current time
			assert( (uint64_t)predictedAction.timestamp < 30000 );
			// Convert to a timestamp based on the real time
			predictedAction.timestamp += game.realtime;
		}
	} else {
		predictedMovementActions->clear();
	}

	return succeeded;
}

void PredictionContext::TryBuildingPlanUsingAction( BaseAction *action ) {
	action->BeforePlanning();

#ifdef CHECK_INFINITE_NEXT_STEP_LOOPS
	::nextStepIterationsCounter = 0;
#endif

	bool hasStartedSequence = false;
	for(;; ) {
		if( !NextPredictionStep( action, &hasStartedSequence ) ) {
			break;
		}

		// Make sure that we have performed rolling back (if needed) first
		assert( !shouldRollback );

#ifdef CHECK_INFINITE_NEXT_STEP_LOOPS
		++nextStepIterationsCounter;
		if( nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD ) {
			continue;
		}
		v_debugOutput.setImmediately( true );
		// An verbose output has been enabled at this stage
		if( nextStepIterationsCounter < NEXT_STEP_INFINITE_LOOP_THRESHOLD + 200 ) {
			continue;
		}
		constexpr const char *message =
			"PredictionContext::BuildPlan(): "
			"an infinite NextPredictionStep() loop has been detected. "
			"Check the server console output of last 200 steps\n";
		G_Error( "%s", message );
	}
#endif

	action->AfterPlanning();
}

void PredictionContext::NextMovementStep( BaseAction *action ) {
	auto *botInput = &this->record->botInput;
	auto *entityPhysicsState = &movementState->entityPhysicsState;

	// Make sure we're modify botInput/entityPhysicsState before copying to ucmd

	// Corresponds to Bot::Think();
	m_subsystem->ApplyPendingTurnToLookAtPoint( botInput, this );
	// Corresponds to m_subsystem->Frame();
	action->ExecActionRecord( this->record, botInput, this );
	// Corresponds to Bot::Think();
	m_subsystem->ApplyInput( botInput, this );

	// ExecActionRecord() might fail or complete the planning execution early.
	// Do not call PMove() in these cases
	if( this->shouldRollback || this->isCompleted ) {
		return;
	}

	const edict_t *self = game.edicts + bot->EntNum();

	// We have to copy required properites to the playerStateForPmove
	// (we can't supply currMinimalPlayerState directly)

	Assert( playerStateForPmove.POVnum == (unsigned)ENTNUM( self ) );
	Assert( playerStateForPmove.playerNum == (unsigned)PLAYERNUM( self ) );

	// TODO: Use fast copying subroutine
	playerStateForPmove.pmove      = currMinimalPlayerState->pmove;
	playerStateForPmove.viewheight = currMinimalPlayerState->viewheight;

	playerStateForPmove.pmove.gravity = (int)level.gravity;
	playerStateForPmove.pmove.pm_type = PM_NORMAL;

	const Vec3 angles( entityPhysicsState->Angles() );
	VectorCopy( entityPhysicsState->Origin(), playerStateForPmove.pmove.origin );
	VectorCopy( entityPhysicsState->Velocity(), playerStateForPmove.pmove.velocity );
	angles.CopyTo( playerStateForPmove.viewangles );

	pmove_t pm;
	// TODO: Eliminate this call?
	memset( &pm, 0, sizeof( pmove_t ) );

	pm.playerState = &playerStateForPmove;
	botInput->CopyToUcmd( &pm.cmd );

	// Skip for trajectory prediction
	// TODO: Provide a cheaper PM_CalcGoodPosition subroutine as well
#if 0
	// TODO: Comparing structs via memcmp is a bug waiting to happen
	if( std::memcmp( (const void *)&oldMinimalPlayerState->pmove, (const void *)&currMinimalPlayerState->pmove, sizeof( pmove_state_t ) ) != 0 ) {
		pm.snapInitially = true;
	}
#endif

	pm.cmd.angles[PITCH] = (short)ANGLE2SHORT( angles.Data()[PITCH] );
	pm.cmd.angles[YAW]   = (short)ANGLE2SHORT( angles.Data()[YAW] );

	VectorSet( playerStateForPmove.pmove.delta_angles, 0, 0, 0 );

	// Check for unsigned value wrapping
	Assert( this->predictionStepMillis && this->predictionStepMillis < 100 );
	Assert( this->predictionStepMillis % 16 == 0 );
	pm.cmd.msec = (uint8_t)this->predictionStepMillis;
	pm.cmd.serverTimeStamp = game.serverTime + this->totalMillisAhead;

	this->frameEvents.Clear();

	// Try using an already retrieved list if possible
	// (this saves some excessive bounds comparison)
	if( auto *shapeList = action->thisFrameCMShapeList ) {
		pmoveShapeList = shapeList;
		// Prevent further reuse
		action->thisFrameCMShapeList = nullptr;
	} else {
		pmoveShapeList = TraceCache().getShapeListForPMoveCollision( this );
	}

	pm.skipCollision = false;
	pmoveShouldTestContents = false;

	if( pmoveShapeList ) {
		if( SV_PossibleShapeListContents( pmoveShapeList ) & MASK_WATER ) {
			pmoveShouldTestContents = true;
		}
	} else {
		// The naive solution of supplying a dummy trace function
		// (that yields a zeroed output with fraction = 1) does not work.
		// An actual logic tied to this flag has to be added in Pmove() for each module_Trace() call.
		pm.skipCollision = true;
	}

	::currPredictionContext = this;

	// We currently test collisions only against a solid world on each movement step and the corresponding PMove() call.
	// Touching trigger entities is handled by Intercepted_PMoveTouchTriggers(), also we use AAS sampling for it.
	// Actions that involve touching trigger entities currently are never predicted ahead.
	// If an action really needs to test against entities, a corresponding prediction step flag
	// should be added and this interception of the module_Trace() should be skipped if the flag is set.

	// Save the G_GS_Trace() pointer
	auto oldModuleTrace = ggs->Trace;
	ggs->Trace = Intercepted_Trace;

	// Do not test entities contents for same reasons
	// Save the G_PointContents4D() pointer
	auto oldModulePointContents = ggs->PointContents;
	ggs->PointContents = Intercepted_PointContents;

	Pmove( ggs, &pm );

	// Restore the G_GS_Trace() pointer
	ggs->Trace = oldModuleTrace;
	// Restore the G_PointContents4D() pointer
	ggs->PointContents = oldModulePointContents;

	// Update the saved player state for using in the next prediction frame
	currMinimalPlayerState->pmove      = playerStateForPmove.pmove;
	currMinimalPlayerState->viewheight = playerStateForPmove.viewheight;

	// Update the entity physics state that is going to be used in the next prediction frame
	entityPhysicsState->UpdateFromPMove( &pm );
}

#ifdef CHECK_INFINITE_NEXT_STEP_LOOPS
int nextStepIterationsCounter;
#endif

void PredictionContext::Debug( const char *format, ... ) const {
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

void PredictionContext::CheatingAccelerate( float frac ) {
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
	// once much stricter bunny-hopping tests were implemented.
	const float oldVelocityZ = newVelocity.Z();
	// Nullify the boost Z velocity
	newVelocity.Z() = 0;
	// Normalize the velocity boost direction
	newVelocity *= Q_Rcp( entityPhysicsState.Speed2D() );
	// Make the velocity boost vector
	newVelocity *= ( frac * speedGainPerSecond ) * ( 0.001f * (float)this->oldStepMillis );
	// Add velocity boost to the entity velocity in the given physics state
	newVelocity += entityPhysicsState.Velocity();
	// Preserve the old velocity Z
	newVelocity.Z() = oldVelocityZ;

	record->SetModifiedVelocity( newVelocity );
}

void PredictionContext::CheatingCorrectVelocity( const vec3_t target ) {
	const auto &entityPhysicsState = this->movementState->entityPhysicsState;

	Vec3 toTargetDir2D( target );
	toTargetDir2D -= entityPhysicsState.Origin();
	toTargetDir2D.Z() = 0;
	if( toTargetDir2D.normalizeFast() && entityPhysicsState.Speed2D() > 1 ) {
		Vec3 velocity2DDir( entityPhysicsState.Velocity() );
		velocity2DDir.Z() = 0;
		velocity2DDir *= Q_Rcp( entityPhysicsState.Speed2D() );

		CheatingCorrectVelocity( velocity2DDir.Dot( toTargetDir2D ), toTargetDir2D );
	}
}

void PredictionContext::CheatingCorrectVelocity( float velocity2DDirDotToTarget2DDir, const Vec3 &toTargetDir2D ) {
	// Respect player class movement limitations
	if( !( this->currMinimalPlayerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_AIRCONTROL ) ) {
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
	const float oldVelocityZ = newVelocity.Z();
	// Normalize the current velocity direction
	newVelocity *= Q_Rcp( speed );
	// Modify the velocity direction
	newVelocity += 0.05f * toTargetDir2D;
	// Normalize the velocity direction again after modification
	if( !newVelocity.normalizeFast() ) {
		return;
	}

	// Restore the velocity magnitude
	newVelocity *= speed;
	// Disallow boosting Z velocity
	if( newVelocity.Z() > oldVelocityZ ) {
		newVelocity.Z() = oldVelocityZ;
		// Try normalizing again
		if( !newVelocity.normalizeFast() ) {
			return;
		}
		newVelocity *= speed;
	}

	record->SetModifiedVelocity( newVelocity );
}