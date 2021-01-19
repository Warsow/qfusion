#ifndef WSW_aca1833f_3aef_4970_aafe_63751e8fce1f_H
#define WSW_aca1833f_3aef_4970_aafe_63751e8fce1f_H

#include "../bot.h"

#include "predictioncontext.h"
#include "environmenttracecache.h"

// For macOS Clang
#include <cmath>
#include <cstdlib>

#ifndef PUBLIC_BUILD
#define CHECK_ACTION_SUGGESTION_LOOPS
#define ENABLE_MOVEMENT_ASSERTIONS
#define CHECK_INFINITE_NEXT_STEP_LOOPS
extern int nextStepIterationsCounter;
static constexpr int NEXT_STEP_INFINITE_LOOP_THRESHOLD = 10000;
#endif

// Useful for debugging but freezes even Release version
#if 0
#define ENABLE_MOVEMENT_DEBUG_OUTPUT
#endif

// Should be applied to a view vector Z to avoid bending (but does not suit all cases).
constexpr float Z_NO_BEND_SCALE = 0.5f;
// A threshold of dot product of velocity dir and intended view dir
constexpr float STRAIGHT_MOVEMENT_DOT_THRESHOLD = 0.8f;

inline float GetPMoveStatValue( const player_state_t *playerState, int statIndex, float defaultValue ) {
	float value = playerState->pmove.stats[statIndex];
	// Put likely case (the value is not specified) first
	return value < 0 ? defaultValue : value;
}

inline float MovementPredictionContext::GetJumpSpeed() const {
	return GetPMoveStatValue( this->currPlayerState, PM_STAT_JUMPSPEED, DEFAULT_JUMPSPEED * GRAVITY_COMPENSATE );
}

inline float MovementPredictionContext::GetDashSpeed() const {
	return GetPMoveStatValue( this->currPlayerState, PM_STAT_DASHSPEED, DEFAULT_DASHSPEED );
}

inline float MovementPredictionContext::GetRunSpeed() const {
	return GetPMoveStatValue( this->currPlayerState, PM_STAT_MAXSPEED, DEFAULT_PLAYERSPEED );
}

inline unsigned MovementPredictionContext::DefaultFrameTime() const {
	return defaultFrameTime;
}

inline Vec3 MovementPredictionContext::NavTargetOrigin() const {
	return bot->NavTargetOrigin();
}

inline float MovementPredictionContext::NavTargetRadius() const {
	return bot->NavTargetRadius();
}

inline bool MovementPredictionContext::IsCloseToNavTarget() const {
	float distance = NavTargetRadius() + 32.0f;
	return NavTargetOrigin().SquareDistanceTo( movementState->entityPhysicsState.Origin() ) < distance * distance;
}

inline int MovementPredictionContext::CurrAasAreaNum() const {
	if( int currAasAreaNum = movementState->entityPhysicsState.CurrAasAreaNum() ) {
		return currAasAreaNum;
	}

	return movementState->entityPhysicsState.DroppedToFloorAasAreaNum();
}

inline int MovementPredictionContext::CurrGroundedAasAreaNum() const {
	const auto *aasWorld = AiAasWorld::Instance();
	const auto &entityPhysicsState = movementState->entityPhysicsState;
	int areaNums[2] = { entityPhysicsState.CurrAasAreaNum(), entityPhysicsState.DroppedToFloorAasAreaNum() };
	for( int i = 0, end = ( areaNums[0] != areaNums[1] ? 2 : 1 ); i < end; ++i ) {
		if( areaNums[i] && aasWorld->AreaGrounded( areaNums[i] ) ) {
			return areaNums[i];
		}
	}
	return 0;
}

inline int MovementPredictionContext::NavTargetAasAreaNum() const {
	return bot->NavTargetAasAreaNum();
}

inline bool MovementPredictionContext::IsInNavTargetArea() const {
	const int navTargetAreaNum = NavTargetAasAreaNum();
	if( !navTargetAreaNum ) {
		return false;
	}

	const auto &entityPhysicsState = this->movementState->entityPhysicsState;
	if( navTargetAreaNum == entityPhysicsState.CurrAasAreaNum() ) {
		return true;
	}

	if( navTargetAreaNum == entityPhysicsState.DroppedToFloorAasAreaNum() ) {
		return true;
	}

	return false;
}

inline bool IsInsideHugeArea( const float *origin, const aas_area_t &area, float offset ) {
	if( area.mins[0] > origin[0] - offset || area.maxs[0] < origin[0] + offset ) {
		return false;
	}

	if( area.mins[1] > origin[1] - offset || area.maxs[1] < origin[1] + offset ) {
		return false;
	}

	return true;
}

inline void MovementPredictionContext::Assert( bool condition, const char *message ) const {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	if( !condition ) {
		if( message ) {
			AI_FailWith( "MovementPredictionContext::Assert()", "%s\n", message );
		} else {
			AI_FailWith( "MovementPredictionContext::Assert()", "An assertion has failed\n" );
		}
	}
#endif
}

inline void BaseMovementAction::Assert( bool condition, const char *message ) const {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	if( !condition ) {
		if( message ) {
			AI_FailWith("BaseMovementAction::Assert()", "An assertion has failed: %s\n", message );
		} else {
			AI_FailWith("BaseMovementAction::Assert()", "An assertion has failed\n");
		}
	}
#endif
}

inline const AiAasRouteCache *MovementPredictionContext::RouteCache() const {
	return bot->RouteCache();
}

inline const ArrayRange<int> MovementPredictionContext::TravelFlags() const {
	return bot->TravelFlags();
}

inline EnvironmentTraceCache &MovementPredictionContext::TraceCache() {
	return environmentTestResultsStack.back();
}

inline void MovementPredictionContext::SaveActionOnStack( BaseMovementAction *action ) {
	auto *topOfStack = &this->predictedMovementActions[this->topOfStackIndex];
	// This was a source of an annoying bug! movement state has been modified during a prediction step!
	// We expect that record state is a saved state BEFORE the step!
	//topOfStack->entityPhysicsState = this->movementState->entityPhysicsState;
	topOfStack->action = action;
	// Make sure the angles can always be modified for input interpolation or aiming
	topOfStack->record.botInput.hasAlreadyComputedAngles = false;
	topOfStack->timestamp = this->totalMillisAhead;

#ifdef ENABLE_MOVEMENT_ASSERTIONS
	constexpr auto *tag = "MovementPredictionContext::SaveActionOnStack()";
	if( !action ) {
		AI_FailWith( tag, "The action is null\n" );
	}
	if( this->predictionStepMillis > 100 ) {
		const char *format =
			"%s: The prediction step millis value %u is way too large. "
			"Is it a result of wrapping of negative values in unsigned context?\n";
		AI_FailWith( tag, format, action->Name() );
	}
	if( this->predictionStepMillis % DefaultFrameTime() ) {
		const char *format = "%s: The prediction step millis value %u is no a multiple of %u\n";
		AI_FailWith( tag, format, action->Name(), this->predictionStepMillis, DefaultFrameTime() );
	}
#endif

	topOfStack->stepMillis = this->predictionStepMillis;
	this->topOfStackIndex++;
}

inline const char *MovementPredictionContext::ActiveActionName() const {
	return activeAction ? activeAction->Name() : nullptr;
}

inline void MovementPredictionContext::MarkSavepoint( BaseMovementAction *markedBy, unsigned frameIndex ) {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	constexpr auto *tag = "MovementPredictionContext::MarkSavepoint()";
	if( !markedBy ) {
		AI_FailWith( tag, "`markedBy` action is null\n" );
	}
	if( this->cannotApplyAction ) {
		constexpr auto *format = "%s: Attempt to mark a savepoint while `cannotApplyAction` context flag is set\n";
		AI_FailWith( tag, format, markedBy->Name() );
	}
	if( this->shouldRollback ) {
		constexpr auto *format = "%s: Attempt to mark a savepoint while `shouldRollback` context flag is set\n";
		AI_FailWith( tag, format, markedBy->Name() );
	}
	if( frameIndex != this->topOfStackIndex && frameIndex != this->topOfStackIndex + 1 ) {
		constexpr auto *format =
			"%s: Attempt to mark a savepoint at index %d while ToS index is %d:"
			" the savepoint index must be the same or be a first next index\n";
		AI_FailWith( tag, format, markedBy->Name(), frameIndex, this->topOfStackIndex );
	}
#endif

	this->savepointTopOfStackIndex = frameIndex;
	Debug( "%s has marked frame %d as a savepoint\n", markedBy->Name(), frameIndex );
}

inline void MovementPredictionContext::SetPendingRollback() {
	this->cannotApplyAction = true;
	this->shouldRollback = true;

#ifdef ENABLE_MOVEMENT_ASSERTIONS
	if( !this->isCompleted ) {
		return;
	}

	constexpr auto *tag = "MovementPredictionContext::SetPendingRollback()";
	constexpr auto *format = "%s: Attempt to rollback while the context is in completed state\n";
	AI_FailWith( tag, format, ActiveActionName() );
#endif
}

inline void MovementPredictionContext::RollbackToSavepoint() {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	constexpr auto *tag = "MovementPredictionContext::RollbackToSavepoint()";
	if( this->isCompleted ) {
		constexpr auto *format = "%s: Attempt to rollback while the context is in completed state\n";
		AI_FailWith( tag, format, ActiveActionName() );
	}
	if( !this->shouldRollback ) {
		constexpr auto *format = "%s: Attempt to rollback while `shouldRollback` context flag is not set\n";
		AI_FailWith( tag, format, ActiveActionName() );
	}
	if( !this->cannotApplyAction ) {
		constexpr auto *format = "%s: Attempt to rollback while `cannotApplyAction` context flag is not set\n";
		AI_FailWith( tag, format, ActiveActionName() );
	}
	if( this->savepointTopOfStackIndex > this->topOfStackIndex ) {
		constexpr auto *format = "The savepoint index %u is greater than the current ToS index %u\n";
		AI_FailWith( tag, format, this->savepointTopOfStackIndex, this->topOfStackIndex );
	}
#endif

	constexpr const char *format = "Rolling back to savepoint frame %d from ToS frame %d\n";
	Debug( format, this->savepointTopOfStackIndex, this->topOfStackIndex );
	this->topOfStackIndex = this->savepointTopOfStackIndex;
}

inline void MovementPredictionContext::SaveSuggestedActionForNextFrame( BaseMovementAction *action ) {
	//Assert(!this->actionSuggestedByAction);
	this->actionSuggestedByAction = action;
}

inline unsigned MovementPredictionContext::MillisAheadForFrameStart( unsigned frameIndex ) const {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	constexpr auto *tag = "MovementPredictionContext::MillisAheadForFrameStart()";
	constexpr auto *format = "The frame index %u must not be greater than the current ToS index %u\n";
	if( frameIndex > topOfStackIndex ) {
		AI_FailWith( tag, format, frameIndex, topOfStackIndex );
	}
#endif
	if( frameIndex < topOfStackIndex ) {
		return (unsigned)( predictedMovementActions[frameIndex].timestamp );
	}
	return totalMillisAhead;
}

inline bool BaseMovementAction::GenericCheckIsActionEnabled( MovementPredictionContext *context,
															 BaseMovementAction *suggestedAction ) const {
	// Put likely case first
	if( !isDisabledForPlanning ) {
		return true;
	}

	context->sequenceStopReason = DISABLED;
	context->cannotApplyAction = true;
	context->actionSuggestedByAction = suggestedAction;
	Debug( "The action has been completely disabled for further planning\n" );
	return false;
}

typedef MovementPredictionContext Context;

inline void BaseMovementAction::CheckDisableOrSwitchPreconditions( Context *context, const char *methodTag ) {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	if( context->isCompleted ) {
		AI_FailWith( va( "%s::%s()", Name(), methodTag ), "The context must not have `isCompleted` flag set" );
	}
	if( context->cannotApplyAction ) {
		AI_FailWith( va( "%s::%s()", Name(), methodTag ), "The context must not have `cannotApplyAction` flag set" );
	}
	if( context->shouldRollback ) {
		AI_FailWith( va( "%s::%s()", Name(), methodTag ), "The context must not have `shouldRollback` flag set" );
	}
	if( this->isDisabledForPlanning ) {
		AI_FailWith( va( "%s::%s()", Name(), methodTag ), "The action must not have been already disabled for planning" );
	}
#endif
}

inline void BaseMovementAction::DisableWithAlternative( Context *context, BaseMovementAction *suggestedAction ) {
	CheckDisableOrSwitchPreconditions( context, "DisableWithAlternative" );

	context->cannotApplyAction = true;
	context->actionSuggestedByAction = suggestedAction;
	this->isDisabledForPlanning = true;
}

inline void BaseMovementAction::SwitchOrStop( Context *context, BaseMovementAction *suggestedAction ) {
	CheckDisableOrSwitchPreconditions( context, "SwitchOrStop" );

	// Few predicted frames are enough if the action cannot be longer applied (but have not caused rollback)
	if( context->topOfStackIndex > 0 ) {
		Debug( "There were enough successfully predicted frames anyway, stopping prediction\n" );
		context->isCompleted = true;
		return;
	}

	DisableWithAlternative( context, suggestedAction );
}

inline void BaseMovementAction::SwitchOrRollback( Context *context, BaseMovementAction *suggestedAction ) {
	CheckDisableOrSwitchPreconditions( context, "SwitchOrRollback" );

	if( context->topOfStackIndex > 0 ) {
		Debug( "There were some frames predicted ahead that lead to a failure, should rollback\n" );
		this->isDisabledForPlanning = true;
		context->SetPendingRollback();
		return;
	}

	DisableWithAlternative( context, suggestedAction );
}

inline float Distance2DSquared( const vec3_t a, const vec3_t b ) {
	float dx = a[0] - b[0];
	float dy = a[1] - b[1];
	return dx * dx + dy * dy;
}

#ifndef SQUARE
#define SQUARE( x ) ( ( x ) * ( x ) )
#endif

static inline bool ShouldCrouchSlideNow( MovementPredictionContext *context ) {
	if( !( context->currPlayerState->pmove.stats[PM_STAT_FEATURES] & PMFEAT_CROUCHSLIDING ) ) {
		return false;
	}

	if( context->currPlayerState->pmove.pm_flags & PMF_CROUCH_SLIDING ) {
		if( context->currPlayerState->pmove.stats[PM_STAT_CROUCHSLIDETIME] > PM_CROUCHSLIDE_FADE ) {
			return true;
		}
	}

	if( context->movementState->entityPhysicsState.Speed2D() > context->GetRunSpeed() * 1.2f ) {
		return true;
	}

	return false;
}

// Height threshold should be set according to used time step
// (we might miss crouch sliding activation if its low and the time step is large)
inline bool ShouldPrepareForCrouchSliding( MovementPredictionContext *context, float heightThreshold = 12.0f ) {
	if( !(context->currPlayerState->pmove.stats[PM_STAT_FEATURES ] & PMFEAT_CROUCHSLIDING ) ) {
		return false;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( entityPhysicsState.GroundEntity() ) {
		return false;
	}

	if( entityPhysicsState.Velocity()[2] > 0 ) {
		return false;
	}

	if( entityPhysicsState.HeightOverGround() > heightThreshold ) {
		return false;
	}

	if( entityPhysicsState.Speed() < context->GetRunSpeed() ) {
		return false;
	}

	return true;
}

class TriggerAreaNumsCache {
	mutable int areaNums[MAX_EDICTS];
public:
	TriggerAreaNumsCache() {
		memset( areaNums, 0, sizeof( areaNums ) );
	}

	int GetAreaNum( int entNum ) const;
};

extern TriggerAreaNumsCache triggerAreaNumsCache;

class RegionBoundsCache {
	const char *const tag;
	const float *const addToMins;
	const float *const addToMaxs;

	mutable int64_t hits { 0 };
	mutable int64_t total { 0 };

	vec3_t cachedForMins { -99999, -99999, -99999 };
	vec3_t cachedForMaxs { -99998, -99998, -99998 };

	// This approach looks much cleaner rather multiple ifdefs spread over the code
#ifndef PUBLIC_BUILD
	static constexpr auto profileHits = true;
#else
	static constexpr auto profileHits = false;
#endif
public:
	RegionBoundsCache( const char *tag_, const float *addToMins_, const float *addToMaxs_ ) noexcept
		: tag( tag_ ), addToMins( addToMins_ ), addToMaxs( addToMaxs_ ) {
	}

	[[nodiscard]]
	std::pair<const float *, const float *> getCachedBounds() const {
		return std::make_pair( cachedForMins, cachedForMaxs );
	}

	void setFrom( const RegionBoundsCache &that ) {
		assert( VectorCompare( addToMins, that.addToMins ) );
		assert( VectorCompare( addToMaxs, that.addToMaxs ) );
		VectorCopy( that.cachedForMins, cachedForMins );
		VectorCopy( that.cachedForMaxs, cachedForMaxs );
	}

	[[nodiscard]]
	bool checkOrUpdateBounds( const float *mins, const float *maxs ) {
		if( profileHits ) {
			total++;
		}

		// TODO: Use SIMD if it becomes noticeable at profiling results
		bool isWithinBounds =
			( mins[0] > cachedForMins[0] ) & ( mins[1] > cachedForMins[1] ) & ( mins[2] > cachedForMins[2] ) &
			( maxs[0] < cachedForMaxs[0] ) & ( maxs[1] < cachedForMaxs[1] ) & ( maxs[2] < cachedForMaxs[2] );

		if( isWithinBounds ) {
			if( profileHits ) {
				hits++;
			}
			return true;
		}

		VectorAdd( mins, addToMins, cachedForMins );
		VectorAdd( maxs, addToMaxs, cachedForMaxs );
		return false;
	}

	~RegionBoundsCache() {
		if( !tag || !profileHits || !total ) {
			return;
		}
		double rate = (double)hits / (double)total;
		printf( "RegionBoundsCache@%s::~RegionBoundsCache(): hit rate was %f\n", tag, rate);
	}
};

class CollisionTopNodeCache {
	mutable RegionBoundsCache defaultBoundsCache;
	mutable RegionBoundsCache zeroStepBoundsCache;
	mutable std::optional<int> defaultCachedNode;
	mutable std::optional<int> cachedZeroStepNode;
public:
	CollisionTopNodeCache() noexcept;

	int getTopNode( const float *absMins, const float *absMaxs, bool izZeroStep ) const;
};

extern CollisionTopNodeCache collisionTopNodeCache;

class CollisionShapesListCache {
	mutable CMShapeList *activeCachedList { nullptr };
	mutable CMShapeList *defaultCachedList { nullptr };
	mutable CMShapeList *defaultClippedList { nullptr };
	mutable CMShapeList *zeroStepCachedList { nullptr };
	mutable CMShapeList *zeroStepClippedList { nullptr };
	mutable RegionBoundsCache defaultBoundsCache;
	mutable RegionBoundsCache zeroStepBoundsCache;

	const CMShapeList *defaultPrepareList( const float *mins, const float *maxs ) const;
public:
	CollisionShapesListCache() noexcept;
	~CollisionShapesListCache();

	const CMShapeList *prepareList( const float *mins, const float *maxs, bool isZeroStep ) const;
};

extern CollisionShapesListCache shapesListCache;

class ReachChainWalker {
protected:
	const AiAasRouteCache *const routeCache;
	int targetAreaNum { -1 };
	int startAreaNums[2] { 0, 0 };
	int numStartAreas { -1 };
	// Step temporaries that might be useful
	int lastTravelTime { 0 };
	int startAreaNum { 0 };
	int lastAreaNum { 0 };
	int lastReachNum { 0 };

	virtual bool Accept( int reachNum, const aas_reachability_t &reach, int travelTime ) = 0;
public:
	void SetAreaNums( const int *startAreaNums_, int numStartAreas_, int targetAreaNum_ ) {
		Vector2Copy( startAreaNums_, startAreaNums );
		this->numStartAreas = numStartAreas_;
		this->targetAreaNum = targetAreaNum_;
	}

	void SetAreaNums( const AiEntityPhysicsState &physicsState, int targetAreaNum_ ) {
		numStartAreas = physicsState.PrepareRoutingStartAreas( startAreaNums );
		this->targetAreaNum = targetAreaNum_;
	}

	explicit ReachChainWalker( const AiAasRouteCache *routeCache_ )
		: routeCache( routeCache_ ) {}

	virtual bool Exec();
};

int TravelTimeWalkingOrFallingShort( const AiAasRouteCache *routeCache, int fromAreaNum, int toAreaNum );

/**
 * Serves for candidate spots selection.
 * Tracing a straight line between two points fails in stairs-like environment way too often.
 * This routine uses extremely coarse arc approximation which still should be sufficient
 * to avoid the mentioned failure in some environment kinds.
 */
bool TraceArcInSolidWorld( const vec3_t from, const vec3_t to );

void DirToKeyInput( const Vec3 &desiredDir, const vec3_t actualForwardDir, const vec3_t actualRightDir, BotInput *input );

#endif
