#ifndef WSW_aca1833f_3aef_4970_aafe_63751e8fce1f_H
#define WSW_aca1833f_3aef_4970_aafe_63751e8fce1f_H

#include "../bot.h"

#include "predictioncontext.h"
#include "environmenttracecache.h"

// Suppress assertions for Windows non-public builds
#if !defined( PUBLIC_BUILD ) && !defined( _WIN32 )
#define ENABLE_MOVEMENT_ASSERTIONS
#endif

#if !defined( PUBLIC_BUILD )
#define CHECK_INFINITE_NEXT_STEP_LOOPS
extern int nextStepIterationsCounter;
static constexpr int NEXT_STEP_INFINITE_LOOP_THRESHOLD = 10000;
#endif

// Useful for debugging but freezes even Release version
#if 1
#define ENABLE_MOVEMENT_DEBUG_OUTPUT
#endif

// Should be applied to a view vector Z to avoid bending (but does not suit all cases).
constexpr float Z_NO_BEND_SCALE = 0.5f;
// A threshold of dot product of velocity dir and intended view dir
constexpr float STRAIGHT_MOVEMENT_DOT_THRESHOLD = 0.8f;

inline float GetPMoveStatValue( const pmove_state_t *pmove, int statIndex, float defaultValue ) {
	const float value = pmove->stats[statIndex];
	// Put likely case (the value is not specified) first
	return value < 0 ? defaultValue : value;
}

inline float PredictionContext::GetJumpSpeed() const {
	return GetPMoveStatValue( &this->currMinimalPlayerState->pmove, PM_STAT_JUMPSPEED, DEFAULT_JUMPSPEED * GRAVITY_COMPENSATE );
}

inline float PredictionContext::GetDashSpeed() const {
	return GetPMoveStatValue( &this->currMinimalPlayerState->pmove, PM_STAT_DASHSPEED, DEFAULT_DASHSPEED );
}

inline float PredictionContext::GetRunSpeed() const {
	return GetPMoveStatValue( &this->currMinimalPlayerState->pmove, PM_STAT_MAXSPEED, GS_DefaultPlayerSpeed( *ggs ) );
}

inline unsigned PredictionContext::DefaultFrameTime() const {
	return defaultFrameTime;
}

inline Vec3 PredictionContext::NavTargetOrigin() const {
	return bot->NavTargetOrigin();
}

inline float PredictionContext::NavTargetRadius() const {
	return bot->NavTargetRadius();
}

inline bool PredictionContext::IsCloseToNavTarget() const {
	float distance = NavTargetRadius() + 32.0f;
	return NavTargetOrigin().squareDistanceTo( movementState->entityPhysicsState.Origin() ) < distance * distance;
}

inline int PredictionContext::CurrAasAreaNum() const {
	if( int currAasAreaNum = movementState->entityPhysicsState.CurrAasAreaNum() ) {
		return currAasAreaNum;
	}

	return movementState->entityPhysicsState.DroppedToFloorAasAreaNum();
}

inline int PredictionContext::CurrGroundedAasAreaNum() const {
	const auto *aasWorld = AiAasWorld::instance();
	const auto &entityPhysicsState = movementState->entityPhysicsState;
	int areaNums[2] = { entityPhysicsState.CurrAasAreaNum(), entityPhysicsState.DroppedToFloorAasAreaNum() };
	for( int i = 0, end = ( areaNums[0] != areaNums[1] ? 2 : 1 ); i < end; ++i ) {
		if( areaNums[i] && aasWorld->isAreaGrounded( areaNums[i] ) ) {
			return areaNums[i];
		}
	}
	return 0;
}

inline int PredictionContext::NavTargetAasAreaNum() const {
	return bot->NavTargetAasAreaNum();
}

inline bool PredictionContext::IsInNavTargetArea() const {
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

inline void PredictionContext::Assert( bool condition, const char *message ) const {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	if( !condition ) {
		if( message ) {
			AI_FailWith( "PredictionContext::Assert()", "%s\n", message );
		} else {
			AI_FailWith( "PredictionContext::Assert()", "An assertion has failed\n" );
		}
	}
#endif
}

inline void BaseAction::Assert( bool condition, const char *message ) const {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	if( !condition ) {
		if( message ) {
			AI_FailWith("BaseAction::Assert()", "An assertion has failed: %s\n", message );
		} else {
			AI_FailWith("BaseAction::Assert()", "An assertion has failed\n");
		}
	}
#endif
}

inline const AiAasRouteCache *PredictionContext::RouteCache() const {
	return bot->RouteCache();
}

inline int PredictionContext::TravelFlags() const {
	return bot->TravelFlags();
}

inline EnvironmentTraceCache &PredictionContext::TraceCache() {
	return environmentTestResultsStack.back();
}

inline void PredictionContext::SaveActionOnStack( BaseAction *action ) {
	auto *topOfStack = &( *predictedMovementActions )[this->topOfStackIndex];
	// This was a source of an annoying bug! movement state has been modified during a prediction step!
	// We expect that record state is a saved state BEFORE the step!
	//topOfStack->entityPhysicsState = this->movementState->entityPhysicsState;
	topOfStack->action = action;
	// Make sure the angles can always be modified for input interpolation or aiming
	topOfStack->record.botInput.hasAlreadyComputedAngles = false;
	topOfStack->timestamp = this->totalMillisAhead;

#ifdef ENABLE_MOVEMENT_ASSERTIONS
	constexpr auto *tag = "PredictionContext::SaveActionOnStack()";
	if( !action ) {
		AI_FailWith( tag, "The action is null\n" );
	}
	if( this->predictionStepMillis > 16 * 16 ) {
		const char *format =
			"%s: The prediction step millis value %u is way too large. "
			"Is it a result of wrapping of negative values in unsigned context?\n";
		AI_FailWith( tag, format, action->getName(), this->predictionStepMillis );
	}
	if( this->predictionStepMillis % DefaultFrameTime() ) {
		const char *format = "%s: The prediction step millis value %u is no a multiple of %u\n";
		AI_FailWith( tag, format, action->getName(), this->predictionStepMillis, DefaultFrameTime() );
	}
#endif

	topOfStack->stepMillis = this->predictionStepMillis;
	this->topOfStackIndex++;
}

inline const char *PredictionContext::ActiveActionName() const {
	//return activeAction ? activeAction->Name() : nullptr;
	return "<unspecified>";
}

inline unsigned PredictionContext::MillisAheadForFrameStart( unsigned frameIndex ) const {
#ifdef ENABLE_MOVEMENT_ASSERTIONS
	constexpr auto *tag = "PredictionContext::MillisAheadForFrameStart()";
	constexpr auto *format = "The frame index %u must not be greater than the current ToS index %u\n";
	if( frameIndex > topOfStackIndex ) {
		AI_FailWith( tag, format, frameIndex, topOfStackIndex );
	}
#endif
	if( frameIndex < topOfStackIndex ) {
		return (unsigned)( ( *predictedMovementActions )[frameIndex].timestamp );
	}
	return totalMillisAhead;
}

inline auto BaseAction::genericCheckIsActionEnabled( PredictionContext *context ) const -> PredictionResult {
	// Put likely case first
	if( !m_isDisabledForPlanning ) {
		return PredictionResult::Continue;
	}

	Debug( "The action has been completely disabled for further planning\n" );
	return PredictionResult::Abort;
}

inline float Distance2DSquared( const vec3_t a, const vec3_t b ) {
	float dx = a[0] - b[0];
	float dy = a[1] - b[1];
	return dx * dx + dy * dy;
}

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
	const int travelFlags;

	int targetAreaNum { -1 };
	int startAreaNums[2] { 0, 0 };
	int numStartAreas { -1 };
	// Step temporaries that might be useful
	int lastTravelTime { 0 };
	int startAreaNum { 0 };
	int startReachNum { 0 };
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

	ReachChainWalker( const AiAasRouteCache *routeCache_, int travelFlags )
		: routeCache( routeCache_ ), travelFlags( travelFlags ) {}

	virtual bool Exec();
};

/**
 * Serves for candidate spots selection.
 * Tracing a straight line between two points fails in stairs-like environment way too often.
 * This routine uses extremely coarse arc approximation which still should be sufficient
 * to avoid the mentioned failure in some environment kinds.
 */
bool TraceArcInSolidWorld( const vec3_t from, const vec3_t to );

[[nodiscard]]
bool findBestStairsExitProps( const AiEntityPhysicsState &entityPhysicsState, int stairsClusterNum, const Bot *bot,
							  int *exitStairsAreaNum, int *exitReachNum = nullptr, int *exitTravelTime = nullptr );

[[nodiscard]]
auto findRampClusterExitReachNumAndTravelTime( const AiEntityPhysicsState &entityPhysicsState,
											   Bot *bot ) -> std::optional<std::pair<int, int>>;

/// \brief Contains signs of forward and right key values for 8 tested directions
inline const int kSideDirSigns[8][2] = {
	{ +1, +0 }, // forward
	{ -1, +0 }, // back
	{ +0, -1 }, // left
	{ +0, +1 }, // right
	{ +1, -1 }, // front left
	{ +1, +1 }, // front right
	{ -1, -1 }, // back left
	{ -1, +1 }, // back right
};

/// \brief Contains fractions for forward and right dirs for 8 tested directions
inline const float kSideDirFractions[8][2] = {
	{ +1.000f, +0.000f }, // front
	{ -1.000f, +0.000f }, // back
	{ +0.000f, -1.000f }, // left
	{ +0.000f, +1.000f }, // right
	{ +0.707f, -0.707f }, // front left
	{ +0.707f, +0.707f }, // front right
	{ -0.707f, -0.707f }, // back left
	{ -0.707f, +0.707f }, // back right
};

static_assert( std::size( kSideDirFractions ) == std::size( kSideDirSigns ) );

#endif
