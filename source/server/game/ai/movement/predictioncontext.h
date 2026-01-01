#ifndef WSW_86ec701e_71a0_4c5e_8b07_29fc000feb4e_H
#define WSW_86ec701e_71a0_4c5e_8b07_29fc000feb4e_H

class BaseAction;
class AiAasRouteCache;

#include "botinput.h"
#include "movementstate.h"
#include "floorclusterareascache.h"
#include "environmenttracecache.h"
#include "nearbytriggerscache.h"
#include "movementscript.h"

class Bot;
class MovementSubsystem;

class PredictionContext : public MovementPredictionConstants {
	friend class FallbackAction;
	friend struct wsw::ai::movement::NearbyTriggersCache;

	Bot *const bot;
	MovementSubsystem *const m_subsystem;
public:

private:
	using PredictedPath = wsw::StaticVector<PredictedMovementAction, MAX_PREDICTED_STATES>;

	struct MinimalSavedPlayerState {
		pmove_state_t pmove;
		float viewheight;
	};

	PredictedPath *const predictedMovementActions;
	wsw::StaticVector<MovementState, MAX_PREDICTED_STATES> botMovementStatesStack;
	wsw::StaticVector<MinimalSavedPlayerState, MAX_PREDICTED_STATES> playerStatesStack;

	player_state_t playerStateForPmove;

	MinimalSavedPlayerState minimalPlayerStateForFrame0;

	PredictedPath goodEnoughPath;
	unsigned goodEnoughPathAdvancement { 0 };
	unsigned goodEnoughPathPenalty { std::numeric_limits<unsigned>::max() };

	PredictedPath lastResortPath;
	unsigned lastResortPathPenalty { std::numeric_limits<unsigned>::max() };

	template <typename T, unsigned N>
	class CachesStack
	{
		static_assert( sizeof( uint64_t ) * 8 >= N, "64-bit bitset capacity overflow" );

		wsw::StaticVector<T, N> values;
		uint64_t isCachedBitset;

		inline void SetBit( unsigned bit ) { isCachedBitset |= ( ( (uint64_t)1 ) << bit ); }
		inline void ClearBit( unsigned bit ) { isCachedBitset &= ~( ( (uint64_t)1 ) << bit ); }

	public:
		CachesStack() : isCachedBitset( 0 ) {}

		void SetCachedValue( const T &value ) {
			assert( values.size() );
			SetBit( values.size() - 1 );
			values.back() = value;
		}
		void SetCachedValue( T &&value ) {
			assert( values.size() );
			SetBit( values.size() - 1 );
			values.back() = std::move( value );
		}
		// When cache stack growth for balancing is needed and no value exists for current stack pos, use this method
		void PushDummyNonCachedValue( T &&value = T() ) {
			ClearBit( values.size() );
			values.emplace_back( std::move( value ) );
		}
		const T *GetCached() const {
			assert( values.size() );
			return ( isCachedBitset & ( ( (uint64_t)1 ) << ( values.size() - 1 ) ) ) ? &values.back() : nullptr;
		}

		unsigned Size() const { return values.size(); }
		// Use when cache stack is being rolled back
		void PopToSize( unsigned newSize ) {
			assert( newSize <= values.size() );
			values.truncate( newSize );
		}
	};

	CachesStack<BotInput, MAX_PREDICTED_STATES> defaultBotInputsCachesStack;
	wsw::StaticVector<EnvironmentTraceCache, MAX_PREDICTED_STATES> environmentTestResultsStack;

	// We have decided to keep the frametime hardcoded.
	// The server code uses a hardcoded one too.
	// Its easy to change it here at least.
	const unsigned defaultFrameTime { 16 };
public:
	wsw::ai::movement::NearbyTriggersCache nearbyTriggersCache;

	MovementState *movementState;
	MovementActionRecord *record;

	MinimalSavedPlayerState *oldMinimalPlayerState { nullptr };
	MinimalSavedPlayerState *currMinimalPlayerState { nullptr };

	unsigned totalMillisAhead;
	unsigned predictionStepMillis;
	// Must be set to game.frameTime for the first step!
	unsigned oldStepMillis;

	unsigned topOfStackIndex;

	bool isCompleted;
	bool shouldRollback;

	struct FrameEvents {
		static constexpr auto MAX_TOUCHED_OTHER_TRIGGERS = 16;
		// Not teleports, jumppads or platforms (usually items).
		// Non-null classname is the only restriction applied.
		uint16_t otherTouchedTriggerEnts[MAX_TOUCHED_OTHER_TRIGGERS];
		int numOtherTouchedTriggers;

		uint16_t touchedJumppadEntNum;
		uint16_t touchedTeleporterEntNum;
		uint16_t touchedPlatformEntNum;

		bool hasJumped: 1;
		bool hasDoubleJumped: 1;
		bool hasDashed: 1;
		bool hasWalljumped: 1;
		bool hasTakenFallDamage: 1;

		FrameEvents() {
			Clear();
		}

		void Clear() {
			numOtherTouchedTriggers = 0;
			hasJumped = false;
			hasDoubleJumped = false;
			hasDashed = false;
			hasWalljumped = false;
			hasTakenFallDamage = false;
			touchedJumppadEntNum = 0;
			touchedTeleporterEntNum = 0;
			touchedPlatformEntNum = 0;
		}
	};

	FrameEvents frameEvents;

	int m_jumppadPathTriggerNum { 0 };
	int m_teleporterPathTriggerNum { 0 };
	int m_platformPathTriggerNum { 0 };

	// ClassifiedEntitiesCache stores parameters of all entities on the map for respective classes.
	// Assume that we won't move more than some limit of units during prediction.
	// We don't really predict movement past using any triggers as well.
	// Collecting nearby entities once before prediction helps to reduce NearbyTriggersCache refilling cost
	// which could otherwise explode on huge maps with lots of entities.
	// Q: What's the purpose of NearbyTriggersCache?
	// A: It's much tighter, as required for reduction of tests during each movement steps.
	// Q: What's the purpose of ClassifiedEntitiesCache?
	// A: It's still useful for other bots (that have a different origin) and for various other calculations.
	wsw::StaticVector<uint16_t, 12> m_teleporterEntNumsToUseDuringPrediction;
	wsw::StaticVector<uint16_t, 12> m_jumppadEntNumsToUseDuringPrediction;
	wsw::StaticVector<uint16_t, 12> m_platformTriggerEntNumsToUseDuringPrediction;
	wsw::StaticVector<uint16_t, 32> m_otherTriggerEntNumsToUseDuringPrediction;

	//class BaseAction *SuggestSuitableAction();
	//class BaseAction *SuggestDefaultAction();

	Vec3 NavTargetOrigin() const;
	float NavTargetRadius() const;
	bool IsCloseToNavTarget() const;
	int CurrAasAreaNum() const;
	int CurrGroundedAasAreaNum() const;
	int NavTargetAasAreaNum() const;
	bool IsInNavTargetArea() const;

	unsigned DefaultFrameTime() const;

	EnvironmentTraceCache &TraceCache();

	// Do not return boolean value, avoid extra branching. Checking results if necessary is enough.
	void NextReachNumAndTravelTimeToNavTarget( int *reachNum, int *travelTimeToNavTarget );

	int NextReachNum() {
		int results[2];
		NextReachNumAndTravelTimeToNavTarget( results, results + 1 );
		return results[0];
	}
	int TravelTimeToNavTarget() {
		int results[2];
		NextReachNumAndTravelTimeToNavTarget( results, results + 1 );
		return results[1];
	}

	const AiAasRouteCache *RouteCache() const;
	int TravelFlags() const;

	explicit PredictionContext( MovementSubsystem *subsystem, PredictedPath *_predictedMovementActions );

	bool BuildPlan( std::span<BaseAction *> actionsToUse );
	void TryBuildingPlanUsingAction( BaseAction *action );
	bool NextPredictionStep( BaseAction *action, bool *hasStartedSequence );
	void SetupStackForStep();

	void NextMovementStep( BaseAction *action );

	void SavePathTriggerNums();
	void SaveNearbyEntities();

	const AiEntityPhysicsState &PhysicsStateBeforeStep() const {
		return ( *predictedMovementActions )[topOfStackIndex].entityPhysicsState;
	}

	bool CanGrowStackForNextStep() const {
		// Note: topOfStackIndex is an array index, MAX_PREDICTED_STATES is an array size
		return this->topOfStackIndex + 1 < MAX_PREDICTED_STATES;
	}

	void SaveActionOnStack( BaseAction *action );

	const char *ActiveActionName() const;

	void SetPendingRollback();
	void RollbackToSavepoint();
	//void SaveSuggestedActionForNextFrame( BaseAction *action );
	unsigned MillisAheadForFrameStart( unsigned frameIndex ) const;

	void SaveGoodEnoughPath( unsigned advancement, unsigned penaltyMillis );
	void SaveLastResortPath( unsigned penaltyMillis );

	void Debug( const char *format, ... ) const;
	// We want to have a full control over movement code assertions, so use custom ones for this class
	void Assert( bool condition, const char *message = nullptr ) const;
	template <typename T>
	void Assert( T conditionLikeValue, const char *message = nullptr ) const {
		Assert( conditionLikeValue != 0, message );
	}

	float GetRunSpeed() const;
	float GetJumpSpeed() const;
	float GetDashSpeed() const;

	void CheatingAccelerate( float frac );

	void CheatingCorrectVelocity( const Vec3 &target ) {
		CheatingCorrectVelocity( target.Data() );
	}

	void CheatingCorrectVelocity( const vec3_t target );
	void CheatingCorrectVelocity( float velocity2DDirDotToTarget2DDir, const Vec3 &toTargetDir2D );

	void OnInterceptedPredictedEvent( int ev, int parm );
	void OnInterceptedPMoveTouchTriggers( pmove_t *pm, vec3_t const previousOrigin );

	// Might be called for failed attempts too
	void ShowBuiltPlanPath( bool useActionsColor = false ) const;
};

#endif
