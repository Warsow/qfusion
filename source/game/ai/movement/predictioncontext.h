#ifndef WSW_86ec701e_71a0_4c5e_8b07_29fc000feb4e_H
#define WSW_86ec701e_71a0_4c5e_8b07_29fc000feb4e_H

class BaseMovementAction;

#include "botinput.h"
#include "movementstate.h"
#include "floorclusterareascache.h"
#include "environmenttracecache.h"
#include "nearbytriggerscache.h"

struct MovementActionRecord {
	BotInput botInput;

private:
	int16_t modifiedVelocity[3];

public:
	int8_t pendingWeapon : 7;
	bool hasModifiedVelocity : 1;

	inline MovementActionRecord()
		: pendingWeapon( -1 ),
		  hasModifiedVelocity( false ) {}

	inline void Clear() {
		botInput.Clear();
		pendingWeapon = -1;
		hasModifiedVelocity = false;
	}

	inline void SetModifiedVelocity( const Vec3 &velocity ) {
		SetModifiedVelocity( velocity.Data() );
	}

	inline void SetModifiedVelocity( const vec3_t velocity ) {
		for( int i = 0; i < 3; ++i ) {
			int snappedVelocityComponent = (int)( velocity[i] * 16.0f );
			if( snappedVelocityComponent > std::numeric_limits<signed short>::max() ) {
				snappedVelocityComponent = std::numeric_limits<signed short>::max();
			} else if( snappedVelocityComponent < std::numeric_limits<signed short>::min() ) {
				snappedVelocityComponent = std::numeric_limits<signed short>::min();
			}
			modifiedVelocity[i] = (signed short)snappedVelocityComponent;
		}
		hasModifiedVelocity = true;
	}

	inline Vec3 ModifiedVelocity() const {
		assert( hasModifiedVelocity );
		float scale = 1.0f / 16.0f;
		return Vec3( scale * modifiedVelocity[0], scale * modifiedVelocity[1], scale * modifiedVelocity[2] );
	}
};

struct MovementPredictionConstants {
	enum SequenceStopReason : uint8_t {
		UNSPECIFIED, // An empty initial value, should be replaced by SWITCHED on actual use
		SUCCEEDED,   // The sequence has been completed successfully
		SWITCHED,    // The action cannot be applied in the current environment, another action is suggested
		DISABLED,    // The action is disabled for application, another action is suggested
		FAILED       // A prediction step has lead to a failure
	};

	static constexpr unsigned MAX_SAVED_LANDING_AREAS = 16;
};

class Bot;
class BotMovementModule;
class BaseScript2;

class MovementPredictionContext : public MovementPredictionConstants
{
	friend class FallbackMovementAction;
	friend class BaseScript2;
	friend class BotMovementModule;

	Bot *const bot;
	BaseScript2 *m_script;
public:
	// Note: We have deliberately lowered this value
	// to prevent fruitless prediction frames that lead to an overflow anyway
	// once much more stricter bunnying checks are implemented
	static constexpr unsigned MAX_PREDICTED_STATES = 32;

	struct alignas ( 1 )HitWhileRunningTestResult {
		bool canHitAsIs : 1;
		bool mayHitOverridingPitch : 1;

		inline HitWhileRunningTestResult()
		{
			static_assert( sizeof( *this ) == 1, "" );
			*( (uint8_t *)( this ) ) = 0;
		}

		inline bool CanHit() const { return canHitAsIs || mayHitOverridingPitch; }

		// Use the method and not a static var (the method result should be inlined w/o any static memory access)
		static inline HitWhileRunningTestResult Failure() { return HitWhileRunningTestResult(); }
	};
private:
	struct PredictedMovementAction {
		AiEntityPhysicsState entityPhysicsState;
		MovementActionRecord record;
		BaseMovementAction *action;
		int64_t timestamp;
		unsigned stepMillis;
		unsigned movementStatesMask;

		PredictedMovementAction()
			: action(nullptr),
			  timestamp( 0 ),
			  stepMillis( 0 ),
			  movementStatesMask( 0 ) {}
	};

	using PredictedPath = wsw::StaticVector<PredictedMovementAction, MAX_PREDICTED_STATES>;

	PredictedPath predictedMovementActions;
	wsw::StaticVector<BotMovementState, MAX_PREDICTED_STATES> botMovementStatesStack;
	wsw::StaticVector<player_state_t, MAX_PREDICTED_STATES> playerStatesStack;

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
		inline CachesStack() : isCachedBitset( 0 ) {}

		inline void SetCachedValue( const T &value ) {
			assert( values.size() );
			SetBit( values.size() - 1 );
			values.back() = value;
		}
		inline void SetCachedValue( T &&value ) {
			assert( values.size() );
			SetBit( values.size() - 1 );
			values.back() = std::move( value );
		}
		// When cache stack growth for balancing is needed and no value exists for current stack pos, use this method
		inline void PushDummyNonCachedValue( T &&value = T() ) {
			ClearBit( values.size() );
			values.emplace_back( std::move( value ) );
		}
		// Should be used when the cached type cannot be copied or moved (use this pointer to allocate a value in-place)
		inline T *UnsafeGrowForNonCachedValue() {
			ClearBit( values.size() );
			return values.unsafe_grow_back();
		}
		inline T *GetUnsafeBufferForCachedValue() {
			SetBit( values.size() - 1 );
			return &values[0] + ( values.size() - 1 );
		}
		inline const T *GetCached() const {
			assert( values.size() );
			return ( isCachedBitset & ( ( (uint64_t)1 ) << ( values.size() - 1 ) ) ) ? &values.back() : nullptr;
		}
		inline const T *GetCachedValueBelowTopOfStack() const {
			assert( values.size() );
			if( values.size() == 1 ) {
				return nullptr;
			}
			return ( isCachedBitset & ( ( (uint64_t)1 ) << ( values.size() - 2 ) ) ) ? &values[values.size() - 2] : nullptr;
		}

		inline unsigned Size() const { return values.size(); }
		// Use when cache stack is being rolled back
		inline void PopToSize( unsigned newSize ) {
			assert( newSize <= values.size() );
			values.truncate( newSize );
		}
	};

	CachesStack<BotInput, MAX_PREDICTED_STATES> defaultBotInputsCachesStack;
	CachesStack<HitWhileRunningTestResult, MAX_PREDICTED_STATES> mayHitWhileRunningCachesStack;
	wsw::StaticVector<EnvironmentTraceCache, MAX_PREDICTED_STATES> environmentTestResultsStack;

	// We have decided to keep the frametime hardcoded.
	// The server code uses a hardcoded one too.
	// Its easy to change it here at least.
	const unsigned defaultFrameTime { 16 };
public:
	wsw::ai::movement::NearbyTriggersCache nearbyTriggersCache;

	BotMovementState *movementState;
	MovementActionRecord *record;

	const player_state_t *oldPlayerState;
	player_state_t *currPlayerState;

	BaseMovementAction *actionSuggestedByAction;
	BaseMovementAction *activeAction;

	unsigned totalMillisAhead;
	unsigned predictionStepMillis;
	// Must be set to game.frameTime for the first step!
	unsigned oldStepMillis;

	unsigned topOfStackIndex;
	unsigned savepointTopOfStackIndex;

	SequenceStopReason sequenceStopReason;
	bool isCompleted;
	bool isTruncated;
	bool cannotApplyAction;
	bool shouldRollback;

	struct FrameEvents {
		static constexpr auto MAX_TOUCHED_OTHER_TRIGGERS = 16;
		// Not teleports, jumppads or platforms (usually items).
		// Non-null classname is the only restriction applied.
		uint16_t otherTouchedTriggerEnts[MAX_TOUCHED_OTHER_TRIGGERS];
		int numOtherTouchedTriggers;

		bool hasJumped: 1;
		bool hasDoubleJumped: 1;
		bool hasDashed: 1;
		bool hasWalljumped: 1;
		bool hasTakenFallDamage: 1;

		bool hasTouchedJumppad: 1;
		bool hasTouchedTeleporter: 1;
		bool hasTouchedPlatform: 1;

		inline FrameEvents() {
			Clear();
		}

		inline void Clear() {
			numOtherTouchedTriggers = 0;
			hasJumped = false;
			hasDoubleJumped = false;
			hasDashed = false;
			hasWalljumped = false;
			hasTakenFallDamage = false;
			hasTouchedJumppad = false;
			hasTouchedTeleporter = false;
			hasTouchedPlatform = false;
		}
	};

	FrameEvents frameEvents;

	inline Vec3 NavTargetOrigin() const;
	inline float NavTargetRadius() const;
	inline bool IsCloseToNavTarget() const;
	inline int CurrAasAreaNum() const;
	inline int CurrGroundedAasAreaNum() const;
	inline int NavTargetAasAreaNum() const;
	inline bool IsInNavTargetArea() const;

	inline unsigned DefaultFrameTime() const;

	inline EnvironmentTraceCache &TraceCache();

	// Do not return boolean value, avoid extra branching. Checking results if necessary is enough.
	void NextReachNumAndTravelTimeToNavTarget( int *reachNum, int *travelTimeToNavTarget );

	inline int NextReachNum() {
		int results[2];
		NextReachNumAndTravelTimeToNavTarget( results, results + 1 );
		return results[0];
	}
	inline int TravelTimeToNavTarget() {
		int results[2];
		NextReachNumAndTravelTimeToNavTarget( results, results + 1 );
		return results[1];
	}

	inline const AiAasRouteCache *RouteCache() const;
	inline const ArrayRange<int> TravelFlags() const;

	explicit MovementPredictionContext( Bot *bot_, BaseScript2 *script_ );

	HitWhileRunningTestResult MayHitWhileRunning();

	void BuildPlan();
	bool NextPredictionStep();
	void SetupStackForStep();

	void NextMovementStep();

	inline const AiEntityPhysicsState &PhysicsStateBeforeStep() const {
		return predictedMovementActions[topOfStackIndex].entityPhysicsState;
	}

	inline bool CanGrowStackForNextStep() const {
		// Note: topOfStackIndex is an array index, MAX_PREDICTED_STATES is an array size
		return this->topOfStackIndex + 1 < MAX_PREDICTED_STATES;
	}

	inline void SaveActionOnStack( BaseMovementAction *action );

	// Frame index is restricted to topOfStack or topOfStack + 1
	inline void MarkSavepoint( BaseMovementAction *markedBy, unsigned frameIndex );

	inline const char *ActiveActionName() const;

	inline void SetPendingRollback();
	inline void RollbackToSavepoint();
	inline void SaveSuggestedActionForNextFrame( BaseMovementAction *action );
	inline unsigned MillisAheadForFrameStart( unsigned frameIndex ) const;

	void SaveGoodEnoughPath( unsigned advancement, unsigned penaltyMillis );
	void SaveLastResortPath( unsigned penaltyMillis );

	class BaseMovementAction *GetCachedActionAndRecordForCurrTime( MovementActionRecord *record_ );

	class BaseMovementAction *TryCheckAndLerpActions( PredictedMovementAction *prevAction,
													  PredictedMovementAction *nextAction,
													  MovementActionRecord *record_ );

	class BaseMovementAction *LerpActionRecords( PredictedMovementAction *prevAction,
		                                         PredictedMovementAction *nextAction,
		                                         MovementActionRecord *record_ );

	bool CheckPredictedOrigin( PredictedMovementAction *prevAction, PredictedMovementAction *nextAction, float frac );
	bool CheckPredictedVelocity( PredictedMovementAction *prevAction, PredictedMovementAction *nextAction, float frac );
	bool CheckPredictedAngles( PredictedMovementAction *prevAction, PredictedMovementAction *nextAction, float frac );

	void SetDefaultBotInput();

	void Debug( const char *format, ... ) const;
	// We want to have a full control over movement code assertions, so use custom ones for this class
	inline void Assert( bool condition, const char *message = nullptr ) const;
	template <typename T>
	inline void Assert( T conditionLikeValue, const char *message = nullptr ) const {
		Assert( conditionLikeValue != 0, message );
	}

	inline float GetRunSpeed() const;
	inline float GetJumpSpeed() const;
	inline float GetDashSpeed() const;

	void CheatingAccelerate( float frac );

	inline void CheatingCorrectVelocity( const Vec3 &target ) {
		CheatingCorrectVelocity( target.Data() );
	}

	void CheatingCorrectVelocity( const vec3_t target );
	void CheatingCorrectVelocity( float velocity2DDirDotToTarget2DDir, const Vec3 &toTargetDir2D );

	void OnInterceptedPredictedEvent( int ev, int parm );
	void OnInterceptedPMoveTouchTriggers( pmove_t *pm, vec3_t const previousOrigin );

	class BaseMovementAction *GetActionAndRecordForCurrTime( MovementActionRecord *record_ );

	// Might be called for failed attempts too
	void ShowBuiltPlanPath( bool useActionsColor = false ) const;
};

#endif
