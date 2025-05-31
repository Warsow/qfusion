#ifndef WSW_b9de0b2c_5940_4e26_a413_88a2966e3c49_H
#define WSW_b9de0b2c_5940_4e26_a413_88a2966e3c49_H

#include "ailocal.h"

class AiTrajectoryPredictor {
public:
	struct Results {
		trace_t *trace;
		vec3_t origin;
		unsigned millisAhead;
		int enterAreaNum;
		int enterAreaFlags;
		int enterAreaContents;
		int leaveAreaNum;
		int leaveAreaContents;
		int leaveAreaFlags;
		int lastAreaNum;

		Results() {
			Clear();
		}

		void Clear() {
			memset( this, 0, sizeof( *this ) );
		}
	};

protected:
	trace_t localTrace;
	Vec3 prevOrigin { 0, 0, 0 };

	void ( *traceFunc )( trace_t *, const vec3_t start, const vec3_t mins, const vec3_t maxs, const vec3_t end, const edict_t *, int ) { nullptr };
	edict_t *ignore { nullptr };
	int contentMask { 0 };

	const AiAasWorld *aasWorld { nullptr };

	vec3_t mins, maxs;
	unsigned stepMillis { 128 };
	unsigned numSteps { 8 };

	int stopEventFlags { 0 };

	int enterAreaNum { 0 };
	int enterAreaContents { 0 };
	int enterAreaFlags { 0 };

	int leaveAreaNum { 0 };
	int leaveAreaContents { 0 };
	int leaveAreaFlags { 0 };

	int ignoreEntNum { 0 };
	bool extrapolateLastStep { false };

	int RunStep( const Vec3 &startOrigin, const Vec3 &startVelocity, Results *results );
	int InspectAasWorldTrace( Results *results );

	// A template that allows code generation for specific flags statically omitting checks for non-specified flags
	template <int Flags>
	int InspectAasWorldTraceForFlags( std::span<const int> tracedAreaNums, Results *results );

	template<typename BooleanLike>
	void SetStopEventsBit( BooleanLike condition, int bit ) {
		// Check whether its a power of two
		assert( !( bit & ( bit - 1 ) ) );
		if( condition ) {
			stopEventFlags |= bit;
		} else {
			stopEventFlags &= ~bit;
		}
	}
public:
	AiTrajectoryPredictor() {
		memset( &localTrace, 0, sizeof( localTrace ) );
		VectorCopy( vec3_origin, mins );
		VectorCopy( vec3_origin, maxs );
	}

	inline void SetColliderBounds( const vec_t *mins_, const vec_t *maxs_ ) {
		VectorCopy( mins_, this->mins );
		VectorCopy( maxs_, this->maxs );
	}

	inline void SetStepMillis( unsigned stepMillis_ ) { this->stepMillis = stepMillis_; }
	inline void SetNumSteps( unsigned numSteps_ ) { this->numSteps = numSteps_; }

	inline void SetEnterAreaNum( int areaNum_ ) {
		this->enterAreaNum = areaNum_;
		SetStopEventsBit( areaNum_, ENTER_AREA_NUM );
	}

	inline void SetLeaveAreaNum( int areaNum_ ) {
		this->leaveAreaNum = areaNum_;
		SetStopEventsBit( areaNum_, LEAVE_AREA_NUM );
	};

	inline void SetEnterAreaProps( int flags_, int contents_ ) {
		this->enterAreaFlags = flags_;
		this->enterAreaContents = contents_;
		SetStopEventsBit( flags_, ENTER_AREA_FLAGS );
		SetStopEventsBit( contents_, ENTER_AREA_CONTENTS );
	}

	inline void SetLeaveAreaProps( int flags_, int contents_ ) {
		this->leaveAreaFlags = flags_;
		this->leaveAreaContents = contents_;
		SetStopEventsBit( flags_, LEAVE_AREA_FLAGS );
		SetStopEventsBit( contents_, LEAVE_AREA_CONTENTS );
	}

	inline void SetEntitiesCollisionProps( bool collideEntities, int ignoreEntNum_ ) {
		assert( ignoreEntNum_ >= 0 );
		this->ignoreEntNum = ignoreEntNum_;
		SetStopEventsBit( collideEntities, HIT_ENTITY );
	}

	inline void SetExtrapolateLastStep( bool value ) { this->extrapolateLastStep = value; }

	enum StopEvent {
		DONE                 = 1 << 0,
		INTERRUPTED          = 1 << 1,
		HIT_SOLID            = 1 << 2,
		HIT_ENTITY           = 1 << 3,
		HIT_LIQUID           = 1 << 4,
		LEAVE_LIQUID         = 1 << 5,
		ENTER_AREA_NUM       = 1 << 6,
		LEAVE_AREA_NUM       = 1 << 7,
		ENTER_AREA_FLAGS     = 1 << 8,
		LEAVE_AREA_FLAGS     = 1 << 9,
		ENTER_AREA_CONTENTS  = 1 << 10,
		LEAVE_AREA_CONTENTS  = 1 << 11
	};

	inline void AddStopEventFlags( int flags ) { this->stopEventFlags |= (StopEvent)flags; }
	inline StopEvent StopEventFlags() const { return (StopEvent)this->stopEventFlags; }

	StopEvent Run( const Vec3 &startVelocity, const Vec3 &startOrigin, Results *results );

	StopEvent Run( const vec3_t startVelocity, const vec3_t startOrigin, Results *results ) {
		return Run( Vec3( startVelocity ), Vec3( startOrigin ), results );
	}

	// Allows doing some additional actions on each step.
	// Interrupts execution by returning false.
	virtual bool OnPredictionStep( const Vec3 &segmentStart, const Results *results ) { return true; }
};

#endif
