#ifndef WSW_60211f6f_d952_4428_8ac0_798bcf238459_H
#define WSW_60211f6f_d952_4428_8ac0_798bcf238459_H

#include "../ailocal.h"

class PredictionContext;

/**
 * Provides lazily-computed results of full-height or jumpable-height
 * short traces in 8 directions (front, left, back-left, front-right...)
 */
class EnvironmentTraceCache {
public:
	static constexpr float kTraceDepth = 32.0f;

	/**
	 * Represents a cached trace result
	 */
	struct TraceResult {
		vec3_t traceDir;
		trace_t trace;

		inline bool IsEmpty() const { return trace.fraction == 1.0f; }
	};
private:
	// Precache this reference as it is used on every prediction step
	const std::span<const aas_areasettings_s> aasAreaSettings;

	TraceResult results[16];
	const CMShapeList *cachedShapeList { nullptr };
	unsigned resultsMask { 0 };
	bool hasComputedShapeList { false };

	template <typename T>
	static inline void Assert( T condition, const char *message = nullptr ) {
#ifndef PUBLIC_BUILD
		if( !condition ) {
			if( message ) {
				AI_FailWith( "EnvironmentTraceCache::Assert()", "%s\n", message );
			} else {
				AI_FailWith( "EnvironmentTraceCache::Assert()", "An assertion has failed\n" );
			}
		}
#endif
	}

	/**
	 * Selects indices of non-blocked dirs among 8 full-height ones.
	 * @param context a current state of movement prediction context
	 * @param nonBlockedDirIndices a buffer for results
	 * @return a number of non-blocked dirs
	 */
	unsigned selectNonBlockedDirs( class PredictionContext *context, unsigned *nonBlockedDirIndices );
public:
	struct Query {
		unsigned mask;
		int index;

		static Query front() { return { 1u << 0, 0 }; }
		static Query back() { return { 1u << 1, 1 }; }
		static Query left() { return { 1u << 2, 2 }; }
		static Query right() { return { 1u << 3, 3 }; }
		static Query frontLeft() { return { 1u << 4, 4 }; }
		static Query frontRight() { return { 1u << 5, 5 }; }
		static Query backLeft() { return { 1u << 6, 6 }; }
		static Query backRight() { return { 1u << 7, 7 }; }

		Query &jumpableHeight() {
			// Check whether the current mask is for a full height
			if( mask <= ( 1u << 7 ) ) {
				mask <<= 8;
			}
			return *this;
		}
	};

	const TraceResult resultForQuery( const Query &query ) {
		Assert( query.mask & this->resultsMask, "A result is not present for the index" );
		return results[query.index];
	}

	EnvironmentTraceCache() : aasAreaSettings( AiAasWorld::instance()->getAreaSettings() ) {}

	void testForResultsMask( class PredictionContext *context, unsigned requiredResultsMask );

	void testForQuery( class PredictionContext *context, const Query &query ) {
		testForResultsMask( context, query.mask );
	}

	const CMShapeList *getShapeListForPMoveCollision( class PredictionContext *context );

	void makeRandomizedKeyMovesToTarget( PredictionContext *context, const Vec3 &intendedMoveDir, int *keyMoves );
	void makeKeyMovesToTarget( PredictionContext *context, const Vec3 &intendedMoveDir, int *keyMoves );
	void makeRandomKeyMoves( PredictionContext *context, int *keyMoves );
};

#endif
