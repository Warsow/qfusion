#ifndef QFUSION_CM_TRACE_H
#define QFUSION_CM_TRACE_H

#include <common/common.h>

struct trace_s;
struct cmodel_state_s;
struct cbrush_s;
struct cface_s;
struct cmodel_s;

// 1/32 epsilon to keep floating point happy
#define DIST_EPSILON    ( 1.0f / 32.0f )
#define RADIUS_EPSILON      1.0f

struct CMTraceContext {
	trace_t *trace;

	vec3_t start, end;
	vec3_t mins, maxs;
	vec3_t startmins, endmins;
	vec3_t startmaxs, endmaxs;
	vec3_t absmins, absmaxs;
	vec3_t extents;
	vec3_t traceDir;
	float boxRadius;

#ifdef CM_USE_SSE
	__m128 xmmAbsmins, xmmAbsmaxs;
	// TODO: Add also xmm copies of trace dir/start once line distance test is vectorized
	__m128 xmmClipBoxLookup[16];
#endif

	int contents;
	bool ispoint;      // optimized case
};

struct CMShapeList;

// TODO: We don't need Ops, just set method pointers in the cms instance
struct Ops {
	struct cmodel_state_s *cms;

	Ops(): cms( nullptr ) {}

	virtual void SetupCollideContext( CMTraceContext *__restrict tlc, trace_t *__restrict tr, const float *start,
									  const float *end, const float *mins, const float *maxs, int brushmask );

	void SetupClipContext( CMTraceContext * ) {}

	virtual void CollideBox( CMTraceContext *tlc, void ( Ops::*method )( CMTraceContext *, const cbrush_s * ),
							 const cbrush_s *brushes, int numbrushes, const cface_s *markfaces, int nummarkfaces );


	virtual void ClipBoxToLeaf( CMTraceContext *tlc, const cbrush_s *brushes,
								int numbrushes, const cface_s *markfaces, int nummarkfaces );

	// Lets avoid making these calls virtual, there is a small but definite performance penalty
	// (something around 5-10%s, and this really matter as all newly introduced engine features rely on fast CM raycasting).
	// They still can be "overridden" for specialized implementations
	// just by using explicitly qualified method with the same signature.
	void TestBoxInBrush( CMTraceContext *tlc, const cbrush_s *brush );
	void ClipBoxToBrush( CMTraceContext *tlc, const cbrush_s *brush );

	void RecursiveHullCheck( CMTraceContext *tlc, int num, float p1f, float p2f, const vec3_t p1, const vec3_t p2 );

	void Trace( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins,
				const vec3_t maxs, const cmodel_s *cmodel, int brushmask, int topNodeHint );

	virtual void BuildShapeList( CMShapeList *list, const float *mins, const float *maxs, int clipMask );
	virtual void ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs );

	virtual void ClipToShapeList( const CMShapeList *list, trace_t *tr,
		                          const float *start, const float *end,
		                          const float *mins, const float *maxs, int clipMask );
};

struct GenericOps final: public Ops {};

struct Sse42Ops: public Ops {
	// Don't even bother about making prototypes if there is no attempt to compile SSE code
	// (this should aid calls devirtualization)
#ifdef CM_USE_SSE
	void SetupCollideContext( CMTraceContext *__restrict tlc, trace_t *__restrict tr, const float *start,
							  const float *end, const float *mins, const float *maxs, int brushmask ) override;

	void ClipBoxToLeaf( CMTraceContext *tlc, const cbrush_s *brushes, int numbrushes,
						const cface_s *markfaces, int nummarkfaces ) override;

	// Overrides a base member by hiding it
	void ClipBoxToBrush( CMTraceContext *tlc, const cbrush_s *brush );

	void BuildShapeList( CMShapeList *list, const float *mins, const float *maxs, int clipMask ) override;
	void ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs ) override;

	void ClipToShapeList( const CMShapeList *list, trace_t *tr,
		                  const float *start, const float *end,
		                  const float *mins, const float *maxs, int clipMask ) override;
#endif
};

struct AvxOps final: public Sse42Ops {
	//void ClipToAvxFriendlyShapes( CMTraceContext *__restrict tlc, const cbrush_t *__restrict shapes, int numShapes );
	void ClipToAvxFriendlyShape( CMTraceContext *__restrict tlc, const cbrush_t *__restrict shape );

	void ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs ) override;

	void ClipToShapeList( const CMShapeList *list, trace_t *tr,
						  const float *start, const float *end,
						  const float *mins, const float *maxs, int clipMask ) override;
};

inline bool doBoundsTest( const float *shapeMins, const float *shapeMaxs, const CMTraceContext *tlc ) {
	return BoundsIntersect( shapeMins, shapeMaxs, tlc->absmins, tlc->absmaxs );
}

inline bool doBoundsAndLineDistTest( const float *__restrict shapeMins,
									 const float *__restrict shapeMaxs,
									 const float *__restrict shapeCenter,
									 float shapeRadius,
									 const CMTraceContext *__restrict tlc ) {
	if( !doBoundsTest( shapeMins, shapeMaxs, tlc ) ) {
		return false;
	}

	// TODO: Vectorize this part. This task is not completed for various reasons.

	vec3_t centerToStart;
	vec3_t proj, perp;

	VectorSubtract( tlc->start, shapeCenter, centerToStart );
	float projMagnitude = DotProduct( centerToStart, tlc->traceDir );
	VectorScale( tlc->traceDir, projMagnitude, proj );
	VectorSubtract( centerToStart, proj, perp );
	float distanceThreshold = shapeRadius + tlc->boxRadius;
	return VectorLengthSquared( perp ) <= distanceThreshold * distanceThreshold;
}

#ifdef CM_USE_SSE

inline bool boundsIntersectSse42( __m128 traceAbsmins, __m128 traceAbsmaxs, __m128 shapeMins, __m128 shapeMaxs ) {
	__m128 cmp1 = _mm_cmpge_ps( shapeMins, traceAbsmaxs );
	__m128 cmp2 = _mm_cmpge_ps( traceAbsmins, shapeMaxs );
	return !_mm_movemask_ps( _mm_or_ps( cmp1, cmp2 ) );
}

inline bool boundsIntersectSse42( __m128 traceAbsmins, __m128 traceAbsmaxs,
								  const float *shapeMins, const float *shapeMaxs ) {
	// This version relies on fast unaligned loads, that's why it requires SSE4.
	__m128 xmmShapeMins = _mm_loadu_ps( shapeMins );
	__m128 xmmShapeMaxs = _mm_loadu_ps( shapeMaxs );

	return boundsIntersectSse42( traceAbsmins, traceAbsmaxs, xmmShapeMins, xmmShapeMaxs );
}

inline bool doBoundsTestSse42( const float *__restrict shapeMins,
							   const float *__restrict shapeMaxs,
							   const CMTraceContext *__restrict tlc ) {
	return boundsIntersectSse42( tlc->xmmAbsmins, tlc->xmmAbsmaxs, shapeMins, shapeMaxs );
}

inline bool doBoundsAndLineDistTestSse42( const float *__restrict shapeMins,
										  const float *__restrict shapeMaxs,
										  const float *__restrict shapeCenter,
										  float shapeRadius,
										  const CMTraceContext *__restrict tlc ) {
	if( !doBoundsTestSse42( shapeMins, shapeMaxs, tlc ) ) {
		return false;
	}

	vec3_t centerToStart;
	vec3_t proj, perp;

	VectorSubtract( tlc->start, shapeCenter, centerToStart );
	const float projMagnitude = DotProduct( centerToStart, tlc->traceDir );
	VectorScale( tlc->traceDir, projMagnitude, proj );
	VectorSubtract( centerToStart, proj, perp );
	const float distanceThreshold = shapeRadius + tlc->boxRadius;
	return VectorLengthSquared( perp ) <= distanceThreshold * distanceThreshold;
}

// TODO: Discover why there's no observable penalty on an Intel CPU, contrary to what it should be

// SSE4.2 code gets compiled with /arch:AVX TODO is it really needed
#if defined( CM_USE_AVX ) || defined( _MSC_VER )
//#define wsw_vex_fence() _mm256_zeroupper()
#define wsw_vex_fence() do {} while (0)
#else
#define wsw_vex_fence() do {} while (0)
#endif

#ifndef _MSC_VER
#define _wsw_ctz( x ) __builtin_ctz( x )
#else
#include <intrin.h>
__forceinline int _wsw_ctz( int x ) {
	assert( x );
	unsigned long result = 0;
	_BitScanForward( &result, x );
	return result;
}
#endif

/**
 * Create this object in scopes that match boundaries
 * of transition between regular SSE2 and VEX-encoded binary code.
 * This fence inserts instructions that help to avoid transition penalties.
 */
struct VexScopedFence {
	VexScopedFence() {
		wsw_vex_fence();
	}
	~VexScopedFence() {
		wsw_vex_fence();
	}
};

#endif

#endif //QFUSION_CM_TRACE_H
