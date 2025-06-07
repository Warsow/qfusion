#include <common/common.h>
#include "cm_local.h"
#include "cm_trace.h"

#define CM_USE_SSE
#define CM_USE_AVX

void AvxOps::ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs ) {
	[[maybe_unused]] volatile VexScopedFence fence;
	const cbrush_t *tmp[64];

	const int numSrcShapes = baseList->numShapes;
	// Prefer using a local buffer as accessing the scratchpad could involve a cache miss
	const cbrush_t **otherShapes = numSrcShapes > 64 ? (const cbrush_t **)list->scratchpad : tmp;
	const cbrush_t **avxFriendlyShapes = list->shapes;
	const cbrush_t **const srcShapes = baseList->shapes;

	int numAvxFriendlyShapes = 0;
	int numOtherShapes = 0;

	const __m128 testedMins = _mm_setr_ps( mins[0], mins[1], mins[2], 0 );
	const __m128 testedMaxs = _mm_setr_ps( maxs[0], maxs[1], maxs[2], 1 );

	BoundsBuilder builder;
	for( int i = 0; i < numSrcShapes; ++i ) {
		const cbrush_t *__restrict b = srcShapes[i];
		const __m128 shapeMins = _mm_loadu_ps( b->mins );
		const __m128 shapeMaxs = _mm_loadu_ps( b->maxs );

		const __m128 cmp1 = _mm_cmpge_ps( shapeMins, testedMaxs );
		const __m128 cmp2 = _mm_cmpge_ps( testedMins, shapeMaxs );
		const __m128 mask = _mm_or_ps( cmp1, cmp2 );

		builder.addMinsAndMaxsIfNoCmpMaskDWordSet( mask, shapeMins, shapeMaxs );

		// Advance / consider it added if no mask bit set
		const bool shouldAdd = ( _mm_movemask_ps( mask ) == 0 );
		const bool isAvxFriendly = ( b->numSseGroups == 2 );

		assert( ( isAvxFriendly && ( b->numsides >= 6 && b->numsides <= 8 ) ) || b->numsides >= 6 );

		avxFriendlyShapes[numAvxFriendlyShapes] = b;
		numAvxFriendlyShapes += ( shouldAdd & isAvxFriendly );

		otherShapes[numOtherShapes] = b;
		numOtherShapes += ( shouldAdd & !isAvxFriendly );
	}

	list->numOtherShapes = numOtherShapes;
	if( numOtherShapes ) {
		// Put other shapes in the tail of the whole one so it behaves as expected for all other subroutines
		memcpy( list->shapes + numAvxFriendlyShapes, otherShapes, numOtherShapes * sizeof( cbrush_t ** ) );
	}

	const int totalNumShapes = numAvxFriendlyShapes + numOtherShapes;
	list->numAvxFriendlyShapes = numAvxFriendlyShapes;
	list->numOtherShapes = numOtherShapes;
	list->numShapes = totalNumShapes;
	list->hasBounds = totalNumShapes > 0;
	if( list->hasBounds ) {
		builder.storeTo( list->mins, list->maxs );
	}

	list->possibleContents = baseList->possibleContents;
}

// Copied from https://stackoverflow.com/questions/60250527 to get the stuff working
// TODO: Check alternatives
// TODO: Inline/reorder stuff

inline __m256 horizontalAllMax( __m256 a ) {
	// permute 128-bit values to compare floats from different halves.
	const __m256 permHalves = _mm256_permute2f128_ps( a, a, 1 );
	//compares 4 values with 4 other values ("old half against the new half")
	const __m256 m0 = _mm256_max_ps( permHalves, a );

	//now we need to find the largest of 4 values in the half:
	const __m256 perm0 = _mm256_permute_ps( m0, 0b01001110 );
	const __m256 m1 = _mm256_max_ps( m0, perm0);

	const __m256 perm1 = _mm256_permute_ps( m1, 0b10110001 );
	const __m256 m2 = _mm256_max_ps( perm1, m1 );
	return m2;
}

inline __m256 horizontalAllMin( __m256 a ) {
	// permute 128-bit values to compare floats from different halves.
	const __m256 permHalves = _mm256_permute2f128_ps( a, a, 1 );
	const __m256 m0 = _mm256_min_ps( permHalves, a );

	//now we need to find the largest of 4 values in the half:
	const __m256 perm0 = _mm256_permute_ps( m0, 0b01001110 );
	const __m256 m1 = _mm256_min_ps( m0, perm0 );

	const __m256 perm1 = _mm256_permute_ps( m1, 0b10110001 );
	const __m256 m2 = _mm256_min_ps( perm1, m1 );
	return m2;
}

void AvxOps::ClipToAvxFriendlyShape( CMTraceContext *__restrict tlc, const cbrush_t *__restrict shape ) {
	assert( shape->numSseGroups == 2 );
	assert( shape->numsides >= 6 && shape->numsides <= 8 );
	assert( shape->simd );

	const uint8_t *buffer = shape->simd;

	// TODO: Ensure alignment

	// TODO: This stuff must be lifted above the planned loop over all avx-friendly shapes
	__m256 ymmStartMinsX = _mm256_broadcast_ss( &tlc->startmins[0] );
	__m256 ymmStartMinsY = _mm256_broadcast_ss( &tlc->startmins[1] );
	__m256 ymmStartMinsZ = _mm256_broadcast_ss( &tlc->startmins[2] );

	__m256 ymmStartMaxsX = _mm256_broadcast_ss( &tlc->startmaxs[0] );
	__m256 ymmStartMaxsY = _mm256_broadcast_ss( &tlc->startmaxs[1] );
	__m256 ymmStartMaxsZ = _mm256_broadcast_ss( &tlc->startmaxs[2] );

	__m256 ymmEndMinsX = _mm256_broadcast_ss( &tlc->endmins[0] );
	__m256 ymmEndMinsY = _mm256_broadcast_ss( &tlc->endmins[1] );
	__m256 ymmEndMinsZ = _mm256_broadcast_ss( &tlc->endmins[2] );

	__m256 ymmEndMaxsX = _mm256_broadcast_ss( &tlc->endmaxs[0] );
	__m256 ymmEndMaxsY = _mm256_broadcast_ss( &tlc->endmaxs[1] );
	__m256 ymmEndMaxsZ = _mm256_broadcast_ss( &tlc->endmaxs[2] );

	const __m256 ymmZero = _mm256_setzero_ps();
	const __m128 xmmFFFs = _mm_castsi128_ps( _mm_set1_epi32( ~0 ) );
	const __m256 ymmFFFs = _mm256_insertf128_ps( _mm256_castps128_ps256( xmmFFFs ), xmmFFFs, 1 );
	const __m256 ymmEps = _mm256_set1_ps( DIST_EPSILON );

	int getout = 0;
	int startout = 0;

	constexpr int strideInElems = 8;

	// http://cbloomrants.blogspot.com/2010/07/07-21-10-x86.html
	// "... on x86 you should write code to take use of complex addressing,
	// you can have fewer data dependencies if you just set up one base variable
	// and then do lots of referencing off it.
	const auto *const __restrict p = (float *)( buffer );

	// TODO: Use aligned loads once the proper data alignment is guaranteed
	const __m256 ymmX = _mm256_loadu_ps( p );
	const __m256 ymmY = _mm256_loadu_ps( p + strideInElems );
	const __m256 ymmZ = _mm256_loadu_ps( p + 2 * strideInElems );
	const __m256 ymmD = _mm256_loadu_ps( p + 6 * strideInElems );

	const __m256 ymmBlendsX = _mm256_loadu_ps( p + 3 * strideInElems );
	const __m256 ymmBlendsY = _mm256_loadu_ps( p + 4 * strideInElems );
	const __m256 ymmBlendsZ = _mm256_loadu_ps( p + 5 * strideInElems );

	const __m256 ymmStartBoundsX = _mm256_blendv_ps( ymmStartMinsX, ymmStartMaxsX, ymmBlendsX );
	const __m256 ymmStartBoundsY = _mm256_blendv_ps( ymmStartMinsY, ymmStartMaxsY, ymmBlendsY );
	const __m256 ymmStartBoundsZ = _mm256_blendv_ps( ymmStartMinsZ, ymmStartMaxsZ, ymmBlendsZ );

	const __m256 ymmStartXMuls = _mm256_mul_ps( ymmStartBoundsX, ymmX );
	const __m256 ymmStartYMuls = _mm256_mul_ps( ymmStartBoundsY, ymmY );
	const __m256 ymmStartZMuls = _mm256_mul_ps( ymmStartBoundsZ, ymmZ );

	const __m256 ymmStartDots = _mm256_add_ps( _mm256_add_ps( ymmStartXMuls, ymmStartYMuls ), ymmStartZMuls );

	const __m256 ymmEndBoundsX = _mm256_blendv_ps( ymmEndMinsX, ymmEndMaxsX, ymmBlendsX );
	const __m256 ymmEndBoundsY = _mm256_blendv_ps( ymmEndMinsY, ymmEndMaxsY, ymmBlendsY );
	const __m256 ymmEndBoundsZ = _mm256_blendv_ps( ymmEndMinsZ, ymmEndMaxsZ, ymmBlendsZ );

	const __m256 ymmEndXMuls = _mm256_mul_ps( ymmEndBoundsX, ymmX );
	const __m256 ymmEndYMuls = _mm256_mul_ps( ymmEndBoundsY, ymmY );
	const __m256 ymmEndZMuls = _mm256_mul_ps( ymmEndBoundsZ, ymmZ );

	const __m256 ymmEndDots = _mm256_add_ps( _mm256_add_ps( ymmEndXMuls, ymmEndYMuls ), ymmEndZMuls );

	const __m256 ymmDist1 = _mm256_sub_ps( ymmStartDots, ymmD );
	const __m256 ymmDist2 = _mm256_sub_ps( ymmEndDots, ymmD );

	const __m256 ymmDist1Gt0 = _mm256_cmp_ps( ymmDist1, ymmZero, _CMP_GT_OQ );

	// "if completely in front of face, no intersection"
	// Check whether d1 > 0 && d2 >= d1 holds for any vector component
	if( _mm256_movemask_ps( _mm256_and_ps( ymmDist1Gt0, _mm256_cmp_ps( ymmDist2, ymmDist1, _CMP_GE_OQ ) ) ) ) {
		return;
	}

	const __m256 ymmDist2Gt0 = _mm256_cmp_ps( ymmDist2, ymmZero, _CMP_GT_OQ );
	const __m256 ymmDist1Le0 = _mm256_xor_ps( ymmDist1Gt0, ymmFFFs );
	const __m256 ymmDist2Le0 = _mm256_xor_ps( ymmDist2Gt0, ymmFFFs );

	// "if( d2 > 0 ) getout = true"
	// Check whether d2 > 0 holds for any vector component
	getout |= _mm256_movemask_ps( ymmDist2Gt0 );
	// "if( d1 > 0 ) startout = true"
	// Check whether d1 > 0 holds for any vector component
	startout |= _mm256_movemask_ps( ymmDist1Gt0 );

	float enterfrac = -1.0f;
	float leavefrac = +1.0f;
	__m256 ymmIndexMask;

	// "if( d1 <= 0 && d2 <= 0 ) continue"
	const __m256 ymmSkipMask = _mm256_and_ps( ymmDist1Le0, ymmDist2Le0 );

	// Check for early exit
	if( _mm256_movemask_ps( ymmSkipMask ) != 0xFF ) {
		// We could use _mm_andnot_ps with the skip mask but its slower (wtf?)
		const __m256 ymmTestMask = _mm256_xor_ps( ymmSkipMask, ymmFFFs );
		const __m256 ymmF = _mm256_sub_ps( ymmDist1, ymmDist2 );

		// TODO: Is it that safe? Use a precise division maybe?
		const __m256 ymmInvDelta = _mm256_rcp_ps( ymmF );
		const __m256 ymmEnterFracs = _mm256_mul_ps( _mm256_sub_ps( ymmDist1, ymmEps ), ymmInvDelta );
		const __m256 ymmFNegMask = _mm256_and_ps( ymmTestMask, _mm256_cmp_ps( ymmF, ymmZero, _CMP_LT_OQ ) );
		const __m256 ymmLeaveFracs = _mm256_mul_ps( _mm256_add_ps( ymmDist1, ymmEps ), ymmInvDelta );
		const __m256 ymmFPosMask = _mm256_and_ps( ymmTestMask, _mm256_cmp_ps( ymmF, ymmZero, _CMP_GT_OQ ) );

		__m256 ymmCurrEnterFracs = _mm256_broadcast_ss( &enterfrac );
		__m256 ymmCurrLeaveFracs = _mm256_broadcast_ss( &leavefrac );

		const __m256 ymmEnterFracsToCmp = _mm256_blendv_ps( ymmCurrEnterFracs, ymmEnterFracs, ymmFPosMask );
		const __m256 ymmMaxEnterFracs = _mm256_max_ps( ymmCurrEnterFracs, ymmEnterFracsToCmp );
		const __m256 ymmLeaveFracsToCmp = _mm256_blendv_ps( ymmCurrLeaveFracs, ymmLeaveFracs, ymmFNegMask );
		const __m256 ymmMinLeaveFracs = _mm256_min_ps( ymmCurrLeaveFracs, ymmLeaveFracsToCmp );

		// Compute a horizontal max of the enter fracs vector and a horizontal min of the leave fracs vector

		// We have to find an index of the new max enter frac (if any) to store the lead side num
		const __m256 ymmAllMaxEnterFracs = horizontalAllMax( ymmMaxEnterFracs );
		const __m256 ymmMatchBestIndexCmp = _mm256_cmp_ps( ymmMaxEnterFracs, ymmAllMaxEnterFracs, _CMP_EQ_OQ );
		const __m256 ymmMatchLessCurrCmp = _mm256_cmp_ps( ymmMaxEnterFracs, ymmCurrEnterFracs, _CMP_GT_OQ );
		enterfrac = _mm256_cvtss_f32( ymmAllMaxEnterFracs );
		// There's no separate AVX just-a-single-min subroutine
		leavefrac = _mm256_cvtss_f32( horizontalAllMin( ymmMinLeaveFracs ) );
		ymmIndexMask = _mm256_and_ps( ymmMatchBestIndexCmp, ymmMatchLessCurrCmp );
	}

	if( !startout ) {
		// original point was inside brush
		tlc->trace->startsolid = true;
		tlc->trace->contents = shape->contents;
		if( !getout ) {
			tlc->trace->allsolid = true;
			tlc->trace->fraction = 0;
		}
		return;
	}

	if( enterfrac - ( 1.0f / 1024.0f ) <= leavefrac ) {
		if( enterfrac > -1 && enterfrac < tlc->trace->fraction ) {
			// TODO: Rewrite these magic offsets
			const int floatStrideInBytes = strideInElems * sizeof( float );
			const int intStrideInBytes = strideInElems * sizeof( float );
			const int shortStrideInBytes = strideInElems * sizeof( short );
			const auto *const sidenums = (int *) ( buffer + 7 * floatStrideInBytes );
			const auto *const shadernums = (int *)( buffer + 7 * floatStrideInBytes + intStrideInBytes );
			const auto *const surfflags = (int *)( buffer + 7 * floatStrideInBytes + 2 * intStrideInBytes );
			const auto *const sidetype = (short *)( buffer + 7 * floatStrideInBytes + 3 * intStrideInBytes );
			const auto *const sidebits = (short *)( buffer + 7 * floatStrideInBytes + 3 * intStrideInBytes + shortStrideInBytes );
			const auto *const sidex = (float *)( buffer );
			const auto *const sidey = (float *)( buffer + floatStrideInBytes );
			const auto *const sidez = (float *)( buffer + 2 * floatStrideInBytes );
			const auto *const sided = (float *)( buffer + 6 * floatStrideInBytes );
			tlc->trace->fraction = wsw::max( 0.0f, enterfrac );
			cplane_s *destPlane = &tlc->trace->plane;
			const int indexMask = _mm256_movemask_ps( ymmIndexMask );
			assert( indexMask );
			const int leadSideNum = sidenums[_wsw_ctz( indexMask )];
			assert( (unsigned)leadSideNum < (unsigned)shape->numsides );
			destPlane->normal[0] = sidex[leadSideNum];
			destPlane->normal[1] = sidey[leadSideNum];
			destPlane->normal[2] = sidez[leadSideNum];
			destPlane->dist = sided[leadSideNum];
			destPlane->signbits = sidebits[leadSideNum];
			destPlane->type = sidetype[leadSideNum];
			tlc->trace->surfFlags = surfflags[leadSideNum];
			tlc->trace->contents = shape->contents;
			tlc->trace->shaderNum = shadernums[leadSideNum];
		}
	}
}

void AvxOps::ClipToShapeList( const CMShapeList *__restrict list, trace_t *tr, const float *start,
							  const float *end, const float *mins, const float *maxs, int clipMask ) {
	[[maybe_unused]] volatile VexScopedFence fence;

	alignas( 16 ) CMTraceContext tlc;
	SetupCollideContext( &tlc, tr, start, end, mins, maxs, clipMask );

	if( list->hasBounds ) {
		if( !BoundsIntersect( list->mins, list->maxs, tlc.absmins, tlc.absmaxs ) ) {
			assert( tr->fraction == 1.0f );
			VectorCopy( end, tr->endpos );
			return;
		}
	}

	float *const fraction = &tlc.trace->fraction;
	const auto *__restrict shapes = list->shapes;
	const int numShapes = list->numShapes;

	const bool callClipToBrush = !VectorCompare( start, end );
	if( list->hasBounds && callClipToBrush ) {
		assert( list->numAvxFriendlyShapes + list->numOtherShapes == list->numShapes );

		SetupClipContext( &tlc );

		int i = 0;
		const int numAvxFriendlyShapes = list->numAvxFriendlyShapes;
		for(; i < numAvxFriendlyShapes; ++i ) {
			const cbrush_s *__restrict b = shapes[i];
			if( !boundsIntersectSse42( tlc.xmmAbsmins, tlc.xmmAbsmaxs, b->mins, b->maxs ) ) {
				continue;
			}
			ClipToAvxFriendlyShape( &tlc, b );
			if( !*fraction ) {
				i = std::numeric_limits<int>::max();
				break;
			}
		}

		// Make sure the virtual call address gets resolved here
		auto clipFn = &Sse42Ops::ClipBoxToBrush;
		for(; i < numShapes; ++i ) {
			const cbrush_t *__restrict b = shapes[i];
			if( !boundsIntersectSse42( tlc.xmmAbsmins, tlc.xmmAbsmaxs, b->mins, b->maxs ) ) {
				continue;
			}
			wsw_vex_fence();
			( this->*clipFn )( &tlc, b );
			wsw_vex_fence();
			if( !*fraction ) {
				break;
			}
		}
	} else {
		// Make sure the virtual call address gets resolved here
		const auto clipFn = callClipToBrush ? &Sse42Ops::ClipBoxToBrush : &Sse42Ops::TestBoxInBrush;
		for( int i = 0; i < numShapes; ++i ) {
			const cbrush_t *__restrict b = shapes[i];
			if( !boundsIntersectSse42( tlc.xmmAbsmins, tlc.xmmAbsmaxs, b->mins, b->maxs ) ) {
				continue;
			}
			wsw_vex_fence();
			( this->*clipFn )( &tlc, b );
			wsw_vex_fence();
			if( !*fraction ) {
				break;
			}
		}
	}

	if( tr->fraction == 1.0f ) {
		VectorCopy( end, tr->endpos );
	} else {
		VectorLerp( start, tr->fraction, end, tr->endpos );
	}
}