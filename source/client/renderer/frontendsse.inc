/*
Copyright (C) 2023 Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#define LOAD_BOX_COMPONENTS( boxMins, boxMaxs ) \
	const __m128 xmmMins  = _mm_loadu_ps( boxMins ); \
	const __m128 xmmMaxs  = _mm_loadu_ps( boxMaxs ); \
	const __m128 xmmMinsX = _mm_shuffle_ps( xmmMins, xmmMins, _MM_SHUFFLE( 0, 0, 0, 0 ) ); \
	const __m128 xmmMinsY = _mm_shuffle_ps( xmmMins, xmmMins, _MM_SHUFFLE( 1, 1, 1, 1 ) ); \
	const __m128 xmmMinsZ = _mm_shuffle_ps( xmmMins, xmmMins, _MM_SHUFFLE( 2, 2, 2, 2 ) ); \
	const __m128 xmmMaxsX = _mm_shuffle_ps( xmmMaxs, xmmMaxs, _MM_SHUFFLE( 0, 0, 0, 0 ) ); \
	const __m128 xmmMaxsY = _mm_shuffle_ps( xmmMaxs, xmmMaxs, _MM_SHUFFLE( 1, 1, 1, 1 ) ); \
	const __m128 xmmMaxsZ = _mm_shuffle_ps( xmmMaxs, xmmMaxs, _MM_SHUFFLE( 2, 2, 2, 2 ) ); \

#define LOAD_COMPONENTS_OF_4_FRUSTUM_PLANES( frustum ) \
	const __m128 xmmXBlends = _mm_load_ps( (const float *)( frustum )->xBlendMasks ); \
	const __m128 xmmYBlends = _mm_load_ps( (const float *)( frustum )->yBlendMasks ); \
	const __m128 xmmZBlends = _mm_load_ps( (const float *)( frustum )->zBlendMasks ); \
	const __m128 xmmPlaneX  = _mm_load_ps( ( frustum )->planeX ); \
	const __m128 xmmPlaneY  = _mm_load_ps( ( frustum )->planeY ); \
	const __m128 xmmPlaneZ  = _mm_load_ps( ( frustum )->planeZ ); \
	const __m128 xmmPlaneD  = _mm_load_ps( ( frustum )->planeD );

#define LOAD_COMPONENTS_OF_8_FRUSTUM_PLANES( frustum ) \
	const __m128 xmmXBlends_03 = _mm_load_ps( (const float *)( frustum )->xBlendMasks ); \
	const __m128 xmmYBlends_03 = _mm_load_ps( (const float *)( frustum )->yBlendMasks ); \
	const __m128 xmmZBlends_03 = _mm_load_ps( (const float *)( frustum )->zBlendMasks ); \
	const __m128 xmmXBlends_47 = _mm_load_ps( ( (const float *)( frustum )->xBlendMasks ) + 4 ); \
	const __m128 xmmYBlends_47 = _mm_load_ps( ( (const float *)( frustum )->yBlendMasks ) + 4 ); \
	const __m128 xmmZBlends_47 = _mm_load_ps( ( (const float *)( frustum )->zBlendMasks ) + 4 ); \
	const __m128 xmmPlaneX_03  = _mm_load_ps( ( frustum )->planeX ); \
	const __m128 xmmPlaneY_03  = _mm_load_ps( ( frustum )->planeY ); \
	const __m128 xmmPlaneZ_03  = _mm_load_ps( ( frustum )->planeZ ); \
	const __m128 xmmPlaneD_03  = _mm_load_ps( ( frustum )->planeD ); \
	const __m128 xmmPlaneX_47  = _mm_load_ps( ( frustum )->planeX + 4 ); \
	const __m128 xmmPlaneY_47  = _mm_load_ps( ( frustum )->planeY + 4 ); \
	const __m128 xmmPlaneZ_47  = _mm_load_ps( ( frustum )->planeZ + 4 ); \
	const __m128 xmmPlaneD_47  = _mm_load_ps( ( frustum )->planeD + 4 );

#define SELECT_NEAREST_BOX_CORNER_COMPONENTS( resultsSuffix, masksSuffix ) \
	const __m128 xmmSelectedX##resultsSuffix = SSE_BLEND_CORNER_COMPONENTS( xmmMaxsX, xmmMinsX, xmmXBlends##masksSuffix ); \
	const __m128 xmmSelectedY##resultsSuffix = SSE_BLEND_CORNER_COMPONENTS( xmmMaxsY, xmmMinsY, xmmYBlends##masksSuffix ); \
	const __m128 xmmSelectedZ##resultsSuffix = SSE_BLEND_CORNER_COMPONENTS( xmmMaxsZ, xmmMinsZ, xmmZBlends##masksSuffix );

#define SELECT_FARTHEST_BOX_CORNER_COMPONENTS( resultsSuffix, masksSuffix ) \
	const __m128 xmmSelectedX##resultsSuffix = SSE_BLEND_CORNER_COMPONENTS( xmmMinsX, xmmMaxsX, xmmXBlends##masksSuffix ); \
	const __m128 xmmSelectedY##resultsSuffix = SSE_BLEND_CORNER_COMPONENTS( xmmMinsY, xmmMaxsY, xmmYBlends##masksSuffix ); \
	const __m128 xmmSelectedZ##resultsSuffix = SSE_BLEND_CORNER_COMPONENTS( xmmMinsZ, xmmMaxsZ, xmmZBlends##masksSuffix );

#define COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( boxSuffix, planesSuffix ) \
	const __m128 xmmXMulX##boxSuffix            = _mm_mul_ps( xmmSelectedX##boxSuffix, xmmPlaneX##planesSuffix ); \
	const __m128 xmmYMulY##boxSuffix            = _mm_mul_ps( xmmSelectedY##boxSuffix, xmmPlaneY##planesSuffix ); \
	const __m128 xmmZMulZ##boxSuffix            = _mm_mul_ps( xmmSelectedZ##boxSuffix, xmmPlaneZ##planesSuffix ); \
	const __m128 xmmXMulX_plus_yMulY##boxSuffix = _mm_add_ps( xmmXMulX##boxSuffix, xmmYMulY##boxSuffix ); \
	const __m128 xmmZMulZ_minus_D##boxSuffix    = _mm_sub_ps( xmmZMulZ##boxSuffix, xmmPlaneD##planesSuffix ); \
	const __m128 xmmDist##boxSuffix             = _mm_add_ps( xmmXMulX_plus_yMulY##boxSuffix, xmmZMulZ_minus_D##boxSuffix );

// Assumes that LOAD_BOX_COMPONENTS was expanded in the scope
#define COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( f, nonZeroIfFullyOutside ) \
	LOAD_COMPONENTS_OF_4_FRUSTUM_PLANES( f ) \
	/* Select mins/maxs using masks for respective plane signbits */ \
	SELECT_NEAREST_BOX_CORNER_COMPONENTS( , ) \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( , ) \
	/* We do not need an exact match with masks of AVX version, just checking for non-zero bits is sufficient */ \
	/* Note: We assume that signed zeros are negative, this is fine for culling purposes */ \
	nonZeroIfFullyOutside = _mm_movemask_ps( xmmDist );

// Assumes that LOAD_BOX_COMPONENTS was expanded in the scope
#define COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( f, zeroIfFullyInside ) \
	LOAD_COMPONENTS_OF_8_FRUSTUM_PLANES( f ) \
	/* Select a farthest corner using masks for respective plane signbits */ \
	SELECT_FARTHEST_BOX_CORNER_COMPONENTS( _03_farthest, _03 ) \
	SELECT_FARTHEST_BOX_CORNER_COMPONENTS( _47_farthest, _47 ) \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _03_farthest, _03 ) \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _47_farthest, _47 ) \
	/* We do not need an exact match with masks of AVX version, just checking for non-zero bits is sufficient */ \
	/* Note: We assume that signed zeros are negative, this is fine for culling purposes */ \
	zeroIfFullyInside = _mm_movemask_ps( _mm_or_ps( xmmDist_03_farthest, xmmDist_47_farthest ) );

// Assumes that LOAD_BOX_COMPONENTS was expanded in the scope
#define COMPUTE_TRISTATE_RESULT_FOR_8_PLANES( f, nonZeroIfFullyOutside, nonZeroIfPartiallyOutside ) \
	LOAD_COMPONENTS_OF_8_FRUSTUM_PLANES( f ) \
	/* Select mins/maxs using masks for respective plane signbits, planes 0..3 */ \
	SELECT_NEAREST_BOX_CORNER_COMPONENTS( _03_nearest, _03 ) \
	/* Select mins/maxs using masks for respective plane signbits, planes 4..7 */ \
	SELECT_NEAREST_BOX_CORNER_COMPONENTS( _47_nearest, _47 ) \
	/* The same but for the farthest corner */ \
	SELECT_FARTHEST_BOX_CORNER_COMPONENTS( _03_farthest, _03 ) \
	SELECT_FARTHEST_BOX_CORNER_COMPONENTS( _47_farthest, _47 ) \
	/* Compute dot products of planes and nearest box corners to check whether the box is fully outside */ \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _03_nearest, _03 ) \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _47_nearest, _47 ) \
	/* Compute dot products of planes and farthest box corners to check whether the box is partially outside */ \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _03_farthest, _03 ) \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _47_farthest, _47 ) \
	/* We do not need an exact match with masks of AVX version, just checking for non-zero bits is sufficient */ \
	/* Note: We assume that signed zeros are negative, this is fine for culling purposes */ \
	nonZeroIfFullyOutside     = _mm_movemask_ps( _mm_or_ps( xmmDist_03_nearest, xmmDist_47_nearest ) ); \
	nonZeroIfPartiallyOutside = _mm_movemask_ps( _mm_or_ps( xmmDist_03_farthest, xmmDist_47_farthest ) );

