/*
Copyright (C) 2024 Chasseur de bots

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

#include "local.h"
#include "frontend.h"

#define LOAD_BOX_COMPONENTS( mins, maxs ) \
	const __m256 ymmMinsX = _mm256_broadcast_ss( &( mins )[0] ); \
	const __m256 ymmMinsY = _mm256_broadcast_ss( &( mins )[1] ); \
	const __m256 ymmMinsZ = _mm256_broadcast_ss( &( mins )[2] ); \
	const __m256 ymmMaxsX = _mm256_broadcast_ss( &( maxs )[0] ); \
	const __m256 ymmMaxsY = _mm256_broadcast_ss( &( maxs )[1] ); \
	const __m256 ymmMaxsZ = _mm256_broadcast_ss( &( maxs )[2] );

#define LOAD_COMPONENTS_OF_8_FRUSTUM_PLANES( f ) \
	const __m256 ymmXBlends = _mm256_loadu_ps( (const float *)( f )->xBlendMasks ); \
	const __m256 ymmYBlends = _mm256_loadu_ps( (const float *)( f )->yBlendMasks ); \
	const __m256 ymmZBlends = _mm256_loadu_ps( (const float *)( f )->zBlendMasks ); \
	const __m256 ymmPlaneX  = _mm256_loadu_ps( ( f )->planeX ); \
	const __m256 ymmPlaneY  = _mm256_loadu_ps( ( f )->planeY ); \
	const __m256 ymmPlaneZ  = _mm256_loadu_ps( ( f )->planeZ ); \
	const __m256 ymmPlaneD  = _mm256_loadu_ps( ( f )->planeD );

/* Note that CULLING_BLEND macro arguments for SSE 4.1 counterpart get reordered for actual application */
#define SELECT_NEAREST_BOX_CORNER_COMPONENTS( resultsSuffix ) \
	const __m256 ymmSelectedX##resultsSuffix = _mm256_blendv_ps( ymmMaxsX, ymmMinsX, ymmXBlends ); \
	const __m256 ymmSelectedY##resultsSuffix = _mm256_blendv_ps( ymmMaxsY, ymmMinsY, ymmYBlends ); \
	const __m256 ymmSelectedZ##resultsSuffix = _mm256_blendv_ps( ymmMaxsZ, ymmMinsZ, ymmZBlends );

#define SELECT_FARTHEST_BOX_CORNER_COMPONENTS( resultsSuffix ) \
	const __m256 ymmSelectedX##resultsSuffix = _mm256_blendv_ps( ymmMinsX, ymmMaxsX, ymmXBlends ); \
	const __m256 ymmSelectedY##resultsSuffix = _mm256_blendv_ps( ymmMinsY, ymmMaxsY, ymmYBlends ); \
	const __m256 ymmSelectedZ##resultsSuffix = _mm256_blendv_ps( ymmMinsZ, ymmMaxsZ, ymmZBlends );

#define COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( resultsSuffix ) \
	const __m256 ymmMulX##resultsSuffix = _mm256_mul_ps( ymmSelectedX##resultsSuffix, ymmPlaneX ); \
	const __m256 ymmMulY##resultsSuffix = _mm256_mul_ps( ymmSelectedY##resultsSuffix, ymmPlaneY ); \
	const __m256 ymmMulZ##resultsSuffix = _mm256_mul_ps( ymmSelectedZ##resultsSuffix, ymmPlaneZ ); \
	const __m256 ymmXMulX_plus_yMulY##resultsSuffix = \
		_mm256_add_ps( ymmMulX##resultsSuffix, ymmMulY##resultsSuffix ); \
	const __m256 ymmZMulZ_minus_ymmPlaneD##resultsSuffix = \
		_mm256_sub_ps( ymmMulZ##resultsSuffix, ymmPlaneD ); \
	const __m256 ymmDist##resultsSuffix = \
		_mm256_add_ps( ymmXMulX_plus_yMulY##resultsSuffix,  ymmZMulZ_minus_ymmPlaneD##resultsSuffix );

#define COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( f, zeroIfFullyInside ) \
	LOAD_COMPONENTS_OF_8_FRUSTUM_PLANES( f ) \
	SELECT_FARTHEST_BOX_CORNER_COMPONENTS() \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE() \
	/* Note: We assume that signed zeros are negative, this is fine for culling purposes */ \
	zeroIfFullyInside = _mm256_movemask_ps( ymmDist );

#define COMPUTE_TRISTATE_RESULT_FOR_8_PLANES( f, nonZeroIfOutside, nonZeroIfPartiallyOutside ) \
	LOAD_COMPONENTS_OF_8_FRUSTUM_PLANES( f ) \
	SELECT_NEAREST_BOX_CORNER_COMPONENTS( _nearest ) \
	SELECT_FARTHEST_BOX_CORNER_COMPONENTS( _farthest ) \
	/* Compute distances between planes and nearest box corners to check whether the box is fully outside */ \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _nearest ) \
	/* Compute distances between planes and farthest box corners to check whether the box is partially outside */ \
	COMPUTE_BOX_CORNER_DISTANCE_TO_PLANE( _farthest ) \
	/* Note: We assume that signed zeros are negative, this is fine for culling purposes */ \
	nonZeroIfOutside          = _mm256_movemask_ps( ymmDist_nearest ); \
	nonZeroIfPartiallyOutside = _mm256_movemask_ps( ymmDist_farthest );

#define IMPLEMENT_cullLeavesByOccluders
#define IMPLEMENT_cullSurfacesInVisLeavesByOccluders

#include "frontendcull.inc"

namespace wsw::ref {

void Frontend::cullSurfacesInVisLeavesByOccludersAvx( unsigned cameraIndex,
													  std::span<const unsigned> indicesOfVisibleLeaves,
													  std::span<const Frustum> occluderFrusta,
													  int *surfMinMaxSpans,
													  uint8_t *surfVisTable ) {
	_mm256_zeroupper();
	cullSurfacesInVisLeavesByOccludersArch<Avx>( cameraIndex, indicesOfVisibleLeaves, occluderFrusta, surfMinMaxSpans, surfVisTable );
	_mm256_zeroupper();
}

auto Frontend::cullLeavesByOccludersAvx( StateForCamera *stateForCamera, std::span<const unsigned> indicesOfLeaves,
										 std::span<const Frustum> occluderFrusta )
	-> std::pair<std::span<const unsigned>, std::span<const unsigned>> {
	_mm256_zeroupper();
	auto result = cullLeavesByOccludersArch<Avx>( stateForCamera, indicesOfLeaves, occluderFrusta );
	_mm256_zeroupper();
	return result;
}

}