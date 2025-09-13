/*
Copyright (C) 2007 Victor Luchits
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

// An SSE2 baseline implementation, as explained in
// https://fgiesen.wordpress.com/2016/04/03/sse-mind-the-gap/
#define SSE_BLEND_CORNER_COMPONENTS( maxs, mins, mask ) _mm_or_ps( _mm_and_ps( mins, mask ), _mm_andnot_ps( mask, maxs ) )

#define IMPLEMENT_collectVisisbleWorldLeaves
#define IMPLEMENT_collectVisibleOccluders
#define IMPLEMENT_buildFrustaOfOccluders
#define IMPLEMENT_cullSurfacesByOccluders
#define IMPLEMENT_cullEntriesWithBounds
#define IMPLEMENT_cullEntryPtrsWithBounds

#include "frontendsse.inc"

#include "local.h"
#include "frontend.h"
#include "frontendcull.inc"

namespace wsw {

auto RendererFrontend::collectVisibleWorldLeavesSse2( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	return collectVisibleWorldLeavesArch<Sse2>( stateForCamera );
}

auto RendererFrontend::collectVisibleOccludersSse2( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	return collectVisibleOccludersArch<Sse2>( stateForCamera );
}

auto RendererFrontend::buildFrustaOfOccludersSse2( StateForCamera *stateForCamera,
												   std::span<const SortedOccluder> sortedOccluders )
	-> std::span<const Frustum> {
	return buildFrustaOfOccludersArch<Sse2>( stateForCamera, sortedOccluders );
}

void RendererFrontend::cullSurfacesByOccludersSse2( StateForCamera *stateForCamera,
													std::span<const unsigned> indicesOfSurfaces,
													std::span<const Frustum> occluderFrusta,
													MergedSurfSpan *mergedSurfSpans,
													uint8_t *surfVisTable ) {
	return cullSurfacesByOccludersArch<Sse2>( stateForCamera, indicesOfSurfaces, occluderFrusta, mergedSurfSpans, surfVisTable );
}

auto RendererFrontend::cullEntriesWithBoundsSse2( StateForCamera *stateForCamera, const void *entries,
												  unsigned numEntries, unsigned boundsFieldOffset,
												  unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
												  std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	return cullEntriesWithBoundsArch<Sse2>( stateForCamera, entries, numEntries, boundsFieldOffset, strideInBytes,
											primaryFrustum, occluderFrusta, tmpIndices );
}

auto RendererFrontend::cullEntryPtrsWithBoundsSse2( StateForCamera *stateForCamera, const void **entryPtrs,
													unsigned numEntries, unsigned boundsFieldOffset,
													const Frustum *__restrict primaryFrustum,
													std::span<const Frustum> occluderFrusta,
													uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	return cullEntryPtrsWithBoundsArch<Sse2>( stateForCamera, entryPtrs, numEntries, boundsFieldOffset,
											  primaryFrustum, occluderFrusta, tmpIndices );
}

}