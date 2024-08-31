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

#define CULLING_BLEND( a, b, mask ) _mm_blendv_ps( b, a, mask )

#include "frontendsse.inc"

#include "local.h"
#include "frontend.h"
#include "frontendcull.inc"

namespace wsw::ref {

auto Frontend::collectVisibleWorldLeavesSse41( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	return collectVisibleWorldLeavesArch<Sse41>( stateForCamera );
}

auto Frontend::collectVisibleOccludersSse41( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	return collectVisibleOccludersArch<Sse41>( stateForCamera );
}

auto Frontend::buildFrustaOfOccludersSse41( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
	-> std::span<const Frustum> {
	return buildFrustaOfOccludersArch<Sse41>( stateForCamera, sortedOccluders );
}

auto Frontend::cullLeavesByOccludersSse41( StateForCamera *stateForCamera, std::span<const unsigned> indicesOfLeaves,
										   std::span<const Frustum> occluderFrusta )
	-> std::pair<std::span<const unsigned>, std::span<const unsigned>> {
	return cullLeavesByOccludersArch<Sse41>( stateForCamera, indicesOfLeaves, occluderFrusta );
}

void Frontend::cullSurfacesInVisLeavesByOccludersSse41( unsigned cameraIndex,
														std::span<const unsigned> indicesOfLeaves,
														std::span<const Frustum> occluderFrusta,
														MergedSurfSpan *mergedSurfSpans,
														uint8_t *surfVisTable ) {
	return cullSurfacesInVisLeavesByOccludersArch<Sse41>( cameraIndex, indicesOfLeaves, occluderFrusta, mergedSurfSpans, surfVisTable );
}

auto Frontend::cullEntriesWithBoundsSse41( const void *entries, unsigned numEntries, unsigned boundsFieldOffset,
										   unsigned strideInBytes, const Frustum *__restrict primaryFrustum,
										   std::span<const Frustum> occluderFrusta, uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	return cullEntriesWithBoundsArch<Sse41>( entries, numEntries, boundsFieldOffset, strideInBytes,
											 primaryFrustum, occluderFrusta, tmpIndices );
}

auto Frontend::cullEntryPtrsWithBoundsSse41( const void **entryPtrs, unsigned numEntries, unsigned boundsFieldOffset,
											 const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
											 uint16_t *tmpIndices )
	-> std::span<const uint16_t> {
	return cullEntryPtrsWithBoundsArch<Sse41>( entryPtrs, numEntries, boundsFieldOffset,
											   primaryFrustum, occluderFrusta, tmpIndices );
}

}