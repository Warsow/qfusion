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

#include <atomic>

namespace wsw::ref {

static bool isLockingOccluders = false;
static vec3_t lockedViewOrigin;
static vec3_t lockedViewAxis;

#ifdef IMPLEMENT_collectVisisbleWorldLeaves

template <unsigned Arch>
auto Frontend::collectVisibleWorldLeavesArch( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	const auto *const pvs             = Mod_ClusterPVS( stateForCamera->viewCluster, rsh.worldModel );
	const unsigned numWorldLeaves     = rsh.worldBrushModel->numvisleafs;
	const auto leaves                 = rsh.worldBrushModel->visleafs;
	unsigned *const visibleLeaves     = stateForCamera->visibleLeavesBuffer->get();
	const Frustum *__restrict frustum = &stateForCamera->frustum;

	unsigned numVisibleLeaves = 0;
	for( unsigned leafNum = 0; leafNum < numWorldLeaves; ++leafNum ) {
		const mleaf_t *__restrict const leaf = leaves[leafNum];
		// TODO: Handle area bits as well
		// TODO: Can we just iterate over all leaves in the cluster
		if( pvs[leaf->cluster >> 3] & ( 1 << ( leaf->cluster & 7 ) ) ) {
			LOAD_BOX_COMPONENTS( leaf->mins, leaf->maxs )
			// TODO: Re-add partial visibility of leaves
			COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( frustum, const int nonZeroIfFullyOutside )
			visibleLeaves[numVisibleLeaves] = leafNum;
			numVisibleLeaves += ( nonZeroIfFullyOutside == 0 );
		}
	}

	return { visibleLeaves, visibleLeaves + numVisibleLeaves };
}

#endif

#ifdef IMPLEMENT_collectVisibleOccluders

template <unsigned Arch>
auto Frontend::collectVisibleOccludersArch( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	const Frustum *__restrict frustum     = &stateForCamera->frustum;
	const unsigned numWorldModelOccluders = rsh.worldBrushModel->numOccluders;
	unsigned *const visibleOccluders      = stateForCamera->visibleOccludersBuffer->get();
	unsigned numVisibleOccluders          = 0;

#ifdef DEBUG_OCCLUDERS
	// TODO: Don't do that for portal camerae
	if( Cvar_Integer( "lockOccluders" ) ) {
		if( !isLockingOccluders ) {
			VectorCopy( stateForCamera->viewOrigin, lockedViewOrigin );
			VectorCopy( stateForCamera->viewAxis, lockedViewAxis );
			isLockingOccluders = true;
		}
	}
#endif

	const OccluderBoundsEntry *const occluderBoundsEntries = rsh.worldBrushModel->occluderBoundsEntries;
	const OccluderDataEntry *const occluderDataEntries     = rsh.worldBrushModel->occluderDataEntries;

	for( unsigned occluderNum = 0; occluderNum < numWorldModelOccluders; ++occluderNum ) {
		visibleOccluders[numVisibleOccluders] = occluderNum;
#ifdef DEBUG_OCCLUDERS
		numVisibleOccluders++;
#else
		const OccluderBoundsEntry &__restrict occluderBounds = occluderBoundsEntries[occluderNum];
		LOAD_BOX_COMPONENTS( occluderBounds.mins, occluderBounds.maxs );
		COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( frustum, const int nonZeroIfFullyOutside );
		numVisibleOccluders += ( nonZeroIfFullyOutside == 0 );
#endif
	}

	const float *__restrict viewOrigin = stateForCamera->viewOrigin;
	const float *__restrict viewAxis   = stateForCamera->viewAxis;
	if( isLockingOccluders ) {
		viewOrigin = lockedViewOrigin;
		viewAxis   = lockedViewAxis;
	}

	// Prune bogus occluders
	unsigned numKeptOccluders = 0;
	for( unsigned i = 0; i < numVisibleOccluders; ++i ) {
		const unsigned occluderNum                   = visibleOccluders[i];
		const OccluderDataEntry &__restrict occluder = occluderDataEntries[occluderNum];

		const float absViewDot = std::abs( DotProduct( viewAxis, occluder.plane ) );
		if( absViewDot < 0.05f ) {
			continue;
		}

		if( std::fabs( DotProduct( viewOrigin, occluder.plane ) - occluder.plane[3] ) < 16.0f ) {
			continue;
		}

		vec3_t toOccluderVec;
		// Hacks, hacks, hacks: reject occluders behind the viewer
		// TODO: Add and use the nearest primary frustum plane?
		VectorSubtract( occluder.innerPolyPoint, viewOrigin, toOccluderVec );

		if( DotProduct( viewAxis, toOccluderVec ) <= 0 ) {
			continue;
		}

		VectorNormalizeFast( toOccluderVec );
		// Note that this condition is perfectly meaningful (bogus frusta often get built without it),
		// but it is poorely defined and is way too restrictive TODO: Clarify it, discover more permissive condition.
		if( std::fabs( DotProduct( toOccluderVec, occluder.plane ) ) < 0.1f ) {
			continue;
		}

		visibleOccluders[numKeptOccluders++] = occluderNum;
	}

	return { visibleOccluders, numKeptOccluders };
}

#endif

#ifdef IMPLEMENT_buildFrustaOfOccluders

template <unsigned Arch>
auto Frontend::buildFrustaOfOccludersArch( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
	-> std::span<const Frustum> {
	Frustum *const occluderFrusta     = stateForCamera->occluderFrusta;
	const unsigned maxOccluders       = wsw::min<unsigned>( sortedOccluders.size(), std::size( stateForCamera->occluderFrusta ) );
	constexpr float selfOcclusionBias = 1.0f;

	const float *__restrict viewOrigin                = stateForCamera->viewOrigin;
	[[maybe_unused]] const float *__restrict viewAxis = stateForCamera->viewAxis;

#ifdef DEBUG_OCCLUDERS
	if( isLockingOccluders ) {
		viewOrigin = lockedViewOrigin;
		viewAxis   = lockedViewAxis;
	}
#endif

	bool hadCulledFrusta = false;
	// Note: We don't process more occluders due to performance and not memory capacity reasons.
	// Best occluders come first so they should make their way into the final result.
	alignas( 16 )bool isCulledByOtherTable[kMaxOccluderFrusta];
	// MSVC fails to get the member array count in compile time
	assert( std::size( isCulledByOtherTable ) == std::size( stateForCamera->occluderFrusta ) );
	std::memset( isCulledByOtherTable, 0, sizeof( bool ) * maxOccluders );

	// Note: An outer loop over all surfaces would have been allowed to avoid redundant component shuffles
	// but this approach requires building all frusta prior to that, which is more expensive.

	const OccluderBoundsEntry *const occluderBoundsEntries = rsh.worldBrushModel->occluderBoundsEntries;
	const OccluderDataEntry *const occluderDataEntries     = rsh.worldBrushModel->occluderDataEntries;

	for( unsigned occluderIndex = 0; occluderIndex < maxOccluders; ++occluderIndex ) {
		if( isCulledByOtherTable[occluderIndex] ) {
			continue;
		}

		const OccluderDataEntry *const __restrict occluder = occluderDataEntries + sortedOccluders[occluderIndex].occluderNum;

		Frustum *const __restrict f = &occluderFrusta[occluderIndex];

		for( unsigned vertIndex = 0; vertIndex < occluder->numVertices; ++vertIndex ) {
			const float *const v1 = occluder->data[vertIndex + 0];
			const float *const v2 = occluder->data[( vertIndex + 1 != occluder->numVertices ) ? vertIndex + 1 : 0];

			cplane_t plane;
			// TODO: Cache?
			PlaneFromPoints( v1, v2, viewOrigin, &plane );

			// Make the normal point inside the frustum
			if( DotProduct( plane.normal, occluder->innerPolyPoint ) - plane.dist < 0 ) {
				VectorNegate( plane.normal, plane.normal );
				plane.dist = -plane.dist;
			}

			f->setPlaneComponentsAtIndex( vertIndex, plane.normal, plane.dist );
		}

		vec4_t cappingPlane;
		Vector4Copy( occluder->plane, cappingPlane );
		// Don't let the surface occlude itself
		if( DotProduct( cappingPlane, viewOrigin ) - cappingPlane[3] > 0 ) {
			Vector4Negate( cappingPlane, cappingPlane );
			cappingPlane[3] += selfOcclusionBias;
		} else {
			cappingPlane[3] -= selfOcclusionBias;
		}

		f->setPlaneComponentsAtIndex( occluder->numVertices, cappingPlane, cappingPlane[3] );
		f->fillComponentTails( occluder->numVertices );

#ifndef DEBUG_OCCLUDERS
		// We have built the frustum.
		// Cull all other frusta by it.
		// Note that the "culled-by" relation is not symmetrical so we have to check from the beginning.

		for( unsigned otherOccluderIndex = 0; otherOccluderIndex < maxOccluders; ++otherOccluderIndex ) {
			if( otherOccluderIndex != occluderIndex ) [[likely]] {
				if( !isCulledByOtherTable[otherOccluderIndex] ) {
					const unsigned otherOccluderNum       = sortedOccluders[otherOccluderIndex].occluderNum;
					const OccluderBoundsEntry &thatBounds = occluderBoundsEntries[otherOccluderNum];
					LOAD_BOX_COMPONENTS( thatBounds.mins, thatBounds.maxs );
					COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( f, const int zeroIfFullyInside );
					if( zeroIfFullyInside == 0 ) {
						isCulledByOtherTable[otherOccluderIndex] = true;
						hadCulledFrusta = true;
					}
				}
			}
		}
#endif
	}

	unsigned numSelectedOccluders = maxOccluders;
	if( hadCulledFrusta ) {
		unsigned numPreservedOccluders = 0;
		for( unsigned occluderNum = 0; occluderNum < maxOccluders; ++occluderNum ) {
			if( !isCulledByOtherTable[occluderNum] ) {
				// TODO: This is a memcpy() call, make the compactification more efficient or use a manual SIMD copy
				occluderFrusta[numPreservedOccluders++] = occluderFrusta[occluderNum];
			}
		}
		numSelectedOccluders = numPreservedOccluders;
	}

#ifdef SHOW_OCCLUDERS_FRUSTA
	vec3_t pointInFrontOfView;
	VectorMA( viewOrigin, 8.0, &m_state.viewAxis[0], pointInFrontOfView );

	// Show preserved frusta
	for( unsigned occluderNum = 0; occluderNum < maxOccluders; ++occluderNum ) {
		if( isCulledByOtherTable[occluderNum] ) {
			continue;
		}

		const OccluderDataEntry *const occluderData = occluderDataEntries + sortedOccluders[occluderNum].occluderNum;

		#ifdef DEBUG_OCCLUDERS
		if( Cvar_Integer( "pinnedOccluderNum" ) != (int)( 1 + occluderNum ) ) {
			continue;
		}


		const float absViewDot      = std::abs( DotProduct( viewAxis, occluderData->plane ) );
		const float distanceToPlane = DotProduct( viewOrigin, occluderData->plane ) - occluderData->plane[3];

		vec3_t toOccluderVec;
		// Hacks, hacks, hacks TODO: Add and use the nearest primary frustum plane?
		VectorSubtract( occluderData->innerPolyPoint, viewOrigin, toOccluderVec );

		const float viewAxisDotToOccluder = DotProduct( viewAxis, toOccluderVec );

		VectorNormalizeFast( toOccluderVec );
		const float planeDotToOccluder = DotProduct( toOccluderVec, occluderData->plane );

		Com_Printf( "Abs view dot=%f distanceToPlane=%f viewAxisDotToOccluder=%f planeDotToOccluder=%f\n",
					absViewDot, distanceToPlane, viewAxisDotToOccluder, planeDotToOccluder );
#endif

		//addDebugLine( surface->occluderPolyMins, surface->occluderPolyMaxs, COLOR_RGB( 0, 128, 255 ) );

		for( unsigned vertIndex = 0; vertIndex < occluderData->numVertices; ++vertIndex ) {
			const float *const v1 = occluderData->data[vertIndex + 0];
			const float *const v2 = occluderData->data[( vertIndex + 1 != occluderData->numVertices ) ? vertIndex + 1 : 0];

			addDebugLine( v1, pointInFrontOfView );
			addDebugLine( v1, v2, COLOR_RGB( 255, 0, 255 ) );

			cplane_t plane;
			// TODO: Inline?
			PlaneFromPoints( v1, v2, viewOrigin, &plane );

			// Make the normal point inside the frustum
			if( DotProduct( plane.normal, occluderData->innerPolyPoint ) - plane.dist < 0 ) {
				VectorNegate( plane.normal, plane.normal );
				plane.dist = -plane.dist;
			}

			vec3_t midpointOfEdge, normalPoint;
			VectorAvg( v1, v2, midpointOfEdge );
			VectorMA( midpointOfEdge, 8.0f, plane.normal, normalPoint );
			addDebugLine( midpointOfEdge, normalPoint );
		}

		vec4_t cappingPlane;
		Vector4Copy( occluderData->plane, cappingPlane );
		// Don't let the surface occlude itself
		if( DotProduct( cappingPlane, viewOrigin ) - cappingPlane[3] > 0 ) {
			Vector4Negate( cappingPlane, cappingPlane );
			cappingPlane[3] += selfOcclusionBias;
		} else {
			cappingPlane[3] -= selfOcclusionBias;
		}

		vec3_t cappingPlanePoint;
		VectorMA( occluderData->innerPolyPoint, 32.0f, cappingPlane, cappingPlanePoint );
		addDebugLine( occluderData->innerPolyPoint, cappingPlanePoint );
	}
#endif

	return { occluderFrusta, occluderFrusta + numSelectedOccluders };
}

#endif

#ifdef IMPLEMENT_cullLeavesByOccluders

template <unsigned Arch>
auto Frontend::cullLeavesByOccludersArch( StateForCamera *stateForCamera,
										  std::span<const unsigned> indicesOfLeaves,
										  std::span<const Frustum> occluderFrusta )
	-> std::pair<std::span<const unsigned>, std::span<const unsigned>> {
#ifdef DEBUG_OCCLUDERS
	const int pinnedOccluderNum = Cvar_Integer( "pinnedOccluderNum" );
#endif

	const unsigned numLeaves               = rsh.worldBrushModel->numvisleafs;
	unsigned *const partiallyVisibleLeaves = stateForCamera->occluderPassPartiallyVisibleLeavesBuffer->get();
	unsigned *const fullyVisibleLeaves     = stateForCamera->occluderPassFullyVisibleLeavesBuffer->get();

	const unsigned numOccluders        = occluderFrusta.size();
	const auto leaves                  = rsh.worldBrushModel->visleafs;
	unsigned numPartiallyVisibleLeaves = 0;
	unsigned numFullyVisibleLeaves     = 0;

	for( const unsigned leafIndex: indicesOfLeaves ) {
		const mleaf_t *const leaf = leaves[leafIndex];
		int wasPartiallyInside   = 0;
		int wasFullyInside       = 0;
		unsigned frustumNum      = 0;

		LOAD_BOX_COMPONENTS( leaf->mins, leaf->maxs );

		do {
			const Frustum *const __restrict f = &occluderFrusta[frustumNum];

#ifdef DEBUG_OCCLUDERS
			if( pinnedOccluderNum && pinnedOccluderNum != (int)( 1 + frustumNum ) ) {
				continue;
			}
#endif

			COMPUTE_TRISTATE_RESULT_FOR_8_PLANES( f, const int nonZeroIfFullyOutside, const int nonZeroIfPartiallyOutside )

			if( !( nonZeroIfFullyOutside | nonZeroIfPartiallyOutside ) ) {
				wasFullyInside = true;
				SHOW_OCCLUDED( leaf->mins, leaf->maxs, COLOR_RGB( 255, 0, 255 ) );
				break;
			}

			wasPartiallyInside |= nonZeroIfPartiallyOutside && ( nonZeroIfFullyOutside == 0 );
		} while( ++frustumNum != numOccluders );

		if( !wasFullyInside ) [[likely]] {
			if( wasPartiallyInside ) [[unlikely]] {
				partiallyVisibleLeaves[numPartiallyVisibleLeaves++] = leafIndex;
			} else {
				fullyVisibleLeaves[numFullyVisibleLeaves++] = leafIndex;
			}
		}
	}

	return { { fullyVisibleLeaves, numFullyVisibleLeaves }, { partiallyVisibleLeaves, numPartiallyVisibleLeaves } };
}

#endif

#ifdef IMPLEMENT_cullSurfacesInVisLeavesByOccluders

template <unsigned Arch>
void Frontend::cullSurfacesInVisLeavesByOccludersArch( unsigned cameraIndex,
													   std::span<const unsigned> indicesOfVisibleLeaves,
													   std::span<const Frustum> occluderFrusta,
													   int *surfMinMaxSpans, uint8_t *surfVisTable ) {
	assert( !occluderFrusta.empty() );

#ifdef DEBUG_OCCLUDERS
	const int pinnedOccluderNum = Cvar_Integer( "pinnedOccluderNum" );
#endif

	const msurface_t *const surfaces    = rsh.worldBrushModel->surfaces;
	const auto leaves                   = rsh.worldBrushModel->visleafs;
	const uint8_t occlusionCullingFrame = m_occlusionCullingFrame;
	const unsigned numOccluders         = occluderFrusta.size();
	for( const unsigned leafNum: indicesOfVisibleLeaves ) {
		const mleaf_s *const leaf             = leaves[leafNum];
		const unsigned *const leafSurfaceNums = leaf->visSurfaces;
		const unsigned numLeafSurfaces        = leaf->numVisSurfaces;

		for( unsigned surfIndex = 0; surfIndex < numLeafSurfaces; ++surfIndex ) {
			const unsigned surfNum = leafSurfaceNums[surfIndex];
			const msurface_t *const __restrict surf = surfaces + surfNum;
			// Note: This is correct, assuming byte reads are not torn.
			// Given this assumption, in the worst case we get duplicated checks
			// which store the same result in the vis table.
			if( surf->occlusionCullingFrames[cameraIndex] != occlusionCullingFrame ) {
				surf->occlusionCullingFrames[cameraIndex] = occlusionCullingFrame;

				LOAD_BOX_COMPONENTS( surf->mins, surf->maxs );

				bool surfVisible = true;
				unsigned frustumNum = 0;
				do {
					const Frustum *__restrict f = &occluderFrusta[frustumNum];

#ifdef DEBUG_OCCLUDERS
					if( pinnedOccluderNum && pinnedOccluderNum != (int)( 1 + frustumNum ) ) {
						continue;
					}
#endif

					COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( f, const int zeroIfFullyInside )

					if( zeroIfFullyInside == 0 ) [[unlikely]] {
						surfVisible = false;
						SHOW_OCCLUDED( surf->mins, surf->maxs, COLOR_RGB( 192, 0, 0 ) );
						break;
					}
				} while( ++frustumNum != numOccluders );

				if( surfVisible ) {
					surfVisTable[surfNum] = 1;
					assert( surf->mergedSurfNum > 0 );
					const unsigned mergedSurfNum = surf->mergedSurfNum - 1;
					surfMinMaxSpans[2 * mergedSurfNum + 0] = wsw::min( surfMinMaxSpans[2 * mergedSurfNum + 0], (int)surfNum );
					surfMinMaxSpans[2 * mergedSurfNum + 1] = wsw::max( surfMinMaxSpans[2 * mergedSurfNum + 1], (int)surfNum );
				}
			}
		}
	}
}

#endif

#ifdef IMPLEMENT_cullEntriesWithBounds

template <unsigned Arch>
auto Frontend::cullEntriesWithBoundsArch( const void *entries, unsigned numEntries, unsigned boundsFieldOffset, unsigned strideInBytes,
										  const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
										  uint16_t *tmpIndices ) -> std::span<const uint16_t> {
	if( !numEntries ) [[unlikely]] {
		return { tmpIndices, 0 };
	}

	const Frustum *const __restrict frustaPtr = occluderFrusta.data();

	unsigned entryNum         = 0;
	unsigned growingOffset    = boundsFieldOffset;
	unsigned numPassedEntries = 0;

	if( !occluderFrusta.empty() ) [[likely]] {
		do {
			const auto *__restrict bounds = (const vec4_t *)( (const uint8_t *)entries + growingOffset );
			LOAD_BOX_COMPONENTS( bounds[0], bounds[1] );
			COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( primaryFrustum, const int nonZeroIfFullyOutside );
			if( nonZeroIfFullyOutside == 0 ) {
				bool occluded = false;
				unsigned frustumNum = 0;
				do {
					COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( frustaPtr + frustumNum, const int zeroIfFullyInside );
					if( zeroIfFullyInside == 0 ) {
						SHOW_OCCLUDED( bounds[0], bounds[1], COLOR_RGB( 255, 192, 255 ) );
						occluded = true;
						break;
					}
				} while( ++frustumNum < occluderFrusta.size() );

				tmpIndices[numPassedEntries] = (uint16_t)entryNum;
				numPassedEntries += !occluded;
			}

			++entryNum;
			growingOffset += strideInBytes;
		} while ( entryNum < numEntries );
	} else {
		do {
			const auto *__restrict bounds = (const vec4_t *)( (const uint8_t *)entries + growingOffset );
			LOAD_BOX_COMPONENTS( bounds[0], bounds[1] );
			COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( primaryFrustum, const int nonZeroIfFullyOutside );

			tmpIndices[numPassedEntries] = (uint16_t)entryNum;
			numPassedEntries += ( nonZeroIfFullyOutside == 0 );

			++entryNum;
			growingOffset += strideInBytes;
		} while( entryNum < numEntries );
	}

	return { tmpIndices, numPassedEntries };
}

#endif

#ifdef IMPLEMENT_cullEntryPtrsWithBounds

template <unsigned Arch>
auto Frontend::cullEntryPtrsWithBoundsArch( const void **entryPtrs, unsigned numEntries, unsigned boundsFieldOffset,
											const Frustum *__restrict primaryFrustum, std::span<const Frustum> occluderFrusta,
											uint16_t *tmpIndices ) -> std::span<const uint16_t> {
	if( !numEntries ) [[unlikely]] {
		return { tmpIndices, 0 };
	}

	const Frustum *const __restrict frustaPtr = occluderFrusta.data();

	unsigned entryNum         = 0;
	unsigned numPassedEntries = 0;
	if( !occluderFrusta.empty() ) [[likely]] {
		do {
			const auto *__restrict bounds = (const vec4_t *)( (const uint8_t *)entryPtrs[entryNum] + boundsFieldOffset );
			LOAD_BOX_COMPONENTS( bounds[0], bounds[1] );
			COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( primaryFrustum, const int nonZeroIfFullyOutside );
			if ( nonZeroIfFullyOutside == 0 ) {
				bool occluded = false;
				unsigned frustumNum = 0;
				do {
					COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( frustaPtr + frustumNum, const int zeroIfFullyInside );
					if( zeroIfFullyInside == 0 ) {
						SHOW_OCCLUDED( bounds[0], bounds[1], COLOR_RGB( 255, 192, 255 ) );
						occluded = true;
						break;
					}
				} while( ++frustumNum < occluderFrusta.size() );

				tmpIndices[numPassedEntries] = (uint16_t)entryNum;
				numPassedEntries += !occluded;
			}

			++entryNum;
		} while( entryNum < numEntries );
	} else {
		do {
			const auto *__restrict bounds = (const vec4_t *)( (const uint8_t *)entryPtrs[entryNum] + boundsFieldOffset );
			LOAD_BOX_COMPONENTS( bounds[0], bounds[1] );
			COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( primaryFrustum, const int nonZeroIfFullyOutside );

			tmpIndices[numPassedEntries] = (uint16_t)entryNum;
			numPassedEntries += ( nonZeroIfFullyOutside == 0 );

			++entryNum;
		} while( entryNum < numEntries );
	}

	return { tmpIndices, numPassedEntries };
}

#endif

}