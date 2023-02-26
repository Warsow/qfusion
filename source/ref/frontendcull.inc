#pragma once

#include <algorithm>

namespace wsw::ref {

static bool isLockingOccluders = false;
static vec3_t lockedViewOrigin;
static vec3_t lockedViewAxis;

template <unsigned Arch>
auto Frontend::collectVisibleWorldLeavesArch() -> std::span<const unsigned> {
	const auto *const pvs             = Mod_ClusterPVS( m_stateForActiveCamera->viewCluster, rsh.worldModel );
	const unsigned numWorldLeaves     = rsh.worldBrushModel->numvisleafs;
	const auto leaves                 = rsh.worldBrushModel->visleafs;
	unsigned *const visibleLeaves     = m_visibleLeavesBuffer.data.get();
	const Frustum *__restrict frustum = &m_stateForActiveCamera->frustum;

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

template <unsigned Arch>
auto Frontend::collectVisibleOccludersArch() -> std::span<const SortedOccluder> {
	const Frustum *__restrict frustum = &m_stateForActiveCamera->frustum;
	unsigned *const visibleOccluders  = m_visibleOccludersBuffer.data.get();
	unsigned numVisibleOccluders      = 0;

#ifdef DEBUG_OCCLUDERS
	if( Cvar_Integer( "lockOccluders" ) ) {
		if( !isLockingOccluders ) {
			VectorCopy( m_state.viewOrigin, lockedViewOrigin );
			VectorCopy( m_state.viewAxis, lockedViewAxis );
			isLockingOccluders = true;
		}
	}
#endif

	const OccluderBoundsEntry *const occluderBoundsEntries = rsh.worldBrushModel->occluderBoundsEntries;
	const OccluderDataEntry *const occluderDataEntries     = rsh.worldBrushModel->occluderDataEntries;
	const unsigned numWorldModelOccluders                  = rsh.worldBrushModel->numOccluders;

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

	const float *__restrict viewOrigin = m_stateForActiveCamera->viewOrigin;
	const float *__restrict viewAxis   = m_stateForActiveCamera->viewAxis;
	if( isLockingOccluders ) {
		viewOrigin = lockedViewOrigin;
		viewAxis   = lockedViewAxis;
	}

	SortedOccluder *const sortedOccluders = m_sortedOccludersBuffer.data.get();
	unsigned numSortedOccluders           = 0;

	for( unsigned i = 0; i < numVisibleOccluders; ++i ) {
		const unsigned occluderNum                   = visibleOccluders[i];
		const OccluderDataEntry &__restrict occluder = occluderDataEntries[occluderNum];

		const float absViewDot = std::abs( DotProduct( viewAxis, occluder.plane ) );
		if( absViewDot < 0.1f ) {
			continue;
		}

		if( std::fabs(DotProduct( viewOrigin, occluder.plane ) - occluder.plane[3] ) < 16.0f ) {
			continue;
		}

		vec3_t toOccluderVec;
		// Hacks, hacks, hacks TODO: Add and use the nearest primary frustum plane?
		VectorSubtract( occluder.innerPolyPoint, viewOrigin, toOccluderVec );

		if( DotProduct( viewAxis, toOccluderVec ) <= 0 ) {
			continue;
		}

		VectorNormalizeFast( toOccluderVec );
		if( std::fabs( DotProduct( toOccluderVec, occluder.plane ) ) < 0.3f ) {
			continue;
		}

		// TODO: Try using a distance to the poly?
		const float score = Q_RSqrt( DistanceSquared( viewOrigin, occluder.innerPolyPoint ) ) * absViewDot;
		sortedOccluders[numSortedOccluders++] = { occluderNum, score };
	}

	// TODO: Don't sort, build a heap instead?
	std::sort( sortedOccluders, sortedOccluders + numSortedOccluders );

#ifdef SHOW_OCCLUDERS
	for( unsigned i = 0; i < numSortedOccluders; ++i ) {
		const OccluderDataEntry &__restrict occluder = occluderDataEntries[sortedOccluders[i].occluderNum];

		for( unsigned vertIndex = 0; vertIndex < occluder.numVertices; ++vertIndex ) {
			const float *const v1 = occluder.data[vertIndex + 0];
			const float *const v2 = occluder.data[( vertIndex + 1 != occluder.numVertices ) ? vertIndex + 1 : 0];
			addDebugLine( v1, v2, COLOR_RGB( 192, 192, 96 ) );
		}
	}
#endif

	return { sortedOccluders, sortedOccluders + numSortedOccluders };
}

template <unsigned Arch>
auto Frontend::buildFrustaOfOccludersArch( std::span<const SortedOccluder> sortedOccluders ) -> std::span<const Frustum> {
	Frustum *const occluderFrusta     = m_occluderFrusta;
	const unsigned maxOccluders       = wsw::min<unsigned>( sortedOccluders.size(), std::size( m_occluderFrusta ) );
	constexpr float selfOcclusionBias = 1.0f;

	const float *__restrict viewOrigin                = m_stateForActiveCamera->viewOrigin;
	[[maybe_unused]] const float *__restrict viewAxis = m_stateForActiveCamera->viewAxis;

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
	assert( std::size( isCulledByOtherTable ) == std::size( m_occluderFrusta ) );
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

template <unsigned Arch>
auto Frontend::cullLeavesByOccludersArch( std::span<const unsigned> indicesOfLeaves,
										  std::span<const Frustum> occluderFrusta )
	-> std::pair<std::span<const unsigned>, std::span<const unsigned>> {
#ifdef DEBUG_OCCLUDERS
	const int pinnedOccluderNum = Cvar_Integer( "pinnedOccluderNum" );
#endif

	unsigned *const partiallyVisibleLeaves = m_occluderPassPartiallyVisibleLeavesBuffer.data.get();
	unsigned *const fullyVisibleLeaves     = m_occluderPassPartiallyVisibleLeavesBuffer.data.get();
	const unsigned numOccluders = occluderFrusta.size();
	const auto leaves           = rsh.worldBrushModel->visleafs;
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

			COMPUTE_TRISTATE_RESULT_FOR_8_PLANES( f, const int nonZeroIfOutside, const int nonZeroIfPartiallyOutside )

			if( !( nonZeroIfOutside | nonZeroIfPartiallyOutside ) ) {
				wasFullyInside = true;
				SHOW_OCCLUDED( leaf->mins, leaf->maxs, COLOR_RGB( 255, 0, 255 ) );
				break;
			}

			wasPartiallyInside |= nonZeroIfPartiallyOutside;
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

template <unsigned Arch>
void Frontend::cullSurfacesInVisLeavesByOccludersArch( std::span<const unsigned> indicesOfVisibleLeaves,
													   std::span<const Frustum> occluderFrusta,
													   MergedSurfSpan *mergedSurfSpans ) {
	assert( !occluderFrusta.empty() );

#ifdef DEBUG_OCCLUDERS
	const int pinnedOccluderNum = Cvar_Integer( "pinnedOccluderNum" );
#endif

	const msurface_t *const surfaces = rsh.worldBrushModel->surfaces;
	const auto leaves = rsh.worldBrushModel->visleafs;
	const unsigned occlusionCullingFrame = m_occlusionCullingFrame;

	// Cull individual surfaces by up to 16 best frusta
	const unsigned numBestOccluders = wsw::min<unsigned>( 16, occluderFrusta.size() );
	for( const unsigned leafNum: indicesOfVisibleLeaves ) {
		const mleaf_s *const leaf             = leaves[leafNum];
		const unsigned *const leafSurfaceNums = leaf->visSurfaces;
		const unsigned numLeafSurfaces        = leaf->numVisSurfaces;

		for( unsigned surfIndex = 0; surfIndex < numLeafSurfaces; ++surfIndex ) {
			const unsigned surfNum = leafSurfaceNums[surfIndex];
			const msurface_t *const __restrict surf = surfaces + surfNum;
			if( surf->occlusionCullingFrame != occlusionCullingFrame ) {
				surf->occlusionCullingFrame = occlusionCullingFrame;

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

					if( !zeroIfFullyInside ) [[unlikely]] {
						surfVisible = false;
						SHOW_OCCLUDED( surf->mins, surf->maxs, COLOR_RGB( 192, 0, 0 ) );
						break;
					}
				} while( ++frustumNum != numBestOccluders );

				if( surfVisible ) {
					assert( surf->drawSurf > 0 );
					const unsigned mergedSurfNum = surf->drawSurf - 1;
					MergedSurfSpan *const __restrict span = &mergedSurfSpans[mergedSurfNum];
					// TODO: Branchless min/max
					span->firstSurface = wsw::min( span->firstSurface, (int)surfNum );
					span->lastSurface = wsw::max( span->lastSurface, (int)surfNum );
				}
			}
		}
	}
}

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

}