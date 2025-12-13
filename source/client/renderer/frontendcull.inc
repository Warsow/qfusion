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

#ifdef IMPLEMENT_cullSurfacesByOccluders
#include <atomic>
#include <bit>
#endif

#ifdef DEBUG_OCCLUDERS
// Share these variables across arch-dependent code
// This also helps to avoid unused variable warnings
// in cases when these variables actually don't get unused for the specific arch
extern bool g_isLockingOccluders;
extern vec3_t g_lockedViewOrigin;
extern vec3_t g_lockedViewAxis;
extern int g_lockedOccluderNum;
#endif

namespace wsw {

#ifdef IMPLEMENT_buildBitMasksOfLeafOccluders

template <unsigned Arch>
void RendererFrontend::buildBitMasksOfLeafOccludersArch( StateForCamera *stateForCamera,
														 std::span<const unsigned> numsOfLeaves,
														 std::span<const Frustum> occluderFrusta,
														 uint64_t *bitMaskOfLeafOccluders ) {
	const auto leaves  = rsh.worldBrushModel->visleafs;

	for( const unsigned leafNum: numsOfLeaves ) {
		const mleaf_t *__restrict const leaf = leaves[leafNum];

		LOAD_BOX_COMPONENTS( leaf->mins, leaf->maxs );

		uint64_t mask         = 0;
		unsigned frustumIndex = 0;
		do {
			const Frustum *__restrict f = &occluderFrusta[frustumIndex];
			COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_8_PLANES( f, const int nonZeroIfFullyOutside );
			if( nonZeroIfFullyOutside == 0 ) {
				mask |= (uint64_t)1 << ( (uint64_t)frustumIndex );
			}
		} while( ++frustumIndex < occluderFrusta.size() );

		bitMaskOfLeafOccluders[leafNum] = mask;
	}
}

#endif

#ifdef IMPLEMENT_collectVisisbleWorldLeaves

template <unsigned Arch>
auto RendererFrontend::collectVisibleWorldLeavesArch( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
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
auto RendererFrontend::collectVisibleOccludersArch( StateForCamera *stateForCamera ) -> std::span<const unsigned> {
	const Frustum *__restrict frustum     = &stateForCamera->frustum;
	const unsigned numWorldModelOccluders = rsh.worldBrushModel->numOccluders;
	unsigned *const visibleOccluders      = stateForCamera->visibleOccludersBuffer->get();
	unsigned numVisibleOccluders          = 0;

	const OccluderBoundsEntry *const occluderBoundsEntries = rsh.worldBrushModel->occluderBoundsEntries;

#ifdef DEBUG_OCCLUDERS
	const int lockedOccluderNum = Cvar_Integer( "lockedOccluderNum" );
	if( (unsigned)( lockedOccluderNum - 1 ) < numWorldModelOccluders ) {
		if( !g_isLockingOccluders ) {
			VectorCopy( stateForCamera->viewOrigin, g_lockedViewOrigin );
			VectorCopy( stateForCamera->viewAxis, g_lockedViewAxis );
			g_lockedOccluderNum = lockedOccluderNum;
			g_isLockingOccluders = true;
		}
	} else {
		g_isLockingOccluders = false;
	}

	if( g_isLockingOccluders ) {
		showOccluderFrustum<Arch>( stateForCamera, 0.0f, g_lockedViewOrigin, g_lockedViewAxis,
								   occluderDataEntries + g_lockedOccluderNum - 1 );
		return {};
	}
#endif

	for( unsigned occluderNum = 0; occluderNum < numWorldModelOccluders; ++occluderNum ) {
		visibleOccluders[numVisibleOccluders] = occluderNum;
		const OccluderBoundsEntry &__restrict occluderBounds = occluderBoundsEntries[occluderNum];
		LOAD_BOX_COMPONENTS( occluderBounds.mins, occluderBounds.maxs );
		COMPUTE_RESULT_OF_FULLY_OUTSIDE_TEST_FOR_4_PLANES( frustum, const int nonZeroIfFullyOutside );
		numVisibleOccluders += ( nonZeroIfFullyOutside == 0 );
	}

	return { visibleOccluders, numVisibleOccluders };
}

#endif

#ifdef IMPLEMENT_buildFrustaOfOccluders

static constexpr float kSelfOcclusionBias = 0.1f;

template <unsigned Arch>
auto RendererFrontend::buildFrustaOfOccludersArch( StateForCamera *stateForCamera, std::span<const SortedOccluder> sortedOccluders )
	-> std::span<const Frustum> {
	Frustum *const occluderFrusta = stateForCamera->occluderFrusta;
	const unsigned maxOccluders   = wsw::min<unsigned>( sortedOccluders.size(), std::size( stateForCamera->occluderFrusta ) );

	const float *__restrict viewOrigin                = stateForCamera->viewOrigin;
	[[maybe_unused]] const float *__restrict viewAxis = stateForCamera->viewAxis;

	bool hadCulledFrusta = false;
	// Note: We don't process more occluders due to performance and not memory capacity reasons.
	// Best occluders come first so they should make their way into the final result.
	alignas( 16 )bool isCulledByOtherTable[kMaxOccluderFrusta];
	// MSVC fails to get the member array count in compile time
	assert( std::size( isCulledByOtherTable ) == std::size( stateForCamera->occluderFrusta ) );
	std::memset( isCulledByOtherTable, 0, sizeof( bool ) * maxOccluders );

	// Note: An outer loop over all surfaces would have been allowed to avoid redundant component shuffles
	// but this approach requires building all frusta prior to that, which is more expensive.

#ifndef DEBUG_OCCLUDERS
	const OccluderBoundsEntry *const occluderBoundsEntries = rsh.worldBrushModel->occluderBoundsEntries;
#endif
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

		// Orient the capping plane inside the frustum
		if( DotProduct( cappingPlane, viewOrigin ) - cappingPlane[3] > 0 ) {
			Vector4Negate( cappingPlane, cappingPlane );
		}

		// Don't let the surface occlude itself
		if( DotProduct( cappingPlane, viewOrigin ) - ( cappingPlane[3] + kSelfOcclusionBias ) < 0 ) {
			cappingPlane[3] += kSelfOcclusionBias;
		} else {
			cappingPlane[3] -= kSelfOcclusionBias;
		}

		assert( DotProduct( cappingPlane, viewOrigin ) - cappingPlane[3] < 0 );

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
	// Show preserved frusta
	for( unsigned occluderIndex = 0; occluderIndex < maxOccluders; ++occluderIndex ) {
		if( isCulledByOtherTable[occluderIndex] ) {
			continue;
		}

		const unsigned occluderNum = sortedOccluders[occluderIndex].occluderNum;
		const OccluderDataEntry *const occluderData = occluderDataEntries + occluderNum;

#ifdef DEBUG_OCCLUDERS
		if( Cvar_Integer( "testedOccluderIndex" ) != (int)( 1 + occluderIndex ) ) {
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

		Com_Printf( "Occluder num for locking=%d absViewDot=%f distanceToPlane=%f viewAxisDotToOccluder=%f planeDotToOccluder=%f\n",
					occluderNum + 1, absViewDot, distanceToPlane, viewAxisDotToOccluder, planeDotToOccluder );
#endif

		//addDebugLine( stateForCamera, surface->occluderPolyMins, surface->occluderPolyMaxs, COLOR_RGB( 0, 128, 255 ) );

		showOccluderFrustum<Arch>( stateForCamera, 8.0f, viewOrigin, viewAxis, occluderData );
	}
#endif

	return { occluderFrusta, occluderFrusta + numSelectedOccluders };
}

#if defined( IMPLEMENT_collectVisibleOccluders ) || defined( IMPLEMENT_buildFrustaOfOccluders )
#if defined( SHOW_OCCLUDERS_FRUSTA ) || defined( DEBUG_OCCLUDERS )
template <unsigned Arch>
void RendererFrontend::showOccluderFrustum( StateForCamera *stateForCamera, float offsetInFrontOfView,
									const float *viewOrigin, const float *viewAxis,
									const OccluderDataEntry *occluderData ) {
	vec3_t pointInFrontOfView;
	VectorMA( viewOrigin, offsetInFrontOfView, viewAxis, pointInFrontOfView );

	// We have to shift vertices as we apply kSelfOcclusionBias

	vec3_t biasedVertices[7];
	assert( occluderData->numVertices <= std::size( biasedVertices ) );
	for( unsigned vertIndex = 0; vertIndex < occluderData->numVertices; ++vertIndex ) {
		const float *originalVertex = occluderData->data[vertIndex];
		if( const float squareDist = DistanceSquared( originalVertex, pointInFrontOfView ); squareDist > 1e-3f ) {
			vec3_t dir;
			VectorSubtract( originalVertex, pointInFrontOfView, dir );
			const float rcpDist = Q_RSqrt( squareDist );
			VectorScale( dir, rcpDist, dir );
			if( const float absDot = std::fabs( DotProduct( dir, occluderData->plane ) ); absDot > 1e-3f ) {
				const float biasAlongTheDir = kSelfOcclusionBias * Q_Rcp( absDot );
				const float newDist         = squareDist * rcpDist + biasAlongTheDir;
				VectorMA( pointInFrontOfView, newDist, dir, biasedVertices[vertIndex] );
			} else {
				VectorCopy( originalVertex, biasedVertices[vertIndex] );
			}
		} else {
			VectorCopy( originalVertex, biasedVertices[vertIndex] );
		}
	}

	vec4_t cappingPlane;
	Vector4Copy( occluderData->plane, cappingPlane );

	// Orient the capping plane inside the frustum
	if( DotProduct( cappingPlane, viewOrigin ) - cappingPlane[3] > 0 ) {
		Vector4Negate( cappingPlane, cappingPlane );
	}

	// Don't let the surface occlude itself
	if( DotProduct( cappingPlane, viewOrigin ) - ( cappingPlane[3] + kSelfOcclusionBias ) < 0 ) {
		cappingPlane[3] += kSelfOcclusionBias;
	} else {
		cappingPlane[3] -= kSelfOcclusionBias;
	}

	vec3_t cappingNormalPoint1, cappingNormalPoint2;
	VectorMA( occluderData->innerPolyPoint, kSelfOcclusionBias, cappingPlane, cappingNormalPoint1 );
	VectorMA( occluderData->innerPolyPoint, kSelfOcclusionBias + 32.0f, cappingPlane, cappingNormalPoint2 );
	addDebugLine( stateForCamera, cappingNormalPoint1, cappingNormalPoint2 );

	for( unsigned vertIndex = 0; vertIndex < occluderData->numVertices; ++vertIndex ) {
		const float *const v1 = biasedVertices[vertIndex + 0];
		const float *const v2 = biasedVertices[( vertIndex + 1 != occluderData->numVertices ) ? vertIndex + 1 : 0];

		addDebugLine( stateForCamera, v1, pointInFrontOfView );
		addDebugLine( stateForCamera, v1, v2, COLOR_RGB( 255, 0, 255 ) );

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
		addDebugLine( stateForCamera, midpointOfEdge, normalPoint );

		addDebugLine( stateForCamera, midpointOfEdge, cappingNormalPoint1 );
	}
}
#endif
#endif

#endif

#ifdef IMPLEMENT_cullSurfacesByOccluders

template <unsigned Arch>
void RendererFrontend::cullSurfacesByOccludersArch( [[maybe_unused]] StateForCamera *stateForCamera,
													std::span<const unsigned> surfNums,
													std::span<const Frustum> occluderFrusta,
											        const uint64_t *surfOccluderMaskTable,
													MergedSurfSpan *mergedSurfSpans, uint8_t *surfVisTable ) {
	assert( !occluderFrusta.empty() );
	assert( !surfNums.empty() );

#ifdef DEBUG_OCCLUDERS
	const int testedOccluderIndex = Cvar_Integer( "testedOccluderIndex" );
#endif

	// TODO: Use library functions once they are available, use intrinsics on ARM64

	auto setMinAtomic = []( int *addr, int newValue ) {
		assert( ( (uintptr_t)addr % alignof( std::atomic<int> ) ) == 0 );
		auto *const a = (std::atomic<int> *)addr;
		int oldValue  = a->load();
		// TODO: Can we do something smarter with memory loads
		while( oldValue > newValue && !a->compare_exchange_weak( oldValue, newValue ) ) {
			oldValue = a->load();
		}
	};

	auto setMaxAtomic = []( int *addr, int newValue ) {
		assert( ( (uintptr_t)addr % alignof( std::atomic<int> ) ) == 0 );
		auto *a      = (std::atomic<int> *)addr;
		int oldValue = a->load();
		// TODO: Can we do something smarter with memory loads
		while( oldValue < newValue && !a->compare_exchange_weak( oldValue, newValue ) ) {
			oldValue = a->load();
		}
	};

	const msurface_t *const surfaces = rsh.worldBrushModel->surfaces;

	size_t surfIndex = 0;
	do {
		bool surfVisible                        = true;
		const unsigned surfNum                  = surfNums[surfIndex];
		uint64_t surfOccluderMask               = surfOccluderMaskTable[surfNum];
		const msurface_t *const __restrict surf = surfaces + surfNum;

		// We can use while loop but this design should eliminate the redundant zero check in countr_zero
		if( surfOccluderMask ) {
			LOAD_BOX_COMPONENTS( surf->mins, surf->maxs );

			do {
				const uint64_t firstBitIndex = std::countr_zero( surfOccluderMask );
				const Frustum *__restrict f  = &occluderFrusta[firstBitIndex];

	#ifdef DEBUG_OCCLUDERS
				if( testedOccluderIndex && testedOccluderIndex != (int)( 1 + frustumIndex ) ) {
					continue;
				}
	#endif

				COMPUTE_RESULT_OF_FULLY_INSIDE_TEST_FOR_8_PLANES( f, const int zeroIfFullyInside )

				if( zeroIfFullyInside == 0 ) [[unlikely]] {
					surfVisible = false;
					SHOW_OCCLUDED( stateForCamera, surf->mins, surf->maxs, COLOR_RGB( 192, 0, 0 ) );
					break;
				}
				surfOccluderMask &= ~( ( (uint64_t)1 ) << firstBitIndex );
			} while( surfOccluderMask );
		}

		if( surfVisible ) {
			surfVisTable[surfNum] = 1;
			assert( surf->mergedSurfNum > 0 );
			const unsigned mergedSurfNum = surf->mergedSurfNum - 1;
			MergedSurfSpan *const __restrict span = &mergedSurfSpans[mergedSurfNum];
			setMinAtomic( &span->firstSurface, (int)surfNum );
			setMaxAtomic( &span->lastSurface, (int)surfNum );
		}
	} while( ++surfIndex != surfNums.size() );
}

#endif

#ifdef IMPLEMENT_cullEntriesWithBounds

template <unsigned Arch>
auto RendererFrontend::cullEntriesWithBoundsArch( [[maybe_unused]] StateForCamera *stateForCamera,
										  const void *entries, unsigned numEntries,
										  unsigned boundsFieldOffset, unsigned strideInBytes,
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
						SHOW_OCCLUDED( stateForCamera, bounds[0], bounds[1], COLOR_RGB( 255, 192, 255 ) );
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
auto RendererFrontend::cullEntryPtrsWithBoundsArch( [[maybe_unused]] StateForCamera *stateForCamera,
											const void **entryPtrs, unsigned numEntries, unsigned boundsFieldOffset,
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
						SHOW_OCCLUDED( stateForCamera, bounds[0], bounds[1], COLOR_RGB( 255, 192, 255 ) );
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