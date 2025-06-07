/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2022 Chasseur de bots

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

#include "cg_local.h"
#include <client/client.h>
#include <common/helpers/links.h>
#include <common/facilities/profilerscope.h>

PolyEffectsSystem::PolyEffectsSystem() {
	m_tmpShapeList = CM_AllocShapeList( cl.cms );
	if( !m_tmpShapeList ) {
		wsw::failWithBadAlloc();
	}
}

PolyEffectsSystem::~PolyEffectsSystem() {
	clear();
	CM_FreeShapeList( cl.cms, m_tmpShapeList );
}

void PolyEffectsSystem::clear() {
	for( CurvedBeamEffect *beam = m_curvedLaserBeamsHead, *next; beam; beam = next ) { next = beam->next;
		destroyCurvedBeamEffect( beam );
	}
	assert( !m_curvedLaserBeamsHead );
	for( StraightBeamEffect *beam = m_straightLaserBeamsHead, *next; beam; beam = next ) { next = beam->next;
		destroyStraightBeamEffect( beam );
	}
	assert( !m_straightLaserBeamsHead );
	for( TransientBeamEffect *beam = m_transientBeamsHead, *next; beam; beam = next ) { next = beam->next;
		destroyTransientBeamEffect( beam );
	}
	assert( !m_transientBeamsHead );
	for( TracerEffect *tracer = m_tracerEffectsHead, *next; tracer; tracer = next ) { next = tracer->next;
		destroyTracerEffect( tracer );
	}
	assert( !m_tracerEffectsHead );
	for( ImpactRosetteEffect *rosette = m_impactRosetteEffectsHead, *next; rosette; rosette = next ) { next = rosette->next;
		destroyImpactRosetteEffect( rosette );
	}
	assert( !m_impactRosetteEffectsHead );
	for( RibbonEffect *ribbon = m_ribbonEffectsHead, *next; ribbon; ribbon = next ) { next = ribbon->next;
		destroyRibbonEffect( ribbon );
	}
	assert( !m_ribbonEffectsHead );
	m_lastTime = 0;
}

auto PolyEffectsSystem::createCurvedBeamEffect( shader_s *material, unsigned povPlayerMask ) -> CurvedBeam * {
	if( auto *mem = m_curvedLaserBeamsAllocator.allocOrNull() ) [[likely]] {
		auto *effect = new( mem )CurvedBeamEffect;
		wsw::link( effect, &m_curvedLaserBeamsHead );
		effect->poly.material = material;
		effect->povPlayerMask = povPlayerMask;
		return effect;
	}
	return nullptr;
}

void PolyEffectsSystem::updateCurvedBeamEffect( CurvedBeam *handle, const float *fromColor, const float *toColor,
												float width, const CurvedBeamUVMode &uvMode,
												std::span<const vec3_t> points ) {
	auto *const __restrict effect = (CurvedBeamEffect *)handle;
	assert( (uintptr_t)effect == (uintptr_t)handle );

	Vector4Copy( fromColor, effect->poly.fromColor );
	Vector4Copy( toColor, effect->poly.toColor );

	effect->poly.width  = width;
	effect->poly.uvMode = uvMode;

	// Caution! We borrow the reference to an external buffer
	effect->poly.points = points;

	if( points.size() > 1 ) [[likely]] {
		BoundsBuilder boundsBuilder;
		for( const float *point: points ) {
			boundsBuilder.addPoint( point );
		}
		// Consider this to be a good estimation
		boundsBuilder.storeToWithAddedEpsilon( effect->poly.cullMins, effect->poly.cullMaxs, width + 1.0f );
	}
}

auto PolyEffectsSystem::CurvedBeamPoly::getStorageRequirements( const float *, const float *, float, float,
																unsigned, void *, unsigned ) const
	-> std::optional<std::pair<unsigned, unsigned>> {
	assert( points.size() > 1 );
	return std::make_pair( (unsigned)( 4 * points.size() ), (unsigned)( 6 * points.size() ) );
}

[[nodiscard]]
auto PolyEffectsSystem::CurvedBeamPoly::fillMeshBuffers( const float *__restrict viewOrigin,
														 const float *__restrict,
														 const Scene::DynamicLight *,
														 std::span<const uint16_t>,
														 void *,
														 vec4_t *__restrict positions,
														 vec4_t *__restrict,
														 vec2_t *__restrict texCoords,
														 byte_vec4_t *__restrict colors,
														 uint16_t *__restrict indices ) const
	-> std::pair<unsigned, unsigned> {
	assert( points.size() > 1 && points.size() < 64 );
	assert( std::isfinite( width ) && width > 0 );

	// std::variant interface sucks
	const UvModeTile *const uvModeTile   = std::get_if<UvModeTile>( &uvMode );
	const UvModeClamp *const uvModeClamp = std::get_if<UvModeClamp>( &uvMode );

	// We have to precalculate some parameters for UvModeFit.
	// This doesn't make the code inefficient for other case.

	auto *const __restrict rcpLengthOfSegments  = (float *)alloca( sizeof( float ) * points.size() );

	bool foundDegenerateSegment    = false;
	unsigned numDrawnSegments      = 0;
	float totalLengthOfDrawnPart   = 0.0f;
	float totalLengthOfPointsChain = 0.0f;
	for( unsigned segmentNum = 0; segmentNum + 1 < points.size(); ++segmentNum ) {
		const float *const from = points[segmentNum + 0];
		const float *const to   = points[segmentNum + 1];

		const float squareSegmentLength = DistanceSquared( from, to );
		if( !foundDegenerateSegment ) [[likely]] {
			if( squareSegmentLength < 1.0f ) [[unlikely]] {
				if( !uvModeClamp ) {
					// Interrupt at this - we don't have to know the full chain length for other cases
					break;
				} else {
					totalLengthOfPointsChain += Q_Sqrt( squareSegmentLength );
					foundDegenerateSegment = true;
				}
			} else {
				const float rcpSegmentLength    = Q_RSqrt( squareSegmentLength );
				const float segmentLength       = squareSegmentLength * rcpSegmentLength;
				rcpLengthOfSegments[segmentNum] = rcpSegmentLength;

				totalLengthOfPointsChain += segmentLength;
				totalLengthOfDrawnPart   += segmentLength;
				numDrawnSegments++;
			}
		} else {
			totalLengthOfPointsChain += Q_Sqrt( squareSegmentLength );
		}
	}

	assert( totalLengthOfDrawnPart <= totalLengthOfPointsChain );

	// Lift reciprocal computations out of the loop

	float rcpTileLength = 0.0f;
	if( uvModeTile ) {
		assert( uvModeTile->tileLength > 0.0f );
		rcpTileLength = Q_Rcp( uvModeTile->tileLength );
	}

	float rcpTotalLengthOfDrawnPart = 0.0f;
	if( !uvModeClamp && totalLengthOfDrawnPart > 0.0f ) {
		rcpTotalLengthOfDrawnPart = Q_Rcp( totalLengthOfDrawnPart );
	}

	float rcpTotalLengthOfPointsChain = 0.0f;
	if( uvModeClamp && totalLengthOfPointsChain > 0.0f ) {
		rcpTotalLengthOfPointsChain = Q_Rcp( totalLengthOfPointsChain );
	}

	byte_vec4_t sharedColor;
	const bool hasVaryingColors = !VectorCompare( fromColor, toColor ) || fromColor[3] != toColor[3];
	if( !hasVaryingColors ) {
		for( unsigned i = 0; i < 4; ++i ) {
			sharedColor[i] = (uint8_t)wsw::clamp( 255.0f * fromColor[i], 0.0f, 255.0f );
		}
	}

	bool hasValidPrevSegment = false;
	unsigned numVertices = 0, numIndices = 0;

	const float halfWidth  = 0.5f * width;
	float totalLengthSoFar = 0.0f;
	// Note: we have to submit separate quads as some segments could be discarded
	for( unsigned segmentNum = 0; segmentNum < numDrawnSegments; ++segmentNum ) {
		const float *const from = points[segmentNum + 0];
		const float *const to   = points[segmentNum + 1];

		const float rcpSegmentLength = rcpLengthOfSegments[segmentNum];

		vec3_t segmentDir;
		VectorSubtract( to, from, segmentDir );
		VectorScale( segmentDir, rcpSegmentLength, segmentDir );

		vec3_t mid, viewToMid, right;
		VectorAvg( from, to, mid );

		VectorSubtract( mid, viewOrigin, viewToMid );
		CrossProduct( viewToMid, segmentDir, right );
		const float squareRightLength = VectorLengthSquared( right );
		if( squareRightLength < wsw::square( 0.001f ) ) [[unlikely]] {
			hasValidPrevSegment = false;
			continue;
		}

		const float rcpRightLength = Q_RSqrt( squareRightLength );
		VectorScale( right, rcpRightLength, right );

		if( hasValidPrevSegment ) [[likely]] {
			VectorCopy( positions[-1], positions[0] );
			VectorCopy( positions[-2], positions[1] );
		} else {
			VectorMA( from, +halfWidth, right, positions[0] );
			VectorMA( from, -halfWidth, right, positions[1] );
		}

		VectorMA( to, -halfWidth, right, positions[2] );
		VectorMA( to, +halfWidth, right, positions[3] );
		positions[0][3] = positions[1][3] = positions[2][3] = positions[3][3] = 1.0f;

		VectorSet( indices + 0, numVertices + 0, numVertices + 1, numVertices + 2 );
		VectorSet( indices + 3, numVertices + 0, numVertices + 2, numVertices + 3 );

		const float lengthAt1 = totalLengthSoFar;
		totalLengthSoFar += Q_Rcp( rcpSegmentLength );
		const float lengthAt2 = totalLengthSoFar;

		float stx1, stx2;
		if( uvModeTile ) {
			stx1 = lengthAt1 * rcpTileLength;
			stx2 = lengthAt2 * rcpTileLength;
		} else if( uvModeClamp ) {
			stx1 = wsw::clamp( lengthAt1 * rcpTotalLengthOfPointsChain, 0.0f, 1.0f );
			stx2 = wsw::clamp( lengthAt2 * rcpTotalLengthOfPointsChain, 0.0f, 1.0f );
		} else {
			stx1 = wsw::clamp( lengthAt1 * rcpTotalLengthOfDrawnPart, 0.0f, 1.0f );
			stx2 = wsw::clamp( lengthAt2 * rcpTotalLengthOfDrawnPart, 0.0f, 1.0f );
		}

		Vector2Set( texCoords[0], stx1, 0.0f );
		Vector2Set( texCoords[1], stx1, 1.0f );
		Vector2Set( texCoords[2], stx2, 1.0f );
		Vector2Set( texCoords[3], stx2, 0.0f );

		if( hasVaryingColors ) {
			float colorFrac1, colorFrac2;
			if( uvModeClamp ) {
				colorFrac1 = lengthAt1 * rcpTotalLengthOfPointsChain;
				colorFrac2 = lengthAt2 * rcpTotalLengthOfPointsChain;
			} else {
				colorFrac1 = lengthAt1 * rcpTotalLengthOfDrawnPart;
				colorFrac2 = lengthAt2 * rcpTotalLengthOfDrawnPart;
			}

			assert( colorFrac1 >= 0.0f && colorFrac1 <= 1.01f );
			assert( colorFrac2 >= 0.0f && colorFrac2 <= 1.01f );

			vec4_t colorAt1, colorAt2;
			Vector4Lerp( this->fromColor, colorFrac1, this->toColor, colorAt1 );
			Vector4Lerp( this->fromColor, colorFrac2, this->toColor, colorAt2 );

			for( unsigned i = 0; i < 4; ++i ) {
				colors[0][i] = colors[1][i] = (uint8_t)wsw::clamp( 255.0f * colorAt1[i], 0.0f, 255.0f );
				colors[2][i] = colors[3][i] = (uint8_t)wsw::clamp( 255.0f * colorAt2[i], 0.0f, 255.0f );
			}
		} else {
			Vector4Copy( sharedColor, colors[0] );
			Vector4Copy( sharedColor, colors[1] );
			Vector4Copy( sharedColor, colors[2] );
			Vector4Copy( sharedColor, colors[3] );
		}

		numVertices += 4;
		numIndices += 6;

		positions += 4, colors += 4, texCoords += 4;
		indices += 6;

		hasValidPrevSegment = true;
	}

	return { numVertices, numIndices };
}

void PolyEffectsSystem::destroyCurvedBeamEffect( CurvedBeam *handle ) {
	auto *effect = (CurvedBeamEffect *)handle;
	assert( (uintptr_t)effect == (uintptr_t)handle );
	wsw::unlink( effect, &m_curvedLaserBeamsHead );
	effect->~CurvedBeamEffect();
	m_curvedLaserBeamsAllocator.free( effect );
}

auto PolyEffectsSystem::createStraightBeamEffect( shader_s *material, unsigned povPlayerMask ) -> StraightBeam * {
	if( void *mem = m_straightLaserBeamsAllocator.allocOrNull() ) [[likely]] {
		auto *effect = new( mem )StraightBeamEffect;
		wsw::link( effect, &m_straightLaserBeamsHead );
		effect->poly.material   = material;
		effect->poly.halfExtent = 0.0f;
		effect->poly.animFrac   = 0.0f;
		effect->povPlayerMask   = povPlayerMask;
		return effect;
	}
	return nullptr;
}

void PolyEffectsSystem::updateStraightBeamEffect( StraightBeam *handle, const float *fromColor, const float *toColor,
												  float width, float tileLength, const float *from, const float *to ) {
	auto *effect = (StraightBeamEffect *)( handle );
	assert( (uintptr_t)effect == (uintptr_t)handle );

	if( effect->poly.material ) [[likely]] {
		if( const float squareLength = DistanceSquared( from, to ); squareLength >= 1.0f ) [[likely]] {
			const float rcpLength = Q_RSqrt( squareLength );
			vec3_t dir;
			VectorSubtract( to, from, dir );
			VectorScale( dir, rcpLength, dir );
			VectorAvg( from, to, effect->poly.origin );
			effect->poly.halfExtent      = 0.5f * ( squareLength * rcpLength );
			effect->poly.appearanceRules = QuadPoly::ViewAlignedBeamRules {
				.dir        = { dir[0], dir[1], dir[2] },
				.width      = width,
				.tileLength = tileLength,
				.fromColor  = { fromColor[0], fromColor[1], fromColor[2], fromColor[3] },
				.toColor    = { toColor[0], toColor[1], toColor[2], toColor[3] },
			};
		} else {
			// Suppress rendering this frame
			effect->poly.halfExtent = 0.0f;
		}
	}
}

void PolyEffectsSystem::destroyStraightBeamEffect( StraightBeam *handle ) {
	auto *effect = ( StraightBeamEffect * )( handle );
	assert( (uintptr_t)effect == (uintptr_t)handle );
	wsw::unlink( effect, &m_straightLaserBeamsHead );
	effect->~StraightBeamEffect();
	m_straightLaserBeamsAllocator.free( effect );
}

void PolyEffectsSystem::destroyTransientBeamEffect( TransientBeamEffect *effect ) {
	wsw::unlink( effect, &m_transientBeamsHead );
	effect->~TransientBeamEffect();
	m_transientBeamsAllocator.free( effect );
}

void PolyEffectsSystem::destroyTracerEffect( TracerEffect *effect ) {
	wsw::unlink( effect, &m_tracerEffectsHead );
	effect->~TracerEffect();
	m_tracerEffectsAllocator.free( effect );
}

void PolyEffectsSystem::destroyImpactRosetteEffect( ImpactRosetteEffect *effect ) {
	wsw::unlink( effect, &m_impactRosetteEffectsHead );
	effect->~ImpactRosetteEffect();
	m_impactRosetteEffectsAllocator.free( effect );
}

void PolyEffectsSystem::destroyRibbonEffect( RibbonEffect *effect ) {
	wsw::unlink( effect, &m_ribbonEffectsHead );
	effect->~RibbonEffect();
	m_ribbonEffectsAllocator.free( effect );
}

void PolyEffectsSystem::spawnTransientBeamEffect( const float *from, const float *to, TransientBeamParams &&params ) {
	if( params.width < 1.0f || !params.material || !params.numPlanes ) [[unlikely]] {
		return;
	}

	const float squareLength = DistanceSquared( from, to );
	if( squareLength < params.width * params.width ) [[unlikely]] {
		return;
	}

	void *mem = m_transientBeamsAllocator.allocOrNull();
	if( !mem ) [[unlikely]] {
		assert( m_transientBeamsHead );
		// Pick the oldest one to reuse.
		// Note: extra bookkeeping of the list tail is not worth it.
		// Note: it's actually possible to extract in a very cheap fashion using links the of underlying allocator.
		TransientBeamEffect *oldestEffect = m_transientBeamsHead->next;
		while( oldestEffect ) {
			if( oldestEffect->next ) {
				oldestEffect = oldestEffect->next;
			} else {
				break;
			}
		}
		wsw::unlink( oldestEffect, &m_transientBeamsHead );
		oldestEffect->~TransientBeamEffect();
		mem = oldestEffect;
	}

	assert( params.timeout < std::numeric_limits<uint16_t>::max() );

	auto *effect = new( mem )TransientBeamEffect;

	effect->spawnTime     = m_lastTime;
	effect->timeout       = params.timeout;
	effect->colorLifespan = params.beamColorLifespan,
	effect->lightProps    = params.lightProps;

	vec3_t dir;
	VectorSubtract( to, from, dir );
	const float rcpLength = Q_RSqrt( squareLength );
	VectorScale( dir, rcpLength, dir );

	VectorAvg( from, to, effect->poly.origin );

	effect->poly.material        = params.material;
	effect->poly.halfExtent      = 0.5f * ( squareLength * rcpLength );
	effect->poly.appearanceRules = QuadPoly::ViewAlignedBeamRules {
		.dir        = { dir[0], dir[1], dir[2] },
		.width      = params.width,
		.tileLength = params.tileLength,
		.numPlanes  = params.numPlanes,
	};

	wsw::link( effect, &m_transientBeamsHead );
}

auto PolyEffectsSystem::spawnTracerEffect( const float *from, const float *to,
										   unsigned povPlayerMask, TracerParams &&params ) -> std::optional<unsigned> {
	assert( params.duration > 50 && params.prestepDistance >= 1.0f && params.width > 0.0f );
	assert( povPlayerMask != 0 );

	const float squareDistance = DistanceSquared( from, to );
	if( squareDistance < wsw::square( params.prestepDistance ) ) [[unlikely]] {
		return std::nullopt;
	}

	vec3_t dir;
	VectorSubtract( to, from, dir );
	const float rcpDistance = Q_RSqrt( squareDistance );
	VectorScale( dir, rcpDistance, dir );

	void *mem = m_tracerEffectsAllocator.allocOrNull();
	if( !mem ) [[unlikely]] {
		assert( m_tracerEffectsHead );
		TracerEffect *oldestEffect = m_tracerEffectsHead->next;
		while( oldestEffect ) {
			if( oldestEffect->next ) {
				oldestEffect = oldestEffect->next;
			} else {
				break;
			}
		}
		wsw::unlink( oldestEffect, &m_tracerEffectsHead );
		oldestEffect->~TracerEffect();
		mem = oldestEffect;
	}

	// Note: Don't include prestep in speed computations.
	// Doing that makes the speed vary depending on the different prestep value.
	const float totalDistance     = squareDistance * rcpDistance;
	const float tracerTimeSeconds = 1e-3f * (float)params.duration;
	const float polyLength        = wsw::max( params.minLength, params.distancePercentage * totalDistance );
	const float speed             = wsw::max( 3000.0f, totalDistance * Q_Rcp( tracerTimeSeconds ) );

	auto *effect                 = new( mem )TracerEffect;
	effect->spawnTime            = m_lastTime;
	effect->timeout              = params.duration;
	effect->povPlayerMask        = povPlayerMask;
	effect->alignForPovParams    = params.alignForPovParams;
	effect->speed                = speed;
	effect->prestepDistance      = params.prestepDistance;
	effect->totalDistance        = totalDistance;
	effect->distanceSoFar        = params.prestepDistance;
	effect->initialColorAlpha    = params.color[3];
	effect->lightFadeInDistance  = 2.0f * params.prestepDistance;
	effect->lightFadeOutDistance = 2.0f * params.prestepDistance;
	effect->smoothEdgeDistance   = params.smoothEdgeDistance;
	effect->poly.material        = params.material;
	effect->poly.halfExtent      = 0.5f * polyLength;
	effect->poly.appearanceRules = QuadPoly::ViewAlignedBeamRules {
		.dir        = { dir[0], dir[1], dir[2] },
		.width      = params.width,
		.tileLength = polyLength,
		.fromColor  = { params.color[0], params.color[1], params.color[2], params.color[3] },
		.toColor    = { params.color[0], params.color[1], params.color[2], params.color[3] },
	};

	VectorCopy( from, effect->from );
	VectorCopy( to, effect->to );
	VectorCopy( dir, effect->dir );

	VectorCopy( params.lightColor, effect->lightColor );
	effect->programLightRadius       = params.programLightRadius;
	effect->coronaLightRadius        = params.coronaLightRadius;
	effect->lightFrameAffinityModulo = params.lightFrameAffinityModulo;
	effect->lightFrameAffinityIndex  = params.lightFrameAffinityIndex;

	wsw::link( effect, &m_tracerEffectsHead );

	return (unsigned)( 1000 * totalDistance * Q_Rcp( speed ) );
}

void PolyEffectsSystem::spawnImpactRosette( ImpactRosetteParams &&params ) {
	assert( params.width.mean > 0 && params.width.spread >= 0.0f );
	assert( params.length.mean > 0 && params.length.spread >= 0.0f );
	assert( params.timeout.min < params.timeout.max );
	assert( params.timeout.min > 0 && params.timeout.max < 1000 );
	assert( params.count.min <= params.count.max );
	assert( params.innerConeAngle < params.outerConeAngle );
	assert( params.spikeMaterial && params.flareMaterial );

	vec3_t origin;
	VectorAdd( params.origin, params.offset, origin );

	vec3_t axis1, axis2;
	MakeNormalVectors( params.dir, axis1, axis2 );
	assert( std::fabs( VectorLengthFast( axis1 ) - 1.0f ) < 0.1f );
	assert( std::fabs( VectorLengthFast( axis2 ) - 1.0f ) < 0.1f );

	vec3_t startPoints[kMaxImpactRosetteElements];
	vec3_t endPoints[kMaxImpactRosetteElements];
	vec3_t dirs[kMaxImpactRosetteElements];
	float length[kMaxImpactRosetteElements];
	BoundsBuilder boundsBuilder;

	unsigned desiredNumElements = params.count.min;
	if( params.count.min < params.count.max ) {
		desiredNumElements += m_rng.nextBounded( params.count.max - params.count.min );
	}

	const unsigned actualNumElements  = wsw::clamp( desiredNumElements, 1u, kMaxImpactRosetteElements );

	// Randomize the initial angle as well
	float circleAngle           = m_rng.nextFloat( 0, (float)M_PI );
	const float circleAngleStep = (float)M_TWOPI * Q_Rcp( (float)actualNumElements );
	for( unsigned i = 0; i < actualNumElements; ++i ) {
		const float scale1 = std::sin( circleAngle ), scale2 = std::cos( circleAngle );
		circleAngle += circleAngleStep;

		vec3_t radialDir { 0.0f, 0.0f, 0.0f };
		VectorMA( radialDir, scale1, axis1, radialDir );
		VectorMA( radialDir, scale2, axis2, radialDir );
		assert( std::fabs( VectorLengthFast( radialDir ) - 1.0f ) < 0.1f );

		const float coneAngle = m_rng.nextFloat( params.innerConeAngle, params.outerConeAngle );

		length[i] = params.length.mean;
		if( const float spread = params.length.spread; spread > 0.0f ) {
			length[i] = wsw::max( 0.1f, length[i] + m_rng.nextFloat( -spread, +spread ) );
		}

		// TODO: Can we do something smarter without these sin/cos calls?
		const float heightOverSurface = std::cos( (float)DEG2RAD( coneAngle ) ) * length[i];
		const float outerRadius = params.spawnRingRadius + std::sin( (float)DEG2RAD( coneAngle ) ) * length[i];

		VectorMA( origin, params.spawnRingRadius, radialDir, startPoints[i] );

		VectorMA( origin, outerRadius, radialDir, endPoints[i] );
		VectorMA( endPoints[i], heightOverSurface, params.dir, endPoints[i] );

		VectorSubtract( endPoints[i], startPoints[i], dirs[i] );

		const float rcpLength = Q_Rcp( length[i] );
		VectorScale( dirs[i], rcpLength, dirs[i] );

		boundsBuilder.addPoint( startPoints[i] );
		boundsBuilder.addPoint( endPoints[i] );
	}

	vec3_t bounds[2];
	boundsBuilder.storeToWithAddedEpsilon( bounds[0], bounds[1] );

	// TODO: Add a fused call
	CM_BuildShapeList( cl.cms, m_tmpShapeList, bounds[0], bounds[1], MASK_SOLID );
	CM_ClipShapeList( cl.cms, m_tmpShapeList, m_tmpShapeList, bounds[0], bounds[1] );

	void *mem = m_impactRosetteEffectsAllocator.allocOrNull();
	// TODO: Use links with a sentinel header...
	if( !mem ) [[unlikely]] {
		assert( m_impactRosetteEffectsHead );
		ImpactRosetteEffect *oldestEffect = m_impactRosetteEffectsHead->next;
		while( oldestEffect ) {
			if( oldestEffect->next ) {
				oldestEffect = oldestEffect->next;
			} else {
				break;
			}
		}
		wsw::unlink( oldestEffect, &m_impactRosetteEffectsHead );
		oldestEffect->~ImpactRosetteEffect();
		mem = oldestEffect;
	}

	auto *const effect         = new( mem )ImpactRosetteEffect;
	effect->spawnTime          = m_lastTime;
	effect->lifetime           = 0;
	effect->startColorLifespan = params.startColorLifespan;
	effect->endColorLifespan   = params.endColorLifespan;
	effect->flareColorLifespan = params.flareColorLifespan;
	effect->lightLifespan      = params.lightLifespan;
	effect->numElements        = actualNumElements;

	effect->elementFlareFrameAffinityModulo = params.elementFlareFrameAffinityModulo;
	effect->effectFlareFrameAffinityModulo  = params.effectFlareFrameAffinityModulo;
	effect->effectFlareFrameAffinityIndex   = params.effectFlareFrameAffinityIndex;
	effect->lightFrameAffinityModulo        = params.lightFrameAffinityModulo;
	effect->lightFrameAffinityIndex         = params.lightFrameAffinityIndex;

	float maxFinalLength = 0.0f;
	for( unsigned i = 0; i < actualNumElements; ++i ) {
		ImpactRosetteElement *const element = &effect->elements[i];

		VectorCopy( startPoints[i], element->from );
		VectorCopy( dirs[i], element->dir );

		element->desiredLength = length[i];
		element->lengthLimit   = length[i];

		element->width = params.width.mean;
		if( const float spread = params.width.spread; spread > 0.0f ) {
			element->width = wsw::max( 0.1f, element->width + m_rng.nextFloat( -spread, +spread ) );
		}

		trace_t trace;
		CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, startPoints[i], endPoints[i],
							vec3_origin, vec3_origin, MASK_SOLID );

		element->lengthLimit *= trace.fraction;

		element->lifetime = params.timeout.min + m_rng.nextBoundedFast( params.timeout.max - params.timeout.min );
		effect->lifetime  = wsw::max( effect->lifetime, element->lifetime );
		maxFinalLength    = wsw::max( element->lengthLimit, maxFinalLength );
	}

	assert( effect->lifetime > 0 );

	effect->spikesPoly.parentEffect = effect;
	effect->spikesPoly.material     = params.spikeMaterial;

	VectorCopy( bounds[0], effect->spikesPoly.cullMins );
	VectorCopy( bounds[1], effect->spikesPoly.cullMaxs );
	effect->spikesPoly.cullMins[3] = 0.0f, effect->spikesPoly.cullMaxs[3] = 1.0f;

	effect->flarePoly.parentEffect  = effect;
	effect->flarePoly.material      = params.flareMaterial;

	const vec3_t halfFlareExtents { maxFinalLength, maxFinalLength, maxFinalLength };
	VectorSubtract( bounds[0], halfFlareExtents, effect->flarePoly.cullMins );
	VectorAdd( bounds[1], halfFlareExtents, effect->flarePoly.cullMaxs );
	effect->flarePoly.cullMins[3] = 0.0f, effect->flarePoly.cullMaxs[3] = 1.0f;

	wsw::link( effect, &m_impactRosetteEffectsHead );
}

auto PolyEffectsSystem::ImpactRosetteSpikesPoly::getStorageRequirements( const float *, const float *, float, float,
																		 unsigned, void *, unsigned ) const
	-> std::optional<std::pair<unsigned, unsigned>> {
    return std::make_pair( 4 * parentEffect->numElements, 6 * parentEffect->numElements );
}

auto PolyEffectsSystem::ImpactRosetteSpikesPoly::fillMeshBuffers( const float *__restrict viewOrigin,
																  const float *__restrict viewAxis,
																  const Scene::DynamicLight *,
																  std::span<const uint16_t>,
																  void *,
																  vec4_t *__restrict positions,
																  vec4_t *__restrict normals,
																  vec2_t *__restrict texCoords,
																  byte_vec4_t *__restrict colors,
																  uint16_t *__restrict indices ) const
	-> std::pair<unsigned, unsigned> {
	assert( parentEffect->numElements > 0 );

	unsigned numAddedVertices = 0;
	unsigned numAddedIndices  = 0;

	unsigned spikeNum = 0;
	do {
		const ImpactRosetteElement &element = parentEffect->elements[spikeNum];
		const float distance = wsw::min( element.lengthLimit, element.desiredLength * element.lifetimeFrac );

		vec3_t endOrigin;
		assert( std::fabs( VectorLengthFast( element.dir ) - 1.0f ) < 0.1f );
		VectorMA( element.from, distance, element.dir, endOrigin );

		// Looking at "bad" angles is not uncommon. Compute the right vector for both points.
		// TODO: We don't need to care of this for lods.
		vec3_t viewToStart, viewToEnd;
		vec3_t startRight, endRight;
		VectorSubtract( element.from, viewOrigin, viewToStart );
		CrossProduct( viewToStart, element.dir, startRight );
		VectorSubtract( endOrigin, viewOrigin, viewToEnd );
		CrossProduct( viewToEnd, element.dir, endRight );

		const float squareStartRightLength = VectorLengthSquared( startRight );
		const float squareEndRightLength   = VectorLengthSquared( endRight );
		// Both conditions are uncommon
		if( squareStartRightLength < wsw::square( 0.001f ) || squareEndRightLength < wsw::square( 0.001f ) ) [[unlikely]] {
			continue;
		}

		const float rcpStartRightLength = Q_RSqrt( squareStartRightLength );
		const float rcpEndRightLength   = Q_RSqrt( squareEndRightLength );
		VectorScale( startRight, rcpStartRightLength, startRight );
		VectorScale( endRight, rcpEndRightLength, endRight );

		const float halfWidth = 0.5f * element.width * element.lifetimeFrac;

		VectorMA( element.from, +halfWidth, startRight, positions[numAddedVertices + 0] );
		VectorMA( element.from, -halfWidth, startRight, positions[numAddedVertices + 1] );
		VectorMA( endOrigin, -halfWidth, endRight, positions[numAddedVertices + 2] );
		VectorMA( endOrigin, +halfWidth, endRight, positions[numAddedVertices + 3] );

		positions[numAddedVertices + 0][3] = positions[numAddedVertices + 1][3] = 1.0f;
		positions[numAddedVertices + 2][3] = positions[numAddedVertices + 3][3] = 1.0f;

		vec4_t startColor, endColor;
		parentEffect->startColorLifespan.getColorForLifetimeFrac( element.lifetimeFrac, startColor );
		parentEffect->endColorLifespan.getColorForLifetimeFrac( element.lifetimeFrac, endColor );

		for( unsigned j = 0; j < 4; ++j ) {
			colors[numAddedVertices + 0][j] = colors[numAddedVertices + 1][j] = (uint8_t)( startColor[j] * 255 );
			colors[numAddedVertices + 2][j] = colors[numAddedVertices + 3][j] = (uint8_t)( endColor[j] * 255 );
		}

		Vector2Set( texCoords[numAddedVertices + 0], 0.0f, 0.0f );
		Vector2Set( texCoords[numAddedVertices + 1], 0.0f, 1.0f );
		Vector2Set( texCoords[numAddedVertices + 2], 1.0f, 1.0f );
		Vector2Set( texCoords[numAddedVertices + 3], 1.0f, 0.0f );

		VectorSet( indices + numAddedIndices + 0, numAddedVertices + 0, numAddedVertices + 1, numAddedVertices + 2 );
		VectorSet( indices + numAddedIndices + 3, numAddedVertices + 0, numAddedVertices + 2, numAddedVertices + 3 );

		numAddedVertices += 4;
		numAddedIndices  += 6;
	} while( ++spikeNum < parentEffect->numElements );

	return { numAddedVertices, numAddedIndices };
}

auto PolyEffectsSystem::ImpactRosetteFlarePoly::getStorageRequirements( const float *, const float *, float, float,
																		unsigned, void *, unsigned ) const
	-> std::optional<std::pair<unsigned, unsigned>> {
	return std::make_pair( 4 * parentEffect->numElements, 6 * parentEffect->numElements );
}

auto PolyEffectsSystem::ImpactRosetteFlarePoly::fillMeshBuffers( const float *__restrict viewOrigin,
																 const float *__restrict viewAxis,
																 const Scene::DynamicLight *,
																 std::span<const uint16_t>,
																 void *,
																 vec4_t *__restrict positions,
																 vec4_t *__restrict normals,
																 vec2_t *__restrict texCoords,
																 byte_vec4_t *__restrict colors,
																 uint16_t *__restrict indices ) const
-> std::pair<unsigned, unsigned> {
	assert( parentEffect->numFlareElementsThisFrame <= parentEffect->numElements );
	assert( parentEffect->numFlareElementsThisFrame > 0 );

	alignas( 16 ) vec4_t left, up;
	// TODO: Flip if needed
	VectorCopy( &viewAxis[AXIS_RIGHT], left );
	VectorCopy( &viewAxis[AXIS_UP], up );

	unsigned numAddedVertices = 0;
	unsigned numAddedIndices  = 0;

	unsigned flareNum = 0;
	do {
		assert( parentEffect->flareElementIndices[flareNum] < parentEffect->numElements );
		const ImpactRosetteElement &element = parentEffect->elements[parentEffect->flareElementIndices[flareNum]];
		// This radius value puts the centerPoint at the end position
		const float radius = wsw::min( element.lengthLimit, element.desiredLength * element.lifetimeFrac );

		vec3_t centerPoint, viewToCenterPoint, right;
		assert( std::fabs( VectorLengthFast( element.dir ) - 1.0f ) < 0.1f );
		VectorMA( element.from, radius, element.dir, centerPoint );

		VectorSubtract( centerPoint, viewOrigin, viewToCenterPoint );
		CrossProduct( viewToCenterPoint, element.dir, right );
		const float squareRightLength = VectorLengthSquared( right );
		if( squareRightLength < wsw::square( 0.001f ) ) [[unlikely]] {
			continue;
		}

		vec4_t interpolatedColor;
		parentEffect->flareColorLifespan.getColorForLifetimeFrac( element.lifetimeFrac, interpolatedColor );

		byte_vec4_t resultingColor;
		Vector4Scale( interpolatedColor, 255.0f, resultingColor );

		Vector4Copy( resultingColor, colors[numAddedVertices + 0] );
		Vector4Copy( resultingColor, colors[numAddedVertices + 1] );
		Vector4Copy( resultingColor, colors[numAddedVertices + 2] );
		Vector4Copy( resultingColor, colors[numAddedVertices + 3] );

		vec3_t point;
		VectorMA( centerPoint, -radius, up, point );
		VectorMA( point, +radius, left, positions[numAddedVertices + 0] );
		VectorMA( point, -radius, left, positions[numAddedVertices + 3] );

		VectorMA( centerPoint, +radius, up, point );
		VectorMA( point, +radius, left, positions[numAddedVertices + 1] );
		VectorMA( point, -radius, left, positions[numAddedVertices + 2] );

		positions[numAddedVertices + 0][3] = positions[numAddedVertices + 1][3] = 1.0f;
		positions[numAddedVertices + 2][3] = positions[numAddedVertices + 3][3] = 1.0f;

		Vector2Set( texCoords[numAddedVertices + 0], 0.0f, 1.0f );
		Vector2Set( texCoords[numAddedVertices + 1], 0.0f, 0.0f );
		Vector2Set( texCoords[numAddedVertices + 2], 1.0f, 0.0f );
		Vector2Set( texCoords[numAddedVertices + 3], 1.0f, 1.0f );

		VectorSet( indices + numAddedIndices + 0, numAddedVertices + 0, numAddedVertices + 1, numAddedVertices + 2 );
		VectorSet( indices + numAddedIndices + 3, numAddedVertices + 0, numAddedVertices + 2, numAddedVertices + 3 );

		numAddedVertices += 4;
		numAddedIndices  += 6;
	} while( ++flareNum < parentEffect->numFlareElementsThisFrame );

	return { numAddedVertices, numAddedIndices };
}

void PolyEffectsSystem::spawnSimulatedRing( SimulatedRingParams &&params ) {
	void *mem = m_ribbonEffectsAllocator.allocOrNull();
	if( !mem ) [[unlikely]] {
		assert( m_ribbonEffectsHead );
		// TODO: Make retrieval of oldest elements from allocators possible
		RibbonEffect *oldestEffect = m_ribbonEffectsHead->next;
		while( oldestEffect ) {
			if( oldestEffect->next ) {
				oldestEffect = oldestEffect->next;
			} else {
				break;
			}
		}
		wsw::unlink( oldestEffect, &m_ribbonEffectsHead );
		oldestEffect->~RibbonEffect();
		mem = oldestEffect;
	}

	auto *const __restrict effect   = new( mem )RibbonEffect;
	effect->spawnTime               = m_lastTime;
	effect->lifetime                = params.lifetime;
	effect->simulationDelay         = params.simulationDelay;
	effect->movementDuration        = params.movementDuration;
	effect->numEdges                = kMaxRibbonEdges;
	effect->numClipMoveSmoothSteps  = params.numClipMoveSmoothSteps;
	effect->numClipAlphaSmoothSteps = params.numClipAlphaSmoothSteps;
	effect->alphaLifespan           = params.alphaLifespan;
	effect->isLooped                = true;
	effect->softenOnContact         = params.softenOnContact;

	effect->poly.parentEffect = effect;
	effect->poly.material     = params.material;

	const float minInnerSpeed = wsw::max( 0.0f, params.innerSpeed.mean - 0.5f * params.innerSpeed.spread );
	const float maxInnerSpeed = params.innerSpeed.mean + 0.5f * params.innerSpeed.spread;

	const float minOuterSpeed = wsw::max( 0.0f, params.outerSpeed.mean - 0.5f * params.outerSpeed.spread );
	const float maxOuterSpeed = params.outerSpeed.mean + 0.5f * params.outerSpeed.spread;

	vec3_t axis1, axis2;
	PerpendicularVector( axis2, params.axisDir );
	CrossProduct( params.axisDir, axis2, axis1 );
	VectorNormalizeFast( axis1 ), VectorNormalizeFast( axis2 );

	vec3_t origin;
	VectorAdd( params.origin, params.offset, origin );

	float angle = 0.0f;
	const float angleStep = (float)M_TWOPI * Q_Rcp( (float)effect->numEdges );
	for( unsigned edgeNum = 0; edgeNum < effect->numEdges; ++edgeNum ) {
		// Hiding latency/breaking dependency chain
		const float outerSpeed = m_rng.nextFloat( minOuterSpeed, maxOuterSpeed );
		const float sinPart    = std::sin( angle ), cosPart = std::cos( angle );
		const float innerSpeed = wsw::min( outerSpeed, m_rng.nextFloat( minInnerSpeed, maxInnerSpeed ) );

		float *const __restrict innerVelocity = effect->innerVelocities[edgeNum];
		float *const __restrict outerVelocity = effect->outerVelocities[edgeNum];

		VectorClear( innerVelocity );
		VectorMA( innerVelocity, sinPart, axis1, innerVelocity );
		VectorMA( innerVelocity, cosPart, axis2, innerVelocity );
		VectorScale( innerVelocity, innerSpeed, innerVelocity );

		VectorClear( outerVelocity );
		VectorMA( outerVelocity, sinPart, axis1, outerVelocity );
		VectorMA( outerVelocity, cosPart, axis2, outerVelocity );
		VectorScale( outerVelocity, outerSpeed, outerVelocity );

		VectorCopy( origin, effect->innerPositions[edgeNum] );
		VectorCopy( origin, effect->outerPositions[edgeNum] );

		effect->innerTexCoords[edgeNum][0] = 0.5f + 0.5f * params.innerTexCoordFrac * sinPart;
		effect->innerTexCoords[edgeNum][1] = 0.5f + 0.5f * params.innerTexCoordFrac * cosPart;

		effect->outerTexCoords[edgeNum][0] = 0.5f + 0.5f * params.outerTexCoordFrac * sinPart;
		effect->outerTexCoords[edgeNum][1] = 0.5f + 0.5f * params.outerTexCoordFrac * cosPart;

		angle += angleStep;
	}

	Vector4Set( effect->poly.cullMins, params.origin[0] - 0.1f, params.origin[1] - 0.1f, params.origin[2] - 0.1f, 0.0f );
	Vector4Set( effect->poly.cullMaxs, params.origin[0] + 0.1f, params.origin[1] + 0.1f, params.origin[2] + 0.1f, 1.0f );

	wsw::link( effect, &m_ribbonEffectsHead );
}

auto PolyEffectsSystem::RibbonPoly::getStorageRequirements( const float *, const float *, float, float,
															unsigned, void *, unsigned ) const
	-> std::optional<std::pair<unsigned, unsigned>> {
	assert( parentEffect->numEdges > 1 );
	// TODO: Use strips
	const unsigned numQuads = parentEffect->numEdges;
	return std::make_pair( 4 * numQuads, 6 * numQuads );
}

auto PolyEffectsSystem::RibbonPoly::fillMeshBuffers( const float *__restrict viewOrigin,
													 const float *__restrict viewAxis,
													 const Scene::DynamicLight *,
													 std::span<const uint16_t>,
													 void *,
													 vec4_t *__restrict positions,
													 vec4_t *__restrict normals,
													 vec2_t *__restrict texCoords,
													 byte_vec4_t *__restrict colors,
													 uint16_t *__restrict indices ) const
	-> std::pair<unsigned, unsigned> {
	assert( parentEffect->numEdges > 1 );
	// TODO: Can we skip some vertices for far lods?

	const vec3_t *const __restrict innerPositions   = parentEffect->innerPositions;
	const vec3_t *const __restrict outerPositions   = parentEffect->outerPositions;
	const vec2_t *const __restrict innerTexCoords   = parentEffect->innerTexCoords;
	const vec2_t *const __restrict outerTexCoords   = parentEffect->outerTexCoords;

	[[maybe_unused]] const float *const __restrict innerContactAlpha = parentEffect->innerContactAlpha;
	[[maybe_unused]] const float *const __restrict outerContactAlpha = parentEffect->outerContactAlpha;

	const float globalAlphaScale                = 255.0f * parentEffect->alphaLifespan.getValueForLifetimeFrac( lifetimeFrac );
	const bool softenOnContact                  = parentEffect->softenOnContact;
	[[maybe_unused]] const auto globalAlphaByte = (uint8_t)globalAlphaScale;

	unsigned numVertices = 0;
	unsigned numIndices  = 0;

	unsigned prevEdgeNum = 0;
	unsigned currEdgeNum = 1;
	do {
		VectorCopy( innerPositions[prevEdgeNum], positions[0] );
		VectorCopy( outerPositions[prevEdgeNum], positions[1] );
		VectorCopy( outerPositions[currEdgeNum], positions[2] );
		VectorCopy( innerPositions[currEdgeNum], positions[3] );
		positions[0][3] = positions[1][3] = positions[2][3] = positions[3][3] = 1.0f;

		if( softenOnContact ) {
			Vector4Set( colors[0], 255, 255, 255, (uint8_t)( globalAlphaScale * innerContactAlpha[prevEdgeNum] ) );
			Vector4Set( colors[1], 255, 255, 255, (uint8_t)( globalAlphaScale * outerContactAlpha[prevEdgeNum] ) );
			Vector4Set( colors[2], 255, 255, 255, (uint8_t)( globalAlphaScale * outerContactAlpha[currEdgeNum] ) );
			Vector4Set( colors[3], 255, 255, 255, (uint8_t)( globalAlphaScale * innerContactAlpha[currEdgeNum] ) );
		} else {
			Vector4Set( colors[0], 255, 255, 255, globalAlphaByte );
			Vector4Set( colors[1], 255, 255, 255, globalAlphaByte );
			Vector4Set( colors[2], 255, 255, 255, globalAlphaByte );
			Vector4Set( colors[3], 255, 255, 255, globalAlphaByte );
		}

		Vector2Copy( innerTexCoords[prevEdgeNum], texCoords[0] );
		Vector2Copy( outerTexCoords[prevEdgeNum], texCoords[1] );
		Vector2Copy( outerTexCoords[currEdgeNum], texCoords[2] );
		Vector2Copy( innerTexCoords[currEdgeNum], texCoords[3] );

		VectorSet( indices + 0, numVertices + 0, numVertices + 1, numVertices + 2 );
		VectorSet( indices + 3, numVertices + 0, numVertices + 2, numVertices + 3 );

		positions += 4, colors += 4, texCoords += 4, numVertices += 4;
		indices += 6, numIndices += 6;

		prevEdgeNum = currEdgeNum;
		++currEdgeNum;
	} while( currEdgeNum < parentEffect->numEdges );

	if( parentEffect->isLooped ) {
		// TODO: Discover how to handle this case within the loop
		currEdgeNum = 0;

		VectorCopy( innerPositions[prevEdgeNum], positions[0] );
		VectorCopy( outerPositions[prevEdgeNum], positions[1] );
		VectorCopy( outerPositions[currEdgeNum], positions[2] );
		VectorCopy( innerPositions[currEdgeNum], positions[3] );
		positions[0][3] = positions[1][3] = positions[2][3] = positions[3][3] = 1.0f;

		if( softenOnContact ) {
			Vector4Set( colors[0], 255, 255, 255, (uint8_t)( globalAlphaScale * innerContactAlpha[prevEdgeNum] ) );
			Vector4Set( colors[1], 255, 255, 255, (uint8_t)( globalAlphaScale * outerContactAlpha[prevEdgeNum] ) );
			Vector4Set( colors[2], 255, 255, 255, (uint8_t)( globalAlphaScale * outerContactAlpha[currEdgeNum] ) );
			Vector4Set( colors[3], 255, 255, 255, (uint8_t)( globalAlphaScale * innerContactAlpha[currEdgeNum] ) );
		} else {
			Vector4Set( colors[0], 255, 255, 255, globalAlphaByte );
			Vector4Set( colors[1], 255, 255, 255, globalAlphaByte );
			Vector4Set( colors[2], 255, 255, 255, globalAlphaByte );
			Vector4Set( colors[3], 255, 255, 255, globalAlphaByte );
		}

		Vector2Copy( innerTexCoords[prevEdgeNum], texCoords[0] );
		Vector2Copy( outerTexCoords[prevEdgeNum], texCoords[1] );
		Vector2Copy( outerTexCoords[currEdgeNum], texCoords[2] );
		Vector2Copy( innerTexCoords[currEdgeNum], texCoords[3] );

		VectorSet( indices + 0, numVertices + 0, numVertices + 1, numVertices + 2 );
		VectorSet( indices + 3, numVertices + 0, numVertices + 2, numVertices + 3 );

		numVertices += 4, numIndices += 6;
	}

	return { numVertices, numIndices };
}

void PolyEffectsSystem::simulateFrame( int64_t currTime ) {
	WSW_PROFILER_SCOPE();

	if( currTime != m_lastTime ) {
		assert( currTime > m_lastTime );

		const float timeDeltaSeconds = 1e-3f * std::min<float>( 33, (float)( currTime - m_lastTime ) );

		simulateBeams( currTime, timeDeltaSeconds );
		simulateTracers( currTime, timeDeltaSeconds );
		simulateRosettes( currTime, timeDeltaSeconds );
		simulateRibbons( currTime, timeDeltaSeconds );

		m_lastTime = currTime;
	}
}

void PolyEffectsSystem::submitToScene( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask ) {
	submitBeams( currTime, request, povPlayerMask );
	submitTracers( currTime, request, povPlayerMask );
	submitRosettes( currTime, request );
	submitRibbons( currTime, request );
}

void PolyEffectsSystem::simulateBeams( int64_t currTime, float ) {
	for( TransientBeamEffect *beam = m_transientBeamsHead, *next = nullptr; beam; beam = next ) { next = beam->next;
		if( beam->spawnTime + beam->timeout <= currTime ) [[unlikely]] {
			destroyTransientBeamEffect( beam );
		} else {
			beam->poly.animFrac = (float)( currTime - beam->spawnTime ) * Q_Rcp( (float)beam->timeout );
		}
	}
}

void PolyEffectsSystem::submitBeams( int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask ) {
	assert( currTime == m_lastTime );

	for( CurvedBeamEffect *beam = m_curvedLaserBeamsHead; beam; beam = beam->next ) {
		if( beam->povPlayerMask & povPlayerMask ) {
			if( beam->poly.material && beam->poly.points.size() > 1 ) [[likely]] {
				request->addDynamicMesh( &beam->poly );
			}
		}
	}

	for( StraightBeamEffect *beam = m_straightLaserBeamsHead; beam; beam = beam->next ) {
		if( beam->povPlayerMask & povPlayerMask ) {
			if( beam->poly.material && beam->poly.halfExtent > 1.0f ) [[likely]] {
				request->addPoly( &beam->poly );
			}
		}
	}

	for( TransientBeamEffect *beam = m_transientBeamsHead; beam; beam = beam->next ) {
		assert( beam->spawnTime + beam->timeout > currTime );
		if( beam->poly.material && beam->poly.halfExtent > 1.0f ) [[likely]] {
			const float colorLifetimeFrac = (float)( currTime - beam->spawnTime ) * Q_Rcp( (float)beam->timeout );

			vec4_t color;
			beam->colorLifespan.getColorForLifetimeFrac( colorLifetimeFrac, color );
			auto *const rules = std::get_if<QuadPoly::ViewAlignedBeamRules>( &beam->poly.appearanceRules );
			Vector4Copy( color, rules->fromColor );
			Vector4Copy( color, rules->toColor );

			if( beam->lightProps ) {
				const auto &[lightTimeout, lightLifespan] = *beam->lightProps;
				if( beam->spawnTime + lightTimeout > currTime ) {
					const float lightLifetimeFrac = (float)( currTime - beam->spawnTime ) * Q_Rcp( (float)lightTimeout );
					assert( lightLifetimeFrac >= 0.0f && lightLifetimeFrac < 1.01f );

					float lightRadius, lightColor[3];
					lightLifespan.getRadiusAndColorForLifetimeFrac( lightLifetimeFrac, &lightRadius, lightColor );
					if( lightRadius > 1.0f ) {
						float lightOrigin[3];

						// The lifetime fraction is in [0, 1] range.
						// The position parameter is in [-1, +1] range.
						// The value -1 gives the "from" point, the value +1 gives the "to" point.
						const float positionParam = 2.0f * lightLifetimeFrac - 1.0f;
						VectorMA( beam->poly.origin, beam->poly.halfExtent * positionParam, rules->dir, lightOrigin );
						request->addLight( lightOrigin, lightRadius, lightRadius, lightColor );
					}
				}
			}

			request->addPoly( &beam->poly );
		}
	}
}

void PolyEffectsSystem::simulateTracers( int64_t currTime, float timeDeltaSeconds ) {
	for( TracerEffect *tracer = m_tracerEffectsHead, *next = nullptr; tracer; tracer = next ) { next = tracer->next;
		if( tracer->spawnTime + tracer->timeout <= currTime ) [[unlikely]] {
			destroyTracerEffect( tracer );
			continue;
		}

		assert( tracer->poly.halfExtent >= 0.5f );

		tracer->distanceSoFar += tracer->speed * timeDeltaSeconds;

		const float distanceOfClosestPoint = tracer->distanceSoFar - tracer->poly.halfExtent;
		if( distanceOfClosestPoint <= tracer->prestepDistance ) [[unlikely]] {
			// Hide it for now
			continue;
		}

		// Don't let the poly penetrate the target position
		const float distanceOfFarthestPoint = tracer->distanceSoFar + tracer->poly.halfExtent;
		if( distanceOfFarthestPoint >= tracer->totalDistance ) [[unlikely]] {
			destroyTracerEffect( tracer );
			continue;
		}

		auto *const rules   = std::get_if<QuadPoly::ViewAlignedBeamRules>( &tracer->poly.appearanceRules );
		rules->toColor[3]   = tracer->initialColorAlpha;
		rules->fromColor[3] = tracer->initialColorAlpha;

		// Soften edges

		float softenClosestEdgeFrac = 1.0f;
		if( distanceOfClosestPoint < tracer->smoothEdgeDistance ) {
			softenClosestEdgeFrac = distanceOfClosestPoint * Q_Rcp( tracer->smoothEdgeDistance );
			rules->fromColor[3] *= softenClosestEdgeFrac;
		}

		const float distanceToImpactPoint = tracer->totalDistance - distanceOfFarthestPoint;
		if( distanceToImpactPoint < tracer->smoothEdgeDistance ) {
			const float softenFarthestEdgeFrac = distanceToImpactPoint * Q_Rcp( tracer->smoothEdgeDistance );
			rules->toColor[3] *= softenFarthestEdgeFrac;
			// Smooth the closest edge as well in this case, if we did not do that yet.
			// Choose the fraction of the farthest edge over the closest one.
			if( softenFarthestEdgeFrac > softenClosestEdgeFrac ) {
				rules->fromColor[3] = tracer->initialColorAlpha * softenFarthestEdgeFrac;
			}
		}

		bool hasAlignedForPov = false;
		// If we should align
		if( const std::optional<TracerParams::AlignForPovParams> &alignForPovParams = tracer->alignForPovParams ) {
			// TODO: Make sure it's stable (check some kind of spawn ids)
			if( const ViewState *viewState = getViewStateForEntity( (int)alignForPovParams->povNum ) ) {
				// If we're really following the initial POV
				// TODO: Detach if teleported
				if( alignForPovParams->povNum == viewState->predictedPlayerState.POVnum ) {
					vec3_t actualViewOrigin;
					VectorCopy( viewState->predictedPlayerState.pmove.origin, actualViewOrigin );
					actualViewOrigin[2] += viewState->predictedPlayerState.viewheight;
					const float *const actualViewAngles = viewState->predictedPlayerState.viewangles;

					// Pin the farthest point at it's regular origin and align the free tail towards the viewer
					// TODO: Think of creating a separate kind of QuadPoly::AppearanceRules for this case?
					vec3_t farthestPoint;
					VectorMA( tracer->from, distanceOfFarthestPoint, tracer->dir, farthestPoint );

					const float squareDistanceToActualViewOrigin = DistanceSquared( farthestPoint, actualViewOrigin );
					// If we can normalize the new direction vector
					if( squareDistanceToActualViewOrigin > wsw::square( 0.1f ) ) [[likely]] {
						const float rcpDistanceToActualViewOrigin = Q_RSqrt( squareDistanceToActualViewOrigin );

						vec3_t actualViewOriginToTracerDir;
						VectorSubtract( farthestPoint, actualViewOrigin, actualViewOriginToTracerDir );
						VectorScale( actualViewOriginToTracerDir, rcpDistanceToActualViewOrigin, actualViewOriginToTracerDir );

						assert( std::fabs( VectorLengthFast( tracer->dir ) - 1.0f ) < 0.1f );
						assert( std::fabs( VectorLengthFast( actualViewOriginToTracerDir ) - 1.0f ) < 0.1f );

						vec3_t actualViewForward, actualViewRight, actualViewUp;
						AngleVectors( actualViewAngles, actualViewForward, actualViewRight, actualViewUp );

						vec3_t shiftedViewOrigin;
						VectorCopy( actualViewOrigin, shiftedViewOrigin );
						const float *axisProjections = alignForPovParams->offsetProjectionsOntoViewAxis;
						VectorMA( shiftedViewOrigin, axisProjections[0], actualViewForward, shiftedViewOrigin );
						VectorMA( shiftedViewOrigin, axisProjections[1], actualViewRight, shiftedViewOrigin );
						VectorMA( shiftedViewOrigin, axisProjections[2], actualViewUp, shiftedViewOrigin );

						const float squareDistanceToShiftedViewOrigin = DistanceSquared( farthestPoint, shiftedViewOrigin );
						if( squareDistanceToShiftedViewOrigin > wsw::square( 0.1f ) ) [[likely]] {
							const float rcpDistanceToShiftedViewOrigin = Q_RSqrt( squareDistanceToShiftedViewOrigin );

							// Modify the poly dir
							VectorSubtract( farthestPoint, shiftedViewOrigin, rules->dir );
							VectorScale( rules->dir, rcpDistanceToShiftedViewOrigin, rules->dir );

							// Modify the origin
							VectorMA( farthestPoint, -tracer->poly.halfExtent, rules->dir, tracer->poly.origin );

							hasAlignedForPov = true;
						}
					}
				}
			}
		}

		// Default path
		if( !hasAlignedForPov ) {
			assert( std::fabs( VectorLengthFast( tracer->dir ) - 1.0f ) < 1e-3f );
			VectorMA( tracer->from, tracer->distanceSoFar, tracer->dir, tracer->poly.origin );
			// Restore the default alignment upon the POV switch (if any)
			VectorCopy( tracer->dir, rules->dir );
		}

		assert( std::fabs( VectorLengthFast( rules->dir ) - 1.0f ) < 1e-3f );

		tracer->selectedForSubmissionAt = currTime;
		tracer->poly.animFrac           = (float)( currTime - tracer->spawnTime ) * Q_Rcp( (float)tracer->timeout );
	}
}

void PolyEffectsSystem::submitTracers( [[maybe_unused]] int64_t currTime, DrawSceneRequest *request, unsigned povPlayerMask ) {
	assert( currTime == m_lastTime );

	for( TracerEffect *tracer = m_tracerEffectsHead; tracer; tracer = tracer->next ) {
		// Don't submit tracers which are considered hidden on this frame
		if( ( tracer->povPlayerMask & povPlayerMask ) && tracer->selectedForSubmissionAt == currTime ) {
			request->addPoly( &tracer->poly );

			if( tracer->programLightRadius > 0.0f || tracer->coronaLightRadius > 0.0f ) {
				bool shouldAddLight = true;
				// If the light display is tied to certain frames (e.g., every 3rd one, starting from 2nd absolute)
				if( const auto modulo = (unsigned)tracer->lightFrameAffinityModulo; modulo > 1 ) {
					using CountType = decltype( cg.frameCount );
					const auto frameIndexByModulo = cg.frameCount % (CountType) modulo;
					shouldAddLight = frameIndexByModulo == (CountType)tracer->lightFrameAffinityIndex;
				}
				if( shouldAddLight ) {
					float radiusFrac = 1.0f;
					if( tracer->lightFadeInDistance > 0.0f ) {
						if( tracer->distanceSoFar < tracer->lightFadeInDistance ) {
							radiusFrac = tracer->distanceSoFar * Q_Rcp( tracer->lightFadeInDistance );
						}
					} else if( tracer->lightFadeOutDistance > 0.0f ) {
						const float distanceLeft = tracer->totalDistance - tracer->distanceSoFar;
						if( distanceLeft < tracer->lightFadeOutDistance ) {
							radiusFrac = distanceLeft * Q_Rcp( tracer->lightFadeOutDistance );
						}
					}
					assert( radiusFrac >= 0.0f && radiusFrac < 1.01f );
					const float programRadius = radiusFrac * tracer->programLightRadius;
					const float coronaRadius  = radiusFrac * tracer->coronaLightRadius;
					if( programRadius > 1.0f || coronaRadius > 1.0f ) {
						request->addLight( tracer->poly.origin, programRadius, coronaRadius, tracer->lightColor );
					}
				}
			}
		}
	}
}

void PolyEffectsSystem::simulateRosettes( int64_t currTime, float timeDeltaSeconds ) {
	for( ImpactRosetteEffect *effect = m_impactRosetteEffectsHead, *next = nullptr; effect; effect = next ) {
		next = effect->next;

		bool mayAddFlaresThisFrame = false;
		if( effect->effectFlareFrameAffinityModulo < 2 ) {
			mayAddFlaresThisFrame = true;
		} else {
			assert( effect->effectFlareFrameAffinityIndex < effect->effectFlareFrameAffinityModulo );
			if( ( cg.frameCount % effect->effectFlareFrameAffinityModulo ) == effect->effectFlareFrameAffinityIndex ) {
				mayAddFlaresThisFrame = true;
			}
		}

		effect->numFlareElementsThisFrame = 0;

		// Check lifetime of each spike
		for( unsigned i = 0; i < effect->numElements; ) {
			const int64_t lifetimeMillisLeft = effect->spawnTime + effect->elements[i].lifetime - currTime;
			if( lifetimeMillisLeft > 0 ) [[likely]] {
				const float rcpLifetime          = Q_Rcp( (float)effect->elements[i].lifetime );
				effect->elements[i].lifetimeFrac = 1.0f - (float)lifetimeMillisLeft * rcpLifetime;
				assert( effect->elements[i].lifetimeFrac >= 0.0f && effect->elements[i].lifetimeFrac <= 1.0f );

				bool shouldAddFlareForThisElement = false;
				if( mayAddFlaresThisFrame ) {
					const unsigned modulo = effect->elementFlareFrameAffinityModulo;
					if( modulo < 2 ) {
						shouldAddFlareForThisElement = true;
					} else {
						// TODO: Optimize division, force a 32-bit division at least
						if( ( cg.frameCount % modulo ) == ( i % modulo ) ) {
							shouldAddFlareForThisElement = true;
						}
					}
				}

				if( shouldAddFlareForThisElement ) {
					effect->flareElementIndices[effect->numFlareElementsThisFrame++] = i;
				}

				++i;
			} else {
				effect->numElements--;
				effect->elements[i] = effect->elements[effect->numElements];
			}
		}

		if( !effect->numElements ) [[unlikely]] {
			destroyImpactRosetteEffect( effect );
		}
	}
}

void PolyEffectsSystem::submitRosettes( int64_t currTime, DrawSceneRequest *request ) {
	assert( currTime == m_lastTime );

	for( ImpactRosetteEffect *effect = m_impactRosetteEffectsHead; effect; effect = effect->next ) {
		assert( effect->numElements );
		request->addDynamicMesh( &effect->spikesPoly );
		if( effect->numFlareElementsThisFrame ) {
			request->addDynamicMesh( &effect->flarePoly );
		}
		if( const std::optional<LightLifespan> &lightLifespan = effect->lightLifespan ) {
			bool shouldAddLight = false;
			if( effect->lightFrameAffinityModulo < 2 ) {
				shouldAddLight = true;
			} else {
				assert( effect->lightFrameAffinityIndex < effect->lightFrameAffinityModulo );
				if( ( cg.frameCount % effect->lightFrameAffinityModulo ) == effect->lightFrameAffinityIndex ) {
					shouldAddLight = true;
				}
			}
			if( shouldAddLight ) {
				const float effectLifetimeFrac = (float)( currTime - effect->spawnTime ) * Q_Rcp( (float)effect->lifetime );
				assert( effectLifetimeFrac >= 0.0f && effectLifetimeFrac <= 1.0f );

				float lightRadius = 0.0f, lightColor[3];
				lightLifespan->getRadiusAndColorForLifetimeFrac( effectLifetimeFrac, &lightRadius, lightColor );

				if( lightRadius > 1.0f ) [[likely]] {
					effect->lastLightEmitterElementIndex = ( effect->lastLightEmitterElementIndex + 1 ) % effect->numElements;
					const ImpactRosetteElement &element  = effect->elements[effect->lastLightEmitterElementIndex];

					vec3_t lightOrigin;
					const float endPointDistance = wsw::min( element.lengthLimit, element.desiredLength * element.lifetimeFrac );
					// Prevent spawning the light in an obstacle
					const float lightOriginDistance = wsw::max( 0.0f, endPointDistance - 1.0f );
					VectorMA( element.from, lightOriginDistance, element.dir, lightOrigin );
					request->addLight( lightOrigin, lightRadius, 0.0f, lightColor );
				}
			}
		}
	}
}

void PolyEffectsSystem::simulateRibbons( int64_t currTime, float timeDeltaSeconds ) {
	for( RibbonEffect *ribbon = m_ribbonEffectsHead, *next = nullptr; ribbon; ribbon = next ) { next = ribbon->next;
		if( ribbon->spawnTime + ribbon->lifetime <= currTime ) [[unlikely]] {
			destroyRibbonEffect( ribbon );
		} else {
			const int64_t startSimulationAt = ribbon->spawnTime + ribbon->simulationDelay;
			if( startSimulationAt <= currTime ) [[likely]] {
				const int64_t stopMovingAt = startSimulationAt + ribbon->movementDuration;
				// Hacks: currently, the simulation is the same as movement simulation
				// After stopping moving, we can use the "frozen" state of an object.
				if( currTime < stopMovingAt ) {
					simulateRibbon( ribbon, timeDeltaSeconds );
				}
			}
		}
	}
}

void PolyEffectsSystem::submitRibbons( int64_t currTime, DrawSceneRequest *request ) {
	for( RibbonEffect *ribbon = m_ribbonEffectsHead; ribbon; ribbon = ribbon->next ) {
		const int64_t startSimulationAt = ribbon->spawnTime + ribbon->simulationDelay;
		if( startSimulationAt <= currTime ) {
			if( DistanceSquared( ribbon->poly.cullMins, ribbon->poly.cullMaxs ) > wsw::square( 1.0f ) ) [[likely]] {
				assert( ribbon->lifetime > ribbon->simulationDelay );
				const int64_t lifetimeSoFar  = currTime - startSimulationAt;
				const int64_t visualLifetime = ribbon->lifetime - ribbon->simulationDelay;
				ribbon->poly.lifetimeFrac    = (float)( lifetimeSoFar ) * Q_Rcp( (float)visualLifetime );
				assert( ribbon->poly.lifetimeFrac >= 0.0f && ribbon->poly.lifetimeFrac <= 1.0f );
				request->addDynamicMesh( &ribbon->poly );
			}
		}
	}
}

void PolyEffectsSystem::simulateRibbon( RibbonEffect *__restrict ribbon, float timeDeltaSeconds ) {
	assert( ribbon->numEdges > 1 && ribbon->numEdges <= kMaxRibbonEdges );
	const unsigned numEdges = ribbon->numEdges;
	unsigned edgeNum;

	// Make sure we aren't going to blow up stack
	static_assert( kMaxRibbonEdges < 64 );

	vec3_t unclippedInnerPositions[kMaxRibbonEdges], unclippedOuterPositions[kMaxRibbonEdges];

	// Use independent builders to break the major dependency chain
	BoundsBuilder innerBoundsBuilder, outerBoundsBuilder;

	// Compute unclipped positions for this step.
	// Compute the enclosing bounding box along the way.
	edgeNum = 0;
	do {
		VectorMA( ribbon->outerPositions[edgeNum], timeDeltaSeconds, ribbon->outerVelocities[edgeNum], unclippedOuterPositions[edgeNum] );
		VectorMA( ribbon->innerPositions[edgeNum], timeDeltaSeconds, ribbon->innerVelocities[edgeNum], unclippedInnerPositions[edgeNum] );
		innerBoundsBuilder.addPoint( unclippedInnerPositions[edgeNum] );
		outerBoundsBuilder.addPoint( unclippedOuterPositions[edgeNum] );
	} while( ++edgeNum < numEdges );

	// TODO: There should be a method to combine bounds (actually there is but arch-specific)
	vec3_t innerMins, innerMaxs, combinedMins, combinedMaxs;
	innerBoundsBuilder.storeTo( innerMins, innerMaxs );
	outerBoundsBuilder.addPoint( innerMins );
	outerBoundsBuilder.addPoint( innerMaxs );
	outerBoundsBuilder.storeToWithAddedEpsilon( combinedMins, combinedMaxs );

	// TODO: Add a fused call
	CM_BuildShapeList( cl.cms, m_tmpShapeList, combinedMins, combinedMaxs, MASK_SOLID | MASK_WATER );
	CM_ClipShapeList( cl.cms, m_tmpShapeList, m_tmpShapeList, combinedMins, combinedMaxs );

	// Note: The empty shape list optimization can be added but it does not seem to be beneficial for current effects
	// which practically always have some nearby collsion shapes.

	float clipMoveFractions[kMaxRibbonEdges];

	edgeNum = 0;
	// TODO: Use absolute units?
	constexpr float kMoveFracEpsilon = 1e-3f;
	if( ribbon->softenOnContact ) {
		trace_t trace;

		float originalOuterAlphaFractions[kMaxRibbonEdges];
		float originalInnerAlphaFractions[kMaxRibbonEdges];

		do {
			// We have to check both vertices independently for softening
			CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, ribbon->outerPositions[edgeNum],
								unclippedOuterPositions[edgeNum], vec3_origin, vec3_origin, MASK_SOLID | MASK_WATER );
			const float outerFrac = trace.fraction;

			CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, ribbon->innerPositions[edgeNum],
								unclippedInnerPositions[edgeNum], vec3_origin, vec3_origin, MASK_SOLID | MASK_WATER );
			const float innerFrac = trace.fraction;

			originalOuterAlphaFractions[edgeNum] = Q_Sqrt( outerFrac );
			originalInnerAlphaFractions[edgeNum] = Q_Sqrt( innerFrac );

			float moveFrac = 0.0f;
			// Use the same logic as in the other (non-softening case)
			if( outerFrac > kMoveFracEpsilon ) {
				if( innerFrac > kMoveFracEpsilon ) {
					moveFrac = wsw::min( innerFrac, outerFrac );
				}
			}

			clipMoveFractions[edgeNum] = moveFrac;
		} while( ++edgeNum < numEdges );

		float tmpBuffer1[kMaxRibbonEdges], tmpBuffer2[kMaxRibbonEdges];

		const float *innerAlphaFractions = smoothRibbonFractions( originalInnerAlphaFractions, tmpBuffer1, tmpBuffer2, {
			.numEdges = numEdges, .numSteps = ribbon->numClipAlphaSmoothSteps, .isLooped = ribbon->isLooped,
		});
		// tmpBuffer1,2 are safe to reuse after this call
		std::memcpy( ribbon->innerContactAlpha, innerAlphaFractions, sizeof( float ) * numEdges );

		const float *outerFractions = smoothRibbonFractions( originalOuterAlphaFractions, tmpBuffer1, tmpBuffer2, {
			.numEdges = numEdges, .numSteps = ribbon->numClipAlphaSmoothSteps, .isLooped = ribbon->isLooped,
		});
		std::memcpy( ribbon->outerContactAlpha, outerFractions, sizeof( float ) * numEdges );
	} else {
		trace_t trace;
		do {
			float moveFrac = 0.0f;
			// Consider the outer hull to be "dragging" the ring,
			// so if the outer vertex is definitely stopped, don't even bother to check the inner one.
			CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, ribbon->outerPositions[edgeNum],
								unclippedOuterPositions[edgeNum], vec3_origin, vec3_origin, MASK_SOLID | MASK_WATER );
			if( const float outerFrac = trace.fraction; trace.fraction > kMoveFracEpsilon ) {
				CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, ribbon->innerPositions[edgeNum],
									unclippedInnerPositions[edgeNum], vec3_origin, vec3_origin, MASK_SOLID | MASK_WATER );
				if( const float innerFrac = trace.fraction; innerFrac > kMoveFracEpsilon ) {
					moveFrac = wsw::min( innerFrac, outerFrac );
				}
			}

			clipMoveFractions[edgeNum] = moveFrac;
		} while( ++edgeNum < numEdges );
	}

	float buffer1[kMaxRibbonEdges], buffer2[kMaxRibbonEdges];
	const float *const __restrict finalMoveFractions = smoothRibbonFractions( clipMoveFractions, buffer1, buffer2, {
		.numEdges = numEdges, .numSteps = ribbon->numClipMoveSmoothSteps, .isLooped = ribbon->isLooped
	});

	// Compute final positions given smoothed move fractions
	edgeNum = 0;
	do {
		assert( finalMoveFractions[edgeNum] >= 0.0f && finalMoveFractions[edgeNum] <= 1.0f );
		const float frac = timeDeltaSeconds * finalMoveFractions[edgeNum];
		VectorMA( ribbon->outerPositions[edgeNum], frac, ribbon->outerVelocities[edgeNum], ribbon->outerPositions[edgeNum] );
		VectorMA( ribbon->innerPositions[edgeNum], frac, ribbon->innerVelocities[edgeNum], ribbon->innerPositions[edgeNum] );
	} while( ++edgeNum < numEdges );

	// Don't recompute bounds (bounds of non-clipped vertices should be sufficient)
	VectorCopy( combinedMins, ribbon->poly.cullMins );
	VectorCopy( combinedMaxs, ribbon->poly.cullMaxs );
	ribbon->poly.cullMins[3] = 0.0f, ribbon->poly.cullMaxs[3] = 1.0f;
}

auto PolyEffectsSystem::smoothRibbonFractions( float *__restrict original,
											   float *__restrict pingPongBuffer1,
											   float *__restrict pingPongBuffer2,
											   SmoothRibbonParams &&params ) -> float * {
	float *const buffers[2] { pingPongBuffer1, pingPongBuffer2 };

	const unsigned numEdges  = params.numEdges;
	const unsigned numSteps  = params.numSteps;
	const bool isLooped      = params.isLooped;
	float *__restrict src    = original;
	unsigned destBufferIndex = 0;

	for( unsigned pingPongStep = 0; pingPongStep < numSteps; ++pingPongStep ) {
		float *const __restrict dest = buffers[destBufferIndex];
		assert( src != dest );

		constexpr float kPeakWeight = 0.50f;
		constexpr float kTailWeight = 0.25f;
		static_assert( kPeakWeight + 2.0f * kTailWeight == 1.0f );

		// Don't let the fraction grow over the original value

		if( isLooped ) {
			dest[0]            = wsw::min( original[0],
										   kPeakWeight * src[0] + kTailWeight * ( src[1] + src[numEdges - 1] ) );
			dest[numEdges - 1] = wsw::min( original[numEdges - 1],
										   kPeakWeight * src[numEdges - 1] + kTailWeight * ( src[0] + src[numEdges - 2] ) );
		} else {
			dest[0]            = wsw::min( original[0],
										   kPeakWeight * src[0] + 2.0f * kTailWeight * src[1] );
			dest[numEdges - 1] = wsw::min( original[numEdges - 1],
										   kPeakWeight * src[numEdges - 1] + 2.0f * kTailWeight * src[numEdges - 2] );
		}

		unsigned edgeNum = 1;
		do {
			dest[edgeNum] = wsw::min( original[edgeNum],
									  kPeakWeight * src[edgeNum] + kTailWeight * ( src[edgeNum - 1] + src[edgeNum + 1] ) );
		} while( ++edgeNum < numEdges - 1 );

		src             = dest;
		destBufferIndex = ( destBufferIndex + 1 ) % 2;
	}

	// It is either a last written buffer, or the original buffer (in case of no steps made).
	return src;
}