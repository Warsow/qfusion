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

#include "transienteffectssystem.h"
#include "../cgame/cg_local.h"
#include "../client/client.h"
#include "../qcommon/links.h"

#include <cstdlib>
#include <cstring>

class BasicHullsHolder {
public:
	BasicHullsHolder() {
		constexpr const uint8_t icosahedronFaces[20][3] {
			{ 0, 11, 5 }, { 0, 5, 1 }, { 0, 1, 7 }, { 0, 7, 10 }, { 0, 10, 11 },
			{ 1, 5, 9 }, { 5, 11, 4 }, { 11, 10, 2 }, { 10, 7, 6 }, { 7, 1, 8 },
			{ 3, 9, 4 }, { 3, 4, 2 }, { 3, 2, 6 }, { 3, 6, 8 }, { 3, 8, 9 },
			{ 4, 9, 5 }, { 2, 4, 11 }, { 6, 2, 10 }, { 8, 6, 7 }, { 9, 8, 1 },
		};

		constexpr const float a = 0.525731f, b = 0.850651f;
		const vec3_t icosahedronVertices[12] {
			{ -a, +b, +0 }, { +a, +b, +0 }, { -a, -b, +0 }, { +a, -b, +0 },
			{ +0, -a, +b }, { +0, +a, +b }, { +0, -a, -b }, { +0, +a, -b },
			{ +b, +0, -a }, { +b, +0, +a }, { -b, +0, -a }, { -b, +0, +a },
		};

		for( const auto &v : icosahedronVertices ) {
			m_vertices.emplace_back( Vertex { { v[0], v[1], v[2], 1.0f } } );
		}
		for( const auto &f: icosahedronFaces ) {
			m_faces.emplace_back( Face { { f[0], f[1], f[2] } } );
		}

		m_icosphereEntries.emplace_back( Entry { 0, 3 * (unsigned)m_faces.size(), (unsigned)m_vertices.size() } );

		MidpointMap midpointCache;
		unsigned oldFacesSize = 0, facesSize = m_faces.size();
		while( !m_icosphereEntries.full() ) {
			for( unsigned i = oldFacesSize; i < facesSize; ++i ) {
				const Face &face = m_faces[i];
				const uint16_t v1 = face.data[0], v2 = face.data[1], v3 = face.data[2];
				assert( v1 != v2 && v2 != v3 && v1 != v3 );
				const uint16_t p = getMidPoint( v1, v2, &midpointCache );
				const uint16_t q = getMidPoint( v2, v3, &midpointCache );
				const uint16_t r = getMidPoint( v3, v1, &midpointCache );
				m_faces.emplace_back( Face { { v1, p, r } } );
				m_faces.emplace_back( Face { { v2, q, p } } );
				m_faces.emplace_back( Face { { v3, r, q } } );
				m_faces.emplace_back( Face { { p, q, r } } );
			}
			oldFacesSize = facesSize;
			facesSize = m_faces.size();
			const unsigned firstIndex = 3 * oldFacesSize;
			const unsigned numIndices = 3 * ( facesSize - oldFacesSize );
			m_icosphereEntries.emplace_back( Entry { firstIndex, numIndices, (unsigned)m_vertices.size() } );
		}

		// TODO: Use fixed vectors
		m_vertices.shrink_to_fit();
		m_faces.shrink_to_fit();

		// TODO can we do that during faces construction.
		buildNeighbours();
	}

	void buildNeighbours() {
		constexpr uint8_t kNumVertexNeighbours = 5;
		alignas( 16 )uint8_t knownNeighboursCount[3072];

		// Note that neighbours of the same vertex differ for different subdivision level
		// so we have to recompute all neighbours for each entry (each subdivision level).
		for( Entry &entry: m_icosphereEntries ) {
			entry.firstNeighboursElement = m_neighbours.size();
			m_neighbours.resize( m_neighbours.size() + entry.numVertices );

			Neighbours *const indicesOfNeighboursForVertex = m_neighbours.data() + entry.firstNeighboursElement;
			// Fill by ~0 to spot non-feasible indices fast
			std::memset( indicesOfNeighboursForVertex, 255, entry.numVertices * sizeof( Neighbours ) );

			assert( entry.numVertices < std::size( knownNeighboursCount ) );
			std::memset( knownNeighboursCount, 0, entry.numVertices );

			unsigned numVerticesWithCompleteNeighbours = 0;
			const unsigned faceOffset = entry.firstIndex / 3;
			const unsigned facesCount = entry.numIndices / 3;
			for( unsigned faceIndex = faceOffset; faceIndex < faceOffset + facesCount; ++faceIndex ) {
				const Face &face = m_faces[faceIndex];
				bool isDoneWithThisEntry = false;
				for( unsigned i = 0; i < 3; ++i ) {
					const auto indexOfVertex = face.data[i];
					assert( indexOfVertex < entry.numVertices );
					assert( knownNeighboursCount[indexOfVertex] <= kNumVertexNeighbours );
					// If neighbours are not complete yet
					if( knownNeighboursCount[indexOfVertex] != kNumVertexNeighbours ) {
						uint16_t *const indicesOfVertex = indicesOfNeighboursForVertex[indexOfVertex].data;
						// For each other vertex try adding it to neighbours
						for( unsigned j = 0; j < 3; ++j ) {
							if( i != j ) {
								const auto otherIndex = face.data[j];
								assert( otherIndex != indexOfVertex );
								uint16_t *indicesEnd = indicesOfVertex + knownNeighboursCount[indexOfVertex];
								if( std::find( indicesOfVertex, indicesEnd, otherIndex ) == indicesEnd ) {
									knownNeighboursCount[indexOfVertex]++;
									*indicesEnd = otherIndex;
								}
							}
						}
						assert( knownNeighboursCount[indexOfVertex] <= kNumVertexNeighbours );
						// If neighbours become complete for this entry
						if( knownNeighboursCount[indexOfVertex] == kNumVertexNeighbours ) {
							numVerticesWithCompleteNeighbours++;
							if( numVerticesWithCompleteNeighbours == entry.numVertices ) {
								isDoneWithThisEntry = true;
								break;
							}
						}
					}
				}
				// Would like to use labeled-breaks if they were a thing
				if( isDoneWithThisEntry ) {
					break;
				}
			}
		}

		// TODO: Use a fixed vector
		m_neighbours.shrink_to_fit();
	}

	struct IcosphereData {
		std::span<const vec4_t> vertices;
		std::span<const uint16_t> indices;
		std::span<const uint16_t[5]> vertexNeighbours;
	};

	[[nodiscard]]
	auto getIcosphereForLevel( unsigned level ) -> IcosphereData {
		assert( level < m_icosphereEntries.size() );
		// Return primitive/opaque data
		using OpaqueNeighbours     = uint16_t[5];
		const auto *vertexData     = (const vec4_t *)m_vertices.data();
		const auto *indexData      = (const uint16_t *)m_faces.data();
		const auto *neighboursData = (const OpaqueNeighbours *)m_neighbours.data();
		const Entry &entry         = m_icosphereEntries[level];
		std::span<const vec4_t> verticesSpan { vertexData, entry.numVertices };
		std::span<const uint16_t> indicesSpan { indexData + entry.firstIndex, entry.numIndices };
		return { verticesSpan, indicesSpan, { neighboursData + entry.firstNeighboursElement, entry.numVertices } };
	}
private:
	struct alignas( 4 ) Vertex { float data[4]; };
	static_assert( sizeof( Vertex ) == sizeof( vec4_t ) );
	struct alignas( 2 ) Face { uint16_t data[3]; };
	static_assert( sizeof( Face ) == sizeof( uint16_t[3] ) );
	struct alignas( 2 ) Neighbours { uint16_t data[5]; };
	static_assert( sizeof( Neighbours ) == sizeof( uint16_t[5] ) );

	using MidpointMap = wsw::TreeMap<unsigned, uint16_t>;

	[[nodiscard]]
	auto getMidPoint( uint16_t i1, uint16_t i2, MidpointMap *midpointCache ) -> uint16_t {
		const unsigned smallest = ( i1 < i2 ) ? i1 : i2;
		const unsigned largest = ( i1 < i2 ) ? i2 : i1;
		const unsigned key = ( smallest << 16 ) | largest;
		if( const auto it = midpointCache->find( key ); it != midpointCache->end() ) {
			return it->second;
		}

		Vertex midpoint {};
		const float *v1 = m_vertices[i1].data, *v2 = m_vertices[i2].data;
		VectorAvg( v1, v2, midpoint.data );
		VectorNormalize( midpoint.data );
		midpoint.data[3] = 1.0f;

		const auto index = (uint16_t)m_vertices.size();
		m_vertices.push_back( midpoint );
		midpointCache->insert( std::make_pair( key, index ) );
		return index;
	}

	wsw::Vector<Vertex> m_vertices;
	wsw::Vector<Face> m_faces;
	wsw::Vector<Neighbours> m_neighbours;

	struct Entry { unsigned firstIndex, numIndices, numVertices, firstNeighboursElement; };
	wsw::StaticVector<Entry, 5> m_icosphereEntries;
};

static BasicHullsHolder basicHullsHolder;

TransientEffectsSystem::TransientEffectsSystem() {
	// TODO: Take care of exception-safety
	while( !m_freeShapeLists.full() ) {
		if( auto *shapeList = CM_AllocShapeList( cl.cms ) ) [[likely]] {
			m_freeShapeLists.push_back( shapeList );
		} else {
			throw std::bad_alloc();
		}
	}
	// TODO: Take care of exception-safety
	if( !( m_tmpShapeList = CM_AllocShapeList( cl.cms ) ) ) [[unlikely]] {
		throw std::bad_alloc();
	}
}

TransientEffectsSystem::~TransientEffectsSystem() {
	for( EntityEffect *effect = m_entityEffectsHead, *next = nullptr; effect; effect = next ) { next = effect->next;
		unlinkAndFree( effect );
	}
	for( FireHull *hull = m_fireHullsHead, *nextHull = nullptr; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFree( hull );
	}
	for( SmokeHull *hull = m_smokeHullsHead, *nextHull = nullptr; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFree( hull );
	}
	for( WaveHull *hull = m_waveHullsHead, *nextHull = nullptr; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFree( hull );
	}
	for( CMShapeList *shapeList: m_freeShapeLists ) {
		CM_FreeShapeList( cl.cms, shapeList );
	}
	CM_FreeShapeList( cl.cms, m_tmpShapeList );
}

static const byte_vec4_t kExplosionReplacementPalette[] {
	{ 255, 108, 0, 24 },
	{ 255, 72, 0, 24 },
	{ 255, 255, 255, 24 },
	{ 255, 128, 0, 16 },
	{ 255, 144, 0, 12 },
};

static const byte_vec4_t kSmokeReplacementPalette[] {
	{ 255, 255, 255, 12 },
	{ 255, 255, 255, 24 },
	{ 255, 255, 255, 16 },
};

void TransientEffectsSystem::spawnExplosion( const float *origin, const float *color, float radius ) {
	/*
	EntityEffect *effect = addSpriteEffect( cgs.media.shaderRocketExplosion, origin, radius, 800u );

	constexpr float lightRadiusScale = 1.0f / 64.0f;
	// 300 for radius of 64
	effect->lightRadius = 300.0f * radius * lightRadiusScale;
	VectorCopy( colorOrange, effect->lightColor );

	//(void)addSpriteEffect( cgs.media.shaderRocketExplosion, origin, 0.67f * radius, 500u );
	 */

	const vec4_t waveColor { 1.0f, 1.0f, 1.0f, 0.06f };
	const vec4_t smokeColor { 1.0f, 1.0f, 1.0f, 0.03f };
	const vec4_t fireColor { 1.0f, 0.7f, 0.1f, 0.8f };

	const vec2_t speedsAndSpreads[5] {
		{ 25.0f, 5.0f }, { 35.0f, 5.0f }, { 45.0f, 12.5f }, { 52.5f, 12.5f }, { 60.0f, 20.0f }
	};

	const float finalOffsets[5] { 8.0f, 6.0f, 4.0f, 2.0f, 0.0f };
	if( auto *hull = allocHull<FireHull, false>( &m_fireHullsHead, &m_fireHullsAllocator, m_lastTime, 850 ) ) {
		setupHullVertices( hull, origin, fireColor, speedsAndSpreads, finalOffsets );
		hull->colorChangeInterval = 15;
		for( unsigned i = 0; i < hull->numLayers; ++i ) {
			auto *layer = &hull->layers[i];
			layer->colorReplacementPalette = kExplosionReplacementPalette;
			layer->colorReplacementChance = 0.015f * (float)( i + 1 );
			layer->colorDropChance = 0.005f * (float)i;
		}
	}

	if( auto *hull = allocHull<WaveHull, true>( &m_waveHullsHead, &m_waveHullsAllocator, m_lastTime, 250 ) ) {
		setupHullVertices( hull, origin, waveColor, 500.0f, 10.0f );
	}

	if( auto *hull = allocHull<SmokeHull, true>( &m_smokeHullsHead, &m_smokeHullsAllocator, m_lastTime, 850 ) ) {
		setupHullVertices( hull, origin, smokeColor, 128.0f, 10.0f );
		hull->colorReplacementPalette = kSmokeReplacementPalette;
		hull->colorChangeInterval = 15;
		hull->colorReplacementChance = 0.02f;
		hull->colorDropChance = 0.001f;
	}
}

void TransientEffectsSystem::spawnCartoonHitEffect( const float *origin, const float *dir, int damage ) {
	if( cg_cartoonHitEffect->integer ) {
		float radius = 0.0f;
		shader_s *material = nullptr;
		if( damage > 64 ) {
			// OUCH!
			std::tie( material, radius ) = std::make_pair( cgs.media.shaderCartoonHit3, 24.0f );
		} else if( damage > 50 ) {
			// POW!
			std::tie( material, radius ) = std::make_pair( cgs.media.shaderCartoonHit, 19.0f );
		} else if( damage > 38 ) {
			// SPLITZOW!
			std::tie( material, radius ) = std::make_pair( cgs.media.shaderCartoonHit2, 15.0f );
		}

		if( material ) {
			// TODO:
			vec3_t localDir;
			if( !VectorLength( dir ) ) {
				VectorNegate( &cg.view.axis[AXIS_FORWARD], localDir );
			} else {
				VectorNormalize2( dir, localDir );
			}

			vec3_t spriteOrigin;
			// Move effect a bit up from player
			VectorCopy( origin, spriteOrigin );
			spriteOrigin[2] += ( playerbox_stand_maxs[2] - playerbox_stand_mins[2] ) + 1.0f;

			EntityEffect *effect = addSpriteEffect( material, spriteOrigin, radius, 700u );
			effect->entity.rotation = 0.0f;
			// TODO: Add a correct sampling of random sphere points as a random generator method
			for( unsigned i = 0; i < 3; ++i ) {
				effect->velocity[i] = m_rng.nextFloat( -10.0f, +10.0f );
			}
		}
	}
}

void TransientEffectsSystem::spawnElectroboltHitEffect( const float *origin, const float *dir ) {
	EntityEffect *effect = addModelEffect( cgs.media.modElectroBoltWallHit, origin, dir, 600 );
	VectorMA( origin, 4.0f, dir, effect->lightOrigin );
	VectorCopy( colorWhite, effect->lightColor );
	effect->lightRadius = 144.0f;
}

void TransientEffectsSystem::spawnInstagunHitEffect( const float *origin, const float *dir, const float *color ) {
	EntityEffect *effect = addModelEffect( cgs.media.modInstagunWallHit, origin, dir, 600u );
	VectorMA( origin, 4.0f, dir, effect->lightOrigin );
	VectorCopy( colorMagenta, effect->lightColor );
	effect->lightRadius = 144.0f;
}

void TransientEffectsSystem::spawnPlasmaImpactEffect( const float *origin, const float *dir ) {
	EntityEffect *effect = addModelEffect( cgs.media.modPlasmaExplosion, origin, dir, 400u );
	VectorMA( origin, 4.0f, dir, effect->lightOrigin );
	VectorCopy( colorGreen, effect->lightColor );
	effect->lightRadius = 108.0f;
	effect->fadedInScale = effect->fadedOutScale = 5.0f;
}

void TransientEffectsSystem::spawnGunbladeBlastImpactEffect( const float *origin, const float *dir ) {
	EntityEffect *effect = addModelEffect( cgs.media.modBladeWallExplo, origin, dir, 600u );
	VectorMA( origin, 8.0f, dir, effect->lightOrigin );
	VectorCopy( colorYellow, effect->lightColor );
	effect->fadedInScale = effect->fadedOutScale = 5.0f;
	effect->lightRadius = 200.0f;
}

void TransientEffectsSystem::spawnGunbladeBladeImpactEffect( const float *origin, const float *dir ) {
	(void)addModelEffect( cgs.media.modBladeWallHit, origin, dir, 300u );
	// TODO: Add light when hitting metallic surfaces?
}

void TransientEffectsSystem::spawnBulletLikeImpactEffect( const float *origin, const float *dir ) {
	(void)addModelEffect( cgs.media.modBulletExplode, origin, dir, 300u );
	// TODO: Add light when hitting metallic surfaces?
}

void TransientEffectsSystem::spawnDustImpactEffect( const float *origin, const float *dir, float radius ) {
	vec3_t axis1, axis2;
	PerpendicularVector( axis2, dir );
	CrossProduct( dir, axis2, axis1 );

	VectorNormalize( axis1 ), VectorNormalize( axis2 );

	float angle = 0.0f;
	constexpr const int count = 12;
	const float speed = 0.67f * radius;
	const float angleStep = (float)M_TWOPI * Q_Rcp( (float)count );
	for( int i = 0; i < count; ++i ) {
		const float scale1 = std::sin( angle ), scale2 = std::cos( angle );
		angle += angleStep;

		vec3_t velocity { 0.0f, 0.0f, 0.0f };
		VectorMA( velocity, speed * scale1, axis1, velocity );
		VectorMA( velocity, speed * scale2, axis2, velocity );

		EntityEffect *effect = addSpriteEffect( cgs.media.shaderSmokePuff2, origin, 10.0f, 700u );
		effect->fadedInScale = 0.33f;
		effect->fadedOutScale = 0.0f;
		effect->initialAlpha = 0.25f;
		effect->fadedOutAlpha = 0.0f;
		VectorCopy( velocity, effect->velocity );
	}
}

void TransientEffectsSystem::spawnDashEffect( const float *origin, const float *dir ) {
	// Model orientation/streching hacks
	vec3_t angles;
	VecToAngles( dir, angles );
	angles[1] += 270.0f;
	EntityEffect *effect = addModelEffect( cgs.media.modDash, origin, dir, 700u );
	AnglesToAxis( angles, effect->entity.axis );
	// Scale Z
	effect->entity.axis[2 * 3 + 2] *= 2.0f;
	// Size hacks
	effect->fadedInScale = effect->fadedOutScale = 0.15f;
}

auto TransientEffectsSystem::addModelEffect( model_s *model, const float *origin, const float *dir, unsigned duration ) -> EntityEffect * {
	EntityEffect *const effect = allocEntityEffect( m_lastTime, duration );

	std::memset( &effect->entity, 0, sizeof( entity_s ) );
	effect->entity.rtype = RT_MODEL;
	effect->entity.renderfx = RF_NOSHADOW;
	effect->entity.model = model;
	effect->entity.customShader = nullptr;
	effect->entity.shaderTime = m_lastTime;
	effect->entity.scale = 0.0f;
	effect->entity.rotation = (float)m_rng.nextBounded( 360 );

	VectorSet( effect->entity.shaderRGBA, 255, 255, 255 );

	NormalVectorToAxis( dir, &effect->entity.axis[0] );
	VectorCopy( origin, effect->entity.origin );
	VectorCopy( origin, effect->lightOrigin );

	return effect;
}

auto TransientEffectsSystem::addSpriteEffect( shader_s *material, const float *origin, float radius, unsigned duration ) -> EntityEffect * {
	EntityEffect *effect = allocEntityEffect( m_lastTime, duration );

	std::memset( &effect->entity, 0, sizeof( entity_s ) );
	effect->entity.rtype = RT_SPRITE;
	effect->entity.renderfx = RF_NOSHADOW;
	effect->entity.radius = radius;
	effect->entity.customShader = material;
	effect->entity.shaderTime = m_lastTime;
	effect->entity.scale = 0.0f;
	effect->entity.rotation = (float)m_rng.nextBounded( 360 );

	VectorSet( effect->entity.shaderRGBA, 255, 255, 255 );

	Matrix3_Identity( effect->entity.axis );
	VectorCopy( origin, effect->entity.origin );
	VectorCopy( origin, effect->lightOrigin );

	return effect;
}

auto TransientEffectsSystem::allocEntityEffect( int64_t currTime, unsigned duration ) -> EntityEffect * {
	void *mem = m_entityEffectsAllocator.allocOrNull();
	if( !mem ) [[unlikely]] {
		// TODO: Prioritize effects so unimportant ones get evicted first
		EntityEffect *oldestEffect = nullptr;
		// TODO: Choose by nearest timeout/lifetime fraction?
		int64_t oldestSpawnTime = std::numeric_limits<int64_t>::max();
		for( EntityEffect *effect = m_entityEffectsHead; effect; effect = effect->next ) {
			if( oldestSpawnTime > effect->spawnTime ) {
				oldestSpawnTime = effect->spawnTime;
				oldestEffect = effect;
			}
		}
		assert( oldestEffect );
		wsw::unlink( oldestEffect, &m_entityEffectsHead );
		oldestEffect->~EntityEffect();
		mem = oldestEffect;
	}

	auto *effect = new( mem )EntityEffect;

	assert( duration <= std::numeric_limits<uint16_t>::max() );
	// Try forcing 16-bit division if a compiler fails to optimize division by constant
	unsigned fadeInDuration = (uint16_t)duration / (uint16_t)10;
	if( fadeInDuration > 33 ) [[likely]] {
		fadeInDuration = 33;
	} else if( fadeInDuration < 1 ) [[unlikely]] {
		fadeInDuration = 1;
	}

	unsigned fadeOutDuration;
	if( duration > fadeInDuration ) [[likely]] {
		fadeOutDuration = duration - fadeInDuration;
	} else {
		fadeOutDuration = fadeInDuration;
		duration = fadeInDuration + 1;
	}

	effect->duration = duration;
	effect->rcpDuration = Q_Rcp( (float)duration );
	effect->fadeInDuration = fadeInDuration;
	effect->rcpFadeInDuration = Q_Rcp( (float)fadeInDuration );
	effect->rcpFadeOutDuration = Q_Rcp( (float)fadeOutDuration );
	effect->spawnTime = currTime;

	wsw::link( effect, &m_entityEffectsHead );
	return effect;
}

// TODO: Turn on "Favor small code" for this template

template <typename Hull, bool HasShapeList>
auto TransientEffectsSystem::allocHull( Hull **head, wsw::FreelistAllocator *allocator,
										int64_t currTime, unsigned lifetime ) -> Hull * {
	void *mem = allocator->allocOrNull();
	CMShapeList *hullShapeList = nullptr;
	if( mem ) [[likely]] {
		if constexpr( HasShapeList ) {
			hullShapeList = m_freeShapeLists.back();
			m_freeShapeLists.pop_back();
		}
	} else {
		Hull *oldestHull = nullptr;
		int64_t oldestSpawnTime = std::numeric_limits<int64_t>::max();
		for( Hull *hull = *head; hull; hull = hull->next ) {
			if( oldestSpawnTime > hull->spawnTime ) {
				oldestSpawnTime = hull->spawnTime;
				oldestHull = hull;
			}
		}
		assert( oldestHull );
		wsw::unlink( oldestHull, head );
		if constexpr( HasShapeList ) {
			hullShapeList = oldestHull->shapeList;
		}
		oldestHull->~Hull();
		mem = oldestHull;
	}

	auto *const hull = new( mem )Hull;
	hull->spawnTime  = currTime;
	hull->lifetime   = lifetime;
	if constexpr( HasShapeList ) {
		hull->shapeList = hullShapeList;
	}

	wsw::link( hull, head );
	return hull;
}

void TransientEffectsSystem::setupHullVertices( BaseRegularSimulatedHull *hull, const float *origin,
												const float *color, float speed, float speedSpead ) {
	const byte_vec4_t initialColor {
		(uint8_t)( color[0] * 255 ),
		(uint8_t)( color[1] * 255 ),
		(uint8_t)( color[2] * 255 ),
		(uint8_t)( color[3] * 255 )
	};

	const float minSpeed = std::max( 0.0f, speed - 0.5f * speedSpead );
	const float maxSpeed = speed + 0.5f * speedSpead;
	wsw::RandomGenerator *__restrict rng = &m_rng;

	const float originX = origin[0], originY = origin[1], originZ = origin[2];
	const auto [verticesSpan, indicesSpan, _] = ::basicHullsHolder.getIcosphereForLevel( hull->subdivLevel );
	const auto *__restrict vertices = verticesSpan.data();
	vec4_t *const __restrict positions    = hull->vertexPositions[0];
	vec3_t *const __restrict velocities   = hull->vertexVelocities;
	vec4_t *const __restrict altPositions = hull->vertexPositions[1];
	float *const __restrict movability    = hull->vertexMovability;
	byte_vec4_t *const __restrict colors  = hull->vertexColors;
	VectorCopy( origin, hull->origin );
	hull->meshIndices = indicesSpan.data();
	hull->numMeshIndices = indicesSpan.size();
	hull->numMeshVertices = verticesSpan.size();
	for( size_t i = 0; i < verticesSpan.size(); ++i ) {
		// Vertex positions are absolute to simplify simulation
		Vector4Set( positions[i], originX, originY, originZ, 1.0f );
		const float vertexSpeed = rng->nextFloat( minSpeed, maxSpeed );
		// Unit vertices define directions
		VectorScale( vertices[i], vertexSpeed, velocities[i] );
		// Set the 4th component to 1.0 for alternating positions once as well
		altPositions[i][3] = 1.0f;
		movability[i] = 1.0f;
		Vector4Copy( initialColor, colors[i] );
	}
}

void TransientEffectsSystem::setupHullVertices( BaseConcentricSimulatedHull *hull,
												const float *origin, const float *color,
												std::span<const vec2_t> speedsAndSpreads,
												std::span<const float> finalOffsets ) {
	assert( speedsAndSpreads.size() == finalOffsets.size() && speedsAndSpreads.size() == hull->numLayers );

	const byte_vec4_t initialColor {
		(uint8_t)( color[0] * 255 ),
		(uint8_t)( color[1] * 255 ),
		(uint8_t)( color[2] * 255 ),
		(uint8_t)( color[3] * 255 )
	};

	const float originX = origin[0], originY = origin[1], originZ = origin[2];
	const auto [verticesSpan, indicesSpan, _] = ::basicHullsHolder.getIcosphereForLevel( hull->subdivLevel );
	const vec4_t *__restrict vertices = verticesSpan.data();

	// Calculate move limits in each direction

	assert( !speedsAndSpreads.empty() );
	float speed = speedsAndSpreads[0][0];
	for( unsigned i = 1; i < speedsAndSpreads.size(); ++i ) {
		speed = std::max( speed, speedsAndSpreads[i][0] );
	}

	wsw::RandomGenerator *const __restrict rng = &m_rng;

	const float radius = 0.5f * speed * ( 1e-3f * (float)hull->lifetime );
	const vec3_t growthMins { originX - speed * radius, originY - speed * radius, originZ - speed * radius };
	const vec3_t growthMaxs { originX + speed * radius, originY + speed * radius, originZ + speed * radius };

	// TODO: Add a fused call
	CM_BuildShapeList( cl.cms, m_tmpShapeList, growthMins, growthMaxs, MASK_SOLID );
	CM_ClipShapeList( cl.cms, m_tmpShapeList, m_tmpShapeList, growthMins, growthMaxs );

	trace_t trace;
	for( size_t i = 0; i < verticesSpan.size(); ++i ) {
		// Vertices of the unit hull define directions
		const float *dir = verticesSpan[i];

		vec3_t limitPoint;
		VectorMA( origin, radius, dir, limitPoint );

		CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, origin, limitPoint, vec3_origin, vec3_origin, MASK_SOLID );
		if( const float f = trace.fraction; f < 1.0f ) {
			hull->limitsAtDirections[i] = f * radius;
		} else {
			hull->limitsAtDirections[i] = radius;
		}
	}

	// Setup layers data
	assert( hull->numLayers >= 1 && hull->numLayers < 8 );
	for( unsigned layerNum = 0; layerNum < hull->numLayers; ++layerNum ) {
		BaseConcentricSimulatedHull::Layer *layer   = &hull->layers[layerNum];
		vec4_t *const __restrict positions          = layer->vertexPositions;
		byte_vec4_t *const __restrict colors        = layer->vertexColors;
		vec2_t *const __restrict speedsAndDistances = layer->vertexSpeedsAndDistances;

		const float baseSpeed = speedsAndSpreads[layerNum][0];
		const float spread = speedsAndSpreads[layerNum][1];
		const float minSpeed = std::max( 0.0f, baseSpeed - 0.5f * spread );
		const float maxSpeed = baseSpeed + 0.5f * spread;

		layer->finalOffset = finalOffsets[layerNum];

		for( size_t i = 0; i < verticesSpan.size(); ++i ) {
			// Position XYZ is computed prior to submission in stateless fashion
			positions[i][3] = 1.0f;

			speedsAndDistances[i][0] = rng->nextFloat( minSpeed, maxSpeed );
			speedsAndDistances[i][1] = 0.0f;

			Vector4Copy( initialColor, colors[i] );
		}
	}

	VectorCopy( origin, hull->origin );
	hull->vertexMoveDirections = vertices;
	hull->meshIndices          = indicesSpan.data();
	hull->numMeshVertices      = verticesSpan.size();
	hull->numMeshIndices       = indicesSpan.size();
}

void TransientEffectsSystem::unlinkAndFree( EntityEffect *effect ) {
	wsw::unlink( effect, &m_entityEffectsHead );
	effect->~EntityEffect();
	m_entityEffectsAllocator.free( effect );
}

void TransientEffectsSystem::unlinkAndFree( SmokeHull *hull ) {
	wsw::unlink( hull, &m_smokeHullsHead );
	m_freeShapeLists.push_back( hull->shapeList );
	hull->~SmokeHull();
	m_smokeHullsAllocator.free( hull );
}

void TransientEffectsSystem::unlinkAndFree( WaveHull *hull ) {
	wsw::unlink( hull, &m_waveHullsHead );
	m_freeShapeLists.push_back( hull->shapeList );
	hull->~WaveHull();
	m_waveHullsAllocator.free( hull );
}

void TransientEffectsSystem::unlinkAndFree( FireHull *hull ) {
	wsw::unlink( hull, &m_fireHullsHead );
	hull->~FireHull();
	m_fireHullsAllocator.free( hull );
}

void TransientEffectsSystem::simulateFrameAndSubmit( int64_t currTime, DrawSceneRequest *request ) {
	// Limit the time step
	const float timeDeltaSeconds = 1e-3f * (float)std::min<int64_t>( 33, currTime - m_lastTime );

	simulateEntityEffectsAndSubmit( currTime, timeDeltaSeconds, request );
	simulateHullsAndSubmit( currTime, timeDeltaSeconds, request );

	m_lastTime = currTime;
}

void TransientEffectsSystem::simulateEntityEffectsAndSubmit( int64_t currTime, float timeDeltaSeconds,
															 DrawSceneRequest *request ) {
	const model_s *const dashModel = cgs.media.modDash;
	const float backlerp = 1.0f - cg.lerpfrac;

	EntityEffect *nextEffect = nullptr;
	for( EntityEffect *__restrict effect = m_entityEffectsHead; effect; effect = nextEffect ) {
		nextEffect = effect->next;

		if( effect->spawnTime + effect->duration <= currTime ) [[unlikely]] {
			unlinkAndFree( effect );
			continue;
		}

		const auto lifetimeMillis = (unsigned)( currTime - effect->spawnTime );
		assert( lifetimeMillis < effect->duration );

		if( lifetimeMillis >= effect->fadeInDuration ) [[likely]] {
			assert( effect->duration > effect->fadeInDuration );
			const float fadeOutFrac = (float)( lifetimeMillis - effect->fadeInDuration ) * effect->rcpFadeOutDuration;
			effect->entity.scale = std::lerp( effect->fadedInScale, effect->fadedOutScale, fadeOutFrac );
		} else {
			const float fadeInFrac = (float)lifetimeMillis * effect->rcpFadeInDuration;
			effect->entity.scale = effect->fadedInScale * fadeInFrac;
		}

		// Dash model hacks
		if( effect->entity.model == dashModel ) [[unlikely]] {
			float *const zScale = effect->entity.axis + ( 2 * 3 ) + 2;
			*zScale -= 4.0f * timeDeltaSeconds;
			if( *zScale < 0.01f ) {
				unlinkAndFree( effect );
				continue;
			}
		}

		vec3_t moveVec;
		VectorScale( effect->velocity, timeDeltaSeconds, moveVec );
		VectorAdd( effect->entity.origin, moveVec, effect->entity.origin );

		const float lifetimeFrac = (float)lifetimeMillis * effect->rcpDuration;

		effect->entity.backlerp = backlerp;
		const float alpha = std::lerp( effect->initialAlpha, effect->fadedOutAlpha, lifetimeFrac );
		effect->entity.shaderRGBA[3] = (uint8_t)( 255 * alpha );

		request->addEntity( &effect->entity );
		if( effect->lightRadius > 1.0f ) {
			// Move the light as well
			VectorAdd( effect->lightOrigin, moveVec, effect->lightOrigin );
			const float lightFrac = 1.0f - lifetimeFrac;
			if( const float lightRadius = effect->lightRadius * lightFrac; lightRadius > 1.0f ) {
				request->addLight( effect->lightOrigin, lightRadius, 0.0f, effect->lightColor );
			}
		}
	}
}

void TransientEffectsSystem::simulateHullsAndSubmit( int64_t currTime, float timeDeltaSeconds,
													 DrawSceneRequest *drawSceneRequest ) {
	wsw::StaticVector<BaseRegularSimulatedHull *, kMaxSmokeHulls + kMaxWaveHulls> activeRegularHulls;
	wsw::StaticVector<BaseConcentricSimulatedHull *, kMaxFireHulls> activeConcentricHulls;
	for( FireHull *hull = m_fireHullsHead, *nextHull = nullptr; hull; hull = nextHull ) { nextHull = hull->next;
		if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
			hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			activeConcentricHulls.push_back( hull );
		} else {
			unlinkAndFree( hull );
		}
	}
	for( SmokeHull *hull = m_smokeHullsHead, *nextHull = nullptr; hull; hull = nextHull ) { nextHull = hull->next;
		if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
			hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			activeRegularHulls.push_back( hull );
		} else {
			unlinkAndFree( hull );
		}
	}
	for( WaveHull *hull = m_waveHullsHead, *nextHull = nullptr; hull; hull = nextHull ) { nextHull = hull->next;
		if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
			hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			activeRegularHulls.push_back( hull );
		} else {
			unlinkAndFree( hull );
		}
	}

	for( BaseRegularSimulatedHull *__restrict hull: activeRegularHulls ) {
		assert( std::size( hull->meshSubmissionBuffer ) == 1 );
		ExternalMesh *__restrict mesh = &hull->meshSubmissionBuffer[0];
		Vector4Copy( hull->mins, mesh->mins );
		Vector4Copy( hull->maxs, mesh->maxs );
		mesh->positions   = hull->vertexPositions[hull->positionsFrame];
		mesh->colors      = hull->vertexColors;
		mesh->indices     = hull->meshIndices;
		mesh->numVertices = hull->numMeshVertices;
		mesh->numIndices  = hull->numMeshIndices;
		mesh->material    = nullptr;
		drawSceneRequest->addExternalMesh( hull->mins, hull->maxs, { hull->meshSubmissionBuffer, 1 } );
	}

	for( const BaseConcentricSimulatedHull *__restrict hull: activeConcentricHulls ) {
		assert( hull->numLayers );
		// Meshes should be placed in memory continuously, so we can supply a span
		assert( hull->layers[hull->numLayers - 1].submittedMesh - hull->layers[0].submittedMesh + 1 == hull->numLayers );
		for( unsigned i = 0; i < hull->numLayers; ++i ) {
			BaseConcentricSimulatedHull::Layer *__restrict layer = &hull->layers[i];
			ExternalMesh *__restrict mesh = hull->layers[i].submittedMesh;
			Vector4Copy( layer->mins, mesh->mins );
			Vector4Copy( layer->maxs, mesh->maxs );
			mesh->positions   = layer->vertexPositions;
			mesh->colors      = layer->vertexColors;
			mesh->indices     = hull->meshIndices;
			mesh->numVertices = hull->numMeshVertices;
			mesh->numIndices  = hull->numMeshIndices;
			mesh->material    = nullptr;
		}
		drawSceneRequest->addExternalMesh( hull->mins, hull->maxs, { hull->layers[0].submittedMesh, hull->numLayers } );
	}
}

void TransientEffectsSystem::BaseRegularSimulatedHull::simulate( int64_t currTime, float timeDeltaSeconds,
																 wsw::RandomGenerator *__restrict rng ) {
	const vec4_t *const __restrict oldPositions = vertexPositions[positionsFrame];
	// Switch old/new positions buffer
	positionsFrame = ( positionsFrame + 1 ) % 2;
	vec4_t *const __restrict newPositions = vertexPositions[positionsFrame];
	vec3_t *const __restrict velocities = vertexVelocities;
	const unsigned numVertices = numMeshVertices;

	BoundsBuilder boundsBuilder;
	assert( timeDeltaSeconds < 0.1f );
	const float speedMultiplier = 1.0f - 1.5f * timeDeltaSeconds;
	for( unsigned i = 0; i < numVertices; ++i ) {
		// Compute ideal positions
		VectorMA( oldPositions[i], timeDeltaSeconds * vertexMovability[i], velocities[i], newPositions[i] );
		VectorScale( velocities[i], speedMultiplier, velocities[i] );
		// TODO: We should be able to supply vec4
		boundsBuilder.addPoint( newPositions[i] );
	}

	vec3_t verticesMins, verticesMaxs;
	boundsBuilder.storeToWithAddedEpsilon( verticesMins, verticesMaxs );
	// TODO: Allow bounds builder to store 4-vectors
	VectorCopy( verticesMins, mins );
	VectorCopy( verticesMaxs, maxs );
	mins[3] = 0.0f, maxs[3] = 1.0f;

	// TODO: Add a fused call
	CM_BuildShapeList( cl.cms, shapeList, verticesMins, verticesMaxs, MASK_SOLID | MASK_WATER );
	CM_ClipShapeList( cl.cms, shapeList, shapeList, verticesMins, verticesMaxs );

	trace_t trace;
	for( unsigned i = 0; i < numVertices; ++i ) {
		if( vertexMovability[i] != 0.0f ) {
			CM_ClipToShapeList( cl.cms, shapeList, &trace, oldPositions[i], newPositions[i], vec3_origin, vec3_origin, MASK_SOLID );
			if( trace.fraction != 1.0f ) [[unlikely]] {
				// TODO: Let it slide along the surface
				VectorAdd( trace.endpos, trace.plane.normal, newPositions[i] );
				// Park the vertex at the position
				vertexMovability[i] = 0.0f;
			}
		}
	}

	if( lastColorChangeTime + colorChangeInterval < currTime ) {
		lastColorChangeTime = currTime;
		processColorChange( vertexColors, numVertices, colorReplacementPalette,
							colorDropChance, colorReplacementChance, rng );
	}
}

void TransientEffectsSystem::BaseConcentricSimulatedHull::simulate( int64_t currTime, float timeDeltaSeconds,
																	wsw::RandomGenerator *__restrict rng ) {
	// Just move all vertices along directions clipping by limits

	BoundsBuilder hullBoundsBuilder;

	const float *__restrict growthOrigin = this->origin;
	const unsigned numVertices = this->numMeshVertices;

	const float speedMultiplier = 1.0f - 1.5f * timeDeltaSeconds;

	// Sanity check
	assert( numLayers >= 1 && numLayers < 8 );
	for( unsigned layerNum = 0; layerNum < numLayers; ++layerNum ) {
		BoundsBuilder layerBoundsBuilder;

		BaseConcentricSimulatedHull::Layer *layer   = &layers[layerNum];
		vec4_t *const __restrict positions          = layer->vertexPositions;
		vec2_t *const __restrict speedsAndDistances = layer->vertexSpeedsAndDistances;

		const float finalOffset = layer->finalOffset;
		for( unsigned i = 0; i < numVertices; ++i ) {
			float speed = speedsAndDistances[i][0];
			float distanceSoFar = speedsAndDistances[i][1];

			speed *= speedMultiplier;
			distanceSoFar += speed * timeDeltaSeconds;

			// Limit growth by the precomputed obstacle distance
			const float limit = std::max( 0.0f, limitsAtDirections[i] - finalOffset );
			distanceSoFar = std::min( distanceSoFar, limit );

			VectorMA( growthOrigin, distanceSoFar, vertexMoveDirections[i], positions[i] );

			// TODO: Allow supplying 4-component in-memory vectors directly
			layerBoundsBuilder.addPoint( positions[i] );

			// Write back to memory
			speedsAndDistances[i][0] = speed;
			speedsAndDistances[i][1] = distanceSoFar;
		}

		// TODO: Allow storing 4-component float vectors to memory directly
		layerBoundsBuilder.storeTo( layer->mins, layer->maxs );
		layer->mins[3] = 0.0f, layer->maxs[3] = 1.0f;

		// Don't relying on what hull is external is more robust

		// TODO: Allow adding other builder directly
		hullBoundsBuilder.addPoint( layer->mins );
		hullBoundsBuilder.addPoint( layer->maxs );
	}

	// TODO: Allow storing 4-component float vectors to memory directly
	hullBoundsBuilder.storeTo( this->mins, this->maxs );
	this->mins[3] = 0.0f, this->maxs[3] = 1.0f;

	if( lastColorChangeTime + colorChangeInterval < currTime ) {
		lastColorChangeTime = currTime;
		for( unsigned i = 0; i < numLayers; ++i ) {
			Layer *const layer = &layers[i];
			processColorChange( layer->vertexColors, numVertices, layer->colorReplacementPalette,
								layer->colorDropChance, layer->colorReplacementChance, rng );
		}
	}
}

void TransientEffectsSystem::processColorChange( byte_vec4_t *__restrict colors, unsigned numColors,
												 std::span<const byte_vec4_t> replacementPalette,
												 float dropChance, float replacementChance,
												 wsw::RandomGenerator *__restrict rng ) {

	assert( numColors );
	unsigned i = 0;
	if( replacementPalette.empty() ) {
		do {
			if( colors[i][3] != 0 ) {
				if( rng->nextFloat() < dropChance ) {
					colors[i][3] = 0;
				}
			}
		} while( ++i < numColors );
	} else {
		do {
			// Don't process elements that became void
			if( colors[i][3] != 0 ) {
				if( rng->nextFloat() < dropChance ) [[unlikely]] {
					colors[i][3] = 0;
				} else if( rng->nextFloat() < replacementChance ) [[unlikely]] {
					const auto *color = replacementPalette[rng->nextBounded( replacementPalette.size() )];
					Vector4Copy( color, colors[i] );
				}
			}
		} while( ++i < numColors );
	}
}