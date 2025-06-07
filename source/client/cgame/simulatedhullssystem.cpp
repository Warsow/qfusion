#include "simulatedhullssystem.h"

#include <common/helpers/links.h>
#include <client/client.h>
#include <common/helpers/memspecbuilder.h>
#include <common/helpers/mmuuid.h>
#include <common/facilities/profilerscope.h>
#include <common/helpers/algorithm.h>
#include "cg_local.h"

#include <memory>
#include <unordered_map>
#include <algorithm>

struct IcosphereData {
	std::span<const vec4_t> vertices;
	std::span<const uint16_t> indices;
	std::span<const uint16_t[5]> vertexNeighbours;
};

class BasicHullsHolder {
	struct Entry { unsigned firstIndex, numIndices, numVertices, firstNeighboursElement; };
public:
	static constexpr unsigned kMaxSubdivLevel = 4;

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

		wsw::StaticVector<Entry, kMaxSubdivLevel + 1> icosphereEntries;
		icosphereEntries.emplace_back( Entry { 0, 3 * (unsigned)m_faces.size(), (unsigned)m_vertices.size() } );

		MidpointMap midpointCache;
		unsigned oldFacesSize = 0, facesSize = m_faces.size();
		while( !icosphereEntries.full() ) {
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
			icosphereEntries.emplace_back( Entry { firstIndex, numIndices, (unsigned)m_vertices.size() } );
		}

		// TODO: Use fixed vectors
		m_vertices.shrink_to_fit();
		m_faces.shrink_to_fit();

		// TODO can we do that during faces construction.
		buildNeighbours( icosphereEntries );

		// Build data of the public interface
		for( unsigned level = 0; level < kMaxSubdivLevel + 1; ++level ) {
			using OpaqueNeighbours     = uint16_t[5];
			const auto *vertexData     = (const vec4_t *)m_vertices.data();
			const auto *indexData      = (const uint16_t *)m_faces.data();
			const auto *neighboursData = (const OpaqueNeighbours *)m_neighbours.data();
			const Entry &entry         = icosphereEntries[level];
			std::span<const vec4_t> verticesSpan { vertexData, entry.numVertices };
			std::span<const uint16_t> indicesSpan { indexData + entry.firstIndex, entry.numIndices };
			std::span<const uint16_t[5]> neighboursSpan { neighboursData + entry.firstNeighboursElement, entry.numVertices };
			m_icospheresForLevels.emplace_back( IcosphereData { verticesSpan, indicesSpan, neighboursSpan } );
		}
	}

	[[nodiscard]]
	auto getIcosphereForLevel( unsigned level ) -> IcosphereData {
		return m_icospheresForLevels[level];
	}
private:
	struct alignas( 4 ) Vertex { float data[4]; };
	static_assert( sizeof( Vertex ) == sizeof( vec4_t ) );
	struct alignas( 2 ) Face { uint16_t data[3]; };
	static_assert( sizeof( Face ) == sizeof( uint16_t[3] ) );
	struct alignas( 2 ) Neighbours { uint16_t data[5]; };
	static_assert( sizeof( Neighbours ) == sizeof( uint16_t[5] ) );

	using MidpointMap = std::unordered_map<unsigned, uint16_t>;

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

	static constexpr unsigned kNumVertexNeighbours = 5;
	static constexpr unsigned kNumFaceNeighbours = 5;

	struct NeighbourFaces { uint16_t data[kNumVertexNeighbours]; };

	// Returns the number of vertices that got their neighbour faces list completed during this call
	[[nodiscard]]
	auto addFaceToNeighbours( uint16_t faceNum, NeighbourFaces *neighbourFaces,
							  uint8_t *knownNeighboursCounts ) -> unsigned {
		unsigned result = 0;

		for( unsigned i = 0; i < 3; ++i ) {
			const uint16_t vertexNum      = m_faces[faceNum].data[i];
			uint8_t *const countForVertex = &knownNeighboursCounts[vertexNum];
			assert( *countForVertex <= kNumFaceNeighbours );
			if( *countForVertex < kNumFaceNeighbours ) {
				uint16_t *const indicesBegin = neighbourFaces[vertexNum].data;
				uint16_t *const indicesEnd   = indicesBegin + *countForVertex;
				// TODO: Is this search really needed?
				if( !wsw::contains( indicesBegin, indicesEnd, faceNum ) ) {
					( *countForVertex )++;
					*indicesEnd = (uint16_t)faceNum;
					// The neighbours list becomes complete
					if( *countForVertex == kNumFaceNeighbours ) {
						result++;
					}
				}
			}
		}

		return result;
	}

	[[nodiscard]]
	auto findFaceWithVertex( const NeighbourFaces *neighbourFaces, unsigned vertexNum, unsigned pickedFacesMask )
		-> std::optional<std::pair<unsigned, unsigned>> {

		// Assume that face #0 is always picked
		for( unsigned faceIndex = 1; faceIndex < kNumFaceNeighbours; ++faceIndex ) {
			if( !( pickedFacesMask & ( 1u << faceIndex ) ) ) {
				const Face &face = m_faces[neighbourFaces->data[faceIndex]];
				for( unsigned indexInFace = 0; indexInFace < 3; ++indexInFace ) {
					if( face.data[indexInFace] == vertexNum ) {
						return std::pair<unsigned, unsigned> { faceIndex, indexInFace };
					}
				}
			}
		}

		return std::nullopt;
	}

	void buildNeighboursForVertexUsingNeighbourFaces( unsigned vertexNum, Neighbours *neighbourVertices,
													  const NeighbourFaces *neighbourFaces ) {
		wsw::StaticVector<uint16_t, kNumVertexNeighbours> vertexNums;

		// Pick the first face unconditionally
		const Face &firstFace    = m_faces[neighbourFaces->data[0]];
		unsigned pickedFacesMask = 0x1;

		// Add 2 vertices of the first face
		for( unsigned i = 0; i < 3; ++i ) {
			if( firstFace.data[i] == vertexNum ) {
				vertexNums.push_back( firstFace.data[( i + 1 ) % 3] );
				vertexNums.push_back( firstFace.data[( i + 2 ) % 3] );
				break;
			}
		}

		assert( vertexNums.size() == 2 );
		while( !vertexNums.full() ) {
			bool prepend = false;
			// Try finding a face that contacts with the last added face forming a triangle fan
			auto maybeIndices = findFaceWithVertex( neighbourFaces, vertexNums.back(), pickedFacesMask );
			if( !maybeIndices ) {
				// Find a face that contacts with the first added face forming a triangle fan
				maybeIndices = findFaceWithVertex( neighbourFaces, vertexNums.front(), pickedFacesMask );
				prepend = true;
			}

			const auto [faceIndex, indexInFace] = maybeIndices.value();
			const Face &face = m_faces[neighbourFaces->data[faceIndex]];
			const unsigned otherIndex1 = ( indexInFace + 1 ) % 3;
			const unsigned otherIndex2 = ( indexInFace + 2 ) % 3;

			// Make sure the face contains the original vertex.
			assert( face.data[otherIndex1] == vertexNum || face.data[otherIndex2] == vertexNum );

			// Add the remaining one to the neighbour vertices list.
			unsigned indexOfVertexToAdd = otherIndex1;
			if( face.data[otherIndex1] == vertexNum ) {
				indexOfVertexToAdd = otherIndex2;
			}

			const uint16_t vertexNumToAdd = face.data[indexOfVertexToAdd];
			if( prepend ) {
				vertexNums.insert( vertexNums.begin(), vertexNumToAdd );
			} else {
				vertexNums.push_back( vertexNumToAdd );
			}

			pickedFacesMask |= ( 1u << faceIndex );
		}

		// Now check the normal direction with regard to direction to vertex

		const float *const v1 = m_vertices[vertexNum].data;
		const float *const v2 = m_vertices[vertexNums[0]].data;
		const float *const v3 = m_vertices[vertexNums[1]].data;

		vec3_t v1To2, v1To3, cross;
		VectorSubtract( v2, v1, v1To2 );
		VectorSubtract( v3, v1, v1To3 );
		CrossProduct( v1To2, v1To3, cross );
		// The vertex is a direction from (0, 0, 0) to itself
		if( DotProduct( v1, cross ) < 0 ) {
			std::reverse( vertexNums.begin(), vertexNums.end() );
		}

		std::copy( vertexNums.begin(), vertexNums.end(), neighbourVertices->data );
	}

	void buildNeighbours( wsw::StaticVector<Entry, kMaxSubdivLevel + 1> &icosphereEntries ) {
		NeighbourFaces neighbourFacesForVertex[3072];
		uint8_t knownNeighbourFacesCounts[3072];

		// Note that neighbours of the same vertex differ for different subdivision level
		// so we have to recompute all neighbours for each entry (each subdivision level).
		for( Entry &entry: icosphereEntries ) {
			entry.firstNeighboursElement = m_neighbours.size();
			m_neighbours.resize( m_neighbours.size() + entry.numVertices );

			Neighbours *const neighbourVerticesForVertex = m_neighbours.data() + entry.firstNeighboursElement;

			// Fill by ~0 to spot non-feasible indices fast
			std::memset( neighbourVerticesForVertex, 255, entry.numVertices * sizeof( Neighbours ) );
			std::memset( neighbourFacesForVertex, 255, entry.numVertices * sizeof( NeighbourFaces ) );

			assert( entry.numVertices < std::size( knownNeighbourFacesCounts ) );
			std::memset( knownNeighbourFacesCounts, 0, sizeof( uint8_t ) * entry.numVertices );

			const unsigned faceOffset = entry.firstIndex / 3;
			const unsigned facesCount = entry.numIndices / 3;

			// Build lists of neighbour face indices for every vertex

			unsigned numVerticesWithCompleteNeighbourFaces = 0;
			for( unsigned faceIndex = faceOffset; faceIndex < faceOffset + facesCount; ++faceIndex ) {
				numVerticesWithCompleteNeighbourFaces += addFaceToNeighbours( faceIndex,
																			  neighbourFacesForVertex,
																			  knownNeighbourFacesCounts );
				if( numVerticesWithCompleteNeighbourFaces == entry.numVertices ) {
					break;
				}
			}

			// Build lists of neighbour vertex indices for every vertex
			for( unsigned vertexIndex = 0; vertexIndex < entry.numVertices; ++vertexIndex ) {
				assert( knownNeighbourFacesCounts[vertexIndex] == kNumFaceNeighbours );

				buildNeighboursForVertexUsingNeighbourFaces( vertexIndex,
															 &neighbourVerticesForVertex[vertexIndex],
															 &neighbourFacesForVertex[vertexIndex] );
			}
		}

		// TODO: Use a fixed vector
		m_neighbours.shrink_to_fit();
	}

	wsw::PodVector<Vertex> m_vertices;
	wsw::PodVector<Face> m_faces;
	wsw::PodVector<Neighbours> m_neighbours;

	wsw::StaticVector<IcosphereData, kMaxSubdivLevel + 1> m_icospheresForLevels;
};

static BasicHullsHolder basicHullsHolder;

std::span<const vec4_t> SimulatedHullsSystem::getUnitIcosphere(unsigned level) {
	const auto [verticesSpan, indicesSpan, neighbourSpan] = ::basicHullsHolder.getIcosphereForLevel(level);
	return verticesSpan;
}

SimulatedHullsSystem::SimulatedHullsSystem() {
	const auto seedUuid  = mm_uuid_t::Random();
	const auto qwordSeed = seedUuid.loPart ^ seedUuid.hiPart;
	const auto dwordSeed = (uint32_t)( ( qwordSeed >> 32 ) ^ ( qwordSeed & 0xFFFFFFFFu ) );
	// TODO: Use the same instance for all effect subsystems
	m_rng.setSeed( dwordSeed );

	// TODO: Lift the number of possible miniviews/draw scene requests to the top scope
	m_storageOfSubmittedMeshOrderDesignators.reserve( 16 * kMaxMeshesPerHull * kMaxHullsWithLayers );
	m_storageOfSubmittedMeshPtrs.reserve( 16 * kMaxMeshesPerHull * kMaxHullsWithLayers );

	// TODO: Take care of exception-safety
	while( !m_freeShapeLists.full() ) {
		if( auto *shapeList = CM_AllocShapeList( cl.cms ) ) [[likely]] {
			m_freeShapeLists.push_back( shapeList );
		} else {
			wsw::failWithBadAlloc();
		}
	}
	// TODO: Take care of exception-safety
	if( !( m_tmpShapeList = CM_AllocShapeList( cl.cms ) ) ) [[unlikely]] {
		wsw::failWithBadAlloc();
	}
}

SimulatedHullsSystem::~SimulatedHullsSystem() {
	clear();
	for( CMShapeList *shapeList: m_freeShapeLists ) {
		CM_FreeShapeList( cl.cms, shapeList );
	}
	CM_FreeShapeList( cl.cms, m_tmpShapeList );
}

void SimulatedHullsSystem::clear() {
	for( FireHull *hull = m_fireHullsHead, *nextHull; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFreeFireHull( hull );
	}
	for( FireClusterHull *hull = m_fireClusterHullsHead, *next; hull; hull = next ) { next = hull->next;
		unlinkAndFreeFireClusterHull( hull );
	}
	for( BlastHull *hull = m_blastHullsHead, *nextHull; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFreeBlastHull( hull );
	}
	for( SmokeHull *hull = m_smokeHullsHead, *nextHull; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFreeSmokeHull( hull );
	}
	for( ToonSmokeHull *hull = m_toonSmokeHullsHead, *nextHull; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFreeToonSmokeHull( hull );
	}
	for( WaveHull *hull = m_waveHullsHead, *nextHull; hull; hull = nextHull ) { nextHull = hull->next;
		unlinkAndFreeWaveHull( hull );
	}
	m_lastTime = 0;
}

void SimulatedHullsSystem::unlinkAndFreeSmokeHull( SmokeHull *hull ) {
	wsw::unlink( hull, &m_smokeHullsHead );
	m_freeShapeLists.push_back( hull->shapeList );
	hull->~SmokeHull();
	m_smokeHullsAllocator.free( hull );
}

void SimulatedHullsSystem::unlinkAndFreeWaveHull( WaveHull *hull ) {
	wsw::unlink( hull, &m_waveHullsHead );
	m_freeShapeLists.push_back( hull->shapeList );
	hull->~WaveHull();
	m_waveHullsAllocator.free( hull );
}

void SimulatedHullsSystem::unlinkAndFreeFireHull( FireHull *hull ) {
	wsw::unlink( hull, &m_fireHullsHead );
	hull->~FireHull();
	m_fireHullsAllocator.free( hull );
}

void SimulatedHullsSystem::unlinkAndFreeFireClusterHull( FireClusterHull *hull ) {
	wsw::unlink( hull, &m_fireClusterHullsHead );
	hull->~FireClusterHull();
	m_fireClusterHullsAllocator.free( hull );
}

void SimulatedHullsSystem::unlinkAndFreeBlastHull( BlastHull *hull ) {
	wsw::unlink( hull, &m_blastHullsHead );
	hull->~BlastHull();
	m_blastHullsAllocator.free( hull );
}

void SimulatedHullsSystem::unlinkAndFreeToonSmokeHull( ToonSmokeHull *hull ) {
	wsw::unlink( hull, &m_toonSmokeHullsHead );
	hull->~ToonSmokeHull();
	m_toonSmokeHullsAllocator.free( hull );
}

auto SimulatedHullsSystem::allocFireHull( int64_t currTime, unsigned lifetime ) -> FireHull * {
	return allocHull<FireHull, false>( &m_fireHullsHead, &m_fireHullsAllocator, currTime, lifetime );
}

auto SimulatedHullsSystem::allocFireClusterHull( int64_t currTime, unsigned lifetime ) -> FireClusterHull * {
	return allocHull<FireClusterHull, false>( &m_fireClusterHullsHead, &m_fireClusterHullsAllocator, currTime, lifetime );
}

auto SimulatedHullsSystem::allocBlastHull( int64_t currTime, unsigned int lifetime ) -> BlastHull * {
	return allocHull<BlastHull, false>( &m_blastHullsHead, &m_blastHullsAllocator, currTime, lifetime );
}

auto SimulatedHullsSystem::allocSmokeHull( int64_t currTime, unsigned lifetime ) -> SmokeHull * {
	return allocHull<SmokeHull, true>( &m_smokeHullsHead, &m_smokeHullsAllocator, currTime, lifetime );
}

auto SimulatedHullsSystem::allocWaveHull( int64_t currTime, unsigned lifetime ) -> WaveHull * {
	return allocHull<WaveHull, true>( &m_waveHullsHead, &m_waveHullsAllocator, currTime, lifetime );
}

auto SimulatedHullsSystem::allocToonSmokeHull(int64_t currTime, unsigned int lifetime) -> ToonSmokeHull * {
	return allocHull<ToonSmokeHull, false>( &m_toonSmokeHullsHead, &m_toonSmokeHullsAllocator, currTime, lifetime );
}

// TODO: Turn on "Favor small code" for this template

template <typename Hull, bool HasShapeList>
auto SimulatedHullsSystem::allocHull( Hull **head, wsw::FreelistAllocator *allocator,
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

void SimulatedHullsSystem::setupHullVertices( BaseRegularSimulatedHull *hull, const float *origin,
											  const float *color, float speed, float speedSpead,
											  const AppearanceRules &appearanceRules,
											  const float *spikeSpeedMask, float maxSpikeSpeed ) {
	const byte_vec4_t initialColor {
		(uint8_t)( color[0] * 255 ),
		(uint8_t)( color[1] * 255 ),
		(uint8_t)( color[2] * 255 ),
		(uint8_t)( color[3] * 255 )
	};

	const float minSpeed = wsw::max( 0.0f, speed - 0.5f * speedSpead );
	const float maxSpeed = speed + 0.5f * speedSpead;
	wsw::RandomGenerator *__restrict rng = &m_rng;

	const float originX = origin[0], originY = origin[1], originZ = origin[2];
	const auto [verticesSpan, indicesSpan, _] = ::basicHullsHolder.getIcosphereForLevel( hull->subdivLevel );
	const auto *__restrict vertices = verticesSpan.data();

	std::memset( hull->vertexForceVelocities, 0, verticesSpan.size() * sizeof( hull->vertexForceVelocities[0] ) );

	vec4_t *const __restrict positions       = hull->vertexPositions[0];
	vec4_t *const __restrict vertexNormals   = hull->vertexNormals;
	vec3_t *const __restrict burstVelocities = hull->vertexBurstVelocities;
	vec4_t *const __restrict altPositions    = hull->vertexPositions[1];
	byte_vec4_t *const __restrict colors     = hull->vertexColors;

	size_t i = 0;
	do {
		// Vertex positions are absolute to simplify simulation
		Vector4Set( positions[i], originX, originY, originZ, 1.0f );
		float vertexSpeed = rng->nextFloat( minSpeed, maxSpeed );
		if( spikeSpeedMask ) {
			vertexSpeed += maxSpikeSpeed * spikeSpeedMask[i];
		}
		// Unit vertices define directions
		VectorScale( vertices[i], vertexSpeed, burstVelocities[i] );
		// Set the 4th component to 1.0 for alternating positions once as well
		altPositions[i][3] = 1.0f;
		Vector4Copy( initialColor, colors[i] );
		if( vertexNormals ) {
			// Supply vertex directions as normals for the first simulation frame
			VectorCopy( vertices[i], vertexNormals[i] );
			vertexNormals[i][3] = 0.0f;
		}
	} while( ++i < verticesSpan.size() );

	hull->minZLastFrame = originZ - 1.0f;
	hull->maxZLastFrame = originZ + 1.0f;

	VectorCopy( origin, hull->origin );

	hull->appearanceRules = appearanceRules;
}

void SimulatedHullsSystem::setupHullVertices( BaseConcentricSimulatedHull *hull, const float *origin,
											  float scale, std::span<const HullLayerParams> layerParams,
											  const AppearanceRules &appearanceRules ) {
	assert( layerParams.size() == hull->numLayers );

	const float originX = origin[0], originY = origin[1], originZ = origin[2];
	const auto [verticesSpan, indicesSpan, neighboursSpan] = ::basicHullsHolder.getIcosphereForLevel( hull->subdivLevel );
	const vec4_t *__restrict vertices = verticesSpan.data();

	// Calculate move limits in each direction

	float maxVertexSpeed = layerParams[0].speed + layerParams[0].maxSpeedSpike + layerParams[0].biasAlongChosenDir ;
	for( unsigned i = 1; i < layerParams.size(); ++i ) {
		const auto &params = layerParams[i];
		maxVertexSpeed = wsw::max( maxVertexSpeed, params.speed + params.maxSpeedSpike + params.biasAlongChosenDir );
	}

	// To prevent noticeable z-fighting in case if hulls of different layers start matching (e.g due to bias)
	constexpr float maxSmallRandomOffset = 1.5f;

	maxVertexSpeed += maxSmallRandomOffset;
	// Scale by the fine-tuning scale multiplier
	maxVertexSpeed *= 1.5f * scale;

	wsw::RandomGenerator *const __restrict rng = &m_rng;

	const float radius = 0.5f * maxVertexSpeed * ( 1e-3f * (float)hull->lifetime );
	const vec3_t growthMins { originX - radius, originY - radius, originZ - radius };
	const vec3_t growthMaxs { originX + radius, originY + radius, originZ + radius };

	// TODO: Add a fused call
	CM_BuildShapeList( cl.cms, m_tmpShapeList, growthMins, growthMaxs, MASK_SOLID );
	CM_ClipShapeList( cl.cms, m_tmpShapeList, m_tmpShapeList, growthMins, growthMaxs );

	if( CM_GetNumShapesInShapeList( m_tmpShapeList ) == 0 ) {
		// Limits at each direction just match the given radius in this case
		std::fill( hull->limitsAtDirections, hull->limitsAtDirections + verticesSpan.size(), radius );
	} else {
		trace_t trace;
		for( size_t i = 0; i < verticesSpan.size(); ++i ) {
			// Vertices of the unit hull define directions
			const float *dir = verticesSpan[i];

			vec3_t limitPoint;
			VectorMA( origin, radius, dir, limitPoint );

			CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, origin, limitPoint, vec3_origin, vec3_origin, MASK_SOLID );
			hull->limitsAtDirections[i] = trace.fraction * radius;
		}
	}

	auto *const __restrict spikeSpeedBoost = (float *)alloca( sizeof( float ) * verticesSpan.size() );

	const float *const __restrict globalBiasDir = verticesSpan[rng->nextBounded( verticesSpan.size() ) ];
	assert( std::fabs( VectorLengthFast( globalBiasDir ) - 1.0f ) < 0.001f );

	// Compute a hull-global bias for every vertex once
	auto *const __restrict globalVertexDotBias = (float *)alloca( sizeof( float ) * verticesSpan.size() );
	for( unsigned i = 0; i < verticesSpan.size(); ++i ) {
		const float *__restrict vertex = verticesSpan[i];
		// We have decided that permitting a negative bias yields better results.
		globalVertexDotBias[i] = DotProduct( vertex, globalBiasDir );
	}

	// Setup layers data
	assert( hull->numLayers >= 1 && hull->numLayers <= kMaxHullLayers );
	for( unsigned layerNum = 0; layerNum < hull->numLayers; ++layerNum ) {
		BaseConcentricSimulatedHull::Layer *layer   = &hull->layers[layerNum];
		const HullLayerParams *__restrict params    = &layerParams[layerNum];
		vec4_t *const __restrict positions          = layer->vertexPositions;
		byte_vec4_t *const __restrict colors        = layer->vertexColors;
		vec2_t *const __restrict speedsAndDistances = layer->vertexSpeedsAndDistances;
		const float *const __restrict layerBiasDir  = verticesSpan[rng->nextBounded( verticesSpan.size() )];

		assert( params->speed >= 0.0f && params->minSpeedSpike >= 0.0f && params->biasAlongChosenDir >= 0.0f );
		assert( params->minSpeedSpike < params->maxSpeedSpike );
		assert( std::fabs( VectorLengthFast( layerBiasDir ) - 1.0f ) < 0.001f );

		layer->finalOffset         = params->finalOffset;
		layer->drawOrderDesignator = (float)( hull->numLayers - layerNum );
		layer->colorChangeTimeline = params->colorChangeTimeline;

		std::fill( spikeSpeedBoost, spikeSpeedBoost + verticesSpan.size(), 0.0f );

		const float *const __restrict baseColor  = params->baseInitialColor;
		const float *const __restrict bulgeColor = params->bulgeInitialColor;

		for( size_t i = 0; i < verticesSpan.size(); ++i ) {
			// Position XYZ is computed prior to submission in stateless fashion
			positions[i][3] = 1.0f;

			const float *vertexDir   = vertices[i];
			const float layerDotBias = DotProduct( vertexDir, layerBiasDir );
			const float layerSqrBias = std::copysign( layerDotBias * layerDotBias, layerDotBias );
			const float vertexBias   = wsw::max( layerSqrBias, globalVertexDotBias[i] );

			speedsAndDistances[i][0] = params->speed + vertexBias * params->biasAlongChosenDir;

			if( rng->tryWithChance( params->speedSpikeChance ) ) [[unlikely]] {
				const float boost = rng->nextFloat( params->minSpeedSpike, params->maxSpeedSpike );
				spikeSpeedBoost[i] += boost;
				const auto &indicesOfNeighbours = neighboursSpan[i];
				for( const unsigned neighbourIndex: indicesOfNeighbours ) {
					spikeSpeedBoost[neighbourIndex] += rng->nextFloat( 0.50f, 0.75f ) * boost;
				}
			} else {
				speedsAndDistances[i][0] += rng->nextFloat( 0.0f, maxSmallRandomOffset );
			}

			speedsAndDistances[i][1] = 0.0f;

			const float colorLerpFrac  = vertexBias * vertexBias;
			const float complementFrac = 1.0f - colorLerpFrac;
			colors[i][0] = (uint8_t)( 255.0f * ( baseColor[0] * colorLerpFrac + bulgeColor[0] * complementFrac ) );
			colors[i][1] = (uint8_t)( 255.0f * ( baseColor[1] * colorLerpFrac + bulgeColor[1] * complementFrac ) );
			colors[i][2] = (uint8_t)( 255.0f * ( baseColor[2] * colorLerpFrac + bulgeColor[2] * complementFrac ) );
			colors[i][3] = (uint8_t)( 255.0f * ( baseColor[3] * colorLerpFrac + bulgeColor[3] * complementFrac ) );
		}

		for( size_t i = 0; i < verticesSpan.size(); ++i ) {
			speedsAndDistances[i][0] += wsw::min( spikeSpeedBoost[i], maxVertexSpeed );
			// Scale by the fine-tuning scale multiplier
			speedsAndDistances[i][0] *= scale;
		}
	}

	VectorCopy( origin, hull->origin );
	hull->vertexMoveDirections = vertices;

	hull->appearanceRules = appearanceRules;
}

void SimulatedHullsSystem::setupHullVertices( BaseKeyframedHull *hull, const float *origin,
											  float scale, const std::span<const OffsetKeyframe> *offsetKeyframeSets,
											  const float maxOffset, const AppearanceRules &appearanceRules ) {
	const float originX = origin[0], originY = origin[1], originZ = origin[2];
	const auto [verticesSpan, indicesSpan, neighboursSpan] = ::basicHullsHolder.getIcosphereForLevel( hull->subdivLevel );

	const vec4_t *__restrict vertices = verticesSpan.data();

	// Calculate move limits in each direction

	const float radius = maxOffset * scale;
	const vec3_t growthMins { originX - radius, originY - radius, originZ - radius };
	const vec3_t growthMaxs { originX + radius, originY + radius, originZ + radius };

	// TODO: Add a fused call
	CM_BuildShapeList( cl.cms, m_tmpShapeList, growthMins, growthMaxs, MASK_SOLID );
	CM_ClipShapeList( cl.cms, m_tmpShapeList, m_tmpShapeList, growthMins, growthMaxs );

	if( CM_GetNumShapesInShapeList( m_tmpShapeList ) == 0 ) {
		// Limits at each direction just match the given radius in this case
		std::fill( hull->limitsAtDirections, hull->limitsAtDirections + verticesSpan.size(), radius );
	} else {
		trace_t trace;
		for( size_t i = 0; i < verticesSpan.size(); ++i ) {
			// Vertices of the unit hull define directions
			const float *dir = verticesSpan[i];

			vec3_t limitPoint;
			VectorMA( origin, radius, dir, limitPoint );

			CM_ClipToShapeList( cl.cms, m_tmpShapeList, &trace, origin, limitPoint, vec3_origin, vec3_origin, MASK_SOLID );
			hull->limitsAtDirections[i] = trace.fraction * radius;
		}
	}

	// Setup layers data
	assert( hull->numLayers >= 1 && hull->numLayers <= kMaxHullLayers );
	for( unsigned layerNum = 0; layerNum < hull->numLayers; ++layerNum ) {
		BaseKeyframedHull::Layer *layer    = &hull->layers[layerNum];
		layer->drawOrderDesignator         = (float)( hull->numLayers - layerNum );
		layer->offsetKeyframeSet           = offsetKeyframeSets[layerNum];
		vec4_t *const __restrict positions = layer->vertexPositions;

		for( size_t i = 0; i < verticesSpan.size(); ++i ) {
			// Position XYZ is computed prior to submission in stateless fashion
			positions[i][3] = 1.0f;
		}
	}

	VectorCopy( origin, hull->origin );
	hull->vertexMoveDirections = vertices;
	hull->scale                = scale;

	hull->appearanceRules = appearanceRules;
}

void SimulatedHullsSystem::calcSmokeBulgeSpeedMask( float *__restrict vertexSpeedMask, unsigned subdivLevel, unsigned maxSpikes ) {
	assert( subdivLevel < 4 );
	assert( maxSpikes && maxSpikes < 10 );

	const IcosphereData &icosphereData    = ::basicHullsHolder.getIcosphereForLevel( subdivLevel );
	const vec4_t *__restrict hullVertices = icosphereData.vertices.data();
	const unsigned numHullVertices        = icosphereData.vertices.size();

	vec3_t spikeDirs[10];
	unsigned numChosenSpikes = 0;
	for( unsigned attemptNum = 0; attemptNum < 4 * maxSpikes; ++attemptNum ) {
		const unsigned vertexNum    = m_rng.nextBounded( std::size( kPredefinedDirs ) );
		const float *__restrict dir = kPredefinedDirs[vertexNum];
		if( dir[2] < -0.1f || dir[2] > 0.7f ) {
			continue;
		}

		bool foundASimilarDir = false;
		for( unsigned spikeNum = 0; spikeNum < numChosenSpikes; ++spikeNum ) {
			if( DotProduct( spikeDirs[spikeNum], dir ) > 0.7f ) {
				foundASimilarDir = true;
				break;
			}
		}
		if( !foundASimilarDir ) {
			VectorCopy( dir, spikeDirs[numChosenSpikes] );
			numChosenSpikes++;
			if( numChosenSpikes == maxSpikes ) {
				break;
			}
		}
	}

	std::fill( vertexSpeedMask, vertexSpeedMask + numHullVertices, 0.0f );

	unsigned vertexNum = 0;
	do {
		const float *__restrict vertexDir = hullVertices[vertexNum];
		float spikeStrength = 0.0f;
		unsigned spikeNum   = 0;
		do {
			// Must be non-negative to contribute to the spike strength
			const float dot = wsw::max( 0.0f, DotProduct( vertexDir, spikeDirs[spikeNum] ) );
			spikeStrength += dot * dot * dot;
		} while( ++spikeNum < numChosenSpikes );
		spikeStrength = wsw::min( 1.0f, spikeStrength );
		spikeStrength *= ( 1.0f - std::fabs( vertexDir[2] ) );
		vertexSpeedMask[vertexNum] = spikeStrength;
	} while( ++vertexNum < numHullVertices );
}

void SimulatedHullsSystem::calcSmokeSpikeSpeedMask( float *__restrict vertexSpeedMask, unsigned subdivLevel, unsigned maxSpikes ) {
	assert( subdivLevel < 4 );
	assert( maxSpikes && maxSpikes < 10 );

	const IcosphereData &icosphereData = ::basicHullsHolder.getIcosphereForLevel( subdivLevel );
	const unsigned numHullVertices     = icosphereData.vertices.size();

	unsigned spikeVertexNums[10];
	unsigned numChosenSpikes = 0;
	for( unsigned numAttempts = 0; numAttempts < 4 * maxSpikes; numAttempts++ ) {
		const unsigned vertexNum    = m_rng.nextBounded( numHullVertices );
		const float *__restrict dir = icosphereData.vertices[vertexNum];
		if( dir[2] < -0.1f || dir[2] > 0.7f ) {
			continue;
		}

		bool foundASimilarDir = false;
		for( unsigned spikeNum = 0; spikeNum < numChosenSpikes; ++spikeNum ) {
			if( DotProduct( icosphereData.vertices[spikeVertexNums[spikeNum]], dir ) > 0.7f ) {
				foundASimilarDir = true;
				break;
			}
		}
		if( !foundASimilarDir ) {
			spikeVertexNums[numChosenSpikes++] = vertexNum;
			if( numChosenSpikes == maxSpikes ) {
				break;
			}
		}
	}

	std::fill( vertexSpeedMask, vertexSpeedMask + numHullVertices, 0.0f );

	const auto *__restrict hullVertexNeighbours = icosphereData.vertexNeighbours.data();

	unsigned spikeNum = 0;
	do {
		const unsigned spikeVertexNum = spikeVertexNums[spikeNum];
		vertexSpeedMask[spikeVertexNum] += 1.0f;
		unsigned neighbourIndex = 0;
		const auto *neighbours = hullVertexNeighbours[spikeVertexNum];
		do {
			const unsigned neighbourVertexNum = neighbours[neighbourIndex];
			vertexSpeedMask[neighbourVertexNum] += 0.67f;
			const auto *nextNeighbours = hullVertexNeighbours[neighbourVertexNum];
			unsigned nextNeighbourIndex = 0;
			do {
				vertexSpeedMask[nextNeighbours[nextNeighbourIndex]] += 0.37f;
			} while( ++nextNeighbourIndex < 5 );
		} while( ++neighbourIndex < 5 );
	} while( ++spikeNum < numChosenSpikes );

	unsigned vertexNum = 0;
	do {
		vertexSpeedMask[vertexNum] = wsw::min( 1.0f, vertexSpeedMask[vertexNum] );
	} while( ++vertexNum < numHullVertices );
}

auto SimulatedHullsSystem::buildMatchingHullPairs( const BaseKeyframedHull **toonHulls, unsigned numToonHulls,
												   const BaseConcentricSimulatedHull **fireHulls, unsigned numFireHulls,
												   wsw::StaticVector<std::optional<uint8_t>, kMaxKeyframedHulls>
												       *pairIndicesForKeyframedHulls,
												   wsw::StaticVector<std::optional<uint8_t>, kMaxConcentricHulls>
												       *pairIndicesForConcentricHulls ) -> unsigned {
	pairIndicesForKeyframedHulls->clear();
	pairIndicesForConcentricHulls->clear();
	pairIndicesForKeyframedHulls->insert( pairIndicesForKeyframedHulls->end(), kMaxKeyframedHulls, std::nullopt );
	pairIndicesForConcentricHulls->insert( pairIndicesForConcentricHulls->end(), kMaxConcentricHulls, std::nullopt );

	unsigned numMatchedPairs = 0;
	// Try finding coupled hulls
	for( unsigned concentricHullIndex = 0; concentricHullIndex < numFireHulls; ++concentricHullIndex ) {
		const BaseConcentricSimulatedHull *concentricHull = fireHulls[concentricHullIndex];
		if( concentricHull->compoundMeshKey != 0 ) {
			for( unsigned keyframedHullIndex = 0; keyframedHullIndex < numToonHulls; ++keyframedHullIndex ) {
				const BaseKeyframedHull *keyframedHull = toonHulls[keyframedHullIndex];
				if( concentricHull->compoundMeshKey == keyframedHull->compoundMeshKey ) {
					( *pairIndicesForConcentricHulls )[concentricHullIndex] = (uint8_t)numMatchedPairs;
					( *pairIndicesForKeyframedHulls )[keyframedHullIndex]   = (uint8_t)numMatchedPairs;
					++numMatchedPairs;
					break;
				}
			}
		}
	}

	assert( numMatchedPairs <= kMaxToonSmokeHulls && numMatchedPairs <= kMaxFireHulls );
	return numMatchedPairs;
}

void SimulatedHullsSystem::simulateFrame( int64_t currTime ) {
	WSW_PROFILER_SCOPE();

	if( currTime != m_lastTime ) {
		assert( currTime > m_lastTime );

		// Limit the time step
		const float timeDeltaSeconds = 1e-3f * (float)wsw::min<int64_t>( 33, currTime - m_lastTime );

		for( FireHull *hull = m_fireHullsHead, *next = nullptr; hull; hull = next ) { next = hull->next;
			if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
				hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			} else {
				unlinkAndFreeFireHull( hull );
			}
		}
		for( FireClusterHull *hull = m_fireClusterHullsHead, *next = nullptr; hull; hull = next ) { next = hull->next;
			if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
				hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			} else {
				unlinkAndFreeFireClusterHull( hull );
			}
		}
		for( BlastHull *hull = m_blastHullsHead, *next = nullptr; hull; hull = next ) { next = hull->next;
			if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
				hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			} else {
				unlinkAndFreeBlastHull( hull );
			}
		}
		for( SmokeHull *hull = m_smokeHullsHead, *next = nullptr; hull; hull = next ) { next = hull->next;
			if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
				hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			} else {
				unlinkAndFreeSmokeHull( hull );
			}
		}
		for( WaveHull *hull = m_waveHullsHead, *next = nullptr; hull; hull = next ) { next = hull->next;
			if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
				hull->simulate( currTime, timeDeltaSeconds, &m_rng );
			} else {
				unlinkAndFreeWaveHull( hull );
			}
		}
		for( ToonSmokeHull *hull = m_toonSmokeHullsHead, *next = nullptr; hull; hull = next ) { next = hull->next;
			if( hull->spawnTime + hull->lifetime > currTime ) [[likely]] {
				hull->simulate( currTime, timeDeltaSeconds );
			} else {
				unlinkAndFreeToonSmokeHull( hull );
			}
		}

		m_lastTime = currTime;
	}
}

void SimulatedHullsSystem::submitToScene( int64_t currTime, DrawSceneRequest *drawSceneRequest, unsigned povPlayerMask ) {
	assert( currTime == m_lastTime );

	// TODO: We should do that while gathering results of parallel tasks

	wsw::StaticVector<BaseRegularSimulatedHull *, kMaxRegularHulls> activeRegularHulls;
	wsw::StaticVector<BaseConcentricSimulatedHull *, kMaxConcentricHulls> activeConcentricHulls;
	wsw::StaticVector<BaseKeyframedHull *, kMaxKeyframedHulls> activeKeyframedHulls;

	for( FireHull *hull = m_fireHullsHead; hull; hull = hull->next ) {
		assert( hull->spawnTime + hull->lifetime > currTime );
		if( hull->povPlayerMask & povPlayerMask ) {
			activeConcentricHulls.push_back( hull );
		}
	}
	for( FireClusterHull *hull = m_fireClusterHullsHead; hull; hull = hull->next ) {
		assert( hull->spawnTime + hull->lifetime > currTime );
		if( hull->povPlayerMask & povPlayerMask ) {
			activeConcentricHulls.push_back( hull );
		}
	}
	for( BlastHull *hull = m_blastHullsHead; hull; hull = hull->next ) {
		assert( hull->spawnTime + hull->lifetime > currTime );
		if( hull->povPlayerMask & povPlayerMask ) {
			activeConcentricHulls.push_back( hull );
		}
	}
	for( SmokeHull *hull = m_smokeHullsHead; hull; hull = hull->next ) {
		assert( hull->spawnTime + hull->lifetime > currTime );
		if( hull->povPlayerMask & povPlayerMask ) {
			activeRegularHulls.push_back( hull );
		}
	}
	for( WaveHull *hull = m_waveHullsHead; hull; hull = hull->next ) {
		assert( hull->spawnTime + hull->lifetime > currTime );
		if( hull->povPlayerMask & povPlayerMask ) {
			activeRegularHulls.push_back( hull );
		}
	}
	for( ToonSmokeHull *hull = m_toonSmokeHullsHead; hull; hull = hull->next ) {
		assert( hull->spawnTime + hull->lifetime > currTime );
		if( hull->povPlayerMask & povPlayerMask ) {
			activeKeyframedHulls.push_back( hull );
		}
	}

	for( BaseRegularSimulatedHull *__restrict hull: activeRegularHulls ) {
		const SolidAppearanceRules *solidAppearanceRules = nullptr;
		const CloudAppearanceRules *cloudAppearanceRules = nullptr;
		if( const auto *solidAndCloudRules = std::get_if<SolidAndCloudAppearanceRules>( &hull->appearanceRules ) ) {
			solidAppearanceRules = &solidAndCloudRules->solidRules;
			cloudAppearanceRules = &solidAndCloudRules->cloudRules;
		} else {
			solidAppearanceRules = std::get_if<SolidAppearanceRules>( &hull->appearanceRules );
			cloudAppearanceRules = std::get_if<CloudAppearanceRules>( &hull->appearanceRules );
		}

		assert( solidAppearanceRules || cloudAppearanceRules );
		SharedMeshData *const __restrict sharedMeshData = &hull->sharedMeshData;
		
		sharedMeshData->simulatedPositions   = hull->vertexPositions[hull->positionsFrame];
		sharedMeshData->simulatedNormals     = hull->vertexNormals;
		sharedMeshData->simulatedColors      = hull->vertexColors;
		sharedMeshData->minZLastFrame        = hull->minZLastFrame;
		sharedMeshData->maxZLastFrame        = hull->maxZLastFrame;
		sharedMeshData->minFadedOutAlpha     = hull->minFadedOutAlpha;
		sharedMeshData->viewDotFade          = hull->vertexViewDotFade;
		sharedMeshData->zFade                = hull->vertexZFade;
		sharedMeshData->simulatedSubdivLevel = hull->subdivLevel;
		sharedMeshData->tesselateClosestLod  = hull->tesselateClosestLod;
		sharedMeshData->lerpNextLevelColors  = hull->leprNextLevelColors;

		sharedMeshData->cacheEntriesInitial.clear();
		sharedMeshData->cacheEntriesHeapalloc.clear();

		sharedMeshData->hasSibling = solidAppearanceRules && cloudAppearanceRules;

		assert( hull->subdivLevel );
		if( solidAppearanceRules ) [[likely]] {
			HullSolidDynamicMesh *const __restrict mesh = &hull->submittedSolidMesh;

			Vector4Copy( hull->mins, mesh->cullMins );
			Vector4Copy( hull->maxs, mesh->cullMaxs );

			mesh->applyVertexDynLight = hull->applyVertexDynLight;
			mesh->material            = solidAppearanceRules->material;
			mesh->m_shared            = sharedMeshData;

			drawSceneRequest->addDynamicMesh( mesh );
		}

		if( cloudAppearanceRules ) [[unlikely]] {
			assert( !cloudAppearanceRules->spanOfMeshProps.empty() );
			assert( cloudAppearanceRules->spanOfMeshProps.size() <= std::size( hull->submittedCloudMeshes ) );

			const float hullLifetimeFrac = (float)( currTime - hull->spawnTime ) * Q_Rcp( (float)hull->lifetime );

			for( size_t meshNum = 0; meshNum < cloudAppearanceRules->spanOfMeshProps.size(); ++meshNum ) {
				const CloudMeshProps &__restrict meshProps  = cloudAppearanceRules->spanOfMeshProps[meshNum];
				HullCloudDynamicMesh *const __restrict mesh = hull->submittedCloudMeshes + meshNum;

				mesh->m_spriteRadius = meshProps.radiusLifespan.getValueForLifetimeFrac( hullLifetimeFrac );
				if( mesh->m_spriteRadius > 1.0f ) [[likely]] {
					mesh->m_alphaScale = meshProps.alphaScaleLifespan.getValueForLifetimeFrac( hullLifetimeFrac );
					// We don't know the final alpha as we have to multiply it by the actual color value.
					// Still, we can conclude that the multiplication result will be zero as a byte for this value.
					if( mesh->m_alphaScale >= ( 1.0f / 255.0f ) ) {
						Vector4Copy( hull->mins, mesh->cullMins );
						Vector4Copy( hull->maxs, mesh->cullMaxs );

						Vector4Copy( meshProps.overlayColor, mesh->m_spriteColor );

						mesh->material            = meshProps.material;
						mesh->applyVertexDynLight = hull->applyVertexDynLight;
						mesh->m_shared            = sharedMeshData;
						mesh->m_lifetimeSeconds   = 1e-3f * (float)( currTime - hull->spawnTime );
						mesh->m_applyRotation     = meshProps.applyRotation;

						mesh->m_tessLevelShiftForMinVertexIndex = meshProps.tessLevelShiftForMinVertexIndex;
						mesh->m_tessLevelShiftForMaxVertexIndex = meshProps.tessLevelShiftForMaxVertexIndex;
						mesh->m_shiftFromDefaultLevelToHide     = meshProps.shiftFromDefaultLevelToHide;

						// It's more convenient to initialize it on demand
						if ( !( mesh->m_speedIndexShiftInTable | mesh->m_phaseIndexShiftInTable )) [[unlikely]] {
							const auto randomWord = (uint16_t) m_rng.next();
							mesh->m_speedIndexShiftInTable = ( randomWord >> 0 ) & 0xFF;
							mesh->m_phaseIndexShiftInTable = ( randomWord >> 8 ) & 0xFF;
						}

						drawSceneRequest->addDynamicMesh( mesh );
					}
				}
			}
		}
	}

	// TODO: Track bounds
	wsw::StaticVector<std::optional<uint8_t>, kMaxKeyframedHulls> pairIndicesForKeyframedHulls;
	wsw::StaticVector<std::optional<uint8_t>, kMaxConcentricHulls> pairIndicesForConcentricHulls;
	buildMatchingHullPairs( (const BaseKeyframedHull **)activeKeyframedHulls.data(), activeKeyframedHulls.size(),
							(const BaseConcentricSimulatedHull **)activeConcentricHulls.data(), activeConcentricHulls.size(),
							&pairIndicesForKeyframedHulls, &pairIndicesForConcentricHulls );

	wsw::StaticVector<unsigned, kMaxKeyframedHulls> meshDataOffsetsForPairs;
	wsw::StaticVector<uint8_t, kMaxKeyframedHulls> numAddedMeshesForPairs;
	wsw::StaticVector<std::pair<Vec3, Vec3>, kMaxKeyframedHulls> boundsForPairs;
	wsw::StaticVector<float, kMaxKeyframedHulls> topAddedLayersForPairs;

	// We multiply given layer orders by 2 to be able to make distinct orders for solid and cloud meshes
	constexpr float solidLayerOrderBoost = 0.0f;
	constexpr float cloudLayerOrderBoost = 1.0f;

	unsigned offsetOfMultilayerMeshData = 0;

	// TODO: zipWithIndex?
	unsigned keyframedHullIndex = 0;
	for( const BaseKeyframedHull *__restrict hull: activeKeyframedHulls ) {
		assert( hull->numLayers );

		const DynamicMesh **submittedMeshesBuffer = m_storageOfSubmittedMeshPtrs.get() + offsetOfMultilayerMeshData;
		float *const submittedOrderDesignators    = m_storageOfSubmittedMeshOrderDesignators.get() + offsetOfMultilayerMeshData;

		const bool isCoupledWithConcentricHull = pairIndicesForKeyframedHulls[keyframedHullIndex] != std::nullopt;
		if( isCoupledWithConcentricHull ) {
			meshDataOffsetsForPairs.push_back( offsetOfMultilayerMeshData );
			topAddedLayersForPairs.push_back( 0.0f );
		}

		unsigned numMeshesToSubmit = 0;
		for( unsigned layerNum = 0; layerNum < hull->numLayers; ++layerNum ) {
			BaseKeyframedHull::Layer *__restrict layer = &hull->layers[layerNum];

			const AppearanceRules *appearanceRules = &hull->appearanceRules;
			if( layer->overrideAppearanceRules ) {
				appearanceRules = layer->overrideAppearanceRules;
			}

			const SolidAppearanceRules *solidAppearanceRules = nullptr;
			const CloudAppearanceRules *cloudAppearanceRules = nullptr;
			if( const auto *solidAndCloudRules = std::get_if<SolidAndCloudAppearanceRules>( appearanceRules ) ) {
				solidAppearanceRules = &solidAndCloudRules->solidRules;
				cloudAppearanceRules = &solidAndCloudRules->cloudRules;
			} else {
				solidAppearanceRules = std::get_if<SolidAppearanceRules>( appearanceRules );
				cloudAppearanceRules = std::get_if<CloudAppearanceRules>( appearanceRules );
			}

			assert( solidAppearanceRules || cloudAppearanceRules );
			SharedMeshData *const __restrict sharedMeshData = layer->sharedMeshData;

			sharedMeshData->simulatedPositions   = layer->vertexPositions;

			const unsigned currentKeyframe = layer->lastKeyframeNum;
			sharedMeshData->prevShadingLayers    = layer->offsetKeyframeSet[currentKeyframe].shadingLayers;
			sharedMeshData->nextShadingLayers    = layer->offsetKeyframeSet[currentKeyframe + 1].shadingLayers;
			sharedMeshData->lerpFrac             = layer->lerpFrac;

			sharedMeshData->simulatedNormals     = nullptr;

			sharedMeshData->minZLastFrame        = 0.0f;
			sharedMeshData->maxZLastFrame        = 0.0f;
			sharedMeshData->zFade                = ZFade::NoFade;
			sharedMeshData->simulatedSubdivLevel = hull->subdivLevel;
			sharedMeshData->tesselateClosestLod  = false;
			sharedMeshData->lerpNextLevelColors  = true;
			sharedMeshData->nextLodTangentRatio  = 0.30f;

			sharedMeshData->cacheEntriesInitial.clear();
			sharedMeshData->cacheEntriesHeapalloc.clear();

			sharedMeshData->hasSibling = solidAppearanceRules && cloudAppearanceRules;

			sharedMeshData->isAKeyframedHull = true;

			if( solidAppearanceRules ) [[likely]] {
				HullSolidDynamicMesh *__restrict mesh = layer->submittedSolidMesh;

				Vector4Copy( layer->mins, mesh->cullMins );
				Vector4Copy( layer->maxs, mesh->cullMaxs );

				// TODO: Make the material configurable
				mesh->material = cgs.shaderWhite;
				mesh->m_shared = sharedMeshData;
				// TODO: Restore this functionality if it could be useful for toon hull
				//mesh->applyVertexDynLight = hull->applyVertexDynLight;

				const float drawOrderDesignator = 2.0f * layer->drawOrderDesignator + solidLayerOrderBoost;

				submittedMeshesBuffer[numMeshesToSubmit]     = mesh;
				submittedOrderDesignators[numMeshesToSubmit] = drawOrderDesignator;

				if( isCoupledWithConcentricHull ) {
					topAddedLayersForPairs.back() = wsw::max( topAddedLayersForPairs.back(), drawOrderDesignator );
				}

				numMeshesToSubmit++;
			}

			if( cloudAppearanceRules ) [[unlikely]] {
				assert( !cloudAppearanceRules->spanOfMeshProps.empty() );
				assert( cloudAppearanceRules->spanOfMeshProps.size() <= std::size( layer->submittedCloudMeshes ) );

				const float hullLifetimeFrac = (float)( currTime - hull->spawnTime ) * Q_Rcp( (float)hull->lifetime );

				for( size_t meshNum = 0; meshNum < cloudAppearanceRules->spanOfMeshProps.size(); ++meshNum ) {
					const CloudMeshProps &__restrict meshProps  = cloudAppearanceRules->spanOfMeshProps[meshNum];
					HullCloudDynamicMesh *const __restrict mesh = layer->submittedCloudMeshes[meshNum];

					mesh->m_spriteRadius = meshProps.radiusLifespan.getValueForLifetimeFrac( hullLifetimeFrac );
					if( mesh->m_spriteRadius > 1.0f ) [[likely]] {
						mesh->m_alphaScale = meshProps.alphaScaleLifespan.getValueForLifetimeFrac( hullLifetimeFrac );
						if( mesh->m_alphaScale >= ( 1.0f / 255.0f ) ) {
							Vector4Copy( layer->mins, mesh->cullMins );
							Vector4Copy( layer->maxs, mesh->cullMaxs );

							Vector4Copy( meshProps.overlayColor, mesh->m_spriteColor );

							mesh->material            = meshProps.material;
							//mesh->applyVertexDynLight = hull->applyVertexDynLight; // TODO: restore this functionality if useful
							mesh->m_shared            = sharedMeshData;
							mesh->m_lifetimeSeconds   = 1e-3f * (float)( currTime - hull->spawnTime );
							mesh->m_applyRotation     = meshProps.applyRotation;

							mesh->m_tessLevelShiftForMinVertexIndex = meshProps.tessLevelShiftForMinVertexIndex;
							mesh->m_tessLevelShiftForMaxVertexIndex = meshProps.tessLevelShiftForMaxVertexIndex;
							mesh->m_shiftFromDefaultLevelToHide     = meshProps.shiftFromDefaultLevelToHide;

							if( !( mesh->m_speedIndexShiftInTable | mesh->m_phaseIndexShiftInTable ) ) [[unlikely]] {
								const auto randomWord          = (uint16_t)m_rng.next();
								mesh->m_speedIndexShiftInTable = ( randomWord >> 0 ) & 0xFF;
								mesh->m_phaseIndexShiftInTable = ( randomWord >> 8 ) & 0xFF;
							}

							const float drawOrderDesignator = 2.0f * layer->drawOrderDesignator + cloudLayerOrderBoost;

							submittedMeshesBuffer[numMeshesToSubmit]     = mesh;
							submittedOrderDesignators[numMeshesToSubmit] = drawOrderDesignator;

							if( isCoupledWithConcentricHull ) {
								topAddedLayersForPairs.back() = wsw::max( topAddedLayersForPairs.back(), drawOrderDesignator );
							}

							numMeshesToSubmit++;
						}
					}
				}
			}
		}

		if( numMeshesToSubmit ) [[likely]] {
			assert( numMeshesToSubmit <= kMaxMeshesPerHull );
			// Submit it right now, otherwise postpone submission to processing of concentric hulls
			if( !isCoupledWithConcentricHull ) {
				drawSceneRequest->addCompoundDynamicMesh( hull->mins, hull->maxs, submittedMeshesBuffer,
														  numMeshesToSubmit, submittedOrderDesignators );
			}
		}

		// Push the number of layers (even if we did not submit anything) to keep the addressing by pair index valid
		if( isCoupledWithConcentricHull ) {
			numAddedMeshesForPairs.push_back( numMeshesToSubmit );
			boundsForPairs.push_back( std::make_pair( Vec3( hull->mins ), Vec3( hull->maxs ) ) );
		}

		keyframedHullIndex++;

		if( isCoupledWithConcentricHull ) {
			// This leaves a sufficient space for fire hull layers
			offsetOfMultilayerMeshData += kMaxMeshesPerHull;
		} else {
			offsetOfMultilayerMeshData += numMeshesToSubmit;
		}
	}

	assert( meshDataOffsetsForPairs.size() == numAddedMeshesForPairs.size() );
	assert( meshDataOffsetsForPairs.size() == boundsForPairs.size() );
	assert( meshDataOffsetsForPairs.size() == topAddedLayersForPairs.size() );

	unsigned concentricHullIndex = 0;
	for( const BaseConcentricSimulatedHull *__restrict hull: activeConcentricHulls ) {
		assert( hull->numLayers );

		float startFromOrder;
		const DynamicMesh **submittedMeshesBuffer;
		float *submittedOrderDesignators;
		if( const std::optional<uint8_t> pairIndex = pairIndicesForConcentricHulls[concentricHullIndex] ) {
			const unsigned meshDataOffset     = meshDataOffsetsForPairs[*pairIndex];
			const unsigned numAddedToonMeshes = numAddedMeshesForPairs[*pairIndex];
			submittedMeshesBuffer             = m_storageOfSubmittedMeshPtrs.get() + meshDataOffset + numAddedToonMeshes;
			submittedOrderDesignators         = m_storageOfSubmittedMeshOrderDesignators.get() + meshDataOffset + numAddedToonMeshes;
			startFromOrder                    = topAddedLayersForPairs[*pairIndex];
		} else {
			submittedMeshesBuffer     = m_storageOfSubmittedMeshPtrs.get() + offsetOfMultilayerMeshData;
			submittedOrderDesignators = m_storageOfSubmittedMeshOrderDesignators.get() + offsetOfMultilayerMeshData;
			startFromOrder            = 0.0f;
		}

		unsigned numMeshesToSubmit = 0;
		for( unsigned layerNum = 0; layerNum < hull->numLayers; ++layerNum ) {
			BaseConcentricSimulatedHull::Layer *__restrict layer = &hull->layers[layerNum];

			const AppearanceRules *appearanceRules = &hull->appearanceRules;
			if( layer->overrideAppearanceRules ) {
				appearanceRules = layer->overrideAppearanceRules;
			}

			const SolidAppearanceRules *solidAppearanceRules = nullptr;
			const CloudAppearanceRules *cloudAppearanceRules = nullptr;
			if( const auto *solidAndCloudRules = std::get_if<SolidAndCloudAppearanceRules>( appearanceRules ) ) {
				solidAppearanceRules = &solidAndCloudRules->solidRules;
				cloudAppearanceRules = &solidAndCloudRules->cloudRules;
			} else {
				solidAppearanceRules = std::get_if<SolidAppearanceRules>( appearanceRules );
				cloudAppearanceRules = std::get_if<CloudAppearanceRules>( appearanceRules );
			}

			assert( solidAppearanceRules || cloudAppearanceRules );
			SharedMeshData *const __restrict sharedMeshData = layer->sharedMeshData;

			sharedMeshData->simulatedPositions   = layer->vertexPositions;
			sharedMeshData->simulatedNormals     = nullptr;
			sharedMeshData->simulatedColors      = layer->vertexColors;

			sharedMeshData->minZLastFrame        = 0.0f;
			sharedMeshData->maxZLastFrame        = 0.0f;
			sharedMeshData->minFadedOutAlpha     = layer->overrideMinFadedOutAlpha.value_or( hull->minFadedOutAlpha );
			sharedMeshData->viewDotFade          = layer->overrideHullFade.value_or( hull->vertexViewDotFade );
			sharedMeshData->zFade                = ZFade::NoFade;
			sharedMeshData->simulatedSubdivLevel = hull->subdivLevel;
			sharedMeshData->tesselateClosestLod  = true;
			sharedMeshData->lerpNextLevelColors  = true;

			sharedMeshData->cacheEntriesInitial.clear();
			sharedMeshData->cacheEntriesHeapalloc.clear();

			sharedMeshData->hasSibling = solidAppearanceRules && cloudAppearanceRules;

			if( solidAppearanceRules ) [[likely]] {
				HullSolidDynamicMesh *const __restrict mesh = layer->submittedSolidMesh;

				Vector4Copy( layer->mins, mesh->cullMins );
				Vector4Copy( layer->maxs, mesh->cullMaxs );
				
				mesh->material            = nullptr;
				mesh->applyVertexDynLight = hull->applyVertexDynLight;
				mesh->m_shared            = sharedMeshData;

				const float drawOrderDesignator = 2.0f * layer->drawOrderDesignator + solidLayerOrderBoost + startFromOrder;

				submittedMeshesBuffer[numMeshesToSubmit]     = mesh;
				submittedOrderDesignators[numMeshesToSubmit] = drawOrderDesignator;
				numMeshesToSubmit++;
			}

			if( cloudAppearanceRules ) [[unlikely]] {
				assert( !cloudAppearanceRules->spanOfMeshProps.empty() );
				assert( cloudAppearanceRules->spanOfMeshProps.size() <= std::size( layer->submittedCloudMeshes ) );

				const float hullLifetimeFrac = (float)( currTime - hull->spawnTime ) * Q_Rcp( (float)hull->lifetime );

				for( size_t meshNum = 0; meshNum < cloudAppearanceRules->spanOfMeshProps.size(); ++meshNum ) {
					const CloudMeshProps &__restrict meshProps  = cloudAppearanceRules->spanOfMeshProps[meshNum];
					HullCloudDynamicMesh *const __restrict mesh = layer->submittedCloudMeshes[meshNum];

					mesh->m_spriteRadius = meshProps.radiusLifespan.getValueForLifetimeFrac( hullLifetimeFrac );
					if( mesh->m_spriteRadius > 1.0f ) [[likely]] {
						mesh->m_alphaScale = meshProps.alphaScaleLifespan.getValueForLifetimeFrac( hullLifetimeFrac );
						if( mesh->m_alphaScale >= ( 1.0f / 255.0f ) ) {
							Vector4Copy( layer->mins, mesh->cullMins );
							Vector4Copy( layer->maxs, mesh->cullMaxs );

							Vector4Copy( meshProps.overlayColor, mesh->m_spriteColor );

							mesh->material            = meshProps.material;
							mesh->applyVertexDynLight = hull->applyVertexDynLight;
							mesh->m_shared            = sharedMeshData;
							mesh->m_lifetimeSeconds   = 1e-3f * (float)( currTime - hull->spawnTime );
							mesh->m_applyRotation     = meshProps.applyRotation;

							mesh->m_tessLevelShiftForMinVertexIndex = meshProps.tessLevelShiftForMinVertexIndex;
							mesh->m_tessLevelShiftForMaxVertexIndex = meshProps.tessLevelShiftForMaxVertexIndex;
							mesh->m_shiftFromDefaultLevelToHide     = meshProps.shiftFromDefaultLevelToHide;

							if( !( mesh->m_speedIndexShiftInTable | mesh->m_phaseIndexShiftInTable ) ) [[unlikely]] {
								const auto randomWord          = (uint16_t)m_rng.next();
								mesh->m_speedIndexShiftInTable = ( randomWord >> 0 ) & 0xFF;
								mesh->m_phaseIndexShiftInTable = ( randomWord >> 8 ) & 0xFF;
							}

							const float drawOrderDesignator = 2.0f * layer->drawOrderDesignator + cloudLayerOrderBoost + startFromOrder;

							submittedMeshesBuffer[numMeshesToSubmit]     = mesh;
							submittedOrderDesignators[numMeshesToSubmit] = drawOrderDesignator;
							numMeshesToSubmit++;
						}
					}
				}
			}
		}

		// If this concentric hull is coupled with a toon hull
		if( const std::optional<uint8_t> pairIndex = pairIndicesForConcentricHulls[concentricHullIndex] ) {
			// The number of meshes of the coupled toon hull
			const unsigned numAddedToonMeshes = numAddedMeshesForPairs[*pairIndex];
			// If we're going to submit something
			if( numAddedToonMeshes | numMeshesToSubmit ) [[likely]] {
				// TODO: Use some "combine bounds" subroutine
				BoundsBuilder combinedBoundsBuilder;
				combinedBoundsBuilder.addPoint( hull->mins );
				combinedBoundsBuilder.addPoint( hull->maxs );

				if( numAddedToonMeshes ) [[likely]] {
					// Roll pointers back to the data of toon hull
					submittedMeshesBuffer     -= numAddedToonMeshes;
					submittedOrderDesignators -= numAddedToonMeshes;

					combinedBoundsBuilder.addPoint( boundsForPairs[*pairIndex].first.Data() );
					combinedBoundsBuilder.addPoint( boundsForPairs[*pairIndex].second.Data() );
				}

				vec4_t combinedMins, combinedMaxs;
				combinedBoundsBuilder.storeToWithAddedEpsilon( combinedMins, combinedMaxs );
				combinedMins[3] = 0.0f, combinedMaxs[3] = 1.0f;

				const unsigned actualNumMeshesToSubmit = numAddedToonMeshes + numMeshesToSubmit;
				assert( actualNumMeshesToSubmit <= kMaxMeshesPerHull );
				drawSceneRequest->addCompoundDynamicMesh( combinedMins, combinedMaxs, submittedMeshesBuffer,
														  actualNumMeshesToSubmit, submittedOrderDesignators );
				// We add meshes to the space reserved by the toon hull, offsetOfMultilayerMeshData is kept the same
				assert( actualNumMeshesToSubmit <= kMaxMeshesPerHull );
			}
		} else {
			// Just submit meshes of this concentric hull, if any
			if( numMeshesToSubmit ) [[likely]] {
				assert( numMeshesToSubmit <= kMaxMeshesPerHull );
				drawSceneRequest->addCompoundDynamicMesh( hull->mins, hull->maxs, submittedMeshesBuffer,
														  numMeshesToSubmit, submittedOrderDesignators );
				offsetOfMultilayerMeshData += numMeshesToSubmit;
			}
		}

		++concentricHullIndex;
	}
}

void SimulatedHullsSystem::BaseRegularSimulatedHull::simulate( int64_t currTime, float timeDeltaSeconds,
															   wsw::RandomGenerator *__restrict rng ) {
	const vec4_t *const __restrict oldPositions = this->vertexPositions[positionsFrame];
	// Switch old/new positions buffer
	positionsFrame = ( positionsFrame + 1 ) % 2;
	vec4_t *const __restrict newPositions = this->vertexPositions[positionsFrame];

	vec3_t *const __restrict forceVelocities = this->vertexForceVelocities;
	vec3_t *const __restrict burstVelocities = this->vertexBurstVelocities;

	const auto &icosphereData  = ::basicHullsHolder.getIcosphereForLevel( subdivLevel );
	const unsigned numVertices = icosphereData.vertices.size();
	auto *const __restrict combinedVelocities = (vec3_t *)alloca( sizeof( vec3_t ) * numVertices );

	BoundsBuilder boundsBuilder;
	assert( timeDeltaSeconds < 0.1f );
	assert( numVertices > 0 );

	// Compute ideal positions (as if there were no obstacles)

	const float burstSpeedDecayMultiplier = 1.0f - 1.5f * timeDeltaSeconds;
	if( expansionStartAt > currTime ) {
		unsigned i = 0;
		do {
			VectorScale( burstVelocities[i], burstSpeedDecayMultiplier, burstVelocities[i] );
			VectorAdd( burstVelocities[i], forceVelocities[i], combinedVelocities[i] );
			VectorMA( oldPositions[i], timeDeltaSeconds, combinedVelocities[i], newPositions[i] );
			// TODO: We should be able to supply vec4
			boundsBuilder.addPoint( newPositions[i] );
		} while( ++i < numVertices );
	} else {
		// Having vertex normals buffer is now mandatory for hulls expansion
		const vec4_t *const __restrict normals = vertexNormals;
		assert( normals );

		const int64_t expansionMillisSoFar = currTime - expansionStartAt;
		const int64_t expansionDuration    = lifetime - ( expansionStartAt - spawnTime );

		assert( expansionMillisSoFar >= 0 );
		assert( expansionDuration > 0 );
		const float expansionFrac = (float)expansionMillisSoFar * Q_Rcp( (float)expansionDuration );
		assert( expansionFrac >= 0.0f && expansionFrac <= 1.0f );

		const float archimedesTopAccelNow     = archimedesTopAccel.getValueForLifetimeFrac( expansionFrac );
		const float archimedesBottomAccelNow  = archimedesBottomAccel.getValueForLifetimeFrac( expansionFrac );
		const float xyExpansionTopAccelNow    = xyExpansionTopAccel.getValueForLifetimeFrac( expansionFrac );
		const float xyExpansionBottomAccelNow = xyExpansionBottomAccel.getValueForLifetimeFrac( expansionFrac );

		const float rcpDeltaZ = ( maxZLastFrame - minZLastFrame ) > 0.1f ? Q_Rcp( maxZLastFrame - minZLastFrame ) : 1.0f;

		unsigned i = 0;
		do {
			const float zFrac               = ( oldPositions[i][2] - minZLastFrame ) * rcpDeltaZ;
			const float archimedesAccel     = std::lerp( archimedesBottomAccelNow, archimedesTopAccelNow, Q_Sqrt( zFrac ) );
			const float expansionAccel      = std::lerp( xyExpansionBottomAccelNow, xyExpansionTopAccelNow, zFrac );
			const float expansionMultiplier = expansionAccel * timeDeltaSeconds;

			forceVelocities[i][0] += expansionMultiplier * normals[i][0];
			forceVelocities[i][1] += expansionMultiplier * normals[i][1];
			forceVelocities[i][2] += archimedesAccel * timeDeltaSeconds;

			VectorScale( burstVelocities[i], burstSpeedDecayMultiplier, burstVelocities[i] );
			VectorAdd( burstVelocities[i], forceVelocities[i], combinedVelocities[i] );

			VectorMA( oldPositions[i], timeDeltaSeconds, combinedVelocities[i], newPositions[i] );
			// TODO: We should be able to supply vec4
			boundsBuilder.addPoint( newPositions[i] );
		} while( ++i < numVertices );
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

	minZLastFrame = std::numeric_limits<float>::max();
	maxZLastFrame = std::numeric_limits<float>::lowest();

	// int type is used to mitigate possible branching while performing bool->float conversions
	auto *const __restrict isVertexNonContacting          = (int *)alloca( sizeof( int ) * numVertices );
	auto *const __restrict indicesOfNonContactingVertices = (unsigned *)alloca( sizeof( unsigned ) * numVertices );

	trace_t clipTrace, slideTrace;
	unsigned numNonContactingVertices = 0;
	if( CM_GetNumShapesInShapeList( shapeList ) == 0 ) {
		unsigned i = 0;
		do {
			isVertexNonContacting[i]          = 1;
			indicesOfNonContactingVertices[i] = i;
		} while( ++i < numVertices );
		numNonContactingVertices = numVertices;
	} else {
		for( unsigned i = 0; i < numVertices; ++i ) {
			CM_ClipToShapeList( cl.cms, shapeList, &clipTrace, oldPositions[i], newPositions[i],
								vec3_origin, vec3_origin, MASK_SOLID );
			if( clipTrace.fraction == 1.0f ) [[likely]] {
				isVertexNonContacting[i] = 1;
				indicesOfNonContactingVertices[numNonContactingVertices++] = i;
			} else {
				isVertexNonContacting[i] = 0;
				bool putVertexAtTheContactPosition = true;

				if( const float squareSpeed = VectorLengthSquared( combinedVelocities[i] ); squareSpeed > 10.0f * 10.0f ) {
					vec3_t velocityDir;
					const float rcpSpeed = Q_RSqrt( squareSpeed );
					VectorScale( combinedVelocities[i], rcpSpeed, velocityDir );
					if( const float dot = std::fabs( DotProduct( velocityDir, clipTrace.plane.normal ) ); dot < 0.95f ) {
						const float speed               = Q_Rcp( rcpSpeed );
						const float idealMoveThisFrame  = timeDeltaSeconds * speed;
						const float distanceToObstacle  = idealMoveThisFrame * clipTrace.fraction;
						const float distanceAlongNormal = dot * distanceToObstacle;

						//   a'     c'
						//      ^ <--- ^    b' + c' = a'    | a' = lengthAlongNormal * surface normal'
						//      |     /                     | b' = -distanceToObstacle * velocity dir'
						//      |    /                      | c' = slide vec
						//      |   / b'                    | P  = trace endpos
						//      |  /
						//      | /
						// ____ |/__________
						//      P

						// c = a - b;

						vec3_t normalVec;
						VectorScale( clipTrace.plane.normal, distanceAlongNormal, normalVec );
						vec3_t vecToObstacle;
						VectorScale( velocityDir, -distanceToObstacle, vecToObstacle );
						vec3_t slideVec;
						VectorSubtract( normalVec, vecToObstacle, slideVec );

						// If the slide distance is sufficient for checks
						if( VectorLengthSquared( slideVec ) > 1.0f * 1.0f ) {
							vec3_t slideStartPoint, slideEndPoint;
							// Add an offset from the surface while testing sliding
							VectorAdd( clipTrace.endpos, clipTrace.plane.normal, slideStartPoint );
							VectorAdd( slideStartPoint, slideVec, slideEndPoint );

							CM_ClipToShapeList( cl.cms, shapeList, &slideTrace, slideStartPoint, slideEndPoint,
												vec3_origin, vec3_origin, MASK_SOLID );
							if( slideTrace.fraction == 1.0f ) {
								VectorCopy( slideEndPoint, newPositions[i] );
								putVertexAtTheContactPosition = false;
								// TODO: Modify velocity as well?
							}
						}
					}
				}

				if( putVertexAtTheContactPosition ) {
					VectorAdd( clipTrace.endpos, clipTrace.plane.normal, newPositions[i] );
				}

				// The final position for contacting vertices is considered to be known.
				const float *__restrict position = newPositions[i];
				minZLastFrame = wsw::min( minZLastFrame, position[2] );
				maxZLastFrame = wsw::max( maxZLastFrame, position[2] );

				// Make the contacting vertex transparent
				vertexColors[i][3] = 0;
			}
		}
	}

	if( numNonContactingVertices ) {
		// Update positions of non-contacting vertices
		unsigned i;
		if( numNonContactingVertices != numVertices ) {
			auto *const __restrict neighboursForVertices = icosphereData.vertexNeighbours.data();
			auto *const __restrict neighboursFreeMoveSum = (int *)alloca( sizeof( int ) * numNonContactingVertices );

			std::memset( neighboursFreeMoveSum, 0, sizeof( float ) * numNonContactingVertices );

			i = 0;
			do {
				const unsigned vertexIndex = indicesOfNonContactingVertices[i];
				assert( vertexIndex < numVertices );
				for( const unsigned neighbourIndex: neighboursForVertices[vertexIndex] ) {
					// Add to the integer accumulator
					neighboursFreeMoveSum[i] += isVertexNonContacting[neighbourIndex];
				}
			} while( ++i < numNonContactingVertices );

			assert( std::size( neighboursForVertices[0] ) == 5 );
			constexpr float rcpNumVertexNeighbours = 0.2f;

			i = 0;
			do {
				const unsigned vertexIndex = indicesOfNonContactingVertices[i];
				const float neighboursFrac = (float)neighboursFreeMoveSum[i] * rcpNumVertexNeighbours;
				assert( neighboursFrac >= 0.0f && neighboursFrac <= 1.0f );
				const float lerpFrac = ( 1.0f / 3.0f ) + ( 2.0f / 3.0f ) * neighboursFrac;

				float *const __restrict position = newPositions[vertexIndex];
				VectorLerp( oldPositions[vertexIndex], lerpFrac, position, position );

				minZLastFrame = wsw::min( minZLastFrame, position[2] );
				maxZLastFrame = wsw::max( maxZLastFrame, position[2] );
			} while( ++i < numNonContactingVertices );
		} else {
			// Just update positions.
			// This is worth the separate branch as its quite common for smoke hulls.
			i = 0;
			do {
				const unsigned vertexIndex       = indicesOfNonContactingVertices[i];
				const float *__restrict position = newPositions[vertexIndex];
				minZLastFrame = wsw::min( minZLastFrame, position[2] );
				maxZLastFrame = wsw::max( maxZLastFrame, position[2] );
			} while( ++i < numNonContactingVertices );
		}
	}

	// Once positions are defined, recalculate vertex normals
	if( vertexNormals ) {
		const auto neighboursOfVertices = icosphereData.vertexNeighbours.data();
		unsigned vertexNum = 0;
		do {
			const uint16_t *const neighboursOfVertex = neighboursOfVertices[vertexNum];
			const float *const __restrict currVertex = newPositions[vertexNum];
			float *const __restrict normal           = vertexNormals[vertexNum];

			unsigned neighbourIndex = 0;
			Vector4Clear( normal );
			do {
				const float *__restrict v2 = newPositions[neighboursOfVertex[neighbourIndex]];
				const float *__restrict v3 = newPositions[neighboursOfVertex[( neighbourIndex + 1 ) % 5]];
				vec3_t currTo2, currTo3, cross;
				VectorSubtract( v2, currVertex, currTo2 );
				VectorSubtract( v3, currVertex, currTo3 );
				CrossProduct( currTo2, currTo3, cross );
				if( const float squaredLength = VectorLengthSquared( cross ); squaredLength > 1.0f ) [[likely]] {
					const float rcpLength = Q_RSqrt( squaredLength );
					VectorMA( normal, rcpLength, cross, normal );
				}
			} while( ++neighbourIndex < 5 );

			// The sum of partial non-zero directories could be zero, check again
			if( const float squaredLength = VectorLengthSquared( normal ); squaredLength > wsw::square( 1e-3f ) ) [[likely]] {
				const float rcpLength = Q_RSqrt( squaredLength );
				VectorScale( normal, rcpLength, normal );
			} else {
				// Copy the unit vector of the original position as a normal
				VectorCopy( icosphereData.vertices[vertexNum], normal );
			}
		} while( ++vertexNum < numVertices );
	}

	const bool hasChangedColors = processColorChange( currTime, spawnTime, lifetime, colorChangeTimeline,
													  { vertexColors, numVertices }, &colorChangeState, rng );
	if( hasChangedColors && noColorChangeVertexColor && !noColorChangeIndices.empty() ) {
		for( const auto index: noColorChangeIndices ) {
			Vector4Copy( noColorChangeVertexColor, vertexColors[index] );
		}
	}
}

void SimulatedHullsSystem::BaseConcentricSimulatedHull::simulate( int64_t currTime, float timeDeltaSeconds,
																  wsw::RandomGenerator *__restrict rng ) {
	// Just move all vertices along directions clipping by limits

	BoundsBuilder hullBoundsBuilder;

	const float *__restrict growthOrigin = this->origin;
	const unsigned numVertices = basicHullsHolder.getIcosphereForLevel( subdivLevel ).vertices.size();

	const float speedMultiplier = 1.0f - 1.5f * timeDeltaSeconds;


	// Sanity check
	assert( numLayers >= 1 && numLayers <= kMaxHullLayers );
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
			const float limit = wsw::max( 0.0f, limitsAtDirections[i] - finalOffset );
			distanceSoFar = wsw::min( distanceSoFar, limit );

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

	for( unsigned i = 0; i < numLayers; ++i ) {
		Layer *const layer = &layers[i];
		processColorChange( currTime, spawnTime, lifetime, layer->colorChangeTimeline,
							{ layer->vertexColors, numVertices }, &layer->colorChangeState, rng );
	}
}

void SimulatedHullsSystem::BaseKeyframedHull::simulate( int64_t currTime, float timeDeltaSeconds ) {
	const float lifetimeFrac         = (float)( currTime - spawnTime ) * Q_Rcp( (float)lifetime );
	// Just move all vertices along directions clipping by limits

	BoundsBuilder hullBoundsBuilder;

	const float *__restrict growthOrigin = this->origin;
	const unsigned numVertices = basicHullsHolder.getIcosphereForLevel( subdivLevel ).vertices.size();

	// Sanity check
	assert( numLayers >= 1 && numLayers < kMaxHullLayers );
	for( unsigned layerNum = 0; layerNum < numLayers; ++layerNum ) {
		BoundsBuilder layerBoundsBuilder;

		BaseKeyframedHull::Layer *__restrict layer  = &layers[layerNum];
		vec4_t *const __restrict positions          = layer->vertexPositions;
		const auto offsetKeyframeSet                = layer->offsetKeyframeSet;

		layer->lastKeyframeNum  = computePrevKeyframeIndex( layer->lastKeyframeNum, currTime, spawnTime,
															lifetime, offsetKeyframeSet );

		const OffsetKeyframe &__restrict currKeyframe = layer->offsetKeyframeSet[layer->lastKeyframeNum];
		const OffsetKeyframe &__restrict nextKeyframe = layer->offsetKeyframeSet[layer->lastKeyframeNum + 1];

		const float offsetInFrame        = lifetimeFrac - currKeyframe.lifetimeFraction;
		const float frameLength          = nextKeyframe.lifetimeFraction - currKeyframe.lifetimeFraction;
		assert( offsetInFrame >= 0.0f && offsetInFrame <= frameLength );
		const float fracFromCurrKeyframe = wsw::clamp( offsetInFrame * Q_Rcp( frameLength ), 0.0f, 1.0f );
		layer->lerpFrac                  = fracFromCurrKeyframe;

		const float finalOffset = layer->finalOffset;
		unsigned vertexNum      = 0;
		do {
			float offset = std::lerp( currKeyframe.offsets[vertexNum],
									  nextKeyframe.offsets[vertexNum],
									  fracFromCurrKeyframe );
			offset *= scale;

			float offsetFromLimit = std::lerp( currKeyframe.offsetsFromLimit[vertexNum],
											   nextKeyframe.offsetsFromLimit[vertexNum],
											   fracFromCurrKeyframe );
			offsetFromLimit *= scale;

			// Limit growth by the precomputed obstacle distance
			const float limit = wsw::max( 0.0f, limitsAtDirections[vertexNum] - offsetFromLimit );
			offset = wsw::min( offset - finalOffset, limit );

			VectorMA( growthOrigin, offset, vertexMoveDirections[vertexNum], positions[vertexNum] );

			// TODO: Allow supplying 4-component in-memory vectors directly
			layerBoundsBuilder.addPoint( positions[vertexNum] );
		} while( ++vertexNum < numVertices );

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
}

auto SimulatedHullsSystem::computePrevKeyframeIndex( unsigned startFromIndex, int64_t currTime,
													 int64_t spawnTime, unsigned effectDuration,
													 std::span<const OffsetKeyframe> offsetKeyframeSet ) -> unsigned {
	const auto currLifetimeFraction = (float)( currTime - spawnTime ) * Q_Rcp( (float)effectDuration );
	unsigned setSize = offsetKeyframeSet.size();

	// Assume that startFromIndex is "good" even if the conditions in the loop won't be held for it
	unsigned currIndex = startFromIndex, lastGoodIndex = startFromIndex;
	for(;; ) {
		if( currIndex == ( setSize - 1 ) ) { // it should never reach the last keyframe as we always need a next one for interpolation
			break;
		}
		if( offsetKeyframeSet[currIndex].lifetimeFraction > currLifetimeFraction ) {
			break;
		}
		lastGoodIndex = currIndex;
		currIndex++;
	}

	return lastGoodIndex;
}

auto SimulatedHullsSystem::computeCurrTimelineNodeIndex( unsigned startFromIndex, int64_t currTime,
														 int64_t spawnTime, unsigned effectDuration,
														 std::span<const ColorChangeTimelineNode> timeline )
	-> unsigned {
	// Sanity checks
	assert( effectDuration && effectDuration < std::numeric_limits<uint16_t>::max() );
	assert( currTime - spawnTime >= 0 && currTime - spawnTime < std::numeric_limits<uint16_t>::max() );

	assert( startFromIndex < timeline.size() );

	const auto currLifetimeFraction = (float)( currTime - spawnTime ) * Q_Rcp( (float)effectDuration );
	assert( currLifetimeFraction >= 0.0f && currLifetimeFraction <= 1.001f );

	// Assume that startFromIndex is "good" even if the conditions in the loop won't be held for it
	unsigned currIndex = startFromIndex, lastGoodIndex = startFromIndex;
	for(;; ) {
		if( currIndex == timeline.size() ) {
			break;
		}
		if( timeline[currIndex].activateAtLifetimeFraction > currLifetimeFraction ) {
			break;
		}
		lastGoodIndex = currIndex;
		currIndex++;
	}

	return lastGoodIndex;
}

enum ColorChangeFlags : unsigned { MayDrop = 0x1, MayReplace = 0x2, MayIncreaseOpacity = 0x4, AssumePow2Palette = 0x8 };

template <unsigned Flags>
static void changeColors( std::span<byte_vec4_t> colorsSpan, wsw::RandomGenerator *__restrict rng,
						  [[maybe_unused]] std::span<const byte_vec4_t> replacementPalette,
						  [[maybe_unused]] float dropChance, [[maybe_unused]] float replacementChance ) {

	byte_vec4_t *const __restrict colors = colorsSpan.data();
	const unsigned numColors             = colorsSpan.size();

	[[maybe_unused]] unsigned paletteIndexMask = 0;
	if constexpr( Flags & AssumePow2Palette ) {
		paletteIndexMask = (unsigned)( replacementPalette.size() - 1 );
	}

	unsigned i = 0;
	do {
		// Don't process elements that became void
		if( colors[i][3] != 0 ) [[likely]] {
			[[maybe_unused]] bool dropped = false;
			if constexpr( Flags & MayDrop ) {
				if( rng->tryWithChance( dropChance ) ) [[unlikely]] {
					colors[i][3] = 0;
					dropped = true;
				}
			}
			if constexpr( Flags & MayReplace ) {
				if( !dropped ) {
					if( rng->tryWithChance( replacementChance ) ) [[unlikely]] {
						const uint8_t *chosenColor;
						if constexpr( Flags & AssumePow2Palette ) {
							chosenColor = replacementPalette[rng->next() & paletteIndexMask];
						} else {
							chosenColor = replacementPalette[rng->nextBounded( replacementPalette.size() ) ];
						}
						auto *const existingColor = colors[i];
						if constexpr( Flags & MayIncreaseOpacity ) {
							Vector4Copy( chosenColor, existingColor );
						} else {
							// Branching could be perfectly avoided in this case
							// (replacement is actually not that "unlikely" for some parts of hull lifetimes
							// so slightly optimizing it makes sense).
							const uint8_t *srcColorsToSelect[2] { existingColor, chosenColor };
							const uint8_t *selectedColor = srcColorsToSelect[chosenColor[3] <= existingColor[3]];
							Vector4Copy( selectedColor, existingColor );
						}
					}
				}
			}
		}
	} while( ++i < numColors );
}

static const struct ColorChangeFnTableHolder {
	using Fn = void (*)( std::span<byte_vec4_t>, wsw::RandomGenerator *, std::span<const byte_vec4_t>, float, float );
	Fn table[16] {};

#define ADD_FN_FOR_FLAGS_TO_TABLE( Flags ) do { table[Flags] = &::changeColors<Flags>; } while( 0 )
	ColorChangeFnTableHolder() noexcept {
		// Add a reference to a template instantiation for each feasible combination of flags.
		// (Don't force generating code for unfeasible ones
		// by a brute-force instantiation of templates for all 16 values).
		ADD_FN_FOR_FLAGS_TO_TABLE( MayDrop | MayReplace | MayIncreaseOpacity | AssumePow2Palette );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayDrop | MayReplace | MayIncreaseOpacity );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayDrop | MayReplace | AssumePow2Palette );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayDrop | MayReplace );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayReplace | MayIncreaseOpacity | AssumePow2Palette );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayReplace | MayIncreaseOpacity );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayReplace | AssumePow2Palette );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayReplace );
		ADD_FN_FOR_FLAGS_TO_TABLE( MayDrop );
	}
#undef ADD_FN_FOR_FLAGS_TO_TABLE
} colorChangeFnTableHolder;

bool SimulatedHullsSystem::processColorChange( int64_t currTime,
											   int64_t spawnTime,
											   unsigned effectDuration,
											   std::span<const ColorChangeTimelineNode> timeline,
											   std::span<byte_vec4_t> colorsSpan,
											   ColorChangeState *__restrict state,
											   wsw::RandomGenerator *__restrict rng ) {
	// This helps to handle non-color-changing hulls in a least-effort fashion
	if( state->lastNodeIndex >= timeline.size() ) [[unlikely]] {
		return false;
	}

	// Compute the current node in an immediate mode. This is inexpensive for a realistic input.
	state->lastNodeIndex = computeCurrTimelineNodeIndex( state->lastNodeIndex, currTime, spawnTime,
														 effectDuration, timeline );

	constexpr int64_t colorChangeIntervalMillis = 15;

	const ColorChangeTimelineNode &currNode = timeline[state->lastNodeIndex];
	if( state->lastColorChangeAt + colorChangeIntervalMillis > currTime ) [[likely]] {
		return false;
	}

	// Do nothing during the first frame
	if( state->lastColorChangeAt <= 0 ) [[unlikely]] {
		state->lastColorChangeAt = currTime;
		return false;
	}

	const auto timeDeltaSeconds = 1e-3f * (float)( currTime - state->lastColorChangeAt );
	assert( timeDeltaSeconds >= 1e-3f * (float)colorChangeIntervalMillis );
	state->lastColorChangeAt = currTime;

	float currSegmentLifetimePercentage;
	if( state->lastNodeIndex + 1 < timeline.size() ) {
		const ColorChangeTimelineNode &nextNode = timeline[state->lastNodeIndex + 1];
		currSegmentLifetimePercentage = nextNode.activateAtLifetimeFraction - currNode.activateAtLifetimeFraction;
	} else {
		currSegmentLifetimePercentage = 1.0f - currNode.activateAtLifetimeFraction;
	}

	assert( currSegmentLifetimePercentage >= 0.0f && currSegmentLifetimePercentage <= 1.0f );
	assert( currSegmentLifetimePercentage > 1e-3f );

	// Reconstruct duration of this timeline segment
	const float currSegmentDurationSeconds = currSegmentLifetimePercentage * ( 1e-3f * (float)effectDuration );

	float dropChance, replacementChance;
	// Protect from going out of value bounds (e.g. after a freeze due to external reasons)
	if( timeDeltaSeconds < currSegmentDurationSeconds ) [[likely]] {
		// Compute how much this color change frame takes of the segment
		// TODO: Don't convert to seconds prior to the division?
		const auto currFrameFractionOfTheSegment = timeDeltaSeconds * Q_Rcp( currSegmentDurationSeconds );
		assert( currFrameFractionOfTheSegment > 0.0f && currFrameFractionOfTheSegment <= 1.0f );
		// Convert accumulative chances for the entire segment to chances for this color change frame.
		dropChance        = currNode.sumOfDropChanceForThisSegment * currFrameFractionOfTheSegment;
		replacementChance = currNode.sumOfReplacementChanceForThisSegment * currFrameFractionOfTheSegment;
	} else {
		dropChance        = currNode.sumOfDropChanceForThisSegment;
		replacementChance = currNode.sumOfReplacementChanceForThisSegment;
	}

	// Don't let the chance drop to zero if the specified integral chance is non-zero.
	// Don't let it exceed 1.0 as well during rapid color changes.
	constexpr float minDropChance = 1e-3f, minReplacementChance = 1e-3f;
	if( currNode.sumOfDropChanceForThisSegment > 0.0f ) {
		dropChance = wsw::clamp( dropChance, minDropChance, 1.0f );
	}
	if( currNode.sumOfReplacementChanceForThisSegment > 0.0f ) {
		replacementChance = wsw::clamp( replacementChance, minReplacementChance, 1.0f );
	}

	const auto palette    = currNode.replacementPalette;
	const bool mayDrop    = dropChance > 0.0f;
	const bool mayReplace = replacementChance > 0.0f && !palette.empty();

	if( mayDrop | mayReplace ) {
		const bool isPalettePow2  = ( palette.size() & ( palette.size() - 1u ) ) == 0;
		const unsigned dropBit    = ( mayDrop ) ? MayDrop : 0;
		const unsigned replaceBit = ( mayReplace ) ? MayReplace : 0;
		const unsigned opacityBit = ( currNode.allowIncreasingOpacity ) ? MayIncreaseOpacity : 0;
		const unsigned pow2Bit    = ( mayReplace & isPalettePow2 ) ? AssumePow2Palette : 0;
		const unsigned fnIndex    = dropBit | replaceBit | opacityBit | pow2Bit;

		// Call the respective template specialization for flags
		::colorChangeFnTableHolder.table[fnIndex]( colorsSpan, rng, palette, dropChance, replacementChance );
	}

	return true;
}

class MeshTesselationHelper {
public:
	vec4_t *m_tmpTessPositions { nullptr };
	vec4_t *m_tmpTessFloatColors { nullptr };
	int8_t *m_tessNeighboursCount { nullptr };
	vec4_t *m_tessPositions { nullptr };
	byte_vec4_t *m_tessByteColors { nullptr };
	uint16_t *m_neighbourLessVertices { nullptr };
	bool *m_isVertexNeighbourLess { nullptr };

	std::unique_ptr<uint8_t[]> m_allocationBuffer;
	unsigned m_lastNumNextLevelVertices { 0 };
	unsigned m_lastAllocationSize { 0 };

	template <bool SmoothColors>
	void exec( const SimulatedHullsSystem::HullDynamicMesh *mesh, const byte_vec4_t *overrideColors, unsigned numSimulatedVertices,
			   unsigned numNextLevelVertices, const uint16_t nextLevelNeighbours[][5] );
private:
	static constexpr unsigned kAlignment = 16;

	void setupBuffers( unsigned numSimulatedVertices, unsigned numNextLevelVertices );

	template <bool SmoothColors>
	void runPassOverOriginalVertices( const SimulatedHullsSystem::HullDynamicMesh *mesh, const byte_vec4_t *overrideColors,
									  unsigned numSimulatedVertices, const uint16_t nextLevelNeighbours[][5] );
	template <bool SmoothColors>
	[[nodiscard]]
	auto collectNeighbourLessVertices( unsigned numSimulatedVertices, unsigned numNextLevelVertices ) -> unsigned;
	template <bool SmoothColors>
	void processNeighbourLessVertices( unsigned numNeighbourLessVertices, const uint16_t nextLevelNeighbours[][5] );
	template <bool SmoothColors>
	void runSmoothVerticesPass( unsigned numNextLevelVertices, const uint16_t nextLevelNeighbours[][5] );
};

template <bool SmoothColors>
void MeshTesselationHelper::exec( const SimulatedHullsSystem::HullDynamicMesh *mesh, const byte_vec4_t *overrideColors,
								  unsigned numSimulatedVertices, unsigned numNextLevelVertices,
								  const uint16_t nextLevelNeighbours[][5] ) {
	setupBuffers( numSimulatedVertices, numNextLevelVertices );

	runPassOverOriginalVertices<SmoothColors>( mesh, overrideColors, numSimulatedVertices, nextLevelNeighbours );
	unsigned numNeighbourLessVertices = collectNeighbourLessVertices<SmoothColors>( numSimulatedVertices, numNextLevelVertices );
	processNeighbourLessVertices<SmoothColors>( numNeighbourLessVertices, nextLevelNeighbours );
	runSmoothVerticesPass<SmoothColors>( numNextLevelVertices, nextLevelNeighbours );
}

void MeshTesselationHelper::setupBuffers( unsigned numSimulatedVertices, unsigned numNextLevelVertices ) {
	if( m_lastNumNextLevelVertices != numNextLevelVertices ) {
		// Compute offsets from the base ptr
		wsw::MemSpecBuilder memSpecBuilder( wsw::MemSpecBuilder::initiallyEmpty() );

		const auto tmpTessPositionsSpec       = memSpecBuilder.addAligned<vec4_t>( numNextLevelVertices, kAlignment );
		const auto tmpTessFloatColorsSpec     = memSpecBuilder.addAligned<vec4_t>( numNextLevelVertices, kAlignment );
		const auto tessPositionsSpec          = memSpecBuilder.addAligned<vec4_t>( numNextLevelVertices, kAlignment );
		const auto tmpTessNeighboursCountSpec = memSpecBuilder.add<int8_t>( numNextLevelVertices );
		const auto tessByteColorsSpec         = memSpecBuilder.add<byte_vec4_t>( numNextLevelVertices );
		const auto neighbourLessVerticesSpec  = memSpecBuilder.add<uint16_t>( numNextLevelVertices );
		const auto isNextVertexNeighbourLess  = memSpecBuilder.add<bool>( numNextLevelVertices );

		if ( m_lastAllocationSize < memSpecBuilder.sizeSoFar() ) [[unlikely]] {
			m_lastAllocationSize = memSpecBuilder.sizeSoFar();
			m_allocationBuffer   = std::make_unique<uint8_t[]>( m_lastAllocationSize );
		}

		void *const basePtr     = m_allocationBuffer.get();
		m_tmpTessPositions      = tmpTessPositionsSpec.get( basePtr );
		m_tmpTessFloatColors    = tmpTessFloatColorsSpec.get( basePtr );
		m_tessNeighboursCount   = tmpTessNeighboursCountSpec.get( basePtr );
		m_tessPositions         = tessPositionsSpec.get( basePtr );
		m_tessByteColors        = tessByteColorsSpec.get( basePtr );
		m_neighbourLessVertices = neighbourLessVerticesSpec.get( basePtr );
		m_isVertexNeighbourLess = isNextVertexNeighbourLess.get( basePtr );

		m_lastNumNextLevelVertices = numNextLevelVertices;
	}

	assert( numSimulatedVertices && numNextLevelVertices && numSimulatedVertices < numNextLevelVertices );
	const unsigned numAddedVertices = numNextLevelVertices - numSimulatedVertices;

	std::memset( m_tessPositions, 0, sizeof( m_tessPositions[0] ) * numNextLevelVertices );
	std::memset( m_tessByteColors, 0, sizeof( m_tessByteColors[0] ) * numNextLevelVertices );
	std::memset( m_isVertexNeighbourLess, 0, sizeof( m_isVertexNeighbourLess[0] ) * numNextLevelVertices );

	std::memset( m_tmpTessPositions + numSimulatedVertices, 0, sizeof( m_tmpTessPositions[0] ) * numAddedVertices );
	std::memset( m_tmpTessFloatColors + numSimulatedVertices, 0, sizeof( m_tmpTessFloatColors[0] ) * numAddedVertices );
	std::memset( m_tessNeighboursCount + numSimulatedVertices, 0, sizeof( m_tessNeighboursCount[0] ) * numAddedVertices );
}

template <bool SmoothColors>
void MeshTesselationHelper::runPassOverOriginalVertices( const SimulatedHullsSystem::HullDynamicMesh *mesh,
														 const byte_vec4_t *overrideColors,
														 unsigned numSimulatedVertices,
														 const uint16_t nextLevelNeighbours[][5] ) {
	vec4_t *const __restrict tmpTessFloatColors  = std::assume_aligned<kAlignment>( m_tmpTessFloatColors );
	vec4_t *const __restrict tmpTessPositions    = std::assume_aligned<kAlignment>( m_tmpTessPositions );
	const byte_vec4_t *const __restrict colors   = overrideColors ? overrideColors : mesh->m_shared->simulatedColors;
	byte_vec4_t *const __restrict tessByteColors = m_tessByteColors;
	int8_t *const __restrict tessNeighboursCount = m_tessNeighboursCount;

	// For each vertex in the original mesh
	for( unsigned vertexIndex = 0; vertexIndex < numSimulatedVertices; ++vertexIndex ) {
		const auto byteColor  = colors[vertexIndex];
		const float *position = mesh->m_shared->simulatedPositions[vertexIndex];

		if constexpr( SmoothColors ) {
			// Write the color to the accum buffer for smoothing it later
			Vector4Copy( byteColor, tmpTessFloatColors[vertexIndex] );
		} else {
			// Write the color directly to the resulting color buffer
			Vector4Copy( byteColor, tessByteColors[vertexIndex] );
		}

		// Copy for the further smooth pass
		Vector4Copy( position, tmpTessPositions[vertexIndex] );

		// For each neighbour of this vertex in the tesselated mesh
		for( const unsigned neighbourIndex: nextLevelNeighbours[vertexIndex] ) {
			if( neighbourIndex >= numSimulatedVertices ) {
				VectorAdd( tmpTessPositions[neighbourIndex], position, tmpTessPositions[neighbourIndex] );
				// Add integer values as is to the float accumulation buffer
				Vector4Add( tmpTessFloatColors[neighbourIndex], byteColor, tmpTessFloatColors[neighbourIndex] );
				tessNeighboursCount[neighbourIndex]++;
			}
		}
	}
}

template <bool SmoothColors>
auto MeshTesselationHelper::collectNeighbourLessVertices( unsigned numSimulatedVertices,
														  unsigned numNextLevelVertices ) -> unsigned {
	assert( numSimulatedVertices && numNextLevelVertices && numSimulatedVertices < numNextLevelVertices );

	vec4_t *const __restrict tmpTessPositions        = std::assume_aligned<kAlignment>( m_tmpTessPositions );
	vec4_t *const __restrict  tmpTessFloatColors     = std::assume_aligned<kAlignment>( m_tmpTessFloatColors );
	int8_t *const __restrict tessNeighboursCount     = m_tessNeighboursCount;
	uint16_t *const __restrict neighbourLessVertices = m_neighbourLessVertices;
	bool *const __restrict isVertexNeighbourLess     = m_isVertexNeighbourLess;
	byte_vec4_t *const __restrict tessByteColors     = m_tessByteColors;

	unsigned numNeighbourLessVertices = 0;
	for( unsigned vertexIndex = numSimulatedVertices; vertexIndex < numNextLevelVertices; ++vertexIndex ) {
		// Wtf? how do such vertices exist?
		if( !tessNeighboursCount[vertexIndex] ) [[unlikely]] {
			neighbourLessVertices[numNeighbourLessVertices++] = vertexIndex;
			isVertexNeighbourLess[vertexIndex] = true;
			continue;
		}

		const float scale = Q_Rcp( (float)tessNeighboursCount[vertexIndex] );
		VectorScale( tmpTessPositions[vertexIndex], scale, tmpTessPositions[vertexIndex] );
		tmpTessPositions[vertexIndex][3] = 1.0f;

		if constexpr( SmoothColors ) {
			// Just scale by the averaging multiplier
			Vector4Scale( tmpTessFloatColors[vertexIndex], scale, tmpTessFloatColors[vertexIndex] );
		} else {
			// Write the vertex color directly to the resulting color buffer
			Vector4Scale( tmpTessFloatColors[vertexIndex], scale, tessByteColors[vertexIndex] );
		}
	}

	return numNeighbourLessVertices;
}

template <bool SmoothColors>
void MeshTesselationHelper::processNeighbourLessVertices( unsigned numNeighbourLessVertices,
														  const uint16_t nextLevelNeighbours[][5] ) {
	vec4_t *const __restrict tmpTessFloatColors            = std::assume_aligned<kAlignment>( m_tmpTessFloatColors );
	const uint16_t *const __restrict neighbourLessVertices = m_neighbourLessVertices;
	const bool *const __restrict isVertexNeighbourLess     = m_isVertexNeighbourLess;
	byte_vec4_t *const __restrict tessByteColors           = m_tessByteColors;

	// Hack for neighbour-less vertices: apply a gathering pass
	// (the opposite to what we do for each vertex in the original mesh)
	for( unsigned i = 0; i < numNeighbourLessVertices; ++i ) {
		const unsigned vertexIndex = neighbourLessVertices[i];
		alignas( 16 ) vec4_t accumulatedColor { 0.0f, 0.0f, 0.0f, 0.0f };
		unsigned numAccumulatedColors = 0;
		for( unsigned neighbourIndex: nextLevelNeighbours[vertexIndex] ) {
			if( !isVertexNeighbourLess[neighbourIndex] ) [[likely]] {
				numAccumulatedColors++;
				if constexpr( SmoothColors ) {
					const float *const __restrict neighbourColor = tmpTessFloatColors[neighbourIndex];
					Vector4Add( neighbourColor, accumulatedColor, accumulatedColor );
				} else {
					const uint8_t *const __restrict neighbourColor = tessByteColors[neighbourIndex];
					Vector4Add( neighbourColor, accumulatedColor, accumulatedColor );
				}
			}
		}
		if( numAccumulatedColors ) [[likely]] {
			const float scale = Q_Rcp( (float)numAccumulatedColors );
			if constexpr( SmoothColors ) {
				Vector4Scale( accumulatedColor, scale, tmpTessFloatColors[vertexIndex] );
			} else {
				Vector4Scale( accumulatedColor, scale, tessByteColors[vertexIndex] );
			}
		}
	}
}

template <bool SmoothColors>
void MeshTesselationHelper::runSmoothVerticesPass( unsigned numNextLevelVertices,
												   const uint16_t nextLevelNeighbours[][5] ) {
	vec4_t *const __restrict tmpTessPositions    = std::assume_aligned<kAlignment>( m_tmpTessPositions );
	vec4_t *const __restrict tmpTessFloatColors  = std::assume_aligned<kAlignment>( m_tmpTessFloatColors );
	vec4_t *const __restrict tessPositions       = std::assume_aligned<kAlignment>( m_tessPositions );
	byte_vec4_t *const __restrict tessByteColors = m_tessByteColors;

	// Each icosphere vertex has 5 neighbours (we don't make a distinction between old and added ones for this pass)
	constexpr float icoAvgScale = 1.0f / 5.0f;
	constexpr float smoothFrac  = 0.5f;

	// Apply the smooth pass
	if constexpr( SmoothColors ) {
		for( unsigned vertexIndex = 0; vertexIndex < numNextLevelVertices; ++vertexIndex ) {
			alignas( 16 ) vec4_t sumOfNeighbourPositions { 0.0f, 0.0f, 0.0f, 0.0f };
			alignas( 16 ) vec4_t sumOfNeighbourColors { 0.0f, 0.0f, 0.0f, 0.0f };

			for( const unsigned neighbourIndex: nextLevelNeighbours[vertexIndex] ) {
				Vector4Add( tmpTessPositions[neighbourIndex], sumOfNeighbourPositions, sumOfNeighbourPositions );
				Vector4Add( tmpTessFloatColors[neighbourIndex], sumOfNeighbourColors, sumOfNeighbourColors );
			}

			Vector4Scale( sumOfNeighbourPositions, icoAvgScale, sumOfNeighbourPositions );

			// Write the average color
			Vector4Scale( sumOfNeighbourColors, icoAvgScale, tessByteColors[vertexIndex] );

			// Write combined positions
			Vector4Lerp( tmpTessPositions[vertexIndex], smoothFrac, sumOfNeighbourPositions, tessPositions[vertexIndex] );
		}
	} else {
		for( unsigned vertexIndex = 0; vertexIndex < numNextLevelVertices; ++vertexIndex ) {
			alignas( 16 ) vec4_t sumOfNeighbourPositions { 0.0f, 0.0f, 0.0f, 0.0f };

			for( const unsigned neighbourIndex: nextLevelNeighbours[vertexIndex] ) {
				Vector4Add( tmpTessPositions[neighbourIndex], sumOfNeighbourPositions, sumOfNeighbourPositions );
			}

			Vector4Scale( sumOfNeighbourPositions, icoAvgScale, sumOfNeighbourPositions );

			// Write combined positions
			Vector4Lerp( tmpTessPositions[vertexIndex], smoothFrac, sumOfNeighbourPositions, tessPositions[vertexIndex] );
		}
	}
}

static thread_local MeshTesselationHelper meshTesselationHelper;

template <SimulatedHullsSystem::ViewDotFade Fade>
[[nodiscard]]
static wsw_forceinline auto calcAlphaFracForViewDirDotNormal( const float *__restrict viewDir,
																	 const float *__restrict normal ) -> float {
	assert( std::fabs( VectorLengthFast( viewDir ) - 1.0f ) < 0.001f );
	assert( std::fabs( VectorLengthFast( normal ) - 1.0f ) < 0.001f );

	const float absDot = std::fabs( DotProduct( viewDir, normal ) );
	if constexpr( Fade == SimulatedHullsSystem::ViewDotFade::FadeOutContour ) {
		return absDot;
	} else if constexpr( Fade == SimulatedHullsSystem::ViewDotFade::FadeOutCenterLinear ) {
		return 1.0f - absDot;
	} else if constexpr( Fade == SimulatedHullsSystem::ViewDotFade::FadeOutCenterQuadratic ) {
		const float frac = ( 1.0f - absDot );
		return frac * frac;
	} else if constexpr( Fade == SimulatedHullsSystem::ViewDotFade::FadeOutCenterCubic ) {
		const float frac = ( 1.0f - absDot );
		return frac * frac * frac;
	} else {
		return 1.0f;
	}
}

template <SimulatedHullsSystem::ZFade ZFade>
[[nodiscard]]
static wsw_forceinline auto calcAlphaFracForDeltaZ( float z, float minZ, float rcpDeltaZ ) -> float {
	if constexpr( ZFade == SimulatedHullsSystem::ZFade::FadeOutBottom ) {
		assert( z >= minZ );
		return ( z - minZ ) * rcpDeltaZ;
	} else {
		// Don't put an assertion here as it's often supposed to be called for default (zero) minZ values
		return 1.0f;
	}
}

using IcosphereVertexNeighbours = const uint16_t (*)[5];

template <SimulatedHullsSystem::ViewDotFade ViewDotFade, SimulatedHullsSystem::ZFade ZFade>
static void calcNormalsAndApplyAlphaFade( byte_vec4_t *const __restrict resultColors,
										  const vec4_t *const __restrict positions,
										  const IcosphereVertexNeighbours neighboursOfVertices,
										  const byte_vec4_t *const __restrict givenColors,
										  const float *const __restrict viewOrigin,
										  unsigned numVertices, float minZ, float maxZ, float minFadedOutAlpha ) {
	[[maybe_unused]] const float rcpDeltaZ = minZ < maxZ ? Q_Rcp( maxZ - minZ ) : 1.0f;
	// Convert to the byte range
	assert( minFadedOutAlpha >= 0.0f && minFadedOutAlpha <= 1.0f );
	minFadedOutAlpha *= 255.0f;

	unsigned vertexNum = 0;
	do {
		const uint16_t *const neighboursOfVertex = neighboursOfVertices[vertexNum];
		const float *const __restrict currVertex = positions[vertexNum];
		vec3_t normal { 0.0f, 0.0f, 0.0f };
		unsigned neighbourIndex = 0;
		do {
			const float *__restrict v2 = positions[neighboursOfVertex[neighbourIndex]];
			const float *__restrict v3 = positions[neighboursOfVertex[( neighbourIndex + 1 ) % 5]];
			vec3_t currTo2, currTo3, cross;
			VectorSubtract( v2, currVertex, currTo2 );
			VectorSubtract( v3, currVertex, currTo3 );
			CrossProduct( currTo2, currTo3, cross );
			if( const float squaredLength = VectorLengthSquared( cross ); squaredLength > 1.0f ) [[likely]] {
				const float rcpLength = Q_RSqrt( squaredLength );
				VectorMA( normal, rcpLength, cross, normal );
			}
		} while( ++neighbourIndex < 5 );

		VectorCopy( givenColors[vertexNum], resultColors[vertexNum] );
		// The sum of partial non-zero directories could be zero, check again
		const float squaredNormalLength = VectorLengthSquared( normal );
		if( squaredNormalLength > wsw::square( 1e-3f ) ) [[likely]] {
			vec3_t viewDir;
			VectorSubtract( currVertex, viewOrigin, viewDir );
			const float squareDistanceToVertex = VectorLengthSquared( viewDir );
			if( squareDistanceToVertex > 1.0f ) [[likely]] {
				const float rcpNormalLength   = Q_RSqrt( squaredNormalLength );
				const float rcpDistance       = Q_RSqrt( squareDistanceToVertex );
				VectorScale( normal, rcpNormalLength, normal );
				VectorScale( viewDir, rcpDistance, viewDir );
				const float givenAlpha        = givenColors[vertexNum][3];
				const float viewDirAlphaFrac  = calcAlphaFracForViewDirDotNormal<ViewDotFade>( viewDir, normal );
				const float deltaZAlphaFrac   = calcAlphaFracForDeltaZ<ZFade>( currVertex[2], minZ, rcpDeltaZ );
				const float combinedAlphaFrac = viewDirAlphaFrac * deltaZAlphaFrac;
				// Disallow boosting the alpha over the existing value (so transparent vertices remain the same)
				const float minAlphaForVertex = wsw::min( givenAlpha, minFadedOutAlpha );
				const float newAlpha          = wsw::max( minAlphaForVertex, givenAlpha * combinedAlphaFrac );
				resultColors[vertexNum][3]    = (uint8_t)wsw::clamp( newAlpha, 0.0f, 255.0f );
			} else {
				resultColors[vertexNum][3] = 0;
			}
		} else {
			resultColors[vertexNum][3] = 0;
		}
	} while( ++vertexNum < numVertices );
}

template <SimulatedHullsSystem::ViewDotFade ViewDotFade, SimulatedHullsSystem::ZFade ZFade>
static void applyAlphaFade( byte_vec4_t *const __restrict resultColors,
							const vec4_t *const __restrict positions,
							const vec4_t *const __restrict normals,
							const byte_vec4_t *const __restrict givenColors,
							const float *const __restrict viewOrigin,
							unsigned numVertices, float minZ, float maxZ, float minFadedOutAlpha ) {
	[[maybe_unused]] const float rcpDeltaZ = minZ < maxZ ? Q_Rcp( maxZ - minZ ) : 1.0f;
	// Convert to the byte range
	assert( minFadedOutAlpha >= 0.0f && minFadedOutAlpha <= 1.0f );
	minFadedOutAlpha *= 255.0f;

	unsigned vertexNum = 0;
	do {
		VectorCopy( givenColors[vertexNum], resultColors[vertexNum] );
		vec3_t viewDir;
		VectorSubtract( positions[vertexNum], viewOrigin, viewDir );
		const float squareDistanceToVertex = VectorLengthSquared( viewDir );
		if( squareDistanceToVertex > 1.0f ) [[likely]] {
			const float rcpDistance = Q_RSqrt( squareDistanceToVertex );
			VectorScale( viewDir, rcpDistance, viewDir );
			const float givenAlpha        = givenColors[vertexNum][3];
			const float viewDirAlphaFrac  = calcAlphaFracForViewDirDotNormal<ViewDotFade>( viewDir, normals[vertexNum] );
			const float deltaZAlphaFrac   = calcAlphaFracForDeltaZ<ZFade>( positions[vertexNum][2], minZ, rcpDeltaZ );
			const float combinedAlphaFrac = viewDirAlphaFrac * deltaZAlphaFrac;
			assert( combinedAlphaFrac >= -0.01f && combinedAlphaFrac <= +1.01f );
			// Disallow boosting the alpha over the existing value (so transparent vertices remain the same)
			const float minAlphaForVertex = wsw::min( givenAlpha, minFadedOutAlpha );
			const float newAlpha          = wsw::max( minAlphaForVertex, givenAlpha * combinedAlphaFrac );
			resultColors[vertexNum][3]    = (uint8_t)wsw::clamp( newAlpha, 0.0f, 255.0f );
		} else {
			resultColors[vertexNum][3] = 0;
		}
	} while( ++vertexNum < numVertices );
}

static void applyLightsToVertices( const vec4_t *__restrict givenPositions,
								   const byte_vec4_t *__restrict givenColors,
								   byte_vec4_t *__restrict destColors,
								   const byte_vec4_t *__restrict alphaSourceColors,
								   const Scene::DynamicLight *__restrict lights,
								   std::span<const uint16_t> affectingLightIndices,
								   unsigned numVertices ) {
	const auto numAffectingLights = (unsigned)affectingLightIndices.size();

	assert( numVertices );
	unsigned vertexIndex = 0;
	do {
		const float *__restrict vertexOrigin  = givenPositions[vertexIndex];
		auto *const __restrict givenColor     = givenColors[vertexIndex];
		auto *const __restrict resultingColor = destColors[vertexIndex];

		alignas( 16 ) vec4_t accumColor;
		VectorScale( givenColor, ( 1.0f / 255.0f ), accumColor );

		unsigned lightNum = 0;
		do {
			const Scene::DynamicLight *__restrict light = lights + affectingLightIndices[lightNum];
			const float squareLightToVertexDistance = DistanceSquared( light->origin, vertexOrigin );
			// May go outside [0.0, 1.0] as we test against the bounding box of the entire hull
			float impactStrength = 1.0f - Q_Sqrt( squareLightToVertexDistance ) * Q_Rcp( light->maxRadius );
			// Just clamp so the code stays branchless
			impactStrength = wsw::clamp( impactStrength, 0.0f, 1.0f );
			VectorMA( accumColor, impactStrength, light->color, accumColor );
		} while( ++lightNum < numAffectingLights );

		resultingColor[0] = (uint8_t)( 255.0f * wsw::clamp( accumColor[0], 0.0f, 1.0f ) );
		resultingColor[1] = (uint8_t)( 255.0f * wsw::clamp( accumColor[1], 0.0f, 1.0f ) );
		resultingColor[2] = (uint8_t)( 255.0f * wsw::clamp( accumColor[2], 0.0f, 1.0f ) );
		resultingColor[3] = alphaSourceColors[vertexIndex][3];
	} while( ++vertexIndex < numVertices );
}

auto SimulatedHullsSystem::HullDynamicMesh::calcSolidSubdivLodLevel( const float *viewOrigin,
																	 float cameraViewTangent,
																	 float viewLodScale,
																	 unsigned drawFlags ) const
	-> std::optional<unsigned> {
	assert( cameraViewTangent > 0.0f );
	assert( viewLodScale >= 0.0f && viewLodScale <= 1.0f );
	assert( m_shared->nextLodTangentRatio > 0.0f && m_shared->nextLodTangentRatio < 1.0f );

	vec3_t center, extentVector;
	VectorAvg( this->cullMins, this->cullMaxs, center );
	VectorSubtract( this->cullMaxs, this->cullMins, extentVector );
	const float squareExtentValue = VectorLengthSquared( extentVector );
	if( squareExtentValue < wsw::square( 1.0f ) ) [[unlikely]] {
		// Skip drawing
		return std::nullopt;
	}

	// Get a suitable subdiv level and store it for further use during this frame
	unsigned chosenSubdivLevel = m_shared->simulatedSubdivLevel;
	if( m_shared->tesselateClosestLod && !( drawFlags & DynamicMesh::ForceLowDetail ) ) {
		chosenSubdivLevel += 1;
	}

	const float extentValue    = Q_Sqrt( squareExtentValue );
	const float squareDistance = DistanceSquared( center, viewOrigin );
	// Don't even try using lesser lods if the mesh is sufficiently close to the viewer
	if( squareDistance > wsw::square( 0.5f * extentValue + 64.0f ) ) {
		// Let the view lod scale affect meshViewTangent multiplicatively, effectively diminishing it
		const float meshViewTangent  = viewLodScale * extentValue * Q_RSqrt( squareDistance );
		const float meshTangentRatio = meshViewTangent * Q_Rcp( cameraViewTangent );

		// Diminish lod tangent ratio in the loop, drop subdiv level every step.
		float lodTangentRatio = m_shared->nextLodTangentRatio;
		for(;; ) {
			if( meshTangentRatio > lodTangentRatio ) {
				break;
			}
			if( chosenSubdivLevel == 0 ) {
				break;
			}
			chosenSubdivLevel--;
			lodTangentRatio *= m_shared->nextLodTangentRatio;
			// Sanity check
			if( lodTangentRatio < 1e-6 ) [[unlikely]] {
				break;
			}
		};
	}

	return chosenSubdivLevel;
}

auto SimulatedHullsSystem::HullDynamicMesh::getCacheEntryByCameraId( unsigned cameraId ) const -> SharedMeshData::CacheEntry * {
	for( SharedMeshData::CacheEntry &e: m_shared->cacheEntriesInitial ) {
		if( e.cameraId == cameraId ) {
			return std::addressof( e );
		}
	}
	for( SharedMeshData::CacheEntry &e: m_shared->cacheEntriesHeapalloc ) {
		if( e.cameraId == cameraId ) {
			return std::addressof( e );
		}
	}
	// Not found, create a new entry for this camera
	bool useHeapAllocStorage = false;
	if( !m_shared->cacheEntriesHeapalloc.empty() ) [[unlikely]] {
		useHeapAllocStorage = true;
	} else {
		// Switch to using heapalloc storage
		if( m_shared->cacheEntriesInitial.full() ) [[unlikely]] {
			m_shared->cacheEntriesHeapalloc.assign( m_shared->cacheEntriesInitial.begin(), m_shared->cacheEntriesInitial.end() );
			m_shared->cacheEntriesInitial.clear();
			useHeapAllocStorage = true;
		}
	}
	if( useHeapAllocStorage ) [[unlikely]] {
		m_shared->cacheEntriesHeapalloc.emplace_back( SharedMeshData::CacheEntry { .cameraId = cameraId } );
		return std::addressof( m_shared->cacheEntriesHeapalloc.back() );
	} else {
		m_shared->cacheEntriesInitial.emplace_back( SharedMeshData::CacheEntry { .cameraId = cameraId } );
		return std::addressof( m_shared->cacheEntriesInitial.back() );
	}
}

static const struct FadeFnHolder {
	using CalcNormalsAndApplyAlphaFadeFn = void (*)( byte_vec4_t *,
													 const vec4_t *,
													 const IcosphereVertexNeighbours,
													 const byte_vec4_t *,
													 const float *,
													 unsigned, float, float, float );
	using ApplyAlphaFadeFn = void (*)( byte_vec4_t *,
									   const vec4_t *,
									   const vec4_t *,
									   const byte_vec4_t *,
									   const float *,
									   unsigned, float, float, float );

	CalcNormalsAndApplyAlphaFadeFn calcNormalsAndApplyFadeFn[10] {};
	ApplyAlphaFadeFn applyFadeFn[10] {};

#define ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade, ZFade ) \
	do { \
        calcNormalsAndApplyFadeFn[indexForFade( SimulatedHullsSystem::ViewDotFade, SimulatedHullsSystem::ZFade )] = \
			&::calcNormalsAndApplyAlphaFade<SimulatedHullsSystem::ViewDotFade, SimulatedHullsSystem::ZFade>; \
		applyFadeFn[indexForFade( SimulatedHullsSystem::ViewDotFade, SimulatedHullsSystem::ZFade )] = \
			&::applyAlphaFade<SimulatedHullsSystem::ViewDotFade, SimulatedHullsSystem::ZFade>; \
	} while( 0 )

	FadeFnHolder() noexcept {
		// Add a reference to a template instantiation for each feasible combination.
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::NoFade, ZFade::FadeOutBottom );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutContour, ZFade::NoFade );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutContour, ZFade::FadeOutBottom );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutCenterLinear, ZFade::NoFade );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutCenterLinear, ZFade::FadeOutBottom );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutCenterQuadratic, ZFade::NoFade );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutCenterQuadratic, ZFade::FadeOutBottom );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutCenterCubic, ZFade::NoFade );
		ADD_FNS_FOR_FADE_TO_TABLES( ViewDotFade::FadeOutCenterCubic, ZFade::FadeOutBottom );
	}

	[[nodiscard]]
	static constexpr auto indexForFade( SimulatedHullsSystem::ViewDotFade viewDotFade,
										SimulatedHullsSystem::ZFade zFade ) -> unsigned {
		assert( (unsigned)viewDotFade < 5 && (unsigned)zFade < 2 );
		return 2 * (unsigned)viewDotFade + (unsigned)zFade;
	}

#undef ADD_FN_FOR_FLAGS_TO_TABLES
} fadeFnHolder;


void SimulatedHullsSystem::HullDynamicMesh::calcOverrideColors( byte_vec4_t *__restrict buffer,
																const float *__restrict viewOrigin,
																const float *__restrict viewAxis,
																unsigned chosenSubdivLevel,
																const Scene::DynamicLight *lights,
																std::span<const uint16_t> affectingLightIndices,
																bool applyViewDotFade,
																bool applyZFade,
																bool applyLights ) const {
	assert( applyViewDotFade || applyZFade || applyLights );

	byte_vec4_t *overrideColors = nullptr;

	if( applyViewDotFade || applyZFade ) {
		const ZFade effectiveZFade        = applyZFade ? m_shared->zFade : ZFade::NoFade;
		const unsigned dataLevelToUse     = wsw::min( chosenSubdivLevel, m_shared->simulatedSubdivLevel );
		const IcosphereData &lodDataToUse = ::basicHullsHolder.getIcosphereForLevel( dataLevelToUse );

		// If tesselation is going to be performed, apply light to the base non-tesselated lod colors
		const auto numVertices = (unsigned)lodDataToUse.vertices.size();
		overrideColors         = buffer;

		// Either we have already calculated normals or do not need normals
		if( m_shared->simulatedNormals || !applyViewDotFade ) {
			// Call specialized implementations for each fade func

			const unsigned fnIndex = ::fadeFnHolder.indexForFade( m_shared->viewDotFade, effectiveZFade );
			const auto applyFadeFn = ::fadeFnHolder.applyFadeFn[fnIndex];

			applyFadeFn( overrideColors, m_shared->simulatedPositions, m_shared->simulatedNormals,
						 m_shared->simulatedColors, viewOrigin,  numVertices,
						 m_shared->minZLastFrame, m_shared->maxZLastFrame, m_shared->minFadedOutAlpha );
		} else {
			// Call specialized implementations for each fade func

			const unsigned fnIndex = ::fadeFnHolder.indexForFade( m_shared->viewDotFade, effectiveZFade );
			const auto applyFadeFn = ::fadeFnHolder.calcNormalsAndApplyFadeFn[fnIndex];

			applyFadeFn( overrideColors, m_shared->simulatedPositions, lodDataToUse.vertexNeighbours.data(),
						 m_shared->simulatedColors, viewOrigin, numVertices,
						 m_shared->minZLastFrame, m_shared->maxZLastFrame, m_shared->minFadedOutAlpha );
		}
	}

	if( applyLights ) {
		unsigned numVertices;
		// If tesselation is going to be performed, apply light to the base non-tesselated lod colors
		if( chosenSubdivLevel > m_shared->simulatedSubdivLevel ) {
			numVertices = ::basicHullsHolder.getIcosphereForLevel( m_shared->simulatedSubdivLevel ).vertices.size();
		} else {
			numVertices = ::basicHullsHolder.getIcosphereForLevel( chosenSubdivLevel ).vertices.size();
		}

		// Copy alpha from these colors
		const byte_vec4_t *alphaSourceColors;
		if( overrideColors ) {
			// We have applied view dot fade, use this view-dot-produced data
			alphaSourceColors = overrideColors;
		} else {
			overrideColors    = buffer;
			alphaSourceColors = m_shared->simulatedColors;
		}

		applyLightsToVertices( m_shared->simulatedPositions, m_shared->simulatedColors,
							   overrideColors, alphaSourceColors,
							   lights, affectingLightIndices, numVertices );
	}
}

auto SimulatedHullsSystem::HullDynamicMesh::getOverrideColors( byte_vec4_t *__restrict localBuffer,
															   const float *__restrict viewOrigin,
															   const float *__restrict viewAxis,
															   unsigned chosenSubdivLevel,
															   const Scene::DynamicLight *lights,
															   std::span<const uint16_t> affectingLightIndices ) const
	-> const byte_vec4_t * {
	const bool shouldApplyViewDotFade   = m_shared->viewDotFade != ViewDotFade::NoFade;
	const bool actuallyApplyViewDotFade = shouldApplyViewDotFade;
	const bool shouldApplyZFade         = m_shared->zFade != ZFade::NoFade;
	const bool actuallyApplyZFade       = shouldApplyZFade && m_shared->maxZLastFrame - m_shared->minZLastFrame > 1.0f;
	const bool actuallyApplyLights      = applyVertexDynLight && !affectingLightIndices.empty();

	if( !( actuallyApplyViewDotFade | actuallyApplyZFade | actuallyApplyLights ) ) {
		return nullptr;
	}

	calcOverrideColors( localBuffer, viewOrigin, viewAxis, chosenSubdivLevel, lights, affectingLightIndices,
						actuallyApplyViewDotFade, actuallyApplyZFade, actuallyApplyLights );

	return localBuffer;
}

struct SolidMeshScratchpad {
	SimulatedHullsSystem::SharedMeshData::CacheEntry *cacheEntry;
	unsigned chosenSubdivLevel;
};

auto SimulatedHullsSystem::HullSolidDynamicMesh::getStorageRequirements( const float *viewOrigin,
																		 const float *viewAxis,
																		 float cameraViewTangent,
																		 float viewLodScale,
																		 unsigned cameraId,
																		 void *scratchpadStorage,
																		 unsigned drawFlags ) const
	-> std::optional<std::pair<unsigned, unsigned>> {
	auto *const scratchpad = (SolidMeshScratchpad *)scratchpadStorage;
	auto *const cacheEntry = getCacheEntryByCameraId( cameraId );
	// Save the relatively expensive lookup result for further use
	scratchpad->cacheEntry = cacheEntry;

	// Check the shared cache first
	if( cacheEntry->chosenSubdivLevel != std::nullopt ) {
		auto *drawOrSkipAddress = std::addressof( *cacheEntry->chosenSubdivLevel );
		if( const auto *drawLevel = std::get_if<unsigned>( drawOrSkipAddress ) ) {
			scratchpad->chosenSubdivLevel = *drawLevel;
		} else {
			return std::nullopt;
		}
	} else if( const std::optional<unsigned> drawLevel = calcSolidSubdivLodLevel( viewOrigin, cameraViewTangent,
																				  viewLodScale, drawFlags ) ) {
		scratchpad->chosenSubdivLevel = *drawLevel;
		cacheEntry->chosenSubdivLevel = *drawLevel;
	} else {
		cacheEntry->chosenSubdivLevel = std::monostate();
		return std::nullopt;
	}

	const IcosphereData &chosenLevelData = ::basicHullsHolder.getIcosphereForLevel( scratchpad->chosenSubdivLevel );
	return std::make_pair<unsigned, unsigned>( chosenLevelData.vertices.size(), chosenLevelData.indices.size() );
}

struct CloudMeshScratchpad {
	SimulatedHullsSystem::SharedMeshData::CacheEntry *cacheEntry;
	unsigned chosenSubdivLevel;
	unsigned minVertexNumForThisCamera;
	unsigned vertexNumLimitForThisCamera;
};

auto SimulatedHullsSystem::HullCloudDynamicMesh::getStorageRequirements( const float *viewOrigin,
																		 const float *viewAxis,
																		 float cameraViewTangent,
																		 float viewLodScale,
																		 unsigned cameraId,
																		 void *scratchpadStorage,
																		 unsigned drawFlags ) const
	-> std::optional<std::pair<unsigned, unsigned>> {
	auto *const scratchpad = (CloudMeshScratchpad *)scratchpadStorage;
	auto *const cacheEntry = getCacheEntryByCameraId( cameraId );
	scratchpad->cacheEntry = cacheEntry;

	if( cacheEntry->chosenSubdivLevel != std::nullopt ) {
		auto *drawOrSkipAddress = std::addressof( *cacheEntry->chosenSubdivLevel );
		if( const auto *drawLevel = std::get_if<unsigned>( drawOrSkipAddress ) ) {
			scratchpad->chosenSubdivLevel = *drawLevel;
		} else {
			return std::nullopt;
		}
	} else if( const std::optional<unsigned> drawLevel = calcSolidSubdivLodLevel( viewOrigin, cameraViewTangent,
																				  viewLodScale, drawFlags ) ) {
		scratchpad->chosenSubdivLevel = *drawLevel;
		cacheEntry->chosenSubdivLevel = *drawLevel;
	} else {
		cacheEntry->chosenSubdivLevel = std::monostate();
		return std::nullopt;
	}

	assert( m_shiftFromDefaultLevelToHide <= 0 );
	if( scratchpad->chosenSubdivLevel > m_shared->simulatedSubdivLevel ) {
		scratchpad->chosenSubdivLevel = m_shared->simulatedSubdivLevel;
	} else {
		const int shiftFromDefaultLevel = (int)scratchpad->chosenSubdivLevel - (int)m_shared->simulatedSubdivLevel;
		if( shiftFromDefaultLevel <= m_shiftFromDefaultLevelToHide ) {
			return std::nullopt;
		}
	}

	assert( m_tessLevelShiftForMinVertexIndex <= 0 );
	assert( m_tessLevelShiftForMaxVertexIndex <= 0 );
	assert( m_tessLevelShiftForMinVertexIndex <= m_tessLevelShiftForMaxVertexIndex );

	if( const int level = (int)scratchpad->chosenSubdivLevel + this->m_tessLevelShiftForMinVertexIndex; level > 0 ) {
		scratchpad->minVertexNumForThisCamera = (unsigned)::basicHullsHolder.getIcosphereForLevel( level - 1 ).vertices.size();
	} else {
		scratchpad->minVertexNumForThisCamera = 0;
	}

	if( const int level = (int)scratchpad->chosenSubdivLevel + this->m_tessLevelShiftForMaxVertexIndex; level >= 0 ) {
		scratchpad->vertexNumLimitForThisCamera = ::basicHullsHolder.getIcosphereForLevel( level ).vertices.size();
	} else {
		scratchpad->vertexNumLimitForThisCamera = ::basicHullsHolder.getIcosphereForLevel( 0 ).vertices.size();
	}

	assert( scratchpad->minVertexNumForThisCamera < scratchpad->vertexNumLimitForThisCamera );
	const unsigned numGridVertices = scratchpad->vertexNumLimitForThisCamera - scratchpad->minVertexNumForThisCamera;
	return std::make_pair( 4 * numGridVertices, 6 * numGridVertices );
}

static void lerpLayerColorsAndRangesBetweenFrames( byte_vec4_t *__restrict destColors, float *__restrict destColorRanges,
												   unsigned numColors, float lerpFrac,
												   const byte_vec4_t *__restrict prevFrameColors,
												   const byte_vec4_t *__restrict nextFrameColors,
												   const float *__restrict prevFrameColorRanges,
												   const float *__restrict nextFrameColorRanges ) {
	assert( numColors );
	unsigned colorNum = 0;
	do {
		Vector4Lerp( prevFrameColors[colorNum], lerpFrac, nextFrameColors[colorNum], destColors[colorNum] );
		destColorRanges[colorNum] = std::lerp( prevFrameColorRanges[colorNum], nextFrameColorRanges[colorNum], lerpFrac );
	} while( ++colorNum < numColors );
}

template <typename LayerType, bool IsLayer0, SimulatedHullsSystem::BlendMode BlendMode, SimulatedHullsSystem::AlphaMode AlphaMode>
static void addLayerContributionToResultColors( const LayerType *__restrict prevLayer, const LayerType *__restrict nextLayer,
												const uint16_t *__restrict vertexNums, unsigned numVerticesToProcess,
												[[maybe_unused]] const float *__restrict viewDotResults, [[maybe_unused]] float lerpFrac,
												byte_vec4_t *__restrict overrideColorsBuffer ) {
	assert( numVerticesToProcess > 0 );

	const unsigned numColors = prevLayer->colors.size();
	assert( numColors > 0 && numColors <= SimulatedHullsSystem::kMaxLayerColors );

	byte_vec4_t lerpedColors[SimulatedHullsSystem::kMaxLayerColors];
	float lerpedColorRanges[SimulatedHullsSystem::kMaxLayerColors];
	lerpLayerColorsAndRangesBetweenFrames( lerpedColors, lerpedColorRanges, numColors, lerpFrac,
										   prevLayer->colors.data(), nextLayer->colors.data(),
										   prevLayer->colorRanges, nextLayer->colorRanges );

	unsigned indexOfNum = 0;
	do {
		const unsigned vertexNum = vertexNums[indexOfNum];

		float rampValue;

		if constexpr( std::is_same_v<SimulatedHullsSystem::MaskedShadingLayer, LayerType> ) {
			rampValue = std::lerp( prevLayer->vertexMaskValues[vertexNum], nextLayer->vertexMaskValues[vertexNum], lerpFrac );
		} else if constexpr( std::is_same_v<SimulatedHullsSystem::DotShadingLayer, LayerType> ) {
			rampValue = viewDotResults[vertexNum];
		} else if constexpr( std::is_same_v<SimulatedHullsSystem::CombinedShadingLayer, LayerType> ) {
			const float vertexDotValue  = viewDotResults[vertexNum];
			const float vertexMaskValue = std::lerp( prevLayer->vertexMaskValues[vertexNum],
													 nextLayer->vertexMaskValues[vertexNum],
													 lerpFrac );

			const float dotInfluence  = prevLayer->dotInfluence;
			const float maskInfluence = 1.0f - dotInfluence;
			rampValue = vertexDotValue * dotInfluence + vertexMaskValue * maskInfluence;
		} else {
			assert( false );
		}

		uint8_t *__restrict resultVertexColor = overrideColorsBuffer[vertexNum];

		unsigned nextColorIndex = 0; // the index of the next color after the point in the color ramp
		while( rampValue > lerpedColorRanges[nextColorIndex] && nextColorIndex < numColors ) {
			nextColorIndex++;
		}

		byte_vec4_t layerColor;
		if( nextColorIndex == 0 ) {
			Vector4Copy( lerpedColors[0], layerColor );
		} else if( nextColorIndex == numColors ) {
			Vector4Copy( lerpedColors[numColors - 1], layerColor );
		} else {
			const unsigned prevColorIndex = nextColorIndex - 1;
			const float offsetInRange     = rampValue - lerpedColorRanges[prevColorIndex];
			const float rangeLength       = lerpedColorRanges[nextColorIndex] - lerpedColorRanges[prevColorIndex];
			assert( offsetInRange >= 0.0f && offsetInRange <= rangeLength );
			const float colorLerpFrac     = wsw::clamp( offsetInRange * Q_Rcp( rangeLength ), 0.0f, 1.0f );
			Vector4Lerp( lerpedColors[prevColorIndex], colorLerpFrac, lerpedColors[nextColorIndex], layerColor );
		}

		if constexpr( IsLayer0 ) {
			Vector4Copy( layerColor, resultVertexColor );
		} else {
			if constexpr( BlendMode == SimulatedHullsSystem::BlendMode::AlphaBlend ) {
				VectorLerp( resultVertexColor, layerColor[3], layerColor, resultVertexColor );
			} else if constexpr( BlendMode == SimulatedHullsSystem::BlendMode::Add ) {
				resultVertexColor[0] = wsw::min( resultVertexColor[0] + layerColor[0], 255 );
				resultVertexColor[1] = wsw::min( resultVertexColor[1] + layerColor[1], 255 );
				resultVertexColor[2] = wsw::min( resultVertexColor[2] + layerColor[2], 255 );
			} else if constexpr( BlendMode == SimulatedHullsSystem::BlendMode::Subtract ) {
				resultVertexColor[0] = wsw::max( resultVertexColor[0] - layerColor[0], 0 );
				resultVertexColor[1] = wsw::max( resultVertexColor[1] - layerColor[1], 0 );
				resultVertexColor[2] = wsw::max( resultVertexColor[2] - layerColor[2], 0 );
			} else {
				assert( false );
			}

			if constexpr( AlphaMode == SimulatedHullsSystem::AlphaMode::Override ) {
				resultVertexColor[3] = layerColor[3];
			} else if constexpr( AlphaMode == SimulatedHullsSystem::AlphaMode::Add ) {
				resultVertexColor[3] = wsw::min( resultVertexColor[3] + layerColor[3], 255 );
			} else if constexpr( AlphaMode == SimulatedHullsSystem::AlphaMode::Subtract ) {
				resultVertexColor[3] = wsw::max( resultVertexColor[3] - layerColor[3], 0 );
			} else {
				assert( false );
			}
		}
	} while( ++indexOfNum < numVerticesToProcess );
}

template <typename LayerType>
struct ToonLayerShadeFnHolder {
	using Fn = void (*)( const LayerType *, const LayerType *, const uint16_t *, unsigned, const float *, float, byte_vec4_t * );

	Fn layer0Fn {};
	Fn blendAndAlphaFnTable[3][3] {};

	[[nodiscard]]
	auto get( unsigned layerNum, SimulatedHullsSystem::BlendMode blendMode, SimulatedHullsSystem::AlphaMode alphaMode ) -> Fn {
		assert( (unsigned)blendMode < 3 && (unsigned)alphaMode < 3 );
		Fn result;
		if( layerNum ) {
			result = blendAndAlphaFnTable[(unsigned)blendMode][(unsigned)alphaMode];
		} else {
			result = layer0Fn;
		}
		assert( result );
		return result;
	}
};

static struct MaskedToonLayerShadeFnHolder : public ToonLayerShadeFnHolder<SimulatedHullsSystem::MaskedShadingLayer> {
	MaskedToonLayerShadeFnHolder() noexcept {
		// Generate specializations which we actually use.
		// Note: Using arbitrary blend/alpha mode for layer0
		layer0Fn = &addLayerContributionToResultColors<SimulatedHullsSystem::MaskedShadingLayer, true,
			SimulatedHullsSystem::BlendMode::Add, SimulatedHullsSystem::AlphaMode::Add>;
		blendAndAlphaFnTable[(unsigned)SimulatedHullsSystem::BlendMode::Add][(unsigned)SimulatedHullsSystem::AlphaMode::Override] =
			&addLayerContributionToResultColors<SimulatedHullsSystem::MaskedShadingLayer, false,
				SimulatedHullsSystem::BlendMode::Add, SimulatedHullsSystem::AlphaMode::Override>;
	}
} g_maskedToonLayerShadeFnHolder;

static struct DotToonLayerShadeFnHolder : public ToonLayerShadeFnHolder<SimulatedHullsSystem::DotShadingLayer> {
	DotToonLayerShadeFnHolder() noexcept {
		// Generate specializations which we actually use
		blendAndAlphaFnTable[(unsigned)SimulatedHullsSystem::BlendMode::Add][(unsigned)SimulatedHullsSystem::AlphaMode::Add] =
			&addLayerContributionToResultColors<SimulatedHullsSystem::DotShadingLayer, false,
				SimulatedHullsSystem::BlendMode::Add, SimulatedHullsSystem::AlphaMode::Add>;
	}
} g_dotToonLayerShadeFnHolder;

static struct CombinedToonLayerShadeFnHolder : public ToonLayerShadeFnHolder<SimulatedHullsSystem::CombinedShadingLayer> {
	// For now, we don't actually use combined layers
	CombinedToonLayerShadeFnHolder() noexcept = default;
} g_combinedToonLayerShadeFnHolder;

auto SimulatedHullsSystem::HullSolidDynamicMesh::fillMeshBuffers( const float *__restrict viewOrigin,
																  const float *__restrict viewAxis,
																  const Scene::DynamicLight *lights,
																  std::span<const uint16_t> affectingLightIndices,
																  void *__restrict scratchpadStorage,
																  vec4_t *__restrict destPositions,
																  vec4_t *__restrict destNormals,
																  vec2_t *__restrict destTexCoords,
																  byte_vec4_t *__restrict destColors,
																  uint16_t *__restrict destIndices ) const
	-> std::pair<unsigned, unsigned> {
	auto *const scratchpad           = (const SolidMeshScratchpad *)scratchpadStorage;
	const unsigned chosenSubdivLevel = scratchpad->chosenSubdivLevel;

	assert( m_shared->simulatedSubdivLevel <= BasicHullsHolder::kMaxSubdivLevel );
	assert( chosenSubdivLevel <= m_shared->simulatedSubdivLevel + 1 );
	assert( m_shared->minZLastFrame <= m_shared->maxZLastFrame );

	// Keep always allocating the default buffer even if it's unused, so we can rely on it
	const auto colorsBufferLevel = wsw::min( chosenSubdivLevel, m_shared->simulatedSubdivLevel );
	const auto colorsBufferSize  = (unsigned)basicHullsHolder.getIcosphereForLevel( colorsBufferLevel ).vertices.size();
	assert( colorsBufferSize && colorsBufferSize < ( 1 << 12 ) );
	auto *const overrideColorsBuffer = (byte_vec4_t *)alloca( sizeof( byte_vec4_t ) * colorsBufferSize );

	const byte_vec4_t *overrideColors;
	if( !m_shared->isAKeyframedHull ) {
		overrideColors = getOverrideColors( overrideColorsBuffer, viewOrigin, viewAxis,
											chosenSubdivLevel, lights, affectingLightIndices );
	} else {
		// If tesselation is going to be performed, apply light to the base non-tesselated lod colors
		const unsigned dataLevelToUse     = wsw::min( chosenSubdivLevel, m_shared->simulatedSubdivLevel );
		const IcosphereData &lodDataToUse = ::basicHullsHolder.getIcosphereForLevel( dataLevelToUse );
		const auto numVertices            = (unsigned)lodDataToUse.vertices.size();

		// Sanity check
		assert( numVertices > 0 && numVertices < 4096 );
		// Calculate vertex normals and view dot products for shading and coarse backface culling purposes.
		// (excluding vertices with negative view dot products from processing produces good results).
		// Reducing shading workload is the primary purpose of this "culling".
		auto *const __restrict viewDotResults = (float *)alloca( sizeof( float ) * numVertices );
		auto *const __restrict vertexNums     = (uint16_t *)alloca( sizeof( uint16_t ) * numVertices );
		unsigned numVerticesToProcess         = 0;

		do {
			const vec4_t *const __restrict positions    = m_shared->simulatedPositions;
			const uint16_t ( *neighboursOfVertices )[5] = lodDataToUse.vertexNeighbours.data();
			unsigned vertexNum = 0;
			do {
				const uint16_t *const __restrict neighboursOfVertex = neighboursOfVertices[vertexNum];
				const float *const __restrict currVertex            = positions[vertexNum];
				vec3_t normal { 0.0f, 0.0f, 0.0f };
				unsigned neighbourIndex = 0;
				do {
					const float *__restrict v2 = positions[neighboursOfVertex[neighbourIndex]];
					const float *__restrict v3 = positions[neighboursOfVertex[( neighbourIndex + 1 ) % 5]];
					vec3_t currTo2, currTo3, cross;
					VectorSubtract( v2, currVertex, currTo2 );
					VectorSubtract( v3, currVertex, currTo3 );
					CrossProduct( currTo2, currTo3, cross );
					if( const float squaredLength = VectorLengthSquared( cross ); squaredLength > 1.0f ) [[likely]] {
						const float rcpLength = Q_RSqrt( squaredLength );
						VectorMA( normal, rcpLength, cross, normal );
					}
				} while( ++neighbourIndex < 5 );

				const float squaredNormalLength = VectorLengthSquared( normal );
				vec3_t viewDir;
				VectorSubtract( currVertex, viewOrigin, viewDir );
				const float squareDistanceToVertex = VectorLengthSquared( viewDir );

				const float squareRcpNormalizingFactor = squaredNormalLength * squareDistanceToVertex;
				// check that both the length of the normal and distance to vertex are not 0 in one branch
				if( squareRcpNormalizingFactor > 1.0f ) [[likely]] {
					const float normalizingFactor    = Q_RSqrt( squareRcpNormalizingFactor );
					const float viewDot              = DotProduct( viewDir, normal ) * normalizingFactor;
					viewDotResults[vertexNum]        = viewDot;
					vertexNums[numVerticesToProcess] = vertexNum;
					numVerticesToProcess += ( viewDot >= 0.0f );
				} else {
					viewDotResults[vertexNum] = 0.0f;
				}
			} while( ++vertexNum < numVertices );
		} while( false );

		std::memset( overrideColorsBuffer, 0, sizeof( overrideColorsBuffer[0] ) * numVertices );
		// Note: The opposite is perfectly reachable, probably due to getting all-degenerate results
		// during view dot computations (we rely on various approximations).
		if( numVerticesToProcess > 0 ) [[likely]] {
			const unsigned numShadingLayers = m_shared->prevShadingLayers.size();
			for( unsigned layerNum = 0; layerNum < numShadingLayers; ++layerNum ) {
				const ShadingLayer &prevShadingLayer = m_shared->prevShadingLayers[layerNum];
				const ShadingLayer &nextShadingLayer = m_shared->nextShadingLayers[layerNum];
				// Call specialized implementations for respective layer types
				if( const auto *const prevMaskedLayer = std::get_if<MaskedShadingLayer>( &prevShadingLayer ) ) {
					const auto *const nextMaskedLayer = std::get_if<MaskedShadingLayer>( &nextShadingLayer );
					const auto addLayerContributionFn = g_maskedToonLayerShadeFnHolder.get( layerNum,
																							prevMaskedLayer->blendMode,
																							prevMaskedLayer->alphaMode );
					addLayerContributionFn( prevMaskedLayer, nextMaskedLayer, vertexNums, numVerticesToProcess,
											viewDotResults, m_shared->lerpFrac, overrideColorsBuffer );
				} else if( const auto *const prevDotLayer = std::get_if<DotShadingLayer>( &prevShadingLayer ) ) {
					const auto *const nextDotLayer    = std::get_if<DotShadingLayer>( &nextShadingLayer );
					const auto addLayerContributionFn = g_dotToonLayerShadeFnHolder.get( layerNum,
																						 prevDotLayer->blendMode,
																						 prevDotLayer->alphaMode );
					addLayerContributionFn( prevDotLayer, nextDotLayer, vertexNums, numVerticesToProcess,
											viewDotResults, m_shared->lerpFrac, overrideColorsBuffer );
				} else if( const auto *const prevCombinedLayer = std::get_if<CombinedShadingLayer>( &prevShadingLayer ) ) {
					const auto *const nextCombinedLayer = std::get_if<CombinedShadingLayer>( &nextShadingLayer );
					const auto addLayerContributionFn   = g_combinedToonLayerShadeFnHolder.get( layerNum,
																								prevCombinedLayer->blendMode,
																								prevCombinedLayer->alphaMode );
					addLayerContributionFn( prevCombinedLayer, nextCombinedLayer, vertexNums, numVerticesToProcess,
											viewDotResults, m_shared->lerpFrac, overrideColorsBuffer );
				} else {
					wsw::failWithLogicError( "Unreachable" );
				}
			}
		}

		overrideColors = overrideColorsBuffer;
	}

	unsigned numResultVertices, numResultIndices;

	// HACK Perform an additional tesselation of some hulls.
	// CPU-side tesselation is the single option in the current codebase state.
	if( m_shared->tesselateClosestLod && chosenSubdivLevel > m_shared->simulatedSubdivLevel ) {
		assert( m_shared->simulatedSubdivLevel + 1 == chosenSubdivLevel );
		const IcosphereData &nextLevelData = ::basicHullsHolder.getIcosphereForLevel( chosenSubdivLevel );
		const IcosphereData &simLevelData  = ::basicHullsHolder.getIcosphereForLevel( m_shared->simulatedSubdivLevel );

		const IcosphereVertexNeighbours nextLevelNeighbours = nextLevelData.vertexNeighbours.data();

		const auto numSimulatedVertices = (unsigned)simLevelData.vertices.size();
		const auto numNextLevelVertices = (unsigned)nextLevelData.vertices.size();

		MeshTesselationHelper *const tesselationHelper = &::meshTesselationHelper;
		if( m_shared->lerpNextLevelColors ) {
			tesselationHelper->exec<true>( this, overrideColors,
										   numSimulatedVertices, numNextLevelVertices, nextLevelNeighbours );
		} else {
			tesselationHelper->exec<false>( this, overrideColors,
											numSimulatedVertices, numNextLevelVertices, nextLevelNeighbours );
		}

		numResultVertices = numNextLevelVertices;
		numResultIndices  = (unsigned)nextLevelData.indices.size();

		// TODO: Eliminate this excessive copying
		std::memcpy( destPositions, tesselationHelper->m_tessPositions, sizeof( destPositions[0] ) * numResultVertices );
		std::memcpy( destColors, tesselationHelper->m_tessByteColors, sizeof( destColors[0] ) * numResultVertices );
		std::memcpy( destIndices, nextLevelData.indices.data(), sizeof( uint16_t ) * numResultIndices );
	} else {
		const IcosphereData &dataToUse = ::basicHullsHolder.getIcosphereForLevel( chosenSubdivLevel );
		const byte_vec4_t *colorsToUse = overrideColors ? overrideColors : m_shared->simulatedColors;

		numResultVertices = (unsigned)dataToUse.vertices.size();
		numResultIndices  = (unsigned)dataToUse.indices.size();

		const vec4_t *simulatedPositions = m_shared->simulatedPositions;
		std::memcpy( destPositions, simulatedPositions, sizeof( simulatedPositions[0] ) * numResultVertices );
		std::memcpy( destColors, colorsToUse, sizeof( colorsToUse[0] ) * numResultVertices );
		std::memcpy( destIndices, dataToUse.indices.data(), sizeof( uint16_t ) * numResultIndices );
	}

	return { numResultVertices, numResultIndices };
};

// TODO: Lift it to the top level, make views of different types that point to it as well

alignas( 16 ) static const uint8_t kRandomBytes[] = {
	0x01, 0x2b, 0x1e, 0xbc, 0x47, 0x26, 0xc3, 0x0a, 0x6c, 0x50, 0x0f, 0x5c, 0x64, 0x14, 0xe8, 0x56, 0x06, 0x05, 0x5b,
	0x8e, 0xe1, 0x0a, 0x3d, 0xcb, 0x9c, 0x94, 0x54, 0xe2, 0x61, 0xba, 0x03, 0xb9, 0xcd, 0x5d, 0x03, 0x6b, 0x2a, 0xfd,
	0x05, 0xec, 0x12, 0x21, 0x0a, 0x5c, 0x43, 0x41, 0xc5, 0xcb, 0x1e, 0xf8, 0x71, 0x17, 0x00, 0x06, 0x35, 0xb2, 0x2d,
	0x50, 0xf3, 0x08, 0x38, 0x69, 0x8e, 0x9e, 0xdc, 0x58, 0x6e, 0x2f, 0xff, 0xfc, 0xad, 0xa3, 0x57, 0x3d, 0xfe, 0x74,
	0x2a, 0x0b, 0x84, 0xaa, 0x61, 0x82, 0xa1, 0xb8, 0x78, 0x11, 0x34, 0x88, 0xb4, 0x89, 0x65, 0x8e, 0xcb, 0x8c, 0xdd,
	0x64, 0xf5, 0xc3, 0x6f, 0xce, 0xf6, 0xf7, 0x58, 0xc0, 0xb3, 0x8a, 0xc4, 0x90, 0xe9, 0x11, 0x38, 0x86, 0x9f, 0x2b,
	0xd4, 0x02, 0xcc, 0xbc, 0x53, 0xd5, 0x65, 0x94, 0x55, 0x1d, 0x1c, 0xc6, 0xe7, 0x4f, 0x53, 0x26, 0xa8, 0xc7, 0x7c,
	0xed, 0x9d, 0x09, 0x67, 0xea, 0xdb, 0xf2, 0x7c, 0x51, 0xe2, 0xf6, 0x0f, 0x38, 0x44, 0x5e, 0x22, 0x4a, 0xea, 0xd8,
	0xf3, 0xce, 0x4e, 0xaf, 0xb6, 0x33, 0x0d, 0x14, 0xaa, 0x77, 0xc5, 0xc3, 0x26, 0x7a, 0xca, 0x8d, 0x0c, 0x9c, 0x34,
	0x8f, 0x63, 0xae, 0x65, 0xf1, 0x0a, 0x87, 0xf1, 0x25, 0x92, 0x6c, 0x56, 0x79, 0x2a, 0x7f, 0xbc, 0x3f, 0x8e, 0x72,
	0x10, 0x30, 0x33, 0x18, 0xa5, 0x83, 0xcc, 0xb9, 0x7d, 0x88, 0xd1, 0xf7, 0xc0, 0x86, 0x4b, 0x5f, 0x94, 0xd3, 0xd3,
	0xfb, 0x50, 0x61, 0xc3, 0x56, 0xe4, 0xcf, 0xb9, 0x72, 0x01, 0x97, 0xf2, 0x1c, 0x17, 0x82, 0x54, 0xf8, 0x5f, 0x77,
	0x2d, 0xde, 0x01, 0xa9, 0xb3, 0x49, 0xa1, 0x5a, 0xf3, 0x66, 0x79, 0x24, 0xfb, 0x44, 0x03, 0x42, 0x12, 0x7a, 0xaf,
	0x17, 0x1a, 0xa3, 0x4e, 0x5d, 0xfb, 0xf8, 0x27, 0x01, 0x70, 0xae, 0xed, 0x4d, 0x7a, 0x25, 0x96, 0x69, 0x56, 0xc3,
	0xf6, 0x24, 0xb0, 0x88, 0x35, 0x0d, 0x5f, 0x9a, 0x70, 0x74, 0x84, 0x7e, 0x64, 0x62, 0x19, 0x09, 0xf5, 0x1b, 0x33,
	0x21, 0xf1, 0xac, 0x5a, 0x90, 0xcd, 0x56, 0x77, 0xfe, 0x75, 0xac, 0xb1, 0x7d, 0xb3, 0x44, 0x87, 0xe9, 0x9e, 0xb6,
	0x4f, 0x5d, 0x50, 0xe1, 0xac, 0x65, 0xaf, 0x61, 0xfd, 0xaa, 0x22, 0x74, 0x6a, 0xf3, 0x28, 0x3e, 0x95, 0x3d, 0x1e,
	0x2d, 0xff, 0x2f, 0xd3, 0xbb, 0x3e, 0x6b, 0xb9, 0x6d, 0x42, 0x31, 0x96, 0x4f, 0xf4, 0xad, 0x57, 0xce, 0x9a, 0xf3,
	0x4b, 0x5c, 0x57, 0x7b, 0x44, 0x40, 0x17, 0x10, 0x73, 0x40, 0xbf, 0x0e, 0xf0, 0xec, 0x2a, 0x3d, 0xbb, 0x4f, 0xea,
	0xb2, 0x5c, 0x53, 0x25, 0x86, 0xa4, 0xf1, 0x35, 0x44, 0x64, 0xdb, 0x6c, 0xcf, 0xce, 0xcb, 0x58, 0xa8, 0x50, 0x07,
	0x32, 0xf5, 0x67, 0x80, 0x2b, 0xbb, 0xc4, 0x57, 0x63, 0x34, 0x56, 0x9c, 0x2c, 0x17, 0x16, 0xb6, 0x9e, 0x47, 0xf0,
	0xd7, 0x39, 0x4d, 0x1f, 0xd9, 0x5e, 0x15, 0x61, 0xf9, 0x8b, 0x27, 0x93, 0x12, 0x0e, 0xac, 0x5c, 0x0c, 0xd9, 0xce,
	0xe3, 0x38, 0x1c, 0x23, 0xf6, 0xaf, 0x9a, 0x1b, 0x71, 0xdf, 0xbb, 0x5b, 0x84, 0x02, 0xf3, 0x52, 0x2b, 0x4e, 0x7a,
	0xdd, 0x75, 0x49, 0xba, 0xb8, 0x8b, 0x2c, 0x9d, 0x68, 0xfb, 0x98, 0x74, 0x07, 0xb6, 0x6d, 0xb4, 0xe7, 0x48, 0x2a,
	0x39, 0xbe, 0x1f, 0x81, 0x36, 0xbb, 0xd1, 0x5a, 0x14, 0x2b, 0x8e, 0x1c, 0x00, 0xd2, 0x6b, 0x53, 0x2f, 0x81, 0x04,
	0x2b, 0x54, 0x07, 0xd5, 0x11, 0x75, 0xdb, 0x45, 0x65, 0x60, 0xa2, 0x44, 0x13, 0xa1, 0x93, 0xd7, 0x69, 0x3a, 0xa4,
	0x59, 0x26, 0x61, 0x7b, 0x73, 0xec, 0x57, 0x36, 0xb1, 0xd0, 0x3f, 0x99, 0x5c, 0xad, 0xe8, 0x6a, 0x87, 0x21, 0xab,
	0x5a, 0xec, 0x56, 0xfb, 0xfd, 0xc9, 0x19, 0x85, 0x38, 0x9e, 0x20, 0xce, 0x18, 0xfc, 0x1e, 0x75, 0x01, 0xb5, 0x8f,
	0x7b, 0x80, 0x47, 0x2f, 0x31, 0x91, 0x99, 0x45, 0x32, 0x60, 0xde, 0xc3, 0x1d, 0xee, 0xfa, 0x35, 0x91, 0xad, 0xd9,
	0x06, 0xdb, 0xb3, 0x6c, 0xe1, 0x33, 0x4d, 0x6e, 0x01, 0xaf, 0xa0, 0x1f, 0x3f, 0x63, 0x4f, 0x2d, 0x54, 0x50, 0xbc,
	0xf3, 0xac, 0xa4, 0x3e, 0xc4, 0x23, 0x02, 0xeb, 0xe2, 0x4e, 0xf0, 0xe3, 0xdd, 0x49, 0x09, 0xb6, 0xc5, 0x41, 0x7d,
	0x79, 0x39, 0xdc, 0x2e, 0xb4, 0xa2, 0x7b, 0xa2, 0x63, 0xc0, 0x08, 0x80, 0xa5, 0xfe, 0xe7, 0x63, 0x2d, 0x92, 0x02,
	0x0c, 0x82, 0x20, 0x4c, 0x11, 0x26, 0xb6, 0x94, 0xd1, 0x08, 0x60, 0xc4, 0x4b, 0x5d, 0x82, 0xac, 0x35, 0x98, 0x0a,
	0xb5, 0x2a, 0x25, 0x21, 0x0c, 0x3c, 0x0b, 0x79, 0xa9, 0x86, 0x08, 0x14, 0x1f, 0x01, 0x90, 0x22, 0x10, 0x11, 0x29,
	0x6c, 0x14, 0x0e, 0x15, 0x97, 0x36, 0xf4, 0xe4, 0x67, 0x53, 0xd0, 0x6b, 0x7a, 0x93, 0x50, 0xbf, 0x41, 0xa0, 0x14,
	0x26, 0x07, 0x33, 0x47, 0xab, 0x01, 0x9b, 0xf7, 0x9c, 0x35, 0xae, 0xda, 0x89, 0xf6, 0x78, 0x16, 0xd8, 0x98, 0xf5,
	0xc0, 0x86, 0x76, 0x74, 0xad, 0xb0, 0xff, 0x7d, 0x46, 0xb2, 0xf3, 0x2f, 0x0f, 0x81, 0x31, 0xe6, 0x85, 0x19, 0x66,
	0x8d, 0xea, 0x8f, 0x3a, 0x94, 0x80, 0x8b, 0x5c, 0x36, 0xee, 0xd1, 0xd1, 0xb4, 0x8b, 0x28, 0x2d, 0x60, 0x4f, 0xe7,
	0xc1, 0x58, 0x0c, 0x3e, 0xec, 0xb2, 0x01, 0x88, 0xda, 0x8d, 0x67, 0x17, 0xa5, 0x34, 0x1f, 0x07, 0xae, 0xfa, 0xc5,
	0x09, 0x22, 0x90, 0x8b, 0x38, 0xdb, 0x5a, 0x0b, 0xfa, 0xd2, 0x13, 0xe4, 0x59, 0x9f, 0xa6, 0x35, 0x4b, 0xe4, 0xc4,
	0xc3, 0xeb, 0x6e, 0xd4, 0x87, 0xf6, 0xc8, 0x25, 0xfe, 0x7c, 0x0c, 0xaf, 0x0b, 0x70, 0x5f, 0xcb, 0xed, 0x48, 0xcf,
	0x50, 0x79, 0x4b, 0xde, 0x42, 0x98, 0x26, 0xe7, 0xcc, 0x97, 0xfa, 0xd4, 0x10, 0xf4, 0x14, 0x16, 0x79, 0x93, 0x55,
	0xea, 0x09, 0xeb, 0x53, 0x43, 0xc5, 0xda, 0xcc, 0xce, 0xb9, 0x17, 0xc5, 0xb2, 0x1d, 0xa2, 0x40, 0x1e, 0x96, 0x08,
	0x0a, 0x67, 0x14, 0x28, 0x57, 0x69, 0x4c, 0x13, 0x32, 0xf8, 0x09, 0xde, 0xe7, 0x9f, 0x0f, 0xa9, 0xb0, 0x55, 0x53,
	0xf4, 0xd8, 0xdd, 0x07, 0x86, 0x22, 0xb0, 0x73, 0x43, 0x98, 0xd1, 0x32, 0xfe, 0xe3, 0x89, 0x5c, 0x93, 0xf9, 0x31,
	0x37, 0x90, 0x05, 0x47, 0x59, 0x13, 0x28, 0x1d, 0xdc, 0xd8, 0xf4, 0x91, 0x58, 0xb9, 0xcd, 0xe3, 0x7b, 0xb4, 0x25,
	0x86, 0x0e, 0xfe, 0x8d, 0x18, 0x10, 0x5c, 0x00, 0xa2, 0xd7, 0x9c, 0x91, 0x04, 0x20, 0xcc, 0x9d, 0xa3, 0xfc, 0x63,
	0xef, 0xf5, 0x00, 0x97, 0xe7, 0x73, 0xb5, 0x38, 0x57, 0x71, 0x96, 0xd0, 0xee, 0xc9, 0x10, 0xcc, 0x2d, 0x56, 0xb6,
	0x91, 0x32, 0x82, 0xa7, 0x33, 0x48, 0x60, 0xd8, 0x8e, 0x5f, 0x45, 0xfd, 0x20, 0x1c, 0xc8, 0x91, 0x22, 0x17, 0x3d,
	0x5e, 0x01, 0xa8, 0xf5, 0xa7, 0x12, 0x9b, 0x76, 0xdd, 0x67, 0x7d, 0x01, 0xb9, 0xd1, 0xb3, 0x3e, 0xbc, 0x4d, 0x7b,
	0x50, 0xf0, 0xb2, 0xe1, 0x22, 0x63, 0xe2, 0x6b, 0x69, 0xbb, 0x9b, 0x0e, 0xf4, 0x9a, 0xdd, 0x78, 0x44, 0x1b, 0xb4,
	0xba, 0x6b, 0x86, 0xf1, 0x05, 0x9b, 0xcf, 0xe4, 0xb8, 0xcf, 0x26, 0xd1, 0xd5, 0xf7, 0xc8, 0x58, 0x4c, 0x7d, 0xf8,
	0xcb, 0x80, 0xfb, 0xf6, 0xf3, 0x2f, 0x2e, 0xe2, 0x84, 0xfc, 0xb0, 0xd1, 0xb8, 0xf3, 0xb2, 0x39, 0x85, 0x17, 0x7c,
	0x47, 0x8c, 0x23, 0x31, 0x4d, 0x33, 0x97, 0x00, 0xd1, 0x34, 0x09, 0xa3, 0xf1, 0x65, 0x4a, 0x08, 0xb9
};

static_assert( std::size( kRandomBytes ) == 1024 );

auto SimulatedHullsSystem::HullCloudDynamicMesh::fillMeshBuffers( const float *__restrict viewOrigin,
																  const float *__restrict viewAxis,
																  const Scene::DynamicLight *lights,
																  std::span<const uint16_t> affectingLightIndices,
																  void *__restrict scratchpadStorage,
																  vec4_t *__restrict destPositions,
																  vec4_t *__restrict destNormals,
																  vec2_t *__restrict destTexCoords,
																  byte_vec4_t *__restrict destColors,
																  uint16_t *__restrict destIndices ) const
-> std::pair<unsigned, unsigned> {
	auto *const scratchpad                     = (const CloudMeshScratchpad *)scratchpadStorage;
	const unsigned chosenSubdivLevel           = scratchpad->chosenSubdivLevel;
	const unsigned minVertexNumForThisCamera   = scratchpad->minVertexNumForThisCamera;
	const unsigned vertexNumLimitForThisCamera = scratchpad->vertexNumLimitForThisCamera;

	assert( m_shared->simulatedSubdivLevel < BasicHullsHolder::kMaxSubdivLevel );
	assert( chosenSubdivLevel <= m_shared->simulatedSubdivLevel + 1 );
	assert( m_shared->minZLastFrame <= m_shared->maxZLastFrame );

	// Keep always allocating the default buffer even if it's unused, so we can rely on its availability
	const auto colorsBufferLevel = wsw::min( chosenSubdivLevel, m_shared->simulatedSubdivLevel );
	const auto colorsBufferSize  = (unsigned)basicHullsHolder.getIcosphereForLevel( colorsBufferLevel ).vertices.size();
	assert( colorsBufferSize && colorsBufferSize < ( 1 << 12 ) );
	auto *const overrideColorsBuffer = (byte_vec4_t *)alloca( sizeof( byte_vec4_t ) * colorsBufferSize );

	const byte_vec4_t *overrideColors = getOverrideColors( overrideColorsBuffer, viewOrigin, viewAxis,
														   chosenSubdivLevel, lights, affectingLightIndices );

	const byte_vec4_t *colorsToUse = overrideColors ? overrideColors : m_shared->simulatedColors;

	alignas( 16 ) vec4_t viewLeft, viewUp;
	// TODO: Flip if needed
	VectorCopy( &viewAxis[AXIS_RIGHT], viewLeft );
	VectorCopy( &viewAxis[AXIS_UP], viewUp );

	alignas( 16 ) vec4_t normal;
	VectorNegate( &viewAxis[AXIS_FORWARD], normal );
	normal[3] = 0.0f;

	const float radius         = m_spriteRadius;
	constexpr float normalizer = 1.0f / 255.0f;

	unsigned numOutVertices = 0;
	unsigned numOutIndices  = 0;

	// We sample the random data by vertex numbers using random shifts that remain stable during the hull lifetime
	assert( vertexNumLimitForThisCamera + m_phaseIndexShiftInTable <= std::size( kRandomBytes ) );
	assert( vertexNumLimitForThisCamera + m_speedIndexShiftInTable <= std::size( kRandomBytes ) );

	unsigned vertexNum = minVertexNumForThisCamera;
	assert( vertexNum < vertexNumLimitForThisCamera );

	[[maybe_unused]] vec3_t tmpLeftStorage, tmpUpStorage;

	do {
		const float *__restrict vertexPosition      = m_shared->simulatedPositions[vertexNum];
		const uint8_t *const __restrict vertexColor = colorsToUse[vertexNum];

		vec4_t *const __restrict positions   = destPositions + numOutVertices;
		vec4_t *const __restrict normals     = destNormals   + numOutVertices;
		byte_vec4_t *const __restrict colors = destColors    + numOutVertices;
		vec2_t *const __restrict texCoords   = destTexCoords + numOutVertices;
		uint16_t *const __restrict indices   = destIndices   + numOutIndices;

		// Test the color alpha first for an early cutoff

		byte_vec4_t resultingColor;
		resultingColor[3] = (uint8_t)wsw::clamp( (float)vertexColor[3] * m_spriteColor[3] * m_alphaScale, 0.0f, 255.0f );
		if( resultingColor[3] < 1 ) {
			continue;
		}

		resultingColor[0] = (uint8_t)wsw::clamp( (float)vertexColor[0] * m_spriteColor[0], 0.0f, 255.0f );
		resultingColor[1] = (uint8_t)wsw::clamp( (float)vertexColor[1] * m_spriteColor[1], 0.0f, 255.0f );
		resultingColor[2] = (uint8_t)wsw::clamp( (float)vertexColor[2] * m_spriteColor[2], 0.0f, 255.0f );

		Vector4Copy( resultingColor, colors[0] );
		Vector4Copy( resultingColor, colors[1] );
		Vector4Copy( resultingColor, colors[2] );
		Vector4Copy( resultingColor, colors[3] );

		// 1 unit is equal to a rotation of 360 degrees
		const float initialPhase    = ( (float)kRandomBytes[vertexNum + m_phaseIndexShiftInTable] - 127.0f ) * normalizer;
		const float angularSpeed    = ( (float)kRandomBytes[vertexNum + m_speedIndexShiftInTable] - 127.0f ) * normalizer;
		const float rotationDegrees = 360.0f * ( initialPhase + angularSpeed * m_lifetimeSeconds );

		const float *left, *up;
		// TODO: Avoid dynamic branching, add templated specializations?
		if( m_applyRotation ) {
			// TODO: This could be probably reduced to a single sincos() calculation + few vector transforms
			RotatePointAroundVector( tmpLeftStorage, normal, viewLeft, rotationDegrees );
			RotatePointAroundVector( tmpUpStorage, normal, viewUp, rotationDegrees );
			left = tmpLeftStorage;
			up   = tmpUpStorage;
		} else {
			left = viewLeft;
			up   = viewUp;
		}

		vec3_t point;
		VectorMA( vertexPosition, -radius, up, point );
		VectorMA( point, +radius, left, positions[0] );
		VectorMA( point, -radius, left, positions[3] );

		VectorMA( vertexPosition, radius, up, point );
		VectorMA( point, +radius, left, positions[1] );
		VectorMA( point, -radius, left, positions[2] );

		positions[0][3] = positions[1][3] = positions[2][3] = positions[3][3] = 1.0f;

		Vector4Copy( normal, normals[0] );
		Vector4Copy( normal, normals[1] );
		Vector4Copy( normal, normals[2] );
		Vector4Copy( normal, normals[3] );

		VectorSet( indices + 0, numOutVertices + 0, numOutVertices + 1, numOutVertices + 2 );
		VectorSet( indices + 3, numOutVertices + 0, numOutVertices + 2, numOutVertices + 3 );

		Vector2Set( texCoords[0], 0.0f, 1.0f );
		Vector2Set( texCoords[1], 0.0f, 0.0f );
		Vector2Set( texCoords[2], 1.0f, 0.0f );
		Vector2Set( texCoords[3], 1.0f, 1.0f );

		numOutVertices += 4;
		numOutIndices  += 6;
	} while( ++vertexNum < vertexNumLimitForThisCamera );

	return { numOutVertices, numOutIndices };
};