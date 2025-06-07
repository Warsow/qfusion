/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.
This file is part of Quake III Arena source code.
Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.
Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

#include "aasworld.h"
#include "aaselementsmask.h"
#include "aasareaswalker.h"
#include <common/helpers/algorithm.h>
#include <common/helpers/sortbyfield.h>
#include <common/types/vector.h>
#include <common/facilities/wswfs.h>
#include "../ailocal.h"
#include "../rewriteme.h"
#include <common/helpers/md5.h>
#include <common/helpers/base64.h>

#include <cmath>
#include <cstdlib>
#include <tuple>

using wsw::operator""_asView;

template<typename T>
class BufferBuilder {
	wsw::PodVector<T> m_underlying;
public:
	explicit BufferBuilder( size_t initialSizeHint ) {
		m_underlying.reserve( initialSizeHint );
	}

	unsigned Size() const { return (unsigned)m_underlying.size(); }

	void Clear() {
		// Preserve the old semantics that used to release memory
		wsw::PodVector<T> tmp;
		std::swap( m_underlying, tmp );
	}

	void Add( const T &elem ) {
		m_underlying.push_back( elem );
	}
	void Add( const T *elems, int numElems ) {
		m_underlying.insert( m_underlying.end(), elems, elems + numElems );
	}

	T *FlattenResult() const {
		static_assert( std::is_trivial_v<T> );
		// TODO: take the pointer
		const size_t sizeInBytes = sizeof( T ) * m_underlying.size();
		auto *const result = (T *)Q_malloc( sizeInBytes );
		std::memcpy( result, m_underlying.data(), sizeInBytes );
		return result;
	}
};

// Static member definition
AiAasWorld *AiAasWorld::s_instance = nullptr;

bool AiAasWorld::init( const wsw::StringView &mapName ) {
	if( s_instance ) {
		AI_FailWith( "AiAasWorld::Init()", "An instance is already present\n" );
	}

	s_instance = (AiAasWorld *)Q_malloc( sizeof( AiAasWorld ) );
	new( s_instance )AiAasWorld;

	const wsw::StringView baseMapName( getBaseMapName( mapName ) );
	// Try to initialize the instance
	if( !s_instance->load( baseMapName ) ) {
		return false;
	}
	s_instance->postLoad( baseMapName );
	return true;
}

void AiAasWorld::shutdown() {
	// This may be called on first map load when an instance has never been instantiated
	if( s_instance ) {
		s_instance->~AiAasWorld();
		Q_free( s_instance );
		// Allow the pointer to be reused, otherwise an assertion will fail on a next Init() call
		s_instance = nullptr;
	}
}

int AiAasWorld::pointAreaNumNaive( const vec3_t point, int topNodeHint ) const {
	if( !m_loaded ) [[unlikely]] {
		return 0;
	}

	assert( topNodeHint > 0 && topNodeHint < m_numnodes );
	int nodenum = topNodeHint;

	while( nodenum > 0 ) {
		aas_node_t *node = &m_nodes[nodenum];
		aas_plane_t *plane = &m_planes[node->planenum];
		vec_t dist = DotProduct( point, plane->normal ) - plane->dist;
		if( dist > 0 ) {
			nodenum = node->children[0];
		} else {
			nodenum = node->children[1];
		}
	}
	return -nodenum;
}

int AiAasWorld::findAreaNum( const vec3_t mins, const vec3_t maxs ) const {
	const vec_t *bounds[2] = { maxs, mins };
	// Test all AABB vertices
	vec3_t origin = { 0, 0, 0 };

	for( int i = 0; i < 8; ++i ) {
		origin[0] = bounds[( i >> 0 ) & 1][0];
		origin[1] = bounds[( i >> 1 ) & 1][1];
		origin[2] = bounds[( i >> 2 ) & 1][2];
		int areaNum = pointAreaNum( origin );
		if( areaNum ) {
			return areaNum;
		}
	}
	return 0;
}

auto AiAasWorld::findAreaNum( const vec3_t origin ) const -> int {
	if( const int areaNum = pointAreaNum( origin ) ) {
		return areaNum;
	}

	vec3_t mins = { -8, -8, 0 };
	VectorAdd( mins, origin, mins );
	vec3_t maxs = { +8, +8, 16 };
	VectorAdd( maxs, origin, maxs );
	return findAreaNum( mins, maxs );
}

auto AiAasWorld::findAreaNum( const edict_t *ent ) const -> int {
	// Reject degenerate case
	if( VectorCompare( ent->r.absmin, ent->r.absmax ) ) {
		return findAreaNum( ent->s.origin );
	}

	if( const int areaNum = pointAreaNum( ent->s.origin ) ) {
		return areaNum;
	}

	return findAreaNum( ent->r.absmin, ent->r.absmax );
}

typedef struct aas_tracestack_s {
	vec3_t start;       //start point of the piece of line to trace
	vec3_t end;         //end point of the piece of line to trace
	int planenum;       //last plane used as splitter
	int nodenum;        //node found after splitting with planenum
} aas_tracestack_t;

auto AiAasWorld::traceAreas( const vec3_t start, const vec3_t end, int *areas_,
							 vec3_t *points, int maxareas ) const -> std::span<const int> {
	if( !m_loaded ) {
		return {};
	}

	vec3_t cur_start, cur_end, cur_mid;
	aas_tracestack_t tracestack[127];
	aas_tracestack_t *tstack_p;

	int numAreas = 0;
	areas_[0] = 0;

	tstack_p = tracestack;
	//we start with the whole line on the stack
	VectorCopy( start, tstack_p->start );
	VectorCopy( end, tstack_p->end );
	tstack_p->planenum = 0;
	//start with node 1 because node zero is a dummy for a solid leaf
	tstack_p->nodenum = 1;      //starting at the root of the tree
	tstack_p++;

	while( 1 ) {
		//pop up the stack
		tstack_p--;
		//if the trace stack is empty (ended up with a piece of the
		//line to be traced in an area)
		if( tstack_p < tracestack ) {
			return { areas_, areas_ + numAreas };
		}

		//number of the current node to test the line against
		int nodenum = tstack_p->nodenum;
		//if it is an area
		if( nodenum < 0 ) {
			areas_[numAreas] = -nodenum;
			if( points ) {
				VectorCopy( tstack_p->start, points[numAreas] );
			}
			numAreas++;
			if( numAreas >= maxareas ) {
				return { areas_, areas_ + numAreas };
			}
			continue;
		}
		//if it is a solid leaf
		if( !nodenum ) {
			continue;
		}

		//the node to test against
		const aas_node_t *aasnode = &m_nodes[nodenum];
		//start point of current line to test against node
		VectorCopy( tstack_p->start, cur_start );
		//end point of the current line to test against node
		VectorCopy( tstack_p->end, cur_end );
		//the current node plane
		aas_plane_t *plane = &m_planes[aasnode->planenum];

		float front = DotProduct( cur_start, plane->normal ) - plane->dist;
		float back = DotProduct( cur_end, plane->normal ) - plane->dist;

		//if the whole to be traced line is totally at the front of this node
		//only go down the tree with the front child
		if( front > 0 && back > 0 ) {
			//keep the current start and end point on the stack
			//and go down the tree with the front child
			tstack_p->nodenum = aasnode->children[0];
			tstack_p++;
			if( tstack_p >= &tracestack[127] ) {
				aiError() << "traceAreas() stack overflow";
				return { areas_, areas_ + numAreas };
			}
		}
		//if the whole to be traced line is totally at the back of this node
		//only go down the tree with the back child
		else if( front <= 0 && back <= 0 ) {
			//keep the current start and end point on the stack
			//and go down the tree with the back child
			tstack_p->nodenum = aasnode->children[1];
			tstack_p++;
			if( tstack_p >= &tracestack[127] ) {
				aiError() << "traceAreas() stack overflow";
				return { areas_, areas_ + numAreas };
			}
		}
		//go down the tree both at the front and back of the node
		else {
			int tmpplanenum = tstack_p->planenum;
			//calculate the hitpoint with the node (split point of the line)
			//put the crosspoint TRACEPLANE_EPSILON pixels on the near side
			float frac = front / ( front - back );
			Q_clamp( frac, 0.0f, 1.0f );
			//frac = front / (front-back);
			//
			cur_mid[0] = cur_start[0] + ( cur_end[0] - cur_start[0] ) * frac;
			cur_mid[1] = cur_start[1] + ( cur_end[1] - cur_start[1] ) * frac;
			cur_mid[2] = cur_start[2] + ( cur_end[2] - cur_start[2] ) * frac;

//			AAS_DrawPlaneCross(cur_mid, plane->normal, plane->dist, plane->type, LINECOLOR_RED);
			//side the front part of the line is on
			int side = front < 0;
			//first put the end part of the line on the stack (back side)
			VectorCopy( cur_mid, tstack_p->start );
			//not necesary to store because still on stack
			//VectorCopy(cur_end, tstack_p->end);
			tstack_p->planenum = aasnode->planenum;
			tstack_p->nodenum = aasnode->children[!side];
			tstack_p++;
			if( tstack_p >= &tracestack[127] ) {
				aiError() << "traceAreas() stack overflow";
				return { areas_, areas_ + numAreas };
			}
			//now put the part near the start of the line on the stack so we will
			//continue with thats part first. This way we'll find the first
			//hit of the bbox
			VectorCopy( cur_start, tstack_p->start );
			VectorCopy( cur_mid, tstack_p->end );
			tstack_p->planenum = tmpplanenum;
			tstack_p->nodenum = aasnode->children[side];
			tstack_p++;
			if( tstack_p >= &tracestack[127] ) {
				aiError() << "traceAreas() stack overflow";
				return { areas_, areas_ + numAreas };
			}
		}
	}
}

void AiAasWorld::setupBoxLookupTable( vec3_t *lookupTable, const float *absMins, const float *absMaxs ) {
	// sign bits 0
	VectorSet( lookupTable[0], absMaxs[0], absMaxs[1], absMaxs[2] );
	VectorSet( lookupTable[1], absMins[0], absMins[1], absMins[2] );
	// sign bits 1
	VectorSet( lookupTable[2], absMins[0], absMaxs[1], absMaxs[2] );
	VectorSet( lookupTable[3], absMaxs[0], absMins[1], absMins[2] );
	// sign bits 2
	VectorSet( lookupTable[4], absMaxs[0], absMins[1], absMaxs[2] );
	VectorSet( lookupTable[5], absMins[0], absMaxs[1], absMins[2] );
	// sign bits 3
	VectorSet( lookupTable[6], absMins[0], absMins[1], absMaxs[2] );
	VectorSet( lookupTable[7], absMaxs[0], absMaxs[1], absMins[2] );
	// sign bits 4:
	VectorSet( lookupTable[8], absMaxs[0], absMaxs[1], absMins[2] );
	VectorSet( lookupTable[9], absMins[0], absMins[1], absMaxs[2] );
	// sign bits 5
	VectorSet( lookupTable[10], absMins[0], absMaxs[1], absMins[2] );
	VectorSet( lookupTable[11], absMaxs[0], absMins[1], absMaxs[2] );
	// sign bits 6
	VectorSet( lookupTable[12], absMaxs[0], absMins[1], absMins[2] );
	VectorSet( lookupTable[13], absMins[0], absMaxs[1], absMaxs[2] );
	// sign bits 7
	VectorSet( lookupTable[14], absMins[0], absMins[1], absMins[2] );
	VectorSet( lookupTable[15], absMaxs[0], absMaxs[1], absMaxs[2] );
}

auto AiAasWorld::findAreasInBox( const vec3_t absMins, const vec3_t absMaxs, int *areaNums,
								 int maxAreas, int topNodeHint ) const -> std::span<const int> {
	if( !m_loaded ) {
		return {};
	}

	// A lookup table for inlined BoxOnPlaneSide() body
	vec3_t lookupTable[16];
	setupBoxLookupTable( lookupTable, absMins, absMaxs );

	constexpr const auto nodesStackSize = 1024;
	// Make sure we can access two additional elements to use a single branch for testing stack overflow
	int nodesStack[nodesStackSize + 2];
	int *stackPtr = &nodesStack[0];
	int *writePtr = &areaNums[0];

	// A mask to exclude duplicates in the output (wtf?).
	// We do not want to add AasElementsMask() for it as this is really a hack.
	// (we need a separate table as non-reentrancy could break some caller algorithms)
	// Every word is capable of storing 32 = 2^5 bits so we need 2^16/2^5 words for the maximum number of areas allowed
	uint32_t areasMask[(1 << 16) >> 5];
	const int actualNumWords = m_numareas % 32 ? ( m_numareas + 1 ) / 32 : m_numareas / 32;
	::memset( areasMask, 0, actualNumWords * sizeof( uint32_t ) );

	assert( topNodeHint > 0 );
	*stackPtr++ = topNodeHint;
	for(;; ) {
		// Pop the node
		stackPtr--;
		if( stackPtr < nodesStack ) {
			break;
		}

		const int nodeNum = *stackPtr;
		// If it is an area
		if( nodeNum < 0 ) {
			const int areaNum = -nodeNum;
			const int wordNum = areaNum / 32;
			const uint32_t bit = 1u << ( areaNum % 32 );
			if( areasMask[wordNum] & bit ) {
				continue;
			}

			areasMask[wordNum] |= bit;

			*writePtr++ = areaNum;
			// Put likely case first
			if( writePtr - areaNums < maxAreas ) {
				continue;
			}

			return { areaNums, writePtr };
		}

		// Skip solid leaves
		if( !nodeNum ) {
			continue;
		}

		const auto *const node = &m_nodes[nodeNum];
		const auto *const plane = &m_planes[node->planenum];
		const auto *__restrict normal = plane->normal;
		const int lookupTableIndex = plane->signBits * 2;
		const auto *__restrict lookup0 = lookupTable[lookupTableIndex + 0];
		const auto *__restrict lookup1 = lookupTable[lookupTableIndex + 1];
		const float planeDist = plane->dist;
		// If on the front side of the node
		if( DotProduct( normal, lookup0 ) >= planeDist ) {
			*stackPtr++ = node->children[0];
		}
		// If on the back side of the node
		if( DotProduct( normal, lookup1 ) < planeDist ) {
			*stackPtr++ = node->children[1];
		}

		// Should not happen at all?
		assert( stackPtr - nodesStack < nodesStackSize );
	}

	return { areaNums, writePtr };
}

int AiAasWorld::findTopNodeForBox( const float *boxMins, const float *boxMaxs ) const {
	// Spread bounds a bit to ensure inclusion of boundary planes in an enclosing node
	vec3_t testedMins { -2, -2, -2 };
	vec3_t testedMaxs { +2, +2, +2 };
	VectorAdd( boxMins, testedMins, testedMins );
	VectorAdd( boxMaxs, testedMaxs, testedMaxs );

	vec3_t lookupTable[16];
	setupBoxLookupTable( lookupTable, boxMins, boxMaxs );

	// Caution! AAS root node is 1 contrary to the CM BSP
	int currNode = 1, lastGoodNode = 1;
	for(;; ) {
		const auto *const node = &m_nodes[currNode];
		const auto *const plane = &m_planes[node->planenum];
		const auto *__restrict normal = plane->normal;
		const int lookupTableIndex = plane->signBits * 2;
		const auto *__restrict lookup0 = lookupTable[lookupTableIndex + 0];
		const auto *__restrict lookup1 = lookupTable[lookupTableIndex + 1];
		const float planeDist = plane->dist;

		// Bits of inlined BoxOnPlaneSide() code follow

		int sides = 0;
		// If on the front side of the node
		if( DotProduct( normal, lookup0 ) >= planeDist ) {
			sides = 1;
		}
		// If on the back side of the node
		if( DotProduct( normal, lookup1 ) < planeDist ) {
			sides |= 2;
		}
		// Stop at finding a splitting node
		if( sides == 3 ) {
			return lastGoodNode;
		}
		// TODO: Is there a proof?
		assert( sides > 0 );
		int child = node->children[sides - 1];
		// Stop at areas and at solid world
		if( child <= 0 ) {
			return currNode;
		}
		lastGoodNode = currNode;
		currNode = child;
	}
}

int AiAasWorld::findTopNodeForSphere( const float *center, float radius ) const {
	// Spread radius a bit
	const float testedRadius = radius + 2.0f;
	const float *const __restrict c = center;

	// Caution! AAS root node is 1 contrary to the CM BSP
	int currNode = 1, lastGoodNode = 1;
	for(;; ) {
		const auto *__restrict node = m_nodes + currNode;
		const auto *__restrict plane = m_planes + node->planenum;
		float distanceToPlane = DotProduct( plane->normal, c ) - plane->dist;
		int child;
		if( distanceToPlane > +testedRadius ) {
			child = node->children[0];
		} else if( distanceToPlane < -testedRadius ) {
			child = node->children[1];
		} else {
			return lastGoodNode;
		}
		// Stop at areas and solid world
		if( child <= 0 ) {
			return currNode;
		}
		lastGoodNode = currNode;
		currNode = child;
	}
}

void AiAasWorld::computeExtraAreaData( const wsw::StringView &baseMapName ) {
	BoundsBuilder boundsBuilder;
	for( int areaNum = 1; areaNum < m_numareas; ++areaNum ) {
		const auto &area = m_areas[areaNum];
		boundsBuilder.addPoint( area.mins );
		boundsBuilder.addPoint( area.maxs );
	}

	boundsBuilder.storeTo( m_worldMins, m_worldMaxs );
	m_worldMins[3] = 0.0f;
	m_worldMaxs[3] = 1.0f;

	for( int areaNum = 1; areaNum < m_numareas; ++areaNum ) {
		trySettingAreaLedgeFlags( areaNum );
		trySettingAreaWallFlags( areaNum );
		trySettingAreaJunkFlags( areaNum );
		trySettingAreaRampFlags( areaNum );
	}

	// Call after all other flags have been set
	trySettingAreaSkipCollisionFlags();

	computeLogicalAreaClusters();
	computeFace2DProjVertices();
	computeAreasLeafsLists();

	// Assumes clusters and area leaves to be already computed
	loadAreaVisibility( baseMapName );
	// Depends of area visibility
	loadFloorClustersVisibility( baseMapName );

	// These computations expect (are going to expect) that logical clusters are valid
	for( int areaNum = 1; areaNum < m_numareas; ++areaNum ) {
		trySettingAreaNoFallFlags( areaNum );
	}

	buildSpecificAreaTypesLists();

	computeInnerBoundsForAreas();

	setupPointAreaNumLookupGrid();
}

void AiAasWorld::trySettingAreaLedgeFlags( int areaNum ) {
	auto *const __restrict aasAreaSettings = this->m_areasettings;
	auto *const __restrict aasReach = this->m_reachability;

	auto *const __restrict areaSettings = aasAreaSettings + areaNum;
	const int endReachNum = areaSettings->firstreachablearea + areaSettings->numreachableareas;
	for( int reachNum = areaSettings->firstreachablearea; reachNum != endReachNum; ++reachNum ) {
		const auto &__restrict reach = aasReach[reachNum];
		if( reach.traveltype != TRAVEL_WALKOFFLEDGE ) {
			continue;
		}

		// If the reachability has a substantial height there's no point in doing reverse reach checks
		if( DistanceSquared( reach.start, reach.end ) > 40 * 40 ) {
			areaSettings->areaflags |= AREA_LEDGE;
			return;
		}

		// Check whether a reverse reachability exists (so we can walk/jump back)
		// TODO: Build a table of reverse reachabilites as well? Could be useful for various purposes
		const auto *__restrict nextAreaSettings = this->m_areasettings + reach.areanum;
		const int endRevReachNum = nextAreaSettings->firstreachablearea + nextAreaSettings->numreachableareas;
		for( int revReachNum = nextAreaSettings->firstreachablearea; revReachNum != endRevReachNum; ++revReachNum ) {
			const auto &__restrict revReach = aasReach[revReachNum];
			// Must point back to the area we built flags for
			if( revReach.areanum != areaNum ) {
				continue;
			}
			// Avoid setting flags in this case as we still can walk or jump back
			if( revReach.traveltype == TRAVEL_WALK || revReach.traveltype == TRAVEL_BARRIERJUMP ) {
				// We have found a reverse reachability so there's no point to continue the inner loop
				break;
			}
			// We have found at least a single direct reachability that qualifies as a ledge.
			areaSettings->areaflags |= AREA_LEDGE;
			return;
		}
	}
}

void AiAasWorld::trySettingAreaWallFlags( int areaNum ) {
	int faceIndexNum = m_areas[areaNum].firstface;
	int endFaceIndexNum = m_areas[areaNum].firstface + m_areas[areaNum].numfaces;
	const float *zAxis = &axis_identity[AXIS_UP];

	for(; faceIndexNum != endFaceIndexNum; ++faceIndexNum ) {
		int faceIndex = m_faceindex[faceIndexNum];
		int areaBehindFace;
		const aas_face_t *face;
		if( faceIndex >= 0 ) {
			face = &m_faces[faceIndex];
			areaBehindFace = face->backarea;
		} else   {
			face = &m_faces[-faceIndex];
			areaBehindFace = face->frontarea;
		}

		// There is no solid but some other area behind the face
		if( areaBehindFace ) {
			continue;
		}

		const aas_plane_t *facePlane = &m_planes[face->planenum];
		// Do not treat bounding ceilings and ground as a wall
		if( fabsf( DotProduct( zAxis, facePlane->normal ) ) < 0.3f ) {
			m_areasettings[areaNum].areaflags |= AREA_WALL;
			break;
		}
	}
}

void AiAasWorld::trySettingAreaJunkFlags( int areaNum ) {
	const aas_area_t &area = m_areas[areaNum];
	int junkFactor = 0;

	// Changed to test only 2D dimensions, otherwise there will be way too many bogus ramp flags set
	for( int i = 0; i < 2; ++i ) {
		if( area.maxs[i] - area.mins[i] < 24.0f ) {
			++junkFactor;
		}
	}
	if( junkFactor > 1 ) {
		m_areasettings[areaNum].areaflags |= AREA_JUNK;
	}
}

void AiAasWorld::trySettingAreaRampFlags( int areaNum ) {
	// Since we extend the trace end a bit below the area,
	// this test is added to avoid classifying non-grounded areas as having a ramp
	if( !( m_areasettings[areaNum].areaflags & AREA_GROUNDED ) ) {
		return;
	}
	// Skip junk areas as well
	if( m_areasettings[areaNum].areaflags & AREA_JUNK ) {
		return;
	}

	// AAS does not make a distinction for areas having an inclined floor.
	// This leads to a poor bot behaviour since bots threat areas of these kind as obstacles.
	// Moreover if an "area" (which should not be a single area) has both flat and inclined floor parts,
	// the inclined part is still ignored.

	// There is an obvious approach of testing ground faces of the area but it does not work for several reasons
	// (some faces are falsely marked as FACE_GROUND).

	const auto &area = m_areas[areaNum];
	// Since an area might contain both flat and inclined part, we cannot just test a trace going through the center
	float stepX = 0.2f * ( area.maxs[0] - area.mins[0] );
	float stepY = 0.2f * ( area.maxs[1] - area.mins[1] );

	static const float zNormalThreshold = cosf( DEG2RAD( 2.0f ) );

	trace_t trace;
	for( int i = -2; i <= 2; ++i ) {
		for( int j = -2; j <= 2; ++j ) {
			Vec3 start( area.center );
			Vec3 end( area.center );
			start.X() += stepX * i;
			start.Y() += stepY * j;
			end.X() += stepX * i;
			end.Y() += stepY * j;

			// These margins added are absolutely required in order to produce satisfiable results
			start.Z() = area.maxs[2] + 16.0f;
			end.Z() = area.mins[2] - 16.0f;

			G_Trace( &trace, start.Data(), nullptr, nullptr, end.Data(), nullptr, MASK_PLAYERSOLID );
			if( trace.fraction == 1.0f || trace.startsolid ) {
				continue;
			}

			if( !ISWALKABLEPLANE( &trace.plane ) ) {
				continue;
			}

			if( trace.plane.normal[2] > zNormalThreshold ) {
				continue;
			}

			// Check whether we're still in the same area
			if( trace.endpos[2] < area.mins[2] || trace.endpos[2] > area.maxs[2] ) {
				continue;
			}

			// TODO: This does not really work for some weird reasons so we have to live with false positives
			// Area bounds extend the actual area geometry,
			// so a point might be within the bounds but outside the area hull
			//Vec3 testedPoint( trace.endpos );
			//testedPoint.Z() += 1.0f;
			//if( PointAreaNum( testedPoint.Data() ) != areaNum ) {
			//	continue;
			//}

			m_areasettings[areaNum].areaflags |= AREA_INCLINED_FLOOR;
			if( trace.plane.normal[2] <= 1.0f - SLIDEMOVE_PLANEINTERACT_EPSILON ) {
				m_areasettings[areaNum].areaflags |= AREA_SLIDABLE_RAMP;
				// All flags that could be set are present
				return;
			}
		}
	}
}

class NofallAreaFlagSolver: public SharedFaceAreasWalker<ArrayBasedFringe<64>> {
	vec3_t testedBoxMins;
	vec3_t testedBoxMaxs;

	const std::span<const aas_area_t> aasAreas;
	const std::span<const aas_areasettings_t> aasAreaSettings;

	bool result { true };

	bool ProcessAreaTransition( int currArea, int nextArea, const aas_face_t *face ) override;
	bool AreAreaContentsBad( int areaNum ) const;
public:
	NofallAreaFlagSolver( int areaNum_, AiAasWorld *aasWorld_ );

	bool Result() const { return result; }

	void Exec() override;
};

NofallAreaFlagSolver::NofallAreaFlagSolver( int areaNum_, AiAasWorld *aasWorld_ )
	: SharedFaceAreasWalker( areaNum_, AasElementsMask::AreasMask(), AasElementsMask::FacesMask() )
	, aasAreas( aasWorld_->getAreas() )
	, aasAreaSettings( aasWorld_->getAreaSettings() ) {

	VectorSet( testedBoxMins, -48, -48, -99999 );
	VectorSet( testedBoxMaxs, +48, +48, +16 );

	const auto &__restrict area = aasAreas[areaNum_];

	VectorAdd( testedBoxMins, area.mins, testedBoxMins );
	VectorAdd( testedBoxMaxs, area.maxs, testedBoxMaxs );
}

bool NofallAreaFlagSolver::AreAreaContentsBad( int areaNum ) const {
	constexpr auto badContents = AREACONTENTS_LAVA | AREACONTENTS_SLIME | AREACONTENTS_DONOTENTER;
	// We also include triggers/movers as undesired to prevent trigger activation without intention
	constexpr auto triggerContents = AREACONTENTS_JUMPPAD | AREACONTENTS_TELEPORTER | AREACONTENTS_MOVER;
	constexpr auto undesiredContents = badContents | triggerContents;

	return (bool)( aasAreaSettings[areaNum].contents & undesiredContents );
}

bool NofallAreaFlagSolver::ProcessAreaTransition( int currAreaNum, int nextAreaNum, const aas_face_t *face ) {
	// Continue traversal if there's a solid contents behind the face
	if( !nextAreaNum ) {
		return true;
	}

	// Put this first as this should be relatively cheap
	if( !visitedAreas->TrySet( nextAreaNum ) ) {
		return true;
	}

	const auto &__restrict nextArea = aasAreas[nextAreaNum];
	// Continue traversal if the area is completely outside of the bounds we're interested in
	if( !BoundsIntersect( nextArea.mins, nextArea.maxs, testedBoxMins, testedBoxMaxs ) ) {
		return true;
	}

	// Interrupt in this case
	if( AreAreaContentsBad( nextAreaNum ) ) {
		result = false;
		return false;
	}

	// We disallow LEDGE areas as initial areas but allow transition to LEDGE areas
	// If the area is a LEDGE area check whether we may actually fall while being within the tested bounds
	const auto &__restrict currAreaSettings = aasAreaSettings[currAreaNum];
	if( currAreaSettings.areaflags & AREA_LEDGE ) {
		const auto &__restrict currArea = aasAreas[currAreaNum];
		if( currArea.mins[2] > nextArea.mins[2] + 20 ) {
			result = false;
			return false;
		}
	}

	queue.Add( nextAreaNum );
	return true;
}

void NofallAreaFlagSolver::Exec() {
	const int startAreaNum = queue.Peek();

	if( aasAreaSettings[startAreaNum].areaflags & AREA_LEDGE ) {
		result = false;
		return;
	}

	if( AreAreaContentsBad( startAreaNum ) ) {
		result = false;
		return;
	}

	SharedFaceAreasWalker::Exec();
}

void AiAasWorld::trySettingAreaNoFallFlags( int areaNum ) {
	auto &__restrict areaSettings = m_areasettings[areaNum];
	if( areaSettings.areaflags & ( AREA_LIQUID | AREA_DISABLED ) ) {
		return;
	}
	// NOFALL flag do not make sense for non-grounded areas
	if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
		return;
	}

	NofallAreaFlagSolver solver( areaNum, this );
	solver.Exec();
	if( solver.Result() ) {
		areaSettings.areaflags |= AREA_NOFALL;
	}
}

void AiAasWorld::trySettingAreaSkipCollisionFlags() {
	const float extentsForFlags[3] = { 64, 48, 32 };
	int flagsToSet[3]              = { AREA_SKIP_COLLISION_64, AREA_SKIP_COLLISION_48, AREA_SKIP_COLLISION_32 };

	// Set all "lesser" flags for a specific flag if a collision test for the flag passes
	for( int flagNum = 0; flagNum < 2; ++flagNum ) {
		for( int rightFlagNum = flagNum + 1; rightFlagNum < 3; ++rightFlagNum ) {
			flagsToSet[flagNum] |= flagsToSet[rightFlagNum];
		}
	}

	const int clipMask       = MASK_PLAYERSOLID | MASK_WATER | CONTENTS_TRIGGER | CONTENTS_JUMPPAD | CONTENTS_TELEPORTER;
	const float playerHeight = playerbox_stand_maxs[2] - playerbox_stand_mins[2];
	const float playerRadius = 0.5f * ( M_SQRT2 * ( playerbox_stand_maxs[0] - playerbox_stand_mins[0] ) );
	for( int areaNum = 1; areaNum < m_numareas; ++areaNum ) {
		int *const areaFlags = &m_areasettings[areaNum].areaflags;
		// If it is already known that the area is bounded by a solid wall or is an inclined floor area
		if( !( *areaFlags & ( AREA_WALL | AREA_INCLINED_FLOOR ) ) ) {
			const aas_area_t &area      = m_areas[areaNum];
			const Vec3 areaRelativeMins = Vec3( area.mins ) - area.center;
			const Vec3 areaRelativeMaxs = Vec3( area.maxs ) - area.center;
			const float addedMinsZ      = ( *areaFlags & AREA_GROUNDED ) ? 1.0f : 0.0f;

			for( int flagNum = 0; flagNum < 3; ++flagNum ) {
				const float extent = extentsForFlags[flagNum];

				const Vec3 addedMins( -extent - playerRadius, -extent - playerRadius, addedMinsZ );
				// Ensure that there's always a room for a player above
				// even if a player barely touches area top by their feet
				const Vec3 addedMaxs( +extent + playerRadius, +extent + playerRadius, playerHeight );

				Vec3 testedMins = areaRelativeMins + addedMins;
				Vec3 testedMaxs = areaRelativeMaxs + addedMaxs;

				float minMaxsZ = playerHeight + extent;
				if( testedMaxs.Z() < minMaxsZ ) {
					testedMaxs.Z() = minMaxsZ;
				}

				trace_t trace;
				G_Trace( &trace, area.center, testedMins.Data(), testedMaxs.Data(), area.center, nullptr, clipMask );
				if ( trace.fraction == 1.0f && !trace.startsolid ) {
					*areaFlags |= flagsToSet[flagNum];
					break;
				}
			}
		}
	}
}

void AiAasWorld::computeInnerBoundsForAreas() {
    m_areaInnerBounds = (int16_t *)Q_malloc( 6 * sizeof( int16_t ) * m_numareas );

    VectorClear( m_areaInnerBounds + 0 );
    VectorClear( m_areaInnerBounds + 3 );

    const vec3_t addToMins { +0.5f, +0.5f, +0.5f };
    const vec3_t addToMaxs { -0.5f, -0.5f, -0.5f };
    for( int areaNum = 1; areaNum < m_numareas; ++areaNum ) {
        const auto &area = m_areas[areaNum];
        vec3_t innerMins, innerMaxs;
        VectorCopy( area.mins, innerMins );
        VectorCopy( area.maxs, innerMaxs );

        for( int faceIndexNum = area.firstface; faceIndexNum < area.firstface + area.numfaces; ++faceIndexNum ) {
            const auto &face = m_faces[std::abs( m_faceindex[faceIndexNum] )];
            for( int edgeIndexNum = face.firstedge; edgeIndexNum < face.firstedge + face.numedges; ++edgeIndexNum ) {
                const auto &edge = m_edges[std::abs( m_edgeindex[edgeIndexNum] )];
                for( int edgeVertex = 0; edgeVertex < 2; ++edgeVertex ) {
                    assert( edge.v[edgeVertex] >= 0 );
                    const float *v = m_vertexes[edge.v[edgeVertex]];
                    // For each coordinate check what "semi-space" it belongs to based on comparison to the area center.
                    // Select the maximal value of mins and minimal one of maxs.
                    for( int i = 0; i < 3; ++i ) {
                        if( v[i] < area.center[i] ) {
                            if( innerMins[i] < v[i] ) {
                                innerMins[i] = v[i];
                            }
                        } else {
                            if( innerMaxs[i] > v[i] ) {
                                innerMaxs[i] = v[i];
                            }
                        }
                    }
                }
            }
        }

        VectorAdd( innerMins, addToMins, innerMins );
        VectorAdd( innerMaxs, addToMaxs, innerMaxs );
        VectorCopy( innerMins, m_areaInnerBounds + areaNum * 6 + 0 );
        VectorCopy( innerMaxs, m_areaInnerBounds + areaNum * 6 + 3 );
    }
}

void AiAasWorld::setupPointAreaNumLookupGrid() {
	const Vec3 worldMins( m_worldMins );
	const Vec3 dimensions( Vec3( m_worldMaxs ) - worldMins );
	const Vec3 cellDimensions( kAreaGridCellSize, kAreaGridCellSize, kAreaGridCellSize );

	size_t gridDataSize = sizeof( int32_t );
	for( int i = 0; i < 3; ++i ) {
		// Truncating the fractional part is used during lookups.
		// Add +1 to account for points that are located on world boundaries (they aren't that uncommon).
		m_numGridCellsPerDimensions[i] = (unsigned)std::floor( dimensions.Data()[i] / kAreaGridCellSize ) + 1;
		gridDataSize *= m_numGridCellsPerDimensions[i];
	}

	m_pointAreaNumLookupGridData = (int32_t *)Q_malloc( (size_t)gridDataSize );

	size_t offset = 0;
	for( unsigned iStep = 0; iStep < m_numGridCellsPerDimensions[0]; ++iStep ) {
		const float minX = worldMins.X() + kAreaGridCellSize * (float)iStep;
		for( unsigned jStep = 0; jStep < m_numGridCellsPerDimensions[1]; ++jStep ) {
			const float minY = worldMins.Y() + kAreaGridCellSize * (float)jStep;
			for( unsigned kStep = 0; kStep < m_numGridCellsPerDimensions[2]; ++kStep ) {
				const float minZ = worldMins.Z() + kAreaGridCellSize * (float)kStep;
				const Vec3 cellMins( minX, minY, minZ );
				const Vec3 cellMaxs( cellMins + cellDimensions );
				const int32_t encoded = computePointAreaNumLookupDataForCell( cellMins, cellMaxs );
				m_pointAreaNumLookupGridData[offset++] = encoded;
			}
		}
	}
}

auto AiAasWorld::computePointAreaNumLookupDataForCell( const Vec3 &cellMins, const Vec3 &cellMaxs ) const -> int32_t {
	// Let cells overlap a bit so we don't have to care of border point lookup issues
	const Vec3 expandedMins( Vec3( -2.0f, -2.0f, -2.0f ) + cellMins );
	const Vec3 expandedMaxs( Vec3( +2.0f, +2.0f, +2.0f ) + cellMaxs );

	int areaNumsBuffer[16];
	const auto areaNums = findAreasInBox( expandedMins, expandedMaxs, areaNumsBuffer, 16 );
	if( areaNums.size() == 1 ) {
		const Vec3 *bounds[2] { &expandedMins, &expandedMaxs };
		int lastCornerAreaNum = -1;
		bool allInsideTheSameArea = true;
		for( unsigned i = 0; i < 8; ++i ) {
			const Vec3 corner( bounds[( i >> 2 ) & 1]->X(), bounds[( i >> 1 ) & 1]->Y(), bounds[( i >> 0 ) & 1]->Z() );
			const int cornerAreaNum = pointAreaNumNaive( corner.Data() );
			if( i > 0 ) {
				if( cornerAreaNum != lastCornerAreaNum ) {
					allInsideTheSameArea = false;
					break;
				}
			}
			lastCornerAreaNum = cornerAreaNum;
		}
		// If all corners are inside the same area, the entire box is inside the same area
		if( allInsideTheSameArea ) {
			assert( lastCornerAreaNum >= 0 );
			return -lastCornerAreaNum;
		}
	}

	// Supply original bounds (findTopNodeForBox() spreads bounds itself)
	const int topNode = findTopNodeForBox( cellMins.Data(), cellMaxs.Data() );
	assert( topNode > 0 && topNode < m_numnodes );
	return topNode;
}

namespace {
#ifndef WSW_USE_SSE2
[[nodiscard]]
inline bool isPointWithinWorldBounds( const float *point, const float *mins, const float *maxs ) {
	return ( point[0] >= mins[0] ) & ( point[0] <= maxs[0] ) &
		   ( point[1] >= mins[1] ) & ( point[1] <= maxs[1] ) &
		   ( point[2] >= mins[2] ) & ( point[2] <= maxs[2] );
}
#else
[[nodiscard]]
inline bool isPointWithinWorldBounds( const float *point, const float *mins, const float *maxs ) {
	assert( mins[3] == 0.0f && maxs[3] == 1.0f );
	__m128 xmmPoint = _mm_setr_ps( point[0], point[1], point[2], 0.5f );
	__m128 xmmMins  = _mm_load_ps( mins );
	__m128 xmmMaxs  = _mm_load_ps( maxs );
	__m128 xmmGt1   = _mm_cmpgt_ps( xmmMins, xmmPoint );
	__m128 xmmGt2   = _mm_cmpgt_ps( xmmPoint, xmmMaxs );
	return !_mm_movemask_ps( _mm_or_ps( xmmGt1, xmmGt2 ) );
}
#endif
}

int AiAasWorld::pointAreaNum( const float *point ) const {
	assert( std::isfinite( point[0] ) );
	assert( std::isfinite( point[1] ) );
	assert( std::isfinite( point[2] ) );

	if( m_loaded ) [[likely]] {
		if( isPointWithinWorldBounds( point, m_worldMins, m_worldMaxs ) ) [[likely]] {
			const Vec3 diffWithMins( Vec3( point ) - Vec3( m_worldMins ) );

			constexpr double rcpAreaGridCellSize = 1.0 / kAreaGridCellSize;
			const auto xCellIndex = (unsigned)( (double)diffWithMins.X() * rcpAreaGridCellSize );
			const auto yCellIndex = (unsigned)( (double)diffWithMins.Y() * rcpAreaGridCellSize );
			const auto zCellIndex = (unsigned)( (double)diffWithMins.Z() * rcpAreaGridCellSize );

			assert( xCellIndex < m_numGridCellsPerDimensions[0] );
			assert( yCellIndex < m_numGridCellsPerDimensions[1] );
			assert( zCellIndex < m_numGridCellsPerDimensions[2] );

			size_t offset = 0;
			offset += xCellIndex * m_numGridCellsPerDimensions[1] * m_numGridCellsPerDimensions[2];
			offset += yCellIndex * m_numGridCellsPerDimensions[2];
			offset += zCellIndex;

			int result = 0;
			const int32_t encoded = m_pointAreaNumLookupGridData[offset];
			if( const int areaNum = -encoded; areaNum > 0 ) {
				result = areaNum;
			} else if( const int topNodeHint = encoded; topNodeHint > 0 ) {
				result = pointAreaNumNaive( point, topNodeHint );
			}
			return result;
		}
	}
	return 0;
}

static void AAS_DData( unsigned char *data, int size ) {
	for( int i = 0; i < size; i++ ) {
		data[i] ^= (unsigned char) i * 119;
	}
}

#define AAS_LUMPS                   14
#define AASLUMP_BBOXES              0
#define AASLUMP_VERTEXES            1
#define AASLUMP_PLANES              2
#define AASLUMP_EDGES               3
#define AASLUMP_EDGEINDEX           4
#define AASLUMP_FACES               5
#define AASLUMP_FACEINDEX           6
#define AASLUMP_AREAS               7
#define AASLUMP_AREASETTINGS        8
#define AASLUMP_REACHABILITY        9
#define AASLUMP_NODES               10
#define AASLUMP_PORTALS             11
#define AASLUMP_PORTALINDEX         12
#define AASLUMP_CLUSTERS            13

class AasFileReader
{
	int fp;
	int lastoffset;

	//header lump
	typedef struct {
		int fileofs;
		int filelen;
	} aas_lump_t;

	//aas file header
	typedef struct aas_header_s {
		int ident;
		int version;
		int bspchecksum;
		//data entries
		aas_lump_t lumps[AAS_LUMPS];
	} aas_header_t;

	aas_header_t header;
	int fileSize;

	char *LoadLump( int lumpNum, int size );

public:
	AasFileReader( const wsw::StringView &mapName );

	~AasFileReader() {
		if( fp ) {
			FS_FCloseFile( fp );
		}
	}

	bool IsValid() { return fp != 0; }

	template<typename T>
	std::tuple<T*, int> LoadLump( int lumpNum ) {
		int oldOffset = lastoffset;
		char *rawData = LoadLump( lumpNum, sizeof( T ) );
		int length = lastoffset - oldOffset;

		return std::make_tuple( (T*)rawData, length / sizeof( T ) );
	};

	bool ComputeChecksum( char **base64Digest );
};

#define AASID                       ( ( 'S' << 24 ) + ( 'A' << 16 ) + ( 'A' << 8 ) + 'E' )
#define AASVERSION_OLD              4
#define AASVERSION                  5

AasFileReader::AasFileReader( const wsw::StringView &baseMapName )
	: lastoffset( 0 ) {
	// Shut up an analyzer
	memset( &header, 0, sizeof( header ) );

	wsw::StaticString<MAX_QPATH> filePath;
	AiAasWorld::makeFileName( filePath, baseMapName, ".aas"_asView );

	fileSize = FS_FOpenFile( filePath.data(), &fp, FS_READ );
	if( !fp || fileSize <= 0 ) {
		G_Printf( S_COLOR_RED "can't open %s\n", filePath.data() );
		return;
	}

	//read the header
	FS_Read( &header, sizeof( aas_header_t ), fp );
	lastoffset = sizeof( aas_header_t );
	//check header identification
	header.ident = LittleLong( header.ident );
	if( header.ident != AASID ) {
		G_Printf( S_COLOR_RED "%s is not an AAS file\n", filePath.data() );
		return;
	}

	//check the version
	header.version = LittleLong( header.version );
	if( header.version != AASVERSION_OLD && header.version != AASVERSION ) {
		G_Printf( S_COLOR_RED "aas file %s is version %i, not %i\n", filePath.data(), header.version, AASVERSION );
		return;
	}
	if( header.version == AASVERSION ) {
		AAS_DData( (unsigned char *) &header + 8, sizeof( aas_header_t ) - 8 );
	}
}

char *AasFileReader::LoadLump( int lumpNum, int size ) {
	int offset = LittleLong( header.lumps[lumpNum].fileofs );
	int length = LittleLong( header.lumps[lumpNum].filelen );

	if( !length ) {
		//just alloc a dummy
		return (char *) Q_malloc( size + 1 );
	}
	//seek to the data
	if( offset != lastoffset ) {
		G_Printf( S_COLOR_YELLOW "AAS file not sequentially read\n" );
		if( FS_Seek( fp, offset, FS_SEEK_SET ) ) {
			G_Printf( S_COLOR_RED "can't seek to aas lump\n" );
			return nullptr;
		}
	}
	//allocate memory
	char *buf = (char *) Q_malloc( length + 1 );
	//read the data
	if( length ) {
		FS_Read( buf, length, fp );
		lastoffset += length;
	}
	return buf;
}

bool AasFileReader::ComputeChecksum( char **base64Digest ) {
	if( FS_Seek( fp, 0, FS_SEEK_SET ) < 0 ) {
		return false;
	}

	// TODO: Read the entire AAS data at start and then use the read chunk for loading of AAS lumps
	char *mem = (char *)Q_malloc( (unsigned)fileSize );
	if( FS_Read( mem, (unsigned)fileSize, fp ) <= 0 ) {
		Q_free( mem );
		return false;
	}

	// Compute a binary MD5 digest of the file data first
	md5_byte_t binaryDigest[16];
	md5_digest( mem, fileSize, binaryDigest );

	// Get a base64-encoded digest in a temporary buffer allocated via malloc()
	size_t base64Length;
	char *tmpBase64Chars = ( char * )base64_encode( binaryDigest, 16, &base64Length );

	// Free the level data
	Q_free( mem );

	// Copy the base64-encoded digest to the game memory storage to avoid further confusion
	*base64Digest = ( char * )Q_malloc( base64Length + 1 );
	// Include the last zero byte in copied chars
	memcpy( *base64Digest, tmpBase64Chars, base64Length + 1 );

	free( tmpBase64Chars );

	return true;
}

bool AiAasWorld::load( const wsw::StringView &mapName ) {
	AasFileReader reader( mapName );

	if( !reader.IsValid() ) {
		return false;
	}

	std::tie( m_vertexes, m_numvertexes ) = reader.LoadLump<aas_vertex_t>( AASLUMP_VERTEXES );
	if( m_numvertexes && !m_vertexes ) {
		return false;
	}

	std::tie( m_planes, m_numplanes ) = reader.LoadLump<aas_plane_t>( AASLUMP_PLANES );
	if( m_numplanes && !m_planes ) {
		return false;
	}

	std::tie( m_edges, m_numedges ) = reader.LoadLump<aas_edge_t>( AASLUMP_EDGES );
	if( m_numedges && !m_edges ) {
		return false;
	}

	std::tie( m_edgeindex, m_edgeindexsize ) = reader.LoadLump<int>( AASLUMP_EDGEINDEX );
	if( m_edgeindexsize && !m_edgeindex ) {
		return false;
	}

	std::tie( m_faces, m_numfaces ) = reader.LoadLump<aas_face_t>( AASLUMP_FACES );
	if( m_numfaces && !m_faces ) {
		return false;
	}

	std::tie( m_faceindex, m_faceindexsize ) = reader.LoadLump<int>( AASLUMP_FACEINDEX );
	if( m_faceindexsize && !m_faceindex ) {
		return false;
	}

	std::tie( m_areas, m_numareas ) = reader.LoadLump<aas_area_t>( AASLUMP_AREAS );
	if( m_numareas && !m_areas ) {
		return false;
	}

	std::tie( m_areasettings, m_numareasettings ) = reader.LoadLump<aas_areasettings_t>( AASLUMP_AREASETTINGS );
	if( m_numareasettings && !m_areasettings ) {
		return false;
	}

	std::tie( m_reachability, m_reachabilitysize ) = reader.LoadLump<aas_reachability_t>( AASLUMP_REACHABILITY );
	if( m_reachabilitysize && !m_reachability ) {
		return false;
	}

	std::tie( m_nodes, m_numnodes ) = reader.LoadLump<aas_node_t>( AASLUMP_NODES );
	if( m_numnodes && !m_nodes ) {
		return false;
	}

	std::tie( m_portals, m_numportals ) = reader.LoadLump<aas_portal_t>( AASLUMP_PORTALS );
	if( m_numportals && !m_portals ) {
		return false;
	}

	std::tie( m_portalindex, m_portalindexsize ) = reader.LoadLump<int>( AASLUMP_PORTALINDEX );
	if( m_portalindexsize && !m_portalindex ) {
		return false;
	}

	std::tie( m_clusters, m_numclusters ) = reader.LoadLump<aas_cluster_t>( AASLUMP_CLUSTERS );
	if( m_numclusters && !m_clusters ) {
		return false;
	}

	m_checksum = nullptr;
	if( !reader.ComputeChecksum( &m_checksum ) ) {
		return false;
	}

	swapData();

	m_loaded = true;
	return true;
}

void AiAasWorld::postLoad( const wsw::StringView &baseMapName ) {
	// This is important for further PostLoad() computations
	AasElementsMask::Init( this );

	categorizePlanes();
	computeExtraAreaData( baseMapName );
}

AiAasWorld::~AiAasWorld() {
	// This is valid to call even if there was no matching Init() call.
	// To avoid possible issues if the code gets reorganized, call it always.
	AasElementsMask::Shutdown();

	if( !m_loaded ) {
		return;
	}

	Q_free( m_checksum );

	Q_free( m_vertexes );
	Q_free( m_planes );
	Q_free( m_edges );
	Q_free( m_edgeindex );
	Q_free( m_faces );
	Q_free( m_faceindex );
	Q_free( m_areas );
	Q_free( m_areasettings );
	Q_free( m_reachability );
	Q_free( m_nodes );
	Q_free( m_portals );
	Q_free( m_portalindex );
	Q_free( m_clusters );

	Q_free( m_areaFloorClusterNums );
	Q_free( m_areaStairsClusterNums );
	Q_free( m_floorClusterDataOffsets );
	Q_free( m_stairsClusterDataOffsets );
	Q_free( m_floorClusterData );
	Q_free( m_stairsClusterData );

	Q_free( m_face2DProjVertexNums );
	Q_free( m_areaMapLeafListOffsets );
	Q_free( m_areaMapLeafsData );
	Q_free( m_areaVisDataOffsets );
	Q_free( m_areaVisData );
	Q_free( m_floorClustersVisTable );
	Q_free( m_groundedPrincipalRoutingAreas );
	Q_free( m_jumppadReachPassThroughAreas );
	Q_free( m_ladderReachPassThroughAreas );
	Q_free( m_elevatorReachPassThroughAreas );
	Q_free( m_walkOffLedgePassThroughAirAreas );
	Q_free( m_areaInnerBounds );
	Q_free( m_pointAreaNumLookupGridData );
}

void AiAasWorld::categorizePlanes() {
	// We do not trust the AAS compiler and classify planes on our own
	for( int i = 0; i < m_numplanes; ++i ) {
		auto *aasPlane = &m_planes[i];
		cplane_t cmPlane;
		VectorCopy( aasPlane->normal, cmPlane.normal );
		cmPlane.dist = aasPlane->dist;
		CategorizePlane( &cmPlane );
		aasPlane->type = cmPlane.type;
		aasPlane->signBits = cmPlane.signbits;
	}
}

void AiAasWorld::swapData() {
	// TODO: We don't really care of big endian

	// We have to shift all vertices/bounding boxes by this value,
	// as the entire bot code expects area mins to match ground,
	// and values loaded as-is are -shifts[2] units above the ground.
	// This behavior is observed not only on maps compiled by the Qfusion-compatible BSPC, but on vanilla Q3 maps as well.
	// XY-shifts are also observed, but are not so painful as the Z one is.
	// Also XY shifts seem to vary (?) from map to map and even in the same map.
	const vec3_t shifts = { 0, 0, -24.0f + 0.25f };

	for( int i = 0; i < m_numvertexes; i++ ) {
		for( int j = 0; j < 3; j++ ) {
			m_vertexes[i][j] = m_vertexes[i][j] + shifts[j];
		}
	}
}

// ClassfiyFunc operator() invocation must yield these results:
// -1: the area should be marked as flooded and skipped
//  0: the area should be skipped without marking as flooded
//  1: the area should be marked as flooded and put in the results list
template<typename ClassifyFunc>
class AreasClusterBuilder {
protected:
	ClassifyFunc classifyFunc;

	BitVector *const areasMask;

	uint16_t *resultsBase;
	uint16_t *resultsPtr;

	const AiAasWorld *aasWorld;

	vec3_t floodedRegionMins;
	vec3_t floodedRegionMaxs;

public:
	AreasClusterBuilder( BitVector *const areasMask_, uint16_t *resultsBuffer, AiAasWorld *aasWorld_ )
		: areasMask( areasMask_ ), resultsBase( resultsBuffer ), aasWorld( aasWorld_ ) {}

	void FloodAreasRecursive( int areaNum );

	void PrepareToFlood() {
		areasMask->Clear();
		resultsPtr = &resultsBase[0];
		ClearBounds( floodedRegionMins, floodedRegionMaxs );
	}

	const uint16_t *ResultAreas() const { return resultsBase; }
	int ResultSize() const { return (int)( resultsPtr - resultsBase ); }
};

template <typename ClassifyFunc>
void AreasClusterBuilder<ClassifyFunc>::FloodAreasRecursive( int areaNum ) {
	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto aasReaches = aasWorld->getReaches();

	// TODO: Rewrite to stack-based non-recursive version

	*resultsPtr++ = (uint16_t)areaNum;
	areasMask->Set( areaNum, true );

	const auto &currArea = aasAreas[areaNum];
	AddPointToBounds( currArea.mins, floodedRegionMins, floodedRegionMaxs );
	AddPointToBounds( currArea.maxs, floodedRegionMins, floodedRegionMaxs );

	const auto &currAreaSettings = aasAreaSettings[areaNum];
	int reachNum = currAreaSettings.firstreachablearea;
	const int maxReachNum = reachNum + currAreaSettings.numreachableareas;
	for( ; reachNum < maxReachNum; ++reachNum ) {
		const auto &reach = aasReaches[reachNum];
		if( areasMask->IsSet( reach.areanum ) ) {
			continue;
		}

		int classifyResult = classifyFunc( currArea, reach, aasAreas[reach.areanum], aasAreaSettings[reach.areanum] );
		if( classifyResult < 0 ) {
			areasMask->Set( reach.areanum, true );
			continue;
		}

		if( classifyResult > 0 ) {
			FloodAreasRecursive( reach.areanum );
		}
	}
}

struct ClassifyFloorArea
{
	int operator()( const aas_area_t &currArea,
					const aas_reachability_t &reach,
					const aas_area_t &reachArea,
					const aas_areasettings_t &reachAreaSetttings ) {
		if( reach.traveltype != TRAVEL_WALK ) {
			// Do not disable the area for further search,
			// it might be reached by walking through some intermediate area
			return 0;
		}

		if( fabsf( reachArea.mins[2] - currArea.mins[2] ) > 1.0f ) {
			// Disable the area for further search
			return -1;
		}

		if( !LooksLikeAFloorArea( reachAreaSetttings ) ) {
			// Disable the area for further search
			return -1;
		}

		return 1;
	}

	bool LooksLikeAFloorArea( const aas_areasettings_t &areaSettings ) {
		if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
			return false;
		}
		if( areaSettings.areaflags & AREA_INCLINED_FLOOR ) {
			return false;
		}
		return true;
	}
};

class FloorClusterBuilder : public AreasClusterBuilder<ClassifyFloorArea> {
	bool IsFloodedRegionDegenerate() const;
public:
	FloorClusterBuilder( BitVector *areasMask_, uint16_t *resultsBuffer, AiAasWorld *aasWorld_ )
		: AreasClusterBuilder( areasMask_, resultsBuffer, aasWorld_ ) {}

	bool Build( int startAreaNum );
};

bool FloorClusterBuilder::IsFloodedRegionDegenerate() const {
	float dimsSum = 0.0f;
	for( int i = 0; i < 2; ++i ) {
		float dims = floodedRegionMaxs[i] - floodedRegionMins[i];
		if( dims < 48.0f ) {
			return true;
		}
		dimsSum += dims;
	}

	// If there are only few single area, apply greater restrictions
	switch( ResultSize() ) {
		case 1: return dimsSum < 256.0f + 32.0f;
		case 2: return dimsSum < 192.0f + 32.0f;
		case 3: return dimsSum < 144.0f + 32.0f;
		default: return dimsSum < 144.0f;
	}
}

bool FloorClusterBuilder::Build( int startAreaNum ) {
	if( !classifyFunc.LooksLikeAFloorArea( aasWorld->getAreaSettings()[startAreaNum] ) ) {
		return false;
	}

	PrepareToFlood();

	FloodAreasRecursive( startAreaNum );

	return ResultSize() && !IsFloodedRegionDegenerate();
}

struct ClassifyStairsArea {
	int operator()( const aas_area_t &currArea,
					const aas_reachability_t &reach,
					const aas_area_t &reachArea,
					const aas_areasettings_t &reachAreaSettings ) {
		if( reach.traveltype != TRAVEL_WALK && reach.traveltype != TRAVEL_WALKOFFLEDGE && reach.traveltype != TRAVEL_JUMP ) {
			// Do not disable the area for further search,
			// it might be reached by walking through some intermediate area
			return 0;
		}

		// Check whether there is a feasible height difference with the current area
		float relativeHeight = fabsf( reachArea.mins[2] - currArea.mins[2] );
		if( relativeHeight < 4 || relativeHeight > -playerbox_stand_mins[2] ) {
			// Disable the area for further search
			return -1;
		}

		// HACK: TODO: Refactor this (operator()) method params
		const auto *aasWorld = AiAasWorld::instance();
		if( aasWorld->floorClusterNum( std::addressof( currArea ) - std::addressof( aasWorld->getAreas()[0] ) ) ) {
			// The area is already in a floor cluster
			return -1;
		}

		if( !LooksLikeAStairsArea( reachArea, reachAreaSettings ) ) {
			// Disable the area for further search
			return -1;
		}

		return 1;
	}

	bool LooksLikeAStairsArea( const aas_area_t &area, const aas_areasettings_t &areaSettings ) {
		if( !( areaSettings.areaflags & AREA_GROUNDED ) ) {
			return false;
		}
		if( areaSettings.areaflags & AREA_INCLINED_FLOOR ) {
			return false;
		}

		// TODO: There should be more strict tests... A substantial amount of false positives is noticed.

		// Check whether the area top projection looks like a stretched rectangle
		float dx = area.maxs[0] - area.mins[0];
		float dy = area.maxs[1] - area.mins[1];

		return dx / dy > 4.0f || dy / dx > 4.0f;
	}
};

class StairsClusterBuilder: public AreasClusterBuilder<ClassifyStairsArea>
{
	int firstAreaIndex;
	int lastAreaIndex;
	vec2_t averageDimensions;

	inline bool ConformsToDimensions( const aas_area_t &area, float conformanceRatio ) {
		for( int j = 0; j < 2; ++j ) {
			float dimension = area.maxs[j] - area.mins[j];
			float avg = averageDimensions[j];
			if( dimension < ( 1.0f / conformanceRatio ) * avg || dimension > conformanceRatio * avg ) {
				return false;
			}
		}
		return true;
	}
public:
	wsw::StaticVector<AreaAndScore, 128> areasAndHeights;

	StairsClusterBuilder( BitVector *areasMask_, uint16_t *resultsBuffer, AiAasWorld *aasWorld_ )
		: AreasClusterBuilder( areasMask_, resultsBuffer, aasWorld_ ), firstAreaIndex(0), lastAreaIndex(0) {}

	bool Build( int startAreaNum );

	const AreaAndScore *begin() const { return &areasAndHeights.front() + firstAreaIndex; }
	const AreaAndScore *end() const { return &areasAndHeights.front() + lastAreaIndex + 1; }
};

bool StairsClusterBuilder::Build( int startAreaNum ) {
	// We do not check intentionally whether the start area belongs to stairs itself and is not just adjacent to stairs.
	// (whether the area top projection dimensions ratio looks like a stair step)
	// (A bot might get blocked on such stairs entrance/exit areas)

	PrepareToFlood();

	const auto aasAreas = aasWorld->getAreas();
	const auto aasAreaSettings = aasWorld->getAreaSettings();

	if( !classifyFunc.LooksLikeAStairsArea( aasAreas[startAreaNum], aasAreaSettings[startAreaNum ] ) ) {
		return false;
	}

	FloodAreasRecursive( startAreaNum );

	const int numAreas = ResultSize();
	if( numAreas < 3 ) {
		return false;
	}

	if( numAreas > 128 ) {
		G_Printf( S_COLOR_YELLOW "Warning: StairsClusterBuilder::Build(): too many stairs-like areas in cluster\n" );
		return false;
	}

	const auto *areaNums = ResultAreas();
	areasAndHeights.clear();
	Vector2Set( averageDimensions, 0, 0 );
	for( int i = 0; i < numAreas; ++i) {
		const int areaNum = areaNums[i];
		// Negate the "score" so lowest areas (having the highest "score") are first after sorting
		const auto &area = aasAreas[areaNum];
		new( areasAndHeights.unsafe_grow_back() )AreaAndScore( areaNum, -area.mins[2] );
		for( int j = 0; j < 2; ++j ) {
			averageDimensions[j] += area.maxs[j] - area.mins[j];
		}
	}

	for( int j = 0; j < 2; ++j ) {
		averageDimensions[j] *= 1.0f / areasAndHeights.size();
	}

	wsw::sortByFieldDescending( areasAndHeights.begin(), areasAndHeights.end(), &AreaAndScore::score );

	// Chop first/last areas if they do not conform to average dimensions
	// This prevents inclusion of huge entrance/exit areas to the cluster
	// Ideally some size filter should be applied to cluster areas too,
	// but it has shown to produce bad results rejecting many feasible clusters.

	this->firstAreaIndex = 0;
	if( !ConformsToDimensions( aasAreas[areasAndHeights[this->firstAreaIndex].areaNum], 1.25f ) ) {
		this->firstAreaIndex++;
	}

	this->lastAreaIndex = areasAndHeights.size() - 1;
	if( !ConformsToDimensions( aasAreas[areasAndHeights[this->lastAreaIndex].areaNum], 1.25f ) ) {
		this->lastAreaIndex--;
	}

	if( end() - begin() < 3 ) {
		return false;
	}

	// Check monotone height increase ("score" decrease)
	float prevScore = areasAndHeights[firstAreaIndex].score;
	for( int i = firstAreaIndex + 1; i < lastAreaIndex; ++i ) {
		float currScore = areasAndHeights[i].score;
		if( fabsf( currScore - prevScore ) <= 1.0f ) {
			return false;
		}
		assert( currScore < prevScore );
		prevScore = currScore;
	}

	// Now add protection against Greek/Cyrillic Gamma-like stairs
	// that include an intermediate platform (like wbomb1 water stairs)
	// Check whether an addition of an area does not lead to unexpected 2D area growth.
	// (this kind of stairs should be split in two or more clusters)
	// This test should split curved stairs like on wdm4 as well.

	vec3_t boundsMins, boundsMaxs;
	ClearBounds( boundsMins, boundsMaxs );
	AddPointToBounds( aasAreas[areasAndHeights[firstAreaIndex].areaNum].mins, boundsMins, boundsMaxs );
	AddPointToBounds( aasAreas[areasAndHeights[firstAreaIndex].areaNum].maxs, boundsMins, boundsMaxs );

	float oldTotal2DArea = ( boundsMaxs[0] - boundsMins[0] ) * ( boundsMaxs[1] - boundsMins[1] );
	const float areaStepGrowthThreshold = 1.25f * averageDimensions[0] * averageDimensions[1];
	for( int areaIndex = firstAreaIndex + 1; areaIndex < lastAreaIndex; ++areaIndex ) {
		const auto &currAasArea = aasAreas[areasAndHeights[areaIndex].areaNum];
		AddPointToBounds( currAasArea.mins, boundsMins, boundsMaxs );
		AddPointToBounds( currAasArea.maxs, boundsMins, boundsMaxs );
		const float newTotal2DArea = ( boundsMaxs[0] - boundsMins[0] ) * ( boundsMaxs[1] - boundsMins[1] );
		// If there was a significant total 2D area growth
		if( newTotal2DArea - oldTotal2DArea > areaStepGrowthThreshold ) {
			lastAreaIndex = areaIndex - 1;
			break;
		}
		oldTotal2DArea = newTotal2DArea;
	}

	if( end() - begin() < 3 ) {
		return false;
	}

	// Check connectivity between adjacent stair steps, it should not be broken after sorting for real stairs

	// Let us assume we have a not stair-like environment of this kind as an algorithm input:
	//   I
	//   I I
	// I I I
	// 1 2 3

	// After sorting it looks like this:
	//     I
	//   I I
	// I I I
	// 1 3 2

	// The connectivity between steps 1<->2, 2<->3 is broken
	// (there are no mutual walk reachabilities connecting some of steps of these false stairs)

	const auto aasReaches = aasWorld->getReaches();
	for( int i = firstAreaIndex; i < lastAreaIndex - 1; ++i ) {
		const int prevAreaNum = areasAndHeights[i + 0].areaNum;
		const int currAreaNum = areasAndHeights[i + 1].areaNum;
		const auto &currAreaSettings = aasAreaSettings[currAreaNum];
		int currReachNum = currAreaSettings.firstreachablearea;
		const int maxReachNum = currReachNum + currAreaSettings.numreachableareas;
		for(; currReachNum < maxReachNum; ++currReachNum ) {
			const auto &reach = aasReaches[currReachNum];
			// We have dropped condition on travel type of the reachability as showing unsatisfiable results
			if( reach.areanum == prevAreaNum ) {
				break;
			}
		}

		if( currReachNum == maxReachNum ) {
			return false;
		}
	}

	return true;
}

void AiAasWorld::computeLogicalAreaClusters() {
	auto floodResultsBuffer = (uint16_t *)Q_malloc( sizeof( uint16_t ) * m_numareas );

	FloorClusterBuilder floorClusterBuilder( AasElementsMask::AreasMask(), floodResultsBuffer, this );

	m_areaFloorClusterNums = (uint16_t *)Q_malloc( sizeof( uint16_t ) * m_numareas );
	memset( m_areaFloorClusterNums, 0, sizeof( uint16_t ) * m_numareas );

	BufferBuilder<uint16_t> floorData( 256 );
	BufferBuilder<int> floorDataOffsets( 32 );
	BufferBuilder<uint16_t> stairsData( 128 );
	BufferBuilder<int> stairsDataOffsets( 16 );

	// Add dummy clusters at index 0 in order to conform to the rest of AAS code
	m_numFloorClusters = 1;
	floorDataOffsets.Add( 0 );
	floorData.Add( 0 );

	for( int i = 1; i < m_numareas; ++i ) {
		// If an area is already marked
		if( m_areaFloorClusterNums[i] ) {
			continue;
		}

		if( !floorClusterBuilder.Build( i ) ) {
			continue;
		}

		// Important: Mark all areas in the built cluster
		int numClusterAreas = floorClusterBuilder.ResultSize();
		const auto *clusterAreaNums = floorClusterBuilder.ResultAreas();
		for( int j = 0; j < numClusterAreas; ++j ) {
			m_areaFloorClusterNums[clusterAreaNums[j]] = (uint16_t)m_numFloorClusters;
		}

		m_numFloorClusters++;
		floorDataOffsets.Add( floorData.Size() );
		floorData.Add( (uint16_t)numClusterAreas );
		floorData.Add( clusterAreaNums, numClusterAreas );
	}

	assert( m_numFloorClusters == (int)floorDataOffsets.Size() );
	m_floorClusterDataOffsets = floorDataOffsets.FlattenResult();
	// Clear as no longer needed immediately for same reasons
	floorDataOffsets.Clear();
	m_floorClusterData = floorData.FlattenResult();
	floorData.Clear();

	StairsClusterBuilder stairsClusterBuilder( AasElementsMask::AreasMask(), floodResultsBuffer, this );

	m_areaStairsClusterNums = (uint16_t *)Q_malloc( sizeof( uint16_t ) * m_numareas );
	memset( m_areaStairsClusterNums, 0, sizeof( uint16_t ) * m_numareas );

	m_numStairsClusters = 1;
	stairsDataOffsets.Add( 0 );
	stairsData.Add( 0 );

	for( int i = 0; i < m_numareas; ++i ) {
		// If an area is already marked
		if( m_areaFloorClusterNums[i] || m_areaStairsClusterNums[i] ) {
			continue;
		}

		if( !stairsClusterBuilder.Build( i ) ) {
			continue;
		}

		// Important: Mark all areas in the built cluster
		for( auto iter = stairsClusterBuilder.begin(), end = stairsClusterBuilder.end(); iter != end; ++iter ) {
			m_areaStairsClusterNums[iter->areaNum] = (uint16_t)m_numStairsClusters;
		}

		m_numStairsClusters++;
		// Add the current stairs data size to the offsets array
		stairsDataOffsets.Add( stairsData.Size() );
		// Add the actual stairs data length for the current cluster
		stairsData.Add( (uint16_t)( stairsClusterBuilder.end() - stairsClusterBuilder.begin() ) );
		// Save areas preserving sorting by height
		for( auto iter = stairsClusterBuilder.begin(), end = stairsClusterBuilder.end(); iter != end; ++iter ) {
			stairsData.Add( (uint16_t)( iter->areaNum ) );
		}
	}

	// Clear as no longer needed to provide free space for further allocations
	Q_free( floodResultsBuffer );

	assert( m_numStairsClusters == (int)stairsDataOffsets.Size() );
	m_stairsClusterDataOffsets = stairsDataOffsets.FlattenResult();
	stairsDataOffsets.Clear();
	m_stairsClusterData = stairsData.FlattenResult();

	constexpr auto *format =
		"AiAasWorld: %d floor clusters, %d stairs clusters "
		"(including dummy zero ones) have been detected\n";
	G_Printf( format, m_numFloorClusters, m_numStairsClusters );
}

void AiAasWorld::computeFace2DProjVertices() {
	m_face2DProjVertexNums = (int *)Q_malloc( sizeof( int ) * 2 * m_numfaces );
	int *vertexNumsPtr = m_face2DProjVertexNums;

	// Skip 2 vertices for the dummy zero face
	vertexNumsPtr += 2;

	const auto *faces = this->m_faces;
	const auto *edgeIndex = this->m_edgeindex;
	const auto *edges = this->m_edges;
	const auto *vertices = this->m_vertexes;

	for( int i = 1; i < m_numfaces; ++i ) {
		const auto &face = faces[i];
		int edgeIndexNum = face.firstedge;
		const int endEdgeIndexNum = edgeIndexNum + face.numedges;
		// Put dummy values by default. Make sure they're distinct.
		int n1 = 0, n2 = 1;
		for(; edgeIndexNum != endEdgeIndexNum; ++edgeIndexNum ) {
			const auto &edge = edges[abs( edgeIndex[edgeIndexNum] )];
			int ev1 = edge.v[0];
			int ev2 = edge.v[1];
			Vec3 dir( vertices[ev1] );
			dir -= vertices[ev2];
			if( !dir.normalizeFast() ) {
				continue;
			}
			if( fabsf( dir.Z() ) > 0.001f ) {
				continue;
			}
			n1 = ev1;
			n2 = ev2;
			break;
		}
		*vertexNumsPtr++ = n1;
		*vertexNumsPtr++ = n2;
	}
}

void AiAasWorld::computeAreasLeafsLists() {
	BufferBuilder<int> leafListsData( 512 );
	BufferBuilder<int> listOffsets( 128 );

	// Add a dummy list for the dummy zero area
	leafListsData.Add( 0 );
	listOffsets.Add( 0 );

	int tmpNums[256 + 1], topNode;
	for( int i = 1, end = m_numareas; i < end; ++i ) {
		const auto &area = m_areas[i];
		// Supply tmpLeafNums + 1 as a buffer so we can prepend the numeber of leaves in-place
		int numLeaves = SV_BoxLeafnums( area.mins, area.maxs, tmpNums + 1, 256, &topNode );
		// Not sure whether the call above can return a value greater than a supplied buffer capacity
		numLeaves = wsw::min( 256, numLeaves );
		// Put the number of leaves to the list head
		tmpNums[0] = numLeaves;
		// The offset of the newly added data is the current builder size
		listOffsets.Add( leafListsData.Size() );
		// Add leaves and the number of leaves in the head
		leafListsData.Add( tmpNums, numLeaves + 1 );
	}

	m_areaMapLeafListOffsets = listOffsets.FlattenResult();
	// Clear early to free some allocation space for flattening of the next result
	listOffsets.Clear();
	m_areaMapLeafsData = leafListsData.FlattenResult();
}

template <typename AcceptAreaFunc>
class ReachPassThroughAreasListBuilder {
	AiAasWorld *aasWorld;
	BitVector *areasMask;

	void AddTracedAreas( const aas_reachability_t &reach, BufferBuilder<uint16_t> &builder );
public:
	ReachPassThroughAreasListBuilder( AiAasWorld *aasWorld_, BitVector *areasMask_ )
		: aasWorld( aasWorld_ ), areasMask( areasMask_ ) {}

	uint16_t *Exec( int travelType );
};

template <typename AcceptAreaFunc>
uint16_t *ReachPassThroughAreasListBuilder<AcceptAreaFunc>::Exec( int travelType ) {
	BufferBuilder<uint16_t> listBuilder( 128 );

	areasMask->Clear();

	// Reserve a space for an actual list size
	listBuilder.Add( 0 );

	const auto aasReaches = aasWorld->getReaches();
	for( int i = 1, end = (int)aasReaches.size(); i < end; ++i ) {
		const auto &reach = aasReaches[i];
		if( ( reach.traveltype & TRAVELTYPE_MASK ) != travelType ) {
			continue;
		}

		AddTracedAreas( reach, listBuilder );
	}

	uint16_t *result = listBuilder.FlattenResult();
	// There was a placeholder for the size added, so the actual list size is lesser by one
	result[0] = (uint16_t)( listBuilder.Size() - 1 );
	return result;
}

template <typename AcceptAreaFunc>
void ReachPassThroughAreasListBuilder<AcceptAreaFunc>::AddTracedAreas( const aas_reachability_t &reach,
																	   BufferBuilder<uint16_t> &listBuilder ) {
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	AcceptAreaFunc acceptAreaFunc;

	int tmpAreaNums[64];
	const auto tracedAreas = aasWorld->traceAreas( reach.start, reach.end, tmpAreaNums, 64 );
	for( const int areaNum: tracedAreas ) {
		// Skip if already set
		if( !areasMask->TrySet( areaNum ) ) {
			continue;
		}
		if( !acceptAreaFunc( aasAreaSettings[areaNum] ) ) {
			continue;
		}
		listBuilder.Add( (uint16_t)areaNum );
	}
}

struct AcceptAnyArea {
	bool operator()( const aas_areasettings_t & ) const { return true; }
};

struct AcceptInAirArea {
	bool operator()( const aas_areasettings_t &areaSettings ) const {
		return !( areaSettings.areaflags & AREA_GROUNDED );
	}
};

void AiAasWorld::buildSpecificAreaTypesLists() {
	BufferBuilder<uint16_t> groundedAreasBuilder( 1024 );

	// Add a placeholder for actual size
	groundedAreasBuilder.Add( 0 );
	for( int i = 1, end = m_numareas; i < end; ++i ) {
		const int areaFlags = m_areasettings[i].areaflags;
		if( !( areaFlags & AREA_GROUNDED ) ) {
			continue;
		}
		if( ( areaFlags & AREA_JUNK ) ) {
			continue;
		}
		// Skip areas that are not in floor clusters
		if( !m_areaFloorClusterNums[i] ) {
			continue;
		}
		groundedAreasBuilder.Add( (uint16_t)i );
	}

	m_groundedPrincipalRoutingAreas = groundedAreasBuilder.FlattenResult();
	// There was a placeholder for the size added, so the actual list size is lesser by one
	m_groundedPrincipalRoutingAreas[0] = (uint16_t)( groundedAreasBuilder.Size() - 1 );
	groundedAreasBuilder.Clear();

	ReachPassThroughAreasListBuilder<AcceptAnyArea> acceptAnyAreaBuilder( this, AasElementsMask::AreasMask() );

	// We can collect all these areas in a single pass.
	// However the code would be less clean and would require
	// allocation of multiple flood buffers for different reach types.
	// (An area is allowed to be present in multiple lists at the same time).
	// We plan using a shared flood buffer across the entire AI codebase.
	// This does not get called during a match time anyway.
	m_jumppadReachPassThroughAreas = acceptAnyAreaBuilder.Exec( TRAVEL_JUMPPAD );
	m_ladderReachPassThroughAreas = acceptAnyAreaBuilder.Exec( TRAVEL_LADDER );
	m_elevatorReachPassThroughAreas = acceptAnyAreaBuilder.Exec( TRAVEL_ELEVATOR );

	ReachPassThroughAreasListBuilder<AcceptInAirArea> acceptInAirAreaBuilder( this, AasElementsMask::AreasMask() );
	m_walkOffLedgePassThroughAirAreas = acceptInAirAreaBuilder.Exec( TRAVEL_WALKOFFLEDGE );
}

template<typename T1, typename T2>
static inline float PerpDot2D( const T1 &v1, const T2 &v2 ) {
	return v1[0] * v2[1] - v1[1] * v2[0];
}

bool AiAasWorld::isAreaWalkableInFloorCluster( int startAreaNum, int targetAreaNum ) const {
	// Lets keep this old behaviour.
	// Consider it walkable even if the area is not necessarily belongs to some floor cluster.
	// In this case an area itself is a "micro-cluster".
	if( startAreaNum == targetAreaNum ) {
		return true;
	}

	// Make hints for a compiler
	const auto *const __restrict floorClusterNums = m_areaFloorClusterNums;

	int startFloorClusterNum = floorClusterNums[startAreaNum];
	if( !startFloorClusterNum ) {
		return false;
	}

	if( startFloorClusterNum != floorClusterNums[targetAreaNum] ) {
		return false;
	}

	const auto *const __restrict aasAreas = m_areas;
	const auto *const __restrict aasFaceIndex = m_faceindex;
	const auto *const __restrict aasFaces = m_faces;
	const auto *const __restrict aasPlanes = m_planes;
	const auto *const __restrict aasVertices = m_vertexes;
	const auto *const __restrict face2DProjVertexNums = m_face2DProjVertexNums;

	const vec3_t testedSegEnd { aasAreas[targetAreaNum].center[0], aasAreas[targetAreaNum].center[1], 0.0f };
	vec3_t testedSegStart { aasAreas[startAreaNum].center[0], aasAreas[startAreaNum].center[1], 0.0f };

	Vec3 rayDir( testedSegEnd );
	rayDir -= testedSegStart;
	rayDir.normalizeFastOrThrow();

	int currAreaNum = startAreaNum;
	while( currAreaNum != targetAreaNum ) {
		const auto &currArea = aasAreas[currAreaNum];
		// For each area face
		int faceIndexNum = currArea.firstface;
		const int endFaceIndexNum = faceIndexNum + currArea.numfaces;
		for(; faceIndexNum != endFaceIndexNum; ++faceIndexNum) {
			int signedFaceNum = aasFaceIndex[faceIndexNum];
			const auto &face = aasFaces[abs( signedFaceNum )];
			const auto &plane = aasPlanes[face.planenum];
			// Reject non-2D faces
			if( std::fabs( plane.normal[2] ) > 0.1f ) {
				continue;
			}
			// We assume we're inside the area.
			// Do not try intersection tests for already "passed" by the ray faces
			int areaBehindFace;
			if( signedFaceNum < 0 ) {
				if( rayDir.Dot( plane.normal ) < 0 ) {
					continue;
				}
				areaBehindFace = face.frontarea;
			} else {
				if( rayDir.Dot( plane.normal ) > 0 ) {
					continue;
				}
				areaBehindFace = face.backarea;
			}

			// If an area behind the face is in another or zero floor cluster
			if( floorClusterNums[areaBehindFace] != startFloorClusterNum ) {
				continue;
			}

			const auto *const projVertexNums = face2DProjVertexNums + 2 * std::abs( signedFaceNum );
			const float *const edgePoint1 = aasVertices[projVertexNums[0]];
			const float *const edgePoint2 = aasVertices[projVertexNums[1]];

			// Here goes the inlined body of FindSegments2DIntersectionPoint().
			// We want this 2D raycast method to be very fast and cheap to call since it is/is going to be widely used.
			// Inlining provides control-flow optimization opportunities for a compiler.

			// Copyright 2001 softSurfer, 2012 Dan Sunday
			// This code may be freely used and modified for any purpose
			// providing that this copyright notice is included with it.
			// SoftSurfer makes no warranty for this code, and cannot be held
			// liable for any real or imagined damage resulting from its use.
			// Users of this code must verify correctness for their application.

			// Compute first segment direction vector
			const vec3_t u = { testedSegEnd[0] - testedSegStart[0], testedSegEnd[1] - testedSegStart[1], 0 };
			// Compute second segment direction vector
			const vec3_t v = { edgePoint2[0] - edgePoint1[0], edgePoint2[1] - edgePoint1[1], 0 };
			// Compute a vector from second start point to the first one
			const vec3_t w = { testedSegStart[0] - edgePoint1[0], testedSegStart[1] - edgePoint1[1], 0 };

			// |u| * |v| * sin( u ^ v ), if parallel than zero, if some of inputs has zero-length than zero
			const float dot = PerpDot2D( u, v );

			// We treat parallel or degenerate cases as a failure
			if( std::fabs( dot ) < 0.0001f ) {
				continue;
			}

			const float invDot = 1.0f / dot;
			const float t1 = PerpDot2D( v, w ) * invDot;
			const float t2 = PerpDot2D( u, w ) * invDot;

			// If the first segment direction vector is "behind" or "ahead" of testedSegStart-to-edgePoint1 vector
			// if( t1 < 0 || t1 > 1 )
			// If the second segment direction vector is "behind" or "ahead" of testedSegStart-to-edgePoint1 vector
			// if( t2 < 0 || t2 > 1 )

			// These conditions are optimized for a happy path
			// Force computations first, then use a single branch
			const bool outside = ( t1 < 0 ) | ( t1 > 1 ) | ( t2 < 0 ) | ( t2 > 1 );
			if( outside ) {
				continue;
			}

			VectorMA( testedSegStart, t1, u, testedSegStart );
			currAreaNum = areaBehindFace;
			goto nextArea;
		}

		// There are no feasible areas behind feasible faces of the current area
		return false;
nextArea:;
	}

	return true;
}

static const wsw::StringView kMapsPrefix( "maps/" );
static const wsw::StringView kAiPrefix( "ai/" );
static const wsw::StringView kAasExtension( ".aas" );

auto AiAasWorld::getBaseMapName( const wsw::StringView &fullName ) -> wsw::StringView {
	wsw::StringView result( fullName );
	result = result.trimLeft( '/' );
	// TODO: Shouldn't we use common FS facilities?
	if( result.startsWith( kMapsPrefix ) ) {
		result = result.drop( kMapsPrefix.size() );
	}
	if( result.endsWith( kAasExtension ) ) {
		result = result.dropRight( kAasExtension.size() );
	}
	return result;
}

void AiAasWorld::makeFileName( wsw::StaticString<MAX_QPATH> &buffer,
							   const wsw::StringView &strippedName,
							   const wsw::StringView &extension ) {
	assert( extension.startsWith( '.' ) );
	buffer.clear();
	buffer << kAiPrefix << strippedName << extension;
}

static constexpr uint32_t FLOOR_CLUSTERS_VIS_VERSION = 1337;
static const char *FLOOR_CLUSTERS_VIS_TAG = "FloorClustersVis";
static const char *FLOOR_CLUSTERS_VIS_EXT = ".floorvis";

void AiAasWorld::loadFloorClustersVisibility( const wsw::StringView &baseMapName ) {
	if( !m_numFloorClusters ) {
		return;
	}

	AiPrecomputedFileReader reader( va( "%sReader", FLOOR_CLUSTERS_VIS_TAG ), FLOOR_CLUSTERS_VIS_VERSION );
	wsw::StaticString<MAX_QPATH> filePath;
	makeFileName( filePath, baseMapName, wsw::StringView( FLOOR_CLUSTERS_VIS_EXT ) );

	const auto expectedSize = (uint32_t)( ( m_numFloorClusters - 1 ) * ( m_numFloorClusters - 1 ) * sizeof( bool ) );
	if( reader.BeginReading( filePath.data() ) == AiPrecomputedFileReader::SUCCESS ) {
		uint8_t *data;
		uint32_t dataLength;
		if( reader.ReadLengthAndData( &data, &dataLength )  ) {
			if( dataLength == expectedSize ) {
				this->m_floorClustersVisTable = (bool *)data;
				return;
			}
			Q_free( data );
		}
	}

	G_Printf( "About to compute floor clusters mutual visibility...\n" );
	const uint32_t actualSize = computeFloorClustersVisibility();
	// Make sure we are not going to write junk bytes
	assert( m_floorClustersVisTable && expectedSize == actualSize );

	AiPrecomputedFileWriter writer( va( "%sWriter", FLOOR_CLUSTERS_VIS_TAG ), FLOOR_CLUSTERS_VIS_VERSION );
	if( !writer.BeginWriting( filePath.data() ) ) {
		return;
	}

	writer.WriteLengthAndData( (const uint8_t *)m_floorClustersVisTable, actualSize );
}

uint32_t AiAasWorld::computeFloorClustersVisibility() {
	// Must not be called for low number of clusters
	assert( m_numFloorClusters );

	const int stride = m_numFloorClusters - 1;
	// Do not allocate data for the dummy zero cluster
	const auto dataSizeInBytes = (uint32_t)( stride * stride * sizeof( bool ) );
	m_floorClustersVisTable = (bool *)Q_malloc( dataSizeInBytes );
	memset( m_floorClustersVisTable, 0, dataSizeInBytes );

	// Start loops from 0 even if we skip the zero cluster for table addressing convenience
	for( int i = 0; i < stride; ++i ) {
		m_floorClustersVisTable[i * stride + i] = true;
		for( int j = i + 1; j < stride; ++j ) {
			// We should shift indices to get actual cluster numbers
			// (we use index 0 for a 1-st valid cluster)
			bool visible = computeVisibilityForClustersPair( i + 1, j + 1 );
			m_floorClustersVisTable[i * stride + j] = visible;
			m_floorClustersVisTable[j * stride + i] = visible;
		}
	}

	return dataSizeInBytes;
}

bool AiAasWorld::computeVisibilityForClustersPair( int floorClusterNum1, int floorClusterNum2 ) const {
	assert( floorClusterNum1 != floorClusterNum2 );

	const std::span<const uint16_t> areaNums1 = floorClusterData( floorClusterNum1 );
	const std::span<const uint16_t> areaNums2 = floorClusterData( floorClusterNum2 );

	// The larger list should be iterated in the outer loop
	const std::span<const uint16_t> &outerAreaNums = ( areaNums1.size() > areaNums2.size() ) ? areaNums1 : areaNums2;
	const std::span<const uint16_t> &innerAreaNums = ( areaNums1.size() > areaNums2.size() ) ? areaNums2 : areaNums1;

	for( const uint16_t outerAreaNum : outerAreaNums ) {
		// Get compressed vis list for the area in the outer list
		const std::span<const uint16_t> outerVisList = areaVisList( outerAreaNum );
		// Use a sequential scan for short lists
		if( outerVisList.size() < 24 && innerAreaNums.size() < 24 ) {
			// For every inner area try finding an inner area num in the vis list
			for( const uint16_t innerAreaNum : innerAreaNums ) {
				if( wsw::contains( outerVisList, innerAreaNum ) ) {
					return true;
				}
			}
			continue;
		}

		const bool *__restrict visRow = decompressAreaVis( outerVisList, AasElementsMask::TmpAreasVisRow() );
		// For every area in inner areas check whether it's set in the row
		for( const uint16_t innerAreaNum : innerAreaNums ) {
			if( visRow[innerAreaNum] ) {
				return true;
			}
		}
	}

	return false;
}

bool AiAasWorld::areAreasInPvs( int areaNum1, int areaNum2 ) const {
	assert( areaNum1 >= 0 );
	assert( areaNum2 >= 0 );

	// Return false if some area is dummy
	if( ( areaNum2 * areaNum1 ) == 0 ) {
		return false;
	}

	// This not only cuts off computations but ensures "restrict" specifier correctness
	if( areaNum1 == areaNum2 ) {
		return true;
	}

	const auto *const data = m_areaMapLeafsData;
	const auto *const offsets = m_areaMapLeafListOffsets;
	const auto *const leafsList1 = data + offsets[areaNum1] + 1;
	const auto *const leafsList2 = data + offsets[areaNum2] + 1;
	for( int i = 0, iMax = leafsList1[-1]; i < iMax; ++i ) {
		for( int j = 0, jMax = leafsList2[-1]; j < jMax; ++j ) {
			if( SV_LeafsInPVS( leafsList1[i], leafsList2[j] ) ) {
				return true;
			}
		}
	}

	return false;
}

static constexpr uint32_t AREA_VIS_VERSION = 1338;
static constexpr const char *AREA_VIS_TAG = "AasAreaVis";
static constexpr const char *AREA_VIS_EXT = ".areavis";

void AiAasWorld::loadAreaVisibility( const wsw::StringView &baseMapName ) {
	if( !m_numFloorClusters ) {
		return;
	}

	AiPrecomputedFileReader reader( va( "%sReader", AREA_VIS_TAG ), AREA_VIS_VERSION );
	wsw::StaticString<MAX_QPATH> filePath;
	makeFileName( filePath, baseMapName, wsw::StringView( AREA_VIS_EXT ) );

	uint8_t *data;
	uint32_t dataLength;
	const uint32_t expectedOffsetsDataSize = sizeof( int32_t ) * m_numareas;
	if( reader.BeginReading( filePath.data() ) == AiPrecomputedFileReader::SUCCESS ) {
		if( reader.ReadLengthAndData( &data, &dataLength ) ) {
			// Sanity check. The number of offsets should match the number of areas
			if( expectedOffsetsDataSize == dataLength ) {
				m_areaVisDataOffsets = (int32_t *)data;
				const char *tag = "AiAasWorld::LoadVisibility()/AiPrecomputedFileReader::ReadLengthAndData()";
				constexpr const char *message = "G_Malloc() should return 16-byte aligned blocks";
				// Just to give vars above another usage so lifting it is required not only for fitting line limit.
				if( ( (uintptr_t)m_areaVisDataOffsets ) % 16 ) {
					AI_FailWith( tag, message );
				}
				if( reader.ReadLengthAndData( &data, &dataLength ) ) {
					m_areaVisData = (uint16_t *)data;
					// Having a proper alignment for area vis data is vital. Keep this assertion.
					if( ( (uintptr_t)m_areaVisDataOffsets ) % 16 ) {
						AI_FailWith( tag, message );
					}
					return;
				}
			}
		}
	}

	G_Printf( "About to compute AAS areas mutual visibility...\n" );

	uint32_t offsetsDataSize, listsDataSize;
	computeAreasVisibility( &offsetsDataSize, &listsDataSize );
	assert( expectedOffsetsDataSize == offsetsDataSize );

	AiPrecomputedFileWriter writer( va( "%sWriter", AREA_VIS_TAG ), AREA_VIS_VERSION );
	if( !writer.BeginWriting( filePath.data() ) ) {
		return;
	}

	if( writer.WriteLengthAndData( (const uint8_t *)m_areaVisDataOffsets, offsetsDataSize ) ) {
		writer.WriteLengthAndData( (const uint8_t *)m_areaVisData, listsDataSize );
	}
}

/**
 * Reduce the code complexity by extracting this helper.
 */
class SparseVisTable {
	bool *table;
	int32_t *listSizes;
	// We avoid wasting memory for zero row/columns
	static constexpr int elemOffset { -1 };
public:
	const int rowSize;

	static int AreaRowOffset( int area ) {
		return area + elemOffset;
	}

	static int AreaForOffset( int rowOffset ) {
		return rowOffset - elemOffset;
	}

	explicit SparseVisTable( int numAreas )
		: rowSize( numAreas + elemOffset ) {
		size_t tableMemSize = sizeof( bool ) * rowSize * rowSize;
		// Never returns on failure
		table = (bool *)Q_malloc( tableMemSize );
		memset( table, 0, tableMemSize );
		listSizes = (int32_t *)Q_malloc( numAreas * sizeof( int32_t ) );
		memset( listSizes, 0, numAreas * sizeof( int32_t ) );
	}

	~SparseVisTable() {
		Q_free( table );
		Q_free( listSizes );
	}

	void MarkAsVisible( int area1, int area2 ) {
		// The caller code is so expensive that we don't care about these scattered writes
		table[AreaRowOffset( area1 ) * rowSize + AreaRowOffset( area2 )] = true;
		table[AreaRowOffset( area2 ) * rowSize + AreaRowOffset( area1 )] = true;
		listSizes[area1]++;
		listSizes[area2]++;
	}

	uint32_t ComputeDataSize() const {
		// We need 15 elements for zero (dummy) area list and for padding of the first feasible list (described below)
		size_t result = 15;
		// We store lists in SIMD-friendly format from the very beginning.
		// List data should start from 16-byte boundaries.
		// Thus list length should be 2 bytes before 16-byte boundaries.
		// A gap between last element of a list and length of a next list must be zero-filled.
		// Assuming that lists data starts from 16-byte-aligned address
		// the gap size in elements is 8 - 1 - (list length in elements) % 8
		for( int i = -elemOffset; i < rowSize - elemOffset; ++i ) {
			size_t numListElems = (unsigned)listSizes[i];
			result += numListElems;
			size_t tail = 8 - 1 - ( numListElems % 8 );
			assert( tail < 8 );
			result += tail;
			// We need a space for the list size as well
			result += 1;
		}
		// Convert to size in bytes
		result *= sizeof( uint16_t );
		// Check whether we do not run out of sane storage limits (should not happen even for huge maps)
		assert( result < std::numeric_limits<int32_t>::max() / 8 );
		return (uint32_t)result;
	}

	const bool *Row( int areaNum ) const {
		assert( areaNum + elemOffset >= 0 );
		assert( areaNum + elemOffset < rowSize );
		return &table[rowSize * ( areaNum + elemOffset )];
	}

	int ListSize( int areaNum ) const {
		assert( areaNum >= 0 && areaNum < rowSize - elemOffset );
		return listSizes[areaNum];
	};
};

/**
 * Reduce the code complexity by extracting this helper.
 */
class ListsBuilder {
	const uint16_t *const listsData;
	uint16_t *__restrict listsPtr;
	int32_t *__restrict offsetsPtr;
public:
	ListsBuilder( uint16_t *listsData_, int32_t *offsetsData_ )
		: listsData( listsData_ ), listsPtr( listsData_ ), offsetsPtr( offsetsData_ ) {
		// The lists data must already have initial 16-byte alignment
		assert( !( ( (uintptr_t)listsData_ ) % 16 ) );
		// We should start writing lists data at 16-bytes boundary
		assert( !( ( (uintptr_t)listsData ) % 16 ) );
		// Let the list for the dummy area follow common list alignment contract.
		// (a short element after the a length should start from 16-byte boundaries).
		// The length of the first real list should start just before 16-byte boundaries as well.
		std::fill_n( listsPtr, 15, 0 );
		listsPtr += 15;
		*offsetsPtr++ = 7;
	}

	void BeginList( int size ) {
		ptrdiff_t offset = listsPtr - listsData;
		assert( offset > 0 && offset < std::numeric_limits<int32_t>::max() );
		assert( (unsigned)size <= std::numeric_limits<uint16_t>::max() );
		*offsetsPtr++ = (int32_t)offset;
		*listsPtr++ = (uint16_t)size;
	}

	void AddToList( int item ) {
		assert( (unsigned)( item - 1 ) < std::numeric_limits<uint16_t>::max() );
		*listsPtr++ = (uint16_t)item;
	}

	void EndList() {
		// Fill the gap between the list end and the next list length by zeroes
		for(;; ) {
			auto address = (uintptr_t)listsPtr;
			// Stop at the address 2 bytes before the 16-byte boundary
			if( !( ( address + 2 ) % 16 ) ) {
				break;
			}
			*listsPtr++ = 0;
		}
	}

	void MarkEnd() {
		// Even if the element that was reserved for a list length is unused
		// for the last list it still contributes to a checksum.
		//*listsPtr = 0;
	}

	ptrdiff_t Offset() const { return listsPtr - listsData; }
};

void AiAasWorld::computeAreasVisibility( uint32_t *offsetsDataSize, uint32_t *listsDataSize ) {
	const int numAreas = m_numareas;
	// This also ensures we can use 32-bit indices for total number of areas
	assert( numAreas && numAreas <= std::numeric_limits<uint16_t>::max() );
	SparseVisTable table( numAreas );

	int numberSoFar = 0;
	int lastReportedProgress = 0;
	// Assuming side = numAreas - 1 the number of elements in the upper part is (side - 1) * side / 2
	const double progressNormalizer = numAreas <= 1 ? 0 : 100.0 / ( ( numAreas - 2 ) * ( numAreas - 1 ) / 2.0 );

	const auto *const __restrict aasAreas = m_areas;
	for( int i = 1; i < numAreas; ++i ) {
		for( int j = i + 1; j < numAreas; ++j ) {
			numberSoFar++;
			if( !areAreasInPvs( i, j ) ) {
				continue;
			}

			int maybeProgress = (int)( numberSoFar * progressNormalizer );
			if( maybeProgress != lastReportedProgress ) {
				G_Printf( "AiAasWorld::ComputeAreasVisibility(): %d%%\n", maybeProgress );
				lastReportedProgress = maybeProgress;
			}

			trace_t trace;
			// TODO: Add and use an optimized version that uses an early exit
			SolidWorldTrace( &trace, aasAreas[i].center, aasAreas[j].center );
			if( trace.fraction != 1.0f ) {
				continue;
			}

			table.MarkAsVisible( i, j );
		}
	}

	*listsDataSize = table.ComputeDataSize();
	auto *const __restrict listsData = (uint16_t *)Q_malloc( *listsDataSize );

	// Let's keep these assertions in release mode for various reasons
	constexpr const char *tag = "AiAasWorld::ComputeAreasVisibility()";
	if( ( (uintptr_t)listsData ) % 8 ) {
		AI_FailWith( tag, "G_Malloc() violates ::malloc() contract (8-byte alignment)" );
	}
	if( ( (uintptr_t)listsData ) % 16 ) {
		AI_FailWith( tag, "G_Malloc() should return 16-byte aligned results" );
	}

	*offsetsDataSize = (uint32_t)( numAreas * sizeof( int32_t ) );
	auto *const listOffsets = (int *)Q_malloc( *offsetsDataSize );

	ListsBuilder builder( listsData, listOffsets );
	const auto rowSize = table.rowSize;
	for( int i = 1; i < numAreas; ++i ) {
		builder.BeginList( table.ListSize( i ) );

		// Scan the table row
		const bool *__restrict tableRow = table.Row( i );
		for( int j = 0; j < rowSize; ++j ) {
			if( tableRow[j] ) {
				builder.AddToList( SparseVisTable::AreaForOffset( j ) );
			}
		}

		builder.EndList();
	}

	builder.MarkEnd();

	// Make sure we've matched the expected data size
	assert( builder.Offset() * sizeof( int16_t ) == *listsDataSize );

#ifdef _DEBUG
	// Validate. Useful for testing whether we have brake something. Test a list of every area.

	// Make sure the list for the dummy area follows the alignment contract
	assert( listOffsets[0] == 7 );
	// The list for the dummy area should have zero size
	assert( !listsData[listOffsets[0]] );
	for( int i = 1; i < numAreas; ++i ) {
		const uint16_t *list = listsData + listOffsets[i];
		assert( list[0] == table.ListSize( i ) );
		const int size = *list++;
		// Check list data alignment once again
		assert( !( ( (uintptr_t)list ) % 16 ) );
		// For every area in list the a table bit must be set
		const auto *const __restrict row = table.Row( i );
		for( int j = 0; j < size; ++j ) {
			assert( row[SparseVisTable::AreaRowOffset( list[j] )] );
		}
		// For every area not in the list a table bit must be zero
		for( int areaNum = 1; areaNum < numAreas; ++areaNum ) {
			if( wsw::contains( list, list + size, areaNum ) ) {
				continue;
			}
			assert( !row[SparseVisTable::AreaRowOffset( areaNum )] );
		}
		// Make sure all areas in the list are valid
		assert( !wsw::contains( list, list + size, 0 ) );
	}
#endif

	m_areaVisData = listsData;
	m_areaVisDataOffsets = listOffsets;
}

bool *AiAasWorld::addToDecompressedAreaVis( std::span<const uint16_t> visList, bool *__restrict buffer ) const {
	const uint16_t *__restrict data = visList.data();
	const size_t size = visList.size();
	for( size_t i = 0; i < size; ++i ) {
		buffer[data[i]] = true;
	}
	return buffer;
}