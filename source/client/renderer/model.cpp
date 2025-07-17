/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2007 Victor Luchits

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

// r_model.c -- model loading and caching

#include "local.h"
#include "frontend.h"
#include "iqm.h"

#include "../../../third-party/recastnavigation/Recast/Include/Recast.h"
#include <common/facilities/gs_qrespath.h>
#include <common/facilities/fscompat.h>
#include <server/game/ai/vec3.h>

#include <algorithm>
#include <climits>
#include <numeric>
#include <span>
#include <unordered_map>

typedef struct {
	unsigned number;
	msurface_t *surf;
} msortedSurface_t;

void Mod_LoadAliasMD3Model( model_t *mod, model_t *parent, void *buffer, bspFormatDesc_t *unused );
void Mod_LoadSkeletalModel( model_t *mod, model_t *parent, void *buffer, bspFormatDesc_t *unused );
void Mod_LoadQ3BrushModel( model_t *mod, model_t *parent, void *buffer, bspFormatDesc_t *format );

void Mod_DestroyAliasMD3Model( maliasmodel_t *model );
void Mod_DestroySkeletalModel( mskmodel_t *model );
void Mod_DestroyQ3BrushModel( mbrushmodel_t *model );

static void R_InitMapConfig( const char *model );
static void R_FinishMapConfig( const model_t *mod );

static uint8_t mod_novis[MAX_MAP_LEAFS / 8];

#define MAX_MOD_KNOWN   512 * MOD_MAX_LODS
static model_t mod_known[MAX_MOD_KNOWN];
static int mod_numknown;
static bool mod_isworldmodel;
model_t *r_prevworldmodel;
static mapconfig_t *mod_mapConfigs;

static const modelFormatDescr_t mod_supportedformats[] =
{
	// Quake III Arena .md3 models
	{ IDMD3HEADER, 4, nullptr, MOD_MAX_LODS, ( const modelLoader_t )Mod_LoadAliasMD3Model },

	// Skeletal models
	{ IQM_MAGIC, sizeof( IQM_MAGIC ), nullptr, MOD_MAX_LODS, ( const modelLoader_t )Mod_LoadSkeletalModel },

	// Q3-alike .bsp models
	{ "*", 4, q3BSPFormats, 0, ( const modelLoader_t )Mod_LoadQ3BrushModel },

	// trailing nullptr
	{ nullptr, 0, nullptr, 0, nullptr }
};

mleaf_t *Mod_PointInLeaf( vec3_t p, model_t *model ) {
	if( !model ) {
		Com_Error( ERR_DROP, "Mod_PointInLeaf: bad model" );
	}

	auto *const bmodel = ( mbrushmodel_t * )model->extradata;
	if( !bmodel ) {
		Com_Error( ERR_DROP, "Mod_PointInLeaf: bad model" );
	}

	if( !bmodel->nodes ) {
		Com_Error( ERR_DROP, "Mod_PointInLeaf: bad model" );
	}

	const auto *__restrict nodes = bmodel->nodes;

	int nodeNum = 0;
	do {
		const auto *__restrict node = nodes + nodeNum;
		const auto *__restrict plane = &node->plane;
		nodeNum = node->children[PlaneDiff( p, plane ) < 0];
	} while( nodeNum >= 0 );

	return bmodel->leafs + ( -1 - nodeNum );
}

static inline uint8_t *Mod_ClusterVS( int cluster, dvis_t *vis ) {
	if( cluster < 0 || !vis ) {
		return mod_novis;
	}
	return ( (uint8_t *)vis->data + cluster * vis->rowsize );
}

uint8_t *Mod_ClusterPVS( int cluster, model_t *model ) {
	return Mod_ClusterVS( cluster, ( ( mbrushmodel_t * )model->extradata )->pvs );
}

static bool R_SurfPotentiallyVisible( const msurface_t *surf ) {
	const shader_t *shader = surf->shader;
	// Exclude old sky surfaces from rendering for now
	if( surf->flags & ( SURF_NODRAW | SURF_SKY ) ) {
		return false;
	}
	if( !surf->mesh.numVerts ) {
		return false;
	}
	if( !shader ) {
		return false;
	}
	return true;
}

bool R_SurfPotentiallyFragmented( const msurface_t *surf ) {
	if( surf->flags & ( SURF_NOMARKS | SURF_NOIMPACT | SURF_NODRAW ) ) {
		return false;
	}
	return ( ( surf->facetype == FACETYPE_PLANAR )
			 || ( surf->facetype == FACETYPE_PATCH )
		/* || (surf->facetype == FACETYPE_TRISURF)*/ );
}

class RecastPolyMeshBuilder {
public:
	RecastPolyMeshBuilder( wsw::PodVector<Vec3> *tmpVerticesBuffer, wsw::PodVector<int> *tmpIndicesBuffer,
						   wsw::PodVector<uint8_t> *tmpAreaFlagsBuffer )
		: m_verticesBuffer( tmpVerticesBuffer )
		, m_indicesBuffer( tmpIndicesBuffer )
		, m_areaFlagsBuffer( tmpAreaFlagsBuffer ) {
		m_context.enableTimer( false );
		m_context.enableLog( false );

		m_verticesBuffer->clear();
		m_indicesBuffer->clear();
		m_areaFlagsBuffer->clear();
	}

	~RecastPolyMeshBuilder() {
		rcFreeHeightField( m_heightfield );
		rcFreeCompactHeightfield( m_compactHeightfield );
		rcFreeContourSet( m_contourSet );
		rcFreePolyMesh( m_polyMesh );
	}

	void addVertex( const float *vertex ) {
		m_verticesBoundsBuilder.addPoint( vertex );
		m_verticesBuffer->emplace_back( Vec3( vertex ) );
		m_indicesBuffer->push_back( (int)m_indicesBuffer->size() );
	}

	[[nodiscard]]
	auto build() -> const rcPolyMesh * {
		assert( !m_indicesBuffer->empty() && !m_verticesBuffer->empty() );
		// Prepare height field

		constexpr float xzCellSize  = 8.0f;
		// Note: Droping it lower leads to production of bogus occluders
		constexpr float yCellSize   = 0.1f;
		constexpr float climbHeight = 0.0f;

		int gridWidth = 0, gridHeight = 0;
		const int numTris = m_indicesBuffer->size() / 3;

		vec3_t mins, maxs;
		m_verticesBoundsBuilder.storeTo( mins, maxs );
		rcCalcGridSize( mins, maxs, xzCellSize, &gridWidth, &gridHeight );

		m_heightfield = rcAllocHeightfield();
		if( !m_heightfield ) {
			return nullptr;
		}

		if( !rcCreateHeightfield( &m_context, *m_heightfield, gridWidth, gridHeight, mins, maxs, xzCellSize, yCellSize ) ) {
			return nullptr;
		}

		if( !m_heightfield->width || !m_heightfield->height ) {
			return nullptr;
		}

		m_areaFlagsBuffer->resize( numTris, 0 );

		uint8_t *const areaFlags = m_areaFlagsBuffer->data();
		const float *vertices    = (float *)m_verticesBuffer->data();
		const int *indices       = m_indicesBuffer->data();
		const int numVertices    = m_verticesBuffer->size();

		// Keep it not greater than the necessary minimum,
		// otherwise it can lead to mismatch between surf plane and actual poly plane
		// which in turn can intervene with occluders selection logic producing bogus frusta
		// TODO: Compute planes again for each invididual poly?
		constexpr float walkableSlopeAngle = 0.1f;
		rcMarkWalkableTriangles( &m_context, walkableSlopeAngle, vertices, numVertices, indices, numTris, areaFlags );

		if( !rcRasterizeTriangles( &m_context, vertices, numVertices, indices, areaFlags, numTris, *m_heightfield ) ) {
			return nullptr;
		}

		rcFilterLowHangingWalkableObstacles( &m_context, climbHeight, *m_heightfield );
		rcFilterLedgeSpans( &m_context, climbHeight, climbHeight, *m_heightfield );
		rcFilterWalkableLowHeightSpans( &m_context, climbHeight, *m_heightfield );

		m_compactHeightfield = rcAllocCompactHeightfield();
		if( !m_compactHeightfield ) {
			return nullptr;
		}

		if( !rcBuildCompactHeightfield( &m_context, climbHeight, climbHeight, *m_heightfield, *m_compactHeightfield ) ) {
			return nullptr;
		}

		if( !rcErodeWalkableArea( &m_context, 1, *m_compactHeightfield ) ) {
			return nullptr;
		}

		constexpr int borderSize      = 3;
		constexpr int minRegionArea   = 16 * 16;
		constexpr int mergeRegionArea = 32 * 32;

		if( !rcBuildDistanceField( &m_context, *m_compactHeightfield ) ) {
			return nullptr;
		}

		if( !rcBuildRegions( &m_context, *m_compactHeightfield, borderSize, minRegionArea, mergeRegionArea ) ) {
			return nullptr;
		}

		constexpr float maxError = 1.0f;
		constexpr int maxEdgeLen = 0;

		m_contourSet = rcAllocContourSet();
		if( !m_contourSet ) {
			return nullptr;
		}

		if( !rcBuildContours( &m_context, *m_compactHeightfield, maxError, maxEdgeLen, *m_contourSet ) ) {
			return nullptr;
		}

		m_polyMesh = rcAllocPolyMesh();
		if( !m_polyMesh ) {
			return nullptr;
		}

		if( !rcBuildPolyMesh( &m_context, *m_contourSet, 7, *m_polyMesh ) ) {
			return nullptr;
		}

		if( !m_polyMesh->npolys ) {
			return nullptr;
		}

		return m_polyMesh;
	}

private:
	rcContext m_context;
	BoundsBuilder m_verticesBoundsBuilder;
	wsw::PodVector<Vec3> *const m_verticesBuffer;
	wsw::PodVector<int> *const m_indicesBuffer;
	wsw::PodVector<uint8_t> *const m_areaFlagsBuffer;
	rcHeightfield *m_heightfield { nullptr };
	rcCompactHeightfield *m_compactHeightfield { nullptr };
	rcContourSet *m_contourSet { nullptr };
	rcPolyMesh *m_polyMesh { nullptr };
};

static wsw::PodVector<Vec3> g_tmpVerticesBuffer;
static wsw::PodVector<int> g_tmpIndicesBuffer;
static wsw::PodVector<uint8_t> g_tmpAreaFlagsBuffer;

static bool Mod_AddOccludersFromListOfSurfs( const wsw::PodVector<const msurface_t *> &surfs,
											 wsw::PodVector<OccluderBoundsEntry> *occluderBoundsEntries,
											 wsw::PodVector<OccluderDataEntry> *occluderDataEntries ) {
	BoundsBuilder boundsBuilder;
	for( const msurface_t *surf: surfs ) {
		boundsBuilder.addPoint( surf->mins );
		boundsBuilder.addPoint( surf->maxs );
	}

	vec3_t mins, maxs;
	boundsBuilder.storeTo( mins, maxs );
	if( DistanceSquared( mins, maxs ) < wsw::square( 192.0f ) ) {
		return false;
	}

	RecastPolyMeshBuilder meshBuilder( &g_tmpVerticesBuffer, &g_tmpIndicesBuffer, &g_tmpAreaFlagsBuffer );

	mat3_t toRecastXForm;
	const vec3_t kRecastGroundNormal { 0.0f, 1.0f, 0.0f };
	Matrix3_ForRotationOfDirs( kRecastGroundNormal, surfs.front()->plane, toRecastXForm );

	for( const msurface_t *surf: surfs ) {
		for( unsigned i = 0; i < surf->mesh.numElems; ++i ) {
			vec3_t v;
			const float *surfVertex = surf->mesh.xyzArray[surf->mesh.elems[i]];
			Matrix3_TransformVector( toRecastXForm, surfVertex, v );
			meshBuilder.addVertex( v );
		}
	}

	const rcPolyMesh *polyMesh = meshBuilder.build();
	if( !polyMesh ) {
		return false;
	}

	const size_t oldOccludersCount = occluderDataEntries->size();

	const int numVertsPerPoly = polyMesh->nvp;
	const float cellSide      = polyMesh->cs;
	const float cellHeight    = polyMesh->ch;
	const float *const orig   = polyMesh->bmin;

	mat3_t fromRecastXForm;
	Matrix3_Transpose( toRecastXForm, fromRecastXForm );

	for( int polyNum = 0; polyNum < polyMesh->npolys; ++polyNum ) {
		if( polyMesh->areas[polyNum] != RC_WALKABLE_AREA ) {
			continue;
		}

		int numVertices = 0;
		const unsigned short* p = &polyMesh->polys[polyNum * numVertsPerPoly * 2];
		for( ; numVertices < numVertsPerPoly; ++numVertices ) {
			if( p[numVertices] == RC_MESH_NULL_IDX ) {
				break;
			}
		}

		assert( numVertices >= 3 && numVertices <= 7 );

		// Triangular polys are often degenerate
		if( numVertices < 4 ) {
			continue;
		}

		occluderDataEntries->emplace_back( OccluderDataEntry {} );
		occluderBoundsEntries->emplace_back( OccluderBoundsEntry {} );

		OccluderDataEntry *const addedDataEntry     = std::addressof( occluderDataEntries->back() );
		OccluderBoundsEntry *const addedBoundsEntry = std::addressof( occluderBoundsEntries->back() );

		BoundsBuilder polyBoundsBuilder;
		VectorClear( addedDataEntry->innerPolyPoint );
		for( int vertexNum = 0; vertexNum < numVertices; ++vertexNum ) {
			const unsigned short *recastShortVertex = &polyMesh->verts[3 * p[vertexNum]];

			vec3_t recastFloatVertex;
			recastFloatVertex[0] = orig[0] + recastShortVertex[0] * cellSide;
			recastFloatVertex[1] = orig[1] + recastShortVertex[1] * cellHeight;
			recastFloatVertex[2] = orig[2] + recastShortVertex[2] * cellSide;

			float *const outVertex = addedDataEntry->data[vertexNum];
			Matrix3_TransformVector( fromRecastXForm, recastFloatVertex, outVertex );

			polyBoundsBuilder.addPoint( outVertex );

			VectorAdd( outVertex, addedDataEntry->innerPolyPoint, addedDataEntry->innerPolyPoint );
		}

		addedDataEntry->numVertices = (unsigned)numVertices;

		polyBoundsBuilder.storeTo( addedBoundsEntry->mins, addedBoundsEntry->maxs );
		addedBoundsEntry->mins[3] = 0.0f, addedBoundsEntry->maxs[3] = 1.0f;

		Vector4Copy( surfs.front()->plane, addedDataEntry->plane );

		VectorScale( addedDataEntry->innerPolyPoint, Q_Rcp( (float)numVertices ), addedDataEntry->innerPolyPoint );
	}

	assert( occluderDataEntries->size() == occluderBoundsEntries->size() );
	return occluderDataEntries->size() > oldOccludersCount;
}

struct PlaneKey {
	const float *plane;

	[[nodiscard]]
	bool operator==( const PlaneKey &that ) const {
		return VectorCompare( plane, that.plane ) && plane[3] == that.plane[3];
	}
};

namespace std {
	template <> struct hash<PlaneKey> {
		[[nodiscard]]
		auto operator()( const PlaneKey& key ) const -> size_t {
			const auto *dwords = (const uint32_t *)key.plane;
			return ( (size_t)dwords[0] * 17 + dwords[1] ) ^ ( (size_t)dwords[2] * 31 + dwords[3] );
		}
	};
}

static void Mod_BuildOccluders( model_t *model ) {
	std::unordered_map<PlaneKey, wsw::PodVector<const msurface_t *>> surfsBinnedByPlanes;

	mbrushmodel_t *const loadbmodel = ( ( mbrushmodel_t * )model->extradata );
	for( unsigned i = 0; i < loadbmodel->numModelSurfaces; i++ ) {
		const msurface_t *const surf = loadbmodel->surfaces + i;
		if( surf->facetype != FACETYPE_PLANAR ) {
			continue;
		}
		if( !( surf->shader && surf->shader->sort == SHADER_SORT_OPAQUE ) ) {
			continue;
		}
		const PlaneKey planeKey { .plane = surf->plane };
		auto it = surfsBinnedByPlanes.find( planeKey );
		if( it != surfsBinnedByPlanes.end() ) {
			it->second.push_back( surf );
		} else {
			surfsBinnedByPlanes.insert( std::make_pair( planeKey, wsw::PodVector<const msurface_t *> { surf } ) );
		}
	}

	wsw::PodVector<OccluderBoundsEntry> occluderBoundsEntries;
	wsw::PodVector<OccluderDataEntry> occluderDataEntries;

	unsigned numSuitableSurfGroups = 0;
	for( const auto &[_, listOfSurfs]: surfsBinnedByPlanes ) {
		if( Mod_AddOccludersFromListOfSurfs( listOfSurfs, &occluderBoundsEntries, &occluderDataEntries ) ) {
			numSuitableSurfGroups++;
		}
	}

	assert( occluderBoundsEntries.size() == occluderDataEntries.size() );
	const unsigned numOccluders = occluderDataEntries.size();

	loadbmodel->numOccluders          = numOccluders;
	loadbmodel->occluderBoundsEntries = (OccluderBoundsEntry *)Q_malloc( sizeof( OccluderBoundsEntry ) * numOccluders );
	loadbmodel->occluderDataEntries   = (OccluderDataEntry *)Q_malloc( sizeof( OccluderDataEntry ) * numOccluders );

	for( unsigned i = 0; i < numOccluders; ++i ) {
		loadbmodel->occluderBoundsEntries[i] = occluderBoundsEntries[i];
		loadbmodel->occluderDataEntries[i]   = occluderDataEntries[i];
	}
}

static void Mod_CreateVisLeafs( model_t *mod ) {
	mbrushmodel_t *const loadbmodel = ( ( mbrushmodel_t * )mod->extradata );

	const unsigned count = loadbmodel->numleafs;
	loadbmodel->visleafs = (mleaf_t **)Q_malloc( ( count + 1 ) * sizeof( *loadbmodel->visleafs ) );
	memset( loadbmodel->visleafs, 0, ( count + 1 ) * sizeof( *loadbmodel->visleafs ) );

	constexpr float minsVal = std::numeric_limits<float>::max();
	constexpr float maxsVal = std::numeric_limits<float>::lowest();

	unsigned numVisLeafs = 0;
	for( unsigned i = 0; i < count; i++ ) {
		mleaf_t *const __restrict leaf = loadbmodel->leafs + i;
		Vector4Set( leaf->mins + 0, minsVal, minsVal, minsVal, minsVal );
		Vector4Set( leaf->mins + 4, minsVal, minsVal, minsVal, minsVal );
		Vector4Set( leaf->maxs + 0, maxsVal, maxsVal, maxsVal, maxsVal );
		Vector4Set( leaf->maxs + 4, maxsVal, maxsVal, maxsVal, maxsVal );

		if( leaf->cluster < 0 || !leaf->numVisSurfaces ) {
			leaf->visSurfaces = nullptr;
			leaf->numVisSurfaces = 0;
			leaf->fragmentSurfaces = nullptr;
			leaf->numFragmentSurfaces = 0;
			leaf->fragmentSurfaces = nullptr;
		} else {
			BoundingDopBuilder<14> boundingDopBuilder;
			unsigned numVisSurfaces = 0;
			unsigned numFragmentSurfaces = 0;
			for( unsigned j = 0; j < leaf->numVisSurfaces; j++ ) {
				const unsigned surfNum = leaf->visSurfaces[j];
				msurface_t *const surf = loadbmodel->surfaces + surfNum;
				if( R_SurfPotentiallyVisible( surf ) ) {
					boundingDopBuilder.addOtherDop( surf->mins, surf->maxs );
					leaf->visSurfaces[numVisSurfaces++] = surfNum;
					if( R_SurfPotentiallyFragmented( surf ) ) {
						leaf->fragmentSurfaces[numFragmentSurfaces++] = surfNum;
					}
				}
			}
			leaf->numVisSurfaces      = numVisSurfaces;
			leaf->numFragmentSurfaces = numFragmentSurfaces;
			if( numVisSurfaces ) {
				boundingDopBuilder.storeTo( leaf->mins, leaf->maxs );
				loadbmodel->visleafs[numVisLeafs++] = leaf;
			}
		}
	}

	loadbmodel->visleafs[numVisLeafs] = nullptr;
	loadbmodel->numvisleafs = numVisLeafs;
}

/*
* Mod_CalculateAutospriteBounds
*
* Make bounding box of an autosprite surf symmetric and enlarges it
* to account for rotation along the longest axis.
*/
static void Mod_CalculateAutospriteBounds( msurface_t *surf, vec3_t mins, vec3_t maxs ) {
	// find the longest axis
	int longestAxis = 2;
	vec3_t radius, center;
	float maxDist = std::numeric_limits<float>::lowest();
	for( int i = 0; i < 3; i++ ) {
		const float dist = maxs[i] - mins[i];
		if( dist > maxDist ) {
			longestAxis = i;
			maxDist = dist;
		}

		// make the bbox symmetrical
		radius[i] = 0.5f * dist;
		center[i] = 0.5f * ( maxs[i] + mins[i] );
		mins[i] = center[i] - radius[i];
		maxs[i] = center[i] + radius[i];
	}

	// shorter axis
	const int shorterAxis1 = ( longestAxis + 1 ) % 3;
	const int shorterAxis2 = ( longestAxis + 2 ) % 3;

	// enlarge the bounding box, accounting for rotation along the longest axis
	maxs[shorterAxis1] = wsw::max( maxs[shorterAxis1], center[shorterAxis1] + radius[shorterAxis2] );
	maxs[shorterAxis2] = wsw::max( maxs[shorterAxis2], center[shorterAxis2] + radius[shorterAxis1] );

	mins[shorterAxis1] = wsw::min( mins[shorterAxis1], center[shorterAxis1] - radius[shorterAxis2] );
	mins[shorterAxis2] = wsw::min( mins[shorterAxis2], center[shorterAxis2] - radius[shorterAxis1] );
}

static void Mod_FinishFaces( model_t *mod ) {
	mbrushmodel_t *const loadbmodel = ( ( mbrushmodel_t * )mod->extradata );

	for( unsigned i = 0; i < loadbmodel->numsurfaces; i++ ) {
		msurface_t *const surf = loadbmodel->surfaces + i;
		const mesh_t *const mesh = &surf->mesh;
		const shader_t *shader = surf->shader;

		if( !R_SurfPotentiallyVisible( surf ) ) {
			continue;
		}

		float *vert = mesh->xyzArray[0];
		// calculate bounding DOP of a surface
		BoundingDopBuilder<14> boundingDopBuilder( vert );
		vert += 4;

		for( unsigned j = 1; j < mesh->numVerts; j++ ) {
			boundingDopBuilder.addPoint( vert );
			vert += 4;
		}

		// foliage surfaces need special treatment for bounds
		if( surf->facetype == FACETYPE_FOLIAGE ) {
			vert = &surf->instances[0][4];
			for( unsigned j = 0; j < surf->numInstances; j++ ) {
				vec3_t temp;
				VectorMA( vert, vert[3], surf->mins, temp );
				boundingDopBuilder.addPoint( temp );

				VectorMA( vert, vert[3], surf->maxs, temp );
				boundingDopBuilder.addPoint( temp );

				vert += 8;
			}
		}

		boundingDopBuilder.storeTo( surf->mins, surf->maxs );

		// handle autosprites
		if( shader->flags & SHADER_AUTOSPRITE ) {
			// handle autosprites as trisurfs to avoid backface culling
			surf->facetype = FACETYPE_TRISURF;

			vec3_t inoutMins, inoutMaxs;
			VectorCopy( surf->mins, inoutMins );
			VectorCopy( surf->maxs, inoutMaxs );
			Mod_CalculateAutospriteBounds( surf, inoutMins, inoutMaxs );
			boundingDopBuilder.addPoint( inoutMins );
			boundingDopBuilder.addPoint( inoutMaxs );
			boundingDopBuilder.storeTo( surf->mins, surf->maxs );
		}
	}
}

static void Mod_SetupSubmodels( model_t *mod ) {
	mbrushmodel_t *const loadbmodel = ( ( mbrushmodel_t * )mod->extradata );

	// set up the submodels
	for( unsigned i = 0; i < loadbmodel->numsubmodels; i++ ) {
		mmodel_t *bm = &loadbmodel->submodels[i];
		model_t *starmod = &loadbmodel->inlines[i];
		mbrushmodel_t *bmodel = ( mbrushmodel_t * )starmod->extradata;

		memcpy( starmod, mod, sizeof( model_t ) );
		if( i ) {
			memcpy( bmodel, loadbmodel, sizeof( mbrushmodel_t ) );
		}

		bmodel->firstModelSurface = bm->firstModelSurface;
		bmodel->numModelSurfaces = bm->numModelSurfaces;

		bmodel->firstModelMergedSurface = bm->firstModelDrawSurface;
		bmodel->numModelMergedSurfaces = bm->numModelDrawSurfaces;

		starmod->extradata = bmodel;
		if( i == 0 ) {
			bmodel->visleafs = loadbmodel->visleafs;
			bmodel->numvisleafs = loadbmodel->numvisleafs;
		} else {
			bmodel->visleafs = nullptr;
			bmodel->numvisleafs = 0;
		}

		VectorCopy( bm->maxs, starmod->maxs );
		VectorCopy( bm->mins, starmod->mins );
		starmod->radius = bm->radius;

		if( i == 0 ) {
			*mod = *starmod;
		} else {
			bmodel->numsubmodels = 0;
		}
	}
}

static int R_CompareSurfacesByMergedSurf( const void *ps1, const void *ps2 ) {
	const auto *const s1 = (msortedSurface_t *)ps1;
	const auto *const s2 = (msortedSurface_t *)ps2;
	if( s1->surf->mergedSurfNum > s2->surf->mergedSurfNum ) {
		return 1;
	}
	if( s1->surf->mergedSurfNum < s2->surf->mergedSurfNum ) {
		return -1;
	}
	return (int)s1->surf->firstDrawSurfVert - (int)s2->surf->firstDrawSurfVert;
}

static void Mod_SortModelSurfaces( model_t *mod, unsigned int modnum ) {
	mbrushmodel_s *const loadbmodel = ( ( mbrushmodel_t * )mod->extradata );

	assert( modnum >= 0 && modnum < loadbmodel->numsubmodels );
	mmodel_t *const bm = loadbmodel->submodels + modnum;

	// ignore empty models
	const unsigned numSurfaces = bm->numModelSurfaces;
	const unsigned firstSurface = bm->firstModelSurface;
	if( !numSurfaces ) {
		return;
	}

	auto *const map = ( unsigned * )Q_malloc( numSurfaces * sizeof( unsigned ) );
	auto *const sortedSurfaces = ( msortedSurface_t * )Q_malloc( numSurfaces * sizeof( msortedSurface_t ) );
	auto *const backupSurfaces = ( msurface_t * )Q_malloc( numSurfaces * sizeof( msurface_t ) );
	for( unsigned i = 0; i < numSurfaces; i++ ) {
		sortedSurfaces[i].number = i;
		sortedSurfaces[i].surf = loadbmodel->surfaces + firstSurface + i;
	}

	memcpy( backupSurfaces, loadbmodel->surfaces + firstSurface, numSurfaces * sizeof( msurface_t ) );
	qsort( sortedSurfaces, numSurfaces, sizeof( msortedSurface_t ), &R_CompareSurfacesByMergedSurf );

	for( unsigned i = 0; i < numSurfaces; i++ ) {
		map[sortedSurfaces[i].number] = i;
	}

	if( !modnum && loadbmodel->visleafs ) {
		mleaf_t *leaf, **pleaf;
		for( pleaf = loadbmodel->visleafs, leaf = *pleaf; leaf; leaf = *++pleaf ) {
			for( unsigned j = 0; j < leaf->numVisSurfaces; j++ ) {
				leaf->visSurfaces[j] = map[leaf->visSurfaces[j]];
				leaf->fragmentSurfaces[j] = map[leaf->fragmentSurfaces[j]];
			}
		}
	}

	for( unsigned i = 0; i < numSurfaces; i++ ) {
		*(loadbmodel->surfaces + firstSurface + i) = backupSurfaces[sortedSurfaces[i].number];
	}

	unsigned lastMergedSurf = loadbmodel->numMergedSurfaces + 1;
	for( unsigned i = 0; i < numSurfaces; i++ ) {
		msurface_t *const surf = loadbmodel->surfaces + firstSurface + i;
		if( surf->mergedSurfNum ) {
			MergedBspSurface *const mergedSurf = &loadbmodel->mergedSurfaces[surf->mergedSurfNum - 1];
			if( lastMergedSurf != surf->mergedSurfNum ) {
				mergedSurf->numWorldSurfaces = 0;
				mergedSurf->firstWorldSurface = firstSurface + i;
				lastMergedSurf = surf->mergedSurfNum;
			}
			mergedSurf->numWorldSurfaces++;
		}
	}

	Q_free( map );
	Q_free( sortedSurfaces );
	Q_free( backupSurfaces );
}

static int Mod_CreateSubmodelBufferObjects( model_t *mod, unsigned modnum ) {
	assert( mod );

	mbrushmodel_t *const loadbmodel = ( ( mbrushmodel_t * )mod->extradata );
	assert( loadbmodel );

	assert( modnum >= 0 && modnum < loadbmodel->numsubmodels );
	mmodel_t *const bm = loadbmodel->submodels + modnum;

	// ignore empty models
	if( !bm->numModelSurfaces ) {
		return 0;
	}

	auto *const surfmap = ( msurface_t ** )Q_malloc( bm->numModelSurfaces * sizeof( void * ) );
	auto *const surfaces = ( msurface_t ** )Q_malloc( bm->numModelSurfaces * sizeof( void * ) );

	unsigned maxTempVBOs = 1024;

	auto *tempVBOs = ( mesh_vbo_t * )Q_malloc( maxTempVBOs * sizeof( mesh_vbo_t ) );
	const unsigned startDrawSurface = loadbmodel->numMergedSurfaces;

	bm->numModelDrawSurfaces = 0;
	bm->firstModelDrawSurface = startDrawSurface;

	unsigned rowbytes, rowlongs;
	int areabytes;

	unsigned numSurfaces = 0;
	uint8_t *visdata = nullptr;
	uint8_t *areadata = nullptr;
	if( !modnum && loadbmodel->pvs ) {
		rowbytes = loadbmodel->pvs->rowsize;
		rowlongs = ( rowbytes + 3 ) / 4;
		areabytes = ( loadbmodel->numareas + 7 ) / 8;

		if( !rowbytes ) {
			return 0;
		}

		// build visibility data for each face, based on what leafs
		// this face belongs to (visible from)
		visdata = ( uint8_t * )Q_malloc( rowlongs * 4 * loadbmodel->numsurfaces );
		areadata = ( uint8_t * )Q_malloc( areabytes * loadbmodel->numsurfaces );

		for( unsigned visLeafIndex = 0; visLeafIndex < loadbmodel->numvisleafs; ++visLeafIndex ) {
			mleaf_t *const leaf = loadbmodel->visleafs[visLeafIndex];
			for( unsigned surfIndex = 0; surfIndex < leaf->numVisSurfaces; surfIndex++ ) {
				const unsigned surfnum = leaf->visSurfaces[surfIndex];
				msurface_t *const surf = loadbmodel->surfaces + surfnum;

				// some buggy maps such as aeroq2 contain visleafs that address faces from submodels...
				if( surfnum < bm->numModelSurfaces && !surfmap[surfnum] ) {
					surfmap[surfnum] = surf;
					surfaces[numSurfaces] = surf;

					auto *const longrow  = ( int * )( visdata + numSurfaces * rowbytes );
					auto *const longrow2 = ( int * )( Mod_ClusterPVS( leaf->cluster, mod ) );

					// merge parent leaf cluster visibility into face visibility set
					// we could probably check for duplicates here because face can be
					// shared among multiple leafs
					for( unsigned j = 0; j < rowlongs; j++ ) {
						longrow[j] |= longrow2[j];
					}

					if( leaf->area >= 0 ) {
						uint8_t *const arearow = areadata + numSurfaces * areabytes;
						arearow[leaf->area >> 3] |= ( 1 << ( leaf->area & 7 ) );
					}

					numSurfaces++;
				}
			}
		}

		memset( surfmap, 0, bm->numModelSurfaces * sizeof( *surfmap ) );
	} else {
		// either a submodel or an unvised map
		rowbytes = 0;
		rowlongs = 0;
		visdata = nullptr;
		areabytes = 0;
		areadata = nullptr;

		msurface_t *surf = loadbmodel->surfaces + bm->firstModelSurface;
		for( unsigned i = 0; i < bm->numModelSurfaces; i++ ) {
			if( R_SurfPotentiallyVisible( surf ) ) {
				surfaces[numSurfaces] = surf;
				numSurfaces++;
			}
			surf++;
		}
	}

	// now linearly scan all faces for this submodel, merging them into
	// vertex buffer objects if they share shader, lightmap texture and we can render
	// them in hardware (some Q3A shaders require GLSL for that)

	// don't use half-floats for XYZ due to precision issues
	vattribmask_t floatVattribs = VATTRIB_POSITION_BIT;
	if( mapConfig.maxLightmapSize > 1024 ) {
		// don't use half-floats for lightmaps if there's not enough precision (half mantissa is 10 bits)
		floatVattribs |= VATTRIB_LMCOORDS_BITS;
	}

	unsigned numTempVBOs = 0;
	unsigned numResultVBOs = 0;
	unsigned numUnmappedSurfaces = numSurfaces;
	for( unsigned i = 0; i < numSurfaces; i++ ) {
		unsigned lastMerged = i;

		// done
		if( numUnmappedSurfaces == 0 ) {
			break;
		}

		// ignore faces already merged
		if( surfmap[i] ) {
			continue;
		}

		msurface_t *const surf = surfaces[i];
		shader_t *const shader = surf->shader;
		auto *const longrow    = ( int * )( visdata + i * rowbytes );
		uint8_t *const arearow = areadata + i * areabytes;

		int numFaces = 1;
		int numVerts = surf->mesh.numVerts;
		int numElems = surf->mesh.numElems;

		// portal or foliage surfaces can not be batched
		if( !( shader->flags & ( SHADER_PORTAL_CAPTURE | SHADER_PORTAL_CAPTURE2 ) ) && !surf->numInstances ) {
			// scan remaining face checking whether we merge them with the current one
			for( unsigned j = i + 1; j < numSurfaces; j++ ) {
				msurface_t *const surf2 = surfaces[j];

				// already merged
				if( surf2->mergedSurfNum ) {
					continue;
				}

				// the following checks ensure the two faces are compatible can can be merged
				// into a single vertex buffer object
				if( surf2->shader != surf->shader || surf2->superLightStyle != surf->superLightStyle ) {
					continue;
				}
				if( surf2->fog != surf->fog ) {
					continue;
				}
				if( numVerts + surf2->mesh.numVerts >= USHRT_MAX ) {
					continue;
				}
				if( surf2->numInstances != 0 ) {
					continue;
				}

				unsigned k = 0;
				int *longrow2 = nullptr;
				// unvised maps and submodels submodel can simply skip PVS checks
				if( !visdata ) {
					goto merge;
				}

				// only merge faces that reside in same map areas
				if( areabytes > 0 ) {
					// if areabits aren't equal, faces have different area visibility
					if( memcmp( arearow, areadata + j * areabytes, areabytes ) ) {
						continue;
					}
				}

				// if two faces potentially see same things, we can merge them
				longrow2 = ( int * )( visdata + j * rowbytes );

				while( k < rowlongs && !( longrow[k] & longrow2[k] ) ) {
					++k;
				}

				if( k != rowlongs ) {
					// merge visibility sets
					for( k = 0; k < rowlongs; k++ ) {
						longrow[k] |= longrow2[k];
					}
merge:
					numFaces++;
					numVerts += surf2->mesh.numVerts;
					numElems += surf2->mesh.numElems;
					surfmap[j] = surf;
					lastMerged = j;
				}
			}
		}

		// create vertex buffer object for this face then upload data
		vattribmask_t vattribs = shader->vattribs | surf->superLightStyle->vattribs | VATTRIB_NORMAL_BIT;
		if( surf->numInstances ) {
			vattribs |= VATTRIB_INSTANCES_BITS;
		}

		// create temp VBO to hold pre-batched info
		if( numTempVBOs == maxTempVBOs ) {
			maxTempVBOs += 1024;
			tempVBOs = (mesh_vbo_s *)Q_realloc( tempVBOs, maxTempVBOs * sizeof( *tempVBOs ) );
		}

		mesh_vbo_t *vbo = &tempVBOs[numTempVBOs++];
		vbo->numVerts = numVerts;
		vbo->numElems = numElems;
		vbo->vertexAttribs = vattribs;
		if( numFaces == 1 ) {
			// non-mergable
			vbo->index = numTempVBOs;
		}

		MergedBspSurface *const mergedSurf = &loadbmodel->mergedSurfaces[loadbmodel->numMergedSurfaces++];
		mergedSurf->superLightStyle = surf->superLightStyle;
		mergedSurf->instances = surf->instances;
		mergedSurf->numInstances = surf->numInstances;
		mergedSurf->fog = surf->fog;
		mergedSurf->shader = surf->shader;
		mergedSurf->numLightmaps = 0;

		// upload vertex and elements data for face itself
		surf->mergedSurfNum = loadbmodel->numMergedSurfaces;
		surf->firstDrawSurfVert = 0;
		surf->firstDrawSurfElem = 0;

		numVerts = surf->mesh.numVerts;
		numElems = surf->mesh.numElems;
		numUnmappedSurfaces--;

		// count lightmaps
		for( int lightmapStyle: surf->superLightStyle->lightmapStyles ) {
			if( lightmapStyle == 255 ) {
				break;
			}
			mergedSurf->numLightmaps++;
		}

		// now if there are any merged faces upload them to the same VBO
		if( numFaces > 1 ) {
			for( unsigned j = i + 1; j <= lastMerged; j++ ) {
				if( surfmap[j] == surf ) {
					msurface_t *const surf2 = surfaces[j];
					surf2->mergedSurfNum = loadbmodel->numMergedSurfaces;
					surf2->firstDrawSurfVert = numVerts;
					surf2->firstDrawSurfElem = numElems;
					numVerts += surf2->mesh.numVerts;
					numElems += surf2->mesh.numElems;
					numUnmappedSurfaces--;
				}
			}
		}

		mergedSurf->numVerts = numVerts;
		mergedSurf->numElems = numElems;
	}

	assert( numUnmappedSurfaces == 0 );

	// merge vertex buffer objects with identical vertex attribs
	unsigned numUnmergedVBOs = numTempVBOs;
	for( unsigned i = 0; i < numTempVBOs; i++ ) {
		if( !numUnmergedVBOs ) {
			break;
		}

		mesh_vbo_t *const vbo = &tempVBOs[i];
		if( vbo->index == 0 ) {
			for( unsigned j = i + 1; j < numTempVBOs; j++ ) {
				mesh_vbo_t *const vbo2 = &tempVBOs[j];
				// If unmerged
				if( vbo2->index == 0 ) {
					if( vbo2->vertexAttribs == vbo->vertexAttribs ) {
						if( vbo->numVerts + vbo2->numVerts < USHRT_MAX ) {
							MergedBspSurface *const mergedSurf = &loadbmodel->mergedSurfaces[startDrawSurface + j];
							mergedSurf->firstVboVert = vbo->numVerts;
							mergedSurf->firstVboElem = vbo->numElems;

							vbo->numVerts += vbo2->numVerts;
							vbo->numElems += vbo2->numElems;

							vbo2->index = i + 1;
							numUnmergedVBOs--;
						}
					}
				}
			}

			vbo->index = i + 1;
		}

		if( vbo->index == i + 1 ) {
			numUnmergedVBOs--;
		}
	}

	assert( numUnmergedVBOs == 0 );

	// create real VBOs and assign owner pointers
	numUnmergedVBOs = numTempVBOs;
	for( unsigned i = 0; i < numTempVBOs; i++ ) {
		mesh_vbo_t *const vbo = &tempVBOs[i];

		if( !numUnmergedVBOs ) {
			break;
		}

		if( vbo->owner != nullptr ) {
			// already assigned to a real VBO
			continue;
		}
		if( vbo->index != i + 1 ) {
			// not owning self, meaning it's been merged to another VBO
			continue;
		}

		MergedBspSurface *mergedSurf = &loadbmodel->mergedSurfaces[startDrawSurface + i];

		// don't use half-floats for XYZ due to precision issues
		vbo->owner = R_CreateMeshVBO( mergedSurf, vbo->numVerts, vbo->numElems, mergedSurf->numInstances,
									  vbo->vertexAttribs, VBO_TAG_WORLD, vbo->vertexAttribs & ~floatVattribs );
		mergedSurf->vbo = (mesh_vbo_s *)vbo->owner;

		if( mergedSurf->numInstances == 0 ) {
			for( unsigned j = i + 1; j < numTempVBOs; j++ ) {
				mesh_vbo_t *vbo2 = &tempVBOs[j];
				if( vbo2->index == i + 1 ) {
					vbo2->owner = vbo->owner;
					mergedSurf = &loadbmodel->mergedSurfaces[startDrawSurface + j];
					mergedSurf->vbo = (mesh_vbo_s *)vbo->owner;
					numUnmergedVBOs--;
				}
			}
		}

		numResultVBOs++;
		numUnmergedVBOs--;
	}

	assert( numUnmergedVBOs == 0 );

	// upload data to merged VBO's and assign offsets to drawSurfs
	for( unsigned i = 0; i < numSurfaces; i++ ) {
		msurface_t *const surf = surfaces[i];
		if( surf->mergedSurfNum ) {
			MergedBspSurface *const drawSurf = &loadbmodel->mergedSurfaces[surf->mergedSurfNum - 1];
			const mesh_t *mesh = &surf->mesh;
			mesh_vbo_t *const vbo = drawSurf->vbo;

			const int vertsOffset = drawSurf->firstVboVert + surf->firstDrawSurfVert;
			const int elemsOffset = drawSurf->firstVboElem + surf->firstDrawSurfElem;

			R_UploadVBOVertexData( vbo, vertsOffset, vbo->vertexAttribs, mesh );
			R_UploadVBOElemData( vbo, vertsOffset, elemsOffset, mesh );
			R_UploadVBOInstancesData( vbo, 0, surf->numInstances, surf->instances );
		}
	}

	bm->numModelDrawSurfaces = loadbmodel->numMergedSurfaces - bm->firstModelDrawSurface;

	Q_free( tempVBOs );
	Q_free( surfmap );
	Q_free( surfaces );

	if( visdata ) {
		Q_free( visdata );
	}
	if( areadata ) {
		Q_free( areadata );
	}

	return (int)numResultVBOs;
}

void Mod_CreateVertexBufferObjects( model_t *mod ) {
	mbrushmodel_t *loadbmodel = ( ( mbrushmodel_t * )mod->extradata );

	// free all VBO's allocated for previous world map so
	// we won't end up with both maps residing in video memory
	// until R_FreeUnusedVBOs call
	if( r_prevworldmodel && r_prevworldmodel->registrationSequence != rsh.registrationSequence ) {
		R_FreeVBOsByTag( VBO_TAG_WORLD );
	}

	// allocate memory for drawsurfs
	loadbmodel->numMergedSurfaces = 0;
	loadbmodel->mergedSurfaces    = (MergedBspSurface *)Q_malloc( sizeof( *loadbmodel->mergedSurfaces ) * loadbmodel->numsurfaces );

	for( unsigned i = 0; i < loadbmodel->numsubmodels; i++ ) {
		Mod_CreateSubmodelBufferObjects( mod, i );
	}

	for( unsigned i = 0; i < loadbmodel->numsubmodels; i++ ) {
		Mod_SortModelSurfaces( mod, i );
	}
}

static void Mod_CreateSkydome( model_t *mod ) {
	mbrushmodel_t *loadbmodel = (( mbrushmodel_t * )mod->extradata);
	for( unsigned i = 0; i < loadbmodel->numsubmodels; i++ ) {
		mmodel_t *bm = loadbmodel->submodels + i;

		msurface_t *surf;
		unsigned j;
		for( j = 0, surf = loadbmodel->surfaces + bm->firstModelSurface; j < bm->numModelSurfaces; j++, surf++ ) {
			if( surf->shader->flags & SHADER_SKY ) {
				loadbmodel->skyShader = surf->shader;
				return;
			}
		}
	}
}

static void Mod_FinalizeBrushModel( model_t *model ) {
	Mod_FinishFaces( model );

	Mod_CreateVisLeafs( model );

	Mod_CreateVertexBufferObjects( model );

	Mod_SetupSubmodels( model );

	Mod_CreateSkydome( model );
}

static void Mod_TouchBrushModel( model_t *model ) {
	auto *loadbmodel = ( ( mbrushmodel_t * )model->extradata );

	for( unsigned modnum = 0; modnum < loadbmodel->numsubmodels; modnum++ ) {
		loadbmodel->inlines[modnum].registrationSequence = rsh.registrationSequence;

		// Hacks for touching some shaders which don't have respective draw surfaces
		// in the current codebase state but still are in use
		// (namely sky shaders but also some other shaders).
		for( modnum = 0; modnum < loadbmodel->numsubmodels; modnum++ ) {
			auto *const bm = loadbmodel->submodels + modnum;
			for( unsigned i = 0; i < bm->numModelSurfaces; ++i  ) {
				auto *const surf = loadbmodel->surfaces + bm->firstModelSurface + i;
				if( surf->shader ) {
					R_TouchShader( surf->shader );
				}
			}
		}
	}

	// touch all shaders and vertex buffer objects for this bmodel

	for( unsigned i = 0; i < loadbmodel->numMergedSurfaces; i++ ) {
		MergedBspSurface *mergedSurf = &loadbmodel->mergedSurfaces[i];
		R_TouchShader( mergedSurf->shader );
		R_TouchMeshVBO( mergedSurf->vbo );
	}

	for( unsigned i = 0; i < loadbmodel->numfogs; i++ ) {
		if( loadbmodel->fogs[i].shader ) {
			R_TouchShader( loadbmodel->fogs[i].shader );
		}
	}

	R_TouchLightmapImages( model );
}

void R_InitModels() {
	memset( mod_novis, 0xff, sizeof( mod_novis ) );
	mod_isworldmodel = false;
	r_prevworldmodel = nullptr;
	mod_mapConfigs = (decltype( mod_mapConfigs ))Q_malloc( sizeof( *mod_mapConfigs ) * MAX_MOD_KNOWN );
}

static void Mod_Free( model_t *model ) {
	if( model->type == mod_alias ) {
		Mod_DestroyAliasMD3Model( (maliasmodel_s *)model->extradata );
	} else if( model->type == mod_skeletal ) {
		Mod_DestroySkeletalModel( (mskmodel_s *)model->extradata );
	} else if( model->type == mod_brush ) {
		Mod_DestroyQ3BrushModel( (mbrushmodel_s *)model->extradata );
	}
	Q_free( model->name );
	memset( model, 0, sizeof( *model ) );
	model->type = mod_free;
}

void R_FreeUnusedModels() {
	for( int i = 0; i < mod_numknown; ++i ) {
		model_s *const mod = &mod_known[i];
		if( mod->name ) {
			if( mod->registrationSequence != rsh.registrationSequence ) {
				Mod_Free( mod );
			}
		}
	}

	// check whether the world model has been freed
	if( rsh.worldModel && rsh.worldModel->type == mod_free ) {
		rsh.worldModel = nullptr;
		rsh.worldBrushModel = nullptr;
	}
}

void R_ShutdownModels() {
	for( int i = 0; i < mod_numknown; i++ ) {
		if( mod_known[i].name ) {
			Mod_Free( &mod_known[i] );
		}
	}

	rsh.worldModel = nullptr;
	rsh.worldBrushModel = nullptr;

	mod_numknown = 0;
	memset( mod_known, 0, sizeof( mod_known ) );
}

void Mod_StripLODSuffix( char *name ) {
	if( const size_t len = strlen( name ); len > 2 ) {
		if( name[len - 2] == '_' ) {
			const char digit = name[len - 1];
			if( digit >= '0' && digit <= '0' + MOD_MAX_LODS ) {
				name[len - 2] = 0;
			}
		}
	}
}

static model_t *Mod_FindSlot( const char *name ) {
	model_t *best = nullptr;
	for( int i = 0; i < mod_numknown; i++ ) {
		model_t *const mod = &mod_known[i];
		if( mod->type == mod_free ) {
			if( !best ) {
				best = mod;
			}
		} else {
			if( !Q_stricmp( mod->name, name ) ) {
				return mod;
			}
		}
	}

	if( best ) {
		return best;
	}

	//
	// find a free model slot spot
	//
	if( mod_numknown == MAX_MOD_KNOWN ) {
		Com_Error( ERR_DROP, "mod_numknown == MAX_MOD_KNOWN" );
	}

	return &mod_known[mod_numknown++];
}

int R_LoadFile_( const char *path, int flags, void **buffer, const char *filename, int fileline ) {
	uint8_t *buf = NULL; // quiet compiler warning

	int fhandle = 0;
	// look for it in the filesystem or pack files
	unsigned len = FS_FOpenFile( path, &fhandle, FS_READ | flags );

	if( !fhandle ) {
		if( buffer ) {
			*buffer = NULL;
		}
		return -1;
	}

	if( !buffer ) {
		FS_FCloseFile( fhandle );
		return len;
	}

	buf = ( uint8_t *)Q_malloc( len + 1 );
	buf[len] = 0;
	*buffer = buf;

	FS_Read( buf, len, fhandle );
	FS_FCloseFile( fhandle );

	return len;
}

void R_FreeFile_( void *buffer, const char *filename, int fileline ) {
	Q_free( buffer );
}

model_t *Mod_ForName( const char *name, bool crash ) {

	if( !name[0] ) {
		Com_Error( ERR_DROP, "Mod_ForName: empty name" );
	}

	//
	// inline models are grabbed only from worldmodel
	//
	if( name[0] == '*' ) {
		const int modnum = atoi( name + 1 );
		if( modnum < 1 || !rsh.worldModel || (unsigned)modnum >= rsh.worldBrushModel->numsubmodels ) {
			Com_Error( ERR_DROP, "bad inline model number" );
		}
		return &rsh.worldBrushModel->inlines[modnum];
	}

	char shortname[MAX_QPATH];
	Q_strncpyz( shortname, name, sizeof( shortname ) );
	COM_StripExtension( shortname );
	const char *extension = &name[strlen( shortname ) + 1];

	model_t *mod = Mod_FindSlot( name );
	if( mod->type == mod_bad ) {
		return nullptr;
	}
	if( mod->type != mod_free ) {
		return mod;
	}

	//
	// load the file
	//
	unsigned *buf;
	(void)R_LoadFile( name, (void **)&buf );
	if( !buf && crash ) {
		Com_Error( ERR_DROP, "Mod_NumForName: %s not found", name );
	}

	mod->type = mod_bad;
	mod->name = (char *)Q_malloc( strlen( name ) + 1 );
	strcpy( mod->name, name );

	// return the nullptr model
	if( !buf ) {
		return nullptr;
	}

	// call the apropriate loader
	bspFormatDesc_t *bspFormat = nullptr;
	const auto *descr = Q_FindFormatDescriptor( mod_supportedformats, ( const uint8_t * )buf, (const bspFormatDesc_t **)&bspFormat );
	if( !descr ) {
		Com_DPrintf( S_COLOR_YELLOW "Mod_NumForName: unknown fileid for %s", mod->name );
		return nullptr;
	}

	if( mod_isworldmodel ) {
		// we only init map config when loading the map from disk
		R_InitMapConfig( name );
	}

	descr->loader( mod, nullptr, buf, bspFormat );
	R_FreeFile( buf );

	if( mod->type == mod_bad ) {
		return nullptr;
	}

	if( mod_isworldmodel ) {
		// we only init map config when loading the map from disk
		R_FinishMapConfig( mod );
	}

	// do some common things
	if( mod->type == mod_brush ) {
		Mod_FinalizeBrushModel( mod );
		mod->touch = &Mod_TouchBrushModel;

		if( mod_isworldmodel ) {
			if( mod->name && Q_stricmp( mod->name, UI_BACKGROUND_MAP_PATH ) != 0 ) {
				Mod_BuildOccluders( mod );
			}
		}
	}

	if( !descr->maxLods ) {
		return mod;
	}

	//
	// load level-of-detail models
	//
	mod->lodnum = 0;
	mod->numlods = 0;
	for( int i = 0; i < descr->maxLods; i++ ) {
		char lodname[MAX_QPATH];
		Q_snprintfz( lodname, sizeof( lodname ), "%s_%i.%s", shortname, i + 1, extension );
		R_LoadFile( lodname, (void **)&buf );
		if( !buf || strncmp( (const char *)buf, descr->header, descr->headerLen ) ) {
			break;
		}

		model_t *lod = mod->lods[i] = Mod_FindSlot( lodname );
		if( lod->name && !strcmp( lod->name, lodname ) ) {
			continue;
		}

		lod->type = mod_bad;
		lod->lodnum = i + 1;
		lod->name = (char *)Q_malloc( strlen( lodname ) + 1 );
		strcpy( lod->name, lodname );

		mod_numknown++;

		descr->loader( lod, mod, buf, bspFormat );
		R_FreeFile( buf );

		mod->numlods++;
	}

	return mod;
}

static void R_TouchModel( model_t *mod ) {
	if( mod->registrationSequence == rsh.registrationSequence ) {
		return;
	}

	// touching a model precaches all images and possibly other assets
	mod->registrationSequence = rsh.registrationSequence;
	if( mod->touch ) {
		mod->touch( mod );
	}

	// handle Level Of Details
	for( int i = 0; i < mod->numlods; i++ ) {
		model_t *const lod = mod->lods[i];
		lod->registrationSequence = rsh.registrationSequence;
		if( lod->touch ) {
			lod->touch( lod );
		}
	}
}

//=============================================================================

/*
* R_InitMapConfig
*
* Clears map config before loading the map from disk. NOT called when the map
* is reloaded from model cache.
*/
static void R_InitMapConfig( const char *model ) {
	memset( &mapConfig, 0, sizeof( mapConfig ) );

	mapConfig.lightmapsPacking = false;
	mapConfig.lightmapArrays = false;
	mapConfig.maxLightmapSize = 0;
	mapConfig.deluxeMaps = false;
	mapConfig.deluxeMappingEnabled = false;
	mapConfig.forceClear = false;
	mapConfig.forceWorldOutlines = false;
	mapConfig.averageLightingIntensity = 1;

	VectorClear( mapConfig.ambient );
	VectorClear( mapConfig.outlineColor );

	if( r_lighting_packlightmaps->integer ) {
		char lightmapsPath[MAX_QPATH], *p;

		mapConfig.lightmapsPacking = true;

		Q_strncpyz( lightmapsPath, model, sizeof( lightmapsPath ) );
		p = strrchr( lightmapsPath, '.' );
		if( p ) {
			*p = 0;
			Q_strncatz( lightmapsPath, "/lm_0000.tga", sizeof( lightmapsPath ) );
			if( FS_FOpenFile( lightmapsPath, nullptr, FS_READ ) != -1 ) {
				Com_DPrintf( S_COLOR_YELLOW "External lightmap stage: lightmaps packing is disabled\n" );
				mapConfig.lightmapsPacking = false;
			}
		}
	}
}

/*
* R_FinishMapConfig
*
* Called after loading the map from disk.
*/
static void R_FinishMapConfig( const model_t *mod ) {
	// ambient lighting
	if( r_fullbright->integer ) {
		VectorSet( mapConfig.ambient, 1, 1, 1 );
		mapConfig.averageLightingIntensity = 1;
	} else {
		ColorNormalize( mapConfig.ambient,  mapConfig.ambient );
	}

	mod_mapConfigs[mod - mod_known] = mapConfig;
}

void R_RegisterWorldModel( const char *model ) {
	r_prevworldmodel = rsh.worldModel;
	rsh.worldModel = nullptr;
	rsh.worldBrushModel = nullptr;
	rsh.worldModelSequence++;

	mod_isworldmodel = true;

	rsh.worldModel = Mod_ForName( model, true );

	mod_isworldmodel = false;

	if( !rsh.worldModel ) {
		return;
	}

	// FIXME: this is ugly...
	mapConfig = mod_mapConfigs[rsh.worldModel - mod_known];

	R_TouchModel( rsh.worldModel );
	rsh.worldBrushModel = ( mbrushmodel_t * )rsh.worldModel->extradata;
}

struct model_s *R_RegisterModel( const char *name ) {
	model_t *const mod = Mod_ForName( name, false );
	if( mod ) {
		R_TouchModel( mod );
	}
	return mod;
}

void R_ModelBounds( const model_t *model, vec3_t mins, vec3_t maxs ) {
	if( model ) {
		VectorCopy( model->mins, mins );
		VectorCopy( model->maxs, maxs );
	} else if( rsh.worldModel ) {
		VectorCopy( rsh.worldModel->mins, mins );
		VectorCopy( rsh.worldModel->maxs, maxs );
	}
}

void R_ModelFrameBounds( const struct model_s *model, int frame, vec3_t mins, vec3_t maxs ) {
	if( model ) {
		switch( model->type ) {
			case mod_alias:
				R_AliasModelFrameBounds( model, frame, mins, maxs );
				break;
			case mod_skeletal:
				R_SkeletalModelFrameBounds( model, frame, mins, maxs );
				break;
			default:
				break;
		}
	}
}

void R_BrushModelBBox( const entity_t *e, vec3_t mins, vec3_t maxs, bool *rotated ) {
	const model_t *model = e->model;

	if( !Matrix3_Compare( e->axis, axis_identity ) ) {
		if( rotated ) {
			*rotated = true;
		}
		for( unsigned i = 0; i < 3; i++ ) {
			mins[i] = e->origin[i] - model->radius * e->scale;
			maxs[i] = e->origin[i] + model->radius * e->scale;
		}
	} else {
		if( rotated ) {
			*rotated = false;
		}
		VectorMA( e->origin, e->scale, model->mins, mins );
		VectorMA( e->origin, e->scale, model->maxs, maxs );
	}
}

int R_LODForSphere( const vec3_t origin, float radius, const float *viewOrigin, float fovLodScale, float viewLodScale ) {
	assert( fovLodScale > 0.0f );
	assert( viewLodScale > 0.0f && viewLodScale <= 1.0f );

	float dist = DistanceFast( origin, viewOrigin );
	// The tested distance should be larger for greater fovs
	dist *= fovLodScale;
	// The tested distance should be larger for smaller views
	dist /= viewLodScale;

	int lod = (int)( dist / radius );
	if( r_lodscale->integer ) {
		lod /= r_lodscale->integer;
	}
	lod += r_lodbias->integer;

	if( lod < 1 ) {
		return 0;
	}
	return lod;
}

void R_LightForOrigin( const vec3_t origin, vec3_t dir, vec4_t ambient, vec4_t diffuse, float radius, bool noWorldLight ) {
	int i, j;
	int k, s;
	int vi[3], elem[4];
	float t[8];
	vec3_t vf, vf2, tdir;
	vec3_t ambientLocal, diffuseLocal;
	vec_t *gridSize, *gridMins;
	int *gridBounds;
	mgridlight_t lightarray[8];

	VectorSet( ambientLocal, 0, 0, 0 );
	VectorSet( diffuseLocal, 0, 0, 0 );

	if( noWorldLight ) {
		VectorSet( dir, 0.0f, 0.0f, 0.0f );
		goto dynamic;
	}
	if( !rsh.worldModel || !rsh.worldBrushModel->lightgrid || !rsh.worldBrushModel->numlightgridelems ) {
		VectorSet( dir, 0.1f, 0.2f, 0.7f );
		goto dynamic;
	}

	gridSize = rsh.worldBrushModel->gridSize;
	gridMins = rsh.worldBrushModel->gridMins;
	gridBounds = rsh.worldBrushModel->gridBounds;

	for( i = 0; i < 3; i++ ) {
		vf[i] = ( origin[i] - gridMins[i] ) / gridSize[i];
		vi[i] = (int)vf[i];
		vf[i] = vf[i] - floor( vf[i] );
		vf2[i] = 1.0f - vf[i];
	}

	elem[0] = vi[2] * gridBounds[3] + vi[1] * gridBounds[0] + vi[0];
	elem[1] = elem[0] + gridBounds[0];
	elem[2] = elem[0] + gridBounds[3];
	elem[3] = elem[2] + gridBounds[0];

	for( i = 0; i < 4; i++ ) {
		lightarray[i * 2 + 0] = rsh.worldBrushModel->lightgrid[rsh.worldBrushModel->lightarray[bound( 0, elem[i] + 0,
																									  (int)rsh.worldBrushModel->numlightarrayelems - 1 )]];
		lightarray[i * 2 + 1] = rsh.worldBrushModel->lightgrid[rsh.worldBrushModel->lightarray[bound( 1, elem[i] + 1,
																									  (int)rsh.worldBrushModel->numlightarrayelems - 1 )]];
	}

	t[0] = vf2[0] * vf2[1] * vf2[2];
	t[1] = vf[0] * vf2[1] * vf2[2];
	t[2] = vf2[0] * vf[1] * vf2[2];
	t[3] = vf[0] * vf[1] * vf2[2];
	t[4] = vf2[0] * vf2[1] * vf[2];
	t[5] = vf[0] * vf2[1] * vf[2];
	t[6] = vf2[0] * vf[1] * vf[2];
	t[7] = vf[0] * vf[1] * vf[2];

	VectorClear( dir );

	for( i = 0; i < 4; i++ ) {
		R_LatLongToNorm( lightarray[i * 2].direction, tdir );
		VectorScale( tdir, t[i * 2], tdir );
		for( k = 0; k < MAX_LIGHTMAPS && ( s = lightarray[i * 2].styles[k] ) != 255; k++ ) {
			dir[0] += lightStyles[s].rgb[0] * tdir[0];
			dir[1] += lightStyles[s].rgb[1] * tdir[1];
			dir[2] += lightStyles[s].rgb[2] * tdir[2];
		}

		R_LatLongToNorm( lightarray[i * 2 + 1].direction, tdir );
		VectorScale( tdir, t[i * 2 + 1], tdir );
		for( k = 0; k < MAX_LIGHTMAPS && ( s = lightarray[i * 2 + 1].styles[k] ) != 255; k++ ) {
			dir[0] += lightStyles[s].rgb[0] * tdir[0];
			dir[1] += lightStyles[s].rgb[1] * tdir[1];
			dir[2] += lightStyles[s].rgb[2] * tdir[2];
		}
	}

	for( j = 0; j < 3; j++ ) {
		if( ambient ) {
			for( i = 0; i < 4; i++ ) {
				for( k = 0; k < MAX_LIGHTMAPS; k++ ) {
					if( ( s = lightarray[i * 2].styles[k] ) != 255 ) {
						ambientLocal[j] += t[i * 2] * lightarray[i * 2].ambient[k][j] * lightStyles[s].rgb[j];
					}
					if( ( s = lightarray[i * 2 + 1].styles[k] ) != 255 ) {
						ambientLocal[j] += t[i * 2 + 1] * lightarray[i * 2 + 1].ambient[k][j] * lightStyles[s].rgb[j];
					}
				}
			}
		}
		if( diffuse || radius ) {
			for( i = 0; i < 4; i++ ) {
				for( k = 0; k < MAX_LIGHTMAPS; k++ ) {
					if( ( s = lightarray[i * 2].styles[k] ) != 255 ) {
						diffuseLocal[j] += t[i * 2] * lightarray[i * 2].diffuse[k][j] * lightStyles[s].rgb[j];
					}
					if( ( s = lightarray[i * 2 + 1].styles[k] ) != 255 ) {
						diffuseLocal[j] += t[i * 2 + 1] * lightarray[i * 2 + 1].diffuse[k][j] * lightStyles[s].rgb[j];
					}
				}
			}
		}
	}

	// convert to grayscale
	if( r_lighting_grayscale->integer ) {
		vec_t grey;

		if( ambient ) {
			grey = ColorGrayscale( ambientLocal );
			ambientLocal[0] = ambientLocal[1] = ambientLocal[2] = bound( 0, grey, 255 );
		}

		if( diffuse || radius ) {
			grey = ColorGrayscale( diffuseLocal );
			diffuseLocal[0] = diffuseLocal[1] = diffuseLocal[2] = bound( 0, grey, 255 );
		}
	}

	dynamic:
	// add dynamic lights
	if( radius ) {
		wsw::ref::Frontend::instance()->dynLightDirForOrigin( origin, radius, dir, diffuseLocal, ambientLocal );
	}

	VectorNormalizeFast( dir );

	if( r_fullbright->integer ) {
		VectorSet( ambientLocal, 1, 1, 1 );
		VectorSet( diffuseLocal, 1, 1, 1 );
	} else {
		const float scale = ( 1 << mapConfig.overbrightBits ) / 255.0f;

		for( i = 0; i < 3; i++ ) {
			ambientLocal[i] = ambientLocal[i] * scale * bound( 0.0f, r_lighting_ambientscale->value, 1.0f );
		}
		ColorNormalize( ambientLocal, ambientLocal );

		for( i = 0; i < 3; i++ ) {
			diffuseLocal[i] = diffuseLocal[i] * scale * bound( 0.0f, r_lighting_directedscale->value, 1.0f );
		}
		ColorNormalize( diffuseLocal, diffuseLocal );
	}

	if( ambient ) {
		VectorCopy( ambientLocal, ambient );
		ambient[3] = 1.0f;
	}

	if( diffuse ) {
		VectorCopy( diffuseLocal, diffuse );
		diffuse[3] = 1.0f;
	}
}

void R_NormToLatLong( const vec_t *normal, uint8_t latlong[2] ) {
	float flatlong[2];

	NormToLatLong( normal, flatlong );
	latlong[0] = (int)( flatlong[0] * 255.0 / M_TWOPI ) & 255;
	latlong[1] = (int)( flatlong[1] * 255.0 / M_TWOPI ) & 255;
}

void R_LatLongToNorm4( const uint8_t latlong[2], vec4_t out ) {
	static float * const sinTable = rsh.sinTableByte;
	float sin_a, sin_b, cos_a, cos_b;

	cos_a = sinTable[( latlong[0] + 64 ) & 255];
	sin_a = sinTable[latlong[0]];
	cos_b = sinTable[( latlong[1] + 64 ) & 255];
	sin_b = sinTable[latlong[1]];

	Vector4Set( out, cos_b * sin_a, sin_b * sin_a, cos_a, 0 );
}

void R_LatLongToNorm( const uint8_t latlong[2], vec3_t out ) {
	vec4_t t;
	R_LatLongToNorm4( latlong, t );
	VectorCopy( t, out );
}

void R_BuildTangentVectors( int numVertexes, vec4_t *xyzArray, vec4_t *normalsArray,
							vec2_t *stArray, int numTris, elem_t *elems, vec4_t *sVectorsArray ) {
	vec3_t stackTVectorsArray[128];
	vec3_t *tVectorsArray;

	if( numVertexes > sizeof( stackTVectorsArray ) / sizeof( stackTVectorsArray[0] ) ) {
		tVectorsArray = (vec3_t *)Q_malloc( sizeof( vec3_t ) * numVertexes );
	} else {
		tVectorsArray = stackTVectorsArray;
	}

	// assuming arrays have already been allocated
	// this also does some nice precaching
	memset( sVectorsArray, 0, numVertexes * sizeof( *sVectorsArray ) );
	memset( tVectorsArray, 0, numVertexes * sizeof( *tVectorsArray ) );

	for( int i = 0; i < numTris; i++, elems += 3 ) {
		float *v[3], *tc[3];
		for( int j = 0; j < 3; j++ ) {
			v[j] = ( float * )( xyzArray + elems[j] );
			tc[j] = ( float * )( stArray + elems[j] );
		}

		vec3_t stvec[3];
		// calculate two mostly perpendicular edge directions
		VectorSubtract( v[1], v[0], stvec[0] );
		VectorSubtract( v[2], v[0], stvec[1] );

		vec3_t cross;
		// we have two edge directions, we can calculate the normal then
		CrossProduct( stvec[1], stvec[0], cross );

		for( int j = 0; j < 3; j++ ) {
			stvec[0][j] = ( ( tc[1][1] - tc[0][1] ) * ( v[2][j] - v[0][j] ) - ( tc[2][1] - tc[0][1] ) * ( v[1][j] - v[0][j] ) );
			stvec[1][j] = ( ( tc[1][0] - tc[0][0] ) * ( v[2][j] - v[0][j] ) - ( tc[2][0] - tc[0][0] ) * ( v[1][j] - v[0][j] ) );
		}

		// inverse tangent vectors if their cross product goes in the opposite
		// direction to triangle normal
		CrossProduct( stvec[1], stvec[0], stvec[2] );
		if( DotProduct( stvec[2], cross ) < 0 ) {
			VectorInverse( stvec[0] );
			VectorInverse( stvec[1] );
		}

		for( int j = 0; j < 3; j++ ) {
			VectorAdd( sVectorsArray[elems[j]], stvec[0], sVectorsArray[elems[j]] );
			VectorAdd( tVectorsArray[elems[j]], stvec[1], tVectorsArray[elems[j]] );
		}
	}

	// normalize
	float *s = *sVectorsArray, *t = *tVectorsArray, *n = *normalsArray;
	for( int i = 0; i < numVertexes; i++ ) {
		// keep s\t vectors perpendicular
		float d = -DotProduct( s, n );
		VectorMA( s, d, n, s );
		VectorNormalize( s );

		d = -DotProduct( t, n );
		VectorMA( t, d, n, t );

		vec3_t cross;
		// store polarity of t-vector in the 4-th coordinate of s-vector
		CrossProduct( n, s, cross );
		if( DotProduct( cross, t ) < 0 ) {
			s[3] = -1;
		} else {
			s[3] = 1;
		}

		s += 4, t += 3, n += 4;
	}

	if( tVectorsArray != stackTVectorsArray ) {
		Q_free( tVectorsArray );
	}
}
