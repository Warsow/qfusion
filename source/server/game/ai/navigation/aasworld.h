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

#ifndef WSW_5dd29ec8_a176_43b0_b48a_b41c8ff11162_H
#define WSW_5dd29ec8_a176_43b0_b48a_b41c8ff11162_H

#include <common/helpers/q_math.h>
#include <common/facilities/q_shared.h>
#include "../vec3.h"

#include <common/common.h>
#include <common/types/stringview.h>
#include <common/types/staticstring.h>

#include <span>

//travel types
#define MAX_TRAVELTYPES             32
#define TRAVEL_INVALID              1       //temporary not possible
#define TRAVEL_WALK                 2       //walking
#define TRAVEL_CROUCH               3       //crouching
#define TRAVEL_BARRIERJUMP          4       //jumping onto a barrier
#define TRAVEL_JUMP                 5       //jumping
#define TRAVEL_LADDER               6       //climbing a ladder
#define TRAVEL_WALKOFFLEDGE         7       //walking of a ledge
#define TRAVEL_SWIM                 8       //swimming
#define TRAVEL_WATERJUMP            9       //jump out of the water
#define TRAVEL_TELEPORT             10      //teleportation
#define TRAVEL_ELEVATOR             11      //travel by elevator
#define TRAVEL_ROCKETJUMP           12      //rocket jumping required for travel
#define TRAVEL_BFGJUMP              13      //bfg jumping required for travel
#define TRAVEL_GRAPPLEHOOK          14      //grappling hook required for travel
#define TRAVEL_DOUBLEJUMP           15      //double jump
#define TRAVEL_RAMPJUMP             16      //ramp jump
#define TRAVEL_STRAFEJUMP           17      //strafe jump
#define TRAVEL_JUMPPAD              18      //jump pad
#define TRAVEL_FUNCBOB              19      //func bob

//additional travel flags
#define TRAVELTYPE_MASK             0xFFFFFF
#define TRAVELFLAG_NOTTEAM1         ( 1 << 24 )
#define TRAVELFLAG_NOTTEAM2         ( 2 << 24 )

//face flags
#define FACE_SOLID                  1       //just solid at the other side
#define FACE_LADDER                 2       //ladder
#define FACE_GROUND                 4       //standing on ground when in this face
#define FACE_GAP                    8       //gap in the ground
#define FACE_LIQUID                 16      //face seperating two areas with liquid
#define FACE_LIQUIDSURFACE          32      //face seperating liquid and air
#define FACE_BRIDGE                 64      //can walk over this face if bridge is closed

//area contents
#define AREACONTENTS_WATER              1
#define AREACONTENTS_LAVA               2
#define AREACONTENTS_SLIME              4
#define AREACONTENTS_CLUSTERPORTAL      8
#define AREACONTENTS_TELEPORTAL         16
#define AREACONTENTS_ROUTEPORTAL        32
#define AREACONTENTS_TELEPORTER         64
#define AREACONTENTS_JUMPPAD            128
#define AREACONTENTS_DONOTENTER         256
#define AREACONTENTS_VIEWPORTAL         512
#define AREACONTENTS_MOVER              1024
#define AREACONTENTS_NOTTEAM1           2048
#define AREACONTENTS_NOTTEAM2           4096
//number of model of the mover inside this area
#define AREACONTENTS_MODELNUMSHIFT      24
#define AREACONTENTS_MAXMODELNUM        0xFF
#define AREACONTENTS_MODELNUM           ( AREACONTENTS_MAXMODELNUM << AREACONTENTS_MODELNUMSHIFT )

//area flags
#define AREA_GROUNDED               1       // a bot can stand on the ground
#define AREA_LADDER                 2       // an area contains one or more ladder faces
#define AREA_LIQUID                 4       // an area contains a liquid
#define AREA_DISABLED               8       // an area is disabled for routing when set
#define AREA_BRIDGE                 16      // an area is on top of a bridge

// These flags are specific to this engine and are set on world loading based on various computations
#define AREA_LEDGE            ( 1 << 10 )  // an area looks like a ledge. This flag is set on world loading.
#define AREA_WALL             ( 1 << 11 )  // an area has bounding solid walls.
#define AREA_JUNK             ( 1 << 12 )  // an area does not look like useful.
#define AREA_INCLINED_FLOOR   ( 1 << 13 )  // an area has an inclined floor (AAS treats these areas as having a flat one)
#define AREA_SLIDABLE_RAMP    ( 1 << 14 )  // an area is a slidable ramp (AREA_INCLINED_FLOOR is implied and set too)

// If any part of a bot box is within an area having one of these flags
// collision can be safely skipped for 32, 48 or 64 units around the bot
// in XY plane during movement prediction.
// It's safe to assume there's no ceiling a bot can bump into as well.
#define AREA_SKIP_COLLISION_32   ( 1 << 20 )
#define AREA_SKIP_COLLISION_48   ( 1 << 21 )
#define AREA_SKIP_COLLISION_64   ( 1 << 22 )

#define AREA_SKIP_COLLISION_MASK  ( AREA_SKIP_COLLISION_32 | AREA_SKIP_COLLISION_48 | AREA_SKIP_COLLISION_64 )

// a movement in an area is safe from falling/entering a hazard
// (this is currently not a 100% guarantee but an optimistic estimation)
#define AREA_NOFALL              ( 1 << 25 )

//========== bounding box =========

//bounding box
typedef struct aas_bbox_s {
	int presencetype;
	int flags;
	vec3_t mins, maxs;
} aas_bbox_t;

//============ settings ===========

//reachability to another area
typedef struct aas_reachability_s {
	int areanum;                        //number of the reachable area
	int facenum;                        //number of the face towards the other area
	int edgenum;                        //number of the edge towards the other area
	vec3_t start;                       //start point of inter area movement
	vec3_t end;                         //end point of inter area movement
	int traveltype;                 //type of travel required to get to the area
	unsigned short int traveltime;//travel time of the inter area movement
} aas_reachability_t;

//area settings
typedef struct aas_areasettings_s {
	//could also add all kind of statistic fields
	int contents;                       //contents of the area
	int areaflags;                      //several area flags
	int presencetype;                   //how a bot can be present in this area
	int cluster;                        //cluster the area belongs to, if negative it's a portal
	int clusterareanum;             //number of the area in the cluster
	int numreachableareas;          //number of reachable areas from this one
	int firstreachablearea;         //first reachable area in the reachable area index
} aas_areasettings_t;

//cluster portal
typedef struct aas_portal_s {
	int areanum;                        //area that is the actual portal
	int frontcluster;                   //cluster at front of portal
	int backcluster;                    //cluster at back of portal
	int clusterareanum[2];          //number of the area in the front and back cluster
} aas_portal_t;

//cluster portal index
typedef int aas_portalindex_t;

//cluster
typedef struct aas_cluster_s {
	int numareas;                       //number of areas in the cluster
	int numreachabilityareas;           //number of areas with reachabilities
	int numportals;                     //number of cluster portals
	int firstportal;                    //first cluster portal in the index
} aas_cluster_t;

//============ 3d definition ============

typedef vec3_t aas_vertex_t;

//just a plane in the third dimension
typedef struct aas_plane_s {
	vec3_t normal;                      //normal vector of the plane
	float dist;                         //distance of the plane (normal vector * distance = point in plane)
	uint16_t type;
	uint16_t signBits;
} aas_plane_t;

//edge
typedef struct aas_edge_s {
	int v[2];                           //numbers of the vertexes of this edge
} aas_edge_t;

//edge index, negative if vertexes are reversed
typedef int aas_edgeindex_t;

//a face bounds an area, often it will also seperate two areas
typedef struct aas_face_s {
	int planenum;                       //number of the plane this face is in
	int faceflags;                      //face flags (no use to create face settings for just this field)
	int numedges;                       //number of edges in the boundary of the face
	int firstedge;                      //first edge in the edge index
	int frontarea;                      //area at the front of this face
	int backarea;                       //area at the back of this face
} aas_face_t;

//face index, stores a negative index if backside of face
typedef int aas_faceindex_t;

//area with a boundary of faces
typedef struct aas_area_s {
	int areanum;                        //number of this area
	//3d definition
	int numfaces;                       //number of faces used for the boundary of the area
	int firstface;                      //first face in the face index used for the boundary of the area
	vec3_t mins;                        //mins of the area
	vec3_t maxs;                        //maxs of the area
	vec3_t center;                      //'center' of the area
} aas_area_t;

//nodes of the bsp tree
typedef struct aas_node_s {
	int planenum;
	int children[2];                    //child nodes of this node, or areas as leaves when negative
	//when a child is zero it's a solid leaf
} aas_node_t;

class alignas( 16 ) AiAasWorld {
	friend class AasFileReader;

	// Try grouping fields to avoid alignment gaps

	int m_numareas { 0 };
	int m_numareasettings { 0 };
	aas_area_t *m_areas { nullptr };
	aas_areasettings_t *m_areasettings { nullptr };

	int m_reachabilitysize { 0 };
	int m_numnodes { 0 };
	aas_reachability_t *m_reachability { nullptr };
	aas_node_t *m_nodes { nullptr };

	int m_numvertexes { 0 };
	int m_numplanes { 0 };
	aas_vertex_t *m_vertexes { nullptr };
	aas_plane_t *m_planes { nullptr };

	int m_numedges { 0 };
	int m_edgeindexsize { 0 };
	aas_edge_t *m_edges { nullptr };
	aas_edgeindex_t *m_edgeindex { nullptr };

	int m_numfaces { 0 };
	int m_faceindexsize { 0 };
	aas_face_t *m_faces { nullptr };
	aas_faceindex_t *m_faceindex { nullptr };

	int m_numportals { 0 };
	int m_portalindexsize { 0 };
	aas_portal_t *m_portals { nullptr };
	aas_portalindex_t *m_portalindex { nullptr };

	int m_numclusters { 0 };
	bool m_loaded { false };
	aas_cluster_t *m_clusters { nullptr };

	// A number of a floor cluster for an area, 0 = not in a floor cluster
	uint16_t *m_areaFloorClusterNums { nullptr };
	// A number of a stairs cluster for an area, 0 = not in a stairs cluster
	uint16_t *m_areaStairsClusterNums { nullptr };

	int m_numFloorClusters { 0 };
	int m_numStairsClusters { 0 };

	// An element #i contains an offset of cluster elements sequence in the joint data
	int *m_floorClusterDataOffsets { nullptr };
	// An element #i contains an offset of cluster elements sequence in the joint data
	int *m_stairsClusterDataOffsets { nullptr };

	// Contains floor clusters element sequences, each one is prepended by the length
	uint16_t *m_floorClusterData { nullptr };
	// Contains stairs clusters element sequences, each one is prepended by the length
	uint16_t *m_stairsClusterData { nullptr };

	// Elements #i*2, #i*2+1 contain numbers of vertices of a 2d face proj for face #i
	int *m_face2DProjVertexNums { nullptr };

	// An element #i contains an offset of leafs list data in the joint data
	int *m_areaMapLeafListOffsets { nullptr };
	// Contains area map (collision/vis) leafs lists, each one is prepended by the length
	int *m_areaMapLeafsData { nullptr };

	bool *m_floorClustersVisTable { nullptr };

	uint16_t *m_areaVisData { nullptr };
	int32_t *m_areaVisDataOffsets { nullptr };

	uint16_t *m_groundedPrincipalRoutingAreas { nullptr };
	uint16_t *m_jumppadReachPassThroughAreas { nullptr };
	uint16_t *m_ladderReachPassThroughAreas { nullptr };
	uint16_t *m_elevatorReachPassThroughAreas { nullptr };
	uint16_t *m_walkOffLedgePassThroughAirAreas { nullptr };

	// Contains bounds of a maximal box that is fully within an area.
	// Mins/maxs are rounded and stored as six 16-bit signed integers.
	int16_t *m_areaInnerBounds { nullptr };

	static constexpr float kAreaGridCellSize { 32.0f };

	int32_t *m_pointAreaNumLookupGridData { nullptr };
	unsigned m_numGridCellsPerDimensions[3] { 0, 0, 0 };

	alignas( 16 ) vec4_t m_worldMins, m_worldMaxs;

	// Should be released by G_Free();
	char *m_checksum { nullptr };

	static AiAasWorld *s_instance;

	[[nodiscard]]
	bool load( const wsw::StringView &baseMapName );
	void postLoad( const wsw::StringView &baseMapName );

	void swapData();
	void categorizePlanes();

	// Computes extra area flags based on loaded world data
	void computeExtraAreaData( const wsw::StringView &baseMapName );
	// Computes extra area floor and stairs clusters
	void computeLogicalAreaClusters();
    // Computes vertices of top 2D face projections
	void computeFace2DProjVertices();
	// Computes map (collision/vis) leafs for areas
	void computeAreasLeafsLists();
	// Builds lists of specific area types
	void buildSpecificAreaTypesLists();

	[[nodiscard]]
	static auto getBaseMapName( const wsw::StringView &fillName ) -> wsw::StringView;

	static void makeFileName( wsw::StaticString<MAX_QPATH> &buffer,
							  const wsw::StringView &strippedName,
							  const wsw::StringView &extension );

	void loadAreaVisibility( const wsw::StringView &baseMapName );
	void computeAreasVisibility( uint32_t *offsetsDataSize, uint32_t *listsDataSize );

	void loadFloorClustersVisibility( const wsw::StringView &baseMapName );

	// Returns the actual data size in bytes
	[[nodiscard]]
	auto computeFloorClustersVisibility() -> uint32_t;

	[[nodiscard]]
	bool computeVisibilityForClustersPair( int floorClusterNum1, int floorClusterNum2 ) const;

	void computeInnerBoundsForAreas();

	void setupPointAreaNumLookupGrid();
	[[nodiscard]]
	auto computePointAreaNumLookupDataForCell( const Vec3 &cellMins, const Vec3 &cellMaxs ) const -> int32_t;

	void trySettingAreaLedgeFlags( int areaNum );
	void trySettingAreaWallFlags( int areaNum );
	void trySettingAreaJunkFlags( int areaNum );
	void trySettingAreaRampFlags( int areaNum );

	void trySettingAreaNoFallFlags( int areaNum );

	// Should be called after all other flags are computed
	void trySettingAreaSkipCollisionFlags();

	[[nodiscard]]
	auto findAreaNum( const vec3_t mins, const vec3_t maxs ) const -> int;

	static void setupBoxLookupTable( vec3_t *__restrict lookupTable,
									 const float *__restrict absMins,
									 const float *__restrict absMaxs );

	template <typename T>
	[[nodiscard]]
	static auto internalSpanToRegularSpan( const T *internalSpan ) -> std::span<const T> {
		const T *data = internalSpan + 1;
		const size_t size = internalSpan[0];
		return { data, data + size };
	}
public:
	AiAasWorld() = default;
	~AiAasWorld();

	// AiAasWorld should be init and shut down explicitly
	// (a game library is not unloaded when a map changes)
	[[maybe_unused]]
	static bool init( const wsw::StringView &mapName );
	static void shutdown();
	[[nodiscard]]
	static auto instance() -> AiAasWorld * { return s_instance; }

	[[nodiscard]]
	bool isLoaded() const { return m_loaded; }
	[[nodiscard]]
	auto getChecksum() const { return m_loaded ? wsw::StringView( (const char *)m_checksum ) : wsw::StringView(); }

	[[nodiscard]]
	auto traceAreas( const Vec3 &start, const Vec3 &end, int *areas_, int maxareas ) const -> std::span<const int> {
		return traceAreas( start.Data(), end.Data(), areas_, nullptr, maxareas );
	}

	[[nodiscard]]
	auto traceAreas( const vec3_t start, const vec3_t end, int *areas_, int maxareas ) const -> std::span<const int> {
		return traceAreas( start, end, areas_, nullptr, maxareas );
	}

	//stores the areas the trace went through and returns the number of passed areas
	[[nodiscard]]
	auto traceAreas( const vec3_t start, const vec3_t end, int *areas_,
					 vec3_t *points, int maxareas ) const -> std::span<const int>;

	[[nodiscard]]
	auto findAreasInBox( const Vec3 &absMins, const Vec3 &absMaxs, int *areaNums,
						 int maxAreas, int topNodeHint = 1 ) const -> std::span<const int> {
		return findAreasInBox( absMins.Data(), absMaxs.Data(), areaNums, maxAreas, topNodeHint );
	}

	//returns the areas the bounding box is in
	[[nodiscard]]
	auto findAreasInBox( const vec3_t absMins, const vec3_t absMaxs, int *areaNums,
						 int maxAreas, int topNodeHint = 1 ) const -> std::span<const int>;

	[[nodiscard]]
	auto findTopNodeForBox( const float *boxMins, const float *boxMaxs ) const -> int;
	[[nodiscard]]
	auto findTopNodeForSphere( const float *center, float radius ) const -> int;

	//returns the area the point is in
	[[nodiscard]]
	auto pointAreaNumNaive( const vec3_t point, int topNodeHint = 1 ) const -> int;

	[[nodiscard]]
	auto pointAreaNum( const float *point ) const -> int;

	// If an area is not found, tries to adjust the origin a bit
	[[nodiscard]]
	auto findAreaNum( const Vec3 &origin ) const -> int {
		return findAreaNum( origin.Data() );
	}

	// If an area is not found, tries to adjust the origin a bit
	[[nodiscard]]
	auto findAreaNum( const vec3_t origin ) const -> int;
	// Tries to find some area the ent is in
	[[nodiscard]]
	auto findAreaNum( const struct edict_s *ent ) const -> int;

	//returns true if the area has one or more ground faces
	[[nodiscard]]
	bool isAreaGrounded( int areanum ) const {
		return ( m_areasettings[areanum].areaflags & AREA_GROUNDED ) != 0;
	}
	[[nodiscard]]
	bool isAreaATeleporter( int areanum ) const {
		return ( m_areasettings[areanum].contents & AREACONTENTS_TELEPORTER ) != 0;
	}
	//returns true if the area is donotenter
	[[nodiscard]]
	bool isAreaADoNotEnterArea( int areanum ) const {
		return ( m_areasettings[areanum].contents & AREACONTENTS_DONOTENTER ) != 0;
	}

	[[nodiscard]]
	auto getVertices() const -> std::span<const aas_vertex_t> {
		return { m_vertexes, m_vertexes + m_numvertexes };
	}
	[[nodiscard]]
	auto getPlanes() const -> std::span<const aas_plane_t> {
		return { m_planes, m_planes + m_numplanes };
	}
	[[nodiscard]]
	auto getEdges() const -> std::span<const aas_edge_t> {
		return { m_edges, m_edges + m_numedges };
	}
	[[nodiscard]]
	auto getEdgeIndex() const -> std::span<const aas_edgeindex_t> {
		return { m_edgeindex, m_edgeindex + m_edgeindexsize };
	}
	[[nodiscard]]
	auto getFaces() const -> std::span<const aas_face_t> {
		return { m_faces, m_faces + m_numfaces };
	}
	[[nodiscard]]
	auto getFaceIndex() const -> std::span<const aas_faceindex_t> {
		return { m_faceindex, m_faceindex + m_faceindexsize };
	}
	[[nodiscard]]
	auto getAreas() const -> std::span<const aas_area_t> {
		return { m_areas, m_areas + m_numareas };
	}
	[[nodiscard]]
	auto getAreaSettings() const -> std::span<const aas_areasettings_s> {
		return { m_areasettings, m_areasettings + m_numareasettings };
	}
	[[nodiscard]]
	auto getReaches() const -> std::span<const aas_reachability_t> {
		return { m_reachability, m_reachability + m_reachabilitysize };
	}
	[[nodiscard]]
	auto getNodes() const -> std::span<const aas_node_t> {
		return { m_nodes, m_nodes + m_numnodes };
	}
	[[nodiscard]]
	auto getPortals() const -> std::span<const aas_portal_t> {
		return { m_portals, m_portals + m_numportals };
	}
	[[nodiscard]]
	auto getPortalIndex() const -> std::span<const aas_portalindex_t> {
		return { m_portalindex, m_portalindex + m_portalindexsize };
	}
	[[nodiscard]]
	auto getClusters() const -> std::span<const aas_cluster_t> {
		return { m_clusters, m_clusters + m_numclusters };
	}

	// A feasible cluster num in non-zero
	[[nodiscard]]
	auto floorClusterNum( int areaNum ) const -> uint16_t {
		return m_loaded ? m_areaFloorClusterNums[areaNum] : (uint16_t)0;
	}

	[[nodiscard]]
	auto areaFloorClusterNums() const -> std::span<const uint16_t> {
		return { m_areaFloorClusterNums, m_areaFloorClusterNums + m_numareas };
	}

	// A feasible cluster num is non-zero
	[[nodiscard]]
	auto stairsClusterNum( int areaNum ) const -> uint16_t {
		return m_loaded ? m_areaStairsClusterNums[areaNum] : (uint16_t)0;
	}

	[[nodiscard]]
	auto areaStairsClusterNums() const -> std::span<const uint16_t> {
		return { m_areaStairsClusterNums, m_areaStairsClusterNums + m_numareas };
	}

	// In order to be conform with the rest of AAS code the zero cluster is dummy
	[[nodiscard]]
	auto floorClusterData( int floorClusterNum ) const -> std::span<const uint16_t> {
		assert( floorClusterNum >= 0 && floorClusterNum < m_numFloorClusters );
		return internalSpanToRegularSpan( m_floorClusterData + m_floorClusterDataOffsets[floorClusterNum] );
	}

	// In order to be conform with the rest of AAS code the zero cluster is dummy
	[[nodiscard]]
	auto stairsClusterData( int stairsClusterNum ) const -> std::span<const uint16_t> {
		assert( stairsClusterNum >= 0 && stairsClusterNum < m_numStairsClusters );
		return internalSpanToRegularSpan( m_stairsClusterData + m_stairsClusterDataOffsets[stairsClusterNum] );
	}

	/**
	 * Performs a 2D ray-casting in a floor cluster.
	 * A floor cluster is not usually a convex N-gon.
	 * A successful result means that there is a straight-walkable path between areas.
	 * @param startAreaNum a start area
	 * @param targetAreaNum a target area
	 * @return true if the segment between areas is fully inside the cluster boundaries.
	 */
	[[nodiscard]]
	bool isAreaWalkableInFloorCluster( int startAreaNum, int targetAreaNum ) const;

	[[nodiscard]]
	auto areaMapLeafsList( int areaNum ) const -> std::span<const int> {
		assert( (unsigned)areaNum < (unsigned)m_numareas );
		return internalSpanToRegularSpan( m_areaMapLeafsData + m_areaMapLeafListOffsets[areaNum] );
	}

	// Returns an address of 6 short values
	[[nodiscard]]
	auto getAreaInnerBounds( int areaNum ) const -> const int16_t * {
	    assert( (unsigned)areaNum < (unsigned)m_numareas );
	    return m_areaInnerBounds + 6 * areaNum;
	}

	/**
	 * Gets a list of grounded areas that are considered "principal" for routing
	 * and that should be tested for blocking by enemies for a proper bot behaviour.
	 * Not all principal areas are included in this list but only grounded ones
	 * as they are tested using a separate code path during determination of areas blocked status.
	 */
	[[nodiscard]]
	auto groundedPrincipalRoutingAreas() const -> std::span<const uint16_t> {
		return internalSpanToRegularSpan( m_groundedPrincipalRoutingAreas );
	}
	[[nodiscard]]
	auto jumppadReachPassThroughAreas() const -> std::span<const uint16_t> {
		return internalSpanToRegularSpan( m_jumppadReachPassThroughAreas );
	}
	[[nodiscard]]
	auto ladderReachPassThroughAreas() const -> std::span<const uint16_t> {
		return internalSpanToRegularSpan( m_ladderReachPassThroughAreas );
	}
	[[nodiscard]]
	auto elevatorReachPassThroughAreas() const -> std::span<const uint16_t> {
		return internalSpanToRegularSpan( m_elevatorReachPassThroughAreas );
	}
	[[nodiscard]]
	auto walkOffLedgePassThroughAirAreas() const -> std::span<const uint16_t> {
		return internalSpanToRegularSpan( m_walkOffLedgePassThroughAirAreas );
	}

	/**
	 * Retrieves a cached mutual floor cluster visibility result.
	 * Clusters are considered visible if some area in a cluster is visible from some other area in another cluster.
	 * @param clusterNum1 a number of first floor cluster
	 * @param clusterNum2 a number of second floor cluster
	 * @return true if supplied floor clusters are visible, false if the visibility test failed
	 * @note there could be false negatives as the visibility determination algorithm is probabilistic.
	 * That's what the "certainly" part stands for.
	 */
	[[nodiscard]]
	bool areFloorClustersCertainlyVisible( int clusterNum1, int clusterNum2 ) const {
		assert( (unsigned)clusterNum1 < (unsigned)m_numFloorClusters );
		assert( (unsigned)clusterNum2 < (unsigned)m_numFloorClusters );
		// Skip the dummy zero leaf
		return m_floorClustersVisTable[( clusterNum1 - 1 ) * ( m_numFloorClusters - 1 ) + clusterNum2 - 1];
	}

	/**
	 * Checks whether areas are in PVS.
	 * Areas are considered to be in PVS if some leaf that area occupies is visible from some other leaf of another area.
	 * @param areaNum1 a number of the first area
	 * @param areaNum2 a number of another area
	 * @return true if areas are in PVS.This test is precise (no false positives/negatives are produced).
	 * @note this is not that cheap to call. Prefer using {@code AreaVisList()} where possible.
	 */
	[[nodiscard]]
	bool areAreasInPvs( int areaNum1, int areaNum2 ) const;

	/**
	 * Returns a list of all areas that are certainly visible from the area.
	 * There could be false negatives but no false positives (in regard to a solid world).
	 * @param areaNum an area number
	 * @return a list of area numbers certainly visible from the area. The first element is the list length.
	 */
	[[nodiscard]]
	auto areaVisList( int areaNum ) const -> std::span<const uint16_t> {
		assert( (unsigned)areaNum < (unsigned)m_numareas );
		return internalSpanToRegularSpan( m_areaVisData + m_areaVisDataOffsets[areaNum] );
	}

	/**
	 * @see DecompressAreaVis(const uint16_t *__resrict, bool *__restrict)
	 * @param areaNum an number of an area
	 * @param row a buffer for a decompressed row
	 * @return an address of the supplied buffer.
	 */
	const bool *decompressAreaVis( int areaNum, bool *__restrict row ) const {
		return decompressAreaVis( areaVisList( areaNum ), row );
	}

	/**
	 * Converts a dense list of areas (certainly) visible from the area to a sparse row addressed by area numbers.
	 * @param visList a list of areas (a result of {@code AreaVisList()} call)
	 * @param row a buffer for a decompressed row
	 * @return an address of the supplied buffer
	 * @warning using this in a loop is not cheap. Consider using {@code FindInVisList()} in this case.
	 */
	bool *decompressAreaVis( std::span<const uint16_t> visList, bool *__restrict row ) const {
		::memset( row, 0, sizeof( bool ) * m_numareas );
		return addToDecompressedAreaVis( visList, row );
	}

	/**
	 * @see AddToDecompressedAreaVis(const uint16_t *__restrict, bool *__restrict)
	 * @param areaNum an number of an area
	 * @param row a buffer for a decompressed row
	 * @return an address of the supplied buffer.
	 */
	bool *addToDecompressedAreaVis( int areaNum, bool *__restrict row ) const {
		return addToDecompressedAreaVis( areaVisList( areaNum ), row );
	}

	/**
	 * Converts a dense list of areas (certainly) visible from the area to a sparse row addressed by area numbers.
	 * Contrary to {@code DecompressAreaVis()} the supplied buffer contents are not erased.
	 * This allows building a lookup table of areas visible from multiple POV areas.
	 * @param visList a list of areas (a result of {@code AreaVisList()} call)
	 * @param row a buffer for a decompressed row (a result of {@code DecompressAreaVis()} call for some other area)
	 * @return an address of the supplied buffer
	 */
	bool *addToDecompressedAreaVis( std::span<const uint16_t> visList, bool *__restrict row ) const;
};

#endif
