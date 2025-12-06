/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (C) 2002-2013 Victor Luchits

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
#ifndef R_LOCAL_H
#define R_LOCAL_H

#include <common/helpers/q_arch.h>
#include <common/helpers/q_math.h>
#include <common/facilities/qfiles.h>
#include <common/facilities/bsp.h>
#include <common/helpers/patch.h>
#include <common/facilities/messagestreams.h>

#ifdef ALIGN
#undef ALIGN
#endif

#define ALIGN( x, a ) ( ( ( x ) + ( ( size_t )( a ) - 1 ) ) & ~( ( size_t )( a ) - 1 ) )

typedef struct mempool_s mempool_t;
typedef struct cinematics_s cinematics_t;
typedef struct qthread_s qthread_t;
typedef struct qmutex_s qmutex_t;
typedef struct qbufPipe_s qbufPipe_t;

typedef unsigned short elem_t;

typedef vec_t instancePoint_t[8]; // quaternion for rotation + xyz pos + uniform scale

#define NUM_CUSTOMCOLORS        16

#include "ref.h"
#include "refmath.h"
#include "vattribs.h"

typedef struct superLightStyle_s {
	vattribmask_t vattribs;
	int lightmapNum[MAX_LIGHTMAPS];
	int lightmapStyles[MAX_LIGHTMAPS];
	int vertexStyles[MAX_LIGHTMAPS];
	float stOffset[MAX_LIGHTMAPS][2];
} superLightStyle_t;

#include "glimp.h"

enum drawSurfaceType_t : unsigned {
	ST_NONE,
	ST_BSP,
	ST_ALIAS,
	ST_SKELETAL,
	ST_SPRITE,
	ST_QUAD_POLY,
	ST_DYNAMIC_MESH,
	ST_PARTICLE,
	ST_CORONA,
	ST_NULLMODEL,

	ST_MAX_TYPES,
};

struct DynamicMeshDrawSurface {
	// TODO: Do we really need to keep temporaries of algorithms here
	unsigned requestedNumVertices;
	unsigned requestedNumIndices;
	unsigned actualNumVertices;
	unsigned actualNumIndices;
	unsigned verticesOffset;
	unsigned indicesOffset;
	const struct DynamicMesh *dynamicMesh;
	char scratchpad[32];
};

struct MergedBspSurface {
	struct shader_s *shader;
	struct mfog_s *fog;
	struct MeshBuffer *buffer;
	struct superLightStyle_s *superLightStyle;
	instancePoint_t *instances;
	unsigned numVerts, numElems;
	unsigned firstVboVert, firstVboElem;
	unsigned firstWorldSurface, numWorldSurfaces;
	unsigned numInstances;
	unsigned numLightmaps;
};

struct MultiDrawElemSpan {
	const GLsizei *counts;
	const GLvoid **indices;
	GLsizei numDraws;
};
struct portalSurface_s;

typedef struct {
	MergedBspSurface *mergedBspSurf;
	portalSurface_s *portalSurface;
	MultiDrawElemSpan mdSpan;
	unsigned dlightBits;
	float portalDistance;
} drawSurfaceBSP_t;

typedef struct {
	struct maliasmesh_s *mesh;
	struct model_s *model;
} drawSurfaceAlias_t;

typedef struct {
	struct mskmesh_s *mesh;
	struct model_s *model;
} drawSurfaceSkeletal_t;

struct shader_s;
struct mfog_s;
struct portalSurface_s;

#define MIN_RENDER_MESHES           2048

typedef struct mesh_s {
	unsigned short numVerts;
	unsigned short numElems;

	elem_t              *elems;

	vec4_t              *xyzArray;
	vec4_t              *normalsArray;
	vec4_t              *sVectorsArray;
	vec2_t              *stArray;
	vec2_t              *lmstArray[MAX_LIGHTMAPS];
	byte_vec4_t         *lmlayersArray[( MAX_LIGHTMAPS + 3 ) / 4];
	byte_vec4_t         *colorsArray[MAX_LIGHTMAPS];

	uint8_t             *blendIndices;
	uint8_t             *blendWeights;
} mesh_t;

typedef struct {
	uint64_t sortKey;
	const void *drawSurf;
	unsigned distKey;
	unsigned surfType;
	// Currently works just in addition to common mergeability checks.
	unsigned mergeabilitySeparator;
} sortedDrawSurf_t;

struct FrontendToBackendShared;

#include "shader.h"

enum {
	RB_VBO_NONE = 0,
};

//===================================================================

struct shader_s;
struct mfog_s;
struct superLightStyle_s;
struct portalSurface_s;

class SimulatedBackendState;

// core
void RB_Init();
void RB_Shutdown();

void R_SetDefaultGLState();
void R_BindRenderTarget( RenderTargetComponents *components );

void RB_BeginUsingBackendState( SimulatedBackendState *backendState );
void RB_EndUsingBackendState( SimulatedBackendState *backendState );

enum : unsigned {
	UPLOAD_GROUP_DYNAMIC_MESH     = 0,
	UPLOAD_GROUP_BATCHED_MESH     = 1,
	// TODO: Redesign the interface of dynamic uploads, use better naming
	UPLOAD_GROUP_BATCHED_MESH_EXT = 2,
	// TODO: Use one text group per camera
	UPLOAD_GROUP_2D_MESH,
	// TODO: Use one debug group per camera - if we care of debug mesh submission performance
	UPLOAD_GROUP_DEBUG_MESH,
};

struct VboSpanLayout;
struct MeshBuffer;

const MeshBuffer *RB_VBOForFrameUploads( unsigned group );
const VboSpanLayout *RB_VBOSpanLayoutForFrameUploads( unsigned group );
unsigned RB_VboCapacityInVertexBytesForFrameUploads( unsigned group );
unsigned RB_VboCapacityInVerticesForFrameUploads( unsigned group );
unsigned RB_VboCapacityInIndexElemsForFrameUploads( unsigned group );

void R_BeginMeshUploads( unsigned group );
void R_SetUploadedSubdataFromMeshUsingOffsets( unsigned group, unsigned baseVertex, unsigned verticesOffsetInBytes,
											   unsigned indicesOffsetInBytes, const mesh_t *mesh );
void R_SetUploadedSubdataFromMeshUsingLayout( unsigned group, unsigned baseVertex, const VboSpanLayout *layout,
											  unsigned indexOfFirstIndex, const mesh_t *mesh );
void R_EndMeshUploads( unsigned group, unsigned vertexDataSizeInBytes, unsigned indexDataSizeInBytes );

#define MAX_UNIFORM_BINDINGS  ( 17 )

void *RB_GetTmpUniformBlock( SimulatedBackendState *backendState, unsigned binding, size_t requestedBlockSize );
void RB_CommitUniformBlock( SimulatedBackendState *backendState, unsigned binding, void *blockData, size_t blockSize );

void R_BeginUniformUploads( unsigned binding );
void R_EndUniformUploads( unsigned binding, unsigned uniformDataSizeInBytes );

struct VertElemSpan {
	unsigned firstVert;
	unsigned numVerts;
	unsigned firstElem;
	unsigned numElems;
};

using DrawMeshVertSpan = std::variant<VertElemSpan, MultiDrawElemSpan>;

/*
==============================================================================

BRUSH MODELS

==============================================================================
*/


//
// in memory representation
//
typedef struct {
	float radius;

	unsigned int firstModelSurface;
	unsigned int numModelSurfaces;

	unsigned int firstModelDrawSurface;
	unsigned int numModelDrawSurfaces;

	vec3_t mins, maxs;
} mmodel_t;

typedef struct mfog_s {
	shader_t        *shader;
	cplane_t        *visibleplane;
	vec3_t mins, maxs;
} mfog_t;

typedef struct mshaderref_s {
	char name[MAX_QPATH];
	int flags;
	int contents;
	shader_t        *shaders[NUM_SHADER_TYPES_BSP];
} mshaderref_t;

#define MAX_REF_SCENES   16 // max scenes rendered per frame
#define MAX_REF_CAMERAS  32 // max cameras rendered per frame

typedef struct msurface_s {

	vec4_t plane;

	float mins[8], maxs[8];

	unsigned facetype, flags;
	unsigned firstDrawSurfVert, firstDrawSurfElem;
	unsigned numInstances;
	unsigned mergedSurfNum;

	mutable unsigned traceFrame;

	mesh_t mesh;

	instancePoint_t *instances;

	shader_t *shader;
	mfog_t *fog;

	struct superLightStyle_s *superLightStyle;
} msurface_t;

typedef struct mnode_s {
	cplane_t        plane;

	// TODO: We can really use short indices!
	int32_t        children[2];
} mnode_t;

typedef struct mleaf_s {
	// leaf specific
	int cluster, area;

	float mins[8];
	float maxs[8];                      // for bounding box culling

	unsigned *visSurfaces;
	unsigned *fragmentSurfaces;

	// visSurfaces gets reassigned...
	void *surfDataToFree;

	unsigned numVisSurfaces;
	unsigned numFragmentSurfaces;
} mleaf_t;

typedef struct {
	uint8_t ambient[MAX_LIGHTMAPS][3];
	uint8_t diffuse[MAX_LIGHTMAPS][3];
	uint8_t styles[MAX_LIGHTMAPS];
	uint8_t direction[2];
} mgridlight_t;

typedef struct {
	int texNum;
	int texLayer;
	float texMatrix[2][2];
} mlightmapRect_t;

struct OccluderBoundsEntry {
	float mins[4], maxs[4];
};

struct OccluderDataEntry {
	unsigned numVertices;
	vec4_t plane;
	vec3_t innerPolyPoint;
	vec3_t data[7];
};

typedef struct mbrushmodel_s {
	const bspFormatDesc_t *format;

	dvis_t          *pvs;

	unsigned int numsubmodels;
	mmodel_t        *submodels;
	struct model_s  *inlines;

	unsigned int numModelSurfaces;
	unsigned int firstModelSurface;

	unsigned int numModelMergedSurfaces;
	unsigned int firstModelMergedSurface;

	msurface_t      *modelSurfaces;

	unsigned int numplanes;
	cplane_t        *planes;

	unsigned int numleafs;              // number of visible leafs, not counting 0
	mleaf_t         *leafs;
	mleaf_t         **visleafs;
	unsigned int numvisleafs;

	unsigned int numnodes;
	mnode_t         *nodes;

	unsigned int numsurfaces;
	msurface_t      *surfaces;

	unsigned int numlightgridelems;
	mgridlight_t    *lightgrid;

	unsigned int numlightarrayelems;
	int             *lightarray;

	unsigned int numfogs;
	mfog_t          *fogs;
	mfog_t          *globalfog;

	/*unsigned*/ int numareas;

	vec3_t gridSize;
	vec3_t gridMins;
	int gridBounds[4];

	unsigned int numMergedSurfaces;
	MergedBspSurface *mergedSurfaces;

	unsigned int numLightmapImages;
	Texture **lightmapImages;

	unsigned int numSuperLightStyles;
	struct superLightStyle_s *superLightStyles;

	OccluderBoundsEntry *occluderBoundsEntries;
	OccluderDataEntry *occluderDataEntries;
	unsigned numOccluders;

	struct shader_s *skyShader;
} mbrushmodel_t;

/*
==============================================================================

ALIAS MODELS

==============================================================================
*/

//
// in memory representation
//
typedef struct {
	short point[3];
	uint8_t latlong[2];                     // use bytes to keep 8-byte alignment
} maliasvertex_t;

typedef struct {
	vec3_t mins, maxs;
	vec3_t scale;
	vec3_t translate;
	float radius;
} maliasframe_t;

typedef struct {
	char name[MD3_MAX_PATH];
	quat_t quat;
	vec3_t origin;
} maliastag_t;

typedef struct {
	char name[MD3_MAX_PATH];
	shader_t        *shader;
} maliasskin_t;

typedef struct maliasmesh_s {
	char name[MD3_MAX_PATH];

	int numverts;
	maliasvertex_t *vertexes;
	vec2_t          *stArray;

	vec4_t          *xyzArray;
	vec4_t          *normalsArray;
	vec4_t          *sVectorsArray;

	int numtris;
	elem_t          *elems;

	int numskins;
	maliasskin_t    *skins;

	MeshBuffer *buffer;
} maliasmesh_t;

typedef struct maliasmodel_s {
	int numframes;
	maliasframe_t   *frames;

	int numtags;
	maliastag_t     *tags;

	int nummeshes;
	maliasmesh_t    *meshes;
	drawSurfaceAlias_t *drawSurfs;

	int numskins;
	maliasskin_t    *skins;

	int numverts;             // sum of numverts for all meshes
	int numtris;             // sum of numtris for all meshes
} maliasmodel_t;

/*
==============================================================================

SKELETAL MODELS

==============================================================================
*/

//
// in memory representation
//
#define SKM_MAX_WEIGHTS     4

//
// in memory representation
//
typedef struct {
	char            *name;
	shader_t        *shader;
} mskskin_t;

typedef struct {
	uint8_t indices[SKM_MAX_WEIGHTS];
	uint8_t weights[SKM_MAX_WEIGHTS];
} mskblend_t;

typedef struct mskmesh_s {
	char            *name;

	uint8_t         *blendIndices;
	uint8_t         *blendWeights;

	unsigned int numverts;
	vec4_t          *xyzArray;
	vec4_t          *normalsArray;
	vec2_t          *stArray;
	vec4_t          *sVectorsArray;

	unsigned int    *vertexBlends;  // [0..numbones-1] reference directly to bones
	// [numbones..numbones+numblendweights-1] reference to model blendweights

	unsigned int maxWeights;        // the maximum number of bones, affecting a single vertex in the mesh

	unsigned int numtris;
	elem_t          *elems;

	mskskin_t skin;

	struct MeshBuffer *buffer;
} mskmesh_t;

typedef struct {
	char            *name;
	signed int parent;
	unsigned int flags;
} mskbone_t;

typedef struct {
	vec3_t mins, maxs;
	float radius;
	bonepose_t      *boneposes;
} mskframe_t;

typedef struct mskmodel_s {
	unsigned int numbones;
	mskbone_t       *bones;

	unsigned int nummeshes;
	mskmesh_t       *meshes;
	drawSurfaceSkeletal_t *drawSurfs;

	unsigned int numtris;
	elem_t          *elems;

	unsigned int numverts;
	vec4_t          *xyzArray;
	vec4_t          *normalsArray;
	vec2_t          *stArray;
	vec4_t          *sVectorsArray;
	uint8_t         *blendIndices;
	uint8_t         *blendWeights;

	unsigned int numblends;
	mskblend_t      *blends;
	unsigned int    *vertexBlends;  // [0..numbones-1] reference directly to bones
	// [numbones..numbones+numblendweights-1] reference to blendweights

	unsigned int numframes;
	mskframe_t      *frames;
	bonepose_t      *invbaseposes;
	void            *stringsDataToFree;
} mskmodel_t;

//===================================================================

//
// Whole model
//

typedef enum { mod_bad = -1, mod_free, mod_brush, mod_alias, mod_skeletal } modtype_t;
typedef void ( *mod_touch_t )( struct model_s *model );

#define MOD_MAX_LODS    4

typedef struct model_s {
	char            *name;
	int registrationSequence;
	mod_touch_t touch;          // touching a model updates registration sequence, images and VBO's

	modtype_t type;

	//
	// volume occupied by the model graphics
	//
	vec3_t mins, maxs;
	float radius;

	//
	// memory representation pointer
	//
	void            *extradata;

	int lodnum;                 // LOD index, 0 for parent model, 1..MOD_MAX_LODS for LOD models
	int numlods;
	struct model_s  *lods[MOD_MAX_LODS];

	mempool_t       *mempool;
} model_t;

//============================================================================

extern model_t *r_prevworldmodel;

void        R_InitModels( void );
void        R_ShutdownModels( void );
void        R_FreeUnusedModels( void );

void        Mod_ClearAll( void );
model_t     *Mod_ForName( const char *name, bool crash );
mleaf_t     *Mod_PointInLeaf( float *p, model_t *model );
uint8_t     *Mod_ClusterPVS( int cluster, model_t *model );

void        Mod_StripLODSuffix( char *name );

//#include "program.h"

#ifdef CGAMEGETLIGHTORIGIN
#define SHADOW_MAPPING          2
#else
#define SHADOW_MAPPING          1
#endif

#define SUBDIVISIONS_MIN        3
#define SUBDIVISIONS_MAX        16
#define SUBDIVISIONS_DEFAULT    5
#define MAX_PORTAL_SURFACES     32
#define MAX_PORTAL_TEXTURES     64

#define NUM_BLOOM_LODS          4

#define BACKFACE_EPSILON        4

#define ON_EPSILON              0.1         // point on plane side epsilon

#define Z_NEAR                  4.0f
#define Z_BIAS                  64.0f

#define SIDE_FRONT              0
#define SIDE_BACK               1
#define SIDE_ON                 2

#define RF_BIT( x )               ( 1ULL << ( x ) )

#define RF_NONE                 0x0
#define RF_MIRRORVIEW           RF_BIT( 0 )
#define RF_PORTALVIEW           RF_BIT( 1 )
#define RF_ENVVIEW              RF_BIT( 2 )
#define RF_SHADOWMAPVIEW        RF_BIT( 3 )
#define RF_DRAWFLAT             RF_BIT( 5 )
#define RF_CLIPPLANE            RF_BIT( 6 )
#define RF_NOOCCLUSIONCULLING   RF_BIT( 7 )
#define RF_LIGHTMAP             RF_BIT( 8 )
#define RF_SOFT_PARTICLES       RF_BIT( 9 )
#define RF_PORTAL_CAPTURE       RF_BIT( 10 )
#define RF_SHADOWMAPVIEW_RGB    RF_BIT( 11 )
#define RF_DRAWBRIGHT           RF_BIT( 12 )

#define RF_CUBEMAPVIEW          ( RF_ENVVIEW )
#define RF_NONVIEWERREF         ( RF_PORTALVIEW | RF_MIRRORVIEW | RF_ENVVIEW | RF_SHADOWMAPVIEW )

#define MAX_REF_ENTITIES        ( MAX_ENTITIES + 48 ) // must not exceed 2048 because of sort key packing

typedef struct portalSurface_s {
	vec4_t mins, maxs;
	const entity_t *entity;
	cplane_t plane, untransformed_plane;
	const shader_t *shader;
	Texture *texures[2];            // front and back portalmaps
	void *statesForCamera[2];
} portalSurface_t;

//====================================================

// globals shared by the frontend and the backend
// the backend should never attempt modifying any of these
typedef struct {
	// any asset (model, shader, texture, etc) with has not been registered
	// or "touched" during the last registration sequence will be freed
	int registrationSequence;
	bool registrationOpen;

	// bumped each time R_RegisterWorldModel is called
	int worldModelSequence;

	float sinTableByte[256];

	model_t         *worldModel;
	mbrushmodel_t   *worldBrushModel;

	struct MeshBuffer *nullVBO;
	struct MeshBuffer *postProcessingVBO;

	vec3_t wallColor, floorColor;

	shader_t        *envShader;
	shader_t        *whiteShader;
	shader_t        *emptyFogShader;

	byte_vec4_t customColors[NUM_CUSTOMCOLORS];
} r_shared_t;

// global frontend variables are stored here
// the backend should never attempt reading or modifying them
typedef struct {
	bool in2D;
	int width2D, height2D;

	int frameBufferWidth, frameBufferHeight;

	int swapInterval;

	struct {
		unsigned average;        // updates 4 times per second
		int64_t time, oldTime;
		unsigned count, oldCount;
	} frameTime;

	volatile bool dataSync;   // call R_Finish

	char speedsMsg[2048];
	msurface_t      *debugSurface;
} r_globals_t;

extern r_shared_t rsh;
extern r_globals_t rf;

extern lightstyle_t lightStyles[MAX_LIGHTSTYLES];

constexpr const unsigned kWorldEntNumber = 0;

#include <common/facilities/configvars.h>

extern BoolConfigVar v_fullbright, v_lightmap, v_drawEntities, v_drawWorld, v_lerpModels, v_drawElements;
extern IntConfigVar v_showTris;

extern BoolConfigVar v_detailTextures;

extern IntConfigVar v_dynamicLight, v_subdivisions;

extern BoolConfigVar v_fastSky, v_portalOnly, v_portalMaps;
extern IntConfigVar v_portalMaps_maxTexSize;

extern BoolConfigVar v_lighting_deluxeMapping, v_lighting_specular, v_lighting_vertexLight;
extern BoolConfigVar v_lighting_packLightmaps, v_lighting_grayscale;
extern IntConfigVar v_lighting_maxLmBlockSize;
extern FloatConfigVar v_lighting_glossIntensity, v_lighting_glossExponent;
extern FloatConfigVar v_lighting_ambientScale, v_lighting_directedScale;
extern FloatConfigVar v_lighting_intensity;
extern IntConfigVar v_lighting_maxGlslDlights;

extern IntConfigVar v_offsetMapping;
extern FloatConfigVar v_offsetMapping_scale;
extern BoolConfigVar v_offsetMapping_reliefMapping;

extern FloatConfigVar v_outlinesWorld, v_outlinesScale, v_outlinesCutoff;

extern BoolConfigVar v_softParticles;
extern FloatConfigVar v_softParticles_scale;

extern BoolConfigVar v_hdr;
extern FloatConfigVar v_hdrGamma, v_hdrExposure;

extern IntConfigVar v_lodBias;
extern FloatConfigVar v_lodScale;

extern FloatConfigVar v_gamma;

extern StringConfigVar v_textureFilter;
extern IntConfigVar v_anisoLevel;
extern BoolConfigVar v_textureCompression;

// TODO: should we allow configuring it?
extern IntConfigVar v_stencilBits;

extern BoolConfigVar v_drawFlat;
extern ColorConfigVar v_wallColor, v_floorColor;

extern IntConfigVar v_swapInterval;

extern BoolConfigVar v_showShaderCache;

void R_NormToLatLong( const vec_t *normal, uint8_t latlong[2] );
void R_LatLongToNorm( const uint8_t latlong[2], vec3_t out );
void R_LatLongToNorm4( const uint8_t latlong[2], vec4_t out );

#define R_LinearFloatFromsRGBFloat( c ) ( ( ( c ) <= 0.04045f ) ? ( c ) * ( 1.0f / 12.92f ) : (float)pow( ( ( c ) + 0.055f ) * ( 1.0f / 1.055f ), 2.4f ) )
#define R_sRGBFloatFromLinearFloat( c ) ( ( ( c ) < 0.0031308f ) ? ( c ) * 12.92f : 1.055f * (float)pow( ( c ), 1.0f / 2.4f ) - 0.055f )
#define R_LinearFloatFromsRGB( c ) Image_LinearFloatFromsRGBFloat( ( c ) * ( 1.0f / 255.0f ) )
#define R_sRGBFloatFromLinear( c ) Image_sRGBFloatFromLinearFloat( ( c ) * ( 1.0f / 255.0f ) )

#define kQuadIndicesInitializer { 0, 1, 2, 0, 2, 3 }
constexpr const uint16_t kQuadIndices[6] = kQuadIndicesInitializer;
// TODO: Coords of "QuadPoly" appear to be different
constexpr const float kQuadTexCoords[4][2] = { { 0.0f, 1.0f }, { 0.0f, 0.0f }, { 1.0f, 0.0f }, { 1.0f, 1.0f } };

//
// r_alias.c
//
model_t *R_AliasModelLOD( const entity_t *e, const float *viewOrigin, float fovLodScale, float viewLodScale );
void R_AliasModelLerpBBox( const entity_t *e, const model_t *mod, vec3_t mins, vec3_t maxs );
bool R_AliasModelLerpTag( orientation_t *orient, const maliasmodel_t *aliasmodel, int framenum, int oldframenum,
							 float lerpfrac, const char *name );
void        R_AliasModelFrameBounds( const model_t *mod, int frame, vec3_t mins, vec3_t maxs );


//
// r_light.c
//
#define MAX_SUPER_STYLES    128

void        R_LightForOrigin( const vec3_t origin, vec3_t dir, vec4_t ambient, vec4_t diffuse, float radius, bool noWorldLight );
void        R_InitLightStyles( model_t *mod );
superLightStyle_t   *R_AddSuperLightStyle( model_t *mod, const int *lightmaps,
										   const uint8_t *lightmapStyles, const uint8_t *vertexStyles, mlightmapRect_t **lmRects );
void        R_SortSuperLightStyles( model_t *mod );
void        R_TouchLightmapImages( model_t *mod );

//
// r_main.c
//

int         R_LoadFile_( const char *path, int flags, void **buffer, const char *filename, int fileline );
void        R_FreeFile_( void *buffer, const char *filename, int fileline );

#define     R_LoadFile( path,buffer ) R_LoadFile_( path,0,buffer,__FILE__,__LINE__ )
#define     R_LoadCacheFile( path,buffer ) R_LoadFile_( path,FS_CACHE,buffer,__FILE__,__LINE__ )
#define     R_FreeFile( buffer ) R_FreeFile_( buffer,__FILE__,__LINE__ )

void        R_BeginFrame( bool forceClear, int swapInterval );
void        R_EndFrame( void );
int         R_SetSwapInterval( int swapInterval, int oldSwapInterval );
void        R_SetGamma( float gamma );
void        R_SetWallFloorColors( int wallColor, int floorColor );
void        R_Flush( void );

int         R_LODForSphere( const vec3_t origin, float radius, const float *viewOrigin, float fovLodScale, float viewLodScale );

void        R_InitCustomColors( void );
int         R_GetCustomColor( int num );
void        R_ShutdownCustomColors( void );

void R_BuildTangentVectors( int numVertexes, vec4_t *xyzArray, vec4_t *normalsArray, vec2_t *stArray,
							int numTris, elem_t *elems, vec4_t *sVectorsArray );

struct ParticleDrawSurface { uint16_t aggregateIndex; uint16_t particleIndex; };



struct FrontendToBackendShared {
	const Scene::DynamicLight *dynamicLights;
	const Scene::ParticlesAggregate *particleAggregates;
	std::span<const uint16_t> visibleProgramLightIndices;
	std::span<const uint16_t> allVisibleLightIndices;
	const VertElemSpan *batchedVertElemSpans;
	const std::pair<VertElemSpan, VboSpanLayout> *batchedVertElemAndVboSpans;
	// TODO: How do i supply mesh
	vec3_t viewOrigin;
	mat3_t viewAxis;
	unsigned renderFlags;
	unsigned cameraId;
	unsigned sceneIndex;
};

//
// r_poly.c
//
bool    R_SurfPotentiallyFragmented( const msurface_t *surf );

//
// r_register.c
//

void        R_ModelBounds( const model_s *model, vec3_t mins, vec3_t maxs );
void        R_ModelFrameBounds( const model_s *model, int frame, vec3_t mins, vec3_t maxs );
void        R_RegisterWorldModel( const char *model );
model_s    *R_RegisterModel( const char *name );

int         R_SkeletalGetBoneInfo( const model_s *mod, int bonenum, char *name, size_t name_size, int *flags );
void        R_SkeletalGetBonePose( const model_s *mod, int bonenum, int frame, bonepose_t *bonepose );
int         R_SkeletalGetNumBones( const model_s *mod, int *numFrames );

shader_s    *R_RegisterShader( const char *name, int type );
shader_s    *R_RegisterPic( const char *name );
shader_s    *R_RegisterRawAlphaMask( const char *name, int width, int height, const uint8_t *data );
shader_s    *R_RegisterSkin( const char *name );
shader_s    *R_RegisterLinearPic( const char *name );

void        R_ReplaceRawSubPic( shader_s *shader, int x, int y, int width, int height, const uint8_t *data );

Skin *R_RegisterSkinFile( const char *name );

shader_s *R_CreateExplicitlyManaged2DMaterial();
void R_ReleaseExplicitlyManaged2DMaterial( shader_s *material );
bool R_UpdateExplicitlyManaged2DMaterialImage( shader_s *material, const char *name, const ImageOptions &options );

[[nodiscard]]
auto R_GetShaderDimensions( const shader_s *shader ) -> std::optional<std::pair<unsigned, unsigned>>;

void        R_BeginRegistration_();
void        R_EndRegistration_();

// By default, we reserve a core for the sound backend.
// Also, a core is always implicitly reserved for the main thread.
struct SuggestNumWorkerThreadsArgs { unsigned numExcludedCores { 1 }; };
[[nodiscard]]
auto suggestNumExtraWorkerThreads( const SuggestNumWorkerThreadsArgs &args ) -> unsigned;

void        R_Shutdown_( bool verbose );

void R_BrushModelBBox( const entity_t *e, vec3_t mins, vec3_t maxs, bool *rotated = nullptr );

struct skmcacheentry_s;

//
// r_skm.c
//
void R_AddSkeletalModelCache( const entity_t *e, const model_t *mod, unsigned sceneIndex );
model_t *R_SkeletalModelLOD( const entity_t *e, const float *viewOrigin, float fovLodScale, float viewLodScale );
skmcacheentry_s *R_GetSkeletalCache( int entNum, int lodNum, unsigned sceneIndex );
dualquat_t *R_GetSkeletalBones( skmcacheentry_s *cache );
bool R_SkeletalRenderAsFrame0( skmcacheentry_s *cache );
void R_SkeletalModelFrameBounds( const model_t *mod, int frame, vec3_t mins, vec3_t maxs );
void R_SkeletalModelLerpBBox( const entity_t *e, const model_t *mod, vec3_t mins, vec3_t maxs );
bool R_SkeletalModelLerpTag( orientation_t *orient, const mskmodel_t *skmodel, int oldframenum, int framenum, float lerpfrac, const char *name );

void        R_InitSkeletalCache( void );
void        R_ClearSkeletalCache( void );
void        R_ShutdownSkeletalCache( void );

msurface_t *R_TransformedTraceLine( VisualTrace *tr, const vec3_t start, const vec3_t end, const model_s *model,
									const float *modelOrigin, const float *modelAxis, int surfumask );

//
// r_vbo.c
//

typedef enum {
	VBO_TAG_NONE,
	VBO_TAG_WORLD,
	VBO_TAG_MODEL,
	VBO_TAG_STREAM
} vbo_tag_t;

typedef struct {
	int overbrightBits;                     // map specific overbright bits

	float ambient[3];
	byte_vec4_t outlineColor;
	byte_vec4_t environmentColor;
	float averageLightingIntensity;

	bool lightmapsPacking;
	bool lightmapArrays;                    // true if using array textures for lightmaps
	int maxLightmapSize;                    // biggest dimension of the largest lightmap
	bool deluxeMaps;                        // true if there are valid deluxemaps in the .bsp
	bool deluxeMappingEnabled;              // true if deluxeMaps is true and r_lighting_deluxemaps->integer != 0

	bool forceClear;

	bool skipSky;

	bool forceWorldOutlines;
} mapconfig_t;

extern mapconfig_t mapConfig;

struct VisualTrace;

[[nodiscard]]
inline bool startsWithUtf8Bom( const char *data, size_t length ) {
	if( length > 2 ) {
		// The data must be cast to an unsigned type first, otherwise a comparison gets elided by a compiler
		const auto *p = (const uint8_t *)data;
		if( ( p[0] == 0xEFu ) & ( p[1] == 0xBBu ) & ( p[2] == 0xBFu ) ) {
			return true;
		}
	}
	return false;
}

// TODO: This is arch-dependent...
[[nodiscard]]
wsw_forceinline bool doOverlapTestFor14Dops( const float *mins1, const float *maxs1, const float *mins2, const float *maxs2 ) {
	// TODO: Inline into call sites/reduce redundant loads

	const __m128 xmmMins1_03 = _mm_loadu_ps( mins1 + 0 ), xmmMaxs1_03 = _mm_loadu_ps( maxs1 + 0 );
	const __m128 xmmMins2_03 = _mm_loadu_ps( mins2 + 0 ), xmmMaxs2_03 = _mm_loadu_ps( maxs2 + 0 );
	const __m128 xmmMins1_47 = _mm_loadu_ps( mins1 + 4 ), xmmMaxs1_47 = _mm_loadu_ps( maxs1 + 4 );
	const __m128 xmmMins2_47 = _mm_loadu_ps( mins2 + 4 ), xmmMaxs2_47 = _mm_loadu_ps( maxs2 + 4 );

	// Mins1 [0..3] > Maxs2[0..3]
	__m128 cmp1 = _mm_cmpgt_ps( xmmMins1_03, xmmMaxs2_03 );
	// Mins2 [0..3] > Maxs1[0..3]
	__m128 cmp2 = _mm_cmpgt_ps( xmmMins2_03, xmmMaxs1_03 );
	// Mins1 [4..7] > Maxs2[4..7]
	__m128 cmp3 = _mm_cmpgt_ps( xmmMins1_47, xmmMaxs2_47 );
	// Mins2 [4..7] > Maxs1[4..7]
	__m128 cmp4 = _mm_cmpgt_ps( xmmMins2_47, xmmMaxs1_47 );
	// Zero if no comparison was successful
	return _mm_movemask_ps( _mm_or_ps( _mm_or_ps( cmp1, cmp2 ), _mm_or_ps( cmp3, cmp4 ) ) ) == 0;
}

[[nodiscard]]
auto findLightsThatAffectBounds( const Scene::DynamicLight *lights, std::span<const uint16_t> lightIndicesSpan,
								 const float *mins, const float *maxs, uint16_t *affectingLightIndices ) -> unsigned;

constexpr uint64_t kSortKeyFogOffset = 0;
constexpr uint64_t kSortKeyFogLength = 5;

constexpr uint64_t kSortKeyPortalOffset = kSortKeyFogOffset + kSortKeyFogLength;
constexpr uint64_t kSortKeyPortalLength = 5;

constexpr uint64_t kSortKeyParamsOffset = kSortKeyPortalOffset + kSortKeyPortalLength;
constexpr uint64_t kSortKeyParamsLength = 15;

constexpr uint64_t kSortKeyEntityOffset = kSortKeyParamsOffset + kSortKeyParamsLength;
constexpr uint64_t kSortKeyEntityLength = 11;

constexpr uint64_t kSortKeyShaderOffset = kSortKeyEntityOffset + kSortKeyEntityLength;
constexpr uint64_t kSortKeyShaderLength = 11;

inline uint64_t R_PackSortKey( unsigned shaderNum, int fogNum, int portalNum, unsigned entNum, int paramsNum ) {
	assert( shaderNum < ( 1 << kSortKeyShaderLength ) );
	assert( fogNum >= -1 && fogNum + 1 < (int)( 1 << kSortKeyFogLength ) );
	assert( portalNum >= -1 && portalNum + 1 < (int)( 1 << kSortKeyPortalLength ) );
	assert( entNum < ( 1 << kSortKeyEntityLength ) );
	assert( paramsNum >= -1 && paramsNum + 1 < (int)( 1 << kSortKeyParamsLength ) );

	return ( ( (uint64_t)shaderNum ) << kSortKeyShaderOffset ) |
		   ( ( (uint64_t)entNum ) << kSortKeyEntityOffset ) |
		   ( ( (uint64_t)( paramsNum + 1 ) ) << kSortKeyParamsOffset ) |
		   ( ( (uint64_t)( portalNum + 1 ) ) << kSortKeyPortalOffset ) |
		   ( ( (uint64_t)( fogNum + 1 ) ) << kSortKeyFogOffset );
}

inline void R_UnpackSortKey( uint64_t sortKey, unsigned *shaderNum, int *fogNum,
							 int *portalNum, unsigned *entNum, int *paramsNum ) {


	*shaderNum = ( sortKey >> kSortKeyShaderOffset ) & ( ( 1 << kSortKeyShaderLength ) - 1 );
	*entNum    = ( sortKey >> kSortKeyEntityOffset ) & ( ( 1 << kSortKeyEntityLength ) - 1 );
	*paramsNum = -1 + (int)( ( sortKey >> kSortKeyParamsOffset ) & ( ( 1 << kSortKeyParamsLength ) - 1 ) );
	*portalNum = -1 + (int)( ( sortKey >> kSortKeyPortalOffset ) & ( ( 1 << kSortKeyPortalLength ) - 1 ) );
	*fogNum    = -1 + (int)( ( sortKey >> kSortKeyFogOffset )    & ( ( 1 << kSortKeyFogLength ) - 1 ) );
}

#define rDebug()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Debug ) ).getWriter()
#define rNotice()  wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Notice ) ).getWriter()
#define rWarning() wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Warning ) ).getWriter()
#define rError()   wsw::PendingRegularMessage( wsw::createRegularMessageStream( wsw::MessageDomain::Renderer, wsw::MessageCategory::Error ) ).getWriter()

#endif // R_LOCAL_H
