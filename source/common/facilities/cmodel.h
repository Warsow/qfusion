/*
   Copyright (C) 1997-2001 Id Software, Inc.

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

typedef struct cmodel_state_s cmodel_state_t;

// Hack! Prevent inclusion of this C++ prototypes (that are convenient) in C code (that is still present somewhere)
#ifdef __cplusplus

struct cmodel_s *CM_LoadMap( cmodel_state_t *cms, const char *name, bool clientload, unsigned *checksum );
struct cmodel_s *CM_InlineModel( cmodel_state_t *cms, int num ); // 1, 2, etc
char *CM_LoadMapMessage( char *name, char *message, int size );

int CM_NumClusters( const cmodel_state_t *cms );
int CM_NumAreas( const cmodel_state_t *cms );
int CM_NumInlineModels( const cmodel_state_t *cms );
char *CM_EntityString( cmodel_state_t *cms );
int CM_EntityStringLen( const cmodel_state_t *cms );
const char *CM_ShaderrefName( const cmodel_state_t *cms, int ref );
int CM_ShaderrefForName( const cmodel_state_t *cms, const char *name );

// creates a clipping hull for an arbitrary bounding box
struct cmodel_s *CM_ModelForBBox( cmodel_state_t *cms, const vec3_t mins, const vec3_t maxs );
struct cmodel_s *CM_OctagonModelForBBox( cmodel_state_t *cms, const vec3_t mins, const vec3_t maxs );

void CM_InlineModelBounds( const cmodel_state_t *cms, const struct cmodel_s *cmodel, vec3_t mins, vec3_t maxs );

/**
 * Given a box defined by bounds finds a best enclosing it BSP node.
 * (all box corners must belong to the node).
 * @param cms a collision model instance
 * @param mins the box mins
 * @param maxs the box maxs
 * @param maxValue the maximum numeric value of a node
 * (useful if a caller wants to store nodes in a compact form using few bits).
 * @return a number of the best enclosing top node.
 * @note the result is never negative (leaves handling at caller-side is error-prone).
 * @note this is not that cheap to call.
 * This call is most suitable for precomputing top nodes of boxes.
 * That's what {@code maxValue} parameter is for.
 * Aside from that this call is useful for getting a top node of a given bounds
 * and then performing collision calls that operate fully withing the bounds and accept the top node hint.
 */
int CM_FindTopNodeForBox( const cmodel_state_t *cms, const vec3_t mins, const vec3_t maxs, unsigned maxValue = ~( 0u ) );

/**
 * Given a sphere finds a best enclosing it BSP node.
 * @param cms a collision model instance
 * @param center a sphere center
 * @param radius a sphere radius
 * @param maxValue the maximum numeric value of a node
 * (useful if a caller wants to store nodes in a compact form using few bits).
 * @return a number of the best enclosing top node
 * @see CM_FindTopNodeForBox(cmodel_state_t *, const vec3_t, const vec3_t, unsigned).
 * @note This call is cheaper to execute than {@code CM_FindTopNodeForBox()}
 */
int CM_FindTopNodeForSphere( const cmodel_state_t *cms, const vec3_t center, float radius, unsigned maxValue = ~( 0u ) );

// returns an ORed contents mask
int CM_TransformedPointContents( const cmodel_state_t *cms, const vec3_t p,
								 const struct cmodel_s *cmodel, const vec3_t origin,
								 const vec3_t angles, int topNodeHint = 0 );

void CM_TransformedBoxTrace( const cmodel_state_t *cms, trace_t *tr,
							 const vec3_t start, const vec3_t end,
							 const vec3_t mins, const vec3_t maxs,
							 const struct cmodel_s *cmodel, int brushmask,
							 const vec3_t origin, const vec3_t angles,
							 int topNodeHint = 0 );

int CM_ClusterRowSize( const cmodel_state_t *cms );
int CM_AreaRowSize( const cmodel_state_t *cms );
int CM_PointLeafnum( const cmodel_state_t *cms, const vec3_t p, int topNodeHint = 0 );

// call with topnode set to the headnode, returns with topnode
// set to the first node that splits the box
int CM_BoxLeafnums( const cmodel_state_t *cms,
					const vec3_t mins, const vec3_t maxs,
					int *list, int listsize,
					int *topnode, int topNodeHint = 0 );

int CM_LeafCluster( const cmodel_state_t *cms, int leafnum );
int CM_LeafArea( const cmodel_state_t *cms, int leafnum );

void CM_SetAreaPortalState( cmodel_state_t *cms, int area1, int area2, bool open );
bool CM_AreasConnected( const cmodel_state_t *cms, int area1, int area2 );

int CM_WriteAreaBits( const cmodel_state_t *cms, uint8_t *buffer );
void CM_ReadAreaBits( cmodel_state_t *cms, uint8_t *buffer );
bool CM_HeadnodeVisible( const cmodel_state_t *cms, int headnode, const uint8_t *visbits );

void CM_WritePortalState( cmodel_state_t *cms, int file );
void CM_ReadPortalState( cmodel_state_t *cms, int file );

void CM_MergePVS( cmodel_state_t *cms, const vec3_t org, uint8_t *out );
void CM_MergePHS( cmodel_state_t *cms, int cluster, uint8_t *out );
int CM_MergeVisSets( cmodel_state_t *cms, const vec3_t org, uint8_t *pvs, uint8_t *areabits );

bool CM_InPVS( const cmodel_state_t *cms, const vec3_t p1, const vec3_t p2 );

bool CM_LeafsInPVS( const cmodel_state_t *cms, int leafnum1, int leafnum2 );

int CM_NumLeafs( const cmodel_state_t *cms );
const vec3_t *CM_GetLeafBounds( const cmodel_state_t *cms, int leafNum );

//
cmodel_state_t *CM_New();

void CM_AddReference( cmodel_state_t *cms );
void CM_ReleaseReference( cmodel_state_t *cms );

struct CMShapeList;

CMShapeList *CM_AllocShapeList( cmodel_state_t *cms );
void CM_FreeShapeList( cmodel_state_t *cms, CMShapeList *list );
CMShapeList *CM_BuildShapeList( cmodel_state_t *cms, CMShapeList *list, const float *mins, const float *maxs, int clipMask );
void CM_ClipShapeList( cmodel_state_t *cms, CMShapeList *, const CMShapeList *, const float *mins, const float *maxs );
void CM_ClipToShapeList( cmodel_state_t *cms, const CMShapeList *list, trace_t *tr,
						 const float *start, const float *end,
						 const float *mins, const float *maxs, int clipMask );

int CM_PossibleShapeListContents( const CMShapeList *list );
int CM_GetNumShapesInShapeList( const CMShapeList *list );

//
void CM_Init( void );
void CM_Shutdown( void );

#endif