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
// cmodel_trace.c

#include "cmodel.h"
#include "cm_local.h"
#include "cm_trace.h"

#include <cstring>

#ifdef CM_SELF_TEST
#include <common/helpers/algorithm.h>
#endif

static inline void CM_SetBuiltinBrushBounds( vec_bounds_t mins, vec_bounds_t maxs ) {
	for( int i = 0; i < (int)( sizeof( vec_bounds_t ) / sizeof( vec_t ) ); ++i ) {
		mins[i] = +999999;
		maxs[i] = -999999;
	}
}

static_assert( sizeof( GenericOps ) <= 16 && sizeof( Sse42Ops ) <= 16 && sizeof( AvxOps ) <= 16 );
static_assert( alignof( GenericOps ) <= alignof( void * ) && alignof( Sse42Ops ) <= alignof( void * ) && alignof( AvxOps ) <= alignof( void * ) );

/*
* CM_InitBoxHull
*
* Set up the planes so that the six floats of a bounding box
* can just be stored out and get a proper clipping hull structure.
*/
void CM_InitBoxHull( cmodel_state_t *cms ) {
	cms->box_brush->numsides = 6;
	cms->box_brush->brushsides = cms->box_brushsides;
	cms->box_brush->contents = CONTENTS_BODY;

	// Make sure CM_CollideBox() will not reject the brush by its bounds
	CM_SetBuiltinBrushBounds( cms->box_brush->maxs, cms->box_brush->mins );

	cms->box_markbrushes[0] = cms->box_brush;

	cms->box_cmodel->builtin = true;
	cms->box_cmodel->numfaces = 0;
	cms->box_cmodel->faces = NULL;
	cms->box_cmodel->brushes = cms->box_brush;
	cms->box_cmodel->numbrushes = 1;

	for( int i = 0; i < 6; i++ ) {
		// brush sides
		cbrushside_t *s = cms->box_brushsides + i;

		// planes
		cplane_t tmp, *p = &tmp;
		VectorClear( p->normal );

		if( ( i & 1 ) ) {
			p->type = PLANE_NONAXIAL;
			p->normal[i >> 1] = -1;
			p->signbits = ( 1 << ( i >> 1 ) );
		} else {
			p->type = i >> 1;
			p->normal[i >> 1] = 1;
			p->signbits = 0;
		}

		p->dist = 0;
		CM_CopyRawToCMPlane( p, &s->plane );
		s->surfFlags = 0;
	}
}

/*
* CM_InitOctagonHull
*
* Set up the planes so that the six floats of a bounding box
* can just be stored out and get a proper clipping hull structure.
*/
void CM_InitOctagonHull( cmodel_state_t *cms ) {
	const vec3_t oct_dirs[4] = {
		{  1,  1, 0 },
		{ -1,  1, 0 },
		{ -1, -1, 0 },
		{  1, -1, 0 }
	};

	cms->oct_brush->numsides = 10;
	cms->oct_brush->brushsides = cms->oct_brushsides;
	cms->oct_brush->contents = CONTENTS_BODY;

	// Make sure CM_CollideBox() will not reject the brush by its bounds
	CM_SetBuiltinBrushBounds( cms->oct_brush->maxs, cms->oct_brush->mins );

	cms->oct_markbrushes[0] = cms->oct_brush;

	cms->oct_cmodel->builtin = true;
	cms->oct_cmodel->numfaces = 0;
	cms->oct_cmodel->faces = NULL;
	cms->oct_cmodel->brushes = cms->oct_brush;
	cms->oct_cmodel->numbrushes = 1;

	// axial planes
	for( int i = 0; i < 6; i++ ) {
		// brush sides
		cbrushside_t *s = cms->oct_brushsides + i;

		// planes
		cplane_t tmp, *p = &tmp;
		VectorClear( p->normal );

		if( ( i & 1 ) ) {
			p->type = PLANE_NONAXIAL;
			p->normal[i >> 1] = -1;
			p->signbits = ( 1 << ( i >> 1 ) );
		} else {
			p->type = i >> 1;
			p->normal[i >> 1] = 1;
			p->signbits = 0;
		}

		p->dist = 0;
		CM_CopyRawToCMPlane( p, &s->plane );
		s->surfFlags = 0;
	}

	// non-axial planes
	for( int i = 6; i < 10; i++ ) {
		// brush sides
		cbrushside_t *s = cms->oct_brushsides + i;

		// planes
		cplane_t tmp, *p = &tmp;
		VectorCopy( oct_dirs[i - 6], p->normal );

		p->type = PLANE_NONAXIAL;
		p->signbits = SignbitsForPlane( p );

		p->dist = 0;
		CM_CopyRawToCMPlane( p, &s->plane );
		s->surfFlags = 0;
	}
}

/*
* CM_ModelForBBox
*
* To keep everything totally uniform, bounding boxes are turned into inline models
*/
cmodel_t *CM_ModelForBBox( cmodel_state_t *cms, const vec3_t mins, const vec3_t maxs ) {
	cbrushside_t *sides = cms->box_brush->brushsides;
	sides[0].plane.dist = maxs[0];
	sides[1].plane.dist = -mins[0];
	sides[2].plane.dist = maxs[1];
	sides[3].plane.dist = -mins[1];
	sides[4].plane.dist = maxs[2];
	sides[5].plane.dist = -mins[2];

	VectorCopy( mins, cms->box_cmodel->mins );
	VectorCopy( maxs, cms->box_cmodel->maxs );

	return cms->box_cmodel;
}

/*
* CM_OctagonModelForBBox
*
* Same as CM_ModelForBBox with 4 additional planes at corners.
* Internally offset to be symmetric on all sides.
*/
cmodel_t *CM_OctagonModelForBBox( cmodel_state_t *cms, const vec3_t mins, const vec3_t maxs ) {
	int i;
	float a, b, d, t;
	float sina, cosa;
	vec3_t offset, size[2];

	for( i = 0; i < 3; i++ ) {
		offset[i] = ( mins[i] + maxs[i] ) * 0.5;
		size[0][i] = mins[i] - offset[i];
		size[1][i] = maxs[i] - offset[i];
	}

	VectorCopy( offset, cms->oct_cmodel->cyl_offset );
	VectorCopy( size[0], cms->oct_cmodel->mins );
	VectorCopy( size[1], cms->oct_cmodel->maxs );

	cbrushside_t *sides = cms->oct_brush->brushsides;
	sides[0].plane.dist = size[1][0];
	sides[1].plane.dist = -size[0][0];
	sides[2].plane.dist = size[1][1];
	sides[3].plane.dist = -size[0][1];
	sides[4].plane.dist = size[1][2];
	sides[5].plane.dist = -size[0][2];

	a = size[1][0]; // halfx
	b = size[1][1]; // halfy
	d = sqrt( a * a + b * b ); // hypothenuse

	cosa = a / d;
	sina = b / d;

	// swap sin and cos, which is the same thing as adding pi/2 radians to the original angle
	t = sina;
	sina = cosa;
	cosa = t;

	// elleptical radius
	d = a * b / sqrt( a * a * cosa * cosa + b * b * sina * sina );
	//d = a * b / sqrt( a * a  + b * b ); // produces a rectangle, inscribed at middle points

	// the following should match normals and signbits set in CM_InitOctagonHull

	VectorSet( sides[6].plane.normal, cosa, sina, 0 );
	sides[6].plane.dist = d;

	VectorSet( sides[7].plane.normal, -cosa, sina, 0 );
	sides[7].plane.dist = d;

	VectorSet( sides[8].plane.normal, -cosa, -sina, 0 );
	sides[8].plane.dist = d;

	VectorSet( sides[9].plane.normal, cosa, -sina, 0 );
	sides[9].plane.dist = d;

	return cms->oct_cmodel;
}

void Ops::ClipBoxToBrush( CMTraceContext *tlc, const cbrush_t *brush ) {
	cm_plane_t *clipplane;
	cbrushside_t *side, *leadside;
	float d1, d2, f;

	if( !brush->numsides ) {
		return;
	}

	float enterfrac = -1;
	float leavefrac = 1;
	clipplane = NULL;

	bool getout = false;
	bool startout = false;
	leadside = NULL;
	side = brush->brushsides;

	const float *startmins = tlc->startmins;
	const float *endmins = tlc->endmins;

	const float *endmaxs = tlc->endmaxs;
	const float *startmaxs = tlc->startmaxs;

	for( int i = 0, end = brush->numsides; i < end; i++, side++ ) {
		cm_plane_t *p = &side->plane;
		int type = p->type;
		float dist = p->dist;

		// push the plane out apropriately for mins/maxs
		if( type < 3 ) {
			d1 = startmins[type] - dist;
			d2 = endmins[type] - dist;
		} else {
			// It has been proven that using a switch is cheaper than using a LUT like in SIMD approach
			const float *normal = p->normal;
			switch( p->signbits & 7 ) {
				case 0:
					d1 = normal[0] * startmins[0] + normal[1] * startmins[1] + normal[2] * startmins[2] - dist;
					d2 = normal[0] * endmins[0] + normal[1] * endmins[1] + normal[2] * endmins[2] - dist;
					break;
				case 1:
					d1 = normal[0] * startmaxs[0] + normal[1] * startmins[1] + normal[2] * startmins[2] - dist;
					d2 = normal[0] * endmaxs[0] + normal[1] * endmins[1] + normal[2] * endmins[2] - dist;
					break;
				case 2:
					d1 = normal[0] * startmins[0] + normal[1] * startmaxs[1] + normal[2] * startmins[2] - dist;
					d2 = normal[0] * endmins[0] + normal[1] * endmaxs[1] + normal[2] * endmins[2] - dist;
					break;
				case 3:
					d1 = normal[0] * startmaxs[0] + normal[1] * startmaxs[1] + normal[2] * startmins[2] - dist;
					d2 = normal[0] * endmaxs[0] + normal[1] * endmaxs[1] + normal[2] * endmins[2] - dist;
					break;
				case 4:
					d1 = normal[0] * startmins[0] + normal[1] * startmins[1] + normal[2] * startmaxs[2] - dist;
					d2 = normal[0] * endmins[0] + normal[1] * endmins[1] + normal[2] * endmaxs[2] - dist;
					break;
				case 5:
					d1 = normal[0] * startmaxs[0] + normal[1] * startmins[1] + normal[2] * startmaxs[2] - dist;
					d2 = normal[0] * endmaxs[0] + normal[1] * endmins[1] + normal[2] * endmaxs[2] - dist;
					break;
				case 6:
					d1 = normal[0] * startmins[0] + normal[1] * startmaxs[1] + normal[2] * startmaxs[2] - dist;
					d2 = normal[0] * endmins[0] + normal[1] * endmaxs[1] + normal[2] * endmaxs[2] - dist;
					break;
				case 7:
					d1 = normal[0] * startmaxs[0] + normal[1] * startmaxs[1] + normal[2] * startmaxs[2] - dist;
					d2 = normal[0] * endmaxs[0] + normal[1] * endmaxs[1] + normal[2] * endmaxs[2] - dist;
					break;
				default:
					d1 = d2 = 0; // shut up compiler
					assert( 0 );
					break;
			}
		}

		if( d2 > 0 ) {
			getout = true; // endpoint is not in solid
		}
		if( d1 > 0 ) {
			startout = true;
		}

		// if completely in front of face, no intersection
		if( d1 > 0 && d2 >= d1 ) {
			return;
		}
		if( d1 <= 0 && d2 <= 0 ) {
			continue;
		}
		// crosses face
		f = d1 - d2;
		if( f > 0 ) {   // enter
			// conform to SIMD versions by using 1.0f / f, not dividing directly
			const float rcpF = 1.0f / f;
			f = ( d1 - DIST_EPSILON ) * rcpF;
			if( f > enterfrac ) {
				enterfrac = f;
				clipplane = p;
				leadside = side;
			}
		} else if( f < 0 ) {   // leave
			const float rcpF = 1.0f / f;
			f = ( d1 + DIST_EPSILON ) * rcpF;
			if( f < leavefrac ) {
				leavefrac = f;
			}
		}
	}

	if( !startout ) {
		// original point was inside brush
		tlc->trace->startsolid = true;
		tlc->trace->contents = brush->contents;
		if( !getout ) {
			tlc->trace->allsolid = true;
			tlc->trace->fraction = 0;
		}
		return;
	}
	if( enterfrac - ( 1.0f / 1024.0f ) <= leavefrac ) {
		if( enterfrac > -1 && enterfrac < tlc->trace->fraction ) {
			if( enterfrac < 0 ) {
				enterfrac = 0;
			}
			tlc->trace->fraction = enterfrac;
			CM_CopyCMToRawPlane( clipplane, &tlc->trace->plane );
			tlc->trace->surfFlags = leadside->surfFlags;
			tlc->trace->contents = brush->contents;
			tlc->trace->shaderNum = leadside->shaderNum;
		}
	}
}

void Ops::TestBoxInBrush( CMTraceContext *tlc, const cbrush_t *brush ) {
	int i;
	cm_plane_t *p;
	cbrushside_t *side;

	if( !brush->numsides ) {
		return;
	}

	const float *startmins = tlc->startmins;
	const float *startmaxs = tlc->startmaxs;

	side = brush->brushsides;
	for( i = 0; i < brush->numsides; i++, side++ ) {
		p = &side->plane;
		int type = p->type;

		// push the plane out appropriately for mins/maxs
		// if completely in front of face, no intersection
		if( type < 3 ) {
			if( startmins[type] > p->dist ) {
				return;
			}
		} else {
			const float *normal = p->normal;
			switch( p->signbits & 7 ) {
				case 0:
					if( normal[0] * startmins[0] + normal[1] * startmins[1] + normal[2] * startmins[2] > p->dist ) {
						return;
					}
					break;
				case 1:
					if( normal[0] * startmaxs[0] + normal[1] * startmins[1] + normal[2] * startmins[2] > p->dist ) {
						return;
					}
					break;
				case 2:
					if( normal[0] * startmins[0] + normal[1] * startmaxs[1] + normal[2] * startmins[2] > p->dist ) {
						return;
					}
					break;
				case 3:
					if( normal[0] * startmaxs[0] + normal[1] * startmaxs[1] + normal[2] * startmins[2] > p->dist ) {
						return;
					}
					break;
				case 4:
					if( normal[0] * startmins[0] + normal[1] * startmins[1] + normal[2] * startmaxs[2] > p->dist ) {
						return;
					}
					break;
				case 5:
					if( normal[0] * startmaxs[0] + normal[1] * startmins[1] + normal[2] * startmaxs[2] > p->dist ) {
						return;
					}
					break;
				case 6:
					if( normal[0] * startmins[0] + normal[1] * startmaxs[1] + normal[2] * startmaxs[2] > p->dist ) {
						return;
					}
					break;
				case 7:
					if( normal[0] * startmaxs[0] + normal[1] * startmaxs[1] + normal[2] * startmaxs[2] > p->dist ) {
						return;
					}
					break;
				default:
					assert( 0 );
					return;
			}
		}
	}

	// inside this brush
	tlc->trace->startsolid = tlc->trace->allsolid = true;
	tlc->trace->fraction = 0;
	tlc->trace->contents = brush->contents;
}

void Ops::CollideBox( CMTraceContext *tlc, void ( Ops::*method )( CMTraceContext *, const cbrush_t * ),
					  const cbrush_t *brushes, int nummarkbrushes, const cface_t *markfaces, int nummarkfaces ) {
	// Save the exact address to avoid pointer chasing in loops
	const float *fraction = &tlc->trace->fraction;

	// trace line against all brushes
	for( int i = 0; i < nummarkbrushes; i++ ) {
		const auto *__restrict b = &brushes[i];
		if( !( b->contents & tlc->contents ) ) {
			continue;
		}
		if( !doBoundsTest( b->mins, b->maxs, tlc ) ) {
			continue;
		}
		( this->*method )( tlc, b );
		if( !*fraction ) {
			return;
		}
	}

	// trace line against all patches
	for( int i = 0; i < nummarkfaces; i++ ) {
		const auto *__restrict patch = &markfaces[i];
		if( !( patch->contents & tlc->contents ) ) {
			continue;
		}
		if( !doBoundsTest( patch->mins, patch->maxs, tlc ) ) {
			continue;
		}
		for( int j = 0; j < patch->numfacets; j++ ) {
			const auto *__restrict facet = &patch->facets[j];
			if( !doBoundsTest( facet->mins, facet->maxs, tlc ) ) {
				continue;
			}
			( this->*method )( tlc, facet );
			if( !*fraction ) {
				return;
			}
		}
	}
}



void Ops::ClipBoxToLeaf( CMTraceContext *tlc, const cbrush_t *brushes, int numbrushes,
						 const cface_t *markfaces, int nummarkfaces ) {
	// Saving the method reference should reduce virtual calls cost by avoid vtable lookup
	auto method = &Ops::ClipBoxToBrush;

	// Save the exact address to avoid pointer chasing in loops
	const float *fraction = &tlc->trace->fraction;

	// trace line against all brushes
	for( int i = 0; i < numbrushes; i++ ) {
		const auto *__restrict b = &brushes[i];
		if( !( b->contents & tlc->contents ) ) {
			continue;
		}
		if( !doBoundsAndLineDistTest( b->mins, b->maxs, b->center, b->radius, tlc ) ) {
			continue;
		}
		( this->*method )( tlc, b );
		if( !*fraction ) {
			return;
		}
	}

	// trace line against all patches
	for( int i = 0; i < nummarkfaces; i++ ) {
		const auto *__restrict patch = &markfaces[i];
		if( !( patch->contents & tlc->contents ) ) {
			continue;
		}
		if( !doBoundsAndLineDistTest( patch->mins, patch->maxs, patch->center, patch->radius, tlc ) ) {
			continue;
		}
		for( int j = 0; j < patch->numfacets; j++ ) {
			const auto *__restrict facet = &patch->facets[j];
			if( !doBoundsAndLineDistTest( facet->mins, facet->maxs, facet->center, facet->radius, tlc ) ) {
				continue;
			}
			( this->*method )( tlc, facet );
			if( !*fraction ) {
				return;
			}
		}
	}
}

void Ops::RecursiveHullCheck( CMTraceContext *tlc, int num, float p1f, float p2f, const vec3_t p1, const vec3_t p2 ) {
	cnode_t *node;
	cplane_t *plane;
	int side;
	float t1, t2, offset;
	float frac, frac2;
	float idist, midf;
	vec3_t mid;

loc0:
	if( tlc->trace->fraction <= p1f ) {
		return; // already hit something nearer
	}
	// if < 0, we are in a leaf node
	if( num < 0 ) {
		cleaf_t *leaf;

		leaf = &cms->map_leafs[-1 - num];
		if( leaf->contents & tlc->contents ) {
			ClipBoxToLeaf( tlc, leaf->brushes, leaf->numbrushes, leaf->faces, leaf->numfaces );
		}
		return;
	}

	//
	// find the point distances to the seperating plane
	// and the offset for the size of the box
	//
	node = cms->map_nodes + num;
	plane = node->plane;

	if( plane->type < 3 ) {
		t1 = p1[plane->type] - plane->dist;
		t2 = p2[plane->type] - plane->dist;
		offset = tlc->extents[plane->type];
	} else {
		t1 = DotProduct( plane->normal, p1 ) - plane->dist;
		t2 = DotProduct( plane->normal, p2 ) - plane->dist;
		if( tlc->ispoint ) {
			offset = 0;
		} else {
			offset = fabsf( tlc->extents[0] * plane->normal[0] ) +
					 fabsf( tlc->extents[1] * plane->normal[1] ) +
					 fabsf( tlc->extents[2] * plane->normal[2] );
		}
	}

	// see which sides we need to consider
	if( t1 >= offset && t2 >= offset ) {
		num = node->children[0];
		goto loc0;
	}
	if( t1 < -offset && t2 < -offset ) {
		num = node->children[1];
		goto loc0;
	}

	// put the crosspoint DIST_EPSILON pixels on the near side
	if( t1 < t2 ) {
		idist = 1.0 / ( t1 - t2 );
		side = 1;
		frac2 = ( t1 + offset + DIST_EPSILON ) * idist;
		frac = ( t1 - offset + DIST_EPSILON ) * idist;
	} else if( t1 > t2 ) {
		idist = 1.0 / ( t1 - t2 );
		side = 0;
		frac2 = ( t1 - offset - DIST_EPSILON ) * idist;
		frac = ( t1 + offset + DIST_EPSILON ) * idist;
	} else {
		side = 0;
		frac = 1;
		frac2 = 0;
	}

	// move up to the node
	Q_clamp( frac, 0, 1 );
	midf = p1f + ( p2f - p1f ) * frac;
	VectorLerp( p1, frac, p2, mid );

	RecursiveHullCheck( tlc, node->children[side], p1f, midf, p1, mid );

	// go past the node
	Q_clamp( frac2, 0, 1 );
	midf = p1f + ( p2f - p1f ) * frac2;
	VectorLerp( p1, frac2, p2, mid );

	RecursiveHullCheck( tlc, node->children[side ^ 1], midf, p2f, mid, p2 );
}

void Ops::SetupCollideContext( CMTraceContext *__restrict tlc, trace_t *__restrict tr, const float *start,
							   const float *end, const float *mins, const float *maxs, int brushmask ) {
	tlc->trace = tr;
	tlc->contents = brushmask;
	VectorCopy( start, tlc->start );
	VectorCopy( end, tlc->end );
	VectorCopy( mins, tlc->mins );
	VectorCopy( maxs, tlc->maxs );

	// TODO: Supply 4-component vectors as arguments of this method so we can use explicit SIMD overloads
	BoundsBuilder boundsBuilder;

	VectorAdd( start, tlc->mins, tlc->startmins );
	boundsBuilder.addPoint( tlc->startmins );

	VectorAdd( start, tlc->maxs, tlc->startmaxs );
	boundsBuilder.addPoint( tlc->startmaxs );

	VectorAdd( end, tlc->mins, tlc->endmins );
	boundsBuilder.addPoint( tlc->endmins );

	VectorAdd( end, tlc->maxs, tlc->endmaxs );
	boundsBuilder.addPoint( tlc->endmaxs );

	boundsBuilder.storeTo( tlc->absmins, tlc->absmaxs );
}

void Ops::Trace( trace_t *tr, const vec3_t start, const vec3_t end, const vec3_t mins, const vec3_t maxs,
							 const cmodel_t *cmodel, int brushmask, int topNodeHint ) {
	assert( topNodeHint >= 0 );

	// fill in a default trace
	memset( tr, 0, sizeof( *tr ) );
	tr->fraction = 1;
	if( !cms->numnodes ) { // map not loaded
		return;
	}

	alignas( 16 ) CMTraceContext tlc;
	SetupCollideContext( &tlc, tr, start, end, mins, maxs, brushmask );

	//
	// check for position test special case
	//
	if( VectorCompare( start, end ) ) {
		auto func = &Ops::TestBoxInBrush;
		if( cmodel != cms->map_cmodels ) {
			if( BoundsIntersect( cmodel->mins, cmodel->maxs, tlc.absmins, tlc.absmaxs ) ) {
				CollideBox( &tlc, func, cmodel->brushes, cmodel->numbrushes, cmodel->faces, cmodel->numfaces );
			}
		} else {
			vec3_t boxmins, boxmaxs;
			for( int i = 0; i < 3; i++ ) {
				boxmins[i] = start[i] + mins[i] - 1;
				boxmaxs[i] = start[i] + maxs[i] + 1;
			}

			int leafs[1024];
			int topnode;
			int numleafs = CM_BoxLeafnums( cms, boxmins, boxmaxs, leafs, 1024, &topnode, topNodeHint );
			for( int i = 0; i < numleafs; i++ ) {
				cleaf_t *leaf = &cms->map_leafs[leafs[i]];
					if( leaf->contents & tlc.contents ) {
						CollideBox( &tlc, func, leaf->brushes, leaf->numbrushes, leaf->faces, leaf->numfaces );
						if( tr->allsolid ) {
							break;
						}
					}
			}
		}

		VectorCopy( start, tr->endpos );
		return;
	}

	//
	// check for point special case
	//
	if( VectorCompare( mins, vec3_origin ) && VectorCompare( maxs, vec3_origin ) ) {
		tlc.ispoint = true;
		VectorClear( tlc.extents );
	} else {
		tlc.ispoint = false;
		VectorSet( tlc.extents,
				   -mins[0] > maxs[0] ? -mins[0] : maxs[0],
				   -mins[1] > maxs[1] ? -mins[1] : maxs[1],
				   -mins[2] > maxs[2] ? -mins[2] : maxs[2] );
	}

	// TODO: Why do we have to prepare all these vars for all cases, otherwise platforms/movers are malfunctioning?
	SetupClipContext( &tlc );

	VectorSubtract( end, start, tlc.traceDir );
	VectorNormalize( tlc.traceDir );
	float squareDiameter = DistanceSquared( mins, maxs );
	if( squareDiameter >= 2.0f ) {
		tlc.boxRadius = 0.5f * sqrtf( squareDiameter ) + 8.0f;
	} else {
		tlc.boxRadius = 8.0f;
	}

	//
	// general sweeping through world
	//
	if( cmodel == cms->map_cmodels ) {
		RecursiveHullCheck( &tlc, topNodeHint, 0, 1, start, end );
	} else if( BoundsIntersect( cmodel->mins, cmodel->maxs, tlc.absmins, tlc.absmaxs ) ) {
		auto func = &Ops::ClipBoxToBrush;
		CollideBox( &tlc, func, cmodel->brushes, cmodel->numbrushes, cmodel->faces, cmodel->numfaces );
	}

	if( tr->fraction == 1 ) {
		VectorCopy( end, tr->endpos );
	} else {
		VectorLerp( start, tr->fraction, end, tr->endpos );
	}
}

#ifdef CM_SELF_TEST
static void CompareTraceResults( const trace_t *tr, const char **tags, int count, bool interrupt = false ) {
	if( count < 2 ) {
		return;
	}

	int i;

	for( i = 0; i < count; ++i ) {
		if( tr[i].fraction < 0.0f || tr[i].fraction > 1.0f ) {
			printf( "fraction: %f\n", tr[i].fraction );
			abort();
		}
	}

	for( i = 0; i < count - 1; ++i ) {
		if( tr[i].fraction != tr[i + 1].fraction ) {
			break;
		}
	}
	if( i != count - 1 ) {
		for( i = 0; i < count; ++i ) {
			printf( "%8s: fraction %lf\n", tags[i], tr[i].fraction );
		}
		if( interrupt ) {
			abort();
		}
	}

	for ( i = 0; i < count - 1; ++i ) {
		if( !VectorCompare( tr[i].endpos, tr[i + 1].endpos ) ) {
			break;
		}
	}
	if( i != count - 1 ) {
		for( i = 0; i < count; ++i ) {
			printf( "%8s: endpos %lf %lf %lf\n", tags[i], tr[i].endpos[0], tr[i].endpos[1], tr[i].endpos[2] );
		}
		if( interrupt ) {
			abort();
		}
	}

	for( i = 0; i < count - 1; ++i ) {
		if( !VectorCompare( tr[i].plane.normal, tr[i + 1].plane.normal ) || tr[i].plane.dist != tr[i + 1].plane.dist ) {
			break;
		}
	}
	if( i != count - 1 ) {
		for( i = 0; i < count; ++i ) {
			const float *n = tr[i].plane.normal;
			printf( "%8s: normal %lf %lf %lf dist %lf\n", tags[i], n[0], n[1], n[2], tr[i].plane.dist );
		}
		if( interrupt ) {
			abort();
		}
	}

	for( i = 0; i < count - 1; ++i ) {
		const auto &tr1 = tr[i];
		const auto &tr2 = tr[i + 1];
		if( tr1.contents != tr2.contents || tr1.surfFlags != tr2.surfFlags || tr1.shaderNum != tr2.shaderNum ) {
			break;
		}
	}
	if( i != count - 1 ) {
		for( i = 0; i < count; ++i ) {
			printf( "%8s: contents %x surfFlags %x shaderNum %x\n",
		   		tags[i], tr[i].contents, tr[i].surfFlags, tr[i].shaderNum );
		}
		if( interrupt ) {
			abort();
		}
	}

	for( i = 0; i < count - 1; ++i ) {
		if( tr[i].startsolid != tr[i + 1].startsolid || tr[i].allsolid != tr[i + 1].allsolid ) {
			break;
		}
	}
	if( i != count - 1 ) {
		for( i = 0; i < count; ++i ) {
			printf( "%8s: startsolid %d allsolid %d\n", tags[i], (int)tr[i].startsolid, (int)tr[i].allsolid );
		}
		if( interrupt ) {
			abort();
		}
	}
}
#endif

/*
* CM_TransformedBoxTrace
*
* Handles offseting and rotation of the end points for moving and
* rotating entities
*/
void CM_TransformedBoxTrace( const cmodel_state_t *cms, trace_t *tr,
							 const vec3_t start, const vec3_t end,
							 const vec3_t mins, const vec3_t maxs,
							 const cmodel_t *cmodel, int brushmask,
							 const vec3_t origin, const vec3_t angles,
							 int topNodeHint ) {
	assert( topNodeHint >= 0 );

	vec3_t start_l, end_l;
	vec3_t a, temp;
	mat3_t axis;
	bool rotated;

	if( !tr ) {
		return;
	}

	if( !cmodel || cmodel == cms->map_cmodels ) {
		cmodel = cms->map_cmodels;
		origin = vec3_origin;
		angles = vec3_origin;
	} else {
		if( !origin ) {
			origin = vec3_origin;
		}
		if( !angles ) {
			angles = vec3_origin;
		}
	}

	// cylinder offset
	if( cmodel == cms->oct_cmodel ) {
		VectorSubtract( start, cmodel->cyl_offset, start_l );
		VectorSubtract( end, cmodel->cyl_offset, end_l );
	} else {
		VectorCopy( start, start_l );
		VectorCopy( end, end_l );
	}

	// subtract origin offset
	VectorSubtract( start_l, origin, start_l );
	VectorSubtract( end_l, origin, end_l );

	// ch : here we could try back-rotate the vector for aabb to get
	// 'cylinder-like' shape, ie width of the aabb is constant for all directions
	// in this case, the orientation of vector would be ( normalize(origin-start), cross(x,z), up )

	// rotate start and end into the models frame of reference
	if( ( angles[0] || angles[1] || angles[2] )
#ifndef CM_ALLOW_ROTATED_BBOXES
		&& !cmodel->builtin
#endif
		) {
		rotated = true;
	} else {
		rotated = false;
	}

	if( rotated ) {
		AnglesToAxis( angles, axis );

		VectorCopy( start_l, temp );
		Matrix3_TransformVector( axis, temp, start_l );

		VectorCopy( end_l, temp );
		Matrix3_TransformVector( axis, temp, end_l );
	}

#ifdef CM_SELF_TEST
	trace_t traces[3];
	const char *tags[3] { "generic", "sse42", "avx" };
	genericOps.cms = const_cast<cmodel_state_s *>( cms );
	genericOps.Trace( &traces[0], start_l, end_l, mins, maxs, cmodel, brushmask, topNodeHint );
	sse42Ops.cms = const_cast<cmodel_state_s *>( cms );
	sse42Ops.Trace( &traces[1], start_l, end_l, mins, maxs, cmodel, brushmask, topNodeHint );
	avxOps.cms = const_cast<cmodel_state_s *>( cms );
	avxOps.Trace( &traces[2], start_l, end_l, mins, maxs, cmodel, brushmask, topNodeHint );
	CompareTraceResults( traces, tags, 3 );
#endif

	// sweep the box through the model
	cms->ops->Trace( tr, start_l, end_l, mins, maxs, cmodel, brushmask, topNodeHint );

	if( rotated && tr->fraction != 1.0 ) {
		VectorNegate( angles, a );
		AnglesToAxis( a, axis );

		VectorCopy( tr->plane.normal, temp );
		Matrix3_TransformVector( axis, temp, tr->plane.normal );
	}

	if( tr->fraction == 1 ) {
		VectorCopy( end, tr->endpos );
	} else {
		VectorLerp( start, tr->fraction, end, tr->endpos );
	}
}

void Ops::BuildShapeList( CMShapeList *list, const float *mins, const float *maxs, int clipMask ) {
	int leafNums[1024], topNode;
	// TODO: This can be optimized
	const int numLeaves = CM_BoxLeafnums( cms, mins, maxs, leafNums, 1024, &topNode );

	auto *const checkedFacesMask = ( uint64_t *)list->scratchpad;
	assert( cms->numfaces + 1 < MAX_MAP_FACES && "Should account for the +1 globalNumber shift" );
	std::memset( checkedFacesMask, 0, cms->numfaces );
	auto *const checkedBrushesMask = ( uint64_t *)( (uint8_t *)list->scratchpad + MAX_MAP_FACES / 8 );
	assert( cms->numbrushes + 1 < MAX_MAP_BRUSHES && "Should account for the +1 globalNumber shift" );
	std::memset( checkedBrushesMask, 0, cms->numbrushes );

	int numShapes = 0;
	int possibleContents = 0;
	const auto *leaves = cms->map_leafs;
	const float *__restrict testedMins = mins;
	const float *__restrict testedMaxs = maxs;
	auto *__restrict destShapes = list->shapes;
	for( int i = 0; i < numLeaves; ++i ) {
		const auto *__restrict leaf = &leaves[leafNums[i]];
		const auto *brushes = leaf->brushes;
		for( int j = 0; j < leaf->numbrushes; ++j ) {
			const auto *__restrict b = &brushes[j];
			const uint64_t qwordIndex = b->globalNumber / 64;
			const uint64_t qwordMask = (uint64_t)1 << ( b->globalNumber % 64 );
			if( checkedBrushesMask[qwordIndex] & qwordMask ) {
				continue;
			}
			checkedBrushesMask[qwordIndex] |= qwordMask;
			if( !( b->contents & clipMask ) ) {
				continue;
			}
			if( !BoundsIntersect( b->mins, b->maxs, testedMins, testedMaxs ) ) {
				continue;
			}
			possibleContents |= b->contents;
			destShapes[numShapes++] = b;
		}

		const auto *faces = leaf->faces;
		for( int j = 0; j < leaf->numfaces; ++j ) {
			const auto *__restrict f = &faces[j];
			const uint64_t qwordIndex = f->globalNumber / 64;
			const uint64_t qwordMask = (uint64_t)1 << ( f->globalNumber % 64 );
			if( checkedFacesMask[qwordIndex] & qwordMask ) {
				continue;
			}
			checkedFacesMask[qwordIndex] |= qwordMask;
			if( !( f->contents & clipMask ) ) {
				continue;
			}
			if( !BoundsIntersect( f->mins, f->maxs, testedMins, testedMaxs ) ) {
				continue;
			}
			for( int k = 0; k < f->numfacets; ++k ) {
				const auto *__restrict b = &f->facets[k];
				if( !BoundsIntersect( b->mins, b->maxs, testedMins, testedMaxs ) ) {
					continue;
				}
				destShapes[numShapes++] = b;
				possibleContents |= f->contents;
			}
		}
	}

	list->hasBounds = false;
	list->numShapes = numShapes;
	list->possibleContents = possibleContents;
}

void Ops::ClipShapeList( CMShapeList *list, const CMShapeList *baseList, const float *mins, const float *maxs ) {
	const int numSrcShapes = baseList->numShapes;
	const auto *srcShapes = baseList->shapes;

	int numDestShapes = 0;
	auto *destShapes = list->shapes;

	const float *__restrict testedMins = mins;
	const float *__restrict testedMaxs = maxs;

	BoundsBuilder builder;
	for( int i = 0; i < numSrcShapes; ++i ) {
		const cbrush_t *__restrict b = srcShapes[i];
		if( !BoundsIntersect( b->mins, b->maxs, testedMins, testedMaxs ) ) {
			continue;
		}
		destShapes[numDestShapes++] = b;
		builder.addPoint( b->mins );
		builder.addPoint( b->maxs );
	}

	list->numShapes = numDestShapes;
	list->hasBounds = numDestShapes > 0;
	if( list->hasBounds ) {
		builder.storeTo( list->mins, list->maxs );
	}

	list->possibleContents = baseList->possibleContents;
}

void Ops::ClipToShapeList( const CMShapeList *list, trace_t *tr, const float *start,
						   const float *end, const float *mins, const float *maxs, int clipMask ) {
	CMTraceContext tlc;
	SetupCollideContext( &tlc, tr, start, end, mins, maxs, clipMask );

	if( list->hasBounds ) {
		if( !BoundsIntersect( list->mins, list->maxs, tlc.absmins, tlc.absmaxs ) ) {
			assert( tr->fraction == 1.0f );
			VectorCopy( end, tr->endpos );
			return;
		}
	}

	const int numShapes = list->numShapes;
	const auto *__restrict shapes = list->shapes;

	float *const __restrict fraction = &tlc.trace->fraction;

	// Make sure the virtual call address gets resolved here
	auto clipFn = &Ops::ClipBoxToBrush;
	if( VectorCompare( start, end ) ) {
		clipFn = &Ops::TestBoxInBrush;
	}

	for( int i = 0; i < numShapes; ++i ) {
		const cbrush_t *__restrict b = shapes[i];
		if( !BoundsIntersect( b->mins, b->maxs, tlc.absmins, tlc.absmaxs ) ) {
			continue;
		}
		( this->*clipFn )( &tlc, b );
		if( !*fraction ) {
			break;
		}
	}

	if( tr->fraction == 1.0f ) {
		VectorCopy( end, tr->endpos );
	} else {
		VectorLerp( start, tr->fraction, end, tr->endpos );
	}
}

CMShapeList *CM_AllocShapeList( cmodel_state_t *cms ) {
	// TODO: Use a necessary amount of memory
	const size_t totalSize = 72 * 1024;
	void *mem = ::malloc( totalSize );
	if( !mem ) {
		return nullptr;
	}
	auto *list = (CMShapeList *)mem;
	new( list )CMShapeList( list + 1, totalSize - sizeof( CMShapeList ) );
	return list;
}

void CM_FreeShapeList( cmodel_state_t *cms, CMShapeList *list ) {
	if( list ) {
		::free( list );
	}
}

int CM_PossibleShapeListContents( const CMShapeList *list ) {
	return list->possibleContents;
}

int CM_GetNumShapesInShapeList( const CMShapeList *list ) {
	return list->numShapes;
}

CMShapeList *CM_BuildShapeList( cmodel_state_t *cms, CMShapeList *list, const float *mins, const float *maxs, int clipMask ) {
#ifdef CM_SELF_TEST
	cbrush_s **tmp1[1024];
	cbrush_s **tmp2[1024];

	genericOps.cms = cms;
	genericOps.BuildShapeList( list, mins, maxs, clipMask );
	const int numShapes1 = list->numShapes;
	std::memcpy( tmp1, list->shapes, list->numShapes * 8 );

	avxOps.cms = cms;
	avxOps.BuildShapeList( list, mins, maxs, clipMask );
	int numShapes2 = list->numShapes;
	if( numShapes1 != numShapes2 ) abort();
	std::memcpy( tmp2, list->shapes, list->numShapes * 8 );

	// We're free to reorder shapes, make sure they're sorted for comparison
	wsw::sortPodNonSpeedCritical( tmp1, tmp1 + list->numShapes );
	wsw::sortPodNonSpeedCritical( tmp2, tmp2 + list->numShapes );
	if( std::memcmp( tmp1, tmp2, list->numShapes * 8 ) ) abort();
#endif

	cms->ops->BuildShapeList( list, mins, maxs, clipMask );
	return list;
}

void CM_ClipShapeList( cmodel_state_t *cms, CMShapeList *list,
	                   const CMShapeList *baseList,
	                   const float *mins, const float *maxs ) {
#ifdef CM_SELF_TEST
	cbrush_s **tmp1[1024];
	cbrush_s **tmp2[1024];

	genericOps.cms = cms;
	genericOps.ClipShapeList( list, baseList, mins, maxs );
	const int numShapes1 = list->numShapes;
	std::memcpy( tmp1, list->shapes, list->numShapes * 8 );

	avxOps.cms = cms;
	avxOps.ClipShapeList( list, baseList, mins, maxs );
	const int numShapes2 = list->numShapes;
	if( numShapes1 != numShapes2 ) abort();
	std::memcpy( tmp2, list->shapes, list->numShapes * 8 );

	// We're free to reorder shapes, make sure they're sorted for comparison
	wsw::sortPodNonSpeedCritical( tmp1, tmp1 + list->numShapes );
	wsw::sortPodNonSpeedCritical( tmp2, tmp2 + list->numShapes );
	if( std::memcmp( tmp1, tmp2, list->numShapes * 8 ) ) abort();
#endif

	cms->ops->ClipShapeList( list, baseList, mins, maxs );
}

void CM_ClipToShapeList( cmodel_state_t *cms, const CMShapeList *list, trace_t *tr,
	                     const float *start, const float *end,
	                     const float *mins, const float *maxs, int clipMask ) {
	memset( tr, 0, sizeof( trace_t ) );
	tr->fraction = 1.0f;
	if( !list || !list->numShapes ) {
		VectorCopy( end, tr->endpos );
	    return;
	}

#ifdef CM_SELF_TEST
	trace_t traces[3];
	const char *tags[3] { "generic", "sse42", "avx" };
	std::memset( &traces[0], 0, sizeof( trace_t ) );
	std::memset( &traces[1], 0, sizeof( trace_t ) );
	std::memset( &traces[2], 0, sizeof( trace_t ) );
	traces[0].fraction = traces[1].fraction = traces[2].fraction = 1.0f;

	genericOps.cms = cms;
	genericOps.ClipToShapeList( list, &traces[0], start, end, mins, maxs, clipMask );
	sse42Ops.cms = cms;
	sse42Ops.ClipToShapeList( list, &traces[1], start, end, mins, maxs, clipMask );
	avxOps.cms = cms;
	avxOps.ClipToShapeList( list, &traces[2], start, end, mins, maxs, clipMask );

	CompareTraceResults( traces, tags, 3 );
#endif

	cms->ops->ClipToShapeList( list, tr, start, end, mins, maxs, clipMask );
}