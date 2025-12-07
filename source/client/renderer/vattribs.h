/*
Copyright (C) 2013 Victor Luchits

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
#ifndef R_VATTRIBS_H
#define R_VATTRIBS_H

#include <common/facilities/qfiles.h>

typedef enum vattrib_e {
	VATTRIB_POSITION        = 0,

	VATTRIB_NORMAL          = 1,

	VATTRIB_SVECTOR         = 2,

	VATTRIB_COLOR0          = 3,

	VATTRIB_TEXCOORDS       = 4,

	VATTRIB_SPRITEPOINT     = 5,

	VATTRIB_BONESINDICES    = 6,
	VATTRIB_BONESWEIGHTS    = 7,

	VATTRIB_LMCOORDS01      = 6,
	VATTRIB_LMCOORDS23      = 7,
	VATTRIB_LMLAYERS0123    = 8,

	VATTRIB_INSTANCE_QUAT   = 9,
	VATTRIB_INSTANCE_XYZS   = 10,

	NUM_VERTEX_ATTRIBS      = 11
} vattrib_t;

#define VATTRIB_BIT( va )     ( 1 << ( va ) )

typedef enum vattribbit_e {
	VATTRIB_POSITION_BIT    = VATTRIB_BIT( 0 ),

	VATTRIB_NORMAL_BIT      = VATTRIB_BIT( 1 ),

	VATTRIB_SVECTOR_BIT     = VATTRIB_BIT( 2 ),

	VATTRIB_COLOR0_BIT      = VATTRIB_BIT( 3 ),

	VATTRIB_TEXCOORDS_BIT   = VATTRIB_BIT( 4 ),

	VATTRIB_AUTOSPRITE_BIT  = VATTRIB_BIT( 5 ),
	VATTRIB_AUTOSPRITE2_BIT = VATTRIB_BIT( 6 ) | VATTRIB_SVECTOR_BIT,

	VATTRIB_BONESINDICES_BIT = VATTRIB_BIT( 7 ),
	VATTRIB_BONESWEIGHTS_BIT = VATTRIB_BIT( 8 ),

	VATTRIB_BONES_BITS      = VATTRIB_BONESINDICES_BIT | VATTRIB_BONESWEIGHTS_BIT,

	VATTRIB_COLORS_BITS     = VATTRIB_COLOR0_BIT,

	VATTRIB_LMCOORDS0_BIT   = VATTRIB_BIT( 9 ),
	VATTRIB_LMCOORDS1_BIT   = VATTRIB_BIT( 10 ),
	VATTRIB_LMCOORDS2_BIT   = VATTRIB_BIT( 11 ),
	VATTRIB_LMCOORDS3_BIT   = VATTRIB_BIT( 12 ),

	VATTRIB_LMCOORDS_BITS   = VATTRIB_LMCOORDS0_BIT | VATTRIB_LMCOORDS1_BIT | VATTRIB_LMCOORDS2_BIT | VATTRIB_LMCOORDS3_BIT,

	VATTRIB_LMLAYERS0123_BIT = VATTRIB_BIT( 13 ),

	VATTRIB_INSTANCE_QUAT_BIT = VATTRIB_BIT( 14 ),
	VATTRIB_INSTANCE_XYZS_BIT = VATTRIB_BIT( 15 ),

	VATTRIB_INSTANCES_BITS  = VATTRIB_INSTANCE_QUAT_BIT | VATTRIB_INSTANCE_XYZS_BIT,

	VATTRIBS_MASK           = VATTRIB_BIT( 16 ) - 1
} vattribbit_t;

typedef unsigned int vattribmask_t;

struct VboSpanLayout {
	vattribmask_t vertexAttribs;
	vattribmask_t halfFloatAttribs;

	unsigned baseOffset;
	unsigned instancesOffset;

	uint8_t vertexSize;

	uint8_t normalsOffset;
	uint8_t sVectorsOffset;
	uint8_t stOffset;
	uint8_t lmstOffset[( MAX_LIGHTMAPS + 1 ) / 2];
	uint8_t lmstSize[( MAX_LIGHTMAPS + 1 ) / 2];
	uint8_t lmlayersOffset[( MAX_LIGHTMAPS + 3 ) / 4];
	uint8_t colorsOffset[MAX_LIGHTMAPS];
	uint8_t bonesIndicesOffset;
	uint8_t bonesWeightsOffset;
	uint8_t spritePointsOffset;              // autosprite or autosprite2 centre + radius
};

#define FLOAT_VATTRIB_TYPE( vattrib,halfFloatVattribs ) \
	( (int)( halfFloatVattribs & vattrib ) == vattrib ? GLhalf : float )

#define FLOAT_VATTRIB_GL_TYPE( vattrib,halfFloatVattribs ) \
	( (int)( halfFloatVattribs & vattrib ) == vattrib ? GL_HALF_FLOAT : GL_FLOAT )

#define FLOAT_VATTRIB_SIZE( vattrib,halfFloatVattribs ) \
	( (int)( halfFloatVattribs & vattrib ) == vattrib ? sizeof( GLhalf ) : sizeof( float ) )

#define MAX_UNIFORM_BINDINGS ( 17 )

typedef vec_t instancePoint_t[8]; // quaternion for rotation + xyz pos + uniform scale

#endif // R_VATTRIBS_H
