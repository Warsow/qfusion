/*
Copyright (C) 1997-2001 Id Software, Inc.
Copyright (c) ZeniMax Media Inc.

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

#include "q_math.h"
#include <common/facilities/syspublic.h>

#include <cstring>

//============================================================================

const vec3_t kPredefinedDirs[NUMVERTEXNORMALS] =
{
#include <common/helpers/anorms.h>
};

// TODO: Lift ByteToDirTable to outer scope

class DirToByteTable {
	enum: uint8_t { kSize = NUMVERTEXNORMALS };

	// Works quite good.
	// Choosing a proper hash function that groups together similar directories is what that matter.
	enum { kNumBins = kSize };
	uint8_t bins[kNumBins];
	uint8_t next[kSize];

	enum: uint8_t { kNullLink = std::numeric_limits<uint8_t>::max() };
	// Ensure we can use 255 and 254 to indicate something else
	static_assert( kSize < std::numeric_limits<uint8_t>::max(), "" );

	// The hit ratio (a success rate of GetFirstHashedFit()) is very good, something about 90 % or even more
	static inline auto Hash( const vec3_t v ) -> uint32_t {
		auto u0 = (uint32_t)( std::fabs( v[0] ) * 3 );
		auto u1 = (uint32_t)( std::fabs( v[1] ) * 3 );
		auto u2 = (uint32_t)( std::fabs( v[2] ) * 3 );
		return u0 * 64 + u1 * 8 + u2;
	}

	auto getFirstHashedFit( const vec3_t v ) const -> int {
		int binIndex = Hash( v ) % kNumBins;
		const auto *normals = ::kPredefinedDirs;
		for( int num = bins[binIndex]; num != kNullLink; ) {
			const auto *n = normals[num];
			if( DotProduct( v, n ) > 0.95f ) {
				return num;
			}
			num = next[num];
		}
		return -1;
	}

	auto scanForFirstFit( const vec3_t v ) const -> int {
		const auto *normals = ::kPredefinedDirs;
		for( int i = 0; i < kSize; ++i ) {
			const auto *n = normals[i];
			if( DotProduct( v, n ) > 0.95f ) {
				return i;
			}
		}
		return -1;
	}

public:
	DirToByteTable() {
		// MSVC has troubles compiling std::fill_n() for kNullLink (?) type and using memset() is error-prone
		for( int i = 0; i < kNumBins; ++i ) {
			bins[i] = kNullLink;
		}

		for( unsigned i = 0; i < kSize; ++i ) {
			int binIndex = Hash( ::kPredefinedDirs[i] ) % kNumBins;
			int oldHead = bins[binIndex];
			// Link old bin head (or "null") as next for the newly added entry
			next[i] = (uint8_t)oldHead;
			// Link i-th entry to bin at bin index
			bins[binIndex] = (uint8_t)i;
		}
	}

	auto dirToByte( const float *dir ) const -> int {
		// Try getting a value in the same hash bin that is good enough
		int byte = getFirstHashedFit( dir );
		if( byte >= 0 ) {
			return byte;
		}

		byte = scanForFirstFit( dir );
		if( byte >= 0 ) {
			return byte;
		}

		// Fallback to the default implementation. Should happen extremely rarely.
		return ::DirToByte( dir );
	}
};

static const DirToByteTable dirToByteTable;

int DirToByteFast( const vec3_t dir ) {
	return ::dirToByteTable.dirToByte( dir );
}

int DirToByte( const vec3_t dir ) {
	int i, best;
	float d, bestd;
	bool normalized;

	if( !dir || VectorCompare( dir, vec3_origin ) ) {
		return NUMVERTEXNORMALS;
	}

	if( DotProduct( dir, dir ) == 1 ) {
		normalized = true;
	} else {
		normalized = false;
	}

	bestd = 0;
	best = 0;
	for( i = 0; i < NUMVERTEXNORMALS; i++ ) {
		d = DotProduct( dir, kPredefinedDirs[i] );
		if( ( d == 1 ) && normalized ) {
			return i;
		}
		if( d > bestd ) {
			bestd = d;
			best = i;
		}
	}

	return best;
}

void ByteToDir( int b, vec3_t dir ) {
	if( b < 0 || b >= NUMVERTEXNORMALS ) {
		VectorSet( dir, 0, 0, 0 );
	} else {
		VectorCopy( kPredefinedDirs[b], dir );
	}
}

//============================================================================

const vec4_t color_table[MAX_S_COLORS] =
{
	{ 0.0, 0.0, 0.0, 1.0 },
	{ 1.0, 0.0, 0.0, 1.0 },
	{ 0.0, 1.0, 0.0, 1.0 },
	{ 1.0, 1.0, 0.0, 1.0 },
	{ 0.0, 0.0, 1.0, 1.0 },
	{ 0.0, 1.0, 1.0, 1.0 },
	{ 1.0, 0.0, 1.0, 1.0 }, // magenta
	{ 1.0, 1.0, 1.0, 1.0 },
	{ 1.0, 0.5, 0.0, 1.0 }, // orange
	{ 0.5, 0.5, 0.5, 1.0 }, // grey
};

/*
* ColorNormalize
*/
vec_t ColorNormalize( const vec_t *in, vec_t *out ) {
	vec_t f = Q_max( Q_max( in[0], in[1] ), in[2] );

	if( f > 1.0f ) {
		f = 1.0f / f;
		out[0] = in[0] * f;
		out[1] = in[1] * f;
		out[2] = in[2] * f;
	} else {
		out[0] = in[0];
		out[1] = in[1];
		out[2] = in[2];
	}

	return f;
}

//============================================================================

void NormToLatLong( const vec3_t normal, float latlong[2] ) {
	// can't do atan2 (normal[1], normal[0])
	if( normal[0] == 0 && normal[1] == 0 ) {
		if( normal[2] > 0 ) {
			latlong[0] = 0; // acos ( 1 )
			latlong[1] = 0;
		} else {
			latlong[0] = M_PI; // acos ( -1 )
			latlong[1] = 0;
		}
	} else {
		latlong[0] = acos( normal[2] );
		latlong[1] = atan2( normal[1], normal[0] );
	}
}

void MakeNormalVectors( const vec3_t forward, vec3_t right, vec3_t up ) {
	float d;

	// this rotate and negate guarantees a vector not colinear with the original
	VectorSet( right, forward[2], -forward[0], forward[1] );
	d = DotProduct( right, forward );
	VectorMA( right, -d, forward, right );
	VectorNormalize( right );
	CrossProduct( right, forward, up );
}

void RotatePointAroundVector( vec3_t dst, const vec3_t dir, const vec3_t point, float degrees ) {
	float t0, t1;
	float c, s;
	vec3_t vr, vu, vf;

	s = DEG2RAD( degrees );
	c = cos( s );
	s = sin( s );

	VectorCopy( dir, vf );
	MakeNormalVectors( vf, vr, vu );

	t0 = vr[0] * c + vu[0] * -s;
	t1 = vr[0] * s + vu[0] *  c;
	dst[0] = ( t0 * vr[0] + t1 * vu[0] + vf[0] * vf[0] ) * point[0]
			 + ( t0 * vr[1] + t1 * vu[1] + vf[0] * vf[1] ) * point[1]
			 + ( t0 * vr[2] + t1 * vu[2] + vf[0] * vf[2] ) * point[2];

	t0 = vr[1] * c + vu[1] * -s;
	t1 = vr[1] * s + vu[1] *  c;
	dst[1] = ( t0 * vr[0] + t1 * vu[0] + vf[1] * vf[0] ) * point[0]
			 + ( t0 * vr[1] + t1 * vu[1] + vf[1] * vf[1] ) * point[1]
			 + ( t0 * vr[2] + t1 * vu[2] + vf[1] * vf[2] ) * point[2];

	t0 = vr[2] * c + vu[2] * -s;
	t1 = vr[2] * s + vu[2] *  c;
	dst[2] = ( t0 * vr[0] + t1 * vu[0] + vf[2] * vf[0] ) * point[0]
			 + ( t0 * vr[1] + t1 * vu[1] + vf[2] * vf[1] ) * point[1]
			 + ( t0 * vr[2] + t1 * vu[2] + vf[2] * vf[2] ) * point[2];
}

void BuildBoxPoints( vec3_t p[8], const vec3_t org, const vec3_t mins, const vec3_t maxs ) {
	VectorAdd( org, mins, p[0] );
	VectorAdd( org, maxs, p[1] );
	VectorSet( p[2], p[0][0], p[0][1], p[1][2] );
	VectorSet( p[3], p[0][0], p[1][1], p[0][2] );
	VectorSet( p[4], p[0][0], p[1][1], p[1][2] );
	VectorSet( p[5], p[1][0], p[1][1], p[0][2] );
	VectorSet( p[6], p[1][0], p[0][1], p[1][2] );
	VectorSet( p[7], p[1][0], p[0][1], p[0][2] );
}

void ProjectPointOntoPlane( vec3_t dst, const vec3_t p, const vec3_t normal ) {
	float d;
	vec3_t n;
	float inv_denom;

	inv_denom = 1.0F / DotProduct( normal, normal );

	d = DotProduct( normal, p ) * inv_denom;

	n[0] = normal[0] * inv_denom;
	n[1] = normal[1] * inv_denom;
	n[2] = normal[2] * inv_denom;

	dst[0] = p[0] - d * n[0];
	dst[1] = p[1] - d * n[1];
	dst[2] = p[2] - d * n[2];
}

//
// assumes "src" is normalized
//
void PerpendicularVector( vec3_t dst, const vec3_t src ) {
	int pos;
	int i;
	float minelem = 1.0F;
	vec3_t tempvec;

	//
	// find the smallest magnitude axially aligned vector
	//
	for( pos = 0, i = 0; i < 3; i++ ) {
		if( fabs( src[i] ) < minelem ) {
			pos = i;
			minelem = fabs( src[i] );
		}
	}
	tempvec[0] = tempvec[1] = tempvec[2] = 0.0F;
	tempvec[pos] = 1.0F;

	//
	// project the point onto the plane defined by src
	//
	ProjectPointOntoPlane( dst, tempvec, src );

	//
	// normalize the result
	//
	VectorNormalize( dst );
}

/*
* ProjectPointOntoVector
*/
void ProjectPointOntoVector( const vec3_t point, const vec3_t vStart, const vec3_t vDir, vec3_t vProj ) {
	vec3_t pVec;

	VectorSubtract( point, vStart, pVec );
	// project onto the directional vector for this segment
	VectorMA( vStart, DotProduct( pVec, vDir ), vDir, vProj );
}

//============================================================================

int Q_rand( int *seed ) {
	*seed = *seed * 1103515245 + 12345;
	return ( (unsigned int)( *seed / 65536 ) % 32768 );
}

// found here: http://graphics.stanford.edu/~seander/bithacks.html
int Q_bitcount( int v ) {
	int c;

	v = v - ( ( v >> 1 ) & 0x55555555 );                    // reuse input as temporary
	v = ( v & 0x33333333 ) + ( ( v >> 2 ) & 0x33333333 );     // temp
	c = ( ( ( v + ( v >> 4 ) ) & 0xF0F0F0F ) * 0x1010101 ) >> 24; // count

	return c;
}

/*
* CalcFov
*/
float CalcFov( float fov_x, float width, float height ) {
	float x;

	if( fov_x < 1 || fov_x > 179 ) {
		Sys_Error( "Bad fov: %f", fov_x );
	}

	x = width / tan( fov_x / 360 * M_PI );

	return atan( height / x ) * 360 / M_PI;
}

/*
* AdjustFov
*/
void AdjustFov( float *fov_x, float *fov_y, float width, float height, bool lock_x ) {
	float x, y;

	if( width * 3 == 4 * height || width * 4 == height * 5 || height > width ) {
		// 4:3, 5:4 ratios or vertical display
		return;
	}

	if( lock_x ) {
		*fov_y = 2 * atan( ( width * 3 ) / ( height * 4 ) * tan( *fov_y * M_PI / 360.0 * 0.5 ) ) * 360 / M_PI;
		return;
	}

	y = CalcFov( *fov_x, 640, 480 );
	x = *fov_x;

	*fov_x = CalcFov( y, height, width );
	if( *fov_x < x ) {
		*fov_x = x;
	} else {
		*fov_y = y;
	}
}

/*
* BoxOnPlaneSide
*
* Returns 1, 2, or 1 + 2
*/
int BoxOnPlaneSide( const vec3_t emins, const vec3_t emaxs, const struct cplane_s *p ) {
	float dist1, dist2;
	int sides;

	// general case
	switch( p->signbits ) {
		case 0:
			dist1 = p->normal[0] * emaxs[0] + p->normal[1] * emaxs[1] + p->normal[2] * emaxs[2];
			dist2 = p->normal[0] * emins[0] + p->normal[1] * emins[1] + p->normal[2] * emins[2];
			break;
		case 1:
			dist1 = p->normal[0] * emins[0] + p->normal[1] * emaxs[1] + p->normal[2] * emaxs[2];
			dist2 = p->normal[0] * emaxs[0] + p->normal[1] * emins[1] + p->normal[2] * emins[2];
			break;
		case 2:
			dist1 = p->normal[0] * emaxs[0] + p->normal[1] * emins[1] + p->normal[2] * emaxs[2];
			dist2 = p->normal[0] * emins[0] + p->normal[1] * emaxs[1] + p->normal[2] * emins[2];
			break;
		case 3:
			dist1 = p->normal[0] * emins[0] + p->normal[1] * emins[1] + p->normal[2] * emaxs[2];
			dist2 = p->normal[0] * emaxs[0] + p->normal[1] * emaxs[1] + p->normal[2] * emins[2];
			break;
		case 4:
			dist1 = p->normal[0] * emaxs[0] + p->normal[1] * emaxs[1] + p->normal[2] * emins[2];
			dist2 = p->normal[0] * emins[0] + p->normal[1] * emins[1] + p->normal[2] * emaxs[2];
			break;
		case 5:
			dist1 = p->normal[0] * emins[0] + p->normal[1] * emaxs[1] + p->normal[2] * emins[2];
			dist2 = p->normal[0] * emaxs[0] + p->normal[1] * emins[1] + p->normal[2] * emaxs[2];
			break;
		case 6:
			dist1 = p->normal[0] * emaxs[0] + p->normal[1] * emins[1] + p->normal[2] * emins[2];
			dist2 = p->normal[0] * emins[0] + p->normal[1] * emaxs[1] + p->normal[2] * emaxs[2];
			break;
		case 7:
			dist1 = p->normal[0] * emins[0] + p->normal[1] * emins[1] + p->normal[2] * emins[2];
			dist2 = p->normal[0] * emaxs[0] + p->normal[1] * emaxs[1] + p->normal[2] * emaxs[2];
			break;
		default:
			dist1 = dist2 = 0; // shut up compiler
			assert( 0 );
			break;
	}

	sides = 0;
	if( dist1 >= p->dist ) {
		sides = 1;
	}
	if( dist2 < p->dist ) {
		sides |= 2;
	}

#if 0
	assert( sides != 0 );
#endif

	return sides;
}

/*
* SignbitsForPlane
*/
int SignbitsForPlane( const cplane_t *out ) {
	int bits, j;

	// for fast box on planeside test

	bits = 0;
	for( j = 0; j < 3; j++ ) {
		if( out->normal[j] < 0 ) {
			bits |= 1 << j;
		}
	}
	return bits;
}

/*
* PlaneTypeForNormal
*/
int PlaneTypeForNormal( const vec3_t normal ) {
	// NOTE: should these have an epsilon around 1.0?
	if( normal[0] >= 1.0 ) {
		return PLANE_X;
	}
	if( normal[1] >= 1.0 ) {
		return PLANE_Y;
	}
	if( normal[2] >= 1.0 ) {
		return PLANE_Z;
	}

	return PLANE_NONAXIAL;
}

/*
* CategorizePlane
*
* A slightly more complex version of SignbitsForPlane and PlaneTypeForNormal,
* which also tries to fix possible floating point glitches (like -0.00000 cases)
*/
void CategorizePlane( cplane_t *plane ) {
	int i;

	plane->signbits = 0;
	plane->type = PLANE_NONAXIAL;
	for( i = 0; i < 3; i++ ) {
		if( plane->normal[i] < 0 ) {
			plane->signbits |= 1 << i;
			if( plane->normal[i] == -1.0f ) {
				plane->signbits = ( 1 << i );
				VectorClear( plane->normal );
				plane->normal[i] = -1.0f;
				break;
			}
		} else if( plane->normal[i] == 1.0f ) {
			plane->type = i;
			plane->signbits = 0;
			VectorClear( plane->normal );
			plane->normal[i] = 1.0f;
			break;
		}
	}
}

void PlaneFromPoints( const float *v1, const float *v2, const float *v3, cplane_t *plane ) {
	vec3_t _1To2, _1To3;

	VectorSubtract( v2, v1, _1To2 );
	VectorSubtract( v3, v1, _1To3 );
	CrossProduct( _1To3, _1To2, plane->normal );
	VectorNormalize( plane->normal );
	plane->dist = DotProduct( v1, plane->normal );
}

#define PLANE_NORMAL_EPSILON    0.00001
#define PLANE_DIST_EPSILON  0.01

/*
* ComparePlanes
*/
bool ComparePlanes( const vec3_t p1normal, vec_t p1dist, const vec3_t p2normal, vec_t p2dist ) {
	if( fabs( p1normal[0] - p2normal[0] ) < PLANE_NORMAL_EPSILON
		&& fabs( p1normal[1] - p2normal[1] ) < PLANE_NORMAL_EPSILON
		&& fabs( p1normal[2] - p2normal[2] ) < PLANE_NORMAL_EPSILON
		&& fabs( p1dist - p2dist ) < PLANE_DIST_EPSILON ) {
		return true;
	}

	return false;
}

/*
* SnapVector
*/
void SnapVector( vec3_t normal ) {
	int i;

	for( i = 0; i < 3; i++ ) {
		if( fabs( normal[i] - 1 ) < PLANE_NORMAL_EPSILON ) {
			VectorClear( normal );
			normal[i] = 1;
			break;
		}
		if( fabs( normal[i] - -1 ) < PLANE_NORMAL_EPSILON ) {
			VectorClear( normal );
			normal[i] = -1;
			break;
		}
	}
}

/*
* SnapPlane
*/
void SnapPlane( vec3_t normal, vec_t *dist ) {
	SnapVector( normal );

	if( fabs( *dist - Q_rint( *dist ) ) < PLANE_DIST_EPSILON ) {
		*dist = Q_rint( *dist );
	}
}

// Copied from Q2 re-release code
void VectorSlerp( const float *a, float t, const float *b, float *d ) {
	assert( std::fabs( VectorLengthFast( a ) - 1.0f < 1e-3f ) );
	assert( std::fabs( VectorLengthFast( b ) - 1.0f < 1e-3f ) );
	const float dot = DotProduct( a, b );

	float aFactor, bFactor;
	if( std::fabs( dot ) <= 0.9995f ) [[likely]] {
		const float ang       = std::acos( dot );
		const float sinOmega  = std::sin( ang );
		const float sinAOmega = std::sin( ( 1.0f - t ) * ang );
		const float sinBOmega = std::sin( t * ang );
		aFactor               = sinAOmega / sinOmega;
		bFactor               = sinBOmega / sinOmega;
	} else {
		aFactor = 1.0f - t;
		bFactor = t;
	}

	d[0] = aFactor * a[0] + bFactor * b[0];
	d[1] = aFactor * a[1] + bFactor * b[1];
	d[2] = aFactor * a[2] + bFactor * b[2];
}

template <unsigned N>
static void createBoundingKDopForSphere( float *mins, float *maxs, const float *center, float radius ) {
	// Contrary to a bounding box, we can't just shift all bounds by the center value.
	// TODO: Optimize, derive equations or at least remove redundant ops.
	// This is just to make things working for an evaluation of the concept.
	BoundingDopBuilder<N> boundingDopBuilder;

	const float halfRadius = 0.5f * radius;
	for( unsigned coordNum = 0; coordNum < 3; ++coordNum ) {
		vec3_t pt { center[0], center[1], center[2] };
		pt[coordNum] = +halfRadius + center[coordNum];
		boundingDopBuilder.addPoint( pt );
		pt[coordNum] = -halfRadius + center[coordNum];
		boundingDopBuilder.addPoint( pt );
	}

	for( unsigned i = 0; i < 8; ++i ) {
		constexpr const float invSqrt3 = 1.0f / 1.732051f;
		const float signs[2] { -1.0f, +1.0f };
		const vec3_t pt {
			signs[( i >> 2 ) & 1] * ( halfRadius * invSqrt3 ) + center[0],
			signs[( i >> 1 ) & 1] * ( halfRadius * invSqrt3 ) + center[1],
			signs[( i >> 0 ) & 1] * ( halfRadius * invSqrt3 ) + center[2]
		};
		boundingDopBuilder.addPoint( pt );
	}

	if constexpr( N == 26 ) {
		constexpr const float invSqrt2 = 1.0f / 1.414214f;
		const vec3_t bevelTouchPoints[] {
			{ +invSqrt2, +invSqrt2, 0.0f }, { -invSqrt2, +invSqrt2, 0.0f },
			{ +invSqrt2, -invSqrt2, 0.0f }, { -invSqrt2, -invSqrt2, 0.0f },

			{ +invSqrt2, 0.0f, +invSqrt2 }, { -invSqrt2, 0.0f, +invSqrt2 },
			{ +invSqrt2, 0.0f, -invSqrt2 }, { -invSqrt2, 0.0f, -invSqrt2 },

			{ 0.0f, +invSqrt2, +invSqrt2 }, { 0.0f, -invSqrt2, +invSqrt2 },
			{ 0.0f, +invSqrt2, -invSqrt2 }, { 0.0f, -invSqrt2, -invSqrt2 }
		};
		for( const vec3_t &refPt: bevelTouchPoints ) {
			const vec3_t pt {
				halfRadius * refPt[0] + center[0],
				halfRadius * refPt[1] + center[1],
				halfRadius * refPt[2] + center[2]
			};
			boundingDopBuilder.addPoint( pt );
		}
	}

	boundingDopBuilder.storeTo( mins, maxs );
}

void createBounding14DopForSphere( float *mins, float *maxs, const float *center, float radius ) {
	createBoundingKDopForSphere<14>( mins, maxs, center, radius );
}

void createBounding26DopForSphere( float *mins, float *maxs, const float *center, float radius ) {
	createBoundingKDopForSphere<26>( mins, maxs, center, radius );
}

bool BoundsAndSphereIntersect( const vec3_t mins, const vec3_t maxs, const vec3_t centre, float radius ) {
	int i;
	float dmin = 0;
	float radius2 = radius * radius;

	for( i = 0; i < 3; i++ ) {
		if( centre[i] < mins[i] ) {
			dmin += ( centre[i] - mins[i] ) * ( centre[i] - mins[i] );
		} else if( centre[i] > maxs[i] ) {
			dmin += ( centre[i] - maxs[i] ) * ( centre[i] - maxs[i] );
		}
	}

	if( dmin <= radius2 ) {
		return true;
	}
	return false;
}

/*
* RadiusFromBounds
*/
float RadiusFromBounds( const vec3_t mins, const vec3_t maxs ) {
	int i;
	vec3_t corner;

	for( i = 0; i < 3; i++ ) {
		corner[i] = fabs( mins[i] ) > fabs( maxs[i] ) ? fabs( mins[i] ) : fabs( maxs[i] );
	}

	return VectorLength( corner );
}

int Q_log2( int val ) {
	int answer = 0;
	while( val >>= 1 )
		answer++;
	return answer;
}

void Matrix3_Identity( mat3_t m ) {
	int i, j;

	for( i = 0; i < 3; i++ )
		for( j = 0; j < 3; j++ )
			if( i == j ) {
				m[i * 3 + j] = 1.0;
			} else {
				m[i * 3 + j] = 0.0;
			}
}

void Matrix3_Copy( const mat3_t m1, mat3_t m2 ) {
	memcpy( m2, m1, sizeof( mat3_t ) );
}

bool Matrix3_Compare( const mat3_t m1, const mat3_t m2 ) {
	int i;

	for( i = 0; i < 9; i++ )
		if( m1[i] != m2[i] ) {
			return false;
		}
	return true;
}

void Matrix3_Multiply( const mat3_t m1, const mat3_t m2, mat3_t out ) {
	out[0] = m1[0] * m2[0] + m1[1] * m2[3] + m1[2] * m2[6];
	out[1] = m1[0] * m2[1] + m1[1] * m2[4] + m1[2] * m2[7];
	out[2] = m1[0] * m2[2] + m1[1] * m2[5] + m1[2] * m2[8];
	out[3] = m1[3] * m2[0] + m1[4] * m2[3] + m1[5] * m2[6];
	out[4] = m1[3] * m2[1] + m1[4] * m2[4] + m1[5] * m2[7];
	out[5] = m1[3] * m2[2] + m1[4] * m2[5] + m1[5] * m2[8];
	out[6] = m1[6] * m2[0] + m1[7] * m2[3] + m1[8] * m2[6];
	out[7] = m1[6] * m2[1] + m1[7] * m2[4] + m1[8] * m2[7];
	out[8] = m1[6] * m2[2] + m1[7] * m2[5] + m1[8] * m2[8];
}

void Matrix3_Transpose( const mat3_t in, mat3_t out ) {
	out[0] = in[0];
	out[4] = in[4];
	out[8] = in[8];

	out[1] = in[3];
	out[2] = in[6];
	out[3] = in[1];
	out[5] = in[7];
	out[6] = in[2];
	out[7] = in[5];
}

void Matrix3_FromAngles( const vec3_t angles, mat3_t m ) {
	AngleVectors( angles, &m[AXIS_FORWARD], &m[AXIS_RIGHT], &m[AXIS_UP] );
}

void Matrix3_ToAngles( const mat3_t m, vec3_t angles ) {
	vec_t c;
	vec_t pitch, yaw, roll;

	pitch = -asin( m[2] );
	c = cos( pitch );
	if( fabs( c ) > 5 * 10e-6 ) {     // Gimball lock?
		// no
		c = 1.0f / c;
		pitch = RAD2DEG( pitch );
		yaw = RAD2DEG( atan2( m[1] * c, m[0] * c ) );
		roll = RAD2DEG( atan2( -m[5] * c, m[8] * c ) );
	} else {
		// yes
		pitch = m[2] > 0 ? -90 : 90;
		yaw = RAD2DEG( atan2( m[3], -m[4] ) );
		roll = 180;
	}

	angles[PITCH] = pitch;
	angles[YAW] = yaw;
	angles[ROLL] = roll;
}

static void Matrix3_RotateBySinCos( const mat3_t in, float s, float c, vec_t x, vec_t y, vec_t z, mat3_t out ) {
	mat3_t t, b;
	vec_t mc = 1 - c, t1, t2;

	t[0] = ( x * x * mc ) + c;
	t[4] = ( y * y * mc ) + c;
	t[8] = ( z * z * mc ) + c;

	t1 = y * x * mc;
	t2 = z * s;
	t[1] = t1 + t2;
	t[3] = t1 - t2;

	t1 = x * z * mc;
	t2 = y * s;
	t[2] = t1 - t2;
	t[6] = t1 + t2;

	t1 = y * z * mc;
	t2 = x * s;
	t[5] = t1 + t2;
	t[7] = t1 - t2;

	Matrix3_Copy( in, b );
	Matrix3_Multiply( b, t, out );
}

void Matrix3_Rotate( const mat3_t in, vec_t angle, vec_t x, vec_t y, vec_t z, mat3_t out ) {
	const float radians = DEG2RAD( angle );
	vec_t s = std::sin( radians );
	vec_t c = std::cos( radians );

	Matrix3_RotateBySinCos( in, s, c, x, y, z, out );
}

void Matrix3_ForRotationOfDirs( const float *fromDir, const float *toDir, mat3_t out ) {
	assert( std::fabs( VectorLength( fromDir ) - 1.0f ) < 0.001f );
	assert( std::fabs( VectorLength( toDir ) - 1.0f ) < 0.001f );

	const float dot = DotProduct( fromDir, toDir );
	if( dot > +0.999f ) [[unlikely]] {
		VectorSet( out + 0, +1.0f, +0.0f, +0.0f );
		VectorSet( out + 3, +0.0f, +1.0f, +0.0f );
		VectorSet( out + 6, +0.0f, +0.0f, +1.0f );
		return;
	}

	if( dot < -0.999f ) [[unlikely]] {
		VectorSet( out + 0, -1.0f, +0.0f, +0.0f );
		VectorSet( out + 3, +0.0f, -1.0f, +0.0f );
		VectorSet( out + 6, +0.0f, +0.0f, -1.0f );
		return;
	}

	vec3_t axis;
	CrossProduct( fromDir, toDir, axis );

	const float c	 = dot;
	const float sqrS =  1.0f - dot * dot ;
	const float rcpS = Q_RSqrt( sqrS );
    // as cos2(x)+sin2(x)=1, sin2(x)=1-cos2(x), yielding two solutions:
    // sin(x)=sqrt(1-cos2(x)) V sin(x)=-sqrt
    // where the second one is the correct one
	const float s    = - sqrS * rcpS;

	VectorScale( axis, rcpS, axis );

	Matrix3_RotateBySinCos( axis_identity, s, c, axis[0], axis[1], axis[2], out );
}

void Matrix3_FromPoints( const vec3_t v1, const vec3_t v2, const vec3_t v3, mat3_t m ) {
	float d;

	m[6] = ( v1[1] - v2[1] ) * ( v3[2] - v2[2] ) - ( v1[2] - v2[2] ) * ( v3[1] - v2[1] );
	m[7] = ( v1[2] - v2[2] ) * ( v3[0] - v2[0] ) - ( v1[0] - v2[0] ) * ( v3[2] - v2[2] );
	m[8] = ( v1[0] - v2[0] ) * ( v3[1] - v2[1] ) - ( v1[1] - v2[1] ) * ( v3[0] - v2[0] );
	VectorNormalizeFast( &m[6] );

	// this rotate and negate guarantees a vector not colinear with the original
	VectorSet( &m[3], m[8], -m[6], m[7] );
	d = -DotProduct( &m[3], &m[6] );
	VectorMA( &m[3], d, &m[6], &m[3] );
	VectorNormalizeFast( &m[3] );
	CrossProduct( &m[3], &m[6], &m[0] );
}

void Matrix3_Normalize( mat3_t m ) {
	VectorNormalize( &m[0] );
	VectorNormalize( &m[3] );
	VectorNormalize( &m[6] );
}

//============================================================================

void Quat_Identity( quat_t q ) {
	q[0] = 0;
	q[1] = 0;
	q[2] = 0;
	q[3] = 1;
}

void Quat_Copy( const quat_t q1, quat_t q2 ) {
	q2[0] = q1[0];
	q2[1] = q1[1];
	q2[2] = q1[2];
	q2[3] = q1[3];
}

void Quat_Quat3( const vec3_t in, quat_t out ) {
	out[0] = in[0];
	out[1] = in[1];
	out[2] = in[2];
	out[3] = -sqrt( Q_max( 1 - in[0] * in[0] - in[1] * in[1] - in[2] * in[2], 0.0f ) );
}

bool Quat_Compare( const quat_t q1, const quat_t q2 ) {
	if( q1[0] != q2[0] || q1[1] != q2[1] || q1[2] != q2[2] || q1[3] != q2[3] ) {
		return false;
	}
	return true;
}

void Quat_Conjugate( const quat_t q1, quat_t q2 ) {
	q2[0] = -q1[0];
	q2[1] = -q1[1];
	q2[2] = -q1[2];
	q2[3] = q1[3];
}

vec_t Quat_DotProduct( const quat_t q1, const quat_t q2 ) {
	return ( q1[0] * q2[0] + q1[1] * q2[1] + q1[2] * q2[2] + q1[3] * q2[3] );
}

vec_t Quat_Normalize( quat_t q ) {
	vec_t length;

	length = q[0] * q[0] + q[1] * q[1] + q[2] * q[2] + q[3] * q[3];
	if( length != 0 ) {
		vec_t ilength = 1.0 / sqrt( length );
		q[0] *= ilength;
		q[1] *= ilength;
		q[2] *= ilength;
		q[3] *= ilength;
	}

	return length;
}

vec_t Quat_Inverse( const quat_t q1, quat_t q2 ) {
	Quat_Conjugate( q1, q2 );

	return Quat_Normalize( q2 );
}

void Quat_FromMatrix3( const mat3_t m, quat_t q ) {
	vec_t tr, s;

	tr = m[0] + m[4] + m[8];
	if( tr > 0.00001 ) {
		s = sqrt( tr + 1.0 );
		q[3] = s * 0.5; s = 0.5 / s;
		q[0] = ( m[7] - m[5] ) * s;
		q[1] = ( m[2] - m[6] ) * s;
		q[2] = ( m[3] - m[1] ) * s;
	} else {
		int i, j, k;

		i = 0;
		if( m[4] > m[i * 3 + i] ) {
			i = 1;
		}
		if( m[8] > m[i * 3 + i] ) {
			i = 2;
		}
		j = ( i + 1 ) % 3;
		k = ( i + 2 ) % 3;

		s = sqrt( m[i * 3 + i] - ( m[j * 3 + j] + m[k * 3 + k] ) + 1.0 );

		q[i] = s * 0.5; if( s != 0.0 ) {
			s = 0.5 / s;
		}
		q[j] = ( m[j * 3 + i] + m[i * 3 + j] ) * s;
		q[k] = ( m[k * 3 + i] + m[i * 3 + k] ) * s;
		q[3] = ( m[k * 3 + j] - m[j * 3 + k] ) * s;
	}

	Quat_Normalize( q );
}

void Quat_Multiply( const quat_t q1, const quat_t q2, quat_t out ) {
	out[0] = q1[3] * q2[0] + q1[0] * q2[3] + q1[1] * q2[2] - q1[2] * q2[1];
	out[1] = q1[3] * q2[1] + q1[1] * q2[3] + q1[2] * q2[0] - q1[0] * q2[2];
	out[2] = q1[3] * q2[2] + q1[2] * q2[3] + q1[0] * q2[1] - q1[1] * q2[0];
	out[3] = q1[3] * q2[3] - q1[0] * q2[0] - q1[1] * q2[1] - q1[2] * q2[2];
}

static void Quat_LLerp( const quat_t q1, const quat_t q2, vec_t t, quat_t out ) {
	vec_t scale0, scale1;

	scale0 = 1.0 - t;
	scale1 = t;

	out[0] = scale0 * q1[0] + scale1 * q2[0];
	out[1] = scale0 * q1[1] + scale1 * q2[1];
	out[2] = scale0 * q1[2] + scale1 * q2[2];
	out[3] = scale0 * q1[3] + scale1 * q2[3];
}

void Quat_Lerp( const quat_t q1, const quat_t q2, vec_t t, quat_t out ) {
	quat_t p1;
	vec_t omega, cosom, sinom, scale0, scale1, sinsqr;

	if( Quat_Compare( q1, q2 ) ) {
		Quat_Copy( q1, out );
		return;
	}

	cosom = q1[0] * q2[0] + q1[1] * q2[1] + q1[2] * q2[2] + q1[3] * q2[3];
	if( cosom < 0.0 ) {
		cosom = -cosom;
		p1[0] = -q1[0]; p1[1] = -q1[1];
		p1[2] = -q1[2]; p1[3] = -q1[3];
	} else {
		p1[0] = q1[0]; p1[1] = q1[1];
		p1[2] = q1[2]; p1[3] = q1[3];
	}

	if( cosom >= 1.0 - 0.0001 ) {
		Quat_LLerp( q1, q2, t, out );
		return;
	}

	sinsqr = 1.0 - cosom * cosom;
	sinom = Q_RSqrt( sinsqr );
	omega = atan2( sinsqr * sinom, cosom );
	scale0 = sin( ( 1.0 - t ) * omega ) * sinom;
	scale1 = sin( t * omega ) * sinom;

	out[0] = scale0 * p1[0] + scale1 * q2[0];
	out[1] = scale0 * p1[1] + scale1 * q2[1];
	out[2] = scale0 * p1[2] + scale1 * q2[2];
	out[3] = scale0 * p1[3] + scale1 * q2[3];
}

void Quat_Vectors( const quat_t q, vec3_t f, vec3_t r, vec3_t u ) {
	vec_t wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2;

	x2 = q[0] + q[0]; y2 = q[1] + q[1]; z2 = q[2] + q[2];

	xx = q[0] * x2; yy = q[1] * y2; zz = q[2] * z2;
	f[0] = 1.0f - yy - zz; r[1] = 1.0f - xx - zz; u[2] = 1.0f - xx - yy;

	yz = q[1] * z2; wx = q[3] * x2;
	r[2] = yz - wx; u[1] = yz + wx;

	xy = q[0] * y2; wz = q[3] * z2;
	f[1] = xy - wz; r[0] = xy + wz;

	xz = q[0] * z2; wy = q[3] * y2;
	f[2] = xz + wy; u[0] = xz - wy;
}

void Quat_ToMatrix3( const quat_t q, mat3_t m ) {
	Quat_Vectors( q, &m[0], &m[3], &m[6] );
}

void Quat_TransformVector( const quat_t q, const vec3_t v, vec3_t out ) {
#if 0
	vec_t wx, wy, wz, xx, yy, yz, xy, xz, zz, x2, y2, z2;

	// 9 muls, 3 adds
	x2 = q[0] + q[0]; y2 = q[1] + q[1]; z2 = q[2] + q[2];
	xx = q[0] * x2; xy = q[0] * y2; xz = q[0] * z2;
	yy = q[1] * y2; yz = q[1] * z2; zz = q[2] * z2;
	wx = q[3] * x2; wy = q[3] * y2; wz = q[3] * z2;

	// 9 muls, 9 subs, 9 adds
	out[0] = ( 1.0f - yy - zz ) * v[0] + ( xy - wz ) * v[1] + ( xz + wy ) * v[2];
	out[1] = ( xy + wz ) * v[0] + ( 1.0f - xx - zz ) * v[1] + ( yz - wx ) * v[2];
	out[2] = ( xz - wy ) * v[0] + ( yz + wx ) * v[1] + ( 1.0f - xx - yy ) * v[2];
#else
	vec3_t t;

	CrossProduct( &q[0], v, t ); // 6 muls, 3 subs
	VectorScale( t, 2, t );      // 3 muls
	CrossProduct( &q[0], t, out );// 6 muls, 3 subs
	VectorMA( out, q[3], t, out );// 3 muls, 3 adds
#endif
}

void Quat_ConcatTransforms( const quat_t q1, const vec3_t v1, const quat_t q2, const vec3_t v2, quat_t q, vec3_t v ) {
	Quat_Multiply( q1, q2, q );
	Quat_TransformVector( q1, v2, v );
	v[0] += v1[0]; v[1] += v1[1]; v[2] += v1[2];
}

//============================================================================

void DualQuat_Identity( dualquat_t dq ) {
	Vector4Set( &dq[0], 0, 0, 0, 1 );
	Vector4Set( &dq[4], 0, 0, 0, 0 );
}

void DualQuat_Copy( const dualquat_t in, dualquat_t out ) {
	Quat_Copy( &in[0], &out[0] );
	Quat_Copy( &in[4], &out[4] );
}

static inline void DualQuat_SetVector( dualquat_t dq, const vec3_t v ) {
	// convert translation vector to dual part
	Vector4Set( &dq[4], 0.5f * ( v[0] * dq[3] + v[1] * dq[2] - v[2] * dq[1] ),
				0.5f * ( -v[0] * dq[2] + v[1] * dq[3] + v[2] * dq[0] ),
				0.5f * ( v[0] * dq[1] - v[1] * dq[0] + v[2] * dq[3] ),
				-0.5f * ( v[0] * dq[0] + v[1] * dq[1] + v[2] * dq[2] ) );
}

void DualQuat_FromAnglesAndVector( const vec3_t angles, const vec3_t v, dualquat_t out ) {
	mat3_t axis;

	AnglesToAxis( angles, axis );
	DualQuat_FromMatrix3AndVector( axis, v, out );
}

void DualQuat_FromMatrix3AndVector( const mat3_t m, const vec3_t v, dualquat_t out ) {
	// regular matrix to a quaternion
	Quat_FromMatrix3( m, out );

	// convert translation vector to dual part
	DualQuat_SetVector( out, v );
}

void DualQuat_FromQuatAndVector( const quat_t q, const vec3_t v, dualquat_t out ) {
	// regular quaternion, copy
	Quat_Copy( q, &out[0] );
	Quat_Normalize( &out[0] );

	// convert translation vector to dual part
	DualQuat_SetVector( out, v );
}

void DualQuat_FromQuat3AndVector( const vec3_t q, const vec3_t v, dualquat_t out ) {
	// regular quaternion, copy
	Quat_Quat3( q, &out[0] );
	Quat_Normalize( &out[0] );

	// convert translation vector to dual part
	DualQuat_SetVector( out, v );
}

void DualQuat_GetVector( const dualquat_t dq, vec3_t v ) {
	const vec_t *const real = &dq[0], *const dual = &dq[4];

	// translation vector
	CrossProduct( real, dual, v );
	VectorMA( v,  real[3], dual, v );
	VectorMA( v, -dual[3], real, v );
	VectorScale( v, 2, v );
}

void DualQuat_ToQuatAndVector( const dualquat_t dq, quat_t q, vec3_t v ) {
	// regular quaternion, copy
	Quat_Copy( &dq[0], q );

	// translation vector
	DualQuat_GetVector( dq, v );
}

void DualQuat_ToMatrix3AndVector( const dualquat_t dq, mat3_t m, vec3_t v ) {
	// convert quaternion to matrix
	Quat_ToMatrix3( &dq[0], m );

	// translation vector
	DualQuat_GetVector( dq, v );
}

void DualQuat_Invert( dualquat_t dq ) {
	vec_t s;
	vec_t *const real = &dq[0], *const dual = &dq[4];

	Quat_Conjugate( real, real );
	Quat_Conjugate( dual, dual );

	s = 2 * Quat_DotProduct( real, dual );
	dual[0] -= real[0] * s;
	dual[1] -= real[1] * s;
	dual[2] -= real[2] * s;
	dual[3] -= real[3] * s;
}

vec_t DualQuat_Normalize( dualquat_t dq ) {
	vec_t length;
	vec_t *const real = &dq[0], *const dual = &dq[4];

	length = real[0] * real[0] + real[1] * real[1] + real[2] * real[2] + real[3] * real[3];
	if( length != 0 ) {
		vec_t ilength = 1.0 / sqrt( length );
		Vector4Scale( real, ilength, real );
		Vector4Scale( dual, ilength, dual );
	}

	return length;
}

void DualQuat_Multiply( const dualquat_t dq1, const dualquat_t dq2, dualquat_t out ) {
	quat_t tq1, tq2;

	Quat_Multiply( &dq1[0], &dq2[4], tq1 );
	Quat_Multiply( &dq1[4], &dq2[0], tq2 );

	Quat_Multiply( &dq1[0], &dq2[0], &out[0] );
	Vector4Set( &out[4], tq1[0] + tq2[0], tq1[1] + tq2[1], tq1[2] + tq2[2], tq1[3] + tq2[3] );
}

void DualQuat_Lerp( const dualquat_t dq1, const dualquat_t dq2, vec_t t, dualquat_t out ) {
	int i, j;
	vec_t k;

	k = dq1[0] * dq2[0] + dq1[1] * dq2[1] + dq1[2] * dq2[2] + dq1[3] * dq2[3];
	k = k < 0 ? -t : t;
	t = 1.0 - t;

	for( i = 0; i < 4; i++ )
		out[i] = dq1[i] * t + dq2[i] * k;
	for( j = 4; j < 8; j++ )
		out[j] = dq1[j] * t + dq2[j] * k;

	Quat_Normalize( &out[0] );
}

/*
 * Distribution functions
 * Standard distribution is expected with mean=0, deviation=1
 */

vec_t LogisticCDF( vec_t x ) {
	return 1.0 / ( 1.0 + exp( -x ) );
}

vec_t LogisticPDF( vec_t x ) {
	float e;
	e = exp( -x );
	return e / ( ( 1.0 + e ) * ( 1.0 + e ) );
}

// closer approximation from
// http://www.wilmott.com/pdfs/090721_west.pdf
vec_t NormalCDF( vec_t x ) {
	float cumnorm = 0.0;
	float sign = 1.0;
	float build = 0;
	float e = 0.0;

	if( x < 0.0 ) {
		sign = -1.0;
	}
	x = fabs( x );
	if( x > 37.0 ) {
		cumnorm = 0.0;
	} else {
		e = expf( -( x * x ) * 0.5 );
		if( x < 7.07106781186547 ) {
			build = 3.52624965998911e-02 * x + 0.700383064443688;
			build = build * x + 6.37396220353165;
			build = build * x + 33.912866078383;
			build = build * x + 112.079291497871;
			build = build * x + 221.213596169931;
			build = build * x + 220.206867912376;
			cumnorm = e * build;
			build = 8.8388347683184e-02;
			build = build * x + 16.064177579207;
			build = build * x + 86.7807322029461;
			build = build * x + 296.564248779674;
			build = build * x + 637.333633378831;
			build = build * x + 793.826512519948;
			build = build * x + 440.413735824752;
			cumnorm /= build;
		} else {
			build = x + 0.65;
			build = x + 4 / build;
			build = x + 3 / build;
			build = x + 2 / build;
			build = x + 1 / build;
			cumnorm = e / build / 2.506628274631;
		}
	}

	if( sign > 0 ) {
		cumnorm = 1 - cumnorm;
	}

	return cumnorm;
}

vec_t NormalPDF( vec_t x ) {
	return exp( ( -x * x ) / 2 ) / sqrt( 2.0 * M_PI );
}
