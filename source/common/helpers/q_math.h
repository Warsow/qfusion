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

#ifndef GAME_QMATH_H
#define GAME_QMATH_H

#include <common/helpers/q_arch.h>
#include <common/helpers/wswbasicmath.h>

//==============================================================
//
//MATHLIB
//
//==============================================================

enum {
	PITCH = 0,      // up / down
	YAW = 1,        // left / right
	ROLL = 2        // fall over
};

enum {
	FORWARD = 0,
	RIGHT = 1,
	UP = 2
};

enum {
	AXIS_FORWARD = 0,
	AXIS_RIGHT = 3,
	AXIS_UP = 6
};

typedef float vec_t;
typedef vec_t vec2_t[2];
typedef vec_t vec3_t[3];
typedef vec_t vec4_t[4];
typedef vec_t vec5_t[5];

typedef vec_t quat_t[4];

typedef vec_t mat3_t[9];

typedef vec_t dualquat_t[8];

typedef uint8_t byte_vec4_t[4];

// 0-2 are axial planes
#define PLANE_X     0
#define PLANE_Y     1
#define PLANE_Z     2
#define PLANE_NONAXIAL  3

// cplane_t structure
typedef struct cplane_s {
	vec3_t normal;
	float dist;
	short type;                 // for fast side tests
	short signbits;             // signx + (signy<<1) + (signz<<1)
} cplane_t;

constexpr const vec3_t vec3_origin { 0, 0, 0 };
constexpr const mat3_t axis_identity { 1, 0, 0, 0, 1, 0, 0, 0, 1 };
constexpr const quat_t quat_identity { 0, 0, 0, 1 };

constexpr const vec4_t colorBlack    { 0, 0, 0, 1 };
constexpr const vec4_t colorRed      { 1, 0, 0, 1 };
constexpr const vec4_t colorGreen    { 0, 1, 0, 1 };
constexpr const vec4_t colorBlue     { 0.25, 0.25, 1, 1 };
constexpr const vec4_t colorYellow   { 1, 1, 0, 1 };
constexpr const vec4_t colorOrange   { 1, 0.5, 0, 1 };
constexpr const vec4_t colorMagenta  { 0.75, 0.25, 1, 1 };
constexpr const vec4_t colorCyan     { 0, 1, 1, 1 };
constexpr const vec4_t colorWhite    { 1, 1, 1, 1 };
constexpr const vec4_t colorLtGrey   { 0.75, 0.75, 0.75, 1 };
constexpr const vec4_t colorMdGrey   { 0.5, 0.5, 0.5, 1 };
constexpr const vec4_t colorDkGrey   { 0.25, 0.25, 0.25, 1 };

#define MAX_S_COLORS 10

extern const vec4_t color_table[MAX_S_COLORS];

#ifndef M_PI
#define M_PI       3.14159265358979323846   // matches value in gcc v2 math.h
#endif

#ifndef M_TWOPI
#define M_TWOPI    6.28318530717958647692
#endif

#define DEG2RAD( a ) ( a * M_PI ) / 180.0F
#define RAD2DEG( a ) ( a * 180.0F ) / M_PI


// returns b clamped to [a..c] range
//#define bound(a,b,c) (max((a), min((b), (c))))

#ifndef Q_max
#define Q_max( a, b ) ( ( a ) > ( b ) ? ( a ) : ( b ) )
#endif

#ifndef Q_min
#define Q_min( a, b ) ( ( a ) < ( b ) ? ( a ) : ( b ) )
#endif

#define bound( a, b, c ) ( ( a ) >= ( c ) ? ( a ) : ( b ) < ( a ) ? ( a ) : ( b ) > ( c ) ? ( c ) : ( b ) )

// clamps a (must be lvalue) to [b..c] range
#define Q_clamp( a, b, c ) ( ( b ) >= ( c ) ? ( a ) = ( b ) : ( a ) < ( b ) ? ( a ) = ( b ) : ( a ) > ( c ) ? ( a ) = ( c ) : ( a ) )

#define clamp_low( a, low ) ( ( a ) = ( a ) < ( low ) ? ( low ) : ( a ) )
#define clamp_high( a, high ) ( ( a ) = ( a ) > ( high ) ? ( high ) : ( a ) )

#define random()    ( ( rand() & 0x7fff ) / ( (float)0x7fff ) )  // 0..1
#define brandom( a, b )    ( ( a ) + random() * ( ( b ) - ( a ) ) )                // a..b
#define crandom()   brandom( -1, 1 )                           // -1..1

int Q_rand( int *seed );
#define Q_random( seed )      ( ( Q_rand( seed ) & 0x7fff ) / ( (float)0x7fff ) )    // 0..1
#define Q_brandom( seed, a, b ) ( ( a ) + Q_random( seed ) * ( ( b ) - ( a ) ) )                      // a..b
#define Q_crandom( seed )     Q_brandom( seed, -1, 1 )

#if ( defined ( __i386__ ) || defined ( __x86_64__ ) || defined( _M_IX86 ) || defined( _M_AMD64 ) || defined( _M_X64 ) )

static inline float Q_RSqrt( float number ) {
	assert( number >= 0 );
	return _mm_cvtss_f32( _mm_rsqrt_ss( _mm_set_ss( number ) ) );
}

static inline float Q_Rcp( float number ) {
	return _mm_cvtss_f32( _mm_rcp_ss( _mm_set_ss( number ) ) );
}

static inline float Q_Sqrt( float number ) {
	assert( number >= 0 );

	// jal : The expression a * rsqrt(b) is intended as a higher performance alternative to a / sqrt(b).
	// The two expressions are comparably accurate, but do not compute exactly the same value in every case.
	// For example, a * rsqrt(a*a + b*b) can be just slightly greater than 1, in rare cases.

	// We have to check for zero or else a NAN is produced (0 times infinity)

	// Force an eager computation of result for further branch-less selection.
	// Note that modern x86 handle infinities/NANs without penalties.
	// Supplying zeroes is rare anyway.
	float maybeSqrt = number * _mm_cvtss_f32( _mm_rsqrt_ss( _mm_set_ss( number ) ) );

	// There's unfortunately no CMOV for the actually used SSE2+ f.p. instruction set.
	// Here's a workaround that uses the integer CMOV.
	// As far as we know MSVC 2019 produces an expected code (while a 2-elements array approach generates a branch).

	// Copy the value bits to an integer
	int32_t maybeSqrtAsInt = *( (int32_t *)&maybeSqrt );
	// This should be a CMOV. The UCOMISS instruction sets the needed flags.
	// Note that in case when `number` bits are also converted to an integer the sign bit should be masked.
	int32_t resultAsInt = number ? maybeSqrtAsInt : 0;
	// Return the value bits as a float
	return *( (float *)( &resultAsInt ) );
}

#else

static inline float Q_RSqrt( float number ) {
	assert( number >= 0 );
	return 1.0f / sqrtf( number );
}

static inline float Q_Rcp( float number ) {
	return 1.0f / number;
}

static inline float Q_Sqrt( float number ) {
	assert( number >= 0 );
	return sqrtf( number );
}

#endif

int Q_log2( int val );

int Q_bitcount( int v );

#define DotProduct( x, y )     ( ( x )[0] * ( y )[0] + ( x )[1] * ( y )[1] + ( x )[2] * ( y )[2] )
#define CrossProduct( v1, v2, cross ) ( ( cross )[0] = ( v1 )[1] * ( v2 )[2] - ( v1 )[2] * ( v2 )[1], ( cross )[1] = ( v1 )[2] * ( v2 )[0] - ( v1 )[0] * ( v2 )[2], ( cross )[2] = ( v1 )[0] * ( v2 )[1] - ( v1 )[1] * ( v2 )[0] )

#define PlaneDiff( point, plane ) ( ( ( plane )->type < 3 ? ( point )[( plane )->type] : DotProduct( ( point ), ( plane )->normal ) ) - ( plane )->dist )

#define VectorSubtract( a, b, c )   ( ( c )[0] = ( a )[0] - ( b )[0], ( c )[1] = ( a )[1] - ( b )[1], ( c )[2] = ( a )[2] - ( b )[2] )
#define VectorAdd( a, b, c )        ( ( c )[0] = ( a )[0] + ( b )[0], ( c )[1] = ( a )[1] + ( b )[1], ( c )[2] = ( a )[2] + ( b )[2] )
#define VectorCopy( a, b )     ( ( b )[0] = ( a )[0], ( b )[1] = ( a )[1], ( b )[2] = ( a )[2] )
#define VectorClear( a )      ( ( a )[0] = ( a )[1] = ( a )[2] = 0 )
#define VectorNegate( a, b )       ( ( b )[0] = -( a )[0], ( b )[1] = -( a )[1], ( b )[2] = -( a )[2] )
#define VectorSet( v, x, y, z )   ( ( v )[0] = ( x ), ( v )[1] = ( y ), ( v )[2] = ( z ) )
#define VectorAvg( a, b, c )        ( ( c )[0] = ( ( a )[0] + ( b )[0] ) * 0.5f, ( c )[1] = ( ( a )[1] + ( b )[1] ) * 0.5f, ( c )[2] = ( ( a )[2] + ( b )[2] ) * 0.5f )
#define VectorMA( a, b, c, d )       ( ( d )[0] = ( a )[0] + ( b ) * ( c )[0], ( d )[1] = ( a )[1] + ( b ) * ( c )[1], ( d )[2] = ( a )[2] + ( b ) * ( c )[2] )
#define VectorCompare( v1, v2 )    ( ( v1 )[0] == ( v2 )[0] && ( v1 )[1] == ( v2 )[1] && ( v1 )[2] == ( v2 )[2] )
#define VectorLengthSquared( v )    ( DotProduct( ( v ), ( v ) ) )
#define VectorLength( v )     ( sqrt( VectorLengthSquared( v ) ) )
#define VectorInverse( v )    ( ( v )[0] = -( v )[0], ( v )[1] = -( v )[1], ( v )[2] = -( v )[2] )
#define VectorLerp( a, c, b, v )     ( ( v )[0] = ( a )[0] + ( c ) * ( ( b )[0] - ( a )[0] ), ( v )[1] = ( a )[1] + ( c ) * ( ( b )[1] - ( a )[1] ), ( v )[2] = ( a )[2] + ( c ) * ( ( b )[2] - ( a )[2] ) )
#define VectorScale( in, scale, out ) ( ( out )[0] = ( in )[0] * ( scale ), ( out )[1] = ( in )[1] * ( scale ), ( out )[2] = ( in )[2] * ( scale ) )

#define DistanceSquared( v1, v2 ) ( ( ( v1 )[0] - ( v2 )[0] ) * ( ( v1 )[0] - ( v2 )[0] ) + ( ( v1 )[1] - ( v2 )[1] ) * ( ( v1 )[1] - ( v2 )[1] ) + ( ( v1 )[2] - ( v2 )[2] ) * ( ( v1 )[2] - ( v2 )[2] ) )
#define Distance( v1, v2 ) ( sqrt( DistanceSquared( v1, v2 ) ) )

#define VectorLengthFast( v )     ( Q_Sqrt( DotProduct( ( v ), ( v ) ) ) )
#define DistanceFast( v1, v2 )     ( Q_Sqrt( DistanceSquared( v1, v2 ) ) )

#define Vector2Set( v, x, y )     ( ( v )[0] = ( x ), ( v )[1] = ( y ) )
#define Vector2Copy( a, b )    ( ( b )[0] = ( a )[0], ( b )[1] = ( a )[1] )
#define Vector2Scale( in, scale, out ) ( ( out )[0] = ( in )[0] * ( scale ), ( out )[1] = ( in )[1] * ( scale ) )
#define Vector2Avg( a, b, c )       ( ( c )[0] = ( ( ( a[0] ) + ( b[0] ) ) * 0.5f ), ( c )[1] = ( ( ( a[1] ) + ( b[1] ) ) * 0.5f ) )

#define Vector4Set( v, a, b, c, d )   ( ( v )[0] = ( a ), ( v )[1] = ( b ), ( v )[2] = ( c ), ( v )[3] = ( d ) )
#define Vector4Clear( a )     ( ( a )[0] = ( a )[1] = ( a )[2] = ( a )[3] = 0 )
#define Vector4Copy( a, b )    ( ( b )[0] = ( a )[0], ( b )[1] = ( a )[1], ( b )[2] = ( a )[2], ( b )[3] = ( a )[3] )
#define Vector4Lerp( a, c, b, v )     ( ( v )[0] = ( a )[0] + ( c ) * ( ( b )[0] - ( a )[0] ), ( v )[1] = ( a )[1] + ( c ) * ( ( b )[1] - ( a )[1] ), ( v )[2] = ( a )[2] + ( c ) * ( ( b )[2] - ( a )[2] ), ( v )[3] = ( a )[3] + ( c ) * ( ( b )[3] - ( a )[3] ) )
#define Vector4Scale( in, scale, out )      ( ( out )[0] = ( in )[0] * scale, ( out )[1] = ( in )[1] * scale, ( out )[2] = ( in )[2] * scale, ( out )[3] = ( in )[3] * scale )
#define Vector4Add( a, b, c )       ( ( c )[0] = ( ( ( ( a )[0] ) + ( ( b )[0] ) ) ), ( c )[1] = ( ( ( ( a )[1] ) + ( ( b )[1] ) ) ), ( c )[2] = ( ( ( ( a )[2] ) + ( ( b )[2] ) ) ), ( c )[3] = ( ( ( ( a )[3] ) + ( ( b )[3] ) ) ) )
#define Vector4Avg( a, b, c )       ( ( c )[0] = ( ( ( ( a )[0] ) + ( ( b )[0] ) ) * 0.5f ), ( c )[1] = ( ( ( ( a )[1] ) + ( ( b )[1] ) ) * 0.5f ), ( c )[2] = ( ( ( ( a )[2] ) + ( b )[2] ) ) * 0.5f ), ( c )[3] = ( ( ( ( a )[3] ) + ( ( b )[3] ) ) * 0.5f ) )
#define Vector4Negate( a, b )      ( ( b )[0] = -( a )[0], ( b )[1] = -( a )[1], ( b )[2] = -( a )[2], ( b )[3] = -( a )[3] )
#define Vector4Inverse( v )         ( ( v )[0] = -( v )[0], ( v )[1] = -( v )[1], ( v )[2] = -( v )[2], ( v )[3] = -( v )[3] )
#define DotProduct4( x, y )    ( ( x )[0] * ( y )[0] + ( x )[1] * ( y )[1] + ( x )[2] * ( y )[2] + ( x )[3] * ( y )[3] )

void VectorSlerp( const float *a, float c, const float *b, float *dest );

inline float VectorNormalize( float *v ) {
	const float squareLen = v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
	float len = 0.0f;

	// TODO: [[likely]]
	if( squareLen > 0 ) {
		len = std::sqrt( squareLen );
		float invLen = 1.0f / len;
		v[0] *= invLen;
		v[1] *= invLen;
		v[2] *= invLen;
	}

	return len;
}

inline float VectorNormalize2( const vec3_t v, vec3_t out ) {
	const float squareLen = v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
	float len = 0.0f;

	// TODO: [[likely]]
	if( squareLen > 0 ) {
		len = std::sqrt( squareLen );
		const float invLen = 1.0f / len;
		out[0] = v[0] * invLen;
		out[1] = v[1] * invLen;
		out[2] = v[2] * invLen;
	} else {
		VectorClear( out );
	}

	return len;
}

inline float Vector4Normalize( float *v ) {
	const float squareLen = v[0] * v[0] + v[1] * v[1] + v[2] * v[2] + v[3] * v[3];
	float len = 0.0f;

	// TODO: [[likely]]
	if( squareLen > 0.0f ) {
		len = std::sqrt( squareLen );
		const float invLen = 1.0f / len;
		v[0] *= invLen;
		v[1] *= invLen;
		v[2] *= invLen;
		v[3] *= invLen;
	}

	return len;
}

inline void VectorNormalizeFast( vec3_t v ) {
	const float invLen = Q_RSqrt( DotProduct( v, v ) );
	v[0] *= invLen;
	v[1] *= invLen;
	v[2] *= invLen;
}

inline void VectorReflect( const float *v, const float *n, float dist, float *out ) {
	const float d = -2 * ( DotProduct( v, n ) - dist );
	VectorMA( v, d, n, out );
}

static inline void ClearBounds( vec3_t mins, vec3_t maxs ) {
	mins[0] = mins[1] = mins[2] = 99999;
	maxs[0] = maxs[1] = maxs[2] = -99999;
}

static inline void AddPointToBounds( const vec3_t v, vec3_t mins, vec3_t maxs ) {
	// A sane compiler should produce a branchless code.
	// We should also use SIMD intrinsics manually if a code path is really hot.
	mins[0] = v[0] < mins[0] ? v[0] : mins[0];
	mins[1] = v[1] < mins[1] ? v[1] : mins[1];
	mins[2] = v[2] < mins[2] ? v[2] : mins[2];
	maxs[0] = v[0] > maxs[0] ? v[0] : maxs[0];
	maxs[1] = v[1] > maxs[1] ? v[1] : maxs[1];
	maxs[2] = v[2] > maxs[2] ? v[2] : maxs[2];
}

float RadiusFromBounds( const vec3_t mins, const vec3_t maxs );

static inline bool BoundsIntersect( const vec3_t mins1, const vec3_t maxs1, const vec3_t mins2, const vec3_t maxs2 ) {
	// Though collision code is likely to use its own BoundsIntersect() version
	// optimized even further, this code is fine too for the rest of the code base

#ifndef WSW_USE_SSE2
	return (bool)( mins1[0] <= maxs2[0] && mins1[1] <= maxs2[1] && mins1[2] <= maxs2[2] &&
				   maxs1[0] >= mins2[0] && maxs1[1] >= mins2[1] && maxs1[2] >= mins2[2] );

#else
	__m128 xmmMins1 = _mm_set_ps( mins1[0], mins1[1], mins1[2], 0 );
	__m128 xmmMaxs1 = _mm_set_ps( maxs1[0], maxs1[1], maxs1[2], 1 );
	__m128 xmmMins2 = _mm_set_ps( mins2[0], mins2[1], mins2[2], 0 );
	__m128 xmmMaxs2 = _mm_set_ps( maxs2[0], maxs2[1], maxs2[2], 1 );

	__m128 cmp1 = _mm_cmpge_ps( xmmMins1, xmmMaxs2 );
	__m128 cmp2 = _mm_cmpge_ps( xmmMins2, xmmMaxs1 );
	return !_mm_movemask_ps( _mm_or_ps( cmp1, cmp2 ) );
#endif
}

class alignas( 16 ) BoundsBuilder {
#ifdef WSW_USE_SSE2
	__m128 m_mins { _mm_setr_ps( +99999, +99999, +99999, 0 ) };
	__m128 m_maxs { _mm_setr_ps( -99999, -99999, -99999, 1 ) };
#else
	vec3_t m_mins { +99999, +99999, +99999 };
	vec3_t m_maxs { -99999, -99999, -99999 };
#endif

#ifdef _DEBUG
	bool m_touched { false };
#endif

	void markAsTouched() {
#ifdef _DEBUG
		m_touched = true;
#endif
	}

	void checkTouched() const {
#ifdef _DEBUG
		assert( m_touched );
#endif
	}
public:
	void addPoint( const float *__restrict p ) {
#ifdef WSW_USE_SSE2
		__m128 xmmP = _mm_setr_ps( p[0], p[1], p[2], 0 );
		m_mins = _mm_min_ps( xmmP, m_mins );
		m_maxs = _mm_max_ps( xmmP, m_maxs );
#else
		m_mins[0] = wsw::min( p[0], m_mins[0] );
		m_maxs[0] = wsw::max( p[0], m_maxs[0] );
		m_mins[1] = wsw::min( p[1], m_mins[1] );
		m_maxs[1] = wsw::max( p[1], m_maxs[1] );
		m_mins[2] = wsw::min( p[2], m_mins[2] );
		m_maxs[2] = wsw::max( p[2], m_maxs[2] );
#endif

		markAsTouched();
	}

#ifdef WSW_USE_SSE2
	void addPoint( __m128 p ) {
		m_mins = _mm_min_ps( p, m_mins );
		m_maxs = _mm_max_ps( p, m_maxs );

		markAsTouched();
	}
#endif

#ifdef WSW_USE_SSE41
	[[deprecated("use addMinsAndMaxs*, this subroutine is only for compatibility with old tests")]]
	void addPointsIfNoCmpMaskDWordSet( __m128 mask, __m128 pt1, __m128 pt2 ) {
		addMinsAndMaxsIfNoCmpMaskDWordSet( mask, _mm_min_ps( pt1, pt2 ), _mm_max_ps( pt1, pt2 ) );
	}

	void addMinsAndMaxsIfNoCmpMaskDWordSet( __m128 mask, __m128 givenMins, __m128 givenMaxs ) {
		assert( !_mm_movemask_ps( _mm_cmpgt_ps( givenMins, givenMaxs ) ) );

		// Find a minimal element of the mask (SSE4.1).
		// This command can only operate on 16-bit words but is fast.
		// The mask is supposed to contain 4 32-bit dwords, each of them either 0 or ~0.

		// We have to flip bits as only the MIN comnand is available
		__m128i flippedMask = _mm_cmpeq_epi32( _mm_castps_si128( mask ), _mm_setzero_si128() );
		__m128i minMaskElem = _mm_minpos_epu16( flippedMask );
		// The byte contains either 0xFF or 0x0.
		// Broadcast it to all lanes (Supplementary SSE3).
		__m128 blendMask = _mm_castsi128_ps( _mm_shuffle_epi8( minMaskElem, _mm_setzero_si128() ) );
		// The blend mask now must contain either all-ones or all-zeros.

		//int values[4];
		//_mm_store_ps( (float *)values, blendMask );
		//printf( "Blend mask: %x %x %x %x\n", values[0], values[1], values[2], values[3] );

		// This should give a hint to load old stuff to registers
		__m128 oldMins = m_mins;
		__m128 oldMaxs = m_maxs;

		// Select either old mins/maxs or given ones using the blend mask
		__m128 selectedMins = _mm_blendv_ps( oldMins, givenMins, blendMask );
		__m128 selectedMaxs = _mm_blendv_ps( oldMaxs, givenMaxs, blendMask );

		// Write comparison results back to memory
		m_mins = _mm_min_ps( oldMins, selectedMins );
		m_maxs = _mm_max_ps( oldMaxs, selectedMaxs );

		// TODO: Shouldn't be marked if some mask bits were set
		markAsTouched();
	}
#endif

	void storeTo( float *__restrict mins, float *__restrict maxs ) const {
		checkTouched();

#ifdef WSW_USE_SSE2
		alignas( 16 ) vec4_t tmpMins, tmpMaxs;
		_mm_store_ps( tmpMins, m_mins );
		_mm_store_ps( tmpMaxs, m_maxs );
		VectorCopy( tmpMins, mins );
		VectorCopy( tmpMaxs, maxs );
#else
		VectorCopy( m_mins, mins );
		VectorCopy( m_maxs, maxs );
#endif
	}

	void storeToWithAddedEpsilon( float *__restrict mins, float *__restrict maxs, float epsilon = 1.0f ) const {
		checkTouched();

#ifdef WSW_USE_SSE2
		alignas( 16 ) vec4_t tmpMins, tmpMaxs;
		__m128 xmmEpsilon = _mm_setr_ps( epsilon, epsilon, epsilon, 0.0f );
		__m128 expandedMins = _mm_sub_ps( m_mins, xmmEpsilon );
		__m128 expandedMaxs = _mm_add_ps( m_maxs, xmmEpsilon );
		_mm_store_ps( tmpMins, expandedMins );
		_mm_store_ps( tmpMaxs, expandedMaxs );
		VectorCopy( tmpMins, mins );
		VectorCopy( tmpMaxs, maxs );
#else
		mins[0] = m_mins[0] - epsilon;
		maxs[0] = m_maxs[0] + epsilon;
		mins[1] = m_mins[1] - epsilon;
		maxs[1] = m_maxs[1] + epsilon;
		mins[2] = m_mins[2] - epsilon;
		maxs[2] = m_maxs[2] + epsilon;
#endif
	}

#ifdef WSW_USE_SSE2
	void storeTo( __m128 *__restrict mins, __m128 *__restrict maxs ) {
		checkTouched();
		*mins = m_mins;
		*maxs = m_maxs;
	}

	void storeToWithAddedEpsilon( __m128 *__restrict mins, __m128 *__restrict maxs, float epsilon = 1.0f ) {
		checkTouched();
		__m128 xmmEpsilon = _mm_setr_ps( epsilon, epsilon, epsilon, 0.0f );
		*mins = _mm_sub_ps( m_mins, xmmEpsilon );
		*maxs = _mm_add_ps( m_maxs, xmmEpsilon );
	}
#endif
};

template <unsigned N>
class BoundingDopBuilder {
	static_assert( ( N == 14 || N == 26 ) );

public:
	BoundingDopBuilder() noexcept {
		constexpr float minsVal = std::numeric_limits<float>::max();
		constexpr float maxsVal = std::numeric_limits<float>::lowest();

		Vector4Set( m_mins + 0, minsVal, minsVal, minsVal, minsVal );
		Vector4Set( m_maxs + 0, maxsVal, maxsVal, maxsVal, maxsVal );

		if constexpr( N == 26 ) {
			Vector4Set( m_mins + 4, minsVal, minsVal, minsVal, minsVal );
			Vector4Set( m_maxs + 4, maxsVal, maxsVal, maxsVal, maxsVal );
			Vector4Set( m_mins + 8, minsVal, minsVal, minsVal, minsVal );
			Vector4Set( m_maxs + 8, maxsVal, maxsVal, maxsVal, maxsVal );
			m_mins[12] = minsVal, m_maxs[12] = maxsVal;
		} else {
			VectorSet( m_mins + 4, minsVal, minsVal, minsVal );
			VectorSet( m_maxs + 4, maxsVal, maxsVal, maxsVal );
		}
	}

	explicit BoundingDopBuilder( const float *initialPoint ) noexcept {
		float *const __restrict mins = m_mins;
		float *const __restrict maxs = m_maxs;

		const float x = initialPoint[0], y = initialPoint[1], z = initialPoint[2];
		mins[0] = x, maxs[0] = x, mins[1] = y, maxs[1] = y, mins[2] = z, maxs[2] = z;

		const float d3 = x + y + z, d4 = -x + y + z;
		mins[3] = d3, maxs[3] = d3, mins[4] = d4, maxs[4] = d4;

		const float d5 = -x - y + z, d6 = x - y + z;
		mins[5] = d5, maxs[5] = d5, mins[6] = d6, maxs[6] = d6;

		if constexpr( N == 26 ) {
			const float d7 = x + y, d8 = x + z, d9 = y + z;
			mins[7] = d7, maxs[7] = d7, mins[8] = d8, maxs[8] = d8, mins[9] = d9, maxs[9] = d9;
			
			const float d10 = x - y, d11 = x - z, d12 = -y + z;
			mins[10] = d10, maxs[10] = d10, mins[11] = d11, maxs[11] = d11, mins[12] = d12, maxs[12] = d12;
		}

		markAsTouched();
	}

	void addPoint( const float *p ) noexcept {
		float *const __restrict mins = m_mins;
		float *const __restrict maxs = m_maxs;

		const float x = p[0], y = p[1], z = p[2];
		mins[0] = wsw::min( mins[0], x ), maxs[0] = wsw::max( maxs[0], x );
		mins[1] = wsw::min( mins[1], y ), maxs[1] = wsw::max( maxs[1], y );
		mins[2] = wsw::min( mins[2], z ), maxs[2] = wsw::max( maxs[2], z );

		const float d3 = x + y + z, d4 = -x + y + z;
		mins[3] = wsw::min( mins[3], d3 ), maxs[3] = wsw::max( maxs[3], d3 );
		mins[4] = wsw::min( mins[4], d4 ), maxs[4] = wsw::max( maxs[4], d4 );

		const float d5 = -x - y + z, d6 = x - y + z;
		mins[5] = wsw::min( mins[5], d5 ), maxs[5] = wsw::max( maxs[5], d5 );
		mins[6] = wsw::min( mins[6], d6 ), maxs[6] = wsw::max( maxs[6], d6 );

		if constexpr( N == 26 ) {
			const float d7 = x + y, d8 = x + z, d9 = y + z;
			mins[7] = wsw::min( mins[7], d7 ), maxs[7] = wsw::max( maxs[7], d7 );
			mins[8] = wsw::min( mins[8], d8 ), maxs[8] = wsw::max( maxs[8], d8 );
			mins[9] = wsw::min( mins[9], d9 ), maxs[9] = wsw::max( maxs[9], d9 );

			const float d10 = x - y, d11 = x - z, d12 = -y + z;
			mins[10] = wsw::min( mins[10], d10 ), maxs[10] = wsw::max( maxs[10], d10 );
			mins[11] = wsw::min( mins[11], d11 ), maxs[11] = wsw::max( maxs[11], d11 );
			mins[12] = wsw::min( mins[12], d12 ), maxs[12] = wsw::max( maxs[12], d12 );
		}

		markAsTouched();
	}

	void addOtherDop( float *mins, float *maxs ) noexcept {
		// TODO: Use SIMD if it's on a speed-critical path
		for( unsigned i = 0; i < N / 2; ++i ) {
			m_mins[i] = wsw::min( m_mins[i], mins[i] );
			m_maxs[i] = wsw::max( m_maxs[i], maxs[i] );
		}

		markAsTouched();
	}

	void storeTo( float *mins, float *maxs ) noexcept {
		checkTouched();

		Vector4Copy( m_mins + 0, mins + 0 );
		Vector4Copy( m_maxs + 0, maxs + 0 );

		if constexpr( N == 26 ) {
			Vector4Copy( m_mins + 4, mins + 4 );
			Vector4Copy( m_maxs + 4, maxs + 4 );
			Vector4Copy( m_mins + 8, mins + 8 );
			Vector4Copy( m_maxs + 8, maxs + 8 );
			mins[12] = mins[13] = mins[14] = mins[15] = m_mins[12];
			maxs[12] = maxs[13] = maxs[14] = maxs[15] = m_maxs[12];
		} else {
			VectorCopy( m_mins + 4, mins + 4 );
			VectorCopy( m_maxs + 4, maxs + 4 );
			mins[7] = m_mins[6], maxs[7] = m_maxs[6];
		}
	}
private:
	void markAsTouched() {
#ifdef _DEBUG
		m_touched = true;
#endif
	}

	void checkTouched() const {
#ifdef _DEBUG
		assert( m_touched );
#endif
	}

	float m_mins[( N == 14 ) ? 7 : 13];
	float m_maxs[( N == 14 ) ? 7 : 13];

	#ifdef _DEBUG
	bool m_touched { false };
#endif
};

void createBounding14DopForSphere( float *mins, float *maxs, const float *center, float radius );
void createBounding26DopForSphere( float *mins, float *maxs, const float *center, float radius );

bool BoundsAndSphereIntersect( const vec3_t mins, const vec3_t maxs, const vec3_t centre, float radius );

#define NUMVERTEXNORMALS    162
extern const vec3_t kPredefinedDirs[NUMVERTEXNORMALS];

int DirToByte( const vec3_t dir );
int DirToByteFast( const vec3_t dir );
void ByteToDir( int b, vec3_t dir );
inline bool IsValidDirByte( int byte ) {
	return (unsigned)byte < (unsigned)NUMVERTEXNORMALS;
}

void NormToLatLong( const vec3_t normal, float latlong[2] );

void MakeNormalVectors( const vec3_t forward, vec3_t right, vec3_t up );

inline void AngleVectors( const vec3_t angles, vec3_t forward, vec3_t right, vec3_t up ) {
	constexpr float deg2Rad = (float)( M_PI ) / 180.0f;

	const float yaw     = deg2Rad * angles[YAW];
	const float sinYaw  = sinf( yaw );
	const float cosYaw  = cosf( yaw );

	const float pitch    = deg2Rad * angles[PITCH];
	const float sinPitch = sinf( pitch );
	const float cosPitch = cosf( pitch );

	const bool calcRight = right != nullptr;
	const bool calcUp    = up != nullptr;

	float sinRoll = 0.0f;
	float cosRoll = 1.0f;
	if( const float rollDegrees = angles[ROLL]; rollDegrees != 0.0f ) [[unlikely]] {
		if( calcRight | calcUp ) {
			float roll = deg2Rad * rollDegrees;
			sinRoll    = sinf( roll );
			cosRoll    = cosf( roll );
		}
	}

	if( forward ) {
		forward[0] = cosPitch * cosYaw;
		forward[1] = cosPitch * sinYaw;
		forward[2] = -sinPitch;
	}

	if( calcRight ) {
		float t  = sinRoll * sinPitch;
		right[0] = ( -1 * t * cosYaw + -1 * cosRoll * -sinYaw );
		right[1] = ( -1 * t * sinYaw + -1 * cosRoll * cosYaw );
		right[2] = -1 * sinRoll * cosPitch;
	}

	if( calcUp ) {
		float t = cosRoll * sinPitch;
		up[0]   = ( t * cosYaw + -sinRoll * -sinYaw );
		up[1]   = ( t * sinYaw + -sinRoll * cosYaw );
		up[2]   = cosRoll * cosPitch;
	}
}

inline float LerpAngle( float a2, float a1, float frac ) {
	if( a1 - a2 > 180 ) {
		a1 -= 360;
	}
	if( a1 - a2 < -180 ) {
		a1 += 360;
	}
	return a2 + frac * ( a1 - a2 );
}

inline float AngleNormalize360( float angle ) {
	return ( 360.0 / 65536 ) * ( (int)( angle * ( 65536 / 360.0 ) ) & 65535 );
}

inline float AngleNormalize180( float angle ) {
	angle = AngleNormalize360( angle );
	if( angle > 180.0 ) {
		angle -= 360.0;
	}
	return angle;
}

inline float AngleDelta( float angle1, float angle2 ) {
	return AngleNormalize180( angle1 - angle2 );
}

inline float anglemod( float a ) {
	return ( 360.0 / 65536 ) * ( (int)( a * ( 65536 / 360.0 ) ) & 65535 );
}

inline void VecToAngles( const vec3_t vec, vec3_t angles ) {
	float yaw, pitch;

	if( vec[1] == 0 && vec[0] == 0 ) {
		yaw = 0;
		if( vec[2] > 0 ) {
			pitch = 90;
		} else {
			pitch = 270;
		}
	} else {
		if( vec[0] ) {
			yaw = RAD2DEG( atan2( vec[1], vec[0] ) );
		} else if( vec[1] > 0 ) {
			yaw = 90;
		} else {
			yaw = -90;
		}
		if( yaw < 0 ) {
			yaw += 360;
		}

		float forward = sqrt( vec[0] * vec[0] + vec[1] * vec[1] );
		pitch = RAD2DEG( atan2( vec[2], forward ) );
		if( pitch < 0 ) {
			pitch += 360;
		}
	}

	angles[PITCH] = -pitch;
	angles[YAW] = yaw;
	angles[ROLL] = 0;
}

inline void AnglesToAxis( const vec3_t angles, mat3_t axis ) {
	AngleVectors( angles, &axis[0], &axis[3], &axis[6] );
	VectorInverse( &axis[3] );
}

// similar to MakeNormalVectors but for rotational matrices
// (FIXME: weird, what's the diff between this and MakeNormalVectors?)
inline void NormalVectorToAxis( const vec3_t forward, mat3_t axis ) {
	VectorCopy( forward, &axis[0] );
	if( forward[0] || forward[1] ) {
		VectorSet( &axis[3], forward[1], -forward[0], 0 );
		VectorNormalize( &axis[3] );
		CrossProduct( &axis[0], &axis[3], &axis[6] );
	} else {
		VectorSet( &axis[3], 1, 0, 0 );
		VectorSet( &axis[6], 0, 1, 0 );
	}
}

void BuildBoxPoints( vec3_t p[8], const vec3_t org, const vec3_t mins, const vec3_t maxs );

int BoxOnPlaneSide( const vec3_t emins, const vec3_t emaxs, const struct cplane_s *plane );

vec_t ColorNormalize( const vec_t *in, vec_t *out );

#define ColorGrayscale( c ) ( 0.299 * ( c )[0] + 0.587 * ( c )[1] + 0.114 * ( c )[2] )

float CalcFov( float fov_x, float width, float height );
void AdjustFov( float *fov_x, float *fov_y, float width, float height, bool lock_x );

#define Q_sign( x ) ( ( x ) < 0 ? -1 : ( ( x ) > 0 ? 1 : 0 ) )
#define Q_rint( x ) ( ( x ) < 0 ? ( (int)( ( x ) - 0.5f ) ) : ( (int)( ( x ) + 0.5f ) ) )

int SignbitsForPlane( const cplane_t *out );
int PlaneTypeForNormal( const vec3_t normal );
void CategorizePlane( cplane_t *plane );
void PlaneFromPoints( const float *v1, const float *v2, const float *v3, cplane_t *plane );
inline void PlaneFromPoints( const vec3_t verts[3], cplane_t *plane ) {
	PlaneFromPoints( verts[0], verts[1], verts[2], plane );
}

bool ComparePlanes( const vec3_t p1normal, vec_t p1dist, const vec3_t p2normal, vec_t p2dist );
void SnapVector( vec3_t normal );
void SnapPlane( vec3_t normal, vec_t *dist );

#define BOX_ON_PLANE_SIDE( emins, emaxs, p )  \
	( ( ( p )->type < 3 ) ?                       \
	  (                                       \
		  ( ( p )->dist <= ( emins )[( p )->type] ) ?  \
		  1                               \
		  :                                   \
		  (                                   \
			  ( ( p )->dist >= ( emaxs )[( p )->type] ) ? \
			  2                           \
			  :                               \
			  3                           \
		  )                                   \
	  )                                       \
	  :                                       \
	  BoxOnPlaneSide( ( emins ), ( emaxs ), ( p ) ) )

void ProjectPointOntoPlane( vec3_t dst, const vec3_t p, const vec3_t normal );
void PerpendicularVector( vec3_t dst, const vec3_t src );
void RotatePointAroundVector( vec3_t dst, const vec3_t dir, const vec3_t point, float degrees );
void ProjectPointOntoVector( const vec3_t point, const vec3_t vStart, const vec3_t vDir, vec3_t vProj );

float LinearMovementWithOvershoot( vec_t start, vec_t end, float duration, float freq, float decay, float t );

void Matrix3_Identity( mat3_t m );
void Matrix3_Copy( const mat3_t m1, mat3_t m2 );
bool Matrix3_Compare( const mat3_t m1, const mat3_t m2 );
void Matrix3_Multiply( const mat3_t m1, const mat3_t m2, mat3_t out );

inline void Matrix3_TransformVector( const mat3_t m, const vec3_t v, vec3_t out ) {
	out[0] = m[0] * v[0] + m[1] * v[1] + m[2] * v[2];
	out[1] = m[3] * v[0] + m[4] * v[1] + m[5] * v[2];
	out[2] = m[6] * v[0] + m[7] * v[1] + m[8] * v[2];
}

void Matrix3_Transpose( const mat3_t in, mat3_t out );
void Matrix3_FromAngles( const vec3_t angles, mat3_t m );
void Matrix3_ToAngles( const mat3_t m, vec3_t angles );
void Matrix3_Rotate( const mat3_t in, vec_t angle, vec_t x, vec_t y, vec_t z, mat3_t out );
void Matrix3_ForRotationOfDirs( const float *fromDir, const float *toDir, mat3_t out );
void Matrix3_FromPoints( const vec3_t v1, const vec3_t v2, const vec3_t v3, mat3_t m );
void Matrix3_Normalize( mat3_t m );

inline void Matrix3_Rotate( const mat3_t in, float angle, const vec3_t axisDir, mat3_t out ) {
	Matrix3_Rotate( in, angle, axisDir[0], axisDir[1], axisDir[2], out );
}

void Quat_Identity( quat_t q );
void Quat_Copy( const quat_t q1, quat_t q2 );
void Quat_Quat3( const vec3_t in, quat_t out );
bool Quat_Compare( const quat_t q1, const quat_t q2 );
void Quat_Conjugate( const quat_t q1, quat_t q2 );
vec_t Quat_DotProduct( const quat_t q1, const quat_t q2 );
vec_t Quat_Normalize( quat_t q );
vec_t Quat_Inverse( const quat_t q1, quat_t q2 );
void Quat_Multiply( const quat_t q1, const quat_t q2, quat_t out );
void Quat_Lerp( const quat_t q1, const quat_t q2, vec_t t, quat_t out );
void Quat_Vectors( const quat_t q, vec3_t f, vec3_t r, vec3_t u );
void Quat_ToMatrix3( const quat_t q, mat3_t m );
void Quat_FromMatrix3( const mat3_t m, quat_t q );
void Quat_TransformVector( const quat_t q, const vec3_t v, vec3_t out );
void Quat_ConcatTransforms( const quat_t q1, const vec3_t v1, const quat_t q2, const vec3_t v2, quat_t q, vec3_t v );

void DualQuat_Identity( dualquat_t dq );
void DualQuat_Copy( const dualquat_t in, dualquat_t out );
void DualQuat_FromAnglesAndVector( const vec3_t angles, const vec3_t v, dualquat_t out );
void DualQuat_FromMatrix3AndVector( const mat3_t m, const vec3_t v, dualquat_t out );
void DualQuat_FromQuatAndVector( const quat_t q, const vec3_t v, dualquat_t out );
void DualQuat_FromQuat3AndVector( const vec3_t q, const vec3_t v, dualquat_t out );
void DualQuat_GetVector( const dualquat_t dq, vec3_t v );
void DualQuat_ToQuatAndVector( const dualquat_t dq, quat_t q, vec3_t v );
void DualQuat_ToMatrix3AndVector( const dualquat_t dq, mat3_t m, vec3_t v );
void DualQuat_Invert( dualquat_t dq );
vec_t DualQuat_Normalize( dualquat_t dq );
void DualQuat_Multiply( const dualquat_t dq1, const dualquat_t dq2, dualquat_t out );
void DualQuat_Lerp( const dualquat_t dq1, const dualquat_t dq2, vec_t t, dualquat_t out );

vec_t LogisticCDF( vec_t x );
vec_t LogisticPDF( vec_t x );
vec_t NormalCDF( vec_t x );
vec_t NormalPDF( vec_t x );

template <unsigned N, typename Value = int>
class alignas( 16 ) MovingAverage {
	static_assert( std::is_integral_v<Value> );
	static_assert( std::is_signed_v<Value> );

	uint64_t m_total { 0 };
	int64_t m_queueSum {0 };
	Value m_values[N + 1] {};
	unsigned m_head { N };
	unsigned m_tail { 0 };
public:
	void clear() {
		memset( m_values, 0, sizeof( m_values ) );
		m_tail = 0;
		m_head = N;
		m_queueSum = 0;
	}

	void add( Value value ) {
		m_total++;
		const Value tailValue = m_values[m_tail];
		m_values[m_head] = value;
		m_head = ( m_head + 1 ) % ( N + 1 );
		m_tail = ( m_tail + 1 ) % ( N + 1 );
		m_queueSum -= tailValue;
		m_queueSum += value;
	}

	[[nodiscard]]
	auto hasSufficentNumberOfSamples() const -> uint64_t { return m_total >= N; }
	[[nodiscard]]
	auto avg() const -> Value { return (Value)( m_queueSum / N ); }
};

#endif // GAME_QMATH_H
