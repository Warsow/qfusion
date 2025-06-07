/*
Copyright (C) 2023 Chasseur de bots

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

/*
The simplex noise code is borrowed from the public domain reference code that accompanies the article
"Simplex noise demystified" by Stefan Gustavson. The code underwent our adaptation/optimization.
*/

/*
Cell bitmask tricks are borrowed from FastNoise2:

MIT License

Copyright (c) 2020 Jordan Peck

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

/*
The concept of bitangent noise is borrowed from https://atyuwen.github.io/posts/bitangent-noise/
*/

/*
The vector hashing function for the Voronoi noise is distributed under these terms:

MIT License

Copyright (c) 2019 A_Riccardi

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
 */

#include "noise.h"

#include <forward_list>

static const vec3_t kGradTable[] = {
	{ +1, +1, +0 }, { -1, +1, +0 }, { +1, -1, +0 }, { -1, -1, +0 },
	{ +1, +0, +1 }, { -1, +0, +1 }, { +1, +0, -1 }, { -1, +0, -1 },
	{ +0, +1, +1 }, { +0, -1, +1 }, { +0, +1, -1 }, { +0, -1, -1 },
};

// Supplementary gradient for quick computation of curl noise
static const vec3_t kAltGradTable[] = {
	{ -0.278317, +0.885368, -0.372375 }, { +0.290462, -0.186040, -0.938627 }, { +0.644209, -0.764222, 0.030982 },
	{ -0.364755, -0.772175, -0.520288 }, { +0.743090, +0.620761, +0.249945 }, { +0.085619, 0.576237, -0.812785 },
	{ -0.098739, -0.785326, +0.611158 }, { -0.367344, -0.491556, -0.789577 }, { -0.108043, 0.726982, -0.678104 },
	{ +0.058054, -0.105136, +0.992762 }, { -0.210956, +0.608395, -0.765083 }, { +0.227787, -0.396772, 0.889204 },
};

static_assert( std::size( kGradTable ) == 12 && std::size( kAltGradTable ) == 12 );

static const uint8_t kIndexTable[] = {
	151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30, 69, 142, 8, 99, 37, 240,
	21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11, 32, 57, 177, 33, 88,
	237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77, 146, 158, 231, 83,
	111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25, 63, 161, 1, 216,
	80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109, 198, 173, 186,
	3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227, 47, 16, 58, 17,
	182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167, 43, 172, 9, 129,
	22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34, 242, 193, 238,
	210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199, 106, 157, 184,
	84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243, 141, 128, 195,
	78, 66, 215, 61, 156, 180, 151, 160, 137, 91, 90, 15, 131, 13, 201, 95, 96, 53, 194, 233, 7, 225, 140, 36, 103, 30,
	69, 142, 8, 99, 37, 240, 21, 10, 23, 190, 6, 148, 247, 120, 234, 75, 0, 26, 197, 62, 94, 252, 219, 203, 117, 35, 11,
	32, 57, 177, 33, 88, 237, 149, 56, 87, 174, 20, 125, 136, 171, 168, 68, 175, 74, 165, 71, 134, 139, 48, 27, 166, 77,
	146, 158, 231, 83, 111, 229, 122, 60, 211, 133, 230, 220, 105, 92, 41, 55, 46, 245, 40, 244, 102, 143, 54, 65, 25,
	63, 161, 1, 216, 80, 73, 209, 76, 132, 187, 208, 89, 18, 169, 200, 196, 135, 130, 116, 188, 159, 86, 164, 100, 109,
	198, 173, 186, 3, 64, 52, 217, 226, 250, 124, 123, 5, 202, 38, 147, 118, 126, 255, 82, 85, 212, 207, 206, 59, 227,
	47, 16, 58, 17, 182, 189, 28, 42, 223, 183, 170, 213, 119, 248, 152, 2, 44, 154, 163, 70, 221, 153, 101, 155, 167,
	43, 172, 9, 129, 22, 39, 253, 19, 98, 108, 110, 79, 113, 224, 232, 178, 185, 112, 104, 218, 246, 97, 228, 251, 34,
	242, 193, 238, 210, 144, 12, 191, 179, 162, 241, 81, 51, 145, 235, 249, 14, 239, 107, 49, 192, 214, 31, 181, 199,
	106, 157, 184, 84, 204, 176, 115, 121, 50, 45, 127, 4, 150, 254, 138, 236, 205, 93, 222, 114, 67, 29, 24, 72, 243,
	141, 128, 195, 78, 66, 215, 61, 156, 180,
};

static_assert( std::size( kIndexTable ) == 512 );

[[nodiscard]]
wsw_forceinline auto dot2( const float *gp, float x, float y ) -> float {
	return gp[0] * x + gp[1] * y;
}

[[nodiscard]]
wsw_forceinline auto dot3( const float *gp, float x, float y, float z ) -> float {
	return gp[0] * x + gp[1] * y + gp[2] * z;
}

auto calcSimplexNoise2D( float x, float y ) -> float {
	// Sanity checks, make sure we're specifying a point within the world bounds
	assert( x >= -(float)std::numeric_limits<uint16_t>::max() && x <= (float)std::numeric_limits<uint16_t>::max() );
	assert( y >= -(float)std::numeric_limits<uint16_t>::max() && y <= (float)std::numeric_limits<uint16_t>::max() );

	constexpr float F2 = 0.36602540378443f; // 0.5f * ( std::sqrtf(3.0f) - 1.0f );
	constexpr float G2 = 0.21132486540518f; // ( 3 - std::sqrtf(3.0f)) / 6;

	// Skew coords
	const float f = ( x + y ) * F2;
	const float i = std::floor( x + f );
	const float j = std::floor( y + f );

	// Unskew
	const float g = ( i + j ) * G2;

	const float cellOriginX = i - g;
	const float cellOriginY = j - g;

	const float x0 = x - cellOriginX;
	const float y0 = y - cellOriginY;

	// masks to find which cell were in
	const bool iShift = x0 >= y0;
	const bool jShift = !iShift;

	// cell vertex offsets
	const float x1 = x0 - (float)iShift + G2;
	const float y1 = y0 - (float)jShift + G2;
	const float x2 = x0 - 1 + 2 * G2;
	const float y2 = y0 - 1 + 2 * G2;

	// wrap cells
	const int iValue = (int)i & 255;
	const int jValue = (int)j & 255;

	// add up cell influences
	const float t0 = 0.5f - x0 * x0 - y0 * y0;
	const float t1 = 0.5f - x1 * x1 - y1 * y1;
	const float t2 = 0.5f - x2 * x2 - y2 * y2;

	float accum = 0.0f;
	if( t0 > 0.0f ) {
		const int gradIndex = ( kIndexTable[iValue + kIndexTable[jValue]] ) % 12;
		accum += wsw::square( wsw::square( t0 ) ) * dot2( kGradTable[gradIndex], x0, y0 );
	}
	if( t1 > 0.0f ) {
		const int gradIndex = ( kIndexTable[iValue + iShift + kIndexTable[jValue + jShift]] ) % 12;
		accum += wsw::square( wsw::square( t1 ) ) * dot2( kGradTable[gradIndex], x1, y1 );
	}
	if( t2 > 0.0f ) {
		const int gradIndex = ( kIndexTable[iValue + 1 + kIndexTable[jValue + 1]] ) % 12;
		accum += wsw::square( wsw::square( t2 ) ) * dot2( kGradTable[gradIndex], x2, y2 );
	}

	// scale to [-1, +1], then to [0, 1]
	const float result = 0.5f * ( 1.0f + 70.0f * accum );
	assert( result >= 0.0f && result <= 1.0f );
	return result;
}

[[nodiscard]]
wsw_forceinline auto calcGradIndex3D( int iValue, int iShift, int jValue, int jShift, int kValue, int kShift ) -> int {
	return ( kIndexTable[iValue + iShift + kIndexTable[jValue + jShift + kIndexTable[kValue + kShift]]] ) % 12;
}

static void addPointContribution( float t, int gradIndex, float x, float y, float z,
								  float *__restrict noiseGrad, float *__restrict altNoiseGrad ) {
	const float w2  = t * t;
	const float w4  = w2 * w2;
	const float w3  = w2 * t;
	const float dwA = -8.0f * w3 * dot3( kGradTable[gradIndex], x, y, z );
	const float dwB = -8.0f * w3 * dot3( kAltGradTable[gradIndex], x, y, z );
	const vec3_t r0 = { x, y, z };
	VectorMA( noiseGrad, w4, kGradTable[gradIndex], noiseGrad );
	VectorMA( noiseGrad, dwA, r0, noiseGrad );
	VectorMA( altNoiseGrad, w4, kAltGradTable[gradIndex], altNoiseGrad );
	VectorMA( altNoiseGrad, dwB, r0, altNoiseGrad );
}

// Try inlining it forcefully into the final subroutines so there's no redundant calls
template <typename Result>
[[nodiscard]]
wsw_forceinline auto calcSimplexNoise3DImpl( float givenX, float givenY, float givenZ ) -> Result {
	// Sanity checks, make sure we're specifying a point within the world bounds
	assert( givenX >= -(float)std::numeric_limits<uint16_t>::max() && givenX <= (float)std::numeric_limits<uint16_t>::max() );
	assert( givenY >= -(float)std::numeric_limits<uint16_t>::max() && givenY <= (float)std::numeric_limits<uint16_t>::max() );
	assert( givenZ >= -(float)std::numeric_limits<uint16_t>::max() && givenZ <= (float)std::numeric_limits<uint16_t>::max() );

	constexpr float F3 = 1.0f / 3.0f;
	constexpr float G3 = 1.0f / 6.0f;

	// Skew coords
	const float f = ( givenX + givenY + givenZ ) * F3;

	const float pointI = std::floor( givenX + f );
	const float pointJ = std::floor( givenY + f );
	const float pointK = std::floor( givenZ + f );

	// Unskew
	const float g = ( pointI + pointJ + pointK ) * G3;

	const float cellOriginX = pointI - g;
	const float cellOriginY = pointJ - g;
	const float cellOriginZ = pointK - g;

	const float x0 = givenX - cellOriginX;
	const float y0 = givenY - cellOriginY;
	const float z0 = givenZ - cellOriginZ;

	// Figure out which cell were in
	const bool x_ge_y = x0 >= y0;
	const bool y_ge_z = y0 >= z0;
	const bool x_ge_z = x0 >= z0;

	const int iShift1 = x_ge_y & x_ge_z;
	const int jShift1 = y_ge_z & !x_ge_y;
	const int kShift1 = !y_ge_z & !x_ge_z;

	const int iShift2 = x_ge_y | x_ge_z;
	const int jShift2 = !x_ge_y | y_ge_z;
	const int kShift2 = !( x_ge_z & y_ge_z );

	// Offsets for the second corner in (x, y, z) coords
	const float x1 = x0 - (float)iShift1 + G3;
	const float y1 = y0 - (float)jShift1 + G3;
	const float z1 = z0 - (float)kShift1 + G3;
	// Offsets for the third corner in (x, y, z) coords
	const float x2 = x0 - (float)iShift2 + 2.0f * G3;
	const float y2 = y0 - (float)jShift2 + 2.0f * G3;
	const float z2 = z0 - (float)kShift2 + 2.0f * G3;
	// Offsets for the last corner in (x, y, z) coords
	const float x3 = x0 - 1.0f + 3.0f * G3;
	const float y3 = y0 - 1.0f + 3.0f * G3;
	const float z3 = z0 - 1.0f + 3.0f * G3;

	// Work out the hashed gradient indices of the four simplex corners
	const int iValue = (int)pointI & 255;
	const int jValue = (int)pointJ & 255;
	const int kValue = (int)pointK & 255;

	// add up cell influences

	const float t0 = 0.5f - x0 * x0 - y0 * y0 - z0 * z0;
	const float t1 = 0.5f - x1 * x1 - y1 * y1 - z1 * z1;
	const float t2 = 0.5f - x2 * x2 - y2 * y2 - z2 * z2;
	const float t3 = 0.5f - x3 * x3 - y3 * y3 - z3 * z3;

	constexpr bool isComputingCurl = std::is_same_v<std::remove_cvref_t<Result>, Vec3>;

	float tmp[isComputingCurl ? 9 : 1];
	[[maybe_unused]] float *const noiseGrad    = tmp + ( isComputingCurl ? 3 : 0 );
	[[maybe_unused]] float *const altNoiseGrad = tmp + ( isComputingCurl ? 6 : 0 );

	if constexpr( isComputingCurl ) {
		VectorClear( noiseGrad );
		VectorClear( altNoiseGrad );
	} else {
		tmp[0] = 0.0f;
	}

	if( t0 > 0.0f ) {
		const int gradIndex = calcGradIndex3D( iValue, 0, jValue, 0, kValue, 0 );
		if constexpr( isComputingCurl ) {
			addPointContribution( t0, gradIndex, x0, y0, z0, noiseGrad, altNoiseGrad );
		} else {
			tmp[0] += wsw::square( wsw::square( t0 ) ) * dot3( kGradTable[gradIndex], x0, y0, z0 );
		}
	}
	if( t1 > 0.0f ) {
		const int gradIndex = calcGradIndex3D( iValue, iShift1, jValue, jShift1, kValue, kShift1 );
		if constexpr( isComputingCurl ) {
			addPointContribution( t1, gradIndex, x1, y1, z1, noiseGrad, altNoiseGrad );
		} else {
			tmp[0] += wsw::square( wsw::square( t1 ) ) * dot3( kGradTable[gradIndex], x1, y1, z1 );
		}
	}
	if( t2 > 0.0f ) {
		const int gradIndex = calcGradIndex3D( iValue, iShift2, jValue, jShift2, kValue, kShift2 );
		if constexpr( isComputingCurl ) {
			addPointContribution( t2, gradIndex, x2, y2, z2, noiseGrad, altNoiseGrad );
		} else {
			tmp[0] += wsw::square( wsw::square( t2 ) ) * dot3( kGradTable[gradIndex], x2, y2, z2 );
		}
	}
	if( t3 > 0.0f ) {
		const int gradIndex = calcGradIndex3D( iValue, 1, jValue, 1, kValue, 1 );
		if constexpr( isComputingCurl ) {
			addPointContribution( t3, gradIndex, x3, y3, z3, noiseGrad, altNoiseGrad );
		} else {
			tmp[0] += wsw::square( wsw::square( t3 ) ) * dot3( kGradTable[gradIndex], x3, y3, z3 );
		}
	}

	if constexpr( isComputingCurl ) {
		CrossProduct( noiseGrad, altNoiseGrad, tmp );
		if( const float outSquareLength = VectorLengthSquared( tmp ); outSquareLength > 0.0f ) [[likely]] {
			const float outRcpLength = Q_RSqrt( outSquareLength );
			VectorScale( tmp, outRcpLength, tmp );
		} else {
			VectorSet( tmp, 0.0f, 0.0f, 1.0f );
		}
		return Vec3( tmp );
	} else {
		// The paper suggests using 32.0 as a normalizing scale.
		// According to sampling results, produced values are within (-0.42, +0.42) range.
		// Upon normalizing, we have to map it from [-1, +1] to [0, +1]
		constexpr float normalizingScale = 32.0f * ( 1.0f / 0.42f );
		const float result               = 0.5f * ( normalizingScale * tmp[0] + 1.0f );
		assert( result >= 0.0f && result <= 1.0f );
		return result;
	}
}

auto calcSimplexNoise3D( float x, float y, float z ) -> float {
	return calcSimplexNoise3DImpl<float>( x, y, z );
}

auto calcSimplexNoiseCurl( float x, float y, float z ) -> Vec3 {
	return calcSimplexNoise3DImpl<Vec3>( x, y, z );
}

wsw_forceinline auto fract( float x ) -> float {
	return x - std::floor( x );
}

wsw_forceinline void calcVec3HashOfVec3( const float *__restrict v, float *__restrict out ) {
	vec3_t p { fract( v[0] * 0.1031f ), fract( v[1] * 0.11369f ), fract( v[2] * 0.13787f ) };
	const float pd = p[0] * ( p[1] + 19.19f ) + p[1] * ( p[0] + 19.19f ) + p[2] * ( p[2] + 19.19f );
	const vec3_t pdv { pd, pd, pd };
	VectorAdd( p, pdv, p );
	out[0] = fract( ( p[0] + p[1] ) * p[2] );
	out[1] = fract( ( p[0] + p[2] ) * p[1] );
	out[2] = fract( ( p[1] + p[2] ) * p[0] );
}

template <bool CalcSquared>
// Try inlining it forcefully into the final subroutines so there's no redundant calls
wsw_forceinline auto calcVoronoiNoiseImpl( float x, float y, float z ) -> float {
	const float cellX = std::floor( x );
	const float cellY = std::floor( y );
	const float cellZ = std::floor( z );
	const float fracX = x - cellX;
	const float fracY = y - cellY;
	const float fracZ = z - cellZ;

	float minDist = 1.0f;

	// TODO: Unroll (use static array of offsets)?

	int xOffset = -1;
	do {
		int yOffset = -1;
		do {
			int zOffset = -1;
			do {
				const vec3_t testedOffset { (float)xOffset, (float)yOffset, (float)zOffset };
				const vec3_t testedCell { cellX + testedOffset[0], cellY + testedOffset[1], cellZ + testedOffset[2] };

				vec3_t pointOffsetInTestedCell;
				calcVec3HashOfVec3( testedCell, pointOffsetInTestedCell );

				const vec3_t pickedPoint {
					testedOffset[0] + pointOffsetInTestedCell[0] - fracX,
					testedOffset[1] + pointOffsetInTestedCell[1] - fracY,
					testedOffset[2] + pointOffsetInTestedCell[2] - fracZ,
				};

				float distToTestedPoint;
				if constexpr( CalcSquared ) {
					distToTestedPoint = VectorLengthSquared( pickedPoint );
				} else {
					distToTestedPoint = VectorLengthFast( pickedPoint );
				}

				minDist = wsw::min( minDist, distToTestedPoint );
			} while( ++zOffset <= +1 );
		} while( ++yOffset <= +1 );
	} while( ++xOffset <= +1 );

	assert( minDist >= 0.0f && minDist <= 1.0f );
	return minDist;
}

auto calcVoronoiNoiseSquared( float x, float y, float z ) -> float {
	return calcVoronoiNoiseImpl<true>( x, y, z );
}

auto calcVoronoiNoiseLinear( float x, float y, float z ) -> float {
	return calcVoronoiNoiseImpl<false>( x, y, z );
}