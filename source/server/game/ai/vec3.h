#ifndef WSW_046c64e4_0cf8_4378_bbe0_4b2b3394c273_H
#define WSW_046c64e4_0cf8_4378_bbe0_4b2b3394c273_H

#include <common/q_math.h>
#include <common/wswexceptions.h>

#include <optional>

class Vec3
{
	vec3_t vec;

public:
	explicit Vec3( const vec3_t that ) {
		VectorCopy( that, Data() );
	}
	Vec3( const Vec3 &that ) {
		VectorCopy( that.Data(), Data() );
	}

	Vec3( vec_t x, vec_t y, vec_t z ) {
		VectorSet( vec, x, y, z );
	}

	Vec3 &operator=( const Vec3 &that ) {
		VectorCopy( that.Data(), Data() );
		return *this;
	}

	float Length() const { return (float)VectorLength( vec ); }
	float LengthFast() const { return VectorLengthFast( vec ); }
	float SquaredLength() const { return VectorLengthSquared( vec ); }

	float DistanceTo( const Vec3 &that ) const { return DistanceTo( that.Data() ); }
	float DistanceTo( const vec3_t that ) const { return (float)Distance( vec, that ); }
	float FastDistanceTo( const Vec3 &that ) const { return FastDistanceTo( that.Data() ); }
	float FastDistanceTo( const vec3_t that ) const { return DistanceFast( vec, that ); }
	float SquareDistanceTo( const Vec3 &that ) const { return SquareDistanceTo( that.Data() ); }
	float SquareDistanceTo( const vec3_t that ) const { return DistanceSquared( vec, that ); }
	float Distance2DTo( const Vec3 &that ) const { return Distance2DTo( that.vec ); }
	float Distance2DTo( const vec3_t that ) const { return std::sqrt( SquareDistance2DTo( that ) ); }
	float FastDistance2DTo( const Vec3 &that ) const { return FastDistance2DTo( that.vec ); }
	float FastDistance2DTo( const vec3_t that ) const { return Q_Sqrt( SquareDistance2DTo( that ) ); }
	float SquareDistance2DTo( const Vec3 &that ) const { return SquareDistanceTo( that.vec ); }
	float SquareDistance2DTo( const vec3_t that ) const {
		float dx = vec[0] - that[0];
		float dy = vec[1] - that[1];
		return dx * dx + dy * dy;
	}

	// A poor man's replacement to named arguments.
	// The default value just helps to avoid division by zero.
	// Greater values are useful for implementing various domain-specific logic
	// when directions of non-zero but relatively short vectors are not considered feasible.
	struct NormalizationParams { float minAcceptableLength { 1e-3f }; };

	[[nodiscard]]
	auto normalize( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> std::optional<float> {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squaredLength    = VectorLengthSquared( vec );
		if( squaredLength >= squaredThreshold ) [[likely]] {
			// We have to return the exact length by the contract,
			// so no reciprocal optimization is applied, even if it could be quite precise.
			const float length    = std::sqrt( squaredLength );
			const float rcpLength = 1.0f / length;
			VectorScale( vec, rcpLength, vec );
			return length;
		}
		return std::nullopt;
	}

	[[maybe_unused]]
	auto normalizeOrFail( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> float {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squaredLength    = VectorLengthSquared( vec );
		if( squaredLength >= squaredThreshold ) [[likely]] {
			const float length    = std::sqrt( squaredLength );
			const float rcpLength = 1.0f / length;
			VectorScale( vec, rcpLength, vec );
			return length;
		}
		wsw::failWithRuntimeError( "Normalization failure" );
	}

	[[nodiscard]]
	auto normalizeFast( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> std::optional<float> {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squaredLength    = VectorLengthSquared( vec );
		if( squaredLength >= squaredThreshold ) [[likely]] {
			const float rcpLength = Q_RSqrt( squaredLength );
			VectorScale( vec, rcpLength, vec );
			return squaredLength * rcpLength;
		}
		return std::nullopt;
	}

	[[maybe_unused]]
	auto normalizeFastOrThrow( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> float {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squaredLength    = VectorLengthSquared( vec );
		if( squaredLength >= squaredThreshold ) [[likely]] {
			const float rcpLength = Q_RSqrt( squaredLength );
			VectorScale( vec, rcpLength, vec );
			return squaredLength * rcpLength;
		}
		wsw::failWithRuntimeError( "Fast normalization failure" );
	}

	[[nodiscard]]
	bool operator==( const Vec3 &that ) const { return VectorCompare( vec, that.vec ); }

	float *Data() { return vec; }
	const float *Data() const { return vec; }

	void Set( const Vec3 &that ) { Set( that.Data() ); }
	void Set( const vec3_t that ) { Set( that[0], that[1], that[2] ); }
	void Set( vec_t x, vec_t y, vec_t z ) {
		VectorSet( this->vec, x, y, z );
	}
	void CopyTo( Vec3 &that ) const { that.Set( *this ); }
	void CopyTo( vec3_t that ) const { VectorCopy( this->vec, that ); }
	void CopyTo( vec_t *x, vec_t *y, vec_t *z ) const {
		if( x ) {
			*x = X();
		}
		if( y ) {
			*y = Y();
		}
		if( z ) {
			*z = Z();
		}
	}

	vec_t &X() { return vec[0]; }
	vec_t &Y() { return vec[1]; }
	vec_t &Z() { return vec[2]; }

	vec_t X() const { return vec[0]; }
	vec_t Y() const { return vec[1]; }
	vec_t Z() const { return vec[2]; }

	void operator+=( const Vec3 &that ) {
		VectorAdd( vec, that.vec, vec );
	}
	void operator+=( const vec3_t that ) {
		VectorAdd( vec, that, vec );
	}
	void operator-=( const Vec3 &that ) {
		VectorSubtract( vec, that.vec, vec );
	}
	void operator-=( const vec3_t that ) {
		VectorSubtract( vec, that, vec );
	}
	void operator*=( float scale ) {
		VectorScale( vec, scale, vec );
	}
	Vec3 operator*( float scale ) const {
		return Vec3( scale * X(), scale * Y(), scale * Z() );
	}
	Vec3 operator+( const Vec3 &that ) const {
		return Vec3( X() + that.X(), Y() + that.Y(), Z() + that.Z() );
	}
	Vec3 operator+( const vec3_t that ) const {
		return Vec3( X() + that[0], Y() + that[1], Z() + that[2] );
	}
	Vec3 operator-( const Vec3 &that ) const {
		return Vec3( X() - that.X(), Y() - that.Y(), Z() - that.Z() );
	}
	Vec3 operator-( const vec3_t that ) const {
		return Vec3( X() - that[0], Y() - that[1], Z() - that[2] );
	}
	Vec3 operator-() const {
		return Vec3( -X(), -Y(), -Z() );
	}

	vec_t Dot( const Vec3 &that ) const {
		return DotProduct( vec, that.vec );
	}
	float Dot( const vec3_t that ) const {
		return DotProduct( vec, that );
	}

	inline Vec3 Cross( const Vec3 &that ) const {
		return Vec3(
			Y() * that.Z() - Z() * that.Y(),
			Z() * that.X() - X() * that.Z(),
			X() * that.Y() - Y() * that.X() );
	}
	inline Vec3 Cross( const vec3_t that ) const {
		return Vec3(
			Y() * that[2] - Z() * that[1],
			Z() * that[0] - X() * that[2],
			X() * that[2] - Y() * that[1] );
	}
};

inline Vec3 operator *( float scale, const Vec3 &v ) {
	return v * scale;
}

#endif
