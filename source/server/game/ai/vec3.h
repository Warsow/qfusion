#ifndef WSW_046c64e4_0cf8_4378_bbe0_4b2b3394c273_H
#define WSW_046c64e4_0cf8_4378_bbe0_4b2b3394c273_H

#include <common/helpers/q_math.h>
#include <common/helpers/exceptions.h>

#include <optional>

class Vec3 {
	// Note: This implementation strives to reduce depth of inline calls
public:
	[[nodiscard]]
	constexpr explicit Vec3( const vec3_t that ) { VectorCopy( that, m_data ); }
	[[nodiscard]]
	constexpr Vec3( const Vec3 &that ) = default;
	[[nodiscard]]
	constexpr Vec3( float x, float y, float z ) { VectorSet( m_data, x, y, z ); }

	[[nodiscard]] auto length() const -> float { return (float)VectorLength( m_data ); }
	[[nodiscard]] auto fastLength() const -> float { return VectorLengthFast( m_data ); }
	[[nodiscard]] constexpr auto squareLength() const -> float { return VectorLengthSquared( m_data ); }

	[[nodiscard]] auto distanceTo( const Vec3 &that ) const -> float { return distanceTo( that.m_data ); }
	[[nodiscard]] auto distanceTo( const vec3_t that ) const -> float { return (float)Distance( m_data, that ); }
	[[nodiscard]] auto fastDistanceTo( const Vec3 &that ) const -> float { return fastDistanceTo( that.m_data ); }
	[[nodiscard]] auto fastDistanceTo( const vec3_t that ) const -> float { return DistanceFast( m_data, that ); }

	[[nodiscard]] auto distance2DTo( const Vec3 &that ) const -> float { return distance2DTo( that.m_data ); }
	[[nodiscard]] auto distance2DTo( const vec3_t that ) const -> float { return std::sqrt( squareDistance2DTo( that ) ); }
	[[nodiscard]] auto fastDistance2DTo( const Vec3 &that ) const -> float { return fastDistance2DTo( that.m_data ); }
	[[nodiscard]] auto fastDistance2DTo( const vec3_t that ) const -> float { return Q_Sqrt( squareDistance2DTo( that ) ); }

	[[nodiscard]]
	constexpr auto squareDistanceTo( const Vec3 &that ) const -> float {
		return DistanceSquared( m_data, that.m_data );
	}
	[[nodiscard]]
	constexpr auto squareDistanceTo( const vec3_t that ) const -> float {
		return DistanceSquared( m_data, that );
	}

	[[nodiscard]]
	constexpr auto squareDistance2DTo( const Vec3 &that ) const -> float {
		return squareDistance2DTo( that.m_data );
	}
	[[nodiscard]]
	constexpr auto squareDistance2DTo( const vec3_t that ) const -> float {
		const float dx = m_data[0] - that[0];
		const float dy = m_data[1] - that[1];
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
		const float squareLength    = VectorLengthSquared( m_data );
		if( squareLength >= squaredThreshold ) [[likely]] {
			// We have to return the exact length by the contract,
			// so no reciprocal optimization is applied, even if it could be quite precise.
			const float length    = std::sqrt( squareLength );
			const float rcpLength = 1.0f / length;
			VectorScale( m_data, rcpLength, m_data );
			return length;
		}
		return std::nullopt;
	}

	[[maybe_unused]]
	auto normalizeOrFail( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> float {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squareLength    = VectorLengthSquared( m_data );
		if( squareLength >= squaredThreshold ) [[likely]] {
			const float length    = std::sqrt( squareLength );
			const float rcpLength = 1.0f / length;
			VectorScale( m_data, rcpLength, m_data );
			return length;
		}
		wsw::failWithRuntimeError( "Normalization failure" );
	}

	[[nodiscard]]
	auto normalizeFast( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> std::optional<float> {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squareLength    = VectorLengthSquared( m_data );
		if( squareLength >= squaredThreshold ) [[likely]] {
			const float rcpLength = Q_RSqrt( squareLength );
			VectorScale( m_data, rcpLength, m_data );
			return squareLength * rcpLength;
		}
		return std::nullopt;
	}

	[[maybe_unused]]
	auto normalizeFastOrThrow( NormalizationParams params = { .minAcceptableLength = 1e-3f } ) -> float {
		assert( params.minAcceptableLength > 0.0f );
		const float squaredThreshold = params.minAcceptableLength * params.minAcceptableLength;
		const float squareLength    = VectorLengthSquared( m_data );
		if( squareLength >= squaredThreshold ) [[likely]] {
			const float rcpLength = Q_RSqrt( squareLength );
			VectorScale( m_data, rcpLength, m_data );
			return squareLength * rcpLength;
		}
		wsw::failWithRuntimeError( "Fast normalization failure" );
	}

	[[nodiscard]]
	bool operator!=( const Vec3 &that ) const { return !VectorCompare( m_data, that.m_data ); }
	[[nodiscard]]
	bool operator==( const Vec3 &that ) const { return VectorCompare( m_data, that.m_data ); }

	[[nodiscard]] constexpr auto data() -> float * { return m_data; }
	[[nodiscard]] constexpr auto data() const -> const float * { return m_data; }

	// TODO: Deprecate these methods
	constexpr void set( const Vec3 &that ) { VectorCopy( that.m_data, m_data ); }
	constexpr void set( const vec3_t that ) { VectorCopy( that, m_data ); }
	constexpr void set( vec_t x, vec_t y, vec_t z ) { VectorSet( m_data, x, y, z ); }
	// TODO: Deprecate these methods
	constexpr void copyTo( Vec3 &that ) const { VectorCopy( m_data, that.m_data ); }
	constexpr void copyTo( vec3_t that ) const { VectorCopy( m_data, that ); }

	[[nodiscard]] constexpr auto x() -> float & { return m_data[0]; }
	[[nodiscard]] constexpr auto y() -> float & { return m_data[1]; }
	[[nodiscard]] constexpr auto z() -> float & { return m_data[2]; }

	[[nodiscard]] constexpr auto x() const -> float { return m_data[0]; }
	[[nodiscard]] constexpr auto y() const -> float { return m_data[1]; }
	[[nodiscard]] constexpr auto z() const -> float { return m_data[2]; }

	constexpr void operator+=( const Vec3 &that ) {
		VectorAdd( m_data, that.m_data, m_data );
	}
	constexpr void operator+=( const vec3_t that ) {
		VectorAdd( m_data, that, m_data );
	}
	constexpr void operator-=( const Vec3 &that ) {
		VectorSubtract( m_data, that.m_data, m_data );
	}
	constexpr void operator-=( const vec3_t that ) {
		VectorSubtract( m_data, that, m_data );
	}
	constexpr void operator*=( float scale ) {
		VectorScale( m_data, scale, m_data );
	}
	[[nodiscard]]
	constexpr auto operator*( float scale ) const -> Vec3 {
		return { scale * m_data[0], scale * m_data[1], scale * m_data[2] };
	}
	[[nodiscard]]
	constexpr auto operator+( const Vec3 &that ) const -> Vec3 {
		return { m_data[0] + that.m_data[0], m_data[1] + that.m_data[1], m_data[2] + that.m_data[2] };
	}
	[[nodiscard]]
	constexpr auto operator+( const vec3_t that ) const -> Vec3 {
		return { m_data[0] + that[0], m_data[1] + that[1], m_data[2] + that[2] };
	}
	[[nodiscard]]
	constexpr auto operator-( const Vec3 &that ) const -> Vec3 {
		return { m_data[0] - that.m_data[0], m_data[1] - that.m_data[1], m_data[2] - that.m_data[2] };
	}
	[[nodiscard]]
	constexpr auto operator-( const vec3_t that ) const -> Vec3 {
		return { m_data[0] - that[0], m_data[1] - that[1], m_data[2] - that[2] };
	}
	[[nodiscard]]
	constexpr auto operator-() const -> Vec3 {
		return { -m_data[0], -m_data[1], -m_data[2] };
	}

	[[nodiscard]] constexpr auto dot( const Vec3 &that ) const -> float { return DotProduct( m_data, that.m_data ); }
	[[nodiscard]] constexpr auto dot( const vec3_t that ) const -> float { return DotProduct( m_data, that ); }

	[[nodiscard]]
	constexpr auto cross( const Vec3 &that ) const -> Vec3 {
		vec3_t result;
		CrossProduct( m_data, that.m_data, result );
		return Vec3( result );
	}
	[[nodiscard]]
	constexpr auto cross( const vec3_t that ) const -> Vec3 {
		vec3_t result;
		CrossProduct( m_data, that, result );
		return Vec3( result );
	}
private:
	float m_data[3];
};

[[nodiscard]]
inline constexpr auto operator *( float scale, const Vec3 &v ) -> Vec3 {
	return v * scale;
}

#endif
