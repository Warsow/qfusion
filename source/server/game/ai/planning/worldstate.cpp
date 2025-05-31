#include "worldstate.h"
#include "../bot.h"

// TODO: Replace by std::bit_cast once it's usable
template <typename T>
[[nodiscard]]
static inline auto bitsOf( const T &value ) -> uint32_t {
	static_assert( sizeof( T ) <= 4 );
	uint32_t result = 0;
	std::memcpy( &result, &value, sizeof( T ) );
	return result;
}

template <typename T>
static auto computeHash( const std::optional<T> *varsBegin, const std::optional<T> *varsEnd ) -> uint32_t {
	uint32_t result = 0;
	for( const std::optional<T> *varIt = varsBegin; varIt != varsEnd; ++varIt ) {
		result = result * 31;
		if( const std::optional<T> &var = *varIt; var.has_value() ) {
			if constexpr( std::is_same_v<T, Vec3> ) {
				const Vec3 &varValue = *var;
				result += bitsOf( varValue.X() );
				result = result * 31;
				result += bitsOf( varValue.Y() );
				result = result * 31;
				result += bitsOf( varValue.Z() );
			} else {
				static_assert( std::is_integral_v<T> || std::is_floating_point_v<T> );
				result += bitsOf( *var );
			}
		}
	}
	return result;
}

auto WorldState::computeHash() const -> uint32_t {
	uint32_t result = 0;
	result = result * 31 + ::computeHash( m_floatVars, std::end( m_floatVars ) );
	result = result * 31 + ::computeHash( m_uintVars, std::end( m_uintVars ) );
	result = result * 31 + ::computeHash( m_boolVars, std::end( m_boolVars ) );
	result = result * 31 + ::computeHash( m_vec3Vars, std::end( m_vec3Vars ) );
	return result;
}

bool WorldState::operator==( const WorldState &that ) const {
	return
	    std::equal( std::begin( m_floatVars ), std::end( m_floatVars ), std::begin( that.m_floatVars ) ) &&
		std::equal( std::begin( m_uintVars ), std::end( m_uintVars ), std::begin( that.m_uintVars ) ) &&
		std::equal( std::begin( m_boolVars ), std::end( m_boolVars ), std::begin( that.m_boolVars ) ) &&
		std::equal( std::begin( m_vec3Vars ), std::end( m_vec3Vars ), std::begin( that.m_vec3Vars ) );
}

#define PRINT_VAR( varName ) do {} while( 0 ); // TODO

void WorldState::DebugPrint( const char *tag ) const {
	// We have to list all vars manually
	// A list of vars does not (and should not) exist
	// since vars instances do not (and should not) exist in optimized by a compiler code
	// (WorldState members are accessed directly instead)

	PRINT_VAR( GoalItemWaitTime );
	PRINT_VAR( SimilarWorldStateInstanceId );

	PRINT_VAR( BotOrigin );
	PRINT_VAR( EnemyOrigin );
	PRINT_VAR( NavTargetOrigin );
	PRINT_VAR( PendingOrigin );

	PRINT_VAR( HasThreateningEnemy );
	PRINT_VAR( HasPickedGoalItem );

	PRINT_VAR( HasPositionalAdvantage );
	PRINT_VAR( CanHitEnemy );
	PRINT_VAR( EnemyCanHit );
	PRINT_VAR( HasJustKilledEnemy );

	PRINT_VAR( IsRunningAway );
	PRINT_VAR( HasRunAway );
}

#define PRINT_DIFF( varName ) do {} while( 0 ) // TODO

void WorldState::DebugPrintDiff( const WorldState &that, const char *oldTag, const char *newTag ) const {
	PRINT_DIFF( GoalItemWaitTime );
	PRINT_DIFF( SimilarWorldStateInstanceId );

	PRINT_DIFF( BotOrigin );
	PRINT_DIFF( EnemyOrigin );
	PRINT_DIFF( NavTargetOrigin );
	PRINT_DIFF( PendingOrigin );

	PRINT_DIFF( HasThreateningEnemy );
	PRINT_DIFF( HasPickedGoalItem );

	PRINT_DIFF( HasPositionalAdvantage );
	PRINT_DIFF( CanHitEnemy );
	PRINT_DIFF( EnemyCanHit );
	PRINT_DIFF( HasJustKilledEnemy );

	PRINT_DIFF( IsRunningAway );
	PRINT_DIFF( HasRunAway );
}
