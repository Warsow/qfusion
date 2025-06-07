#ifndef WSW_b6521d59_fa45_4a78_bc5b_bd1261aec3a7_H
#define WSW_b6521d59_fa45_4a78_bc5b_bd1261aec3a7_H

#include "../ailocal.h"

#include <cstdint>
#include <optional>
#include <utility>

#include <common/helpers/exceptions.h>
#include "../vec3.h"

class WorldState {
public:
	[[nodiscard]]
	WorldState() = default;

#ifndef PUBLIC_BUILD
	// Just to track assignment correctness (so every world state is an assignment descendant of a current world state)
	bool isCopiedFromOtherWorldState() { return m_isCopiedFromOtherWorldState; }

	[[nodiscard]]
	WorldState( const WorldState &that ) {
		copyFromOtherWorldState( that );
	}

	[[maybe_unused]]
	auto operator=( const WorldState &that ) -> WorldState & {
		if( this != std::addressof( that ) ) {
			copyFromOtherWorldState( that );
		}
		return *this;
	}

	[[nodiscard]]
	WorldState( WorldState &&that ) {
		copyFromOtherWorldState( that );
	}

	[[maybe_unused]]
	auto operator=( WorldState &&that ) noexcept -> WorldState & {
		copyFromOtherWorldState( that );
		return *this;
	}

	const char *producedByAction { nullptr };
#endif

	enum BuiltinUIntVarKeys {
		GoalItemWaitTime,
		SimilarWorldStateInstanceId,

		// This sucks and there's no viable alternative...
		_kBuiltinUIntVarsCount
	};

	[[nodiscard]]
	auto getUInt( BuiltinUIntVarKeys key ) const -> std::optional<unsigned> {
		assert( key >= 0 && key < std::size( m_uintVars ) );
		return m_uintVars[key];
	}

	void setUInt( BuiltinUIntVarKeys key, unsigned value ) {
		assert( key >= 0 && key < std::size( m_uintVars ) );
		m_uintVars[key] = value;
	}

	void clearUInt( BuiltinUIntVarKeys key ) {
		m_uintVars[key] = std::nullopt;
	}

	enum BuiltinFloatVarKeys {
		PotentialHazardDamage,
		ThreatInflictedDamage,

		_kBuiltinFloatVarsCount
	};

	[[nodiscard]]
	auto getFloat( BuiltinFloatVarKeys key ) const -> std::optional<float> {
		assert( key >= 0 && key < std::size( m_floatVars ) );
		return m_floatVars[key];
	}

	auto setFloat( BuiltinFloatVarKeys key, float value ) {
		assert( key >= 0 && key < std::size( m_floatVars ) );
		m_floatVars[key] = value;
	}

	void clearFloat( BuiltinFloatVarKeys key ) {
		m_floatVars[key] = std::nullopt;
	}

	enum BuiltinBoolVarKeys {
		HasThreateningEnemy,
		HasPickedGoalItem,

		IsRunningAway,
		HasRunAway,

		HasReactedToHazard,
		HasReactedToThreat,

		IsReactingToEnemyLost,
		HasReactedToEnemyLost,
		MightSeeLostEnemyAfterTurn,

		HasPositionalAdvantage,
		CanHitEnemy,
		EnemyCanHit,
		HasJustKilledEnemy,

		_kBuiltinBoolVarsCount
	};

	[[nodiscard]]
	auto getBool( BuiltinBoolVarKeys key ) const -> std::optional<bool> {
		assert( key >= 0 && key < std::size( m_boolVars ) );
		return m_boolVars[key];
	}

	auto setBool( BuiltinBoolVarKeys key, bool value ) {
		assert( key >= 0 && key < std::size( m_boolVars ) );
		m_boolVars[key] = value;
	}

	void clearBool( BuiltinBoolVarKeys key ) {
		m_boolVars[key] = std::nullopt;
	}

	enum BuiltinVec3VarKeys {
		BotOrigin,
		EnemyOrigin,
		NavTargetOrigin,

		PendingCoverSpot,
		PendingElevatorDest,
		PendingJumppadDest,
		PendingTeleportDest,

		ThreatPossibleOrigin,
		LostEnemyLastSeenOrigin,

		_kBuiltinVec3VarsCount
	};

	[[nodiscard]]
	auto getVec3( BuiltinVec3VarKeys key ) const -> std::optional<Vec3> {
		assert( key >= 0 && key < std::size( m_vec3Vars ) );
		return m_vec3Vars[key];
	}

	auto setVec3( BuiltinVec3VarKeys key, const Vec3 &value ) {
		assert( key >= 0 && key < std::size( m_vec3Vars ) );
		m_vec3Vars[key] = value;
	}

	void clearVec3( BuiltinVec3VarKeys key ) {
		m_vec3Vars[key] = std::nullopt;
	}

	[[nodiscard]]
	auto computeHash() const -> uint32_t;

	[[nodiscard]]
	bool operator==( const WorldState &that ) const;

	void DebugPrint( const char *tag ) const;

	void DebugPrintDiff( const WorldState &that, const char *oldTag, const char *newTag ) const;
private:
	bool m_isCopiedFromOtherWorldState { false };

	// The memory layout could be more optimal, but we no longer care that much

	std::optional<unsigned> m_uintVars[_kBuiltinUIntVarsCount];
	std::optional<float> m_floatVars[_kBuiltinFloatVarsCount];
	std::optional<bool> m_boolVars[_kBuiltinBoolVarsCount];
	std::optional<Vec3> m_vec3Vars[_kBuiltinVec3VarsCount];

#ifndef PUBLIC_BUILD
	void copyFromOtherWorldState( const WorldState &that ) {
		// std::optionals are perfectly copyable in this case
		std::memcpy( (void *)this, (void *)std::addressof( that ), sizeof( WorldState ) );
		m_isCopiedFromOtherWorldState = true;
	}
#endif
};

#endif
