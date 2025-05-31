#include "keptinfovpointtracker.h"
#include "../bot.h"

void KeptInFovPointTracker::update() {
	m_point = selectCurrentPoint();
}

auto KeptInFovPointTracker::selectCurrentPoint() -> std::optional<Vec3> {
	const auto &botMiscTactics = m_bot->GetMiscTactics();
	if( !botMiscTactics.shouldRushHeadless ) {
		if( const std::optional<SelectedEnemy> maybeSelectedEnemy = m_bot->GetSelectedEnemy() ) {
			if( auto maybePoint = selectPointBasedOnEnemies( *maybeSelectedEnemy ) ) {
				return maybePoint;
			}
		}

		unsigned timeout = botMiscTactics.shouldKeepXhairOnEnemy ? 2000 : 1000;
		// Care longer if retreating
		if( botMiscTactics.willRetreat ) {
			timeout = 2 * timeout;
		}

		if( const TrackedEnemy *enemy = m_awarenessModule->ChooseLostOrHiddenEnemy( timeout ) ) {
			if( auto maybePoint = selectPointBasedOnLostOrHiddenEnemy( enemy ) ) {
				return maybePoint;
			}
		}

		if( const BotAwarenessModule::HurtEvent *hurtEvent = m_awarenessModule->GetValidHurtEvent() ) {
			// Don't check the origin PVS as the origin is a rough guess and may locate in solid
			return hurtEvent->possibleOrigin;
		}
	}

	return std::nullopt;
}

bool KeptInFovPointTracker::isPointInPvs( const Vec3 &point ) const {
	const Vec3 mins( Vec3( -8, -8, -8 ) + point );
	const Vec3 maxs( Vec3( +8, +8, +8 ) + point );

	int leafNums[16], unused = 0;
	const int numLeafs = SV_BoxLeafnums( mins.Data(), maxs.Data(), leafNums, (int)std::size( leafNums ), &unused );

	const auto *const botEnt = game.edicts + m_bot->EntNum();
	for( int botLeafIndex = 0; botLeafIndex < botEnt->r.num_clusters; ++botLeafIndex ) {
		const int botLeaf = botEnt->r.leafnums[botLeafIndex];
		for( int pointLeafIndex = 0; pointLeafIndex < numLeafs; ++pointLeafIndex ) {
			if( SV_LeafsInPVS( botLeaf, leafNums[pointLeafIndex] ) ) {
				return true;
			}
		}
	}

	return false;
}

[[nodiscard]]
static inline bool isWithinValidRange( const Vec3 &a, const Vec3 &b, float threshold ) {
	const float squareDistance = a.SquareDistanceTo( b );
	// Discard way too close points
	return squareDistance < threshold * threshold && squareDistance > 32.0f * 32.0f;
}

static const float kSqrtOfFloatMax = std::sqrt( std::numeric_limits<float>::max() );

auto KeptInFovPointTracker::selectPointBasedOnEnemies( const SelectedEnemy &selectedEnemy ) -> std::optional<Vec3> {
	const Vec3 botOrigin( m_bot->Origin() );
	const Vec3 enemyOrigin( selectedEnemy.LastSeenOrigin() );

	assert( kSqrtOfFloatMax * kSqrtOfFloatMax > kSqrtOfFloatMax );
	assert( (double)kSqrtOfFloatMax * (double)kSqrtOfFloatMax <= (double)std::numeric_limits<float>::max() );

	float distanceThreshold = kSqrtOfFloatMax;
	if( !m_bot->GetMiscTactics().shouldKeepXhairOnEnemy ) {
		if( !selectedEnemy.HasQuad() && !selectedEnemy.IsACarrier() ) {
			distanceThreshold = 768.0f + 1024.0f * selectedEnemy.MaxThreatFactor();
			distanceThreshold *= 0.5f + 0.5f * m_bot->GetEffectiveOffensiveness();
		}
	}

	if( isWithinValidRange( botOrigin, enemyOrigin, distanceThreshold ) && isPointInPvs( enemyOrigin ) ) {
		return enemyOrigin;
	}

	return std::nullopt;
}

auto KeptInFovPointTracker::selectPointBasedOnLostOrHiddenEnemy( const TrackedEnemy *enemy ) -> std::optional<Vec3> {
	const Vec3 enemyOrigin( enemy->LastSeenOrigin() );

	assert( kSqrtOfFloatMax * kSqrtOfFloatMax > kSqrtOfFloatMax );
	assert( (double)kSqrtOfFloatMax * (double)kSqrtOfFloatMax <= (double)std::numeric_limits<float>::max() );

	float distanceThreshold = kSqrtOfFloatMax;
	if( !m_bot->GetMiscTactics().shouldKeepXhairOnEnemy ) {
		distanceThreshold = 384.0f;
		if( enemy->m_ent ) {
			// Compute a threat factor this "lost or hidden" enemy could have had
			// if this enemy was included in "selected enemies"
			// TODO: The "false" literal is misleading, extract separate methods
			distanceThreshold += 1024.0f * SelectedEnemy::ComputeThreatFactor( m_bot, enemy->m_ent );
		}
		distanceThreshold *= 0.5f + 0.5f * m_bot->GetEffectiveOffensiveness();
	}

	if( isWithinValidRange( Vec3( m_bot->Origin() ), enemyOrigin, distanceThreshold ) && isPointInPvs( enemyOrigin ) ) {
		return enemyOrigin;
	}

	return std::nullopt;
}