#include "hazardsdetector.h"
#include "entitiespvscache.h"
#include "../classifiedentitiescache.h"
#include "../bot.h"

[[nodiscard]]
static inline bool isGenericProjectileVisible( const edict_t *viewer, const edict_t *ent ) {
	const vec3_t viewOrigin { viewer->s.origin[0], viewer->s.origin[1], viewer->s.origin[2] + (float)viewer->viewheight };

	trace_t trace;
	G_Trace( &trace, viewOrigin, nullptr, nullptr, ent->s.origin, viewer, MASK_OPAQUE );
	return trace.fraction == 1.0f || trace.ent == ENTNUM( ent );
}

[[nodiscard]]
// Try testing both origins. Its very coarse but should produce satisfiable results in-game.
static inline bool isLaserBeamVisible( const edict_t *viewer, const edict_t *ent ) {
	const vec3_t viewOrigin { viewer->s.origin[0], viewer->s.origin[1], viewer->s.origin[2] + (float)viewer->viewheight };

	trace_t trace;
	G_Trace( &trace, viewOrigin, nullptr, nullptr, ent->s.origin, viewer, MASK_OPAQUE );
	if( trace.fraction == 1.0f || trace.ent == ENTNUM( ent ) ) {
		return true;
	}

	if( DistanceSquared( trace.endpos, ent->s.origin ) < wsw::square( 1.0f ) ) {
		return true;
	}

	G_Trace( &trace, viewOrigin, nullptr, nullptr, ent->s.origin2, viewer, MASK_OPAQUE );
	if( trace.fraction == 1.0f || trace.ent == ENTNUM( ent ) ) {
		return true;
	}

	return DistanceSquared( trace.endpos, ent->s.origin2 ) < wsw::square( 1.0f );
}

[[nodiscard]]
static inline bool isLaserBeamInPvs( const edict_t *self, const edict_t *ent ) {
	if( EntitiesPvsCache::Instance()->AreInPvs( self, ent ) ) {
		return true;
	}
	// TODO: Reuse cached leaves
	if( SV_InPVS( self->s.origin, ent->s.origin2 ) ) {
		return true;
	}
	// Try this
	vec3_t midPoint;
	VectorAvg( ent->s.origin, ent->s.origin2, midPoint );
	// TODO: Reuse cached leaves
	return SV_InPVS( self->s.origin, midPoint );
}

void HazardsDetector::Clear() {
	maybeVisibleDangerousRockets.clear();
	visibleDangerousRockets.clear();
	maybeVisibleDangerousWaves.clear();
	visibleDangerousWaves.clear();
	maybeVisibleDangerousPlasmas.clear();
	visibleDangerousPlasmas.clear();
	maybeVisibleDangerousBlasts.clear();
	visibleDangerousBlasts.clear();
	maybeVisibleDangerousGrenades.clear();
	visibleDangerousGrenades.clear();
	maybeVisibleDangerousLasers.clear();
	visibleDangerousLasers.clear();

	maybeVisibleOtherRockets.clear();
	visibleOtherRockets.clear();
	maybeVisibleOtherWaves.clear();
	visibleOtherWaves.clear();
	maybeVisibleOtherPlasmas.clear();
	visibleOtherPlasmas.clear();
	maybeVisibleOtherBlasts.clear();
	visibleOtherBlasts.clear();
	maybeVisibleOtherGrenades.clear();
	visibleOtherGrenades.clear();
	maybeVisibleOtherLasers.clear();
	visibleOtherLasers.clear();
}

void HazardsDetector::Exec() {
	Clear();

	// Note that we always skip own rockets, plasma, etc.
	// Otherwise, all own bot shot events yield a hazard.
	// There are some cases when an own rocket can hurt, but they are either extremely rare or handled by bot fire code.
	// Own grenades are the only exception. We check grenade think time to skip grenades just fired by bot.
	// If a grenade is about to explode and is close to bot, its likely it has bounced of the world and can hurt.


	const auto *const entsCache = wsw::ai::ClassifiedEntitiesCache::instance();
	const auto *const gameEnts  = game.edicts;
	const auto *const botEnt    = gameEnts + bot->EntNum();

	struct {
		HazardsDetector::EntsAndDistancesVector *dangerous, *other;
		std::span<const uint16_t> entNums;
		float distanceThreshold;
	} regularEntitiesToTest[] {
		{ &maybeVisibleDangerousRockets, &maybeVisibleOtherRockets, entsCache->getAllRockets(), 550.0f },
		{ &maybeVisibleDangerousPlasmas, &maybeVisibleOtherPlasmas, entsCache->getAllPlasmas(), 750.0f },
		{ &maybeVisibleDangerousBlasts,  &maybeVisibleOtherBlasts,  entsCache->getAllBlasts(),  950.0f },
		{ &maybeVisibleDangerousLasers,  &maybeVisibleOtherLasers,  entsCache->getAllLasers(),  kLasergunRange + 128.0f },
		{ &maybeVisibleDangerousWaves,   &maybeVisibleOtherWaves,   entsCache->getAllWaves(),   kWaveDetectionRadius }
	};

	const bool isTeamBasedGametype = GS_TeamBasedGametype( *ggs );
	const bool allowTeamDamage = g_allow_teamdamage->integer != 0;

	for( auto &[dangerous, other, entNums, distanceThreshold]: regularEntitiesToTest ) {
		for( const auto entNum: entNums ) {
			const auto *ent = gameEnts + entNum;
			assert( ent->s.type != ET_GRENADE );

			if( ent->s.ownerNum != botEnt->s.number ) [[likely]] {
				if( !isTeamBasedGametype || allowTeamDamage || ( botEnt->s.team != ent->s.team ) ) {
					const float squareDistance = DistanceSquared( botEnt->s.origin, ent->s.origin );
					if( squareDistance < wsw::square( distanceThreshold ) ) [[unlikely]] {
						dangerous->emplace_back( { .entNum = ent->s.number, .distance = Q_Sqrt( squareDistance ) } );
					} else if( !isTeamBasedGametype || ent->s.team != botEnt->s.team ) {
						other->emplace_back( { .entNum = ent->s.number, .distance = Q_Sqrt( squareDistance ) } );
					}
				}
			}
		}
	}

	if( const auto grenadeNumsSpan = entsCache->getAllGrenades(); !grenadeNumsSpan.empty() ) {
		const auto timeout = GS_GetWeaponDef( ggs, WEAP_GRENADELAUNCHER )->firedef.timeout;
		const bool allowSelfDamage = g_allow_selfdamage->integer != 0;
		for( const auto entNum: grenadeNumsSpan ) {
			const auto *ent = gameEnts + entNum;
			assert( ent->s.type == ET_GRENADE );

			const bool isOwnGrenade = ent->s.ownerNum == botEnt->s.number;
			if( isOwnGrenade ) [[unlikely]] {
				if( !allowSelfDamage ) {
					continue;
				}
				// Ignore own grenades in first 500 millis
				if( ent->nextThink - level.time > (int64_t) timeout - 500 ) {
					continue;
				}
			} else {
				if( isTeamBasedGametype && !allowTeamDamage && ent->s.team == botEnt->s.team ) {
					continue;
				}
			}

			const float squareDistance = DistanceSquared( ent->s.origin, ent->s.origin );
			const EntAndDistance entry = { .entNum = ent->s.number, .distance = Q_Sqrt( squareDistance ) };
			if( squareDistance < wsw::square( 300.0f ) ) [[unlikely]] {
				maybeVisibleDangerousGrenades.push_back( entry );
			} else if ( !isTeamBasedGametype || ent->s.team != botEnt->s.team ) {
				maybeVisibleOtherGrenades.push_back( entry );
			}
		}
	}

	// If all potentially dangerous entities have been processed successfully
	// (no entity has been rejected due to limit/capacity overflow)
	// filter other visible entities of the same kind.

	unsigned visCheckCallsQuotum = 32;

	// Process in order of importance (this gives greater quota for really dangerous stuff)

	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleDangerousRockets, &visibleDangerousRockets, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleDangerousWaves, &visibleDangerousWaves, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleDangerousGrenades, &visibleDangerousGrenades, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleDangerousLasers, &visibleDangerousLasers, botEnt,
											visCheckCallsQuotum, ::isLaserBeamInPvs, ::isLaserBeamVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleDangerousBlasts, &visibleDangerousBlasts, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleDangerousPlasmas, &visibleDangerousPlasmas, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );

	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleOtherRockets, &visibleOtherRockets, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleOtherWaves, &visibleOtherWaves, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleOtherGrenades, &visibleOtherGrenades, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleOtherLasers, &visibleOtherLasers, botEnt,
											visCheckCallsQuotum, ::isLaserBeamInPvs, ::isLaserBeamVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleOtherBlasts, &visibleOtherBlasts, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );
	visCheckCallsQuotum -= visCheckRawEnts( &maybeVisibleOtherPlasmas, &visibleOtherPlasmas, botEnt,
											visCheckCallsQuotum, ::isGenericEntityInPvs, ::isGenericProjectileVisible );

	// Try catching underflow
	assert( visCheckCallsQuotum <= 32 );

#if 0
	if( visibleDangerousRockets.size() + visibleOtherRockets.size() ) {
		Com_Printf( "Rockets  : %d, %d\n", visibleDangerousRockets.size(), visibleOtherRockets.size() );
	}
	if( visibleDangerousGrenades.size() + visibleOtherGrenades.size() ) {
		Com_Printf( "Grenades : %d, %d\n", visibleDangerousGrenades.size(), visibleOtherGrenades.size() );
	}
	if( visibleDangerousBlasts.size() + visibleOtherBlasts.size() ) {
		Com_Printf( "Blasts   : %d, %d\n", visibleDangerousBlasts.size(), visibleOtherBlasts.size() );
	}
	if( visibleDangerousLasers.size() + visibleOtherLasers.size() ) {
		Com_Printf( "Lasers   : %d, %d\n", visibleDangerousLasers.size(), visibleOtherLasers.size() );
	}
	if( visibleDangerousWaves.size() + visibleOtherWaves.size() ) {
		Com_Printf( "Waves    : %d, %d\n", visibleDangerousWaves.size(), visibleOtherWaves.size() );
	}
	if( visibleDangerousPlasmas.size() + visibleOtherPlasmas.size() ) {
		Com_Printf( "Plasmas  : %d, %d\n", visibleDangerousPlasmas.size(), visibleOtherPlasmas.size());
	}
#endif
}