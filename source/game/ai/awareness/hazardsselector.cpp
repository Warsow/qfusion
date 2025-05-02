#include "hazardsselector.h"
#include "awarenessmodule.h"
#include "../bot.h"
#include "../../../common/wswalgorithm.h"

void HazardsSelector::BeginUpdate() {
	if( primaryHazard ) {
		primaryHazard->DeleteSelf();
	}

	primaryHazard = nullptr;
}

void HazardsSelector::EndUpdate() {
	// Set the primary hazard timeout after all (TODO: why not from beginning?)
	if( primaryHazard ) {
		primaryHazard->timeoutAt = level.time + Hazard::TIMEOUT;
	}
}

bool HazardsSelector::TryAddHazard( float damageScore,
									const vec3_t hitPoint,
									const vec3_t direction,
									const edict_t *owner,
									float splashRadius ) {
	if( primaryHazard ) {
		if( primaryHazard->damage >= damageScore ) {
			return false;
		}
	}

	if( Hazard *hazard = hazardsPool.New() ) {
		hazard->damage = damageScore;
		hazard->hitPoint.Set( hitPoint );
		hazard->direction.Set( direction );
		hazard->attacker = owner;
		hazard->splashRadius = splashRadius;
		if( primaryHazard ) {
			primaryHazard->DeleteSelf();
		}
		primaryHazard = hazard;
		return true;
	}

	return false;
}

class PlasmaBeam {
	friend class PlasmaBeamsBuilder;
public:
	explicit PlasmaBeam( const edict_t *firstProjectile )
		: m_startProjectile( firstProjectile )
		, m_endProjectile( firstProjectile )
		, m_ownerNum( firstProjectile->s.ownerNum )
		, m_damage( firstProjectile->projectileInfo.maxDamage ) {}

	const edict_t *m_startProjectile { nullptr };
	const edict_t *m_endProjectile { nullptr };
	int m_ownerNum { 0 };
	float m_damage { 0.0f };

	[[nodiscard]]
	auto startOrigin() const { return Vec3( m_startProjectile->s.origin ); }
	[[nodiscard]]
	auto endOrigin() const { return Vec3( m_endProjectile->s.origin ); }

	void addProjectile( const edict_t *nextProjectile ) {
		// Consider the owner to be undefined (it's the computationally cheaper alternative)
		if( m_startProjectile->s.ownerNum != nextProjectile->s.ownerNum ) [[unlikely]] {
			m_ownerNum = 0;
		}
		m_endProjectile = nextProjectile;
		m_damage += nextProjectile->projectileInfo.maxDamage;
	}
};

struct EntAndLineParam {
	int entNum;
	float t;

	// Evict entities with lesser line parameters from the heap first
	[[nodiscard]]
	bool operator<( const EntAndLineParam &that ) const { return t > that.t; }
};

class PlasmaBeamsBuilder;

class SameDirBeamsList {
	friend class PlasmaBeamsBuilder;

	// Non-null for non-discarded beams
	PlasmaBeam *m_plasmaBeams { nullptr };
	EntAndLineParam *m_projectilesHeap { nullptr };

	Vec3 m_avgDirection;
	// All projectiles in this list belong to this line defined as a (point, direction) pair
	const Vec3 m_lineEquationPoint;
	unsigned m_projectilesCount { 0 };
	unsigned m_plasmaBeamsCount { 0 };
	// These objects are kept even if they are discarded as they act as spatial bins for directions
	bool m_isDiscarded { false };
public:
	SameDirBeamsList( const edict_t *firstEntity, const edict_t *bot, PlasmaBeamsBuilder *parent );

	[[nodiscard]]
	bool tryAddingProjectile( const edict_t *projectile );

	void buildBeams();

	[[nodiscard]]
	auto calcLineEquationParam( const edict_t *projectile ) -> float {
		const float *origin = projectile->s.origin;

		if( std::fabs( m_avgDirection.X() ) > 0.1f ) {
			return ( origin[0] - m_lineEquationPoint.X() ) * Q_Rcp( m_avgDirection.X() );
		}
		if( std::fabs( m_avgDirection.Y() ) > 0.1f ) {
			return ( origin[1] - m_lineEquationPoint.Y() ) * Q_Rcp( m_avgDirection.Y() );
		}
		return ( origin[2] - m_lineEquationPoint.Z() ) * Q_Rcp( m_avgDirection.Z() );
	}
};

class PlasmaBeamsBuilder {
	static constexpr unsigned kMaxAcceptedProjectiles = 64;
	static constexpr unsigned kNumListsForDirections  = kMaxAcceptedProjectiles;
	static constexpr unsigned kMaxNonDiscardedLists   = 24;

	struct alignas( EntAndLineParam ) EntAndLineParamBuffer {
		char _contents[sizeof( EntAndLineParam ) * kMaxAcceptedProjectiles];
	};

	struct alignas( PlasmaBeam ) PlasmaBeamsBuffer {
		char _contents[sizeof( PlasmaBeam ) * kMaxAcceptedProjectiles];
	};

	EntAndLineParamBuffer m_entAndLineParamBuffers[kMaxNonDiscardedLists];
	PlasmaBeamsBuffer m_plasmaBeamsBuffers[kMaxNonDiscardedLists];
	unsigned m_numBuffersInUse { 0 };

	wsw::StaticVector<SameDirBeamsList, kNumListsForDirections> m_sameDirLists;

	const edict_t *const m_bot;
public:
	explicit PlasmaBeamsBuilder( const edict_t *bot ) : m_bot( bot ) {}

	[[nodiscard]]
	auto allocNextBuffers() -> std::pair<void *, void *> {
		assert( m_numBuffersInUse < kMaxNonDiscardedLists );
		void *first  = &m_entAndLineParamBuffers[m_numBuffersInUse];
		void *second = &m_plasmaBeamsBuffers[m_numBuffersInUse];
		m_numBuffersInUse++;
		return std::make_pair( first, second );
	}

	void addProjectilesByEntNums( const EntNumsVector &entNums );
	void findMostHazardousBeams( HazardsSelector *hazardsSelector );
};

// Make sure we don't allocate on stack too much.
// TODO: Let the caller use alloca() and supply buffers to the constructor?
static_assert( sizeof( PlasmaBeamsBuilder ) < 1u << 16 );

SameDirBeamsList::SameDirBeamsList( const edict_t *firstEntity, const edict_t *bot, PlasmaBeamsBuilder *parent )
	: m_avgDirection( firstEntity->velocity ), m_lineEquationPoint( firstEntity->s.origin ) {
	if( m_avgDirection.normalizeFast() ) [[likely]] {
		const Vec3 botToLinePoint = m_lineEquationPoint - bot->s.origin;
		const float squaredDistToBeamLine = botToLinePoint.Cross( m_avgDirection ).SquaredLength();
		if( squaredDistToBeamLine < wsw::square( 200.0f ) ) {
			auto [projectilesBuffer, beamsBuffer] = parent->allocNextBuffers();
			m_projectilesHeap = (EntAndLineParam *)projectilesBuffer;
			m_plasmaBeams     = (PlasmaBeam *)beamsBuffer;

			m_projectilesHeap[m_projectilesCount++] = {
				.entNum = firstEntity->s.number, .t = calcLineEquationParam( firstEntity )
			};
		} else {
			m_isDiscarded = true;
		}
	} else {
		m_isDiscarded = true;
	}
}

bool SameDirBeamsList::tryAddingProjectile( const edict_t *projectile ) {
	if( Vec3 velocityDir( projectile->velocity ); velocityDir.normalizeFast() ) {
		if( velocityDir.Dot( m_avgDirection ) > 0.995f ) {
			// Just consume the projectile (it belongs to this spatial bin) but don't actually add
			if( !m_isDiscarded ) {
				// Update the average direction
				m_avgDirection += velocityDir;
				// This never fails as dirs are spatially close
				(void)m_avgDirection.normalizeFast();

				m_projectilesHeap[m_projectilesCount++] = {
					.entNum = projectile->s.number, .t = calcLineEquationParam( projectile )
				};
				// TODO: Add/use specialized subroutines for using heaps (similar to wsw::sortByField())
				wsw::push_heap( m_projectilesHeap, m_projectilesHeap + m_projectilesCount );
			}
			return true;
		}
	}
	return false;
}

void SameDirBeamsList::buildBeams() {
	if( !m_isDiscarded ) {
		assert( m_projectilesCount );

		const edict_t *const gameEnts = game.edicts;

		// Get the projectile that has a maximal line equation parameter
		wsw::pop_heap( m_projectilesHeap, m_projectilesHeap + m_projectilesCount );
		--m_projectilesCount;
		const edict_t *prevProjectile = gameEnts + m_projectilesHeap[m_projectilesCount].entNum;

		m_plasmaBeams[m_plasmaBeamsCount++] = PlasmaBeam( prevProjectile );

		while( m_projectilesCount > 0 ) {
			// Get the projectile with the maximal line parameter remaining so far
			wsw::pop_heap( m_projectilesHeap, m_projectilesHeap + m_projectilesCount );
			--m_projectilesCount;
			const edict_t *currProjectile = gameEnts + m_projectilesHeap[m_projectilesCount].entNum;

			const float prevToCurrDistance = DistanceSquared( prevProjectile->s.origin, currProjectile->s.origin );
			if( prevToCurrDistance < wsw::square( 300.0f ) ) {
				// Add the projectile to the last beam
				m_plasmaBeams[m_plasmaBeamsCount - 1].addProjectile( currProjectile );
			} else {
				// Construct new plasma beam at the end of beams array
				m_plasmaBeams[m_plasmaBeamsCount++] = PlasmaBeam( currProjectile );
			}
		}
	}
}

void PlasmaBeamsBuilder::addProjectilesByEntNums( const EntNumsVector &entNums ) {
	const edict_t *const gameEnts = game.edicts;

	// Don't take more than kMaxAcceptedProjectiles entities.
	// Even if they all go to the same list, a list cannot accept more than kMaxAcceptedProjectiles entities.
	for( unsigned i = 0, limit = wsw::min( entNums.size(), kMaxAcceptedProjectiles ); i < limit; ++i ) {
		const edict_t *const ent = gameEnts + entNums[i];
		assert( ent->s.type == ET_PLASMA );

		bool hasAddedEntity = false;
		for( SameDirBeamsList &listForDir: m_sameDirLists ) {
			if( listForDir.tryAddingProjectile( ent ) ) {
				hasAddedEntity = true;
				break;
			}
		}

		if( !hasAddedEntity ) {
			if( m_numBuffersInUse < kMaxNonDiscardedLists ) {
				void *mem = m_sameDirLists.unsafe_grow_back();
				new( mem )SameDirBeamsList( ent, m_bot, this );
			} else {
				// Stop all processing at this.
				return;
			}
		}
	}
}

void PlasmaBeamsBuilder::findMostHazardousBeams( HazardsSelector *hazardsSelector ) {
	for( SameDirBeamsList &list: m_sameDirLists ) {
		list.buildBeams();
	}

	const auto *const weaponDef  = GS_GetWeaponDef( ggs, WEAP_PLASMAGUN );
	const auto *const gameEnts   = game.edicts;
	const float *const botOrigin = m_bot->s.origin;

	float splashRadius = 0.0f;
	// Consider it to be twice the average
	splashRadius += (float)weaponDef->firedef.splash_radius;
	splashRadius += (float)weaponDef->firedef_weak.splash_radius;

	float maxDamageScoreSoFar = 0.0f;

	for( const SameDirBeamsList &beamsList: m_sameDirLists ) {
		if( beamsList.m_isDiscarded ) {
			continue;
		}

		for( unsigned i = 0; i < beamsList.m_plasmaBeamsCount; ++i ) {
			const PlasmaBeam *const beam = beamsList.m_plasmaBeams + i;

			// Do cheap dot product -based tests first

			if( beam->m_startProjectile == beam->m_endProjectile ) {
				const Vec3 botToBeam = beam->startOrigin() - botOrigin;
				// If this projectile has entirely passed the bot and is flying away, skip it
				if( botToBeam.Dot( beamsList.m_avgDirection ) > 0 ) {
					continue;
				}
			} else {
				const Vec3 botToBeamStart = beam->startOrigin() - botOrigin;
				const Vec3 botToBeamEnd   = beam->endOrigin() - botOrigin;

				const float dotBotToStartWithDir = botToBeamStart.Dot( beamsList.m_avgDirection );
				const float dotBotToEndWithDir   = botToBeamEnd.Dot( beamsList.m_avgDirection );

				// If the aggregate beam has entirely passed the bot and is flying away, skip it
				if( dotBotToStartWithDir > 0 && dotBotToEndWithDir > 0 ) {
					continue;
				}
			}

			// It works for single-projectile beams too
			const Vec3 tracedBeamStart = beam->startOrigin();
			const Vec3 tracedBeamEnd   = beam->endOrigin() + 512.0f * beamsList.m_avgDirection;

			trace_t trace;
			// PVS prior checks are useless in this case.
			// They will always pass, except some bizarre case with merging beams from different sides of a wall,
			// which is extremely unlikely.
			// TODO: Test against some kind of "fat" bot bounds
			// TODO: Check line-to-bot distance, assuming hit point is past the bot with regard to line parameter
			G_Trace( &trace, tracedBeamStart.Data(), nullptr, nullptr, tracedBeamEnd.Data(), nullptr, MASK_SHOT );
			if( trace.fraction != 1.0f ) {
				// Direct hit
				if( m_bot == gameEnts + trace.ent ) {
					// Raise score for blatant directs
					const float damageScore = 1.5f * beam->m_damage;
					const edict_t *owner    = beam->m_ownerNum ? gameEnts + beam->m_ownerNum : nullptr;
					if( damageScore > maxDamageScoreSoFar ) {
						if( hazardsSelector->TryAddHazard( damageScore, trace.endpos,
														   beamsList.m_avgDirection.Data(),
														   owner, splashRadius ) ) {
							maxDamageScoreSoFar = damageScore;
						}
					}
				} else {
					const float hitToBotDistance = DistanceFast( botOrigin, trace.endpos );
					if( hitToBotDistance < wsw::square( splashRadius ) ) {
						// Check also the trace between the hit point and the bot.
						// This still fails for pillar-like environment, but it's better than nothing.
						const Vec3 hitPoint( Vec3( trace.endpos ) + trace.plane.normal );
						G_Trace( &trace, botOrigin, nullptr, nullptr, hitPoint.Data(), m_bot, MASK_SHOT );
						if( trace.fraction == 1.0f ) {
							const float distanceFrac = hitToBotDistance * Q_Rcp( splashRadius );
							const float damageScore  = beam->m_damage * ( 1.0f - distanceFrac );
							const edict_t *owner     = beam->m_ownerNum ? gameEnts + beam->m_ownerNum : nullptr;
							if( damageScore > maxDamageScoreSoFar ) {
								if( hazardsSelector->TryAddHazard( damageScore, trace.endpos,
																	beamsList.m_avgDirection.Data(),
																	owner, splashRadius ) ) {
									maxDamageScoreSoFar = damageScore;
								}
							}
						}
					}
				}
			}
		}
	}
}

void HazardsSelector::FindWaveHazards( const EntNumsVector &entNums ) {
	auto *const gameEdicts = game.edicts;
	const auto *weaponDef = GS_GetWeaponDef( ggs, WEAP_SHOCKWAVE );
	const edict_t *self = game.edicts + bot->EntNum();
	trace_t trace;
	for( auto entNum: entNums ) {
		edict_t *wave = gameEdicts + entNum;
		float hazardRadius;
		if( wave->style == MOD_SHOCKWAVE_S ) {
			hazardRadius = weaponDef->firedef.splash_radius + 32.0f;
		} else {
			hazardRadius = weaponDef->firedef_weak.splash_radius + 24.0f;
		}

		// We try checking whether the wave passes near the bot inflicting a corona damage.
		// TODO: This code assumes that the bot origin remains the same.
		// This is not so bad because hazards are checked each Think() frame
		// and there is some additional extent applied to the damage radius,
		// but it would be nice to predict an actual trajectory intersection.

		// Compute a distance from wave linear movement line to bot
		Vec3 lineDir( wave->velocity );
		float squareSpeed = lineDir.SquaredLength();
		if( squareSpeed < 1 ) {
			continue;
		}

		float waveSpeed = Q_Sqrt( squareSpeed );
		Vec3 botToLinePoint( wave->s.origin );
		botToLinePoint -= self->s.origin;
		Vec3 projection( lineDir );
		projection *= botToLinePoint.Dot( lineDir );
		Vec3 perpendicular( botToLinePoint );
		perpendicular -= projection;
		const float squareDistance =  perpendicular.SquaredLength();
		if( squareDistance > hazardRadius * hazardRadius ) {
			continue;
		}

		// We're sure the wave is in PVS and is visible by bot, that's what HazardsDetector yields
		// Now check whether the wave hits an obstacle on a safe distance.

		Vec3 traceEnd( lineDir );
		traceEnd *= HazardsDetector::kWaveDetectionRadius * Q_Rcp( waveSpeed );
		traceEnd += wave->s.origin;
		G_Trace( &trace, wave->s.origin, nullptr, nullptr, traceEnd.Data(), wave, MASK_SHOT );
		bool isDirectHit = false;
		if( trace.fraction != 1.0f ) {
			if( DistanceSquared( trace.endpos, self->s.origin ) > hazardRadius * hazardRadius ) {
				continue;
			}
			isDirectHit = ( trace.ent == ENTNUM( self ) );
		}
		// Put the likely case first
		float damage = wave->projectileInfo.maxDamage;
		if( !isDirectHit ) {
			float distance = Q_Sqrt( squareDistance );
			float damageScore = damage * ( 3.0f - 2.0f * ( distance / hazardRadius ) );
			// Treat the nearest point on the line as a hit point
			// perpendicular = hitPoint - self->s.origin;
			Vec3 hitPoint( perpendicular );
			hitPoint += self->s.origin;
			Vec3 hitDir( perpendicular );
			hitDir *= 1.0f / distance;
			TryAddHazard( damageScore, hitPoint.Data(), hitDir.Data(), gameEdicts + wave->s.ownerNum, hazardRadius );
		} else {
			TryAddHazard( 3.0f * damage, trace.endpos, lineDir.Data(), gameEdicts + wave->s.ownerNum, hazardRadius );
		}
	}
}

void HazardsSelector::FindPlasmaHazards( const EntNumsVector &entNums ) {
	PlasmaBeamsBuilder plasmaBeamsBuilder( game.edicts + bot->EntNum() );
	plasmaBeamsBuilder.addProjectilesByEntNums( entNums );
	plasmaBeamsBuilder.findMostHazardousBeams( this );
}

void HazardsSelector::FindLaserHazards( const EntNumsVector &entNums ) {
	trace_t trace;
	edict_t *const gameEdicts = game.edicts;
	const edict_t *self = game.edicts + bot->EntNum();
	float maxDamageScore = 0.0f;

	for( unsigned i = 0; i < entNums.size(); ++i ) {
		edict_t *beam = gameEdicts + entNums[i];
		G_Trace( &trace, beam->s.origin, vec3_origin, vec3_origin, beam->s.origin2, beam, MASK_AISOLID );
		if( trace.fraction == 1.0f ) {
			continue;
		}

		if( self != game.edicts + trace.ent ) {
			continue;
		}

		edict_t *owner = game.edicts + beam->s.ownerNum;

		Vec3 direction( beam->s.origin2 );
		direction -= beam->s.origin;
		float squareLen = direction.SquaredLength();
		if( squareLen > 1 ) {
			direction *= 1.0f / sqrtf( squareLen );
		} else {
			// Very rare but really seen case - beam has zero length
			vec3_t forward, right, up;
			AngleVectors( owner->s.angles, forward, right, up );
			direction += forward;
			direction += right;
			direction += up;
			direction.normalizeFastOrThrow();
		}

		// Modify potential damage from a beam by its owner accuracy
		float damageScore = 50.0f;
		if( owner->team != self->team && owner->r.client ) {
			const auto &ownerStats = owner->r.client->stats;
			if( ownerStats.accuracy_shots[AMMO_LASERS] > 10 ) {
				float extraDamage = 75.0f;
				extraDamage *= ownerStats.accuracy_hits[AMMO_LASERS];
				extraDamage /= ownerStats.accuracy_shots[AMMO_LASERS];
				damageScore += extraDamage;
			}
		}

		if( damageScore > maxDamageScore ) {
			if( TryAddHazard( damageScore, trace.endpos, direction.Data(), owner, 0.0f ) ) {
				maxDamageScore = damageScore;
			}
		}
	}
}

void HazardsSelector::FindProjectileHazards( const EntNumsVector &entNums ) {
	trace_t trace;
	float minPrjFraction = 1.0f;
	float minDamageScore = 0.0f;
	edict_t *const gameEdicts = game.edicts;

	for( unsigned i = 0; i < entNums.size(); ++i ) {
		edict_t *target = gameEdicts + entNums[i];
		Vec3 end = Vec3( target->s.origin ) + 2.0f * Vec3( target->velocity );
		G_Trace( &trace, target->s.origin, target->r.mins, target->r.maxs, end.Data(), target, MASK_AISOLID );
		if( trace.fraction >= minPrjFraction ) {
			continue;
		}

		minPrjFraction = trace.fraction;
		float hitVecLen = DistanceFast( bot->Origin(), trace.endpos );
		if( hitVecLen >= 1.25f * target->projectileInfo.radius ) {
			continue;
		}

		float damageScore = target->projectileInfo.maxDamage;
		damageScore *= 1.0f - hitVecLen / ( 1.25f * target->projectileInfo.radius );
		if( damageScore <= minDamageScore ) {
			continue;
		}

		// Velocity may be zero for some projectiles (e.g. grenades)
		Vec3 direction( target->velocity );
		float squaredLen = direction.SquaredLength();
		if( squaredLen > 0.1f ) {
			direction *= 1.0f / sqrtf( squaredLen );
		} else {
			direction = Vec3( &axis_identity[AXIS_UP] );
		}
		if( TryAddHazard( damageScore, trace.endpos, direction.Data(),
						  gameEdicts + target->s.ownerNum,
						  1.25f * target->projectileInfo.radius ) ) {
			minDamageScore = damageScore;
		}
	}
}