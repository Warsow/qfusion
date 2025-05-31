#include "sidestepdodgeproblemsolver.h"
#include "spotsproblemsolverslocal.h"

bool SideStepDodgeProblemSolver::findSingle( vec_t *spotOrigin ) {
	trace_t trace;
	Vec3 tmpVec3( 0, 0, 0 );
	Vec3 *droppedToFloorOrigin = nullptr;
	int testedAreaNum = -1;
	if( auto originEntity = originParams.originEntity ) {
		if( originEntity->groundentity ) {
			tmpVec3.Set( originEntity->s.origin );
			tmpVec3.Z() += originEntity->r.mins[2];
			droppedToFloorOrigin = &tmpVec3;
		} else if( Bot *bot = originEntity->bot ) {
			const auto &entityPhysicsState = bot->EntityPhysicsState();
			if( entityPhysicsState->HeightOverGround() < 32.0f ) {
				tmpVec3.Set( entityPhysicsState->Origin() );
				tmpVec3.Z() += playerbox_stand_mins[2];
				tmpVec3.Z() -= entityPhysicsState->HeightOverGround();
				droppedToFloorOrigin = &tmpVec3;
			}
		} else {
			auto *ent = const_cast<edict_t *>( originEntity );
			Vec3 end( originEntity->s.origin );
			end.Z() += originEntity->r.mins[2] - 32.0f;
			G_Trace( &trace, ent->s.origin, ent->r.mins, ent->r.maxs, end.Data(), ent, MASK_SOLID );
			if( trace.fraction != 1.0f && ISWALKABLEPLANE( &trace.plane ) ) {
				tmpVec3.Set( trace.endpos );
				droppedToFloorOrigin = &tmpVec3;
			}
		}
	} else {
		Vec3 end( originParams.origin );
		end.Z() -= 48.0f;
		G_Trace( &trace, const_cast<float *>( originParams.origin ), nullptr, nullptr, end.Data(), nullptr, MASK_SOLID );
		if( trace.fraction != 1.0f && ISWALKABLEPLANE( &trace.plane ) ) {
			tmpVec3.Set( trace.endpos );
			droppedToFloorOrigin = &tmpVec3;
		}
	}

	if( !droppedToFloorOrigin ) {
		return false;
	}

	float velocityInfluence = 0.0f;
	vec3_t velocityDir { 0.0f, 0.0f, 0.0f };
	// Perform deferred retrieval of these additional parameters
	if( const auto *originEntity = originParams.originEntity ) {
		if( Bot *bot = originEntity->bot ) {
			const auto &entityPhysicsState = bot->EntityPhysicsState();
			int areaNums[2] { 0, 0 };
			entityPhysicsState->PrepareRoutingStartAreas( areaNums );
			testedAreaNum = areaNums[0];
			if( !originEntity->groundentity ) {
				float speed = entityPhysicsState->Speed();
				if( speed > GS_DefaultPlayerSpeed( *ggs ) ) {
					velocityInfluence = 0.75f;
				} else if( speed > 1 ) {
					velocityInfluence = 0.5f;
				}
				if( velocityInfluence > 0 ) {
					VectorCopy( entityPhysicsState->Velocity(), velocityDir );
					VectorScale( velocityDir, 1.0f / speed, velocityDir );
				}
			}
		}
	}

	Vec3 testedOrigin( *droppedToFloorOrigin );
	// Make sure the tested box will be at least few units above the ground
	testedOrigin.Z() += ( -playerbox_stand_mins[2] ) + 3.0f;

	Vec3 keepVisible2DDir( problemParams.keepVisibleOrigin );
	keepVisible2DDir -= originParams.origin;
	keepVisible2DDir.Z() = 0;

	const auto checkAgainstKeepVisibleOrigin = (bool)keepVisible2DDir.normalize();

	edict_t *const ignore = originParams.originEntity ? const_cast<edict_t *>( originParams.originEntity ) : nullptr;

	float bestScore = -1.0f;
	vec3_t bestVec { 0, 0, 0 };

	const auto *const aasWorld = AiAasWorld::instance();
	// Check whether a computation of this area num has been deferred
	if( testedAreaNum < 0 ) {
		testedAreaNum = aasWorld->findAreaNum( testedOrigin );
	}

	// If the tested area num belongs to an AAS floor cluster
	// we should test all nearby spots in the cluster
	// as they are almost guaranteed to be walkable if
	// isAreaWalkableInFloorCluster() call succeeds.
	// We should prefer tactical spots over random origins.
	// TODO: We can group all spots by floor cluster nums at loading
	if( aasWorld->floorClusterNum( testedAreaNum ) ) {
		uint16_t insideSpotNum;
		const auto *const __restrict spots = tacticalSpotsRegistry->spots;
		// Must be cleaned up... todo: return a RAII wrapper?
		const auto &queryVector = tacticalSpotsRegistry->FindSpotsInRadius( originParams, &insideSpotNum );
		for( auto spotNum: queryVector ) {
			const auto &spot = spots[spotNum];
			// Skip nearby spots
			if( spotNum == insideSpotNum ) {
				continue;
			}

			Vec3 vec( spot.origin );
			vec -= originParams.origin;
			vec.Z() = 0;

			const float squareDistance = vec.SquaredLength();
			if( squareDistance < 32 * 32 ) {
				continue;
			}

			// Check walkability in the cluster
			if( !aasWorld->isAreaWalkableInFloorCluster( testedAreaNum, spot.aasAreaNum ) ) {
				continue;
			}

			if( checkAgainstKeepVisibleOrigin ) {
				// Check actual visibility of the origin specified in problem params
				G_Trace( &trace, testedOrigin.Data(), nullptr, nullptr, problemParams.keepVisibleOrigin, ignore, MASK_SOLID );
				if( trace.fraction != 1.0f || trace.startsolid ) {
					continue;
				}
			}

			Vec3 dir( vec );
			const float distance = std::sqrt( squareDistance );
			dir *= 1.0f / distance;

			float score;
			// Give side spots a greater score, but make sure the score is always positive for each spot
			if( checkAgainstKeepVisibleOrigin ) {
				score = ( 1.0f - fabsf( dir.Dot( keepVisible2DDir ) ) ) + 0.1f;
			} else {
				score = 1.0f;
			}

			// Modulate score by distance (give nearby spots a priority too)
			score *= 0.5f + 0.5f * ( 1.0f - ( distance / originParams.searchRadius ) );
			// Modulate by velocity influence
			score *= ( 1.0f - velocityInfluence ) + velocityInfluence * ( 1.0f + dir.Dot( velocityDir ) );
			if( score <= bestScore ) {
				continue;
			}

			bestScore = score;
			vec.CopyTo( bestVec );
		}

		if( bestScore > 0 ) {
			VectorCopy( bestVec, spotOrigin );
			VectorAdd( spotOrigin, testedOrigin.Data(), spotOrigin );
			return true;
		}
	}

	bestScore = -1.0f;
	vec3_t bestDir { 0, 0, 0 };

	const float dx = playerbox_stand_maxs[0] - playerbox_stand_mins[0];
	const float dy = playerbox_stand_maxs[1] - playerbox_stand_mins[1];
	for( int i = -1; i <= 1; ++i ) {
		for( int j = -1; j <= 1; ++j ) {
			// Disallow diagonal moves (can't be checked so easily by just adding an offset)
			// Disallow staying at the current origin as well.
			if( ( i && j ) || !( i || j ) ) {
				continue;
			}

			testedOrigin.X() = droppedToFloorOrigin->X() + i * dx;
			testedOrigin.Y() = droppedToFloorOrigin->Y() + j * dy;

			// Check actual visibility of the origin specified in problem params
			G_Trace( &trace, testedOrigin.Data(), nullptr, nullptr, problemParams.keepVisibleOrigin, ignore, MASK_SOLID );
			if( trace.fraction != 1.0f || trace.startsolid ) {
				continue;
			}

			const float *mins = playerbox_stand_mins;
			const float *maxs = playerbox_stand_maxs;
			// Test whether the box intersects solid
			G_Trace( &trace, testedOrigin.Data(), mins, maxs, testedOrigin.Data(), ignore, MASK_SOLID );
			if( trace.fraction != 1.0f || trace.startsolid ) {
				continue;
			}

			// Check ground below
			Vec3 groundTraceEnd( testedOrigin );
			groundTraceEnd.Z() -= 72.0f;
			G_Trace( &trace, testedOrigin.Data(), nullptr, nullptr, groundTraceEnd.Data(), ignore, MASK_SOLID );
			if( trace.fraction == 1.0f || trace.allsolid ) {
				continue;
			}
			if( !ISWALKABLEPLANE( &trace.plane ) ) {
				continue;
			}
			if( trace.contents & ( CONTENTS_LAVA | CONTENTS_SLIME | CONTENTS_NODROP | CONTENTS_DONOTENTER ) ) {
				continue;
			}

			Vec3 spot2DDir( testedOrigin.Data() );
			spot2DDir -= originParams.origin;
			spot2DDir.Z() = 0;
			if( !spot2DDir.normalize() ) {
				continue;
			}

			// Give side spots a greater score, but make sure the score is always positive for each spot
			float score = ( 1.0f - std::fabs( spot2DDir.Dot( keepVisible2DDir ) ) ) + 0.1f;
			// Modulate by velocity influence
			score *= ( 1.0f - velocityInfluence ) + velocityInfluence * ( 1.0f + spot2DDir.Dot( velocityDir ) );
			if( score <= bestScore ) {
				continue;
			}

			bestScore = score;
			spot2DDir.CopyTo( bestDir );
		}
	}

	if( bestScore > 0 ) {
		VectorCopy( bestDir, spotOrigin );
		VectorScale( spotOrigin, sqrtf( dx * dx + dy * dy ), spotOrigin );
		VectorAdd( spotOrigin, testedOrigin.Data(), spotOrigin );
		return true;
	}

	return false;
}