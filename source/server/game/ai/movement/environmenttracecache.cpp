#include "environmenttracecache.h"
#include "movementlocal.h"

/**
 * Makes a trace directory for a given direction number
 * @param dirNum a number of a direction among 8 tested ones
 * @param front2DDir a current front (forward) direction for a bot
 * @param right2DDir a current right direction for a bot
 * @param traceDir a result storage
 */
static inline void makeTraceDir( unsigned dirNum, const vec3_t front2DDir, const vec3_t right2DDir, vec3_t traceDir ) {
	const float *fractions = kSideDirFractions[dirNum];
	VectorScale( front2DDir, fractions[0], traceDir );
	VectorMA( traceDir, fractions[1], right2DDir, traceDir );
	VectorNormalizeFast( traceDir );
}

unsigned EnvironmentTraceCache::selectNonBlockedDirs( PredictionContext *context, unsigned *nonBlockedDirIndices ) {
	// Test for all 8 lower bits of full-height mask
	this->testForResultsMask( context, 0xFF );

	unsigned numNonBlockedDirs = 0;
	for( unsigned i = 0; i < 8; ++i ) {
		const TraceResult &traceResult = results[i];
		if( traceResult.IsEmpty() ) {
			nonBlockedDirIndices[numNonBlockedDirs++] = i;
		}
	}
	return numNonBlockedDirs;
}

void EnvironmentTraceCache::makeRandomizedKeyMovesToTarget( PredictionContext *context, const Vec3 &intendedMoveDir, int *keyMoves ) {
	unsigned nonBlockedDirIndices[8];
	unsigned numNonBlockedDirs = selectNonBlockedDirs( context, nonBlockedDirIndices );

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const Vec3 forwardDir( entityPhysicsState.ForwardDir() );
	const Vec3 rightDir( entityPhysicsState.RightDir() );
	assert( ( intendedMoveDir.Length() - 1.0f ) < 0.01f );

	// Choose randomly from all non-blocked dirs based on scores
	// For each non - blocked area make an interval having a corresponding to the area score length.
	// An interval is defined by lower and upper bounds.
	// Upper bounds are stored in the array.
	// Lower bounds are upper bounds of the previous array memner (if any) or 0 for the first array memeber.
	float dirDistributionUpperBound[8];
	float scoresSum = 0.0f;
	for( unsigned i = 0; i < numNonBlockedDirs; ++i ) {
		vec3_t keyMoveVec;
		const float *fractions = kSideDirFractions[nonBlockedDirIndices[i]];
		VectorScale( forwardDir.Data(), fractions[0], keyMoveVec );
		VectorMA( keyMoveVec, fractions[1], rightDir.Data(), keyMoveVec );
		scoresSum += 0.55f + 0.45f * intendedMoveDir.Dot( keyMoveVec );
		dirDistributionUpperBound[i] = scoresSum;
	}

	// A uniformly distributed random number in (0, scoresSum)
	const float rn = random() * scoresSum;
	for( unsigned i = 0; i < numNonBlockedDirs; ++i ) {
		if( rn > dirDistributionUpperBound[i] ) {
			continue;
		}

		int dirIndex = nonBlockedDirIndices[i];
		const int *dirMoves = kSideDirSigns[dirIndex];
		Vector2Copy( dirMoves, keyMoves );
		return;
	}

	Vector2Set( keyMoves, 0, 0 );
}

void EnvironmentTraceCache::makeKeyMovesToTarget( PredictionContext *context, const Vec3 &intendedMoveDir, int *keyMoves ) {
	unsigned nonBlockedDirIndices[8];
	unsigned numNonBlockedDirs = selectNonBlockedDirs( context, nonBlockedDirIndices );

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const Vec3 forwardDir( entityPhysicsState.ForwardDir() );
	const Vec3 rightDir( entityPhysicsState.RightDir() );
	assert( ( intendedMoveDir.Length() - 1.0f ) < 0.01f );

	float bestScore = 0.0f;
	auto bestDirIndex = std::numeric_limits<unsigned>::max();
	for( unsigned i = 0; i < numNonBlockedDirs; ++i ) {
		vec3_t keyMoveVec;
		unsigned dirIndex = nonBlockedDirIndices[i];
		const float *const fractions = kSideDirFractions[dirIndex];
		VectorScale( forwardDir.Data(), fractions[0], keyMoveVec );
		VectorMA( keyMoveVec, fractions[1], rightDir.Data(), keyMoveVec );
		float score = 0.55f + 0.45f * intendedMoveDir.Dot( keyMoveVec );
		if( score > bestScore ) {
			bestScore = score;
			bestDirIndex = dirIndex;
		}
	}
	if( bestScore > 0 ) {
		const int *dirMoves = kSideDirSigns[bestDirIndex];
		Vector2Copy( dirMoves, keyMoves );
		return;
	}

	Vector2Set( keyMoves, 0, 0 );
}

void EnvironmentTraceCache::makeRandomKeyMoves( PredictionContext *context, int *keyMoves ) {
	unsigned nonBlockedDirIndices[8];
	unsigned numNonBlockedDirs = selectNonBlockedDirs( context, nonBlockedDirIndices );
	if( numNonBlockedDirs ) {
		int dirIndex = nonBlockedDirIndices[(unsigned)( 0.9999f * numNonBlockedDirs * random() )];
		const int *const dirMoves = kSideDirSigns[dirIndex];
		Vector2Copy( dirMoves, keyMoves );
		return;
	}
	Vector2Set( keyMoves, 0, 0 );
}

void EnvironmentTraceCache::testForResultsMask( PredictionContext *context, unsigned requiredResultsMask ) {
	// There must not be any extra bits
	Assert( ( requiredResultsMask & ~0xFFFFu ) == 0 );
	// All required traces have been already cached
	if( ( this->resultsMask & requiredResultsMask ) == requiredResultsMask ) {
		return;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	vec3_t front2DDir, right2DDir, traceEnd;
	Vec3 angles( entityPhysicsState.Angles() );
	angles.Data()[PITCH] = 0.0f;
	AngleVectors( angles.Data(), front2DDir, right2DDir, nullptr );

	const float *origin = entityPhysicsState.Origin();
	constexpr auto contentsMask = MASK_SOLID | MASK_WATER;

	// First, test all full side traces.
	// If a full side trace is empty, a corresponding "jumpable" side trace can be set as empty too.

	// Test these bits for a quick computations shortcut
	const unsigned actualFullSides = this->resultsMask & 0xFFu;
	const unsigned resultFullSides = requiredResultsMask & 0xFFu;
	// If we do not have some of required result bit set
	if( ( actualFullSides & resultFullSides ) != resultFullSides ) {
		const auto *shapeList = getShapeListForPMoveCollision( context );

		vec3_t mins;
		VectorCopy( playerbox_stand_mins, mins );
		mins[2] += 1.0f;
		const float *const maxs = playerbox_stand_maxs;
		for( unsigned i = 0, mask = 1; i < 8; ++i, mask <<= 1 ) {
			// Skip not required sides
			if( !( mask & requiredResultsMask ) ) {
				continue;
			}
			// Skip already computed sides
			if( mask & this->resultsMask ) {
				continue;
			}

			makeTraceDir( i, front2DDir, right2DDir, traceEnd );
			// Save the trace dir
			TraceResult *const fullResult = &results[i];
			VectorCopy( traceEnd, fullResult->traceDir );
			// Convert from a direction to the end point
			VectorScale( traceEnd, kTraceDepth, traceEnd );
			VectorAdd( traceEnd, origin, traceEnd );
			// Compute the trace of the cached result
			SV_ClipToShapeList( shapeList, &fullResult->trace, origin, traceEnd, mins, maxs, contentsMask );
			this->resultsMask |= mask;
			if( fullResult->trace.fraction != 1.0f ) {
				continue;
			}

			// If full trace is empty, we can set partial trace as empty too
			// Results for jumpable height have indices shifted by 8
			TraceResult *const jumpableResult = &results[i + 8];
			jumpableResult->trace.fraction = 1.0f;
			VectorCopy( fullResult->traceDir, jumpableResult->traceDir );
			this->resultsMask |= ( mask << 8u );
		}
	}

	// Test these bits for quick computation shortcut
	const unsigned actualJumpableSides = this->resultsMask & 0xFF00u;
	const unsigned resultJumpableSides = requiredResultsMask & 0xFF00u;
	// If we do not have some of required result bit set
	if( ( actualJumpableSides & resultJumpableSides ) != resultJumpableSides ) {
		const auto *shapeList = getShapeListForPMoveCollision( context );

		vec3_t mins;
		VectorCopy( playerbox_stand_mins, mins );
		mins[2] += AI_JUMPABLE_HEIGHT;
		const float *const maxs = playerbox_stand_maxs;
		for( unsigned i = 0, mask = 0x100; i < 8; ++i, mask <<= 1 ) {
			// Skip not required sides
			if( !( mask & requiredResultsMask ) ) {
				continue;
			}
			// Skip already computed sides
			if( mask & this->resultsMask ) {
				continue;
			}

			makeTraceDir( i, front2DDir, right2DDir, traceEnd );
			// Save the trace dir
			TraceResult *const result = &results[i + 8];
			VectorCopy( traceEnd, result->traceDir );
			// Convert from a direction to the end point
			VectorScale( traceEnd, kTraceDepth, traceEnd );
			VectorAdd( traceEnd, origin, traceEnd );
			// Compute the trace of the cached result
			SV_ClipToShapeList( shapeList, &result->trace, origin, traceEnd, mins, maxs, contentsMask );
			this->resultsMask |= mask;
		}
	}

	// Check whether all requires side traces have been computed
	Assert( ( this->resultsMask & requiredResultsMask ) == requiredResultsMask );
}

const CMShapeList *EnvironmentTraceCache::getShapeListForPMoveCollision( PredictionContext *context ) {
    if( hasComputedShapeList ) {
        return cachedShapeList;
    }

    cachedShapeList = nullptr;
    hasComputedShapeList = true;

    const auto &__restrict physicsState = context->movementState->entityPhysicsState;
    // TODO: This is just a hack. There should be guarantees that millis for the
    // current step are always set prior to shape list retrieval in this frame
    const unsigned millis = wsw::max( context->predictionStepMillis, 48u );
    const float moveDist = ( 0.001f * (float)millis * physicsState.Speed() );
    const float sideExtent = wsw::max( kTraceDepth, moveDist );

    const float zExtent = wsw::max( moveDist, 16.0f );
    float topExtent, bottomExtent;
    if ( physicsState.Velocity()[2] > 0 ) {
        std::tie( topExtent, bottomExtent ) = std::make_pair( zExtent, 16.0f );
    } else {
        std::tie( topExtent, bottomExtent ) = std::make_pair( 16.0f, zExtent );
    }

    const float *__restrict origin = physicsState.Origin();
    Vec3 regionMins = Vec3( -sideExtent, -sideExtent, -bottomExtent );
    regionMins += playerbox_stand_mins;
    regionMins += origin;
    Vec3 regionMaxs = Vec3( +sideExtent, +sideExtent, +topExtent );
    regionMaxs += playerbox_stand_maxs;
    regionMaxs += origin;

    if( physicsState.GroundEntity() ) {
        bool isZeroStep = !context->topOfStackIndex;
        return ( cachedShapeList = shapesListCache.prepareList( regionMins.Data(), regionMaxs.Data(), isZeroStep ) );
    }

    const auto *aasWorld = AiAasWorld::instance();
    const int areaNum = physicsState.CurrAasAreaNum();
    const auto areaFlags = aasWorld->getAreaSettings()[areaNum].areaflags;

    if( ( areaFlags & AREA_SKIP_COLLISION_MASK ) && physicsState.HeightOverGround() > bottomExtent ) {
        if( sideExtent <= 32.0f ) {
            assert( areaFlags & AREA_SKIP_COLLISION_32 );
            return nullptr;
        }
        if( sideExtent <= 48.0f ) {
            if( areaFlags & AREA_SKIP_COLLISION_48 ) {
                return nullptr;
            }
        } else if( sideExtent <= 64.0f ) {
            if( areaFlags & AREA_SKIP_COLLISION_64 ) {
                return nullptr;
            }
        }
    }

    const int16_t *areaInnerBounds = aasWorld->getAreaInnerBounds( areaNum );
    vec3_t areaInnerMins, areaInnerMaxs;
    // Unpack
    VectorCopy( areaInnerBounds + 0, areaInnerMins );
    VectorCopy( areaInnerBounds + 3, areaInnerMaxs );

    // TODO: There should be a "withinBounds" subroutine, preferably using SIMD
    int i = 0;
    for(; i < 3; ++i ) {
        if( areaInnerMins[i] > regionMins.Data()[i] ) {
            break;
        }
        if( areaInnerMaxs[i] < regionMaxs.Data()[i] ) {
            break;
        }
    }

    // If the region is within the area inner bounds
    if( i == 3 ) {
        return nullptr;
    }

    bool isZeroStep = !context->topOfStackIndex;
    return ( cachedShapeList = shapesListCache.prepareList( regionMins.Data(), regionMaxs.Data(), isZeroStep ) );
}