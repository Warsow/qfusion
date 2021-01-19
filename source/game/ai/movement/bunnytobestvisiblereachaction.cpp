#include "bunnytobestvisiblereachaction.h"
#include "movementlocal.h"

struct Walker : public ReachChainWalker {
	const float *const botOrigin { nullptr };
	const aas_reachability_t *foundReach { nullptr };
	const AiAasWorld *const aasWorld;
	Vec3 result { 0, 0, 0 };

	explicit Walker( Context *context )
		: ReachChainWalker( context->RouteCache() )
		, botOrigin( context->movementState->entityPhysicsState.Origin() )
		, aasWorld( AiAasWorld::Instance() ) {}

	bool TestReachVis( const aas_reachability_t &reach, float playerZOffset = 0.0f, float reachZOffset = 12.0f );
	bool PerformRaycast( const aas_reachability_t &reach, float playerZOffset, float reachZOffset );

	bool Accept( int, const aas_reachability_t &reach, int travelTime ) override;

	bool Exec() override;
};

void BunnyToBestVisibleReachAction::PlanPredictionStep( Context *context ) {
	if( !GenericCheckIsActionEnabled( context ) ) {
		return;
	}

	if( !CheckCommonBunnyHopPreconditions( context ) ) {
		return;
	}

	Walker walker( context );
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	walker.SetAreaNums( entityPhysicsState, context->NavTargetAasAreaNum() );
	Vec3 intendedLookDir( entityPhysicsState.ForwardDir() );
    if( walker.Exec() ) {
    	walker.result.CopyTo( intendedLookDir );
    }

    if( !SetupBunnyHopping( intendedLookDir, context ) ) {
    	context->SetPendingRollback();
    }
}

bool Walker::Accept( int, const aas_reachability_t &reach, int travelTime ) {
	if( travelTime > 500 ) {
		return false;
	}

	const int travelType = reach.traveltype & TRAVELTYPE_MASK;
	if( travelType == TRAVEL_WALK ) {
		return TestReachVis( reach );
	}

	if( travelType != TRAVEL_WALKOFFLEDGE ) {
		if( travelType != TRAVEL_BARRIERJUMP ) {
			return false;
		}
		if( reach.end[2] - reach.start[2] > 40.0f ) {
			return false;
		}
		const float playerZOffset = playerbox_stand_viewheight;
		return TestReachVis( reach, playerZOffset );
	}

	if( reach.start[2] - reach.end[2] < 40.0f ) {
		return TestReachVis( reach );
	}

	if( aasWorld->AreaSettings()[reach.areanum].areaflags & AREA_NOFALL ) {
		return TestReachVis( reach );
	}

	return false;
}

bool Walker::PerformRaycast( const aas_reachability_t &reach, float playerZOffset, float reachZOffset ) {
	const auto *__restrict clusterNums = aasWorld->AreaFloorClusterNums();
	if( clusterNums[startAreaNum] && clusterNums[startAreaNum] == clusterNums[lastAreaNum] ) {
		return aasWorld->IsAreaWalkableInFloorCluster( startAreaNum, lastAreaNum );
	}

	trace_t trace;
	// Calling TraceArcInSolidWorld() seems to be way too expensive.
	// The reach chain gets straightened every prediction frame.
	Vec3 traceStart( botOrigin );
	traceStart.Z() += playerZOffset;
	Vec3 traceEnd( reach.start );
	traceEnd.Z() += reachZOffset;
	SolidWorldTrace( &trace, traceStart.Data(), traceEnd.Data() );
	return trace.fraction == 1.0f;
}

bool Walker::TestReachVis( const aas_reachability_t &reach, float playerZOffset, float reachZOffset ) {
	const float squareDistance = Distance2DSquared( botOrigin, reach.start );
	float markReachThreshold = 144.0f;

	if( startAreaNum != lastAreaNum ) {
		// Use a lower distance threshold for next areas in chain.
		// The higher value for the first reachability should prevent looping.
		markReachThreshold = 96.0f;

		// Interrupt at way too far reachabilities
		if( squareDistance > SQUARE( 512 ) ) {
			return false;
		}

		if( !PerformRaycast( reach, playerZOffset, reachZOffset ) ) {
			return false;
		}
	}

	// Unfortunately we have to check the trace first
	// as a sequence of successful vis checks from the beginning is expected
	if( squareDistance < SQUARE( markReachThreshold ) ) {
		// Continue walking the reach chain w/o marking the found reach
		return true;
	}

	foundReach = &reach;
	return true;
}

bool Walker::Exec() {
	if( ReachChainWalker::Exec() && foundReach ) {
		result.Set( foundReach->start );
		result -= botOrigin;
		result.NormalizeFast();
		return true;
	}
	return false;
}
