#include "usewalkablenodescript.h"
#include "movementlocal.h"

void UseWalkableNodeScript::Activate( const vec3_t nodeOrigin_, float reachRadius_, int nodeAasAreaNum_, unsigned timeout_ ) {
	VectorCopy( nodeOrigin_, this->nodeOrigin );
	this->reachRadius = reachRadius_;
	this->nodeAasAreaNum = nodeAasAreaNum_;
	this->timeout = timeout_;
	if( !nodeAasAreaNum ) {
		nodeAasAreaNum = AiAasWorld::instance()->findAreaNum( nodeOrigin_ );
	}
	GenericGroundMovementScript::Activate();
}

bool UseWalkableNodeScript::TryDeactivate( PredictionContext *context ) {
	// Call the superclass method first
	if( GenericGroundMovementScript::TryDeactivate( context ) ) {
		return true;
	}

	// If the spot can be reached by radius
	const float *botOrigin = context ? context->movementState->entityPhysicsState.Origin() : bot->Origin();
	if( Distance2DSquared( botOrigin, nodeOrigin ) < wsw::square( reachRadius ) ) {
		status = COMPLETED;
		return true;
	}

	if( GenericGroundMovementScript::ShouldSkipTests( context )) {
		return false;
	}

	if( level.time - activatedAt > timeout ) {
		status = INVALID;
		return true;
	}

	if( !TestActualWalkability( nodeAasAreaNum, nodeOrigin, context ) ) {
		status = INVALID;
		return true;
	}

	return false;
}

MovementScript *FallbackAction::TryFindWalkReachFallback( PredictionContext *context, const aas_reachability_t &nextReach ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	// Allow following WALK reachabilities but make sure
	// they do not lead to junk areas and are fairly far away to prevent looping.
	float squareDistance = DistanceSquared( entityPhysicsState.Origin(), nextReach.end );
	if( squareDistance < wsw::square( 32.0f ) ) {
		return nullptr;
	}

	const auto &areaSettings = AiAasWorld::instance()->getAreaSettings()[nextReach.areanum];
	if( areaSettings.areaflags & AREA_JUNK ) {
		return nullptr;
	}

	auto *script = &m_subsystem->useWalkableNodeScript;
	unsigned timeout = (unsigned)( 1000.0f * sqrtf( squareDistance ) / context->GetRunSpeed() );
	// Note: We have to add several units to the target Z, otherwise a collision test
	// on next frame is very likely to immediately deactivate it
	Vec3 target( nextReach.end );
	target.Z() += -playerbox_stand_mins[2];
	script->Activate( target.Data(), 16.0f, AiAasWorld::instance()->findAreaNum( target ), timeout );
	return script;
}

MovementScript *FallbackAction::TryFindNearbyRampAreasFallback( PredictionContext *context ) {
	int currGroundedAreaNum = context->CurrGroundedAasAreaNum();
	if( !currGroundedAreaNum ) {
		return nullptr;
	}

	const auto *aasWorld = AiAasWorld::instance();
	const auto aasAreaSettings = aasWorld->getAreaSettings();
	const auto aasAreas = aasWorld->getAreas();
	const auto aasReach = aasWorld->getReaches();

	const auto &currAreaSettings = aasAreaSettings[currGroundedAreaNum];
	int reachNum = currAreaSettings.firstreachablearea;
	const int endReachNum = reachNum + currAreaSettings.numreachableareas;
	for(; reachNum != endReachNum; reachNum++ ) {
		const auto reach = aasReach[reachNum];
		if( reach.traveltype != TRAVEL_WALK ) {
			continue;
		}
		int reachAreaNum = reach.areanum;
		if( !( aasAreaSettings[reachAreaNum].areaflags & AREA_INCLINED_FLOOR ) ) {
			continue;
		}

		// Set the current grounded area num as a forbidden to avoid looping
		if( const int *areaNum = TryFindBestInclinedFloorExitArea( context, reachAreaNum, currGroundedAreaNum ) ) {
			const auto &bestArea = aasAreas[*areaNum];
			Vec3 areaPoint( bestArea.center );
			areaPoint.Z() = bestArea.mins[2] + 1.0f + -playerbox_stand_mins[2];
			auto *fallback = &m_subsystem->useWalkableNodeScript;
			fallback->Activate( areaPoint.Data(), 32.0f, *areaNum );
			return fallback;
		}
	}

	return nullptr;
}