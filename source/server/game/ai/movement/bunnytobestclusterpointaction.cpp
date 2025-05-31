#include "bunnytobestclusterpointaction.h"
#include "movementlocal.h"

BunnyToBestFloorClusterPointAction::BunnyToBestFloorClusterPointAction( MovementSubsystem *subsystem )
	: BunnyTestingMultipleLookDirsAction( subsystem, NAME, COLOR_RGB( 255, 0, 255 ) ) {
	suggestedAction = &m_subsystem->bunnyTestingNextReachDirsAction;
}

void BunnyToBestFloorClusterPointAction::OnApplicationSequenceStarted( PredictionContext *context ) {
	Super::OnApplicationSequenceStarted( context );

	FloorClusterAreasCache *const caches[2] = {
		&m_subsystem->predictionContext.sameFloorClusterAreasCache,
		&m_subsystem->predictionContext.nextFloorClusterAreasCache
	};

	bool *const testedFlags[2] = { &this->hasTestedSameCluster, &this->hasTestedNextCluster };

	for( int i = 0; i < 2; ++i ) {
		if( *testedFlags[i] ) {
			continue;
		}
		*testedFlags[i] = true;

		int areaNum;
		if( !caches[i]->GetClosestToTargetPoint( context, localDirStorage.Data(), &areaNum ) ) {
			continue;
		}

		localDirStorage -= context->movementState->entityPhysicsState.Origin();
		if( localDirStorage.normalize() ) {
			currDir = localDirStorage.Data();
			return;
		}
	}

    currDir = nullptr;
}

void BunnyToBestFloorClusterPointAction::OnApplicationSequenceFailed( PredictionContext *context, unsigned ) {
	if( hasTestedNextCluster ) {
		return;
	}

	assert( hasTestedSameCluster );
	// Make sure we can restart this action after rolling back
	disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
	// Force this action to be applied next frame (regardless of rolling back)
	context->SaveSuggestedActionForNextFrame( this );
}