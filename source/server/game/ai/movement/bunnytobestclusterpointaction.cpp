#include "bunnytobestclusterpointaction.h"
#include "movementlocal.h"

BunnyToBestFloorClusterPointAction::BunnyToBestFloorClusterPointAction( MovementSubsystem *subsystem )
	: BunnyTestingMultipleLookDirsAction( subsystem, NAME, COLOR_RGB( 255, 0, 255 ) ) {
}

void BunnyToBestFloorClusterPointAction::onApplicationSequenceStarted( PredictionContext *context ) {
	Super::onApplicationSequenceStarted( context );

	FloorClusterAreasCache *const caches[2] = {
		&m_subsystem->m_sameFloorClusterAreasCache,
		&m_subsystem->m_nextFloorClusterAreasCache,
	};

	bool *const testedFlags[2] = { &m_hasTestedSameCluster, &m_hasTestedNextCluster };

	for( int i = 0; i < 2; ++i ) {
		if( *testedFlags[i] ) {
			continue;
		}
		*testedFlags[i] = true;

		int areaNum;
		if( !caches[i]->GetClosestToTargetPoint( context, m_localDirStorage.data(), &areaNum ) ) {
			continue;
		}

		m_localDirStorage -= context->movementState->entityPhysicsState.Origin();
		if( m_localDirStorage.normalize() ) {
			m_currDir = m_localDirStorage.data();
			return;
		}
	}

    m_currDir = nullptr;
}

void BunnyToBestFloorClusterPointAction::onApplicationSequenceFailed( PredictionContext *context, unsigned ) {
	if( m_hasTestedNextCluster ) {
		return;
	}

	assert( m_hasTestedSameCluster );
	// Make sure we can restart this action after rolling back
	m_disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
	// Force this action to be applied next frame (regardless of rolling back)
}