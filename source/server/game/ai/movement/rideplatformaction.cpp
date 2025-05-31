#include "rideplatformaction.h"
#include "movementlocal.h"
#include "../ailocal.h"
#include <common/wswalgorithm.h>

void RidePlatformAction::BeforePlanning() {
	BaseAction::BeforePlanning();
	m_currTestedAreaIndex = 0;
	m_foundPlatform       = nullptr;
	m_stage               = StageWait;
	m_exitAreas.clear();
}

void RidePlatformAction::PlanPredictionStep( PredictionContext *context ) {
	auto *const defaultAction = context->SuggestDefaultAction();
	if( !GenericCheckIsActionEnabled( context, defaultAction ) ) {
		return;
	}

	if( !m_foundPlatform ) {
		const edict_t *platform = GetPlatform( context );
		if( !platform ) {
			context->cannotApplyAction = true;
			context->actionSuggestedByAction = defaultAction;
			Debug( "Cannot apply the action (cannot find a platform below)\n" );
			return;
		}
		if( !DetermineStageAndProperties( context, platform ) ) {
			context->cannotApplyAction = true;
			context->actionSuggestedByAction = defaultAction;
			Debug( "Cannot determine what to do (enter, ride or exit)\n" );
			return;
		}
		m_foundPlatform = platform;
	}

	if( m_stage == StageEnter ) {
		SetupEnterPlatformMovement( context );
	} else if( m_stage == StageExit ) {
		SetupExitPlatformMovement( context );
	} else {
		SetupRidePlatformMovement( context );
	}
}

void RidePlatformAction::CheckPredictionStepResults( PredictionContext *context ) {
	BaseAction::CheckPredictionStepResults( context );
	if( context->cannotApplyAction || context->isCompleted ) {
		return;
	}

	assert( m_stage == StageExit );

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	if( const edict_t *groundEntity = entityPhysicsState.GroundEntity(); groundEntity && groundEntity->use != Use_Plat ) {
		const int exitAreaNum = m_exitAreas[m_currTestedAreaIndex];
		const int currAreaNum = entityPhysicsState.CurrAasAreaNum();
		const int droppedToFloorAreaNum = entityPhysicsState.DroppedToFloorAasAreaNum();
		if( currAreaNum == exitAreaNum || droppedToFloorAreaNum == exitAreaNum ) {
			context->isCompleted = true;
			return;
		}

		const auto *const aasWorld = AiAasWorld::instance();
		if( const int exitClusterNum = aasWorld->floorClusterNum( exitAreaNum ) ) {
			for( const int areaNum: { currAreaNum, droppedToFloorAreaNum } ) {
				if( exitClusterNum == aasWorld->floorClusterNum( areaNum ) ) {
					context->isCompleted = true;
					return;
				}
			}
		}

		// TODO: We should trace areas (AasTraceAreas) each step...
		Vec3 playerBoxMins( context->movementState->entityPhysicsState.Origin() );
		Vec3 playerBoxMaxs( context->movementState->entityPhysicsState.Origin() );
		playerBoxMins += playerbox_stand_mins, playerBoxMaxs += playerbox_stand_maxs;
		const auto &exitArea = aasWorld->getAreas()[exitAreaNum];
		if( BoundsIntersect( playerBoxMins.Data(), playerBoxMaxs.Data(), exitArea.mins, exitArea.maxs ) ) {
			context->isCompleted = true;
			return;
		}
	}

	const unsigned sequenceDuration = SequenceDuration( context );
	if( sequenceDuration < 500 ) {
		context->SaveSuggestedActionForNextFrame( this );
		return;
	}

	if( originAtSequenceStart.SquareDistance2DTo( entityPhysicsState.Origin() ) < 32 * 32 ) {
		Debug( "The bot is likely stuck trying to use area #%d to exit the platform\n", m_currTestedAreaIndex );
		context->SetPendingRollback();
		return;
	}

	if( sequenceDuration > 750 ) {
		Debug( "The bot still has not reached the target exit area\n" );
		context->SetPendingRollback();
		return;
	}

	context->SaveSuggestedActionForNextFrame( this );
}

void RidePlatformAction::OnApplicationSequenceStopped( PredictionContext *context,
													   SequenceStopReason stopReason,
													   unsigned stoppedAtFrameIndex ) {
	BaseAction::OnApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );
	if( stopReason != FAILED && stopReason != DISABLED ) {
		return;
	}

	m_currTestedAreaIndex++;
}

void RidePlatformAction::SetupEnterPlatformMovement( PredictionContext *context ) {
	auto *botInput = &context->record->botInput;
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	botInput->isLookDirSet       = true;
	botInput->isUcmdSet          = true;
	botInput->canOverrideUcmd    = true;
	botInput->canOverrideLookVec = true;

	// Look at the trigger in 2D world projection
	Vec3 intendedLookDir( getTriggerOrigin( m_foundPlatform ) );
	intendedLookDir -= entityPhysicsState.Origin();
	intendedLookDir.Z() = 0;
	if( intendedLookDir.normalizeFast() ) {
		botInput->SetIntendedLookDir( intendedLookDir, true );
		botInput->SetForwardMovement( 1 );
	} else {
		// Set a random input that should not lead to blocking
		botInput->SetForwardMovement( 1 );
		botInput->SetRightMovement( 1 );
	}

	// Our aim is firing a trigger and nothing else, walk carefully
	botInput->SetWalkButton( true );

	context->isCompleted = true;
}

void RidePlatformAction::SetupRidePlatformMovement( PredictionContext *context ) {
	auto *botInput = &context->record->botInput;
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;

	// Put all this shared clutter at the beginning

	botInput->isLookDirSet       = true;
	botInput->isUcmdSet          = true;
	botInput->canOverrideUcmd    = true;
	botInput->canOverrideLookVec = true;

	Debug( "Stand idle on the platform, do not plan ahead\n" );
	context->isCompleted = true;

	if( const std::optional<SelectedEnemy> &selectedEnemy = bot->GetSelectedEnemy() ) {
		Vec3 toEnemy( selectedEnemy->LastSeenOrigin() );
		toEnemy -= context->movementState->entityPhysicsState.Origin();
		botInput->SetIntendedLookDir( toEnemy, false );
	} else if( const std::optional<Vec3> &keptInFovPoint = bot->GetKeptInFovPoint() ) {
		Vec3 toPoint( *keptInFovPoint );
		toPoint -= context->movementState->entityPhysicsState.Origin();
		botInput->SetIntendedLookDir( toPoint, false );
	}

	// Try staying in the middle of platform
	if( const auto *groundEntity = entityPhysicsState.GroundEntity(); groundEntity && groundEntity->use == Use_Plat ) {
		Vec3 toTarget( getTriggerOrigin( groundEntity ) );
		toTarget.Z() = entityPhysicsState.Origin()[2];
		toTarget -= entityPhysicsState.Origin();
		// Don't do that staying sufficiently close to the middle
		if( toTarget.normalizeFast( { .minAcceptableLength = 20.0f } ) ) {
			const Vec3 forwardDir( entityPhysicsState.ForwardDir() ), rightDir( entityPhysicsState.RightDir() );
			DirToKeyInput( toTarget, forwardDir.Data(), rightDir.Data(), botInput );
			botInput->SetWalkButton( true );
		}
	}
}

void RidePlatformAction::SetupExitPlatformMovement( PredictionContext *context ) {
	if( m_currTestedAreaIndex >= m_exitAreas.size() ) {
		Debug( "All suggested exit area tests have failed\n" );
		this->isDisabledForPlanning = true;
		context->SetPendingRollback();
		return;
	}

	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	auto *botInput = &context->record->botInput;

	botInput->isLookDirSet       = true;
	botInput->isUcmdSet          = true;
	botInput->canOverrideUcmd    = false;
	botInput->canOverrideLookVec = false;

	const auto &area = AiAasWorld::instance()->getAreas()[m_exitAreas[m_currTestedAreaIndex]];
	Vec3 intendedLookDir( area.center );
	intendedLookDir.Z() = area.mins[2] + 32;
	intendedLookDir -= entityPhysicsState.Origin();

	if( const std::optional<float> maybeDistance = intendedLookDir.normalizeFast() ) {
		botInput->SetIntendedLookDir( intendedLookDir, true );
		botInput->SetForwardMovement( 1 );
		botInput->SetWalkButton( true );
		float dot = intendedLookDir.Dot( entityPhysicsState.ForwardDir() );
		if( dot < 0.3f ) {
			botInput->SetTurnSpeedMultiplier( 3.0f );
		} else if( dot > 0.9f ) {
			botInput->SetWalkButton( false );
			if( *maybeDistance > 72.0f && dot > 0.95f ) {
				botInput->SetSpecialButton( true );
			}
		}
	} else {
		// Set a random input that should not lead to blocking
		botInput->SetForwardMovement( 1 );
		botInput->SetWalkButton( true );
	}
}

const edict_t *RidePlatformAction::GetPlatform( PredictionContext *context ) const {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const edict_t *groundEntity = entityPhysicsState.GroundEntity();
	if( groundEntity ) {
		return groundEntity->use == Use_Plat ? groundEntity : nullptr;
	}

	trace_t trace;
	Vec3 startPoint( entityPhysicsState.Origin() );
	startPoint.Z() += playerbox_stand_mins[2];
	Vec3 endPoint( entityPhysicsState.Origin() );
	endPoint.Z() += playerbox_stand_mins[2];
	endPoint.Z() -= 32.0f;
	edict_t *const ignore = game.edicts + bot->EntNum();
	G_Trace( &trace, startPoint.Data(), playerbox_stand_mins, playerbox_stand_maxs, endPoint.Data(), ignore, MASK_ALL );
	if( trace.ent != -1 ) {
		groundEntity = game.edicts + trace.ent;
		if( groundEntity->use == Use_Plat ) {
			return groundEntity;
		}
	}

	return nullptr;
}

bool RidePlatformAction::DetermineStageAndProperties( PredictionContext *context, const edict_t *platform ) {
	if( platform->moveinfo.state != STATE_TOP && platform->moveinfo.state != STATE_BOTTOM ) {
		m_stage = StageWait;
		return true;
	}

	const Vec3 addToMins( -48.0f, -48.0f, -8.0f ), addToMaxs( +48.0f, +48.0f, +24.0f );

	Vec3 startMins( platform->r.mins ), startMaxs( platform->r.maxs );
	startMins.Z() = platform->r.maxs[2] + platform->moveinfo.start_origin[2];
	startMaxs.Z() = platform->r.maxs[2] + platform->moveinfo.start_origin[2];
	startMins += addToMins, startMaxs += addToMaxs;

	Vec3 endMins( platform->r.mins ), endMaxs( platform->r.maxs );
	endMins.Z() = platform->r.maxs[2] + platform->moveinfo.end_origin[2];
	endMaxs.Z() = platform->r.maxs[2] + platform->moveinfo.end_origin[2];
	endMins += addToMins, endMaxs += addToMaxs;

	wsw::StaticVector<int, 24> startBoxAreas, endBoxAreas;
	FindSuitableAreasInBox( startMins, startMaxs, &startBoxAreas );
	FindSuitableAreasInBox( endMins, endMaxs, &endBoxAreas );

	// Prune shared areas
	wsw::StaticVector<int, 24> sharedAreas;
	for( unsigned turn = 0; turn < 2; ++turn ) {
		const wsw::StaticVector<int, 24> &sourceAreas    = turn ? startBoxAreas : endBoxAreas;
		const wsw::StaticVector<int, 24> &checkedInAreas = turn ? endBoxAreas : startBoxAreas;
		for( const int areaNum: sourceAreas ) {
			if( wsw::contains( checkedInAreas, areaNum ) && !wsw::contains( sharedAreas, areaNum ) ) {
				sharedAreas.push_back( areaNum );
			}
		}
	}

	if( !sharedAreas.empty() ) {
		for( wsw::StaticVector<int, 24> *areas: { &startBoxAreas, &endBoxAreas } ) {
			for( unsigned i = 0; i < areas->size(); ) {
				if( wsw::contains( sharedAreas, areas->operator[]( i ) ) ) {
					areas->operator[]( i ) = areas->back();
					areas->pop_back();
				} else {
					++i;
				}
			}
		}
	}

	bool succeeded = false;
	if( const int navTargetAreaNum = bot->NavTargetAasAreaNum() ) {
		wsw::StaticVector<int, 24> startReachAreas;
		if( const int bestStartTravelTime = FindNavTargetReachableAreas( startBoxAreas, navTargetAreaNum, &startReachAreas ) ) {
			wsw::StaticVector<int, 24> endReachAreas;
			if( const int bestEndTravelTime = FindNavTargetReachableAreas( endBoxAreas, navTargetAreaNum, &endReachAreas ) ) {
				// If the top travel time is smaller/better
				if( bestStartTravelTime < bestEndTravelTime ) {
					if( platform->moveinfo.state == STATE_TOP ) {
						m_stage = StageExit;
						m_exitAreas.clear();
						m_exitAreas.insert( m_exitAreas.end(), startReachAreas.begin(), startReachAreas.end() );
					} else {
						m_stage = StageEnter;
					}
					succeeded = true;
				} else if( bestStartTravelTime > bestEndTravelTime ) {
					if( platform->moveinfo.state == STATE_TOP ) {
						m_stage = StageEnter;
					} else {
						m_stage = StageExit;
						m_exitAreas.clear();
						m_exitAreas.insert( m_exitAreas.end(), endReachAreas.begin(), endReachAreas.end() );
					}
					succeeded = true;
				}
			}
		}
	}

	// TODO: Sort exit areas
	if( !succeeded ) {
		m_exitAreas.clear();
		if( platform->moveinfo.state == STATE_TOP ) {
			m_exitAreas.insert( m_exitAreas.end(), startBoxAreas.begin(), startBoxAreas.end() );
		} else {
			m_exitAreas.insert( m_exitAreas.end(), endBoxAreas.begin(), endBoxAreas.end() );
		}
		if( m_exitAreas.empty() ) {
			return false;
		}
		m_stage = StageExit;
	}

	assert( m_stage != StageExit || !m_exitAreas.empty() );
	return true;
}

void RidePlatformAction::FindSuitableAreasInBox( const Vec3 &boxMins, const Vec3 &boxMaxs,
												wsw::StaticVector<int, 24> *foundAreas ) {
	const auto *aasWorld        = AiAasWorld::instance();
	const auto *aasAreas        = aasWorld->getAreas().data();
	const auto *aasAreaSettings = aasWorld->getAreaSettings().data();

	foundAreas->clear();

	int areaNumsBuffer[48];
	for( const int areaNum: aasWorld->findAreasInBox( boxMins, boxMaxs, areaNumsBuffer, (int)std::size( areaNumsBuffer ) ) ) {
		const auto &areaSettings = aasAreaSettings[areaNum];
		if( ( areaSettings.areaflags & AREA_GROUNDED ) ) {
			if( !( areaSettings.areaflags & ( AREA_JUNK | AREA_DISABLED ) ) ) {
				if( !( areaSettings.contents & ( AREACONTENTS_MOVER | AREACONTENTS_DONOTENTER ) ) ) {
					const auto &area = aasAreas[areaNum];
					trace_t trace;
					Vec3 traceStart( 0.5f * ( Vec3( area.mins ) + Vec3( area.maxs ) ) );
					traceStart.Z() = area.maxs[2] - 1.0f;
					Vec3 traceEnd( traceStart );
					traceEnd.Z() = area.mins[2] - 48.0f;
					G_Trace( &trace, traceStart.Data(), nullptr, nullptr, traceEnd.Data(), nullptr, MASK_SOLID );
					if( trace.fraction != 1.0f ) {
						if( trace.ent == 0 || game.edicts[trace.ent].use != Use_Plat ) {
							foundAreas->push_back( areaNum );
							if( foundAreas->full() ) {
								break;
							}
						}
					}
				}
			}
		}
	}
}

int RidePlatformAction::FindNavTargetReachableAreas( std::span<const int> givenAreas,
													 int navTargetAreaNum,
													 wsw::StaticVector<int, 24> *foundAreas ) {
	foundAreas->clear();

	int bestTravelTime = 0;
	for( const int areaNum: givenAreas ) {
		if( const int travelTime = bot->RouteCache()->FindRoute( areaNum, navTargetAreaNum, bot->TravelFlags() ) ) {
			if( !bestTravelTime || travelTime < bestTravelTime ) {
				bestTravelTime = travelTime;
			}
			foundAreas->push_back( areaNum );
			if( foundAreas->full() ) [[unlikely]] {
				break;
			}
		}
	}

	return bestTravelTime;
}