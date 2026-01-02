#include "bunnyfollowingreachchainaction.h"
#include "movementlocal.h"

enum class TravelTypeClass { Compatible, Trigger, Incompatible };

[[nodiscard]]
static auto classifyTravelType( const aas_reachability_t &reach ) -> TravelTypeClass {
	const auto travelType = reach.traveltype & TRAVELTYPE_MASK;
	if( travelType == TRAVEL_WALK ) {
		return TravelTypeClass::Compatible;
	} else if( travelType == TRAVEL_TELEPORT || travelType == TRAVEL_JUMPPAD || travelType == TRAVEL_ELEVATOR ) {
		return TravelTypeClass::Trigger;
	} else if( travelType == TRAVEL_WALKOFFLEDGE ) {
		// WTF?
		if( reach.start[2] > reach.end[2] ) {
			if( reach.start[2] - reach.end[2] < 40.0f ) {
				return TravelTypeClass::Compatible;
			}
		}
	} else if( travelType == TRAVEL_BARRIERJUMP ) {
		// WTF?
		if( reach.end[2] > reach.start[2] ) {
			if( reach.end[2] - reach.start[2] < 32.0f ) {
				return TravelTypeClass::Compatible;
			}
		}
	}
	return TravelTypeClass::Incompatible;
}

auto BunnyFollowingReachChainAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( const auto result = genericCheckIsActionEnabled( context ); result != PredictionResult::Continue ) {
		return result;
	}

	if( const auto result = checkCommonBunnyHopPreconditions( context ); result != PredictionResult::Continue ) {
		return result;
	}

	const auto &__restrict entityPhysicsState = context->movementState->entityPhysicsState;
	const int currentAreaNum = entityPhysicsState.CurrAasAreaNum();
	const int droppedAreaNum = entityPhysicsState.DroppedToFloorAasAreaNum();
	if( currentAreaNum != m_cachedCurrentAreaNum || droppedAreaNum != m_cachedDroppedAreaNum ) {
		m_cachedNextReachNum = 0;
		m_cachedReachNum = context->NextReachNum();
		m_cachedReachPointsToTrigger = false;
		if( m_cachedReachNum ) {
			const auto aasReaches = AiAasWorld::instance()->getReaches();
			const auto &reach = aasReaches[m_cachedReachNum];
			const TravelTypeClass travelTypeClass = classifyTravelType( reach );
			if( travelTypeClass == TravelTypeClass::Incompatible ) {
				return PredictionResult::Abort;
			}
			if( travelTypeClass == TravelTypeClass::Trigger ) {
				m_cachedReachPointsToTrigger = true;
			}
			const int targetAreaNum = context->NavTargetAasAreaNum();
			if( const int nextAreaNum = reach.areanum; nextAreaNum != targetAreaNum ) {
				if( !m_bot->RouteCache()->FindRoute( nextAreaNum, targetAreaNum, m_bot->TravelFlags(), &m_cachedNextReachNum ) ) {
					return PredictionResult::Abort;
				}
				if( classifyTravelType( aasReaches[m_cachedNextReachNum] ) == TravelTypeClass::Incompatible ) {
					return PredictionResult::Abort;
				}
			}
		}
		m_cachedCurrentAreaNum = currentAreaNum;
		m_cachedDroppedAreaNum = droppedAreaNum;
	}

	int chosenReachNum = -1;
	const auto aasReaches = AiAasWorld::instance()->getReaches();
	if( m_cachedReachNum ) {
		const auto &reach = aasReaches[m_cachedReachNum];
		if( Distance2DSquared( reach.start, entityPhysicsState.Origin() ) > wsw::square( 32.0f ) ) {
			chosenReachNum = m_cachedReachNum;
		} else if( m_cachedReachPointsToTrigger ) {
			chosenReachNum = m_cachedReachNum;
			// Keep looking at the trigger but make sure we can normalize
			if( Distance2DSquared( reach.start, entityPhysicsState.Origin() ) > wsw::square( 1.0f ) ) {
				chosenReachNum = m_cachedReachNum;
			} else {
				return PredictionResult::Abort;
			}
		} else {
			if( m_cachedNextReachNum ) {
				const auto &nextReach = aasReaches[m_cachedNextReachNum];
				if( Distance2DSquared( nextReach.start, entityPhysicsState.Origin() ) > wsw::square( 32.0f ) ) {
					chosenReachNum = m_cachedNextReachNum;
				} else {
					return PredictionResult::Abort;
				}
			}
		}
	}

	assert( chosenReachNum >= -1 );

	Vec3 lookVec( 0, 0, 0 );
	if( chosenReachNum > 0 ) {
		lookVec.Set( aasReaches[chosenReachNum].start );
		lookVec.Z() += 32.0f;
		lookVec -= entityPhysicsState.Origin();
		lookVec.Z() *= Z_NO_BEND_SCALE;
	} else {
		const Vec3 navTargetOrigin( context->NavTargetOrigin() );
		if( navTargetOrigin.SquareDistance2DTo( entityPhysicsState.Origin() ) > wsw::square( 8.0f ) ) {
			navTargetOrigin.CopyTo( lookVec );
			lookVec -= entityPhysicsState.Origin();
		} else {
			return PredictionResult::Abort;
		}
	}

	if( !lookVec.normalizeFast( { .minAcceptableLength = 1.0f } ) ) {
		return PredictionResult::Abort;
	}
	if( !setupBunnyHopping( lookVec, context ) ) {
		return PredictionResult::Abort;
	}

	return PredictionResult::Continue;
}