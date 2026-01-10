#include "bunnytestingnextreachdirsaction.h"
#include "movementlocal.h"
#include "floorclusterareascache.h"
#include "../navigation/aasstaticroutetable.h"
#include "../manager.h"

BunnyTestingNextReachDirsAction::BunnyTestingNextReachDirsAction( MovementSubsystem *subsystem )
	: BunnyTestingSavedLookDirsAction( subsystem, NAME, COLOR_RGB( 0, 192, 0 ) ) {
	m_maxSuggestedLookDirs = kMaxSuggestedLookDirs;
}

void BunnyTestingNextReachDirsAction::beforePlanning() {
	BunnyTestingSavedLookDirsAction::beforePlanning();
	// We plan to allow varying bot skill dynamically.
	// This value should be recomputed every planning frame.

	const float skill = m_bot->Skill();
	// Bunny-hopping is enabled for easy bots under certain conditions. Allow only up to 2 dirs in this case.
	if( skill <= 0.33f ) {
		m_maxSuggestedLookDirs = 2;
		return;
	}

	m_maxSuggestedLookDirs = kMaxSuggestedLookDirs;
	// Use the maximum possible number of suggested dirs for hard bots.
	if( skill >= 0.66f ) {
		return;
	}

	// TODO: All these decisions should not be made at movement m_subsystem level

	// Check whether the bot is carrier. Use the maximal possible number of look dirs in this case.
	const edict_t *self = game.edicts + m_bot->EntNum();
	if( ( ( self->s.effects & EF_CARRIER ) || self->s.modelindex2 ) || m_bot->ShouldRushHeadless() ) {
		return;
	}

	const auto *inventory = self->r.client->ps.inventory;
	// Check whether the bot has a powerup. Ensure a best behaviour in this case.
	if( ( inventory[POWERUP_QUAD] | inventory[POWERUP_SHELL] | inventory[POWERUP_REGEN] ) ) {
		return;
	}

	// Grow quadratic starting from a weakest mid-skill bot
	float skillFrac = ( skill - 0.33f ) / ( 0.66f - 0.33f );
	Assert( skillFrac > 0.0f && skillFrac < 1.0f );
	m_maxSuggestedLookDirs = (unsigned)( 2 + ( skillFrac * skillFrac ) * kMaxSuggestedLookDirs );
	m_maxSuggestedLookDirs = wsw::min( m_maxSuggestedLookDirs, (unsigned)kMaxSuggestedLookDirs );
}

class NextReachDirsCollector final : public ReachChainWalker {
	friend class BunnyTestingNextReachDirsAction;

	PredictionContext *const context;
	const AiAasWorld *const aasWorld;
	const std::span<const aas_area_t> aasAreas;
	const std::span<const aas_areasettings_t> aasAreaSettings;
	const std::span<const uint16_t> aasFloorClusterNums;
	const std::span<const uint16_t> aasStairsClusterNums;
	const Hazard *hazardToEvade;
	AreaAndScore *const candidatesBegin;
	Vec3 traceStartPoint;
	int metStairsClusterNum { 0 };
	int stairsExitAreaNum { 0 };
	int groundedStartAreaNum { 0 };
	int startFloorClusterNum { 0 };
	unsigned numCandidates { 0 };
	const unsigned maxCandidates;

	inline bool CheckForStairsCluster( int areaNum );
public:
	NextReachDirsCollector( const Bot *bot_, PredictionContext *context_, AreaAndScore *candidates_, unsigned maxCandidates_ )
		: ReachChainWalker( context_->RouteCache(), context_->TravelFlags() )
		, context( context_ )
		, aasWorld( AiAasWorld::instance() )
		, aasAreas( aasWorld->getAreas() )
		, aasAreaSettings( aasWorld->getAreaSettings() )
		, aasFloorClusterNums( aasWorld->areaFloorClusterNums() )
		, aasStairsClusterNums( aasWorld->areaStairsClusterNums() )
		, candidatesBegin( candidates_ )
		, traceStartPoint( context->movementState->entityPhysicsState.Origin() )
		, maxCandidates( maxCandidates_ ) {
		const auto &physicsState = context->movementState->entityPhysicsState;
		SetAreaNums( physicsState, context->NavTargetAasAreaNum() );
		traceStartPoint.z() += playerbox_stand_viewheight;

		hazardToEvade = bot_->PrimaryHazard();
		if( bot_->ShouldRushHeadless() || ( hazardToEvade && !hazardToEvade->SupportsImpactTests() ) ) {
			hazardToEvade = nullptr;
		}

		if( ( groundedStartAreaNum = context->CurrGroundedAasAreaNum() ) ) {
			startFloorClusterNum = aasWorld->floorClusterNum( groundedStartAreaNum );
			// Non-grounded areas cannot belong to a stairs cluster
			CheckForStairsCluster( groundedStartAreaNum );
		}
	}

	bool Accept( int, const aas_reachability_t &reach, int ) override;
};

void BunnyTestingNextReachDirsAction::saveSuggestedLookDirs( PredictionContext *context ) {
	Assert( m_suggestedLookDirs.empty() );

	if( context->IsInNavTargetArea() ) {
		return;
	}

	AreaAndScore candidates[kMaxSuggestedLookDirs];
	NextReachDirsCollector collector( m_bot, context, candidates, kMaxSuggestedLookDirs );
	if( !collector.Exec() ) {
		Debug( "Can't find areas for straightening a look dir\n" );
	}

	saveCandidateAreaDirs( context, candidates, candidates + collector.numCandidates );
	Assert( m_suggestedLookDirs.size() <= m_maxSuggestedLookDirs );

	if( !collector.lastReachNum ) {
		return;
	}

	const auto &stoppedAtReach = AiAasWorld::instance()->getReaches()[collector.lastReachNum];
	const auto travelType = stoppedAtReach.traveltype & TRAVELTYPE_MASK;
	if( travelType != TRAVEL_TELEPORT && travelType != TRAVEL_JUMPPAD && travelType != TRAVEL_ELEVATOR ) {
		return;
	}

	// TODO: Check a trace/an coarse arc trace to the trigger and ensure we do not hit anything besides maybe the trigger

	// If there is a trigger entity in the reach chain, try keep looking at it
	Assert( m_maxSuggestedLookDirs > 0 );
	// Evict the last dir, the trigger should have a priority over it
	if( m_suggestedLookDirs.size() == m_maxSuggestedLookDirs ) {
		m_suggestedLookDirs.pop_back();
	}

	Vec3 toTriggerDir( stoppedAtReach.start );
	toTriggerDir -= context->movementState->entityPhysicsState.Origin();
	if( !toTriggerDir.normalizeFast() ) {
		return;
	}

	// The target area of reachStoppedAt is the area "behind" trigger.
	// The prediction gets always interrupted on touching trigger.
	// Just supply a dummy value and rely on touching the trigger during prediction.
	m_suggestedLookDirs.emplace_back( SuggestedDir( toTriggerDir, 0 ) );
}

inline bool NextReachDirsCollector::CheckForStairsCluster( int areaNum ) {
	// Skip if a stairs cluster has been already met
	// (limiting to a single handled stairs cluster is satisfactory)
	if( metStairsClusterNum ) {
		return true;
	}

	const int stairsClusterNum = aasStairsClusterNums[areaNum];
	if( !stairsClusterNum ) {
		return true;
	}

	if( stairsExitAreaNum == areaNum ) {
		return true;
	}

	if( const auto *exitAreaNum = TryFindBestStairsExitArea( context, stairsClusterNum ) ) {
		stairsExitAreaNum = *exitAreaNum;
		return true;
	}

	// Exclude all other areas for this
	return false;
}

bool NextReachDirsCollector::Accept( int, const aas_reachability_t &reach, int ) {
	const auto travelType = ( reach.traveltype & TRAVELTYPE_MASK );
	const int areaNum = reach.areanum;
	const auto &__restrict areaSettings = aasAreaSettings[areaNum];

	if( travelType != TRAVEL_WALK ) {
		if( travelType == TRAVEL_WALKOFFLEDGE ) {
			if( !( areaSettings.areaflags & AREA_NOFALL ) ) {
				return false;
			}
		} else if( travelType == TRAVEL_BARRIERJUMP ) {
			if( reach.end[2] - reach.start[2] > 40.0f ) {
				return false;
			}
		} else {
			return false;
		}
	}

	if( !CheckForStairsCluster( areaNum ) ) {
		// Skip the area and continue walking
		return true;
	}

	// Skip the area and continue walking if these conditions are unmet
	if( areaSettings.contents & AREACONTENTS_DONOTENTER ) {
		return true;
	}
	if( !( areaSettings.areaflags & AREA_GROUNDED ) || ( areaSettings.areaflags & AREA_DISABLED ) ) {
		return true;
	}

	const auto &__restrict area = aasAreas[areaNum];
	Vec3 areaPoint( area.center[0], area.center[1], area.mins[2] - playerbox_stand_mins[2] );

	const float squareDistanceToArea = areaPoint.squareDistanceTo( traceStartPoint );
	// Skip way too close areas (otherwise the bot might fall into endless looping)
	if( squareDistanceToArea < wsw::square( 96 ) ) {
		return true;
	}

	// Skip way too far areas (this is mainly an optimization for the following SolidWorldTrace() call)
	if( squareDistanceToArea > wsw::square( 1024 ) ) {
		return true;
	}

	// Skip "junk" areas if they are far.
	// Avoid wasting CPU cycles for these areas.
	// They might still be useful being close to a bot, e.g. if a bot is trapped at unrecognized stairs cluster.
	if( squareDistanceToArea > wsw::square( 256 + 64 ) && ( areaSettings.areaflags & AREA_JUNK ) ) {
		return true;
	}

	if( hazardToEvade && hazardToEvade->HasImpactOnPoint( areaPoint ) ) {
		return true;
	}

	bool dropDirPriority = false;

	// Make sure the bot can see the ground
	// On failure, restore minScore (it might have been set to the value of the rejected area score on this loop step)
	if( startFloorClusterNum && startFloorClusterNum == aasFloorClusterNums[areaNum] ) {
		if( !aasWorld->isAreaWalkableInFloorCluster( groundedStartAreaNum, areaNum ) ) {
			return true;
		}
	} else {
		// TraceArcInSolidWorld() is very coarse and should not be 100% trusted.
		// Take the dir but make sure it gets tested last.
		if( !TraceArcInSolidWorld( traceStartPoint.data(), areaPoint.data() ) ) {
			dropDirPriority = true;
		}

		// This is very likely to indicate a significant elevation of the area over the bot area.
		// TODO: This test leads to a failure if the target area is direct-reachable via falling
		if( !AasStaticRouteTable::instance()->getTravelTimeWalkingOrFallingShort( areaNum, groundedStartAreaNum ) ) {
			return true;
		}
	}

	if( numCandidates == maxCandidates ) {
		// Interrupt walking at this
		return false;
	}

	assert( numCandidates < maxCandidates );

	float score;
	if( dropDirPriority ) {
		// Closer areas get a greater (less-modulo negative) score in the same priority class
		// but these areas should get tested after all areas with a positive score.
		score = -( (float)( numCandidates + 1 ) );
	} else {
		// Give closer areas a priority
		score = (float)( maxCandidates + 1 - numCandidates );
	}

	new( candidatesBegin + numCandidates )AreaAndScore( areaNum, score );
	numCandidates++;
	return true;
}