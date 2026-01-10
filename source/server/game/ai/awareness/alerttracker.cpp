#include "alerttracker.h"

void AlertTracker::EnableAutoAlert( const AiAlertSpot &alertSpot,
									AlertCallback callback,
									AiComponent *receiver ) {
	constexpr const char *tag = "AlertTracker::EnableAutoAlert";
	// First check duplicate ids. Fail on error since callers of this method are internal.
	for( const auto &presentSpot: alertSpots ) {
		if( presentSpot.id == alertSpot.id ) {
			AI_FailWith( tag, "Duplicated alert spot (id=%d)\n", alertSpot.id );
		}
	}

	if( alertSpots.full() ) {
		AI_FailWith( tag, "Can't add an alert spot (id=%d)\n: too many spots", alertSpot.id );
	}

	alertSpots.emplace_back( AlertSpot( alertSpot, callback, receiver ) );
}

void AlertTracker::DisableAutoAlert( int id ) {
	for( unsigned i = 0; i < alertSpots.size(); ++i ) {
		if( alertSpots[i].id == id ) {
			alertSpots.erase( alertSpots.begin() + i );
			return;
		}
	}

	AI_FailWith( "AlertTracker::DisableAutoAlert()", "Can't find alert spot by id %d\n", id );
}

void AlertTracker::CheckAlertSpots( const wsw::StaticVector<uint16_t, MAX_CLIENTS> &visibleTargets ) {
	float scores[MAX_ALERT_SPOTS];

	edict_t *const gameEdicts = game.edicts;
	// First compute scores (good for instruction cache)
	for( unsigned i = 0; i < alertSpots.size(); ++i ) {
		float score = 0.0f;
		const auto &alertSpot = alertSpots[i];
		const float squareRadius = alertSpot.radius * alertSpot.radius;
		const float invRadius = 1.0f / alertSpot.radius;
		for( uint16_t entNum: visibleTargets ) {
			edict_t *ent = gameEdicts + entNum;
			float squareDistance = DistanceSquared( ent->s.origin, alertSpot.origin.data() );
			if( squareDistance > squareRadius ) {
				continue;
			}
			float distance = Q_Sqrt( squareDistance + 0.001f );
			score += 1.0f - distance * invRadius;
			// Put likely case first
			if( !( ent->s.effects & EF_CARRIER ) ) {
				score *= alertSpot.regularEnemyInfluenceScale;
			} else {
				score *= alertSpot.carrierEnemyInfluenceScale;
			}
		}
		// Clamp score by a max value
		clamp_high( score, 3.0f );
		// Convert score to [0, 1] range
		score /= 3.0f;
		// Get a square root of score (values closer to 0 gets scaled more than ones closer to 1)
		// Note: preserving zero value is important, otherwise an infinite alert is observed.
		if( score ) {
			score = 1.0f / sqrtf( score );
		}
		// Sanitize
		Q_clamp( score, 0.0f, 1.0f );
		scores[i] = score;
	}

	// Then call callbacks
	const int64_t levelTime = level.time;
	for( unsigned i = 0; i < alertSpots.size(); ++i ) {
		auto &alertSpot = alertSpots[i];
		const auto nonReportedFor = (uint64_t)( levelTime - alertSpot.lastReportedAt );
		if( nonReportedFor >= 1000 ) {
			alertSpot.lastReportedScore = 0.0f;
		}

		// Since scores are sanitized, they are in range [0.0f, 1.0f], and abs(scoreDelta) is in range [-1.0f, 1.0f];
		float scoreDelta = scores[i] - alertSpot.lastReportedScore;
		if( scoreDelta >= 0 ) {
			if( nonReportedFor >= 1000 - scoreDelta * 500 ) {
				alertSpot.Alert( bot, scores[i] );
			}
		} else {
			if( nonReportedFor >= 500 - scoreDelta * 500 ) {
				alertSpot.Alert( bot, scores[i] );
			}
		}
	}
}