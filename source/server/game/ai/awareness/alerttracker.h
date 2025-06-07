#ifndef WSW_48432071_bb5e_4cae_8342_f3d800ee9d60_H
#define WSW_48432071_bb5e_4cae_8342_f3d800ee9d60_H

#include "../ailocal.h"
#include <common/types/staticvector.h>
#include "../component.h"

/**
 * An alert spot definition that is visible for scripts
 */
struct AiAlertSpot {
	int id;
	Vec3 origin;
	float radius;
	float regularEnemyInfluenceScale;
	float carrierEnemyInfluenceScale;

	AiAlertSpot( int id_,
				 const Vec3 &origin_,
				 float radius_,
				 float regularEnemyInfluenceScale_ = 1.0f,
				 float carrierEnemyInfluenceScale_ = 1.0f )
		: id( id_ )
		, origin( origin_ )
		, radius( radius_ )
		, regularEnemyInfluenceScale( regularEnemyInfluenceScale_ )
		, carrierEnemyInfluenceScale( carrierEnemyInfluenceScale_ ) {}
};

class Bot;

/**
 * A helper class that encapsulates details of checking for alert
 * (that happens when an enemy is fairly close to a defended spot)
 * and alert reporting to the alert messages receiver.
 */
class AlertTracker {
	friend class BotAwarenessModule;
public:
	typedef void ( AiComponent::*AlertCallback )( Bot *detector, int spotId, float alertLevel );
private:
	/**
	 * An internal augmented version of AiAlertSpot
	 */
	struct AlertSpot: public AiAlertSpot {
		int64_t lastReportedAt;
		float lastReportedScore;
		AlertCallback callback;
		AiComponent *receiver;

		AlertSpot( const AiAlertSpot &spot, AlertCallback callback_, AiComponent *receiver_ )
			: AiAlertSpot( spot )
			, lastReportedAt( 0 )
			, lastReportedScore( 0.0f )
			, callback( callback_ )
			, receiver( receiver_ ) {};

		inline void Alert( Bot *bot, float score ) {
			( receiver->*callback )( bot, id, score );
			lastReportedAt = level.time;
			lastReportedScore = score;
		}
	};

	void EnableAutoAlert( const AiAlertSpot &alertSpot, AlertCallback callback, AiComponent *receiver );
	void DisableAutoAlert( int id );

	static constexpr unsigned MAX_ALERT_SPOTS = 3;
	wsw::StaticVector<AlertSpot, MAX_ALERT_SPOTS> alertSpots;

	Bot *const bot;

	explicit AlertTracker( Bot *bot_ ): bot( bot_ ) {}

	void CheckAlertSpots( const wsw::StaticVector<uint16_t, MAX_CLIENTS> &visibleTargets );
};

#endif
