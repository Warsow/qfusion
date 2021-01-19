#ifndef WSW_582d80d5_6d3e_4e8f_ae18_66b47d842894_H
#define WSW_582d80d5_6d3e_4e8f_ae18_66b47d842894_H

#include "bunnyhopaction.h"

class BunnyTestingMultipleLookDirsAction : public BunnyHopAction {
	friend class BunnyStraighteningReachChainAction;
	friend class BunnyToBestShortcutAreaAction;
	friend class BunnyInterpolatingChainAtStartAction;
protected:
	BaseMovementAction *suggestedAction { nullptr };
	const float *currDir {nullptr };

	virtual void OnApplicationSequenceFailed( MovementPredictionContext *context, unsigned stoppedAtFrameIndex ) {};
public:
	BunnyTestingMultipleLookDirsAction( BaseScript2 *script, const char *name_, int debugColor_ )
		: BunnyHopAction( script, name_, debugColor_ ) {}

	void BeforePlanning() override;
	void OnApplicationSequenceStopped( MovementPredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;
	void PlanPredictionStep( MovementPredictionContext *context ) override;
};

class BunnyTestingSavedLookDirsAction : public BunnyTestingMultipleLookDirsAction {
protected:
	static constexpr auto kMaxSuggestedLookDirs = 40;

	struct SuggestedDir {
		Vec3 dir;
		int area;
		unsigned pathPenalty { 0 };

		SuggestedDir( const Vec3 &dir_, int area_ )
			: dir( dir_ ), area( area_ ) {}

		SuggestedDir( const Vec3 &dir_, int area_, unsigned pathPenalty_ )
			: dir( dir_ ), area( area_ ), pathPenalty( pathPenalty_ ) {}
	};

	wsw::StaticVector<SuggestedDir, kMaxSuggestedLookDirs> suggestedLookDirs;

	unsigned maxSuggestedLookDirs {kMaxSuggestedLookDirs };
	unsigned currSuggestedLookDirNum { 0 };

	void BeforePlanning() override {
		BunnyTestingMultipleLookDirsAction::BeforePlanning();
		currSuggestedLookDirNum = 0;
		suggestedLookDirs.clear();
	}

	void OnApplicationSequenceStarted( MovementPredictionContext *context ) final;

	void OnApplicationSequenceFailed( MovementPredictionContext *context, unsigned stoppedAtFrameIndex ) final;

	virtual void SaveSuggestedLookDirs( MovementPredictionContext *context ) = 0;

	/**
	 * Assuming that look dirs and areas have been just saved, derives additional ones
	 * that have the same base area (if any) but slightly rotated direction.
	 * This method producing more data for additional attempts significantly increases
	 * success rate of building predicted movement trajectories.
	 * @todo this works good but the used algorithm is very basic
	 * and this should really be implemented by descendants in their specific ways.
	 */
	void DeriveMoreDirsFromSavedDirs();

	/**
	 * A helper method to select best N areas that is optimized for small areas count.
	 * Modifies the collection in-place putting best areas at its beginning.
	 * Returns the new end iterator for the selected areas range.
	 * The begin iterator is assumed to remain the same.
	 */
	AreaAndScore *TakeBestCandidateAreas( AreaAndScore *inputBegin, AreaAndScore *inputEnd, unsigned maxAreas );

	void SaveCandidateAreaDirs( MovementPredictionContext *context,
								AreaAndScore *candidateAreasBegin,
								AreaAndScore *candidateAreasEnd );

	BunnyTestingSavedLookDirsAction( BaseScript2 *script, const char *name_, int debugColor_ )
		: BunnyTestingMultipleLookDirsAction( script, name_, debugColor_ ) {}
};

#endif
