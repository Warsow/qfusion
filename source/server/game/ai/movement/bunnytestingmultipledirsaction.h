#ifndef WSW_582d80d5_6d3e_4e8f_ae18_66b47d842894_H
#define WSW_582d80d5_6d3e_4e8f_ae18_66b47d842894_H

#include "bunnyhopaction.h"

class BunnyTestingMultipleLookDirsAction : public BunnyHopAction {
protected:
	const float *m_currDir {nullptr };

	virtual void onApplicationSequenceFailed( PredictionContext *context, unsigned stoppedAtFrameIndex ) {};
public:
	BunnyTestingMultipleLookDirsAction( MovementSubsystem *subsystem, const char *name, int debugColor )
		: BunnyHopAction( subsystem, name, debugColor ) {}

	void beforePlanning() override;
	void onApplicationSequenceStopped( PredictionContext *context,
									   SequenceStopReason stopReason,
									   unsigned stoppedAtFrameIndex ) override;
	[[nodiscard]]
	auto planPredictionStep( PredictionContext *context ) -> PredictionResult override;
};

class BunnyTestingSavedLookDirsAction : public BunnyTestingMultipleLookDirsAction {
protected:
	static constexpr auto kMaxSuggestedLookDirs = 48;

	struct SuggestedDir {
		Vec3 dir;
		int area;
		unsigned pathPenalty { 0 };

		SuggestedDir( const Vec3 &dir_, int area_ )
			: dir( dir_ ), area( area_ ) {}

		SuggestedDir( const Vec3 &dir_, int area_, unsigned pathPenalty_ )
			: dir( dir_ ), area( area_ ), pathPenalty( pathPenalty_ ) {}
	};

	wsw::StaticVector<SuggestedDir, kMaxSuggestedLookDirs> m_suggestedLookDirs;

	unsigned m_maxSuggestedLookDirs { kMaxSuggestedLookDirs };
	unsigned m_currSuggestedLookDirNum { 0 };

	void beforePlanning() override {
		BunnyTestingMultipleLookDirsAction::beforePlanning();
		m_currSuggestedLookDirNum = 0;
		m_suggestedLookDirs.clear();
	}

	void onApplicationSequenceStarted( PredictionContext *context ) final;

	void onApplicationSequenceFailed( PredictionContext *context, unsigned stoppedAtFrameIndex ) final;

	virtual void saveSuggestedLookDirs( PredictionContext *context ) = 0;

	/**
	 * Assuming that look dirs and areas have been just saved, derives additional ones
	 * that have the same base area (if any) but slightly rotated direction.
	 * This method producing more data for additional attempts significantly increases
	 * success rate of building predicted movement trajectories.
	 * @todo this works good but the used algorithm is very basic
	 * and this should really be implemented by descendants in their specific ways.
	 */
	void deriveMoreDirsFromSavedDirs();

	/**
	 * A helper method to select best N areas that is optimized for small areas count.
	 * Modifies the collection in-place putting best areas at its beginning.
	 * Returns the new end iterator for the selected areas range.
	 * The begin iterator is assumed to remain the same.
	 */
	[[nodiscard]]
	auto takeBestCandidateAreas( AreaAndScore *inputBegin, AreaAndScore *inputEnd, unsigned maxAreas ) -> AreaAndScore *;

	void saveCandidateAreaDirs( PredictionContext *context,
								AreaAndScore *candidateAreasBegin,
								AreaAndScore *candidateAreasEnd );

	BunnyTestingSavedLookDirsAction( MovementSubsystem *subsystem, const char *name_, int debugColor_ )
		: BunnyTestingMultipleLookDirsAction( subsystem, name_, debugColor_ ) {}
};

#endif
