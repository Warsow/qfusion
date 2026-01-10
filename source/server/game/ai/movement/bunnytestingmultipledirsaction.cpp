#include "bunnytestingmultipledirsaction.h"
#include "movementlocal.h"
#include <common/helpers/algorithm.h>

void BunnyTestingMultipleLookDirsAction::beforePlanning() {
	BunnyHopAction::beforePlanning();

	// Ensure the suggested action has been set in subtype constructor
	m_currDir = nullptr;
}

void BunnyTestingSavedLookDirsAction::onApplicationSequenceStarted( PredictionContext *context ) {
	BunnyTestingMultipleLookDirsAction::onApplicationSequenceStarted( context );
	if( !m_currSuggestedLookDirNum ) {
		m_suggestedLookDirs.clear();
		saveSuggestedLookDirs( context );
		// TODO: Could be better if this gets implemented individually by each descendant.
		// The generic version is used now just to provide a generic solution quickly at cost of being suboptimal.
		deriveMoreDirsFromSavedDirs();
	}
	if( m_currSuggestedLookDirNum >= m_suggestedLookDirs.size() ) {
		return;
	}

	const SuggestedDir &suggestedDir = m_suggestedLookDirs[m_currSuggestedLookDirNum];
	m_currDir = suggestedDir.dir.data();
	if( unsigned penalty = suggestedDir.pathPenalty ) {
		ensurePathPenalty( penalty );
	}
}

void BunnyTestingSavedLookDirsAction::onApplicationSequenceFailed( PredictionContext *context, unsigned ) {
	// If another suggested look dir does not exist
	if( m_currSuggestedLookDirNum + 1 >= m_suggestedLookDirs.size() ) {
		return;
	}

	m_currSuggestedLookDirNum++;
	// Allow the action application after the context rollback to savepoint
	m_disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
	// Ensure this action will be used after rollback
}

void BunnyTestingMultipleLookDirsAction::onApplicationSequenceStopped( PredictionContext *context,
																	   SequenceStopReason stopReason,
																	   unsigned stoppedAtFrameIndex ) {
	BunnyHopAction::onApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );

	if( stopReason == FAILED ) {
		onApplicationSequenceFailed( context, stoppedAtFrameIndex );
	}
}

inline float SuggestObstacleAvoidanceCorrectionFraction( const PredictionContext *context ) {
	// Might be negative!
	float speedOverRunSpeed = context->movementState->entityPhysicsState.Speed() - context->GetRunSpeed();
	if( speedOverRunSpeed > 500.0f ) {
		return 0.15f;
	}
	return 0.35f - 0.20f * speedOverRunSpeed / 500.0f;
}

auto BunnyTestingMultipleLookDirsAction::planPredictionStep( PredictionContext *context ) -> PredictionResult {
	if( const auto result = genericCheckIsActionEnabled( context ); result != PredictionResult::Continue ) {
		return result;
	}

	if( !m_currDir ) {
		Debug( "There is no suggested look dirs yet/left\n" );
		return PredictionResult::Abort;
	}

	// Do this test after GenericCheckIsActionEnabled(), otherwise disabledForApplicationFrameIndex does not get tested
	if( const auto result = checkCommonBunnyHopPreconditions( context ); result != PredictionResult::Continue ) {
		return result;
	}

	context->record->botInput.SetIntendedLookDir( m_currDir, true );

	if( !setupBunnyHopping( context->record->botInput.IntendedLookDir(), context ) ) {
		return PredictionResult::Restart;
	}

	return PredictionResult::Continue;
}

class DirRotatorsCache {
	enum { kMaxRotations = 36 };

public:
	struct Rotator {
		mat3_t matrix;
		unsigned pathPenalty;

		Vec3 rotate( const Vec3 &__restrict v ) const {
			vec3_t result;
			assert( std::fabs( v.length() - 1.0f ) < 0.001f );
			Matrix3_TransformVector( matrix, v.data(), result );
			return Vec3( result );
		}
	};
private:
	Rotator values[kMaxRotations];
public:
	DirRotatorsCache() noexcept {
		// We can't (?) use axis_identity due to initialization order issues (?), can we?
		constexpr const mat3_t identity = {
			1, 0, 0,
			0, 1, 0,
			0, 0, 1
		};

		int index = 0;
		// The step is not monotonic and is not uniform (at least for the first row) intentionally
		constexpr const float angles[kMaxRotations / 2] = {
			4.0f, 8.0f, 12.0f, 20.0f, 16.0f, 28.0f, 24.0f, 32.0f, 36.0f,
			40.0f, 45.0f, 55.0f, 65.0f, 75.0f, 85.0f, 95.0f, 105.0f, 120.0f
		};
		static_assert( wsw::max_element( std::begin( angles ), std::end( angles ) ) == std::end( angles ) - 1 );
		constexpr const float maxAngle = std::end( angles )[-1];
		constexpr const float minPenaltyAngle = 30.0f;
		for( float angle : angles ) {
			unsigned penalty = 0;
			if( angle > minPenaltyAngle ) {
				assert( angle <= maxAngle );
				const float frac = ( angle - minPenaltyAngle ) / ( maxAngle - minPenaltyAngle );
				// Make the penalty grow as x^2 in [0, 1] range
				penalty = (unsigned)( 300 * frac * frac );
			}
			// TODO: Just negate some elements? Does not really matter for a static initializer
			for( int sign = -1; sign <= 1; sign += 2 ) {
				auto &r = values[index++];
				Matrix3_Rotate( identity, (float)sign * angle, 0, 0, 1, r.matrix );
				r.pathPenalty = penalty;
			}
		}
	}

	class const_iterator {
		friend class DirRotatorsCache;
		const Rotator *p;
		explicit const_iterator( const Rotator *p_ ) : p( p_ ) {}
	public:
		const_iterator& operator++() {
		    p++;
			return *this;
		}
		bool operator!=( const const_iterator &that ) const {
			return p != that.p;
		}
		Rotator operator*() const {
			return *p;
		}
	};

	[[nodiscard]]
	const_iterator begin() const { return const_iterator( values ); }
	[[nodiscard]]
	const_iterator end() const { return const_iterator( values + kMaxRotations ); }
};

static DirRotatorsCache dirRotatorsCache;

static inline bool areDirsSimilar( const Vec3 &__restrict a, const Vec3 &__restrict b ) {
	assert( std::fabs( a.squareLength() - 1.0f ) < 0.1f );
	assert( std::fabs( b.squareLength() - 1.0f ) < 0.1f );
	return a.dot( b ) > 0.998f;
}

void BunnyTestingSavedLookDirsAction::deriveMoreDirsFromSavedDirs() {
	// TODO: See notes in the method javadoc about this very basic approach

	if( m_suggestedLookDirs.empty() ) {
		return;
	}

	// First prune similar suggested areas.
	// (a caller code that supplies suggested areas may test similarity
	// for its own optimization purposes but it is not mandatory).
	for( size_t baseDirIndex = 0; baseDirIndex < m_suggestedLookDirs.size() - 1u; ++baseDirIndex ) {
		const Vec3 &__restrict baseTestedDir = m_suggestedLookDirs[baseDirIndex].dir;
		size_t nextDirIndex = baseDirIndex + 1;
		while( nextDirIndex < m_suggestedLookDirs.size() ) {
			const Vec3 &__restrict nextTestedDir = m_suggestedLookDirs[nextDirIndex].dir;
			if( areDirsSimilar( baseTestedDir, nextTestedDir ) ) {
				// This base dir was OK, move to the next one
				nextDirIndex++;
			} else {
				// Prune the similar next dir. Replace by the last dir, then shrink the container.
				m_suggestedLookDirs[nextDirIndex] = m_suggestedLookDirs.back();
				m_suggestedLookDirs.pop_back();
			}
		}
	}

	// Ensure we can assume at least one free array cell in the loop below.
	if( m_suggestedLookDirs.full() ) {
		return;
	}

	// Actually, derive more dirs from saved dirs

	assert( !m_suggestedLookDirs.empty() );
	// Save this fixed value (as the dirs array is going to grow)
	const size_t lastBaseDirIndex = m_suggestedLookDirs.size() - 1u;
	// For every base dir from kept given ones
	for( size_t baseDirIndex = 0; baseDirIndex <= lastBaseDirIndex; ++baseDirIndex ) {
		const auto &__restrict base = m_suggestedLookDirs[baseDirIndex];
		// Produce a rotated dir for every possible rotation
		for( const auto &rotator: dirRotatorsCache ) {
			const Vec3 rotated( rotator.rotate( base.dir ) );
			// Check whether there is a similar dir
			bool hasASimilarDir = false;
			for( const auto &__restrict existing: m_suggestedLookDirs ) {
				if( areDirsSimilar( rotated, existing.dir ) ) {
					hasASimilarDir = true;
					break;
				}
			}
			if( !hasASimilarDir ) {
				// Save the rotated dir as a suggested one
				m_suggestedLookDirs.emplace_back( SuggestedDir( rotated, base.area, rotator.pathPenalty ) );
				if( m_suggestedLookDirs.full() ) {
					return;
				}
			}
		}
	}
}

auto BunnyTestingSavedLookDirsAction::takeBestCandidateAreas( AreaAndScore *inputBegin,
															  AreaAndScore *inputEnd,
															  unsigned maxAreas ) -> AreaAndScore * {
	assert( inputEnd >= inputBegin );
	const uintptr_t numAreas = inputEnd - inputBegin;
	const uintptr_t numResultAreas = numAreas < maxAreas ? numAreas : maxAreas;

	// Move best area to the array head, repeat it for the array tail
	for( uintptr_t i = 0, end = numResultAreas; i < end; ++i ) {
		// Set the start area as a current best one
		auto &startArea = *( inputBegin + i );
		for( uintptr_t j = i + 1; j < numAreas; ++j ) {
			auto &currArea = *( inputBegin + j );
			if( currArea.score > startArea.score ) {
				std::swap( currArea, startArea );
			}
		}
	}

	return inputBegin + numResultAreas;
}

void BunnyTestingSavedLookDirsAction::saveCandidateAreaDirs( PredictionContext *context,
															 AreaAndScore *candidateAreasBegin,
															 AreaAndScore *candidateAreasEnd ) {
	const auto &entityPhysicsState = context->movementState->entityPhysicsState;
	const int navTargetAreaNum = context->NavTargetAasAreaNum();
	const auto aasAreas = AiAasWorld::instance()->getAreas();

	AreaAndScore *takenAreasBegin = candidateAreasBegin;
	assert( m_maxSuggestedLookDirs <= m_suggestedLookDirs.capacity() );
	const unsigned maxAreas = m_maxSuggestedLookDirs;
	AreaAndScore *takenAreasEnd = takeBestCandidateAreas( candidateAreasBegin, candidateAreasEnd, maxAreas );

	m_suggestedLookDirs.clear();
	for( auto iter = takenAreasBegin; iter < takenAreasEnd; ++iter ) {
		const int areaNum = ( *iter ).areaNum;
		Vec3 target( 0, 0, 0 );
		if( areaNum != navTargetAreaNum ) {
			const auto &area = aasAreas[areaNum];
			target.set( area.center );
			target.z() = area.mins[2] + 32.0f;
		} else {
			context->NavTargetOrigin().copyTo( target );
		}
		if( target.squareDistance2DTo( entityPhysicsState.Origin() ) > wsw::square( 24.0f ) ) {
			Vec3 dir( target - entityPhysicsState.Origin() );
			dir *= Q_RSqrt( dir.squareLength() );
			m_suggestedLookDirs.emplace_back( SuggestedDir { dir, areaNum } );
		}
	}
}