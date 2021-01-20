#include "bunnytestingmultipledirsaction.h"
#include "movementlocal.h"

#include "../../../../third-party/gcem/include/gcem.hpp"

void BunnyTestingMultipleLookDirsAction::BeforePlanning() {
	BunnyHopAction::BeforePlanning();
	currDir = nullptr;
}

void BunnyTestingSavedLookDirsAction::OnApplicationSequenceStarted( MovementPredictionContext *context ) {
	BunnyTestingMultipleLookDirsAction::OnApplicationSequenceStarted( context );
	if( !currSuggestedLookDirNum ) {
		suggestedLookDirs.clear();
		SaveSuggestedLookDirs( context );
		// TODO: Could be better if this gets implemented individually by each descendant.
		// The generic version is used now just to provide a generic solution quickly at cost of being suboptimal.
		DeriveMoreDirsFromSavedDirs();
	}
	if( currSuggestedLookDirNum >= suggestedLookDirs.size() ) {
		return;
	}

	const SuggestedDir &suggestedDir = suggestedLookDirs[currSuggestedLookDirNum];
	currDir = suggestedDir.dir.Data();
	if( unsigned penalty = suggestedDir.pathPenalty ) {
		EnsurePathPenalty( penalty );
	}
}

void BunnyTestingSavedLookDirsAction::OnApplicationSequenceFailed( MovementPredictionContext *context, unsigned ) {
	// If another suggested look dir does not exist
	if( currSuggestedLookDirNum + 1 >= suggestedLookDirs.size() ) {
		return;
	}

	currSuggestedLookDirNum++;
	// Allow the action application after the context rollback to savepoint
	disabledForApplicationFrameIndex = std::numeric_limits<unsigned>::max();
	// Ensure this action will be used after rollback
	context->SaveSuggestedActionForNextFrame( this );
}

void BunnyTestingMultipleLookDirsAction::OnApplicationSequenceStopped( Context *context,
																	   SequenceStopReason stopReason,
																	   unsigned stoppedAtFrameIndex ) {
	BunnyHopAction::OnApplicationSequenceStopped( context, stopReason, stoppedAtFrameIndex );

	if( stopReason == FAILED ) {
		OnApplicationSequenceFailed( context, stoppedAtFrameIndex );
	}
}

inline float SuggestObstacleAvoidanceCorrectionFraction( const Context *context ) {
	// Might be negative!
	float speedOverRunSpeed = context->movementState->entityPhysicsState.Speed() - context->GetRunSpeed();
	if( speedOverRunSpeed > 500.0f ) {
		return 0.15f;
	}
	return 0.35f - 0.20f * speedOverRunSpeed / 500.0f;
}

void BunnyTestingMultipleLookDirsAction::PlanPredictionStep( Context *context ) {
	if( !GenericCheckIsActionEnabled( context, suggestedAction ) ) {
		return;
	}

	if( !currDir ) {
		Debug( "There is no suggested look dirs yet/left\n" );
		context->SetPendingRollback();
		return;
	}

	// Do this test after GenericCheckIsActionEnabled(), otherwise disabledForApplicationFrameIndex does not get tested
	if( !CheckCommonBunnyHopPreconditions( context ) ) {
		return;
	}

	context->record->botInput.SetIntendedLookDir(currDir, true );

	if( !SetupBunnyHopping( context->record->botInput.IntendedLookDir(), context ) ) {
		context->SetPendingRollback();
		return;
	}
}

class DirRotatorsCache {
	enum { kMaxRotations = 28 };

public:
	struct Rotator {
		mat3_t matrix;
		unsigned pathPenalty;

		Vec3 rotate( const Vec3 &__restrict v ) const {
			vec3_t result;
			assert( std::fabs( v.Length() - 1.0f ) < 0.001f );
			Matrix3_TransformVector( matrix, v.Data(), result );
			return Vec3( result );
		}
	};
private:
	Rotator values[kMaxRotations];
public:
	DirRotatorsCache() noexcept {
		// We can't (?) use axis_identity due to initialization order issues (?), can we?
		mat3_t identity = {
			1, 0, 0,
			0, 1, 0,
			0, 0, 1
		};

		int index = 0;
		// The step is not monotonic and is not uniform intentionally
		const float angles[kMaxRotations / 2] = {
			4.0f, 8.0f, 12.0f, 20.0f, 16.0f, 28.0f, 24.0f, 40.0f, 36.0f, 32.0f, 45.0f, 55.0f, 65.0f, 75.0f
		};
		for( float angle : angles ) {
		    unsigned penalty = 0;
		    if( angle > 30.0f ) {
		    	assert( angle <= 75.0f );
		    	float frac = ( angle - 30.0f ) / ( 75.0f - 30.0f );
		    	// Make the penalty grow as x^2 in [0, 1] range
		        penalty = (unsigned)( 200 * frac * frac );
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
	assert( std::fabs( a.SquaredLength() - 1.0f ) < 0.1f );
	assert( std::fabs( b.SquaredLength() - 1.0f ) < 0.1f );
	constexpr const float dotThreshold = gcem::cos( DEG2RAD( 3.0f ) );
	return a.Dot( b ) > dotThreshold;
}

void BunnyTestingSavedLookDirsAction::DeriveMoreDirsFromSavedDirs() {
	// TODO: See notes in the method javadoc about this very basic approach

	if( suggestedLookDirs.empty() ) {
		return;
	}

	// First, compute "less bending" counterparts of given dirs for further dot comparisons
	// (avoid normalizing on every iteration)
	wsw::StaticVector<Vec3, kMaxSuggestedLookDirs> cachedLessBendingDirs;
	for( const auto &__restrict suggestedDir: suggestedLookDirs ) {
		const Vec3 &__restrict dir = suggestedDir.dir;
		assert( std::fabs( dir.SquaredLength() - 1.0f ) < 0.1f );
		cachedLessBendingDirs.emplace_back( Vec3( dir ) );
		Vec3 &__restrict lessBendingDir = cachedLessBendingDirs.back();
		lessBendingDir.Z() *= Z_NO_BEND_SCALE;
		lessBendingDir *= Q_RSqrt( lessBendingDir.SquaredLength() );
		assert( std::fabs( lessBendingDir.SquaredLength() - 1.0f ) < 0.1f );
	}

	// First prune similar suggested areas.
	// (a code that fills suggested areas may test similarity
	// for its own optimization purposes but it is not mandatory).
	for( size_t baseDirIndex = 0; baseDirIndex < suggestedLookDirs.size() - 1u; ++baseDirIndex ) {
		const Vec3 &__restrict baseTestedDir = cachedLessBendingDirs[baseDirIndex];
		for( size_t nextDirIndex = baseDirIndex + 1; nextDirIndex < suggestedLookDirs.size(); ) {
			const Vec3 &__restrict nextTestedDir = cachedLessBendingDirs[nextDirIndex];
			if( !areDirsSimilar( baseTestedDir, nextTestedDir ) ) {
				nextDirIndex++;
				continue;
			}
			// Prune the similar dir and its respective "less-bending" counterpart
			suggestedLookDirs[nextDirIndex] = suggestedLookDirs.back();
			suggestedLookDirs.pop_back();
			cachedLessBendingDirs[nextDirIndex] = cachedLessBendingDirs.back();
			cachedLessBendingDirs.pop_back();
		}
	}

	// Ensure we can assume at least one free array cell in the loop below.
	if( suggestedLookDirs.size() == suggestedLookDirs.capacity() ) {
		return;
	}

	// Actually, derive more dirs from saved dirs

	assert( !suggestedLookDirs.empty() );
	// Save this fixed value (as the dirs array is going to grow)
	const size_t lastBaseDirIndex = suggestedLookDirs.size() - 1u;
	// For every base dir from kept given ones
	for( size_t baseDirIndex = 0; baseDirIndex <= lastBaseDirIndex; ++baseDirIndex ) {
		const auto &__restrict base = suggestedLookDirs[baseDirIndex];
		const auto &__restrict baseDir = base.dir;
		const auto &__restrict baseTestedDir = cachedLessBendingDirs[baseDirIndex];
		// Produce a rotated dir for every possible rotation
		for( const auto &rotator: dirRotatorsCache ) {
			Vec3 rotated( rotator.rotate( baseDir ) );
			Vec3 lessBendingRotated( rotated );
			lessBendingRotated.Z() *= Z_NO_BEND_SCALE;
			lessBendingRotated *= Q_RSqrt( lessBendingRotated.SquaredLength() );
			// Check whether there is a similar dir using comparisons of respective "less bending" dirs
			bool hasASimilarDir = false;
			for( const auto &__restrict lessBendingExisting: cachedLessBendingDirs ) {
				if( areDirsSimilar( lessBendingRotated, lessBendingExisting ) ) {
					hasASimilarDir = true;
					break;
				}
			}
			if( hasASimilarDir ) {
				continue;
			}
			// Save the rotated dir as a suggested one. Also save the respective "less bending" one for further steps
			cachedLessBendingDirs.push_back( lessBendingRotated );
			suggestedLookDirs.emplace_back( SuggestedDir( rotated, base.area, rotator.pathPenalty ) );
			if( suggestedLookDirs.size() == suggestedLookDirs.capacity() ) {
				return;
			}
		}
	}
}

AreaAndScore *BunnyTestingSavedLookDirsAction::TakeBestCandidateAreas( AreaAndScore *inputBegin,
																	   AreaAndScore *inputEnd,
																	   unsigned maxAreas ) {
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

void BunnyTestingSavedLookDirsAction::SaveCandidateAreaDirs( MovementPredictionContext *context,
															 AreaAndScore *candidateAreasBegin,
															 AreaAndScore *candidateAreasEnd ) {
	const auto &__restrict entityPhysicsState = context->movementState->entityPhysicsState;
	const int navTargetAreaNum = context->NavTargetAasAreaNum();
	const auto *__restrict aasAreas = AiAasWorld::Instance()->Areas();

	AreaAndScore *takenAreasBegin = candidateAreasBegin;
	assert( maxSuggestedLookDirs <= suggestedLookDirs.capacity() );
	unsigned maxAreas = maxSuggestedLookDirs - suggestedLookDirs.size();
	AreaAndScore *takenAreasEnd = TakeBestCandidateAreas( candidateAreasBegin, candidateAreasEnd, maxAreas );

	suggestedLookDirs.clear();
	for( auto iter = takenAreasBegin; iter < takenAreasEnd; ++iter ) {
		const int areaNum = ( *iter ).areaNum;
		assert( (unsigned)areaNum < (unsigned)AiAasWorld::Instance()->NumAreas() );
		Vec3 dir( context->NavTargetOrigin() );
		if( areaNum != navTargetAreaNum ) {
			const auto &area = aasAreas[areaNum];
			dir.Set( area.center );
			dir.Z() = area.mins[2] + 32.0f;
		}
		dir -= entityPhysicsState.Origin();
		if( areaNum != navTargetAreaNum ) {
			dir.Z() *= Z_NO_BEND_SCALE;
		}
		const auto squareLen = dir.SquaredLength();
		if( squareLen > 1.0f ) {
			dir *= Q_RSqrt( squareLen );
			suggestedLookDirs.emplace_back( SuggestedDir( dir, areaNum ) );
		}
	}
}