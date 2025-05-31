#include "snd_leaf_props_cache.h"
#include "snd_effect_sampler.h"
#include <common/glob.h>
#include <common/singletonholder.h>
#include <common/tasksystem.h>
#include <common/wswstringview.h>
#include <common/wswstringsplitter.h>
#include <common/wswstaticstring.h>
#include <common/wswfs.h>

#include <new>
#include <limits>
#include <cstdlib>
// TODO: Use our custom implementation
#include <vector>

using wsw::operator""_asView;

class LeafPropsIOHelper {
protected:
	int leafCounter { 0 };
public:
	virtual ~LeafPropsIOHelper() = default;
};

struct EfxPresetEntry;

class LeafPropsReader final: public CachedComputationReader, public LeafPropsIOHelper {
public:
private:
	bool ParseLine( char *line, unsigned lineLength, LeafProps *props );
public:
	explicit LeafPropsReader( const LeafPropsCache *parent_, int fileFlags )
		: CachedComputationReader( parent_, fileFlags, true ) {}

	enum Status {
		OK,
		DONE,
		ERROR
	};

	Status ReadNextProps( LeafProps *props );
};

class LeafPropsWriter final: public CachedComputationWriter, public LeafPropsIOHelper {
public:
	explicit LeafPropsWriter( const LeafPropsCache *parent_ )
		: CachedComputationWriter( parent_ ) {}

	bool WriteProps( const LeafProps &props );
};

LeafPropsReader::Status LeafPropsReader::ReadNextProps( LeafProps *props ) {
	if( fsResult < 0 ) {
		return ERROR;
	}

	SkipWhiteSpace();
	if( !*dataPtr ) {
		return DONE;
	}

	char *nextLine = strchr( dataPtr, '\n' );
	if( nextLine ) {
		*nextLine = '\0';
	}

	size_t lineLength = nextLine - dataPtr;
	if( lineLength > std::numeric_limits<uint32_t>::max() ) {
		dataPtr = nullptr;
		return ERROR;
	}

	if( !ParseLine( dataPtr, (uint32_t)lineLength, props ) ) {
		dataPtr = nullptr;
		return ERROR;
	}

	if( nextLine ) {
		dataPtr = nextLine + 1;
	} else {
		fileData[0] = '\0';
		dataPtr = fileData;
	}

	leafCounter++;

	return OK;
}

bool LeafPropsReader::ParseLine( char *line, unsigned lineLength, LeafProps *props ) {
	char *linePtr = line;
	char *endPtr = nullptr;

	// Parsing errors are caught implicitly by the counter comparison below
	const auto num = (int)std::strtol( linePtr, &endPtr, 10 );
	if( !*endPtr || !isspace( *endPtr ) ) {
		return false;
	}

	if( num != leafCounter ) {
		return false;
	}

	// Trim whitespace at start and end. This is mandatory for FindPresetByName() call.

	linePtr = endPtr;
	while( ( linePtr - line < lineLength ) && ::isspace( *linePtr ) ) {
		linePtr++;
	}

	endPtr = line + lineLength;
	while( endPtr > linePtr && ( !*endPtr || ::isspace( *endPtr ) ) ) {
		endPtr--;
	}

	if( endPtr <= linePtr ) {
		return false;
	}

	// Terminate after the character we have stopped at
	*++endPtr = '\0';

	int lastPartNum = 0;
	float parts[4];
	for(; lastPartNum < 4; ++lastPartNum ) {
		double v = ::strtod( linePtr, &endPtr );
		if( endPtr == linePtr ) {
			break;
		}
		// The value is numeric but is out of range.
		// It is not a preset for sure, return with failure immediately.
		// The range is [0, 1] for first 4 parts and [1000, 20000] for last 2 parts.
		if( lastPartNum <= 3 ) {
			if( v < 0.0 || v > 1.0 ) {
				return false;
			}
		} else if( v < 1000.0f || v > 20000.0f ) {
			return false;
		}
		if( lastPartNum != 3 ) {
			if( !isspace( *endPtr ) ) {
				return false;
			}
		} else if( *endPtr ) {
			return false;
		}
		parts[lastPartNum] = (float)v;
		linePtr = endPtr + 1;
	}

	// There should be 4 parts
	if( lastPartNum != 4 ) {
		return false;
	}

	// The HF reference range must be valid
	props->setRoomSizeFactor( parts[0] );
	props->setSkyFactor( parts[1] );
	props->setSmoothnessFactor( parts[2] );
	props->setMetallnessFactor( parts[3] );
	return true;
}

bool LeafPropsWriter::WriteProps( const LeafProps &props ) {
	if( fsResult < 0 ) {
		return false;
	}

	char buffer[MAX_STRING_CHARS];
	int charsPrinted = Q_snprintfz( buffer, sizeof( buffer ),
									"%d %.2f %.2f %.2f %.2f\r\n",
									leafCounter,
									props.getRoomSizeFactor(),
									props.getSkyFactor(),
									props.getSmoothnessFactor(),
									props.getMetallnessFactor());

	int charsWritten = FS_Write( buffer, (size_t)charsPrinted, fd );
	if( charsWritten != charsPrinted ) {
		fsResult = -1;
		return false;
	}

	leafCounter++;
	return true;
}

static SingletonHolder<LeafPropsCache> instanceHolder;

LeafPropsCache *LeafPropsCache::Instance() {
	return instanceHolder.instance();
}

void LeafPropsCache::Init() {
	instanceHolder.init();
}

void LeafPropsCache::Shutdown() {
	instanceHolder.shutdown();
}

void LeafPropsCache::ResetExistingState() {
	if( leafProps ) {
		Q_free( leafProps );
	}

	leafProps = (LeafProps *)Q_malloc( sizeof( LeafProps ) * NumLeafs() );
}

bool LeafPropsCache::TryReadFromFile( int fsFlags ) {
	LeafPropsReader reader( this, fsFlags );
	return TryReadFromFile( &reader );
}

bool LeafPropsCache::TryReadFromFile( LeafPropsReader *reader ) {
	int numReadProps = 0;
	for(;; ) {
		LeafProps props;
		switch( reader->ReadNextProps( &props ) ) {
			case LeafPropsReader::OK:
				if( numReadProps + 1 > NumLeafs() ) {
					return false;
				}
				this->leafProps[numReadProps] = props;
				numReadProps++;
				break;
			case LeafPropsReader::DONE:
				return numReadProps == NumLeafs();
			default:
				return false;
		}
	}
}

bool LeafPropsCache::SaveToCache() {
	LeafPropsWriter writer( this );
	for( int i = 0, end = NumLeafs(); i < end; ++i ) {
		if( !writer.WriteProps( this->leafProps[i] ) ) {
			return false;
		}
	}

	return true;
}

struct LeafPropsBuilder {
	float roomSizeFactor { 0.0f };
	float skyFactor { 0.0f };
	float smoothnessFactor { 0.0f };
	float metalnessFactor { 0.0f };
	int numProps { 0 };

	void operator+=( const LeafProps &propsToAdd ) {
		roomSizeFactor += propsToAdd.getRoomSizeFactor();
		skyFactor += propsToAdd.getSkyFactor();
		smoothnessFactor += propsToAdd.getSmoothnessFactor();
		metalnessFactor += propsToAdd.getMetallnessFactor();
		++numProps;
	}

	LeafProps Result() {
		LeafProps result;
		if( numProps ) {
			float scale = 1.0f / numProps;
			roomSizeFactor *= scale;
			skyFactor *= scale;
			smoothnessFactor *= scale;
			metalnessFactor *= scale;
		}
		result.setRoomSizeFactor( roomSizeFactor );
		result.setSkyFactor( skyFactor );
		result.setSmoothnessFactor( smoothnessFactor );
		result.setMetallnessFactor( metalnessFactor );
		return result;
	}
};

class LeafPropsSampler: public GenericRaycastSampler {
	static constexpr unsigned MAX_RAYS = 1024;

	// Inline buffers for algorithm intermediates.
	// Note that instances of this class should be allocated dynamically, so do not bother about arrays size.
	vec3_t dirs[MAX_RAYS];
	float distances[MAX_RAYS];
	const unsigned maxRays;

	unsigned numRaysHitSky { 0 };
	unsigned numRaysHitSmoothSurface { 0 };
	unsigned numRaysHitAbsorptiveSurface { 0 };
	unsigned numRaysHitMetal { 0 };

public:
	explicit LeafPropsSampler( bool fastAndCoarse )
		: maxRays( fastAndCoarse ? MAX_RAYS / 2 : MAX_RAYS ) {
		SetupSamplingRayDirs( dirs, maxRays );
	}

	[[nodiscard]]
	bool CheckAndAddHitSurfaceProps( const trace_t &trace ) override;

	[[nodiscard]]
	auto ComputeLeafProps( const vec3_t origin ) -> std::optional<LeafProps>;
};

static LeafProps ComputeLeafProps( LeafPropsSampler *sampler, int leafNum, bool fastAndCoarse );

bool LeafPropsCache::ComputeNewState( bool fastAndCoarse ) {
	leafProps[0] = LeafProps();

	const int actualNumLeafs = NumLeafs();
	// CBA to do a proper partitioning (we can't have more than 2^16 tasks)
	if( actualNumLeafs >= std::numeric_limits<uint16_t>::max() ) {
		return false;
	}

	try {
		TaskSystem taskSystem( { .numExtraThreads = S_SuggestNumExtraThreadsForComputations() } );
		std::vector<LeafPropsSampler> samplersForWorkers;
		for( unsigned i = 0, numWorkers = taskSystem.getNumberOfWorkers(); i < numWorkers; ++i ) {
			samplersForWorkers.emplace_back( LeafPropsSampler( fastAndCoarse ) );
		}
		// TODO: Let the task system manage it automatically?
		unsigned subrangeLength = 4;
		if( ( actualNumLeafs / subrangeLength ) + 16 >= TaskSystem::kMaxTaskEntries ) {
			subrangeLength = ( actualNumLeafs / TaskSystem::kMaxTaskEntries ) + 1;
		}
		auto fn = [=,&samplersForWorkers,this]( unsigned workerIndex, unsigned beginLeafIndex, unsigned endLeafIndex ) {
			for( unsigned leafIndex = beginLeafIndex; leafIndex < endLeafIndex; ++leafIndex ) {
				leafProps[leafIndex] = ComputeLeafProps( &samplersForWorkers[workerIndex], (int)leafIndex, fastAndCoarse );
			}
		};
		// Start early to test dynamic submission
		const TaskSystem::ExecutionHandle executionHandle = taskSystem.startExecution();
		(void)taskSystem.addForSubrangesInRange( { 1u, (unsigned)actualNumLeafs }, subrangeLength, {}, std::move( fn ) );
		return taskSystem.awaitCompletion( executionHandle );
	} catch( ... ) {
		return false;
	}
}

auto LeafPropsSampler::ComputeLeafProps( const vec3_t origin ) -> std::optional<LeafProps> {
	GenericRaycastSampler::ResetMutableState( dirs, nullptr, distances, origin );
	this->numRaysHitAbsorptiveSurface = 0;
	this->numRaysHitSmoothSurface = 0;
	this->numRaysHitMetal = 0;
	this->numRaysHitSky = 0;

	this->numPrimaryRays = maxRays;

	EmitPrimaryRays();

	// Happens mostly if rays outgoing from origin start in solid
	if( !numPrimaryHits ) {
		return std::nullopt;
	}

	assert( numRaysHitAnySurface >= numPrimaryHits );

	assert( numRaysHitSmoothSurface + numRaysHitAbsorptiveSurface <= numRaysHitAnySurface );
	// A neutral leaf is either surrounded by fully neutral surfaces or numbers of smooth and absorptive surfaces match
	// A frac is  0.0 for a neutral leaf
	// A frac is -1.0 for a leaf that is surrounded by absorptive surfaces
	// A frac is +1.0 for a leaf that is surrounded by smooth surfaces
	float frac = ( (float)numRaysHitSmoothSurface - (float)numRaysHitAbsorptiveSurface );
	frac *= 1.0f / (float)numRaysHitAnySurface;
	assert( frac >= -1.0f && frac <= +1.0f );

	// A smoothness is 0.5 for a neutral leaf
	// A smoothness is 0.0 for a leaf that is surrounded by absorptive surfaces
	// A smoothness is 1.0 for a leaf that is surrounded by smooth surfaces
	const float smoothness = 0.5f + 0.5f * frac;
	assert( smoothness >= 0.0f && smoothness <= 1.0f );

	LeafProps props {};
	props.setSmoothnessFactor( smoothness );
	props.setRoomSizeFactor( ComputeRoomSizeFactor() );
	const float rcpNumPrimaryHits = 1.0f / (float)numPrimaryHits;
	props.setSkyFactor( std::pow( (float)numRaysHitSky * rcpNumPrimaryHits, 0.25f ) );
	props.setMetallnessFactor( (float)numRaysHitMetal * rcpNumPrimaryHits );

	return props;
}

bool LeafPropsSampler::CheckAndAddHitSurfaceProps( const trace_t &trace ) {
	const auto contents = trace.contents;
	if( contents & ( CONTENTS_WATER | CONTENTS_SLIME ) ) {
		numRaysHitSmoothSurface++;
	} else if( contents & CONTENTS_LAVA ) {
		numRaysHitAbsorptiveSurface++;
	}

	const auto surfFlags = trace.surfFlags;
	if( surfFlags & ( SURF_SKY | SURF_NOIMPACT | SURF_NOMARKS | SURF_FLESH | SURF_NOSTEPS ) ) {
		if( surfFlags & SURF_SKY ) {
			numRaysHitSky++;
		}
		return false;
	}

	// Already tested for smoothness / absorption
	if( contents & MASK_WATER ) {
		return false;
	}

	// TODO: using enum (doesn't work with GCC 10)
	using IM = SurfImpactMaterial;

	const IM material = decodeSurfImpactMaterial( surfFlags );
	if( material == IM::Metal || material == IM::Glass ) {
		numRaysHitSmoothSurface++;
	} else if( material == IM::Stucco || material == IM::Dirt || material == IM::Wood || material == IM::Sand ) {
		numRaysHitAbsorptiveSurface++;
	}

	if( material == IM::Metal ) {
		numRaysHitMetal++;
	}

	return true;
}

static LeafProps ComputeLeafProps( LeafPropsSampler *sampler, int leafNum, bool fastAndCoarse ) {
	const vec3_t *leafBounds = S_GetLeafBounds( leafNum );
	const float *leafMins = leafBounds[0];
	const float *leafMaxs = leafBounds[1];

	vec3_t extent;
	VectorSubtract( leafMaxs, leafMins, extent );
	const float squareExtent = VectorLengthSquared( extent );

	LeafPropsBuilder propsBuilder;

	const float unitCellSize = 96.0f;
	const float numUnitCells = squareExtent * Q_RSqrt( unitCellSize );

	int maxSamples;
	if( fastAndCoarse ) {
		// Use a 1.5-pow growth in this case
		maxSamples = wsw::clamp( (int)( std::pow( numUnitCells, 1.5f ) ), 32, 64 );
	} else {
		// Let maxSamples grow quadratic depending linearly of square extent value.
		// Cubic growth is more "natural" but leads to way too expensive computations.
		maxSamples = wsw::clamp( (int)( numUnitCells * numUnitCells ), 48, 192 );
	}

	int numSamples = 0;
	int numAttempts = 0;

	// Always start at the leaf center
	vec3_t point;
	VectorMA( leafMins, 0.5f, extent, point );
	bool hasValidPoint = true;

	while( numSamples < maxSamples ) {
		numAttempts++;
		// Attempts may fail for 2 reasons: can't pick a valid point and a point sampling has failed.
		// Use the shared loop termination condition for these cases.
		if( numAttempts > 7 + 2 * maxSamples ) {
			return propsBuilder.Result();
		}

		if( !hasValidPoint ) {
			for( int i = 0; i < 3; ++i ) {
				point[i] = leafMins[i] + ( 0.1f + 0.9f * EffectSamplers::SamplingRandom() ) * extent[i];
			}

			// Check whether the point is really in leaf (the leaf is not a box but is inscribed in the bounds box)
			// Some bogus leafs (that are probably degenerate planes) lead to infinite looping otherwise
			if( S_PointLeafNum( point ) != leafNum ) {
				continue;
			}

			hasValidPoint = true;
		}

		// Might fail if the rays outgoing from the point start in solid
		if( const auto maybeProps = sampler->ComputeLeafProps( point ) ) {
			propsBuilder += *maybeProps;
			numSamples++;
			// Invalidate previous point used for sampling
			hasValidPoint = false;
			continue;
		}
	}

	return propsBuilder.Result();
}
