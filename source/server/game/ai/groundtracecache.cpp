#include "groundtracecache.h"
#include <common/types/staticvector.h>
#include "ailocal.h"

struct CachedTrace {
	trace_t trace;
	int64_t computedAt;
	float depth;
};

AiGroundTraceCache::AiGroundTraceCache() {
	data = Q_malloc( MAX_EDICTS * sizeof( CachedTrace ) );
	memset( data, 0, MAX_EDICTS * sizeof( CachedTrace ) );
}

AiGroundTraceCache::~AiGroundTraceCache() {
	if( data ) {
		Q_free( data );
	}
}

AiGroundTraceCache *AiGroundTraceCache::instance = nullptr;
static wsw::StaticVector<AiGroundTraceCache, 1> instanceHolder;

void AiGroundTraceCache::Init() {
	assert( instanceHolder.empty() );
	instance = new( instanceHolder.unsafe_grow_back() )AiGroundTraceCache;
}

void AiGroundTraceCache::Shutdown() {
	if( instance ) {
		instance = nullptr;
		instanceHolder.clear();
	}
}

void AiGroundTraceCache::GetGroundTrace( const edict_s *ent, float depth, trace_t *trace, uint64_t maxMillisAgo ) {
	edict_t *entRef = const_cast<edict_t *>( ent );
	CachedTrace *cachedTrace = (CachedTrace *)data + ENTNUM( entRef );

	if( (int64_t)( cachedTrace->computedAt + maxMillisAgo ) >= level.time ) {
		if( cachedTrace->depth >= depth ) {
			trace->startsolid = cachedTrace->trace.startsolid;
			if( cachedTrace->trace.fraction == 1.0f ) {
				trace->fraction = 1.0f;
				return;
			}
			float cachedHitDepth = cachedTrace->depth * cachedTrace->trace.fraction;
			if( cachedHitDepth > depth ) {
				trace->fraction = 1.0f;
				return;
			}
			// Copy trace data
			*trace = cachedTrace->trace;
			// Recalculate result fraction
			trace->fraction = cachedHitDepth / depth;
			return;
		}
	}

	vec3_t end = { ent->s.origin[0], ent->s.origin[1], ent->s.origin[2] - depth };
	G_Trace( &cachedTrace->trace, entRef->s.origin, nullptr, nullptr, end, entRef, MASK_AISOLID );
	// Copy trace data
	*trace = cachedTrace->trace;
	cachedTrace->depth = depth;
	cachedTrace->computedAt = level.time;
	return;
}

// Uses the same algorithm as GetGroundTrace() but avoids trace result copying and thus a is a bit faster.
bool AiGroundTraceCache::TryDropToFloor( const struct edict_s *ent, float depth, vec3_t result, uint64_t maxMillisAgo ) {
	edict_t *entRef = const_cast<edict_t *>( ent );
	CachedTrace *cachedTrace = (CachedTrace *)data + ENTNUM( entRef );

	VectorCopy( ent->s.origin, result );

	if( (int64_t)( cachedTrace->computedAt + maxMillisAgo ) >= level.time ) {
		if( cachedTrace->depth >= depth ) {
			if( cachedTrace->trace.fraction == 1.0f ) {
				return false;
			}
			float cachedHitDepth = cachedTrace->depth * cachedTrace->trace.fraction;
			if( cachedHitDepth > depth ) {
				return false;
			}

			VectorCopy( cachedTrace->trace.endpos, result );
			result[2] += 16.0f; // Add some delta
			return true;
		}
	}

	vec3_t end = { ent->s.origin[0], ent->s.origin[1], ent->s.origin[2] - depth };
	G_Trace( &cachedTrace->trace, entRef->s.origin, nullptr, nullptr, end, entRef, MASK_AISOLID );
	cachedTrace->depth = depth;
	cachedTrace->computedAt = level.time;
	if( cachedTrace->trace.fraction == 1.0f ) {
		return false;
	}

	VectorCopy( cachedTrace->trace.endpos, result );
	result[2] += 16.0f;
	return true;
}
