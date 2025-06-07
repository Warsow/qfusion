#ifndef WSW_20b581f0_0ddd_42e4_8f61_c3ae781fcc6b_H
#define WSW_20b581f0_0ddd_42e4_8f61_c3ae781fcc6b_H

#include <common/facilities/q_collision.h>

class AiGroundTraceCache {
	/**
	 * Declare an untyped pointer in order to prevent inclusion of g_local.h
	 */
	void *data;

	static AiGroundTraceCache *instance;
public:
	// TODO: Make private, use a SingletonHolder
	AiGroundTraceCache();
	~AiGroundTraceCache();

	static void Init();
	static void Shutdown();

	static AiGroundTraceCache *Instance() {
		assert( instance );
		return instance;
	}

	void GetGroundTrace( const struct edict_s *ent, float depth, trace_t *trace, uint64_t maxMillisAgo = 0 );
	bool TryDropToFloor( const struct edict_s *ent, float depth, vec3_t result, uint64_t maxMillisAgo = 0 );
};

#endif
