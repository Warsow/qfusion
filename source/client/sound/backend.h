#ifndef WSW_3d1517a5_58dc_4c2e_b388_4ed859c532e2_H
#define WSW_3d1517a5_58dc_4c2e_b388_4ed859c532e2_H

// TODO: lift it to the top level
#include <server/game/ai/vec3.h>
#include <common/types/staticvector.h>
#include <common/facilities/q_comref.h>
#include "soundsetcache.h"
#include "sourcemanager.h"
#include "snd_local.h"
#include <array>

namespace wsw::snd {

class Backend {
	friend class ALSoundSystem;
public:
	void init( bool verbose );
	void shutdown( bool verbose );

	void stopSounds( unsigned flags );

	[[maybe_unused]]
	auto loadSound( const SoundSetProps &props ) -> const SoundSet *;
	[[nodiscard]]
	auto findSoundSet( const SoundSetProps &props ) -> const SoundSet *;

	void beginRegistration();
	void endRegistration();

	struct EntitySpatialParamsBatch {
		EntitySpatialParams params[8];
		unsigned count { 0 };
	};

	void setEntitySpatialParams( const EntitySpatialParamsBatch &batch );
	void processFrameUpdates( const EntitySpatialParams &listenerSpatialParams );

	void startLocalSound( const SoundSet *sound, float volume );
	void startLocalSoundByName( const PodVector<char> &name, float volume );
	void startFixedSound( const SoundSet *sound, const Vec3 &origin, int channel, float volume, float attenuation );
	void startRelativeSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, int channel, float volume, float attenuation );
	void addLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, uintptr_t identifyingToken, float volume, float attenuation );

	void startBackgroundTrack( const wsw::PodVector<char> &intro, const wsw::PodVector<char> &loop, int mode );
	void stopBackgroundTrack();
	void lockBackgroundTrack( bool lock );
	void advanceBackgroundTrack( int value );
	void activate( bool active );

	[[nodiscard]] auto getSourceManager() -> SourceManager * { return m_sourceManager; }

private:
	SourceManager *m_sourceManager { nullptr };
	SoundSetCache *m_soundSetCache { nullptr };

	ALCcontext *m_context { nullptr };
	ALCdevice *m_device { nullptr };

	unsigned m_registrationSequence { 1 };

	bool m_initialized { false };
};

}

#endif