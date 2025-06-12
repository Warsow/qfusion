#ifndef WSW_3d1517a5_58dc_4c2e_b388_4ed859c532e2_H
#define WSW_3d1517a5_58dc_4c2e_b388_4ed859c532e2_H

// TODO: lift it to the top level
#include <server/game/ai/vec3.h>
#include <common/types/staticvector.h>
#include <common/helpers/freelistallocator.h>
#include <common/helpers/randomgenerator.h>
#include <common/facilities/q_comref.h>
#include "snd_local.h"
#include <array>

namespace wsw::snd {

class Backend {
	friend class ALSoundSystem;
public:
	void init( bool verbose );
	void shutdown( bool verbose );

	void stopSounds( unsigned flags );

	void processFrameUpdates();

	[[maybe_unused]]
	auto loadSound( const SoundSetProps &props ) -> const SoundSet *;
	[[nodiscard]]
	auto findSoundSet( const SoundSetProps &props ) -> const SoundSet *;

	void endRegistration();

	struct EntitySpatialParamsBatch {
		int entNums[8];
		vec3_t origins[8];
		vec3_t velocities[8];
		mat3_t axes[8];
		unsigned count { 0 };
	};

	void setEntitySpatialParams( const EntitySpatialParamsBatch &batch );

	void setListener( int entNum, const Vec3 &origin, const Vec3 &velocity, const std::array<Vec3, 3> &axis );

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

private:
	void unlinkAndFree( SoundSet *soundSet );
	void forceLoading( SoundSet *soundSet );
	[[nodiscard]]
	bool loadBuffersFromFile( const wsw::StringView &filePath, ALuint *buffer, ALuint *stereoBuffer, unsigned *durationMillis );
	[[nodiscard]]
	auto uploadBufferData( const wsw::StringView &logFilePath, const snd_info_t &info, const void *data ) -> ALuint;

	[[nodiscard]]
	auto getBufferForPlayback( const SoundSet *soundSet, bool preferStereo = false ) -> std::optional<std::pair<ALuint, unsigned>>;
	[[nodiscard]]
	auto getPitchForPlayback( const SoundSet *soundSet ) -> float;

	static constexpr unsigned kMaxSoundSets = 256;

	SoundSet *m_registeredSoundSetsHead { nullptr };
	wsw::MemberBasedFreelistAllocator<sizeof( SoundSet ) + MAX_QPATH + 1, kMaxSoundSets> m_soundSetsAllocator;

	PodBufferHolder<uint8_t> m_fileDataBuffer;
	PodBufferHolder<uint8_t> m_resamplingBuffer;

	wsw::StringSpanStorage<unsigned, unsigned> m_tmpPathListStorage;

	wsw::RandomGenerator m_rng;

	bool m_initialized { false };
};

}

#endif