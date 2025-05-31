#include <common/q_math.h>
#include <client/snd_public.h>
#include <common/wswstaticvector.h>

#include "backend.h"
#include "snd_local.h"

namespace wsw::snd {

class ALSoundSystem : public SoundSystem {
public:
	struct ThreadProcArg {
		qbufPipe_s *pipe;
		ALSoundSystem *instance;
	};

	[[nodiscard]]
	static auto tryCreate( client_state_s *client, bool verbose ) -> ALSoundSystem *;

	ALSoundSystem( client_state_s *client, qbufPipe_s *pipe, qthread_s *thread, bool verbose );
	~ALSoundSystem() override;

	void deleteSelf( bool verbose ) override;

	void postInit() override;

	void beginRegistration() override;
	void endRegistration() override;

	void stopSounds( unsigned flags ) override;

	void updateListener( int entNum, const float *origin, const float *velocity, const mat3_t axis ) override;

	void activate( bool isActive ) override;

	void processFrameUpdates() override;

	void setEntitySpatialParams( int entNum, const float *origin, const float *velocity, const float *axis ) override;

	[[nodiscard]]
	auto registerSound( const SoundSetProps &props ) -> const SoundSet * override;

	void startFixedSound( const SoundSet *sfx, const float *origin, int channel, float volume, float attenuation ) override;
	void startRelativeSound( const SoundSet *sfx, SoundSystem::AttachmentTag, int entNum, int channel, float volume, float attenuation ) override;
	void startLocalSound( const char *name, float volume ) override;
	void startLocalSound( const SoundSet *sfx, float volume ) override;

	void addLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, uintptr_t identifyingToken, float volume, float attenuation ) override;

	void startBackgroundTrack( const char *intro, const char *loop, int mode ) override;
	void stopBackgroundTrack() override;
	void prevBackgroundTrack() override;
	void nextBackgroundTrack() override;
	void pauseBackgroundTrack() override;
private:
	void flushEntitySpatialParams();

	Backend::EntitySpatialParamsBatch m_spatialParamsBatch;

	qthread_s *m_thread { nullptr };
	qbufPipe_s *m_pipe { nullptr };
	bool m_useVerboseShutdown { false };

	Backend m_backend;
};

}