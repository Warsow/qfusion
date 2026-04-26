#include <common/helpers/q_math.h>
#include <client/snd_public.h>
#include <common/helpers/qthreads.h>
#include <common/types/staticvector.h>

#include "backend.h"
#include "snd_local.h"

namespace wsw::snd {

class ALSoundSystem : public SoundSystem {
public:
	[[nodiscard]]
	static auto tryCreate( client_state_s *client, bool verbose ) -> ALSoundSystem *;

	ALSoundSystem( qbufPipe_s *pipe, qthread_s *thread, Backend *backend );
	~ALSoundSystem() override;

	void deleteSelf( bool verbose ) override;

	void postInit() override;

	void beginRegistration() override;
	void endRegistration() override;

	void stopSounds( unsigned flags ) override;

	void activate( bool isActive ) override;

	void processFrameUpdates( const EntitySpatialParams &listenerSpatialParams ) override;
	void setEntitySpatialParams( const EntitySpatialParams &spatialParams ) override;

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

	[[nodiscard]] auto getBackend() -> Backend * { return m_backend; }
private:
	struct ThreadFuncArg {
		qbufPipe_s *pipe;
		// An address of an atomic boolean variable
		void *isSoundSystemInitialized;
	};

	static auto threadFunc( void *threadProcArg ) -> void *;

	struct UpdateFuncArg {
		// An address of an atomic boolean variable
		void *isSoundSystemInitialized;
	};

	static void updateFunc( void *arg );

	void flushEntitySpatialParams();

	Backend::EntitySpatialParamsBatch m_spatialParamsBatch;

	qbufPipe_s *const m_pipe;
	qthread_s *const m_thread;
	Backend *const m_backend;
};

}