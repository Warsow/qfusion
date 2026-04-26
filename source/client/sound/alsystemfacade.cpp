#include "alsystemfacade.h"

#include <common/helpers/singletonholder.h>
#include <common/helpers/pipeutils.h>
#include <common/facilities/sysclock.h>

#include "snd_local.h"
#include "snd_propagation.h"
#include "snd_leaf_props_cache.h"
#include "environmentupdates.h"

#include <atomic>

static SingletonHolder<wsw::snd::ALSoundSystem> alSoundSystemHolder;

static std::atomic_bool g_isSoundSystemInitialized;
static std::atomic_bool g_useVerboseOperationLogging;

extern cvar_s *s_globalfocus;

namespace wsw::snd {

wsw::snd::ALSoundSystem *wsw::snd::ALSoundSystem::tryCreate( client_state_s *client, bool verbose ) {
	auto *arg = (ThreadFuncArg *)Q_malloc( sizeof( ThreadFuncArg ) );
	if( !arg ) {
		return nullptr;
	}

	qbufPipe_s *pipe = QBufPipe_Create( 1024 * 1024, 1 );
	if( !pipe ) {
		Q_free( arg );
		return nullptr;
	}

	arg->pipe                     = pipe;
	arg->isSoundSystemInitialized = &g_isSoundSystemInitialized;

	qthread_s *thread = QThread_Create( &ALSoundSystem::threadFunc, arg );
	if( !thread ) {
		Q_free( arg );
		QBufPipe_Destroy( &pipe );
		return nullptr;
	}

	g_useVerboseOperationLogging = verbose;

	// Now, create the backend
	Backend *backend = nullptr;
	// Passing the address variable by value is more clear
	Backend **ppBackend = &backend;

	callOverPipe( pipe, [=]() {
		try {
			*ppBackend = new Backend( verbose );
		} catch( ... ) {
			sError() << "Failed to create the sound system backend";
		}
	});
	QBufPipe_Finish( pipe );

	g_useVerboseOperationLogging = false;

	if( backend ) {
		// Always successful as we just copy pointers
		alSoundSystemHolder.init( pipe, thread, backend );
		g_isSoundSystemInitialized = true;
		return ::alSoundSystemHolder.instance();
	}

	return nullptr;
}


void ALSoundSystem::updateFunc( void *arg ) {
	auto *const isSoundSystemInitialized = (std::atomic_bool *)( ( (UpdateFuncArg *)arg )->isSoundSystemInitialized );
	if( *isSoundSystemInitialized ) {
		// TODO: Should this functionality belong to the backend?
		if( s_doppler->modified ) {
			if( s_doppler->value > 0.0f ) {
				alDopplerFactor( s_doppler->value );
			} else {
				alDopplerFactor( 0.0f );
			}
			s_doppler->modified = false;
		}

		if( s_sound_velocity->modified ) {
			// If environment effects are supported, we can set units to meters ratio.
			// In this case, we have to scale the hardcoded s_sound_velocity value.
			// (The engine used to assume that units to meters ratio is 1).
			float appliedVelocity = s_sound_velocity->value;
			if( appliedVelocity <= 0.0f ) {
				appliedVelocity = 0.0f;
			}
			alDopplerVelocity( appliedVelocity );
			alSpeedOfSound( appliedVelocity );
			s_sound_velocity->modified = false;
		}

		auto *soundSystem            = (wsw::snd::ALSoundSystem *)SoundSystem::instance();
		SourceManager *sourceManager = soundSystem->getBackend()->getSourceManager();

		S_UpdateMusic( sourceManager );
		sourceManager->updateStreamSources();
	}
}

static constexpr int64_t kUpdateIntervalMillis = 10;

auto ALSoundSystem::threadFunc( void *arg ) -> void * {
	qbufPipe_s *pipe               = ( ( ThreadFuncArg *)arg )->pipe;
	void *isSoundSystemInitialized = ( ( ThreadFuncArg *)arg )->isSoundSystemInitialized;

	// Don't hold the arg heap memory forever
	std::free( arg );

	struct WaiterFuncArg {
		int64_t *lastUpdateTime;
		void *isSoundSystemInitialized;
	};

	const auto waiterFunc = []( qbufPipe_s *queue, void *arg, bool timeout ) -> int {
		assert( arg );

		const int read = QBufPipe_ReadCmds( queue );
		if( read < 0 ) {
			// shutdown
			return read;
		}

		auto *waiterFuncArg        = (WaiterFuncArg *)arg;
		const int64_t currentTime  = Sys_Milliseconds();
		if( timeout || currentTime >= *waiterFuncArg->lastUpdateTime + kUpdateIntervalMillis ) {
			*waiterFuncArg->lastUpdateTime = currentTime;
			UpdateFuncArg updateFuncArg { .isSoundSystemInitialized = waiterFuncArg->isSoundSystemInitialized };
			ALSoundSystem::updateFunc( &updateFuncArg );
		}

		return read;
	};

	int64_t lastUpdateTime = 0;
	WaiterFuncArg waiterFuncArg { .lastUpdateTime = &lastUpdateTime, .isSoundSystemInitialized = isSoundSystemInitialized };
	QBufPipe_Wait( pipe, waiterFunc, &waiterFuncArg, kUpdateIntervalMillis );

	return nullptr;
}

void ALSoundSystem::deleteSelf( bool verbose ) {
	g_isSoundSystemInitialized   = false;
	g_useVerboseOperationLogging = verbose;
	::alSoundSystemHolder.shutdown();
	g_useVerboseOperationLogging = false;
}

ALSoundSystem::ALSoundSystem( qbufPipe_s *pipe, qthread_s *thread, Backend *backend )
	: m_pipe( pipe ), m_thread( thread ), m_backend( backend ) {}

ALSoundSystem::~ALSoundSystem() {
	stopSounds( StopMusic );
	// wake up the mixer
	activate( true );

	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );

	// TODO: Delegate this to the backend
	if( s_environment_effects->integer ) {
		LeafPropsCache::Shutdown();
		CachedLeafsGraph::Shutdown();
		PropagationTable::Shutdown();
	}

	m_backend->m_useVerboseShutdown = g_useVerboseOperationLogging;
	callOverPipe( m_pipe, [this]() { delete m_backend; } );
	QBufPipe_Finish( m_pipe );

	sendTerminateCmd( m_pipe );

	// wait for the queue to be terminated
	QBufPipe_Finish( m_pipe );

	// wait for the backend thread to die
	QThread_Join( m_thread );

	qbufPipe_s *pipe = m_pipe;
	QBufPipe_Destroy( &pipe );
	assert( !pipe );
}

void ALSoundSystem::postInit() {
	// TODO: Delegate this to the backend
	if( s_environment_effects->integer ) {
		LeafPropsCache::Init();
		CachedLeafsGraph::Init();
		PropagationTable::Init();

		LeafPropsCache::Instance()->EnsureValid();
		CachedLeafsGraph::Instance()->EnsureValid();
		PropagationTable::Instance()->EnsureValid();
	}
}

void ALSoundSystem::beginRegistration() {
	callMethodOverPipe( m_pipe, m_backend, &Backend::beginRegistration );

	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );
}

void ALSoundSystem::endRegistration() {
	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );

	callMethodOverPipe( m_pipe, m_backend, &Backend::endRegistration );

	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );

	if( s_environment_effects->integer ) {
		LeafPropsCache::Instance()->EnsureValid();
		CachedLeafsGraph::Instance()->EnsureValid();
		PropagationTable::Instance()->EnsureValid();
	}
}

void ALSoundSystem::stopSounds( unsigned flags ) {
	callMethodOverPipe( m_pipe, m_backend, &Backend::stopSounds, flags );
}

auto ALSoundSystem::registerSound( const SoundSetProps &props ) -> const SoundSet * {
	callMethodOverPipe( m_pipe, m_backend, &Backend::loadSound, props );
	QBufPipe_Finish( m_pipe );
	return m_backend->findSoundSet( props );
}

void ALSoundSystem::activate( bool active ) {
	if( !active && s_globalfocus->integer ) {
		return;
	}

	// TODO: Let the activate() backend call manage the track state?
	callMethodOverPipe( m_pipe, m_backend, &Backend::lockBackgroundTrack, !active );
	callMethodOverPipe( m_pipe, m_backend, &Backend::activate, active );
}

void ALSoundSystem::startFixedSound( const SoundSet *sound, const float *origin, int channel, float volume, float attenuation ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, m_backend, &Backend::startFixedSound, sound, Vec3( origin ), channel, volume, attenuation );
	}
}

void ALSoundSystem::startRelativeSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, int channel, float volume, float attenuation ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, m_backend, &Backend::startRelativeSound, sound, attachmentTag, entNum, channel, volume, attenuation );
	}
}

void ALSoundSystem::startLocalSound( const char *name, float volume ) {
	if( name && volume > 0.0f ) {
		callMethodOverPipe( m_pipe, m_backend, &Backend::startLocalSoundByName, getPathForName( wsw::StringView( name ) ), volume );
	}
}

void ALSoundSystem::startLocalSound( const SoundSet *sound, float volume ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, m_backend, &Backend::startLocalSound, sound, volume );
	}
}

void ALSoundSystem::addLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, uintptr_t identifyingToken, float volume, float attenuation ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, m_backend, &Backend::addLoopSound, sound, attachmentTag, entNum, identifyingToken, volume, attenuation );
	}
}

void ALSoundSystem::startBackgroundTrack( const char *intro, const char *loop, int mode ) {
	wsw::PodVector<char> introPath( getPathForName( intro ? wsw::StringView( intro ) : wsw::StringView() ) );
	wsw::PodVector<char> loopPath( getPathForName( loop ? wsw::StringView( loop ) : wsw::StringView() ) );

	callMethodOverPipe( m_pipe, m_backend, &Backend::startBackgroundTrack, introPath, loopPath, mode );
}

void ALSoundSystem::stopBackgroundTrack() {
	callMethodOverPipe( m_pipe, m_backend, &Backend::stopBackgroundTrack );
}

void ALSoundSystem::prevBackgroundTrack() {
	callMethodOverPipe( m_pipe, m_backend, &Backend::advanceBackgroundTrack, -1 );
}

void ALSoundSystem::nextBackgroundTrack() {
	callMethodOverPipe( m_pipe, m_backend, &Backend::advanceBackgroundTrack, +1 );
}

void ALSoundSystem::pauseBackgroundTrack() {
	callMethodOverPipe( m_pipe, m_backend, &Backend::advanceBackgroundTrack, 0 );
}

void ALSoundSystem::setEntitySpatialParams( const EntitySpatialParams &spatialParams ) {
	if( m_spatialParamsBatch.count == std::size( m_spatialParamsBatch.params ) ) [[unlikely]] {
		flushEntitySpatialParams();
	}

	m_spatialParamsBatch.params[m_spatialParamsBatch.count++] = spatialParams;
}

void ALSoundSystem::processFrameUpdates( const EntitySpatialParams &listenerSpatialParams ) {
	flushEntitySpatialParams();
	callMethodOverPipe( m_pipe, m_backend, &Backend::processFrameUpdates, listenerSpatialParams );
}

void ALSoundSystem::flushEntitySpatialParams() {
	if( m_spatialParamsBatch.count > 0 ) {
		callMethodOverPipe( m_pipe, m_backend, &Backend::setEntitySpatialParams, m_spatialParamsBatch );
		m_spatialParamsBatch.count = 0;
	}
}

}