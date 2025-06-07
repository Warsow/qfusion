#include "alsystemfacade.h"

#include <common/helpers/singletonholder.h>
#include <common/helpers/pipeutils.h>

#include "snd_local.h"
#include "snd_env_sampler.h"

static SingletonHolder<wsw::snd::ALSoundSystem> alSoundSystemHolder;
static bool s_registering;
int s_registration_sequence = 1;

extern cvar_s *s_globalfocus;

namespace wsw::snd {

wsw::snd::ALSoundSystem *wsw::snd::ALSoundSystem::tryCreate( client_state_s *client, bool verbose ) {
	auto *arg = (ThreadProcArg *)Q_malloc( sizeof( ThreadProcArg ) );
	if( !arg ) {
		return nullptr;
	}

	qbufPipe_s *pipe = QBufPipe_Create( 0x100000, 1 );
	if( !pipe ) {
		Q_free( arg );
		return nullptr;
	}

	arg->pipe     = pipe;
	// Pass the address of the instance.
	// It won't be accessed prior to the constructor call.
	// TODO: There should be guarantees regarding the object placement within the holder.
	arg->instance = (ALSoundSystem *)&::alSoundSystemHolder;

	// TODO: Pass both this instance and pipe via a heap-allocated argument
	qthread_s *thread = QThread_Create( S_BackgroundUpdateProc, arg );
	if( !thread ) {
		Q_free( arg );
		QBufPipe_Destroy( &pipe );
		return nullptr;
	}

	::alSoundSystemHolder.init( client, pipe, thread, verbose );
	ALSoundSystem *instance = ::alSoundSystemHolder.instance();
	assert( (void *)instance == (void *)&::alSoundSystemHolder );

	if( !instance->m_backend.m_initialized ) {
		// This should dispose the thread and the pipe
		instance->deleteSelf( verbose );
		return nullptr;
	}

	return instance;
}

void ALSoundSystem::deleteSelf( bool verbose ) {
	m_useVerboseShutdown = verbose;
	::alSoundSystemHolder.shutdown();
}

ALSoundSystem::ALSoundSystem( client_state_s *client, qbufPipe_s *pipe, qthread_s *thread, bool verbose )
	: SoundSystem( client ) {
	m_pipe   = pipe;
	m_thread = thread;

	callMethodOverPipe( pipe, &m_backend, &Backend::init, verbose );
	QBufPipe_Finish( pipe );
}

ALSoundSystem::~ALSoundSystem() {
	stopSounds( StopMusic );
	// wake up the mixer
	activate( true );

	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );

	ENV_Shutdown();

	// shutdown backend

	callMethodOverPipe( m_pipe, &m_backend, &Backend::shutdown, m_useVerboseShutdown );
	sendTerminateCmd( m_pipe );

	// wait for the queue to be terminated
	QBufPipe_Finish( m_pipe );

	// wait for the backend thread to die
	QThread_Join( m_thread );

	QBufPipe_Destroy( &m_pipe );
}

void ALSoundSystem::postInit() {
	ENV_Init();
}

void ALSoundSystem::beginRegistration() {
	s_registration_sequence++;
	if( !s_registration_sequence ) {
		s_registration_sequence = 1;
	}

	s_registering = true;

	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );
}

void ALSoundSystem::endRegistration() {
	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );

	callMethodOverPipe( m_pipe, &m_backend, &Backend::endRegistration );

	// wait for the queue to be processed
	QBufPipe_Finish( m_pipe );

	s_registering = false;

	ENV_EndRegistration();
}

void ALSoundSystem::stopSounds( unsigned flags ) {
	callMethodOverPipe( m_pipe, &m_backend, &Backend::stopSounds, flags );
}

auto ALSoundSystem::registerSound( const SoundSetProps &props ) -> const SoundSet * {
	callMethodOverPipe( m_pipe, &m_backend, &Backend::loadSound, props );
	QBufPipe_Finish( m_pipe );
	return m_backend.findSoundSet( props );
}

void ALSoundSystem::activate( bool active ) {
	if( !active && s_globalfocus->integer ) {
		return;
	}

	// TODO: Let the activate() backend call manage the track state?
	callMethodOverPipe( m_pipe, &m_backend, &Backend::lockBackgroundTrack, !active );
	callMethodOverPipe( m_pipe, &m_backend, &Backend::activate, active );
}

void ALSoundSystem::startFixedSound( const SoundSet *sound, const float *origin, int channel, float volume, float attenuation ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, &m_backend, &Backend::startFixedSound, sound, Vec3( origin ), channel, volume, attenuation );
	}
}

void ALSoundSystem::startRelativeSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, int channel, float volume, float attenuation ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, &m_backend, &Backend::startRelativeSound, sound, attachmentTag, entNum, channel, volume, attenuation );
	}
}

void ALSoundSystem::startLocalSound( const char *name, float volume ) {
	if( name && volume > 0.0f ) {
		callMethodOverPipe( m_pipe, &m_backend, &Backend::startLocalSoundByName, getPathForName( wsw::StringView( name ) ), volume );
	}
}

void ALSoundSystem::startLocalSound( const SoundSet *sound, float volume ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, &m_backend, &Backend::startLocalSound, sound, volume );
	}
}

void ALSoundSystem::addLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag, int entNum, uintptr_t identifyingToken, float volume, float attenuation ) {
	if( sound ) {
		callMethodOverPipe( m_pipe, &m_backend, &Backend::addLoopSound, sound, attachmentTag, entNum, identifyingToken, volume, attenuation );
	}
}

void ALSoundSystem::startBackgroundTrack( const char *intro, const char *loop, int mode ) {
	wsw::PodVector<char> introPath( getPathForName( intro ? wsw::StringView( intro ) : wsw::StringView() ) );
	wsw::PodVector<char> loopPath( getPathForName( loop ? wsw::StringView( loop ) : wsw::StringView() ) );

	callMethodOverPipe( m_pipe, &m_backend, &Backend::startBackgroundTrack, introPath, loopPath, mode );
}

void ALSoundSystem::stopBackgroundTrack() {
	callMethodOverPipe( m_pipe, &m_backend, &Backend::stopBackgroundTrack );
}

void ALSoundSystem::prevBackgroundTrack() {
	callMethodOverPipe( m_pipe, &m_backend, &Backend::advanceBackgroundTrack, -1 );
}

void ALSoundSystem::nextBackgroundTrack() {
	callMethodOverPipe( m_pipe, &m_backend, &Backend::advanceBackgroundTrack, +1 );
}

void ALSoundSystem::pauseBackgroundTrack() {
	callMethodOverPipe( m_pipe, &m_backend, &Backend::advanceBackgroundTrack, 0 );
}

void ALSoundSystem::updateListener( int entNum, const vec3_t origin, const vec3_t velocity, const mat3_t axis ) {
	std::array<Vec3, 3> argAxis {
		Vec3 { axis[0], axis[1], axis[2] },
		Vec3 { axis[3], axis[4], axis[5] },
		Vec3 { axis[6], axis[7], axis[8] }
	};

	callMethodOverPipe( m_pipe, &m_backend, &Backend::setListener, entNum, Vec3( origin ), Vec3( velocity ), argAxis );
}

void ALSoundSystem::setEntitySpatialParams( int entNum, const float *origin, const float *velocity, const float *axis ) {
	if( m_spatialParamsBatch.count == std::size( m_spatialParamsBatch.entNums ) ) [[unlikely]] {
		flushEntitySpatialParams();
	}

	m_spatialParamsBatch.entNums[m_spatialParamsBatch.count] = entNum;
	VectorCopy( origin, m_spatialParamsBatch.origins[m_spatialParamsBatch.count] );
	VectorCopy( velocity, m_spatialParamsBatch.velocities[m_spatialParamsBatch.count] );
	Matrix3_Copy( axis, m_spatialParamsBatch.axes[m_spatialParamsBatch.count] );
	m_spatialParamsBatch.count++;
}

void ALSoundSystem::processFrameUpdates() {
	flushEntitySpatialParams();
	callMethodOverPipe( m_pipe, &m_backend, &Backend::processFrameUpdates );
}

void ALSoundSystem::flushEntitySpatialParams() {
	if( m_spatialParamsBatch.count > 0 ) {
		callMethodOverPipe( m_pipe, &m_backend, &Backend::setEntitySpatialParams, m_spatialParamsBatch );
		m_spatialParamsBatch.count = 0;
	}
}

}