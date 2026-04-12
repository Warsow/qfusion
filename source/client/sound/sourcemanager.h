/*
===========================================================================
Copyright (C) 1999-2005 Id Software, Inc.
Copyright (C) 2005 Stuart Dalton (badcdev@gmail.com)
Copyright (C) 2017-2026 Chasseur de bots

This file is part of Quake III Arena source code.

Quake III Arena source code is free software; you can redistribute it
and/or modify it under the terms of the GNU General Public License as
published by the Free Software Foundation; either version 2 of the License,
or (at your option) any later version.

Quake III Arena source code is distributed in the hope that it will be
useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Foobar; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
===========================================================================
*/

#ifndef WSW_176cbcc8_6069_4b78_8228_4138691177e0_H
#define WSW_176cbcc8_6069_4b78_8228_4138691177e0_H

#include <client/snd_public.h>
#include <common/facilities/protocol.h>
#include <common/helpers/freelistallocator.h>
#include "efxpresetsregistry.h"

#define AL_ALEXT_PROTOTYPES
#define AL_LIBTYPE_STATIC

#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alext.h>
#include <AL/efx.h>

struct SoundSet;
struct cvar_s;

struct SamplingProps {
	float quality { 0.0f };
	unsigned numSamples { 0 };
	uint16_t valueIndex { 0 };
};

struct PanningUpdateState {
	static constexpr auto MAX_POINTS = 80;
	float reflectionPoints[MAX_POINTS][3];
	unsigned numPrimaryRays;
	unsigned numPassedSecondaryRays;
};

struct ReverbEffectProps {
	EfxReverbProps reverbProps {};
	float directObstruction { 0.0f };
	float secondaryRaysObstruction { 0.0f };
};

struct EnvUpdateState {
	int64_t nextUpdateAt { 0 };
	int64_t lastUpdateAt { 0 };

	ReverbEffectProps effectProps;

	SamplingProps directObstructionSamplingProps;

	float lastUpdateOrigin[3] { 0.0f, 0.0f, 0.0f };
	float lastUpdateVelocity[3] { 0.0f, 0.0f, 0.0f };

	// A distance between emitter and listener at last props update
	float distanceAtLastUpdate { 0.0f };
};

struct Source {
	Source *prev { nullptr };
	Source *next { nullptr };

	int64_t spawnedAt { 0 };

	// Used for bitset-based algorithms
	unsigned index { ~0u };

	ALuint source { 0 };

	ALuint directFilter { 0 };
	ALuint effect { 0 };
	ALuint effectSlot { 0 };

	const SoundSet *sfx { nullptr };
	unsigned bufferIndex { 0 };

	cvar_s *volumeVar { nullptr };

	uintptr_t loopIdentifyingToken { 0 };

	int priority { 0 };
	int entNum { 0 };
	int channel { 0 };
	SoundSystem::AttachmentTag attachmentTag { SoundSystem::OriginAttachment };
	float chosenPitch { 0.0f };

	float fvol { 0.0f };
	float attenuation { 0.0f };

	// Just a stub to make stream code compile !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	bool isActive { false };

	bool isLooping { false };
	bool isTracking { false };
	bool isLingering { false };
	bool touchedThisFrame { false };
	bool effectActive { false };

	int64_t lingeringTimeoutAt { 0 };

	EnvUpdateState envUpdateState {};
	PanningUpdateState panningUpdateState {};

	float origin[3] { 0.0f, 0.0f, 0.0f }, velocity[3] { 0.0f, 0.0f, 0.0f };
};

class SourceManager {
public:
	static constexpr unsigned kMaxSources = 128;

	SourceManager();
	~SourceManager();

	[[nodiscard]]
	auto getActiveSourcesHead() -> Source * { return m_activeSourcesHead; };

	void setEntitySpatialParams( const EntitySpatialParams &params ) {
		assert( (unsigned)params.entNum < (unsigned)MAX_EDICTS );
		m_entitySpatialParams[params.entNum] = params;
	}

	void startFixedSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex,
						  float pitch, const vec3_t origin, int channel, float fvol, float attenuation );
	void startRelativeSound( const SoundSet *sfx, SoundSystem::AttachmentTag attachmentTag,
							 std::pair<ALuint, unsigned> bufferAndIndex,
							 float pitch, int entnum, int channel, float fvol, float attenuation );
	void startLocalSound( const SoundSet *sound, std::pair<ALuint, unsigned> bufferAndIndex, float pitch, float fvol );
	void touchLoopSound( const SoundSet *sound, SoundSystem::AttachmentTag attachmentTag,
						 std::pair<ALuint, unsigned> bufferAndIndex,
						 float pitch, int entnum, uintptr_t identifyingToken, float fvol, float attenuation );

	void updateSources( int64_t millisNow, const float *listenerOrigin );
	void stopAllSources( bool retainLocal );
private:
	void processZombieSources( int64_t millisNow, const float *listenerOrigin, Source **zombieSources,
							   unsigned numZombieSources, unsigned numActiveEffects );
	[[nodiscard]]
	auto killZombieSources( Source **zombieSources, unsigned numZombieSources,
							unsigned numActiveEffects, unsigned effectNumberThreshold ) -> unsigned;

	void disableExcessiveEffects( const float *listenerOrigin, unsigned numActiveEffects, unsigned effectNumberThreshold );

	void updateSpatialParams( Source *src );
	void killSource( Source *src );

	[[nodiscard]]
	auto allocSource( int priority, int entNum, int channel ) -> Source *;

	void startOneshotSound( const SoundSet *sfx, std::pair<ALuint, unsigned> bufferAndIndex, float pitch,
							const float *origin, int entNum, int channel, SoundSystem::AttachmentTag attachmentTag,
							float fvol, float attenuation );

	void setupSource( Source *src, const SoundSet *sfx, std::pair<ALuint, unsigned> chosenBufferAndIndex, float chosenPitch,
					  int priority, int entNum, int channel, SoundSystem::AttachmentTag attachmentTag, float fvol, float attenuation );

	Source *m_activeSourcesHead { nullptr };

	wsw::MemberBasedFreelistAllocator<sizeof( Source ), kMaxSources> m_sourceAllocator;

	struct CachedHandles {
		ALuint source { 0 };
		ALuint directFilter { 0 };
		ALuint effect { 0 };
		ALuint effectSlot { 0 };
	};

	[[nodiscard]]
	bool createHandles( CachedHandles *handles );
	void destroyHandles( CachedHandles *handles );

	// Note: We cut the actual capacity in half if effects are enabled
	wsw::StaticVector<CachedHandles, kMaxSources> m_cachedHandles;
	EntitySpatialParams m_entitySpatialParams[MAX_EDICTS];
};

#endif