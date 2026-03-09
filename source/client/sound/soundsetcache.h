/*
Copyright (C) 1999-2005 Id Software, Inc.
Copyright (C) 2005 Stuart Dalton (badcdev@gmail.com)
Copyright (C) 2026 Chasseur de bots

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef WSW_69d2d850_d89d_4d0d_9ef5_491e12b8feb0_H
#define WSW_69d2d850_d89d_4d0d_9ef5_491e12b8feb0_H

#include "filedatabuffercache.h"
#include <common/helpers/randomgenerator.h>

#include "snd_local.h"

class SoundSetCache {
public:
	~SoundSetCache();

	void freeUnusedSoundSets( int registrationSequence );

	[[maybe_unused]]
	auto loadSound( int registrationSequence, const SoundSetProps &props ) -> const SoundSet *;
	[[nodiscard]]
	auto findSoundSet( const SoundSetProps &props ) -> const SoundSet *;

	[[nodiscard]]
	auto getBufferForPlayback( const SoundSet *soundSet, bool preferStereo = false ) -> std::optional<std::pair<ALuint, unsigned>>;
	[[nodiscard]]
	auto getPitchForPlayback( const SoundSet *soundSet ) -> float;
private:
	void unlinkAndFree( SoundSet *soundSet );
	void forceLoading( SoundSet *soundSet );

	void releaseFileDataBuffers( SoundSet *soundSet );

	static constexpr unsigned kMaxSoundSets { 256 };

	SoundSet *m_registeredSoundSetsHead { nullptr };
	wsw::MemberBasedFreelistAllocator<sizeof( SoundSet ) + MAX_QPATH + 1, kMaxSoundSets> m_soundSetsAllocator;

	FileDataBufferCache m_fileDataBufferCache;

	wsw::StringSpanStorage<unsigned, unsigned> m_tmpPathListStorage;

	wsw::RandomGenerator m_rng;
};

#endif