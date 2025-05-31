/*
Copyright (C) 2002-2003 Victor Luchits

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

#ifndef SOUND_PUBLIC_H
#define SOUND_PUBLIC_H

// snd_public.h -- sound dll information visible to engine

#define ATTN_NONE 0

//===============================================================

#include <common/wswstringview.h>
#include <common/stringspanstorage.h>
#include <initializer_list>
#include <variant>

struct SoundSet;

struct SoundSetProps {
	struct Exact {
		wsw::StringView value;
		explicit Exact( const wsw::StringView &value_ ) : value( value_ ) {}
		explicit Exact( const char *value_ ) : value( value_ ) {}
	};
	// Note: For now, patterns are only allowed in the basename part, not in extension or directory part.
	// Up to 16 files can be loaded by specifying Pattern, excessive files are ingored.
	struct Pattern {
		wsw::StringView pattern;
		explicit Pattern( const wsw::StringView &pattern_ ) : pattern( pattern_ ) {}
		explicit Pattern( const char *pattern_ ) : pattern( pattern_ ) {}
	};
	std::variant<Exact, Pattern> name;
	// Up to 16 values, there may be duplicates. An empty span means using default pitch.
	// Valid values are positive (1.0 for no pitch shift at all), invalid values are excluded.
	std::initializer_list<float> pitchVariations;
	// Assumed to be in [0, 1] range for majority of sounds (but values exceeding this range are allowed).
	// Spammy sounds like ricochets, plasma explosions, laser impact sounds should have it close to zero.
	// It should not be treated as generic gameplay importance of the sound,
	// but as a hint allowing lowering quality of sound processing for saving performance
	// (the sound stays playing but in a lower quality, without effects, etc).
	float processingQualityHint { 1.0f };
	bool lazyLoading { false };
};

struct client_state_s;

class SoundSystem {
	static SoundSystem *s_instance;

	client_state_s *const m_client;

#ifdef WIN32
	/**
	 * It's a good idea to limit the access to the {@code InstanceOrNull()}
	 * method only to this function to prevent spreading of this hack over the codebase
	 */
	friend void AppActivate( int, int, int );

	/**
	 * A hack that makes the instance state accessible for the Win32-specific code
	 */
	static SoundSystem *instanceOrNull() { return s_instance; }
#endif
protected:
	explicit SoundSystem( client_state_s *client ) : m_client( client ) {}
public:
	struct InitOptions {
		bool verbose { false };
		bool useNullSystem { false };
	};

	enum AttachmentTag {
		OriginAttachment,
		WeaponAttachment,
		HeadAttachment,
		FeetAttachment,
	};

	[[nodiscard]]
	static bool init( client_state_s *client, const InitOptions &options );

	static void shutdown( bool verbose );

	[[nodiscard]]
	static auto instance() -> SoundSystem * {
		assert( s_instance );
		return s_instance;
	}

	virtual ~SoundSystem() = default;

	// TODO: Fix this
	virtual void deleteSelf( bool verbose ) = 0;

	/**
	 * @todo this is just to break a circular dependency. Refactor global objects into SoundSystem member fields.
	 */
	virtual void postInit() = 0;

	virtual void beginRegistration() = 0;
	virtual void endRegistration() = 0;

	enum StopFlags : unsigned { StopMusic = 0x1, RetainLocal = 0x2 };

	virtual void stopSounds( unsigned flags = 0 ) = 0;

	virtual void updateListener( int entNum, const float *origin, const float *velocity, const mat3_t axis ) = 0;
	virtual void activate( bool isActive ) = 0;

	virtual void processFrameUpdates() = 0;

	virtual void setEntitySpatialParams( int entNum, const float *origin, const float *velocity, const float *axis ) = 0;

	[[nodiscard]]
	virtual auto registerSound( const SoundSetProps &props ) -> const SoundSet * = 0;

	[[nodiscard]]
	auto getExactName( const SoundSet *sound ) -> std::optional<wsw::StringView>;

	virtual void startFixedSound( const SoundSet *sound, const float *origin, int channel, float fvol, float attenuation ) = 0;
	virtual void startRelativeSound( const SoundSet *sound, AttachmentTag attachmentTag, int entNum, int channel, float fvol, float attenuation ) = 0;

	virtual void startLocalSound( const char *name, float fvol ) = 0;
	virtual void startLocalSound( const SoundSet *sound, float fvol ) = 0;
	virtual void addLoopSound( const SoundSet *sound, AttachmentTag attachmentTag, int entNum, uintptr_t identifyingToken, float fvol, float attenuation ) = 0;

	virtual void startBackgroundTrack( const char *intro, const char *loop, int mode ) = 0;
	virtual void stopBackgroundTrack() = 0;
	virtual void nextBackgroundTrack() = 0;
	virtual void prevBackgroundTrack() = 0;
	virtual void pauseBackgroundTrack() = 0;

	// TODO: A zero-terminated string container is more appropriate here
	[[nodiscard]]
	static auto getPathForName( const wsw::StringView &name ) -> wsw::PodVector<char>;

	[[nodiscard]]
	static bool getPathListForPattern( const wsw::StringView &pattern, wsw::StringSpanStorage<unsigned, unsigned> *pathListStorage );
};

#endif