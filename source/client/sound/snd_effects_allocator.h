#ifndef QFUSION_SND_EFFECTS_ALLOCATOR_H
#define QFUSION_SND_EFFECTS_ALLOCATOR_H

#include "snd_env_effects.h"

#include <new>

// TODO: Rewrite effects to be descendants of AllocatorChild from CEF branch and just add/use Effect::DeleteSelf()
class alignas( 8 )EffectsAllocator {
	template<typename> friend class SingletonHolder;

	static_assert( sizeof( EaxReverbEffect ) >= sizeof( UnderwaterFlangerEffect ), "" );

	static constexpr auto MAX_EFFECT_SIZE = sizeof( EaxReverbEffect );

	static constexpr auto ENTRY_SIZE =
		MAX_EFFECT_SIZE % 8 ? MAX_EFFECT_SIZE + ( 8 - MAX_EFFECT_SIZE % 8 ) : MAX_EFFECT_SIZE;

	// For every source two effect storage cells are reserved (for current and old effect).
	alignas( 8 ) uint8_t storage[2 * ENTRY_SIZE * MAX_SRC];
	// For every source contains an effect type for the corresponding entry, or zero if an entry is unused.
	ALint effectTypes[MAX_SRC][2];

	void *AllocEntry( const src_t *src, ALint effectTypes );
	void FreeEntry( const void *entry );

	// We could return a reference to an effect type array cell,
	// but knowledge of these 2 indices is useful for debugging
	inline void GetEntryIndices( const void *entry, int *sourceIndex, int *interleavedSlotIndex );

	EffectsAllocator() {
		memset( storage, 0, sizeof( storage ) );
		memset( effectTypes, 0, sizeof( effectTypes ) );
	}

public:
	static EffectsAllocator *Instance();
	static void Init();
	static void Shutdown();

	UnderwaterFlangerEffect *NewFlangerEffect( const src_t *src ) {
		return new( AllocEntry( src, AL_EFFECT_FLANGER ) )UnderwaterFlangerEffect();
	}

	EaxReverbEffect *NewReverbEffect( const src_t *src ) {
		return new( AllocEntry( src, AL_EFFECT_EAXREVERB ) )EaxReverbEffect();
	}

	void DeleteEffect( Effect *effect );
};

#endif
