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

#include "soundsetcache.h"

#include <common/helpers/links.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/wswfs.h>

#include <span>

SoundSetCache::~SoundSetCache() {
	for( SoundSet *soundSet = m_registeredSoundSetsHead, *next; soundSet; soundSet = next ) { next = soundSet->next;
		unlinkAndFree( soundSet );
	}
}

void SoundSetCache::unlinkAndFree( SoundSet *soundSet ) {
	releaseFileDataBuffers( soundSet );
	wsw::unlink( soundSet, &m_registeredSoundSetsHead );
	soundSet->~SoundSet();
	m_soundSetsAllocator.free( soundSet );
}

void SoundSetCache::releaseFileDataBuffers( SoundSet *soundSet ) {
	for( unsigned i = 0; i < soundSet->numBuffers; ++i ) {
		m_fileDataBufferCache.release( soundSet->buffers[i] );
		soundSet->buffers[i] = nullptr;
	}
	soundSet->numBuffers = 0;
}

void SoundSetCache::freeUnusedSoundSets( int registrationSequence ) {
	for( SoundSet *soundSet = m_registeredSoundSetsHead, *next; soundSet; soundSet = next ) { next = soundSet->next;
		if( soundSet->registrationSequence != registrationSequence ) {
			unlinkAndFree( soundSet );
		}
	}
}

[[nodiscard]]
static auto getSoundSetName( const SoundSetProps &props ) -> wsw::StringView {
	if( const auto *exact = std::get_if<SoundSetProps::Exact>( &props.name ) ) {
		return exact->value;
	}
	if( const auto *pattern = std::get_if<SoundSetProps::Pattern>( &props.name ) ) {
		return pattern->pattern;
	}
	wsw::failWithLogicError( "Unreachable" );
}

[[nodiscard]]
static bool matchesByName( const SoundSetProps &lhs, const SoundSetProps &rhs ) {
	if( lhs.name.index() == rhs.name.index() ) {
		if( const auto *leftExact = std::get_if<SoundSetProps::Exact>( &lhs.name ) ) {
			const auto *rightExact = std::get_if<SoundSetProps::Exact>( &rhs.name );
			return leftExact->value.equalsIgnoreCase( rightExact->value );
		}
		if( const auto *leftPattern = std::get_if<SoundSetProps::Pattern>( &lhs.name ) ) {
			const auto *rightPattern = std::get_if<SoundSetProps::Pattern>( &rhs.name );
			return leftPattern->pattern.equalsIgnoreCase( rightPattern->pattern );
		}
		wsw::failWithLogicError( "Unreachable" );
	}
	return false;
}

[[nodiscard]]
static auto sanitizePitchVariations( const wsw::StringView &soundSetName, float *targetValues,
									 std::span<const float> givenValues ) -> unsigned {
	unsigned numSanitizedValues = 0;

	bool hasIllegalValues = false;
	bool hasTooManyValues = false;
	for( const float &value: givenValues ) {
		if( value <= 0.0f ) {
			hasIllegalValues = true;
		} else {
			if( numSanitizedValues == SoundSet::kMaxPitchVariations ) {
				hasTooManyValues = true;
			} else {
				targetValues[numSanitizedValues++] = value;
			}
		}
	}

	if( hasIllegalValues ) {
		sWarning() << "Pitch variations for sound set" << soundSetName << "have illegal values";
	}
	if( hasTooManyValues ) {
		sWarning() << "Too many pitch variations for sound set" << soundSetName;
	}

	assert( numSanitizedValues <= SoundSet::kMaxPitchVariations );
	return numSanitizedValues;
}

auto SoundSetCache::loadSound( int registrationSequence, const SoundSetProps &props ) -> const SoundSet * {
	[[maybe_unused]] const wsw::StringView name( getSoundSetName( props ) );

	float pitchVariations[SoundSet::kMaxPitchVariations];
	const unsigned numPitchVariations = sanitizePitchVariations( name, pitchVariations, props.pitchVariations );

	for( SoundSet *soundSet = m_registeredSoundSetsHead; soundSet; soundSet = soundSet->next ) {
		if( matchesByName( props, soundSet->props ) ) {
			bool paramsDiffer = false;
			// Note: We can't just assign values if we load different buffers for different variations
			if( soundSet->numPitchVariations != numPitchVariations ||
				!std::equal( pitchVariations, pitchVariations + numPitchVariations, soundSet->pitchVariations ) ) {
				paramsDiffer = true;
			}
			if( soundSet->props.processingQualityHint != props.processingQualityHint ) {
				paramsDiffer = true;
			}
			bool useThisSound = false;
			if( paramsDiffer ) {
				if( props.paramsOverrideMode == SoundSetProps::OverrideExistingParams ) {
					sWarning() << "Overwriting properties for already registered sound" << name;
					std::copy( pitchVariations, pitchVariations + numPitchVariations, soundSet->pitchVariations );
					soundSet->numPitchVariations          = numPitchVariations;
					soundSet->props.processingQualityHint = props.processingQualityHint;
					useThisSound = true;
				}
			} else {
				useThisSound = true;
			}
			if( useThisSound ) {
				soundSet->registrationSequence = registrationSequence;
				const bool hasToLoad = !props.lazyLoading && ( !soundSet->isLoaded && !soundSet->hasFailedLoading );
				if( hasToLoad ) {
					forceLoading( soundSet );
				}
				return soundSet;
			}
			// Otherwise, continue the loop and to find another sound set with exactly matching params.
		}
	}

	uint8_t *const mem = m_soundSetsAllocator.allocOrNull();
	if( !mem ) {
		sError() << "Failed to allocate a sound set for" << name << "(too many sound sets)";
		return nullptr;
	}

	auto *const newSoundSet = new( mem )SoundSet { .props = props, .registrationSequence = registrationSequence };

	// Save a deep copy of the name
	auto *const nameMem = (char *)( mem + sizeof( SoundSet ) );
	if( const auto *exactName = std::get_if<SoundSetProps::Exact>( &props.name ) ) {
		exactName->value.copyTo( nameMem, MAX_QPATH + 1 );
		newSoundSet->props.name = SoundSetProps::Exact { wsw::StringView( nameMem, exactName->value.size() ) };
	} else if( const auto *namePattern = std::get_if<SoundSetProps::Pattern>( &props.name ) ) {
		namePattern->pattern.copyTo( nameMem, MAX_QPATH + 1 );
		newSoundSet->props.name = SoundSetProps::Pattern { wsw::StringView( nameMem, namePattern->pattern.size() ) };
	} else {
		wsw::failWithLogicError( "Unreachable" );
	}

	std::copy( pitchVariations, pitchVariations + numPitchVariations, newSoundSet->pitchVariations );
	newSoundSet->numPitchVariations = numPitchVariations;

	wsw::link( newSoundSet, &m_registeredSoundSetsHead );

	if( !props.lazyLoading ) {
		forceLoading( newSoundSet );
	}

	return newSoundSet;
}

auto SoundSetCache::findSoundSet( const SoundSetProps &props ) -> const SoundSet * {
	for( SoundSet *soundSet = m_registeredSoundSetsHead; soundSet; soundSet = soundSet->next ) {
		if( matchesByName( props, soundSet->props ) ) {
			return soundSet;
		}
	}
	return nullptr;
}

// TODO: We need generic FS facilities for things like this
[[nodiscard]]
static bool getPathListForPattern( const wsw::StringView &pattern, wsw::StringSpanStorage<unsigned, unsigned> *pathListStorage ) {
	pathListStorage->clear();

	wsw::StringView dirName;
	wsw::StringView extension;
	wsw::StringView baseName = pattern;

	const std::optional<unsigned> dotIndex   = pattern.indexOf( '.' );
	const std::optional<unsigned> slashIndex = pattern.lastIndexOf( '/' );

	if( dotIndex != std::nullopt ) {
		if( slashIndex == std::nullopt || *slashIndex < *dotIndex ) {
			extension = pattern.drop( *dotIndex + 1 );
			baseName  = baseName.take( *dotIndex );
		}
	}
	if( slashIndex != std::nullopt ) {
		dirName  = pattern.take( *slashIndex );
		baseName = baseName.drop( *slashIndex + 1 );
	} else {
		dirName = wsw::StringView( "/" );
	}

	// For now, we limit patterns to basename
	wsw::PodVector<char> ztPattern( baseName.data(), baseName.size() );
	ztPattern.append( '\0' );
	wsw::PodVector<char> ztBaseName;

	wsw::fs::SearchResultHolder searchResultHolder;
	if( const auto maybeSearchResult = searchResultHolder.findDirFiles( dirName, extension ) ) {
		for( const wsw::StringView &fileName: *maybeSearchResult ) {
			bool isAcceptedByPattern = false;
			if( const std::optional<wsw::StringView> maybeBaseName = wsw::fs::stripExtension( fileName ) ) {
				ztBaseName.assign( maybeBaseName->data(), maybeBaseName->size() );
				ztBaseName.append( '\0' );
				if( Com_GlobMatch( ztPattern.data(), ztBaseName.data(), false ) ) {
					isAcceptedByPattern = true;
				}
			}
			if( isAcceptedByPattern ) {
				wsw::StaticString<MAX_QPATH> fullNameBuffer;
				fullNameBuffer << dirName << '/' << fileName;
				if( extension.empty() ) {
					if( const char *foundExtension = FS_FirstExtension( fullNameBuffer.data(), SOUND_EXTENSIONS, std::size( SOUND_EXTENSIONS ) ) ) {
						if( const char *existingExtension = COM_FileExtension( fullNameBuffer.data() ) ) {
							fullNameBuffer.erase( (unsigned)( existingExtension - fullNameBuffer.data() ) );
							fullNameBuffer << wsw::StringView( foundExtension );
						}
						bool alreadyPresent = false;
						for( const wsw::StringView &addedView : *pathListStorage ) {
							if( addedView.equalsIgnoreCase( fullNameBuffer.asView() ) ) {
								alreadyPresent = true;
								break;
							}
						}
						if( !alreadyPresent ) {
							pathListStorage->add( fullNameBuffer.asView() );
						}
					}
				} else {
					pathListStorage->add( fullNameBuffer.asView() );
				}
			}
		}
	}

	return !pathListStorage->empty();
}

void SoundSetCache::forceLoading( SoundSet *soundSet ) {
	assert( !soundSet->isLoaded && !soundSet->hasFailedLoading && soundSet->numBuffers == 0 );

	bool succeeded = false;
	if( const auto *exactName = std::get_if<SoundSetProps::Exact>( &soundSet->props.name ) ) {
		const wsw::PodVector<char> filePathData = SoundSystem::getPathForName( exactName->value );
		const wsw::StringView filePath( filePathData.data(), filePathData.size() - 1, wsw::StringView::ZeroTerminated );
		if( !filePath.empty() ) {
			if( const FileDataBuffer *fileDataBuffer = m_fileDataBufferCache.get( filePath ) ) {
				soundSet->buffers[0] = fileDataBuffer;
				soundSet->numBuffers = 1;
				succeeded            = true;
			} else {
				sError() << "Failed to load AL buffers for" << filePath;
			}
		} else {
			sError() << "Failed to find a path for exact name" << exactName->value;
		}
	} else if( const auto *namePattern = std::get_if<SoundSetProps::Pattern>( &soundSet->props.name ) ) {
		if( getPathListForPattern( namePattern->pattern, &m_tmpPathListStorage ) ) {
			const size_t maxBuffers = std::size( soundSet->buffers );
			for( const wsw::StringView &filePath: m_tmpPathListStorage ) {
				if( soundSet->numBuffers < maxBuffers ) {
					if( const FileDataBuffer *buffer = m_fileDataBufferCache.get( filePath ) ) {
						soundSet->buffers[soundSet->numBuffers++] = buffer;
					} else {
						sError() << "Failed to load AL buffers for" << filePath;
						releaseFileDataBuffers( soundSet );
					}
				} else {
					sWarning() << "Too many files matching" << namePattern->pattern;
					break;
				}
			}
			// TODO: Allow specifying "all-or-nothing" success policy?
			succeeded = soundSet->numBuffers > 0;
		} else {
			sError() << "Failed to get path list for pattern" << namePattern->pattern;
		}
	} else {
		wsw::failWithLogicError( "Unreachable" );
	}

	soundSet->isLoaded         = succeeded;
	soundSet->hasFailedLoading = !succeeded;
}

[[nodiscard]]
static auto choseIndex( unsigned limit, unsigned lastChosenIndex, wsw::RandomGenerator *rng ) -> unsigned {
	assert( limit < std::numeric_limits<uint8_t>::max() );
	if( limit < 2 ) [[likely]] {
		return 0;
	} else if( limit > 2 ) [[likely]] {
		for( unsigned attempt = 0; attempt < 20; ++attempt ) {
			if( const unsigned index = rng->nextBounded( limit ); index != lastChosenIndex ) {
				return index;
			}
		}
		wsw::failWithRuntimeError( "RNG bug" );
	} else {
		assert( limit == 2 && lastChosenIndex < 2 );
		// Allow chosing the last chosen item, but reduce the chance
		const unsigned altIndex = ( lastChosenIndex + 1 ) % 2;
		const unsigned values[3] { altIndex, lastChosenIndex, altIndex };
		return values[rng->nextBounded( std::size( values ) )];
	}
}

auto SoundSetCache::getBufferForPlayback( const SoundSet *soundSet, bool preferStereo ) -> std::optional<std::pair<ALuint, unsigned>> {
	if( soundSet ) {
		if( !soundSet->isLoaded ) {
			if( soundSet->hasFailedLoading ) {
				return std::nullopt;
			}
			// TODO? forceLoading( const SoundSet *) looks awkward as well
			forceLoading( const_cast<SoundSet *>( soundSet ) );
			if( soundSet->hasFailedLoading ) {
				return std::nullopt;
			}
			assert( soundSet->isLoaded );
		}
		const unsigned numBuffers = soundSet->numBuffers;
		assert( numBuffers > 0 );
		const unsigned index = choseIndex( numBuffers, soundSet->lastChosenBufferIndex, &m_rng );
		ALuint chosenBuffer;
		if( preferStereo ) {
			chosenBuffer = soundSet->buffers[index]->stereoBuffer;
			// Looks like it is originally a mono sound
			if( !chosenBuffer ) {
				chosenBuffer = soundSet->buffers[index]->buffer;
			}
		} else {
			chosenBuffer = soundSet->buffers[index]->buffer;
		}
		assert( alIsBuffer( chosenBuffer ) );
		soundSet->lastChosenBufferIndex = (uint8_t)index;
		return std::make_pair( chosenBuffer, index );
	}
	return std::nullopt;
}

// TODO: We don't need to disallow using previously chosen pitch
// if we guarantee choosing a different buffer (if the number of buffers > 2)
auto SoundSetCache::getPitchForPlayback( const SoundSet *soundSet ) -> float {
	if( soundSet ) {
		if( const auto numPitchVariations = soundSet->numPitchVariations; numPitchVariations > 0 ) {
			const auto index  = choseIndex( numPitchVariations, soundSet->lastChosenPitchIndex, &m_rng );
			const float pitch = soundSet->pitchVariations[index];
			assert( pitch > 0.0f );
			soundSet->lastChosenPitchIndex = (uint8_t)index;
			return pitch;
		}
	}
	return 1.0f;
}


