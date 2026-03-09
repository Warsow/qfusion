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

#include "filedatabuffercache.h"

#include "snd_local.h"
#include <common/helpers/links.h>
#include <common/helpers/scopeexitaction.h>

FileDataBufferCache::FileDataBufferCache() : m_entryAllocator( sizeof( Entry ), kMaxEntries ) {}

FileDataBufferCache::~FileDataBufferCache() {
	for( Entry *entry = m_allEntriesHead, *next; entry; entry = next ) { next = entry->next[Entry::ListLinks];
		sWarning() << "An entry for" << entry->filePath << "has not been explicitly released";
		unlinkAndFree( entry );
	}
}

void FileDataBufferCache::unlinkAndFree( Entry *entry ) {
	wsw::unlink( entry, &m_allEntriesHead, Entry::ListLinks );
	wsw::unlink( entry, &m_hashBins[entry->binIndex], Entry::BinLinks );
	destroyFileDataBuffer( &entry->fileDataBuffer );
	entry->~Entry();
	m_entryAllocator.free( entry );
}

auto FileDataBufferCache::get( const wsw::HashedStringView &filePath ) -> const FileDataBuffer * {
	if( filePath.length() > MAX_QPATH ) [[unlikely]] {
		sWarning() << "The given file path length exceeds limitations";
		return nullptr;
	}

	const unsigned binIndex = filePath.getHash() % kNumHashBins;
	for( Entry *entry = m_hashBins[binIndex]; entry; entry = entry->next[Entry::BinLinks] ) {
		if( entry->filePath.asView().equalsIgnoreCase( filePath ) ) {
			entry->refCount++;
			return &entry->fileDataBuffer;
		}
	}

	if( !m_entryAllocator.isFull() ) {
		// Note: We have decided to avoid creation of entries which indicate loading failure
		if( std::optional<FileDataBuffer> maybeBuffer = loadFileDataBuffer( filePath ) ) {
			static_assert( std::is_trivially_copyable_v<FileDataBuffer> );
			auto *newEntry     = new( m_entryAllocator.allocOrNull() )Entry;
			newEntry->binIndex = binIndex;
			newEntry->refCount = 1;
			newEntry->filePath.assign( filePath );
			wsw::link( newEntry, &m_allEntriesHead, Entry::ListLinks );
			wsw::link( newEntry, &m_hashBins[binIndex], Entry::BinLinks );
			newEntry->fileDataBuffer = *maybeBuffer;
			return &newEntry->fileDataBuffer;
		}
	} else {
		sWarning() << "Failed to allocate a cache entry";
	}

	return nullptr;
}

void FileDataBufferCache::release( const FileDataBuffer *fileDataBuffer ) {
	if( fileDataBuffer ) {
		assert( m_entryAllocator.mayOwn( fileDataBuffer ) );
		// TODO: containerof
		auto *entry = (Entry *)( (uintptr_t)(const uint8_t *)fileDataBuffer - offsetof( Entry, fileDataBuffer ) );
		assert( m_entryAllocator.mayOwn( entry ) );
		assert( entry->refCount > 0 );
		entry->refCount--;
		if( !entry->refCount ) {
			unlinkAndFree( entry );
		}
	}
}

// TODO: Do we really need bias?
template <typename T>
static void runResamplingLoop( const T *__restrict in, T *__restrict out, size_t numSteps, int bias ) {
	size_t stepNum = 0;
	if( bias == 0 ) {
		// Mix channels
		do {
			*out = (short)( ( in[0] + in[1] ) / 2 );
			out++;
			in += 2;
		} while( ++stepNum < numSteps );
	} else {
		const int channelIndex = bias < 0 ? 0 : 1;
		// Copy the channel
		do {
			*out = in[channelIndex];
			out++;
			in += 2;
		} while( ++stepNum < numSteps );
	}
}

auto FileDataBufferCache::loadFileDataBuffer( const wsw::StringView &filePath ) -> std::optional<FileDataBuffer> {
	sDebug() << "Loading buffers for" << filePath;

	ALuint monoBuffer   = 0;
	ALuint stereoBuffer = 0;

	[[maybe_unused]] wsw::ScopeExitAction cleanupAction( [&] {
		alDeleteBuffers( 1, &monoBuffer );
		alDeleteBuffers( 1, &stereoBuffer );
	});

	wsw::PodVector<char> ztFilePath( filePath );
	ztFilePath.append( '\0' );

	snd_info_t fileInfo;
	if( !S_LoadSound( ztFilePath.data(), &m_fileDataBuffer, &fileInfo ) ) {
		// It produces verbose output on its own
		return std::nullopt;
	}

	const void *monoData;
	snd_info_t monoInfo;
	if( fileInfo.numChannels == 1 ) {
		monoData = m_fileDataBuffer.get();
		monoInfo = fileInfo;
	} else {
		const void *stereoData      = m_fileDataBuffer.get();
		const snd_info_t stereoInfo = fileInfo;

		stereoBuffer = createBufferFromData( filePath, stereoInfo, stereoData );
		if( !stereoBuffer ) {
			sError() << "Failed to upload stereo buffer data";
			return std::nullopt;
		}

		// TODO: Check whether info parameters are really handled prpoperly

		const size_t monoSizeInBytes = stereoInfo.samplesPerChannel * stereoInfo.bytesPerSample;
		if( !m_resamplingBuffer.tryReserving( monoSizeInBytes ) ) {
			sError() << "Failed to reserve resampling buffer data";
			return std::nullopt;
		}

		monoData = m_resamplingBuffer.get();
		if( stereoInfo.bytesPerSample == 2 ) {
			runResamplingLoop<int16_t>( (int16_t *)stereoData, (int16_t *)monoData,
										stereoInfo.samplesPerChannel, s_stereo2mono->integer );
		} else {
			runResamplingLoop<int8_t>( (int8_t *)stereoData, (int8_t *)monoData,
									   stereoInfo.samplesPerChannel, s_stereo2mono->integer );
		}

		monoInfo             = stereoInfo;
		monoInfo.numChannels = 1;
		monoInfo.sizeInBytes = (int)monoSizeInBytes;
	}

	monoBuffer = createBufferFromData( filePath, monoInfo, monoData );
	if( !monoBuffer ) {
		if( fileInfo.numChannels == 1 ) {
			sError() << "Failed to upload the buffer data";
		} else {
			sError() << "Failed to upload the mono buffer data";
		}
		return std::nullopt;
	}

	cleanupAction.cancel();

	assert( alIsBuffer( monoBuffer ) );
	assert( !stereoBuffer || alIsBuffer( stereoBuffer ) );
	return FileDataBuffer {
		.buffer         = monoBuffer,
		.stereoBuffer   = stereoBuffer,
		.durationMillis = (unsigned)( ( 1000 * (int64_t)fileInfo.samplesPerChannel ) / fileInfo.sampleRate ),
	};
}

auto FileDataBufferCache::createBufferFromData( const wsw::StringView &filePath, const snd_info_t &info, const void *data ) -> ALuint {
	ALenum error = alGetError();
	if( error != AL_NO_ERROR ) {
		sWarning() << "Had an error" << error << "prior to loading of data for" << filePath;
	}

	ALuint buffer = 0;
	alGenBuffers( 1, &buffer );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		sWarning() << "Failed to create buffer:" << wsw::StringView( S_ErrorMessage( error ) ) << "while loading data for" << filePath;
		return 0;
	}

	const ALenum format = S_SoundFormat( info.bytesPerSample, info.numChannels );
	alBufferData( buffer, format, data, info.sizeInBytes, info.sampleRate );
	if( ( error = alGetError() ) != AL_NO_ERROR ) {
		sWarning() << "Failed to set buffer data" << wsw::StringView( S_ErrorMessage( error ) ) << "while loading data for" << filePath;
		alDeleteBuffers( 1, &buffer );
		return 0;
	}

	// Note: We have decided to drop forceful unloading of other buffers in case of AL_OUT_OF_MEMORY as its utility is questionable

	return buffer;
}

void FileDataBufferCache::destroyFileDataBuffer( FileDataBuffer *buffer ) {
	if( buffer ) {
		ALuint buffers[2];
		ALsizei numBuffers = 0;
		if( buffer->buffer ) {
			buffers[numBuffers++] = buffer->buffer;
		}
		if( buffer->stereoBuffer ) {
			buffers[numBuffers++] = buffer->stereoBuffer;
		}
		if( numBuffers ) {
			alDeleteBuffers( numBuffers, buffers );
		}
	}
}