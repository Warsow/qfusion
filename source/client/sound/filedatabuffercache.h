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

#ifndef WSW_4e251764_23a7_49ae_b633_5fd425ba1b35_H
#define WSW_4e251764_23a7_49ae_b633_5fd425ba1b35_H

#include <common/types/stringview.h>
#include <common/types/staticstring.h>
#include <common/helpers/freelistallocator.h>
#include <common/types/podbuffer.h>
#include <common/facilities/q_comref.h>

struct snd_info_s;

struct FileDataBuffer {
	unsigned buffer { 0 };
	unsigned stereoBuffer { 0 };
	unsigned durationMillis { 0 };
};

class FileDataBufferCache {
public:
	FileDataBufferCache();
	~FileDataBufferCache();

	[[nodiscard]]
	auto get( const wsw::HashedStringView &filePath ) -> const FileDataBuffer *;

	[[nodiscard]]
	auto get( const wsw::StringView &filePath ) -> const FileDataBuffer * {
		return get( wsw::HashedStringView( filePath ) );
	}

	void release( const FileDataBuffer *fileDataBuffer );
private:
	struct Entry {
		enum Links { ListLinks, BinLinks };
		Entry *prev[2] { nullptr, nullptr };
		Entry *next[2] { nullptr, nullptr };
		wsw::StaticString<MAX_QPATH> filePath;
		FileDataBuffer fileDataBuffer;
		unsigned binIndex { 0 };
		unsigned refCount { 1 };
	};

	void unlinkAndFree( Entry *entry );

	[[nodiscard]]
	auto loadFileDataBuffer( const wsw::StringView &filePath ) -> std::optional<FileDataBuffer>;
	[[nodiscard]]
	auto createBufferFromData( const wsw::StringView &logFilePath, const snd_info_s &info, const void *data ) -> unsigned;

	void destroyFileDataBuffer( FileDataBuffer *buffer );

	static constexpr unsigned kMaxEntries { 1024 };
	static constexpr unsigned kNumHashBins { 97 };

	Entry *m_allEntriesHead { nullptr };
	Entry *m_hashBins[kNumHashBins] {};

	wsw::HeapBasedFreelistAllocator m_entryAllocator;

	PodBuffer<uint8_t> m_fileDataBuffer;
	PodBuffer<uint8_t> m_resamplingBuffer;
};

#endif
