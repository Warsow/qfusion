#include "wswfs.h"
#include <common/types/staticstring.h>
#include <common/helpers/exceptions.h>
#include <common/facilities/fscompat.h>

#include <array>

namespace wsw::fs {

void IOHandle::destroyHandle( int underlying ) {
	assert( underlying );
	FS_FCloseFile( underlying );
}

IOHandle::~IOHandle() {
	if( m_underlying ) {
		FS_FCloseFile( m_underlying );
	}
}

bool IOHandle::isAtEof() const {
	return m_hadError || FS_Eof( m_underlying );
}

bool IOHandle::rewind() {
	if( !m_underlying ) {
		wsw::failWithLogicError( "Using a moved object" );
	}
	// TODO: It should reset errors of underlying objects
	m_hadError = ( FS_Seek( m_underlying, 0, FS_SEEK_SET ) != 0 );
	return !m_hadError;
}

auto ReadHandle::read( uint8_t *buffer, size_t bufferSize ) -> std::optional<size_t> {
	if( m_hadError ) {
		return std::nullopt;
	}
	if( !m_underlying ) {
		wsw::failWithLogicError( "Using a moved object" );
	}
	// TODO: Does it check errno?
	if( int bytesRead = FS_Read( buffer, bufferSize, m_underlying ); bytesRead >= 0 ) {
		return (size_t)bytesRead;
	}
	m_hadError = true;
	return std::nullopt;
}

auto ReadHandle::asConstMappedData() const -> std::optional<const uint8_t *> {
	// Currently not implemented
	return std::nullopt;
}

bool WriteHandle::write( const uint8_t *buffer, size_t length ) {
	if( m_hadError ) {
		return false;
	}
	if( !m_underlying ) {
		wsw::failWithLogicError( "Using a moved object" );
	}
	// TODO: Does it check errno?
	if( int res = FS_Write( buffer, length, m_underlying ); res >= 0 && ( (size_t)res == length ) ) {
		return true;
	}
	m_hadError = true;
	return false;
}

auto WriteHandle::asMappedData() -> std::optional<uint8_t *> {
	// Currently not implemented
	return std::nullopt;
}

[[nodiscard]]
static auto open( const wsw::StringView &path, int rawMode, CacheUsage cacheUsage ) -> std::pair<int, int> {
	int num = 0, size;
	int mode = rawMode;
	if( cacheUsage & CacheUsage::UseCacheFS ) {
		mode |= FS_CACHE;
	}
	if( path.isZeroTerminated() ) {
		size = FS_FOpenFile( path.data(), &num, mode );
	} else {
		wsw::StaticString<MAX_QPATH + 1> tmp( path );
		size = FS_FOpenFile( tmp.data(), &num, mode );
	}
	return std::make_pair( num, size );
}

auto openAsReadHandle( const wsw::StringView &path, CacheUsage cacheUsage ) -> std::optional<ReadHandle> {
	if( auto [num, size] = open( path, FS_READ, cacheUsage ); size >= 0 ) {
		return ReadHandle( num, size );
	}
	return std::nullopt;
}

auto openAsWriteHandle( const wsw::StringView &path, CacheUsage cacheUsage ) -> std::optional<WriteHandle> {
	if( auto [num, size] = open( path, FS_WRITE, cacheUsage ); size >= 0 ) {
		return WriteHandle( num );
	}
	return std::nullopt;
}

auto openAsReadWriteHandle( const wsw::StringView &path, CacheUsage cacheUsage ) -> std::optional<ReadWriteHandle> {
	if( auto [num, size] = open( path, FS_READ | FS_WRITE, cacheUsage ); size >= 0 ) {
		return ReadWriteHandle( num, size );
	}
	return std::nullopt;
}

bool BufferedReader::isAtEof() const {
	if( m_hadError ) {
		return true;
	}
	if( m_currPos < m_limitPos ) {
		return false;
	}
	return m_handle.isAtEof();
}

auto BufferedReader::read( uint8_t *buffer, size_t bufferSize ) -> std::optional<size_t> {
	assert( m_limitPos >= m_currPos );

	if( m_hadError ) {
		return std::nullopt;
	}

	// Try draining the buffered data first
	const size_t bytesWereLeft = m_limitPos - m_currPos;
	if( bytesWereLeft ) {
		if( bytesWereLeft >= bufferSize ) {
			std::memcpy( buffer, m_buffer + m_currPos, bufferSize );
			m_currPos += bufferSize;
			return bufferSize;
		}
		std::memcpy( buffer, m_buffer + m_currPos, bytesWereLeft );
		m_currPos += bytesWereLeft;
		buffer += bytesWereLeft;
		bufferSize -= bytesWereLeft;
		assert( m_currPos == m_limitPos );
	}

	if( auto maybeBytesRead = m_handle.read( buffer, bufferSize ) ) {
		return bytesWereLeft + *maybeBytesRead;
	}

	m_hadError = true;
	return std::nullopt;
}

auto BufferedReader::readToNewline( char *buffer, size_t bufferSize ) -> std::optional<LineReadResult> {
	unsigned bytesRead = 0;
	char *p = buffer;
	for(;; ) {
		// Scan the present reader buffer
		const auto maxCharsToScan = wsw::min( (unsigned)bufferSize, m_limitPos - m_currPos );
		for( unsigned i = m_currPos, end = m_currPos + maxCharsToScan; i < end; ++i ) {
			const auto ch = (char)m_buffer[i];
			if( ch == '\n' ) {
				bytesRead += ( i - m_currPos );
				m_currPos = i + 1;
				LineReadResult result { bytesRead, false };
				return result;
			}
			if( ch == '\r' ) {
				bytesRead += ( i - m_currPos );
				m_currPos = i + 1;
				if( i + 1 != maxCharsToScan ) {
					if( m_buffer[i + 1] == '\n' ) {
						m_currPos++;
					}
				}
				LineReadResult result { bytesRead, false };
				return result;
			}
			*p++ = ch;
		}

		// We have not met a newline in the present reader buffer

		bytesRead += maxCharsToScan;
		m_currPos += maxCharsToScan;
		assert( bufferSize >= maxCharsToScan );
		bufferSize -= maxCharsToScan;
		if( !bufferSize ) {
			LineReadResult result { bytesRead, true };
			return result;
		}

		assert( m_currPos == m_limitPos );

		// Fill the reader buffer for the next scan-for-newline iteration

		if( isAtEof() ) {
			LineReadResult result { bytesRead, false };
			return result;
		}

		if( auto maybeBytesRead = m_handle.read( m_buffer, sizeof( m_buffer ) ) ) {
			m_currPos = 0;
			m_limitPos = *maybeBytesRead;
			m_buffer[*maybeBytesRead] = 0;
		} else {
			m_hadError = true;
			return std::nullopt;
		}
	}
}

auto openAsBufferedReader( const wsw::StringView &path, CacheUsage cacheUsage ) -> std::optional<BufferedReader> {
	if( auto [num, size] = open( path, FS_READ, cacheUsage ); size >= 0 ) {
		return BufferedReader( ReadHandle( num, size ) );
	}
	return std::nullopt;
}

auto SearchResultHolder::findDirFiles( const wsw::StringView &dir, const wsw::StringView &ext )
	-> std::optional<CallResult> {
	if( dir.length() >= m_dir.capacity() ) {
		return std::nullopt;
	}
	if( ext.length() >= m_ext.capacity() ) {
		return std::nullopt;
	}

	m_lastSearchOff = 0;
	m_lastSearchSize = 0;
	m_lastRetrievalNum = 0;

	m_invocationNum++;

	m_dir.assign( dir );
	m_ext.assign( ext );

	m_totalNumFiles = FS_GetFileList( m_dir.data(), m_ext.data(), nullptr, 0, 0, 0 );
	assert( m_totalNumFiles >= 0 );

	const_iterator begin( this, 0 );
	const_iterator end( this, m_totalNumFiles );
	CallResult result { begin, end, (unsigned)m_totalNumFiles };
	return result;
}

[[nodiscard]]
auto SearchResultHolder::fetchNextInBuffer( int num ) -> wsw::StringView {
	assert( m_ptr );
	const size_t len = std::strlen( m_ptr );
	wsw::StringView result( m_ptr, len, wsw::StringView::ZeroTerminated );
	m_ptr += len + 1;
	m_lastRetrievalNum = num;
	return result;
}

[[nodiscard]]
auto SearchResultHolder::getFileForNum( int num ) -> wsw::StringView {
	assert( (unsigned)num < (unsigned)m_totalNumFiles );

	// This is currently optimized for the most realistic use case
	// of a sequential iteration + single retrieval on every step.
	// Underlying search facilities should be rewritten anyway.
	if( num >= m_lastSearchOff && num < m_lastSearchOff + m_lastSearchSize ) {
		if( m_lastRetrievalNum + 1 == num ) {
			return fetchNextInBuffer( num );
		}
	}

	m_lastSearchSize = FS_GetFileList( m_dir.data(), m_ext.data(), m_buffer, sizeof( m_buffer ), num, m_totalNumFiles );
	m_lastSearchOff = num;
	m_ptr = m_buffer;

	if( m_lastSearchSize ) {
		return fetchNextInBuffer( num );
	}

	// Should not happen once we start really holding search result values
	wsw::failWithRuntimeError();
}

// TODO: All of this should be supported by underlying APIs
bool walkDir( const wsw::StringView &dir, const WalkDirVisitor &visitor, const WalkDirOptions &options ) {
	wsw::fs::SearchResultHolder holder;
	if( auto maybeCallResult = holder.findDirFiles( dir, wsw::StringView() ) ) {
		for( const wsw::StringView &name: *maybeCallResult ) {
			visitor( dir, name );
		}
	} else {
		if( options.errorPolicy == InterruptOnError ) {
			return false;
		}
	}

	if( options.maxDepth ) {
		// TODO: Should not require another call
		if( auto maybeCallResult = holder.findDirFiles( dir, "/"_asView ) ) {
			for( const wsw::StringView &name: *maybeCallResult ) {
				// TODO: Could it happen?
				// TODO: Should it be a distinct policy case?
				if( name.length() + dir.length() + 1 >= MAX_QPATH ) {
					if( options.errorPolicy == InterruptOnError ) {
						return false;
					}
				} else {
					const WalkDirOptions subdirOptions { options.errorPolicy, (uint8_t)( options.maxDepth - 1 ) };
					wsw::StaticString<MAX_QPATH> subdirPath;
					subdirPath << dir << '/' << name.dropRight( 1 );
					if( !walkDir( subdirPath.asView(), visitor, subdirOptions ) ) {
						if( options.errorPolicy == InterruptOnError ) {
							return false;
						}
					}
				}
			}
		} else {
			if( options.errorPolicy == InterruptOnError ) {
				return false;
			}
		}
	}

	return true;
}

[[nodiscard]]
auto getExtension( const wsw::StringView &fileName ) -> std::optional<wsw::StringView> {
	if( const auto maybeSplitPair = splitAtExtension( fileName ) ) {
		return maybeSplitPair->second;
	}
	return std::nullopt;
}

[[nodiscard]]
auto stripExtension( const wsw::StringView &fileName ) -> std::optional<wsw::StringView> {
	if( const auto maybeSplitPair = splitAtExtension( fileName ) ) {
		return maybeSplitPair->first;
	}
	return std::nullopt;
}

[[nodiscard]]
auto splitAtExtension( const wsw::StringView &fileName ) -> std::optional<std::pair<wsw::StringView, wsw::StringView>> {
	wsw::StringView tmp( fileName );
	unsigned pathPrefixLen = 0;
	if( const auto maybeSlashIndex = fileName.lastIndexOf( '/' ) ) {
		pathPrefixLen = *maybeSlashIndex + 1;
		tmp = fileName.drop( pathPrefixLen );
	}

	if( const auto maybeDotIndex = tmp.lastIndexOf( '.' ) ) {
		const auto dotIndex = *maybeDotIndex;
		if( dotIndex + 1 < tmp.size() ) {
			const auto realDotIndex = dotIndex + pathPrefixLen;
			return fileName.dropMid( realDotIndex, 0 );
		}
	}

	return std::nullopt;
}

[[nodiscard]]
auto findFirstExtension( const wsw::StringView &name, const wsw::StringView *begin, const wsw::StringView *end ) ->
	std::optional<std::pair<wsw::StringView, wsw::StringView>> {
	assert( name.isZeroTerminated() );
	assert( begin <= end );

	// Compatibility conversions, should eventually be gone
	const char *extensions[16];
	if( (size_t)( end - begin ) >= std::size( extensions ) ) {
		return std::nullopt;
	}

	for( const wsw::StringView *ext = begin; ext != end; ++ext ) {
		if( !ext->isZeroTerminated() ) {
			return std::nullopt;
		}
		extensions[ext - begin] = ext->data();
	}

	const char *rawResult = FS_FirstExtension( name.data(), extensions, (int)( end - begin ) );
	if( !rawResult ) {
		return std::nullopt;
	}

	// This is an additional validation for a reverse conversion, should be gone as well
	[[maybe_unused]] bool found = false;
	wsw::StringView rawResultView( rawResult );
	for( const wsw::StringView *ext = begin; ext != end; ++ext ) {
		if( ext->equalsIgnoreCase( rawResultView ) ) {
			found = true;
			break;
		}
	}
	assert( found );

	if( auto maybeBaseName = stripExtension( name ) ) {
		return std::make_pair( *maybeBaseName, rawResultView );
	}

	return std::make_pair( name, rawResultView );
}

}
