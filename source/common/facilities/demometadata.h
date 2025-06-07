#ifndef WSW_250158dd_010e_46bf_a42e_4ec08585607d_H
#define WSW_250158dd_010e_46bf_a42e_4ec08585607d_H

#include <common/types/staticvector.h>
#include <common/types/stringview.h>
#include <common/helpers/exceptions.h>
#include <common/common.h>

namespace wsw {

// These keys are mandatory.
// We could have settled on fixed binary offsets instead of keys but an extensible k/v set is a better approach.
const wsw::StringView kDemoKeyServerName( "ServerName"_asView );
const wsw::StringView kDemoKeyTimestamp( "Timestamp"_asView );
const wsw::StringView kDemoKeyDuration( "Duration"_asView );
const wsw::StringView kDemoKeyMapName( "MapName"_asView );
const wsw::StringView kDemoKeyMapChecksum( "MapChecksum"_asView );
const wsw::StringView kDemoKeyGametype( "Gametype"_asView );

const wsw::StringView kDemoTagSinglePov( "SinglePOV" );
const wsw::StringView kDemoTagMultiPov( "MultiPOV" );

class DemoMetadataWriter {
	char *const m_basePtr;
	unsigned m_writeOff { 0 };
	unsigned m_numPairsWritten { 0 };
	unsigned m_numTagsWritten { 0 };
	bool m_incomplete { false };

	[[nodiscard]]
	auto freeBytesLeft() const -> unsigned { return SNAP_MAX_DEMO_META_DATA_SIZE - m_writeOff; }

	[[nodiscard]]
	bool tryAppendingTag( const wsw::StringView &tag ) {
		assert( m_numPairsWritten );
		if( tag.length() + 1 <= freeBytesLeft() ) {
			tag.copyTo( m_basePtr + m_writeOff, freeBytesLeft() );
			assert( m_basePtr[m_writeOff + tag.length()] == '\0' );
			m_writeOff += tag.length() + 1;
			return true;
		}
		return false;
	}

	[[nodiscard]]
	bool tryAppendingPair( const wsw::StringView &key, const wsw::StringView &value ) {
		assert( !m_numTagsWritten );
		if( key.length() + value.length() + 2 <= freeBytesLeft() ) {
			key.copyTo( m_basePtr + m_writeOff, freeBytesLeft() );
			assert( m_basePtr[m_writeOff + key.length()] == '\0' );
			m_writeOff += key.length() + 1;

			value.copyTo( m_basePtr + m_writeOff, freeBytesLeft() );
			assert( m_basePtr[m_writeOff + value.length()] == '\0' );
			m_writeOff += value.length() + 1;

			return true;
		}
		return false;
	}
public:
	explicit DemoMetadataWriter( char *data ) : m_basePtr( data ) {
		std::memset( data, 0, SNAP_MAX_DEMO_META_DATA_SIZE );
		m_writeOff += 8;
	}

	[[maybe_unused]]
	bool writePair( const wsw::StringView &key, const wsw::StringView &value ) {
		if( !m_incomplete ) {
			if( m_numTagsWritten ) {
				wsw::failWithLogicError( "Attempt to write a key-value pair after tags" );
			}
			if( tryAppendingPair( key, value ) ) {
				m_numPairsWritten++;
				return true;
			}
			m_incomplete = true;
		}
		return false;
	}

	[[maybe_unused]]
	bool writeTag( const wsw::StringView &tag ) {
		if( !m_incomplete ) {
			if( !m_numPairsWritten ) {
				wsw::failWithLogicError( "Attempt to write a tag prior to key-value pairs" );
			}
			if( tryAppendingTag( tag ) ) {
				m_numTagsWritten++;
				return true;
			}
			m_incomplete = true;
		}
		return false;
	}

	[[nodiscard]]
	auto markCurrentResult() const -> std::pair<size_t, bool> {
		uint32_t numPairsToMark = LittleLong( m_numPairsWritten );
		uint32_t numTagsToMark = LittleLong( m_numTagsWritten );
		std::memcpy( m_basePtr + 0, &numPairsToMark, 4 );
		std::memcpy( m_basePtr + 4, &numTagsToMark, 4 );
		return { m_writeOff, !m_incomplete };
	}
};

class DemoMetadataReader {
	const char *const m_basePtr;
	const unsigned m_dataSize;
	unsigned m_readOff { 0 };
	unsigned m_numPairs { 0 };
	unsigned m_numTags { 0 };
	unsigned m_numPairsRead { 0 };
	unsigned m_numTagsRead { 0 };
public:
	DemoMetadataReader( const char *data, unsigned dataSize )
		: m_basePtr( data ), m_dataSize( dataSize ) {
		assert( m_basePtr[dataSize] == '\0' );
		if( dataSize >= 8 ) {
			unsigned numPairs, numTags;
			std::memcpy( &numPairs, data + 0, 4 );
			std::memcpy( &numTags, data + 4, 4 );
			m_numPairs = LittleLong( numPairs );
			m_numTags = LittleLong( numTags );
			m_readOff += 8;
		}
	}

	[[nodiscard]]
	bool hasNextPair() const { return m_numPairsRead < m_numPairs; }

	[[nodiscard]]
	auto readNextPair() -> std::optional<std::pair<wsw::StringView, wsw::StringView>> {
		if( !hasNextPair() ) {
			wsw::failWithLogicError( "No key-value pairs to read" );
		}
		const auto keyLen = std::strlen( m_basePtr + m_readOff );
		if( m_readOff + keyLen + 1 < m_dataSize ) {
			assert( m_basePtr[m_readOff + keyLen] == '\0' );
			const auto valueLen = std::strlen( m_basePtr + m_readOff + keyLen + 1 );
			if( m_readOff + keyLen + valueLen + 2 <= m_dataSize ) {
				assert( m_basePtr[m_readOff + keyLen + valueLen + 1] == '\0' );
				wsw::StringView key( m_basePtr + m_readOff, keyLen, wsw::StringView::ZeroTerminated );
				wsw::StringView value( m_basePtr + m_readOff + keyLen + 1, valueLen, wsw::StringView::ZeroTerminated );
				m_readOff += keyLen + valueLen + 2;
				m_numPairsRead++;
				return std::make_pair( key, value );
			}
		}
		return std::nullopt;
	}

	[[nodiscard]]
	bool hasNextTag() const { return m_numTagsRead < m_numTags; }

	[[nodiscard]]
	auto readNextTag() -> std::optional<wsw::StringView> {
		if( hasNextPair() ) {
			wsw::failWithLogicError( "Reading of key-value pairs is incomplete" );
		}
		if( !hasNextTag() ) {
			wsw::failWithLogicError( "No tags to read" );
		}
		const auto tagLen = std::strlen( m_basePtr + m_readOff );
		if( m_readOff + tagLen < m_dataSize ) {
			assert( m_basePtr[m_readOff + tagLen] == '\0' );
			wsw::StringView tag( m_basePtr + m_readOff, tagLen, wsw::StringView::ZeroTerminated );
			m_readOff += tagLen + 1;
			m_numTagsRead++;
			return tag;
		}
		return std::nullopt;
	}
};

}

#endif