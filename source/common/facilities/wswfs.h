#ifndef WSW_78fa5efa_4f6f_4fac_bfb6_a9168794ae5c_H
#define WSW_78fa5efa_4f6f_4fac_bfb6_a9168794ae5c_H

#include <cstdint>
#include <optional>

#include <common/helpers/q_arch.h>
#include <common/facilities/q_shared.h>

#include <common/types/function.h>
#include <common/types/stringview.h>
#include <common/types/staticstring.h>

namespace wsw::fs {

class IOHandle {
protected:
	int m_underlying;
	bool m_hadError { false };
	explicit IOHandle( int underlying )
		: m_underlying( underlying ) {}
	static void destroyHandle( int );
public:
	IOHandle( const IOHandle &that ) = delete;
	IOHandle &operator=( const IOHandle &that ) = delete;
	IOHandle( IOHandle &&that ) = delete;
	IOHandle &operator=( IOHandle &that ) = delete;

	[[nodiscard]]
	bool isAtEof() const;
	[[nodiscard]]
	bool rewind();

	virtual ~IOHandle();
};

class BufferedReader;

enum CacheUsage : unsigned {
	SkipCacheFS = 0x0,
	UseCacheFS = 0x1
};

class ReadHandle : public virtual IOHandle {
	friend auto openAsReadHandle( const wsw::StringView &, CacheUsage ) -> std::optional<ReadHandle>;
	friend auto openAsBufferedReader( const wsw::StringView &, CacheUsage ) -> std::optional<BufferedReader>;
protected:
	int m_initialSize;
	explicit ReadHandle( int underlying, int initialSize )
		: IOHandle( underlying ), m_initialSize( initialSize ) {}
public:
	ReadHandle( ReadHandle &&that ) noexcept
		: IOHandle( that.m_underlying ), m_initialSize( that.m_initialSize ) {
		that.m_underlying = 0;
	}

	[[maybe_unused]]
	auto operator=( ReadHandle &&that ) noexcept -> ReadHandle & {
		if( m_underlying ) {
			destroyHandle( m_underlying );
		}
		m_underlying = that.m_underlying;
		m_initialSize = that.m_initialSize;
		that.m_underlying = 0;
		return *this;
	}

	[[nodiscard]]
	auto getInitialFileSize() const -> size_t { return (size_t)m_initialSize; }

	template <std::enable_if_t<!std::is_same_v<char, uint8_t>, int> = 0>
	[[nodiscard]]
	auto read( char *buffer, size_t length ) -> std::optional<size_t> {
		return read( (uint8_t *)buffer, length );
	}

	[[nodiscard]]
	bool readExact( uint8_t *buffer, size_t length ) {
		return read( buffer, length ) == length;
	}

	template <std::enable_if_t<!std::is_same_v<char, uint8_t>, int> = 0>
	[[nodiscard]]
	bool readExact( char *buffer, size_t length ) {
		return read( buffer, length ) == length;
	}

	[[nodiscard]]
	virtual auto read( uint8_t *buffer, size_t length ) -> std::optional<size_t>;
	[[nodiscard]]
	virtual auto asConstMappedData() const -> std::optional<const uint8_t *>;
};

class WriteHandle : public virtual IOHandle {
	friend auto openAsWriteHandle( const wsw::StringView &, CacheUsage ) -> std::optional<WriteHandle>;
protected:
	explicit WriteHandle( int underlying )
		: IOHandle( underlying ) {}
public:
	WriteHandle( WriteHandle &&that ) noexcept
		: IOHandle( that.m_underlying ) {
		that.m_underlying = 0;
	}

	[[maybe_unused]]
	auto operator=( WriteHandle &&that ) noexcept -> WriteHandle & {
		if( m_underlying ) {
			destroyHandle( m_underlying );
		}
		m_underlying = that.m_underlying;
		that.m_underlying = 0;
		return *this;
	}

	template <std::enable_if_t<!std::is_same_v<char, uint8_t>, int> = 0>
	[[nodiscard]]
	bool write( const char *buffer, size_t length ) {
		return write( (const uint8_t *)buffer, length );
	}

	[[nodiscard]]
	virtual bool write( const uint8_t *buffer, size_t length );
	[[nodiscard]]
	virtual auto asMappedData() -> std::optional<uint8_t *>;
};

class ReadWriteHandle : public ReadHandle, public WriteHandle {
	friend auto openAsReadWriteHandle( const wsw::StringView &, CacheUsage ) -> std::optional<ReadWriteHandle>;
protected:
	explicit ReadWriteHandle( int underlying, int underlyingSize )
		: IOHandle( underlying )
		, ReadHandle( underlying, underlyingSize )
		, WriteHandle( underlying ) {}
public:
	ReadWriteHandle( ReadWriteHandle &&that ) noexcept
		: IOHandle( that.m_underlying )
		, ReadHandle( that.m_underlying, that.m_initialSize )
		, WriteHandle( that.m_underlying ) {
		that.m_underlying = 0;
	}

	[[maybe_unused]]
	auto operator=( ReadWriteHandle &&that ) noexcept -> ReadWriteHandle & {
		if( m_underlying ) {
			destroyHandle( m_underlying );
		}
		m_underlying = that.m_underlying;
		m_initialSize = that.m_initialSize;
		that.m_underlying = 0;
		return *this;
	}
};

[[nodiscard]]
auto openAsReadHandle( const wsw::StringView &path, CacheUsage cacheUsage = SkipCacheFS )
	-> std::optional<ReadHandle>;

[[nodiscard]]
auto openAsWriteHandle( const wsw::StringView &path, CacheUsage cacheUsage = SkipCacheFS )
	-> std::optional<WriteHandle>;

[[nodiscard]]
auto openAsReadWriteHandle( const wsw::StringView &path, CacheUsage cacheUsage = SkipCacheFS )
	-> std::optional<ReadWriteHandle>;

class BufferedReader {
	ReadHandle m_handle;

	uint8_t m_buffer[1024];
	unsigned m_currPos { 0 };
	unsigned m_limitPos { 0 };
	bool m_hadError { false };
public:
	explicit BufferedReader( ReadHandle &&handle ) noexcept
		: m_handle( std::move( handle ) ) {
		// This is not mandatory as the instance buffer is not assumed to be a string but eliminates a lint warning
		m_buffer[0] = 0;
	}

	[[nodiscard]]
	bool isAtEof() const;

	[[nodiscard]]
	auto getInitialFileSize() -> size_t {
		return m_handle.getInitialFileSize();
	}

	template <std::enable_if_t<!std::is_same_v<char, uint8_t>, int> = 0>
	auto read( char *buffer, size_t length ) -> std::optional<size_t> {
		return read( (uint8_t *)buffer, length );
	}

	[[nodiscard]]
	bool readExact( uint8_t *buffer, size_t length ) {
		return read( buffer, length ) == length;
	}

	template <std::enable_if_t<!std::is_same_v<char, uint8_t>, int> = 0>
	[[nodiscard]]
	bool readExact( char *buffer, size_t length ) {
		return read( buffer, length ) == length;
	}

	[[nodiscard]]
	auto read( uint8_t *buffer, size_t length ) -> std::optional<size_t>;

	struct LineReadResult {
		const unsigned bytesRead;
		const bool wasIncomplete;
	};

	[[nodiscard]]
	auto readToNewline( char *buffer, size_t bufferSize ) -> std::optional<LineReadResult>;
};

[[nodiscard]]
auto openAsBufferedReader( const wsw::StringView &path, CacheUsage cacheUsage = SkipCacheFS )
	-> std::optional<BufferedReader>;

/**
 * Instances of this class are supposed to hold the FS search result state
 * (they do not currently do that for ABI reasons) that could be expensive
 * to recreate and are supposed to be reusable.
 */
class SearchResultHolder {
	int m_totalNumFiles { 0 };
	unsigned m_invocationNum { 0 };
	const char *m_ptr { nullptr };
	int m_lastSearchOff { 0 };
	int m_lastSearchSize { 0 };
	int m_lastRetrievalNum { 0 };

	char m_buffer[1024];
	static_assert( MAX_QPATH * 2 < sizeof( m_buffer ), "Should be capable of storing any path" );

	wsw::StaticString<MAX_QPATH> m_dir;
	wsw::StaticString<MAX_QPATH> m_ext;

	[[nodiscard]]
	auto fetchNextInBuffer( int num ) -> wsw::StringView;

	[[nodiscard]]
	auto getFileForNum( int num ) -> wsw::StringView;
public:
	class const_iterator {
		friend class SearchResultHolder;

		SearchResultHolder *const m_parent;
		const unsigned m_invocationNum;
		int m_fileNum;

		const_iterator( SearchResultHolder *parent, int fileNum )
			: m_parent( parent ), m_invocationNum( parent->m_invocationNum ), m_fileNum( fileNum ) {}

	public:
		auto operator++() -> const_iterator & {
			assert( m_parent->m_invocationNum == m_invocationNum );
			m_fileNum++;
			return *this;
		}

		[[nodiscard]]
		bool operator!=( const const_iterator &that ) const {
			assert( m_parent->m_invocationNum == m_invocationNum );
			assert( m_parent == that.m_parent );
			assert( m_invocationNum == that.m_invocationNum );
			return m_fileNum != that.m_fileNum;
		}

		auto operator*() const -> wsw::StringView {
			assert( m_parent->m_invocationNum == m_invocationNum );
			return m_parent->getFileForNum( m_fileNum );
		}
	};

	class CallResult {
		friend class SearchResultHolder;

		const_iterator m_begin, m_end;
		const unsigned m_numFiles;

		CallResult( const_iterator begin_, const_iterator end_, unsigned numFiles_ )
			: m_begin( begin_ ), m_end( end_ ), m_numFiles( numFiles_ ) {}
	public:
		[[nodiscard]]
		auto getNumFiles() const -> unsigned { return m_numFiles; }
		[[nodiscard]]
		auto begin() const -> const_iterator { return m_begin; }
		[[nodiscard]]
		auto end() const -> const_iterator { return m_end; }
	};

	[[nodiscard]]
	auto findDirFiles( const wsw::StringView &dir, const wsw::StringView &ext ) -> std::optional<CallResult>;
};

using WalkDirVisitor = wsw::Function<void( const wsw::StringView &dir, const wsw::StringView &entryName )>;

enum WalkErrorPolicy : uint8_t {
	InterruptOnError,
	ContinueWalking
};

struct WalkDirOptions {
	WalkErrorPolicy errorPolicy { ContinueWalking };
	uint8_t maxDepth { 255 };
};

[[nodiscard]]
bool walkDir( const wsw::StringView &dir, const WalkDirVisitor &visitor, const WalkDirOptions &options );

[[nodiscard]]
auto getExtension( const wsw::StringView &fileName ) -> std::optional<wsw::StringView>;

[[nodiscard]]
auto stripExtension( const wsw::StringView &fileName ) -> std::optional<wsw::StringView>;

[[nodiscard]]
auto splitAtExtension( const wsw::StringView &fileName ) -> std::optional<std::pair<wsw::StringView, wsw::StringView>>;

/**
 * Given a file name, tries to find a first matching VFS entry with one of specified extensions.
 * @param name a file name. Could have an arbitrary extension, or no extension at all.
 * @param begin a beginning of the supplied extensions array
 * @param end an end of the supplied extensions array
 * @todo supply a range instead of two iterators
 * @return a (maybe) pair of a basename (without extension) and an extension that produces a first VFS match.
 */
[[nodiscard]]
auto findFirstExtension( const wsw::StringView &name, const wsw::StringView *begin, const wsw::StringView *end ) ->
	std::optional<std::pair<wsw::StringView, wsw::StringView>>;

}

#endif
