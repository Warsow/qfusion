#ifndef WSW_27409e02_9398_49e5_989a_f64535e0ec7a_H
#define WSW_27409e02_9398_49e5_989a_f64535e0ec7a_H

#include <common/types/stringview.h>
#include <type_traits>

namespace wsw {

class StringSplitter {
	wsw::StringView m_data;
	size_t m_tokenNum { 0 };

	template <typename Separator>
	[[nodiscard]]
	static auto lenOf( Separator separator ) -> unsigned {
		if constexpr( std::is_same_v<wsw::StringView, std::remove_cvref_t<Separator>> ) {
			// Make sure we are going to avdance
			assert( separator.length() > 0 );
			return separator.length();
		}
		return 1;
	}

	template <typename Separator>
	[[nodiscard]]
	auto getNext_( Separator separator, unsigned options ) -> std::optional<wsw::StringView> {
		for(;; ) {
			if( auto maybeIndex = m_data.indexOf( separator ) ) {
				auto index = *maybeIndex;
				// Disallow empty tokens
				if( index || ( options & AllowEmptyTokens ) ) {
					auto result = m_data.take( index );
					m_data = m_data.drop( index + lenOf( separator ) );
					m_tokenNum++;
					return result;
				}
				m_data = m_data.drop( lenOf( separator ) );
			} else if( !m_data.empty() ) {
				auto result = m_data;
				// Preserve the underlying pointer (may be useful in future)
				m_data = m_data.drop( m_data.length() );
				m_tokenNum++;
				assert( m_data.empty() );
				return result;
			} else {
				return std::nullopt;
			}
		}
	}

	template <typename Separator>
	[[nodiscard]]
	auto getNextWithNum_( Separator separator, unsigned options ) -> std::optional<std::pair<wsw::StringView, size_t>> {
		for(;; ) {
			if( auto maybeIndex = m_data.indexOf( separator ) ) {
				auto index = *maybeIndex;
				if( index || ( options & AllowEmptyTokens ) ) {
					auto view = m_data.take( index );
					m_data = m_data.drop( index + lenOf( separator ) );
					auto num = m_tokenNum;
					m_tokenNum++;
					return std::make_pair( view, num );
				}
				m_data = m_data.drop( lenOf( separator ) );
			} else if( !m_data.empty() ) {
				auto view = m_data;
				m_data = m_data.drop( m_data.length() );
				auto num = m_tokenNum;
				m_tokenNum++;
				assert( m_data.empty() );
				return std::make_pair( view, num );
			} else {
				return std::nullopt;
			}
		}
	}
public:
	explicit StringSplitter( wsw::StringView data )
		: m_data( data ) {}

	[[nodiscard]]
	auto getLastTokenNum() const -> size_t {
		assert( m_tokenNum );
		return m_tokenNum - 1;
	}

	enum : unsigned { AllowEmptyTokens = 0x1 };

	[[nodiscard]]
	auto getNext( char separator = ' ', unsigned options = 0 ) -> std::optional<wsw::StringView> {
		return getNext_( separator, options );
	}

	[[nodiscard]]
	auto getNext( const wsw::CharLookup &separatorChars, unsigned options = 0 ) -> std::optional<wsw::StringView> {
		return getNext_( separatorChars, options );
	}

	[[nodiscard]]
	auto getNext( const wsw::StringView &separatorString, unsigned options = 0 ) -> std::optional<wsw::StringView> {
		assert( !separatorString.empty() );
		return getNext_( separatorString, options );
	}

	[[nodiscard]]
	auto getNextWithNum( char separator = ' ', unsigned options = 0 ) -> std::optional<std::pair<wsw::StringView, size_t>> {
		return getNextWithNum_( separator, options );
	}

	[[nodiscard]]
	auto getNextWithNum( const wsw::CharLookup &separatorChars, unsigned options = 0 ) -> std::optional<std::pair<wsw::StringView, size_t>> {
		return getNextWithNum_( separatorChars, options );
	}

	[[nodiscard]]
	auto getNextWithNum( const wsw::StringView &separatorString, unsigned options = 0 ) -> std::optional<std::pair<wsw::StringView, size_t>> {
		assert( !separatorString.empty() );
		return getNextWithNum_( separatorString, options );
	}
};

}

#endif
