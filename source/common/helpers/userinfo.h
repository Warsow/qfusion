#ifndef WSW_a5f35ea3_9940_4a81_bf50_3a00d22f77b5_H
#define WSW_a5f35ea3_9940_4a81_bf50_3a00d22f77b5_H

#include <common/helpers/freelistallocator.h>
#include <common/types/smallassocarray.h>
#include <common/types/stringview.h>

namespace wsw {

class UserInfo {
	static constexpr auto kMaxPairs = 16u;
	MemberBasedFreelistAllocator<MAX_INFO_KEY + MAX_INFO_VALUE + 2, kMaxPairs, 8> m_allocator;
	SmallAssocArray<wsw::HashedStringView, wsw::StringView, kMaxPairs> m_keysAndValues;

	[[nodiscard]]
	bool parse_( const wsw::StringView &input );
public:
	void clear();

	[[nodiscard]]
	bool isEmpty() const { return m_keysAndValues.isEmpty(); }

	[[nodiscard]]
	bool parse( const wsw::StringView &input );

	[[nodiscard]]
	static bool isValidKey( const wsw::StringView &key );
	[[nodiscard]]
	static bool isValidValue( const wsw::StringView &value );

	template <typename Writer, typename AppendArg = wsw::StringView>
	void serialize( Writer *writer ) {
		for( const auto &[key, value] : m_keysAndValues ) {
			writer->append( '\\' );
			writer->append( AppendArg( key.data(), key.size() ) );
			writer->append( '\\' );
			writer->append( AppendArg( value.data(), value.size() ) );
		}
	}

	[[nodiscard]]
	bool set( const wsw::HashedStringView &key, const wsw::StringView &value );

	[[nodiscard]]
	auto get( const wsw::HashedStringView &key ) const -> std::optional<wsw::StringView> {
		if( const auto it = m_keysAndValues.find( key ); it != m_keysAndValues.end() ) {
			return it.value();
		}
		return std::nullopt;
	}

	[[nodiscard]]
	auto getOrThrow( const wsw::HashedStringView &key ) const -> wsw::StringView {
		if( const auto it = m_keysAndValues.find( key ); it != m_keysAndValues.end() ) {
			return it.value();
		}
		wsw::failWithLogicError( "Failed to find a value by key" );
	}

	[[nodiscard]]
	auto getOrEmpty( const wsw::HashedStringView &key ) const -> wsw::StringView {
		if( const auto it = m_keysAndValues.find( key ); it != m_keysAndValues.end() ) {
			return it.value();
		}
		return wsw::StringView();
	}

	// For testing/debugging
	[[nodiscard]]
	bool operator==( const UserInfo &that ) const {
		if( m_keysAndValues.size() == that.m_keysAndValues.size() ) {
			// Pairs aren't ordered, retrieve each
			for( const auto &[key, value] : m_keysAndValues ) {
				if( const auto maybeThatValue = that.get( key ) ) {
					if( maybeThatValue->equals( value ) ) {
						continue;
					}
				}
				return false;
			}
			// Check whether some `that` keys are missing in `this` userinfo
			for( const auto &[thatKey, _] : that.m_keysAndValues ) {
				if( this->get( thatKey ) == std::nullopt ) {
					return false;
				}
			}
			return true;
		}
		return false;
	}
};

}

#endif