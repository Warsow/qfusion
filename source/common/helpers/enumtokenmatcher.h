#ifndef WSW_d327d889_480b_4807_b7ac_ba3c44329664_H
#define WSW_d327d889_480b_4807_b7ac_ba3c44329664_H

#include <common/types/stringview.h>
#include <common/types/podvector.h>
#include <array>

namespace wsw {

// Allows an efficient matching of short string tokens against string representations of enum values.
// Registered names are assumed to have an externally managed lifetime, preferrably &'static one.
template <typename Enum, typename Derived>
class EnumTokenMatcher {
public:
	EnumTokenMatcher() {
		for( int16_t &head: m_smallLengthHeads ) {
			head = -1;
		}
	}

	EnumTokenMatcher( std::initializer_list<std::pair<wsw::StringView, Enum>> namesAndValues ) : EnumTokenMatcher() {
		m_patterns.reserve( namesAndValues.size() );
		for( const auto &[name, value] : namesAndValues ) {
			add( name, value );
		}
		m_patterns.shrink_to_fit();
	}

	[[nodiscard]]
	auto match( const wsw::StringView &token ) const -> std::optional<Enum> {
		const size_t length = token.length();
		if( length < m_minLengthSoFar || length > m_maxLengthSoFar ) [[unlikely]] {
			return std::nullopt;
		}
		if( length - 1 < std::size( m_smallLengthHeads ) ) [[likely]] {
			return matchInList( m_smallLengthHeads[length - 1], token );
		}
		return matchInList( m_largeLengthHead, token );
	}

	// Sometimes iteration over registered names and values could be useful for non-performance-demanding tasks
	// as the matcher itself could serve as a fairly good documentation of enum values.

	struct const_iterator {
		template <typename E, typename D> friend class EnumTokenMatcher;
	public:
		[[nodiscard]]
		bool operator!=( const const_iterator &that ) {
			assert( m_parent == that.m_parent );
			return m_index != that.m_index;
		}
		[[maybe_unused]]
		auto operator++() -> std::pair<wsw::StringView, Enum> {
			m_index++;
			return *( *this );
		}
		[[maybe_unused]]
		auto operator++( int ) -> std::pair<wsw::StringView, Enum> {
			std::pair<wsw::StringView, Enum> result = *( *this );
			++( *this );
			return result;
		}
		[[nodiscard]]
		auto operator*() const -> std::pair<wsw::StringView, Enum> {
			const TokenPattern &pattern = m_parent->m_patterns[m_index];
			return { pattern.name, pattern.value };
		}
	private:
		const_iterator( const EnumTokenMatcher<Enum, Derived> *parent, size_t index ) : m_parent( parent ), m_index( index ) {}
		const EnumTokenMatcher<Enum, Derived> *m_parent { nullptr };
		size_t m_index { 0 };
	};

	[[nodiscard]]
	auto begin() const -> const_iterator { return const_iterator { this, 0 }; }
	[[nodiscard]]
	auto end() const -> const_iterator { return const_iterator { this, m_patterns.size() }; }

	// We have to use the static getter (even if it's undesired) as a workaround
	// https://developercommunity.visualstudio.com/t/c-class-with-inline-static-member-of-the-same-type/973593
	static Derived &instance() {
		// Note: function-static variables incur performance penalty due to thread-safe initialization guarantees
		static Derived s_instance;
		return s_instance;
	}

	// DeclaredConfigVar - compatibility stuff
	template <typename DesiredType>
	[[nodiscard]]
	static auto matchFn( const void *obj, const wsw::StringView &token ) -> std::optional<DesiredType> {
		const std::optional<Enum> matched = ( ( const EnumTokenMatcher<Enum, Derived> *)obj )->match( token );
		return matched ? std::optional( (DesiredType)*matched ) : std::nullopt;
	}

	template <typename DesiredType>
	// TODO: A fixed vector/fixed heap-alloc array is sufficient
	[[nodiscard]]
	auto getEnumValues() -> wsw::PodVector<DesiredType> {
		static_assert( sizeof( DesiredType ) >= sizeof( Enum ) );
		wsw::PodVector<DesiredType> result;
		// TODO: Just specify the initial capacity as an argument
		result.reserve( m_patterns.size() );
		for( const TokenPattern &pattern: m_patterns ) {
			result.push_back( (DesiredType)pattern.value );
		}
		return result;
	}
protected:
	struct TokenPattern {
		wsw::StringView name;
		int16_t ownIndex { -1 };
		int16_t nextIndexInList { -1 };
		Enum value;

		TokenPattern( const wsw::StringView &name, Enum value ): name( name ), value( value ) {}
		[[nodiscard]]
		bool match( const wsw::StringView &token ) const { return name.equalsIgnoreCase( token ); }
	};

	void add( const wsw::StringView &name, Enum value ) {
		assert( name.length() && name.length() <= std::numeric_limits<unsigned>::max() );
		assert( m_patterns.size() < std::numeric_limits<int16_t>::max() );
		const auto newPatternIndex  = (int16_t)m_patterns.size();
		const auto newPatternLength = (uint16_t)name.length();

		// Figure out what bin should be used for this name
		int16_t *headIndex = &m_largeLengthHead;
		if( newPatternLength - 1 < (int16_t)std::size( m_smallLengthHeads ) ) {
			headIndex = &m_smallLengthHeads[newPatternLength - 1];
		}

		// Track bounds
		m_minLengthSoFar = wsw::min( newPatternLength, m_minLengthSoFar );
		m_maxLengthSoFar = wsw::max( newPatternLength, m_maxLengthSoFar );

		m_patterns.emplace_back( TokenPattern( name, value ) );
		// Link the newly created pattern to the head of the list
		m_patterns.back().ownIndex        = newPatternIndex;
		m_patterns.back().nextIndexInList = *headIndex >= 0 ? m_patterns[*headIndex].ownIndex : -1;
		*headIndex                        = newPatternIndex;
	}

	[[nodiscard]]
	auto matchInList( int headIndex, const wsw::StringView &token ) const -> std::optional<Enum> {
		for( int index = headIndex; index >= 0; ) {
			const TokenPattern &pattern = m_patterns[index];
			if( pattern.match( token ) ) {
				return pattern.value;
			}
			index = pattern.nextIndexInList;
		}
		return std::nullopt;
	}

	wsw::PodVector<TokenPattern> m_patterns;
	// We can't use pointers due to possible relocations of m_patterns data
	int16_t m_smallLengthHeads[15];
	int16_t m_largeLengthHead { -1 };
	uint16_t m_minLengthSoFar { std::numeric_limits<uint16_t>::max() };
	uint16_t m_maxLengthSoFar { 0 };
};

}

#endif
