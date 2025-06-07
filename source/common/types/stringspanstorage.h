#ifndef WSW_75afb0b1_2a6b_4ef1_9536_39185e8ca404_H
#define WSW_75afb0b1_2a6b_4ef1_9536_39185e8ca404_H

#include "stringview.h"
#include "staticstring.h"
#include "staticvector.h"
#include "podvector.h"
#include <limits>

namespace wsw {

template <typename Off, typename Len,
	typename CharBuffer = wsw::PodVector<char>,
	// TODO: Use a more compact span storage as a default one?
	typename InternalSpan = std::pair<Off, Len>,
	typename SpanBuffer = wsw::PodVector<InternalSpan>>
class StringSpanStorage {
protected:
	CharBuffer m_charsBuffer;
	SpanBuffer m_spansBuffer;
public:
	using size_type = size_t;
	using difference_type = ptrdiff_t;

	void reserveChars( size_t charsToReserve ) {
		m_charsBuffer.reserve( charsToReserve );
	}

	void reserveSpans( size_type spansToReserve ) {
		m_spansBuffer.reserve( spansToReserve );
	}

	void shrink_to_fit() {
		m_charsBuffer.shrink_to_fit();
		m_spansBuffer.shrink_to_fit();
	}

	[[maybe_unused]]
	auto add( const wsw::StringView &s ) -> unsigned {
		const auto len = s.length();
		assert( len <= std::numeric_limits<Len>::max() );
		const auto off = m_charsBuffer.size();
		assert( off <= std::numeric_limits<Off>::max() );
		m_charsBuffer.append( s.data(), len );
		m_charsBuffer.push_back( '\0' );
		assert( m_spansBuffer.size() < std::numeric_limits<unsigned>::max() );
		auto resultSpanNum = (unsigned)m_spansBuffer.size();
		m_spansBuffer.push_back( { (Off)off, (Len)len } );
		return resultSpanNum;
	}

	// Caution: the returned pointer is unstable and could be invalidated upon the next call.
	// TODO: Redesigning the regular API so it can accept arbitrary iterators including ones that can
	// return modified values of an underlying sequence could be a better long-term solution.
	[[maybe_unused]]
	auto addReservedSpace( unsigned len ) -> std::pair<char *, unsigned> {
		assert( len <= std::numeric_limits<Len>::max() );
		const auto off = m_charsBuffer.size();
		assert( off <= std::numeric_limits<Off>::max() );
		m_charsBuffer.resize( m_charsBuffer.size() + len + 1 );
		assert( m_spansBuffer.size() < std::numeric_limits<unsigned>::max() );
		auto resultSpanNum = (unsigned)m_spansBuffer.size();
		m_spansBuffer.push_back( { (Off)off, (Len)len } );
		return { m_charsBuffer.data() + off, resultSpanNum };
	}

	template <typename ResultSpan = InternalSpan>
	[[nodiscard]]
	auto addReturningPair( const wsw::StringView &s ) -> std::pair<unsigned, ResultSpan> {
		const auto len = s.length();
		assert( len <= std::numeric_limits<Len>::max() );
		const auto off = m_charsBuffer.size();
		assert( off <= std::numeric_limits<Off>::max() );
		m_charsBuffer.append( s.data(), len );
		m_charsBuffer.push_back( '\0' );
		const auto resultSpanNum = (unsigned)m_spansBuffer.size();
		assert( m_spansBuffer.size() < std::numeric_limits<unsigned>::max() );
		if constexpr( std::is_same_v<std::remove_cvref_t<InternalSpan>, std::remove_cvref_t<ResultSpan>> ) {
			ResultSpan resultSpan = { (Off)off, (Len)len };
			m_spansBuffer.push_back( resultSpan );
			return std::make_pair( resultSpanNum, resultSpan );
		} else {
			m_spansBuffer.push_back( { (Off) off, (Len) len } );
			return std::make_pair( resultSpanNum, {(Off) off, (Len) len } );
		}
	}

	template <typename ResultSpan>
	[[nodiscard]]
	auto addReturningSpan( const wsw::StringView &s ) -> ResultSpan {
		return addReturningPair( s ).second;
	}

	void clear() {
		m_charsBuffer.clear();
		m_spansBuffer.clear();
	}

	[[nodiscard]]
	auto size() const -> size_type { return m_spansBuffer.size(); }

	[[nodiscard]]
	bool empty() const { return m_spansBuffer.empty(); }

	[[nodiscard]]
	auto operator[]( size_type index ) const -> wsw::StringView {
		assert( index < m_spansBuffer.size() );
		const auto [off, len] = m_spansBuffer[index];
		return wsw::StringView( m_charsBuffer.data() + off, len, wsw::StringView::ZeroTerminated );
	}

	struct const_iterator {
		const StringSpanStorage &m_parent;
		size_type m_index;

		const_iterator( const StringSpanStorage &parent, size_type index )
			: m_parent( parent ), m_index( index ) {}

		[[nodiscard]]
		bool operator!=( const const_iterator &that ) const {
			assert( std::addressof( m_parent ) == std::addressof( that.m_parent ) );
			return m_index != that.m_index;
		}
		[[maybe_unused]]
		auto operator++() -> const_iterator & {
			assert( m_index < m_parent.size() );
			m_index++;
			return *this;
		}
		[[nodiscard]]
		auto operator*() const -> wsw::StringView {
			assert( m_index < m_parent.size() );
			return m_parent[m_index];
		}
		[[nodiscard]]
		auto operator-( const const_iterator &that ) const -> difference_type {
			assert( std::addressof( m_parent ) == std::addressof( that.m_parent ) );
			assert( m_index <= (size_type)std::numeric_limits<difference_type>::max() );
			assert( that.m_index <= (size_type)std::numeric_limits<difference_type>::max() );
			return (difference_type)m_index - (difference_type)that.m_index;
		}
	};

	[[nodiscard]]
	auto begin() const -> const_iterator { return cbegin(); }
	[[nodiscard]]
	auto end() const -> const_iterator { return cend(); }
	[[nodiscard]]
	auto cbegin() const -> const_iterator { return const_iterator( *this, 0 ); }
	[[nodiscard]]
	auto cend() const -> const_iterator { return const_iterator( *this, size() ); }

	[[nodiscard]]
	auto front() const -> wsw::StringView { return ( *this )[0]; }
	[[nodiscard]]
	auto back() const -> wsw::StringView { return ( *this )[size() - 1]; }

	void pop_back() {
		const auto [off, len] = m_spansBuffer.back();
		m_spansBuffer.pop_back();
		m_charsBuffer.erase( off, len + 1 );
	}
};

template <typename Off, typename Len, unsigned SpansCapacity, unsigned CharsCapacity>
class StringSpanStaticStorage : public StringSpanStorage<Off, Len,
	wsw::StaticString<CharsCapacity>,
	std::pair<Off, Len>,
	wsw::StaticVector<std::pair<Off, Len>, SpansCapacity>> {
	static_assert( CharsCapacity >= 2 * SpansCapacity );
public:
	[[nodiscard]]
	static constexpr auto charsCapacity() -> unsigned { return CharsCapacity; }
	[[nodiscard]]
	static constexpr auto spansCapacity() -> unsigned { return SpansCapacity; }

	[[nodiscard]]
	bool canAdd( const wsw::StringView &s ) const {
		if( this->m_spansBuffer.size() == this->m_spansBuffer.capacity() ) {
			return false;
		}
		return this->m_charsBuffer.size() + 1 <= this->m_charsBuffer.capacity();
	}
};

}

#endif