#ifndef WSW_05a4e0ff_8b49_494a_8449_2684d048eadf_H
#define WSW_05a4e0ff_8b49_494a_8449_2684d048eadf_H

#include <cstdlib>

namespace wsw {

// TODO: Use better constraints
template <typename Container, typename T>
	requires requires( Container c ) {
		{ c.begin() };
		{ c.end() };
	}
[[nodiscard]]
constexpr bool contains( const Container &c, const T &value ) {
	for( auto it = c.begin(); it != c.end(); ++it ) {
		if( *it == value ) {
			return true;
		}
	}
	return false;
}

template <typename It, typename T>
[[nodiscard]]
constexpr bool contains( It begin, It end, const T &value ) {
	for( auto it = begin; it != end; ++it ) {
		if( *it == value ) {
			return true;
		}
	}
	return false;
}

template <typename Container, typename Predicate>
requires requires( Container c, Predicate p ) {
	{ c.begin() };
	{ c.end() };
}
[[nodiscard]]
constexpr bool any_of( const Container &c, Predicate &&p ) {
	for( auto it = c.begin(); it != c.end(); ++it ) {
		if( p( *it ) ) {
			return true;
		}
	}
	return false;
}

template <typename It, typename Predicate>
[[nodiscard]]
constexpr bool any_of( It begin, It end, Predicate &&p ) {
	for( auto it = begin; it != end; ++it ) {
		if( p( *it ) ) {
			return true;
		}
	}
	return false;
}

template <typename It, typename T>
constexpr void fill( It begin, It end, const T &value ) {
	for( auto it = begin; it != end; ++it ) {
		*it = value;
	}
}

template <typename It, typename T>
[[nodiscard]]
constexpr auto find( It begin, It end, const T &value ) -> It {
	for( auto it = begin; it != end; ++it ) {
		if( *it == value ) {
			return it;
		}
	}
	return end;
}

template <typename It, typename Predicate>
[[nodiscard]]
constexpr auto find_if( It begin, It end, Predicate &&p ) -> It {
	for( auto it = begin; it != end; ++it ) {
		if( p( *it ) ) {
			return it;
		}
	}
	return end;
}

template <typename It>
[[nodiscard]]
constexpr auto max_element( It begin, It end ) -> It {
	if( begin != end ) [[likely]] {
		It best = begin;
		It it   = begin;
		it++;
		for(; it != end; ++it ) {
			if( *best < *it ) {
				best = it;
			}
		}
		return best;
	}
	return end;
}

template <typename It, typename Less>
[[nodiscard]]
bool is_heap( It begin, It end, const Less &less ) {
	assert( begin <= end );
	auto data = begin;
	std::make_unsigned_t<decltype( end - begin )> size = end - begin;
	for( decltype( size ) index = 1; index < size; ++index ) {
		auto parentIndex = ( index - 1 ) / 2;
		if( less( data[parentIndex], data[index] ) ) {
			return false;
		}
	}
	return true;
}

template <typename It>
[[nodiscard]]
bool is_heap( It begin, It end ) {
	const auto less = []( const decltype( *begin ) &l, const decltype( *begin ) &r ) -> bool { return l < r; };
	return wsw::is_heap( begin, end, less );
}

template <typename It, typename Less>
void push_heap( It begin, It end, const Less &less ) {
	assert( begin < end );
	std::make_unsigned_t<decltype( end - begin )> currIndex = ( end - begin ) - 1;
	assert( currIndex >= 0 );
	It data = begin;
	for(;; ) {
		if( currIndex < 1 ) [[unlikely]] {
			break;
		}
		const auto parentIndex = ( currIndex - 1 ) / 2;
		if( less( data[parentIndex], data[currIndex] ) ) {
			std::swap( data[currIndex], data[parentIndex] );
		} else {
			break;
		}
		currIndex = parentIndex;
	}
#if 0
	assert( wsw::is_heap( begin, end, less ) );
#endif
}

template <typename It>
void push_heap( It begin, It end ) {
	const auto less = []( const decltype( *begin ) &l, const decltype( *begin ) &r ) -> bool { return l < r; };
	wsw::push_heap( begin, end, less );
}

template <typename It, typename Less>
void pop_heap( It begin, It end, const Less &less ) {
	assert( begin < end );
	std::make_unsigned_t<decltype( end - begin )> size = end - begin;
	assert( size > 0 );
	auto data = begin;
	size -= 1;
	std::swap( data[0], data[size] );
	decltype( size ) currIndex = 0;
	for(;; ) {
		const auto leftChildIndex = currIndex * 2 + 1;
		// Check overflow
		assert( leftChildIndex > currIndex );
		if( leftChildIndex >= size ) [[unlikely]] {
			break;
		}
		auto childIndex            = leftChildIndex;
		const auto rightChildIndex = leftChildIndex + 1;
		assert( rightChildIndex > currIndex );
		if( rightChildIndex < size ) [[likely]] {
			if( less( data[leftChildIndex], data[rightChildIndex] ) ) {
				childIndex = rightChildIndex;
			}
		}
		if( less( data[currIndex], data[childIndex] ) ) {
			std::swap( data[currIndex], data[childIndex] );
		} else {
			break;
		}
		currIndex = childIndex;
	}
#if 0
	assert( wsw::is_heap( begin, end - 1, less ) );
#endif
}

template <typename It>
void pop_heap( It begin, It end ) {
	const auto less = []( const decltype( *begin ) &l, const decltype( *begin ) &r ) -> bool { return l < r; };
	wsw::pop_heap( begin, end, less );
}

template <typename It, typename Less>
void make_heap( It begin, It end, const Less &less ) {
	assert( begin <= end );
	for( auto it = begin + 1; it <= end; ++it ) {
		wsw::push_heap( begin, it, less );
	}
#if 0
	assert( wsw::is_heap( begin, end, less ) );
#endif
}

template <typename It>
void make_heap( It begin, It end ) {
	const auto less = []( const decltype( *begin ) &l, const decltype( *begin ) &r ) -> bool { return l < r; };
	wsw::make_heap( begin, end, less );
}

template <typename T, typename Less>
void sortPodNonSpeedCritical( T *begin, T *end, const Less &less ) {
	assert( begin <= end );
	// Inspired by https://deplinenoise.wordpress.com/2014/02/23/using-c11-capturing-lambdas-w-vanilla-c-api-functions/
#ifndef WIN32
	auto qsortfn = ::qsort_r;
	auto cmp     = []( const void *lp, const void *rp, void *lessp ) -> int {
#else
	auto qsortfn = ::qsort_s;
	auto cmp     = []( void *lessp, const void *lp, const void *rp ) -> int {
#endif
		const T &l       = *( (const T *)lp );
		const T &r       = *( (const T *)rp );
		const Less &less = *( (const Less *)lessp );
		if( less( l, r ) ) { return -1; }
		if( less( r, l ) ) { return +1; }
		return 0;
	};
	qsortfn( begin, (size_t)( end - begin ), sizeof( T ), cmp, (void *)&less );
}

template <typename T>
void sortPodNonSpeedCritical( T *begin, T *end ) {
	// TODO: Should we implement it using sortPodNonSpeedCritical<T, std::less<T>>?
	::qsort( begin, (size_t)( end - begin ), sizeof( T ), []( const void *lp, const void *rp ) -> int {
		const T &l = *( (const T *)lp );
		const T &r = *( (const T *)rp );
		if( l < r ) { return -1; }
		if( r < l ) { return +1; }
		return 0;
	});
}

}

#endif