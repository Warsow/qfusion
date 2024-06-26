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

template <typename T>
void sortPodNonSpeedCritical( T *begin, T *end ) {
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