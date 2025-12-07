#ifndef WSW_918a8022_22e1_4162_8c27_a12c933dcf39_H
#define WSW_918a8022_22e1_4162_8c27_a12c933dcf39_H

#include <cassert>
#include <cstdlib>
#include <cstring>
#include <type_traits>
#include <malloc.h>

#include <common/helpers/wswbasicmath.h>
#include <common/helpers/exceptions.h>

template <typename T>
class PodBuffer {
	static_assert( std::is_trivial_v<T> );
public:
	PodBuffer() noexcept = default;
	~PodBuffer() noexcept { destroy(); }

	PodBuffer( const PodBuffer<T> & ) = delete;
	auto operator=( const PodBuffer<T> & ) -> PodBuffer<T> & = delete;

	[[nodiscard]]
	auto releaseOwnership() -> T * {
		T *result  = m_data;
		m_data     = nullptr;
		m_capacity = 0;
		return result;
	}

	PodBuffer( PodBuffer<T> &&that ) noexcept {
		m_data     = that.m_data;
		m_capacity = that.m_capacity;

		that.m_data     = nullptr;
		that.m_capacity = 0;
	}

	[[maybe_unused]]
	auto operator=( PodBuffer<T> &&that ) noexcept -> PodBuffer<T> & {
		destroy();

		m_data     = that.m_data;
		m_capacity = that.m_capacity;

		that.m_data     = nullptr;
		that.m_capacity = 0;

		return *this;
	}

	[[nodiscard]]
	bool tryReserving( size_t minDesiredCapacity ) noexcept {
		if( m_capacity < minDesiredCapacity ) [[unlikely]] {
			if constexpr( alignof( T ) <= alignof( void * ) ) {
				m_data = (T *)std::realloc( m_data, sizeof( T ) * minDesiredCapacity );
				if( !m_data ) [[unlikely]] {
					return false;
				}
			} else {
				constexpr size_t roundedUpAlignment = wsw::ceilPowerOf2( alignof( T ) );
#ifndef _MSC_VER
				// Ensure that the size is a multiple of roundedUpAlignment (that's aligned_alloc() requirement)
				size_t roundedUpElemSize;
				if constexpr( sizeof( T ) < roundedUpAlignment ) {
					roundedUpElemSize = roundedUpAlignment;
				} else if constexpr( sizeof( T ) % roundedUpAlignment ) {
					roundedUpElemSize = sizeof( T ) + roundedUpAlignment - sizeof( T ) % roundedUpAlignment;
				} else {
					roundedUpElemSize = sizeof( T );
				}
				const size_t oldSizeInBytes = roundedUpElemSize * m_capacity;
				const size_t newSizeInBytes = roundedUpElemSize * minDesiredCapacity;
				void *const newData = std::aligned_alloc( roundedUpAlignment, newSizeInBytes );
				if( !newData ) [[unlikely]] {
					return false;
				}
				// Do copying for consistency with realloc implementation.
				// Also, doing the opposite could have been unexpected for users.
				std::memcpy( newData, m_data, oldSizeInBytes );
				std::free( m_data );
				m_data = ( T *)newData;
#else
				// Note: The order of size and alignment differs from std::aligned_alloc()
				m_data = ( T *)_aligned_realloc( m_data, sizeof( T ) * minDesiredCapacity, roundedUpAlignment );
				if( !m_data ) [[unlikely]] {
					return false;
				}
#endif

			}
			m_capacity = minDesiredCapacity;
		}
		return true;
	}

	void reserve( size_t minDesiredCapacity ) {
		if( !tryReserving( minDesiredCapacity ) ) [[unlikely]] {
			wsw::failWithBadAlloc();
		}
	}

	[[nodiscard]]
	auto capacity() const noexcept -> size_t { return m_capacity; }

	[[nodiscard]]
	auto get() noexcept -> T * { return m_data; }
	[[nodiscard]]
	auto get() const noexcept -> const T * { return m_data; }

	[[nodiscard]]
	auto reserveAndGet( size_t desiredCapacity ) -> T * {
		reserve( desiredCapacity );
		return get();
	}

	void reserveZeroed( size_t desiredCapacity ) {
		reserve( desiredCapacity );
		// TODO: We don't need to copy the previous content in this case, add a specialization?
		// TODO: Add a specialized calloc()-based implementation?
		std::memset( m_data, 0, sizeof( T ) * desiredCapacity );
	}

	[[nodiscard]]
	auto reserveZeroedAndGet( size_t minDesiredCapacity ) -> T * {
		reserveZeroed( minDesiredCapacity );
		return get();
	}

	[[nodiscard]]
	auto makeADeepCopy() const -> PodBuffer<T> {
		PodBuffer<T> result;
		result.reserve( m_capacity );
		std::memcpy( result.get(), m_data, sizeof( T ) * m_capacity );
		return result;
	}

private:
	void destroy() noexcept {
#ifndef _MSC_VER
		std::free( m_data );
#else
		if constexpr( alignof( T ) <= alignof( void * ) ) {
			std::free( m_data );
		} else {
			_aligned_free( m_data );
		}
#endif
	}

	T *m_data { nullptr };
	size_t m_capacity { 0 };
};

#endif