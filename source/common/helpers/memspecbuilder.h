#ifndef WSW_87e98728_9bef_4abb_bfd1_43b49bbc70dc_H
#define WSW_87e98728_9bef_4abb_bfd1_43b49bbc70dc_H

#include <cstdlib>
#include <cstdint>
#include <cassert>
#include <tuple>

namespace wsw {

class MemSpecBuilder {
	size_t m_size { 0 };

	MemSpecBuilder() = default;
public:
	template <typename T>
	class ChunkSpec {
		friend class MemSpecBuilder;
		size_t m_offset;
	public:
		[[nodiscard]]
		auto get( void *base ) const -> T * {
			auto *p = (uint8_t *)base;
			p += m_offset;
			assert( !( (uintptr_t)p % alignof( T ) ) );
			return (T *)p;
		}
	};

	template <typename T>
	[[nodiscard]]
	auto add() -> ChunkSpec<T> {
		return add<T>( 1 );
	}

	template <typename T>
	[[nodiscard]]
	auto add( size_t numElems ) -> ChunkSpec<T> {
		if( const auto remainder = m_size % alignof( T ) ) {
			m_size += alignof( T ) - remainder;
		}
		ChunkSpec<T> result;
		result.m_offset = m_size;
		m_size += sizeof( T ) * numElems;
		return result;
	}

	template <typename T>
	[[nodiscard]]
	auto addAligned( size_t numElems, size_t alignment ) -> ChunkSpec<T> {
		assert( alignment && alignment >= alignof( T ) );
		assert( !( alignment & ( alignment - 1 ) ) );
		if( const auto remainder = m_size % alignment ) {
			m_size += alignment - remainder;
		}
		ChunkSpec<T> result;
		result.m_offset = m_size;
		m_size += sizeof( T ) * numElems;
		return result;
	}

	// TODO: Variadic?

	template <typename T1, typename T2>
	[[nodiscard]]
	auto add() -> std::tuple<ChunkSpec<T1>, ChunkSpec<T2>> {
		// Force an eager evaluation in the specified order
		const auto spec1 = add<T1>();
		const auto spec2 = add<T2>();
		return std::make_tuple( spec1, spec2 );
	}

	template <typename T1, typename T2, typename T3>
	[[nodiscard]]
	auto add() -> std::tuple<ChunkSpec<T1>, ChunkSpec<T2>, ChunkSpec<T3>> {
		const auto spec1 = add<T1>();
		const auto spec2 = add<T2>();
		const auto spec3 = add<T3>();
		return std::make_tuple( spec1, spec2, spec3 );
	}

	[[nodiscard]]
	auto sizeSoFar() -> size_t { return m_size; }

	[[nodiscard]]
	static auto initiallyEmpty() -> MemSpecBuilder {
		return MemSpecBuilder();
	}

	template <typename T>
	[[nodiscard]]
	static auto withInitialSizeOf() -> MemSpecBuilder {
		MemSpecBuilder builder;
		(void)builder.add<T>();
		return builder;
	}
};

}

#endif
