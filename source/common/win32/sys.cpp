#include "../local.h"
#include "../common.h"
#include <common/types/podbufferholder.h>

#include <windows.h>

#if ( defined( _M_IX86 ) || defined( _M_AMD64 ) || defined( _M_X64 ) )
// For __cpuid() intrinsic
#include <intrin.h>
#else
#error Unsupported platform
#endif

auto testProcessorFeatures() -> unsigned {
	int cpuInfo[4];
	__cpuid( cpuInfo, 0 );
	// Check whether CPUID is supported at all (is it relevant nowadays?)
	if( cpuInfo[0] == 0 ) {
		return 0;
	}
	unsigned features = 0;
	// Get standard feature bits (look for description here https://en.wikipedia.org/wiki/CPUID)
	__cpuid( cpuInfo, 1 );
	const int ECX = cpuInfo[2];
	const int EDX = cpuInfo[3];
	if( ECX & ( 1 << 28 ) ) {
		features |= Q_CPU_FEATURE_AVX;
	} else if( ECX & ( 1 << 20 ) ) {
		features |= Q_CPU_FEATURE_SSE42;
	} else if( ECX & ( 1 << 19 ) ) {
		features |= Q_CPU_FEATURE_SSE41;
	} else if( EDX & ( 1 << 26 ) ) {
		features |= Q_CPU_FEATURE_SSE2;
	}
	return 0;
}

auto testNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>> {
	DWORD bufferLenInBytes = 0;
	::GetLogicalProcessorInformation( nullptr, &bufferLenInBytes );
	if( ::GetLastError() != ERROR_INSUFFICIENT_BUFFER ) {
		return std::nullopt;
	}

	const unsigned numSlpiElems = bufferLenInBytes / sizeof( SYSTEM_LOGICAL_PROCESSOR_INFORMATION );
	assert( bufferLen % sizeof( SYSTEM_LOGICAL_PROCESSOR_INFORMATION ) == 0 );

	PodBufferHolder<SYSTEM_LOGICAL_PROCESSOR_INFORMATION> bufferHolder;
	if( !bufferHolder.tryReserving( numSlpiElems ) ) {
		return std::nullopt;
	}

	// TODO: add and use .tryReservingAndGet() call
	SYSTEM_LOGICAL_PROCESSOR_INFORMATION *const slpi = bufferHolder.get();
	if( !::GetLogicalProcessorInformation( slpi, &bufferLenInBytes ) ) {
		return std::nullopt;
	}

	unsigned physical = 0;
	unsigned logical  = 0;
	for( unsigned i = 0; i < numSlpiElems; ++i ) {
		if( slpi[i].Relationship == RelationProcessorCore ) {
			physical++;

			const ULONG_PTR processorMask = slpi[i].ProcessorMask;
			static_assert( sizeof( processorMask ) == sizeof( ptrdiff_t ) );
			// We can't rely on popcnt instruction support
			for( uint64_t j = 0, bitMask = 1; j < sizeof( ptrdiff_t ) * 8; ++j, bitMask <<= 1 ) {
				if( processorMask & bitMask ) {
					logical++;
				}
			}
		}
	}

	return std::make_pair( physical, logical );
}

