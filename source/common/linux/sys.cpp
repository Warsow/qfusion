#include "../common.h"
#include "../local.h"

#include <stdio.h>
#include <unistd.h>

auto testProcessorFeatures() -> unsigned {
	unsigned features = 0;
#ifndef __clang__
	// Clang does not even have this intrinsic, executables work fine without it.
	__builtin_cpu_init();
#endif // clang-specific code
	if( __builtin_cpu_supports( "avx" ) ) {
		features |= Q_CPU_FEATURE_AVX;
	} else if( __builtin_cpu_supports( "sse4.2" ) ) {
		features |= Q_CPU_FEATURE_SSE42;
	} else if( __builtin_cpu_supports( "sse4.1" ) ) {
		features |= Q_CPU_FEATURE_SSE41;
	} else if( __builtin_cpu_supports( "sse2" ) ) {
		features |= Q_CPU_FEATURE_SSE2;
	}
	return features;
}

// TODO: Read /proc/cpuinfo directly

class ProcessPipeReader {
	FILE *fp;
public:
	explicit ProcessPipeReader( const char *command ) {
		fp = ::popen( command, "r" );
	}

	~ProcessPipeReader() {
		if( fp ) {
			(void)::pclose( fp );
		}
	}

	char *ReadNext( char *buffer, size_t bufferSize ) {
		if( !fp || ::feof( fp ) ) {
			return nullptr;
		}

		assert( bufferSize <= (size_t)std::numeric_limits<int>::max() );
		char *result = fgets( buffer, (int)bufferSize, fp );
		if( !result && ::ferror( fp ) ) {
			(void)pclose( fp );
			fp = nullptr;
		}
		return result;
	}
};

static const char *SkipWhiteSpace( const char *p ) {
	while( ::isspace( *p ) ) {
		p++;
	}
	return p;
}

static const char *StringForKey( const char *key, const char *line ) {
	// Skip a whitespace before line contents
	const char *p = SkipWhiteSpace( line );
	if( !*p ) {
		return nullptr;
	}
	// Skip a whitespace before key characters (ignoring this could lead to hard-to-find bugs)
	const char *k = SkipWhiteSpace( key );
	if( !*k ) {
		return nullptr;
	}

	// Match a line part by the key
	while( *k && ( ::tolower( *k ) == ::tolower( *p ) ) ) {
		k++, p++;
	}

	// If there is an unmatched key part
	if( *k ) {
		return nullptr;
	}

	// Skip a whitespace before the separating colon
	p = SkipWhiteSpace( p );
	if( *p++ != ':' ) {
		return nullptr;
	}

	// Skip a whitespace before contents
	return SkipWhiteSpace( p );
}

static double NumberForKey( const char *key, const char *line ) {
	if( const char *s = StringForKey( key, line ) ) {
		char *endptr;
		long value = strtol( s, &endptr, 10 );
		if( !*endptr || ::isspace( *endptr ) ) {
			static_assert( sizeof( long ) >= sizeof( int ), "incorrect strtol() result checks" );
			const auto min = std::numeric_limits<int>::min();
			const auto max = std::numeric_limits<int>::max();
			if( value >= min && value <= max ) {
				return value;
			}
		}
	}
	return std::numeric_limits<double>::quiet_NaN();
}

[[nodiscard]]
static auto testNumberOfProcessorsUsingLscpu() -> std::optional<std::pair<unsigned, unsigned>> {
	// We could try parsing "/proc/cpuinfo" but it is really complicated.
	// This utility provides much more convenient output and IPC details is hidden anyway.
	ProcessPipeReader reader( "lscpu" );
	char buffer[3072];

	unsigned cpus = 0;
	unsigned threadsPerCore = 0;
	double n;
	while( !cpus || !threadsPerCore ) {
		const char *line = reader.ReadNext( buffer, sizeof( buffer ) );
		if( !line ) {
			return std::nullopt;
		}
		if( !cpus ) {
			n = NumberForKey( "CPU(s)", line );
			if( !std::isnan( n ) && n > 0 ) {
				cpus = (unsigned)n;
			}
		}
		if( !threadsPerCore ) {
			n = NumberForKey( "Thread(s) per core", line );
			if( !std::isnan( n ) && n > 0 ) {
				threadsPerCore = (unsigned)n;
			}
		}
	}

	// TODO: Is is going to fail for Intel big/little design
	if( cpus % threadsPerCore ) {
		Com_Printf( S_COLOR_YELLOW "Weird number of CPU(s) for threads(s) per core: %d for %d", cpus, threadsPerCore );
		return std::nullopt;
	}

	return std::make_pair( cpus / threadsPerCore, cpus );
}

auto testNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>> {
	if( auto maybeResults = testNumberOfProcessorsUsingLscpu() ) {
		return maybeResults;
	}

	Com_Printf( S_COLOR_YELLOW "Warning: `lscpu` command can't be executed. Falling back to inferior methods\n" );

	// This is quite poor.
	// We hope that `lscpu` works for the most client installationsq
	if( long confValue = sysconf( _SC_NPROCESSORS_ONLN ) ) {
		// Sanity checks
		confValue = wsw::clamp<long>( confValue, 1, 256 );
		return std::make_pair( confValue, confValue );
	}

	return std::nullopt;
}