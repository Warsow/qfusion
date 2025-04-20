#include "common.h"
#include "wswexceptions.h"
#include "glob.h"
#include "local.h"

int Com_GlobMatch( const char *pattern, const char *text, const bool casecmp ) {
	return glob_match( pattern, text, casecmp );
}

//============================================================================

/*
* Com_AddPurePakFile
*/
void Com_AddPakToPureList( purelist_t **purelist, const char *pakname, const unsigned checksum ) {
	purelist_t *purefile;
	const size_t len = strlen( pakname ) + 1;

	purefile = ( purelist_t* )Q_malloc( sizeof( purelist_t ) + len );
	purefile->filename = ( char * )( ( uint8_t * )purefile + sizeof( *purefile ) );
	memcpy( purefile->filename, pakname, len );
	purefile->checksum = checksum;
	purefile->next = *purelist;
	*purelist = purefile;
}

/*
* Com_CountPureListFiles
*/
unsigned Com_CountPureListFiles( purelist_t *purelist ) {
	unsigned numpure;
	purelist_t *iter;

	numpure = 0;
	iter = purelist;
	while( iter ) {
		numpure++;
		iter = iter->next;
	}

	return numpure;
}

/*
* Com_FindPakInPureList
*/
purelist_t *Com_FindPakInPureList( purelist_t *purelist, const char *pakname ) {
	purelist_t *purefile = purelist;

	while( purefile ) {
		if( !strcmp( purefile->filename, pakname ) ) {
			break;
		}
		purefile = purefile->next;
	}

	return purefile;
}

/*
* Com_FreePureList
*/
void Com_FreePureList( purelist_t **purelist ) {
	purelist_t *purefile = *purelist;

	while( purefile ) {
		purelist_t *next = purefile->next;
		Q_free( purefile );
		purefile = next;
	}

	*purelist = NULL;
}

void *Q_malloc( size_t size ) {
	// TODO: Ensure 16-byte alignment
	// Zero memory as lots of old stuff rely on the old mempool behaviour
	void *buf = std::calloc( size, 1 );

	if( !buf ) {
		wsw::failWithBadAlloc();
	}

	return buf;
}

void *Q_realloc( void *buf, size_t newsize ) {
	void *newbuf = realloc( buf, newsize );

	if( !newbuf && newsize ) {
		wsw::failWithBadAlloc();
	}

	// TODO: Zero memory too? There's no portable way of doing that

	return newbuf;
}

void Q_free( void *buf ) {
	std::free( buf );
}

char *Q_strdup( const char *str ) {
	auto len = std::strlen( str );
	auto *result = (char *)Q_malloc( len + 1 );
	std::memcpy( result, str, len + 1 );
	return result;
}

struct SystemFeaturesHolder {
	unsigned processorFeatures { 0 };
	std::optional<std::pair<unsigned, unsigned>> numberOfProcessors;
	SystemFeaturesHolder() {
		processorFeatures  = testProcessorFeatures();
		numberOfProcessors = testNumberOfProcessors();

		// We have only set the most significant feature bit for code clarity.
		// Since this bit implies all least-significant bits presence, set these bits
		if( processorFeatures ) {
			assert( wsw::isPowerOf2( processorFeatures ) );
			processorFeatures |= ( processorFeatures - 1u );
		}
	}
};

[[nodiscard]]
static auto getSystemFeaturesHolder() -> const SystemFeaturesHolder * {
	// This design is thread-safe
	static SystemFeaturesHolder instance;
	return &instance;
}

unsigned Sys_GetProcessorFeatures() {
	return getSystemFeaturesHolder()->processorFeatures;
}

auto Sys_GetNumberOfProcessors() -> std::optional<std::pair<unsigned, unsigned>> {
	return getSystemFeaturesHolder()->numberOfProcessors;
}