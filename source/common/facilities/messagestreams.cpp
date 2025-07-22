#include <common/facilities/messagestreams.h>
#include <common/helpers/enumtokenmatcher.h>
#include <common/facilities/configvars.h>
#include <common/helpers/freelistallocator.h>
#include <common/types/staticvector.h>
#include <common/helpers/qthreads.h>
#include <common/facilities/cvar.h>
#include <common/facilities/fscompat.h>
#include <common/facilities/syspublic.h>
#include <common/local.h>
#include <common/common.h>
#include <common/syslocal.h>
// TODO...
#include <client/console.h>

using wsw::operator""_asView;

// We dislike the idea to add "null" values to the base enum definitions, as well as making them flags per se.
// At the same time we do not want EnumFlagsConfigVar to convert sequential values
// to untyped bit masks automatically (TODO!!!!!: Should we rather consider this option?).
// Thus, we have to define the mapping from sequential values to flags manually.

enum class MessageDomainFlags : unsigned {
	None     = 0u,
	Common   = 1u << (unsigned)wsw::MessageDomain::Common,
	Server   = 1u << (unsigned)wsw::MessageDomain::Server,
	Client   = 1u << (unsigned)wsw::MessageDomain::Client,
	Sound    = 1u << (unsigned)wsw::MessageDomain::Sound,
	Renderer = 1u << (unsigned)wsw::MessageDomain::Renderer,
	UI       = 1u << (unsigned)wsw::MessageDomain::UI,
	CGame    = 1u << (unsigned)wsw::MessageDomain::CGame,
	Game     = 1u << (unsigned)wsw::MessageDomain::Game,
	AI       = 1u << (unsigned)wsw::MessageDomain::AI,
};

enum class MessageCategoryFlags : unsigned {
	None    = 0u,
	Debug   = 1u << (unsigned)wsw::MessageCategory::Debug,
	Notice  = 1u << (unsigned)wsw::MessageCategory::Notice,
	Warning = 1u << (unsigned)wsw::MessageCategory::Warning,
	Error   = 1u << (unsigned)wsw::MessageCategory::Error,
};

class DomainMatcher : public wsw::EnumTokenMatcher<MessageDomainFlags, DomainMatcher> {
public:
	// We guess, there's no need to handle non-abbreviated string values
	// (while that is perfectly possible just by declaring respective entries in the list)
	DomainMatcher() : wsw::EnumTokenMatcher<MessageDomainFlags, DomainMatcher>( {
		{ "None"_asView, MessageDomainFlags::None },
		{ "COM"_asView, MessageDomainFlags::Common },
		{ "SV"_asView, MessageDomainFlags::Server },
		{ "CL"_asView, MessageDomainFlags::Client },
		{ "S"_asView, MessageDomainFlags::Sound },
		{ "R"_asView, MessageDomainFlags::Renderer },
		{ "UI"_asView, MessageDomainFlags::UI },
		{ "CG"_asView, MessageDomainFlags::CGame },
		{ "G"_asView, MessageDomainFlags::Game },
		{ "AI"_asView, MessageDomainFlags::AI },
	}) {}
};

class CategoryMatcher : public wsw::EnumTokenMatcher<MessageCategoryFlags, CategoryMatcher> {
public:
	CategoryMatcher() : wsw::EnumTokenMatcher<MessageCategoryFlags, CategoryMatcher>( {
		{ "None"_asView, MessageCategoryFlags::None },
		{ "Debug"_asView, MessageCategoryFlags::Debug },
		{ "Info"_asView, MessageCategoryFlags::Notice },
		{ "Warning"_asView, MessageCategoryFlags::Warning },
		{ "Error"_asView, MessageCategoryFlags::Error },
	}) {}
};

// We guess, we should not save values of these vars, as it could seriously affect basic console interaction.
// Uses which really may utilize functionality of these vars should set these vars via the executable command line.

static EnumFlagsConfigVar<MessageDomainFlags, DomainMatcher> v_outputDomainMask( "com_outputDomainMask"_asView, {
	.byDefault = (MessageDomainFlags)~0, .flags = 0,
});

static EnumFlagsConfigVar<MessageCategoryFlags, CategoryMatcher> v_outputCategoryMask( "com_outputCategoryMask"_asView, {
	.byDefault = (MessageCategoryFlags)~0, .flags = 0,
});

static const struct DomainTraits {
	const char *printedPrefix;
	EnumFlagsConfigVar<MessageCategoryFlags, CategoryMatcher> overridenCategoryMask;
} g_domainTraits[9] {
	{ "COM", { "com_overrideOutputCategoryMask_COM"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "SV", { "com_overrideOutputCategoryMask_SV"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "CL", { "com_overrideOutputCategoryMask_CL"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "S", { "com_overrideOutputCategoryMask_S"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "R", { "com_overrideOutputCategoryMask_R"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "UI", { "com_overrideOutputCategoryMask_UI"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "CG", { "com_overrideOutputCategoryMask_CG"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "G", { "com_overrideOutputCategoryMask_G"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
	{ "AI", { "com_overrideOutputCategoryMask_AI"_asView, { .byDefault = MessageCategoryFlags::None, .flags = 0 } } },
};

extern qmutex_t *com_print_mutex;
extern cvar_t *logconsole;
extern cvar_t *logconsole_append;
extern cvar_t *logconsole_flush;
extern cvar_t *logconsole_timestamp;
extern int log_file;

static const char *kPrintedMessageColorForCategory[4] {
	S_COLOR_GREY, S_COLOR_WHITE, S_COLOR_YELLOW, S_COLOR_RED
};

class alignas( 16 ) RegularMessageStreamsAllocator {
	wsw::Mutex m_mutex;
	wsw::HeapBasedFreelistAllocator m_allocator;

	static constexpr size_t kSize = MAX_PRINTMSG + sizeof( wsw::RegularMessageStream );
	static constexpr size_t kCapacity = 1024;

	static constexpr size_t kCategoryCount = std::size( kPrintedMessageColorForCategory );
	static constexpr size_t kDomainCount = std::size( g_domainTraits );

	wsw::StaticVector<wsw::RegularMessageStream, kCategoryCount * kDomainCount> m_nullStreams;
public:
	RegularMessageStreamsAllocator() : m_allocator( kSize, kCapacity ) {
		// TODO: This is very flaky, but alternatives aren't perfect either...
		for( unsigned domainIndex = 0; domainIndex < kDomainCount; ++domainIndex ) {
			const auto domain( ( wsw::MessageDomain)( domainIndex ) );
			for( unsigned categoryIndex = 0; categoryIndex < kCategoryCount; ++categoryIndex ) {
				const auto category( ( wsw::MessageCategory )( categoryIndex ) );
				new( m_nullStreams.unsafe_grow_back() )wsw::RegularMessageStream( nullptr, 0, domain, category );
			}
		}
	}

	[[nodiscard]]
	auto nullStreamFor( wsw::MessageDomain domain, wsw::MessageCategory category ) -> wsw::RegularMessageStream * {
		const auto indexForDomain   = (unsigned)domain;
		const auto indexForCategory = (unsigned)category;
		return std::addressof( m_nullStreams[indexForDomain * kCategoryCount + indexForCategory] );
	}

	[[nodiscard]]
	bool isANullStream( wsw::RegularMessageStream *stream ) {
		return (size_t)( stream - m_nullStreams.data() ) < std::size( m_nullStreams );
	}

	[[nodiscard]]
	auto alloc( wsw::MessageDomain domain, wsw::MessageCategory category ) -> wsw::RegularMessageStream * {
		[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
		if( !m_allocator.isFull() ) [[likely]] {
			uint8_t *mem = m_allocator.allocOrNull();
			auto *buffer = (char *)( mem + sizeof( wsw::RegularMessageStream ) );
			return new( mem )wsw::RegularMessageStream( buffer, MAX_PRINTMSG, domain, category );
		} else if( auto *mem = (uint8_t *)::malloc( kSize ) ) {
			auto *buffer = (char *)( mem + sizeof( wsw::RegularMessageStream ) );
			return new( mem )wsw::RegularMessageStream( buffer, MAX_PRINTMSG, domain, category );
		} else {
			return nullStreamFor( domain, category );
		}
	}

	[[nodiscard]]
	auto free( wsw::RegularMessageStream *stream ) {
		if( !isANullStream( stream ) ) [[likely]] {
			[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
			stream->~RegularMessageStream();
			if( m_allocator.mayOwn( stream ) ) [[likely]] {
				m_allocator.free( stream );
			} else {
				::free( stream );
			}
		}
	}
};

static RegularMessageStreamsAllocator g_regularMessageStreamsAllocator;

[[nodiscard]]
static bool isMessageAcceptedByFilters( wsw::MessageDomain domain, wsw::MessageCategory category ) {
	if( v_outputDomainMask.initialized() ) [[likely]] {
		if( !v_outputDomainMask.isAnyBitSet( (MessageDomainFlags)( 1u << (unsigned)domain ) ) ) {
			return false;
		}
	}
	const auto &overriddenCategoryMaskVar = g_domainTraits[(unsigned)domain].overridenCategoryMask;
	if( overriddenCategoryMaskVar.initialized() ) [[likely]] {
		// Check whether some mask bits are set (this is generally unlikely)
		// We retrieve an unsigned value once to reduce the number of value lookups.
		if( const auto maskBits = (unsigned)overriddenCategoryMaskVar.get() ) [[unlikely]] {
			// TODO: Should the override mask bits fully replace general bits, like they do for now?
			return ( maskBits & ( 1 << (unsigned)category ) ) != 0;
		}
	}
	if( category != wsw::MessageCategory::Debug ) {
		if( v_outputCategoryMask.initialized() ) [[likely]] {
			// If the category bit is unset
			if( !v_outputCategoryMask.isAnyBitSet( (MessageCategoryFlags )( 1u << (unsigned)category ) ) ) {
				return false;
			}
		}
	} else {
		// Hacks for the Debug category - let the developer var control it.
		// Note that we still can enable/suppress Debug messages using individual masks.
		// TODO: Should we rather patch the category mask value dynamically?
		if( developer ) [[likely]] {
			return developer->integer != 0;
		}
	}
	return true;
}

auto wsw::createRegularMessageStream( wsw::MessageDomain domain, wsw::MessageCategory category ) -> wsw::RegularMessageStream * {
	if( isMessageAcceptedByFilters( domain, category ) ) {
		return ::g_regularMessageStreamsAllocator.alloc( domain, category );
	}
	return ::g_regularMessageStreamsAllocator.nullStreamFor( domain, category );
}

void wsw::submitRegularMessageStream( wsw::RegularMessageStream *stream ) {
	if( isMessageAcceptedByFilters( stream->m_domain, stream->m_category ) ) {
		// TODO: Eliminate Com_Printf()
		if( !::g_regularMessageStreamsAllocator.isANullStream( stream ) ) {
			stream->m_data[wsw::min( stream->m_limit, stream->m_offset )] = '\0';
			const auto indexForCategory = (unsigned)stream->m_category;
			assert( indexForCategory <= std::size( kPrintedMessageColorForCategory ) );
			const char *color = kPrintedMessageColorForCategory[indexForCategory];
			Com_Printf( "%s%s\n", color, stream->m_data );
		} else {
			Com_Printf( S_COLOR_RED "A null line stream was used. The line content was discarded\n" );
		}
	}
	::g_regularMessageStreamsAllocator.free( stream );
}

class FailureMessageStreamsAllocator {
public:
	[[nodiscard]]
	auto alloc( wsw::FailureKind kind ) -> wsw::FailureMessageStream * {
		const int currIndex = ( kind == wsw::DropFailure ) ? 0 : 1;
		if( m_streamHolders[0].size() +  m_streamHolders[1].size() > 0 ) {
			const char *kindNames[2] {"drop", "fatal" };
			const char *currName = kindNames[currIndex];
			// TODO: Should we just throw instead of calling Sys_Error()?
			if( m_streamHolders[currIndex].full() ) {
				Sys_Error( "Attempting to allocate a %s failure stream recursively", currName );
			} else {
				const char *otherName = kindNames[( currIndex + 1 ) % 2];
				Sys_Error( "Attempting to allocate a %s failure stream while %s stream is open", currName, otherName );
			}
		}
		char *data = m_streamCharData[currIndex];
		return new( m_streamHolders[currIndex].unsafe_grow_back() )wsw::FailureMessageStream( data, MAX_PRINTMSG, kind );
	}
	void free( wsw::FailureMessageStream *stream ) {
		const int index = stream->m_kind == wsw::DropFailure ? 0 : 1;
		if( m_streamHolders[index].empty() ) {
			Sys_Error( "The respective failure stream holder is empty" );
		}
		if( m_streamHolders[index].data() != stream ) {
			Sys_Error( "Attempting to free a stray stream" );
		}
		m_streamHolders[index].pop_back();
	}
private:
	wsw::StaticVector<wsw::FailureMessageStream, 1> m_streamHolders[2];
	char m_streamCharData[2][MAX_PRINTMSG];
};

static thread_local FailureMessageStreamsAllocator tl_failureMessageStreamsAllocator;

auto wsw::createFailureMessageStream( FailureKind kind ) -> FailureMessageStream * {
	return tl_failureMessageStreamsAllocator.alloc( kind );
}

void wsw::submitFailureMessageStream( FailureMessageStream *stream ) {
	const auto len = wsw::min( stream->m_limit, stream->m_offset );
	assert( len <= MAX_PRINTMSG );
	char message[MAX_PRINTMSG];
	memcpy( message, stream->m_data, len );
	message[len] = '\0';
	const auto code = ( stream->m_kind == DropFailure ) ? ERR_DROP : ERR_FATAL;
	tl_failureMessageStreamsAllocator.free( stream );
	Com_Error( code, "%s", message );
}

/*
============================================================================

CLIENT / SERVER interactions

============================================================================
*/

static int rd_target;
static char *rd_buffer;
static int rd_buffersize;
static void ( *rd_flush )( int target, const char *buffer, const void *extra );
static const void *rd_extra;

void Com_BeginRedirect( int target, char *buffer, int buffersize,
						void ( *flush )( int, const char*, const void* ), const void *extra ) {
	if( !target || !buffer || !buffersize || !flush ) {
		return;
	}

	QMutex_Lock( com_print_mutex );

	rd_target = target;
	rd_buffer = buffer;
	rd_buffersize = buffersize;
	rd_flush = flush;
	rd_extra = extra;

	*rd_buffer = 0;
}

void Com_EndRedirect( void ) {
	rd_flush( rd_target, rd_buffer, rd_extra );

	rd_target = 0;
	rd_buffer = NULL;
	rd_buffersize = 0;
	rd_flush = NULL;
	rd_extra = NULL;

	QMutex_Unlock( com_print_mutex );
}

void Com_DeferConsoleLogReopen( void ) {
	if( logconsole != NULL ) {
		logconsole->modified = true;
	}
}

void Com_CloseConsoleLog( bool lock, bool shutdown ) {
	if( shutdown ) {
		lock = true;
	}

	if( lock ) {
		QMutex_Lock( com_print_mutex );
	}

	if( log_file ) {
		FS_FCloseFile( log_file );
		log_file = 0;
	}

	if( shutdown ) {
		logconsole = NULL;
	}

	if( lock ) {
		QMutex_Unlock( com_print_mutex );
	}
}

void Com_ReopenConsoleLog( void ) {
	char errmsg[MAX_PRINTMSG] = { 0 };

	QMutex_Lock( com_print_mutex );

	Com_CloseConsoleLog( false, false );

	if( logconsole && logconsole->string && logconsole->string[0] ) {
		size_t name_size;
		char *name;

		name_size = strlen( logconsole->string ) + strlen( ".log" ) + 1;
		name = ( char* )Q_malloc( name_size );
		Q_strncpyz( name, logconsole->string, name_size );
		COM_DefaultExtension( name, ".log", name_size );

		if( FS_FOpenFile( name, &log_file, ( logconsole_append && logconsole_append->integer ? FS_APPEND : FS_WRITE ) ) == -1 ) {
			log_file = 0;
			Q_snprintfz( errmsg, MAX_PRINTMSG, "Couldn't open: %s\n", name );
		}

		Q_free( name );
	}

	QMutex_Unlock( com_print_mutex );

	if( errmsg[0] ) {
		Com_Printf( "%s", errmsg );
	}
}

/*
* Com_Printf
*
* Both client and server can use this, and it will output
* to the apropriate place.
*/
void Com_Printf( const char *format, ... ) {
	va_list argptr;
	char msg[MAX_PRINTMSG];

	/*
	time_t timestamp;
	char timestamp_str[MAX_PRINTMSG];
	struct tm *timestampptr;
	timestamp = time( NULL );
	timestampptr = gmtime( &timestamp );
	strftime( timestamp_str, MAX_PRINTMSG, "%Y-%m-%dT%H:%M:%SZ ", timestampptr );
	*/

	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	QMutex_Lock( com_print_mutex );

	/*
	if( rd_target ) {
		if( (int)( strlen( msg ) + strlen( rd_buffer ) ) > ( rd_buffersize - 1 ) ) {
			rd_flush( rd_target, rd_buffer, rd_extra );
			*rd_buffer = 0;
		}
		strcat( rd_buffer, msg );

		QMutex_Unlock( com_print_mutex );
		return;
	}*/

	// also echo to debugging console
	Sys_ConsoleOutput( msg );

	Con_Print( msg );

	if( log_file ) {
		/*
		if( logconsole_timestamp && logconsole_timestamp->integer ) {
			FS_Printf( log_file, "%s", timestamp_str );
		}*/
		/*
		FS_Printf( log_file, "%s", msg );
		*/
		/*
		if( logconsole_flush && logconsole_flush->integer ) {
			FS_Flush( log_file ); // force it to save every time
		}*/
	}

	QMutex_Unlock( com_print_mutex );
}


/*
* Com_DPrintf
*
* A Com_Printf that only shows up if the "developer" cvar is set
*/
void Com_DPrintf( const char *format, ... ) {
	va_list argptr;
	char msg[MAX_PRINTMSG];

	if( !developer || !developer->integer ) {
		return; // don't confuse non-developers with techie stuff...

	}
	va_start( argptr, format );
	Q_vsnprintfz( msg, sizeof( msg ), format, argptr );
	va_end( argptr );

	Com_Printf( "%s", msg );
}