#include "outputmessages.h"
#include "enumtokenmatcher.h"
#include "configvars.h"
#include "freelistallocator.h"
#include "wswstaticvector.h"
#include "common.h"

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

static BoolConfigVar v_enableOutputDomainPrefix { "com_enableOutputDomainPrefix"_asView, {
	.byDefault = false, .flags = CVAR_ARCHIVE,
}};

extern qmutex_t *com_print_mutex;
extern cvar_t *logconsole;
extern cvar_t *logconsole_append;
extern cvar_t *logconsole_flush;
extern cvar_t *logconsole_timestamp;
extern int log_file;

static const char *kPrintedMessageColorForCategory[4] {
	S_COLOR_GREY, S_COLOR_WHITE, S_COLOR_YELLOW, S_COLOR_RED
};

class alignas( 16 ) MessageStreamsAllocator {
	wsw::Mutex m_mutex;
	wsw::HeapBasedFreelistAllocator m_allocator;

	static constexpr size_t kSize = MAX_PRINTMSG + sizeof( wsw::OutputMessageStream );
	static constexpr size_t kCapacity = 1024;

	static constexpr size_t kCategoryCount = std::size( kPrintedMessageColorForCategory );
	static constexpr size_t kDomainCount = std::size( g_domainTraits );

	wsw::StaticVector<wsw::OutputMessageStream, kCategoryCount * kDomainCount> m_nullStreams;
public:
	MessageStreamsAllocator() : m_allocator( kSize, kCapacity ) {
		// TODO: This is very flaky, but alternatives aren't perfect either...
		for( unsigned domainIndex = 0; domainIndex < kDomainCount; ++domainIndex ) {
			const auto domain( ( wsw::MessageDomain)( domainIndex ) );
			for( unsigned categoryIndex = 0; categoryIndex < kCategoryCount; ++categoryIndex ) {
				const auto category( ( wsw::MessageCategory )( categoryIndex ) );
				new( m_nullStreams.unsafe_grow_back() )wsw::OutputMessageStream( nullptr, 0, domain, category );
			}
		}
	}

	[[nodiscard]]
	auto nullStreamFor( wsw::MessageDomain domain, wsw::MessageCategory category ) -> wsw::OutputMessageStream * {
		const auto indexForDomain   = (unsigned)domain;
		const auto indexForCategory = (unsigned)category;
		return std::addressof( m_nullStreams[indexForDomain * kCategoryCount + indexForCategory] );
	}

	[[nodiscard]]
	bool isANullStream( wsw::OutputMessageStream *stream ) {
		return (size_t)( stream - m_nullStreams.data() ) < std::size( m_nullStreams );
	}

	[[nodiscard]]
	auto alloc( wsw::MessageDomain domain, wsw::MessageCategory category ) -> wsw::OutputMessageStream * {
		[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
		if( !m_allocator.isFull() ) [[likely]] {
			uint8_t *mem = m_allocator.allocOrNull();
			auto *buffer = (char *)( mem + sizeof( wsw::OutputMessageStream ) );
			return new( mem )wsw::OutputMessageStream( buffer, MAX_PRINTMSG, domain, category );
		} else if( auto *mem = (uint8_t *)::malloc( kSize ) ) {
			auto *buffer = (char *)( mem + sizeof( wsw::OutputMessageStream ) );
			return new( mem )wsw::OutputMessageStream( buffer, MAX_PRINTMSG, domain, category );
		} else {
			return nullStreamFor( domain, category );
		}
	}

	[[nodiscard]]
	auto free( wsw::OutputMessageStream *stream ) {
		if( !isANullStream( stream ) ) [[likely]] {
			[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
			stream->~OutputMessageStream();
			if( m_allocator.mayOwn( stream ) ) [[likely]] {
				m_allocator.free( stream );
			} else {
				::free( stream );
			}
		}
	}
};

static MessageStreamsAllocator g_logLineStreamsAllocator;

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

auto wsw::createMessageStream( wsw::MessageDomain domain, wsw::MessageCategory category ) -> wsw::OutputMessageStream * {
	if( isMessageAcceptedByFilters( domain, category ) ) {
		return ::g_logLineStreamsAllocator.alloc( domain, category );
	}
	return ::g_logLineStreamsAllocator.nullStreamFor( domain, category );
}

void wsw::submitMessageStream( wsw::OutputMessageStream *stream ) {
	if( isMessageAcceptedByFilters( stream->m_domain, stream->m_category ) ) {
		// TODO: Eliminate Com_Printf()
		if( !::g_logLineStreamsAllocator.isANullStream( stream ) ) {
			stream->m_data[wsw::min( stream->m_limit, stream->m_offset )] = '\0';
			const auto indexForCategory = (unsigned)stream->m_category;
			assert( indexForCategory <= std::size( kPrintedMessageColorForCategory ) );
			const char *color = kPrintedMessageColorForCategory[indexForCategory];
			if( v_enableOutputDomainPrefix.initialized() && v_enableOutputDomainPrefix.get() ) {
				const auto indexForDomain = (unsigned)stream->m_domain;
				assert( indexForDomain <= std::size( g_domainTraits ) );
				const char *prefix = g_domainTraits[indexForDomain].printedPrefix;
				Com_Printf( S_COLOR_GREY "[%s] %s%s\n", prefix, color, stream->m_data );
			} else {
				Com_Printf( "%s%s\n", color, stream->m_data );
			}
		} else {
			Com_Printf( S_COLOR_RED "A null line stream was used. The line content was discarded\n" );
		}
	}
	::g_logLineStreamsAllocator.free( stream );
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