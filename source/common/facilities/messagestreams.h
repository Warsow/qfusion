#ifndef WSW_8d319689_66d9_4c8c_bd74_ca751aedeea3_H
#define WSW_8d319689_66d9_4c8c_bd74_ca751aedeea3_H

#include <common/helpers/textstreamwriter.h>
#include <cassert>

class RegularMessageStreamsAllocator;
class FailureMessageStreamsAllocator;

namespace wsw {

enum class MessageDomain : uint8_t {
	Common,
	Server,
	Client,
	Sound,
	Renderer,
	UI,
	CGame,
	Game,
	AI,
};

enum class MessageCategory : uint8_t {
	Debug,
	Notice,
	Warning,
	Error,
};

class OutputMessageStream {
public:
	OutputMessageStream( char *data, unsigned limit ) noexcept : m_data( data ), m_limit( limit ) {}

	[[nodiscard]]
	auto reserve( size_t size ) noexcept -> char * {
		return ( m_offset + size < m_limit ) ? m_data + m_offset : nullptr;
	}

	void advance( size_t size ) noexcept {
		m_offset += (unsigned)size;
		assert( m_offset <= m_limit );
	}

	[[nodiscard]]
	auto offset() const -> unsigned { return m_offset; }
protected:
	char *const m_data;
	const unsigned m_limit { 0 };
	unsigned m_offset { 0 };
};

class RegularMessageStream : public OutputMessageStream {
	friend auto createRegularMessageStream( MessageDomain, MessageCategory ) -> RegularMessageStream *;
	friend void submitRegularMessageStream( RegularMessageStream * );

	friend class ::RegularMessageStreamsAllocator;
public:
	RegularMessageStream( char *data, unsigned limit, MessageDomain domain, MessageCategory category ) noexcept
		: OutputMessageStream( data, limit ), m_domain( domain ), m_category( category ) {}
private:
	const MessageDomain m_domain;
	const MessageCategory m_category;
};

[[nodiscard]]
auto createRegularMessageStream( MessageDomain, MessageCategory ) -> RegularMessageStream *;

void submitRegularMessageStream( RegularMessageStream * );

class PendingRegularMessage {
public:
	explicit PendingRegularMessage( wsw::RegularMessageStream *stream ) : m_stream( stream ), m_writer( stream ) {}
	~PendingRegularMessage() { submitRegularMessageStream( m_stream ); }

	[[nodiscard]]
	auto getWriter() -> TextStreamWriter & { return m_writer; }
private:
	wsw::RegularMessageStream *const m_stream;
	wsw::TextStreamWriter m_writer;
};

enum FailureKind { DropFailure, FatalFailure };

class FailureMessageStream;

[[nodiscard]]
auto createFailureMessageStream( FailureKind kind ) -> FailureMessageStream *;
[[noreturn]]
void submitFailureMessageStream( FailureMessageStream * );

class FailureMessageStream : public OutputMessageStream {
	friend class ::FailureMessageStreamsAllocator;
	friend auto createFailureMessageStream( FailureKind ) -> FailureMessageStream *;
	friend void submitFailureMessageStream( FailureMessageStream * );
public:
	FailureMessageStream( char *data, unsigned limit, FailureKind kind )
		: OutputMessageStream( data, limit ), m_kind( kind ) {}
private:
	const FailureKind m_kind;
};

class PendingFailureMessage {
public:
	explicit PendingFailureMessage( wsw::FailureMessageStream *stream ) : m_stream( stream ), m_writer( stream ) {}
	~PendingFailureMessage() { submitFailureMessageStream( m_stream ); };

	[[nodiscard]]
	auto getWriter() -> TextStreamWriter & { return m_writer; }
private:
	wsw::FailureMessageStream *const m_stream;
	wsw::TextStreamWriter m_writer;
};

}

#define failDrop() wsw::PendingFailureMessage( wsw::createFailureMessageStream( wsw::DropFailure ) ).getWriter()
#define failFatal() wsw::PendingFailureMessage( wsw::createRegularMessageStream( wsw::FatalFailure ) ).getWriter()

//
// Compat stuff
//

typedef enum {
	ERR_FATAL,      // exit the entire game with a popup window
	ERR_DROP,       // print to console and disconnect from game
} com_error_code_t;

// this is only here so the functions in q_shared.c and q_math.c can link

struct CmdArgs;

#define MAX_PRINTMSG    3072

#ifndef _MSC_VER
void Com_Printf( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
void Com_DPrintf( const char *format, ... ) __attribute__( ( format( printf, 1, 2 ) ) );
void Com_Error( com_error_code_t code, const char *format, ... ) __attribute__( ( format( printf, 2, 3 ) ) ) __attribute__( ( noreturn ) );
// TODO: Move to common/local.h
void Com_Quit( const CmdArgs & ) __attribute__( ( noreturn ) );
#else
void Com_Printf( _Printf_format_string_ const char *format, ... );
void Com_DPrintf( _Printf_format_string_ const char *format, ... );
__declspec( noreturn ) void Com_Error( com_error_code_t code, _Printf_format_string_ const char *format, ... );
// TODO: Move to common/local.h
__declspec( noreturn ) void Com_Quit( const CmdArgs & );
#endif

#define Q_COLOR_ESCAPE  '^'
#define S_COLOR_ESCAPE  "^"

#define COLOR_BLACK     '0'
#define COLOR_RED       '1'
#define COLOR_GREEN     '2'
#define COLOR_YELLOW    '3'
#define COLOR_BLUE      '4'
#define COLOR_CYAN      '5'
#define COLOR_MAGENTA   '6'
#define COLOR_WHITE     '7'
#define COLOR_ORANGE    '8'
#define COLOR_GREY      '9'
#define ColorIndex( c )   ( ( ( ( c ) - '0' ) < MAX_S_COLORS ) && ( ( ( c ) - '0' ) >= 0 ) ? ( ( c ) - '0' ) : 7 )

#define S_COLOR_BLACK   "^0"
#define S_COLOR_RED     "^1"
#define S_COLOR_GREEN   "^2"
#define S_COLOR_YELLOW  "^3"
#define S_COLOR_BLUE    "^4"
#define S_COLOR_CYAN    "^5"
#define S_COLOR_MAGENTA "^6"
#define S_COLOR_WHITE   "^7"
#define S_COLOR_ORANGE  "^8"
#define S_COLOR_GREY    "^9"

#endif