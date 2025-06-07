#ifndef WSW_8d319689_66d9_4c8c_bd74_ca751aedeea3_H
#define WSW_8d319689_66d9_4c8c_bd74_ca751aedeea3_H

#include <common/helpers/textstreamwriter.h>
#include <cassert>

class MessageStreamsAllocator;

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
	friend auto createMessageStream( MessageDomain, MessageCategory ) -> OutputMessageStream *;
	friend void submitMessageStream( OutputMessageStream * );

	friend class ::MessageStreamsAllocator;
public:
	OutputMessageStream( char *data, unsigned limit, MessageDomain domain, MessageCategory category ) noexcept
		: m_data( data ), m_limit( limit ), m_domain( domain ), m_category( category ) {}

	[[nodiscard]]
	auto reserve( size_t size ) noexcept -> char * {
		return ( m_offset + size < m_limit ) ? m_data + m_offset : nullptr;
	}

	void advance( size_t size ) noexcept {
		m_offset += (unsigned)size;
		assert( m_offset <= m_limit );
	}
private:
	char *const m_data;
	const unsigned m_limit { 0 };
	unsigned m_offset { 0 };
	const MessageDomain m_domain;
	const MessageCategory m_category;
};

[[nodiscard]]
auto createMessageStream( MessageDomain, MessageCategory ) -> OutputMessageStream *;

void submitMessageStream( OutputMessageStream * );

class PendingOutputMessage {
public:
	explicit PendingOutputMessage( wsw::OutputMessageStream *stream ) : m_stream( stream ), m_writer( stream ) {}
	~PendingOutputMessage() { submitMessageStream( m_stream ); }

	[[nodiscard]]
	auto getWriter() -> TextStreamWriter & { return m_writer; }
private:
	wsw::OutputMessageStream *const m_stream;
	wsw::TextStreamWriter m_writer;
};

}

#endif