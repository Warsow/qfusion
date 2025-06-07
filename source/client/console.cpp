/*
Copyright (C) 1997-2001 Id Software, Inc.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/
// console.c

#include "client.h"
#include <common/facilities/cmdargs.h>
#include <common/facilities/cmdcompat.h>
#include <common/facilities/cmdsystem.h>
#include <common/helpers/freelistallocator.h>
#include <common/types/podbufferholder.h>
#include <common/facilities/profilerscope.h>
#include <common/helpers/singletonholder.h>
#include <common/helpers/stringsplitter.h>
#include <common/types/staticstring.h>
#include <common/types/vector.h>
#include <common/facilities/wswfs.h>

#include <cctype>
#include <tuple>
#include <typeinfo>

using wsw::operator""_asView;

CmdSystem *CL_GetCmdSystem();
void CL_Cmd_SubmitCompletionRequest( const wsw::StringView &name, unsigned requestId, const wsw::StringView &partial );
CompletionResult CL_GetPossibleCommands( const wsw::StringView &partial );
CompletionResult CL_GetPossibleAliases( const wsw::StringView &partial );
CompletionResult Cvar_CompleteBuildList( const wsw::StringView &partial );

// TODO: Generalize, lift to the top level
// We didn't initially lean to this design as it has a poor compatibility with non-POD types

template <typename T>
[[maybe_unused]]
static inline auto link( T *item, T *head ) -> T * {
	assert( item != head );

	T *const oldNext = head->next;

	item->prev = head;
	head->next = item;

	oldNext->prev = item;
	item->next    = oldNext;

	return item;
}

template <typename T>
[[maybe_unused]]
static inline auto unlink( T *item ) -> T * {
	item->prev->next = item->next;
	item->next->prev = item->prev;
	return item;
}

volatile bool con_initialized;

static bool con_hasKeyboardFocus;

static cvar_t *con_maxNotificationTime;
static cvar_t *con_maxNotificationLines;
static cvar_t *con_chatmode;

class Console {
public:
	Console();
	~Console();

	void clearLines();
	void clearNotifications();
	void clearInput();

	void drawPane( unsigned width, unsigned height );
	void drawNotifications( unsigned width, unsigned height );

	enum NotificationBehaviour : uint8_t {
		DrawNotification,
		SuppressNotification,
	};

	void addText( wsw::StringView text, NotificationBehaviour notificationBehaviour );

	void handleKeyDownEvent( int quakeKey );
	void handleCharInputEvent( int key );

	void acceptCommandCompletionResult( unsigned requestId, CompletionResult &&result );

	[[nodiscard]]
	auto dumpLinesToBuffer() const -> wsw::PodVector<char>;
private:
	static constexpr unsigned kMaxLines                = 1024u;
	static constexpr unsigned kUseDefaultHeapLineLimit = 128u;
	static constexpr unsigned kInputLengthLimit        = 256u;
	static constexpr unsigned kLineTruncationLimit     = 4u * 4096u;

	class DrawnLinesBuilder;

	struct LineEntry {
		virtual ~LineEntry() = default;

		[[nodiscard]]
		virtual auto getRequiredCapacityForDumping() const -> unsigned { wsw::failWithLogicError( "Unreachable" ); };
		[[nodiscard]]
		virtual auto dumpCharsToBuffer( char *buffer ) const -> unsigned { wsw::failWithLogicError( "Unreachable" ); };
		[[nodiscard]]
		virtual auto measureNumberOfLines( unsigned resizeId, unsigned glyphWidth,
										   unsigned widthLimit, Console *console ) const -> unsigned {
			wsw::failWithLogicError( "Unreachable" );
		};
		[[nodiscard]]
		virtual auto getCharSpansForDrawing( unsigned resizeId, unsigned glyphWidth, unsigned widthLimit,
											 Console *console, DrawnLinesBuilder *builder ) const
											 -> std::span<const wsw::StringView> {
			wsw::failWithLogicError( "Unreachable" );
		};

		LineEntry *prev { nullptr }, *next { nullptr };
	};

	struct RegularEntry final : public LineEntry {
		struct alignas( 2 ) WordSpan {
			uint16_t offset;
			uint16_t length;
			uint16_t startColor;
		};

		struct WordSpansCache {
			WordSpan *wordSpans { nullptr };
			unsigned numWordSpans { 0 };
			unsigned lastDataSize { 0 };
		};

		struct MeasureCache {
			unsigned lastResizeId { 0 };
			unsigned lastDataSize { 0 };
			unsigned numLines { 0 };
		};

		int64_t m_timestamp { 0 };
		char *m_data { nullptr };
		unsigned m_dataSize { 0 };
		unsigned m_capacity { 0 };

		mutable WordSpansCache m_wordSpansCache;
		mutable MeasureCache m_measureCache;

		NotificationBehaviour m_notificationBehaviour { DrawNotification };
		// Using this flag instead simplifies drawing/managing of new lines
		// (new nodes are going to be created only on demand, if a current entry has this flag set).
		bool m_isFrozenForAddition { false };
		// Either the limits were reached, or reallocation failure did not let add characters to the line
		bool m_isTruncated { false };

		~RegularEntry() override { std::free( m_wordSpansCache.wordSpans ); }

		[[nodiscard]]
		auto getRequiredCapacityForDumping() const -> unsigned override;
		[[nodiscard]]
		auto dumpCharsToBuffer( char *buffer ) const -> unsigned override;
		[[nodiscard]]
		auto measureNumberOfLines( unsigned resizeId, unsigned width, unsigned widthLimit, Console * ) const -> unsigned override;
		[[nodiscard]]
		auto getCharSpansForDrawing( unsigned resizeId, unsigned glyphWidth, unsigned widthLimit,
									 Console *, DrawnLinesBuilder *builder ) const -> std::span<const wsw::StringView> override;
		[[nodiscard]]
		auto getWordSpans() const -> std::span<const WordSpan>;
	};

	struct CompletionEntry final: public LineEntry {
		explicit CompletionEntry( CompletionResult &&completionResult )
			: m_completionResult( std::forward<CompletionResult>( completionResult ) ) {}

		CompletionResult m_completionResult;
		char *m_requestData { nullptr };
		char *m_headingData { nullptr };
		unsigned m_requestDataSize { 0 };
		unsigned m_headingDataSize { 0 };
		unsigned m_requestId { 0 };
		enum CompletionMode { Name, Arg } m_completionMode { Name };

		struct MeasureCache {
			unsigned resizeId { 0 };
			unsigned numColumns { 0 };
			unsigned maxTokenLength { 0 };
			bool fitsASingleLine { false };
		};

		mutable MeasureCache m_measureCache;

		[[nodiscard]]
		auto getRequiredCapacityForDumping() const -> unsigned override;
		[[nodiscard]]
		auto dumpCharsToBuffer( char *buffer ) const -> unsigned override;
		[[nodiscard]]
		auto measureNumberOfLines( unsigned resizeId, unsigned glyphWidth, unsigned widthLimit,
								   Console *console ) const -> unsigned override;
		[[nodiscard]]
		auto getCharSpansForDrawing( unsigned resizeId, unsigned glyphWidth, unsigned widthLimit,
									 Console *console, DrawnLinesBuilder *builder ) const
									 -> std::span<const wsw::StringView> override;

		[[nodiscard]]
		bool checkIfFitsSingleLine( unsigned glyphWidth, unsigned widthLimit ) const;

		void updateMeasureCache( MeasureCache *cache, unsigned resizeId, unsigned glyphWidth, unsigned widthLimit ) const;
	};

	// This should be faster than regular dynamic_cast<>, and also this design prevents it from spreading over codebase

	template <typename T>
	[[nodiscard]]
	static auto entryAs( LineEntry *lineEntry ) -> T * {
		static_assert( std::is_base_of_v<LineEntry, T> && std::is_final_v<T> );
		return lineEntry && typeid( *lineEntry ) == typeid( T ) ? static_cast<T *>( lineEntry ) : nullptr;
	};

	template <typename T>
	[[nodiscard]]
	static auto entryAs( const LineEntry *lineEntry ) -> const T * {
		static_assert( std::is_base_of_v<LineEntry, T> && std::is_final_v<T> );
		return lineEntry && typeid( *lineEntry ) == typeid( T ) ? static_cast<const T *>( lineEntry ) : nullptr;
	}

	struct HistoryEntry {
		HistoryEntry *prev { nullptr }, *next { nullptr };
		char *data { nullptr };
		unsigned dataSize { 0 };
	};

	void addCharsToCurrentLine( wsw::StringView chars );
	void startNewLine( NotificationBehaviour notificationBehaviour, size_t sizeHint );
	void destroyLineEntry( LineEntry *entry );
	void destroyAllLines();
	void destroyOldestLineIfNeeded();

	void addToHistory( wsw::StringView line );
	void setCurrHistoryEntry( HistoryEntry *historyEntry );

	[[nodiscard]]
	bool isAwaitingAsyncCompletion() const;

	void addCompletionEntry( unsigned requestId, const wsw::StringView &originalRequest,
							 CompletionEntry::CompletionMode intendedMode, char color,
							 const wsw::StringView &itemName, CompletionResult &&completionResult );

	[[nodiscard]]
	auto getNextCompletionRequestId();

	void handleSubmitKeyAction();
	void handleCompleteKeyAction();
	void handleBackspaceKeyAction();
	void handleStepLeftKeyAction();
	void handleStepRightKeyAction();
	void handlePositionCursorAtStartAction();
	void handlePositionCursorAtEndAction();
	void handleDeleteKeyAction();
	void handleHistoryUpKeyAction();
	void handleHistoryDownKeyAction();
	void handleClipboardCopyKeyAction();
	void handleClipboardPasteKeyAction();
	void handleScrollUpKeyAction();
	void handleScrollDownKeyAction();
	void handlePositionPageAtStartAction();
	void handlePositionPageAtEndAction();

	wsw::HeapBasedFreelistAllocator m_linesAllocator { sizeof( RegularEntry ) + kUseDefaultHeapLineLimit + 1, kMaxLines };
	wsw::HeapBasedFreelistAllocator m_historyAllocator { sizeof( HistoryEntry ) + kInputLengthLimit + 1, 32 };

	mutable wsw::Mutex m_mutex;

	// TODO: That's why headnode-based design is painful for non-POD types
	// TODO: Extract Linux-like container_of()-based utilities? That won't function that good for non-POD types as well.
	struct : public LineEntry {} m_lineEntriesHeadnode {};

	HistoryEntry m_historyEntriesHeadnode {};

	HistoryEntry *m_currHistoryEntry { nullptr };

	unsigned m_numLines { 0 };
	unsigned m_paneResizeId { 1 };
	unsigned m_lastPaneWidth { 0 };
	unsigned m_lastPaneHeight { 0 };

	unsigned m_requestedLineNumOffset { 0 };

	wsw::StaticString<kInputLengthLimit> m_inputLine;
	unsigned m_inputPos { 0 };

	unsigned m_completionRequestIdsCounter { 0 };
	unsigned m_lastAsyncCompletionRequestId { 0 };
	unsigned m_lastAsyncCompletionKeepLength { 0 };
	unsigned m_lastAsyncCompletionFullLength { 0 };
	int64_t m_lastAsyncCompletionRequestAt { 0 };

	class DrawnLinesBuilder {
	public:
		void clear();
		void addShallowCopyOfCompleteLine( const wsw::StringView &line );
		void addCharsToCurrentLine( const wsw::StringView &chars );
		void addCharsToCurrentLine( char color, const wsw::StringView &chars );
		void addCharsWithPrefixHighlight( const wsw::StringView &chars, unsigned prefixLen,
										  char prefixColor, char bodyStartColor, char bodyColor );
		void padCurrentLineByChars( char ch, unsigned count );
		void completeCurrentLine();
		[[nodiscard]]
		auto getFinalSpans() -> std::span<const wsw::StringView>;
	private:
		wsw::PodVector<wsw::StringView> m_tmpSpans;
		wsw::PodVector<unsigned> m_tmpOffsets;
		wsw::PodVector<char> m_tmpChars;
		unsigned m_charsSizeAtLineStart { 0 };
	};

	DrawnLinesBuilder m_drawnLinesBuilder;
	wsw::PodVector<char> m_tmpCompletionTokenPrefixColors;
};

static SingletonHolder<Console> g_console;

Console::Console() {
	m_lineEntriesHeadnode.prev = &m_lineEntriesHeadnode;
	m_lineEntriesHeadnode.next = &m_lineEntriesHeadnode;

	m_historyEntriesHeadnode.prev = &m_historyEntriesHeadnode;
	m_historyEntriesHeadnode.next = &m_historyEntriesHeadnode;
}

Console::~Console() {
	destroyAllLines();

	for( HistoryEntry *entry = m_historyEntriesHeadnode.next, *next; entry != &m_historyEntriesHeadnode; entry = next ) {
		next = entry->next;
		entry->~HistoryEntry();
		m_historyAllocator.free( entry );
	}
}

void Console::destroyAllLines() {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	for( LineEntry *entry = m_lineEntriesHeadnode.next, *next; entry != &m_lineEntriesHeadnode; entry = next ) {
		next = entry->next;
		destroyLineEntry( entry );
	}

	assert( m_lineEntriesHeadnode.prev = &m_lineEntriesHeadnode );
	assert( m_lineEntriesHeadnode.next = &m_lineEntriesHeadnode );
}

void Console::destroyOldestLineIfNeeded() {
	if( m_numLines == kMaxLines ) {
		assert( m_lineEntriesHeadnode.prev != &m_lineEntriesHeadnode );
		LineEntry *oldestEntry = unlink( m_lineEntriesHeadnode.prev );
		assert( oldestEntry );
		destroyLineEntry( oldestEntry );
	}
}

void Console::clearLines() {
	destroyAllLines();
	m_requestedLineNumOffset = 0;
}

void Console::clearNotifications() {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	for( LineEntry *entry = m_lineEntriesHeadnode.next; entry != &m_lineEntriesHeadnode; entry = entry->next ) {
		if( RegularEntry *const regularEntry = entryAs<RegularEntry>( entry ) ) {
			if( regularEntry->m_notificationBehaviour == DrawNotification ) {
				regularEntry->m_timestamp = std::numeric_limits<decltype( regularEntry->m_timestamp )>::min();
			}
		}
	}
}

void Console::clearInput() {
	m_inputLine.clear();
	m_inputPos = 0;

	// Discard pending completion results, if any
	m_lastAsyncCompletionRequestId  = 0;
	m_lastAsyncCompletionRequestAt  = 0;
	m_lastAsyncCompletionKeepLength = 0;
	m_lastAsyncCompletionFullLength = 0;
}

// TODO: This is not really correct, we should try matching "\r\n" sequence first.
// Luckily, practically everything uses just a single "\n".
static wsw::CharLookup kSeparatorChars( wsw::StringView( "\r\n" ) );

void Console::addText( wsw::StringView text, NotificationBehaviour notificationBehaviour ) {
	if( text.empty() ) [[unlikely]] {
		return;
	}

	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> mutexLock( &m_mutex );

	wsw::StringSplitter splitter( text );
	while( const auto maybeLine = splitter.getNext( kSeparatorChars, wsw::StringSplitter::AllowEmptyTokens ) ) {
		const bool newCharsEndWithLineFeed = maybeLine->data() + maybeLine->length() < text.data() + text.size();
		wsw::StringView charsToAdd;
		if( newCharsEndWithLineFeed ) {
			charsToAdd = maybeLine->trimRight();
		} else {
			charsToAdd = *maybeLine;
		}

		if( const RegularEntry *const regularLineEntry = entryAs<RegularEntry>( m_lineEntriesHeadnode.next ) ) {
			if( regularLineEntry->m_isFrozenForAddition ) {
				startNewLine( notificationBehaviour, charsToAdd.size() );
			}
		} else {
			startNewLine( notificationBehaviour, charsToAdd.size() );
		}

		addCharsToCurrentLine( charsToAdd );

		assert( entryAs<RegularEntry>( m_lineEntriesHeadnode.next ) != nullptr );

		// Could change during execution of the loop step
		auto *const actualCurrLineEntry = static_cast<RegularEntry *>( m_lineEntriesHeadnode.next );

		if( notificationBehaviour != SuppressNotification ) {
			// Bump notification properties in this case
			actualCurrLineEntry->m_notificationBehaviour = notificationBehaviour;
			actualCurrLineEntry->m_timestamp             = cls.realtime;
		}
		if( newCharsEndWithLineFeed ) {
			actualCurrLineEntry->m_isFrozenForAddition = true;
		}
	}
}

void Console::addCharsToCurrentLine( wsw::StringView chars ) {
	assert( entryAs<RegularEntry>( m_lineEntriesHeadnode.next ) != nullptr );

	auto *lineEntry = static_cast<RegularEntry *>( m_lineEntriesHeadnode.next );
	assert( (LineEntry *)lineEntry != (LineEntry *)&m_lineEntriesHeadnode );

	// Don't try adding extra characters in this case, even if we may manage to allocate memory at this point.
	if( lineEntry->m_isTruncated ) [[unlikely]] {
		return;
	}

	assert( lineEntry->m_dataSize <= kLineTruncationLimit );
	if( lineEntry->m_dataSize + chars.size() > kLineTruncationLimit ) {
		const size_t numCharsToAdd = kLineTruncationLimit - lineEntry->m_dataSize;
		chars = chars.take( numCharsToAdd );
		lineEntry->m_isTruncated = true;
	}

	// This also handles an empty original argument, as well as truncation
	if( chars.empty() ) [[unlikely]] {
		return;
	}

	// The addition of characters is going to invalidate word spans.
	// Also, this is the single real operation ~RegularLineEntry() does, so we are free to forget calling it.
	std::free( lineEntry->m_wordSpansCache.wordSpans );
	lineEntry->m_wordSpansCache.wordSpans = nullptr;

	if( lineEntry->m_dataSize + chars.size() < lineEntry->m_capacity ) [[likely]] {
		std::memcpy( lineEntry->m_data + lineEntry->m_dataSize, chars.data(), chars.size() );
		lineEntry->m_dataSize += chars.size();
		lineEntry->m_data[lineEntry->m_dataSize] = '\0';
	} else {
		LineEntry *const prev = lineEntry->prev;
		LineEntry *const next = lineEntry->next;

		RegularEntry *newEntry = nullptr;
		// The buffer for short lines has a fixed size, so the capacity of short lines can't grow.
		// Allocate a new line on the default heap in this case.
		if( m_linesAllocator.mayOwn( lineEntry ) ) {
			void *mem = std::malloc( sizeof( RegularEntry ) + lineEntry->m_dataSize + chars.size() + 1 );
			if( !mem ) [[unlikely]] {
				lineEntry->m_isTruncated = true;
				return;
			}

			newEntry                          = new( mem )RegularEntry;
			newEntry->m_data                  = (char *)( newEntry + 1 );
			newEntry->m_dataSize              = lineEntry->m_dataSize + chars.size();
			newEntry->m_capacity              = lineEntry->m_dataSize + chars.size();
			newEntry->m_timestamp             = lineEntry->m_timestamp;
			newEntry->m_notificationBehaviour = lineEntry->m_notificationBehaviour;

			std::memcpy( newEntry->m_data, lineEntry->m_data, lineEntry->m_dataSize );
			std::memcpy( newEntry->m_data + lineEntry->m_dataSize, chars.data(), chars.size() );
			newEntry->m_data[newEntry->m_dataSize] = '\0';

			m_linesAllocator.free( lineEntry );
		} else {
			unsigned newCapacity = ( 3u * ( lineEntry->m_dataSize + chars.size() ) ) / 2;
			// Don't let it allocate extra bytes as the data never grows beyond this limit
			if( newCapacity > kLineTruncationLimit ) {
				newCapacity = kLineTruncationLimit;
			}

			// Make a backup of fields that should be preserved.
			// Note that formatting cache fields would get reset to a default state.
			const auto oldDataSize              = lineEntry->m_dataSize;
			const auto oldTimestamp             = lineEntry->m_timestamp;
			const auto oldNotificationBehaviour = lineEntry->m_notificationBehaviour;

			// Try reallocating the underlying chunk
			void *mem = std::realloc( (void *)lineEntry, sizeof( RegularEntry ) + newCapacity + 1 );
			if( !mem ) [[unlikely]] {
				lineEntry->m_isTruncated = true;
				return;
			}

			// Construct a new object in the same place.
			// Patch the data pointer, so it points right after the entry header again.
			newEntry         = new( mem )RegularEntry;
			newEntry->m_data = ( char *)( newEntry + 1 );

			newEntry->m_dataSize              = oldDataSize + chars.size();
			newEntry->m_capacity              = newCapacity;
			newEntry->m_timestamp             = oldTimestamp;
			newEntry->m_notificationBehaviour = oldNotificationBehaviour;

			// Copy only new characters (old ones were relocated)
			std::memcpy( newEntry->m_data + oldDataSize, chars.data(), chars.size() );
			newEntry->m_data[newEntry->m_dataSize] = '\0';
		}

		// Patch links, including siblings
		prev->next     = newEntry;
		newEntry->prev = prev;

		next->prev     = newEntry;
		newEntry->next = next;

		lineEntry = newEntry;
	}

	assert( lineEntry->m_data[lineEntry->m_dataSize] == '\0' );
	assert( wsw::StringView( lineEntry->m_data, lineEntry->m_dataSize ).endsWith( chars ) );
}

void Console::startNewLine( NotificationBehaviour notificationBehaviour, size_t sizeHint ) {
	destroyOldestLineIfNeeded();

	assert( !m_linesAllocator.isFull() );

	void *newEntryMem = nullptr;
	unsigned capacity = kUseDefaultHeapLineLimit;
	// Allocate directly in the default heap from the beginning
	if( sizeHint > kUseDefaultHeapLineLimit ) {
		newEntryMem = std::malloc( sizeof( RegularEntry ) + sizeHint + 1 );
		// Fall back to the freelist allocator (let a truncation happen later)
		if( !newEntryMem ) [[unlikely]] {
			newEntryMem = m_linesAllocator.allocOrNull();
		} else {
			capacity = sizeHint;
		}
	} else {
		newEntryMem = m_linesAllocator.allocOrNull();
	}

	auto *const newEntry              = new( newEntryMem )RegularEntry;
	newEntry->m_data                  = (char *)( newEntry + 1 );
	newEntry->m_dataSize              = 0;
	newEntry->m_capacity              = capacity;
	newEntry->m_timestamp             = cls.realtime;
	newEntry->m_notificationBehaviour = notificationBehaviour;

	// Make sure that our assumptions on tight packing are valid
	assert( (uintptr_t)newEntry->m_data == (uintptr_t)newEntryMem + sizeof( RegularEntry ) );

	link( (LineEntry *)newEntry, (LineEntry *)&m_lineEntriesHeadnode );
	m_numLines++;

	if( m_requestedLineNumOffset ) {
		if( m_requestedLineNumOffset < std::numeric_limits<decltype( m_requestedLineNumOffset )>::max() ) {
			m_requestedLineNumOffset++;
		}
	}
}

void Console::destroyLineEntry( LineEntry *entry ) {
	unlink( entry );

	m_numLines--;

	entry->~LineEntry();
	// We assume that all but maybe regular line entries are allocated in the default heap
	if( m_linesAllocator.mayOwn( entry ) ) {
		m_linesAllocator.free( entry );
	} else {
		std::free( entry );
	}
}

[[nodiscard]]
static auto calcNumVisibleCodePoints( std::span<const char> utf8Chars ) -> unsigned {
	const char *s = utf8Chars.data();
	unsigned result = 0;

	while( s < utf8Chars.data() + utf8Chars.size() ) {
		wchar_t c;
		const int gc = Q_GrabWCharFromColorString( &s, &c, NULL );
		if( gc == GRABCHAR_CHAR ) {
			result++;
		} else if( gc == GRABCHAR_COLOR ) {
			;
		} else if( gc == GRABCHAR_END ) {
			break;
		} else {
			assert( 0 );
		}
	}

	return result;
}

[[nodiscard]]
static auto calcPrefixLenForNumVisibleCodePoints( const char *utf8Chars, unsigned numCodePoints ) -> unsigned {
	const char *start = utf8Chars;

	while( *utf8Chars && numCodePoints ) {
		wchar_t c;
		const int gc = Q_GrabWCharFromColorString( &utf8Chars, &c, NULL );
		if( gc == GRABCHAR_CHAR ) {
			numCodePoints--;
		} else if( gc == GRABCHAR_COLOR ) {
			;
		} else if( gc == GRABCHAR_END ) {
			break;
		} else {
			assert( 0 );
		}
	}

	return (unsigned)( utf8Chars - start );
}

void Con_DrawNotify( unsigned width, unsigned height ) {
	if( con_initialized ) {
		g_console.instance()->drawNotifications( width, height );
	}
}

void Con_DrawConsole( unsigned width, unsigned height ) {
	if( con_initialized ) {
		g_console.instance()->drawPane( width, height );
	}
}

extern const vec4_t kConsoleBackgroundColor { 0.10f, 0.05f, 0.17f, 0.7f };

void Console::drawPane( unsigned width, unsigned height ) {
	WSW_PROFILER_SCOPE();

	const int smallCharHeight = SCR_FontHeight( cls.consoleFont );
	if( !smallCharHeight ) {
		return;
	}

	const int pixelRatio   = VID_GetPixelRatio();
	const int sideMargin   = 8 * pixelRatio;
	if( width < (unsigned)sideMargin ) {
		return;
	}

	if( m_lastPaneWidth != width || m_lastPaneHeight != height ) {
		m_lastPaneWidth  = width;
		m_lastPaneHeight = height;
		m_paneResizeId++;
		if( m_paneResizeId == 0 ) [[unlikely]] {
			m_paneResizeId++;
		}
	}

	// get date from system
	time_t long_time;
	time( &long_time );
	struct tm *newtime = localtime( &long_time );

	char version[256];
	Q_snprintfz( version, sizeof( version ), "%02d:%02d %s v%4.2f", newtime->tm_hour, newtime->tm_min, APPLICATION, APP_VERSION );

	if( strlen( APP_VERSION_STAGE ) > 0 ) {
		Q_strncatz( version, " ", sizeof( version ) );
		Q_strncatz( version, APP_VERSION_STAGE, sizeof( version ) );
	}

	R_Set2DMode( true );

	// draw the background
	R_DrawStretchPic( 0, 0, width, height, 0, 0, 1, 1, kConsoleBackgroundColor, cls.whiteShader );

	const int bottomLineHeight = 2 * pixelRatio;
	SCR_DrawFillRect( 0, height - bottomLineHeight, width, bottomLineHeight, colorOrange );

	const int versionMargin = 4 * pixelRatio;
	const int versionX      = width - SCR_strWidth( version, cls.consoleFont, 0, 0 ) - versionMargin;
	const int versionY      = height - SCR_FontHeight( cls.consoleFont ) - versionMargin;
	SCR_DrawString( versionX, versionY, ALIGN_LEFT_TOP, version, cls.consoleFont, colorOrange, 0 );

	// Draw the input prompt, user text, and cursor if desired

	const int promptWidth         = SCR_strWidth( "]", cls.consoleFont, 1, 0 );
	const int inputWidth          = width - sideMargin * 2 - promptWidth - SCR_strWidth( "_", cls.consoleFont, 1, 0 );
	const int inputReservedHeight = 14 * pixelRatio + smallCharHeight;

	const int inputX = sideMargin + promptWidth;
	const int inputY = height - inputReservedHeight;

	const int inputTextWidth = SCR_strWidth( m_inputLine.data(), cls.consoleFont, 0, 0 );
	const int preWidth = m_inputPos ? SCR_strWidth( m_inputLine.data(), cls.consoleFont, m_inputPos, 0 ) : 0;

	int inputPrestep = 0;
	if( inputTextWidth > inputWidth ) {
		// don't let the cursor go beyond the left screen edge
		clamp_high( inputPrestep, preWidth );
		// don't let it go beyond the right screen edge
		clamp_low( inputPrestep, preWidth - inputWidth );
		// don't leave an empty space after the string when deleting a character
		if( ( inputTextWidth - inputPrestep ) < inputWidth ) {
			inputPrestep = inputTextWidth - inputWidth;
		}
	}

	SCR_DrawRawChar( inputX - promptWidth, inputY, ']', cls.consoleFont, colorWhite );

	SCR_DrawClampString( inputX - inputPrestep, inputY, m_inputLine.data(), inputX, inputY,
						 inputX + inputWidth, inputY + 2 * smallCharHeight, cls.consoleFont, colorWhite, 0 );

	if( (int)( cls.realtime >> 8 ) & 1 ) {
		SCR_DrawRawChar( inputX + preWidth - inputPrestep, inputY, '_', cls.consoleFont, colorWhite );
	}

	int lineY = height - smallCharHeight - inputReservedHeight;
	int minY  = 0;
	if( const int rem = lineY % smallCharHeight ) {
		const int extraHeight = smallCharHeight - rem;
		lineY -= extraHeight / 2;
		// Make sure that the calculations are exact
		minY = extraHeight - ( extraHeight / 2 );
	}

	const unsigned lineWidthLimit = width - 2 * sideMargin;

	// Lock during drawing lines

	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	if( m_requestedLineNumOffset ) {
		// Check how many visual lines are going to be there
		// This should be relatively fast due to caching
		unsigned totalNumLines = 0;
		for( LineEntry *entry = m_lineEntriesHeadnode.next; entry != &m_lineEntriesHeadnode; entry = entry->next ) {
			totalNumLines += entry->measureNumberOfLines( m_paneResizeId, promptWidth, lineWidthLimit, this );
		}

		// Patch it each frame
		const unsigned numFittingLines = lineY / smallCharHeight;
		if( totalNumLines > numFittingLines ) {
			if( m_requestedLineNumOffset > totalNumLines - numFittingLines ) {
				m_requestedLineNumOffset = totalNumLines - numFittingLines;
			}
		} else {
			m_requestedLineNumOffset = 0;
		}
	}

	if( m_requestedLineNumOffset ) {
		const int arrowWidth   = SCR_strWidth( "^", cls.consoleFont, 0, 0 );
		const int arrowSpacing = 3 * arrowWidth;

		if( lineY >= minY ) {
			// draw arrows to show the buffer is backscrolled
			for( unsigned x = arrowSpacing; x + arrowSpacing <= width; x += arrowSpacing ) {
				SCR_DrawRawChar( x, lineY, '^', cls.consoleFont, colorOrange );
			}
			// the arrows obscure one line of scrollback
			lineY -= smallCharHeight;
		}
	}

	// TODO: Get rid of this copying, allow supplying spans directly
	static wsw::PodVector<char> scrDrawStringBuffer;

	// draw from the bottom up
	unsigned numSkippedLines = 0;
	for( const LineEntry *entry = m_lineEntriesHeadnode.next; entry != &m_lineEntriesHeadnode; entry = entry->next ) {
		const unsigned numEntryLines = entry->measureNumberOfLines( m_paneResizeId, promptWidth, lineWidthLimit, this );
		if( numSkippedLines + numEntryLines <= m_requestedLineNumOffset ) {
			numSkippedLines += numEntryLines;
		} else {
			std::span<const wsw::StringView> spansToDraw = entry->getCharSpansForDrawing( m_paneResizeId, promptWidth,
																						  lineWidthLimit, this,
																						  &m_drawnLinesBuilder );
			assert( spansToDraw.size() == numEntryLines );
			unsigned numLinesToSkip = 0;
			if( numSkippedLines < m_requestedLineNumOffset ) {
				numLinesToSkip = m_requestedLineNumOffset - numSkippedLines;
				numSkippedLines += numLinesToSkip;
			}

			// TODO: Account for partial display
			auto it = std::make_reverse_iterator( spansToDraw.end() );
			std::advance( it, numLinesToSkip );

			const auto end = std::make_reverse_iterator( spansToDraw.begin() );
			for(; it != end; ++it ) {
				if( lineY < minY ) {
					break;
				}
				scrDrawStringBuffer.assign( it->data(), it->size() );
				scrDrawStringBuffer.push_back( '\0' );
				SCR_DrawString( sideMargin, lineY, ALIGN_LEFT_TOP, scrDrawStringBuffer.data(), cls.consoleFont, colorWhite, 0 );
				lineY -= smallCharHeight;
			}
			if( lineY < minY ) {
				break;
			}
		}
	}
}

auto Console::RegularEntry::getRequiredCapacityForDumping() const -> unsigned {
	if( m_dataSize ) {
		if( m_dataSize > 1 && m_data[m_dataSize - 2] == '\r' && m_data[m_dataSize - 1] == '\n' ) {
			return m_dataSize;
		}
		return m_dataSize + 2;
	}
	return 0;
}

auto Console::RegularEntry::dumpCharsToBuffer( char *buffer ) const -> unsigned {
	if( m_dataSize ) {
		std::memcpy( buffer, m_data, m_dataSize );
		if( m_dataSize > 1 && m_data[m_dataSize - 2] == '\r' && m_data[m_dataSize - 1] == '\n' ) {
			return m_dataSize;
		}
		buffer[m_dataSize + 0] = '\r';
		buffer[m_dataSize + 1] = '\n';
		return m_dataSize + 2;
	}
	return 0;
}

auto Console::RegularEntry::measureNumberOfLines( unsigned resizeId, unsigned glyphWidth,
												  unsigned widthLimit, Console * ) const -> unsigned {
	if( m_dataSize * glyphWidth <= widthLimit ) [[likely]] {
		return 1;
	}

	if( m_measureCache.lastResizeId == resizeId && m_measureCache.lastDataSize == m_dataSize ) {
		return m_measureCache.numLines;
	}

	unsigned numLines          = 1;
	unsigned lineWidthSoFar    = 0;
	unsigned prevSpanEndOffset = 0;
	for( const WordSpan &wordSpan: getWordSpans() ) {
		const unsigned spanWidth    = glyphWidth * wordSpan.length;
		const unsigned charsBetween = wordSpan.offset - prevSpanEndOffset;
		const unsigned advance      = spanWidth + glyphWidth * charsBetween;
		if( lineWidthSoFar + advance <= widthLimit ) [[likely]] {
			lineWidthSoFar += advance;
		} else if( spanWidth <= widthLimit ) [[likely]] {
			lineWidthSoFar = spanWidth;
			++numLines;
		} else {
			lineWidthSoFar += advance;
		}
		prevSpanEndOffset = wordSpan.offset + wordSpan.length;
	}

	assert( m_dataSize <= std::numeric_limits<uint16_t>::max() );
	assert( numLines <= std::numeric_limits<unsigned>::max() );

	m_measureCache = MeasureCache { .lastResizeId = resizeId, .lastDataSize = m_dataSize, .numLines = numLines };
	return numLines;
}

void Console::DrawnLinesBuilder::clear() {
	m_tmpOffsets.clear();
	m_tmpChars.clear();
	m_tmpSpans.clear();

	m_charsSizeAtLineStart = 0;
}

void Console::DrawnLinesBuilder::addShallowCopyOfCompleteLine( const wsw::StringView &line ) {
	// Make sure that the dynamically built line is empty/is complete
	assert( m_charsSizeAtLineStart == m_tmpChars.size() );
	m_tmpOffsets.push_back( ~0u );
	m_tmpSpans.push_back( line );
}

void Console::DrawnLinesBuilder::addCharsToCurrentLine( const wsw::StringView &chars ) {
	m_tmpChars.insert( m_tmpChars.end(), chars.begin(), chars.end() );
}

void Console::DrawnLinesBuilder::addCharsToCurrentLine( char color, const wsw::StringView &chars ) {
	m_tmpChars.push_back( '^' );
	m_tmpChars.push_back( color );
	m_tmpChars.insert( m_tmpChars.end(), chars.begin(), chars.end() );
}

void Console::DrawnLinesBuilder::addCharsWithPrefixHighlight( const wsw::StringView &chars, unsigned prefixLen,
															  char prefixColor, char bodyStartColor, char bodyColor ) {
	m_tmpChars.push_back( '^' );
	m_tmpChars.push_back( prefixColor );
	addCharsToCurrentLine( chars.take( prefixLen ) );
	if( chars.size() > prefixLen ) [[likely]] {
		m_tmpChars.push_back( '^' );
		m_tmpChars.push_back( bodyStartColor );
		m_tmpChars.push_back( chars[prefixLen] );
		if( chars.size() > prefixLen + 1 ) [[likely]] {
			m_tmpChars.push_back( '^' );
			m_tmpChars.push_back( bodyColor );
			addCharsToCurrentLine( chars.drop( prefixLen + 1 ) );
		}
	}
}

void Console::DrawnLinesBuilder::padCurrentLineByChars( char ch, unsigned count ) {
	m_tmpChars.insert( m_tmpChars.end(), count, ch );
}

void Console::DrawnLinesBuilder::completeCurrentLine() {
	m_tmpOffsets.push_back( m_charsSizeAtLineStart );
	m_tmpSpans.push_back( { m_tmpChars.data() + m_charsSizeAtLineStart, m_tmpChars.size() - m_charsSizeAtLineStart } );
	m_charsSizeAtLineStart = m_tmpChars.size();
}

auto Console::DrawnLinesBuilder::getFinalSpans() -> std::span<const wsw::StringView> {
	assert( m_charsSizeAtLineStart == m_tmpChars.size() );
	assert( m_tmpOffsets.size() == m_tmpSpans.size() );

	// If there were chars added to this buffer, it's also likely that there were reallocations.
	// Note: A match of initial and final data pointers doesn't guarantee that there weren't reallocations in-between.
	// TODO: Use a custom vector type that counts reallocations
	if( !m_tmpChars.empty() ) {
		for( size_t i = 0; i < m_tmpSpans.size(); ++i ) {
			// Patch pointers in string views that point to the tmpChars buffer
			if( const unsigned maybeOffset = m_tmpOffsets[i]; maybeOffset != ~0u ) {
				const wsw::StringView oldSpan = m_tmpSpans[i];
				m_tmpSpans[i] = wsw::StringView( m_tmpChars.data() + maybeOffset, oldSpan.size() );
			}
		}
	}
	return m_tmpSpans;
}

auto Console::RegularEntry::getCharSpansForDrawing( unsigned resizeId, unsigned glyphWidth, unsigned widthLimit,
													Console *, DrawnLinesBuilder *drawnLinesBuilder ) const
													-> std::span<const wsw::StringView> {
	drawnLinesBuilder->clear();

	if( m_dataSize * glyphWidth <= widthLimit ) [[likely]] {
		// Submit data as-is
		drawnLinesBuilder->addShallowCopyOfCompleteLine( wsw::StringView( m_data, m_dataSize ) );
		return drawnLinesBuilder->getFinalSpans();
	}

	(void)resizeId;
	// TODO: Share with measureNumberOfLines(), modify lastResizeId?

	unsigned lineWidthSoFar    = 0;
	unsigned prevSpanEndOffset = 0;
	unsigned lineStartOffset   = 0;
	int lineStartColorIndex    = COLOR_WHITE - '0';
	for( const WordSpan &wordSpan: getWordSpans() ) {
		const unsigned spanWidth    = glyphWidth * wordSpan.length;
		const unsigned charsBetween = wordSpan.offset - prevSpanEndOffset;
		const unsigned widthAdvance = spanWidth + glyphWidth * charsBetween;
		if( lineWidthSoFar + widthAdvance <= widthLimit ) [[likely]] {
			lineWidthSoFar += widthAdvance;
		} else if( spanWidth <= widthLimit ) [[likely]] {
			assert( lineStartOffset <= prevSpanEndOffset );
			const size_t lineLength = prevSpanEndOffset - lineStartOffset;
			if( lineStartColorIndex == COLOR_WHITE - '0' ) {
				drawnLinesBuilder->addShallowCopyOfCompleteLine( wsw::StringView( m_data + lineStartOffset, lineLength ) );
			} else {
				const char prefix[2] = { '^', (char)( lineStartColorIndex + '0' ) };
				drawnLinesBuilder->addCharsToCurrentLine( { prefix, 2 } );
				drawnLinesBuilder->addCharsToCurrentLine( { m_data + lineStartOffset, lineLength } );
				drawnLinesBuilder->completeCurrentLine();
			}

			// Start new line
			lineStartOffset     = wordSpan.offset;
			lineWidthSoFar      = spanWidth;
			lineStartColorIndex = wordSpan.startColor;
		} else {
			lineWidthSoFar += widthAdvance;
		}
		prevSpanEndOffset = wordSpan.offset + wordSpan.length;
	}

	// Handle the last line (checking after the loop)
	if( lineStartColorIndex == COLOR_WHITE - '0' ) {
		drawnLinesBuilder->addShallowCopyOfCompleteLine( { m_data + lineStartOffset, m_dataSize - lineStartOffset } );
	} else {
		const char prefix[2] = { '^', (char)( lineStartColorIndex + '0' ) };
		drawnLinesBuilder->addCharsToCurrentLine( { prefix, 2 } );
		drawnLinesBuilder->addCharsToCurrentLine( { m_data + lineStartOffset, m_dataSize - lineStartOffset } );
		drawnLinesBuilder->completeCurrentLine();
	}

	return drawnLinesBuilder->getFinalSpans();
}

auto Console::RegularEntry::getWordSpans() const -> std::span<const Console::RegularEntry::WordSpan> {
	if( m_wordSpansCache.lastDataSize == m_dataSize ) {
		assert( m_wordSpansCache.wordSpans != nullptr || m_dataSize == 0 );
		return { m_wordSpansCache.wordSpans, m_wordSpansCache.numWordSpans };
	}

	assert( m_dataSize <= std::numeric_limits<uint16_t>::max() );
	assert( m_data[m_dataSize] == '\0' );

	// TODO: Implement a custom wsw::Vector template that allows transferring the underlying buffer ownership
	// TODO: What to do in case of allocation failure?
	PodBufferHolder<WordSpan> builtSpansHolder;
	builtSpansHolder.reserve( 32u );
	unsigned numBuiltSpans = 0;

	const char *p = m_data;
	assert( *p != '\0' );

	bool isInsideAWord      = true;
	const char *wordStart   = p;

	int lastColorIndex      = COLOR_WHITE - '0';
	int wordStartColorIndex = lastColorIndex;
	for(;; ) {
		wchar_t wchar     = 0;
		int colorIndex    = 0;
		const char *oldp  = p;
		const int grabRes = Q_GrabWCharFromColorString( &p, &wchar, &colorIndex );

		if( grabRes == GRABCHAR_CHAR ) {
			assert( p - oldp > 0 );
			assert( oldp - wordStart >= 0 );
			if( Q_IsBreakingSpaceChar( wchar ) ) {
				if( isInsideAWord ) {
					if( oldp - wordStart > 0 ) {
						// Note: growth to a ceil power of 2 is perfectly fine there
						const size_t countToReserve = wsw::ceilPowerOf2( numBuiltSpans + 1 );
						void *mem = builtSpansHolder.reserveAndGet( countToReserve ) + numBuiltSpans;
						new( mem )WordSpan {
							.offset     = (uint16_t)( wordStart - m_data ),
							.length     = (uint16_t)( oldp - wordStart ),
							.startColor = (uint16_t)wordStartColorIndex
						};
						numBuiltSpans++;
					}
					isInsideAWord = false;
				}
			} else {
				if( !isInsideAWord ) {
					isInsideAWord       = true;
					wordStart           = oldp;
					wordStartColorIndex = lastColorIndex;
				}
			}
		} else if( grabRes == GRABCHAR_COLOR ) {
			// Just update the currently tracked color
			assert( p - oldp > 0 );
			lastColorIndex = colorIndex;
		} else {
			break;
		}
	}
	if( isInsideAWord ) {
		void *mem = builtSpansHolder.reserveAndGet( numBuiltSpans + 1 ) + numBuiltSpans;
		new( mem )WordSpan {
			.offset     = (uint16_t)( wordStart - m_data ),
			.length     = (uint16_t)( p - wordStart ),
			.startColor = (uint16_t)wordStartColorIndex,
		};
		numBuiltSpans++;
	}

	assert( numBuiltSpans <= std::numeric_limits<unsigned>::max() );

	m_wordSpansCache = WordSpansCache {
		.wordSpans    = builtSpansHolder.releaseOwnership(),
		.numWordSpans = numBuiltSpans,
		.lastDataSize = m_dataSize,
	};
	return { m_wordSpansCache.wordSpans, m_wordSpansCache.numWordSpans };
}

auto Console::CompletionEntry::getRequiredCapacityForDumping() const -> unsigned {
	unsigned result = m_headingDataSize + 2;
	if( !m_completionResult.empty() ) {
		for( const wsw::StringView &token: m_completionResult ) {
			result += token.size() + 1;
		}
		result++;
	}
	return result;
}

auto Console::CompletionEntry::dumpCharsToBuffer( char *buffer ) const -> unsigned {
	unsigned offset = m_headingDataSize + 2;
	std::memcpy( buffer, m_headingData, m_headingDataSize );
	buffer[offset - 2] = '\r';
	buffer[offset - 1] = '\n';

	if( !m_completionResult.empty() ) {
		for( const wsw::StringView &token: m_completionResult ) {
			std::memcpy( buffer + offset, token.data(), token.size() );
			offset += token.size() + 1;
			buffer[offset - 1] = ' ';
		}

		if( !m_completionResult.empty() ) {
			buffer[offset - 1] = '\r';
			buffer[offset - 0] = '\n';
		}
	}

	return offset;
}

bool Console::CompletionEntry::checkIfFitsSingleLine( unsigned glyphWidth, unsigned widthLimit ) const {
	unsigned totalWidth = 0;
	for( auto it = m_completionResult.cbegin(); it != m_completionResult.cend(); ) {
		totalWidth += glyphWidth * ( *it ).length();
		if( totalWidth > widthLimit ) {
			return false;
		}
		// TODO: Make the iterator support random access/advance, so we don't need this in the body
		++it;
		if( totalWidth == widthLimit && it != m_completionResult.cend() ) {
			return false;
		}
		// Account for trailing space
		totalWidth += glyphWidth;
	}
	return true;
}

void Console::CompletionEntry::updateMeasureCache( MeasureCache *cache, unsigned resizeId,
												   unsigned glyphWidth, unsigned widthLimit ) const {
	if( cache->resizeId != resizeId ) {
		// Reset all fields to prevent misuse
		*cache = MeasureCache { .resizeId = resizeId };

		cache->fitsASingleLine = checkIfFitsSingleLine( glyphWidth, widthLimit );
		if( !cache->fitsASingleLine ) {
			unsigned maxTokenLength = 0;
			for( const wsw::StringView &token: m_completionResult ) {
				maxTokenLength = wsw::max<unsigned>( maxTokenLength, token.length() );
			}
			// TODO: This calculation assumes a trailing space after the last column
			if( glyphWidth * ( maxTokenLength + 1 ) < widthLimit / 2 ) [[likely]] {
				cache->numColumns     = widthLimit / ( glyphWidth * ( maxTokenLength + 1 ) );
				cache->maxTokenLength = maxTokenLength;
			}
		}
	}
}

auto Console::CompletionEntry::measureNumberOfLines( unsigned resizeId, unsigned glyphWidth,
													 unsigned widthLimit, Console * ) const -> unsigned {
	updateMeasureCache( &m_measureCache, resizeId, glyphWidth, widthLimit );
	if( m_measureCache.fitsASingleLine ) {
		return 2;
	}
	if( m_measureCache.numColumns ) {
		unsigned numRows = m_completionResult.size() / m_measureCache.numColumns;
		if( numRows * m_measureCache.numColumns < m_completionResult.size() ) {
			numRows++;
		}
		return 1 + numRows;
	}
	return 1 + m_completionResult.size();
}

[[nodiscard]]
static auto takeCommandLikePrefix( const wsw::StringView &text ) -> wsw::StringView {
	return text.takeWhile( []( char ch ) { return (size_t)ch > (size_t)' ' && ch != ';'; } );
}

auto Console::CompletionEntry::getCharSpansForDrawing( unsigned resizeId, unsigned glyphWidth, unsigned widthLimit,
													   Console *console, DrawnLinesBuilder *builder ) const
													   -> std::span<const wsw::StringView> {
	updateMeasureCache( &m_measureCache, resizeId, glyphWidth, widthLimit );

	builder->clear();
#if 1
	builder->addShallowCopyOfCompleteLine( wsw::StringView( m_headingData, m_headingDataSize ) );
#else
	builder->addCharsToCurrentLine( { m_headingData, m_headingDataSize } );
	builder->addCharsToCurrentLine( COLOR_WHITE, wsw::StringView( " (valid for `" ) );
	builder->addCharsToCurrentLine( { m_requestData, m_requestDataSize } );
	builder->addCharsToCurrentLine( { "`)", 1 } );
	builder->completeCurrentLine();
#endif

	struct ColorTracker {
		char lastCharUpper { '\0' };
		unsigned lastColorIndex { ~0u };
		[[nodiscard]]
		char nextBodyColorForToken( const wsw::StringView &token, unsigned prefixLen ) {
			constexpr char allowedColors[] { COLOR_GREEN, COLOR_YELLOW, COLOR_CYAN, COLOR_MAGENTA, COLOR_ORANGE };
			if( token.length() > prefixLen ) [[likely]] {
				if( const char currCharUpper = std::toupper( token[prefixLen] ); currCharUpper != lastCharUpper ) {
					lastColorIndex = ( lastColorIndex + 1 ) % std::size( allowedColors );
					lastCharUpper  = currCharUpper;
				}
				return allowedColors[lastColorIndex];
			} else {
				lastColorIndex = ( lastColorIndex + 1 ) % std::size( allowedColors );
				return COLOR_BLACK;
			}
		}
	} colorTracker;

	bool shouldAttemptToHighlight = false;
	// If the entry belongs to the last completion attempt
	// TODO: This condition breaks if the counter is used for things that do not add CompletionEntry instances
	if( m_requestId == console->m_completionRequestIdsCounter ) {
		// The completion entry must be the most recent one in the console.
		// Iterate for all entries, starting from the newest one.
		const LineEntry *entry = console->m_lineEntriesHeadnode.next;
		for(; entry != &console->m_lineEntriesHeadnode; entry = entry->next ) {
			if( entry == this ) {
				shouldAttemptToHighlight = true;
				break;
			}
			// Keep iterating if other entries in front of this one belong to the same completion request.
			const auto *const otherCompletionEntry = entryAs<const CompletionEntry>( entry );
			if( !otherCompletionEntry || otherCompletionEntry->m_requestId != m_requestId ) {
				break;
			}
		}
	}

	[[maybe_unused]] wsw::StringView inputToMatch      = console->m_inputLine.asView();
	[[maybe_unused]] bool isHighlightingPrefixes       = false;
	[[maybe_unused]] bool shouldPruneBroadAlterantives = false;
	[[maybe_unused]] unsigned highlightedPrefixLen     = 0;

	if( shouldAttemptToHighlight ) {
		[[maybe_unused]] wsw::StringView originalRequest = wsw::StringView { m_requestData, m_requestDataSize };
		// "Normalize" for comparisons
		if( originalRequest.startsWith( '\\' ) || originalRequest.startsWith( '/' ) ) {
			originalRequest = originalRequest.drop( 1 );
		}
		if( inputToMatch.startsWith( '\\' ) || inputToMatch.startsWith( '/' ) ) {
			inputToMatch = inputToMatch.drop( 1 );
		}

		if( m_completionMode == CompletionMode::Name ) {
			// Make sure the request was the same or more broad than the input line
			if( inputToMatch.startsWith( originalRequest, wsw::IgnoreCase ) ) {
				// Make sure there's a single command-like token without trailing characters
				if( takeCommandLikePrefix( inputToMatch ).length() == inputToMatch.length() ) {
					highlightedPrefixLen         = inputToMatch.length();
					isHighlightingPrefixes       = true;
					shouldPruneBroadAlterantives = inputToMatch.length() > originalRequest.length();
				}
			}
		} else if( m_completionMode == CompletionMode::Arg ) {
			const wsw::StringView &originalPrefix = takeCommandLikePrefix( originalRequest );
			const wsw::StringView &inputPrefix    = takeCommandLikePrefix( inputToMatch );
			// Make sure the commands/command-like tokens match
			if( originalPrefix.equalsIgnoreCase( inputPrefix ) ) {
				originalRequest = originalRequest.drop( originalPrefix.length() ).trim();
				inputToMatch    = inputToMatch.drop( inputPrefix.length() ).trim();
				// Make sure the argument request is the same or more broad than the input line
				if( inputToMatch.startsWith( originalRequest, wsw::IgnoreCase ) ) {
					highlightedPrefixLen         = inputToMatch.length();
					isHighlightingPrefixes       = true;
					shouldPruneBroadAlterantives = inputToMatch.length() > originalRequest.length();
				}
			}
		} else {
			wsw::failWithLogicError( "Unreachable" );
		}
	}

	[[maybe_unused]] const auto addHighlightedTokenTrackingColors = [&]( const wsw::StringView &token ) {
		bool shouldHighlight = true;
		// Check whether the input line mismatches the token
		if( shouldPruneBroadAlterantives ) {
			shouldHighlight = token.startsWith( inputToMatch, wsw::IgnoreCase );
		}
		if( shouldHighlight ) {
			const char bodyStartColor = colorTracker.nextBodyColorForToken( token, highlightedPrefixLen );
			builder->addCharsWithPrefixHighlight( token, highlightedPrefixLen, COLOR_WHITE, bodyStartColor, COLOR_GREY );
		} else {
			builder->addCharsToCurrentLine( COLOR_BLACK, token );
		}
	};

	if( m_measureCache.fitsASingleLine ) {
		for( const wsw::StringView &token: m_completionResult ) {
			if( isHighlightingPrefixes ) {
				addHighlightedTokenTrackingColors( token );
			} else {
				builder->addCharsToCurrentLine( token );
			}
			builder->padCurrentLineByChars( ' ', 1 );
		}
		builder->completeCurrentLine();
	} else if( m_measureCache.numColumns ) {
		unsigned numRows = m_completionResult.size() / m_measureCache.numColumns;
		if( numRows * m_measureCache.numColumns < m_completionResult.size() ) {
			numRows++;
		}
		[[maybe_unused]] const auto addHighlightedTokenUsingColorsTable = [&]( const wsw::StringView &token, unsigned index ) {
			bool shouldHighlight = true;
			// Check whether the input line mismatches the token
			if( shouldPruneBroadAlterantives ) {
				shouldHighlight = token.startsWith( inputToMatch, wsw::IgnoreCase );
			}
			if( shouldHighlight ) {
				const char bodyStartColor = console->m_tmpCompletionTokenPrefixColors[index];
				builder->addCharsWithPrefixHighlight( token, highlightedPrefixLen, COLOR_WHITE, bodyStartColor, COLOR_GREY );
			} else {
				builder->addCharsToCurrentLine( COLOR_BLACK, token );
			}
		};
		if( isHighlightingPrefixes ) {
			console->m_tmpCompletionTokenPrefixColors.clear();
			for( const wsw::StringView &token: m_completionResult ) {
				const char bodyStartColor = colorTracker.nextBodyColorForToken( token, highlightedPrefixLen );
				console->m_tmpCompletionTokenPrefixColors.push_back( bodyStartColor );
			}
		}
		for( unsigned rowNum = 0; rowNum < numRows; ++rowNum ) {
			for( unsigned columnNum = 0; columnNum < m_measureCache.numColumns; ++columnNum ) {
				// Note: The stride is the number of rows in this layout
				const unsigned tokenIndex = columnNum * numRows + rowNum;
				if( tokenIndex >= m_completionResult.size() ) [[unlikely]] {
					break;
				}
				const wsw::StringView &token = m_completionResult[tokenIndex];
				if( isHighlightingPrefixes ) {
					addHighlightedTokenUsingColorsTable( token, tokenIndex );
				} else {
					builder->addCharsToCurrentLine( token );
				}
				builder->padCurrentLineByChars( ' ', m_measureCache.maxTokenLength + 1 - token.length() );
			}
			builder->completeCurrentLine();
		}
	} else {
		for( const wsw::StringView &token: m_completionResult ) {
			if( isHighlightingPrefixes ) {
				addHighlightedTokenTrackingColors( token );
			} else {
				builder->addCharsToCurrentLine( token );
			}
			builder->completeCurrentLine();
		}
	}
	return builder->getFinalSpans();
}

void Console::drawNotifications( unsigned width, unsigned height ) {
	WSW_PROFILER_SCOPE();

	const auto maxLines = (unsigned)con_maxNotificationLines->integer;
	if( maxLines <= 0 ) {
		return;
	}

	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	const int64_t minTimestamp = cls.realtime - 1000 * wsw::max( 0, con_maxNotificationTime->integer );

	// TODO: Allow specifying a filter to match?
	wsw::StaticVector<const RegularEntry *, kMaxLines> matchingLines;
	for( const LineEntry *entry = m_lineEntriesHeadnode.next; entry != &m_lineEntriesHeadnode; entry = entry->next ) {
		if( const RegularEntry *regularEntry = entryAs<RegularEntry>( entry ) ) {
			if( regularEntry->m_notificationBehaviour == Console::DrawNotification ) {
				if( regularEntry->m_timestamp >= minTimestamp && regularEntry->m_dataSize > 0 ) {
					matchingLines.push_back( regularEntry );
					if( matchingLines.size() == maxLines ) {
						break;
					}
				}
			}
		}
	}

	if( matchingLines.empty() ) {
		return;
	}

	R_Set2DMode( true );

	const int pixelRatio  = VID_GetPixelRatio();
	const size_t fontHeight = SCR_FontHeight( cls.consoleFont );

	if( const size_t totalHeight = matchingLines.size() * fontHeight; totalHeight > height ) {
		const size_t extraLines = totalHeight / fontHeight;
		matchingLines.erase( matchingLines.begin(), matchingLines.begin() + extraLines );
	}

	int textY = 0;
	const int textX = 8 * pixelRatio;
	for( const RegularEntry *line: matchingLines ) {
		// TODO: Is it guaranteed to be zero-terminated?
		SCR_DrawString( textX, textY, ALIGN_LEFT_TOP, line->m_data, cls.consoleFont, colorWhite, 0 );
		textY += fontHeight;
	}
}

void Console::addToHistory( wsw::StringView line ) {
	line = line.trim();

	assert( line.size() <= kInputLengthLimit );
	if( line.empty() ) [[unlikely]] {
		return;
	}

	void *mem;
	if( m_historyAllocator.isFull() ) {
		assert( m_historyEntriesHeadnode.prev != &m_historyEntriesHeadnode );
		HistoryEntry *const oldestEntry = m_historyEntriesHeadnode.prev;
		oldestEntry->~HistoryEntry();
		mem = oldestEntry;
	} else {
		mem = m_historyAllocator.allocOrNull();
	}

	auto *const newEntry = new( mem )HistoryEntry;
	auto *const chars    = (char *)( newEntry + 1 );

	line.copyTo( chars, kInputLengthLimit );

	newEntry->data     = chars;
	newEntry->dataSize = line.size();

	link( newEntry, &m_historyEntriesHeadnode );
}

void Console::setCurrHistoryEntry( HistoryEntry *historyEntry ) {
	m_currHistoryEntry = historyEntry;
	if( historyEntry ) {
		m_inputLine.assign( historyEntry->data, historyEntry->dataSize );
	} else {
		m_inputLine.clear();
	}
	m_inputPos = m_inputLine.size();
}

auto Console::getNextCompletionRequestId() {
	unsigned result = ++m_completionRequestIdsCounter;
	// Never let it go to zero due to wrapping
	if( result == 0 ) [[unlikely]] {
		result = ++m_completionRequestIdsCounter;
	}
	return result;
}

void Console::addCompletionEntry( unsigned requestId, const wsw::StringView &originalRequest,
								  CompletionEntry::CompletionMode intendedMode, char color,
								  const wsw::StringView &itemName, CompletionResult &&completionResult ) {
	wsw::StaticString<16> countPrefix;
	countPrefix << completionResult.size() << ' ';
	wsw::StringView suffix( "s available:" );
	// Just use "s" to make plural forms of all actual arguments
	if( completionResult.size() == 1 ) {
		suffix = suffix.drop( 1 );
	}

	size_t allocationSize = sizeof( CompletionEntry );
	allocationSize += originalRequest.size() + 1;
	allocationSize += 2 + countPrefix.size() + itemName.size() + suffix.size() + 1;

	if( void *mem = std::malloc( allocationSize ) ) [[likely]] {
		// Moving the completion result won't fail
		auto *const newEntry       = new( mem )CompletionEntry( std::forward<CompletionResult>( completionResult ) );
		newEntry->m_requestId      = requestId;
		newEntry->m_completionMode = intendedMode;

		newEntry->m_requestData     = (char *)( newEntry + 1 );
		newEntry->m_requestDataSize = originalRequest.size();
		originalRequest.copyTo( newEntry->m_requestData, newEntry->m_requestDataSize + 1 );
		assert( newEntry->m_requestData[newEntry->m_requestDataSize] == '\0' );

		newEntry->m_headingData     = newEntry->m_requestData + newEntry->m_requestDataSize + 1;
		newEntry->m_headingData[0]  = '^';
		newEntry->m_headingData[1]  = color;
		newEntry->m_headingDataSize = 2;

		countPrefix.copyTo( newEntry->m_headingData + newEntry->m_headingDataSize, countPrefix.size() + 1 );
		newEntry->m_headingDataSize += countPrefix.size();
		itemName.copyTo( newEntry->m_headingData + newEntry->m_headingDataSize, itemName.size() + 1 );
		newEntry->m_headingDataSize += itemName.size();
		suffix.copyTo( newEntry->m_headingData + newEntry->m_headingDataSize, suffix.size() + 1 );
		newEntry->m_headingDataSize += suffix.size();
		assert( newEntry->m_headingData[newEntry->m_headingDataSize] == '\0' );

		destroyOldestLineIfNeeded();

		link( (LineEntry *)newEntry, (LineEntry *)&m_lineEntriesHeadnode );
		m_numLines++;
	}
}

[[nodiscard]]
static bool isExactlyACommand( const wsw::HashedStringView &text ) {
	return CL_GetCmdSystem()->isARegisteredCommand( text );
}

[[nodiscard]]
static bool isExactlyAVar( const wsw::HashedStringView &text ) {
	if( text.size() <= MAX_STRING_CHARS ) {
		if( text.isZeroTerminated() ) {
			return Cvar_Find( text.data() );
		}
		return Cvar_Find( wsw::StaticString<MAX_STRING_CHARS>( text ).data() );
	}
	return false;
}

[[nodiscard]]
static bool isExactlyAnAlias( const wsw::HashedStringView &text ) {
	return CL_GetCmdSystem()->isARegisteredAlias( text );
}

bool Console::isAwaitingAsyncCompletion() const {
	return m_lastAsyncCompletionRequestAt + 500 > cls.realtime;
}

void Console::acceptCommandCompletionResult( unsigned requestId, CompletionResult &&result ) {
	// Timed out/obsolete?
	if( m_lastAsyncCompletionRequestId != requestId ) {
		return;
	}

	if( !result.empty() ) {
		// Sanity checks TODO: Track modification id's for robustness?
		if( m_lastAsyncCompletionFullLength == m_inputLine.length() && m_inputLine.length() < kInputLengthLimit ) {
			// Sanity checks
			if( m_lastAsyncCompletionKeepLength <= m_lastAsyncCompletionFullLength ) {
				const unsigned maxCharsToAdd = kInputLengthLimit - m_inputLine.size();
				m_inputLine.erase( m_lastAsyncCompletionKeepLength );
				m_inputLine.append( ' ' );
				if( result.size() == 1 ) {
					m_inputLine.append( result.front().take( maxCharsToAdd ) );
				} else {
					m_inputLine.append( result.getCommonPrefix().take( maxCharsToAdd ) );
					[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
					// TODO: Keep an original request within a completion result?
					wsw::StringView originalRequest = m_inputLine.asView().take( m_lastAsyncCompletionFullLength );
					if( originalRequest.startsWith( '\\' ) || originalRequest.startsWith( '/' ) ) {
						originalRequest = originalRequest.drop( 1 );
					}
					addCompletionEntry( requestId, originalRequest, CompletionEntry::Arg,
										COLOR_GREEN, "argument"_asView, std::forward<CompletionResult>( result ) );
				}
				m_inputPos = m_inputLine.size();
			}
		}
	}

	m_lastAsyncCompletionRequestId  = 0;
	m_lastAsyncCompletionRequestAt  = 0;
	m_lastAsyncCompletionKeepLength = 0;
	m_lastAsyncCompletionFullLength = 0;
}

void Console::handleCompleteKeyAction() {
	if( isAwaitingAsyncCompletion() ) {
		return;
	}

	// Don't make a completion attempt while the cursor is in the middle of the line (should we?)
	if( m_inputPos != m_inputLine.size() ) {
		return;
	}
	if( m_inputLine.full() ) [[unlikely]] {
		return;
	}

	bool droppedFirstChar     = false;
	wsw::StringView inputText = m_inputLine.asView();
	if( inputText.startsWith( '/' ) || inputText.startsWith( '\\' ) ) {
		inputText        = inputText.drop( 1 );
		droppedFirstChar = true;
	}

	const wsw::HashedStringView maybeCommandLikePrefix( takeCommandLikePrefix( inputText ) );
	if( maybeCommandLikePrefix.empty() ) [[unlikely]] {
		return;
	}

	if( isExactlyACommand( maybeCommandLikePrefix ) ) {
		const wsw::StringView &partial = inputText.drop( maybeCommandLikePrefix.size() ).trim();

		m_lastAsyncCompletionRequestId  = getNextCompletionRequestId();
		m_lastAsyncCompletionRequestAt  = cls.realtime;
		m_lastAsyncCompletionKeepLength = maybeCommandLikePrefix.size() + ( droppedFirstChar ? 1 : 0 );
		m_lastAsyncCompletionFullLength = m_inputLine.size();

		CL_Cmd_SubmitCompletionRequest( maybeCommandLikePrefix, m_lastAsyncCompletionRequestId, partial );
	} else if( isExactlyAVar( maybeCommandLikePrefix ) ) {
		// TODO: Implement completion for command values?
		;
	} else if( isExactlyAnAlias( maybeCommandLikePrefix ) ) {
		// TODO: Could aliases accept args?
		;
	} else if( maybeCommandLikePrefix.size() == inputText.size() ) {
		CompletionResult cmdCompletionResult   = CL_GetPossibleCommands( maybeCommandLikePrefix );
		CompletionResult varCompletionResult   = Cvar_CompleteBuildList( maybeCommandLikePrefix );
		CompletionResult aliasCompletionResult = CL_GetPossibleAliases( maybeCommandLikePrefix );

		wsw::StaticVector<std::tuple<CompletionResult *, wsw::StringView, char>, 3> nonEmptyResults;
		if( !cmdCompletionResult.empty() ) {
			nonEmptyResults.push_back( std::make_tuple( &cmdCompletionResult, "command"_asView, COLOR_MAGENTA ) );
		}
		if( !varCompletionResult.empty() ) {
			nonEmptyResults.push_back( std::make_tuple( &varCompletionResult, "var"_asView, COLOR_CYAN ) );
		}
		if( !aliasCompletionResult.empty() ) {
			nonEmptyResults.push_back( std::make_tuple( &aliasCompletionResult, "alias"_asView, COLOR_ORANGE ) );
		}

		if( nonEmptyResults.empty() ) {
			clNotice() << "No matching aliases, commands or vars were found";
		} else {
			const auto replaceInputBy = [&]( const wsw::StringView &charsToUse ) {
				m_inputLine.erase( droppedFirstChar ? 1 : 0 );
				m_inputLine.append( charsToUse.take( kInputLengthLimit - m_inputLine.size() ) );
				m_inputPos = m_inputLine.size();
			};

			bool didAnInstantCompletion = false;
			if( nonEmptyResults.size() == 1 ) {
				if( const auto *completionResult = std::get<0>( nonEmptyResults.front() ); completionResult->size() == 1 ) {
					replaceInputBy( completionResult->front() );
					didAnInstantCompletion = true;
				}
			}

			if( !didAnInstantCompletion ) {
				wsw::StringView commonPrefix = std::get<0>( nonEmptyResults.front() )->getCommonPrefix();
				if( !commonPrefix.empty() ) {
					for( unsigned i = 1; i < nonEmptyResults.size(); ++i ) {
						const wsw::StringView &nextPrefix = std::get<0>( nonEmptyResults[i] )->getCommonPrefix();
						commonPrefix = commonPrefix.take( commonPrefix.getCommonPrefixLength( nextPrefix, wsw::IgnoreCase ) );
						if( commonPrefix.empty() ) {
							break;
						}
					}
				}
				if( !commonPrefix.empty() ) {
					// If the common prefix of results adds something to the current input
					if( maybeCommandLikePrefix.length() < commonPrefix.length() ) {
						replaceInputBy( commonPrefix );
					}
				}

				// Make sure they are identified by the same request id, so they are highlighted interactively as a whole
				const unsigned requestId = getNextCompletionRequestId();
				// Add up to 3 entries in an atomic fashion
				[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
				for( auto &nonEmptyResultTuple: nonEmptyResults ) {
					CompletionResult *completionResult = std::get<0>( nonEmptyResultTuple );
					const wsw::StringView &itemDesc    = std::get<1>( nonEmptyResultTuple );
					const char color                   = std::get<2>( nonEmptyResultTuple );
					addCompletionEntry( requestId, maybeCommandLikePrefix, CompletionEntry::Name, color,
										itemDesc, std::forward<CompletionResult>( *completionResult ) );
				}
			}
		}
	}
}

void Console::handleBackspaceKeyAction() {
	if( m_inputPos > 0 ) {
		// TODO: Restore the old behaviour that allows breaking in the middle of a color token?
		handleStepLeftKeyAction();
		handleDeleteKeyAction();
	}
}

void Console::handleStepLeftKeyAction() {
	if( m_inputPos > 0 ) {
		if( const unsigned numCodePoints = calcNumVisibleCodePoints( { m_inputLine.data(), m_inputPos } ) ) {
			m_inputPos = calcPrefixLenForNumVisibleCodePoints( m_inputLine.data(), numCodePoints - 1 );
		}
	}
}

void Console::handleStepRightKeyAction() {
	if( m_inputPos < m_inputLine.size() ) {
		const unsigned numCodePoints = calcNumVisibleCodePoints( { m_inputLine.data(), m_inputPos } );
		m_inputPos = calcPrefixLenForNumVisibleCodePoints( m_inputLine.data(), numCodePoints + 1 );
	}
}

void Console::handlePositionCursorAtStartAction() {
	m_inputPos = 0;
}

void Console::handlePositionCursorAtEndAction() {
	m_inputPos = m_inputLine.size();
}

void Console::handleDeleteKeyAction() {
	if( m_inputPos < m_inputLine.size() ) {
		char *s = m_inputLine.data() + m_inputPos;
		if( Q_GrabWCharFromUtf8String( (const char **)&s ) ) {
			assert( s && s > m_inputLine.data() + m_inputPos );
			const ptrdiff_t utf8SpanLength = s - ( m_inputLine.data() + m_inputPos );
			m_inputLine.erase( m_inputPos, (unsigned)( utf8SpanLength ) );
		}
	}
}

void Console::handleHistoryUpKeyAction() {
	HistoryEntry *historyEntry = nullptr;
	if( m_currHistoryEntry ) {
		if( m_currHistoryEntry->next != &m_historyEntriesHeadnode ) {
			historyEntry = m_currHistoryEntry->next;
		}
	} else {
		if( m_historyEntriesHeadnode.next != &m_historyEntriesHeadnode ) {
			historyEntry = m_historyEntriesHeadnode.next;
		}
	}
	setCurrHistoryEntry( historyEntry );
}

void Console::handleHistoryDownKeyAction() {
	HistoryEntry *historyEntry = nullptr;
	if( m_currHistoryEntry ) {
		if( m_currHistoryEntry->prev != &m_historyEntriesHeadnode ) {
			historyEntry = m_currHistoryEntry->prev;
		}
	} else {
		if( m_historyEntriesHeadnode.prev != &m_historyEntriesHeadnode ) {
			historyEntry = m_historyEntriesHeadnode.prev;
		}
	}
	setCurrHistoryEntry( historyEntry );
}

void Console::handleScrollUpKeyAction() {
	if( m_requestedLineNumOffset < std::numeric_limits<decltype( m_requestedLineNumOffset )>::max() ) {
		m_requestedLineNumOffset++;
	}
}

void Console::handleScrollDownKeyAction() {
	if( m_requestedLineNumOffset > 0 ) {
		m_requestedLineNumOffset--;
	}
}

void Console::handlePositionPageAtStartAction() {
	m_requestedLineNumOffset = std::numeric_limits<decltype( m_requestedLineNumOffset )>::max();
}

void Console::handlePositionPageAtEndAction() {
	m_requestedLineNumOffset = 0;
}

void Console::handleClipboardCopyKeyAction() {
	const wsw::PodVector<char> &lines = dumpLinesToBuffer();
	CL_SetClipboardData( lines.data() );
}

void Console::handleClipboardPasteKeyAction() {
	assert( m_inputPos <= m_inputLine.size() );

	bool shouldFlushLine  = false;
	char *const clipboardData = CL_GetClipboardData();

	const wsw::CharLookup separators { wsw::StringView( "\r\n" ) };
	wsw::StringSplitter splitter { wsw::StringView( clipboardData ) };
	while( const std::optional<wsw::StringView> maybeLine = splitter.getNext( separators ) ) {
		if( const wsw::StringView &trimmedLine = maybeLine->trim(); !trimmedLine.empty() ) {
			if( shouldFlushLine ) {
				// Flush it TODO make this more clear
				handleSubmitKeyAction();
				assert( m_inputLine.empty() && m_inputPos == 0 );
			}
			size_t maxCharsToInsert = m_inputLine.capacity() - m_inputLine.size();
			if( !maxCharsToInsert ) {
				// Flush if we can't insert even a single character
				handleSubmitKeyAction();
				assert( m_inputLine.empty() && m_inputPos == 0 );
				maxCharsToInsert = m_inputLine.capacity();
			}
			const size_t numCharsToInsert = wsw::min( maxCharsToInsert, trimmedLine.size() );
			m_inputLine.insert( m_inputLine.begin() + m_inputPos, trimmedLine.take( numCharsToInsert ) );
			m_inputPos = m_inputLine.size();
			// Allow the last line stay in the input buffer
			shouldFlushLine = true;
		}
	}
	CL_FreeClipboardData( clipboardData );
}

[[nodiscard]]
static inline bool isKeyDown( int key ) {
	return wsw::cl::KeyHandlingSystem::instance()->isKeyDown( key );
}

[[nodiscard]]
static bool isCtrlKeyDown() { return isKeyDown( K_LCTRL ) || isKeyDown( K_RCTRL ); }

void Console::handleSubmitKeyAction() {
	enum { Command, Chat, Teamchat } type;

	if( cls.state <= CA_HANDSHAKE || cls.demoPlayer.playing ) {
		type = Command;
	} else if( m_inputLine.startsWith( '\\') || m_inputLine.startsWith( '/' ) ) {
		type = Command;
	} else if( isCtrlKeyDown() ) {
		type = Teamchat;
	} else if( con_chatmode && con_chatmode->integer == 1 ) {
		type = Chat;
	} else {
		const wsw::HashedStringView prefix( takeCommandLikePrefix( m_inputLine.asView() ) );
		if( isExactlyACommand( prefix ) || isExactlyAVar( prefix ) || isExactlyAnAlias( prefix ) ) {
			type = Command;
		} else {
			type = Chat;
		}
	}

	// do appropriate action
	if( type != Command ) {
		if( !m_inputLine.asView().trimLeft().empty() ) {
			if( type == Teamchat ) {
				Con_SendTeamChatMessage( m_inputLine.asView() );
			} else {
				Con_SendCommonChatMessage( m_inputLine.asView() );
			}
		}
	} else {
		const unsigned skipLen = ( m_inputLine.startsWith( '\\' ) || m_inputLine.startsWith( '/' ) ) ? 1 : 0;
		if( skipLen < m_inputLine.size() ) {
			CL_Cbuf_AppendCommand( m_inputLine.data() + skipLen );
		}
	}

	// echo to the console and cycle command history
	clNotice() << m_inputLine;

	addToHistory( m_inputLine.asView() );

	setCurrHistoryEntry( nullptr );
}

void Console::handleKeyDownEvent( int key ) {
	if( isAwaitingAsyncCompletion() ) {
		return;
	}

	if( ( key == K_INS || key == KP_INS ) && ( isKeyDown( K_LSHIFT ) || isKeyDown( K_RSHIFT ) ) ) {
		return handleClipboardPasteKeyAction();
	}
	if( key == K_ENTER || key == KP_ENTER ) {
		return handleSubmitKeyAction();
	}
	if( key == K_TAB ) {
		return handleCompleteKeyAction();
	}
	if( key == K_LEFTARROW || key == KP_LEFTARROW ) {
		return handleStepLeftKeyAction();
	}
	if( key == K_BACKSPACE ) {
		return handleBackspaceKeyAction();
	}
	if( key == K_DEL ) {
		return handleDeleteKeyAction();
	}
	if( key == K_RIGHTARROW || key == KP_RIGHTARROW ) {
		return handleStepRightKeyAction();
	}
	if( key == K_UPARROW || key == KP_UPARROW ) {
		return handleHistoryUpKeyAction();
	}
	if( key == K_DOWNARROW || key == KP_DOWNARROW ) {
		return handleHistoryDownKeyAction();
	}
	if( key == K_PGUP || key == KP_PGUP || key == K_MWHEELUP ) { // wsw : pb : support mwheel in console
		return handleScrollUpKeyAction();
	}
	if( key == K_PGDN || key == KP_PGDN || key == K_MWHEELDOWN ) { // wsw : pb : support mwheel in console
		return handleScrollDownKeyAction();
	}
	if( key == K_HOME || key == KP_HOME ) {
		return isCtrlKeyDown() ? handlePositionPageAtStartAction() : handlePositionCursorAtStartAction();
	}
	if( key == K_END || key == KP_END ) {
		return isCtrlKeyDown() ? handlePositionPageAtEndAction() : handlePositionCursorAtEndAction();
	}

	// key is a normal printable key normal which wil be HANDLE later in response to WM_CHAR event
}

void Console::handleCharInputEvent( int key ) {
	if( isAwaitingAsyncCompletion() ) {
		return;
	}

	switch( key ) {
		case 22: // CTRL+V
			return handleClipboardPasteKeyAction();
		case 12: // CTRL+L
			return CL_Cbuf_AppendCommand( "clear" );
		case 16: // CTRL+P
			return handleHistoryUpKeyAction();
		case 14: // CTRL+N
			return handleHistoryDownKeyAction();
		case 3: // CTRL+C
			return handleClipboardCopyKeyAction();
		case 1: // CTRL+A
			return handlePositionCursorAtStartAction();
		case 5: // CTRL+E
			return handlePositionCursorAtEndAction();
		default:
			break;
	}

	if( ( key >= ' ' && key < 0xFFFF ) && m_inputPos < m_inputLine.capacity() ) {
		const char *utf = Q_WCharToUtf8Char( key );
		const size_t utflen = std::strlen( utf );

		if( m_inputLine.size() + utflen < m_inputLine.capacity() ) {
			m_inputLine.insert( m_inputLine.data() + m_inputPos, wsw::StringView( utf, utflen ) );
			m_inputPos += utflen;
		}
	}
}

auto Console::dumpLinesToBuffer() const -> wsw::PodVector<char> {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	size_t bufferSize = 0;
	for( LineEntry *entry = m_lineEntriesHeadnode.prev; entry != &m_lineEntriesHeadnode; entry = entry->prev ) {
		bufferSize += entry->getRequiredCapacityForDumping();
	}

	wsw::PodVector<char> result;
	result.resize( bufferSize + 1 );

	char *writePtr = result.data();
	for( LineEntry *entry = m_lineEntriesHeadnode.prev; entry != &m_lineEntriesHeadnode; entry = entry->prev ) {
		const unsigned advance = entry->dumpCharsToBuffer( writePtr );
		writePtr += advance;
	}

	result.back() = '\0';
	return result;
}

[[nodiscard]]
static auto translateNumPadKey( int key ) -> int {
	switch( key ) {
		case KP_SLASH: return '/';
		case KP_STAR: return '*';
		case KP_MINUS: return '-';
		case KP_PLUS: return '+';
		case KP_HOME: return '7';
		case KP_UPARROW: return '8';
		case KP_PGUP: return '9';
		case KP_LEFTARROW: return '4';
		case KP_5: return '5';
		case KP_RIGHTARROW: return '6';
		case KP_END: return '1';
		case KP_DOWNARROW: return '2';
		case KP_PGDN: return '3';
		case KP_INS: return '0';
		case KP_DEL: return '.';
		default: return key;
	}
}

[[nodiscard]]
static bool canManipulateConsoleInThisState() {
	return con_initialized && cls.state != CA_GETTING_TICKET && cls.state != CA_CONNECTING && cls.state != CA_CONNECTED;
}

bool Con_HasKeyboardFocus() {
	return con_hasKeyboardFocus;
}

bool Con_HandleCharEvent( wchar_t key ) {
	if( Con_HasKeyboardFocus() ) {
		if( canManipulateConsoleInThisState() ) {
			g_console.instance()->handleCharInputEvent( translateNumPadKey( key ) );
		}
		return true;
	}
	return false;
}

bool Con_HandleKeyEvent( int key, bool down ) {
	if( Con_HasKeyboardFocus() ) {
		// TODO: Check whether the key is a "consolekey" (see old keys.cpp)
		if( down ) {
			if( canManipulateConsoleInThisState() ) {
				g_console.instance()->handleKeyDownEvent( key );
			}
		}
		return true;
	}
	return false;
}

void Con_AcceptCompletionResult( unsigned requestId, const CompletionResult &result ) {
	if( con_initialized ) {
		// TODO: Move the completion result properly
		g_console.instance()->acceptCommandCompletionResult( requestId, CompletionResult { result } );
	}
}

void Con_Close( void ) {
	if( con_initialized ) {
		g_console.instance()->clearInput();
		g_console.instance()->clearNotifications();
	}

	CL_ClearInputState();
}

void Con_ToggleConsole_f( const CmdArgs & ) {
	if( canManipulateConsoleInThisState() ) {
		CL_ClearInputState();
		g_console.instance()->clearInput();
		g_console.instance()->clearNotifications();
		con_hasKeyboardFocus = !con_hasKeyboardFocus;
	}
}

static void Con_Clear_f( const CmdArgs & ) {
	if( con_initialized ) {
		g_console.instance()->clearLines();
		g_console.instance()->clearInput();
	}
}

static void Con_Dump_f( const CmdArgs &cmdArgs ) {
	if( !con_initialized ) {
		return;
	}

	if( cmdArgs.size() < 2 ) {
		clNotice() << "Usage: condump <filename>";
		return;
	}

	wsw::PodVector<char> name;
	name.resize( 16, '\0' );
	name.insert( name.begin(), cmdArgs[1].begin(), cmdArgs[1].end() );
	COM_DefaultExtension( name.data(), ".txt", name.size() );
	COM_SanitizeFilePath( name.data() );
	name.erase( name.begin() + std::strlen( name.data() ), name.end() );

	if( !COM_ValidateRelativeFilename( name.data() ) ) {
		clNotice() << "Invalid filename";
	} else {
		const wsw::StringView nameView( name.data(), name.size(), wsw::StringView::ZeroTerminated );
		if( auto maybeHandle = wsw::fs::openAsWriteHandle( nameView ) ) {
			const wsw::PodVector<char> &dump = g_console.instance()->dumpLinesToBuffer();
			if( !maybeHandle->write( dump.data(), dump.size() - 1 ) ) {
				clWarning() << "Failed to write" << name;
			} else {
				clNotice() << "Dumped console text to" << name;
			}
		} else {
			clWarning() << "Failed to open" << name;
		}
	}
}

void Con_ClearNotify( void ) {
	if( con_initialized ) {
		g_console.instance()->clearNotifications();
	}
}

void Con_CheckResize( void ) {
}

void Con_Init( void ) {
	if( con_initialized ) {
		return;
	}

	g_console.init();

	clNotice() << "Console initialized";

	con_maxNotificationTime  = Cvar_Get( "con_maxNotificationTime", "3", CVAR_ARCHIVE );
	con_maxNotificationLines = Cvar_Get( "con_maxNotificationLines", "0", CVAR_ARCHIVE );
	con_chatmode             = Cvar_Get( "con_chatmode", "3", CVAR_ARCHIVE );

	CL_Cmd_Register( "toggleconsole"_asView, Con_ToggleConsole_f );
	CL_Cmd_Register( "clear"_asView, Con_Clear_f );
	CL_Cmd_Register( "condump"_asView, Con_Dump_f );
	con_initialized = true;
}

void Con_Shutdown( void ) {
	if( !con_initialized ) {
		return;
	}

	CL_Cmd_Unregister( "toggleconsole"_asView );
	CL_Cmd_Unregister( "clear"_asView );
	CL_Cmd_Unregister( "condump"_asView );

	g_console.shutdown();

	con_initialized = false;
}

void Con_Print( const char *txt ) {
	if( con_initialized ) {
		g_console.instance()->addText( wsw::StringView( txt ), Console::DrawNotification );
	}
}

void Con_PrintSilent( const char *txt ) {
	if( con_initialized ) {
		g_console.instance()->addText( wsw::StringView( txt ), Console::SuppressNotification );
	}
}

[[nodiscard]]
static uint64_t Con_SendChatMessage( const wsw::StringView &text, bool team ) {
	// MAX_CHAT_BYTES includes trailing '\0'
	wsw::StaticString<MAX_CHAT_BYTES - 1> sanitizedText;
	for( const char ch: text ) {
		sanitizedText.append( ( ch != '"' ) ? ch : '\'' );
		if( sanitizedText.full() ) [[unlikely]] {
			break;
		}
	}

	const char *cmd;
	if( team && CL_Cmd_Exists( wsw::StringView( "say_team" ) ) ) {
		cmd = "say_team";
	} else if( CL_Cmd_Exists( wsw::StringView( "say" ) ) ) {
		cmd = "say";
	} else {
		cmd = "cmd say";
	}

	const auto oldCmdNum = cls.reliableSequence;
	CL_Cmd_ExecuteNow( va( "%s \"%s\"\n", cmd, sanitizedText.data() ) );
	assert( oldCmdNum + 1 == cls.reliableSequence );
	return oldCmdNum;
}

uint64_t Con_SendCommonChatMessage( const wsw::StringView &text ) {
	return Con_SendChatMessage( text, false );
}

uint64_t Con_SendTeamChatMessage( const wsw::StringView &text ) {
	return Con_SendChatMessage( text, true );
}