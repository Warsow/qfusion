/*
Copyright (C) 2025 Chasseur de bots

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

#include "client.h"
#include <common/facilities/cmdargs.h>
#include <common/facilities/profilerscope.h>
#include <common/helpers/singletonholder.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/tonum.h>
#include <common/helpers/qthreads.h>

#include <unordered_map>

using wsw::operator""_asView;

class ProfilerHud : public wsw::ProfilerArgsSupplier, public wsw::ProfilerResultSink {
public:
	void beginSupplyingArgs() override {
		m_mutex.lock();
	}

	auto getArgs( wsw::ProfilingSystem::FrameGroup group ) -> wsw::ProfilerArgs override;

	void endSupplyingArgs() override {
		m_mutex.unlock();
	}

	void beginAcceptingResults( wsw::ProfilingSystem::FrameGroup group ) override {
		assert( m_activeResultGroup == std::nullopt );
		m_mutex.lock();
		if( !m_isFrozen ) {
			m_activeResultGroup = group;
			m_groupStates[group].threadFrameProfilingResults.clear();
			m_groupStates[group].threadFrameRootDiscoveryResults.clear();
		}
	}

	void endAcceptingResults( wsw::ProfilingSystem::FrameGroup group ) override {
		if( !m_isFrozen ) {
			m_activeResultGroup = std::nullopt;
		}
		m_mutex.unlock();
	}

	// Note: Currently we supply thread indices as ids (this is perfectly valid but could be surprising)
	// TODO: Supply thread names?

	void addDiscoveredRoot( uint64_t threadId, unsigned scopeId ) override {
		if( !m_isFrozen ) {
			GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
			auto &results          = groupState.threadFrameRootDiscoveryResults[threadId];
			results.discoveredScopes.push_back( scopeId );
		}
	}

	void addCallStats( uint64_t threadId, unsigned callScopeId, const CallStats &callStats ) override {
		if( !m_isFrozen ) {
			GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
			auto &results          = groupState.threadFrameProfilingResults[threadId];
			results.callStats      = callStats;
			groupState.threadProfilingTimelines[threadId].add( callStats );
		}
	}

	void addCallChildStats( uint64_t threadId, unsigned childScopeId, const CallStats &callStats ) override {
		if( !m_isFrozen ) {
			GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
			auto &results          = groupState.threadFrameProfilingResults[threadId];
			results.childStats.append( { childScopeId, callStats } );
		}
	}

	void update( int gameMsec, int realMsec );

	void drawSelf( unsigned screenWidth, unsigned screenHeight );

	void listRoots( wsw::ProfilingSystem::FrameGroup group );

	[[nodiscard]]
	bool startTracking( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token );

	void reset();

	void setEnabled( bool enabled );

	void listScopes();

	void toggleFrozenState();
private:
	void doReset();

	struct GroupState;

	[[nodiscard]]
	auto drawDiscoveredRoots( Draw2DRequest *request, const GroupState &groupState, const wsw::StringView &title,
							  int startX, int startY, int width, int margin, int lineHeight ) -> int;
	[[nodiscard]]
	auto drawProfilingStats( Draw2DRequest *request, const GroupState &groupState, const wsw::StringView &title,
							 int startX, int startY, int width, int margin, int lineHeight ) -> int;

	class Timeline {
	public:
		static constexpr unsigned kNumSamples = 255;
		static_assert( wsw::isPowerOf2( kNumSamples + 1 ) );
	private:
		CallStats m_values[kNumSamples + 1] {};
		unsigned m_head { kNumSamples };
		unsigned m_tail { 0 };
	public:
		Timeline() { clear(); }

		void clear() {
			wsw::fill( std::begin( m_values ), std::end( m_values ), CallStats {} );
			m_tail = 0;
			m_head = kNumSamples;
		}

		void add( const CallStats &callStats ) {
			m_values[m_head] = callStats;
			m_head           = ( m_head + 1 ) % ( kNumSamples + 1 );
			m_tail           = ( m_tail + 1 ) % ( kNumSamples + 1 );
		}

		struct Iterator {
			const Timeline *parent;
			unsigned position;
			[[nodiscard]]
			bool operator!=( const Iterator &that ) const {
				return position != that.position;
			}
			[[maybe_unused]]
			auto operator++() -> Iterator & {
				position = ( position + 1 ) % ( kNumSamples + 1 );
				return *this;
			}
			[[nodiscard]]
			auto operator*() const -> const CallStats & {
				return parent->m_values[position];
			}
		};

		[[nodiscard]]
		auto begin() const -> Iterator { return { this, m_tail }; }
		[[nodiscard]]
		auto end() const -> Iterator { return { this, m_head }; }
	};

	static void drawTimeline( Draw2DRequest *request, const Timeline &timeline, int startX, int startY,
							  int width, int height, int margin );

	wsw::Mutex m_mutex;

	std::optional<wsw::ProfilingSystem::FrameGroup> m_activeResultGroup;

	struct ThreadRootDiscoveryResults {
		wsw::PodVector<unsigned> discoveredScopes;
	};

	struct ThreadProfilingResults {
		CallStats callStats;
		wsw::PodVector<std::pair<unsigned, CallStats>> childStats;
	};

	struct GroupState {
		enum OperationMode { NoOp, ListRoots, ProfileCall };

		std::optional<unsigned> selectedScope;

		OperationMode operationMode { NoOp };

		std::unordered_map<uint64_t, ThreadProfilingResults> threadFrameProfilingResults;
		std::unordered_map<uint64_t, ThreadRootDiscoveryResults> threadFrameRootDiscoveryResults;

		std::unordered_map<uint64_t, Timeline> threadProfilingTimelines;

		void clear() {
			selectedScope = std::nullopt;
			operationMode = NoOp;

			threadFrameProfilingResults.clear();
			threadFrameRootDiscoveryResults.clear();

			threadProfilingTimelines.clear();
		}
	};

	GroupState m_groupStates[2];

	Timeline m_gameFrameTimeTimeline;
	Timeline m_realFrameTimeTimeline;

	int m_lastGameFrameTime { 0 };
	int m_lastRealFrameTime { 0 };

	bool m_isEnabled { false };
	bool m_isFrozen { false };
};

static SingletonHolder<ProfilerHud> g_profilerHudHolder;

[[nodiscard]]
static auto parseProfilerGroup( const wsw::StringView &token ) -> std::optional<wsw::ProfilingSystem::FrameGroup> {
	if( token.equalsIgnoreCase( "cl"_asView ) || token.equalsIgnoreCase( "client"_asView ) ) {
		return wsw::ProfilingSystem::ClientGroup;
	}
	if( token.equalsIgnoreCase( "sv"_asView ) || token.equalsIgnoreCase( "server"_asView ) ) {
		return wsw::ProfilingSystem::ServerGroup;
	}
	return std::nullopt;
}

void CL_ProfilerHud_Init() {
	CL_Cmd_Register( "pf_listroots"_asView, []( const CmdArgs &args ) {
		if( std::optional<wsw::ProfilingSystem::FrameGroup> maybeGroup = parseProfilerGroup( args[1] ) ) {
			g_profilerHudHolder.instance()->listRoots( *maybeGroup );
		} else {
			clNotice() << "Usage: pf_listroots <cl|sv>";
		}
	});
	CL_Cmd_Register( "pf_track"_asView, []( const CmdArgs &args ) {
		bool handled = false;
		if( std::optional<wsw::ProfilingSystem::FrameGroup> maybeGroup = parseProfilerGroup( args[1] ) ) {
			if( const wsw::StringView token = args[2]; !token.empty() ) {
				if( g_profilerHudHolder.instance()->startTracking( *maybeGroup, token ) ) {
					handled = true;
				}
			}
		}
		if( !handled ) {
			clNotice() << "Usage: pf_track <cl|sv> <id>";
		}
	});
	CL_Cmd_Register( "pf_listscopes"_asView, []( const CmdArgs &args ) {
		g_profilerHudHolder.instance()->listScopes();
	});
	CL_Cmd_Register( "pf_reset"_asView, []( const CmdArgs &args ) {
		g_profilerHudHolder.instance()->reset();
	});
	CL_Cmd_Register( "pf_enable"_asView, []( const CmdArgs &args ) {
		if( std::optional<int64_t> value = wsw::toNum<int64_t>( args[1] ) ) {
			g_profilerHudHolder.instance()->setEnabled( *value != 0 );
		} else {
			clNotice() << "Usage: pf_enable <1|0>";
		}
	});
	CL_Cmd_Register( "pf_freeze"_asView, []( const CmdArgs &args ) {
		g_profilerHudHolder.instance()->toggleFrozenState();
	});
	g_profilerHudHolder.init();
}

void CL_ProfilerHud_Shutdown() {
	CL_Cmd_Unregister( "pf_listroots"_asView );
	CL_Cmd_Unregister( "pf_track"_asView );
	CL_Cmd_Unregister( "pf_listscopes"_asView );
	CL_Cmd_Unregister( "pf_reset"_asView );
	CL_Cmd_Unregister( "pf_enable"_asView );
	CL_Cmd_Unregister( "pf_freeze"_asView );
	g_profilerHudHolder.shutdown();
}

void CL_ProfilerHud_Update( int gameMsec, int realMsec ) {
	g_profilerHudHolder.instance()->update( gameMsec, realMsec );
}

void CL_ProfilerHud_Draw( unsigned width, unsigned height ) {
	WSW_PROFILER_SCOPE();

	g_profilerHudHolder.instance()->drawSelf( width, height );
}

wsw::ProfilerArgsSupplier *CL_GetProfilerArgsSupplier() {
	return g_profilerHudHolder.instance();
}
wsw::ProfilerResultSink *CL_GetProfilerResultSink() {
	return g_profilerHudHolder.instance();
}

auto ProfilerHud::getArgs( wsw::ProfilingSystem::FrameGroup group ) -> wsw::ProfilerArgs {
	assert( group == 0 || group == 1 );
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	if( m_isEnabled && !m_isFrozen ) {
		const GroupState &groupState = m_groupStates[group];
		switch( groupState.operationMode ) {
			case GroupState::NoOp: {
				return { std::monostate() };
			};
			case GroupState::ListRoots: {
				return { wsw::ProfilerArgs::DiscoverRootScopes() };
			};
			case GroupState::ProfileCall: {
				return { wsw::ProfilerArgs::ProfileCall { .scopeIndex = groupState.selectedScope.value() } };
			};
			default: {
				wsw::failWithLogicError( "Unreachable" );
			}
		}
	} else {
		return { std::monostate() };
	}
}

void ProfilerHud::listRoots( wsw::ProfilingSystem::FrameGroup group ) {
	assert( group == 0 || group == 1 );

	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	m_groupStates[group].clear();
	m_groupStates[group].operationMode = GroupState::ListRoots;

	m_isFrozen = false;
}

bool ProfilerHud::startTracking( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token ) {
	assert( group == 0 || group == 1 );

	const std::optional<size_t> maybeNumber = wsw::toNum<size_t>( token );
	if( !maybeNumber ) {
		return false;
	}

	const std::span<const wsw::ProfilingSystem::RegisteredScope> scopes = wsw::ProfilingSystem::getRegisteredScopes();
	if( *maybeNumber >= scopes.size() ) {
		return false;
	}

	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	GroupState &groupState = m_groupStates[group];
	groupState.clear();

	groupState.operationMode = GroupState::ProfileCall;
	groupState.selectedScope = *maybeNumber;

	m_isFrozen = false;

	return true;
}

void ProfilerHud::reset() {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	doReset();
}

void ProfilerHud::doReset() {
	for( GroupState &groupState: m_groupStates ) {
		groupState.clear();
	}

	// Note: We don't care of always-displayed client properties

	m_isFrozen = false;
}

void ProfilerHud::setEnabled( bool enabled ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	if( m_isEnabled != enabled ) {
		doReset();
		m_isEnabled = enabled;
	}
}

void ProfilerHud::listScopes() {
	clNotice() << "Available scopes:";
	const std::span<const wsw::ProfilingSystem::RegisteredScope> scopes = wsw::ProfilingSystem::getRegisteredScopes();
	for( size_t i = 0; i < scopes.size(); ++i ) {
		const auto &scope = scopes[i];
		wsw::StaticString<48> fileString;
		wsw::StaticString<16> idString;
		// TODO: Allow getting rid of string quotes in the output
		idString << '@' << i;
		idString.resize( 4, ' ' );
		fileString << scope.file.take( fileString.capacity() - 8 );
		fileString << ':' << scope.line;
		fileString.resize( fileString.capacity(), '_' );
		clNotice() << idString << fileString << scope.readableFunction;
	}
}

void ProfilerHud::toggleFrozenState() {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
	m_isFrozen = !m_isFrozen;
}

extern const vec4_t kConsoleBackgroundColor;

template <typename S>
static void drawSideAlignedPair( Draw2DRequest *request, const S &left, const S &right, int startX, int startY,
								 int width, int margin, int lineHeight, const float *color = colorWhite ) {
	const int leftX      = startX + margin;
	const int rightX     = startX + width - margin;
	const int leftWidth  = SCR_DrawString( request, leftX, startY, ALIGN_LEFT_TOP, left, nullptr, color );
	const int rightWidth = SCR_DrawString( request, rightX, startY, ALIGN_RIGHT_TOP, right, nullptr, color );
	if( leftWidth + rightWidth + 2 * margin + 48 < width ) {
		const int barX     = startX + leftWidth + 2 * margin;
		const int barY     = startY + ( 7 * lineHeight ) / 8;
		const int barWidth = (int)width - leftWidth - rightWidth - 4 * margin;
		SCR_DrawFillRect( request, barX, barY, barWidth, 1, colorDkGrey );
	}
}

static void drawHeader( Draw2DRequest *request, const wsw::StringView &title, int startX, int startY,
						int width, int margin, int lineHeight ) {
	const int textWidth = SCR_DrawString( request, startX + margin, startY, ALIGN_LEFT_TOP, title );
	const int barX      = startX + textWidth + 2 * margin;
	const int barY      = startY + ( 7 * lineHeight ) / 8;
	const int barWidth  = (int)width  - textWidth - 3 * margin;
	SCR_DrawFillRect( request, barX, barY, barWidth, 1, colorLtGrey );
}

auto ProfilerHud::drawDiscoveredRoots( Draw2DRequest *request, const GroupState &groupState, const wsw::StringView &title,
									   int startX, int startY, int width, int margin, int lineHeight ) -> int {
	const std::span<const wsw::ProfilingSystem::RegisteredScope> &scopes = wsw::ProfilingSystem::getRegisteredScopes();

	int totalLines = 2;
	for( const auto &[_, threadResults]: groupState.threadFrameRootDiscoveryResults ) {
		totalLines += 1 + (int)threadResults.discoveredScopes.size();
	}

	SCR_DrawFillRect( request, startX, startY, (int)width, lineHeight * totalLines + 3 * margin, kConsoleBackgroundColor );

	int y = startY;

	drawHeader( request, title, startX, y, width, margin, lineHeight );
	y += lineHeight + margin;

	SCR_DrawString( request, startX + margin, y, ALIGN_LEFT_TOP, "Available profiling roots", nullptr, colorLtGrey );
	y += lineHeight + margin;

	unsigned threadIndex = 0;
	for( const auto &[_, threadResults]: groupState.threadFrameRootDiscoveryResults ) {
		wsw::StaticString<64> threadHeader, threadStats;
		(void)threadHeader.appendf( "Thread #%-2d", threadIndex );
		(void)threadStats.appendf( "%d", (unsigned)threadResults.discoveredScopes.size() );

		drawSideAlignedPair( request, threadHeader.asView(), threadStats.asView(), startX, y, width, margin, lineHeight, colorLtGrey );
		y += lineHeight;

		for( const unsigned scopeId: threadResults.discoveredScopes ) {
			const wsw::StringView &fn = scopes[scopeId].readableFunction;
			wsw::StaticString<16> idString;
			idString << '@' << scopeId;
			drawSideAlignedPair( request, fn, idString.asView(), startX, y, width, margin, lineHeight, colorMdGrey );
			y += lineHeight;
		}

		threadIndex++;
	}

	return y - startY + margin;
}

static constexpr int kGraphHeight = 64;

auto ProfilerHud::drawProfilingStats( Draw2DRequest *request, const GroupState &groupState, const wsw::StringView &title,
									  int startX, int startY, int width, int margin, int lineHeight ) -> int {
	const std::span<const wsw::ProfilingSystem::RegisteredScope> &scopes = wsw::ProfilingSystem::getRegisteredScopes();

	int totalLines           = 2;
	int totalGraphs          = 0;
	unsigned threadGraphMask = 0;
	do {
		unsigned threadIndex     = 0;
		for( const auto &[threadId, threadResults]: groupState.threadFrameProfilingResults ) {
			totalLines += 1 + (int)threadResults.childStats.size();
			if( threadResults.callStats.enterCount > 0 ) {
				totalLines++;
			}
			assert( groupState.threadProfilingTimelines.contains( threadId ) );
			const Timeline &timeline = groupState.threadProfilingTimelines.find( threadId )->second;
			for( const CallStats &callStats: timeline ) {
				if( callStats.enterCount > 0 ) {
					totalGraphs++;
					threadGraphMask |= 1 << threadIndex;
					break;
				}
			}
			threadIndex++;
		}
	} while( false );

	const int totalHeight = lineHeight * totalLines + 3 * margin + ( 2 * margin + kGraphHeight ) * totalGraphs;
	SCR_DrawFillRect( request, startX, startY, (int)width, totalHeight, kConsoleBackgroundColor );

	int y = startY;

	drawHeader( request, title, startX, y, width, margin, lineHeight );
	y += lineHeight + margin;

	wsw::StaticString<64> commonHeader( "Profiling " );
	constexpr unsigned maxNameLimit = 48;
	commonHeader << scopes[groupState.selectedScope.value()].readableFunction.take( maxNameLimit );

	SCR_DrawString( request, startX + margin, y, ALIGN_LEFT_TOP, commonHeader.asView(), nullptr, colorLtGrey );
	y += lineHeight + margin;

	unsigned threadIndex = 0;
	for( const auto &[threadId, threadResults]: groupState.threadFrameProfilingResults ) {
		wsw::StaticString<64> threadHeader, threadStats;
		(void)threadHeader.appendf( "Thread #%-2d", threadIndex );

		(void)threadStats.appendf( "count=%-4d", threadResults.callStats.enterCount );
		(void)threadStats.appendf( " us=%-5d", (unsigned)threadResults.callStats.totalTime );

		drawSideAlignedPair( request, threadHeader.asView(), threadStats.asView(), startX, y, width, margin, lineHeight, colorLtGrey );
		y += lineHeight;

		if( threadGraphMask & ( 1 << threadIndex ) ) {
			y += margin;
			const auto it = groupState.threadProfilingTimelines.find( threadId );
			assert( it != groupState.threadProfilingTimelines.end() );
			drawTimeline( request, it->second, startX, y, width, kGraphHeight, margin );
			y += kGraphHeight + margin;
		}

		unsigned realNameLimit = 0;
		for( const auto &[scopeId, _] : threadResults.childStats ) {
			realNameLimit = wsw::max<unsigned>( realNameLimit, scopes[scopeId].readableFunction.length() );
		}
		realNameLimit = wsw::min( realNameLimit, maxNameLimit );

		auto remaining = (int64_t)threadResults.callStats.totalTime;
		for( const auto &[scopeId, callStats] : threadResults.childStats ) {
			wsw::StaticString<maxNameLimit + 16> childDesc;
			(void)childDesc.appendf( "@%d ", scopeId );
			childDesc << scopes[scopeId].readableFunction.take( realNameLimit );

			wsw::StaticString<64> childStats;
			(void)childStats.appendf( "count=%-4d", callStats.enterCount );
			(void)childStats.appendf( " us=%-5d", (unsigned)callStats.totalTime );

			drawSideAlignedPair( request, childDesc.asView(), childStats.asView(), startX, y, width, margin, lineHeight, colorMdGrey );
			y += lineHeight;

			remaining -= (int64_t)callStats.totalTime;
		}

		if( threadResults.callStats.enterCount > 0 ) {
			wsw::StaticString<64> otherStats;
			(void)otherStats.appendf( "us=%-5d", (unsigned)wsw::max<int64_t>( 0, remaining ) );

			drawSideAlignedPair( request, "Other"_asView, otherStats.asView(), startX, y, width, margin, lineHeight, colorDkGrey );
			y += lineHeight;
		}

		threadIndex++;
	}

	return y - startY + margin;
}

void ProfilerHud::update( int gameMsec, int realMsec ) {
	// This should be safe to call without locking

	if( m_isEnabled && !m_isFrozen ) {
		m_gameFrameTimeTimeline.add( CallStats { (uint64_t)gameMsec, 1 } );
		m_lastGameFrameTime = gameMsec;
		m_realFrameTimeTimeline.add( CallStats { (uint64_t)realMsec, 1 } );
		m_lastRealFrameTime = realMsec;
	}
}

void ProfilerHud::drawTimeline( Draw2DRequest *request, const Timeline &timeline, int startX, int startY,
								int width, int height, int margin ) {
	const float barColor[4] = { 1.0f, 1.0f, 1.0f, 0.1f };

	int graphWidthStep = 1;
	// TODO: The number 48 is arbitrary, measure the exact text width
	while( 2 * margin + ( graphWidthStep + 1 ) * (int)Timeline::kNumSamples + 48 <= width ) {
		graphWidthStep++;
	}

	const int graphWidth = graphWidthStep * (int)Timeline::kNumSamples;
	const int bottomY    = startY + height;

	int x = startX + margin;

	// We apply side margins but use the full height to simplify layout calculations
	SCR_DrawFillRect( request, x, startY, 1, height, barColor );
	SCR_DrawFillRect( request, x + graphWidth, startY, 1, height, barColor );
	SCR_DrawFillRect( request, x, startY, graphWidth, 1, barColor );
	SCR_DrawFillRect( request, x, startY + height, graphWidth, 1, barColor );

	wsw::StaticString<32> minText( "min="_asView ), maxText( "max="_asView ), avgText( "avg="_asView );

	uint64_t minValue = std::numeric_limits<uint64_t>::max();
	uint64_t maxValue = std::numeric_limits<uint64_t>::lowest();
	uint64_t accum    = 0;
	uint64_t total    = 0;
	for( const CallStats &callStats: timeline ) {
		if( callStats.enterCount > 0 ) {
			minValue = wsw::min( callStats.totalTime, minValue );
			maxValue = wsw::max( callStats.totalTime, maxValue );
			accum += callStats.totalTime;
			total++;
		}
	}
	if( total > 0 ) {
		const float rcpMaxValue = 1.0f / (float)maxValue;
		for( const CallStats &callStats: timeline ) {
			if( callStats.enterCount > 0 ) {
				const float heightFrac = (float)callStats.totalTime * rcpMaxValue;
				const int barHeight    = (int)( (float)height * heightFrac );
				SCR_DrawFillRect( request, x, bottomY - barHeight, graphWidthStep, barHeight, barColor );
			}
			x += graphWidthStep;
		}
		(void)minText.appendf( "%-5d", (int)minValue );
		(void)maxText.appendf( "%-5d", (int)maxValue );
		(void)avgText.appendf( "%-5d", (int)( accum / total ) );
	} else {
		minText << "N/A  "_asView;
		maxText << "N/A  "_asView;
		avgText << "N/A  "_asView;
	}

	const int textX = startX + width - margin;
	int textY       = startY;

	SCR_DrawString( request, textX, textY, ALIGN_RIGHT_TOP, minText.asView(), nullptr, colorMdGrey );
	textY += ( height ) / 3;
	SCR_DrawString( request, textX, textY, ALIGN_RIGHT_TOP, avgText.asView(), nullptr, colorMdGrey );
	textY += ( height ) / 3;
	SCR_DrawString( request, textX, textY, ALIGN_RIGHT_TOP, maxText.asView(), nullptr, colorMdGrey );
}

void ProfilerHud::drawSelf( unsigned screenWidth, unsigned ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	if( !m_isEnabled ) {
		return;
	}

	Draw2DRequest *const request = CreateDraw2DRequest();

	const int margin      = 8 * VID_GetPixelRatio();
	const auto lineHeight = (int)SCR_FontHeight( cls.consoleFont );

	const int paneX     = ( 2 * (int)screenWidth ) / 3;
	const int paneWidth = (int)screenWidth - paneX - margin;
	int y               = margin;

	const int paneHeight = 9 * lineHeight + 5 * margin + 2 * kGraphHeight + 3 * margin;
	SCR_DrawFillRect( request, paneX, y, paneWidth, paneHeight, kConsoleBackgroundColor );
	y += margin;

	drawHeader( request, m_isFrozen ? "S U M M A R Y [*]"_asView : "S U M M A R Y"_asView, paneX, y, paneWidth, margin, lineHeight );
	y += lineHeight + margin;

	wsw::StaticString<16> realFrameTime( "%d", m_lastRealFrameTime );
	drawSideAlignedPair( request, "Real frame time"_asView, realFrameTime.asView(), paneX, y, paneWidth, margin, lineHeight, colorMdGrey );
	y += lineHeight + margin;

	drawTimeline( request, m_realFrameTimeTimeline, paneX, y, paneWidth, kGraphHeight, margin );
	y += kGraphHeight + margin;

	wsw::StaticString<16> gameFrameTime( "%d", m_lastGameFrameTime );
	drawSideAlignedPair( request, "Game frame time"_asView, gameFrameTime.asView(), paneX, y, paneWidth, margin, lineHeight, colorMdGrey );
	y += lineHeight + margin;

	drawTimeline( request, m_gameFrameTimeTimeline, paneX, y, paneWidth, kGraphHeight, margin );
	y += kGraphHeight + margin;

	drawHeader( request, "H E L P"_asView, paneX, y, paneWidth, margin, lineHeight );
	y += lineHeight + margin;

	const std::pair<const char *, const char *> cmdDescs[] {
		{ "pf_listscopes", "Print available profiling scopes to the console" },
		{ "pf_listroots <cl|sv>", "Discover available call tree roots" },
		{ "pf_track <cl|sv> <id>", "Start detailed profiling of a scope" },
		{ "pf_freeze", "Toggle the frozen state" },
		{ "pf_reset", "Reset everything to the idle state" },
	};

	for( const auto &[syntax, desc]: cmdDescs ) {
		drawSideAlignedPair( request, syntax, desc, paneX, y, paneWidth, margin, lineHeight, colorMdGrey );
		y += lineHeight;
	}
	y += margin;

	const wsw::StringView regularGroupTitles[2] { "C L I E N T"_asView, "S E R V E R"_asView };
	const wsw::StringView frozenGroupTitles[2] { "C L I E N T [*]"_asView, "S E R V E R [*]"_asView };
	const wsw::StringView *groupTitles = m_isFrozen ? frozenGroupTitles : regularGroupTitles;

	for( size_t groupIndex = 0; groupIndex < std::size( m_groupStates ); ++groupIndex ) {
		const GroupState &groupState = m_groupStates[groupIndex];
		if( groupState.operationMode == GroupState::ListRoots ) {
			y += drawDiscoveredRoots( request, groupState, groupTitles[groupIndex], paneX, y, paneWidth, margin, lineHeight );
		} else if( groupState.operationMode == GroupState::ProfileCall ) {
			y += drawProfilingStats( request, groupState, groupTitles[groupIndex], paneX, y, paneWidth, margin, lineHeight );
		}
	}

	CommitDraw2DRequest( request );
}
