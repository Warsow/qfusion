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
#include "../common/cmdargs.h"
#include "../common/profilerscope.h"
#include "../common/singletonholder.h"
#include "../common/wswtonum.h"

#include <vector>
#include <string>

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

	// TODO: Supply thread names?
	void beginAcceptingResults( wsw::ProfilingSystem::FrameGroup group, unsigned totalThreads ) override {
		assert( m_activeResultGroup == std::nullopt );
		m_mutex.lock();
		m_activeResultGroup = group;
		m_groupStates[group].discoveredRootScopes.clear();
		m_groupStates[group].threadResults.clear();
		// TODO: Resize only if needed
		m_groupStates[group].threadResults.resize( totalThreads );
	}

	void endAcceptingResults( wsw::ProfilingSystem::FrameGroup group ) override {
		m_activeResultGroup = std::nullopt;
		m_mutex.unlock();
	}

	void addDiscoveredRoot( unsigned threadIndex, unsigned scopeId ) override {
		GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
		groupState.discoveredRootScopes.append( scopeId );
	}

	void addCallStats( unsigned threadIndex, unsigned callScopeId, const CallStats &callStats ) override {
		GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
		ThreadProfilingResults &results = groupState.threadResults.at( threadIndex );
		results.callStats = callStats;
	}

	void addCallChildStats( unsigned threadIndex, unsigned childScopeId, const CallStats &callStats ) override {
		GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
		ThreadProfilingResults &results = groupState.threadResults.at( threadIndex );
		results.childStats.append( { childScopeId, callStats } );
	}

	void drawSelf( unsigned screenWidth, unsigned screenHeight );

	void listRoots( wsw::ProfilingSystem::FrameGroup group );

	[[nodiscard]]
	bool select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token );

	void reset();

	void setEnabled( bool enabled );

	void listScopes();
private:
	void doReset();

	struct GroupState;

	[[nodiscard]]
	auto drawDiscoveredRoots( const GroupState &groupState, const wsw::StringView &title, int startX, int startY,
							  int width, int margin, int lineHeight ) -> int;
	[[nodiscard]]
	auto drawProfilingStats( const GroupState &groupState, const wsw::StringView &title, int startX, int startY,
							 int width, int margin, int lineHeight ) -> int;

	wsw::Mutex m_mutex;

	std::optional<wsw::ProfilingSystem::FrameGroup> m_activeResultGroup;

	struct ThreadProfilingResults {
		CallStats callStats;
		wsw::PodVector<std::pair<unsigned, CallStats>> childStats;
	};

	struct GroupState {
		enum OperationMode { NoOp, ListRoots, ProfileCall };

		wsw::PodVector<unsigned> discoveredRootScopes;
		std::optional<unsigned> selectedScope;

		OperationMode operationMode { NoOp };

		std::vector<ThreadProfilingResults> threadResults;
	};

	GroupState m_groupStates[2];
	bool m_isEnabled { false };
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
			clNotice() << "Usage: pf_listroots <client|server>";
		}
	});
	CL_Cmd_Register( "pf_select"_asView, []( const CmdArgs &args ) {
		bool handled = false;
		if( std::optional<wsw::ProfilingSystem::FrameGroup> maybeGroup = parseProfilerGroup( args[1] ) ) {
			if( const wsw::StringView token = args[2]; !token.empty() ) {
				if( g_profilerHudHolder.instance()->select( *maybeGroup, token ) ) {
					handled = true;
				}
			}
		}
		if( !handled ) {
			clNotice() << "Usage: pf_select <client|server> #<scope-number>";
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
	g_profilerHudHolder.init();
}

void CL_ProfilerHud_Shutdown() {
	CL_Cmd_Unregister( "pf_listroots"_asView );
	CL_Cmd_Unregister( "pf_select"_asView );
	CL_Cmd_Unregister( "pf_listscopes"_asView );
	CL_Cmd_Unregister( "pf_reset"_asView );
	CL_Cmd_Unregister( "pf_enable"_asView );
	g_profilerHudHolder.shutdown();
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

	if( m_isEnabled ) {
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
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	assert( group == 0 || group == 1 );
	m_groupStates[group].operationMode = GroupState::ListRoots;
}

bool ProfilerHud::select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token ) {
	assert( group == 0 || group == 1 );

	if( !token.startsWith( '#' ) ) {
		return false;
	}

	const std::optional<size_t> maybeNumber = wsw::toNum<size_t>( token.drop( 1 ) );
	if( !maybeNumber ) {
		return false;
	}

	const std::span<const wsw::ProfilingSystem::RegisteredScope> scopes = wsw::ProfilingSystem::getRegisteredScopes();
	if( *maybeNumber >= scopes.size() ) {
		return false;
	}

	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	GroupState &groupState = m_groupStates[group];
	groupState.operationMode = GroupState::ProfileCall;
	groupState.selectedScope = *maybeNumber;

	return true;
}

void ProfilerHud::reset() {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	doReset();
}

void ProfilerHud::doReset() {
	for( GroupState &groupState: m_groupStates ) {
		groupState.operationMode = GroupState::NoOp;
		groupState.threadResults.clear();
		groupState.discoveredRootScopes.clear();
		groupState.selectedScope = std::nullopt;
	}
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
		clNotice() << '#' << i << scope.file << scope.readableFunction << scope.line;
	}
}

extern const vec4_t kConsoleBackgroundColor;

template <typename S>
static void drawSideAlignedPair( const S &left, const S &right, int startX, int startY,
								 int width, int margin, int lineHeight, const float *color = colorWhite ) {
	const int leftX      = startX + margin;
	const int rightX     = startX + width - margin;
	const int leftWidth  = SCR_DrawString( leftX, startY, ALIGN_LEFT_TOP, left, nullptr, color );
	const int rightWidth = SCR_DrawString( rightX, startY, ALIGN_RIGHT_TOP, right, nullptr, color );
	if( leftWidth + rightWidth + 2 * margin + 48 < width ) {
		const int barX     = startX + leftWidth + 2 * margin;
		const int barY     = startY + ( 7 * lineHeight ) / 8;
		const int barWidth = (int)width - leftWidth - rightWidth - 4 * margin;
		SCR_DrawFillRect( barX, barY, barWidth, 1, colorDkGrey );
	}
}

static void drawHeader( const wsw::StringView &title, int startX, int startY, int width, int margin, int lineHeight ) {
	const int textWidth = SCR_DrawString( startX + margin, startY, ALIGN_LEFT_TOP, title );
	const int barX      = startX + textWidth + 2 * margin;
	const int barY      = startY + ( 7 * lineHeight ) / 8;
	const int barWidth  = (int)width  - textWidth - 3 * margin;
	SCR_DrawFillRect( barX, barY, barWidth, 1, colorLtGrey );
}

auto ProfilerHud::drawDiscoveredRoots( const GroupState &groupState, const wsw::StringView &title,
									   int startX, int startY, int width, int margin, int lineHeight ) -> int {
	const std::span<const wsw::ProfilingSystem::RegisteredScope> &scopes = wsw::ProfilingSystem::getRegisteredScopes();

	const int totalLines = 2 + (int)groupState.discoveredRootScopes.size();
	SCR_DrawFillRect( startX, startY, (int)width, lineHeight * totalLines + 3 * margin, kConsoleBackgroundColor );

	int y = startY;

	drawHeader( title, startX, y, width, margin, lineHeight );
	y += lineHeight + margin;

	SCR_DrawString( startX + margin, y, ALIGN_LEFT_TOP, "Available profiling roots", nullptr, colorLtGrey );
	y += lineHeight + margin;

	for( const unsigned scopeId: groupState.discoveredRootScopes ) {
		const wsw::StringView &fn = scopes[scopeId].readableFunction;
		wsw::StaticString<16> idString;
		idString << '#' << scopeId;
		drawSideAlignedPair( fn, idString.asView(), startX, y, width, margin, lineHeight, colorMdGrey );
		y += lineHeight;
	}

	return y - startY + margin;
}

auto ProfilerHud::drawProfilingStats( const GroupState &groupState, const wsw::StringView &title,
									  int startX, int startY, int width, int margin, int lineHeight ) -> int {
	const std::span<const wsw::ProfilingSystem::RegisteredScope> &scopes = wsw::ProfilingSystem::getRegisteredScopes();

	int totalLines = 2;
	for( const ThreadProfilingResults &threadResults: groupState.threadResults ) {
		totalLines += 1 + (int)threadResults.childStats.size();
		if( threadResults.callStats.enterCount > 0 ) {
			totalLines++;
		}
	}

	SCR_DrawFillRect( startX, startY, (int)width, lineHeight * totalLines + 3 * margin, kConsoleBackgroundColor );

	int y = startY;

	drawHeader( title, startX, y, width, margin, lineHeight );
	y += lineHeight + margin;

	wsw::StaticString<64> commonHeader( "Profiling " );
	constexpr unsigned maxNameLimit = 48;
	commonHeader << scopes[groupState.selectedScope.value()].readableFunction.take( maxNameLimit );

	SCR_DrawString( startX + margin, y, ALIGN_LEFT_TOP, commonHeader.asView(), nullptr, colorLtGrey );
	y += lineHeight + margin;

	unsigned threadIndex = 0;
	for( const ThreadProfilingResults &threadResults: groupState.threadResults ) {
		wsw::StaticString<64> threadHeader, threadStats;
		(void)threadHeader.appendf( "Thread #%-2d", threadIndex );
		(void)threadStats.appendf( "count=%-4d", threadResults.callStats.enterCount );
		(void)threadStats.appendf( " us=%-5d", (unsigned)threadResults.callStats.totalTime );

		drawSideAlignedPair( threadHeader.asView(), threadStats.asView(), startX, y, width, margin, lineHeight, colorLtGrey );
		y += lineHeight;

		unsigned realNameLimit = 0;
		for( const auto &[scopeId, _] : threadResults.childStats ) {
			realNameLimit = wsw::max<unsigned>( realNameLimit, scopes[scopeId].readableFunction.length() );
		}
		realNameLimit = wsw::min( realNameLimit, maxNameLimit );

		auto remaining = (int64_t)threadResults.callStats.totalTime;
		for( const auto &[scopeId, callStats] : threadResults.childStats ) {
			wsw::StaticString<maxNameLimit + 16> childDesc;
			(void)childDesc.appendf( "#%d ", scopeId );
			childDesc << scopes[scopeId].readableFunction.take( realNameLimit );

			wsw::StaticString<64> childStats;
			(void)childStats.appendf( "count=%-4d", callStats.enterCount );
			(void)childStats.appendf( " us=%-5d", (unsigned)callStats.totalTime );

			drawSideAlignedPair( childDesc.asView(), childStats.asView(), startX, y, width, margin, lineHeight, colorMdGrey );
			y += lineHeight;

			remaining -= (int64_t)callStats.totalTime;
		}

		if( threadResults.callStats.enterCount > 0 ) {
			wsw::StaticString<64> otherStats;
			(void)otherStats.appendf( "us=%-5d", (unsigned)wsw::max<int64_t>( 0, remaining ) );

			drawSideAlignedPair( "Other"_asView, otherStats.asView(), startX, y, width, margin, lineHeight, colorDkGrey );
			y += lineHeight;
		}

		threadIndex++;
	}

	return y - startY + margin;
}

void ProfilerHud::drawSelf( unsigned screenWidth, unsigned ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	if( !m_isEnabled ) {
		return;
	}

	const int margin      = 8 * VID_GetPixelRatio();
	const auto lineHeight = (int)SCR_FontHeight( cls.consoleFont );

	const int paneX     = ( 2 * (int)screenWidth ) / 3;
	const int paneWidth = (int)screenWidth - paneX - margin;
	int y               = margin;

	SCR_DrawFillRect( paneX, y, (int)paneWidth, 5 * lineHeight + 3 * margin, kConsoleBackgroundColor );
	y += margin;

	drawHeader( "H E L P"_asView, paneX, y, paneWidth, margin, lineHeight );
	y += lineHeight + margin;

	const std::pair<const char *, const char *> cmdDescs[] {
		{ "pf_listscopes", "Print available profiling scopes to the console" },
		{ "pf_listroots <cl|sv>", "Discover available call tree roots" },
		{ "pf_select <cl|sv> #<id>", "Select a scope for detailed profiling" },
		{ "pf_reset", "Reset everything to the idle state" },
	};

	for( const auto &[syntax, desc]: cmdDescs ) {
		drawSideAlignedPair( syntax, desc, paneX, y, paneWidth, margin, lineHeight, colorMdGrey );
		y += lineHeight;
	}
	y += margin;

	const wsw::StringView groupTitles[2] { "C L I E N T"_asView, "S E R V E R"_asView };

	for( size_t groupIndex = 0; groupIndex < std::size( m_groupStates ); ++groupIndex ) {
		const GroupState &groupState = m_groupStates[groupIndex];
		if( groupState.operationMode == GroupState::ListRoots ) {
			y += drawDiscoveredRoots( groupState, groupTitles[groupIndex], paneX, y, paneWidth, margin, lineHeight );
		} else if( groupState.operationMode == GroupState::ProfileCall ) {
			y += drawProfilingStats( groupState, groupTitles[groupIndex], paneX, y, paneWidth, margin, lineHeight );
		}
	}

}
