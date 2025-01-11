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
#include "../common/singletonholder.h"

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
		m_groupStates[group].discoveredNames.clear();
		m_groupStates[group].threadResults.clear();
		// TODO: Resize only if needed
		m_groupStates[group].threadResults.resize( totalThreads );
	}

	void endAcceptingResults( wsw::ProfilingSystem::FrameGroup group ) override {
		m_activeResultGroup = std::nullopt;
		m_mutex.unlock();
	}

	void addDiscoveredRoot( unsigned threadIndex, const wsw::StringView &root ) override {
		GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
		groupState.discoveredNames.emplace_back( std::string { root.data(), root.size() } );
	}

	void addCallStats( unsigned threadIndex, const wsw::StringView &call, const CallStats &callStats ) override {
		GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
		ThreadProfilingResults &results = groupState.threadResults.at( threadIndex );
		results.callStats = callStats;
	}

	void addCallChildStats( unsigned threadIndex, const wsw::StringView &child, const CallStats &callStats ) override {
		GroupState &groupState = m_groupStates[m_activeResultGroup.value()];
		ThreadProfilingResults &results = groupState.threadResults.at( threadIndex );
		results.childStats.emplace_back( std::make_pair( std::string { child.data(), child.size() }, callStats ) );
	}

	void drawSelf( unsigned width, unsigned height );

	void listRoots( wsw::ProfilingSystem::FrameGroup group );

	void select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token );

	void reset();

	void listScopes();
private:
	wsw::Mutex m_mutex;

	std::optional<wsw::ProfilingSystem::FrameGroup> m_activeResultGroup;

	struct ThreadProfilingResults {
		CallStats callStats;
		std::vector<std::pair<std::string, CallStats>> childStats;
	};

	struct GroupState {
		enum OperationMode { NoOp, ListRoots, ProfileCall };

		std::vector<std::string> discoveredNames;
		std::string selectedName;

		OperationMode operationMode { NoOp };

		std::vector<ThreadProfilingResults> threadResults;
	};

	GroupState m_groupStates[2];
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
			if( const wsw::StringView name = args[2]; !name.empty() ) {
				g_profilerHudHolder.instance()->select( *maybeGroup, name );
				handled = true;
			}
		}
		if( !handled ) {
			clNotice() << "Usage: pf_select <client|server> <name>";
		}
	});
	CL_Cmd_Register( "pf_listscopes"_asView, []( const CmdArgs &args ) {
		g_profilerHudHolder.instance()->listScopes();
	});
	CL_Cmd_Register( "pf_reset"_asView, []( const CmdArgs &args ) {
		g_profilerHudHolder.instance()->reset();
	});
	g_profilerHudHolder.init();
}

void CL_ProfilerHud_Shutdown() {
	CL_Cmd_Unregister( "pf_listroots"_asView );
	CL_Cmd_Unregister( "pf_select"_asView );
	CL_Cmd_Unregister( "pf_listscopes"_asView );
	CL_Cmd_Unregister( "pf_reset"_asView );
	g_profilerHudHolder.shutdown();
}

void CL_ProfilerHud_Draw( unsigned width, unsigned height ) {
	g_profilerHudHolder.instance()->drawSelf( width, height );
}

wsw::ProfilerArgsSupplier *CL_GetProfilerArgsSupplier() {
	return g_profilerHudHolder.instance();
}
wsw::ProfilerResultSink *CL_GetProfilerResultSink() {
	return g_profilerHudHolder.instance();
}

auto ProfilerHud::getArgs( wsw::ProfilingSystem::FrameGroup group ) -> wsw::ProfilerArgs {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );
	assert( group == 0 || group == 1 );
	const GroupState &groupState = m_groupStates[group];
	switch( groupState.operationMode ) {
		case GroupState::NoOp: {
			return { std::monostate() };
		};
		case GroupState::ListRoots: {
			return { wsw::ProfilerArgs::DiscoverRootScopes() };
		};
		case GroupState::ProfileCall: {
			return { wsw::StringView( groupState.selectedName.data(), groupState.selectedName.size() ) };
		};
		default: {
			wsw::failWithLogicError( "Unreachable" );
		}
	}
}

void ProfilerHud::listRoots( wsw::ProfilingSystem::FrameGroup group ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	assert( group == 0 || group == 1 );
	m_groupStates[group].operationMode = GroupState::ListRoots;
}

void ProfilerHud::select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	assert( group == 0 || group == 1 );
	GroupState &groupState = m_groupStates[group];
	groupState.operationMode = GroupState::ProfileCall;
	groupState.selectedName.assign( token.data(), token.size() );
}

void ProfilerHud::reset() {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	m_groupStates[0].operationMode = GroupState::NoOp;
	m_groupStates[1].operationMode = GroupState::NoOp;
}

void ProfilerHud::listScopes() {
	clNotice() << "Available scopes:";
	for( const auto &scope: wsw::ProfilingSystem::getRegisteredScopes() ) {
		clNotice() << scope.file << scope.readableFunction << scope.line;
	}
}

void ProfilerHud::drawSelf( unsigned width, unsigned ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	const int margin      = 8 * VID_GetPixelRatio();
	const auto lineHeight = (int)SCR_FontHeight( cls.consoleFont );

	int x = (int)width - margin;
	int y = margin;

	const auto drawTextLine = [&]( const char *text, const float *color ) {
		SCR_DrawString( x, y, ALIGN_RIGHT_TOP, text, cls.consoleFont, color );
		y += lineHeight;
	};

	// Draw help messages
	if( m_groupStates[0].operationMode == GroupState::NoOp && m_groupStates[1].operationMode == GroupState::NoOp ) {
		drawTextLine( "There are no active profiling operations", colorWhite );
		drawTextLine( "Use these commands:", colorWhite );
		drawTextLine( "pf_listroots <client|server>", colorWhite );
		drawTextLine( "pf_select <client|server> <name>", colorWhite );
		drawTextLine( "pf_reset", colorWhite );
	} else {
		const auto drawGroupHeader = [&]( size_t groupIndex ) {
			drawTextLine( groupIndex > 0 ? "SERVER" : "CLIENT", colorWhite );
		};
		for( size_t groupIndex = 0; groupIndex < std::size( m_groupStates ); ++groupIndex ) {
			const GroupState &groupState = m_groupStates[groupIndex];
			switch( groupState.operationMode ) {
				case GroupState::NoOp: {
				} break;
				case GroupState::ListRoots: {
					drawGroupHeader( groupIndex );
					if( !groupState.discoveredNames.empty() ) {
						drawTextLine( "Available profiling roots:", colorWhite );
						for( const auto &name: groupState.discoveredNames ) {
							drawTextLine( name.c_str(), colorLtGrey );
						}
					} else {
						drawTextLine( "Failed to discover profiling roots", colorRed );
					}
				} break;
				case GroupState::ProfileCall: {
					drawGroupHeader( groupIndex );
					if( !groupState.threadResults.empty() ) {
						wsw::StaticString<256> commonHeader( "Profiling " );
						constexpr unsigned maxNameLimit = 48;
						commonHeader << wsw::StringView( groupState.selectedName.c_str() ).take( maxNameLimit );

						drawTextLine( commonHeader.data(), colorWhite );

						unsigned threadIndex = 0;
						for( const ThreadProfilingResults &threadResults: groupState.threadResults ) {
							wsw::StaticString<256> threadHeader;
							(void)threadHeader.appendf( "Thread #%-2d", threadIndex );
							(void)threadHeader.appendf( " count=%-2d", threadResults.callStats.enterCount );
							(void)threadHeader.appendf( " us=%-5d", (unsigned)threadResults.callStats.totalTime );
							drawTextLine( threadHeader.data(), colorLtGrey );

							unsigned realNameLimit = 0;
							for( const auto &[name, _] : threadResults.childStats ) {
								realNameLimit = wsw::max<unsigned>( realNameLimit, name.length() );
							}
							realNameLimit = wsw::min( realNameLimit, maxNameLimit );

							threadIndex++;
							for( const auto &[name, callStats] : threadResults.childStats ) {
								wsw::StaticString<256> childDesc;
								childDesc << wsw::StringView( name.data(), name.size() ).take( realNameLimit );
								childDesc.resize( realNameLimit, ' ' );
								childDesc.append( " : "_asView );
								(void)childDesc.appendf( "count=%-4d", callStats.enterCount );
								(void)childDesc.appendf( " us=%-5d", (unsigned)callStats.totalTime );
								drawTextLine( childDesc.data(), colorMdGrey );
							}
						}
					} else {
						// TODO: This is actually unreachable
						drawTextLine( "Failed to get any profiling results", colorRed );
					}
				} break;
				default: {
					wsw::failWithLogicError( "Unreachable" );
				}
			}
		}
	}
}
