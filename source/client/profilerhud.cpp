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

	void drawSelf( unsigned width, unsigned height );

	void listRoots( wsw::ProfilingSystem::FrameGroup group );

	[[nodiscard]]
	bool select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token );

	void reset();

	void setEnabled( bool enabled );

	void listScopes();
private:
	void doReset();

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

void ProfilerHud::drawSelf( unsigned width, unsigned ) {
	[[maybe_unused]] volatile wsw::ScopedLock<wsw::Mutex> lock( &m_mutex );

	if( !m_isEnabled ) {
		return;
	}

	const int margin      = 8 * VID_GetPixelRatio();
	const auto lineHeight = (int)SCR_FontHeight( cls.consoleFont );

	int x = (int)width - margin;
	int y = margin;

	const auto drawTextLine = [&]( const char *text, const float *color ) {
		SCR_DrawString( x, y, ALIGN_RIGHT_TOP, text, cls.consoleFont, color );
		y += lineHeight;
	};
	const auto drawTextViewLine = [&]( const wsw::StringView &text, const float *color ) {
		if( text.isZeroTerminated() ) {
			drawTextLine( text.data(), color );
		} else {
			if( text.length() < 256 ) {
				wsw::StaticString<256> s;
				s << text;
				drawTextLine( s.data(), color );
			} else {
				static wsw::PodVector<char> s;
				s.clear();
				s.append( text.data(), text.size() );
				s.append( '\0' );
				drawTextLine( s.data(), color );
			}
		}
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
		const std::span<const wsw::ProfilingSystem::RegisteredScope> &scopes = wsw::ProfilingSystem::getRegisteredScopes();
		for( size_t groupIndex = 0; groupIndex < std::size( m_groupStates ); ++groupIndex ) {
			const GroupState &groupState = m_groupStates[groupIndex];
			switch( groupState.operationMode ) {
				case GroupState::NoOp: {
				} break;
				case GroupState::ListRoots: {
					drawGroupHeader( groupIndex );
					if( !groupState.discoveredRootScopes.empty() ) {
						drawTextLine( "Available profiling roots:", colorWhite );
						for( const unsigned scopeId: groupState.discoveredRootScopes ) {
							drawTextViewLine( scopes[scopeId].readableFunction, colorLtGrey );
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
						commonHeader << scopes[groupState.selectedScope.value()].readableFunction.take( maxNameLimit );

						drawTextLine( commonHeader.data(), colorWhite );

						unsigned threadIndex = 0;
						for( const ThreadProfilingResults &threadResults: groupState.threadResults ) {
							wsw::StaticString<256> threadHeader;
							(void)threadHeader.appendf( "Thread #%-2d", threadIndex );
							(void)threadHeader.appendf( " count=%-2d", threadResults.callStats.enterCount );
							(void)threadHeader.appendf( " us=%-5d", (unsigned)threadResults.callStats.totalTime );
							drawTextLine( threadHeader.data(), colorLtGrey );

							unsigned realNameLimit = 0;
							for( const auto &[scopeId, _] : threadResults.childStats ) {
								realNameLimit = wsw::max<unsigned>( realNameLimit, scopes[scopeId].readableFunction.length() );
							}
							realNameLimit = wsw::min( realNameLimit, maxNameLimit );

							threadIndex++;
							for( const auto &[scopeId, callStats] : threadResults.childStats ) {
								wsw::StaticString<256> childDesc;
								childDesc << scopes[scopeId].readableFunction.take( realNameLimit );
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
