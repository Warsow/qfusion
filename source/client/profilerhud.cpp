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
#include "../common/wswprofiler.h"

#include <vector>
#include <string>

using wsw::operator""_asView;

class ProfilerHud : public wsw::ProfilerArgsSupplier, public wsw::ProfilerResultSink {
public:
	[[nodiscard]]
	auto getProfilerArgs( wsw::ProfilingSystem::FrameGroup group ) -> wsw::ProfilerArgs;

	void update();
	void drawSelf( int x, int y, unsigned width, unsigned height );

	void listRoots( wsw::ProfilingSystem::FrameGroup group ) {
		m_hasPendingListRootsCmd[group] = true;
	}
	[[nodiscard]]
	auto select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token ) -> std::optional<wsw::StringView>;
	void reset() {
		m_hasPendingReset = true;
	}
private:
	bool m_hasPendingListRootsCmd[2] { false, false };
	std::vector<std::string> m_availableNames[2];
	std::string m_pendingSelectedNames[2];
	bool m_hasPendingReset { false };
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
			clNotice() << "Usage: pf_listroots <group>";
		}
	});
	CL_Cmd_Register( "pf_select"_asView, []( const CmdArgs &args ) {
		bool handled = false;
		if( std::optional<wsw::ProfilingSystem::FrameGroup> maybeGroup = parseProfilerGroup( args[1] ) ) {
			const auto maybeError = g_profilerHudHolder.instance()->select( *maybeGroup, args[2] );
			if( !maybeError ) {
				handled = true;
			} else {
				clNotice() << "pf_select:" << *maybeError;
			}
		}
		if( !handled ) {
			clNotice() << "Usage: pf_select <group> <token>";
		}
	});
	CL_Cmd_Register( "pf_reset"_asView, []( const CmdArgs &args ) {
		g_profilerHudHolder.instance()->reset();
	});
	g_profilerHudHolder.init();
}

void CL_ProfilerHud_Shutdown() {
	CL_Cmd_Unregister( "pf_listroots"_asView );
	CL_Cmd_Unregister( "pf_select"_asView );
	CL_Cmd_Unregister( "pf_reset"_asView );
	g_profilerHudHolder.shutdown();
}

void CL_ProfilerHud_Update() {
	g_profilerHudHolder.instance()->update();
}

void CL_ProfilerHud_Draw( int x, int y, unsigned width, unsigned height ) {
	g_profilerHudHolder.instance()->drawSelf( x, y, width, height );
}

auto ProfilerHud::select( wsw::ProfilingSystem::FrameGroup group, const wsw::StringView &token ) -> std::optional<wsw::StringView> {
	if( m_availableNames[group].empty() ) {
		return "There's no available names to select from"_asView;
	}
	for( const auto &availableName: m_availableNames[group] ) {
		if( token.equalsIgnoreCase( wsw::StringView( availableName.data(), availableName.size() ) ) ) {
			m_pendingSelectedNames[group] = availableName;
			return std::nullopt;
		}
	}
	m_pendingSelectedNames[group].clear();
	return "Failed to find matching name"_asView;
}

void ProfilerHud::update() {
	// Sets pending arguments
}

void ProfilerHud::drawSelf( int x, int y, unsigned width, unsigned height ) {
	// Draw help messages
}
