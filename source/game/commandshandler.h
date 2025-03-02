#ifndef WSW_09180ffb_50be_4694_b83a_1e336d950fea_H
#define WSW_09180ffb_50be_4694_b83a_1e336d950fea_H

#include "../common/freelistallocator.h"
#include "../common/wswstringview.h"
#include "../common/commandshandler.h"
#include "../common/cmdargs.h"

class ClientCommandsHandler : public wsw::CommandsHandler<wsw::VarArgCommandCallback<bool, edict_t *, uint64_t, const CmdArgs &>> {
	static inline const wsw::StringView kScriptTag { "script" };
	static inline const wsw::StringView kBuiltinTag { "builtin" };

	using Callback = wsw::VarArgCommandCallback<bool, edict_t *, uint64_t, const CmdArgs &>;

	class ScriptCommandCallback final : public Callback {
	public:
		explicit ScriptCommandCallback( wsw::PodVector<char> &&name ): Callback( kScriptTag, std::move( name ) ) {}
		[[nodiscard]]
		bool operator()( edict_t *arg, uint64_t, const CmdArgs &cmdArgs ) override;
	};

	class Builtin1ArgCallback final : public Callback {
		void (*m_fn)( edict_t *, const CmdArgs &cmdArgs );
	public:
		Builtin1ArgCallback( const wsw::HashedStringView &name, void (*fn)( edict_t *, const CmdArgs & ) )
			: Callback( kBuiltinTag, name ), m_fn( fn ) {}
		[[nodiscard]]
		bool operator()( edict_t *ent, uint64_t, const CmdArgs &cmdArgs ) override {
			m_fn( ent, cmdArgs ); return true;
		}
	};

	class Builtin2ArgsCallback final : public Callback {
		void (*m_fn)( edict_t *, uint64_t, const CmdArgs & );
	public:
		Builtin2ArgsCallback( const wsw::HashedStringView &name, void (*fn)( edict_t *, uint64_t, const CmdArgs & ) )
			: Callback( kBuiltinTag, name ), m_fn( fn ) {}
		[[nodiscard]]
		bool operator()( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs ) override {
			m_fn( ent, clientCommandNum, cmdArgs ); return true;
		}
	};

	static constexpr size_t kMaxCallbackSize = wsw::max( sizeof( ScriptCommandCallback ),
														 wsw::max( sizeof( Builtin1ArgCallback ), sizeof( Builtin2ArgsCallback ) ) );

	// Make sure we can always construct a new argument for addOrReplace()
	wsw::MemberBasedFreelistAllocator<kMaxCallbackSize, MAX_GAMECOMMANDS + 1> m_allocator;

	[[nodiscard]]
	static bool checkNotWriteProtected( const wsw::StringView &name );

	void addAndNotify( Callback *callback, bool allowToReplace );
public:
	static void init();
	static void shutdown();
	static ClientCommandsHandler *instance();

	ClientCommandsHandler();
	~ClientCommandsHandler();

	void precacheCommands();

	void handleClientCommand( edict_t *ent, uint64_t clientCommandNum, const CmdArgs &cmdArgs );
	void addScriptCommand( const wsw::StringView &name );

	void addBuiltin( const wsw::HashedStringView &name, void (*handler)( edict_t *, const CmdArgs & ) );
	void addBuiltin( const wsw::HashedStringView &name, void (*handler)( edict_t *, uint64_t, const CmdArgs & ) );
};

#endif
