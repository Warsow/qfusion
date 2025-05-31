#ifndef WSW_ff1a0640_75a0_4708_b7a8_36505b65f930_H
#define WSW_ff1a0640_75a0_4708_b7a8_36505b65f930_H

#include <QObject>

#include <common/q_arch.h>

#include <common/wswstringview.h>
#include <common/wswfs.h>
#include <common/stringspanstorage.h>

namespace wsw::ui {

class GametypeDef {
	Q_GADGET

	friend class GametypeDefParser;
	friend class GametypesModel;
public:
	enum GameplayFlags : unsigned {
		NoFlags,
		Team   = 0x1,
		Round  = 0x2,
		Race   = 0x4
	};
	Q_ENUM( GameplayFlags );
	enum BotConfig : unsigned {
		NoBots,
		ExactNumBots,
		BestNumBotsForMap,
		FixedNumBotsForMap,
		ScriptSpawnedBots
	};
private:
	struct MapInfo {
		unsigned fileNameSpanIndex { ~0u };
		unsigned fullNameSpanIndex { ~0u };
		std::optional<std::pair<unsigned, unsigned>> numPlayers;
	};

	wsw::PodVector<MapInfo> m_mapInfoList;

	unsigned m_nameSpanIndex { ~0u };
	unsigned m_titleSpanIndex { ~0u };
	unsigned m_descSpanIndex { ~0u };

	unsigned m_flags { NoFlags };
	unsigned m_botConfig { NoBots };

	std::optional<unsigned> m_exactNumBots;

	wsw::StringSpanStorage<uint16_t, uint16_t> m_stringDataStorage;

	void addMap( const wsw::StringView &mapName ) {
		m_mapInfoList.push_back( { m_stringDataStorage.add( mapName ), ~0u, std::nullopt } );
	}

	void addMap( const wsw::StringView &mapName, unsigned minPlayers, unsigned maxPlayers ) {
		assert( minPlayers && minPlayers < 32 && maxPlayers && maxPlayers < 32 && minPlayers <= maxPlayers );
		const auto numPlayers = std::make_pair( minPlayers, maxPlayers );
		m_mapInfoList.push_back( { m_stringDataStorage.add( mapName ), ~0u, numPlayers } );
	}
public:
	[[nodiscard]]
	auto getFlags() const -> GameplayFlags { return (GameplayFlags)m_flags; }
	[[nodiscard]]
	auto getName() const -> wsw::StringView { return m_stringDataStorage[m_nameSpanIndex]; }
	[[nodiscard]]
	auto getTitle() const -> wsw::StringView { return m_stringDataStorage[m_titleSpanIndex]; }
	[[nodiscard]]
	auto getDesc() const -> wsw::StringView { return m_stringDataStorage[m_descSpanIndex]; }
};

class GametypeDefParser {
	static constexpr size_t kBufferSize = 4096;

	std::optional<wsw::fs::BufferedReader> m_reader;
	char m_lineBuffer[kBufferSize];

	enum ReadResultFlags {
		None,
		Eof           = 0x1,
		HadEmptyLines = 0x2
	};

	GametypeDef m_gametypeDef;

	[[nodiscard]]
	auto readNextLine() -> std::optional<std::pair<wsw::StringView, unsigned>>;

	[[nodiscard]]
	bool expectSection( const wsw::StringView &heading );

	[[nodiscard]]
	bool parseTitle();
	[[nodiscard]]
	bool parseFlags();
	[[nodiscard]]
	bool parseBotConfig();
	[[nodiscard]]
	bool parseMaps();
	[[nodiscard]]
	bool parseDescription();

	explicit GametypeDefParser( const wsw::StringView &filePath ) {
		m_reader = wsw::fs::openAsBufferedReader( filePath );
	}

	[[nodiscard]]
	auto exec_() -> std::optional<GametypeDef>;
public:
	[[nodiscard]]
	static auto exec( const wsw::StringView &filePath ) -> std::optional<GametypeDef> {
		return GametypeDefParser( filePath ).exec_();
	}
};

}

#endif
