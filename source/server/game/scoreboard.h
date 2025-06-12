#ifndef WSW_7ed480ad_c97e_4b99_914c_d01a9cf6c8e3_H
#define WSW_7ed480ad_c97e_4b99_914c_d01a9cf6c8e3_H

#include <common/helpers/q_math.h>
#include <common/facilities/q_comref.h>

#include <common/types/staticvector.h>
#include <common/types/stringview.h>
#include <common/types/stringspanstorage.h>

struct Client;

template <typename> class SingletonHolder;

namespace wsw::g {

class Scoreboard : public wsw::ScoreboardShared {
	template <typename> friend class SingletonHolder;

	wsw::StaticVector<ColumnKind, kMaxColumns> m_columnKinds;
	wsw::StaticVector<unsigned, kMaxColumns> m_titleColumnSpans;
	wsw::StaticVector<unsigned, kMaxColumns> m_kindsOfCustomSlots;
	wsw::StringSpanStorage<uint8_t, uint8_t> m_columnTitlesStorage;
	wsw::StringSpanStorage<uint8_t, uint8_t> m_columnAssetsStorage;

	ReplicatedScoreboardData m_replicatedData;

	enum State : unsigned { NoState, Schema, Update };
	State m_state { NoState };

	unsigned m_pingSlot { ~0u };
	unsigned m_statusSlot { ~0u };
	unsigned m_titleSpanColumnsLeft { 0u };

	void expectState( State expectedState );
	void checkPlayerNum( unsigned playerNum ) const;
	void checkSlot( unsigned slot, ColumnKind expectedKind ) const;
	[[nodiscard]]
	auto registerUserColumn( const wsw::StringView &title, ColumnKind kind, unsigned titleColumnSpan = 1 ) -> unsigned;

	void beginUpdating();
	void endUpdating();

	[[nodiscard]]
	auto preparePlayerSpecificData( unsigned index, unsigned clientNum ) -> const ReplicatedScoreboardData *;
public:
	static void init();
	static void shutdown();
	[[nodiscard]]
	static auto instance() -> Scoreboard *;

	[[nodiscard]]
	auto getRawReplicatedDataForClient( unsigned clientNum ) -> const ReplicatedScoreboardData *;
	[[nodiscard]]
	auto getRawReplicatedDataForDemo() -> const ReplicatedScoreboardData *;

	void beginDefiningSchema();
	void endDefiningSchema();

	[[nodiscard]]
	auto registerAsset( const wsw::StringView &path ) -> unsigned;
	[[nodiscard]]
	auto registerIconColumn( const wsw::StringView &title, unsigned titleColumnSpan = 1 ) -> unsigned {
		return registerUserColumn( title, Icon, titleColumnSpan );
	}
	[[nodiscard]]
	auto registerNumberColumn( const wsw::StringView &title ) -> unsigned {
		return registerUserColumn( title, Number );
	}
	[[nodiscard]]
	auto registerGlyphColumn( const wsw::StringView &title ) -> unsigned {
		return registerUserColumn( title, Glyph );
	}

	void setPlayerIcon( const Client *client, unsigned slot, unsigned icon );
	void setPlayerNumber( const Client *client, unsigned slot, int value );
	void setPlayerGlyph( const Client *client, unsigned slot, unsigned codePoint );

	void setPlayerStatusIcon( const Client *client, unsigned icon );
	void setPlayerStatusGlyph( const Client *client, unsigned codePoint );

	void update();
};

}

#endif