#ifndef WSW_4058b151_ce14_4cc5_9704_b6233bef9d53_H
#define WSW_4058b151_ce14_4cc5_9704_b6233bef9d53_H

#include <QAbstractListModel>

#include <common/common.h>
#include <common/types/staticstring.h>
#include <common/types/staticvector.h>

struct ReplicatedScoreboardData;

namespace wsw::ui {

class PlayersModel : public QAbstractListModel {
	enum Role {
		Number = Qt::UserRole + 1,
		Nickname
	};

	wsw::StaticString<(MAX_NAME_CHARS + 1) * MAX_CLIENTS> m_oldNameData;

	struct Entry {
		std::pair<unsigned, unsigned> nameSpan;
		unsigned playerNum;
	};

	wsw::StaticVector<Entry, MAX_CLIENTS> m_players;
public:
	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	void update( const ReplicatedScoreboardData &scoreboardData );
};

}

#endif
