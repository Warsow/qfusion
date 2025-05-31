#ifndef WSW_f81d4f4c_a751_44e3_a265_6a0aab68dad3_H
#define WSW_f81d4f4c_a751_44e3_a265_6a0aab68dad3_H

#include <QAbstractListModel>

#include <common/common.h>
#include <common/wswstringview.h>
#include <common/wswstaticstring.h>
#include <common/wswpodvector.h>
#include <common/wswstaticvector.h>

namespace wsw::ui {

class ActionRequestsModel : public QAbstractListModel {
	Q_OBJECT

	struct alignas( 16 ) Entry {
		QString title;
		QString desc;
		wsw::StaticString<16> tag;
		int64_t timeoutAt { 0 };
		// (command (off, len), key)
		std::pair<std::pair<unsigned, unsigned>, int> actions[9];
		wsw::PodVector<char> actionsDataBuffer;
		unsigned numActions;

		[[nodiscard]]
		auto getMatchingAction( int key ) const -> std::optional<wsw::StringView>;
	};

	wsw::StaticVector<Entry, 5> m_entries;

	enum Role {
		Title = Qt::UserRole + 1,
		Desc,
		ExpectsInput
	};

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	void removeAt( unsigned index );
public:
	[[nodiscard]]
	bool empty() const { return m_entries.empty(); }
	void update();
	void touch( const wsw::StringView &tag, unsigned timeout, const wsw::StringView &title, const wsw::StringView &actionDesc,
			 	const std::pair<wsw::StringView, int> *actionsBegin, const std::pair<wsw::StringView, int> *actionsEnd );
	void removeByTag( const wsw::StringView &tag );
	void clear();
	[[nodiscard]]
	bool handleKeyEvent( int quakeKey );
};

}

#endif