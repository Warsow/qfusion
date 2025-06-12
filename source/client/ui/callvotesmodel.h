#ifndef WSW_7bec9edc_f137_4385_ad94_191ee7088622_H
#define WSW_7bec9edc_f137_4385_ad94_191ee7088622_H

#include <QAbstractListModel>
#include <QJsonArray>

#include <common/types/stringview.h>
#include <common/types/vector.h>
#include <common/types/stringspanstorage.h>
#include <common/facilities/q_comref.h>

namespace wsw::ui {

class CallvotesModelProxy;

class CallvotesGroupsModel : public QAbstractListModel {
	friend class CallvotesListModel;

	enum Role {
		Name = Qt::UserRole + 1,
		Group
	};

	CallvotesModelProxy *m_proxy;
	wsw::StaticVector<unsigned, MAX_CALLVOTES> m_roleIndices;

	explicit CallvotesGroupsModel( CallvotesModelProxy *proxy ) : m_proxy( proxy ) {}

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
};

class CallvotesListModel : public QAbstractListModel {
	friend class CallvotesModelProxy;

	Q_OBJECT
public:
	enum Kind {
		NoArgs,
		Boolean,
		Number,
		Player,
		Minutes,
		MapList,
		Options,
	};
	Q_ENUM( Kind );

	enum Flags {
		Regular = 0x1,
		Operator = 0x2,
	};
	Q_ENUM( Flags );
private:
	enum Role {
		Name = Qt::UserRole + 1,
		Desc,
		Flags,
		Group,
		ArgsKind,
		ArgsHandle,
		Current,
	};

	static inline const QVector<int> kRoleCurrentChangeset { Current };

	CallvotesGroupsModel m_groupsModel;
	CallvotesModelProxy *const m_proxy;
	wsw::StaticVector<int, MAX_CALLVOTES> m_parentEntryNums;
	wsw::StaticVector<int, MAX_CALLVOTES> m_displayedEntryNums;

	bool m_hasSetGroupModelOwnership { false };

	void beginReloading();
	void addNum( int num );
	void endReloading();
public:
	explicit CallvotesListModel( CallvotesModelProxy *proxy ) : m_groupsModel( proxy ), m_proxy( proxy ) {}

	void notifyOfChangesAtNum( int num );

	// Generic signals are not really usable in QML, add the our one
	Q_SIGNAL void currentChanged( int index, QVariant value );

	Q_INVOKABLE QJsonArray getOptionsList( int handle ) const;

	Q_INVOKABLE void setGroupFilter( int group );

	[[nodiscard]]
	Q_INVOKABLE QObject *getGroupsModel();

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
};

class CallvotesModelProxy {
	friend class CallvotesListModel;
	friend class CallvotesGroupsModel;
public:
	struct Entry {
		QString name;
		QString desc;
		QString current;
		unsigned flags;
		unsigned group;
		CallvotesListModel::Kind kind;
		int argsHandle;
	};
private:
	struct OptionTokens {
		wsw::PodVector<char> content;
		wsw::PodVector<std::pair<uint16_t, uint16_t>> spans;
	};

	wsw::Vector<Entry> m_entries;
	wsw::Vector<std::pair<OptionTokens, int>> m_options;

	static constexpr unsigned kMaxGroups = 16;

	wsw::StringSpanStaticStorage<uint8_t, uint8_t, kMaxGroups, 256> m_groupDataStorage;

	CallvotesListModel m_regularModel { this };
	CallvotesListModel m_operatorModel { this };

	[[nodiscard]]
	auto addArgs( const std::optional<wsw::StringView> &maybeArgs )
		-> std::optional<std::pair<CallvotesListModel::Kind, std::optional<int>>>;

	[[nodiscard]]
	auto parseAndAddOptions( const wsw::StringView &encodedOptions ) -> std::optional<int>;

	[[nodiscard]]
	bool tryParsingCallvoteGroups( const wsw::StringView &groups );

	[[nodiscard]]
	auto tryReloading() -> std::optional<wsw::StringView>;

	[[nodiscard]]
	auto findGroupByTag( const wsw::StringView &tag ) const -> std::optional<unsigned>;
public:
	[[nodiscard]]
	auto getEntry( int entryNum ) const -> const Entry & { return m_entries[entryNum]; }

	[[nodiscard]]
	auto getRegularModel() -> CallvotesListModel * { return &m_regularModel; }
	[[nodiscard]]
	auto getOperatorModel() -> CallvotesListModel * { return &m_operatorModel; }

	void reload();

	void handleConfigString( unsigned configStringNum, const wsw::StringView &string );
};

}

#endif
