#ifndef WSW_290ddc6d_0a8e_448e_8285_9e54dd121bc2_H
#define WSW_290ddc6d_0a8e_448e_8285_9e54dd121bc2_H

#include <QAbstractListModel>
#include <QJsonArray>

#include <optional>

#include <common/common.h>
#include <common/wswstringview.h>
#include <common/wswstaticvector.h>
#include <common/wswvector.h>
#include <common/stringspanstorage.h>

namespace wsw::ui {

class GametypeOptionsModel : public QAbstractListModel {
	Q_OBJECT
public:
	enum Kind {
		Boolean,
		ExactlyNOfList,
	};
	Q_ENUM( Kind )

	Q_PROPERTY( bool available READ isAvailable NOTIFY availableChanged )
	Q_PROPERTY( QString tabTitle READ getTabTitle NOTIFY tabTitleChanged )

	Q_SIGNAL void availableChanged( bool available );
	Q_SIGNAL void tabTitleChanged( const QString &title );

	Q_INVOKABLE QByteArray getSelectorItemIcon( int optionRow, int indexInRow ) const;
	Q_INVOKABLE QByteArray getSelectorItemTitle( int optionRow, int indexInRow ) const;

	Q_INVOKABLE void select( int optionRow, const QVariantList &selectedItemIndices );
private:
	enum class Role {
		Title = Qt::UserRole + 1,
		Kind,
		NumItems,
		SelectionLimit,
		Current
	};

	struct OptionEntry {
		unsigned titleSpanIndex;
		unsigned commandSpanIndex;
		Kind kind;
		unsigned numItems { 0 };
		unsigned selectionLimit { 0 };
		// A span in the shared buffer of entries
		std::pair<unsigned, unsigned> selectableItemsSpan;
	};

	struct SelectableItemEntry {
		unsigned titleSpanIndex;
		unsigned iconSpanIndex;
	};

	[[nodiscard]]
	bool isAvailable() const { return !m_allOptionEntries.empty(); };
	[[nodiscard]]
	auto getTabTitle() const -> QString { return m_tabTitle; }

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	[[nodiscard]]
	bool parseEntryParts( const wsw::StringView &string, wsw::StaticVector<wsw::StringView, 4> &parts );

	[[nodiscard]]
	auto addOptionListItems( const wsw::StringView &string ) -> std::optional<std::pair<unsigned, unsigned>>;

	[[nodiscard]]
	auto addString( const wsw::StringView &string ) -> unsigned;
	[[nodiscard]]
	auto getString( unsigned stringSpanIndex ) const -> QByteArray;

	[[nodiscard]]
	auto getSelectableEntry( int optionRow, int chosenIndex ) const -> const SelectableItemEntry &;

	[[nodiscard]]
	bool doReloadFromConfigStrings();
	[[nodiscard]]
	bool parseConfigString( const wsw::StringView &configString );

	[[nodiscard]]
	static bool validateBooleanOptionValue( int rawValue );
	[[nodiscard]]
	static bool validateExactlyNOfListOptionValue( int rawValue, unsigned selectionLimit, unsigned numItems );

	void clear();

	QString m_tabTitle;

	static inline const QVector<int> kCurrentRoleAsVector { (int)Role::Current };

	static constexpr unsigned kMaxOptions = MAX_GAMETYPE_OPTIONS;
	static constexpr unsigned kMaxCommandLen = 24;
	static constexpr unsigned kMaxOptionLen = 12;


	/// A datum of all possible options supplied by the server once (some could be currently hidden).
	wsw::StaticVector<OptionEntry, kMaxOptions> m_allOptionEntries;
	/// A flattened list of selectable items of every option (that are not of Boolean kind)
	wsw::Vector<SelectableItemEntry> m_selectableItemEntries;

	struct OptionRow { unsigned entryIndex; int currentValue; };
	/// Maps 1<->1 with displayed rows.
	wsw::Vector<OptionRow> m_allowedOptions;

	wsw::StringSpanStorage<unsigned, unsigned> m_stringDataStorage;
public:
	void reload();
	void handleOptionsStatusCommand( const wsw::StringView &status );
};

}

#endif