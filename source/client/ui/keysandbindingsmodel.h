#ifndef WSW_531a9137_a4c6_43b4_961e_eda1109e7c80_H
#define WSW_531a9137_a4c6_43b4_961e_eda1109e7c80_H

#include <QObject>
#include <QColor>
#include <QJsonArray>
#include <QJsonObject>

#include <array>
#include <string>
#include <unordered_map>

#include <common/types/podvector.h>
#include <common/types/stringview.h>

class QQuickItem;

namespace wsw::ui {

class QtUISystem;

struct CommandsColumnEntry;

class KeysAndBindingsModel : public QObject {
	Q_OBJECT

	friend class QtUISystem;
public:
	enum BindingGroup {
		MovementGroup = 1,
		ActionGroup,
		WeaponGroup,
		RespectGroup,
		UnknownGroup
	};
	Q_ENUM( BindingGroup )

	[[nodiscard]]
	Q_INVOKABLE QColor colorForGroup( int group ) const;

	Q_SIGNAL void keyExternalHighlightChanged( int targetQuakeKey, bool targetHighlighted );
	Q_SIGNAL void commandExternalHighlightChanged( int targetCommandNum, bool targetHighlighted );

	Q_INVOKABLE void onKeyItemContainsMouseChanged( int quakeKey, bool contains );
	Q_INVOKABLE void onCommandItemContainsMouseChanged( int commandNum, bool contains );

	Q_INVOKABLE void startTrackingUpdates();
	Q_INVOKABLE void stopTrackingUpdates();

	Q_INVOKABLE void bind( int quakeKey, int commandNum );
	Q_INVOKABLE void unbind( int quakeKey );

	[[nodiscard]]
	Q_INVOKABLE QByteArray getKeyNameToDisplay( int quakeKey ) const;
	[[nodiscard]]
	Q_INVOKABLE QByteArray getCommandNameToDisplay( int commandNum ) const;

	[[nodiscard]]
	Q_INVOKABLE int getMouseWheelKeyCode( bool scrollUp ) const;
	[[nodiscard]]
	Q_INVOKABLE int getMouseButtonKeyCode( int buttonNum ) const;

	Q_SIGNAL void mouseKeyBindingChanged( int changedQuakeKey );
	[[nodiscard]]
	Q_INVOKABLE int getMouseKeyBindingGroup( int quakeKey );
private:
	QJsonArray m_keyboardMainPadRowModel[6];
	QJsonArray m_keyboardArrowPadRowModel[5];
	QJsonArray m_keyboardNumPadRowModel[5];

	QJsonArray m_commandsMovementColumnModel;
	QJsonArray m_commandsActionsColumnModel;
	QJsonArray m_commandsWeaponsColumnModel[2];
	QJsonArray m_commandsRespectColumnModel[2];

	// We split properties for every row to avoid a full reload of every row on model change

	Q_SIGNAL void keyboardMainPadRow1Changed();
	Q_SIGNAL void keyboardMainPadRow2Changed();
	Q_SIGNAL void keyboardMainPadRow3Changed();
	Q_SIGNAL void keyboardMainPadRow4Changed();
	Q_SIGNAL void keyboardMainPadRow5Changed();
	Q_SIGNAL void keyboardMainPadRow6Changed();

	Q_SIGNAL void keyboardArrowPadRow1Changed();
	Q_SIGNAL void keyboardArrowPadRow2Changed();
	Q_SIGNAL void keyboardArrowPadRow3Changed();
	Q_SIGNAL void keyboardArrowPadRow4Changed();
	Q_SIGNAL void keyboardArrowPadRow5Changed();

	Q_SIGNAL void keyboardNumPadRow1Changed();
	Q_SIGNAL void keyboardNumPadRow2Changed();
	Q_SIGNAL void keyboardNumPadRow3Changed();
	Q_SIGNAL void keyboardNumPadRow4Changed();
	Q_SIGNAL void keyboardNumPadRow5Changed();

	Q_SIGNAL void commandsMovementColumnChanged();
	Q_SIGNAL void commandsActionsColumnChanged();
	Q_SIGNAL void commandsWeaponsColumn1Changed();
	Q_SIGNAL void commandsWeaponsColumn2Changed();
	Q_SIGNAL void commandsRespectColumn1Changed();
	Q_SIGNAL void commandsRespectColumn2Changed();

	Q_PROPERTY( QJsonArray keyboardMainPadRow1 MEMBER ( m_keyboardMainPadRowModel[0] ) NOTIFY keyboardMainPadRow1Changed );
	Q_PROPERTY( QJsonArray keyboardMainPadRow2 MEMBER ( m_keyboardMainPadRowModel[1] ) NOTIFY keyboardMainPadRow2Changed );
	Q_PROPERTY( QJsonArray keyboardMainPadRow3 MEMBER ( m_keyboardMainPadRowModel[2] ) NOTIFY keyboardMainPadRow3Changed );
	Q_PROPERTY( QJsonArray keyboardMainPadRow4 MEMBER ( m_keyboardMainPadRowModel[3] ) NOTIFY keyboardMainPadRow4Changed );
	Q_PROPERTY( QJsonArray keyboardMainPadRow5 MEMBER ( m_keyboardMainPadRowModel[4] ) NOTIFY keyboardMainPadRow5Changed );
	Q_PROPERTY( QJsonArray keyboardMainPadRow6 MEMBER ( m_keyboardMainPadRowModel[5] ) NOTIFY keyboardMainPadRow6Changed );

	Q_PROPERTY( QJsonArray keyboardArrowPadRow1 MEMBER ( m_keyboardArrowPadRowModel[0] ) NOTIFY keyboardArrowPadRow1Changed );
	Q_PROPERTY( QJsonArray keyboardArrowPadRow2 MEMBER ( m_keyboardArrowPadRowModel[1] ) NOTIFY keyboardArrowPadRow2Changed );
	Q_PROPERTY( QJsonArray keyboardArrowPadRow3 MEMBER ( m_keyboardArrowPadRowModel[2] ) NOTIFY keyboardArrowPadRow3Changed );
	Q_PROPERTY( QJsonArray keyboardArrowPadRow4 MEMBER ( m_keyboardArrowPadRowModel[3] ) NOTIFY keyboardArrowPadRow4Changed );
	Q_PROPERTY( QJsonArray keyboardArrowPadRow5 MEMBER ( m_keyboardArrowPadRowModel[4] ) NOTIFY keyboardArrowPadRow5Changed );
	
	Q_PROPERTY( QJsonArray keyboardNumPadRow1 MEMBER ( m_keyboardNumPadRowModel[0] ) NOTIFY keyboardNumPadRow1Changed );
	Q_PROPERTY( QJsonArray keyboardNumPadRow2 MEMBER ( m_keyboardNumPadRowModel[1] ) NOTIFY keyboardNumPadRow2Changed );
	Q_PROPERTY( QJsonArray keyboardNumPadRow3 MEMBER ( m_keyboardNumPadRowModel[2] ) NOTIFY keyboardNumPadRow3Changed );
	Q_PROPERTY( QJsonArray keyboardNumPadRow4 MEMBER ( m_keyboardNumPadRowModel[3] ) NOTIFY keyboardNumPadRow4Changed );
	Q_PROPERTY( QJsonArray keyboardNumPadRow5 MEMBER ( m_keyboardNumPadRowModel[4] ) NOTIFY keyboardNumPadRow5Changed );

	Q_PROPERTY( QJsonArray commandsMovementColumn MEMBER m_commandsMovementColumnModel NOTIFY commandsMovementColumnChanged );
	Q_PROPERTY( QJsonArray commandsActionsColumn MEMBER m_commandsActionsColumnModel NOTIFY commandsActionsColumnChanged );
	Q_PROPERTY( QJsonArray commandsWeaponsColumn1 MEMBER ( m_commandsWeaponsColumnModel[0] ) NOTIFY commandsWeaponsColumn1Changed );
	Q_PROPERTY( QJsonArray commandsWeaponsColumn2 MEMBER ( m_commandsWeaponsColumnModel[1] ) NOTIFY commandsWeaponsColumn2Changed );
	Q_PROPERTY( QJsonArray commandsRespectColumn1 MEMBER ( m_commandsRespectColumnModel[0] ) NOTIFY commandsRespectColumn1Changed );
	Q_PROPERTY( QJsonArray commandsRespectColumn2 MEMBER ( m_commandsRespectColumnModel[1] ) NOTIFY commandsRespectColumn2Changed );

	template <typename Array>
	void reloadKeyBindings( Array &array, const wsw::StringView &changedSignalPrefix );

	void reloadKeyBindings( QJsonArray *rowsBegin, QJsonArray *rowsEnd, const wsw::StringView &changedSignalPrefix );

	[[nodiscard]]
	bool reloadRowKeyBindings( QJsonArray &row );

	[[nodiscard]]
	bool reloadRowKeyEntry( QJsonValueRef ref );

	void reloadMouseKeyBindings();
	[[nodiscard]]
	bool reloadMouseKeyBinding( int quakeKey );

	void reloadColumnCommandBindings( QJsonArray &columns, const wsw::StringView &changedSignal );

	[[nodiscard]]
	auto registerKnownCommands( std::unordered_map<std::string, int> &dest,
							    const CommandsColumnEntry *begin,
							    const CommandsColumnEntry *end,
							    BindingGroup bindingGroup,
							    int startFromNum ) -> int;

	template <typename Array>
	auto registerKnownCommands( std::unordered_map<std::string, int> &dest,
								const Array &commands,
								BindingGroup bindingGroup,
								int startFromNum ) -> int;

	void registerKnownCommands();

	[[nodiscard]]
	auto commandsColumnToJsonArray( struct CommandsColumnEntry *begin, struct CommandsColumnEntry *end ) -> QJsonArray;

	template <typename Column>
	[[nodiscard]]
	auto commandsColumnToJsonArray( Column &column ) -> QJsonArray;

	[[nodiscard]]
	auto getCommandNum( const wsw::StringView &command ) const -> std::optional<int>;

	void reload();

	KeysAndBindingsModel();
private:
	void checkUpdates();

	std::array<std::optional<BindingGroup>, 10> m_mouseKeyBindingGroups;

	static constexpr auto kMaxCommands = 48;
	std::array<BindingGroup, kMaxCommands> m_commandBindingGroups;

	std::unordered_map<std::string, int> m_otherBindingNums;
	std::unordered_map<std::string, int> m_weaponBindingNums;
	std::unordered_map<std::string, int> m_respectBindingNums;

	std::array<wsw::StringView, kMaxCommands> m_commandsForGlobalNums;
	std::array<wsw::StringView, kMaxCommands> m_commandsDescForGlobalNums;

	// TODO: Optimize
	std::array<wsw::PodVector<int>, kMaxCommands> m_boundKeysForCommand;

	std::array<std::string, 256> m_lastKeyBindings;

	bool m_isTrackingUpdates { false };
};

}

#endif
