#include "huddatamodel.h"
#include "local.h"
#include "cgameimports.h"
#include <common/gs_public.h>
#include <common/wswalgorithm.h>
#include <client/client.h>
#include <client/cgame/mediacache.h>

#include <QColor>
#include <QQmlEngine>

using wsw::operator""_asView;

// TODO: Should we use "hud_" prefix?

static StringConfigVar v_regularHud { "cg_regularHud"_asView, { .byDefault = "default"_asView, .flags = CVAR_ARCHIVE } };
static StringConfigVar v_miniviewHud { "cg_miniviewHud"_asView, { .byDefault = "default"_asView, .flags = CVAR_ARCHIVE } };

static IntConfigVar v_allowedNumRowsForMiniviewPane[2] {
	{ "hud_allowedNumRowsForMiniviewPane1"_asView, { .byDefault = 4, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
	{ "hud_allowedNumRowsForMiniviewPane2"_asView, { .byDefault = 4, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
};
static IntConfigVar v_preferredNumRowsForMiniviewPane[2] {
	{ "hud_preferredNumRowsForMiniviewPane1"_asView, { .byDefault = 3, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
	{ "hud_preferredNumRowsForMiniviewPane2"_asView, { .byDefault = 3, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
};
static IntConfigVar v_allowedNumColumnsForMiniviewPane[2] {
	{ "hud_allowedNumColumnsForMiniviewPane1"_asView, { .byDefault = 2, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
	{ "hud_allowedNumColumnsForMiniviewPane2"_asView, { .byDefault = 2, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
};
static IntConfigVar v_preferredNumColumnsForMiniviewPane[2] {
	{ "hud_preferredNumColumnsForMiniviewPane1"_asView, { .byDefault = 1, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
	{ "hud_preferredNumColumnsForMiniviewPane2"_asView, { .byDefault = 1, .min = inclusive( 1 ), .max = exclusive( 100 ), .flags = CVAR_ARCHIVE } },
};

namespace wsw::ui {

class WeaponPropsCache {
	struct Entry {
		QByteArray fullName;
		QByteArray shortName;
		QColor color;
		QByteArray iconPath;
		QByteArray modelPath;
	};

	wsw::StaticVector<Entry, WEAP_TOTAL - 1> m_entries;
	mutable QStringList m_availableRegularCrosshairs;
	mutable QStringList m_availableStrongCrosshairs;
public:
	WeaponPropsCache() {
		QByteArray iconPrefix( "image://wsw/gfx/hud/icons/weapon/" );
		QByteArray modelPrefix( "models/weapons/" );

		m_entries.emplace_back( Entry {
			"Gunblade", "gb", QColor::fromRgbF( 1.0, 1.0, 0.5 ),
			iconPrefix + "gunblade", modelPrefix + "gunblade/gunblade.md3",
		});
		m_entries.emplace_back( Entry {
			"Machinegun", "mg", QColor::fromRgbF( 0.5, 0.5, 0.5 ),
			iconPrefix + "machinegun", modelPrefix + "machinegun/machinegun.md3",
		});
		m_entries.emplace_back( Entry {
			"Riotgun", "rg", QColor::fromRgbF( 1.0, 0.5, 0.0 ),
			iconPrefix + "riot", modelPrefix + "riotgun/riotgun.md3",
		});
		m_entries.emplace_back( Entry {
			"Grenade Launcher", "gl", QColor::fromRgbF( 0.0, 0.0, 1.0 ),
			iconPrefix + "grenade", modelPrefix + "glauncher/glauncher.md3"
		});
		m_entries.emplace_back( Entry {
			"Rocket Launcher", "rl", QColor::fromRgbF( 0.7, 0.0, 0.0 ),
			iconPrefix + "rocket", modelPrefix + "rlauncher/rlauncher.md3"
		});
		m_entries.emplace_back( Entry {
			"Plasmagun", "pg", QColor::fromRgbF( 0.0, 0.7, 0.0 ),
			iconPrefix + "plasma", modelPrefix + "plasmagun/plasmagun.md3"
		});
		m_entries.emplace_back( Entry {
			"Lasergun", "lg",  QColor::fromRgbF( 0.9, 0.9, 0.0 ),
			iconPrefix + "laser", modelPrefix + "lasergun/lasergun.md3"
		});
		m_entries.emplace_back( Entry {
			"Electrobolt", "eb", QColor::fromRgbF( 0.0, 0.5, 1.0 ),
			iconPrefix + "electro", modelPrefix + "electrobolt/electrobolt.md3"
		});
		m_entries.emplace_back( Entry {
			"Shockwave", "sw", QColor::fromRgbF( 0.3, 0.7, 1.0 ),
			iconPrefix + "shockwave", modelPrefix + "shockwave/shockwave.md3"
		});
		m_entries.emplace_back( Entry {
			"Instagun", "ig", QColor::fromRgbF( 0.0, 1.0, 1.0 ),
			iconPrefix + "instagun", modelPrefix + "instagun/instagun.md3"
		});
	}

	[[nodiscard]]
	auto getWeaponIconPath( int weapon ) const -> QByteArray {
		assert( (unsigned)weapon < (unsigned)WEAP_TOTAL );
		return weapon ? m_entries[weapon - 1].iconPath : QByteArray();
	}

	[[nodiscard]]
	auto getWeaponModelPath( int weapon ) const -> QByteArray {
		assert( (unsigned)weapon < (unsigned)WEAP_TOTAL );
		return weapon ? m_entries[weapon - 1].modelPath : QByteArray();
	}

	[[nodiscard]]
	auto getWeaponFullName( int weapon ) const -> QByteArray {
		assert( weapon && (unsigned)weapon < (unsigned)WEAP_TOTAL );
		return m_entries[weapon - 1].fullName;
	}

	[[nodiscard]]
	auto getWeaponShortName( int weapon ) const -> QByteArray {
		assert( weapon && (unsigned)weapon < (unsigned)WEAP_TOTAL );
		return m_entries[weapon - 1].shortName;
	}

	[[nodiscard]]
	auto getWeaponColor( int weapon ) const -> QColor {
		assert( weapon && (unsigned)weapon < (unsigned)WEAP_TOTAL );
		return m_entries[weapon - 1].color;
	}

	[[nodiscard]]
	auto getAvailableRegularCrosshairs() const -> QStringList {
		if( m_availableRegularCrosshairs.empty() ) {
			const wsw::StringSpanStorage<unsigned, unsigned> &crosshairs = getRegularCrosshairFiles();
			m_availableRegularCrosshairs.reserve( (int)crosshairs.size() );
			for( const wsw::StringView &name: crosshairs ) {
				m_availableRegularCrosshairs.append( QString::fromLatin1( name.data(), (int)name.size() ) );
			}
		}
		return m_availableRegularCrosshairs;
	}

	[[nodiscard]]
	auto getAvailableStrongCrosshairs() const -> QStringList {
		if( m_availableStrongCrosshairs.empty() ) {
			const wsw::StringSpanStorage<unsigned, unsigned> &crosshairs = getStrongCrosshairFiles();
			m_availableStrongCrosshairs.reserve( (int)crosshairs.size() );
			for( const wsw::StringView &name: crosshairs ) {
				m_availableStrongCrosshairs.append( QString::fromLatin1( name.data(), (int)name.size() ) );
			}
		}
		return m_availableStrongCrosshairs;
	}

	[[nodiscard]]
	auto getFragsFeedIconPath( unsigned meansOfDeath ) const -> QByteArray {
		assert( meansOfDeath >= MOD_GUNBLADE_W && meansOfDeath < (unsigned)MOD_COUNT );
		// No static guarantees here but we can spot bugs easily in this case
		if( meansOfDeath >= MOD_GUNBLADE_W && meansOfDeath <= MOD_INSTAGUN_S ) {
			return getWeaponIconPath( (int)( WEAP_GUNBLADE + ( meansOfDeath - MOD_GUNBLADE_W ) / 2 ) );
		}
		// TODO: What's the point of having these separate values?
		if( meansOfDeath == MOD_GRENADE_SPLASH_S || meansOfDeath == MOD_GRENADE_SPLASH_W ) {
			return getWeaponIconPath( WEAP_GRENADELAUNCHER );
		}
		if( meansOfDeath == MOD_ROCKET_SPLASH_S || meansOfDeath == MOD_ROCKET_SPLASH_W ) {
			return getWeaponIconPath( WEAP_ROCKETLAUNCHER );
		}
		if( meansOfDeath == MOD_PLASMA_SPLASH_S || meansOfDeath == MOD_PLASMA_SPLASH_W ) {
			return getWeaponIconPath( WEAP_PLASMAGUN );
		}
		if( meansOfDeath >= MOD_SHOCKWAVE_SPLASH_W && meansOfDeath <= MOD_SHOCKWAVE_CORONA_S ) {
			return getWeaponIconPath( WEAP_SHOCKWAVE );
		}
		return getWeaponIconPath( WEAP_GUNBLADE );
	}
};

static WeaponPropsCache weaponPropsCache;

auto InventoryModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Displayed, "displayed" },
		{ HasWeapon, "hasWeapon" },
		{ Active, "active" },
		{ IconPath, "iconPath" },
		{ Color, "color" },
		{ WeakAmmoCount, "weakAmmoCount" },
		{ StrongAmmoCount, "strongAmmoCount" }
	};
}

auto InventoryModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto InventoryModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < m_entries.size() ) {
			switch( role ) {
				case Displayed: return m_entries[row].displayed;
				case HasWeapon: return m_entries[row].hasWeapon;
				case Active: return m_entries[row].active;
				case IconPath: return weaponPropsCache.getWeaponIconPath( m_entries[row].weaponNum );
				case Color: return weaponPropsCache.getWeaponColor( m_entries[row].weaponNum );
				case WeakAmmoCount: return m_entries[row].weakCount;
				case StrongAmmoCount: return m_entries[row].strongCount;
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

InventoryModel::InventoryModel() {
	while( !m_entries.full() ) {
		m_entries.emplace_back( Entry {
			.weaponNum   = (int)( WEAP_GUNBLADE + m_entries.size() ),
			.weakCount   = 0,
			.strongCount = 0,
			.displayed   = false,
			.hasWeapon   = false,
			.active      = false,
		});
	}
	assert( WEAP_TOTAL - WEAP_GUNBLADE == m_entries.size() );
	static_assert( WEAP_TOTAL - WEAP_GUNBLADE == kNumInventoryItems );
}

void InventoryModel::checkPropertyChanges( unsigned viewStateIndex ) {
	const int activeWeapon = CG_ActiveWeapon( viewStateIndex );

	assert( WEAP_TOTAL - WEAP_GUNBLADE == m_entries.size() );
	static_assert( WEAP_TOTAL - WEAP_GUNBLADE == kNumInventoryItems );
	for( int weapon = WEAP_GUNBLADE; weapon < WEAP_TOTAL; ++weapon ) {
		Entry &entry = m_entries[weapon - 1];
		assert( entry.weaponNum == weapon );

		const auto [weakCount, strongCount] = CG_WeaponAmmo( viewStateIndex, weapon );
		const bool hasWeapon = CG_HasWeapon( viewStateIndex, weapon );

		m_changedRolesStorage.clear();
		if( entry.weakCount != weakCount ) {
			if( weapon != WEAP_GUNBLADE ) {
				entry.weakCount = weakCount;
			} else {
				entry.weakCount = -1;
			}
			m_changedRolesStorage.append( WeakAmmoCount );
		}
		if( entry.strongCount != strongCount ) {
			if( weapon != WEAP_GUNBLADE ) {
				entry.strongCount = strongCount;
			} else {
				entry.strongCount = -strongCount;
			}
			m_changedRolesStorage.append( StrongAmmoCount );
		}
		if( entry.hasWeapon != hasWeapon ) {
			entry.hasWeapon = hasWeapon;
			m_changedRolesStorage.append( HasWeapon );
		}

		if( const bool canBeDisplayed = weakCount | strongCount | hasWeapon; entry.displayed != canBeDisplayed ) {
			entry.displayed = canBeDisplayed;
			m_changedRolesStorage.append( Displayed );
		}

		if( const bool isActive = ( weapon == activeWeapon ); entry.active != isActive ) {
			entry.active = isActive;
			m_changedRolesStorage.append( Active );
		}

		if( !m_changedRolesStorage.empty() ) {
			const QModelIndex modelIndex( createIndex( weapon - 1, 0 ) );
			Q_EMIT dataChanged( modelIndex, modelIndex, m_changedRolesStorage );
		}
	}
}

auto TeamListModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Health, "health" }, { Armor, "armor" }, { WeaponIconPath, "weaponIconPath" },
		{ Nickname, "nickname" }, { Powerups, "powerups" }
	};
}

auto TeamListModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto TeamListModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const auto row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			switch( role ) {
				case Health: return m_entries[row].health;
				case Armor: return m_entries[row].armor;
				case WeaponIconPath: return weaponPropsCache.getWeaponIconPath( (int)m_entries[row].weapon );
				case Nickname: return toStyledText( CG_PlayerName( m_entries[row].playerNum ) );
				case Powerups: return m_entries[row].powerups;
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

void TeamListModel::resetWithScoreboardData( const ReplicatedScoreboardData &scoreboardData ) {
	beginResetModel();
	fillEntries( scoreboardData, m_entries );
	saveNicknameUpdateCounters( m_entries );
	endResetModel();
}

void TeamListModel::saveNicknameUpdateCounters( const EntriesVector &entries ) {
	const auto *const tracker = wsw::ui::NameChangesTracker::instance();
	for( const Entry &entry: entries ) {
		m_nicknameUpdateCounters[entry.playerNum] = tracker->getLastNicknameUpdateCounter( entry.playerNum );
	}
}

void TeamListModel::fillEntries( const ReplicatedScoreboardData &scoreboardData, EntriesVector &entries ) {
	entries.clear();
	for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
		if( !scoreboardData.isPlayerGhosting( i ) && scoreboardData.getPlayerTeam( i ) == m_team ) {
			const unsigned playerNum = scoreboardData.getPlayerNum( i );
			if( playerNum != m_povPlayerNum ) {
				entries.emplace_back( Entry {
					.playerNum = playerNum,
					.health    = scoreboardData.getPlayerHealth( i ),
					.armor     = scoreboardData.getPlayerArmor( i ),
					.weapon    = scoreboardData.getPlayerWeapon( i ),
					.powerups  = scoreboardData.getPlayerPowerupBits( i ),
				});
			}
		}
	}

	// Sort by player num for now
	const auto cmp = []( const Entry &lhs, const Entry &rhs ) {
		return lhs.playerNum < rhs.playerNum;
	};
	wsw::sortPodNonSpeedCritical( entries.begin(), entries.end(), cmp );
}

void TeamListModel::update( const ReplicatedScoreboardData &scoreboardData, unsigned povPlayerNum ) {
	int povTeam = -1;
	for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
		if( scoreboardData.getPlayerNum( i ) == povPlayerNum ) {
			povTeam = scoreboardData.getPlayerTeam( i );
			break;
		}
	}

	if( povTeam < 0 ) {
		wsw::failWithLogicError( "Failed to find the current POV in the scoreboard" );
	}

	// The POV player is in another team.
	// This also naturally covers many POV changes.
	if( povTeam != m_team ) {
		m_team = povTeam;
		assert( m_team == TEAM_ALPHA || m_team == TEAM_BETA );
		m_povPlayerNum = povPlayerNum;
		resetWithScoreboardData( scoreboardData );
		return;
	}

	// Handle remaining POV change cases
	if( m_povPlayerNum != povPlayerNum ) {
		m_povPlayerNum = povPlayerNum;
		resetWithScoreboardData( scoreboardData );
		return;
	}

	// Backup entries
	m_oldEntries.clear();
	std::copy( std::begin( m_entries ), std::end( m_entries ), std::back_inserter( m_oldEntries ) );

	fillEntries( scoreboardData, m_entries );

	// TODO: This can be optimized in case of having a single-row difference
	if( m_entries.size() != m_oldEntries.size() ) {
		beginResetModel();
		saveNicknameUpdateCounters( m_entries );
		endResetModel();
	} else {
		const auto *const tracker = wsw::ui::NameChangesTracker::instance();
		constexpr int kFullUpdate = 1000;
		// Just dispatch updates
		for( unsigned i = 0; i < m_entries.size(); ++i ) {
			const auto &oldEntry = m_oldEntries[i];
			const auto &entry = m_entries[i];
			int numUpdates = 0;
			if( entry.playerNum != oldEntry.playerNum ) {
				// Do a full reset
				numUpdates = kFullUpdate;
			} else {
				const unsigned counter = tracker->getLastNicknameUpdateCounter( entry.playerNum );
				if( counter != m_nicknameUpdateCounters[entry.playerNum] ) {
					m_nicknameUpdateCounters[entry.playerNum] = counter;
					numUpdates = kFullUpdate;
				}
			}

			bool hasHealthUpdates = false;
			bool hasArmorUpdates  = false;
			bool hasWeaponUpdates = false;
			if( numUpdates != kFullUpdate ) {
				hasHealthUpdates = entry.health != oldEntry.health;
				numUpdates += hasHealthUpdates;
				hasArmorUpdates = entry.armor != oldEntry.armor;
				numUpdates += hasArmorUpdates;
				hasWeaponUpdates = entry.weapon != oldEntry.weapon;
				numUpdates += hasWeaponUpdates;
			}

			if( numUpdates > 0 ) {
				// Specify a full row update by default
				const QVector<int> *changedRoles = nullptr;
				// Optimize for most frequent updates
				if( numUpdates == 1 ) {
					if( hasHealthUpdates ) {
						changedRoles = &kHealthAsRole;
					} else if( hasArmorUpdates ) {
						changedRoles = &kArmorAsRole;
					} else if( hasWeaponUpdates ) {
						changedRoles = &kWeaponIconPathAsRole;
					}
				} else if( numUpdates == 2 ) {
					if( hasHealthUpdates && hasArmorUpdates ) {
						changedRoles = &kHealthAndArmorAsRole;
					}
				}

				const QModelIndex modelIndex( createIndex( (int)i, 0 ) );
				if( changedRoles ) {
					Q_EMIT dataChanged( modelIndex, modelIndex, *changedRoles );
				} else {
					Q_EMIT dataChanged( modelIndex, modelIndex );
				}
			}
		}
	}
}

auto FragsFeedModel::roleNames() const -> QHash<int, QByteArray> {
	return { { { Victim, "victim" }, { Attacker, "attacker" }, { IconPath, "iconPath" } } };
}

auto FragsFeedModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto FragsFeedModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( const auto row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			const auto &entry = m_entries[row];
			switch( role ) {
				case Victim: return toDisplayedName( entry.victimName.asView(), entry.victimTeamColor );
				case Attacker: return toDisplayedName( entry.attackerName.asView(), entry.attackerTeamColor );
				case IconPath: return weaponPropsCache.getFragsFeedIconPath( entry.meansOfDeath );
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

auto FragsFeedModel::toDisplayedName( const wsw::StringView &rawName, const std::optional<int> &teamColor ) -> QString {
	// Optimize for retrieval of an empty attacker
	if( !rawName.empty() ) {
		if( !teamColor ) {
			return toStyledText( rawName );
		} else {
			return wrapInColorTags( rawName, *teamColor );
		}
	}
	return QString();
}

void FragsFeedModel::addFrag( const std::pair<wsw::StringView, int> &victimAndTeam,
							  int64_t timestamp, unsigned meansOfDeath,
							  const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam ) {
	// TODO: Add to pending entries in case of a huge feed size?
	if( m_entries.full() ) {
		beginRemoveRows( QModelIndex(), 0, 0 );
		m_entries.erase( m_entries.begin() );
		endRemoveRows();
	}

	beginInsertRows( QModelIndex(), (int)m_entries.size(), (int)m_entries.size() );

	m_entries.emplace_back( Entry {} );
	Entry &entry = m_entries.back();
	entry.meansOfDeath = meansOfDeath;
	entry.timestamp = timestamp;

	if( m_hudDataModel->m_hasTwoTeams ) {
		entry.victimName.assign( victimAndTeam.first );
		removeColorTokens( &entry.victimName );

		entry.victimTeamColor = ( victimAndTeam.second == TEAM_ALPHA ) ?
			CG_TeamAlphaDisplayedColor() : CG_TeamBetaDisplayedColor();
		if( attackerAndTeam ) {
			entry.attackerName.assign( attackerAndTeam->first );
			removeColorTokens( &entry.attackerName );
			// Avoid a redundant team color lookup in this case
			if( attackerAndTeam->second == victimAndTeam.second ) {
				entry.attackerTeamColor = entry.victimTeamColor;
			} else {
				entry.attackerTeamColor = ( attackerAndTeam->second == TEAM_ALPHA ) ?
					CG_TeamAlphaDisplayedColor() : CG_TeamBetaDisplayedColor();
			}
		}
	} else {
		entry.victimName.assign( victimAndTeam.first );
		if( attackerAndTeam ) {
			entry.attackerName.assign( attackerAndTeam->first );
		}
	}

	endInsertRows();
}

void FragsFeedModel::reset() {
	beginResetModel();
	m_entries.clear();
	endResetModel();
}

template <typename Entries>
[[nodiscard]]
static auto getNumTimedOutEntries( const Entries &entries, int64_t currTime, unsigned timeout ) -> unsigned {
	unsigned i = 0;
	for(; i < entries.size(); ++i ) {
		if( entries[i].timestamp + timeout > currTime ) {
			break;
		}
	}
	return i;
}

void FragsFeedModel::update( int64_t currTime ) {
	if( const unsigned numTimedOutEntries = getNumTimedOutEntries( m_entries, currTime, 5000u ) ) {
		beginRemoveRows( QModelIndex(), 0, (int)numTimedOutEntries - 1 );
		m_entries.erase( m_entries.begin(), m_entries.begin() + numTimedOutEntries );
		endRemoveRows();
	}
}

auto MessageFeedModel::roleNames() const -> QHash<int, QByteArray> {
	return { { Message, "message" } };
}

auto MessageFeedModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto MessageFeedModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() && role == Message ) {
		if( const auto row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			return toStyledText( m_entries[row].message.asView() );
		}
	}
	return QVariant();
}

void MessageFeedModel::addMessage( const wsw::StringView &message, int64_t timestamp ) {
	// We add to an intermediate circular buffer so the primary model is not flooded by additions/removals
	m_pendingEntries[m_pendingEntriesTail].timestamp = timestamp;
	m_pendingEntries[m_pendingEntriesTail].message.assign( message );
	m_pendingEntriesTail = ( m_pendingEntriesTail + 1 ) % kMaxEntries;
	if( m_numPendingEntries == kMaxEntries ) {
		m_pendingEntriesHead++;
	} else {
		m_numPendingEntries++;
	}
}

void MessageFeedModel::update( int64_t currTime ) {
	if( const unsigned numTimedOutEntries = getNumTimedOutEntries( m_entries, currTime, 7500u ) ) {
		beginRemoveRows( QModelIndex(), 0, (int)numTimedOutEntries - 1 );
		m_entries.erase( m_entries.begin(), m_entries.begin() + numTimedOutEntries );
		endRemoveRows();
	}

	if( m_numPendingEntries ) {
		const auto totalNumEntries = m_numPendingEntries + m_entries.size();
		if( totalNumEntries > kMaxEntries ) {
			const auto numEntriesToRemove = totalNumEntries - kMaxEntries;
			assert( numEntriesToRemove > 0 && numEntriesToRemove <= kMaxEntries );
			beginRemoveRows( QModelIndex(), 0, (int)( numEntriesToRemove - 1 ) );
			m_entries.erase( m_entries.begin(), m_entries.begin() + numEntriesToRemove );
			endRemoveRows();
		}

		assert( m_entries.size() + m_numPendingEntries <= kMaxEntries );
		beginInsertRows( QModelIndex(), (int)m_entries.size(), (int)m_entries.size() + (int)m_numPendingEntries - 1 );
		unsigned cursor = m_pendingEntriesHead;
		for( unsigned i = 0; i < m_numPendingEntries; ++i ) {
			assert( !m_entries.full() );
			m_entries.push_back( m_pendingEntries[cursor] );
			cursor = ( cursor + 1 ) % kMaxEntries;
		}
		endInsertRows();

		m_numPendingEntries = m_pendingEntriesHead = m_pendingEntriesTail = 0;
	}

	if( m_entries.empty() ) {
		m_isFadingOut = false;
	} else {
		m_isFadingOut = true;
		for( const Entry &entry: m_entries ) {
			if( entry.timestamp + 5000 > currTime ) {
				m_isFadingOut = false;
				break;
			}
		}
	}
}

void MessageFeedModel::reset() {
	beginResetModel();
	m_entries.clear();
	m_numPendingEntries = m_pendingEntriesHead = m_pendingEntriesTail = 0;
	m_isFadingOut = false;
	endResetModel();
}

auto AwardsModel::roleNames() const -> QHash<int, QByteArray> {
	return { { Message, "message" } };
}

auto AwardsModel::rowCount( const QModelIndex & ) const -> int {
	return (int)m_entries.size();
}

auto AwardsModel::data( const QModelIndex &modelIndex, int role ) const -> QVariant {
	if( modelIndex.isValid() ) {
		if( const int row = modelIndex.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			return toStyledText( m_entries[row].message.asView() );
		}
	}
	return QVariant();
}

void AwardsModel::addAward( const wsw::StringView &award, int64_t timestamp ) {
	m_pendingEntries.emplace_back( Entry { timestamp, award } );
}

void AwardsModel::update( int64_t currTime ) {
	if( const auto numTimedOutEntries = getNumTimedOutEntries( m_entries, currTime, 3000 ) ) {
		beginRemoveRows( QModelIndex(), 0, (int)numTimedOutEntries - 1 );
		m_entries.erase( m_entries.begin(), m_entries.begin() + numTimedOutEntries );
		endRemoveRows();
	}

	if( !m_pendingEntries.empty() ) {
		if( m_entries.full() ) {
			beginRemoveRows( QModelIndex(), 0, 0 );
			m_entries.erase( m_entries.begin() );
			endRemoveRows();
		}

		beginInsertRows( QModelIndex(), (int) m_entries.size(), (int) m_entries.size() );
		// TODO: Move from the font element
		auto *const entry = new( m_entries.unsafe_grow_back() )Entry( m_pendingEntries.front() );
		// Compensate the queue delay
		entry->timestamp += ( currTime - m_pendingEntries.front().timestamp );
		// This is fine for this small data set (todo: use a deque?)
		m_pendingEntries.erase( m_pendingEntries.begin() );
		endInsertRows();
	}
}

void AwardsModel::reset() {
	beginResetModel();
	m_entries.clear();
	m_pendingEntries.clear();
	endResetModel();
}

auto HudCommonDataModel::getWeaponFullName( int weapon ) const -> QByteArray {
	return weaponPropsCache.getWeaponFullName( weapon );
}

auto HudCommonDataModel::getWeaponShortName( int weapon ) const -> QByteArray {
	return weaponPropsCache.getWeaponShortName( weapon );
}

auto HudCommonDataModel::getWeaponIconPath( int weapon ) const -> QByteArray {
	return weaponPropsCache.getWeaponIconPath( weapon );
}

auto HudCommonDataModel::getWeaponModelPath( int weapon ) const -> QByteArray {
	return weaponPropsCache.getWeaponModelPath( weapon );
}

auto HudCommonDataModel::getWeaponColor( int weapon ) const -> QColor {
	return weaponPropsCache.getWeaponColor( weapon );
}

auto HudCommonDataModel::getAvailableRegularCrosshairs() const -> QStringList {
	return weaponPropsCache.getAvailableRegularCrosshairs();
}

auto HudCommonDataModel::getAvailableStrongCrosshairs() const -> QStringList {
	return weaponPropsCache.getAvailableStrongCrosshairs();
}

auto HudCommonDataModel::getRegularCrosshairFilePath( const QByteArray &fileName ) const -> QByteArray {
	return getCrosshairFilePath( kRegularCrosshairsDirName, fileName );
}

auto HudCommonDataModel::getStrongCrosshairFilePath( const QByteArray &fileName ) const -> QByteArray {
	return getCrosshairFilePath( kStrongCrosshairsDirName, fileName );
}

auto HudCommonDataModel::getCrosshairFilePath( const wsw::StringView &prefix, const QByteArray &fileName ) -> QByteArray {
	QByteArray result;
	if( !fileName.isEmpty() ) {
		const wsw::StringView fileNameView( fileName.data(), (size_t)fileName.size() );
		makeCrosshairFilePath<QByteArray, QLatin1String>( &result, prefix, fileNameView );
	}
	return result;
}

static const QByteArray kIconPathPrefix( "image://wsw/" );

auto HudCommonDataModel::getIndicatorIconPath( int iconNum ) const -> QByteArray {
	if( iconNum ) {
		if( const auto maybePath = CG_HudIndicatorIconPath( iconNum ) ) {
			return kIconPathPrefix + QByteArray::fromRawData( maybePath->data(), (int)maybePath->size() );
		}
	}
	return QByteArray();
}

auto HudCommonDataModel::getIndicatorStatusString( int stringNum ) const -> QByteArray {
	if( stringNum ) {
		if( const auto maybePath = CG_HudIndicatorStatusString( stringNum ) ) {
			return QByteArray::fromRawData( maybePath->data(), (int)maybePath->size() );
		}
	}
	return QByteArray();
}

auto HudCommonDataModel::getFrametimeDataRowModel() -> QObject * {
	if( !m_hasSetFrametimeModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_frametimeDataRowModel, QQmlEngine::CppOwnership );
	}
	return &m_frametimeDataRowModel;
}

auto HudCommonDataModel::getPingDataRowModel() -> QObject * {
	if( !m_hasSetPingModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_pingDataRowModel, QQmlEngine::CppOwnership );
	}
	return &m_pingDataRowModel;
}

auto HudCommonDataModel::getPacketlossDataRowModel() -> QObject * {
	if( !m_hasSetPacketlossModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_packetlossDataRowModel, QQmlEngine::CppOwnership );
	}
	return &m_packetlossDataRowModel;
}

// TODO: Don't even box, use QByteArray wrapping facilities?
static const QByteArray kWarmup( "WARMUP" );
static const QByteArray kCountdown( "COUNTDOWN" );
static const QByteArray kOvertime( "OVERTIME" );
static const QByteArray kNoMatchState;

void HudCommonDataModel::setFormattedTime( QByteArray *dest, int value ) {
	assert( value >= 0 );
	dest->clear();
	if( value >= 100 ) {
		dest->setNum( value );
	} else {
		dest->clear();
		const int q = value / 10;
		const int r = value - ( q * 10 );
		dest->append( (char)( '0' + q ) );
		dest->append( (char)( '0' + r ) );
	}
}

void HudCommonDataModel::setStyledName( QByteArray *dest, const wsw::StringView &name ) {
	// TODO: toStyledText() should allow accepting an external buffer of an arbitrary structurally compatible type
	*dest = toStyledText( name ).toLatin1();
}

HudCommonDataModel::HudCommonDataModel( int pixelsPerLogicalUnit )
	: m_pixelsPerLogicalUnit( pixelsPerLogicalUnit )
	, m_regularHudChangesTracker( &v_regularHud )
	, m_miniviewHudChangesTracker( &v_miniviewHud ) {
	static_assert( (int)wsw::ui::HudCommonDataModel::NoAnim == (int)HUD_INDICATOR_NO_ANIM );
	static_assert( (int)wsw::ui::HudCommonDataModel::AlertAnim == (int)HUD_INDICATOR_ALERT_ANIM );
	static_assert( (int)wsw::ui::HudCommonDataModel::ActionAnim == (int)HUD_INDICATOR_ACTION_ANIM );

	assert( !m_matchTimeMinutes && !m_matchTimeSeconds );
	setFormattedTime( &m_formattedMinutes, m_matchTimeMinutes );
	setFormattedTime( &m_formattedSeconds, m_matchTimeSeconds );
}

auto HudCommonDataModel::getRegularLayoutModel() -> QObject * {
	if( !m_hasSetRegularLayoutModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_regularLayoutModel, QQmlEngine::CppOwnership );
		m_hasSetRegularLayoutModelOwnership = true;
	}
	return &m_regularLayoutModel;
}

auto HudCommonDataModel::getMiniviewLayoutModel() -> QObject * {
	if( !m_hasSetMinivewLayoutModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_miniviewLayoutModel, QQmlEngine::CppOwnership );
		m_hasSetMinivewLayoutModelOwnership = true;
	}
	return &m_miniviewLayoutModel;
}

auto HudCommonDataModel::getFragsFeedModel() -> QObject * {
	if( !m_hasSetFragsFeedModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_fragsFeedModel, QQmlEngine::CppOwnership );
		m_hasSetFragsFeedModelOwnership = true;
	}
	return &m_fragsFeedModel;
}

auto HudCommonDataModel::getMiniviewModelForIndex( int indexOfModel ) -> QObject * {
	assert( (size_t)indexOfModel < std::size( m_miniviewDataModels ) );
	QQmlEngine::setObjectOwnership( &m_miniviewDataModels[indexOfModel], QQmlEngine::CppOwnership );
	return &m_miniviewDataModels[indexOfModel];
}

auto HudCommonDataModel::getMiniviewPlayerNumForIndex( int indexOfModel ) -> QVariant {
	assert( (size_t)indexOfModel < std::size( m_miniviewDataModels ) );
	return m_miniviewDataModels[indexOfModel].getPlayerNum().value();
}

auto HudCommonDataModel::getFixedMiniviewPositionForIndex( int indexOfModel ) const -> QVariant {
	for( const FixedPositionMinivewEntry &entry: m_fixedPositionMinviews ) {
		if( entry.indexOfModel == indexOfModel ) {
			qreal scale = 1.0f;
			if( m_pixelsPerLogicalUnit != 1 ) {
				scale /= m_pixelsPerLogicalUnit;
			}
			return QRectF {
				scale * (qreal)entry.position.x,
				scale * (qreal)entry.position.y,
				scale * (qreal)entry.position.width,
				scale * (qreal)entry.position.height,
			};
		}
	}
	wsw::failWithRuntimeError( "Illegal index of model" );
}

auto HudCommonDataModel::getFixedPositionMiniviewIndices() const -> QVariant {
	QVariantList result;
	for( const FixedPositionMinivewEntry &entry: m_fixedPositionMinviews ) {
		result.append( entry.indexOfModel );
	}
	return result;
}

auto HudCommonDataModel::getMiniviewIndicesForPane( int paneNum ) const -> QVariant {
	assert( paneNum == 1 || paneNum == 2 );
	QVariantList result;
	for( const HudControlledMiniviewEntry &entry: m_hudControlledMinviewsForPane[paneNum - 1] ) {
		result.append( entry.indexOfModel );
	}
	return result;
}

auto HudCommonDataModel::getAllowedNumRowsForMiniviewPane( int paneNum ) const -> int {
	assert( paneNum == 1 || paneNum == 2 );
	return v_allowedNumRowsForMiniviewPane[paneNum - 1].get();
}

auto HudCommonDataModel::getAllowedNumColumnsForMiniviewPane( int paneNum ) const -> int {
	assert( paneNum == 1 || paneNum == 2 );
	return v_allowedNumColumnsForMiniviewPane[paneNum - 1].get();
}

auto HudCommonDataModel::getPreferredNumRowsForMiniviewPane( int paneNum ) const -> int {
	assert( paneNum == 1 || paneNum == 2 );
	return v_preferredNumRowsForMiniviewPane[paneNum - 1].get();
}

auto HudCommonDataModel::getPreferredNumColumnsForMiniviewPane( int paneNum ) const -> int {
	assert( paneNum == 1 || paneNum == 2 );
	return v_preferredNumColumnsForMiniviewPane[paneNum - 1].get();
}

auto HudCommonDataModel::getViewStateIndexForMiniviewModelIndex( int miniviewModelIndex ) const -> unsigned {
	assert( (size_t)miniviewModelIndex < std::size( m_miniviewDataModels ) );
	return m_miniviewDataModels[miniviewModelIndex].getViewStateIndex();
}

void HudCommonDataModel::addFragEvent( const std::pair<wsw::StringView, int> &victimAndTeam,
								 int64_t timestamp, unsigned int meansOfDeath,
								 const std::optional<std::pair<wsw::StringView, int>> &attackerAndTeam ) {
	m_fragsFeedModel.addFrag( victimAndTeam, timestamp, meansOfDeath, attackerAndTeam );
}

void HudCommonDataModel::resetHudFeed() {
	m_fragsFeedModel.reset();
	for( HudPovDataModel &povModel: m_miniviewDataModels ) {
		povModel.resetHudFeed();
	}
}

auto HudCommonDataModel::findPovModelByPlayerNum( unsigned playerNum ) -> HudPovDataModel * {
	for( HudPovDataModel &model: m_miniviewDataModels ) {
		if( model.getPlayerNum() == std::optional( playerNum ) ) {
			return std::addressof( model );
		}
	}
	return nullptr;
}

void HudCommonDataModel::addToMessageFeed( unsigned playerNum, const wsw::StringView &message, int64_t timestamp ) {
	if( HudPovDataModel *povModel = findPovModelByPlayerNum( playerNum ) ) {
		povModel->addToMessageFeed( message, timestamp );
	}
}

void HudCommonDataModel::addAward( unsigned playerNum, const wsw::StringView &award, int64_t timestamp ) {
	if( HudPovDataModel *povModel = findPovModelByPlayerNum( playerNum ) ) {
		povModel->addAward( award, timestamp );
	}
}

void HudCommonDataModel::addStatusMessage( unsigned playerNum, const wsw::StringView &message, int64_t timestamp ) {
	if( HudPovDataModel *povModel = findPovModelByPlayerNum( playerNum ) ) {
		povModel->addStatusMessage( message, timestamp );
	}
}

PerfDataRowModel::PerfDataRowModel() {
	m_samples.fill( 0.0, 12 );
	m_prevSamples.fill( 0.0, 12 );
}

void PerfDataRowModel::update( int64_t timestamp, float valueToAdd ) {
	m_samples.removeLast();
	m_samples.prepend( (qreal)valueToAdd );

	constexpr int64_t peakTimeout = 333;

	const bool minTimedOut = m_peakMinTimestamp + peakTimeout <= timestamp;
	const bool maxTimedOut = m_peakMaxTimestamp + peakTimeout <= timestamp;

	bool recalculate                = false;
	bool hasTestedSamplesMatch      = false;
	bool hasFoundMismatchingSamples = false;
	if( minTimedOut || maxTimedOut || m_displayedPeakMin > valueToAdd || m_displayedPeakMax < valueToAdd ) {
		recalculate = true;
	} else {
		hasTestedSamplesMatch = true;
		if( m_samples != m_prevSamples ) {
			hasFoundMismatchingSamples = true;
			recalculate                = true;
		}
	}

	if( recalculate ) {
		qreal average  = 0.0;
		qreal minValue = std::numeric_limits<qreal>::max();
		qreal maxValue = std::numeric_limits<qreal>::lowest();
		for( const qreal value: m_samples ) {
			minValue = wsw::min( minValue, value );
			maxValue = wsw::max( maxValue, value );
			average += value;
		}

		const auto oldActualMax        = m_actualMax, oldActualMin = m_actualMin;
		const auto oldDisplayedPeakMax = m_displayedPeakMax, oldDisplayedPeakMin = m_displayedPeakMin;
		const auto oldAverage          = m_average;

		m_actualMax = maxValue;
		m_actualMin = minValue;

		m_average = average / (qreal)m_samples.size();
		if( m_average != oldAverage ) {
			Q_EMIT averageChanged( m_average );
		}

		if( minTimedOut || m_displayedPeakMin > minValue ) {
			m_displayedPeakMin = minValue;
			m_peakMinTimestamp = timestamp;
			if( m_displayedPeakMin != oldDisplayedPeakMin ) {
				Q_EMIT displayedPeakMinChanged( m_displayedPeakMin );
			}
		}
		if( maxTimedOut || m_displayedPeakMax < maxValue ) {
			m_displayedPeakMax = maxValue;
			m_peakMaxTimestamp = timestamp;
			if( m_displayedPeakMax != oldDisplayedPeakMax ) {
				Q_EMIT displayedPeakMaxChanged( m_displayedPeakMax );
			}
		}

		// Check whether there are actual updates.
		// Otherwise, peak timeouts lead to redundant Qml updates even if nothing actually changes.
		if( m_actualMax != oldActualMax || m_actualMin != oldActualMin || hasFoundMismatchingSamples ||
			( !hasTestedSamplesMatch && m_samples != m_prevSamples ) ) {
			// Save samples
			m_prevSamples.clear();
			m_prevSamples.append( m_samples );
			Q_EMIT sampleDataChanged();
		}
	}
}

void HudCommonDataModel::addToFrametimeTimeline( int64_t timestamp, float frametime ) {
	m_frametimeDataRowModel.update( timestamp, frametime );
}

void HudCommonDataModel::addToPingTimeline( int64_t timestamp, float ping ) {
	m_pingDataRowModel.update( timestamp, ping );
}

void HudCommonDataModel::addToPacketlossTimeline( int64_t timestamp, bool hadPacketloss ) {
	m_packetlossDataRowModel.update( timestamp, hadPacketloss ? 1.0f : 0.0f );
}

// TODO: Should it be shared for regular/miniview models?
static const wsw::StringView kDefaultHudName( "default"_asView );

void HudCommonDataModel::handleVarChanges( StringConfigVar *var, InGameHudLayoutModel *model, HudNameString *currName ) {
	wsw::StringView name( var->get() );
	// Protect from redundant load() calls
	if( !name.equalsIgnoreCase( currName->asView() ) ) {
		if( name.length() > HudLayoutModel::kMaxHudNameLength ) {
			var->setImmediately( kDefaultHudName );
			name = kDefaultHudName;
		}
		if( !model->load( name ) ) {
			if( !name.equalsIgnoreCase( kDefaultHudName ) ) {
				var->setImmediately( kDefaultHudName );
				name = kDefaultHudName;
				// This could fail as well but we assume that the data of the default HUD is not corrupt.
				// A HUD won't be displayed in case of a failure.
				(void)model->load( kDefaultHudName );
			}
		}
		currName->assign( name );
	}
}

void HudCommonDataModel::onHudUpdated( const QByteArray &name, HudLayoutModel::Flavor flavor ) {
	StringConfigVar *var;
	HudLayoutModel *model;
	if( flavor != HudLayoutModel::Miniview ) {
		var   = &v_regularHud;
		model = &m_regularLayoutModel;
	} else {
		var   = &v_miniviewHud;
		model = &m_miniviewLayoutModel;
	}

	const wsw::StringView nameView( name.data(), (size_t)name.size() );
	if( var->get().equalsIgnoreCase( nameView ) ) {
		// Try (re-)loading the respective model
		if( !model->load( nameView ) ) {
			// In case of failure, try loading the default HUD
			if( !nameView.equalsIgnoreCase( kDefaultHudName ) ) {
				var->setImmediately( kDefaultHudName );
				(void)model->load( kDefaultHudName );
			}
		}
	}
}

void HudCommonDataModel::checkPropertyChanges( int64_t currTime ) {
	if( m_regularHudChangesTracker.checkAndReset() ) {
		handleVarChanges( &v_regularHud, &m_regularLayoutModel, &m_regularHudName );
	}
	if( m_miniviewHudChangesTracker.checkAndReset() ) {
		handleVarChanges( &v_miniviewHud, &m_miniviewLayoutModel, &m_miniviewHudName );
	}

	if( const bool hadTwoTeams = m_hasTwoTeams; hadTwoTeams != ( m_hasTwoTeams = CG_HasTwoTeams() ) ) {
		Q_EMIT hasTwoTeamsChanged( m_hasTwoTeams );
	}

	Team realClientTeam = TeamSpectators;
	if( const int rawRealClientTeam = CG_RealClientTeam(); rawRealClientTeam != TEAM_SPECTATOR ) {
		if( rawRealClientTeam == TEAM_ALPHA ) {
			realClientTeam = TeamAlpha;
		} else if( rawRealClientTeam == TEAM_BETA ) {
			realClientTeam = TeamBeta;
		} else {
			realClientTeam = TeamPlayers;
		}
	}
	if( m_realClientTeam != realClientTeam ) {
		m_realClientTeam = realClientTeam;
		Q_EMIT realClientTeamChanged( realClientTeam );
	}

	// The best we can get in the current codebase state...
	const auto isCVarSet = []( const char *name ) -> bool {
		const float value = Cvar_Value( name ); return Q_rint( value ) != 0;
	};

	int newActiveItemsMask = 0;
	if( isCVarSet( "cg_showTeamInfo" ) ) {
		newActiveItemsMask |= (int)HudLayoutModel::ShowTeamInfo;
	}
	if( isCVarSet( "cg_showFragsFeed" ) ) {
		newActiveItemsMask |= (int)HudLayoutModel::ShowFragsFeed;
	}
	if( isCVarSet( "cg_showMessageFeed" ) ) {
		newActiveItemsMask |= (int)HudLayoutModel::ShowMessageFeed;
	}
	if( isCVarSet( "cg_showAwards" ) ) {
		newActiveItemsMask |= (int)HudLayoutModel::ShowAwards;
	}
	if( isCVarSet( "cg_showFPS" ) ) {
		newActiveItemsMask |= (int)HudLayoutModel::ShowFps;
	}
	if( isCVarSet( "cg_showNet" ) ) {
		newActiveItemsMask |= (int)HudLayoutModel::ShowNet;
	}
	if( m_activeItemsMask != newActiveItemsMask ) {
		m_activeItemsMask = newActiveItemsMask;
		Q_EMIT activeItemsMaskChanged( newActiveItemsMask );
	}

	if( m_pendingAlphaScore != m_alphaScore ) {
		m_alphaScore = m_pendingAlphaScore;
		Q_EMIT alphaScoreChanged( m_alphaScore );
	}
	if( m_pendingBetaScore != m_betaScore ) {
		m_betaScore = m_pendingBetaScore;
		Q_EMIT betaScoreChanged( m_betaScore );
	}

	const wsw::StringView alphaName( ::cl.configStrings.getTeamAlphaName().value_or( wsw::StringView() ) );
	if( !m_alphaName.equals( alphaName ) ) {
		m_alphaName.assign( alphaName );
		setStyledName( &m_styledAlphaName, alphaName );
		Q_EMIT alphaNameChanged( m_styledAlphaName );
	}

	const wsw::StringView betaName( ::cl.configStrings.getTeamBetaName().value_or( wsw::StringView() ) );
	if( !m_betaName.equals( betaName ) ) {
		m_betaName.assign( betaName );
		setStyledName( &m_styledBetaName, betaName );
		Q_EMIT betaNameChanged( m_styledBetaName );
	}

	// Check separately displayed clan name changes.
	// This also naturally accounts for switching to non-individual gametypes.
	const auto *const nameChangesTracker = NameChangesTracker::instance();
	if( m_pendingIndividualAlphaPlayerNum ) {
		const unsigned counter = nameChangesTracker->getLastClanUpdateCounter( *m_pendingIndividualAlphaPlayerNum );
		if( counter != m_lastIndividualAlphaClanCounter ) {
			setStyledName( &m_styledAlphaClan, CG_PlayerClan( *m_pendingIndividualAlphaPlayerNum ) );
			Q_EMIT alphaClanChanged( m_styledAlphaClan );
		}
	} else {
		if( !m_styledAlphaClan.isEmpty() ) {
			m_styledAlphaClan.clear();
			Q_EMIT alphaClanChanged( m_styledAlphaClan );
		}
	}
	if( m_pendingIndividualBetaPlayerNum ) {
		const unsigned counter = nameChangesTracker->getLastClanUpdateCounter( *m_pendingIndividualBetaPlayerNum );
		if( counter != m_lastIndividualBetaClanCounter ) {
			setStyledName( &m_styledBetaClan, CG_PlayerClan( *m_pendingIndividualBetaPlayerNum ) );
			Q_EMIT betaClanChanged( m_styledBetaClan );
		}
	} else {
		if( !m_styledBetaClan.isEmpty() ) {
			m_styledBetaClan = QByteArray();
			Q_EMIT betaClanChanged( m_styledBetaClan );
		}
	}

	if( m_numAliveAlphaPlayers != m_pendingNumAliveAlphaPlayers ) {
		m_numAliveAlphaPlayers = m_pendingNumAliveAlphaPlayers;
		m_alphaPlayersStatus = getStatusForNumberOfPlayers( m_numAliveAlphaPlayers );
		Q_EMIT alphaPlayersStatusChanged( m_alphaPlayersStatus );
	}

	if( m_numAliveBetaPlayers != m_pendingNumAliveBetaPlayers ) {
		m_numAliveBetaPlayers = m_pendingNumAliveBetaPlayers;
		m_betaPlayersStatus = getStatusForNumberOfPlayers( m_numAliveBetaPlayers );
		Q_EMIT betaPlayersStatusChanged( m_betaPlayersStatus );
	}

	if( const auto oldColor = m_rawAlphaColor; oldColor != ( m_rawAlphaColor = CG_TeamAlphaDisplayedColor() ) ) {
		m_alphaColor = toQColor( m_rawAlphaColor );
		Q_EMIT alphaColorChanged( m_alphaColor );
	}

	if( const auto oldColor = m_rawBetaColor; oldColor != ( m_rawBetaColor = CG_TeamBetaDisplayedColor() ) ) {
		m_betaColor = toQColor( m_rawBetaColor );
		Q_EMIT betaColorChanged( m_betaColor );
	}

	for( int i = 0; i < 3; ++i ) {
		const auto oldIndicatorState( m_indicatorStates[i] );
		m_indicatorStates[i] = wsw::ui::ObjectiveIndicatorState( CG_HudIndicatorState( i ) );
		if( oldIndicatorState != m_indicatorStates[i] ) {
			if( i == 0 ) {
				Q_EMIT indicator1StateChanged( getIndicator1State() );
			} else if( i == 1 ) {
				Q_EMIT indicator2StateChanged( getIndicator2State() );
			} else if( i == 2 ) {
				Q_EMIT indicator3StateChanged( getIndicator3State() );
			}
		}
	}

	const auto [minutes, seconds] = CG_GetMatchClockTime();
	if( minutes != m_matchTimeMinutes ) {
		assert( minutes >= 0 );
		m_matchTimeMinutes = minutes;
		setFormattedTime( &m_formattedMinutes, minutes );
		Q_EMIT matchTimeMinutesChanged( m_formattedMinutes );
	}
	if( seconds != m_matchTimeSeconds ) {
		assert( (unsigned)seconds < 60u );
		m_matchTimeSeconds = seconds;
		setFormattedTime( &m_formattedSeconds, seconds );
		Q_EMIT matchTimeSecondsChanged( m_formattedSeconds );
	}

	const QByteArray *newMatchStateString = &kNoMatchState;
	const int rawMatchState = GS_MatchState( *cggs );
	if( rawMatchState == MATCH_STATE_WARMUP ) {
		newMatchStateString = &kWarmup;
	} else if( rawMatchState == MATCH_STATE_COUNTDOWN ) {
		newMatchStateString = &kCountdown;
	} else if( GS_MatchExtended( *cggs ) ) {
		newMatchStateString = &kOvertime;
	}
	if( m_matchStateString.compare( *newMatchStateString ) != 0 ) {
		m_matchStateString = *newMatchStateString;
		Q_EMIT matchStateStringChanged( m_matchStateString );
	}

	const bool wasInWarmupState = m_isInWarmupState;
	if( wasInWarmupState != ( m_isInWarmupState = rawMatchState <= MATCH_STATE_WARMUP ) ) {
		Q_EMIT isInWarmupStateChanged( m_isInWarmupState );
	}

	const bool wasInPostmatchState = m_isInPostmatchState;
	if( wasInPostmatchState != ( m_isInPostmatchState = rawMatchState > MATCH_STATE_PLAYTIME ) ) {
		Q_EMIT isInPostmatchStateChanged( m_isInPostmatchState );
	}

	m_fragsFeedModel.update( currTime );

	updateMiniviewData( currTime );
}

void HudCommonDataModel::updateMiniviewData( int64_t currTime ) {
	std::span<const uint8_t> pane1ViewStateIndices, pane2ViewStateIndices, tileViewStateIndices;
	std::span<const Rect> tilePositions;

	CG_GetMultiviewConfiguration( &pane1ViewStateIndices, &pane2ViewStateIndices, &tileViewStateIndices, &tilePositions );
	assert( pane1ViewStateIndices.size() + pane2ViewStateIndices.size() + tileViewStateIndices.size() <= kMaxMiniviews );
	assert( tileViewStateIndices.size() == tilePositions.size() );

	bool layoutChanged = m_hudControlledMinviewsForPane[0].size() != pane1ViewStateIndices.size() ||
						 m_hudControlledMinviewsForPane[1].size() != pane2ViewStateIndices.size();
	if( !layoutChanged ) {
		if( m_fixedPositionMinviews.size() != tileViewStateIndices.size() ) {
			layoutChanged = true;
		} else {
			for( unsigned i = 0; i < m_fixedPositionMinviews.size(); ++i ) {
				const FixedPositionMinivewEntry &entry = m_fixedPositionMinviews[i];
				// Note: indexOfModel has 1-1 correspondence to view state index in this case
				if( entry.viewStateIndex != tileViewStateIndices[i] ) {
					layoutChanged = true;
					break;
				}
			}
		}
	}

	// Even if the layout stays unchanged, we have to update view indices of models
	// and respective entires for procedural/explicit retrieval of data.

	// Clear view state indices of all models to detect use of wrong models
	for( HudPovDataModel &model: m_miniviewDataModels ) {
		model.clearViewStateIndex();
		assert( !model.hasValidViewStateIndex() );
	}

	int numUsedModelsSoFar = 0;
	m_fixedPositionMinviews.clear();
	for( unsigned viewIndex = 0; viewIndex < tileViewStateIndices.size(); ++viewIndex ) {
		const unsigned viewStateIndex = tileViewStateIndices[viewIndex];
		m_fixedPositionMinviews.emplace_back( FixedPositionMinivewEntry {
			.indexOfModel   = numUsedModelsSoFar,
			.viewStateIndex = viewStateIndex,
			.position       = tilePositions[viewIndex],
		});
		m_miniviewDataModels[numUsedModelsSoFar].setViewStateIndex( viewStateIndex );
		m_miniviewDataModels[numUsedModelsSoFar].setPlayerNum( CG_GetPlayerNumForViewState( viewStateIndex ).value() );
		numUsedModelsSoFar++;
	};

	for( int paneNum = 0; paneNum < 2; ++paneNum ) {
		wsw::StaticVector<HudControlledMiniviewEntry, kMaxMiniviews> &entries = m_hudControlledMinviewsForPane[paneNum];
		entries.clear();
		for( const unsigned viewStateIndex: ( paneNum == 0 ? pane1ViewStateIndices : pane2ViewStateIndices ) ) {
			entries.emplace_back( HudControlledMiniviewEntry {
				.indexOfModel = numUsedModelsSoFar,
				.paneNumber   = paneNum,
			});
			m_miniviewDataModels[numUsedModelsSoFar].setViewStateIndex( viewStateIndex );
			m_miniviewDataModels[numUsedModelsSoFar].setPlayerNum( CG_GetPlayerNumForViewState( viewStateIndex ).value() );
			numUsedModelsSoFar++;
		}
	}

	assert( (size_t)numUsedModelsSoFar == pane1ViewStateIndices.size() + pane2ViewStateIndices.size() + tileViewStateIndices.size() );
	for( int modelNum = 0; modelNum < numUsedModelsSoFar; ++modelNum ) {
		assert( m_miniviewDataModels[modelNum].hasValidViewStateIndex() );
		m_miniviewDataModels[modelNum].checkPropertyChanges( currTime );
	}
	for( int modelNum = numUsedModelsSoFar; modelNum < (int)std::size( m_miniviewDataModels ); ++modelNum ) {
		assert( !m_miniviewDataModels[modelNum].hasValidViewStateIndex() );
		m_miniviewDataModels[modelNum].clearPlayerNum();
	}

	const bool hadTiledMiniviews = m_hasTiledMiniviews;
	m_hasTiledMiniviews = !m_fixedPositionMinviews.empty();
	if( m_hasTiledMiniviews != hadTiledMiniviews ) {
		Q_EMIT hasTiledMiniviewsChanged( m_hasTiledMiniviews );
	}

	if( layoutChanged ) {
		Q_EMIT miniviewLayoutChangedPass1();
		Q_EMIT miniviewLayoutChangedPass2();
	}

	int highlightedMiniviewIndex         = -1;
	const unsigned primaryViewStateIndex = CG_GetPrimaryViewStateIndex();
	for( int index = 0; index < (int)std::size( m_miniviewDataModels ); ++index ) {
		if( !m_miniviewDataModels[index].hasValidViewStateIndex() ) {
			break;
		}
		if( m_miniviewDataModels[index].getViewStateIndex() == primaryViewStateIndex ) {
			highlightedMiniviewIndex = index;
			break;
		}
	}
	if( m_highlightedMiniviewIndex != highlightedMiniviewIndex ) {
		m_highlightedMiniviewIndex = highlightedMiniviewIndex;
		Q_EMIT highlightedMiniviewIndexChanged( highlightedMiniviewIndex );
	}
}

void HudCommonDataModel::updateScoreboardData( const ReplicatedScoreboardData &scoreboardData ) {
	m_pendingAlphaScore = scoreboardData.alphaScore;
	m_pendingBetaScore = scoreboardData.betaScore;
	if( CG_HasTwoTeams() ) {
		updateTeamPlayerStatuses( scoreboardData );
	}
}

auto HudPovDataModel::getActiveWeaponIcon() const -> QByteArray {
	return m_activeWeapon ? weaponPropsCache.getWeaponIconPath( m_activeWeapon ) : QByteArray();
}

auto HudPovDataModel::getActiveWeaponName() const -> QByteArray {
	return m_activeWeapon ? weaponPropsCache.getWeaponFullName( m_activeWeapon ) : QByteArray();
}

auto HudPovDataModel::getActiveWeaponColor() const -> QColor {
	return m_activeWeapon ? weaponPropsCache.getWeaponColor( m_activeWeapon ) : QColor( Qt::black );
}

auto HudPovDataModel::getInventoryModel() -> QObject * {
	if( !m_hasSetInventoryModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_inventoryModel, QQmlEngine::CppOwnership );
		m_hasSetInventoryModelOwnership = true;
	}
	return &m_inventoryModel;
}

auto HudPovDataModel::getTeamListModel() -> QObject * {
	if( !m_hasSetTeamListModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_teamListModel, QQmlEngine::CppOwnership );
		m_hasSetTeamListModelOwnership = true;
	}
	return &m_teamListModel;
}

auto HudPovDataModel::getMessageFeedModel() -> QObject * {
	if( !m_hasSetMessageFeedModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_messageFeedModel, QQmlEngine::CppOwnership );
		m_hasSetMessageFeedModelOwnership = true;
	}
	return &m_messageFeedModel;
}

auto HudPovDataModel::getAwardsModel() -> QObject * {
	if( !m_hasSetAwardsModelOwnership ) {
		QQmlEngine::setObjectOwnership( &m_awardsModel, QQmlEngine::CppOwnership );
		m_hasSetAwardsModelOwnership = true;
	}
	return &m_awardsModel;
}

void HudPovDataModel::addStatusMessage( const wsw::StringView &message, int64_t timestamp ) {
	const wsw::StringView truncatedMessage( message.take( m_originalStatusMessage.capacity() ) );
	// Almost always perform updates, except the single case: a rapid stream of the same textual message.
	// In this case, let initial transitions to complete, otherwise the message won't be noticeable at all.
	if( m_originalStatusMessage.equals( truncatedMessage ) ) {
		if( m_lastStatusMessageTimestamp + 500 > timestamp ) {
			return;
		}
	}

	m_lastStatusMessageTimestamp = timestamp;
	m_originalStatusMessage.assign( truncatedMessage );
	m_formattedStatusMessage = toStyledText( truncatedMessage );
	Q_EMIT statusMessageChanged( m_formattedStatusMessage );
}

void HudPovDataModel::resetHudFeed() {
	m_lastStatusMessageTimestamp = 0;
	m_originalStatusMessage.clear();
	m_formattedStatusMessage.clear();
	Q_EMIT statusMessageChanged( m_formattedStatusMessage );

	m_messageFeedModel.reset();
	m_awardsModel.reset();

	// TODO: Can we still consider resetHudName() to be valid name if we do this
	m_nicknameUpdateCounter = 0;
}

void HudPovDataModel::checkPropertyChanges( int64_t currTime ) {
	const bool hadActivePov = m_hasActivePov, wasUsingChasePov = m_isUsingChasePov, wasPovAlive = m_isPovAlive;
	m_hasActivePov    = CG_ActivePovOfViewState( m_viewStateIndex ) != std::nullopt;
	m_isUsingChasePov = m_hasActivePov && CG_IsUsingChasePov( m_viewStateIndex );
	m_isPovAlive      = m_hasActivePov && CG_IsPovAlive( m_viewStateIndex );
	if( wasPovAlive != m_isPovAlive ) {
		Q_EMIT isPovAliveChanged( m_isPovAlive );
	}
	if( wasUsingChasePov != m_isUsingChasePov ) {
		Q_EMIT isUsingChasePovChanged( m_isUsingChasePov );
	}
	if( hadActivePov != m_hasActivePov ) {
		Q_EMIT hasActivePovChanged( m_hasActivePov );
	}

	const auto oldActiveWeapon = m_activeWeapon;
	if( oldActiveWeapon != ( m_activeWeapon = CG_ActiveWeapon( m_viewStateIndex ) ) ) {
		Q_EMIT activeWeaponIconChanged( getActiveWeaponIcon() );
		Q_EMIT activeWeaponNameChanged( getActiveWeaponName() );
		Q_EMIT activeWeaponColorChanged( getActiveWeaponColor() );
	}

	const auto oldActiveWeaponWeakAmmo = m_activeWeaponWeakAmmo;
	const auto oldActiveWeaponStrongAmmo = m_activeWeaponStrongAmmo;
	std::tie( m_activeWeaponWeakAmmo, m_activeWeaponStrongAmmo ) = CG_WeaponAmmo( m_viewStateIndex, m_activeWeapon );
	if( m_activeWeapon == WEAP_GUNBLADE ) {
		// TODO: Is it always available?
		m_activeWeaponWeakAmmo = -1;
		m_activeWeaponStrongAmmo = -m_activeWeaponStrongAmmo;
	}
	if( oldActiveWeaponWeakAmmo != m_activeWeaponWeakAmmo ) {
		Q_EMIT activeWeaponWeakAmmoChanged( m_activeWeaponWeakAmmo );
	}
	if( oldActiveWeaponStrongAmmo != m_activeWeaponStrongAmmo ) {
		Q_EMIT activeWeaponStrongAmmoChanged( m_activeWeaponStrongAmmo );
	}

	const auto oldHealth = m_health;
	m_health = CG_Health( m_viewStateIndex );
	if( oldHealth != m_health ) {
		Q_EMIT healthChanged( m_health );
	}

	const auto oldArmor = m_armor;
	m_armor = CG_Armor( m_viewStateIndex );
	if( oldArmor != m_armor ) {
		Q_EMIT armorChanged( m_armor );
	}

	m_inventoryModel.checkPropertyChanges( m_viewStateIndex );
	m_awardsModel.update( currTime );

	const bool wasMessageFeedFadingOut = m_messageFeedModel.isFadingOut();
	m_messageFeedModel.update( currTime );
	const bool isMessageFeedFadingOut = m_messageFeedModel.isFadingOut();
	if( wasMessageFeedFadingOut != isMessageFeedFadingOut ) {
		Q_EMIT isMessageFeedFadingOutChanged( isMessageFeedFadingOut );
	}

	if( m_playerNum < MAX_CLIENTS ) {
		const unsigned counter = NameChangesTracker::instance()->getLastNicknameUpdateCounter( m_playerNum );
		if( m_nicknameUpdateCounter != counter ) {
			m_nicknameUpdateCounter = counter;
			m_formattedNickname     = toStyledText( CG_PlayerName( m_playerNum ) );
			Q_EMIT nicknameChanged( m_formattedNickname );
		}
	} else {
		// Reset it for consistency, even if we should not use such models
		if( !m_formattedNickname.isEmpty() ) {
			m_formattedNickname.clear();
			Q_EMIT nicknameChanged( m_formattedNickname );
		}
	}
}

void HudPovDataModel::updateScoreboardData( const ReplicatedScoreboardData &scoreboardData ) {
	if( CG_HasTwoTeams() ) {
		if( const auto maybeActivePovNum = CG_ActivePovOfViewState( m_viewStateIndex ) ) {
			m_teamListModel.update( scoreboardData, *maybeActivePovNum );
		}
	}
}

void HudPovDataModel::setViewStateIndex( unsigned viewStateIndex ) {
	assert( viewStateIndex <= MAX_CLIENTS );
	m_viewStateIndex = viewStateIndex;
}

auto HudPovDataModel::getViewStateIndex() const -> unsigned {
	assert( m_viewStateIndex <= MAX_CLIENTS );
	return m_viewStateIndex;
}

bool HudPovDataModel::hasValidViewStateIndex() const {
	return m_viewStateIndex <= MAX_CLIENTS;
}

void HudPovDataModel::clearViewStateIndex() {
	m_viewStateIndex = ~0u;
}

void HudPovDataModel::setPlayerNum( unsigned playerNum ) {
	if( m_playerNum != playerNum ) {
		resetHudFeed();
		m_playerNum = playerNum;
	}
}

auto HudPovDataModel::getPlayerNum() const -> std::optional<unsigned> {
	return m_playerNum != ~0u ? std::optional( m_playerNum ) : std::nullopt;
}

void HudPovDataModel::clearPlayerNum() {
	if( m_playerNum != ~0u ) {
		resetHudFeed();
		m_playerNum = ~0u;
	}
}

static const QByteArray kStatusesForNumberOfPlayers[] {
	"",
	"\u2605",
	"\u2605 \u2605",
	"\u2605 \u2605 \u2605",
	"\u2605 \u2605 \u2605 \u2605",
	"\u2605 \u2605 \u2605 \u2605 \u2605",
	"\u2605 \u2605 \u2605 \u2605 \u2605 \u2605"
};

auto HudCommonDataModel::getStatusForNumberOfPlayers( int numPlayers ) const -> QByteArray {
	return kStatusesForNumberOfPlayers[wsw::min( numPlayers, (int)std::size( kStatusesForNumberOfPlayers ) - 1 )];
}

void HudCommonDataModel::updateTeamPlayerStatuses( const ReplicatedScoreboardData &scoreboardData ) {
	m_pendingNumAliveAlphaPlayers = 0;
	m_pendingNumAliveBetaPlayers = 0;

	m_pendingIndividualAlphaPlayerNum = std::nullopt;
	m_pendingIndividualBetaPlayerNum = std::nullopt;

	if( GS_IndividualGametype( *cggs ) ) {
		for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
			if( scoreboardData.isPlayerConnected( i ) ) {
				if( const int team = scoreboardData.getPlayerTeam( i ); team > TEAM_PLAYERS ) {
					if( team == TEAM_ALPHA ) {
						m_pendingIndividualAlphaPlayerNum = scoreboardData.getPlayerNum( i );
						if( !scoreboardData.isPlayerGhosting( i ) ) {
							m_pendingNumAliveAlphaPlayers++;
						}
					} else {
						m_pendingIndividualBetaPlayerNum = scoreboardData.getPlayerNum( i );
						if( !scoreboardData.isPlayerGhosting( i ) ) {
							m_pendingNumAliveBetaPlayers++;
						}
					}
				}
				// Check for early exit
				if( m_pendingIndividualAlphaPlayerNum && m_pendingIndividualBetaPlayerNum ) {
					break;
				}
			}
		}
	} else {
		for( unsigned i = 0; i < MAX_CLIENTS; ++i ) {
			if( scoreboardData.isPlayerConnected( i ) && !scoreboardData.isPlayerGhosting( i ) ) {
				if( const int team = scoreboardData.getPlayerTeam( i ); team > TEAM_PLAYERS ) {
					if( team == TEAM_ALPHA ) {
						m_pendingNumAliveAlphaPlayers++;
					} else {
						m_pendingNumAliveBetaPlayers++;
					}
				}
			}
		}
	}
}

}