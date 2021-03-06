#include "hudlayoutmodel.h"

#include <QPointF>
#include <QSizeF>

using wsw::operator""_asView;

namespace wsw::ui {

auto HudLayoutModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Origin, "origin" },
		{ Size, "size" },
		{ SelfAnchors, "selfAnchors" },
		{ AnchorItemAnchors, "anchorItemAnchors" },
		{ DisplayedAnchors, "displayedAnchors" },
		{ SelfAnchors, "selfAnchors" },
		{ AnchorItemAnchors, "anchorItemAnchors" },
		{ AnchorItemIndex, "anchorItemIndex" }
	};
}

auto HudLayoutModel::rowCount( const QModelIndex & ) const -> int {
	return m_fieldSize.isValid() ? (int)m_entries.size() : 0;
}

auto HudLayoutModel::data( const QModelIndex &index, int role ) const -> QVariant {
	if( index.isValid() ) {
		if( int row = index.row(); (unsigned)row < (unsigned)m_entries.size() ) {
			switch( role ) {
				case Origin: return m_entries[row].rectangle.topLeft();
				case Size: return m_entries[row].rectangle.size();
				case DisplayedAnchors: return m_entries[row].displayedAnchors;
				case DisplayedAnchorItemIndex: return m_entries[row].displayedAnchorItem.value_or( 0 );
				case SelfAnchors: return m_entries[row].selfAnchors;
				case AnchorItemAnchors: return m_entries[row].anchorItemAnchors;
				case AnchorItemIndex: return m_entries[row].realAnchorItem;
				default: return QVariant();
			}
		}
	}
	return QVariant();
}

HudLayoutModel::HudLayoutModel() {
	// Just for debugging
	Entry entry;
	entry.rectangle = QRectF( 0, 0, 32, 96 );
	entry.selfAnchors = Top | Left;
	entry.anchorItemAnchors = Top | Left;
	m_entries.push_back( entry );
	entry.rectangle = QRectF( 0, 0, 96, 32 );
	entry.selfAnchors = Bottom | Right;
	entry.anchorItemAnchors = Bottom | Right;
	m_entries.push_back( entry );
	entry.rectangle = QRectF( 0, 0, 64, 64 );
	entry.selfAnchors = VCenter | HCenter;
	entry.anchorItemAnchors = VCenter | HCenter;
	m_entries.push_back( entry );
}

void HudLayoutModel::setFieldSize( qreal width, qreal height ) {
	QSize size( width, height );
	if( size != m_fieldSize ) {
		beginResetModel();
		m_fieldSize = size;
		endResetModel();
	}
}

void HudLayoutModel::trackDragging( int index, qreal x, qreal y ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	m_entries[index].pendingOrigin = QPointF( x, y );
	updateMarkers( index );
}

void HudLayoutModel::finishDragging( int index ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	Entry &dragged = m_entries[index];
	if( dragged.rectangle.topLeft() != dragged.pendingOrigin ) {
		dragged.rectangle.moveTopLeft( dragged.pendingOrigin );
		QModelIndex modelIndex( createIndex( index, 0 ) );
		Q_EMIT dataChanged( modelIndex, modelIndex, kOriginRoleAsVector );
	}
}

void HudLayoutModel::updateAnchors( int index ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	std::optional<int> maybeNewAnchorItem;
	std::optional<std::pair<int, int>> newAnchors;
	for( unsigned i = 0; i < m_entries.size(); ++i ) {
		if( i != (unsigned)index ) {
			if( const auto maybeFlagsPair = getMatchingAnchors( m_entries[index], m_entries[i] ) ) {
				newAnchors = maybeFlagsPair;
				maybeNewAnchorItem = i;
			}
		}
	}

	// Can't reanchor
	if( !maybeNewAnchorItem ) {
		return;
	}

	const int newAnchorItem = *maybeNewAnchorItem;
	const int oldAnchorItem = m_entries[index].realAnchorItem;
	if( oldAnchorItem != newAnchorItem ) {
		if( oldAnchorItem >= 0 ) {
			m_entries[oldAnchorItem].displayedAnchors = 0;
			QModelIndex modelIndex( createIndex( oldAnchorItem, 0 ) );
			Q_EMIT dataChanged( modelIndex, modelIndex, kDisplayedAnchorsAsRole );
		}
	}

	m_entries[index].realAnchorItem = newAnchorItem;
	m_entries[index].displayedAnchors = 0;

	std::tie( m_entries[index].selfAnchors, m_entries[index].anchorItemAnchors ) = *newAnchors;

	const QModelIndex draggedModelIndex( createIndex( index, 0 ) );
	Q_EMIT dataChanged( draggedModelIndex, draggedModelIndex, kAllAnchorsAsRole );

	m_entries[newAnchorItem].displayedAnchors = 0;
	const QModelIndex anchorItemModelIndex( createIndex( newAnchorItem, 0 ) );
	Q_EMIT dataChanged( anchorItemModelIndex, anchorItemModelIndex, kDisplayedAnchorsAsRole );
}

void HudLayoutModel::updatePosition( int index, qreal x, qreal y ) {
	assert( (unsigned)index < (unsigned)m_entries.size() );
	QPointF point( x, y );
	Entry &entry = m_entries[index];
	if( entry.rectangle.topLeft() != point ) {
		entry.rectangle.moveTopLeft( point );
		QModelIndex modelIndex( createIndex( index, 0 ) );
		Q_EMIT dataChanged( modelIndex, modelIndex, kOriginRoleAsVector );
	}
}

class SmallIntSet {
	wsw::StaticVector<int, 3> m_nums;
public:
	[[nodiscard]]
	auto begin() const { return m_nums.begin(); }
	[[nodiscard]]
	auto end() const { return m_nums.end(); }
	void add( int num ) {
		if( std::find( m_nums.begin(), m_nums.end(), num ) == m_nums.end() ) {
			m_nums.push_back( num );
		}
	}
};

void HudLayoutModel::updateMarkers( int draggedIndex ) {
	assert( (unsigned)draggedIndex < m_entries.size() );
	Entry &dragged = m_entries[draggedIndex];

	std::optional<int> anchorItem;
	std::pair<int, int> flagsPair { 0, 0 };
	for( unsigned i = 0; i < m_entries.size(); ++i ) {
		if( i != (unsigned)draggedIndex ) {
			if( const auto maybeFlagsPair = getMatchingAnchors( dragged, m_entries[i] ) ) {
				flagsPair = *maybeFlagsPair;
				anchorItem = i;
			}
		}
	}

	SmallIntSet modifiedRows;

	// Dispatch updates
	if( dragged.displayedAnchorItem != anchorItem ) {
		// Clear flags of a (maybe) old item
		if( dragged.displayedAnchorItem ) {
			const int oldItemIndex = *dragged.displayedAnchorItem;
			m_entries[oldItemIndex].displayedAnchors = 0;
			modifiedRows.add( oldItemIndex );
		}
		dragged.displayedAnchorItem = anchorItem;
		modifiedRows.add( draggedIndex );
	}

	if( dragged.displayedAnchorItem ) {
		const int currItemIndex = *dragged.displayedAnchorItem;
		if( m_entries[currItemIndex].displayedAnchors != flagsPair.second ) {
			m_entries[currItemIndex].displayedAnchors = flagsPair.second;
			modifiedRows.add( currItemIndex );
		}
	}

	if( dragged.displayedAnchors != flagsPair.first ) {
		dragged.displayedAnchors = flagsPair.first;
		modifiedRows.add( draggedIndex );
	}

	for( int row: modifiedRows ) {
		QModelIndex modelIndex( createIndex( row, 0 ) );
		Q_EMIT dataChanged( modelIndex, modelIndex, kDisplayedAnchorsAsRole );
	}
}

// Pairs are arranged in their priority order
const HudLayoutModel::AllowedAnchorPair HudLayoutModel::kAllowedAnchorPairs[] {
	{ VCenter | HCenter, VCenter | HCenter },
	{ Top | HCenter, Bottom | HCenter },
	{ Bottom | HCenter, Top | HCenter },
	{ VCenter | Left, VCenter | Right },
	{ VCenter | Right, VCenter | Left },

	{ Top | Left, Bottom | Left }, { Top | Left, Bottom | Right },
	{ Top | Right, Bottom | Left }, { Top | Right, Bottom | Right },
	{ Bottom | Left, Top | Left }, { Bottom | Left, Top | Right },
	{ Bottom | Right, Top | Left }, { Bottom | Right, Top | Right }
};

auto HudLayoutModel::getMatchingAnchors( const Entry &dragged, const Entry &other )
	-> std::optional<std::pair<int, int>> {
	QRectF rectangle( dragged.rectangle );
	rectangle.moveTopLeft( dragged.pendingOrigin );

	if( rectangle.intersects( other.rectangle ) ) {
		// This is not efficient but this is not a performance-demanding code
		for( const auto &[selfAnchors, otherAnchors]: kAllowedAnchorPairs ) {
			const QPointF selfPoint( getPointForAnchors( rectangle, selfAnchors ) );
			const QPointF otherPoint( getPointForAnchors( other.rectangle, otherAnchors ) );
			const QPointF diff( selfPoint - otherPoint );
			// Units are device-independent so this is correct
			if( diff.x() * diff.x() + diff.y() * diff.y() < 15 * 15 ) {
				return std::make_pair( selfAnchors, otherAnchors );
			}
		}
	}

	return std::nullopt;
}

static constexpr auto kHorizontalBitsMask = 0x7;
static constexpr auto kVerticalBitsMask = 0x7 << 3;

auto HudLayoutModel::getPointForAnchors( const QRectF &r, int anchors ) -> QPointF {
	qreal x;
	switch( anchors & kHorizontalBitsMask ) {
		case Left: x = r.left(); break;
		case HCenter: x = 0.5 * ( r.left() + r.right() ); break;
		case Right: x = r.right(); break;
		default: throw std::invalid_argument( "Invalid X anchor bits" );
	}
	qreal y;
	switch( anchors & kVerticalBitsMask ) {
		case Top: y = r.top(); break;
		case VCenter: y = 0.5 * ( r.top() + r.bottom() ); break;
		case Bottom: y = r.bottom(); break;
		default: throw std::invalid_argument( "Invalid Y anchor bits" );
	}
	return QPointF( x, y );
}

[[nodiscard]]
static inline auto getHorizontalAnchorName( int bits ) -> wsw::StringView {
	switch( bits & kHorizontalBitsMask ) {
		case HudLayoutModel::Left: return "Left"_asView;
		case HudLayoutModel::HCenter: return "HCenter"_asView;
		case HudLayoutModel::Right: return "Right"_asView;
		default: return "None"_asView;
	}
}

[[nodiscard]]
static inline auto getVerticalAnchorName( int bits ) -> wsw::StringView {
	switch( bits & kVerticalBitsMask ) {
		case HudLayoutModel::Top: return "Top"_asView;
		case HudLayoutModel::VCenter: return "VCenter"_asView;
		case HudLayoutModel::Bottom: return "Bottom"_asView;
		default: return "None"_asView;
	}
}

auto HudLayoutModel::getAnchorNames( int anchors ) -> std::pair<wsw::StringView, wsw::StringView> {
	return { getVerticalAnchorName( anchors ), getHorizontalAnchorName( anchors ) };
}

}