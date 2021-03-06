#ifndef WSW_8c488c22_5afd_4fad_9e25_9304dd129d10_H
#define WSW_8c488c22_5afd_4fad_9e25_9304dd129d10_H

#include "../qcommon/qcommon.h"
#include "../qcommon/wswstdtypes.h"

#include <QAbstractListModel>
#include <QQuickItem>
#include <QSizeF>
#include <QPointF>
#include <QRectF>

namespace wsw::ui {

class HudLayoutModel : public QAbstractListModel {
	Q_OBJECT
public:
	Q_INVOKABLE void trackDragging( int index, qreal x, qreal y );
	Q_INVOKABLE void finishDragging( int index );
	Q_INVOKABLE void setFieldSize( qreal width, qreal height );
	Q_INVOKABLE void updatePosition( int index, qreal x, qreal y );
	Q_INVOKABLE void updateAnchors( int index );

	HudLayoutModel();

	enum HorizontalAnchorBits {
		Left    = 0x1,
		HCenter = 0x2,
		Right   = 0x4
	};
	Q_ENUM( HorizontalAnchorBits );

	enum VerticalAnchorBits {
		Top      = 0x1 << 3,
		VCenter  = 0x2 << 3,
		Bottom   = 0x4 << 3
	};
	Q_ENUM( VerticalAnchorBits );
private:
	enum Role {
		Origin = Qt::UserRole + 1,
		Size,
		DisplayedAnchors,
		DisplayedAnchorItemIndex,
		SelfAnchors,
		AnchorItemAnchors,
		AnchorItemIndex
	};

	struct Entry {
		QRectF rectangle;
		QPointF pendingOrigin;
		int displayedAnchors { 0 };
		int selfAnchors { 0 };
		int anchorItemAnchors { 0 };
		int realAnchorItem { -1 };
		std::optional<int> displayedAnchorItem;
	};

	static inline const QVector<int> kDisplayedAnchorsAsRole { DisplayedAnchors };
	static inline const QVector<int> kAllAnchorsAsRole { DisplayedAnchors, SelfAnchors, AnchorItemAnchors };
	static inline const QVector<int> kOriginRoleAsVector { Origin };

	QSizeF m_fieldSize;

	wsw::Vector<Entry> m_entries;

	void updateMarkers( int draggedIndex );

	struct AllowedAnchorPair {
		int selfAnchors;
		int otherAnchors;
	};

	static const AllowedAnchorPair kAllowedAnchorPairs[];

	[[nodiscard]]
	auto getMatchingAnchors( const Entry &dragged, const Entry &other ) -> std::optional<std::pair<int, int>>;

	[[nodiscard]]
	static auto getPointForAnchors( const QRectF &r, int anchors ) -> QPointF;

	[[nodiscard]]
	static auto getAnchorNames( int anchors ) -> std::pair<wsw::StringView, wsw::StringView>;

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &, int role ) const -> QVariant override;
};

}

#endif
