#ifndef WSW_5abcba0e_4672_47ce_b467_a04ee4bf4e0c_H
#define WSW_5abcba0e_4672_47ce_b467_a04ee4bf4e0c_H

#include <QAbstractListModel>
#include <QDateTime>
#include <QList>

#include <common/q_arch.h>
#include <common/q_shared.h>
#include <common/wswstaticstring.h>
#include <common/wswpodvector.h>
#include <common/freelistallocator.h>

// TODO we can use a specialized one based on a freelist allocator
#include <deque>

struct MessageFault;

namespace wsw::cl { struct ChatMessage; }

namespace wsw::ui {

class ChatProxy;

class ChatModelsShared {
protected:
	struct Line {
		char *basePtr;
		uint8_t nameLen;
		uint8_t timestampLen;
		uint8_t textLen;
		[[nodiscard]]
		auto getName() const -> wsw::StringView {
			return { basePtr, nameLen, wsw::StringView::ZeroTerminated };
		}
		[[nodiscard]]
		auto getTimestamp() const -> wsw::StringView {
			return { basePtr + nameLen + 1, timestampLen, wsw::StringView::ZeroTerminated };
		}
		[[nodiscard]]
		auto getText() const -> wsw::StringView {
			return { basePtr + nameLen + 1 + timestampLen + 1, textLen, wsw::StringView::ZeroTerminated };
		}
	};
};

class ChatModel : protected ChatModelsShared, public QAbstractListModel {
	friend class ChatProxy;
protected:
	explicit ChatModel( ChatProxy *proxy ) : m_proxy( proxy ) {}

	ChatProxy *const m_proxy;

	virtual void beginClear() = 0;
	virtual void endClear() = 0;

	virtual void beginRemoveOldestLine( Line *line ) = 0;
	virtual void endRemoveOldestLine() = 0;

	virtual void beginAddingLine( Line *line, const QDate &date, int timeHours, int timeMinutes ) = 0;
	virtual void endAddingLine() = 0;
};

class CompactChatModel : public ChatModel {
	friend class ChatProxy;

	explicit CompactChatModel( ChatProxy *proxy ) : ChatModel( proxy ) {}

	enum Role { Text = Qt::UserRole + 1, Name, Timestamp };

	void beginClear() override { beginResetModel(); }
	void endClear() override { endResetModel(); }

	void beginRemoveOldestLine( Line *line ) override;

	void endRemoveOldestLine() override { endRemoveRows(); };

	void beginAddingLine( Line *, const QDate &, int, int ) override { beginInsertRows( QModelIndex(), 0, 0 ); }
	void endAddingLine() override { endInsertRows(); }

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
};

class RichChatModel : public ChatModel {
	friend class ChatProxy;

	wsw::StaticString<MAX_NAME_BYTES> m_lastMessageName;
	QDate m_currHeadingDate;
	int m_currHeadingHour { -999 };
	int m_currHeadingMinute { 0 };
	int m_lastMessageMinute { 0 };
	unsigned m_totalGroupLengthSoFar { 0 };

	// Default ListView sections don't work with the desired direction.
	// The approach of manual sections management is also more flexible.
	struct Entry {
		Line *regularLine { nullptr };
		Line *sectionLine { nullptr };
	};

	std::deque<Entry> m_entries;

	enum Role { RegularMessageText = Qt::UserRole + 1, SectionName, SectionTimestamp };

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;

	[[nodiscard]]
	bool canAddToCurrGroup( const Line *line, const QDate &date, int timeHours, int timeMinutes );

	void beginClear() override;
	void endClear() override;

	void beginRemoveOldestLine( Line *line ) override;
	void endRemoveOldestLine() override;

	void beginAddingLine( Line *line, const QDate &date, int timeHours, int timeMinutes ) override;
	void endAddingLine() override;

	explicit RichChatModel( ChatProxy *proxy ) : ChatModel( proxy ) {}
};

class ChatProxy : public QObject, protected ChatModelsShared {
	Q_OBJECT

	friend class ChatModel;
	friend class CompactChatModel;
	friend class RichChatModel;

	CompactChatModel m_compactModel { this };
	RichChatModel m_richModel { this };

	ChatModel *m_childModels[2] { &m_compactModel, &m_richModel };

	static constexpr unsigned kMaxTimeBytes = 6;
	static constexpr unsigned kFullLineSize = sizeof( Line ) + MAX_CHAT_BYTES + MAX_NAME_BYTES + kMaxTimeBytes;
	static constexpr unsigned kMaxLines = 4 * 4096;

	wsw::HeapBasedFreelistAllocator m_linesAllocator { kFullLineSize, kMaxLines, alignof( Line ) };
	std::deque<Line *> m_lineRefs;

	wsw::PodVector<uint64_t> m_pendingCommandNums;

	QDate m_lastMessageQtDate;
	int m_lastMessageTimeHours { 0 };
	int m_lastMessageTimeMinutes { 0 };
	wsw::StaticString<kMaxTimeBytes> m_lastMessageFormattedTime;
	int64_t m_lastMessageFrameTimestamp { 0 };
	bool m_wasInTheSameFrame { false };
	bool m_hasSetCompactModelOwnership { false };
	bool m_hasSetRichModelOwnership { false };

	[[nodiscard]]
	bool removeFromPendingCommands( uint64_t commandNum );
public:
	enum ChatKind : uint8_t { Chat, TeamChat };
private:
	ChatKind m_kind;
public:
	explicit ChatProxy( ChatKind kind ): m_kind( kind ) {}

	Q_SIGNAL void muted();
	Q_SIGNAL void floodDetected();

	[[nodiscard]]
	Q_INVOKABLE QObject *getCompactModel();
	[[nodiscard]]
	Q_INVOKABLE QObject *getRichModel();

	Q_INVOKABLE void sendMessage( const QString &text );

	void clear();

	void addReceivedMessage( const wsw::cl::ChatMessage &message, int64_t frameTimestamp );
	void handleMessageFault( const MessageFault &messageFault );
};

}

#endif
