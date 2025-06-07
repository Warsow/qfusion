#ifndef WSW_86716366_baac_4acf_9b1c_cb6d22b28663_H
#define WSW_86716366_baac_4acf_9b1c_cb6d22b28663_H

#include <common/common.h>
#include <common/types/stringspanstorage.h>
#include <common/types/vector.h>
#include "local.h"

#include <QAbstractListModel>
#include <QSharedPointer>
#include <QThreadPool>
#include <QMutex>
#include <QDateTime>
#include <QDate>

namespace wsw::ui {

class DemosResolver : public QObject {
	Q_OBJECT

	friend class EnumerateFilesTask;
	friend class RunQueryTask;
	friend class DemosModel;

	wsw::StringSpanStorage<unsigned, unsigned> m_fileNameSpans[2];

	QThreadPool m_threadPool;
	int m_turn { 0 };
	bool m_isReady { true };

	wsw::PodVector<unsigned> m_addedNew;
	wsw::PodVector<unsigned> m_goneOld;

	using StringSpan = std::pair<uint32_t, uint8_t>;
	using StringDataStorage = wsw::StringSpanStorage<uint32_t, uint8_t>;

	static constexpr unsigned kMaxOtherKeysAndValues = 8;
	static constexpr unsigned kMaxTags = 8;

	struct ResolveTaskResult;

	struct MetadataEntry {
		ResolveTaskResult *parent { nullptr };
		MetadataEntry *prev { nullptr }, *next { nullptr };
		// StaticVector<?,?> is uncopyable and that's reasonable.
		std::pair<unsigned, unsigned> otherKeysAndValues[kMaxOtherKeysAndValues];
		unsigned tagIndices[kMaxTags];
		uint64_t rawTimestamp;
		QDateTime timestamp;
		QDate sectionDate;
		unsigned fileNameIndex;
		unsigned fileNameHash;
		std::pair<unsigned, unsigned> baseNameSpan;
		unsigned hashBinIndex;
		unsigned serverNameIndex;
		unsigned mapNameIndex;
		unsigned mapChecksumIndex;
		unsigned gametypeIndex;
		int duration;
		unsigned numOtherKeysAndValues;
		unsigned numTags;

		[[nodiscard]]
		auto getServerName() const -> wsw::StringView { return parent->storage[serverNameIndex]; }
		[[nodiscard]]
		auto getMapName() const -> wsw::StringView { return parent->storage[mapNameIndex]; }
		[[nodiscard]]
		auto getFileName() const -> wsw::StringView { return parent->storage[fileNameIndex]; }
		[[nodiscard]]
		auto getGametype() const -> wsw::StringView { return parent->storage[gametypeIndex]; }
		[[nodiscard]]
		auto getDemoName() const -> wsw::StringView {
			return getFileName().takeMid( baseNameSpan.first, baseNameSpan.second );
		}
	};

	static constexpr unsigned kNumBins { 79 };
	MetadataEntry *m_hashBins[kNumBins] {};

	struct ResolveTaskResult {
		ResolveTaskResult *prev { nullptr }, *next { nullptr };
		wsw::Vector<MetadataEntry> entries;
		StringDataStorage storage;
		int numRefs { 0 };
	};

	unsigned m_numPendingTasks { 0 };
	unsigned m_numCompletedTasks { 0 };
	wsw::PodVector<ResolveTaskResult *> m_taskResultsToProcess;

	ResolveTaskResult *m_taskResultsHead { nullptr };
	wsw::PodVector<const MetadataEntry *> m_entries;

	wsw::PodVector<uint64_t> m_lastQueryResults;
	wsw::StaticString<30> m_lastQuery;

	[[nodiscard]]
	auto getCount() const -> int {
		if( m_lastQuery.empty() ) {
			return (int)m_entries.size();
		}
		return m_lastQueryResults.size();
	};

	[[nodiscard]]
	auto getEntry( int row ) const -> const MetadataEntry * {
		if( m_lastQuery.empty() ) {
			return m_entries[row];
		}
		return m_entries[m_lastQueryResults[row]];
	}

	void enumerateFiles();
	void purgeMetadata( const wsw::StringView &file );
	void resolveMetadata( unsigned first, unsigned last );
	void resolveMetadata( unsigned index, wsw::Vector<MetadataEntry> *entries, StringDataStorage *storage );
	void parseMetadata( const char *data, size_t dataSize,
					 	const wsw::StringView &fullFileName,
					 	const StringSpan &baseNameSpan,
					    const std::optional<StringSpan> &prefixTagSpan,
					    wsw::Vector<MetadataEntry > *entries, StringDataStorage *storage );
	void processTaskResults();
	void updateDefaultDisplayedList();
	void runQuery();

	template <typename WordMatcher>
	void runQueryUsingWordMatcher( const wsw::StringView &query );

	Q_SIGNAL void resolveTaskCompleted( ResolveTaskResult *result );
	Q_SLOT void takeResolveTaskResult( ResolveTaskResult *result );
	Q_SIGNAL void resolveTasksReady( bool isReady );
	Q_SIGNAL void runQueryTasksReady( bool isReady );
	Q_SLOT void setReady( bool ready );
public:
	DemosResolver();
	~DemosResolver() override;

	Q_SIGNAL void isReadyChanged( bool isReady );

	Q_PROPERTY( bool isReady READ isReady NOTIFY isReadyChanged );
	Q_INVOKABLE void reload();
	Q_INVOKABLE void query( const QString &query );

	Q_SIGNAL void progressUpdated( QVariant maybePercents );

	[[nodiscard]]
	bool isReady() const { return m_isReady; };
};

class DemosModel : public QAbstractListModel {
	Q_OBJECT

	enum Role {
		Section = Qt::UserRole + 1,
		Timestamp,
		ServerName,
		DemoName,
		FileName,
		MapName,
		Gametype,
		Tags
	};

	DemosResolver *const m_resolver;

	Q_SLOT void onIsResolverReadyChanged( bool isReady );

	[[nodiscard]]
	auto formatTags( const DemosResolver::MetadataEntry *entry ) const -> QByteArray;
public:
	explicit DemosModel( DemosResolver *resolver );

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
};

class QtUISystem;

// Just to expose a typed/structured demo playback interface
class DemoPlayer : public QObject {
	Q_OBJECT

	friend class QtUISystem;

	QString m_timestamp;
	QString m_gametype;
	QString m_mapName;
	QString m_serverName;
	QString m_demoName;

	int m_duration { 0 };
	int m_progress { 0 };
	bool m_isPlaying { false };
	bool m_isPaused { false };

	explicit DemoPlayer( QtUISystem * ) {}

	void checkUpdates();
	void reloadMetadata();
public:
	Q_SIGNAL void isPlayingChanged( bool isPlaying );
	Q_PROPERTY( bool isPlaying MEMBER m_isPlaying NOTIFY isPlayingChanged );

	Q_SIGNAL void isPausedChanged( bool isPaused );
	Q_PROPERTY( bool isPaused MEMBER m_isPaused NOTIFY isPausedChanged );

	Q_SIGNAL void durationChanged( int duration );
	Q_PROPERTY( int duration MEMBER m_duration NOTIFY durationChanged );

	Q_SIGNAL void progressChanged( int progress );
	Q_PROPERTY( int progress MEMBER m_progress NOTIFY progressChanged );

	Q_SIGNAL void timestampChanged( QString timestamp );
	Q_PROPERTY( QString timestamp MEMBER m_timestamp NOTIFY timestampChanged );

	Q_SIGNAL void gametypeChanged( QString gametype );
	Q_PROPERTY( QString gametype MEMBER m_gametype NOTIFY gametypeChanged );

	Q_SIGNAL void mapNameChanged( QString mapName );
	Q_PROPERTY( QString mapName MEMBER m_mapName NOTIFY mapNameChanged );

	Q_SIGNAL void serverNameChanged( QString serverName );
	Q_PROPERTY( QString serverName MEMBER m_serverName NOTIFY serverNameChanged );

	Q_SIGNAL void demoNameChanged( QString demoName );
	Q_PROPERTY( QString demoName MEMBER m_demoName NOTIFY demoNameChanged );

	Q_INVOKABLE void play( const QByteArray &fileName );
	Q_INVOKABLE void pause();
	Q_INVOKABLE void stop();
	Q_INVOKABLE void seek( qreal frac );

	Q_INVOKABLE QByteArray formatDuration( int durationSeconds );
};

}

#endif