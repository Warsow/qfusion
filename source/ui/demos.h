#ifndef WSW_86716366_baac_4acf_9b1c_cb6d22b28663_H
#define WSW_86716366_baac_4acf_9b1c_cb6d22b28663_H

#include "../qcommon/qcommon.h"
#include "../qcommon/stringspanstorage.h"
#include "../qcommon/wswstdtypes.h"

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

	wsw::Vector<unsigned> m_addedNew;
	wsw::Vector<unsigned> m_goneOld;

	using StringSpan = std::pair<unsigned, unsigned>;
	using StringDataStorage = wsw::StringSpanStorage<unsigned, unsigned>;

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
	wsw::Vector<ResolveTaskResult *> m_taskResultsToProcess;

	ResolveTaskResult *m_taskResultsHead { nullptr };
	wsw::Vector<const MetadataEntry *> m_entries;

	wsw::Vector<uint64_t> m_lastQueryResults;
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
					 	const std::pair<unsigned, unsigned> &baseNameSpan,
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
		Gametype
	};

	DemosResolver *const m_resolver;

	Q_SLOT void onIsResolverReadyChanged( bool isReady );
public:
	explicit DemosModel( DemosResolver *resolver );

	[[nodiscard]]
	auto roleNames() const -> QHash<int, QByteArray> override;
	[[nodiscard]]
	auto rowCount( const QModelIndex & ) const -> int override;
	[[nodiscard]]
	auto data( const QModelIndex &index, int role ) const -> QVariant override;
};

}

#endif