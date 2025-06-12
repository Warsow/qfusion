#include "demos.h"
#include <common/facilities/protocol.h>
#include <common/facilities/cvar.h>
#include <common/common.h>
#include <common/helpers/links.h>
#include <common/helpers/algorithm.h>
#include <common/helpers/tonum.h>
#include <common/version.h>
#include <common/facilities/wswfs.h>
#include <common/facilities/sysclock.h>
#include <client/client.h>
#include "wordsmatcher.h"

namespace wsw::ui {

DemosModel::DemosModel( DemosResolver *resolver ) : m_resolver( resolver ) {
	connect( m_resolver, &DemosResolver::isReadyChanged, this, &DemosModel::onIsResolverReadyChanged );
}

auto DemosModel::roleNames() const -> QHash<int, QByteArray> {
	return {
		{ Section, "section" },
		{ Timestamp, "timestamp" },
		{ ServerName, "serverName" },
		{ DemoName, "demoName" },
		{ FileName, "fileName" },
		{ MapName, "mapName" },
		{ Gametype, "gametype" },
		{ Tags, "tags" }
	};
}

auto DemosModel::rowCount( const QModelIndex & ) const -> int {
	return m_resolver->isReady() ? m_resolver->getCount() : 0;
}

[[nodiscard]]
static inline auto toQVariant( const wsw::StringView &view ) -> QVariant {
	return QByteArray( view.data(), view.size() );
}

[[nodiscard]]
static inline auto formatTimestamp( const QDateTime &timestamp ) -> QString {
	return timestamp.toString( Qt::DefaultLocaleShortDate );
}

auto DemosModel::formatTags( const DemosResolver::MetadataEntry *entry ) const -> QByteArray {
	QByteArray result;
	for( unsigned i = 0; i < entry->numTags; ++i ) {
		const wsw::StringView &view = entry->parent->storage[entry->tagIndices[i]];
		result.append( view.data(), (int)view.size() );
		if( i + 1 != entry->numTags ) {
			result.append( ',' );
		}
	}
	return result;
}

auto DemosModel::data( const QModelIndex &index, int role ) const -> QVariant {
	assert( m_resolver->isReady() );
	if( index.isValid() ) {
		if( const int row = index.row(); (unsigned)row < (unsigned)m_resolver->getCount() ) {
			switch( role ) {
				case Section: return m_resolver->getEntry( row )->sectionDate;
				case Timestamp: return formatTimestamp( m_resolver->getEntry( row )->timestamp );
				case ServerName: return toQVariant( m_resolver->getEntry( row )->getServerName() );
				case DemoName: return toQVariant( m_resolver->getEntry( row )->getDemoName() );
				case FileName: return toQVariant( m_resolver->getEntry( row )->getFileName() );
				case MapName: return toQVariant( m_resolver->getEntry( row )->getMapName() );
				case Gametype: return toQVariant( m_resolver->getEntry( row )->getGametype() );
				case Tags: return formatTags( m_resolver->getEntry( row ) );
				default:;
			}
		}
	}
	return QVariant();
}

void DemosModel::onIsResolverReadyChanged( bool ) {
	beginResetModel();
	endResetModel();
}

DemosResolver::DemosResolver() {
	m_threadPool.setExpiryTimeout( 3000 );
	// Establish connections for inter-thread transmissions of task results
	Qt::ConnectionType connectionType = Qt::QueuedConnection;
	connect( this, &DemosResolver::resolveTaskCompleted, this, &DemosResolver::takeResolveTaskResult, connectionType );
	connect( this, &DemosResolver::resolveTasksReady, this, &DemosResolver::setReady, connectionType );
	connect( this, &DemosResolver::runQueryTasksReady, this, &DemosResolver::setReady, connectionType );
}

DemosResolver::~DemosResolver() {
	ResolveTaskResult *next;
	for( ResolveTaskResult *result = m_taskResultsHead; result; result = next ) {
		next = result->next;
		delete result;
	}
}

// A wrapper for using the custom thread pool
class EnumerateFilesTask : public QRunnable {
	DemosResolver *const m_resolver;
public:
	explicit EnumerateFilesTask( DemosResolver *resolver ) : m_resolver( resolver ) {}
	void run() override { m_resolver->enumerateFiles(); }
};

class RunQueryTask : public QRunnable {
	DemosResolver *const m_resolver;
public:
	explicit RunQueryTask( DemosResolver *resolver ) : m_resolver( resolver ) {}
	void run() override { m_resolver->runQuery(); }
};

void DemosResolver::reload() {
	assert( m_isReady );

	setReady( false );

	m_lastQueryResults.clear();
	m_lastQuery.clear();

	Q_EMIT progressUpdated( QVariant() );

	m_threadPool.start( new EnumerateFilesTask( this ) );
}

void DemosResolver::query( const QString &query ) {
	assert( m_isReady );

	const QByteArray queryBytes( query.toLatin1() );
	assert( (unsigned)queryBytes.size() < m_lastQuery.capacity() );
	const wsw::StringView queryView( queryBytes.data(), queryBytes.size() );

	if( m_lastQuery.asView().equalsIgnoreCase( queryView ) ) {
		return;
	}

	setReady( false );

	m_lastQuery.assign( queryView );
	m_lastQueryResults.clear();

	Q_EMIT progressUpdated( QVariant() );

	m_threadPool.start( new RunQueryTask( this ) );
}

void DemosResolver::setReady( bool ready ) {
	m_isReady = ready;
	Q_EMIT isReadyChanged( ready );
}

static const wsw::StringView kSearchPathRoot( "demos" );
static const wsw::StringView kDemoExtension( APP_DEMO_EXTENSION_STR );

template <typename Container>
[[nodiscard]]
static bool containsIgnoringCase( Container &c, const wsw::StringView &v ) {
	for( const wsw::StringView &existing: c ) {
		if( existing.equalsIgnoreCase( v ) ) {
			return true;
		}
	}
	return false;
}

void DemosResolver::enumerateFiles() {
	wsw::fs::SearchResultHolder holder;
	const auto &oldFileNames = m_fileNameSpans[m_turn];
	const auto nextTurn = ( m_turn + 1 ) % 2;
	auto &newFileNames = m_fileNameSpans[nextTurn];
	newFileNames.clear();

	const auto visitor = [&]( const wsw::StringView &dir, const wsw::StringView &name ) {
		wsw::StaticString<MAX_QPATH> path;
		path << dir << '/' << name;
		newFileNames.add( path.asView() );
	};

	(void)wsw::fs::walkDir( kSearchPathRoot, visitor, wsw::fs::WalkDirOptions {
		.errorPolicy = wsw::fs::ContinueWalking, .maxDepth = 1
	});

	m_addedNew.clear();
	m_goneOld.clear();

	// TODO: Rewrite to use a "hash-join" (RDBMS-like)
	for( unsigned i = 0; i < newFileNames.size(); ++i ) {
		if( !containsIgnoringCase( oldFileNames, newFileNames[i] ) ) {
			m_addedNew.push_back( i );
		}
	}
	for( unsigned i = 0; i < oldFileNames.size(); ++i ) {
		if( !containsIgnoringCase( newFileNames, oldFileNames[i] ) ) {
			m_goneOld.push_back( i );
		}
	}

	if( m_addedNew.empty() && m_goneOld.empty() ) {
		Q_EMIT resolveTasksReady( true );
		return;
	}

	m_turn = ( m_turn + 1 ) % 2;
	for( unsigned oldIndex: m_goneOld ) {
		purgeMetadata( oldFileNames[oldIndex] );
	}

	Q_EMIT progressUpdated( 0 );

	m_numPendingTasks = 1;
	m_numCompletedTasks = 0;
	m_taskResultsToProcess.clear();
	// Continue this task for now
	resolveMetadata( 0, m_addedNew.size() );
}

void DemosResolver::purgeMetadata( const wsw::StringView &fileName ) {
	const auto hash = wsw::HashedStringView( fileName ).getHash();
	const auto binIndex = hash % kNumBins;
	for( MetadataEntry *entry = m_hashBins[binIndex]; entry; entry = entry->next ) {
		if( entry->fileNameHash == hash && entry->parent->storage[entry->fileNameIndex] == fileName ) {
			wsw::unlink( entry, &m_hashBins[binIndex] );
			entry->parent->numRefs--;
			if( !entry->parent->numRefs ) {
				wsw::unlink( entry->parent, &m_taskResultsHead );
				delete entry->parent;
			}
			return;
		}
	}
	wsw::failWithLogicError( "unreachable" );
}

void DemosResolver::resolveMetadata( unsigned first, unsigned last ) {
	ResolveTaskResult *taskResult = nullptr;
	try {
		taskResult = new ResolveTaskResult;
		for ( unsigned index = first; index < last; ++index ) {
			resolveMetadata( index, &taskResult->entries, &taskResult->storage );
		}
		Q_EMIT takeResolveTaskResult( taskResult );
	} catch (...) {
		delete taskResult;
		Q_EMIT takeResolveTaskResult( nullptr );
	}
}

void DemosResolver::resolveMetadata( unsigned index, wsw::Vector<MetadataEntry> *entries, StringDataStorage *storage ) {
	const wsw::StringView filePath( m_fileNameSpans[m_turn][index].data() );
	assert( filePath.isZeroTerminated() );
	assert( filePath.length() <= std::numeric_limits<unsigned>::max() );

	std::optional<StringSpan> prefixTagSpan;
	StringSpan baseNameSpan( 0, filePath.length() );
	if( const auto slashIndex = filePath.lastIndexOf( '/' ) ) {
		const unsigned dirNameLen = *slashIndex;
		baseNameSpan.first = ( dirNameLen + 1 );
		baseNameSpan.second -= ( dirNameLen + 1 );

		wsw::StringView dirName( filePath.take( *slashIndex ) );
		const auto oldDirNameLen = dirName.length();
		assert( oldDirNameLen <= std::numeric_limits<unsigned>::max() );
		if( dirName.startsWith( kSearchPathRoot ) ) {
			dirName = dirName.drop( kSearchPathRoot.length() );
		}
		if( dirName.startsWith( '/' ) ) {
			dirName = dirName.drop( 1 );
			// FS facilities must return a normalized path
			assert( !dirName.startsWith( '/' ) );
		}
		assert( oldDirNameLen >= dirName.length() );
		unsigned tagOffset = oldDirNameLen - dirName.length();
		unsigned tagLength = dirName.length();
		if( const auto prevSlashIndex = dirName.lastIndexOf( '/' ) ) {
			tagOffset += *prevSlashIndex;
			tagLength = *slashIndex - *prevSlashIndex;
		}
		assert( tagOffset + tagLength <= filePath.length() );
		assert( tagOffset + tagLength <= std::numeric_limits<uint8_t>::max() );
		// TODO: This condition should be checked earlier
		if( tagLength ) {
			prefixTagSpan = std::make_pair( (uint8_t)tagOffset, (uint8_t)tagLength );
		}
	}

	if( filePath.endsWith( kDemoExtension ) ) {
		baseNameSpan.second -= kDemoExtension.length();
	}

	assert( baseNameSpan.first + baseNameSpan.second <= filePath.length() );

	int handle = 0;
	int length = FS_FOpenFile( filePath.data(), &handle, FS_READ | SNAP_DEMO_GZ );
	if( length > 0 ) {
		char metadata[SNAP_MAX_DEMO_META_DATA_SIZE];
		const size_t realSize = SNAP_ReadDemoMetaData( handle, metadata, sizeof( metadata ) );
		if( realSize <= sizeof( metadata ) ) {
			parseMetadata( metadata, realSize, filePath, baseNameSpan, prefixTagSpan, entries, storage );
		}
	}

	FS_FCloseFile( handle );
}

static const wsw::StringView kMandatoryKeys[6] {
	kDemoKeyServerName, kDemoKeyTimestamp, kDemoKeyDuration, kDemoKeyMapName, kDemoKeyMapChecksum, kDemoKeyGametype
};

static const wsw::CharLookup kAllowedTagChars {
	// Locale-independent
	[](char ch) { return ( ch >= 'a' && ch <= 'z' ) || ( ch >= 'A' && ch <= 'Z' ) || ( ch >= '0' && ch <= '9' ); }
};


void DemosResolver::parseMetadata( const char *data, size_t dataSize,
								   const wsw::StringView &fileName,
								   const StringSpan &baseNameSpan,
								   const std::optional<StringSpan> &prefixTagSpan,
								   wsw::Vector<MetadataEntry> *entries,
								   StringDataStorage *stringData ) {
	constexpr auto kMaxStringLen = std::numeric_limits<uint8_t>::max();
	struct StorageAdapter {
		StringDataStorage *const storage;
		[[nodiscard]]
		auto add( const wsw::StringView &s ) -> unsigned {
			return storage->add( s );
		}
		[[nodiscard]]
		auto addLowercase( const wsw::StringView &s ) -> unsigned {
			auto [writePtr, index] = storage->addReservedSpace( s.length() );
			assert( s.length() < kMaxStringLen );
			for( char ch: s ) {
				*writePtr++ = (char)std::tolower( ch );
			}
			return index;
		}
	};

	wsw::StaticVector<std::pair<wsw::StringView, wsw::StringView>, kMaxOtherKeysAndValues> otherKeysAndValues;
	std::optional<wsw::StringView> parsedMandatoryValues[6];
	wsw::StaticVector<wsw::StringView, kMaxTags> tags;

	wsw::DemoMetadataReader reader( data, dataSize );
	while( reader.hasNextPair() ) {
		const auto maybeKeyValue = reader.readNextPair();
		if( !maybeKeyValue ) {
			return;
		}
		const auto [key, value] = *maybeKeyValue;
		if( key.length() > kMaxStringLen || value.length() > kMaxStringLen ) {
			return;
		}

		bool isAMandatoryKey = false;
		for( const auto &mandatoryKey: kMandatoryKeys ) {
			if( mandatoryKey.equalsIgnoreCase( key ) ) {
				parsedMandatoryValues[std::addressof( mandatoryKey ) - kMandatoryKeys] = value;
				isAMandatoryKey = true;
				break;
			}
		}
		if( !isAMandatoryKey ) {
			if( !otherKeysAndValues.full() ) {
				otherKeysAndValues.push_back( *maybeKeyValue );
			}
			// Continue pairs retrieval regardless of pairs capacity exhaustion
		}
	}

	if( prefixTagSpan ) {
		assert( prefixTagSpan->second <= kMaxStringLen );
		tags.push_back( fileName.drop( prefixTagSpan->first ).take( prefixTagSpan->second ) );
	}

	while( reader.hasNextTag() && !tags.full() ) {
		const auto maybeTag = reader.readNextTag();
		if( maybeTag && maybeTag->length() <= kMaxStringLen && maybeTag->containsOnly( kAllowedTagChars ) ) {
			tags.push_back( *maybeTag );
		} else {
			return;
		}
	}

	StorageAdapter adapter { stringData };
	if( !wsw::contains( parsedMandatoryValues, parsedMandatoryValues + std::size( parsedMandatoryValues ), std::nullopt ) ) {
		if( const auto maybeTimestamp = wsw::toNum<uint64_t>( *parsedMandatoryValues[1] ) ) {
			if( const auto maybeDuration = wsw::toNum<int>( *parsedMandatoryValues[2] ) ) {
				const uint32_t fileNameHash = wsw::HashedStringView( fileName ).getHash();
				const QDateTime timestamp   = QDateTime::fromSecsSinceEpoch( (qint64)*maybeTimestamp );
				MetadataEntry &__restrict entry = entries->emplace_back( MetadataEntry {
					.rawTimestamp     = *maybeTimestamp,
					.timestamp        = timestamp,
					.sectionDate      = timestamp.date(),
					.fileNameIndex    = stringData->add( fileName ),
					.fileNameHash     = fileNameHash,
					.baseNameSpan     = baseNameSpan,
					.hashBinIndex     = fileNameHash % kNumBins,
					.serverNameIndex  = adapter.add( *parsedMandatoryValues[0] ),
					.mapNameIndex     = adapter.add( *parsedMandatoryValues[3] ),
					.mapChecksumIndex = adapter.addLowercase( *parsedMandatoryValues[4] ),
					.gametypeIndex 	  = adapter.addLowercase( *parsedMandatoryValues[5] ),
					.duration         = *maybeDuration,
				});
				entry.numOtherKeysAndValues = 0;
				for( const auto &[key, value]: otherKeysAndValues ) {
					const auto keyIndex   = adapter.addLowercase( key );
					const auto valueIndex = adapter.addLowercase( value );
					entry.otherKeysAndValues[entry.numOtherKeysAndValues++] = { keyIndex, valueIndex };
				}
				entry.numTags = 0;
				for( const auto &tag: tags ) {
					entry.tagIndices[entry.numTags++] = adapter.addLowercase( tag );
				}
			}
		}
	}
}

void DemosResolver::takeResolveTaskResult( ResolveTaskResult *result ) {
	if( result ) {
		m_taskResultsToProcess.push_back( result );
	}
	m_numCompletedTasks++;
	const auto progress = (int)( ( (float)m_numCompletedTasks * 100.0f ) / (float)m_numPendingTasks );
	Q_EMIT progressUpdated( progress );
	if( m_numPendingTasks == m_numCompletedTasks ) {
		processTaskResults();
		Q_EMIT resolveTasksReady( true );
	}
}

void DemosResolver::processTaskResults() {
	for( ResolveTaskResult *const result: m_taskResultsToProcess ) {
		for( MetadataEntry &entry: result->entries ) {
			// Link to parent
			entry.parent = result;
			// Link to hash bin by name
			wsw::link( std::addressof( entry ), &m_hashBins[entry.hashBinIndex] );
		}
		result->numRefs = (int)result->entries.size();
		wsw::link( result, &m_taskResultsHead );
	}

	m_taskResultsToProcess.clear();
	updateDefaultDisplayedList();
}

void DemosResolver::updateDefaultDisplayedList() {
	m_entries.clear();
	for( ResolveTaskResult *result = m_taskResultsHead; result; result = result->next ) {
		for( const MetadataEntry &entry: result->entries ) {
			m_entries.push_back( std::addressof( entry ) );
		}
	}

	// Sort the default list by date
	const auto cmp = []( const MetadataEntry *lhs, const MetadataEntry *rhs ) {
		return lhs->rawTimestamp < rhs->rawTimestamp;
	};
	wsw::sortPodNonSpeedCritical( m_entries.begin(), m_entries.end(), cmp );
}

void DemosResolver::runQuery() {
	if( m_lastQuery.empty() ) {
		// Just reset
		// This has been still required to force the model update
		Q_EMIT runQueryTasksReady( true );
		return;
	}

	const auto devMode = developer->integer;
	uint64_t startMicros = 0;
	if( devMode ) {
		// Doesn't matter that much as it's a background task but we're curious
		startMicros = Sys_Microseconds();
	}

	// TODO: Parse the query DSL
	runQueryUsingWordMatcher<WordsMatcher>( m_lastQuery.asView() );

	if( devMode ) {
		uiDebug() << "Running a query" << m_lastQuery << "took" << ( Sys_Microseconds() - startMicros ) << "micros";
	}

	Q_EMIT runQueryTasksReady( true );
}

struct MatchResultAccum {
	unsigned bestDistance { std::numeric_limits<unsigned>::max() };
	unsigned commonLength { 0 };

	[[nodiscard]]
	bool addOrInterrupt( const WordsMatcher::Match &match ) {
		if( !match.editDistance ) {
			return false;
		}
		if( match.editDistance < bestDistance ) {
			bestDistance = match.editDistance;
			commonLength = match.commonLength;
		}
		return true;
	}

	[[nodiscard]]
	bool hasResults() const { return bestDistance != std::numeric_limits<unsigned>::max(); }
};

// Was templated for different algorithms trial during development. Kept as-is for now.
template <typename WordMatcher>
void DemosResolver::runQueryUsingWordMatcher( const wsw::StringView &word ) {
	WordMatcher matcher( word );

	constexpr uint64_t mismatchShift = 32u;
	constexpr uint64_t prefixLenShift = 35u;
	constexpr uint64_t resultIndexMask = ( (uint64_t)1 << mismatchShift ) - 1;
	static_assert( resultIndexMask == 0xFFFF'FFFFu );

	unsigned maxDist = 0;
	if( word.length() > 8 ) {
		maxDist = 4;
	} else if( word.length() > 4 ) {
		maxDist = 3;
	} else if( word.length() > 2 ) {
		maxDist = 2;
	} else if( word.length() > 1 ) {
		maxDist = 1;
	}

	bool hadFuzzyMatches = false;
	assert( m_lastQueryResults.empty() );
	for( unsigned index = 0; index < m_entries.size(); ++index ) {
		const MetadataEntry *const entry = m_entries[index];
		MatchResultAccum resultAccum;

		// Test MetadataEntry fields. Interrupt early on an exact match.
		if( const auto maybeMatch = matcher.match( entry->getDemoName(), maxDist ) ) {
			if( !resultAccum.addOrInterrupt( *maybeMatch ) ) {
				m_lastQueryResults.push_back( index );
				continue;
			}
		}

		if( const auto maybeMatch = matcher.match( entry->getServerName(), maxDist ) ) {
			if( !resultAccum.addOrInterrupt( *maybeMatch ) ) {
				m_lastQueryResults.push_back( index );
				continue;
			}
		}

		if( const auto maybeMatch = matcher.match( entry->getMapName(), wsw::max( 1u, maxDist + 1 ) ) ) {
			if( !resultAccum.addOrInterrupt( *maybeMatch ) ) {
				m_lastQueryResults.push_back( index );
				continue;
			}
		}

		if( const auto maybeMatch = matcher.match( entry->getGametype(), wsw::max( 1u, maxDist + 1 ) ) ) {
			if( !resultAccum.addOrInterrupt( *maybeMatch ) ) {
				m_lastQueryResults.push_back( index );
				continue;
			}
		}

		if( !resultAccum.hasResults() ) {
			continue;
		}

		hadFuzzyMatches = true;

		constexpr uint64_t maxPrefixLen = 16u;
		constexpr uint64_t maxMismatch = 8u;
		static_assert( prefixLenShift > mismatchShift );
		static_assert( 1u << ( prefixLenShift - mismatchShift ) == maxMismatch );

		// Add penalty bits to the index
		// Prefix bits have a priority
		// Results with the same prefix (actually, prefix + postfix) length are sorted by an edit distance

		// Limit prefix len that is taken into account
		const unsigned commonLength = wsw::min( (unsigned)maxPrefixLen, resultAccum.commonLength );
		// A better (larger) prefix match has lesser penalty bits
		const uint64_t prefixPenaltyBits = (uint64_t)( maxPrefixLen - commonLength ) << prefixLenShift;

		// Make sure there's a sufficient room for a mismatch distance
		assert( resultAccum.bestDistance < maxMismatch );
		// A better (smaller) mismatch has smaller penalty bits
		const uint64_t mismatchPenaltyBits = (uint64_t)resultAccum.bestDistance << mismatchShift;

		// Make sure these bits don't overlap
		assert( !( mismatchPenaltyBits & prefixPenaltyBits ) );
		const uint64_t penaltyBits = mismatchPenaltyBits | prefixPenaltyBits;

		// Make sure these bits don't overlap with the base index
		assert( !( index & penaltyBits ) );
		m_lastQueryResults.push_back( (uint64_t)index | penaltyBits );
	}

	if( !hadFuzzyMatches ) {
		// Preserve the default ordering
		return;
	}

	// Sort entries so best matches (with smaller penalty bits) are first.
	// A default ordering of entries with the same penalty is preserved.
	std::stable_sort( m_lastQueryResults.begin(), m_lastQueryResults.end() );

	// Unmask indices
	for( uint64_t &index: m_lastQueryResults ) {
		index &= resultIndexMask;
		assert( index < m_entries.size() );
	}
}

void DemoPlayer::checkUpdates() {
	const int oldProgress = m_progress;
	m_progress = (int)( ::cls.demoPlayer.time / 1000 );
	if( oldProgress != m_progress ) {
		Q_EMIT progressChanged( m_progress );
	}

	const bool wasPlaying = m_isPlaying;
	m_isPlaying = ::cls.demoPlayer.playing;
	if( wasPlaying != m_isPlaying ) {
		Q_EMIT isPlayingChanged( m_isPlaying );
	}

	const bool wasPaused = m_isPaused;
	m_isPaused = ::cls.demoPlayer.playing && ::cls.demoPlayer.paused;
	if( wasPaused != m_isPaused ) {
		Q_EMIT isPausedChanged( m_isPaused );
	}

	if( m_isPlaying && wasPlaying != m_isPlaying ) {
		if( cls.demoPlayer.meta_data_realsize ) {
			reloadMetadata();
		}
	}
}

void DemoPlayer::reloadMetadata() {
	QString serverName, timestamp, mapName, gametype;
	int duration = 0;

	wsw::DemoMetadataReader reader( cls.demoPlayer.meta_data, cls.demoPlayer.meta_data_realsize );
	while( reader.hasNextPair() ) {
		if( const auto maybePair = reader.readNextPair() ) {
			const auto [key, value] = *maybePair;
			if( key.equalsIgnoreCase( kDemoKeyServerName ) ) {
				serverName = toStyledText( value );
			} else if( key.equalsIgnoreCase( kDemoKeyTimestamp ) ) {
				uint64_t timestampValue = wsw::toNum<uint64_t>( value ).value_or( 0 );
				timestamp = formatTimestamp( QDateTime::fromSecsSinceEpoch( timestampValue ) );
			} else if( key.equalsIgnoreCase( kDemoKeyDuration ) ) {
				duration = wsw::toNum<int>( value ).value_or( 0 );
			} else if( key.equalsIgnoreCase( kDemoKeyMapName ) ) {
				mapName = toStyledText( value );
			} else if( key.equalsIgnoreCase( kDemoKeyGametype ) ) {
				gametype = toStyledText( value );
			}
		} else {
			break;
		}
	}

	QString demoName = QString::fromLatin1( cls.demoPlayer.name );
	if( m_demoName != demoName ) {
		m_demoName = demoName;
		Q_EMIT demoNameChanged( m_demoName );
	}
	if( m_serverName != serverName ) {
		m_serverName = serverName;
		Q_EMIT serverNameChanged( serverName );
	}
	if( m_timestamp != timestamp ) {
		m_timestamp = timestamp;
		Q_EMIT timestampChanged( timestamp );
	}
	if( m_mapName != mapName ) {
		m_mapName = mapName;
		Q_EMIT mapNameChanged( mapName );
	}
	if( m_gametype != gametype ) {
		m_gametype = gametype;
		Q_EMIT gametypeChanged( gametype );
	}
	if( m_duration != duration ) {
		m_duration = duration;
		Q_EMIT durationChanged( duration );
	}
}

void DemoPlayer::play( const QByteArray &fileName ) {
	wsw::StringView demoName( fileName.data(), fileName.size() );
	if( demoName.startsWith( '/' ) ) {
		demoName = demoName.drop( 1 );
	}
	const wsw::StringView prefix( "demos/"_asView );
	if( demoName.startsWith( prefix ) ) {
		demoName = demoName.drop( prefix.length() );
	}
	wsw::StaticString<MAX_QPATH> buffer;
	buffer << "demo "_asView << demoName;
	CL_Cbuf_AppendCommand( buffer.data() );
}

void DemoPlayer::pause() {
	CL_Cbuf_AppendCommand( "demopause" );
}

void DemoPlayer::stop() {
	CL_Cbuf_AppendCommand( "disconnect" );
}

void DemoPlayer::seek( qreal frac ) {
	const int totalSeconds = wsw::clamp( (int)( std::round( m_duration * frac + 0.5 ) ), 0, m_duration );
	const auto [seconds, minutes] = std::div( totalSeconds, 60 );
	wsw::StaticString<32> buffer( "demojump %02d:%02d", (int)seconds, (int)minutes );
	CL_Cbuf_AppendCommand( buffer.data() );
}

auto DemoPlayer::formatDuration( int durationSeconds ) -> QByteArray {
	const auto [seconds, minutes] = std::div( std::abs( durationSeconds ), 60 );
	wsw::StaticString<16> buffer( "-%02d:%02d", (int)seconds, (int)minutes );
	if( durationSeconds >= 0 ) {
		return QByteArray( buffer.data() + 1, buffer.size() - 1 );
	}
	return QByteArray( buffer.data(), buffer.size() );
}

}