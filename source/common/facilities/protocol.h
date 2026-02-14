#ifndef WSW_250158dd_010e_46bf_a42e_4ec08585607d_H
#define WSW_250158dd_010e_46bf_a42e_4ec08585607d_H

#include <common/types/staticvector.h>
#include <common/types/stringview.h>
#include <common/helpers/exceptions.h>
#include <common/facilities/q_comref.h>

struct snapshot_s;

struct ginfo_s;
struct client_s;
struct cmodel_state_s;
struct client_entities_s;

#define PORT_INFO_SERVER    27950
#define PORT_SERVER         44400
#define PORT_HTTP_SERVER    44444

// serverdata flags
#define SV_BITFLAGS_PURE            ( 1 << 0 )
#define SV_BITFLAGS_RELIABLE        ( 1 << 1 )
#define SV_BITFLAGS_HTTP            ( 1 << 3 )
#define SV_BITFLAGS_HTTP_BASEURL    ( 1 << 4 )

// framesnap flags
#define FRAMESNAP_FLAG_DELTA        ( 1 << 0 )
#define FRAMESNAP_FLAG_ALLENTITIES  ( 1 << 1 )
#define FRAMESNAP_FLAG_MULTIPOV     ( 1 << 2 )

//=========================================

#define UPDATE_BACKUP   32  // copies of entity_state_t to keep buffered
// must be power of two

#define UPDATE_MASK ( UPDATE_BACKUP - 1 )

#define SNAP_MAX_DEMO_META_DATA_SIZE    16 * 1024

// define this 0 to disable compression of demo files
#define SNAP_DEMO_GZ                    FS_GZ

void SNAP_ParseBaseline( struct msg_s *msg, entity_state_t *baselines );
void SNAP_SkipFrame( struct msg_s *msg, struct snapshot_s *header );
struct snapshot_s *SNAP_ParseFrame( struct msg_s *msg, struct snapshot_s *lastFrame, struct snapshot_s *backup, entity_state_t *baselines, int showNet );

void SNAP_WriteFrameSnapToClient( const struct ginfo_s *gi, struct client_s *client, struct msg_s *msg,
								  int64_t frameNum, int64_t gameTime,
								  const entity_state_t *baselines, const struct client_entities_s *client_entities,
								  int numcmds, const gcommand_t *commands, const char *commandsData );

void SNAP_BuildClientFrameSnap( struct cmodel_state_s *cms, struct ginfo_s *gi, int64_t frameNum, int64_t timeStamp,
								const float *skyPortalPovOrigin, struct client_s *client, const game_state_t *gameState,
								const ReplicatedScoreboardData *scoreboardData, struct client_entities_s *client_entities );

void SNAP_FreeClientFrames( struct client_s *client );

void SNAP_RecordDemoMessage( int demofile, struct msg_s *msg, int offset );
int SNAP_ReadDemoMessage( int demofile, struct msg_s *msg );

namespace wsw { class ConfigStringStorage; }

void SNAP_BeginDemoRecording( int demofile, unsigned int spawncount, unsigned int snapFrameTime,
							  const char *sv_name, unsigned int sv_bitflags, struct purelist_s *purelist,
							  const wsw::ConfigStringStorage &configStrings, entity_state_t *baselines );

void SNAP_StopDemoRecording( int demofile );
void SNAP_WriteDemoMetaData( const char *filename, const char *meta_data, size_t meta_data_realsize );
size_t SNAP_ClearDemoMeta( char *meta_data, size_t meta_data_max_size );
size_t SNAP_ReadDemoMetaData( int demofile, char *meta_data, size_t meta_data_size );

//
// server to client
//
enum svc_ops_e {
	svc_bad,

	// the rest are private to the client and server
	svc_nop,
	svc_servercmd,          // [string] string
	svc_serverdata,         // [int] protocol ...
	svc_spawnbaseline,
	svc_download,           // [short] size [size bytes]
	svc_playerinfo,         // variable
	svc_packetentities,     // [...]
	svc_gamecommands,
	svc_match,
	svc_scoreboard,
	svc_clcack,
	svc_servercs,           //tmp jalfixme : send reliable commands as unreliable
	svc_frame,
	svc_demoinfo,
	svc_extension           // for future expansion
};

//==============================================

//
// client to server
//
enum clc_ops_e {
	clc_bad,
	clc_nop,
	clc_move,               // [[usercmd_t]
	clc_svcack,
	clc_clientcommand,      // [string] message
	clc_extension
};


namespace wsw {

// These keys are mandatory.
// We could have settled on fixed binary offsets instead of keys but an extensible k/v set is a better approach.
const wsw::StringView kDemoKeyServerName( "ServerName"_asView );
const wsw::StringView kDemoKeyTimestamp( "Timestamp"_asView );
const wsw::StringView kDemoKeyDuration( "Duration"_asView );
const wsw::StringView kDemoKeyMapName( "MapName"_asView );
const wsw::StringView kDemoKeyMapChecksum( "MapChecksum"_asView );
const wsw::StringView kDemoKeyGametype( "Gametype"_asView );

const wsw::StringView kDemoTagSinglePov( "SinglePOV" );
const wsw::StringView kDemoTagMultiPov( "MultiPOV" );

class DemoMetadataWriter {
	char *const m_basePtr;
	unsigned m_writeOff { 0 };
	unsigned m_numPairsWritten { 0 };
	unsigned m_numTagsWritten { 0 };
	bool m_incomplete { false };

	[[nodiscard]]
	auto freeBytesLeft() const -> unsigned { return SNAP_MAX_DEMO_META_DATA_SIZE - m_writeOff; }

	[[nodiscard]]
	bool tryAppendingTag( const wsw::StringView &tag ) {
		assert( m_numPairsWritten );
		if( tag.length() + 1 <= freeBytesLeft() ) {
			tag.copyTo( m_basePtr + m_writeOff, freeBytesLeft() );
			assert( m_basePtr[m_writeOff + tag.length()] == '\0' );
			m_writeOff += tag.length() + 1;
			return true;
		}
		return false;
	}

	[[nodiscard]]
	bool tryAppendingPair( const wsw::StringView &key, const wsw::StringView &value ) {
		assert( !m_numTagsWritten );
		if( key.length() + value.length() + 2 <= freeBytesLeft() ) {
			key.copyTo( m_basePtr + m_writeOff, freeBytesLeft() );
			assert( m_basePtr[m_writeOff + key.length()] == '\0' );
			m_writeOff += key.length() + 1;

			value.copyTo( m_basePtr + m_writeOff, freeBytesLeft() );
			assert( m_basePtr[m_writeOff + value.length()] == '\0' );
			m_writeOff += value.length() + 1;

			return true;
		}
		return false;
	}
public:
	explicit DemoMetadataWriter( char *data ) : m_basePtr( data ) {
		std::memset( data, 0, SNAP_MAX_DEMO_META_DATA_SIZE );
		m_writeOff += 8;
	}

	[[maybe_unused]]
	bool writePair( const wsw::StringView &key, const wsw::StringView &value ) {
		if( !m_incomplete ) {
			if( m_numTagsWritten ) {
				wsw::failWithLogicError( "Attempt to write a key-value pair after tags" );
			}
			if( tryAppendingPair( key, value ) ) {
				m_numPairsWritten++;
				return true;
			}
			m_incomplete = true;
		}
		return false;
	}

	[[maybe_unused]]
	bool writeTag( const wsw::StringView &tag ) {
		if( !m_incomplete ) {
			if( !m_numPairsWritten ) {
				wsw::failWithLogicError( "Attempt to write a tag prior to key-value pairs" );
			}
			if( tryAppendingTag( tag ) ) {
				m_numTagsWritten++;
				return true;
			}
			m_incomplete = true;
		}
		return false;
	}

	[[nodiscard]]
	auto markCurrentResult() const -> std::pair<size_t, bool> {
		uint32_t numPairsToMark = LittleLong( m_numPairsWritten );
		uint32_t numTagsToMark = LittleLong( m_numTagsWritten );
		std::memcpy( m_basePtr + 0, &numPairsToMark, 4 );
		std::memcpy( m_basePtr + 4, &numTagsToMark, 4 );
		return { m_writeOff, !m_incomplete };
	}
};

class DemoMetadataReader {
	const char *const m_basePtr;
	const unsigned m_dataSize;
	unsigned m_readOff { 0 };
	unsigned m_numPairs { 0 };
	unsigned m_numTags { 0 };
	unsigned m_numPairsRead { 0 };
	unsigned m_numTagsRead { 0 };
public:
	DemoMetadataReader( const char *data, unsigned dataSize )
		: m_basePtr( data ), m_dataSize( dataSize ) {
		assert( m_basePtr[dataSize] == '\0' );
		if( dataSize >= 8 ) {
			unsigned numPairs, numTags;
			std::memcpy( &numPairs, data + 0, 4 );
			std::memcpy( &numTags, data + 4, 4 );
			m_numPairs = LittleLong( numPairs );
			m_numTags = LittleLong( numTags );
			m_readOff += 8;
		}
	}

	[[nodiscard]]
	bool hasNextPair() const { return m_numPairsRead < m_numPairs; }

	[[nodiscard]]
	auto readNextPair() -> std::optional<std::pair<wsw::StringView, wsw::StringView>> {
		if( !hasNextPair() ) {
			wsw::failWithLogicError( "No key-value pairs to read" );
		}
		const auto keyLen = std::strlen( m_basePtr + m_readOff );
		if( m_readOff + keyLen + 1 < m_dataSize ) {
			assert( m_basePtr[m_readOff + keyLen] == '\0' );
			const auto valueLen = std::strlen( m_basePtr + m_readOff + keyLen + 1 );
			if( m_readOff + keyLen + valueLen + 2 <= m_dataSize ) {
				assert( m_basePtr[m_readOff + keyLen + valueLen + 1] == '\0' );
				wsw::StringView key( m_basePtr + m_readOff, keyLen, wsw::StringView::ZeroTerminated );
				wsw::StringView value( m_basePtr + m_readOff + keyLen + 1, valueLen, wsw::StringView::ZeroTerminated );
				m_readOff += keyLen + valueLen + 2;
				m_numPairsRead++;
				return std::make_pair( key, value );
			}
		}
		return std::nullopt;
	}

	[[nodiscard]]
	bool hasNextTag() const { return m_numTagsRead < m_numTags; }

	[[nodiscard]]
	auto readNextTag() -> std::optional<wsw::StringView> {
		if( hasNextPair() ) {
			wsw::failWithLogicError( "Reading of key-value pairs is incomplete" );
		}
		if( !hasNextTag() ) {
			wsw::failWithLogicError( "No tags to read" );
		}
		const auto tagLen = std::strlen( m_basePtr + m_readOff );
		if( m_readOff + tagLen < m_dataSize ) {
			assert( m_basePtr[m_readOff + tagLen] == '\0' );
			wsw::StringView tag( m_basePtr + m_readOff, tagLen, wsw::StringView::ZeroTerminated );
			m_readOff += tagLen + 1;
			m_numTagsRead++;
			return tag;
		}
		return std::nullopt;
	}
};

}

constexpr const float kSoundAttenuationMaxDistance = 8000.0f;
constexpr const float kSoundAttenuationRefDistance = 250.0f;

[[nodiscard]]
auto calcSoundGainForDistanceAndAttenuation( float distance, float attenuation ) -> float;

#endif