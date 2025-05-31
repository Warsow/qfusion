#ifndef QFUSION_SND_CACHED_COMPUTATION_H
#define QFUSION_SND_CACHED_COMPUTATION_H


#include "snd_local.h"

class CachedComputation {
	friend class CachedComputationIOHelper;

	const char *const logTag;
	const char *const fileExtension;
	const char *const fileVersion;
	char mapName[MAX_QPATH];
	char mapHash[MAX_CONFIGSTRING_CHARS];
	mutable int numLeafs { -1 };
	/**
	 * If a computation failure is not acceptable but manages to happen from time to time,
	 * we have to provide some dummy data for accessors.
	 */
	bool isUsingValidData { false };
protected:
	/**
	 * Any resources that belong to another map should be released in this call.
	 * @note map properties (name, hash, number of leaves) are already set to actual ones to the moment of this call.
	 */
	virtual void ResetExistingState() = 0;
	/**
	 * This call should make an attempt to read a serialized computation data from the filesystem.
	 * Two attempts are made:
	 * <ul>
	 * <li> the first attempt is intended to look for a data in the game filesystem
	 * (the data that is is intended to be supplied along with default maps)
	 * <li> the second attempt is intended to look for a data in the local cache
	 * (the data that is computed on a consumer machine for custom maps)
	 * </ul>
	 * @param fsFlags flags that are passed to {@code trap_FS_Open()} call and define a search location for an attempt.
	 * @return true if the data has been found and loaded from the specified search location.
	 * @note map properties (name, hash, number of leaves) are already set to actual ones to the moment of this call.
	 */
	virtual bool TryReadFromFile( int fsFlags ) = 0;
	/**
	 * This call should compute an actual data for the map.
	 * @param fastAndCoarse should be true for computations on a consumer machine.
	 * Actual implementation algorithms should take this flag into account
	 * and use faster/coarse methods of computation to prevent blocking of user application.
	 * The true value of this flag is for fine precomputation of data for supplying withing default game distribution.
	 * @return true if the data has been computed successfully.
	 * @note map properties (name, hash, number of leaves) are already set to actual ones to the moment of this call.
	 */
	virtual bool ComputeNewState( bool fastAndCoarse ) = 0;
	/**
	 * This call should try saving the computation data.
	 * It is intended to perform saving to the filesystem cache (using {@code FS_CACHE} flag).
	 * @return true if the results have been saved to the filesystem cache successfully.
	 * @note map properties (name, hash, number of leaves) are already set to actual ones to the moment of this call.
	 */
	virtual bool SaveToCache() = 0;
	/**
	 * This is a hook that is called when {@code EnsureValid()} has already reset existing state
	 * and is about to return regardless of actual computation status.
	 */
	virtual void CommitUpdate() {}

	/**
	 * It is intended to be overridden in descendants.
	 * The default implementation just triggers a fatal error if a real data computation has failed.
	 */
	virtual void ProvideDummyData() {
		Com_Error( ERR_FATAL, "Providing a dummy data is unsupported for this descendant of CachedComputation" );
	}

	virtual void NotifyOfBeingAboutToCompute();
	virtual void NotifyOfComputationSuccess();
	virtual void NotifyOfComputationFailure();
	virtual void NotifyOfSerializationSuccess();
	virtual void NotifyOfSerializationFailure();

	int NumLeafs() const { return numLeafs; };

public:
	explicit CachedComputation( const char *logTag_, const char *fileExtension_, const char *fileVersion_ )
		: logTag( logTag_), fileExtension( fileExtension_ ), fileVersion( fileVersion_ ) {
		assert( *fileExtension_ == '.' );
		mapName[0] = '\0';
		mapHash[0] = '\0';
	}

	virtual ~CachedComputation() = default;

	void EnsureValid();

	/**
	 * Allows checking whether a dummy data is provided for accessors.
	 */
	bool IsUsingValidData() const { return isUsingValidData; }
};

class CachedComputationIOHelper {
protected:
	const CachedComputation *const parent_;
	char fileName[MAX_STRING_CHARS];
	int fd { -1 };
	int fsResult;

	const char *MapName() const { return parent_->mapName; }
	const char *MapHash() const { return parent_->mapHash; }
	const char *Extension() const { return parent_->fileExtension; }
	const char *Version() const { return parent_->fileVersion; }
public:
	CachedComputationIOHelper( const CachedComputation *parent_, int fileFlags )
		: parent_( parent_ ) {
		Q_snprintfz( fileName, sizeof( fileName ), "sounds/%s", MapName() );
		COM_StripExtension( fileName );
		assert( *Extension() == '.' );
		Q_strncatz( fileName, Extension(), sizeof( fileName ) );
		fsResult = FS_FOpenFile( fileName, &fd, fileFlags );
	}

	virtual ~CachedComputationIOHelper() {
		if( fd >= 0 ) {
			FS_FCloseFile( fd );
		}
	}
};

class CachedComputationReader: public CachedComputationIOHelper {
protected:
	char *fileData { nullptr };
	char *dataPtr { nullptr };
	int fileSize { -1 };

	void SkipWhiteSpace() {
		size_t skippedLen = strspn( dataPtr, "\t \r\n");
		dataPtr += skippedLen;
	}

	bool ExpectString( const char *string );

	bool ReadInt32( int32_t *result ) {
		if( Read( result, 4 ) ) {
			*result = LittleLong( *result );
			return true;
		}
		return false;
	}

	bool Read( void *data, size_t size );

	size_t BytesLeft() {
		if( dataPtr <= fileData + fileSize ) {
			return (size_t)( ( fileData + fileSize ) - dataPtr );
		}
		return 0;
	}
public:
	CachedComputationReader( const CachedComputation *parent_, int fileFlags, bool textMode = false );

	~CachedComputationReader() override {
		if( fileData ) {
			Q_free( fileData );
		}
	}
};

class CachedComputationWriter: public CachedComputationIOHelper {
protected:
	// Useful for debugging
	size_t bytesWritten { 0 };

	bool WriteString( const char *string );

	bool WriteInt32( int32_t value ) {
#if !defined( ENDIAN_LITTLE ) || defined( ENDIAN_BIG )
#error The data is assumed to be write as-is and is expected to be in little-endian byte order
#endif
		return Write( &value, 4 );
	}

	bool Write( const void *data, size_t size );
public:
	explicit CachedComputationWriter( const CachedComputation *parent_ );
};

#endif
