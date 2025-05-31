#ifndef WSW_b5e304ab_2fbb_4fda_9279_cea9a676291f_H
#define WSW_b5e304ab_2fbb_4fda_9279_cea9a676291f_H

#include "ailocal.h"

class AiPrecomputedFileHandler {
public:
	typedef void *( *AllocFn )( size_t  );
	typedef void ( *FreeFn )( void * );
protected:
	const char *tag;
	AllocFn allocFn;
	FreeFn freeFn;
	uint8_t *data;

	uint32_t expectedVersion;
	int fp;
	int dataSize;

	bool useAasChecksum;
	bool useMapChecksum;

public:
	AiPrecomputedFileHandler( const char *tag_, uint32_t expectedVersion_,
						  AllocFn allocFn_ = nullptr, FreeFn freeFn_ = nullptr )
		: tag( tag_ ),
		  allocFn( allocFn_ ),
		  freeFn( freeFn_ ),
		  data( nullptr ),
		  expectedVersion( expectedVersion_ ),
		  fp( -1 ),
		  dataSize( 0 ),
		  useAasChecksum( true ),
		  useMapChecksum( true ) {}

	virtual ~AiPrecomputedFileHandler();
};

class AiPrecomputedFileReader: public virtual AiPrecomputedFileHandler {
public:
	enum LoadingStatus {
		MISSING,
		FAILURE,
		VERSION_MISMATCH,
		SUCCESS
	};
private:
	LoadingStatus ExpectFileString( const char *expected, const char *message ) {
		return ExpectFileString( wsw::StringView( expected ), message );
	}
	LoadingStatus ExpectFileString( const wsw::StringView &expected, const char *message );
public:
	AiPrecomputedFileReader( const char *tag_, uint32_t expectedVersion_, AllocFn allocFn_ = nullptr, FreeFn freeFn_ = nullptr )
		: AiPrecomputedFileHandler( tag_, expectedVersion_, allocFn_, freeFn_ ) {}

	LoadingStatus BeginReading( const char *filePath );

	bool ReadLengthAndData( uint8_t **data, uint32_t *dataLength );

	template <typename T>
	bool ReadAsTypedBuffer( T **data, uint32_t *dataLength ) {
		uint8_t *rawData;
		uint32_t rawLength;
		if( !ReadLengthAndData( &rawData, &rawLength ) ) {
			return false;
		}
		if( rawLength % sizeof( T ) ) {
			Q_free( data );
			return false;
		}
		*data       = (T *)rawData;
		*dataLength = rawLength / sizeof( T );
		return true;
	}
};

class AiPrecomputedFileWriter: public virtual AiPrecomputedFileHandler {
	char *filePath;
	bool failedOnWrite;
public:
	AiPrecomputedFileWriter( const char *tag_, uint32_t expectedVersion_, AllocFn allocFn_ = nullptr, FreeFn freeFn_ = nullptr )
		: AiPrecomputedFileHandler( tag_, expectedVersion_, allocFn_, freeFn_ ),
		  filePath( nullptr ),
		  failedOnWrite( false ) {}

	~AiPrecomputedFileWriter() override;

	bool BeginWriting( const char *filePath_ );

	bool WriteString( const char *string );
	bool WriteString( const wsw::StringView &string );
	bool WriteLengthAndData( const uint8_t *data, uint32_t dataLength );

	template <typename T>
	bool WriteTypedBuffer( const T *data, uint32_t dataLength ) {
		return WriteLengthAndData( (const uint8_t *)data, sizeof( T ) * dataLength );
	}
};

#endif
