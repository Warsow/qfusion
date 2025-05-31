#ifndef QFUSION_SND_LEAF_PROPS_CACHE_H
#define QFUSION_SND_LEAF_PROPS_CACHE_H

#include "snd_cached_computation.h"

class LeafPropsSampler;
class LeafPropsReader;

struct alignas( 4 )LeafProps {
	uint8_t m_roomSizeFactor;
	uint8_t m_skyFactor;
	uint8_t m_smoothnessFactor;
	uint8_t m_metalnessFactor;

	static float PackValue( float value ) { return (uint8_t)( value * 255 ); }
	static float UnpackValue( uint8_t packed ) { return packed / 255.0f; }

#define MK_ACCESSORS( accessorName, fieldName )                                             \
	float get##accessorName() const { return UnpackValue( m_##fieldName ); }                \
	void set##accessorName( float fieldName ) { m_##fieldName = PackValue( fieldName ); }

	MK_ACCESSORS( RoomSizeFactor, roomSizeFactor );
	MK_ACCESSORS( SkyFactor, skyFactor );
	MK_ACCESSORS( SmoothnessFactor, smoothnessFactor );
	MK_ACCESSORS( MetallnessFactor, metalnessFactor );

#undef MK_ACCESSORS
};

class LeafPropsCache: public CachedComputation {
	template <typename> friend class SingletonHolder;

	LeafProps *leafProps { nullptr };
private:

	bool TryReadFromFile( LeafPropsReader *reader );

	void ResetExistingState() override;
	bool TryReadFromFile( int fsFlags ) override;
	bool ComputeNewState( bool fastAndCoarse ) override;
	bool SaveToCache() override;

	LeafPropsCache(): CachedComputation( "LeafPropsCache", ".leafprops", "LeafProps@v1344" ) {}
public:
	static LeafPropsCache *Instance();
	static void Init();
	static void Shutdown();

	~LeafPropsCache() override {
		if( leafProps ) {
			Q_free( leafProps );
		}
	}

	// TODO: Merge with GetPresetForLeaf() and use Either as a return type
	const LeafProps &GetPropsForLeaf( int leafNum ) const {
		return leafProps[leafNum];
	}
};

#endif
