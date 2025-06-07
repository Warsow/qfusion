#ifndef WSW_697bc4c8_d217_476d_9a62_2cebe0a8b142_H
#define WSW_697bc4c8_d217_476d_9a62_2cebe0a8b142_H

#include <tuple>
#include "ailocal.h"
#include <common/types/staticvector.h>
#include <common/helpers/hash.h>

void GT_asRegisterScriptWeightConfig( class AiWeightConfig *weightConfig, const edict_t *configOwner );
void GT_asReleaseScriptWeightConfig( class AiWeightConfig *weightConfig, const edict_t *configOwner );
void GENERIC_asGetScriptWeightConfigVarValueProps( const void *scriptObject, float *value, float *minValue, float *maxValue, float *defaultValue );
void GENERIC_asSetScriptWeightConfigVarValue( void *scriptObject, float value );

class AiWeightConfigVar;
class AiScriptWeightConfigVar;
class AiScriptWeightConfigVarGroup;

// This accepts the exact type as as template parameter to make accessors operate on the exact child type.
template <typename Child>
class AiWeightConfigVarGroupChild {
	friend class AiWeightConfigVarGroup;
	friend class AiWeightConfigVar;
protected:
	const char *name;
	Child *nextSibling { nullptr };
	Child *nextInHashBin { nullptr };
	uint32_t nameHash;
	uint32_t nameLength : 31;
	bool isTouched : 1;

	explicit AiWeightConfigVarGroupChild( const char *name_ ) : name( name_ ) {
		const auto [hash, length] = wsw::getHashAndLength( name_ );
		this->nameHash = hash;
		this->nameLength = length;
		this->isTouched = false;
	}

public:
	// Exposed for script interface
	const char *Name() const { return name; }
	Child *Next() { return nextSibling; }
	const Child *Next() const { return nextSibling; }
	unsigned NameHash() const { return nameHash; }
	unsigned NameLength() const { return nameLength; }
};

class AiWeightConfigVarGroup : public AiWeightConfigVarGroupChild<AiWeightConfigVarGroup> {
	friend class AiWeightConfigVar;

	template <typename T>
	void LinkItem( T *item, T **linkedItemsHead, T ***hashBins, unsigned *numItems );

	template <typename T>
	void AddItemToHashBin( T *item, T ***hashBins );

	template <typename T>
	T *GetItemByName( const char *name_, unsigned nameHash_, T *linkedItemsHead, T **hashBins, unsigned numItems );

	template <typename T>
	T *GetItemByPath( const char *path, T * ( AiWeightConfigVarGroup::*getByNameMethod )( const char *, unsigned ) );

	template <typename T>
	void AddScriptItem( const char *name_, void *scriptObject, T **allocatedItemsHead );

protected:
	AiWeightConfigVarGroup *childGroupsHead { nullptr };
	AiWeightConfigVar *childVarsHead { nullptr };

	// If there are few child items, hash bins are not allocated and a corresponding list should be used for searching.
	// We use a hash and not a trie because its a natural arppoach for already linked node-like items.
	AiWeightConfigVarGroup **groupsHashBins { nullptr };
	AiWeightConfigVar **varsHashBins { nullptr };

	// Vars and groups defined and used in native code are intended
	// to be declared as members of this class subtypes, so no extra memory management is nessessary.
	// Vars and groups that correspond to their script counterparts are added dynamically by gametype scripts.
	// Thus we have to allocate these vars via G_Malloc() and link to these lists for further freeing via G_Free().
	// Do not bother about performance because this is done only
	// when bot enters the game and sane count of script groups/vars is low.
	AiScriptWeightConfigVarGroup *allocatedGroupsHead { nullptr };
	AiScriptWeightConfigVar *allocatedVarsHead { nullptr };

	// 4 bytes are likely to be lost due to alignment of consequent instances of this class
	unsigned numChildVars { 0 };
	unsigned numChildGroups { 0 };

	// This should be enough for any sane configs.
	static constexpr const auto NUM_HASH_BINS = 59;
	static constexpr const auto MIN_HASH_ITEMS = 4;

	void LinkGroup( AiWeightConfigVarGroup *childGroup );
	void LinkVar( AiWeightConfigVar *childVar );

	bool Parse( const char *data, const char **restOfTheData );
	int ParseNextEntry( const char *data, const char **nextData );
	bool Write( int fileHandle, int depth ) const;

public:
	inline AiWeightConfigVarGroup( AiWeightConfigVarGroup *parent, const char *name_ )
		: AiWeightConfigVarGroupChild( name_ ) {
		if( parent ) {
			parent->LinkGroup( this );
		}
	}

	virtual ~AiWeightConfigVarGroup();

	// Exposed for script interface.
	AiWeightConfigVarGroup *GetGroupByName( const char *name_, unsigned nameHash_ = 0 );
	AiWeightConfigVar *GetVarByName( const char *name_, unsigned nameHash_ = 0 );

	AiWeightConfigVarGroup *GroupsListHead() { return childGroupsHead; }
	const AiWeightConfigVarGroup *GroupsListHead() const { return childGroupsHead; }
	AiWeightConfigVar *VarsListHead() { return childVarsHead; }
	const AiWeightConfigVar *VarsListHead() const { return childVarsHead; }

	// Note: groups and vars do not share namespace
	virtual AiWeightConfigVarGroup *GetGroupByPath( const char *path );
	virtual AiWeightConfigVar *GetVarByPath( const char *path );

	void AddScriptVar( const char *name_, void *scriptObject );
	void AddScriptGroup( const char *name_, void *scriptObject );

	void ResetToDefaultValues();

	void CheckTouched( const char *parentName = nullptr );
	void Touch( const char *parentName = nullptr );

	void CopyValues( const AiWeightConfigVarGroup &that );

	bool operator==( const AiWeightConfigVarGroup &that ) const;
	bool operator!=( const AiWeightConfigVarGroup &that ) const { return !( *this == that ); }
};

inline char *G_Strdup( const char *str ) {
	auto len = strlen( str );
	char *mem = (char *)Q_malloc( len + 1 );
	memcpy( mem, str, len + 1 );
	return mem;
}

class AiNativeWeightConfigVarGroup : public AiWeightConfigVarGroup {
public:
	AiNativeWeightConfigVarGroup( AiWeightConfigVarGroup *parent, const char *name_ )
		: AiWeightConfigVarGroup( parent, name_ ) {}
};

class AiScriptWeightConfigVarGroup : public AiWeightConfigVarGroup {
	friend class AiWeightConfigVarGroup;
	void *scriptObject;
	AiScriptWeightConfigVarGroup *nextAllocated { nullptr };
public:
	AiScriptWeightConfigVarGroup( AiWeightConfigVarGroup *parent, const char *name_, void *scriptObject_ )
		: AiWeightConfigVarGroup( parent, G_Strdup( name_ ) ), scriptObject( scriptObject_ ) {
		(void)( scriptObject );
	}

	~AiScriptWeightConfigVarGroup() override {
		Q_free( const_cast<char *>( name ) );
	}

	AiScriptWeightConfigVarGroup( const AiScriptWeightConfigVarGroup &that ) = delete;
	const AiScriptWeightConfigVarGroup &operator=( const AiScriptWeightConfigVarGroup &that ) = delete;
	AiScriptWeightConfigVarGroup( AiScriptWeightConfigVarGroup &&that ) = delete;
	AiScriptWeightConfigVarGroup &operator=( AiScriptWeightConfigVarGroup &&that ) = delete;
};

class AiWeightConfigVar : public AiWeightConfigVarGroupChild<AiWeightConfigVar> {
	friend class AiWeightConfigVarGroup;
public:
	AiWeightConfigVar( AiWeightConfigVarGroup *parent, const char *name_ )
		: AiWeightConfigVarGroupChild( name_ ) {
		if( parent ) {
			parent->LinkVar( this );
		}
	}

	virtual ~AiWeightConfigVar() {}

	virtual void GetValueProps( float *value_, float *minValue_, float *maxValue_, float *defaultValue_ ) const = 0;
	virtual void SetValue( float value_ ) = 0;

	void ResetToDefaultValues() {
		float value, minValue, maxValue, defaultValue;
		GetValueProps( &value, &minValue, &maxValue, &defaultValue );
		SetValue( defaultValue );
		isTouched = false;
	}

	void Touch( const char *parentName = nullptr ) {
		if( !isTouched ) {
			isTouched = true;
			return;
		}
		// TODO: Show full name somehow?
		if( parentName ) {
			G_Printf( S_COLOR_YELLOW "WARNING: var %s in group %s is already touched\n", name, parentName );
		} else {
			G_Printf( S_COLOR_YELLOW "WARNING: var %s has been already touched\n", name );
		}
	}

	void CheckTouched( const char *parentName = nullptr ) {
		if( isTouched ) {
			isTouched = false;
			return;
		}
		if( parentName ) {
			G_Printf( S_COLOR_YELLOW "WARNING: var %s in group %s has not been touched\n", name, parentName );
		} else {
			G_Printf( S_COLOR_YELLOW "WARNING: var %s has not been touched\n", name );
		}
	}

	bool operator==( const AiWeightConfigVar &that ) const {
		float thisValue, thatValue;
		float dummy[3];
		this->GetValueProps( &thisValue, dummy + 0, dummy + 1, dummy + 2 );
		that.GetValueProps( &thatValue, dummy + 0, dummy + 1, dummy + 2 );
		return std::fabs( (double)thisValue - (double)thatValue ) < 0.000001;
	}

	bool operator!=( const AiWeightConfigVar &that ) const { return !( *this == that ); }
};

// We do not want to even mention script weight config vars in the native code.
// The only operation needed is to set a var value, and this can be done by name.
class AiNativeWeightConfigVar : AiWeightConfigVar {
	float minValue;
	float maxValue;
	float defaultValue;
	float value;
public:
	AiNativeWeightConfigVar( AiWeightConfigVarGroup *parent,
							 const char *name_,
							 float minValue_,
							 float maxValue_,
							 float defaultValue_ )
		: AiWeightConfigVar( parent, name_ )
		, minValue( minValue_ )
		, maxValue( maxValue_ )
		, defaultValue( defaultValue_ )
		, value( defaultValue_ ) {
#ifdef _DEBUG
		if( minValue >= maxValue ) {
			AI_FailWith( "AiNativeWeightConfigVar()", "%s: minValue %f >= maxValue %f\n", name, minValue, maxValue );
		}
		if( defaultValue < minValue ) {
			AI_FailWith( "AiNativeWeightConfigVar()", "%s: defaultValue %f < minValue %f\n", name, defaultValue, minValue );
		}
		if( defaultValue > maxValue ) {
			AI_FailWith( "AiNativeWeightConfigVar()", "%s: defaultValue %f > maxValue %f\n", name, defaultValue, maxValue );
		}
#endif
	}

	operator float() const { return value; }
	float MinValue() const { return minValue; }
	float MaxValue() const { return maxValue; }
	float DefaultValue() const { return defaultValue; }

	void GetValueProps( float *value_, float *minValue_, float *maxValue_, float *defaultValue_ ) const override {
		*value_ = this->value;
		*minValue_ = this->minValue;
		*maxValue_ = this->maxValue;
		*defaultValue_ = this->defaultValue;
	}

	void SetValue( float value_ ) override {
		this->value = value_;
	}
};

class AiScriptWeightConfigVar : public AiWeightConfigVar {
	friend class AiWeightConfigVarGroup;
	void *scriptObject;
	AiScriptWeightConfigVar *nextAllocated { nullptr };
public:
	AiScriptWeightConfigVar( AiWeightConfigVarGroup *parent, const char *name_, void *scriptObject_ )
		: AiWeightConfigVar( parent, G_Strdup( name_ ) ), scriptObject( scriptObject_ ) {}

	~AiScriptWeightConfigVar() override {
		Q_free( const_cast<char *>( name ) );
	}

	AiScriptWeightConfigVar( const AiScriptWeightConfigVar &that ) = delete;
	AiScriptWeightConfigVar &operator=( const AiScriptWeightConfigVar &that ) = delete;
	AiScriptWeightConfigVar( AiScriptWeightConfigVar &&that ) = delete;
	AiScriptWeightConfigVar &operator=( AiScriptWeightConfigVar &&that ) = delete;

	void GetValueProps( float *value, float *minValue, float *maxValue, float *defaultValue ) const override {
		GENERIC_asGetScriptWeightConfigVarValueProps( scriptObject, value, minValue, maxValue, defaultValue );
	}
	void SetValue( float value ) override {
		GENERIC_asSetScriptWeightConfigVarValue( scriptObject, value );
	}
};

class AiWeightConfig : protected AiWeightConfigVarGroup {
	const edict_t *owner;
	bool isRegisteredInScript;

	bool LoadFromData( const char *data );

	inline const char *SkipRootInPath( const char *path ) const;
protected:
	const AiWeightConfigVarGroup *Root() const { return this; }
	AiWeightConfigVarGroup *Root() { return this; }

	// Must be called in child constructor after all child native objects have been constructed
	void RegisterInScript() {
		GT_asRegisterScriptWeightConfig( this, owner );
		isRegisteredInScript = true;
	}

	AiWeightConfigVarGroup *GetGroupByPath( const char *path ) override;
	AiWeightConfigVar *GetVarByPath( const char *path ) override;
public:
	explicit AiWeightConfig( const edict_t *owner_ )
		: AiWeightConfigVarGroup( nullptr, "Weights" ),
		owner( owner_ ),
		isRegisteredInScript( false ) {}

	~AiWeightConfig() {
		if( isRegisteredInScript ) {
			GT_asReleaseScriptWeightConfig( this, owner );
		}
	}

	bool Load( const char *filename );
	bool Save( const char *filename );

	bool Save( int fileHandle ) {
		return AiWeightConfigVarGroup::Write( fileHandle, 0 );
	}

	using AiWeightConfigVarGroup::ResetToDefaultValues;

	// We have to do these wrappers since AiBaseWeightConfigVarGroup is not a public type of `this` (and `that`)
	void CopyValues( const AiWeightConfig &that ) {
		AiWeightConfigVarGroup::CopyValues( that );
	}

	bool operator==( const AiWeightConfig &that ) { return AiWeightConfigVarGroup::operator==( that ); }
	bool operator!=( const AiWeightConfig &that ) { return AiWeightConfigVarGroup::operator!=( that ); }
};

// If we have two linked chains of items that are supposed
// to have the same length and same corresponding item names and to be iterated and checked in parallel,
// this class provides a reusable way to do it
template <typename T1, typename T2>
class ZippedItemChainsIterator {
	T1 first;
	T2 second;
	const char *tag;

	inline void CheckMatch() {
#ifdef _DEBUG
		if( first ) {
			if( !second ) {
				AI_FailWith( tag, "A first item named %s has null counterpart\n", first->Name() );
			}

			if( first->NameHash() != second->NameHash() || Q_stricmp( first->Name(), second->Name() ) ) {
				AI_FailWith( tag, "Item names mismatch: first item name is %s, second one is %s\n", first->Name(), second->Name() );
			}
		} else if( second ) {
			AI_FailWith( tag, "A second item named %s has null counterpart\n", second->Name() );
		}
#endif
	}

public:
	ZippedItemChainsIterator( T1 firstChainHead, T2 secondChainHead, const char *tag_ )
		: first( firstChainHead ), second( secondChainHead ), tag( tag_ ) {
		CheckMatch();
	}

	inline void Next() {
		first = first->Next();
		second = second->Next();
		CheckMatch();
	}

	inline bool HasNext() const { return first != nullptr; };

	inline T1 First() { return first; }
	inline T2 Second() { return second; }
};

template <typename T1, typename T2>
inline static ZippedItemChainsIterator<T1, T2> ZipItemChains( T1 firstChainHead, T2 secondChainHead, const char *tag ) {
	return ZippedItemChainsIterator<T1, T2>( firstChainHead, secondChainHead, tag );
};

#endif
