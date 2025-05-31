/*
Copyright (C) 2008 German Garcia

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*/

#ifndef __QAS_PUBLIC_H__
#define __QAS_PUBLIC_H__

#include "angelscript.h"

// public interfaces

typedef struct asstring_s {
	char *buffer;
	unsigned int len, size;
	int asRefCount;
} asstring_t;

typedef struct asvec3_s {
	vec3_t v;
} asvec3_t;

class CScriptArrayInterface
{
protected:
	virtual ~CScriptArrayInterface() {};

public:
	virtual void AddRef() const = 0;
	virtual void Release() const = 0;

	virtual void Resize( unsigned int numElements ) = 0;
	virtual unsigned int GetSize() const = 0;

	// Get a pointer to an element. Returns 0 if out of bounds
	virtual const void *At( unsigned int index ) const = 0;

	virtual void InsertAt( unsigned int index, void *value ) = 0;
	virtual void RemoveAt( unsigned int index ) = 0;
	virtual void Sort( unsigned int index, unsigned int count, bool asc ) = 0;
	virtual void Reverse() = 0;
	virtual int  Find( unsigned int index, void *value ) const = 0;
};

class CScriptDictionaryInterface
{
protected:
	virtual ~CScriptDictionaryInterface() {};

public:
	virtual void AddRef() const = 0;
	virtual void Release() const = 0;

	// Sets/Gets a variable type value for a key
	virtual void Set( const asstring_t &key, void *value, int typeId ) = 0;
	virtual bool Get( const asstring_t &key, void *value, int typeId ) const = 0;

	// Sets/Gets an integer number value for a key
	virtual void Set( const asstring_t &key, int64_t &value ) = 0;
	virtual bool Get( const asstring_t &key, int64_t &value ) const = 0;

	// Sets/Gets a real number value for a key
	virtual void Set( const asstring_t &key, double &value ) = 0;
	virtual bool Get( const asstring_t &key, double &value ) const = 0;

	// Returns true if the key is set
	virtual bool Exists( const asstring_t &key ) const = 0;

	// Deletes the key
	virtual void Delete( const asstring_t &key ) = 0;

	// Deletes all keys
	virtual void DeleteAll() = 0;
};

class CScriptAnyInterface
{
protected:
	virtual ~CScriptAnyInterface() {};

public:
	// Memory management
	virtual int AddRef() const = 0;
	virtual int Release() const = 0;

	// Store the value, either as variable type, integer number, or real number
	virtual void Store( void *ref, int refTypeId ) = 0;
	virtual void Store( asINT64 &value ) = 0;
	virtual void Store( double &value ) = 0;

	// Retrieve the stored value, either as variable type, integer number, or real number
	virtual bool Retrieve( void *ref, int refTypeId ) const = 0;
	virtual bool Retrieve( asINT64 &value ) const = 0;
	virtual bool Retrieve( double &value ) const = 0;

	// Get the type id of the stored value
	virtual int  GetTypeId() const = 0;
};

/******* C++ objects *******/
asIScriptEngine *qasCreateEngine( bool *asMaxPortability );
asIScriptContext *qasAcquireContext( asIScriptEngine *engine );
void qasReleaseContext( asIScriptContext *ctx );
void qasReleaseEngine( asIScriptEngine *engine );
asIScriptContext *qasGetActiveContext( void );

// array tools
CScriptArrayInterface *qasCreateArrayCpp( unsigned int length, void *ot );
void qasReleaseArrayCpp( CScriptArrayInterface *arr );

// string tools
asstring_t *qasStringFactoryBuffer( const char *buffer, unsigned int length );
void qasStringRelease( asstring_t *str );
asstring_t *qasStringAssignString( asstring_t *self, const char *string, unsigned int strlen );

// dictionary tools
CScriptDictionaryInterface *qasCreateDictionaryCpp( asIScriptEngine *engine );
void qasReleaseDictionaryCpp( CScriptDictionaryInterface *dict );

// any tools
CScriptAnyInterface *qasCreateAnyCpp( asIScriptEngine *engine );
void qasReleaseAnyCpp( CScriptAnyInterface *any );

// projects / bundles
asIScriptModule *qasLoadScriptProject( asIScriptEngine *engine, const char *moduleName, const char *rootDir, const char *dir, const char *filename, const char *ext );

#endif // __QAS_PUBLIC_H__
