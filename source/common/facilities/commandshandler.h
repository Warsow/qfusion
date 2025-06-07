#ifndef WSW_348385b1_722e_499e_9e68_c3f6855fb36b_H
#define WSW_348385b1_722e_499e_9e68_c3f6855fb36b_H

#include <cstdint>
#include <utility>
#include <tuple>

#include <common/helpers/hash.h>
#include <common/helpers/links.h>

namespace wsw {

// Parametrize by the actual derived class so links are of the proper type
template <typename Derived>
struct GenericCommandCallback {
	enum { HashLinks, ListLinks };

	Derived *prev[2] { nullptr, nullptr };
	Derived *next[2] { nullptr, nullptr };

protected:
	wsw::PodVector<char> m_nameBuffer;
	const wsw::StringView m_tag;
	wsw::HashedStringView m_name;
	unsigned m_binIndex { ~0u };

	GenericCommandCallback( const wsw::StringView &tag, wsw::PodVector<char> &&name_ )
		: m_nameBuffer( std::move( name_ ) ), m_tag( tag )
		, m_name( wsw::StringView( m_nameBuffer.data(), m_nameBuffer.size() ) ) {}

	GenericCommandCallback( const wsw::StringView &tag, const wsw::HashedStringView &name )
		: m_nameBuffer( name.data(), name.size() ), m_tag( tag )
		, m_name( name ) {}

public:
	[[nodiscard]]
	auto getName() const -> const wsw::HashedStringView & { return m_name; }
	[[nodiscard]]
	auto getTag() const -> const wsw::StringView & { return m_tag; }

	[[nodiscard]]
	auto nextInBin() -> Derived * { return next[HashLinks]; }
	[[nodiscard]]
	auto nextInList() -> Derived * { return next[ListLinks]; }

	[[nodiscard]]
	auto nextInBin() const -> const Derived * { return next[HashLinks]; }
	[[nodiscard]]
	auto nextInList() const -> const Derived * { return next[ListLinks]; }

	[[nodiscard]]
	auto getBinIndex() const -> unsigned { return m_binIndex; }
	void setBinIndex( unsigned index ) { m_binIndex = index; }

	virtual ~GenericCommandCallback() = default;
};

template <typename Callback>
class CommandsHandler {
protected:
	static constexpr unsigned kNumBins = 197;

	Callback *m_listHead { nullptr };
	Callback *m_hashBins[kNumBins] {};
	unsigned m_size { 0 };

	void link( Callback *entry, unsigned binIndex ) {
		entry->setBinIndex( binIndex );
		wsw::link( entry, &m_hashBins[binIndex], Callback::HashLinks );
		wsw::link( entry, &m_listHead, Callback::ListLinks );
		m_size++;
	}

	void unlink( Callback *entry ) {
		assert( entry->getBinIndex() < kNumBins );
		wsw::unlink( entry, &m_hashBins[entry->getBinIndex()], Callback::HashLinks );
		wsw::unlink( entry, &m_listHead, Callback::ListLinks );
		assert( m_size > 0 );
		m_size--;
	}

	[[nodiscard]]
	virtual bool add( Callback *entry ) {
		const wsw::HashedStringView name( entry->getName() );
		const unsigned binIndex = name.getHash() % kNumBins;
		if( findByName( name, binIndex ) ) {
			return false;
		}
		link( entry, binIndex );
		return true;
	}

	[[nodiscard]]
	virtual auto addOrReplace( Callback *entry ) -> Callback * {
		const wsw::HashedStringView name( entry->getName() );
		const unsigned binIndex = name.getHash() % kNumBins;
		Callback *existing = nullptr;
		if( ( existing = findByName( name, binIndex ) ) ) {
			unlink( existing );
		}
		link( entry, binIndex );
		return existing;
	}

	[[nodiscard]]
	auto findByName( const wsw::HashedStringView &name ) -> Callback * {
		return findByName( name, name.getHash() % kNumBins );
	}

	[[nodiscard]]
	auto findByName( const wsw::HashedStringView &name, unsigned binIndex ) -> Callback * {
		Callback *entry = m_hashBins[binIndex];
		while( entry ) {
			if( entry->getName().equalsIgnoreCase( name ) ) {
				return entry;
			}
			entry = (Callback *)entry->nextInBin();
		}
		return nullptr;
	}

	template <typename Destructor>
	void removeByTag( const wsw::StringView &tag, Destructor &&destructor ) {
		for( Callback *entry = m_listHead; entry; ) {
			auto *const nextEntry = (Callback *)entry->nextInList();
			if( tag.equalsIgnoreCase( entry->getTag() ) ) {
				unlink( entry );
				destructor( entry );
			}
			entry = nextEntry;
		}
	}

	[[nodiscard]]
	auto removeByName( const wsw::StringView &name ) -> Callback * {
		if( Callback *callback = findByName( wsw::HashedStringView( name ) ) ) {
			unlink( callback );
			return callback;
		}
		return nullptr;
	}

	[[nodiscard]]
	auto removeByName( const wsw::HashedStringView &name ) -> Callback * {
		if( Callback *callback = findByName( name ) ) {
			unlink( callback );
			return callback;
		}
		return nullptr;
	}
public:
	virtual ~CommandsHandler() = default;
};

template <typename Result, typename... Args>
class VarArgCommandCallback : public GenericCommandCallback<VarArgCommandCallback<Result, Args...>> {
protected:
	VarArgCommandCallback( const wsw::StringView &tag, const wsw::HashedStringView &cmd )
		: GenericCommandCallback<VarArgCommandCallback<Result, Args...>>( tag, cmd ) {}
	VarArgCommandCallback( const wsw::StringView &tag, wsw::PodVector<char> &&cmd )
		: GenericCommandCallback<VarArgCommandCallback<Result, Args...>>( tag, std::move( cmd ) ) {}
public:
	[[nodiscard]]
	virtual auto operator()( Args... args ) -> Result = 0;
};

}

#endif
