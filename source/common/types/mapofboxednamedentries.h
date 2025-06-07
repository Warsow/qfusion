#ifndef WSW_1143f90d_cee5_437b_b67e_0964808a316c_H
#define WSW_1143f90d_cee5_437b_b67e_0964808a316c_H

#include <common/types/stringview.h>
#include <common/helpers/links.h>

#include <type_traits>
#include <cstdlib>

template <typename Derived>
struct BoxedHashMapNamedEntry {
	template <typename, unsigned, wsw::CaseSensitivity> friend class MapOfBoxedNamedEntries;
	Derived *m_prevInHashBin { nullptr };
	Derived *m_nextInHashBin { nullptr };
	wsw::HashedStringView m_nameAndHash;
	unsigned m_binIndex { 0 };
};

template <typename Derived>
struct BoxedListEntry {
	Derived *m_prevInList { nullptr };
	Derived *m_nextInList { nullptr };
};

template <typename Entry, unsigned NumBins, wsw::CaseSensitivity CaseSensitivity>
class MapOfBoxedNamedEntries final {
	static constexpr bool kAreItemsLinkedInList = std::is_base_of_v<BoxedListEntry<Entry>, Entry>;
public:
	MapOfBoxedNamedEntries( const MapOfBoxedNamedEntries & ) = delete;
	auto operator=( const MapOfBoxedNamedEntries & ) -> MapOfBoxedNamedEntries & = delete;

	MapOfBoxedNamedEntries() = default;
	~MapOfBoxedNamedEntries() { destroyAllEntries(); }

	[[nodiscard]]
	auto findByName( const wsw::HashedStringView &name ) -> Entry * {
		for( Entry *entry = m_entryBins[name.getHash() % NumBins]; entry; entry = entry->m_nextInHashBin ) {
			if constexpr( CaseSensitivity == wsw::MatchCase ) {
				if( entry->m_nameAndHash.equals( name ) ) {
					return entry;
				}
			} else {
				if( entry->m_nameAndHash.equalsIgnoreCase( name ) ) {
					return entry;
				}
			}
		}
		return nullptr;
	}

	[[nodiscard]]
	auto findByName( const wsw::HashedStringView &name ) const -> const Entry * {
		return constFindByName( name );
	}

	[[nodiscard]]
	auto constFindByName( const wsw::HashedStringView &name ) const -> const Entry * {
		for( const Entry *entry = m_entryBins[name.getHash() % NumBins]; entry; entry = entry->m_nextInHashBin ) {
			if constexpr( CaseSensitivity == wsw::MatchCase ) {
				if( entry->m_nameAndHash.equals( name ) ) {
					return entry;
				}
			} else {
				if( entry->m_nameAndHash.equalsIgnoreCase( name ) ) {
					return entry;
				}
			}
		}
		return nullptr;
	}

	[[nodiscard]]
	auto releaseOwnership( Entry *entry ) -> Entry * {
		assert( entry->m_binIndex < NumBins );
		wsw::unlink<Entry>( entry, &m_entryBins[entry->m_binIndex], &Entry::m_prevInHashBin, &Entry::m_nextInHashBin );
		if constexpr( kAreItemsLinkedInList ) {
			wsw::unlink<Entry>( entry, &m_listHead, &Entry::m_prevInList, &Entry::m_nextInList );
		}
		return entry;
	}

	void remove( Entry *entry ) {
		destroyEntry( releaseOwnership( entry ) );
		m_numEntries--;
	}

	[[maybe_unused]]
	bool insertOrReplaceTakingOwneship( Entry *entry ) {
		assert( !entry->m_nameAndHash.empty() );
		entry->m_binIndex = entry->m_nameAndHash.getHash() % NumBins;

		bool foundSame = false;
		if( Entry *existing = findByName( entry->m_nameAndHash ) ) {
			remove( existing );
			foundSame = true;
		}

		wsw::link<Entry>( entry, &m_entryBins[entry->m_binIndex], &Entry::m_prevInHashBin, &Entry::m_nextInHashBin );
		if constexpr( kAreItemsLinkedInList ) {
			wsw::link<Entry>( entry, &m_listHead, &Entry::m_prevInList, &Entry::m_nextInList );
		}

		m_numEntries++;
		return foundSame;
	}

	void insertUniqueTakingOwneship( Entry *entry ) {
		assert( !entry->m_nameAndHash.empty() );
		entry->m_binIndex = entry->m_nameAndHash.getHash() % NumBins;

		assert( !findByName( entry->m_nameAndHash ) );

		wsw::link<Entry>( entry, &m_entryBins[entry->m_binIndex], &Entry::m_prevInHashBin, &Entry::m_nextInHashBin );
		if constexpr( kAreItemsLinkedInList ) {
			wsw::link<Entry>( entry, &m_listHead, &Entry::m_prevInList, &Entry::m_nextInList );
		}

		m_numEntries++;
	}

	void clear() {
		destroyAllEntries();
		m_listHead   = nullptr;
		m_numEntries = 0;
		std::memset( m_entryBins, 0, sizeof( m_entryBins ) );
	}

	[[nodiscard]] bool empty() const { return !m_numEntries; }
	[[nodiscard]] auto size() const -> unsigned { return m_numEntries; }

	class const_iterator {
		template <typename, unsigned, wsw::CaseSensitivity> friend class MapOfBoxedNamedEntries;
	public:
		[[maybe_unused]]
		auto operator++() -> const_iterator & {
			m_entry = m_entry->m_nextInList;
			return *this;
		}
		[[maybe_unused]]
		auto operator++( int ) -> const_iterator & {
			const_iterator result = *this;
			m_entry = m_entry->m_nextInList;
			return result;
		}
		[[nodiscard]] bool operator==( const const_iterator &that ) const { return m_entry == that.m_entry; }
		[[nodiscard]] bool operator!=( const const_iterator &that ) const { return m_entry != that.m_entry; };
		[[nodiscard]] auto operator*() const -> const Entry * { return m_entry; }
	private:
		explicit const_iterator( const Entry *entry ) : m_entry( entry ) {}
		const Entry *m_entry { nullptr };
	};

	[[nodiscard]] auto begin() const -> const_iterator { return const_iterator( m_listHead ); };
	[[nodiscard]] auto end() const -> const_iterator { return const_iterator( nullptr ); }
	[[nodiscard]] auto cbegin() const -> const_iterator { return const_iterator( m_listHead ); };
	[[nodiscard]] auto cend() const -> const_iterator { return const_iterator( nullptr ); }
private:
	void destroyAllEntries() {
		if constexpr( kAreItemsLinkedInList ) {
			for( Entry *entry = m_listHead, *next = nullptr; entry; entry = next ) { next = entry->m_nextInList;
				destroyEntry( entry );
			}
		} else {
			for( unsigned i = 0; i < NumBins; ++i ) {
				for( Entry *entry = m_entryBins[i], *next = nullptr; entry; entry = next ) { next = entry->m_nextInHashBin;
					destroyEntry( entry );
				}
			}
		}
	}

	void destroyEntry( Entry *entry ) {
		entry->~Entry();
		std::free( entry );
	}

	Entry *m_entryBins[NumBins] {};
	// Unused if no links are specified
	Entry *m_listHead { nullptr };
	unsigned m_numEntries { 0 };
};

#endif