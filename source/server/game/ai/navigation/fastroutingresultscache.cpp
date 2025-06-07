#include "fastroutingresultscache.h"
#include <common/helpers/links.h>
#include <common/helpers/algorithm.h>

void FastRoutingResultsCache::reset() {
	wsw::fill( std::begin( m_nodes ), std::end( m_nodes ), Node {} );
	wsw::fill( std::begin( m_hashBins ), std::end( m_hashBins ), -1 );
	m_cursorIndex = 0;

	// Make all zeroed nodes linked to the zero hash bin

	// Link all nodes in chain
	m_nodes[0].prev[0] = -1;
	m_nodes[0].next[0] = +1;

	assert( std::size( m_nodes ) == kMaxCachedResults );
	for( unsigned i = 1; i < kMaxCachedResults - 1; ++i ) {
		m_nodes[i].prev[0] = (int16_t)( i - 1 );
		m_nodes[i].next[0] = (int16_t)( i + 1 );
	}

	m_nodes[kMaxCachedResults - 1].prev[0] = kMaxCachedResults - 2;
	m_nodes[kMaxCachedResults - 1].next[0] = -1;

	// Link the head of the nodes chain to the zero bin
	m_hashBins[0] = 0;
}

auto FastRoutingResultsCache::getCachedResultForKey( uint16_t binIndex, uint64_t key ) const -> const Node * {
	int16_t nodeIndex = m_hashBins[binIndex];
	while( nodeIndex >= 0 ) {
		Node *const cacheNode = m_nodes + nodeIndex;
		if( cacheNode->key == key ) {
			// We can just return the cacheNode.
			// However, it's better to prevent an eventual loss of the cached result
			// even if it's frequently used due to being overwritten by the buffer cursor.

			// Unlink it and link again.
			// TODO: Should we change the public interface so this is explicit for the caller?

			const auto reachability = cacheNode->reachability;
			const auto travelTime   = cacheNode->travelTime;
			// Unlink it
			wsw::unlink( cacheNode, &m_hashBins[cacheNode->binIndex], 0, m_nodes );
			// Prepare for linking to the zero bin
			*cacheNode = Node {};
			// Link it to the zero bin
			wsw::link( cacheNode, &m_hashBins[0], 0, m_nodes );

			Node *resultNode = const_cast<FastRoutingResultsCache *>( this )->allocAndRegisterForKey( binIndex, key );
			resultNode->reachability = reachability;
			resultNode->travelTime   = travelTime;
			return resultNode;
		}
		nodeIndex = cacheNode->next[0];
	}
	return nullptr;
}

auto FastRoutingResultsCache::allocAndRegisterForKey( uint16_t binIndex, uint64_t key ) -> Node * {
	Node *node = m_nodes + m_cursorIndex;

	assert( (unsigned)node->binIndex < (unsigned)kNumHashBins );

#if 0
	int16_t nodeIndex = m_hashBins[node->binIndex];
	bool found = false;
	while( nodeIndex >= 0 ) {
		const Node *currNode = m_nodes + nodeIndex;
		if( currNode == node ) {
			found = true;
			break;
		}
		nodeIndex = currNode->next[0];
	}
	assert( found );
#endif

	// Unlink it from its (old) hash bin
	wsw::unlink( node, &m_hashBins[node->binIndex], 0, m_nodes );

	// Update its hash properties
	node->binIndex = binIndex;
	node->key      = key;

	// Link it to the new hash bin
	wsw::link( node, &m_hashBins[binIndex], 0, m_nodes );

	m_cursorIndex = ( m_cursorIndex + 1 ) % kMaxCachedResults;

	return node;
}