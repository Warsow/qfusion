#ifndef WSW_a12c8f1d_84d5_404d_9746_2a20dfdb8145_H
#define WSW_a12c8f1d_84d5_404d_9746_2a20dfdb8145_H

#include <cassert>

namespace wsw {

// TODO: Get rid of overload with indices, make remaining of the old subroutines be based on the field ptr one

template <typename Item>
[[maybe_unused]]
inline auto link( Item *item, Item **listHeadRef, Item *( Item::*prevField ), Item *( Item::*nextField ) ) -> Item * {
	if( *listHeadRef ) [[likely]] {
		( *listHeadRef )->*prevField = item;
	}

	item->*prevField = nullptr;
	item->*nextField = *listHeadRef;
	*listHeadRef     = item;

	return item;
}

template <typename Item>
[[maybe_unused]]
inline auto unlink( Item *item, Item **listHeadRef, Item *( Item::*prevField ), Item *( Item::*nextField ) ) -> Item * {
	if( Item *const next = item->*nextField ) [[likely]] {
		next->*prevField = item->*prevField;
	}
	if( Item *const prev = item->*prevField ) [[likely]] {
		prev->*nextField = item->*nextField;
	} else {
		assert( item == *listHeadRef );
		*listHeadRef = item->*nextField;
	}

	item->*prevField = nullptr;
	item->*nextField = nullptr;
	return item;
}

/**
 * Links an item that has an intrusive array of links to a list head.
 * The item becomes the list head, the former head is linked to the "next" link
 * and the "prev" link is nullified (that's the list head contract).
 * @tparam Item any type that has two accessible arrays of links:
 * {@code Item *prev[]} and {@code Item *next[]}.
 * Arrays of pointers are used instead of single pointers
 * so an item can be linked to multiple lists simultaneously.
 * Consequently these arrays should have a size corresponding to
 * a desired number of lists an item can be linked to.
 * @param item an item to link
 * @param listHeadRef an address of the list head which is itself a pointer
 * @param listIndex an index of list to link to
 * @return the newly linked item (same as the argument) conforming to fluent API style.
 */
template<typename Item>
[[maybe_unused]]
inline auto link( Item *item, Item **listHeadRef, int linksIndex ) -> Item * {
	if( *listHeadRef ) {
		( *listHeadRef )->prev[linksIndex] = item;
	}
	item->prev[linksIndex] = nullptr;
	item->next[linksIndex] = *listHeadRef;
	*listHeadRef = item;
	return item;
}

/**
 * Unlinks an item that has an intrusive array of links from a list head.
 * Modifies the list head as well if the item was the head.
 * @tparam Item any type that has two accessible arrays of links:
 * {@code Item *prev[]} and {@code Item *next[]}.
 * Arrays of pointers are used instead of single pointers
 * so an item can be linked to multiple lists.
 * Consequently these arrays should have a size corresponding to a
 * desired number of lists an item can be linked to simultaneously.
 * @param item an item to unlink
 * @param listHeadRef an address of the list head which is itself a pointer
 * @param listIndex an index of list to unlink from.
 * @return the newly unlinked item (same as the argument) conforming to fluent API style.
 */
template<typename Item>
[[maybe_unused]]
inline auto unlink( Item *item, Item **listHeadRef, int listIndex ) -> Item * {
	if( auto *next = item->next[listIndex] ) {
		next->prev[listIndex] = item->prev[listIndex];
	}
	if( auto *prev = item->prev[listIndex] ) {
		prev->next[listIndex] = item->next[listIndex];
	} else {
		assert( item == *listHeadRef );
		*listHeadRef = item->next[listIndex];
	}

	item->prev[listIndex] = nullptr;
	item->next[listIndex] = nullptr;
	return item;
}

/**
 * A specialized version of {@code link} that is similar to {@code link(Item *, Item**, int)}
 * but operates on 16-bit signed indices for links instead of pointers
 * and accepts a base array as an additional argument.
 * Non-negative link indices are assumed to be valid and refer to the corresponding {@code basePtr} cells.
 * Negative link indices indicate "null" links.
 * These utilities are intended to be used if memory consumption/cache efficiency matters
 * ({@code basePtr} usually do not belongs to an item but is kept outside it).
 * @tparam Item any type that has two accessible arrays of links:
 * {@code int16_t *prev[]} and {@code int16_t *next[]}.
 * There arrays should have a size corresponding to a desired number of list an item can be linked to simultaneously.
 * @param item an item to link
 * @param listHeadRef an address of the current list head
 * @param listIndex an index of list to link to
 * @param basePtr an address of items storage the link indices refer to
 * @return the newly linked item (same as the argument) conforming to fluent API style.
 */
template<typename Item>
[[maybe_unused]]
inline auto link( Item *item, int16_t *listHeadRef, int listIndex, Item *basePtr ) -> Item * {
	const intptr_t offset = item - basePtr;
	assert( offset >= 0 );
	assert( offset <= ( 1 << 15u ) );
	const auto itemIndex = (int16_t)offset;

	// If the list head is defined (is not "null"), set its prev link to the newly linked item
	if( *listHeadRef >= 0 ) {
		Item *headItem = basePtr + *listHeadRef;
		headItem->prev[listIndex] = itemIndex;
	}

	item->prev[listIndex] = -1;
	item->next[listIndex] = *listHeadRef;

	*listHeadRef = itemIndex;

	return item;
}

/**
 * A specialized version of {@code unlink} that is similar to {@code unlink(Item *, Item**, int)}
 * but operates on 16-bit signed indices for links instead of pointers
 * and accepts a base array as an additional argument.
 * Non-negative link indices are assumed to be valid and refer to the corresponding {@code basePtr} cells.
 * Negative link indices indicate "null" links.
 * These utilities are intended to be used if memory consumption/cache efficiency matters
 * ({@code basePtr} usually do not belongs to an item but is kept outside it).
 * @tparam Item any type that has two accessible arrays of links:
 * {@code int16_t *prev[]} and {@code int16_t *next[]}.
 * There arrays should have a size corresponding to a desired number of list an item can be linked to simultaneously.
 * @param item an item to unlink
 * @param listHeadRef an address of the current list head
 * @param listIndex an index of list to unlink from
 * @param basePtr an address of items storage the link indices refer to
 * @return the newly unlinked item (same as the argument) conforming to fluent API style.
 */
template <typename Item>
inline auto unlink( Item *item, int16_t *listHeadRef, int listIndex, Item *basePtr ) -> Item * {
	const int16_t nextItemIndex = item->next[listIndex];
	// If the next item for the item is defined
	if( nextItemIndex >= 0 ) {
		Item *nextItem = basePtr + nextItemIndex;
		nextItem->prev[listIndex] = item->prev[listIndex];
	}
	const int16_t prevItemIndex = item->prev[listIndex];
	// If the prev item for the item is defined
	if( prevItemIndex >= 0 ) {
		Item *prevItem = basePtr + prevItemIndex;
		prevItem->next[listIndex] = item->next[listIndex];
	} else {
		// Check whether this item is the list head
		assert( item - basePtr == *listHeadRef );
		*listHeadRef = item->next[listIndex];
	}

	item->prev[listIndex] = -1;
	item->next[listIndex] = -1;
	return item;
}

/**
 * A simplified version of {@code Link(Item *, Item **, int)} for items that need only one pair of links.
 * @tparam Item any type that has {@code next} and {@code prev} links of the same type.
 * @param item an item to link
 * @param listHeadRef and address of the current list head
 * @return the newly linked item (same as the argument) conforming to fluent API style.
 */
template<typename Item>
[[maybe_unused]]
inline auto link( Item *item, Item **listHeadRef ) -> Item * {
	if( *listHeadRef ) {
		( *listHeadRef )->prev = item;
	}
	item->prev = nullptr;
	item->next = *listHeadRef;
	*listHeadRef = item;
	return item;
}

/**
 * A simplified version of {@code Unlink(Item, Item **, int) for items that need only one pair of links.
 * @tparam Item any type that has {@code next} and {@code prev} links of the same type.
 * @param item an item to unlink
 * @param listHeadRef an address of the current list head
 * @return the newly unlinked item (same as the argument) conforming to fluent API style.
 */
template<typename Item>
[[maybe_unused]]
inline auto unlink( Item *item, Item **listHeadRef ) -> Item * {
	if( auto *next = item->next ) {
		next->prev = item->prev;
	}
	if( auto *prev = item->prev ) {
		prev->next = item->next;
	} else {
		assert( item == *listHeadRef );
		*listHeadRef = item->next;
	}

	item->prev = nullptr;
	item->next = nullptr;
	return item;
}

}

#endif
