#ifndef WSW_5b8e8bc5_0b56_427f_a425_d00fa41edf1d_H
#define WSW_5b8e8bc5_0b56_427f_a425_d00fa41edf1d_H

/**
 * A helper for sorting linked lists (in ascending order by default).
 * A list item must be structurally compatible to an argument of {@code ::Link(Item *, Item **, int)}.
 * @tparam Item a type of a list item
 * @tparam Less a type of a less callable object
 * @param inputListHead a head of raw (unsorted) input list
 * @param inputListIndex an index of links that should be used for iteration over the raw list
 * @param sortedListIndex an index of links for linking sorted items.
 * @param less a callable object that is a binary predicate returning true if the first arg is less than the second
 * @return a head of a newly made sorted list.
 */
template <typename Item, typename Less>
Item *SortLinkedList( Item *inputListHead, int inputListIndex, int sortedListIndex, Less less ) {
	Item *sortedHead = nullptr;
	for( Item *inputItem = inputListHead; inputItem; inputItem = inputItem->next[inputListIndex] ) {
		Item *sortedItem = sortedHead;
		Item *sortedListTail = nullptr;
		// Find first item among sorted ones that is not less than this one
		for(; sortedItem; sortedItem = sortedItem->next[sortedListIndex] ) {
			sortedListTail = sortedItem;
			if( !less( *sortedItem, *inputItem ) ) {
				break;
			}
		}
		// If there was an item not less than the current input one
		if( sortedItem ) {
			// Link the new item before this one and after "prev for this one"
			Item *const prevForSortedOne = sortedItem->prev[sortedListIndex];
			if( prevForSortedOne ) {
				prevForSortedOne->next[sortedListIndex] = inputItem;
			} else {
				sortedHead = inputItem;
			}
			inputItem->prev[sortedListIndex] = prevForSortedOne;
			inputItem->next[sortedListIndex] = sortedItem;
			sortedItem->prev[sortedListIndex] = inputItem;
			continue;
		}
		// All items are lest than the sorted one.
		// Link the new one at the end of the list
		if( sortedListTail ) {
			sortedListTail->next[sortedListIndex] = inputItem;
			inputItem->prev[sortedListIndex] = sortedListTail;
		} else {
			inputItem->prev[sortedListIndex] = nullptr;
			assert( !sortedHead );
			sortedHead = inputItem;
		}
		inputItem->next[sortedListIndex] = nullptr;
	}
	return sortedHead;
};

#endif
