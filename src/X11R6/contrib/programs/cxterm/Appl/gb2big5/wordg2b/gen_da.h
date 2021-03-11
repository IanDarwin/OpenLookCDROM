/* @(#)DynArray.h  	v1.3	91/02/01	mcpong */
 
#ifndef _DynArray_H_
#define _DynArray_H_
 
#include <sys/types.h>	/* DynArray.h uses caddr_t */
 
/* Purpose:
 *
 * The abstract data type (ADT) DynArray (i.e., Dynamica Array)
 * represents an unlimited size array of any element item type (caddr_t).
 *
 * The client of this ADT implementation has to use C type cast
 * to put and retrieve the client's data from a DynArray variable.
 */

/* Declaration of DynArray.
 * -- C can't (completely) hide the type declaration details.
 * -- However, the clients can assume no knowledge of the details.
 */
typedef struct _DynArray {
	long	nMaxItem ;
	long	nNumItem ;
	long	iCount ;
	caddr_t * content ;
		/* points to start of array, elements assumed of type (caddr_t)
		 */
} DynArray, * PtrDynArray, ** AddrPtrDynArray ;
 
/********* export functions *******/

 
PtrDynArray	da_InitDynArray(
     /*IN int sizeHint */)
/*
 * Allocate storage for a DynArray of a certain maximum size;

 * and Initialize the actual size to 0.
 *
 * IF   given sizeHint is -ve THEN will use default maximum size of 16
 * ELSE use sizeHint as the maximum size.
 *
 * RETURN pointer to the initialized DynArray.
 * RETURN NULL if can't allocate storage for it.
 */
;

void	da_DestroyDynArray(
     /*IN PtrDynArray da */)
/*
 * Free the storage of the given DynArray da.
 *
 * PRE: da != NULL (If NULL, simply return.)
 */
;

int     da_SetEmptyDynArray(
     /*IN PtrDynArray da */)
/*
 * Set the number of elements of the given DynArray da to zero.
 *
 * (If da == NULL, simply return.)
 *
 * RETURN  0 if add OK.
 * RETURN -1 => not OK (either da == NULL
 *              or no storage for elements of da, i.e., da->content == NULL)
 */
;

int 	da_AddItem(
     /*IN PtrDynArray da,*/
     /*IN caddr_t item */)
/*
 * Add the given 'item' to the end of the given 'da'.
 *
 * PRE: da != NULL
 *
 * RETURN  0 if add OK.
 * RETURN -1 => add not OK (either da IS NULL or da has to be expanded
 *              to store item but can't reallocate memory for it)
 */
;

caddr_t	da_FirstItem(
     /*IN PtrDynArray da */)
/*
 * RETURN the first element's content of the given 'da'.
 *
 * PRE: da != NULL
 * POST:
 * The internal index to array element is incremented automatically.
 */
;

caddr_t da_Last_Item(
     /*IN PtrDynArray da */)
/*
 * RETURN the last element's content of the given 'da'.
 *
 * The internal index to array element is decremented automatically.
 */
;
caddr_t	da_Next_Item(
     /*IN PtrDynArray da */)
/*
 * RETURN the next element's content of the given 'da'.
 *	OR NULL if no next item.
 *
 * PRE: da != NULL
 * POST:
 * The internal index to array element is incremented automatically.
 */
;

caddr_t da_Prev_Item(
     /*IN PtrDynArray da */)
/*
 * RETURN the previous element's content of the given 'da'.
 *	OR NULL if no previous item.
 *
 * PRE: da != NULL
 * POST:
 * The internal index to array element is decremented automatically.
 */
;

caddr_t	da_I_th_Item(
     /*IN PtrDynArray da,*/
     /*IN int	i */)
/*
 * RETURN the i-th element's content of the given 'da'.
 *
 * PRE: da != NULL
 * POST: The internal index to array element becomes i
 *	 (in the range 0 .. nNumItem - 1).
 */
;

int	da_NumberOfItems(
     /*IN PtrDynArray da */)
/*
 * RETURN the no. of element items in the given dynamic array da.
 *
 * PRE: da != NULL
 */
;
/* Note: use of macro to implement da_NumberOfItems() will be more efficient,
 *	 but it exposes the implementation details.
#define da_NumberOfItems(da) ((da) ? (da)->nNumItem : 0L)
 */

caddr_t	da_DynArrayStartAddress(
     /*IN PtrDynArray da */)
/*
 * RETURN the start address of the given DynArray da;
 *        i.e.  the address of the 1st element of da.
 */
;
/* Note: providing da_DynArrayStartAddress() is for performance sake,,
 *	 but it exposes the implementation details.
 */
#endif /*_DynArray_H_*/
