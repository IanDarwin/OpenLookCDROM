/* @(#)gen_da.c  	1.4	92/05/07	mcpong */
 
/* gen_da.c
 * -- Routines to handle the GENeric data structure DynArray:
 * --	Dynamic Array of items.
 * -- Each array element can store a pointer to any kind of item.
 * -- See gen_da.h for the details of DynArray.
 */

#ifndef lint
static  char *sccsid = "@(#)gen_da.c	1.3 91/02/01";
#endif
 
#include <malloc.h>
#include <stdio.h>	/* stderr, fprintf() */
#include <stdlib.h>	/* exit() */

#include "gen_da.h"
 
/*******************
Start_Implementation
*******************/
 
PtrDynArray	da_InitDynArray( sizeHint )
     /*IN*/ int sizeHint ;
/*
 * Allocate storage for a DynArray of a certain maximum size;
 * and Initialize the actual size to 0.
 *
 * if (   given sizeHint is -ve ) { will use default maximum size of 16
 * ELSE use sizeHint as the maximum size.
 *
 * RETURN pointer to the initialized DynArray.
 * RETURN NULL if can't allocate storage for it.
 */
{
	PtrDynArray	da;
	int 	iNewSize ;
 
	da = (PtrDynArray)malloc( sizeof(DynArray) );
	if (  ! da  ) {
		return( (PtrDynArray)0 );
	}
	iNewSize = ( sizeHint == 0 ) ? 16
				     : ( ( sizeHint == 1 ) ? 2 : sizeHint ) ;
			/* iNewSize at least == 2 s.t. in realloc in da_AddItem()
			 * da_nMaxItem >> 1 won't give 0 which causes mysterious
			 * error!!
			 */
	da->content = (caddr_t *)calloc( iNewSize, sizeof(caddr_t) );
	da->nMaxItem = (long) iNewSize ;
	da->nNumItem = 0L ;
	da->iCount = 0L ;
	if (  ! da->content  ) {
		free( (char *)da );
		return( (PtrDynArray)0 );
	}
	return( da );
}
 

void	da_DestroyDynArray( da )
     /*IN*/ PtrDynArray da ;
/*
 * Free the storage of the given DynArray da.
 *
 * (If da == NULL, simply return.)
 */
{
	if (  ! da  ) {
		return;
	}
	if (  da->content  ) {
		free( (char *)(da->content) );
	}
	free( (char *)da );
}

int	da_SetEmptyDynArray( da )
     /*IN*/ PtrDynArray da ;
/*
 * Set the number of elements of the given DynArray da to zero.
 *
 * (If da == NULL, simply return.)
 *
 * RETURN  0 if add OK.
 * RETURN -1 => not OK (either da == NULL
 *		or no storage for elements of da, i.e., da->content == NULL)
 */
{
	if (  ! da  ||  ! da->content ) {
		return	-1 ;
	}
	da->nNumItem = 0L ;
	da->iCount = 0L ;
	return	0 ;
}

int 	da_AddItem( da, item )
     /*IN*/ PtrDynArray da ;
     /*IN*/ caddr_t item ;
/*
 * Add the given 'item' to the end of the given 'da'.
 *
 * RETURN  0 if add OK.
 * RETURN -1 => add not OK (either da == NULL or da has to be expanded
 *              to store item but can't reallocate memory for it)
 */
{
	long lNewSize ;

	if (  ! da  ) {
		return( -1 );
	}
	if (  da->nNumItem >= da->nMaxItem  ) {
		da->nMaxItem *= 2 ;
		da->content = (caddr_t *)realloc( (char *)(da->content),
				sizeof(caddr_t) * (da->nMaxItem) );

		if (  ! da->content  ) {
			return( -1 );
		}
	}
	da->content[ da->nNumItem++ ] = item;
	return( 0 );
}
 
caddr_t	da_FirstItem( da )
     /*IN*/ PtrDynArray da ;
/*
 * RETURN the first element's content of the given 'da'.
 *
 * The internal index to array element is incremented automatically.
 */
{
	if (  ! da  ) {
		return( (caddr_t)0 );
	}
	if (  da->nNumItem == 0  ) {
		return( (caddr_t)0 );
	}
	da->iCount = 1L ;
	return( da->content[0] );
}
 
caddr_t	da_Last_Item( da )
     /*IN*/ PtrDynArray da ;
/*
 * RETURN the last element's content of the given 'da'.
 *
 * The internal index to array element is decremented automatically.
 */
{
	if (  ! da  ) {
		return( (caddr_t)0 );
	}
	if (  da->nNumItem == 0  ) {
		return( (caddr_t)0 );
	}
	da->iCount = da->nNumItem ;
	return( da->content[ -- (da->iCount) ] );
}
 
caddr_t	da_Next_Item( da )
     /*IN*/ PtrDynArray da ;
/*
 * RETURN the next element's content of the given 'da'.
 *
 * The internal index to array element is incremented automatically.
 */
{
	if (  ! da  ) {
		return( (caddr_t)0 );
	}
	if (  (da->iCount) >= da->nNumItem  ) {
		return( (caddr_t)0 );
	}
	return( da->content[ (da->iCount) ++ ] );
}

caddr_t	da_Prev_Item( da )
     /*IN*/ PtrDynArray da ;
/*
 * RETURN the previous element's content of the given 'da'.
 *
 * The internal index to array element is decremented automatically.
 */
{
	if (  ! da  ) {
		return( (caddr_t)0 );
	}
	if (  (da->iCount) <= 0  ) {
		return( (caddr_t)0 );
	}
	return( da->content[ -- (da->iCount) ] );
}

caddr_t	da_I_th_Item( da, i )
     /*IN*/ PtrDynArray da ;
     /*IN*/ int	i ;
/*
 * RETURN the i-th element's content of the given 'da'.
 *
 * POST: The internal index to array element is i
 *	 (in the range 0 .. nNumItem - 1).
 */
{
	if (  ! da  ) {
		return( (caddr_t)0 );
	}
	if (  i >= da->nNumItem  || i < 0 ) {
		return( (caddr_t)0 );
	}
	da->iCount = (long) i + 1 ;
	/* POST: iCount points to next item, hence + 1.
	 */
	return( da->content[ i ] );
}

int     da_NumberOfItems( da )
     /*IN*/ PtrDynArray da ;
/*
 * RETURN the no. of element items in the given dynamic array da.
 *
 * PRE: da != NULL
 */
{
	return	((da) ? (da)->nNumItem : 0L);
}

caddr_t	da_DynArrayStartAddress( da )
     /*IN*/ PtrDynArray da ;
/*
 * RETURN the start address of the given DynArray da;
 *        i.e.  the address of the 1st element of da.
 */
{
	if (  ! da  ) {
		return( (caddr_t)0 );
	}
	if (  da->nNumItem == 0  ) {
		return( (caddr_t)0 );
	}
	da->iCount = 1L ;
	return( (caddr_t) ( da->content ) );
}
 
/* End of file: gen_da.c */
