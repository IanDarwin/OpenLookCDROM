/*
 * $Id: DICT.c,v 1.1 1993/08/17 03:19:34 mcpong Exp mcpong $
 */

/*$D **********************************************************************
 *	Module name:	DICT.c
 *	Purpose:	(1) utility routines to access BCD Dictionary
 *			    in "compiled" Trie form.
 *	Organization:	CSD, HKUST
 *	Creator name:	mcpong@uxmail.ust.hk
 *	Creation Date:	1993/08/17 (Tue)
 *
 *	Modification history:
 *	====================
 *	who	date	
 *		why	...
 *			...
 *		changes/fixes	...
 *				...
 *	who	date
 *		...
 *	.
 *	.
 *	.
 */

/*$DE *******  End  of documentation section ******************************
 */


/*$INC ****** Begin of #include file section *******************************
 *	... any other commenting notes, as you wish.
 */

#include <stdio.h>
#include <assert.h>
#include "gen_da.h"

#include "DICT.h"	/* which contains API */

/*$INCE *****  End  of #include file section *******************************/


/*$I ******** Begin of imported entity section *****************************
 *	... any other commenting notes, as you wish.
 */

/*$IE *******  End  of imported entity section *****************************/


/*$E ******** Begin of exported entity section *****************************
 *	... any other commenting notes, as you wish.
 */

/*$EE *******  End  of exported entity section *****************************/


/*$P ******** Begin of private entity section ******************************
 *	... any other commenting notes, as you wish.
 */

/*$PE *******  End  of private entity section ******************************/



/*$B ******** Begin of module body section *********************************
 *	... any other commenting notes, as you wish.
 */

/*EXPORT_FUN*/	PtrTrieNode     MatchTrieNode( pdaSons, bpHzCiBuffer )
        PtrDynArray     pdaSons ;
        unsigned char * bpHzCiBuffer ;
{
        PtrTrieNode     p ;

        for ( p = (PtrTrieNode) da_FirstItem( pdaSons );
              p != NULL ;
              p = (PtrTrieNode) da_Next_Item( pdaSons ) ) {
                if ( p->hzByte1 == *bpHzCiBuffer &&
                     p->hzByte2 == *( bpHzCiBuffer + 1 ) ) {
                        /* Hz matched p's */
                        return  p ;
                }
        } /*for*/

        assert( p == NULL );
	return	NULL ;
} /* MatchTrieNode */

/*$CE *******  End  of MatchTrieNode() *******************************/

/*$BE *******  End  of module body section *********************************/

/*$ME *******  End  of <whole module file name> ****************************/

