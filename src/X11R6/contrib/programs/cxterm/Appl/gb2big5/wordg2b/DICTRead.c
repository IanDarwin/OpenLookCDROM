/*
 * $Id: DICTRead.c,v 1.1 1993/08/17 03:19:34 mcpong Exp mcpong $
 */

/*$D **********************************************************************
 *	Module name:	DICTRead.c
 *	Purpose:	(1) API to load and access BCD Dictionary,
 *			    which is stored in "compiled" Trie form.
 *	Organization:	CSD, HKUST
 *	Creator name:	mcpong@uxmail.ust.hk
 *	Creation Date:	1993/08/03 (Tue)
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

#define	MaxHzCiLength	40

/*$INCE *****  End  of #include file section *******************************/


/*$I ******** Begin of imported entity section *****************************
 *	... any other commenting notes, as you wish.
 */

extern	void    FatalError(
	/*IN char *	cpMsg			*/
);
extern	PtrTrieNode     MatchTrieNode(
	/*IN PtrDynArray     pdaSons ;		*/
        /*IN unsigned char * bpHzCiBuffer ;	*/
);

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

static	void	RestoreTrieNode( p, fileHandle )
	/*IN*/	PtrTrieNode	p ;
	/*IN*/	FILE *		fileHandle ;
{
	int	n ;
	int	nSons ;
	PtrDynArray	pdaSons ;
	PtrTrieNode	pSon ;

	fread( p, sizeof( TrieNode ), 1, fileHandle );

	fread( & nSons, sizeof( nSons ), 1, fileHandle );

	if ( nSons == 0 ) {
		return;
	}

	pdaSons = da_InitDynArray( nSons );
	p->pdaSons = pdaSons ;

	for ( n = 0 ; n < nSons ; n++ ) {
		pSon = (PtrTrieNode) malloc( sizeof( TrieNode ) );
		da_AddItem( pdaSons, pSon );

		RestoreTrieNode( pSon, fileHandle );
	}
} /* RestoreTrieNode */

/*$C
 * Dict_LoadHzCiDictionary -- load the Hanzi Ci (word) dictionary with
 *		word class information.
 * IN:   the compiled dictionary file name.
 *
 * PRE:  the compiled dictionary file must exist and be correct.
 * POST: the dictionary file is opened and read into memory.
 *
 * RETURN     HandleDict if loading is OK;
 *	otherwise, fatal error and exit.
 *                
 */
/*EXPORT_FUN*/	HandleDict	Dict_LoadHzCiDictionary( cpDICTFileName )
	char *	cpDICTFileName ;
{
	int	i ;
	int	nFirstLevelTrieNode ;
	int	index ;
	int	nSons ;
	PtrTrieNode	p ;
	PtrDynArray	pdaSons ;
	HandleDict	handleDict ;
	FILE *	fileHandle ;

	fileHandle = fopen( cpDICTFileName, "r" );

	if ( fileHandle == NULL ) {
		fprintf( stderr, "can't fopen DICTFileName \"%s\"\n",
					cpDICTFileName );
		exit( -1 );
	/*	FatalError( "can't fopen DICTFileName" );
	*/
	}

#ifdef DEBUG
	fprintf( stderr, "Reading %s ... ", cpDICTFileName );
#endif/*DEBUG*/

	handleDict = (HandleDict) malloc( sizeof( struct _HandleDict ) );
	fread( & nFirstLevelTrieNode, sizeof( nFirstLevelTrieNode ), 1, fileHandle );
	fread( & (handleDict->nMaxNumberOfHzInWord), sizeof( handleDict->nMaxNumberOfHzInWord ), 1, fileHandle );
	handleDict->paFirstHzTrieNode =
		(PtrTrieNode *) calloc( NFirstHzTable, sizeof( PtrTrieNode ) );

	for ( i = 0 ; i < nFirstLevelTrieNode ; i++ ) {

		fread( & index, sizeof( index ), 1, fileHandle );
		p = (PtrTrieNode) malloc( sizeof( TrieNode ) );
		handleDict->paFirstHzTrieNode[ index ] = p ;

		RestoreTrieNode( p, fileHandle );
	} /*for*/
#ifdef DEBUG
	fprintf( stderr, "Reading done\n" );
#endif/*DEBUG*/
	fclose( fileHandle );

	return	handleDict ;
} /* Dict_LoadHzCiDictionary */

/*$C
 * Dict_MaxNumberofHzInWord -- 
 *
 * IN HandleDict
 *
 * RETURN the maximum number of hanzi of the longest word(s) in the dictionary.
 *
 * PRE:  the compiled dictionary file must exist and be correct.
 * POST: no change
 *
 */
/*EXPORT_FUN*/	int	Dict_MaxNumberofHzInWord( handleDict )
	HandleDict	handleDict ;
{
	return	handleDict->nMaxNumberOfHzInWord ;
}

/*$C
 * Dict_GetHzCiInfo -- 
 *
 * IN	handleDict is a handle to the data structure of the Dictionary of HzCi.
 * IN	bp is a pointer to a hanzi word (not necessarily null-terminated).
 * IN	nHzCount is the no. of hanzi to be processed in bp.
 *
 * OUT	*ucAddrNClass holds the number of classes that the hanzi word belongs to;
 *	== 0 means the hanzi word is not in the dictionary.
 * OUT	*ulAddrClassValues holds the bit mask of the class of the hanzi word;
 *	== 0 means the hanzi word is not in the dictionary.
 * OUT  *fAddrProb holds the probability (0.0 .. 1.0) of the hanzi word;
 *      == 0.0 means the hanzi word is not in the dictionary.
 *
 * PRE:  bp != NULL & has nHzCount hanzi.
 * POST: no side effect to global environment.
 *
 * RETURN     Dict_OK if OK;
 *	  Dict_NOT_OK if can't find the given word in the handleDict.
 */
/*EXPORT_FUN*/	int	Dict_GetHzCiInfo( handleDict, bp, nHzCount, iAddrNClass, ulAddrClassValues, fAddrProb )
	/*IN*/	HandleDict	handleDict ;
	/*IN*/	PtrByte		bp ;
	/*IN*/	int		nHzCount ;
	/*OUT*/	int		*iAddrNClass ;
	/*OUT*/	unsigned long	*ulAddrClassValues ;
	/*OUT*/	float           *fAddrProb ;
{
	PtrTrieNode	pTrieNode ;
	int		index ;
	int		iTrieLevel ;

	/* first assume hanzi word not in dictionary.
	 */
	*fAddrProb	= 0.0 ;
	*iAddrNClass	= 0 ;
	*ulAddrClassValues = 0 ;

	/* play safe to guard invalid IN value:
	 */
	if ( bp == NULL || *bp == NULL ) {
		return Dict_NOT_OK ;
	}

	index = HashHz( bp );
	pTrieNode = handleDict->paFirstHzTrieNode[ index ] ;
	if ( pTrieNode == NULL ) {
		return Dict_NOT_OK ;
	}

	/* ASSERT: matched one hanzi. */
        iTrieLevel = 1 ;

	/* INVARIANT: iTrieLevel == no. of hanzi processed already. */

        /* continue to traverse to further levels:
         */
        while ( iTrieLevel < nHzCount ) {
                bp += NHzBytes ;
                iTrieLevel ++ ;

                /* ASSERT: bp points to the Hz to be matched or inserted.
                 */
                if ( pTrieNode->pdaSons == NULL ) {
			return Dict_NOT_OK ;
                }
                pTrieNode = MatchTrieNode( pTrieNode->pdaSons, bp );
		if ( pTrieNode == NULL ) {
			return Dict_NOT_OK ;
		}
        } /*while*/

	if ( pTrieNode == NULL ) {
		return Dict_NOT_OK ;
	}
	*fAddrProb	= (float) pTrieNode->fProb ;
	*iAddrNClass	= (int) pTrieNode->ucNClass ;
	*ulAddrClassValues = pTrieNode->ulWordClasses ;
	return Dict_OK ;
	
} /* Dict_GetHzCiClass */

/*$CE *******  End  of Dict_GetHzCiClass() *******************************/


/********* BEGIN: useful for Dict_GetHzCiMaximalMatchQueue() *********/

static	PtrDynArray	gpdaMaximalMatch = NULL ;

/*********   END: useful for Dict_GetHzCiMaximalMatchQueue() *********/

/*$C
 * Dict_InitMaximalMatchQueue -- initialize the storage for MaximalMatchQueue
 *
 * POST:	always true
 */
/*EXPORT_FUN*/  void	Dict_InitMaximalMatchQueue()
{
	gpdaMaximalMatch = da_InitDynArray( 16 );
}

/*$C
 * Dict_GetHzCiMaximalMatchQueue -- 
 *
 * IN	handleDict is a handle to the data structure of the Dictionary of HzCi.
 * IN	bp is a pointer to a hanzi word (not necessarily null-terminated).
 * INOUT -- IN  *nAddrHzCount is the no. of hanzi to be processed in bp, and
 *       -- OUT *nAddrHzCount is the no. of hanzi actually matched in bp.
 *
 * PRE:  bp != NULL & has *nAddrHzCount hanzi.
 * POST: no side effect to global environment.
 *
 * RETURN     Dict_OK if OK;
 *	  Dict_NOT_OK if error.
 */
/*EXPORT_FUN*/	int	Dict_GetHzCiMaximalMatchQueue( handleDict, bp, nAddrHzCount )
	/*IN*/	HandleDict	handleDict ;
	/*IN*/	PtrByte		bp ;
	/*INOUT*/	int		*nAddrHzCount ;
{
	PtrTrieNode	pTrieNode ;
	int		index ;
	int		iTrieLevel ;

	/* play safe to guard against invalid IN value:
	 */
	if ( bp == NULL || *bp == NULL ) {
		*nAddrHzCount = 0 ;	/* iTrieLevel = 0 */
		return Dict_NOT_OK ;
	}

	da_SetEmptyDynArray( gpdaMaximalMatch );

	index = HashHz( bp );
	pTrieNode = handleDict->paFirstHzTrieNode[ index ] ;
	if ( pTrieNode == NULL ) {
		/* no match of 1st hanzi */
		*nAddrHzCount = 0 ;
		return Dict_NOT_OK ;
	}
	da_AddItem( gpdaMaximalMatch, pTrieNode );

	/* ASSERT: matched one hanzi. */
        iTrieLevel = 1 ;

	/* INVARIANT: iTrieLevel == no. of hanzi processed already. */

        /* continue to traverse to further levels:
         */
        while ( iTrieLevel < *nAddrHzCount ) {
                bp += NHzBytes ;
                /* ASSERT: bp points to the Hz to be matched or inserted.
                 */
                if ( pTrieNode->pdaSons == NULL ) {
			*nAddrHzCount = iTrieLevel ;
			return Dict_NOT_OK ;
                }
                pTrieNode = MatchTrieNode( pTrieNode->pdaSons, bp );
		if ( pTrieNode == NULL ) {
			*nAddrHzCount = iTrieLevel ;
			return Dict_NOT_OK ;
		}
		da_AddItem( gpdaMaximalMatch, pTrieNode );

		iTrieLevel ++ ;
        } /*while*/

	if ( pTrieNode == NULL ) {
		*nAddrHzCount = iTrieLevel ;
		return Dict_NOT_OK ;
	}
	return Dict_OK ;
	
} /* Dict_GetHzCiMaximalMatchQueue */

/*$C
 * Dict_FirstInMaximalMatchQueue -- Get information of First entry in MaximalMatchQueue
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/	PtrTrieNode	Dict_FirstInMaximalMatchQueue()
{
	return	(PtrTrieNode) da_FirstItem( gpdaMaximalMatch );
} /* Dict_FirstInMaximalMatchQueue */

/*$C
 * Dict_Next_InMaximalMatchQueue -- Get information of Next entry in MaximalMatchQueue
 * -- must be used after calling to Dict_FirstInMaximalMatchQueue().
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/	PtrTrieNode	Dict_Next_InMaximalMatchQueue()
{
	return	(PtrTrieNode) da_Next_Item( gpdaMaximalMatch );
} /* Dict_Next_InMaximalMatchQueue */

/*$C
 * Dict_FirstInMaximalMatchQueue -- Get information of First entry in MaximalMatchQueue
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/	PtrTrieNode	Dict_Last_InMaximalMatchQueue()
{
	return	(PtrTrieNode) da_Last_Item( gpdaMaximalMatch );
} /* Dict_FirstInMaximalMatchQueue */

/*$C
 * Dict_Prev_InMaximalMatchQueue -- Get information of Prev entry in MaximalMatchQueue
 * -- must be used after calling to Dict_Last_InMaximalMatchQueue().
 *
 * RETURN     PtrTrieNode or NULL
 */
/*EXPORT_FUN*/	PtrTrieNode	Dict_Prev_InMaximalMatchQueue()
{
	return	(PtrTrieNode) da_Prev_Item( gpdaMaximalMatch );
} /* Dict_Prev_InMaximalMatchQueue */

/*$BE *******  End  of module body section *********************************/

/*$ME *******  End  of <whole module file name> ****************************/

