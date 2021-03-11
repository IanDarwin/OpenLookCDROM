/*
 * $Id: DICTSave.c,v 1.1 1993/08/17 03:19:34 mcpong Exp mcpong $
 */

/*$D **********************************************************************
 *	Module name:	DICTSave.c
 *	Purpose:	(1)
 *			void    SaveHzCiEntryTable( cpDICTFileName )
 *			    to save information from BCD Dictionary,
 *			    in "compiled" Trie form into the file
 *				    cpDICTFileName
 *	Organization:	CSD, HKUST
 *	Creator name:	mcpong@cs.ust.hk
 *	Creation Date:	1993/08/17 (Tue)
 *
 *	Modification history:
 *	====================
 *	mcpong	19940420(Wed)	TrieNode includes probabilities from training.
 *				Also clean up the program structure.
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

extern	PtrTrieNode     raFirstHzTrieNode[ ];

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

	int	gnMaxLenHzWord = 0 ;	/* maximum length of hanzi words */
					/* in AddTrie.c, will be initialized
					 * to be the value from DictBase
					 */

/*$EE *******  End  of exported entity section *****************************/


/*$P ******** Begin of private entity section ******************************
 *	... any other commenting notes, as you wish.
 */

	int             giOutTrieLevel = 0 ;
	FILE *          gFileOut ;

/*$PE *******  End  of private entity section ******************************/



/*$B ******** Begin of module body section *********************************
 *	... any other commenting notes, as you wish.
 */

static	void	CheckHzCollision( iTrieLevel, pTrieNode, bpHzCiBuffer )
	int		iTrieLevel ;
	PtrTrieNode	pTrieNode ;
	unsigned char *	bpHzCiBuffer ;
{
	/* check that the Hz to be added is the same as
	 * the Hz in the TrieNode:
	 */
	if ( pTrieNode->hzByte1 != *bpHzCiBuffer ||
	     pTrieNode->hzByte2 != *( bpHzCiBuffer + 1 ) ) {
		/* print out to highlight error:
		 */
		printf( "hz collided at TrieLevel: %d %s - %c%c,%c%c\n",
			iTrieLevel,
			bpHzCiBuffer - (iTrieLevel - 1),

			pTrieNode->hzByte1,
			pTrieNode->hzByte2,

			*bpHzCiBuffer,
			*( bpHzCiBuffer + 1 )
		);
	}
} /* CheckHzCollision */

#ifdef  OBSOLETE
static	void	InsertNewClassValue( p, iHzCiClass, fHzCiProb )
	PtrTrieNode     p ;
	int	iHzCiClass ;
	float	fHzCiProb ;
{
	int	newClassValue ;

	newClassValue = 1L << iHzCiClass ;

	/* there may be same entry (same phrase (HzCi) & HzCiClass)
	 * but with different pronunciation,
	 * thus check if HzCiClass same as the new class,
	 * then don't increase nClass count and simply return :
	 */
	if ( p->ulWordClasses & newClassValue ) {
		return;
	}
	p->ulWordClasses |= newClassValue ;
	p->nClass ++ ;
	p->cFreq = (char) fHzCiProb ;
} /* InsertNewClassValue */
#else /*OBSOLETE*/
static	void	InsertNewClassValue( p, nClass, ulWordClasses, fHzCiProb )
	PtrTrieNode     p ;
	int		nClass ;
	unsigned long	ulWordClasses ;
	float		fHzCiProb ;
{
	p->ulWordClasses = ulWordClasses ;
	p->ucNClass = (unsigned char) nClass ;
	p->fProb = fHzCiProb ;
} /* InsertNewClassValue */
#endif/*OBSOLETE*/

static	PtrTrieNode	NewTrieNode( bpHzCiBuffer )
	unsigned char *	bpHzCiBuffer ;
{
	PtrTrieNode	p ;

	p = (PtrTrieNode) calloc( sizeof( TrieNode ), 1 );
	p->hzByte1 = *bpHzCiBuffer ;
	p->hzByte2 = *( bpHzCiBuffer + 1 ) ;
	return	p ;
} /* NewTrieNode */

static	PtrTrieNode	MatchOrInsertTrieNode( pdaSons, bpHzCiBuffer )
	PtrDynArray	pdaSons ;
	unsigned char * bpHzCiBuffer ;
{
	PtrTrieNode	p ;

	p = MatchTrieNode( pdaSons, bpHzCiBuffer );
	if ( p != NULL ) {
		return	p ;
	}

	/* Insert new TrieNode with Hz information */

	p = NewTrieNode( bpHzCiBuffer );
	if ( da_AddItem( pdaSons, p ) ) {
		FatalError( "da_AddItem( pdaSons, p )" );
	} else {
		return	p ;
	}
} /* MatchOrInsertTrieNode */

void	InsertHzCiEntry( raFirstHzTrieNode, baHzCiBuffer, nClass, ulWordClass, fHzCiProb )
	PtrTrieNode     raFirstHzTrieNode[ ];
	unsigned char *	baHzCiBuffer ;
	int		nClass ;
	unsigned long	ulWordClass ;
	float		fHzCiProb ;
{
	unsigned char *	bpHzCiBuffer ;
	int		iHashValue ;
	int		iTrieLevel ;
	PtrTrieNode	pTrieNode ;

	int		nHzCount ;
	
	bpHzCiBuffer = baHzCiBuffer ;
	nHzCount = strlen( baHzCiBuffer ) / NHzBytes ;
	if ( nHzCount > gnMaxLenHzWord ) gnMaxLenHzWord = nHzCount ;

	/* To get the 1st level TrieNode.
	 * Make sure that it's not NULL.
	 */
	iHashValue = HashHz( bpHzCiBuffer );
	if ( raFirstHzTrieNode[ iHashValue ] == NULL ) {
		raFirstHzTrieNode[ iHashValue ] = NewTrieNode( bpHzCiBuffer );
	}
	pTrieNode = raFirstHzTrieNode[ iHashValue ];

	iTrieLevel = 1 ;
	CheckHzCollision( iTrieLevel, pTrieNode, bpHzCiBuffer );

	/* continue to traverse to further levels 
	 */
	while ( iTrieLevel < nHzCount ) {
		bpHzCiBuffer += NHzBytes ;

		/* ASSERT: bpHzCiBuffer points to the Hz to be matched or inserted.
		 */
		if ( pTrieNode->pdaSons == NULL ) {
			pTrieNode->pdaSons = da_InitDynArray( NInitPdaSons );
		}
		pTrieNode = MatchOrInsertTrieNode( pTrieNode->pdaSons, bpHzCiBuffer );
		iTrieLevel ++ ;
	} /*while*/

	/* remember the new classValue:
	 */
	InsertNewClassValue( pTrieNode, nClass, ulWordClass, fHzCiProb );
} /* InsertHzCiEntry */

/*********************************************************/
/*********************************************************/

void	InsertHzCiEntryIntoHandleDict( handleDict, baHzCiBuffer, nClass, ulWordClass, fHzCiProb )
	HandleDict	handleDict ;
	unsigned char *	baHzCiBuffer ;
	int		nClass ;
	unsigned long	ulWordClass ;
	float		fHzCiProb ;
{
	unsigned char *	bpHzCiBuffer ;
	int		iHashValue ;
	int		iTrieLevel ;
	PtrTrieNode	pTrieNode ;

	int		nHzCount ;
	
	bpHzCiBuffer = baHzCiBuffer ;
	nHzCount = strlen( baHzCiBuffer ) / NHzBytes ;
	if ( nHzCount > gnMaxLenHzWord ) gnMaxLenHzWord = nHzCount ;

	/* To get the 1st level TrieNode.
	 * Make sure that it's not NULL.
	 */
	iHashValue = HashHz( bpHzCiBuffer );
	if ( handleDict->paFirstHzTrieNode[ iHashValue ] == NULL ) {
		handleDict->paFirstHzTrieNode[ iHashValue ] = NewTrieNode( bpHzCiBuffer );
	}
	pTrieNode = handleDict->paFirstHzTrieNode[ iHashValue ];

	iTrieLevel = 1 ;
	CheckHzCollision( iTrieLevel, pTrieNode, bpHzCiBuffer );

	/* continue to traverse to further levels 
	 */
	while ( iTrieLevel < nHzCount ) {
		bpHzCiBuffer += NHzBytes ;

		/* ASSERT: bpHzCiBuffer points to the Hz to be matched or inserted.
		 */
		if ( pTrieNode->pdaSons == NULL ) {
			pTrieNode->pdaSons = da_InitDynArray( NInitPdaSons );
		}
		pTrieNode = MatchOrInsertTrieNode( pTrieNode->pdaSons, bpHzCiBuffer );
		iTrieLevel ++ ;
	} /*while*/

	/* remember the new classValue:
	 */
	InsertNewClassValue( pTrieNode, nClass, ulWordClass, fHzCiProb );
} /* InsertHzCiEntryIntoHandleDict */

/*********************************************************/
/*********************************************************/

#ifdef  OBSOLETE
static	void	TraverseSons( pdaSons, FuncOnTrieNode, FuncBeforeSons )
	PtrDynArray	pdaSons ;
	void	(*FuncOnTrieNode)( ) ;
	void	(*FuncBeforeSons)( ) ;
{
	PtrTrieNode	p ;
	PtrDynArray	pdaSonsNextLevel ;
	int		nSons ;

	giOutTrieLevel ++ ;
	for ( p = (PtrTrieNode) da_FirstItem( pdaSons );
	      p != NULL ;
	      p = (PtrTrieNode) da_Next_Item( pdaSons ) ) {

		(*FuncOnTrieNode)( p );

		pdaSonsNextLevel = p->pdaSons ;
		nSons = da_NumberOfItems( pdaSonsNextLevel );
		if ( nSons > 0 ) {
			if ( FuncBeforeSons ) {
				(*FuncBeforeSons)( p );
			}
			TraverseSons( pdaSonsNextLevel, FuncOnTrieNode, FuncBeforeSons );
		}
	}
	giOutTrieLevel -- ;
} /* TraverseSons */
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

/**************************/

static	void	SaveNumberOfSons( pdaSons )
	PtrDynArray	pdaSons ;
{
	int	nSons ;

	nSons = da_NumberOfItems( pdaSons ) ;
	fwrite( & nSons, sizeof( nSons ), 1, gFileOut );
} /* SaveNumberOfSons */

static	void	SaveSons( /* PtrDynArray pdaSons */ );

static	void	SaveTrieNode( p )
	PtrTrieNode	p ;
{
	PtrDynArray	pdaSons ;
	int		nSons ;

	fwrite( p, sizeof(TrieNode), 1, gFileOut );

	pdaSons = p->pdaSons ;
	nSons = da_NumberOfItems( pdaSons );
	fwrite( & nSons, sizeof( int ), 1, gFileOut );
	if ( nSons > 0 ) {
		SaveSons( pdaSons );
	}
} /* SaveTrieNode */

static	void	SaveSons( pdaSons )
	PtrDynArray	pdaSons ;
{
	PtrTrieNode	p ;

	for ( p = (PtrTrieNode) da_FirstItem( pdaSons );
	      p != NULL ;
	      p = (PtrTrieNode) da_Next_Item( pdaSons ) ) {

		SaveTrieNode( p );
	} /*for*/
} /* SaveSons */

void	SaveHzCiEntryTable( cpDICTFileName, raFirstHzTrieNode )
	char *		cpDICTFileName ;
	PtrTrieNode *	raFirstHzTrieNode ;
{
	int	i ;
	int	nFirstLevelTrieNode ;
	int	nSons ;
	PtrTrieNode	p ;
	PtrDynArray	pdaSons ;

	gFileOut = fopen( cpDICTFileName, "w" );
	if ( gFileOut == NULL ) {
		FatalError( "can't open DICTFileName" );
	}

	nFirstLevelTrieNode = 0 ;

	for ( i = 0 ; i < NFirstHzTable ; i++ ) {
		if ( raFirstHzTrieNode[ i ] != NULL ) {
			nFirstLevelTrieNode ++ ;
		}
	} /*for*/
	fwrite( & nFirstLevelTrieNode, sizeof( nFirstLevelTrieNode ), 1, gFileOut );
	fwrite( & gnMaxLenHzWord, sizeof( gnMaxLenHzWord ), 1, gFileOut );

	for ( i = 0 ; i < NFirstHzTable ; i++ ) {
		p = raFirstHzTrieNode[ i ];
		if ( p == NULL ) {
			/* no entry */
			continue ;
		}

		fwrite( & i, sizeof( i ), 1, gFileOut );
		SaveTrieNode( p );

	} /*for*/

	fclose( gFileOut );
} /* SaveHzCiEntryTable() */

/**************************/
