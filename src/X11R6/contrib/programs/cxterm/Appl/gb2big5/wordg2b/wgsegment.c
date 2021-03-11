#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <search.h>
#include "modularC.h"
#include "DICT.h"   /* which contains API */

/*----------------------------------------------
	extern
----------------------------------------------*/

/*----------------------------------------------
	export
----------------------------------------------*/

#ifdef  MAIN
static	HandleDict	ghandleDictFMM ;
static	HandleDict	ghandleDictBMM ;
static	int		gnMaxByteOfHzFMM ;
static	int		gnMaxByteOfHzBMM ;
static	int		giDebugLevel = 0 ;

#else /*MAIN*/

extern	HandleDict	ghandleDictFMM ;
extern	HandleDict	ghandleDictBMM ;
extern	int		gnMaxByteOfHzFMM ;
extern	int		gnMaxByteOfHzBMM ;
extern	int		giDebugLevel ;
#endif/*MAIN*/

/*----------------------------------------------
	private
----------------------------------------------*/

#define MinProb		4.494317e-07
#define IMin(a,b)		((a) < (b) ? (a) : (b))

#ifndef CSegmentDelimiter
#define CSegmentDelimiter       '/'
#endif

#define	NSizeSegmentedBuffer	512 /* large enough for a segmented sentence */

static	Byte	gbaFMMSegmentedBuffer[ NSizeSegmentedBuffer ] ;
static	Byte	gbaBMMSegmentedBuffer[ NSizeSegmentedBuffer ] ;
static	Byte	gbaReverseBMMSegmentedBuffer[ NSizeSegmentedBuffer ] ;
static	Byte	gbaFinalSegmentedBuffer[ NSizeSegmentedBuffer ] ;
static	PtrByte	gbpFinalSegmentedBuffer ; /* points into gbaFinalSegmentedBuffer */

/* the following are used in CopySegmentedWord() called by SegmentSentence () */
static	int	nByteNumberOfSentenceBuffer ;
static	PtrByte	bpSentenceBuffer ; 	/* points into gbaSentenceBuffer[] */
static	PtrByte	bpFMMSegmentedBuffer ;
static	PtrByte	bpBMMSegmentedBuffer ;

/*----------------------------------------------
	CopyFMMSegmentedWord()
----------------------------------------------*/
static	void	CopyFMMSegmentedWord( nByteOfWord )
	int	nByteOfWord ;
{
	strncpy( bpFMMSegmentedBuffer, bpSentenceBuffer, nByteOfWord );
	bpFMMSegmentedBuffer += nByteOfWord ;
	*bpFMMSegmentedBuffer ++ = CSegmentDelimiter ;

#ifdef  DEBUG
	*bpFMMSegmentedBuffer = '\0' ;
#endif/*DEBUG*/
	bpSentenceBuffer += nByteOfWord ;
	nByteNumberOfSentenceBuffer -= nByteOfWord ;
} /* CopyFMMSegmentedWord */

/*----------------------------------------------
	CopyBMMSegmentedWord()
----------------------------------------------*/
static	void	CopyBMMSegmentedWord( nByteOfWord )
	int	nByteOfWord ;
{
	strncpy( bpBMMSegmentedBuffer, bpSentenceBuffer, nByteOfWord );
	bpBMMSegmentedBuffer += nByteOfWord ;
	*bpBMMSegmentedBuffer ++ = CSegmentDelimiter ;

#ifdef  DEBUG
	*bpBMMSegmentedBuffer = '\0' ;
#endif/*DEBUG*/
	bpSentenceBuffer += nByteOfWord ;
	nByteNumberOfSentenceBuffer -= nByteOfWord ;
} /* CopyBMMSegmentedWord */

/*----------------------------------------------
	FMMSegmentSentence()
----------------------------------------------*/
static	void	FMMSegmentSentence( gbaSentenceBuffer )
	PtrByte		gbaSentenceBuffer ;
{
	int	nByteOfWord ;
	int	nHzCount ;
	PtrTrieNode	pTrieNode ;

	bpSentenceBuffer = gbaSentenceBuffer ;
	nByteNumberOfSentenceBuffer = strlen( gbaSentenceBuffer );
	bpFMMSegmentedBuffer = gbaFMMSegmentedBuffer ;
	*bpFMMSegmentedBuffer++ = CSegmentDelimiter ;
	while ( *bpSentenceBuffer ) {
		/* scan for the maximum no. of bytes of hanzi as a word:
		 */
		nByteOfWord = IMin( gnMaxByteOfHzFMM, nByteNumberOfSentenceBuffer );
		nHzCount = nByteOfWord / NHzBytes ;

		/* set up the queue of status of each hanzi in the word:
		 */
		(void) Dict_GetHzCiMaximalMatchQueue( ghandleDictFMM, bpSentenceBuffer,
							& nHzCount );
		if ( nHzCount <= 1 ) {
			/* == 0 => no match => 1st hanzi is a single-char word:
			 * == 1 => match a single-char word:
			 */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "no match %c%c\n", *bpSentenceBuffer, *(bpSentenceBuffer+1) );
	}
#endif/*DEBUG*/
			CopyFMMSegmentedWord( NHzBytes );
			continue ;
		}
		nByteOfWord = nHzCount * NHzBytes ;
				/* CopyFMMSegmentedWord at most so many bytes */

		/* examine from last hanzi in queue back to the start of the word:
		 */
		pTrieNode = Dict_Last_InMaximalMatchQueue();
		if ( pTrieNode == NULL ) {
			FatalError( "impossible Dict_Last_InMaximalMatchQueue error" );
		}
		if ( pTrieNode->ucNClass != 0 ) {
			/* Found last hanzi of a word to be segmented. */
			nByteOfWord = nHzCount * NHzBytes ;
			CopyFMMSegmentedWord( nByteOfWord );
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
			printf( "%-12s %2d nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
				, gbaFMMSegmentedBuffer
				, nByteOfWord
				, pTrieNode->ucNClass
				, pTrieNode->ulWordClasses
				, pTrieNode->fProb
				);
	}
#endif/*DEBUG*/
			continue;
		} /*if*/

		/* Last_InMaximalMatchQueue not last hanzi of a word,
		 * continue scanning towards the start of the word:
		 */
		for (;;) {
			nByteOfWord -= NHzBytes ;
			pTrieNode = Dict_Prev_InMaximalMatchQueue();
			if ( pTrieNode == NULL ) {
				/* No match of word in gnMaxByteOfHzFMM;
				 * assume *bpSentenceBuffer points to a single-char word;
				 * continue segmentation from next hanzi:
				 */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "no Prev_ %c%c\n", *bpSentenceBuffer, *(bpSentenceBuffer+1) );
	}
#endif/*DEBUG*/
				CopyFMMSegmentedWord( NHzBytes );
				break;
			}
			if ( pTrieNode->ucNClass != 0 ) {
				/* Found last hanzi of a word to be segmented. */
				CopyFMMSegmentedWord( nByteOfWord );
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "%-12s %2d nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
					, gbaFMMSegmentedBuffer
					, nByteOfWord
					, pTrieNode->ucNClass
					, pTrieNode->ulWordClasses
					, pTrieNode->fProb
					);
	}
#endif/*DEBUG*/
				break;
			}
		} /*for*/
	} /*while*/
	* bpFMMSegmentedBuffer = '\0' ;
#ifdef  DEBUG
	if ( giDebugLevel >= 2 ) {
		printf( "FMM segment: %s\n", gbaFMMSegmentedBuffer );
	}
#endif/*DEBUG*/
} /* FMMSegmentSentence */

/*----------------------------------------------
	BMMSegmentSentence()
----------------------------------------------*/
#ifdef  OBSOLETE
static	void	BMMSegmentSentence( gbaSentenceBuffer )
	PtrByte		gbaSentenceBuffer ;
{
	int	nByteOfWord ;
	int	nHzCount ;
	PtrTrieNode	pTrieNode ;

	bpSentenceBuffer = gbaSentenceBuffer ;
	nByteNumberOfSentenceBuffer = strlen( gbaSentenceBuffer );
	bpBMMSegmentedBuffer = gbaBMMSegmentedBuffer ;
	*bpBMMSegmentedBuffer++ = CSegmentDelimiter ;
	while ( *bpSentenceBuffer ) {
		/* scan for the maximum no. of bytes of hanzi as a word:
		 */
		nByteOfWord = IMin( gnMaxByteOfHzBMM, nByteNumberOfSentenceBuffer );
		nHzCount = nByteOfWord / NHzBytes ;

		/* set up the queue of status of each hanzi in the word:
		 */
		(void) Dict_GetHzCiMaximalMatchQueue( ghandleDictBMM, bpSentenceBuffer,
							& nHzCount );
		if ( nHzCount <= 1 ) {
			/* == 0 => no match => 1st hanzi is a single-char word:
			 * == 1 => match a single-char word:
			 */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "no match %c%c\n", *bpSentenceBuffer, *(bpSentenceBuffer+1) );
	}
#endif/*DEBUG*/
			CopyBMMSegmentedWord( NHzBytes );
			continue ;
		}

		/* examine from last hanzi in queue back to the start of the word:
		 */
		pTrieNode = Dict_Last_InMaximalMatchQueue();
		if ( pTrieNode == NULL ) {
			FatalError( "impossible Dict_Last_InMaximalMatchQueue error" );
		}
		if ( pTrieNode->ucNClass != 0 ) {
			/* Found last hanzi of a word to be segmented. */
			nByteOfWord = nHzCount * NHzBytes ;
			CopyBMMSegmentedWord( nByteOfWord );
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
			printf( "%-12s %2d nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
				, gbaBMMSegmentedBuffer
				, nByteOfWord
				, pTrieNode->ucNClass
				, pTrieNode->ulWordClasses
				, pTrieNode->fProb
				);
	}
#endif/*DEBUG*/
			continue;
		} /*if*/
		for (;;) {
			nByteOfWord -= NHzBytes ;
			pTrieNode = Dict_Prev_InMaximalMatchQueue();
			if ( pTrieNode == NULL ) {
				/* No match of word in gnMaxByteOfHzBMM;
				 * assume *bpSentenceBuffer points to a single-char word;
				 * continue segmentation from next hanzi:
				 */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "no Prev_ %c%c\n", *bpSentenceBuffer, *(bpSentenceBuffer+1) );
	}
#endif/*DEBUG*/
				CopyBMMSegmentedWord( NHzBytes );
				break;
			}
			if ( pTrieNode->ucNClass != 0 ) {
				/* Found last hanzi of a word to be segmented. */
				CopyBMMSegmentedWord( nByteOfWord );
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "%-12s %2d nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
					, gbaBMMSegmentedBuffer
					, nByteOfWord
					, pTrieNode->ucNClass
					, pTrieNode->ulWordClasses
					, pTrieNode->fProb
					);
	}
#endif/*DEBUG*/
				break;
			}
		} /*for*/
	} /*while*/
	* bpBMMSegmentedBuffer = '\0' ;
#ifdef  DEBUG
#else /*DEBUG*/
	if ( giDebugLevel >= 2 ) {
		printf( "BMM segment: %s\n", gbaBMMSegmentedBuffer );
	}
#endif/*DEBUG*/
} /* BMMSegmentSentence */
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

static	void	BMMSegmentSentence( gbaSentenceBuffer )
	PtrByte		gbaSentenceBuffer ;
{
	int	nByteOfWord ;
	int	nHzCount ;
	PtrTrieNode	pTrieNode ;

	bpSentenceBuffer = gbaSentenceBuffer ;
	nByteNumberOfSentenceBuffer = strlen( gbaSentenceBuffer );
	bpBMMSegmentedBuffer = gbaBMMSegmentedBuffer ;
	*bpBMMSegmentedBuffer++ = CSegmentDelimiter ;
	while ( *bpSentenceBuffer ) {
		/* scan for the maximum no. of bytes of hanzi as a word:
		 */
		nByteOfWord = IMin( gnMaxByteOfHzBMM, nByteNumberOfSentenceBuffer );
		nHzCount = nByteOfWord / NHzBytes ;

		/* set up the queue of status of each hanzi in the word:
		 */
		(void) Dict_GetHzCiMaximalMatchQueue( ghandleDictBMM, bpSentenceBuffer,
							& nHzCount );
		if ( nHzCount <= 1 ) {
			/* == 0 => no match => 1st hanzi is a single-char word:
			 * == 1 => match a single-char word:
			 */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "no match %c%c\n", *bpSentenceBuffer, *(bpSentenceBuffer+1) );
	}
#endif/*DEBUG*/
			CopyBMMSegmentedWord( NHzBytes );
			continue ;
		}
		nByteOfWord = nHzCount * NHzBytes ;
				/* CopyBMMSegmentedWord at most so many bytes */

		/* examine from last hanzi in queue back to the start of the word:
		 */
		pTrieNode = Dict_Last_InMaximalMatchQueue();
		if ( pTrieNode == NULL ) {
			FatalError( "impossible Dict_Last_InMaximalMatchQueue error" );
		}
		if ( pTrieNode->ucNClass != 0 ) {
			/* Found last hanzi of a word to be segmented. */
			nByteOfWord = nHzCount * NHzBytes ;
			CopyBMMSegmentedWord( nByteOfWord );
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
			printf( "%-12s %2d nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
				, gbaBMMSegmentedBuffer
				, nByteOfWord
				, pTrieNode->ucNClass
				, pTrieNode->ulWordClasses
				, pTrieNode->fProb
				);
	}
#endif/*DEBUG*/
			continue;
		} /*if*/

		/* Last_InMaximalMatchQueue not last hanzi of a word,
		 * continue scanning towards the start of the word:
		 */
		for (;;) {
			nByteOfWord -= NHzBytes ;
			pTrieNode = Dict_Prev_InMaximalMatchQueue();
			if ( pTrieNode == NULL ) {
				/* No match of word in gnMaxByteOfHzBMM;
				 * assume *bpSentenceBuffer points to a single-char word;
				 * continue segmentation from next hanzi:
				 */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "no Prev_ %c%c\n", *bpSentenceBuffer, *(bpSentenceBuffer+1) );
	}
#endif/*DEBUG*/
				CopyBMMSegmentedWord( NHzBytes );
				break;
			}
			if ( pTrieNode->ucNClass != 0 ) {
				/* Found last hanzi of a word to be segmented. */
				CopyBMMSegmentedWord( nByteOfWord );
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
				printf( "%-12s %2d nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
					, gbaBMMSegmentedBuffer
					, nByteOfWord
					, pTrieNode->ucNClass
					, pTrieNode->ulWordClasses
					, pTrieNode->fProb
					);
	}
#endif/*DEBUG*/
				break;
			}
		} /*for*/
	} /*while*/
	* bpBMMSegmentedBuffer = '\0' ;
#ifdef  DEBUG
	if ( giDebugLevel >= 2 ) {
		printf( "BMM segment: %s\n", gbaBMMSegmentedBuffer );
	}
#endif/*DEBUG*/
} /* BMMSegmentSentence */

void	str_ReverseSegmentedBuffer( bpBufferIn, bpBufferOut )
	PtrByte	bpBufferIn ;
	PtrByte	bpBufferOut ;
{
	PtrByte	bp ;
	PtrByte	bpEnd ;
	PtrByte	bpOut ;

	bp = (PtrByte) bpBufferIn ;
	bpEnd = (PtrByte) strchr( bpBufferIn, '\0' ) - 1 ;

	bpOut = (PtrByte) bpBufferOut ;

	while ( bp <= bpEnd ) {
		if ( *bpEnd == CSegmentDelimiter ) {
			*bpOut++ = CSegmentDelimiter ;
			*bpEnd-- ;
			continue ;
		}
		*bpOut++ = *(bpEnd-1) ;
		*bpOut++ = *bpEnd ;
		bpEnd -= 2 ;
	}
	*bpOut = '\0' ;
	/* debug:
	 */
#ifdef  DEBUG
	if ( giDebugLevel >= 2 ) {
		printf( "BMM reverse: %s\n", bpBufferOut );
	}
#endif/*DEBUG*/
} /* str_ReverseSegmentedBuffer */

/* IN bpStart and bpEnd point to the start and the end of a buffer
 *	which hold several null-terminated strings.
 * PRE: bpEnd point to '\0'.
 */
void	CopyAmbiguiousSegmentIntoFinalSegmentedBuffer( bpEnd, bpStart )
	PtrByte	bpEnd ;
	PtrByte	bpStart ;
{
	PtrByte	bp ;
	int	nSegment ;

	bp = bpStart ;
	while ( bp < bpEnd ) {
		nSegment = strlen( bp ) ;
		strncpy( gbpFinalSegmentedBuffer, bp, nSegment );
		gbpFinalSegmentedBuffer += nSegment ;
		*gbpFinalSegmentedBuffer++ = CSegmentDelimiter ;
		bp += nSegment + 1 ;	/* include the '\0' */
	}
} /* CopyAmbiguiousSegmentIntoFinalSegmentedBuffer */

/* IN bp1 & bp2 point to first hanzi of segments, terminated by CSegmentDelimiter,
 *    probably with CSegmentDelimiter in between;
 *	e.g. bp1 points to "hzhz/hz/"
 *	     bp2 points to "hz/hzhz/"
 */
ResolveBetweenUnmatchSegments( bp1, bp2 )
	PtrByte	bp1 ;
	PtrByte	bp2 ;
{
	int		nClass ;
	unsigned long	ulWordClasses ;
	float		fProb ;
	float		fProbProduct1 ;
	float		fProbProduct2 ;
	PtrByte		bp1Save ;
	PtrByte		bp2Save ;
	PtrByte		bp1Next ;
	PtrByte		bp2Next ;

#ifdef  DEBUG
			if ( giDebugLevel >= 6 ) {
				printf( "resolve: %s -- %s\n", bp1, bp2 );
			}
#endif/*DEBUG*/
	bp1Save = bp1 ;
	bp2Save = bp2 ;

	/* Convert all CSegmentDelimiter inside the given strings to '\0':
	 */
	fProbProduct1 = 1.0 ;
	for (;;) {
		bp1Next = (PtrByte) strchr( bp1, CSegmentDelimiter );
		if ( bp1Next != NULL ) {
			*bp1Next = '\0' ;
			/* forward search ghandleDictFMM for the word bp1:
			 * If single-char word, Dict_GetHzCiInfo may return
			 * Dict_OK but with fProb == 0;
			 * thus test fProb == 0 directly.
			 */
			(void) Dict_GetHzCiInfo( ghandleDictFMM,
				bp1, strlen( bp1 ) / NHzBytes,
				& nClass, & ulWordClasses, & fProb );
			if ( fProb == 0 ) {
				fProb = MinProb ;
			}
			fProbProduct1 *= fProb ;
			bp1 = bp1Next + 1 ;	/* skip CSegmentDelimiter */
		} else {
			break;
		}
	} /*loop*/

	fProbProduct2 = 1.0 ;
	for (;;) {
		bp2Next = (PtrByte) strchr( bp2, CSegmentDelimiter );
		if ( bp2Next != NULL ) {
			*bp2Next = '\0' ;
			/* forward search ghandleDictFMM for the word bp2:
			 * If single-char word, Dict_GetHzCiInfo may return
			 * Dict_OK but with fProb == 0;
			 * thus test fProb == 0 directly.
			 */
			(void) Dict_GetHzCiInfo( ghandleDictFMM,
				bp2, strlen( bp2 ) / NHzBytes,
				& nClass, & ulWordClasses, & fProb );
			if ( fProb == 0 ) {
				fProb = MinProb ;
			}
			fProbProduct2 *= fProb ;
			bp2 = bp2Next + 1 ;	/* skip CSegmentDelimiter */
		} else {
			break;
		}
	} /*loop*/

	if ( fProbProduct1 > fProbProduct2 ) {
		CopyAmbiguiousSegmentIntoFinalSegmentedBuffer( bp1, bp1Save );
	} else {
		CopyAmbiguiousSegmentIntoFinalSegmentedBuffer( bp2, bp2Save );
	}

#ifdef  DEBUG
			if ( giDebugLevel >= 6 ) {
				*gbpFinalSegmentedBuffer = '\0' ;
				printf( "Resolved %s ", gbaFinalSegmentedBuffer );
				printf( "prob1 %e prob2 %e\n", fProbProduct1, fProbProduct2 );
			}
#endif/*DEBUG*/
} /* ResolveBetweenUnmatchSegments */

/* Compare gbaFMMSegmentedBuffer and gbaReverseBMMSegmentedBufer
 * and resolve ambiguity:
 */
void	CompareFMMBMMToGetFinalSegmentedBuffer()
{
	/* Name Convention: xxx1 related to FMM stuff; xxx2 related to BMM stuff.
	 */
	PtrByte	bp1 ;
	PtrByte	bp2 ;
	PtrByte	bp1Save ;
	PtrByte	bp2Save ;
	PtrByte	bp1LastSegmentStart ;
	PtrByte	bp2LastSegmentStart ;
	modc_Bool	hasUnequalLengthSegment ;
	int	nSegment1 ;
	Byte	bSave1 ;
	Byte	bSave2 ;
	int	nByteInAmbString1 ;
	int	nByteInAmbString2 ;

	/* Remember start of string:
	 */
	gbpFinalSegmentedBuffer = gbaFinalSegmentedBuffer ;  /* + 1 to skip CSegmentDelimiter */
	*gbpFinalSegmentedBuffer++ = CSegmentDelimiter ;

	bp1 = gbaFMMSegmentedBuffer + 1 ;    /* + 1 to skip CSegmentDelimiter */
	bp2 = gbaReverseBMMSegmentedBuffer + 1 ; /* + 1 to skip CSegmentDelimiter */
#ifdef  DEBUG
	if ( giDebugLevel >= 3 ) {
		printf( "compare FMM: %s\n", gbaFMMSegmentedBuffer );
	}
#endif/*DEBUG*/
	while ( *bp1 ) {
		/* ASSERT: bp1 & bp2 point to immediately after CSegmentDelimiter
		 *	   in buffers.
		 */

		/* Remember start of possible ambiguous segments:
		 */
		bp1Save = bp1 ;
		bp2Save = bp2 ;

		/* Remember start of next segment:
		 */
		bp1LastSegmentStart = bp1 ;
		bp2LastSegmentStart = bp2 ;

		/* to find unmatched segments till their cumulative lengths
		 * are equal in both buffers:
		 * -- first move along AmbString1 to find CSegmentDelimiter,
		 *    then move along the shorter of AmbString1 & AmbString2.
		 */
		hasUnequalLengthSegment = FALSE ;	/* assume initially */
		nByteInAmbString1 = 0 ;
		while ( *bp1 != CSegmentDelimiter ) {
			bp1 ++ ;
		}
		nByteInAmbString1 += ( bp1 - bp1LastSegmentStart );
		bp1 ++ ;  /* + 1 to skip CSegmentDelimiter */
		bp1LastSegmentStart = bp1 ;

		nByteInAmbString2 = 0 ;
		while ( *bp2 != CSegmentDelimiter ) {
			bp2 ++ ;
		}
		nByteInAmbString2 += ( bp2 - bp2LastSegmentStart );
		bp2 ++ ;  /* + 1 to skip CSegmentDelimiter */
		bp2LastSegmentStart = bp2 ;

		while ( nByteInAmbString1 != nByteInAmbString2 ) {
			hasUnequalLengthSegment = TRUE ;

			/* to make both bp1 & bp2 point at next CSegmentDelimiter:
			 */
			if ( nByteInAmbString1 < nByteInAmbString2 ) {
				while ( *bp1 != CSegmentDelimiter ) {
					bp1 ++ ;
				}
				nByteInAmbString1 += ( bp1 - bp1LastSegmentStart );
				bp1 ++ ;  /* + 1 to skip CSegmentDelimiter */
				bp1LastSegmentStart = bp1 ;
			} else if ( nByteInAmbString1 > nByteInAmbString2 ) {
				while ( *bp2 != CSegmentDelimiter ) {
					bp2 ++ ;
				}
				nByteInAmbString2 += ( bp2 - bp2LastSegmentStart );
				bp2 ++ ;  /* + 1 to skip CSegmentDelimiter */
				bp2LastSegmentStart = bp2 ;
			/* } else {
				( nByteInAmbString1 == nByteInAmbString2 ) */
			}

		} /*while*/
		/* ASSERT: bp1 & bp2 point to immediately after CSegmentDelimiter
		 *	   in buffers.
		 */

		if ( hasUnequalLengthSegment ) {
			/* make the unmatched segments in the form of
			 * "hzhz/.../hz/" before ResolveBetweenUnmatchSegments:
			 *-- easier to handle in ResolveBetweenUnmatchSegments
			 * than make it of the form "hzhz/.../hz" (without
			 * CSegmentDelimiter '/' at the end of string).
			 */
			bSave1 = *bp1 ;
			bSave2 = *bp2 ;
			*bp1 = '\0' ;
			*bp2 = '\0' ;
			ResolveBetweenUnmatchSegments( bp1Save, bp2Save );
			*bp1 = bSave1 ;
			*bp2 = bSave2 ;

		} else {
			/* Equal lengths of segments means same segment;
			 * copy segment to gbaFinalSegmentedBuffer:
			 */
			nSegment1 = ( bp1 - 1 ) - bp1Save ;
			strncpy( gbpFinalSegmentedBuffer, bp1Save, nSegment1 );
			gbpFinalSegmentedBuffer += nSegment1 ;
#ifdef  DEBUG
				if ( giDebugLevel >= 6 ) {
					*gbpFinalSegmentedBuffer = '\0' ;
					printf( "SameSeg- %s\n", gbaFinalSegmentedBuffer );
				}
#endif/*DEBUG*/
			*gbpFinalSegmentedBuffer++ = CSegmentDelimiter ;
		}
	} /*while*/
	*gbpFinalSegmentedBuffer = '\0' ;
#ifdef  DEBUG
	if ( giDebugLevel >= 1 ) {
		printf( "FinalSegment %s\n", gbaFinalSegmentedBuffer );
	}
#endif/*DEBUG*/
} /* CompareFMMBMMToGetFinalSegmentedBuffer */

/*----------------------------------------------
	SegmentSentence()
----------------------------------------------*/
	/*
	 * RETURN gbaFinalSegmentedBuffer, storing a string of segmented words,
	 *	delimited at start and end, and separated by CSegmentDelimiter.
	 */
PtrByte	SegmentSentence( gbaSentenceBuffer )
	PtrByte		gbaSentenceBuffer ;
{
	int	nByteOfWord ;
	int	nHzCount ;

	IF *gbaSentenceBuffer == '\0' THEN
		*gbpFinalSegmentedBuffer = '\0' ;
		return	gbaFinalSegmentedBuffer ;
	ENDI

	FMMSegmentSentence( gbaSentenceBuffer );
	str_ReverseBuffer( gbaSentenceBuffer );
	BMMSegmentSentence( gbaSentenceBuffer );

	str_ReverseSegmentedBuffer( gbaBMMSegmentedBuffer, gbaReverseBMMSegmentedBuffer );

	CompareFMMBMMToGetFinalSegmentedBuffer();
	return	gbaFinalSegmentedBuffer ;
} /* SegmentSentence */

#ifdef  MAIN

#define	DefaultFMMDICTFileName	"/tmp/DICT.FMM"
#define	DefaultBMMDICTFileName	"/tmp/DICT.BMM"

extern	HandleDict	Dict_LoadHzCiDictionary();
extern	int		Dict_MaxNumberofHzInWord();
static	char	buffer[ 512 ] ;

/* usage: argv[0] [ DICT.FMMname [ DICT.BMMname ] ]
 */
void	main( argc, argv )
	int	argc ;
	char *	argv[] ;
{
	int		nClass ;
	unsigned long	ulWordClasses ;
	float		fProb ;

	if ( argc >= 2 ) {
		ghandleDictFMM = Dict_LoadHzCiDictionary( argv[1] );
	} else {
		ghandleDictFMM = Dict_LoadHzCiDictionary( DefaultFMMDICTFileName );
	}
	if ( argc >= 3 ) {
		ghandleDictBMM = Dict_LoadHzCiDictionary( argv[2] );
	} else {
		ghandleDictBMM = Dict_LoadHzCiDictionary( DefaultBMMDICTFileName );
	}
	if ( argc > 3 ) {
		giDebugLevel = atoi( argv[2] );
	}

	gnMaxByteOfHzFMM = NHzBytes * Dict_MaxNumberofHzInWord( ghandleDictFMM );
	gnMaxByteOfHzBMM = NHzBytes * Dict_MaxNumberofHzInWord( ghandleDictBMM );

	Dict_InitMaximalMatchQueue();

	for(;;){
		printf( "input hanzi phrase: " );
		gets( buffer );
#ifdef  OBSOLETE
		if ( Dict_GetHzCiInfo( handleDictFMM, buffer, strlen( buffer ) / 2,
			& nClass, & ulWordClasses, & fProb ) == Dict_NOT_OK ) {
			printf( "not found in dictionary\n" );
		} else {
			printf( "nClass= %2d ulWordClasses= 0x%-.8x nProb=%e\n"
				, nClass
				, ulWordClasses
				, fProb
				);
		}
#else /*OBSOLETE*/
#endif/*OBSOLETE*/
		SegmentSentence( buffer );
	} /*for*/
} /*main*/
#else /*MAIN*/
#endif/*MAIN*/
