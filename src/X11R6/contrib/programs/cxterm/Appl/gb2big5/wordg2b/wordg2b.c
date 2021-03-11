/*==========================================================================
	wordg2b.c

	Author(s):
		Man-Chi Pong mcpong@cs.ust.hk	19940124 (Mon)
	Version(s):
		1.0 (19940124 (Mon))
	Function:
		Read CharDictionary,
		Read WordDictionary,
		Read input GB file,
		Convert to output B5 file.
	Input:
		CharDictionary,
		WordDictionary,
		input GB file,
	Output:
		output B5 file.
	Usage:
		wordg2b [-h] [-D giDebugLevel] [-i NameFileIn] [-o NameFileOut]
			[-c NameCharDict] [-w NameWordDict]
			[-p NamePrefixWordBigramDict]
			[-s NameSuffixWordBigramDict]
			[-f NameFMMDICT]
			[-b NameBMMDICT]

			'NameFileIn' is the input GB file to be converted;
			'NameFileOut' is the output converted Big5 file;
			'NameCharDict' is the GB-B5 CharDictionary;
			'NameWordDict' is the GB-B5 WordDictionary;
			'NamePrefixWordBigramDict' is the GB-B5 WordBigramDictionary
				with single-char-word as prefix of bigram;
			'NameSuffixWordBigramDict' is the GB-B5 WordBigramDictionary
				with single-char-word as suffix of bigram;
	printf( "-D giDebugLevel => display debug information according to the DebugLevel number.\n");

	Make:
		cc wordg2b.c -o wordg2b
	
	History:
		Man-Chi Pong mcpong@cs.ust.hk	19940124 (Mon) first created
		Man-Chi Pong mcpong@cs.ust.hk	19940427 (Wed)
			use DICT.FMM & DICT.BMM
	See also:
		./README
===========================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <search.h>
#include "modularC.h"
#include "DICT.h"

/*----------------------------------------------
	types
----------------------------------------------*/
#ifdef  BMMFMM
#else /*BMMFMM*/
typedef unsigned char	Byte, * PtrByte ;
#endif/*BMMFMM*/

/* Hash Table as CharDict Table:
 */
typedef struct {
	Byte	baGBchar[2] ;
	Byte	baB5char[2] ;
} CharDictEntry, * PtrCharDictEntry ;

/* a separate int gipFlagOfAmbiguousGBChar[ NHzHashTable / sizeof(int) ] ;
 * with each bit representing one code-point of GB s.t.
 *	bit = '1' => an ambiguous GB char;
 *	bit = '0' => non-ambiguous.
 */

/* Binary Search Table as WordDict Table:
 */
typedef struct {
	PtrByte	bpGBword ;
	PtrByte	bpB5word ;
} WordDictEntry, * PtrWordDictEntry ;

/* Binary Search Table as WordBigramDictEntry Table:
 */
typedef struct {
	PtrByte	bpGBword1 ;
	PtrByte	bpGBword2 ;
	PtrByte	bpB5word1 ;
	PtrByte	bpB5word2 ;
} WordBigramDictEntry, * PtrWordBigramDictEntry ;

/*----------------------------------------------
	defaults & macros
----------------------------------------------*/

#ifdef  BMMFMM
#define	NameBMMDICT	"DICT.BMM"
#define	NameFMMDICT	"DICT.FMM"
#endif/*BMMFMM*/

#define	EnvNameDictPath	"WORDG2B_PATH"

#define	WordDictVersionString	"#g2bword.1.0"

#define	NameFileIn	"wordg2b.in"
#define	NameFileOut	"wordg2b.out"

#define	NameCharDict	"g2bchar.tab"
#define	NameWordDict	"g2bword.tab"

#define	NamePrefixWordBigramDict	"prefix.scw.bigram.tab"
#define	NameSuffixWordBigramDict	"suffix.scw.bigram.tab"

#define	MaxCharPerLine	4096	/* assume < MaxCharPerLine char per input line */

#define IsNotGBCode(cp)		( (Byte) *(cp) < (Byte) 0xa1 )   /* 0xa1a1 1st GB symbol */
#define IsGBCode(cp)    	( (Byte) *(cp) >= (Byte) 0xa1 )

#define IsNotGBHanzi(cp)        ( (Byte) *(cp) < (Byte) 0xb0 )   /* 0xb0a1 1st GBHanzi */
#define IsGBHanzi(cp)   	( (Byte) *(cp) >= (Byte) 0xb0 )

#define IsNotB5Code(cp) 	( (Byte) *(cp) < (Byte) 0xa0 )   /* 0xa140 1st B5 symbol */
#define IsB5Code(cp)    	( (Byte) *(cp) >= (Byte) 0xa0 )

#define IsNotB5Hanzi(cp)        ( (Byte) *(cp) < (Byte) 0xa4 )   /* 0xa440 1st B5Hanzi */
#define IsB5Hanzi(cp)   	( (Byte) *(cp) >= (Byte) 0xa4 )


#define	HashOneHz(pHz)  ( ( ( *(pHz) & 0x7f ) << 8 ) | *(pHz+1) )
#define	NHzHashTable	32768	/* if not & 0x7f, then 65536 */

#define	NCharDictTable	NHzHashTable
#define	NCharDictFlag	(NHzHashTable / sizeof(int) )

#ifndef	CSegmentDelimiter
#define	CSegmentDelimiter	'/'
#endif

#define	StringDirectoryDelimiter	"/"

#define	LargeBufferSize	1024

/*----------------------------------------------
	globals
----------------------------------------------*/

#ifdef  BMMFMM
	HandleDict      ghandleDictFMM ;
	HandleDict      ghandleDictBMM ;
	int     gnMaxByteOfHzFMM ;
	int     gnMaxByteOfHzBMM ;
	char	caBMMDICTFileName[128] =	NameBMMDICT ;
	char	caFMMDICTFileName[128] =	NameFMMDICT ;
#endif/*BMMFMM*/
	char	caNameFileIn[128] =	NameFileIn ;
	char	caNameFileOut[128] =	NameFileOut ;
	char	caNameCharDict[128] =	NameCharDict ;
	char	caNameWordDict[128] =	NameWordDict ;
	char	caNamePrefixWordBigramDict[128] = NamePrefixWordBigramDict ;
	char	caNameSuffixWordBigramDict[128] = NameSuffixWordBigramDict ;
	FILE *	fFileIn ;	/* file pointer to input file of Hz text corpus */
	FILE *	fFileOut ;

	char *	gcpNameDictPath ;
	char *	gcpCurrentWorkDir = "." ;

	int	giDebugLevel = 0 ;

	/* for input handling -- used in GetNextHz() & GetLine() :
	 */
static	char    gcaLine[ MaxCharPerLine ];

static	char	gcStartHanzi = '/' ;	/* default value */

	/* Char Dictionary -- dynamically allocated :
	 */
	int *	gipFlagOfAmbiguousGBChar ;	/* array of flags of "Ambiguity".
						 * See typedef of CharDictEntry.
						 */

	PtrCharDictEntry	gpCharDictEntry ;
	int	gnCharDictEntry = 0 ; 

	/* Word Dictionary -- dynamically allocated :
	 */
	PtrWordDictEntry	gpWordDictEntry ;
	int	gnWordDictEntry = 0 ; 

	/* PrefixWordBigramDictionary -- dynamically allocated :
	 */
	PtrWordBigramDictEntry	gpPrefixWordBigramDictEntry ;
	int	gnPrefixWordBigramDictEntry = 0 ; 

	/* SuffixWordBigramDictionary -- dynamically allocated :
	 */
	PtrWordBigramDictEntry	gpSuffixWordBigramDictEntry ;
	int	gnSuffixWordBigramDictEntry = 0 ; 

	/* for WordBigram analysis:
	 */
	PtrByte	gbpNullString = (PtrByte) "" ;	/* constant string */

#ifdef  DEBUG
	/* char type such that display gbaWord1 in dbx in cxterm
	 * can show hanzi
	 */
	char	gbaWord1[40] ;	/* less than 20 hanzi/word in Dict */
	char	gbaWord2[40] ;	/* less than 20 hanzi/word in Dict */
	PtrByte	gbpWord1 = (PtrByte) & gbaWord1[0] ;
	PtrByte	gbpWord2 = (PtrByte) & gbaWord2[0] ;
#else /*DEBUG*/
	Byte	gbaWord1[40] ;	/* less than 20 hanzi/word in Dict */
	Byte	gbaWord2[40] ;	/* less than 20 hanzi/word in Dict */
	PtrByte	gbpWord1 = & gbaWord1[0] ;
	PtrByte	gbpWord2 = & gbaWord2[0] ;
#endif/*DEBUG*/

static	Byte	gbaSentenceBuffer[ LargeBufferSize ] ;
static	Byte	gbaInputBuffer[ LargeBufferSize ] ;
static	PtrByte	gbpInputBuffer = & gbaInputBuffer[0] ;
static	modc_Bool	gIsEndOfInputFile ;
	PtrByte gbpStartSegmented ;	/* Start of Segmented sentence buffer */
	PtrByte gbpCurrentSegmented ;
	PtrByte gbpLastSegmented ;


#ifdef  DEBUG
	int	gnWordDictConvert = 0 ;
	PtrCharDictEntry	gpcde ;
	PtrWordDictEntry	gpwde ;

	int	gnLineInput = 0 ;
	int	gnSentenceWithNoAmbiguousHz = 0 ;
	int	gnWordWithNoAmbiguousHz = 0 ;
	int	gn1CharWordWithAmbiguousHz = 0 ;
	int	gnNCharWordWithAmbiguousHz = 0 ;
	int	gnSuffixConvert = 0 ;
	int	gnPrefixConvert = 0 ;
#endif/*DEBUG*/

/*----------------------------------------------
	extern
----------------------------------------------*/

extern	PtrByte	SegmentSentence( /* gbaSentenceBuffer */ );

/*----------------------------------------------
	FUNCTIONS
----------------------------------------------*/

/* forward */
	void	SetFlagOfAmbiguousGBCharGivenHashAddress( /* iHash */ );

/*----------------------------------------------
	command_help()
----------------------------------------------*/
#ifdef  DEBUG
static	DebugString( cp )
	char *	cp ;
{
	printf( "%s\n", cp );
} /* DebugString */
#endif/*DEBUG*/

static	void	command_help()
{
	printf( "usage:\n");
	printf( "wordg2b [-h] [-i NameFileIn] [-o NameFileOut]\n" );
	printf( "        [-b NameBMMDICT] [-f NameFMMDICT]\n" );
	printf( "        [-c NameCharDict] [-w NameWordDict ]\n" );
	printf( "        [-p NamePrefixWordBigramDict] [-s NameSuffixWordBigramDict ]\n" );

	printf( "    -h  => display help message.\n");
	printf( "input files:\n");
	printf( "	'NameFileIn' is the input GB file to be converted;\n" );
	printf( "	'NameBMMDICT' is the dictionary file for backward maximal matching\n" );
	printf( "	'NameFMMDICT' is the dictionary file for forward  maximal matching\n" );
	printf( "	'NameCharDict' is the GB-B5 CharDictionary;\n" );
	printf( "	'NameWordDict' is the GB-B5 WordDictionary;\n" );
	printf( "	'NamePrefixWordBigramDict' is the GB-B5 WordBigramDictionary;\n" );
	printf( "		   with single-char-word as prefix of bigram;\n" );
	printf( "	'NameSuffixWordBigramDict' is the GB-B5 WordBigramDictionary\n" );
	printf( "		   with single-char-word as suffix of bigram;\n" );
	printf( "output file:\n");
	printf( "	'NameFileOut' is the converted output Big5 file\n" );
	printf( "default:\n");
	printf( "	'NameFileIn'  =  '%s'\n", NameFileIn );
	printf( "	'NameFileOut' =  '%s'\n", NameFileOut );
	printf( "	'NameBMMDICT' =  '%s'\n", NameBMMDICT );
	printf( "	'NameFMMDICT' =  '%s'\n", NameFMMDICT );
	printf( "	'NameCharDict'=  '%s'\n", NameCharDict );
	printf( "	'NameWordDict'=  '%s'\n", NameWordDict );
	printf( "	'NamePrefixWordBigramDict'=  '%s'\n", NamePrefixWordBigramDict );
	printf( "	'NameSuffixWordBigramDict'=  '%s'\n", NameSuffixWordBigramDict );
	printf( "You may \"setenv WORDG2B_PATH <directory-of-dictionaries>\"\n" );
	exit(0);
} /* command_help */

/*----------------------------------------------
	FatalError()
----------------------------------------------*/
static	void	FatalError( message )
	char *	message ;
{
	fprintf( stderr, "%s\n", message );
	abort();
	exit( 1 );
} /* FatalError */


static	void	SetFlagOfAmbiguousGBCharGivenHashAddress( iHash )
	int	iHash ;
/*
 * Set the flag of the ambiguous char corresponding to the GivenHashAddress.
 */
{
	int	iWord ;
	int	iFlagInWord ;
	int	result ;

	iWord = iHash / sizeof( int );
	iFlagInWord = iHash % sizeof( int );
	gipFlagOfAmbiguousGBChar[ iWord ] |= ( 1 << iFlagInWord );
} /* SetFlagOfAmbiguousGBCharGivenHashAddress */

static	modc_Bool	IsAmbiguousCharGivenHashAddress( iHash )
	int	iHash ;
/*
 * RETURN TRUE if the char corresponding to the GivenHashAddress is ambiguous.
 */
{
	int	iWord ;
	int	iFlagInWord ;
	int	result ;

	iWord = iHash / sizeof( int );
	iFlagInWord = iHash % sizeof( int );
	result = gipFlagOfAmbiguousGBChar[ iWord ] & ( 1 << iFlagInWord ) ;
	return	(modc_Bool) result ;
} /* IsAmbiguousCharGivenHashAddress */

static	modc_Bool	IsAmbiguousBuffer( bpBuffer )
	PtrByte	bpBuffer ;
/*
 * RETURN TRUE if bpBuffer has ambiguous GBchar.
 */
{
	PtrByte	bpGB ;
	int	iHash ;
	
	/* scan thru' the word, to see if any hanzi is ambiguous:
	 */
	bpGB = bpBuffer ;
	WHILE *bpGB != '\0' DO
		iHash = HashOneHz( bpGB );
		IF IsAmbiguousCharGivenHashAddress( iHash ) THEN
			return	TRUE ;
		ENDI
		bpGB += 2 ;
	ENDW
	return	FALSE ;
} /* IsAmbiguousBuffer */

static	int	CompareHanziString( bpLeft, bpRight )
	PtrByte bpLeft ;
	PtrByte bpRight ;
{

	WHILE *bpLeft && *bpRight &&
	      *bpLeft == *bpRight DO
		bpLeft  ++ ;
		bpRight ++ ;
	ENDW
	return	(int) *bpLeft - (int) *bpRight ;
} /* CompareHanziString */

static	int	CompareWord( pWordDictEntry1, pWordDictEntry2 )
	PtrWordDictEntry	pWordDictEntry1 ;
	PtrWordDictEntry	pWordDictEntry2 ;
/*
 * Used in bsearch() of WordDict.
 * Don't use strcmp() in place of CompareWord()
 * because strcmp() is for ASCII character set
 *	and don't know if it works for 8-bit or not.
 */
{
	PtrByte bpLeft ;
	PtrByte bpRight ;

	bpLeft  = pWordDictEntry1->bpGBword ;
	bpRight = pWordDictEntry2->bpGBword ;
	return	CompareHanziString( bpLeft, bpRight );
} /* CompareWord */

static	int	CompareWordBigram( pWordBigramDictEntry1, pWordBigramDictEntry2 )
	PtrWordBigramDictEntry	pWordBigramDictEntry1 ;
	PtrWordBigramDictEntry	pWordBigramDictEntry2 ;
/*
 * Used in bsearch() of WordBigramDict.
 * Don't use strcmp() in place of CompareWordBigram()
 * because strcmp() is for ASCII character set
 *	and don't know if it works for 8-bit or not.
 */
{
	PtrByte bp1 ;
	PtrByte bp2 ;
	int	iResult ;

	bp1 = pWordBigramDictEntry1->bpGBword1 ;
	bp2 = pWordBigramDictEntry2->bpGBword1 ;
	iResult = CompareHanziString( bp1, bp2 );
	IF iResult != 0 THEN
		return	iResult ;
	ENDI

	bp1 = pWordBigramDictEntry1->bpGBword2 ;
	bp2 = pWordBigramDictEntry2->bpGBword2 ;
	return	CompareHanziString( bp1, bp2 );
} /* CompareWordBigram */

static	PtrWordDictEntry	MatchWordDict( bpBuffer )
	PtrByte bpBuffer ;
/*
 * RETURN NULL if the word in the bpBuffer is not matched in WordDict;
 *   ELSE the WordDictEntry if matched.
 */
{
	PtrWordDictEntry	pWordDictEntry ;
	WordDictEntry	rWordDictEntry ;

	rWordDictEntry.bpGBword = bpBuffer ;

	pWordDictEntry = (PtrWordDictEntry) bsearch(
				(char *)  & rWordDictEntry,
				(char *)   gpWordDictEntry,
				(unsigned) gnWordDictEntry,
				     sizeof( WordDictEntry ),
				     CompareWord
				);
	return	pWordDictEntry ;
} /* MatchWordDict */

static	PtrWordBigramDictEntry	MatchSuffixWordBigramDict( bpWord2 )
	PtrByte bpWord2 ;
/*
 * RETURN NULL if the pair of given words is not matched in WordDict;
 *   ELSE the WordBigramDictEntry if matched.
 */
{
	PtrWordBigramDictEntry	pWordBigramDictEntry ;
	WordBigramDictEntry	rWordBigramDictEntry ;

	rWordBigramDictEntry.bpGBword1 = gbpWord1 ;
	rWordBigramDictEntry.bpGBword2 = bpWord2 ;

	pWordBigramDictEntry = (PtrWordBigramDictEntry) bsearch(
				(char *)        & rWordBigramDictEntry,
				(char *)   gpSuffixWordBigramDictEntry,
				(unsigned) gnSuffixWordBigramDictEntry,
					   sizeof( WordBigramDictEntry ),
					   CompareWordBigram
				);
	return	pWordBigramDictEntry ;
} /* MatchSuffixWordBigramDict */

static	PtrWordBigramDictEntry	MatchPrefixWordBigramDict( bpWord1 )
	PtrByte bpWord1 ;
/*
 * RETURN NULL if the pair of given words is not matched in WordDict;
 *   ELSE the WordBigramDictEntry if matched.
 */
{
	PtrWordBigramDictEntry	pWordBigramDictEntry ;
	WordBigramDictEntry	rWordBigramDictEntry ;

	rWordBigramDictEntry.bpGBword1 = bpWord1 ;
	rWordBigramDictEntry.bpGBword2 = gbpWord2 ;

	pWordBigramDictEntry = (PtrWordBigramDictEntry) bsearch(
				(char *)        & rWordBigramDictEntry,
				(char *)   gpPrefixWordBigramDictEntry,
				(unsigned) gnPrefixWordBigramDictEntry,
					   sizeof( WordBigramDictEntry ),
					   CompareWordBigram
				);
	return	pWordBigramDictEntry ;
} /* MatchPrefixWordBigramDict */

static	PtrByte	GB2B5char( bpGB )
	PtrByte	bpGB ;
{
	int	iHash ;
	PtrByte	bpResult ;
	static	Byte	baB5charNotConverted[ 2 ] = { 0xa1, 0xbc }; /* blank square symbol */

	iHash = HashOneHz( bpGB );
	bpResult = gpCharDictEntry[ iHash ].baB5char ;
	IF *bpResult THEN
		return bpResult ;
	ELSE
		return baB5charNotConverted ;
	ENDI
} /* GB2B5char */

static	void	ConvertHanziByHanzi( bpBuffer )
	PtrByte	bpBuffer ;
/*
 * convert hanzi by hanzi in the bpBuffer.
 */
{
	PtrByte	bpGB ;
	PtrByte	bpB5 ;

#ifdef  DEBUG
	gnWordWithNoAmbiguousHz ++ ;
#else /*DEBUG*/
#endif/*DEBUG*/

	bpGB = bpBuffer ;
	WHILE *bpGB != '\0' DO
		bpB5 = GB2B5char( bpGB );
		fprintf( fFileOut, "%c%c", *bpB5, *(bpB5+1) );
		bpGB += 2 ;
	ENDW
} /* ConvertHanziByHanzi */

static	void	ConvertMultiCharWord( bpBuffer )
	PtrByte	bpBuffer ;
/*
 * if word in bpBuffer is found in WordDict then
 *	convert word
 * else
 *	convert hanzi by hanzi.
 */
{
	PtrByte	bpGB ;
	PtrByte	bpB5 ;
	PtrWordDictEntry pWordDictEntry ;

	pWordDictEntry = MatchWordDict( bpBuffer );
	IF pWordDictEntry != NULL THEN
#ifdef  DEBUG
		gnWordDictConvert ++ ;
#endif/*DEBUG*/
		fprintf( fFileOut, "%s", pWordDictEntry->bpB5word );
		return;
	ENDI

	ConvertHanziByHanzi( bpBuffer );
} /* ConvertMultiCharWord */


static	modc_Bool	ConvertSuffixSingleCharWordIfPossible( bpGB )
	PtrByte	bpGB ;
/*
 * if word in bpBuffer is found in SuffixWordBigramDict then
 *	convert word
 *	RETURN TRUE
 * else
 *	RETURN FALSE => SingleCharWord not yet converted
 */
{
	PtrWordBigramDictEntry	pWordBigramDictEntry ;
	PtrByte	bpB5 ;

#ifdef  OBSOLETE
	return	FALSE ;
#else /*OBSOLETE*/
	/* test -- dummy action -- simply return */
#endif/*OBSOLETE*/

	pWordBigramDictEntry = MatchSuffixWordBigramDict( bpGB );
	IF pWordBigramDictEntry != NULL THEN
#ifdef  DEBUG
		gnSuffixConvert ++ ;
#endif/*DEBUG*/
		fprintf( fFileOut, "%s", pWordBigramDictEntry->bpB5word2 );
		return	TRUE ;
	ELSE
		return	FALSE ;
	ENDI
} /* ConvertSuffixSingleCharWordIfPossible */

static	modc_Bool	ConvertPrefixSingleCharWordIfPossible( bpGB )
	PtrByte	bpGB ;
/*
 * if word in bpBuffer is found in PrefixWordBigramDict then
 *	convert word
 *	RETURN TRUE
 * else
 *	RETURN FALSE => SingleCharWord not yet converted
 */
{
	PtrWordBigramDictEntry	pWordBigramDictEntry ;
	PtrByte	bpB5 ;

#ifdef  OBSOLETE
	return	FALSE ;
#else /*OBSOLETE*/
	/* test -- dummy action -- simply return */
#endif/*OBSOLETE*/

	pWordBigramDictEntry = MatchPrefixWordBigramDict( bpGB );
	IF pWordBigramDictEntry != NULL THEN
#ifdef  DEBUG
		gnPrefixConvert ++ ;
#endif/*DEBUG*/
		fprintf( fFileOut, "%s", pWordBigramDictEntry->bpB5word1 );
		return	TRUE ;
	ELSE
		return	FALSE ;
	ENDI
} /* ConvertPrefixSingleCharWordIfPossible */


	/* RETURN no. of hanzi's in the line got; thus 0 => no word is got.
	 */
static	int	GetLineIntoBuffer( bpBuffer )
	PtrByte bpBuffer ;
{
	PtrByte bp ;

	IF fgets( bpBuffer, MaxCharPerLine, fFileIn ) != NULL THEN
#ifdef  DEBUG
		gnLineInput ++ ;
#endif/*DEBUG*/
#ifdef  OBSOLETE
		/* replace (one and only one) '\n' by '\0':
		 */
		bp = (unsigned char *) strchr( bpBuffer, '\n' );
		*bp = '\0' ;
#else /*OBSOLETE*/
		/* keep the (one and only one) '\n' before '\0'. */
#endif/*OBSOLETE*/
		return	( (int) ( bp - bpBuffer ) ) / 2 ;
	ELSE
		gIsEndOfInputFile = TRUE ;
		*bpBuffer = '\0' ;
		return	0 ;
	ENDI
} /* GetLineIntoBuffer */

	/* ConvertWord() will convert given word if
	 *    unambiguous word
	 * or multi-char ambiguous word
	 * or single-char ambiguous word as 2nd word in SuffixWordBigramDict;
	 * RETURN FALSE only if single-char ambiguous word not yet converted.
	 */
static	modc_Bool	ConvertWord( bpBuffer, nHanziInWord )
	PtrByte bpBuffer ;
	int	nHanziInWord ;
{
	PtrByte bp ;
	modc_Bool	isWordWithNoAmbiguousHz ;
	modc_Bool	isConverted ;

	/* check if any ambiguous hanzi in word:
	 */
	isWordWithNoAmbiguousHz = TRUE ;	/* assume no ambiguous hanzi */
	bp = bpBuffer ;
	WHILE *bp != '\0' DO
		IF IsAmbiguousCharGivenHashAddress( HashOneHz( bp ) ) THEN
			isWordWithNoAmbiguousHz = FALSE ;
			break;
		ELSE
			bp += 2 ;
		ENDI
	ENDW

	isConverted = TRUE ;	/* assume can be converted */
	IF isWordWithNoAmbiguousHz THEN
#ifdef  DEBUG
		gnWordWithNoAmbiguousHz ++ ;
#endif/*DEBUG*/
		ConvertHanziByHanzi( bpBuffer );
	ELSE
		IF nHanziInWord > 1 THEN
#ifdef  DEBUG
			gnNCharWordWithAmbiguousHz ++ ;
#endif/*DEBUG*/
			ConvertMultiCharWord( bpBuffer );
		ELSE /* single char word */
#ifdef  DEBUG
			gn1CharWordWithAmbiguousHz ++ ;
#endif/*DEBUG*/
			isConverted = 
				ConvertSuffixSingleCharWordIfPossible( bpBuffer );
		ENDI
	ENDI
	return	isConverted ;
} /* ConvertWord */


static	int	GetWordIntoBuffer( bpBuffer )
	PtrByte	bpBuffer ;
/*
 * RETURN no. of hanzi in the word got
 * RETURN 0 => no word is got; and *gbpCurrentSegmented == '\0'.
 */
{
	modc_Bool	isLastWord ;
	int	nHanzi ;

	gbpLastSegmented = gbpCurrentSegmented ;
	gbpCurrentSegmented = (PtrByte) strchr( gbpCurrentSegmented,
					(int) CSegmentDelimiter );
	isLastWord = ( gbpCurrentSegmented == NULL );
	IF isLastWord THEN
		gbpCurrentSegmented = gbpLastSegmented + strlen( gbpLastSegmented );
	ELSE
		*gbpCurrentSegmented = '\0' ;
	ENDI
	strcpy( bpBuffer, gbpLastSegmented );
	nHanzi = ( (int) ( gbpCurrentSegmented - gbpLastSegmented ) ) / 2 ;
	IF isLastWord THEN
		/* *gbpCurrentSegmented == '\0' */
	ELSE
		gbpCurrentSegmented ++ ;	/* + 1 to skip '\0' */
	ENDI
	return	nHanzi ;
} /* GetWordIntoBuffer */

static	void	ConvertSentenceWordByWord()
{
	PtrByte	bp ;	 /* points to input buffer line from fFileIn */
	PtrByte	bpB5 ;
	int	nHanziInWord1 ;
	int	nHanziInWord2 ;
	modc_Bool	isPrevWordConverted ;

	/* read input line by line:
	gcaLine = fgets( gcaLine, MaxCharPerLine, fFileIn );
	gcaLine terminated by '\n'.
	 */
	nHanziInWord1 = GetWordIntoBuffer( gbpWord1 );
	IF nHanziInWord1 == 0 THEN
		/* no word needs to be converted */
		return;
	ENDI

	/* ConvertWord() will convert given word if
	 *    unambiguous word
	 * or multi-char ambiguous word
	 * or single-char ambiguous word as 2nd word in SuffixWordBigramDict;
	 * RETURN FALSE only if single-char ambiguous word not yet converted.
	 */
	isPrevWordConverted = ConvertWord( gbpWord1, nHanziInWord1 );

	LOOP
		nHanziInWord2 = GetWordIntoBuffer( gbpWord2 );
		IF nHanziInWord2 == 0 THEN
			break;
		ENDI
		/* ASSERT: gbpWord2 contains the newly got word. */

		IF ! isPrevWordConverted THEN
			/* consult PrefixWordBigramDict to convert PrevWord,
			 * else use ConvertHanziByHanzi:
			 */
			IF ! ConvertPrefixSingleCharWordIfPossible( gbpWord1 ) THEN
				bpB5 = GB2B5char( gbpWord1 );
				fprintf( fFileOut, "%c%c", *bpB5, *(bpB5+1) );
			ENDI
		ENDI

		isPrevWordConverted = ConvertWord( gbpWord2, nHanziInWord2 );
		strcpy( gbpWord1, gbpWord2 );
	ENDL
	IF ! isPrevWordConverted THEN
		bpB5 = GB2B5char( gbpWord1 );
		fprintf( fFileOut, "%c%c", *bpB5, *(bpB5+1) );
	ENDI
} /* ConvertSentenceWordByWord */

/*----------------------------------------------
	GetOneSentence
----------------------------------------------*/

/*
 * PRE:  gbpInputBuffer points to a hanzi, unless gIsEndOfInputFile == TRUE.
 * POST: gbaSentenceBuffer is filled with a sentence scanned in from InputFile.
 *
 *	A sentence is delimited by a non-GBHanzi.
 */
static	void	GetOneSentence()
{
	PtrByte	bpSentence ;
	modc_Bool	hasFoundGBSymbol ;
	
	bpSentence = gbaSentenceBuffer ;
	hasFoundGBSymbol = FALSE ;

LScanLine:
	IF gIsEndOfInputFile THEN
		*bpSentence = '\0' ;
		return ;
	ENDI

	WHILE *gbpInputBuffer DO
		IF IsNotGBHanzi( gbpInputBuffer ) THEN
			/* ether an ASCII char or an GB symbol */
			hasFoundGBSymbol = TRUE ;
			break;
		ENDI
		*bpSentence++ = *gbpInputBuffer++ ;
		*bpSentence++ = *gbpInputBuffer++ ;
	ENDW
	IF hasFoundGBSymbol THEN
		*bpSentence = '\0' ;
		return;
	ENDI

	GetLineIntoBuffer( gbaInputBuffer );
	gbpInputBuffer = gbaInputBuffer ;

	goto LScanLine;
} /* GetOneSentence */

/*----------------------------------------------
	ConvertNonGBHanzi
----------------------------------------------*/

/*
 *	Convert NonGBHanzi (which delimit sentences) to Big5.
 */
static	void	ConvertNonGBHanzi()
{
	PtrByte	bpB5 ;
	modc_Bool	hasFoundGBHanzi ;
	
	hasFoundGBHanzi = FALSE ;
LScanLine:
	IF gIsEndOfInputFile THEN
		return ;
	ENDI

	WHILE *gbpInputBuffer DO
		IF IsNotGBCode( gbpInputBuffer ) THEN
			fprintf( fFileOut, "%c", *gbpInputBuffer );
			gbpInputBuffer ++ ;
		ELIF IsNotGBHanzi( gbpInputBuffer ) THEN /* IsGBCode */
			bpB5 = GB2B5char(  gbpInputBuffer  );
			fprintf( fFileOut, "%c%c", *bpB5, *(bpB5+1) );
			gbpInputBuffer += 2 ;
		ELSE
			hasFoundGBHanzi = TRUE ;
			break;
		ENDI
	ENDW
	IF hasFoundGBHanzi THEN
		return;
	ENDI

	GetLineIntoBuffer( gbaInputBuffer );
	gbpInputBuffer = gbaInputBuffer ;

	goto LScanLine;
} /* ConvertNonGBHanzi */

static	PtrByte	GetHanziStringFromBuffer( bpAddr )
	PtrByte *bpAddr ;
/*
 * IN	  bp points to a white-space or null terminated string in a buffer.
 * RETURN a copy of the string pointed to by IN bp.
 *        The caller needs to free the copy after use.
 * OUT    bp points to the byte after the string in the buffer.
 */
{
	Byte baBuffer[ 40 ];	/* no word with > 20 chars */
	PtrByte	bpBuffer ;
	PtrByte	bp ;

	bp = *bpAddr ;

	bpBuffer = baBuffer ;
	WHILE ! isspace( *bp ) DO
		*bpBuffer++ = *bp++ ;
		*bpBuffer++ = *bp++ ;
	ENDW
	*bpBuffer = '\0' ;

	*bpAddr = bp ;
	return	(PtrByte) strdup( baBuffer );
} /* GetHanziStringFromBuffer */

static	FILE *	OpenFullFileName( cpNameFileIn, cpDictType )
	char *	cpNameFileIn ;
	char *	cpDictType ;
{
	char	caFullFileName[ 1024 ];
	FILE *	fFile ;

	if ( *cpNameFileIn == '/' ) { /* => a full path name */
		strcpy( caFullFileName, cpNameFileIn );
	} else {
		strcpy( caFullFileName, gcpNameDictPath );
		strcat( caFullFileName, StringDirectoryDelimiter );
		strcat( caFullFileName, cpNameFileIn );
	}
	fFile = fopen( caFullFileName, "r" );
	if ( fFile == NULL ) {
		printf( "%s \"%s\" NOT exist.  Nothing done!\n",
			cpDictType, caFullFileName );
		exit(1);
	}
	return	fFile ;
} /* OpenFullFileName */

static	void	ReadCharDict( caNameFileIn )
	char *	caNameFileIn ;
{
	FILE *	fFileIn ;
	int	nLine ;
	int	iHash ;
	unsigned char *	bp ;
	PtrCharDictEntry p ;

	gpCharDictEntry = (PtrCharDictEntry) calloc( NHzHashTable,
						   sizeof( CharDictEntry ) );
	IF gpCharDictEntry == NULL THEN
		FatalError( "error calloc( gpCharDictEntry )" );
	ENDI
	gipFlagOfAmbiguousGBChar = (int *) calloc( NCharDictFlag,
						   sizeof( int ) );
	IF gipFlagOfAmbiguousGBChar == NULL THEN
		FatalError( "error calloc( gipFlagOfAmbiguousGBChar )" );
	ENDI

	fFileIn = OpenFullFileName( caNameFileIn, "CharDictionary" );

	nLine = 0 ;

	/* read input line by line:
	gcaLine = fgets( gcaLine, MaxCharPerLine, fFileIn );
	gcaLine terminated by '\n'.
	 */
	WHILE fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL DO
		IF gcaLine[0] == '#' THEN
			continue;
		ENDI
		bp = (unsigned char *) gcaLine ;
		iHash = HashOneHz( bp );
		p = & gpCharDictEntry[ iHash ] ;
		p->baGBchar[0] = *bp++ ;
		p->baGBchar[1] = *bp++ ;
		p->baB5char[0] = *bp++ ;
		p->baB5char[1] = *bp++ ;
		IF ( *bp == '\n' ) && ( *(bp+1) == '\0' ) THEN
			/* need to check both bytes of GBcode
			 * to avoid first byte == '\n'
			 */
		ELSE
			SetFlagOfAmbiguousGBCharGivenHashAddress( iHash );
		ENDI
		nLine ++ ;
	ENDW
	gnCharDictEntry = nLine ; 
	printf( "NameCharDict    ='%s'\tgnCharDictEntry = %6d\n",
		caNameFileIn, gnCharDictEntry );

	fclose( fFileIn );

#ifdef  OBSOLETE
 {
	int	nAmbiguousChar ;
	int	i ;
	FILE *	fFileOut ;
#define FileOutTestCharDict	"t.char"

	/* test contents of CharDict:
	 */
	fFileOut = fopen( FileOutTestCharDict, "w");
	if ( fFileOut == NULL) {
		printf("can't open <%s>.  ", fFileOut );
		printf("Nothing done!\n");
		exit(1);
	}

	nAmbiguousChar = 0 ;
	for ( i = 0 ; i < NCharDictTable ; i++ ) {
		gpcde = & gpCharDictEntry[ i ] ;
		if ( IsAmbiguousCharGivenHashAddress( i ) ) {
			nAmbiguousChar ++ ;
			fprintf( fFileOut, "%c%c\n",
						gpcde->baGBchar[0],
						gpcde->baGBchar[1] );
		}
	}
	printf( "FileOutTestCharDict ='%s'\tnAmbiguousChar  = %6d\n",
		FileOutTestCharDict, nAmbiguousChar );
	fclose( fFileOut );
 }
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

} /* ReadCharDict */

static	void	ReadWordDict( caNameFileIn )
	char *	caNameFileIn ;
{
	FILE *	fFileIn ;
	int	nLine ;
	Byte	baBuffer[ 1024 ];	/* no word with > 20 chars */
	PtrByte	bpBuffer ;
	PtrByte	bp ;
	PtrByte	bpTmp ;
	PtrWordDictEntry	p ;

	fFileIn = OpenFullFileName( caNameFileIn, "WordDictionary" );

	/* read first input line,
	 * which should contain the value of the total no. of entries:
	 */
	IF fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL THEN
		IF ( sscanf( gcaLine, "%s%d", baBuffer, & gnWordDictEntry ) != 2 ) ||
		   strcmp( baBuffer, WordDictVersionString ) != 0 THEN
			FatalError( "no WordDictVersionString in WordDict" );
		ENDI
		IF gnWordDictEntry > 0 THEN
			gpWordDictEntry = (PtrWordDictEntry) calloc( gnWordDictEntry,
						     sizeof( WordDictEntry ) );
			IF gpWordDictEntry == NULL THEN
				FatalError( "error calloc( gpWordDictEntry )" );
			ENDI
		ENDI
	ELSE
		FatalError( "error reading the no. of entries of WordDict" );
	ENDI
	printf( "NameWordDict    ='%s'\tgnWordDictEntry = %6d\n",
		caNameFileIn, gnWordDictEntry );

	/* read input line by line:
	gbpLine = fgets( gcaLine, MaxCharPerLine, fFileIn );
	 */
	nLine = 0 ;
	WHILE fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL DO
		IF gcaLine[0] == '#' THEN
			continue;
		ENDI
		IF nLine >= gnWordDictEntry THEN
			printf( "no. of entries of WordDict > no. specified in first line\n" );
			break;
		ENDI
		bp = (unsigned char *) gcaLine ;
		p = & gpWordDictEntry[ nLine ] ;

		p->bpGBword = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the <tab> separating GBwords & B5words */
		p->bpB5word = GetHanziStringFromBuffer( & bp );

		nLine ++ ;
	ENDW
	gnWordDictEntry = nLine ;

	fclose( fFileIn );

	qsort( (char *) gpWordDictEntry, gnWordDictEntry, sizeof( WordDictEntry ),
		CompareWord );

#ifdef  OBSOLETE
 {
	int	i ;
	FILE *	fFileOut ;
#define FileOutTestWordDict	"t.word"

	/* test contents of WordDict:
	 */
	fFileOut = fopen( FileOutTestWordDict, "w");
	if ( fFileOut == NULL) {
		printf("can't open <%s>.  ", fFileOut );
		printf("Nothing done!\n");
		exit(1);
	}
	printf( "FileOutTestWordDict ='%s'\n", FileOutTestWordDict );
	for ( i = 0 ; i < gnWordDictEntry ; i++ ) {
		p = & gpWordDictEntry[ i ] ;
			fprintf( fFileOut, "%s\t%s\n",
						p->bpGBword,
						p->bpB5word );
	}
	printf( "gnWordDictEntry = %d\n", gnWordDictEntry );
	fclose( fFileOut );
 }
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

} /* ReadWordDict */

static	void	ReadPrefixWordBigramDict( caNameFileIn )
	char *	caNameFileIn ;
{
	FILE *	fFileIn ;
	int	nLine ;
	Byte	baBuffer[ 40 ];	/* no word with > 20 chars */
	PtrByte	bpBuffer ;
	PtrByte	bp ;
	PtrWordBigramDictEntry	p ;

	fFileIn = OpenFullFileName( caNameFileIn, "PrefixWordBigramDictionary" );

	/* read first input line,
	 * which should contain the value of the total no. of entries:
	 */
	IF fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL THEN
		IF ( sscanf( gcaLine, "%s%d", baBuffer, & gnPrefixWordBigramDictEntry ) != 2 ) ||
		   strcmp( baBuffer, WordDictVersionString ) != 0 THEN
			FatalError( "no WordDictVersionString in PrefixWordBigramDict" );
		ENDI
		IF gnPrefixWordBigramDictEntry > 0 THEN
			gpPrefixWordBigramDictEntry = (PtrWordBigramDictEntry) calloc( gnPrefixWordBigramDictEntry,
						     sizeof( WordBigramDictEntry ) );
			IF gpPrefixWordBigramDictEntry == NULL THEN
				FatalError( "error calloc( gpPrefixWordBigramDictEntry )" );
			ENDI
		ENDI
	ELSE
		FatalError( "error reading the no. of entries of PrefixWordBigramDict" );
	ENDI

	printf( "NamePrefixWordBigramDict ='%s'  gnPrefixWordBigramDictEntry = %4d\n",
		caNameFileIn, gnPrefixWordBigramDictEntry );
	nLine = 0 ;

	/* read input line by line:
	gbpLine = fgets( gcaLine, MaxCharPerLine, fFileIn );
	 */
	WHILE fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL DO
		IF gcaLine[0] == '#' THEN
			continue;
		ENDI
		/* replace (one and only one) '\n' by '\0':
		 */
		bp = (PtrByte) strchr( gcaLine, '\n' );
		*bp = '\0' ;

		/* get strings from the input line and put into DictEntry:
		 */
		p = & gpPrefixWordBigramDictEntry[ nLine ] ;

		bp = (PtrByte) gcaLine ;

		p->bpGBword1 = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the SPACE separating GBword1 & GBword2 */

		p->bpGBword2 = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the <tab> separating GBwords & B5words */

		p->bpB5word1 = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the SPACE separating B5word1 & B5word2 */

		p->bpB5word2 = GetHanziStringFromBuffer( & bp );

		nLine ++ ;
	ENDW
	IF gnPrefixWordBigramDictEntry != nLine THEN
		FatalError( "no. of entries of PrefixWordBigramDict incorrect c.f. actual no.\n" );
	ENDI

	fclose( fFileIn );
#ifdef  OBSOLETE
 {
	int	i ;
	int	nPrefixWordBigramDictEntry ;
	FILE *	fFileOut ;
#define FileOutPrefixWordBigramDict	"t.suffix.word.bigram"

	/* test contents of PrefixWordBigramDict:
	 */
	fFileOut = fopen( FileOutTestPrefixWordBigramDict, "w");
	if ( fFileOut == NULL) {
		printf("can't open <%s>.  ", fFileOut );
		printf("Nothing done!\n");
		exit(1);
	}
	printf( "FileOutTestPrefixWordBigramDict ='%s'\n", FileOutTestPrefixWordBigramDict );
	nPrefixWordBigramDictEntry = 0 ;
	for ( i = 0 ; i < gnPrefixWordBigramDictEntry ; i++ ) {
		p = & gpPrefixWordBigramDictEntry[ i ] ;
			fprintf( fFileOut, "%s %s\t%s %s\n",
						p->bpGBword1,
						p->bpGBword2,
						p->bpB5word1,
						p->bpB5word2 );
	}
	printf( "nPrefixWordBigramDictEntry = %d\n", nPrefixWordBigramDictEntry );
	fclose( fFileOut );
 }
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

} /* ReadPrefixWordBigramDict */

static	void	ReadSuffixWordBigramDict( caNameFileIn )
	char *	caNameFileIn ;
{
	FILE *	fFileIn ;
	int	nLine ;
	Byte	baBuffer[ 40 ];	/* no word with > 20 chars */
	PtrByte	bpBuffer ;
	PtrByte	bp ;
	PtrWordBigramDictEntry	p ;

	fFileIn = OpenFullFileName( caNameFileIn, "SuffixWordBigramDictionary" );

	/* read first input line,
	 * which should contain the value of the total no. of entries:
	 */
	IF fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL THEN
		IF ( sscanf( gcaLine, "%s%d", baBuffer, & gnSuffixWordBigramDictEntry ) != 2 ) ||
		   strcmp( baBuffer, WordDictVersionString ) != 0 THEN
			FatalError( "no WordDictVersionString in SuffixWordBigramDict" );
		ENDI
		IF gnSuffixWordBigramDictEntry > 0 THEN
			gpSuffixWordBigramDictEntry = (PtrWordBigramDictEntry) calloc( gnSuffixWordBigramDictEntry,
						     sizeof( WordBigramDictEntry ) );
			IF gpSuffixWordBigramDictEntry == NULL THEN
				FatalError( "error calloc( gpSuffixWordBigramDictEntry )" );
			ENDI
		ENDI
	ELSE
		FatalError( "error reading the no. of entries of SuffixWordBigramDict" );
	ENDI

	printf( "NameSuffixWordBigramDict ='%s'  gnSuffixWordBigramDictEntry = %4d\n",
		caNameFileIn, gnSuffixWordBigramDictEntry );
	nLine = 0 ;

	/* read input line by line:
	gbpLine = fgets( gcaLine, MaxCharPerLine, fFileIn );
	 */
	WHILE fgets( gcaLine, MaxCharPerLine, fFileIn ) != NULL DO
		IF gcaLine[0] == '#' THEN
			continue;
		ENDI
		/* replace (one and only one) '\n' by '\0':
		 */
		bp = (PtrByte) strchr( gcaLine, '\n' );
		*bp = '\0' ;

		/* get strings from the input line and put into DictEntry:
		 */
		p = & gpSuffixWordBigramDictEntry[ nLine ] ;

		bp = (PtrByte) gcaLine ;

		p->bpGBword1 = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the SPACE separating GBword1 & GBword2 */

		p->bpGBword2 = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the <tab> separating GBwords & B5words */

		p->bpB5word1 = GetHanziStringFromBuffer( & bp );
		bp++ ;	/* skip the SPACE separating B5word1 & B5word2 */

		p->bpB5word2 = GetHanziStringFromBuffer( & bp );

		nLine ++ ;
	ENDW
	IF gnSuffixWordBigramDictEntry != nLine THEN
		FatalError( "no. of entries of SuffixWordBigramDict incorrect c.f. actual no.\n" );
	ENDI

	fclose( fFileIn );
#ifdef  OBSOLETE
 {
	int	i ;
	int	nSuffixWordBigramDictEntry ;
	FILE *	fFileOut ;
#define FileOutSuffixWordBigramDict	"t.suffix.word.bigram"

	/* test contents of SuffixWordBigramDict:
	 */
	fFileOut = fopen( FileOutTestSuffixWordBigramDict, "w");
	if ( fFileOut == NULL) {
		printf("can't open <%s>.  ", fFileOut );
		printf("Nothing done!\n");
		exit(1);
	}
	printf( "FileOutTestSuffixWordBigramDict ='%s'\n", FileOutTestSuffixWordBigramDict );
	nSuffixWordBigramDictEntry = 0 ;
	for ( i = 0 ; i < gnSuffixWordBigramDictEntry ; i++ ) {
		p = & gpSuffixWordBigramDictEntry[ i ] ;
			fprintf( fFileOut, "%s %s\t%s %s\n",
						p->bpGBword1,
						p->bpGBword2,
						p->bpB5word1,
						p->bpB5word2 );
	}
	printf( "nSuffixWordBigramDictEntry = %d\n", nSuffixWordBigramDictEntry );
	fclose( fFileOut );
 }
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

} /* ReadSuffixWordBigramDict */

/*----------------------------------------------
	OpenHzCiDictionary
----------------------------------------------*/

static	void	OpenHzCiDictionary()
{
	char	caFullDictName[ 1024 ];

#ifdef  BMMFMM
	if ( *caFMMDICTFileName == '/' ) { /* => a full path name */
		strcpy( caFullDictName, caFMMDICTFileName );
	} else {
		strcpy( caFullDictName, gcpNameDictPath );
		strcat( caFullDictName, StringDirectoryDelimiter );
		strcat( caFullDictName, caFMMDICTFileName );
	}
	ghandleDictFMM = Dict_LoadHzCiDictionary( caFullDictName );
	gnMaxByteOfHzFMM = NHzBytes * Dict_MaxNumberofHzInWord( ghandleDictFMM );

	if ( *caBMMDICTFileName == '/' ) { /* => a full path name */
		strcpy( caFullDictName, caBMMDICTFileName );
	} else {
		strcpy( caFullDictName, gcpNameDictPath );
		strcat( caFullDictName, StringDirectoryDelimiter );
		strcat( caFullDictName, caBMMDICTFileName );
	}
	ghandleDictBMM = Dict_LoadHzCiDictionary( caFullDictName );
	gnMaxByteOfHzBMM = NHzBytes * Dict_MaxNumberofHzInWord( ghandleDictBMM );


	Dict_InitMaximalMatchQueue();
#endif/*BMMFMM*/
}

/*----------------------------------------------
	InitGetInputFile
----------------------------------------------*/

static	void	InitGetInputFile()
{
	GetLineIntoBuffer( gbaInputBuffer );
	gbpInputBuffer = gbaInputBuffer ;
} /* InitGetInputFile */

/*----------------------------------------------
	ProcessingInputFile
----------------------------------------------*/

static	void	ProcessingInputFile()
{
/*--- Open user's FileIn and FileOut: ---*/

	fFileIn = fopen(caNameFileIn, "r");
	if (fFileIn==NULL) {
		printf("input text file <%s> NOT exists.  Nothing done!\n",
				caNameFileIn );
		exit(1);
	}
#ifdef  DEBUG
	printf("caNameFileIn    ='%s'\n", caNameFileIn);
#endif/*DEBUG*/

	fFileOut = fopen( caNameFileOut, "w");
	if ( fFileOut == NULL) {
		printf("can't open output file <%s>.  Nothing done!\n",
				caNameFileOut );
		exit(1);
	}
#ifdef  DEBUG
	printf("caNameFileOut   ='%s'\n", caNameFileOut );
#endif/*DEBUG*/

/*--- Open CharDict & WordDict & WordBigramDict's in EnvNameDictPath: ---*/

	ReadCharDict( caNameCharDict );

	ReadWordDict( caNameWordDict );

	ReadPrefixWordBigramDict( caNamePrefixWordBigramDict );

	ReadSuffixWordBigramDict( caNameSuffixWordBigramDict );

/*--- Open Trie-files for FMM & BMM: ---*/

	OpenHzCiDictionary();

/*--- Process Input File: ---*/

	InitGetInputFile();
	LOOP
		ConvertNonGBHanzi();
		GetOneSentence();
		IF *gbaSentenceBuffer IS '\0' THEN	/* no more sentence */
			break;
		ENDI
		IF IsAmbiguousBuffer( gbaSentenceBuffer ) THEN
			gbpStartSegmented = SegmentSentence( gbaSentenceBuffer );
			gbpCurrentSegmented = gbpStartSegmented + 1 ;
				/* + 1 to skip the first CSegmentDelimiter */
			ConvertSentenceWordByWord();
		ELSE
			/* ConvertSentenceCharByChar:
			 */
#ifdef  DEBUG
			if ( giDebugLevel >= 1 ) {
				printf( "      HzByHz %s\n", gbaSentenceBuffer );
			}
#endif/*DEBUG*/
			ConvertHanziByHanzi( gbaSentenceBuffer );
		ENDI
	ENDL

#ifdef  DEBUG
	printf( "gnLineInput      = %8d\n", gnLineInput );
	printf( "gn1CharWordWithAmbiguousHz= %8d\n", gn1CharWordWithAmbiguousHz );
	printf( "gnNCharWordWithAmbiguousHz= %8d\n", gnNCharWordWithAmbiguousHz );
/*
	printf( "gnWordWithAmbiguousHz     = %8d\n",
			gn1CharWordWithAmbiguousHz + gnNCharWordWithAmbiguousHz );
*/
	printf( "gnWordWithNoAmbiguousHz   = %8d\n", gnWordWithNoAmbiguousHz );
	printf( "gnSentenceWithNoAmbiguousHz= %7d\n", gnSentenceWithNoAmbiguousHz );
	printf( "gnWordDictConvert= %8d\n", gnWordDictConvert );
	printf( "gnPrefixConvert  = %8d\n", gnPrefixConvert );
	printf( "gnSuffixConvert  = %8d\n", gnSuffixConvert );
#endif/*DEBUG*/
	fclose( fFileIn );
	fclose( fFileOut );

} /* ProcessingInputFile */

/*----------------------------------------------
	main
----------------------------------------------*/
int main(argc, argv)
int	argc;
char	*argv[];
{
	int	i ;
	int	n ;

/*--- 0. command & ---*/
	i=1;
	while (i<argc)
	if (argv[i][0]=='-') {
		switch (argv[i][1]) {
			case 'D':
				if ((++i) < argc) {
					giDebugLevel = atoi( argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'i':
				if ((++i) < argc) {
					strcpy( caNameFileIn, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'o':
				if ((++i) < argc) {
					strcpy( caNameFileOut, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'c':
				if ((++i) < argc) {
					strcpy( caNameCharDict, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'w':
				if ((++i) < argc) {
					strcpy( caNameWordDict, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'p':
				if ((++i) < argc) {
					strcpy( caNamePrefixWordBigramDict, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 's':
				if ((++i) < argc) {
					strcpy( caNameSuffixWordBigramDict, argv[i++] );
				} else {
					command_help();
				}
				break;
#ifdef  BMMFMM
			case 'f':
				if ((++i) < argc) {
					strcpy( caFMMDICTFileName, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'b':
				if ((++i) < argc) {
					strcpy( caBMMDICTFileName, argv[i++] );
				} else {
					command_help();
				}
				break;
#endif/*BMMFMM*/
		/**********
			case 's':
				++i ;
				gcStartHanzi = *argv[i++] ;
				break;
		**********/
			case 'h':
			default:	command_help(); i++;
		}
	} else {
		printf("warning: ");
		printf("useless argument <%s> in command line\n",argv[i]);
		i++;
	}

/*--- 0.1 environment variables ---*/
	gcpNameDictPath = getenv( EnvNameDictPath );
	if ( gcpNameDictPath == NULL ) {
		gcpNameDictPath = gcpCurrentWorkDir ;
	}

/*--- 1. main processing ---*/

	ProcessingInputFile();
	exit( 0 );
} /* main */
