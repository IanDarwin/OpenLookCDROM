/*==========================================================================
	addword.c

	Author(s):
		Man-Chi Pong mcpong@cs.ust.hk	19940524 (Tue)
	Version(s):
		1.0 (19940524 (Tue))
	Function:
		-- B5 stands for Big5 --
		Read user's input GB word & corresponding B5 word
		Output to a file of word-probability table, each entry is a word
			with default unknown word class and usage probability
		Output to a file of GB-B5 word table
	Input:
		input GB file,
	Output:
		a file of word-probability table named "wordprob.gb+"
		-- It can be added the DICT by the program AddDICT.

		a file of GB-B5 word table       named "g2bword.+"
		-- It can be concatenated with the default table
		   "g2bword.tab" to form the new one.

	Usage:
		addword [-h] [-m NameWordProbabilityTableFile] [-w NameGBB5WordTableFile]
		output:
			'WordProbabilityTableFile' and 'GBB5WordTableFile'
	Make:
		cc addword.c -o addword
	
	History:
		Man-Chi Pong mcpong@cs.ust.hk	19940524 (Tue) first created

	See also:
===========================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <search.h>

/*----------------------------------------------
	types
----------------------------------------------*/
typedef unsigned char	Byte, * PtrByte ;

/*----------------------------------------------
	defaults & macros
----------------------------------------------*/

#define	NameWordProbabilityTableFile	"wordprob.gb+"
#define	NameGBB5WordTableFile	"g2bword.+"

#define	LargeBufferSize	1024

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


/*----------------------------------------------
	globals
----------------------------------------------*/

	char	caNameWordProbabilityTableFile[128] =	NameWordProbabilityTableFile ;
	char	caNameGBB5WordTableFile[128] =	NameGBB5WordTableFile ;
	FILE *	fFileWordProbability ;
	FILE *	fFileGBB5Word ;

	int	giDebugLevel = 0 ;
	PtrByte	gbpNullString = (PtrByte) "" ;	/* constant string */

	/* for input buffer :
	 */
static	char    gcaLine[ MaxCharPerLine ];
static	char    gcaB5Word[ MaxCharPerLine ];

#ifdef  DEBUG
	/* char type such that display gbaWord1 in dbx in cxterm
	 * can show hanzi
	 */
	char	gbaWord1[40] ;	/* less than 20 hanzi/word in Dict */
	char	gbaWord2[40] ;	/* less than 20 hanzi/word in Dict */
	PtrByte	gbpWord1 = (PtrByte) & gbaWord1[0] ;
	PtrByte	gbpWord2 = (PtrByte) & gbaWord2[0] ;
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
	DebugString()
----------------------------------------------*/
#ifdef  DEBUG
static	DebugString( cp )
	char *	cp ;
{
	printf( "%s\n", cp );
} /* DebugString */
#endif/*DEBUG*/

/*----------------------------------------------
	command_help()
----------------------------------------------*/
static	void	command_help()
{
	printf( "\n");
	printf( "usage:\n");
	printf( "addword [-h] [-m NameWordProbabilityTableFile] [-w NameGBB5WordTableFile]\n" );
	printf( "output:\n" );
	printf( "	'WordProbabilityTableFile' and 'GBB5WordTableFile'\n" );
	printf( "default:\n");
	printf( "	'NameWordProbabilityTableFile' = '%s'\n", NameWordProbabilityTableFile );
	printf( "	'NameGBB5WordTableFile' = '%s'\n", NameGBB5WordTableFile );
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


/*----------------------------------------------
	ProcessingInput
----------------------------------------------*/

static	void	ProcessingInput()
{
static	char *	gcpPromptGBWord = "input a word in GB (<ctrl-d> to terminate):\n" ;
static	char *	gcpPromptB5Word = "input the corresponding word in Big5:\n" ;
#ifdef  OBSOLETE
#define	IClassDefault	31
			/* 0th-30th word classes assigned specific part-of-speech */
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

#ifdef  MYPROBLIST
#define	fProbDefault	4.494317E-07
#else /*MYPROBLIST*/
#define	fProbDefault	2.0e-07
#endif/*MYPROBLIST*/
			/* a minimal value smaller than values from training */

	int	gnWordInput = 0 ;

/*--- Open output files: ---*/

	fFileWordProbability = fopen( caNameWordProbabilityTableFile, "w" );
	if ( fFileWordProbability == NULL ) {
		printf("can't open <%s>.\n", caNameWordProbabilityTableFile );
		exit(1);
	}

	fFileGBB5Word = fopen( caNameGBB5WordTableFile, "w" );
	if ( fFileGBB5Word == NULL ) {
		printf("can't open <%s>.\n", caNameGBB5WordTableFile);
		exit(1);
	}

	printf( "Will output to the following two files:\n" );
	printf( "caNameWordProbabilityTableFile ='%s'\n", caNameWordProbabilityTableFile );
	printf( "caNameGBB5WordTableFile ='%s'\n", caNameGBB5WordTableFile );

/*--- Loop for input words: ---*/
	gnWordInput = 0 ;
	printf( gcpPromptGBWord );
	while ( gets( gcaLine ) != NULL ) {

#ifdef  OBSOLETE
		fprintf( fFileWordProbability, "%s %2d %e\n",
				gcaLine, IClassDefault, fProbDefault );
#else /*OBSOLETE*/
		fprintf( fFileWordProbability, "%s %e\n",
				gcaLine, fProbDefault );
#endif/*OBSOLETE*/

		printf( gcpPromptB5Word );
		gets( gcaB5Word );
		fprintf( fFileGBB5Word, "%s\t%s\n",
				gcaLine, gcaB5Word );

		gnWordInput ++ ;
		printf( gcpPromptGBWord );
	} /*while*/

#ifdef  DEBUG
	printf( "gnWordInput = %8d\n", gnWordInput );
#endif/*DEBUG*/
	fclose( fFileWordProbability );
	fclose( fFileGBB5Word );

} /* ProcessingInput */

/*----------------------------------------------
	main
----------------------------------------------*/
int main(argc, argv)
int	argc;
char	*argv[];
{
	int	i ;
	int	n ;

/*--- 0. command ---*/
	i=1;
	while (i<argc)
	if (argv[i][0]=='-') {
		switch (argv[i][1]) {
			case 'p':
				if ((++i) < argc) {
					strcpy( caNameWordProbabilityTableFile, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'w':
				if ((++i) < argc) {
					strcpy( caNameGBB5WordTableFile, argv[i++] );
				} else {
					command_help();
				}
				break;
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

/*--- 1. main processing ---*/

	ProcessingInput();
	exit( 0 );
} /* main */
