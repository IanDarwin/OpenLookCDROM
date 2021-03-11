/*==========================================================================
	PrintAmbHz.c

	Author(s):
		Man-Chi Pong mcpong@cs.ust.hk	19940524 (Tue)
	Version(s):
		1.0 (19940524 (Tue))
	Function:
		-- B5 stands for Big5 --
		Read a file of GB-B5 hanzi table;
		Output to a file of ambiguous GB hanzi;
		Output to a file of ambiguous B5 hanzi.
		(An ambiguous GB hanzi maps to more than one ambiguous B5 hanzi.)
	Input:
		a file of GB-B5 hanzi table.
	Output:
		a file of word-probability table.
		a file of GB-B5 word table.
	Usage:
		PrintAmbHz [-h] [-i GBB5CharFile] [-b AmbB5CharFile] [-g AmbGBCharFile]
		input:
			'GBB5CharFile'
		output:
			'AmbB5CharFile'
			'AmbGBCharFile'
	Make:
		cc PrintAmbHz.c -o PrintAmbHz
	
	History:
		Man-Chi Pong mcpong@cs.ust.hk	19940525 (wed) first created

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

#define	CComment	'#'

#define	NameGBB5CharFile	"g2bchar.tab"
#define	NameAmbB5CharFile	"amb.b5"
#define	NameAmbGBCharFile	"amb.gb"

#define	LargeBufferSize	1024

#define	MaxCharPerLine	4096	/* assume < MaxCharPerLine char per input line */

/*----------------------------------------------
	globals
----------------------------------------------*/

	char	caGBB5CharFile[128] =	NameGBB5CharFile ;
	char	caAmbB5CharFile[128] =	NameAmbB5CharFile ;
	char	caAmbGBCharFile[128] =	NameAmbGBCharFile ;
	FILE *	fGBB5CharFile ;
	FILE *	fAmbB5CharFile ;
	FILE *	fAmbGBCharFile ;

	PtrByte	gbpNullString = (PtrByte) "" ;	/* constant string */

	/* for input buffer :
	 */
static	char    gcaLine[ MaxCharPerLine ];

/*----------------------------------------------
	extern
----------------------------------------------*/

/*----------------------------------------------
	FUNCTIONS
----------------------------------------------*/

/*----------------------------------------------
	command_help()
----------------------------------------------*/
static	void	command_help()
{
	printf( "\n");
	printf( "usage:\n");
	printf( "PrintAmbHz [-h] [-i GBB5CharFile] [-b AmbB5CharFile] [-g AmbGBCharFile]\n" );
	printf( "input:\n" );
	printf( "	GB-B5 Char Table File\n" );
	printf( "output:\n" );
	printf( "	Ambiguous B5 Char File and Ambiguous GB Char File\n" );
	printf( "default:\n");
	printf( "	'NameGBB5CharFile'  = '%s'\n", NameGBB5CharFile );
	printf( "	'NameAmbB5CharFile' = '%s'\n", NameAmbB5CharFile );
	exit(0);
} /* command_help */

/*----------------------------------------------
	ProcessingInput
----------------------------------------------*/

static	void	ProcessingInput()
{
	int	gnChar ;
	int	gnAmbigousB5Char ;
	int	gnAmbigousGBChar ;
	int	nLen ;
	int	i ;
	char *	cp ;

/*--- Open output files: ---*/

	fGBB5CharFile = fopen( caGBB5CharFile, "r" );
	assert( fGBB5CharFile != NULL );

	fAmbB5CharFile = fopen( caAmbB5CharFile, "w" );
	assert( fAmbB5CharFile != NULL );

	fAmbGBCharFile = fopen( caAmbGBCharFile, "w" );
	assert( fAmbGBCharFile != NULL );

#ifdef  DEBUG
	printf( "caGBB5CharFile ='%s'\n", caGBB5CharFile );
	printf( "caAmbGBCharFile ='%s'\n", caAmbGBCharFile );
	printf( "caAmbB5CharFile ='%s'\n", caAmbB5CharFile );
#endif/*DEBUG*/

/*--- Loop for AmbigousGBH: ---*/
	gnChar = 0 ;
	gnAmbigousGBChar = 0 ;
	gnAmbigousB5Char = 0 ;
	while ( fgets( gcaLine, MaxCharPerLine, fGBB5CharFile ) != NULL ) {
		if ( gcaLine[0] == CComment ) {
			continue;
		}
		gnChar ++ ;
		nLen = strlen( gcaLine ) - 1 ;	/* terminated by \n\0, thus - 1 */
		if ( nLen > 4 ) { /* GBB5... more than one B5 hanzi => ambigous */
			gnAmbigousGBChar ++ ;
			fprintf( fAmbGBCharFile, "%c%c\n", gcaLine[0], gcaLine[1] );

			cp = gcaLine + 2 ;
			i = 2 ;
			while ( i < nLen ) {
				gnAmbigousB5Char ++ ;
				fprintf( fAmbB5CharFile, "%c%c\n", *cp, *(cp + 1) );
				cp += 2 ;
				i += 2 ;
			}
		}
	} /*while*/

	printf( "gnChar           = %8d\n", gnChar           );
	printf( "gnAmbigousGBChar = %8d\n", gnAmbigousGBChar );
	printf( "gnAmbigousB5Char = %8d\n", gnAmbigousB5Char );

	fclose( fGBB5CharFile );

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
			case 'i':
				if ((++i) < argc) {
					strcpy( caGBB5CharFile, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'b':
				if ((++i) < argc) {
					strcpy( caAmbB5CharFile, argv[i++] );
				} else {
					command_help();
				}
				break;
			case 'g':
				if ((++i) < argc) {
					strcpy( caAmbGBCharFile, argv[i++] );
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
