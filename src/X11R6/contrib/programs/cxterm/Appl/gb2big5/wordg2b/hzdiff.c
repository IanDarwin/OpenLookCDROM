/*==========================================================================
	hzdiff.c

	Author(s):
		Man-Chi Pong mcpong@cs.ust.hk	19940124 (Mon)
	Version(s):
		1.0 (19940124 (Mon))
	Function:

		count the no. of different hanzi's in the 2 input files
	Input:
		2 input files of Big5 char converted from different gb2b5 programs
	Output:
		output the no. of different hanzi's in the 2 input files
	Usage:
		hzdiff [-h] -1 file1 -2 file2 -o fileOfDiffLines
	Make:
		cc hzdiff.c -o hzdiff
	
	History:
		Man-Chi Pong mcpong@cs.ust.hk	19940124 (Mon)
		-- input files consist of pure hanzi per line.
		Man-Chi Pong mcpong@cs.ust.hk	19940429 (Fri)
		-- modify to allow input files of mixed ASCII & hanzi.
	See also:
===========================================================================*/
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <search.h>
#include "modularC.h"
#include "gen_da.h"

/*----------------------------------------------
	types
----------------------------------------------*/
typedef unsigned char	Byte, * PtrByte ;

/*----------------------------------------------
	defaults
	& macros
----------------------------------------------*/

#define	NameFileIn1	"fileIn1"
#define	NameFileIn2	"file2In"
#define	NameFileOut	"hzdiff.out"

#define	MaxCharPerLine	4096	/* assume < MaxCharPerLine char per input line */

#define IsNotHanziCode(cp)	( *(cp) < 0xa0 )
#define IsHanziCode(cp)	( *(cp) >= 0xa0 )
#define IsNotHanzi(cp)	( *(cp) < 0xb0 )
#define IsHanzi(cp) 	( *(cp) >= 0xb0 )

/*----------------------------------------------
	globals
----------------------------------------------*/

	char	caNameFileIn1[128] = NameFileIn1 ;
	char	caNameFileIn2[128] = NameFileIn2 ;
	char	caNameFileOut[128] = NameFileOut ;
	FILE *	fFileIn1 ;	/* file pointer to input file */
	FILE *	fFileIn2 ;	/* file pointer to input file */
	FILE *	fFileOut ;	/* file pointer to output file */

	/* for input handling -- used in GetNextHz() & GetLine() :
	 */
static	char *	gcpNullString = "" ;	/* constant string */
static	char    gcaLine1[ MaxCharPerLine ];
static	char    gcaLine2[ MaxCharPerLine ];


/*----------------------------------------------
	FUNCTIONS
----------------------------------------------*/

/*----------------------------------------------
	command_help()
----------------------------------------------*/
command_help()
{
	printf( "\n");
	printf( "usage:\n");
	printf( "hzdiff [-h] -1 file1 -2 file2 [-o fileOut]\n" );

	printf( "input  files:\n");
	printf( "	2 input files of Big5 char converted from different gb2b5 programs\n" );
	printf( "output:\n");
	printf( "	no. of different hanzi's in the 2 input files\n" );
	printf( "	and an output file, each line containing the different hanzi's\n" );
	printf( "	(and the line of file1 where the difference occurs)\n" );
	printf( "\n");
	exit(0);
} /* command_help */

/*----------------------------------------------
	FatalError()
----------------------------------------------*/
void	FatalError( message )
	char *	message ;
{
	fprintf( stderr, "%s\n", message );
	abort();
	exit( 1 );
} /* FatalError */


/*----------------------------------------------
	main
----------------------------------------------*/
int main(argc, argv)
int	argc;
char	*argv[];
{
	int	i ;
	int	n ;
	PtrByte	bp1 ;	 /* points to input buffer line from fFileIn1 */
	PtrByte	bp2 ;	 /* points to input buffer line from fFileIn2 */

	int	nLine ;
	int	nDifferentHz ;

/*--- 0. command ---*/
	i=1;
	while (i<argc)
	if (argv[i][0]=='-') {
		switch (argv[i][1]) {
			case '1':
				if ((++i) < argc) {
					strcpy( caNameFileIn1, argv[i++] );
				} else {
					command_help();
				}
				break;
			case '2':
				if ((++i) < argc) {
					strcpy( caNameFileIn2, argv[i++] );
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

/*--- 1. text ---*/

	fFileIn1 = fopen(caNameFileIn1, "r");
	if (fFileIn1==NULL) {
		printf("text file <%s> NOT exists.  ", caNameFileIn1);
		printf("Nothing done!\n");
		exit(1);
	}
	printf("caNameFileIn1    ='%s'\n", caNameFileIn1);

	fFileIn2 = fopen(caNameFileIn2, "r");
	if (fFileIn2==NULL) {
		printf("text file <%s> NOT exists.  ", caNameFileIn2);
		printf("Nothing done!\n");
		exit(2);
	}
	printf("caNameFileIn2    ='%s'\n", caNameFileIn2);

	fFileOut = fopen(caNameFileOut, "w");
	if (fFileOut==NULL) {
		printf("output file <%s> NOT writable.  ", caNameFileOut);
		printf("Nothing done!\n");
		exit(3);
	}
	printf("caNameFileOut    ='%s'\n", caNameFileOut);

	nDifferentHz = 0 ;	/* initialize */
	nLine = 0 ;	/* initialize */

	/* read input line by line:
	gcaLine = fgets( gcaLine, MaxCharPerLine, fFileIn );
	gcaLine terminated by '\n'.
	 */
	WHILE fgets( gcaLine1, MaxCharPerLine, fFileIn1 ) != NULL DO
#ifdef  OBSOLETE
		/* replace (one and only one) '\n' by '\0':
		 */
		bp = (unsigned char *) strchr( gcaLine, '\n' );
		*bp = '\0' ;
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

		IF fgets( gcaLine2, MaxCharPerLine, fFileIn2 ) == NULL THEN
			FatalError( "not same no. of lines in both input files" );
		ENDI

		nLine ++ ;

		bp1 = (PtrByte) gcaLine1 ;
		bp2 = (PtrByte) gcaLine2 ;
		WHILE *bp1 != '\n' &&
		      *bp2 != '\n'    DO
			IF IsNotHanziCode( bp1 ) THEN
				bp1 ++ ;
				bp2 ++ ;
				continue;
			ENDI

			IF ( *bp1 == *bp2 ) && ( *(bp1+1) == *(bp2+1) ) THEN
				/* do nothing */
			ELSE
				nDifferentHz ++ ;
				fprintf( fFileOut, "%c%c %c%c %s",
						/* no \n at end of format
						 * because gcaLine1 ends with \n
						 */
					*bp1, *(bp1+1),
					*bp2, *(bp2+1), gcaLine1 );
			ENDI
			bp1 += 2 ;
			bp2 += 2 ;
		ENDW
		IF *bp1 != '\n' && *bp2 != '\n' THEN
			sprintf( gcaLine1, "not same no. of hanzi's in line %d of input files", nLine );
			FatalError( gcaLine1 );
		ENDI
	ENDW

	printf( "nLine in file = %8d\n", nLine );
	printf( "nDifferentHz  = %8d\n", nDifferentHz );
	fclose( fFileIn1 );
	fclose( fFileIn2 );
	fclose( fFileOut );

	exit( 0 );
} /* main */
