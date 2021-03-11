/*==========================================================================
	hzcount.c

	Author(s):
		Man-Chi Pong mcpong@cs.ust.hk	19940430 (Sat)
	Version(s):
		1.0 (19940430 (Sat))
	Function:

		count the no. of hanzi's in an input file of ASCII & GB/Big5 hanzi
	Input:
		an input file
	Output:
		output the no. of hanzi's in the input file
	Usage:
		hzcount [-h] fileIn
	Make:
		cc hzcount.c -o hzcount
	
	History:
		Man-Chi Pong mcpong@cs.ust.hk	19940430 (Sat)
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
#define	NameFileOut	"hzcount.out"

#define	MaxCharPerLine	4096	/* assume < MaxCharPerLine char per input line */

#define IsNotHanziCode(cp)	( *(cp) < 0xa0 )
#define IsHanziCode(cp)	( *(cp) >= 0xa0 )
#define IsNotHanzi(cp)	( *(cp) < 0xb0 )
#define IsHanzi(cp) 	( *(cp) >= 0xb0 )

/*----------------------------------------------
	globals
----------------------------------------------*/

	char	caNameFileIn1[128] = NameFileIn1 ;
	FILE *	fFileIn1 ;	/* file pointer to input file */

	/* for input handling -- used in GetNextHz() & GetLine() :
	 */
static	char *	gcpNullString = "" ;	/* constant string */
static	char    gcaLine1[ MaxCharPerLine ];


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
	printf( "hzcount [-h] fileIn \n" );

	printf( "	count no. of hanzi's in fileIn\n" );
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
	int	nHzCount ;

/*--- 0. command ---*/
	i=1;
	if (argv[i][0]=='-') {
		switch (argv[i][1]) {
		/**********
			case 's':
				++i ;
				gcStartHanzi = *argv[i++] ;
				break;
		**********/
			case 'h':
			default:	command_help(); i++;
		}
	}
#ifdef  OBSOLETE
	else {
		printf("warning: ");
		printf("useless argument <%s> in command line\n",argv[i]);
		i++;
	}
#else /*OBSOLETE*/
#endif/*OBSOLETE*/

/*--- 1. text ---*/
	if ( argc <= i ) {
		FatalError( "input file name not given" );
	}
	strcpy( caNameFileIn1, argv[ i ] );

	fFileIn1 = fopen(caNameFileIn1, "r");
	if (fFileIn1==NULL) {
		printf("text file <%s> NOT exists.  ", caNameFileIn1);
		printf("Nothing done!\n");
		exit(1);
	}
	printf("caNameFileIn1    ='%s'\n", caNameFileIn1);

	nHzCount = 0 ;	/* initialize */
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

		nLine ++ ;

		bp1 = (PtrByte) gcaLine1 ;
		WHILE *bp1 DO
			IF IsNotHanziCode( bp1 ) THEN
				bp1 ++ ;
			ELSE
				nHzCount ++ ;
				bp1 += 2 ;
			ENDI
		ENDW
	ENDW

	printf( "nLine in file = %8d\n", nLine );
	printf( "nHzCount  = %8d\n", nHzCount );
	fclose( fFileIn1 );

	exit( 0 );
} /* main */
