#include <stdio.h>
#include <string.h>
#include <assert.h>
#include "modularC.h"

/*----------------------------------------------
	macro
----------------------------------------------*/
#define NameFileIn      "checkdup.in"
#define NameFileOut     "checkdup.out"
#define CDelimiter	' '

#define IsGBCode(cp)            ( (Byte) *(cp) >= (Byte) 0xa1 )

/*----------------------------------------------
	global
----------------------------------------------*/

typedef	unsigned char	Byte, *PtrByte ;

	char    caNameFileIn[128] =     NameFileIn ;
	char    caNameFileOut[128] =    NameFileOut ;

	FILE *	gFileIn ;
	FILE *	gFileOut ;

	modc_Bool	gisForwardScanString = TRUE ;

/*----------------------------------------------
	extern
----------------------------------------------*/

extern void	str_ReverseBuffer(
	/*IN	unsigned char *	baHzCiBuffer ;	*/
	);

extern void	InsertHzCiEntry(
	/*IN	unsigned char *	baHzCiBuffer ;	*/
	/*IN	int		nHzCount ;	*/
	/*IN	float		fHzCiProb ;	*/
	);

/*----------------------------------------------
	private function
----------------------------------------------*/
static	command_help( argv )
	char *argv[] ;
{
	printf("\n");
	printf("usage:\n");
	printf("	%s [-h] [-i inputWordFile] [-o outputDuplicatedWordFile]\n",
			argv[0] );
	printf("where\n");
        printf("        -h => print this help message\n" );
        printf("default inputWordFile            is %s\n", NameFileIn );
        printf("default outputDuplicatedWordFile is %s\n", NameFileOut );
	printf("\n");
	exit(1);
}

static	unsigned char *	SkipSpaces( bp )
	unsigned char *	bp ;
{
	while ( *bp == ' ' ) bp++ ;
	return bp ;
}

#define NBUFFERSIZE	128

#ifdef  OBSOLETE
	unsigned char	baHzCiBuffer[ NBUFFERSIZE ];
#else /*OBSOLETE*/
	/* declare as char for dbx to print char */
	char	baHzCiBuffer[ NBUFFERSIZE ];
#endif/*OBSOLETE*/

static	void	ProcessInputFile()
{
#define Is1stHzByte(x)	((x) & 0x80)
	unsigned char	buffer[ NBUFFERSIZE ];
	unsigned char	baLastWord[ NBUFFERSIZE ];
	unsigned char	baThisWord[ NBUFFERSIZE ];
	int	nLineCount ;
	int	nDupLine ;
	int	nWordLength ;
	char *	cp ;

	if ( fgets( buffer, NBUFFERSIZE, gFileIn ) == NULL ) {
		return;
	}
	nLineCount = 1 ;
#ifdef  OBSOLETE
	/* use CDelimiter to find end of a word: */
	cp = strchr( buffer, CDelimiter );
	if ( cp ) *cp = '\0' ;
#else /*OBSOLETE*/
	/* scan the word until not GB hanzi: */
	cp = (char *) buffer ;
	while ( *cp && IsGBCode( cp ) ) {
		cp += 2 ;
	}
	if ( *cp ) *cp = '\0' ;
#endif/*OBSOLETE*/
	strcpy( baLastWord, buffer );

	nDupLine = 0 ;
	while ( fgets( buffer, NBUFFERSIZE, gFileIn ) != NULL ) {

		/* assert: to handle an input line of
		 *	hanziString classId{,classId} probability\n
		 *                 ^                 ^
		 *                 separated by space
		 */
		nLineCount ++ ;
#ifdef  OBSOLETE
		cp = strchr( buffer, CDelimiter );
		if ( cp ) *cp = '\0' ;
#else /*OBSOLETE*/
		cp = (char *) buffer ;
		while ( *cp && IsGBCode( cp ) ) {
			cp += 2 ;
		}
		if ( *cp ) *cp = '\0' ;
#endif/*OBSOLETE*/
		if ( strcmp( baLastWord, buffer ) == 0 ) {
			fprintf( gFileOut, "%s\n", baLastWord );
			nDupLine ++ ;
		} else {
			strcpy( baLastWord, buffer );
		}
	} /*while*/
	printf( "no. of lines read = %5d\n", nLineCount );
	printf( "no. of dup. lines = %5d\n", nDupLine );
}

main( argc, argv )
int     argc;
char    *argv[];
{
	int	i ;

/*--- 0. command ---*/
	i=1;
	while (i<argc)
	if (argv[i][0]=='-') {
		switch (argv[i][1]) {
			case 'i':
				if ((++i) < argc)
					strcpy( caNameFileIn, argv[i++]);
				else
					command_help( argv );
				break;
			case 'o':
				if ((++i) < argc)
					strcpy( caNameFileOut, argv[i++]);
				else
					command_help( argv );
				break;
			case 'h':
			default:	command_help( argv ); i++;
		}
	} else {
		printf("warning: ");
		printf("useless argument <%s> in command line\n",argv[i]);
		i++;
	}

	printf( "caNameFileIn ='%s'\n", caNameFileIn );

	gFileIn = fopen( caNameFileIn, "r" );
	assert( gFileIn != NULL );

	printf( "caNameFileOut='%s'\n", caNameFileOut );
	gFileOut = fopen( caNameFileOut, "w" );
	assert( gFileOut != NULL );

	ProcessInputFile();

	exit( 0 );
} /*main*/
