static char *RCSid = "$Header$";

/*
 * $Log$
 */

#include "surfmodl.h"
#include <ctype.h>
char separate[] = {" ,\t"};

int inreal(Filin,Realvar,Comment,Line_num,Interactive)
FILE * Filin;
vartype *Realvar;
text80 *Comment;
int Line_num;
boolean Interactive;
{
    char *Line;            /* line of input */
    int i;                   /* points to character in Line */
    int j;                   /* general index */
    int Num;                 /* number of numeric entry */
    int Firstdig;            /* pointer to first digit of entry */
    int Lennum;              /* length of total numeric entry */
    int Ndeci;               /* # decimal pts. in entry */
    boolean Retcode;             /* return code from function */
    int Lastcomma;           /* keep track of whether last significant
                                    character was a comma */
    double atof();
    char *malloc(), *strtok();
    int nbytes;
    char *str;

  Lastcomma = TRUE;
  Comment[0] = ' ';
  Num = 0;
  Line = malloc(128);
  Line[0] = '*';
  while ( Line[0] == '*' ) {
     if(getline(Filin,&Line,&nbytes)) {
	perror("getline");
	exit(1);
    }
  }
  str = Line;
  while ( (str = strtok(str,separate)) != NULL) {
	if (str[0] == '*'|| str[0] == ';'||str[0] == '%') {
		strcpy(Comment,str);
		break;
	}
	if (isalpha(str[0])) 
		;
	else
		Realvar[Num++] = atof(str);
	str = NULL;
  }
  return(Num);
} /* function Inreal */

/*
 * Read a line from a file, and store the bytes in the supplied buffer. The
 * "nbuf" is the number of bytes returned. Realloc buf for long lines
 * Check for I/O errors too. Return status.
 */
#define NLINE 128

getline(Infile,buf, nbuf)
FILE *Infile;
char   **buf;
int *nbuf;
{

	int	c;
	unsigned	MemLen;
	char*	realloc();

	*nbuf = 0;
	MemLen = NLINE;
	while( (c = getc( Infile )) != EOF  && c != '\n') {
		(*buf)[*nbuf] = c;
		(*nbuf)++;
		if( *nbuf + 1 >= MemLen ) {	/* extend buffer */
			MemLen += NLINE;
			if( !(*buf = realloc( *buf, MemLen)) ) {
				fprintf(stderr,"Line too long");
			return (1);
		}
	}
	}
	if (c == EOF) {
		if (ferror(Infile)) {
			fprintf(stderr,"File read error");
			return (1);
		}
		return (1);
	}
	(*buf)[*nbuf] = '\0';
		  /* Trim the string */

	return (0);
}

