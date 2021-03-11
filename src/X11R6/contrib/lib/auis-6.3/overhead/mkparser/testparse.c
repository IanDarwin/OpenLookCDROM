/* testparse.c	- test the parse object */


#include <foo.h>

	int 
lex() {

/* tokens are:  "$","error","$illegal.",  	/* 0-2 */
/*	"'?'","'a'", "'b'","'/'",		/* 3-6 */
/*	"script","slash", NULL			/* 7-8 (non-terminals) */

			  /* a  b  a  b  ?  / */
	static int toks[] = {4, 5, 4, 5, 3, 6};
	static int inx = 0;
	if (inx >= sizeof(toks)/sizeof(toks[0]))  return 0;  /* eof */
	return toks[inx++];
}


main()
{
	struct parser *p = foo_New();

	parser_SetDebug(1);
	printf("parser_Parse returns %d\n", parser_Parse(p, lex, NULL));
}
