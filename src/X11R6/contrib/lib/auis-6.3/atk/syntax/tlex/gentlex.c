/* gentlex.c - process .tlx files to generate tlex_description in .tlc */
/*
	Copyright Carnegie Mellon University 1992 - All rights reserved
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/
#ifndef NORCSID
	char *tlex_gentlex_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/syntax/tlex/RCS/gentlex.c,v 1.6 1992/12/19 21:51:43 wjh R6tape $";
#endif

#include <stdio.h>
#include <ctype.h>

#include <global.h>
#include <gentlex.h>


char *Prefix = NULL;
static int PrefixLen = 0;
char *tlxFile = NULL;
boolean linenos;		/* add #line's unless -l switch appears */
int LineNo = 0;
char *TokenNames = NULL;
int numtokens;
char **Token;
int namessize = 0;
struct line *Classes = NULL;
struct line *Actions[256] = {NULL};
struct line *GlobalHandler = NULL;
struct line *ErrorHandler = NULL;
struct line *ResWordHandler = NULL;
int MaxSeverity = 0;	/* maximum severity passed to Error() */
int CurrSym = 0;	/* integer portion of next symbol from GenSym() */


	void
Error(severity, msg)
	int severity;
	char *msg;
{
	if (severity > MaxSeverity)
		MaxSeverity = severity;
	/* XXX??? should add file name */
	fprintf(stderr, "line %d - %s\n", LineNo, msg);
}

	void
ErrorA(severity, msg, Arg)
	int severity;
	char *msg;
	char *Arg;
{
	int n = strlen(Arg);
	while(n > 0 && isspace(Arg[n-1])) n--;
	if (severity > MaxSeverity)
		MaxSeverity = severity;
	fprintf(stderr, "line %d - %s: \'%.*s\'\n", LineNo, msg, n, Arg);
}

	char *
GenSym()
{
	char *s = (char *)malloc(10 + PrefixLen);
	sprintf(s, "%s_%d", Prefix, CurrSym++);
	return s;
}


/* freeze(sx, ex)
	create a malloced version of the text	*sx...*(ex-1)
*/
	char *
freeze(sx, ex)
	char *sx, *ex;
{
	char *v;
	int len = (ex == NULL) ? strlen(sx) : ex-sx;
	v = (char *)malloc(len+1);
	strncpy(v, sx, len);
	v[len] = '\0';
	return v;
}

/* create a copy of s with escape characters for non-printable characters
		quotes, and \
	if plen is not NULL, set *plen to length of returned string
*/
	char *
Escapify(s, plen)
	char *s;
	int *plen;
{
	static char escaped[200];
	char *ex;
	for (ex = escaped; ex-escaped < sizeof(escaped) - 2 && *s; s++) {
		switch(*s) {
		case '\'':
		case '\"':
		case '\\':
			sprintf(ex, "\\%c", *s);
			ex += 2;
			break;
		case '\n':
			sprintf(ex, "\\n");
			ex += 2;
			break;
		default:
			if (*s < ' ' || *s >= '\177') {
				sprintf(ex, "\\%03o", UNSIGN(*s));
				ex += 4;
			}
			else
				*ex++ = *s;
			break;
		}
	}
	if (plen)
		*plen = ex - escaped;
	*ex = '\0';
	return escaped;
};


	static void
usage(args)
	char **args;
{
	fprintf(stderr,
		"usage: %s [-l] [-p prefix] [filename.tlx [filename.tab.c]]\n",
		args[0]);
	exit(9);
}

/* 
 computation of names

Prefix: 	if there is a -p switch, its operand is the Prefix value
		otherwise the first argument must be .../ffff.tlc and Prefix is ffff

tlxFile:	if there are no file arguments there must have been a -p and
		tlxFile is Prefix ~ ".tlx" otherwise tlxFile is the first file argument

tabcfile:	if there are two file arguments, the second is tabcfile
		otherwise, if there is one file argument it must have form 
				.../ffff.tlx and tabcfile is .../ffff.tab.c
		otherwise, tabcfile is Prefix ~ ".tab.c"

outnm:	the file name is Prefix ~ ".tlc" and the directory is that of tlxFile

*/		

	void
main(argc, args)
	int argc;
	char **args;
{
	char **argsp = args;
	FILE *ftabc, *ftlx, *fout;
	char *tabcfile, *outnm, *ext;
	int n;

	/* compute Prefix from switch or first file */

	Prefix = NULL;
	linenos = TRUE;
	if (argc < 2) usage(args);
	while (argsp[1][0] == '-') {
		/* process a switch */
		switch (argsp[1][1]) {
		case 'l':
			linenos = FALSE;
			argsp++, argc--;
			break;
		case 'p':
			Prefix = argsp[1]+2;
			if (Prefix == '\0') {
				if (argc < 3 || argsp[2][0] == '-') 
					usage(args);
				Prefix = argsp[2];
				argc -= 2;
				argsp += 2;
			}
			else 
				argc--,  argsp++;
			break;
		default:
			usage(args);
		}
	}
	if (Prefix == NULL) {
		/* get prefix from first file arg */
		if (argc < 2) {
			fprintf(stderr, "Provide at least the -p switch or a file name\n");
			usage(args);
		}
		tlxFile = argsp[1];
		ext = tlxFile + strlen(tlxFile) - strlen(".tlx");
		if (strcmp(ext, ".tlx") != 0) usage(args);
		outnm = (char *)strrchr(tlxFile, '/');
		outnm =  (outnm == NULL) ? tlxFile : outnm+1;
		n = ext - outnm;
		Prefix = (char *)malloc(n+1);
		strncpy(Prefix, outnm, n);
		Prefix[n] = '\0';
	}
	/* check prefix and compute PrefixLen*/
	PrefixLen = 0;
	for (ext = Prefix; *ext && (isalnum(*ext) || *ext == '_'); ext++) 
		PrefixLen++;
	if (*ext) usage(args);

	/* get .tlx file name */
	if (argc < 2) {
		/* get name from Prefix */
		tlxFile = (char *)malloc(PrefixLen+10);
		strcpy(tlxFile, Prefix);
		strcpy(tlxFile+PrefixLen, ".tlx");
	}
	else {
		/* get name from args */
		if (argsp[1][0] == '-') usage(args);
		tlxFile = argsp[1];
	}

	/* get .tab.c filename */
	if (argc < 2) {
		/* get name from Prefix */
		tabcfile = (char *)malloc(PrefixLen+10);
		strcpy(tabcfile, Prefix);
		strcpy(tabcfile+PrefixLen, ".tab.c");
	}
	else if (argc == 2) {
		/* get name from .tlx file name */
		ext = tlxFile + strlen(tlxFile) - strlen(".tlx");
		if (strcmp(ext, ".tlx") != 0) usage(args);
		n = ext - tlxFile;
		tabcfile = (char *)malloc(n+10);
		strncpy(tabcfile, tlxFile, n);
		strcpy(tabcfile+n, ".tab.c");
	}
	else {
		/* get name from args */
		if (argsp[2][0] == '-') usage(args);
		tabcfile = argsp[2];
	}

	/* compute outnm from Prefix and tlxFile */
	ext = (char *)strrchr(tlxFile, '/');
	if (ext == NULL) {
		outnm = (char *)malloc(PrefixLen+10);
		strcpy(outnm, Prefix);
		strcpy(outnm+PrefixLen, ".tlc");
	}
	else {
		ext++;
		outnm = (char *)malloc((ext - tlxFile) + PrefixLen+10);
		strncpy(outnm, tlxFile, ext-tlxFile);
		strcat(outnm, Prefix);
		strcat(outnm, ".tlc");
	}


	for (n = 0; n < 256; n++)
		Actions[n] = NULL;	

	ftabc = fopen(tabcfile, "r");
	if ( ! ftabc)
		usage(args);
	ReadTabc(ftabc);
	fclose(ftabc);

	ftlx = fopen(tlxFile, "r");
	if ( ! ftlx)
		usage(args);
	ReadTlx(ftlx);
	fclose(ftlx);

	ComputeDefaults();

	fout = fopen(outnm, "w");
	if ( ! fout) {
		ErrorA(FATAL, "cannot create output file", outnm);
		exit(9);
	}
	else
		printf("Writing: %s\n", outnm);  fflush(stdout);
	WriteTlc(fout, outnm);
	fclose(fout);

	exit(0);
}


