/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
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
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/cmd/RCS/dumpbin.c,v 2.9 1992/12/15 21:47:14 rr2b R6tape $";
#endif


 

#include <andrewos.h>
#include <stdio.h>
#include <ctype.h>

#define NUMLONGS 1 /* EXP_MAXOPTS/32 */

extern char *index();

static char pref[]="messages.binaryoptions";
long hatol();

main(argc, argv)
int argc;
char **argv;
{
FILE *f;
char line[256];
register char *s = 0, *t, *u;
long dum, dum2;
register int i, offset=0;

    if (argc != 2) {
	printf("usage: dumpbin preferencefile\n");
	exit(0);
    }
    if ((f = fopen(*++argv, "r")) == NULL) {
	printf("Could not open preference file '%s' for input.\n", *argv);
	exit(0);
    }
    while (fgets(line, sizeof(line), f)) {
	fold(line);
	if (!strncmp(line, pref, sizeof(pref)-1)) {
	    s = line+sizeof(pref);
	    break;
	}
    }
    fclose(f);
    if (!s) exit(1);
    while (s && offset <= NUMLONGS) {
	t = index(s, ',');
	if (t) *t++ = '\0';
	u = index(s, '/');
	if (u) *u++ = '\0';
	dum = hatol(s);
	dum2 = u ? hatol(u) : 0;
	dum &= dum2;
	for (i=0;i<32;i++, dum>>=1) if (dum&1) printf("%.02d\n", i+offset*32);
	s = t;
	++offset;
    }
}

/***********************************************************************/

long hatol(s)
char *s;
{
    long n;
    char c;

    n = 0;
    while (c = *s) {
	if (c >= '0' && c <= '9') {
	    n = (16 * n) + c - '0';
	} else if (c >= 'a' && c <= 'f') {
	    n = (16 * n) + c - 'a' + 10;
	} /* ignore all other characters, including leading 0x and whitespace */
	++s;
    }
    return(n);
}	    

/***********************************************************************/

fold(s)
register char *s;
{
    while (*s) {
	if (isupper(*s)) *s = tolower(*s);
	s++;
    }
    if (*--s == '\n') *s = '\0';
}
