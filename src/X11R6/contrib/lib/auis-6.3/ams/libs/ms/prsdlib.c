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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/ams/libs/ms/RCS/prsdlib.c,v 2.7 1992/12/15 21:20:51 rr2b R6tape $";
#endif

/*
 * prsdlib.c - A small library of routines used by parsedate.
 *
 * This file is made up of 3 files from libcmu:  fold.c nxtarg.c skipto.c.
 * The only change is that the actual text of fold.h has been
 * inserted (it was included in fold.c), because prsdate.y does
 * not access fold.h.
 *
 */




/* beginning of fold.c */

/*  fold  --  perform case folding
 *
 *  Usage:  p = fold (out,in,whichway);
 *	    p = foldup (out,in);
 *	    p = folddown (out,in);
 *	char *p,*in,*out;
 *	enum {FOLDUP, FOLDDOWN} whichway;
 *
 *  Fold performs case-folding, moving string "in" to
 *  "out" and folding one case to another en route.
 *  Folding may be upper-to-lower case (folddown) or
 *  lower-to-upper case.
 *  Foldup folds to upper case; folddown folds to lower case.
 *  The same string may be specified as both "in" and "out".
 *  The address of "out" is returned for convenience.
 *
 */

/* beginning of fold.h */
/*  macros for fold() routine */

typedef enum {
	FOLDUP, FOLDDOWN} 
FOLDMODE;

char *fold(),*foldup(),*folddown();

/* end of fold.h */

/* continuation of fold.c */

char *fold (out,in,whichway)
char *in,*out;
FOLDMODE whichway;
{
	register char *i,*o;
	register char lower = 0;
	char upper = 0;
	int delta = 0;

	switch (whichway)
	{
	case FOLDUP:
		lower = 'a';		/* lower bound of range to change */
		upper = 'z';		/* upper bound of range */
		delta = 'A' - 'a';	/* amount of change */
		break;
	case FOLDDOWN:
		lower = 'A';
		upper = 'Z';
		delta = 'a' - 'A';
	}

	i = in;
	o = out;
	do {
		if (*i >= lower && *i <= upper)		*o++ = *i++ + delta;
		else					*o++ = *i++;
	} 
	while (*i);
	*o = '\0';
	return (out);
}

char *foldup (out,in)
char *in,*out;
{
	return (fold(out,in,FOLDUP));
}

char *folddown (out,in)
char *in,*out;
{
	return (fold(out,in,FOLDDOWN));
}

/* end of fold.c */



/* beginning of nxtarg.c */

/*
 *  nxtarg -- strip off arguments from a string
 *
 *  Usage:  p = nxtarg (&q,brk);
 *	char *p,*q,*brk;
 *	extern char _argbreak;
 *
 *	q is pointer to next argument in string
 *	after call, p points to string containing argument,
 *	q points to remainder of string
 *
 *  Leading blanks and tabs are skipped; the argument ends at the
 *  first occurence of one of the characters in the string "brk".
 *  When such a character is found, it is put into the external
 *  variable "_argbreak", and replaced by a null character; if the
 *  arg string ends before that, then the null character is
 *  placed into _argbreak;
 *  If "brk" is 0, then " " is substituted.
 *
 *  HISTORY
 * 01-Jul-83  Steven Shafer (sas) at Carnegie-Mellon University
 *	Bug fix: added check for "back >= front" in loop to chop trailing
 *	white space.
 *
 * 20-Nov-79  Steven Shafer (sas) at Carnegie-Mellon University
 *	Rewritten for VAX.  By popular demand, a table of break characters
 *	has been added (implemented as a string passed into nxtarg).
 *
 *  Originally	from klg (Ken Greer); IUS/SUS UNIX.
 */

char _argbreak;
char *skipto();

char *nxtarg (q,brk)
char **q,*brk;
{
	register char *front,*back;
	front = *q;			/* start of string */
	/* leading blanks and tabs */
	while (*front && (*front == ' ' || *front == '\t')) front++;
	/* find break character at end */
	if (brk == 0)  brk = " ";
	back = skipto (front,brk);
	_argbreak = *back;
	*q = (*back ? back+1 : back);	/* next arg start loc */
	/* elim trailing blanks and tabs */
	back -= 1;
	while ((back >= front) && (*back == ' ' || *back == '\t')) back--;
	back++;
	if (*back)  *back = '\0';
	return (front);
}

/* end of nxtarg.c */



/* beginning of skipto.c */

/************************************************************************
 *  skipover and skipto -- skip over characters in string
 *
 *  Usage:	p = skipto (string,charset);
 *		p = skipover (string,charset);
 *
 *  char *p,*charset,*string;
 *
 *  Skipto returns a pointer to the first character in string which
 *  is in the string charset; it "skips until" a character in charset.
 *  Skipover returns a pointer to the first character in string which
 *  is not in the string charset; it "skips over" characters in charset.
 ************************************************************************
 * HISTORY
 * 26-Jun-81  David Smith (drs) at Carnegie-Mellon University
 *	Skipover, skipto rewritten to avoid inner loop at expense of space.
 *
 * 20-Nov-79  Steven Shafer (sas) at Carnegie-Mellon University
 *	Skipover, skipto adapted for VAX from skip() and skipx() on the PDP-11
 *	(from Ken Greer).  The names are more mnemonic.
 *
 *	Sindex adapted for VAX from indexs() on the PDP-11 (thanx to Ralph
 *	Guggenheim).  The name has changed to be more like the index()
 *	and rindex() functions from Bell Labs; the return value (pointer
 *	rather than integer) has changed partly for the same reason,
 *	and partly due to popular usage of this function.
 */

static unsigned char tab[256] = {
	0};

char *skipto (string,charset)
unsigned char *string, *charset;
{
	register unsigned char *setp,*strp;

	tab[0] = 1;		/* Stop on a null, too. */
	for (setp=charset;  *setp;  setp++) tab[*setp]=1;
	for (strp=string;  tab[*strp]==0;  strp++)  ;
	for (setp=charset;  *setp;  setp++) tab[*setp]=0;
	return ((char *)strp);
}

char *skipover (string,charset)
unsigned char *string, *charset;
{
	register unsigned char *setp,*strp;

	tab[0] = 0;		/* Do not skip over nulls. */
	for (setp=charset;  *setp;  setp++) tab[*setp]=1;
	for (strp=string;  tab[*strp];  strp++)  ;
	for (setp=charset;  *setp;  setp++) tab[*setp]=0;
	return ((char *)strp);
}

/* end of skipto.c */
