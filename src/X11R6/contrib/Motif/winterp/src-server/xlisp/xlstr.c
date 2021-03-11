/* -*-C-*-
********************************************************************************
*
* File:         xlstr.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlstr.c,v 2.4 1994/06/06 15:59:24 npm Exp $
* Description:  xlisp string and character built-in functions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:48 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
* XLISP version 2.1, Copyright (c) 1989, by David Betz.
*
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Luke Tierney and David Betz not be
* used in advertising or publicity pertaining to distribution of the software
* without specific, written prior permission.  Enterprise Integration
* Technologies, Hewlett-Packard Company, Niels Mayer, Luke Tierney and David
* Betz make no representations about the suitability of this software for any
* purpose. It is provided "as is" without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* LUKE TIERNEY AND DAVID BETZ DISCLAIM ALL WARRANTIES WITH REGARD TO THIS
* SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS,
* IN NO EVENT SHALL ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD
* COMPANY, NIELS MAYER, LUKE TIERNEY NOR DAVID BETZ BE LIABLE FOR ANY SPECIAL,
* INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
* LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
* OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE.
*
********************************************************************************
*/
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlstr.c,v 2.4 1994/06/06 15:59:24 npm Exp $";

/*
*------------------------------------------------------------------------------
* See ./winterp/COPYRIGHT for information on contacting the authors.
* Please e-mail comments, modifications, questions, improvements and
* bugfixes to the WINTERP mailing list winterp@netcom.com. Please send 
* mailing list subscribe/unsubscribe notices to winterp-request@netcom.com .
* Post XLISP-specific questions/information to the USENET newsgroup
* comp.lang.lisp.x.
*------------------------------------------------------------------------------
*/

#include "xlisp.h"

/* local definitions */
#define fix(n)	cvfixnum((FIXTYPE)(n))
#define TLEFT	1
#define TRIGHT	2

/* external variables */
extern LVAL k_start,k_end,k_1start,k_1end,k_2start,k_2end;
extern LVAL true;

/* getbounds - get the start and end bounds of a string */
#ifdef ANSI
static void NEAR getbounds(LVAL str, LVAL skey, LVAL ekey, unsigned *pstart, unsigned *pend)
#else
LOCAL VOID getbounds(str,skey,ekey,pstart,pend)
  LVAL str,skey,ekey; unsigned *pstart,*pend;
#endif
{
    LVAL arg;
    unsigned len;
    FIXTYPE n;

    /* get the length of the string */
    len = getslength(str);

    /* get the starting index */
    if (xlgkfixnum(skey,&arg)) {
	*pstart = (unsigned) (n = getfixnum(arg));
	if (n < 0 || n > len)
	    xlerror("string index out of bounds",arg);
    }
    else
	*pstart = 0;

    /* get the ending index */
#ifdef COMMONLISP	/* allow NIL to mean "to end of string" */
    if (xlgetkeyarg(ekey, &arg) && arg != NIL) {
	if (!fixp(arg)) xlbadtype(arg);
	*pend = (unsigned)(n = getfixnum(arg));
	if (n < 0 || n > len)
	    xlerror("string index out of bounds",arg);
    }
#else
    if (xlgkfixnum(ekey,&arg)) {
	*pend = (unsigned)(n = getfixnum(arg));
	if (n < 0 || n > len)
	    xlerror("string index out of bounds",arg);
    }
#endif
    else
	*pend = len;

    /* make sure the start is less than or equal to the end */
    if (*pstart > *pend)
	xlerror("starting index error",cvfixnum((FIXTYPE)*pstart));
}

/* strcompare - compare strings */
#ifdef ANSI
static LVAL NEAR strcompare(int fcn, int icase)
#else
LOCAL LVAL strcompare(fcn,icase)
  int fcn,icase;
#endif
{
    unsigned start1,end1,start2,end2;
    int ch1,ch2;
    char FAR *p1, FAR *p2;
    LVAL str1,str2;

    /* get the strings */
    str1 = xlgastrorsym();
    str2 = xlgastrorsym();

    /* get the substring specifiers */
    getbounds(str1,k_1start,k_1end,&start1,&end1);
    getbounds(str2,k_2start,k_2end,&start2,&end2);

    xllastarg();

    /* setup the string pointers */
    p1 = &getstring(str1)[start1];
    p2 = &getstring(str2)[start2];

    /* compare the strings */
    for (; start1 < end1 && start2 < end2; ++start1,++start2) {
	ch1 = *p1++;
	ch2 = *p2++;
	if (icase) {
	    if (isupper(ch1)) ch1 = tolower(ch1);
	    if (isupper(ch2)) ch2 = tolower(ch2);
	}
	if (ch1 != ch2)
	    switch (fcn) {
	    case '<':	return (ch1 < ch2 ? fix(start1) : NIL);
	    case 'L':	return (ch1 <= ch2 ? fix(start1) : NIL);
	    case '=':	return (NIL);
	    case '#':	return (fix(start1));
	    case 'G':	return (ch1 >= ch2 ? fix(start1) : NIL);
	    case '>':	return (ch1 > ch2 ? fix(start1) : NIL);
	    }
    }

    /* check the termination condition */
    switch (fcn) {
    case '<':	return (start1 >= end1 && start2 < end2 ? fix(start1) : NIL);
    case 'L':	return (start1 >= end1 ? fix(start1) : NIL);
    case '=':	return (start1 >= end1 && start2 >= end2 ? true : NIL);
    case '#':	return (start1 >= end1 && start2 >= end2 ? NIL : fix(start1));
    case 'G':	return (start2 >= end2 ? fix(start1) : NIL);
    case '>':	return (start2 >= end2 && start1 < end1 ? fix(start1) : NIL);
    }
    return (NIL);   /* avoid compiler warning */
}

/* string comparision functions */
LVAL xstrlss() { return (strcompare('<',FALSE)); } /* string< */
LVAL xstrleq() { return (strcompare('L',FALSE)); } /* string<= */
LVAL xstreql() { return (strcompare('=',FALSE)); } /* string= */
LVAL xstrneq() { return (strcompare('#',FALSE)); } /* string/= */
LVAL xstrgeq() { return (strcompare('G',FALSE)); } /* string>= */
LVAL xstrgtr() { return (strcompare('>',FALSE)); } /* string> */

/* string comparison functions (not case sensitive) */
LVAL xstrilss() { return (strcompare('<',TRUE)); } /* string-lessp */
LVAL xstrileq() { return (strcompare('L',TRUE)); } /* string-not-greaterp */
LVAL xstrieql() { return (strcompare('=',TRUE)); } /* string-equal */
LVAL xstrineq() { return (strcompare('#',TRUE)); } /* string-not-equal */
LVAL xstrigeq() { return (strcompare('G',TRUE)); } /* string-not-lessp */
LVAL xstrigtr() { return (strcompare('>',TRUE)); } /* string-greaterp */

/* changecase - change case */
#ifdef ANSI
static LVAL NEAR changecase(int fcn, int destructive)
#else
LOCAL LVAL changecase(fcn,destructive)
  int fcn,destructive;
#endif
{
    char FAR *srcp, FAR *dstp;
    unsigned start,end,len,i;
    int ch;
    LVAL src,dst;

    /* get the string */
#ifdef COMMONLISP
    src = (destructive? xlgastring() : xlgastrorsym());
#else
    src = xlgastring();
#endif

    /* get the substring specifiers */
    getbounds(src,k_start,k_end,&start,&end);
    len = getslength(src);

    xllastarg();

    /* make a destination string */
    dst = (destructive ? src : newstring(len));

    /* setup the string pointers */
    srcp = getstring(src);
    dstp = getstring(dst);

    /* copy the source to the destination */
    for (i = 0; i < len; ++i) {
	ch = *srcp++;
	if (i >= start && i < end)
	    switch (fcn) {
	    case 'U':	if (islower(ch)) ch = toupper(ch); break;
	    case 'D':	if (isupper(ch)) ch = tolower(ch); break;
	    }
	*dstp++ = ch;
    }
    *dstp = '\0';

    /* return the new string */
    return (dst);
}

/* case conversion functions */
LVAL xupcase()	 { return (changecase('U',FALSE)); }
LVAL xdowncase() { return (changecase('D',FALSE)); }

/* destructive case conversion functions */
LVAL xnupcase()	  { return (changecase('U',TRUE)); }
LVAL xndowncase() { return (changecase('D',TRUE)); }

/* inbag - test if a character is in a bag */
#ifdef ANSI
static int NEAR inbag(int ch, LVAL bag)
#else
LOCAL int inbag(ch,bag)
  int ch; LVAL bag;
#endif
{
				    /* TAA MOD -- rewritten for \0 */
				    /*		  and chars >= 128 */
    char FAR *p = getstring(bag);
    unsigned len =getslength(bag);

    while (len--)
	if (*p++ == ch)
	    return (TRUE);
    return (FALSE);
}

/* trim - trim character from a string */
#ifdef ANSI
static LVAL NEAR trim(int fcn)
#else
LOCAL LVAL trim(fcn)
  int fcn;
#endif
{
    char FAR *leftp, FAR *rightp, FAR *dstp;
    LVAL bag,src,dst;

    /* get the bag and the string */
    bag = xlgastrorsym();
    src = xlgastrorsym();
    xllastarg();

    /* setup the string pointers */
    leftp = getstring(src);
    rightp = leftp + getslength(src) - 1;

    /* trim leading characters */
    if (fcn & TLEFT)
	while (leftp <= rightp && inbag(*leftp,bag))
	    ++leftp;

    /* trim character from the right */
    if (fcn & TRIGHT)
	while (rightp >= leftp && inbag(*rightp,bag))
	    --rightp;

    /* make a destination string and setup the pointer */
    dst = newstring((unsigned)rightp-(unsigned)leftp+1);
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (leftp <= rightp)
	*dstp++ = *leftp++;
    *dstp = '\0';

    /* return the new string */
    return (dst);
}

/* trim functions */
LVAL xtrim()	  { return (trim(TLEFT|TRIGHT)); }
LVAL xlefttrim()  { return (trim(TLEFT)); }
LVAL xrighttrim() { return (trim(TRIGHT)); }

#ifndef COMMONLISPF	/* revised version is CONCATENATE, in xlseq.c */
/* xstrcat - concatenate a bunch of strings */
LVAL xstrcat()
{
    FRAMEP saveargv;
    LVAL tmp,val;
    char *str;
    int saveargc,len;

    /* save the argument list */
    saveargv = xlargv;
    saveargc = xlargc;

    /* find the length of the new string */
    for (len = 0; moreargs(); ) {
	tmp = xlgastring();
	len += (int)getslength(tmp);
	if (len < 0) xlerror("string too long",tmp);
    }

    /* create the result string */
    val = newstring(len);
    str = getstring(val);

    /* restore the argument list */
    xlargv = saveargv;
    xlargc = saveargc;

    /* combine the strings */
    for (*str = '\0'; moreargs(); ) {
	tmp = nextarg();
	strcat(str,getstring(tmp));
    }

    /* return the new string */
    return (val);
}

/* xsubseq - return a subsequence */
/* New correct version in xlseq.c */
LVAL xsubseq()
{
    char *srcp,*dstp;
    int start,end,len;
    LVAL src,dst;

    /* get string and starting and ending positions */
    src = xlgastring();

    /* get the starting position */
    dst = xlgafixnum(); start = (int)getfixnum(dst);
    if (start < 0 || start > getslength(src))
	xlerror("string index out of bounds",dst);

    /* get the ending position */
    if (moreargs()) {
	dst = xlgafixnum(); end = (int)getfixnum(dst);
	if (end < 0 || end > getslength(src))
	    xlerror("string index out of bounds",dst);
    }
    else
	end = getslength(src);
    xllastarg();

    /* setup the source pointer */
    srcp = getstring(src) + start;
    len = end - start;

    /* make a destination string and setup the pointer */
    dst = newstring(len);
    dstp = getstring(dst);

    /* copy the source to the destination */
    while (--len >= 0)
	*dstp++ = *srcp++;
    *dstp = '\0';

    /* return the substring */
    return (dst);
}

#endif

/* xstring - return a string consisting of a single character */
LVAL xstring()
{
    LVAL arg;

    /* get the argument */
    arg = xlgetarg();
    xllastarg();

#ifndef NILSYMBOL
    /* make sure its not NIL */
    if (null(arg))
	xlbadtype(arg);
#endif

    /* check the argument type */
    switch (ntype(arg)) {
    case STRING:
	return (arg);
    case SYMBOL:
	return (getpname(arg));
    case CHAR:
	buf[0] = (int)getchcode(arg);
	buf[1] = '\0';
	return (cvstring(buf));
    case FIXNUM:
	buf[0] = getfixnum(arg);
	buf[1] = '\0';
	return (cvstring(buf));
    default:
	xlbadtype(arg);
	return (NIL);	/* avoid compiler warning */
    }
}

/* xchar - extract a character from a string */
LVAL xchar()
{
    LVAL str,num;
    FIXTYPE n;

    /* get the string and the index */
    str = xlgastring();
    num = xlgafixnum();
    xllastarg();

    /* range check the index */
    if ((n = getfixnum(num)) < 0 || n >= getslength(str))
	xlerror("index out of range",num);

    /* return the character */
    return (cvchar(getstringch(str,(unsigned int)n)));
}

/* xcharint - convert a character to an integer */
LVAL xcharint()
{
    LVAL arg;
    arg = xlgachar();
    xllastarg();
    return (cvfixnum((FIXTYPE)getchcode(arg)));
}

/* xintchar - convert an integer to a character */
LVAL xintchar()
{
    LVAL arg;
    arg = xlgafixnum();
    xllastarg();
    return (cvchar((int)getfixnum(arg)));
}

/* xcharcode - built-in function 'char-code' */
/* TAA mod so that result is 7 bit ascii code */
LVAL xcharcode()
{
    int ch;
    ch = 0x7f  & getchcode(xlgachar());
    xllastarg();
    return (cvfixnum((FIXTYPE)ch));
}

/* xcodechar - built-in function 'code-char' */
/* like int-char except range must be 0-127 */
LVAL xcodechar()
{
    LVAL arg;
    FIXTYPE ch;
    arg = xlgafixnum(); ch = getfixnum(arg);
    xllastarg();
    return (ch >= 0 && ch <= 127 ? cvchar((int)ch) : NIL);
}

/* xuppercasep - built-in function 'upper-case-p' */
LVAL xuppercasep()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (isupper(ch) ? true : NIL);
}

/* xlowercasep - built-in function 'lower-case-p' */
LVAL xlowercasep()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (islower(ch) ? true : NIL);
}

/* xbothcasep - built-in function 'both-case-p' */
LVAL xbothcasep()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (isupper(ch) || islower(ch) ? true : NIL);
}

/* xdigitp - built-in function 'digit-char-p' */
LVAL xdigitp()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (isdigit(ch) ? cvfixnum((FIXTYPE)(ch - '0')) : NIL);
}

/* xchupcase - built-in function 'char-upcase' */
LVAL xchupcase()
{
    LVAL arg;
    int ch;
    arg = xlgachar(); ch = getchcode(arg);
    xllastarg();
    return (islower(ch) ? cvchar(toupper(ch)) : arg);
}

/* xchdowncase - built-in function 'char-downcase' */
LVAL xchdowncase()
{
    LVAL arg;
    int ch;
    arg = xlgachar(); ch = getchcode(arg);
    xllastarg();
    return (isupper(ch) ? cvchar(tolower(ch)) : arg);
}

/* xdigitchar - built-in function 'digit-char' */
LVAL xdigitchar()
{
    LVAL arg;
    FIXTYPE n;
    arg = xlgafixnum(); n = getfixnum(arg);
    xllastarg();
    return (n >= 0 && n <= 9 ? cvchar((int)n + '0') : NIL);
}

/* xalphanumericp - built-in function 'alphanumericp' */
LVAL xalphanumericp()
{
    int ch;
    ch = getchcode(xlgachar());
    xllastarg();
    return (isupper(ch) || islower(ch) || isdigit(ch) ? true : NIL);
}

/* chrcompare - compare characters */
#ifdef ANSI
static LVAL NEAR chrcompare(int fcn, int icase)
#else
LOCAL LVAL chrcompare(fcn,icase)
  int fcn,icase;
#endif
{
    int ch1,ch2,icmp;
    LVAL arg;

    /* get the characters */
    arg = xlgachar(); ch1 = getchcode(arg);

    /* convert to lowercase if case insensitive */
    if (icase && isupper(ch1))
	ch1 = tolower(ch1);

    /* handle each remaining argument */
    for (icmp = TRUE; icmp && moreargs(); ch1 = ch2) {

	/* get the next argument */
	arg = xlgachar(); ch2 = getchcode(arg);

	/* convert to lowercase if case insensitive */
	if (icase && isupper(ch2))
	    ch2 = tolower(ch2);

	/* compare the characters */
	switch (fcn) {
	case '<':	icmp = (ch1 < ch2); break;
	case 'L':	icmp = (ch1 <= ch2); break;
	case '=':	icmp = (ch1 == ch2); break;
	case '#':	icmp = (ch1 != ch2); break;
	case 'G':	icmp = (ch1 >= ch2); break;
	case '>':	icmp = (ch1 > ch2); break;
	}
    }

    /* return the result */
    return (icmp ? true : NIL);
}

/* character comparision functions */
LVAL xchrlss() { return (chrcompare('<',FALSE)); } /* char< */
LVAL xchrleq() { return (chrcompare('L',FALSE)); } /* char<= */
LVAL xchreql() { return (chrcompare('=',FALSE)); } /* char= */
LVAL xchrneq() { return (chrcompare('#',FALSE)); } /* char/= */
LVAL xchrgeq() { return (chrcompare('G',FALSE)); } /* char>= */
LVAL xchrgtr() { return (chrcompare('>',FALSE)); } /* char> */

/* character comparision functions (case insensitive) */
LVAL xchrilss() { return (chrcompare('<',TRUE)); } /* char-lessp */
LVAL xchrileq() { return (chrcompare('L',TRUE)); } /* char-not-greaterp */
LVAL xchrieql() { return (chrcompare('=',TRUE)); } /* char-equalp */
LVAL xchrineq() { return (chrcompare('#',TRUE)); } /* char-not-equalp */
LVAL xchrigeq() { return (chrcompare('G',TRUE)); } /* char-not-lessp */
LVAL xchrigtr() { return (chrcompare('>',TRUE)); } /* char-greaterp */

