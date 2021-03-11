/* -*-C-*-
********************************************************************************
*
* File:         xlread.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlread.c,v 2.4 1994/06/06 15:59:23 npm Exp $
* Description:  xlisp expression input routine
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:53 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlread.c,v 2.4 1994/06/06 15:59:23 npm Exp $";

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
#ifndef ANSI
#include <math.h>   /* for atof(), ANSI puts it in stdlib also,
			which is included in xlisp.h. What a mess! */
#endif /* ANSI */

/* symbol parser modes */
#define DONE	0
#define NORMAL	1
#define ESCAPE	2

/* external variables */
extern LVAL true,s_dot;
extern LVAL s_quote,s_function,s_bquote,s_comma,s_comat;
extern LVAL s_rtable,k_wspace,k_const,k_nmacro,k_tmacro;
extern LVAL k_sescape,k_mescape;

/* For xlload bug fix */
extern LVAL xlvalue;
extern CONTEXT *xltarget;
extern int xlmask;

/* string constants */
#define WSPACE "\t \f\r\n"
#define CONST1 "!$%&*+-./0123456789:<=>?@[]^_{}~"
#define CONST2 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"


/* forward declarations */
#ifdef ANSI
LOCAL LVAL NEAR callmacro(LVAL fptr, int ch); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR psymbol(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR punintern(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR pnumber(LVAL fptr, int radix); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR pquote(LVAL fptr, LVAL sym); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR plist(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR pvector(LVAL fptr); /* NPM: changed this to LOCAL */
#ifdef STRUCTS
LOCAL LVAL NEAR pstruct(LVAL fptr); /* NPM: changed this to LOCAL */
#endif /* STRUCTS */
LOCAL LVAL NEAR readlist(LVAL fptr, int *plen);	/* NPM: changed this to LOCAL */
LOCAL void NEAR pcomment(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL void NEAR badeof(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL void NEAR upcase(char *str); /* NPM: changed this to LOCAL */
LOCAL int  NEAR storech(char *buf, int c, int ch); /* NPM: changed this to LOCAL */
LOCAL int  NEAR nextch(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL int  NEAR checkeof(LVAL fptr); /* NPM: changed this to LOCAL */
LOCAL int  NEAR readone(LVAL fptr, LVAL FAR *pval); /* NPM: changed this to LOCAL */
LOCAL int  NEAR pname(LVAL fptr, int *pescflag); /* NPM: changed this to LOCAL */
#else /* !defined(ANSI) */
LOCAL FORWARD LVAL callmacro();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL psymbol(),punintern(); /* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL pnumber(),pquote(),plist(),pvector(); /* NPM: changed this to LOCAL */
#ifdef STRUCTS
LOCAL FORWARD LVAL pstruct();	/* NPM: changed this to LOCAL */
#endif /* STRUCTS */
LOCAL FORWARD LVAL readlist();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID pcomment();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID badeof();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID upcase();	/* NPM: changed this to LOCAL */
#endif /* ANSI */

/* xlload - load a file of xlisp expressions */
int xlload(fname,vflag,pflag)
  char *fname; int vflag,pflag;
{
    char fullname[STRMAX+1];
    LVAL fptr,expr;
    CONTEXT cntxt;
    FILEP fp;
    int sts, mask;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fptr);
    xlsave(expr);

    /* default the extension */
    if (needsextension(fname)) {
	strcpy(fullname,fname);
	strcat(fullname,".lsp");
	fname = fullname;
    }

    /* allocate a file node */
#ifdef BETTERIO
    fptr = cvfile(CLOSED,S_FORREADING);
#else
    fptr = cvfile(CLOSED);
#endif

    /* open the file */

#ifdef PATHNAMES
    if ((fp = ospopen(fname,TRUE)) == CLOSED)
#else
    if ((fp = OSAOPEN(fname,OPEN_RO)) == CLOSED)
#endif
    {
	xlpopn(2);
	return (FALSE);
    }
    setfile(fptr,fp);

    /* print the information line */
    if (vflag)	/* TAA MOD -- changed from printing to stdout */
	{ sprintf(buf,"; loading \"%s\"\n",fname); dbgputstr(buf); }

    /* read, evaluate and possibly print each expression in the file */
    xlbegin(&cntxt,CF_ERROR|CF_UNWIND,true);	/* TAA mod so file gets closed */
    if ((mask = setjmp(cntxt.c_jmpbuf)) != 0)	/* TAA mod -- save mask */
	sts = FALSE;
    else {
	while (xlread(fptr,&expr)) {
	    expr = xleval(expr);
	    if (pflag)
		stdprint(expr);
	}
	sts = TRUE;
    }
    xlend(&cntxt);

    /* close the file */
    OSCLOSE(getfile(fptr));
    setfile(fptr,CLOSED);

    /* restore the stack */
    xlpopn(2);

    /* check for unwind protect TAA MOD */
    if ((mask & ~CF_ERROR) != 0)
	xljump(xltarget, xlmask, xlvalue);

    /* return status */
    return (sts);
}

/* xlread - read an xlisp expression */
int xlread(fptr,pval)
  LVAL fptr,*pval;
{
    int sts;

    /* read an expression */
    while ((sts = readone(fptr,pval)) == FALSE)
	;

    /* return status */
    return (sts == EOF ? FALSE : TRUE);
}

/* readone - attempt to read a single expression */
LOCAL int NEAR readone(fptr,pval)
  LVAL fptr, FAR *pval;
{
    LVAL val,type;
    int ch;

    /* get a character and check for EOF */
    if ((ch = xlgetc(fptr)) == EOF)
	return (EOF);

    /* handle white space */
    if ((type = tentry(ch)) == k_wspace)
	return (FALSE);

    /* handle symbol constituents */
    /* handle single and multiple escapes */  /* combined by TAA MOD */
    else if (type == k_const ||
	     type == k_sescape || type == k_mescape) {
	xlungetc(fptr,ch);
	*pval = psymbol(fptr);
	return (TRUE);
    }

    /* handle read macros */
    else if (consp(type)) {
	if (((val = callmacro(fptr,ch)) != NIL) && consp(val)) {
	    *pval = car(val);
	    return (TRUE);
	}
	else
	    return (FALSE);
    }

    /* handle illegal characters */
    else {
/*	xlerror("illegal character",cvfixnum((FIXTYPE)ch)); */
	xlerror("illegal character",cvchar(ch));    /* friendlier TAA MOD*/
	return (0);  /* compiler warning */
    }
}

/* rmhash - read macro for '#' */
LVAL rmhash()
{
    LVAL fptr,val;
    char *bufp;		/* TAA fix to allow control character literals */
	int i;
    int ch;

    /* protect some pointers */
    xlsave1(val);

    /* get the file and macro character */

    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* make the return value */
    val = consa(NIL);

    /* check the next character */
    switch (ch = xlgetc(fptr)) {
    case '\'':
	rplaca(val,pquote(fptr,s_function));
	break;
    case '(':
	xlungetc(fptr,ch);
	rplaca(val,pvector(fptr));
	break;

#ifdef COMMONLISP
    case '.':
	readone(fptr,&car(val));
	rplaca(val,xleval(car(val)));
	break;
#endif

    case 'b':
    case 'B':
	rplaca(val,pnumber(fptr,2));
	break;
    case 'o':
    case 'O':
	rplaca(val,pnumber(fptr,8));
	break;
    case 'x':
    case 'X':
	rplaca(val,pnumber(fptr,16));
	break;
#ifdef STRUCTS
    case 's':
    case 'S':
	rplaca(val,pstruct(fptr));
	break;
#endif
    case '\\':
	for (i = 0; i < STRMAX-1; i++) {
	    ch = xlgetc(fptr);	/* TAA fix to scan at end of file */
	    if (ch == EOF ||
		((tentry(buf[i] = ch)  != k_const) &&
		(i > 0) &&	/* TAA fix for left and right paren */
		buf[i] != '\\' && buf[i] != '|')) {
		xlungetc(fptr, buf[i]);
		break;
	    }
	}
	buf[i] = 0;
	ch = buf[0];
	if (strlen(buf) > 1) {	/* TAA Fixed */
	    i = buf[strlen(buf)-1]; /* Value of last character */
	    upcase(buf);
	    bufp = &buf[0];
	    ch = 0;
	    if (strncmp(bufp,"M-",2) == 0) {
		ch = 128;
		bufp += 2;
	    }
	    if (strcmp(bufp,"NEWLINE") == 0)
		ch += '\n';
	    else if (strcmp(bufp,"SPACE") == 0)
		ch += ' ';
	    else if (strcmp(bufp,"RUBOUT") == 0)
		ch += 127;
	    else if (strlen(bufp) == 1)
		ch += i;
	    else if (strncmp(bufp,"C-",2) == 0 && strlen(bufp) == 3)
		ch += bufp[2] & 31;
	    else xlerror("unknown character name",cvstring(buf));
	}
	rplaca(val,cvchar(ch));
	break;
    case ':':
	rplaca(val,punintern(fptr));
	break;
    case '|':
	pcomment(fptr);
	val = NIL;
	break;
#ifdef COMPLX
    case 'c':
    case 'C':  /* From XLISP-STAT, Copyright (c) 1988, Luke Tierney */
	{
	    LVAL list;
	    readone(fptr, &list);
	    if (! consp(list) || ! consp(cdr(list)) || cdr(cdr(list)) != NIL)
		xlerror("bad complex number specification", list);
	    rplaca(val, newcomplex(car(list), car(cdr(list))));
	    break;
	}
#endif
    default:
/*	xlerror("illegal character after #",cvfixnum((FIXTYPE)ch)); */
	xlerror("illegal character after #",cvchar(ch)); /*TAA Mod */
    }

    /* restore the stack */
    xlpop();

    /* return the value */
    return (val);
}

/* rmquote - read macro for '\'' */
LVAL rmquote()
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_quote)));
}

/* rmdquote - read macro for '"' */
LVAL rmdquote()
{
    char buf[STRMAX+1],*p, FAR *sptr;
    LVAL fptr,str,newstr;
    int len,blen,ch,d2,d3;

    /* protect some pointers */
    xlsave1(str);

    /* get the file and macro character */
    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* loop looking for a closing quote */
    len = blen = 0; p = buf;
    while ((ch = checkeof(fptr)) != '"') {

	/* handle escaped characters */
	switch (ch) {
	case '\\':
		switch (ch = checkeof(fptr)) {
		case 't':
			ch = '\011';
			break;
		case 'n':
			ch = '\012';
			break;
		case 'f':
			ch = '\014';
			break;
		case 'r':
			ch = '\015';
			break;
		default:
			if (ch >= '0' && ch <= '7') {
			    d2 = checkeof(fptr);
			    d3 = checkeof(fptr);
			    if (d2 < '0' || d2 > '7'
			     || d3 < '0' || d3 > '7')
				xlfail("invalid octal digit");
			    ch -= '0'; d2 -= '0'; d3 -= '0';
			    ch = (ch << 6) | (d2 << 3) | d3;
			}
			break;
		}
	}


	/* check for buffer overflow */

	if (blen >= STRMAX) {
	    newstr = newstring(len + STRMAX);
	    sptr = getstring(newstr);
	    if (str != NIL)
		MEMCPY(sptr, getstring(str), len);
	    *p = '\0';
	    MEMCPY(sptr+len, buf, blen+1);
	    p = buf;
	    blen = 0;
	    len += STRMAX;
	    str = newstr;
	}


	/* store the character */
	*p++ = ch; ++blen;
    }

    /* append the last substring */

    if (str == NIL || blen) {
	newstr = newstring(len + blen);
	sptr = getstring(newstr);
	if (str != NIL) MEMCPY(sptr, getstring(str), len);
	*p = '\0';
	MEMCPY(sptr+len, buf, blen+1);
	str = newstr;
    }


    /* restore the stack */
    xlpop();

    /* return the new string */
    return (consa(str));
}

/* rmbquote - read macro for '`' */
LVAL rmbquote()
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* parse the quoted expression */
    return (consa(pquote(fptr,s_bquote)));
}

/* rmcomma - read macro for ',' */
LVAL rmcomma()
{
    LVAL fptr,sym;
    int ch;

    /* get the file and macro character */
    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* check the next character */
    if ((ch = xlgetc(fptr)) == '@')
	sym = s_comat;
    else {
	xlungetc(fptr,ch);
	sym = s_comma;
    }

    /* make the return value */
    return (consa(pquote(fptr,sym)));
}

/* rmlpar - read macro for '(' */
LVAL rmlpar()
{
    LVAL fptr;

    /* get the file and macro character */
    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* make the return value */
    return (consa(plist(fptr)));
}

/* rmrpar - read macro for ')' */
LVAL rmrpar()
{
    xlfail("misplaced right paren");
    return (NIL);   /* never returns */
}

/* rmsemi - read macro for ';' */
LVAL rmsemi()
{
    LVAL fptr;
    int ch;

    /* get the file and macro character */
    fptr = xlgetarg();	/* internal -- don't bother with error checks */

    /* skip to end of line */
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n')
	;

    /* return nil (nothing read) */
    return (NIL);
}

/* pcomment - parse a comment delimited by #| and |# */
LOCAL VOID NEAR pcomment(fptr)
  LVAL fptr;
{
    int lastch,ch,n;

    /* look for the matching delimiter (and handle nesting) */
    for (n = 1, lastch = -1; n > 0 && (ch = xlgetc(fptr)) != EOF; ) {
	if (lastch == '|' && ch == '#')
	    { --n; ch = -1; }
	else if (lastch == '#' && ch == '|')
	    { ++n; ch = -1; }
	lastch = ch;
    }
}

/* pnumber - parse a number */
LOCAL LVAL NEAR pnumber(fptr,radix)
  LVAL fptr; int radix;
{
    int digit,ch;
    long num;

    for (num = 0L; (ch = xlgetc(fptr)) != EOF; ) {
	if (islower(ch)) ch = toupper(ch);
	if (!('0' <= ch && ch <= '9') && !('A' <= ch && ch <= 'F'))
	    break;
	if ((digit = (ch <= '9' ? ch - '0' : ch - 'A' + 10)) >= radix)
	    break;
	num = num * (long)radix + (long)digit;
    }
    xlungetc(fptr,ch);
    return (cvfixnum((FIXTYPE)num));
}

/* plist - parse a list */
LOCAL LVAL NEAR plist(fptr)
  LVAL fptr;
{
    LVAL val,expr,lastnptr,nptr;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(val);
    xlsave(expr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL; nextch(fptr) != ')'; )

	/* get the next expression */
	switch (readone(fptr,&expr)) {
	case EOF:
	    badeof(fptr);
	case TRUE:

	    /* check for a dotted tail */
	    if (expr == s_dot) {

		/* make sure there's a node */
		if (lastnptr == NIL)
		    xlfail("invalid dotted pair");

		/* parse the expression after the dot */
		if (!xlread(fptr,&expr))
		    badeof(fptr);
		rplacd(lastnptr,expr);

		/* make sure its followed by a close paren */
		if (nextch(fptr) != ')')
		    xlfail("invalid dotted pair");
	    }

	    /* otherwise, handle a normal list element */
	    else {
		nptr = consa(expr);
		if (lastnptr == NIL)
		    val = nptr;
		else
		    rplacd(lastnptr,nptr);
		lastnptr = nptr;
	    }
	    break;
	}

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return successfully */
    return (val);
}

/* pvector - parse a vector */
LOCAL LVAL NEAR pvector(fptr)
 LVAL fptr;
{
    LVAL list,val;
    int len,i;

    /* protect some pointers */
    xlsave1(list);

    /* read the list */
    list = readlist(fptr,&len);

    /* make a vector of the appropriate length */
    val = newvector(len);

    /* copy the list into the vector */
    for (i = 0; i < len; ++i, list = cdr(list))
	setelement(val,i,car(list));

    /* restore the stack */
    xlpop();

    /* return successfully */
    return (val);
}

#ifdef STRUCTS
/* pstruct - parse a structure */
LOCAL LVAL NEAR pstruct(fptr)
 LVAL fptr;
{
    LVAL list,val;
    int len;

    /* protect some pointers */
    xlsave1(list);

    /* read the list */
    list = readlist(fptr,&len);

    /* make the structure */
    val = xlrdstruct(list);

    /* restore the stack */
    xlpop();

    /* return successfully */
    return (val);
}
#endif

/* pquote - parse a quoted expression */
LOCAL LVAL NEAR pquote(fptr,sym)
  LVAL fptr,sym;
{
    LVAL val,p;

    /* protect some pointers */
    xlsave1(val);

    /* allocate two nodes */
    val = consa(sym);
    rplacd(val,consa(NIL));

    /* initialize the second to point to the quoted expression */
    if (!xlread(fptr,&p))
	badeof(fptr);
    rplaca(cdr(val),p);

    /* restore the stack */
    xlpop();

    /* return the quoted expression */
    return (val);
}

/* psymbol - parse a symbol name */
LOCAL LVAL NEAR psymbol(fptr)
  LVAL fptr;
{
    int escflag;
    LVAL val;
    pname(fptr,&escflag);
    return (escflag || !isnumber(buf,&val) ? xlenter(buf) : val);
}

/* punintern - parse an uninterned symbol */
LOCAL LVAL NEAR punintern(fptr)
  LVAL fptr;
{
    int escflag;
    pname(fptr,&escflag);
    return (xlmakesym(buf));
}

/* pname - parse a symbol/package name */
#ifdef ANSI
static int NEAR pname(LVAL fptr, int *pescflag)
#else
LOCAL int pname(fptr,pescflag)
  LVAL fptr; int *pescflag;
#endif
{
    int mode,ch,i;
    LVAL type;

    /* initialize */
    *pescflag = FALSE;
    mode = NORMAL;
    i = 0;

    /* accumulate the symbol name */
    while (mode != DONE) {

	/* handle normal mode */
	while (mode == NORMAL)
	    if ((ch = xlgetc(fptr)) == EOF)
		mode = DONE;
	    else if ((type = tentry(ch)) == k_sescape) {
		i = storech(buf,i,checkeof(fptr));
		*pescflag = TRUE;
	    }
	    else if (type == k_mescape) {
		*pescflag = TRUE;
		mode = ESCAPE;
	    }
	    else if (type == k_const
		 ||  (consp(type) && car(type) == k_nmacro))
		i = storech(buf,i,islower(ch) ? toupper(ch) : ch);
	    else
		mode = DONE;

	/* handle multiple escape mode */
	while (mode == ESCAPE)
	    if ((ch = xlgetc(fptr)) == EOF)
		badeof(fptr);
	    else if ((type = tentry(ch)) == k_sescape)
		i = storech(buf,i,checkeof(fptr));
	    else if (type == k_mescape)
		mode = NORMAL;
	    else
		i = storech(buf,i,ch);
    }
    buf[i] = 0;

    /* check for a zero length name */
    if (i == 0)
	xlfail("zero length name");	/* TAA fix, Jeff Prothero improved*/

    /* unget the last character and return it */
    xlungetc(fptr,ch);
    return (ch);
}

/* readlist - read a list terminated by a ')' */
LOCAL LVAL NEAR readlist(fptr,plen)
 LVAL fptr; int *plen;
{
    LVAL list,expr,lastnptr,nptr;
    int ch;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(list);
    xlsave(expr);

    /* get the open paren */
    if ((ch = nextch(fptr)) != '(')
	xlfail("expecting an open paren");
    xlgetc(fptr);

    /* keep appending nodes until a closing paren is found */
    for (lastnptr = NIL, *plen = 0; (ch = nextch(fptr)) != ')'; ) {

	/* check for end of file */
	if (ch == EOF)
	    badeof(fptr);

	/* get the next expression */
	switch (readone(fptr,&expr)) {
	case EOF:
	    badeof(fptr);
	case TRUE:
	    nptr = consa(expr);
	    if (lastnptr == NIL)
		list = nptr;
	    else
		rplacd(lastnptr,nptr);
	    lastnptr = nptr;
	    ++(*plen);
	    break;
	}
    }

    /* skip the closing paren */
    xlgetc(fptr);

    /* restore the stack */
    xlpopn(2);

    /* return the list */
    return (list);
}

/* storech - store a character in the print name buffer */
LOCAL int NEAR storech(buf,i,ch)
  char *buf; int i,ch;
{
    if (i < STRMAX)
	buf[i++] = ch;
    return (i);
}

/* tentry - get a readtable entry */
LVAL tentry(ch)
  int ch;
{
    LVAL rtable;
    rtable = getvalue(s_rtable);
    if (!vectorp(rtable) || ch < 0 || ch >= getsize(rtable))
	return (NIL);
    return (getelement(rtable,ch));
}

/* nextch - look at the next non-blank character */
LOCAL int NEAR nextch(fptr)
  LVAL fptr;
{
    int ch;

    /* return and save the next non-blank character */
    while ((ch = xlgetc(fptr)) != EOF && isspace(ch))
	;
    xlungetc(fptr,ch);
    return (ch);
}

/* checkeof - get a character and check for end of file */
LOCAL int NEAR checkeof(fptr)
  LVAL fptr;
{
    int ch;

    if ((ch = xlgetc(fptr)) == EOF)
	badeof(fptr);
    return (ch);
}

/* badeof - unexpected eof */
LOCAL VOID NEAR badeof(fptr)
  LVAL fptr;
{
    xlgetc(fptr);
    xlfail("unexpected EOF");
}

/* isnumber - check if this string is a number */
int isnumber(str,pval)
  char *str; LVAL *pval;
{
    int dl,dr;
    char *p;

    /* initialize */
    p = str; dl = dr = 0;

    /* check for a sign */
    if (*p == '+' || *p == '-')
	p++;

    /* check for a string of digits */
    while (isdigit(*p))
	p++, dl++;

    /* check for a decimal point */
    if (*p == '.') {
	p++;
	while (isdigit(*p))
	    p++, dr++;
    }

    /* check for an exponent */
    if ((dl || dr) && *p == 'E') {
	p++;

	/* check for a sign */
	if (*p == '+' || *p == '-')
	    p++;

	/* check for a string of digits */
	while (isdigit(*p))
	    p++, dr++;
    }

    /* make sure there was at least one digit and this is the end */
    if ((dl == 0 && dr == 0) || *p)
	return (FALSE);

    /* convert the string to an integer and return successfully */
    if (pval != NULL) {
	if (*str == '+') ++str;
	if (str[strlen(str)-1] == '.') str[strlen(str)-1] = 0;
	*pval = (dr ? cvflonum(atof(str)) : cvfixnum(ICNV(str)));
    }
    return (TRUE);
}

/* defmacro - define a read macro */
#ifdef ANSI
static void NEAR defmacro(int ch, LVAL type, int offset)
#else
LOCAL VOID defmacro(ch,type,offset)
  int ch; LVAL type; int offset;
#endif
{
    extern FUNDEF funtab[];
    LVAL subr;
    subr = cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset);
    setelement(getvalue(s_rtable),ch,cons(type,subr));
}

/* callmacro - call a read macro */
LOCAL LVAL NEAR callmacro(fptr,ch)
  LVAL fptr; int ch;
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(cdr(getelement(getvalue(s_rtable),ch)));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(fptr);
    pusharg(cvchar(ch));
    xlfp = newfp;
    return (xlapply(2));
}

/* upcase - translate a string to upper case */
LOCAL VOID NEAR upcase(str)
  char *str;
{
    for (; *str != '\0'; ++str)
	if (islower(*str))
	    *str = toupper(*str);
}

/* xlrinit - initialize the reader */
VOID xlrinit()
{
    LVAL rtable;
    char *p;
    int ch;

    /* create the read table */
    rtable = newvector(256);
    setvalue(s_rtable,rtable);

    /* initialize the readtable */
    for (p = WSPACE; (ch = *p++) != 0; )
	setelement(rtable,ch,k_wspace);
    for (p = CONST1; (ch = *p++) != 0; )
	setelement(rtable,ch,k_const);
    for (p = CONST2; (ch = *p++) != 0; )
	setelement(rtable,ch,k_const);

    /* setup the escape characters */
    setelement(rtable,'\\',k_sescape);
    setelement(rtable,'|', k_mescape);

    /* install the read macros */
    defmacro('#', k_nmacro,FT_RMHASH);
    defmacro('\'',k_tmacro,FT_RMQUOTE);
    defmacro('"', k_tmacro,FT_RMDQUOTE);
    defmacro('`', k_tmacro,FT_RMBQUOTE);
    defmacro(',', k_tmacro,FT_RMCOMMA);
    defmacro('(', k_tmacro,FT_RMLPAR);
    defmacro(')', k_tmacro,FT_RMRPAR);
    defmacro(';', k_tmacro,FT_RMSEMI);
}

