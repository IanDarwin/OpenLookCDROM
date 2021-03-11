/* -*-C-*-
********************************************************************************
*
* File:         xlfio.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlfio.c,v 2.4 1994/06/06 15:59:14 npm Exp $
* Description:  xlisp file i/o
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:32 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlfio.c,v 2.4 1994/06/06 15:59:14 npm Exp $";

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
#ifdef ENHFORMAT
#include <math.h>
#endif

/* external variables */
extern LVAL k_direction,k_input,k_output;
extern LVAL s_stdin,s_stdout,true;
extern int xlfsize;

#ifdef BETTERIO
extern LVAL k_io, k_elementtype;
extern LVAL a_fixnum, a_char;
extern LVAL k_exist, k_nexist, k_error, k_rename, k_newversion;
extern LVAL k_overwrite, k_append, k_supersede, k_rendel, k_probe, k_create;
extern LVAL k_start, k_end;
#endif

/* forward declarations */
#ifdef ANSI
LOCAL LVAL NEAR getstroutput(LVAL stream); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR printit(int pflag, int tflag); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR flatsize(int pflag); /* NPM: changed this to LOCAL */
#else
LOCAL FORWARD LVAL getstroutput(); /* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL printit();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL flatsize();	/* NPM: changed this to LOCAL */
#endif

/* xread - read an expression */
LVAL xread()
{
    LVAL fptr,eof,val;

    /* get file pointer and eof value */
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
#endif
    eof = (moreargs() ? xlgetarg() : NIL);
    if (xlargc > 1) xltoomany();    /* toss out now unused arg */

    /* read an expression */
    if (!xlread(fptr,&val))
	val = eof;

    /* return the expression */
    return (val);
}

/* xprint - built-in function 'print' */
LVAL xprint()
{
    return (printit(TRUE,TRUE));
}

/* xprin1 - built-in function 'prin1' */
LVAL xprin1()
{
    return (printit(TRUE,FALSE));
}

/* xprinc - built-in function princ */
LVAL xprinc()
{
    return (printit(FALSE,FALSE));
}

#ifdef BETTERIO
/* xfreshline - start a new line if not at begining of line */
LVAL xfreshline()
{
    LVAL fptr;

    /* get file pointer */
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* optionally terminate the print line and return action */
    return (xlfreshline(fptr)? true : NIL);
}
#endif


/* xterpri - terminate the current print line */
LVAL xterpri()
{
    LVAL fptr;

    /* get file pointer */
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* terminate the print line and return nil */
    xlterpri(fptr);
    return (NIL);
}

/* printit - common print function */
LOCAL LVAL NEAR printit(pflag,tflag)
  int pflag,tflag;
{
    LVAL fptr,val;

    /* get expression to print and file pointer */
    val = xlgetarg();
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* print the value */
    xlprint(fptr,val,pflag);

    /* terminate the print line if necessary */
    if (tflag)
	xlterpri(fptr);

    /* return the result */
    return (val);
}

/* xflatsize - compute the size of a printed representation using prin1 */
LVAL xflatsize()
{
    return (flatsize(TRUE));
}

/* xflatc - compute the size of a printed representation using princ */
LVAL xflatc()
{
    return (flatsize(FALSE));
}

/* flatsize - compute the size of a printed expression */
LOCAL LVAL NEAR flatsize(pflag)
  int pflag;
{
    LVAL val;

    /* get the expression */
    val = xlgetarg();
    xllastarg();

    /* print the value to compute its size */
    xlfsize = 0;
    xlprint(NIL,val,pflag);

    /* return the length of the expression */
    return (cvfixnum((FIXTYPE)xlfsize));
}

#ifdef BETTERIO

enum ACTIONS {A_NIL, A_ERR, A_REN, A_OVER, A_APP, A_SUPER, A_CREATE};

/* xopen - open a file */
LVAL xopen()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;		/* file name strings */
#endif
    FILEP fp;		/* opened file pointer */
    LVAL fname;		/* file name string LVAL */
    LVAL temp;		/* key arguments */
    int iomode;		/* file mode, as stored in node */
#ifdef ANSI
/* There doesn't seem to be any consistancy here in the handling of
   "CDECL" when dealing with pointers to procedures  TAA */
#ifdef FILETABLE
    FILEP (*opencmd)(const char *, MODETYPE);
#else
#ifdef __TURBOC__
    FILEP CDECL (*opencmd)(const char *, MODETYPE);
			    /* file type, TRUE if binary */
#else
    FILEP (* CDECL opencmd)(const char *, MODETYPE);
#endif
#endif
#else
    FILEP (*opencmd)();	    /* file type, TRUE if binary */
#endif
    enum ACTIONS exist; /* exist action */
    enum ACTIONS nexist;/* non-exist action */

    /* get file name */
#ifdef MEDMEM
    MEMCPY(name, getstring(fname = xlgetfname()), STRMAX);
    name[STRMAX-1] = 0;
#else
    name = getstring(fname = xlgetfname());
#endif

    /* get direction */
    if (xlgetkeyarg(k_direction,&temp) && temp != k_input) {
	if (temp == k_output) iomode = S_FORWRITING;
	else if (temp == k_io) iomode = S_FORREADING|S_FORWRITING;
	else if (temp == k_probe) iomode = 0;
	else goto argerror;
    }
    else iomode = S_FORREADING;

    /* get type */

    if (xlgetkeyarg(k_elementtype,&temp) && temp != a_char ) {
	if (temp == a_fixnum ) {
	    if (iomode) iomode |= S_BINARY; /* mark as binary file type */
#ifdef FILETABLE
	    opencmd = OSBOPEN;
#else
#ifdef MSC
	    opencmd = (FILEP (* CDECL)(const char *, MODETYPE)) OSBOPEN;
#else
	    opencmd = OSBOPEN;
#endif
#endif
	}
	else goto argerror;
    }
    else
#ifdef FILETABLE
	opencmd = OSAOPEN;
#else
#ifdef MSC
	opencmd = (FILEP (* CDECL)(const char *, MODETYPE)) OSAOPEN;
#else
	opencmd = OSAOPEN;
#endif
#endif

    /* get exists action */

    if (xlgetkeyarg(k_exist, &temp) &&
	(iomode & S_FORWRITING) &&  /* ignore value if :input or :probe */
	temp != k_rename && temp != k_newversion) {
	if (null(temp)) exist = A_NIL;
	else if (temp == k_error) exist = A_ERR;
	else if (temp == k_overwrite) exist = A_OVER;
	else if (temp == k_append) exist = A_APP;
	else if (temp == k_supersede || temp == k_rendel)
	    exist = A_SUPER;
	else goto argerror;
    }
    else exist = A_REN;

    /* get non-exist action */

    if (xlgetkeyarg(k_nexist, &temp)) {
	if (null(temp)) nexist = A_NIL;
	else if (temp == k_error) nexist = A_ERR;
	else if (temp == k_create) nexist = A_CREATE;
	else goto argerror;
    }
    else {  /* handle confusing mess of defaults */
	if (iomode == S_FORREADING || exist == A_OVER || exist == A_APP)
	    nexist = A_ERR;
	else if (iomode & S_FORWRITING) nexist = A_CREATE;
	else nexist = A_NIL;
    }

    xllastarg();

    /* attempt to open the file */

    if ((fp = (*opencmd)(name, (iomode & S_FORWRITING) ? OPEN_UPDATE : OPEN_RO))!=CLOSED) {
	/* success! */
	if (iomode & S_FORWRITING) switch (exist) { /* do exist action */
	    case A_ERR: /* give error */
		OSCLOSE(fp);
		xlerror("file exists", fname);
		break;
	    case A_REN: /* create new version */
		OSCLOSE(fp);
		fp = CLOSED;
		if (!renamebackup(name))
		    xlerror("couldn't create backup file", fname);
		break;
	    case A_APP: /* position to end of file */
		OSSEEKEND(fp);
		break;
	    case A_SUPER:   /* supersede file */
		OSCLOSE(fp);
		fp = CLOSED;
		break;
	    case A_NIL:	    /* return NIL */
		OSCLOSE(fp);
		return NIL;
	    /*case A_OVER:*/	/* overwrite -- does nothing special */
	    default: ;
	}
    }
    else {  /* file does not exist */
	switch (nexist) {
	    case A_ERR: /* give error */
		xlerror("file does not exist", fname);
		break;
	    case A_NIL:	    /* return NIL */
		return NIL;
	    /*case A_CREATE:*/	/* create a new file */
	    default: ;
	}
    }

    /* we now create the file if it is not already open */
    if (fp == CLOSED)
	if ((fp = (*opencmd)(name, (iomode&S_FORREADING)? CREATE_UPDATE: CREATE_WR)) == CLOSED)
	    xlerror("couldn't create file", fname);

    /* take concluding actions */
    if (iomode == 0) { /* probe */
	OSCLOSE(fp);
	fp = CLOSED;
    }

    return cvfile(fp,iomode);

    argerror: xlerror("invalid argument", temp);
    return NIL;
}
#else
/* xopen - open a file */
LVAL xopen()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif
    char *mode;
    FILEP fp;
    LVAL dir;

    /* get the file name and direction */
#ifdef MEDMEM
    MEMCPY(name, getstring(xlgetfname()), STRMAX);
    name[STRMAX-1] = 0;
#else
    name = getstring(xlgetfname());
#endif
    if (!xlgetkeyarg(k_direction,&dir))
	dir = k_input;

    xllastarg();

    /* get the mode */
    if (dir == k_input) {
	mode = OPEN_RO;
    }
    else if (dir == k_output) {
	mode = CREATE_WR;
    }
    else
	xlerror("bad direction",dir);

    /* try to open the file */
    return (((fp = OSAOPEN(name,mode)) != CLOSED) ? cvfile(fp) : NIL);
}
#endif


#ifdef BETTERIO
/* xfileposition - get position of file stream */
LVAL xfileposition()
{
    long i,j,fsize;
    int t;
    LVAL pos, fptr;
    FILEP fp;
    /* get file pointer */
    fp = getfile(fptr = xlgastream());

    /* make sure the file exists */
    if (fp == CLOSED)
	xlfail("file not open");

    /* get current position, adjusting for posible "unget" */
    j = OSTELL(fp) + (getsavech(fptr) ? -1L : 0L);

    if (moreargs()) { /* must be set position */
	pos = xlgetarg();
	xllastarg();
	if (pos == k_end) t=OSSEEKEND(fp);
	else if (pos == k_start) t=OSSEEK(fp,0L);
	else if (fixp(pos)) {	/* check for in range, then position */
	    /* STDIO allows positioning beyond end of file, so we must check
		the file size (boo his!) */
	    i = getfixnum(pos);
	    t = OSSEEKEND(fp);
	    fsize = OSTELL(fp);
	    if (t == 0 && fp != CONSOLE && (i < 0 || i > fsize)) {
		OSSEEK(fp,j);
		xlerror("position outside of file", pos);
	    }
	    t = OSSEEK(fp, i);
	}
	else xlbadtype(pos);

	setsavech(fptr,'\0');	/* toss unget character, if any */
	fptr->n_sflags &= ~(S_READING|S_WRITING);
				/* neither reading or writing currently */
	/* t is non-zero if couldn't do seek */
	return (t != 0 || fp == CONSOLE ? NIL : true);
    }

    return ((j == -1L || fp == CONSOLE) ? NIL : cvfixnum(j));
}

/* xfilelength - returns length of file */
LVAL xfilelength()
{
    FILEP fp;
    long i,j;

    /* get file pointer */
    fp = getfile(xlgastream());
    xllastarg();

    /* make sure the file exists */
    if (fp == CLOSED)
	xlfail("file not open");

    /* not all stdio packages will catch the following gaffe */
    if (fp == CONSOLE) return NIL;

    if ((i=OSTELL(fp)) == -1L ||
	OSSEEKEND(fp) ||
	(j = OSTELL(fp)) == -1L ||
	OSSEEK(fp,i)) {
	return NIL;
    }

    return cvfixnum(j);
}


#endif


/* xclose - close a file */
LVAL xclose()
{
    LVAL fptr;
    FILEP fp;	/* TAA MOD to allow closing closed files,
		    prohibit closing the console, return the correct
		    values (true on success), and close string streams */


    /* get file pointer */
    fptr = xlgetarg();
    xllastarg();

    /* handle string stream case by converting to a closed file! */
    if (ustreamp(fptr)) {
	fptr->n_type = STREAM;
	setfile(fptr, CLOSED);
	setsavech(fptr, '\0');
	return (true);
    }

    /* give error of not file stream */
    if (!streamp(fptr)) xlbadtype(fptr);

#if (defined(UNIX) || defined(WINTERP))
    if (ntype(fptr) == XLTYPE_PIPE)
      xlfail("Pipes must be closed with PCLOSE, not CLOSE.");
#endif /* (defined(UNIX) || defined(WINTERP)) */

    /* make sure the file exists */
    if ((fp = getfile(fptr)) == CLOSED || fp == CONSOLE)
	return (NIL);

    /* close the file */
    OSCLOSE(fp);
    setsavech(fptr, '\0');
    setfile(fptr,CLOSED);

    /* return true */
    return (true);
}

/* xrdchar - read a character from a file */
LVAL xrdchar()
{
    LVAL fptr;
    int ch;

    /* get file pointer */
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
#endif
    xllastarg();

    /* get character and check for eof */
    return ((ch = xlgetc(fptr)) == EOF ? NIL : cvchar(ch));
}

/* xrdbyte - read a byte from a file */
LVAL xrdbyte()
{
    LVAL fptr;
    int ch;

    /* get file pointer */
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
#endif
    xllastarg();

    /* get character and check for eof */
    return ((ch = xlgetc(fptr)) == EOF ? NIL : cvfixnum((FIXTYPE)ch));
}

/* xpkchar - peek at a character from a file */
LVAL xpkchar()
{
    LVAL flag,fptr;
    int ch;

    /* peek flag and get file pointer */
    flag = (moreargs() ? xlgetarg() : NIL);
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
#endif
    xllastarg();

    /* skip leading white space and get a character */
    if (!null(flag))
	while ((ch = xlpeek(fptr)) != EOF && isspace(ch))
	    xlgetc(fptr);
    else
	ch = xlpeek(fptr);

    /* return the character */
    return (ch == EOF ? NIL : cvchar(ch));
}

/* xwrchar - write a character to a file */
LVAL xwrchar()
{
    LVAL fptr,chr;

    /* get the character and file pointer */
    chr = xlgachar();
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* put character to the file */
    xlputc(fptr,getchcode(chr));

    /* return the character */
    return (chr);
}

/* xwrbyte - write a byte to a file */
LVAL xwrbyte()
{
    LVAL fptr,chr;

    /* get the byte and file pointer */
    chr = xlgafixnum();
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* put byte to the file */
    xlputc(fptr,(int)getfixnum(chr));

    /* return the character */
    return (chr);
}

/* xreadline - read a line from a file */
LVAL xreadline()
{
    char *p, FAR *sptr;
    LVAL fptr,str,newstr;
    int len,blen,ch;

    /* protect some pointers */
    xlsave1(str);

    /* get file pointer */
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(FALSE) : getvalue(s_stdin));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdin));
#endif
    xllastarg();

    /* get character and check for eof */
    len = blen = 0; p = buf;
    while ((ch = xlgetc(fptr)) != EOF && ch != '\n') {

	/* check for buffer overflow TAA MOD to use memcpy instead of strcat*/
	if (blen >= STRMAX) {
	    newstr = newstring(len + STRMAX);
	    sptr = getstring(newstr);
	    if (str != NIL) MEMCPY(sptr, getstring(str), len);
	    MEMCPY(sptr+len, buf, blen);
	    p = buf; blen = 0;
	    len += STRMAX;
	    str = newstr;
	}

	/* store the character */
	*p++ = ch; ++blen;
    }

    /* check for end of file */
    if (len == 0 && p == buf && ch == EOF) {
	xlpop();
	return (NIL);
    }

    /* append the last substring */
    /* conditional removed because code always executes! */
    newstr = newstring(len + blen);
    sptr = getstring(newstr);
    if (str != NIL) MEMCPY(sptr, getstring(str), len);
    MEMCPY(sptr+len, buf, blen);
    sptr[len+blen] = '\0';
    str = newstr;

    /* restore the stack */
    xlpop();

    /* return the string */
    return (str);
}


/* xmkstrinput - make a string input stream */
/* TAA MOD - reworked for unsigned lengths */

LVAL xmkstrinput()
{
    unsigned start,end,len,i;
    FIXTYPE temp;
    char FAR *str;
    LVAL string,val;

    /* protect the return value */
    xlsave1(val);

    /* get the string and length */
    string = xlgastring();
    str = getstring(string);
    len = getslength(string);

    /* get the starting offset */
    if (moreargs()) {
	val = xlgafixnum();
	temp = getfixnum(val);
	if (temp < 0 || temp > len)
	    xlerror("string index out of bounds",val);
	start = (unsigned) temp;
    }
    else start = 0;

    /* get the ending offset */
    if (moreargs()) {	    /* TAA mod to allow NIL for end offset */
	val = nextarg();
	if (null(val)) end = len;
	else if (fixp(val)) {
	    temp = getfixnum(val);
	    if (temp < start || temp > len)
		xlerror("string index out of bounds",val);
	    end = (unsigned) temp;
	}
	else xlbadtype(val);

	xllastarg();
    }
    else end = len;

    /* make the stream */
    val = newustream();

    /* copy the substring into the stream */
    for (i = start; i < end; ++i)
	xlputc(val,str[i]);

    /* restore the stack */
    xlpop();

    /* return the new stream */
    return (val);
}

/* xmkstroutput - make a string output stream */
LVAL xmkstroutput()
{
    return (newustream());
}

/* xgetstroutput - get output stream string */
LVAL xgetstroutput()
{
    LVAL stream;
    stream = xlgaustream();
    xllastarg();
    return (getstroutput(stream));
}

/* xgetlstoutput - get output stream list */
LVAL xgetlstoutput()
{
    LVAL stream,val;

    /* get the stream */
    stream = xlgaustream();
    xllastarg();

    /* get the output character list */
    val = gethead(stream);

    /* empty the character list */
    sethead(stream,NIL);
    settail(stream,NIL);

    /* return the list */
    return (val);
}
#ifdef ENHFORMAT
/* Modified for ~d e f and g formats TAA */

#define FMTMAX 256
#ifdef ANSI
static void toomanyopt(LVAL fmt)
#else
static VOID toomanyopt(fmt)
LVAL fmt;
#endif
{
    xlerror("too many prefix parameters in format",fmt);
}

/* decode prefix parameters and modifiers for a format directive */
/* TAA MOD Entirely rewritten -- return value -1 for unassigned since
   negative numbers are inappropriate for all arguments we are concerned
   with. Also clips args to reasonable values, allows both : and @ modifiers
   at once. */
#ifdef ANSI
static char FAR * NEAR decode_pp(char FAR *fmt, FIXTYPE *pp, int maxnpp,
		       int *npp, int *colon, int *atsign, LVAL lfmt)
#else
LOCAL char *decode_pp( fmt, pp, maxnpp, npp, colon, atsign, lfmt)
char	*fmt;
FIXTYPE pp[];		/* prefix parameters */
int	maxnpp;		/* maximum number of them */
int	*npp;		/* actual number of them */
int	*colon;		/* colon modifier given? */
int	*atsign;	/* atsign modifier given? */
LVAL	lfmt;		/* format string in case of failure */
#endif
{
    int i;
    int gotone = 0;
    FIXTYPE accum;

    for (i = 0; i < maxnpp; i++) pp[i] = -1;	/* initially all undefined */
    *npp = 0;
    *colon = 0;
    *atsign = 0;
    do {
	if (*fmt == '\'') { /* character code */
	    pp[*npp] = *(++fmt);
	    gotone = 1;
	    fmt++;
	}
	else if (*fmt == 'v' || *fmt == 'V') { /* lisp arg is value */
	    accum = getfixnum(xlgafixnum());
	    if (accum < 0) accum = 0;	/* clip at reasonable values */
	    else if (accum>FMTMAX) accum = FMTMAX;
	    pp[*npp] = accum;
	    gotone = 1;
	    fmt++;
	}
	else if (isdigit(*fmt)) { /* integer literal */
	    accum = 0;
	    do {
		accum = accum*10 + (int)(*fmt++ - '0');
		if (accum > FMTMAX)
		    accum = FMTMAX; /* Clip at reasonable value */
	    } while (isdigit(*fmt));
	    gotone = 1;
	    pp[*npp] = accum;
	}
	else if (*fmt == ',') {	    /* empty field */
	    gotone = 1;
	}
	else  break;		    /* nothing to process */

	if (*fmt != ',') break;		/* no comma -- done */
	*npp += 1;		    /* got an argument */
	fmt++;				/* toss comma */
	if( *npp >= maxnpp ) toomanyopt(lfmt);
    } while (TRUE);
    *npp += gotone;

    do {    /* pick up any colon or atsign modifier */
	if (*fmt == ':') *colon = 1;
	else if (*fmt == '@') *atsign = 1;
	else break;
	fmt++;
    } while (TRUE);
    return fmt;
}

#define mincol	pp[0]
#define colinc	pp[1]
#define minpad	pp[2]
#define padchar pp[3]


/* opt_print - print a value using prefix parameter options */
#ifdef ANSI
static VOID NEAR opt_print(LVAL stream, LVAL val, int pflag, FIXTYPE *pp,
		    int colon, int atsign)
#else
LOCAL VOID opt_print(stream,val,pflag,pp,colon,atsign)
LVAL	stream;
LVAL	val;
int	pflag;		/* quoting or not */
FIXTYPE pp[];		/* prefix parameters */
int	colon;		/* colon modifier given? */
int	atsign;		/* at-sign modifier given? */
#endif
{
    int flatsize;
    int i;

    if (mincol < 0) mincol = 0; /* handle default values */
    if (colinc < 1) colinc = 1;	   /* also arg of 0 for colinc */
    if (minpad < 0) minpad = 0;
    if (padchar < 0) padchar = ' ';

    if( mincol < minpad )
	    mincol = minpad;

    if( mincol > 0 && atsign ) {	/* padding may be required on left */
	if (colon && null(val))		/* flat size is 2 */
	    flatsize = 2;
	else {
	    xlfsize = 0;
	    xlprint(NIL,val,pflag);	/* print to get the flat size */
	    flatsize = xlfsize;
	}
	for( i = 0; i < minpad; flatsize++, i++ )
	    xlputc(stream,(int)padchar);
	while( flatsize < mincol ) {
	    for( i = 0; i < colinc; i++ )
		xlputc(stream,(int)padchar);
	    flatsize += (int)colinc;
	}
    }

    /* print the value */
    if( colon && null(val)) {
	xlputstr(stream,"()");
	flatsize = 2;
    }
    else {
	xlfsize = 0;
	xlprint(stream,val,pflag);
	flatsize = xlfsize;
    }

    if( mincol > 0 && !atsign ) {	/* padding required on right */
	for( i = 0; i < minpad; flatsize++, i++ )
	    xlputc(stream,(int)padchar);
	while( flatsize < mincol ) {
	    for( i = 0; i < colinc; i++ )
		xlputc(stream,(int)padchar);
	    flatsize += (int)colinc;
	}
    }
}

#define round pp[1]
#ifdef ANSI
static VOID NEAR num_print(LVAL stream,LVAL val,int pflag,FIXTYPE *pp,int atsign)
#else
LOCAL VOID num_print(stream,val,pflag,pp,atsign)
LVAL	stream;
LVAL	val;
int	pflag;		/* quoting or not */
FIXTYPE pp[];		/* prefix parameters */
int	atsign;		/* at-sign modifier given? */
#endif
{
    char cmd[50];
    int fillchar, i;

    fillchar = (int)pp[(pflag=='D'? 1 : 2)];

    if (fillchar < 0) fillchar = ' ';

    if (pflag == 'D' && fixp(val)) { /* ~d and fixnum */
	sprintf(buf, (atsign?"%+ld":"%ld"), (long) getfixnum(val));
    }
    else if (pflag == 'D' || !(fixp(val) || floatp(val))) { /* not a number */
	padchar = colinc = minpad = -1; /* zap arg if provided */
	opt_print(stream,val,FALSE,pp,0,0);
	return;
    }
    else {  /* one of the floating point formats, and a number */
	FLOTYPE num = fixp(val) ? (FLOTYPE)getfixnum(val) : getflonum(val);

	if (pflag == 'F' && fabs(num) > 1e100)
	    pflag = 'E';    /* don't generate extra big number */
	strcpy(cmd,"%");
	if (atsign) strcat(cmd,"+");
	if (round >= 0) {
	    sprintf(buf, ".%d", (int) round);
	    strcat(cmd, buf);
	}
	buf[0] = tolower(pflag);
	buf[1] = '\0';
	strcat(cmd,buf);
	sprintf(buf, cmd, (double)num);
    }
    if (mincol > 0) {	/* need to fill */
	for (i = (int)mincol-strlen(buf); i-- > 0;)
	    xlputc(stream,fillchar);
    }
    xlputstr(stream,buf);
}

#ifdef BETTERIO
#undef colinc
/* tabulate */
#ifdef ANSI
static void NEAR tab_print(LVAL stream, FIXTYPE *pp, int atsign)
#else
LOCAL VOID tab_print(stream, pp, atsign)
LVAL stream;
FIXTYPE pp[];
int atsign;
#endif
{
    int pos = xlgetcolumn(stream);  /* where are we now??? */
    int count;			    /* number of spaces to insert */
    int column = (int)pp[0];	    /* desired column */
    int colinc = (int)pp[1];	    /* desired column increment */

    if (column < 0) column = 1; /* handle defaults */
    if (colinc < 0) colinc = 1;

    if (atsign) { /* relative */
	if (colinc == 0) colinc = 1;
	count = column + (colinc - (pos + column) % colinc) % colinc;
    }
    else { /* absolute */
	if (pos >= column) {
	    if (colinc > 0) {
		int k = (pos+ (colinc-1) - column)/colinc;
		count = column-pos + k*colinc;
		if (count==0) count = colinc;
	    }
	    else count = 0;
	}
	else count = column - pos;
    }
    while (count-- > 0)
	xlputc(stream, ' ');
}
#endif

#define MAXNPP	4
#endif

/* xformat - formatted output function */
LVAL xformat()
{
    char FAR *fmt;
    LVAL stream,val;
    int ch;
#ifdef ENHFORMAT
    LVAL lfmt;
    int npp;		/* number of prefix parameters */
    FIXTYPE pp[MAXNPP];	    /* list of prefix parameters */
    int colon, atsign;	/* : and @ modifiers given? */
#endif

    xlsave1(val);			/* TAA fix */

    /* get the stream and format string */
    stream = xlgetarg();
    if (null(stream)) {
	val = stream = newustream();
    }
    else {
	if (stream == true)
	    stream = getvalue(s_stdout);
					/* fix from xlispbug.417 */
	else if (streamp(stream)) {	/* copied from xlgetfile() */
		if (getfile(stream) == CLOSED)
			xlfail("file not open");
	}
	else if (!ustreamp(stream))
		xlbadtype(stream);
	val = NIL;
    }
#ifdef ENHFORMAT
    fmt = getstring(lfmt=xlgastring());
#else
    fmt = getstring(xlgastring());
#endif

    /* process the format string */
    while ((ch = *fmt++) != 0)
	if (ch == '~') {
#ifdef ENHFORMAT
	    fmt = decode_pp( fmt, pp, MAXNPP, &npp, &colon, &atsign, lfmt);
#endif
	    ch = *fmt++;
	    if (islower(ch)) ch = toupper(ch);
	    switch (ch) {
	    case '\0':
		xlerror("expecting a format directive",cvstring(fmt-1));
	    case 'A':
#ifdef ENHFORMAT
		opt_print(stream,xlgetarg(),FALSE,pp,colon,atsign);
#else
		xlprint(stream,xlgetarg(),FALSE);
#endif
		break;
	    case 'S':
#ifdef ENHFORMAT
		opt_print(stream,xlgetarg(),TRUE,pp,colon,atsign);
#else
		xlprint(stream,xlgetarg(),TRUE);
#endif
		break;
#ifdef ENHFORMAT
	    case 'D':
		if (npp > 2) toomanyopt(lfmt);
	    case 'E': case 'F': case 'G':
		if (npp > 3) toomanyopt(lfmt);
		num_print(stream,xlgetarg(),ch,pp,atsign);
		break;
#endif
#if defined(BETTERIO) & defined(ENHFORMAT)
	    case '&':
		if ( pp[0] < 0 ) pp[0] = 1;
		if ((pp[0])-- > 0)
		    xlfreshline(stream);
		while( (pp[0])-- > 0 )
		    xlterpri(stream);
		break;
	    case 'T':
		tab_print(stream,pp,atsign);
		break;
#endif
	    case '%':
#ifdef ENHFORMAT
		if( pp[0] < 0 ) pp[0] = 1;
		while( (pp[0])-- > 0 )
		    xlterpri(stream);
#else
		xlterpri(stream);
#endif
		break;
	    case '~':
#ifdef ENHFORMAT
		if( pp[0] <= 0 ) pp[0] = 1;
		while( (pp[0])-- > 0 )
		    xlputc(stream,'~');
#else
		xlputc(stream,'~');
#endif
		break;
	    case '\n':
#ifdef ENHFORMAT
		if( colon )
		    break;
		if( atsign )
		     xlterpri(stream);
#endif
		while (*fmt && *fmt != '\n' && isspace(*fmt))
		    ++fmt;
		break;
	    default:
		xlerror("unknown format directive",cvstring(fmt-1));
	    }
	}
	else
	    xlputc(stream,ch);

    /* NPM: bugfix by TAA comp.lang.lisp.x, 9/29/91 */
    if (!null(val)) val= getstroutput(val); 
    
    /* unprotect */
    xlpop();

    /* return the value */
    return (val);
}


/* getstroutput - get the output stream string (internal) */
LOCAL LVAL NEAR getstroutput(stream)
  LVAL stream;
{
    char FAR *str;
    LVAL next,val;
    unsigned len;	    /* TAA MOD */
    int ch;

    /* compute the length of the stream */
    for (len = 0, next = gethead(stream); !null(next); next = cdr(next)) {
	if (++len > MAXSLEN) xltoolong();   /* TAA MOD addition for overflow detect */
    }

    /* create a new string */
    val = newstring(len);

    /* copy the characters into the new string */
    str = getstring(val);
    while ((ch = xlgetc(stream)) != EOF)
	*str++ = ch;
    *str = '\0';

    /* return the string */
    return (val);
}

