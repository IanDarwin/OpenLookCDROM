/* -*-C-*-
********************************************************************************
*
* File:         xlprint.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlprin.c,v 2.5 1994/06/06 15:59:22 npm Exp $
* Description:  xlisp print routine
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:54 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlprin.c,v 2.5 1994/06/06 15:59:22 npm Exp $";

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

#ifdef WINTERP
extern LVAL
Wres_Get_Symbol	/* w_resources.c */
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL res
#endif /* _NO_PROTO */
 );
#endif /* WINTERP */

/* external variables */
extern LVAL s_printcase,k_downcase,k_const,k_nmacro;
extern LVAL s_ifmt,s_ffmt;
extern LVAL obarray;
extern FUNDEF funtab[];
#ifdef PRINDEPTH
extern LVAL s_printlevel, s_printlength;	/* TAA mod */
#endif
#ifdef HASHFCNS
extern LVAL a_hashtable;
#endif

/* forward declarations */
#ifdef ANSI
#ifdef COMMONLISP
LOCAL void NEAR putsymbol(LVAL fptr, char FAR *str); /* NPM: changed this to LOCAL */
#else /* !defined(COMMONLISP) */
LOCAL void NEAR putsymbol(LVAL fptr, char FAR *str, int escflag); /* NPM: changed this to LOCAL */
#endif /* COMMONLISP */
LOCAL void NEAR putstring(LVAL fptr, LVAL str);	/* NPM: changed this to LOCAL */
LOCAL void NEAR putqstring(LVAL fptr, LVAL str); /* NPM: changed this to LOCAL */
LOCAL void NEAR putatm(LVAL fptr, char *tag, LVAL val);	/* NPM: changed this to LOCAL */
LOCAL void NEAR putsubr(LVAL fptr, char *tag, LVAL val); /* NPM: changed this to LOCAL */
LOCAL void NEAR putclosure(LVAL fptr, LVAL val); /* NPM: changed this to LOCAL */
LOCAL void NEAR putfixnum(LVAL fptr, FIXTYPE n); /* NPM: changed this to LOCAL */
LOCAL void NEAR putflonum(LVAL fptr, FLOTYPE n); /* NPM: changed this to LOCAL */
LOCAL void NEAR putchcode(LVAL fptr, int ch, int escflag); /* NPM: changed this to LOCAL */
LOCAL void NEAR putoct(LVAL fptr, int n); /* NPM: changed this to LOCAL */
#else /* !defined(ANSI) */
LOCAL FORWARD VOID putsymbol();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putstring();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putqstring(); /* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putatm();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putsubr();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putclosure(); /* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putfixnum();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putflonum();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putchcode();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID putoct();	/* NPM: changed this to LOCAL */
#endif /* ANSI */

#ifdef PRINDEPTH
#ifdef ANSI
void xlprintl(LVAL fptr, LVAL vptr, int flag);
#else /* ANSI */
FORWARD VOID xlprintl();
#endif /* ANSI */

int plevel,plength;

/* xlprint - print an xlisp value */
VOID xlprint(fptr,vptr,flag)
  LVAL fptr,vptr; int flag;
{
    LVAL temp;
    temp = getvalue(s_printlevel);
    if (fixp(temp) && getfixnum(temp) <= 32767 && getfixnum(temp) >= 0) {
	plevel = (int)getfixnum(temp);
    }
    else {
	plevel = 32767;	    /* clamp to "reasonable" level */
    }
    temp = getvalue(s_printlength);
    if (fixp(temp) && getfixnum(temp) <= 32767 && getfixnum(temp) >= 0) {
	plength = (int)getfixnum(temp);
    }
    else
	plength = 32767;
    xlprintl(fptr,vptr,flag);
}

VOID xlprintl(fptr,vptr,flag)
#else /* !defined(PRINDEPTH) */
#define xlprintl xlprint		/* alias */
VOID xlprint(fptr,vptr,flag)
#endif /* PRINDEPTH */
  LVAL fptr,vptr; int flag;
{
    LVAL nptr,next;
    int n,i;
#ifdef PRINDEPTH
    int llength;
#endif /* PRINDEPTH */

#ifndef NILSYMBOL
    /* print nil */
    if (vptr == NIL) {
	xlputstr(fptr,
	    (((!flag) || (getvalue(s_printcase) != k_downcase))?"NIL":"nil"));
	return;
    }
#endif /* NILSYMBOL */

    /* check value type */
    switch (ntype(vptr)) {
    case SUBR:
	    putsubr(fptr,"Subr",vptr);
	    break;
    case FSUBR:
	    putsubr(fptr,"FSubr",vptr);
	    break;
    case CONS:
#ifdef PRINDEPTH
	    if (plevel-- == 0) {	    /* depth limitation */
		xlputc(fptr,'#');
		plevel++;
		break;
	    }
#endif /* PRINDEPTH */
	    xlputc(fptr,'(');
#ifdef PRINDEPTH
	    llength = plength;
#endif /* PRINDEPTH */
	    for (nptr = vptr; nptr != NIL; nptr = next) {
#ifdef PRINDEPTH
		if (llength-- == 0) { /* length limitiation */
		    xlputstr(fptr,"... ");
		    break;
		}
#endif /* PRINDEPTH */
		xlprintl(fptr,car(nptr),flag);
		if ((next = cdr(nptr)) != NIL)
		    if (consp(next))
			xlputc(fptr,' ');
		    else {
			xlputstr(fptr," . ");
			xlprintl(fptr,next,flag);
			break;
		    }
	    }
	    xlputc(fptr,')');
#ifdef PRINDEPTH
	    plevel++;
#endif /* PRINDEPTH */
	    break;
    case SYMBOL:
#ifdef COMMONLISP   /* check for uninterned symbol */
	{
	    char FAR *str = getstring(getpname(vptr));
	    if (flag) {
		next = getelement(getvalue(obarray), hash(str, HSIZE));
		for (; !null(next); next = cdr(next))
		    if (car(next) == vptr) goto doprintsym;
		xlputstr(fptr,"#:");
		doprintsym: putsymbol(fptr,str);
	    }
#ifdef MEDMEM
	    else putstring(fptr,getpname(vptr));
#else /* !defined(MEDMEM) */
	    else xlputstr(fptr,str);
#endif /* MEDMEM */
	    break;
	}
#else /* !defined(COMMONLISP) */
	    putsymbol(fptr,getstring(getpname(vptr)),flag);
	    break;
#endif /* COMMONLISP */
    case FIXNUM:
	    putfixnum(fptr,getfixnum(vptr));
	    break;
    case FLONUM:
	    putflonum(fptr,getflonum(vptr));
	    break;
    case CHAR:
	    putchcode(fptr,getchcode(vptr),flag);
	    break;
    case STRING:
	    if (flag)
		putqstring(fptr,vptr);
	    else
		putstring(fptr,vptr);
	    break;
    case STREAM:
#ifdef BETTERIO
#ifdef FILETABLE
	{
	    char *msg;
	    FILEP fp = getfile(vptr);
	    if (fp == CLOSED)	xlputstr(fptr, "#<Closed-Stream>");
	    else {
		switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING)) {
		    case S_FORREADING: msg = "Input-Stream"; break;
		    case S_FORWRITING: msg = "Output-Stream"; break;
		    default: msg = "IO-Stream"; break;
		}
		sprintf(buf,"#<%s %d:\"%s\">", msg, fp+1, filetab[fp].name);
		xlputstr(fptr,buf);
	    }
	}
#else /* !defined(FILETABLE) */
	{
	    char *msg;
	    FILEP fp = getfile(vptr);
	    if (fp == CLOSED)	msg = "Closed-Stream";
	    else if (fp == STDIN) msg = "Stdin-Stream";
	    else if (fp == STDOUT) msg = "Stdout-Stream";
	    else if (fp == CONSOLE) msg = "Terminal-Stream";
	    else switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING)) {
		case S_FORREADING: msg = "Input-Stream"; break;
		case S_FORWRITING: msg = "Output-Stream"; break;
		default: msg = "IO-Stream"; break;
	    }
	    putatm(fptr,msg,vptr);
	}
#endif /* FILETABLE */
#else /* !defined(BETTERIO) */
	putatm(fptr,"File-Stream",vptr);
#endif /* BETTERIO */
	break;
    case USTREAM:
	    putatm(fptr,"Unnamed-Stream",vptr);
	    break;
    case OBJECT:
#ifdef OBJPRNT
	    /* putobj fakes a (send obj :prin1 file) call */
	    putobj(fptr,vptr);
#else /* OBJPRNT */
	    putatm(fptr,"Object",vptr);
#endif /* OBJPRNT */
	    break;
    case VECTOR:
#ifdef PRINDEPTH
	    if (plevel-- == 0) {	    /* depth limitation */
		xlputc(fptr,'#');
		plevel++;
		break;
	    }
#endif /* PRINDEPTH */
	    xlputc(fptr,'#'); xlputc(fptr,'(');
#ifdef PRINDEPTH
	    llength = plength;
#endif /* PRINDEPTH */
	    for (i = 0, n = getsize(vptr); n-- > 0; ) {
#ifdef PRINDEPTH
		if (llength-- == 0) { /* length limitiation */
		    xlputstr(fptr,"... ");
		    break;
		}
#endif /* PRINDEPTH */
		xlprintl(fptr,getelement(vptr,i++),flag);
		if (n) xlputc(fptr,' ');
	    }
	    xlputc(fptr,')');
#ifdef PRINDEPTH
	    plevel++;
#endif /* PRINDEPTH */
	    break;
#ifdef STRUCTS
    case STRUCT:
#ifdef HASHFCNS
	    if (getelement(vptr,0) == a_hashtable) {
		putatm(fptr,"Hash-table",vptr);
		break;
	    }
#endif /* HASHFCNS */
	    xlprstruct(fptr,vptr,flag);
	    break;
#endif /* STRUCTS */
    case CLOSURE:
	    putclosure(fptr,vptr);
	    break;
#ifdef COMPLX
    case COMPLEX:
	xlputstr(fptr, "#C(");
	if (ntype(next = getelement(vptr,0)) == FIXNUM)
	    putfixnum(fptr, getfixnum(next));
	else
	    putflonum(fptr, getflonum(next));
	xlputc(fptr,' ');
	if (ntype(next = getelement(vptr,1)) == FIXNUM)
	    putfixnum(fptr, getfixnum(next));
	else
	    putflonum(fptr, getflonum(next));
	xlputc(fptr, ')');
	break;
#endif /* COMPLX */
    case FREE:
	    putatm(fptr,"Free",vptr);
	    break;

#ifdef WINTERP
    case XLTYPE_XtAccelerators:
	    putatm(fptr, "XtAccelerators", vptr);
	    break;
    case XLTYPE_XtTranslations:
	    putatm(fptr, "XtTranslations", vptr);
	    break;
    case XLTYPE_XEvent:
	    putatm(fptr, "XEvent", vptr);
	    break;
    case XLTYPE_Window:
	    putatm(fptr, "Window", vptr);
	    break;
    case XLTYPE_Pixel:
	    putatm(fptr, "Pixel", vptr);
	    break;
    case XLTYPE_Pixmap:
	    putatm(fptr, "Pixmap", vptr);
	    break;
    case XLTYPE_XImage:
	    putatm(fptr, "XImage", vptr);
	    break;
    case XLTYPE_XmString:
	    putatm(fptr, "XmString", vptr);
	    break;
    case XLTYPE_XT_RESOURCE:
#ifdef COMMONLISP
	    putsymbol(fptr,
		      getstring(getpname(Wres_Get_Symbol(vptr))));
#else /* !defined(COMMONLISP) */
	    putsymbol(fptr,
		      getstring(getpname(Wres_Get_Symbol(vptr))),
		      flag);
#endif /* COMMONLISP */
	    break;
    case XLTYPE_CALLBACKOBJ:
	    putatm(fptr, "CALLBACK-OBJ", vptr);
	    break;
    case XLTYPE_TIMEOUTOBJ:
	    putatm(fptr, "TIMEOUT-OBJ", vptr);
	    break;
    case XLTYPE_PIXMAP_REFOBJ:
	    putatm(fptr, "PIXMAP-REFOBJ", vptr);
	    break;
    case XLTYPE_EVHANDLEROBJ:
	    putatm(fptr, "EVHANDLER-OBJ", vptr);
	    break;
    case XLTYPE_FDINPUTCBOBJ:
	    putatm(fptr, "FDINPUTCB-OBJ", vptr);
	    break;
#ifdef WINTERP_XTANGO_WIDGET
    case XLTYPE_TANGO_PATH:
	    putatm(fptr, "TANGO_PATH", vptr);
	    break;
    case XLTYPE_TANGO_TRANS:
	    putatm(fptr, "TANGO_TRANS", vptr);
	    break;
    case XLTYPE_TANGOIMAGEOBJ:
	    /* fall through to XLTYPE_WIDGETOBJ case... */
#endif /* WINTERP_XTANGO_WIDGET */
    case XLTYPE_WIDGETOBJ:
	    /* WINTERP versions <= 1.12 used Wcls_Print_WIDGETOBJ(fptr, vptr);
	       from w_classes.c.
	       Versions > 1.12 expect a method :PRIN1 on WIDGET_CLASS. */
	    /* putobj fakes a (send obj :prin1 file) call */
	    putobj(fptr,vptr);
	    break;
#endif /* WINTERP */

#if (defined(UNIX) || defined(WINTERP))
    case XLTYPE_PIPE:
#ifdef BETTERIO
	    if (getfile(vptr) == CLOSED)
	      putatm(fptr, "Closed-Pipe-Stream", vptr);
	    else
	      switch (vptr->n_sflags & (S_FORREADING | S_FORWRITING)) {
	      case S_FORREADING:
		putatm(fptr, "Input-Pipe-Stream", vptr);
		break;
	      case S_FORWRITING:
		putatm(fptr, "Output-Pipe-Stream", vptr);
		break;
	      default:
		putatm(fptr, "IO-Pipe-Stream", vptr);
		break;
	      }
#else /* !defined(BETTERIO) */
	    putatm(fptr, "Pipe-Stream", vptr);
#endif /* BETTERIO */
	    break;
#endif /* (defined(UNIX) || defined(WINTERP)) */

    default:
	    putatm(fptr,"Unknown",vptr);	/* was 'Foo`   TAA Mod */
	    break;
    }
}

/* xlterpri - terminate the current print line */
VOID xlterpri(fptr)
  LVAL fptr;
{
    xlputc(fptr,'\n');
}

#ifdef BETTERIO
extern int lposition;	/* imported from the *stuff.c file */
/* xlgetcolumn -- find the current file column */

int xlgetcolumn(fptr)
  LVAL fptr;
{
    if (fptr == NIL) return 0;
    else if (ntype(fptr) == USTREAM) { /* hard work ahead :-( */
	LVAL ptr = gethead(fptr);
	int count = 0;

	while (ptr != NIL) {
	    if (getchcode(ptr) == '\n') count = 0 ;
	    else count++;
	    ptr = cdr(ptr);
	}
	return count;
    }
    else if (getfile(fptr) == CONSOLE)
	return lposition;
    else
	return ((fptr->n_sflags & S_WRITING)? fptr->n_cpos : 0);
}


/* xlfreshline -- start new line if not at beginning of line */
int xlfreshline(fptr)
  LVAL fptr;
{
    if (xlgetcolumn(fptr) != 0) {
	xlterpri(fptr);
	return TRUE;
    }
    return FALSE;
}
#endif /* BETTERIO */


/* xlputstr - output a string */
VOID xlputstr(fptr,str)		/* NPM: TAA fix to xlputstr() comp.lang.lisp.x 9/29/91 */
  LVAL fptr; char *str;
{
/* solve reentrancy problems if gc prints messages and
   xlputstr output is directed to a string stream */
    if (ustreamp(fptr)) {
        int oplevel=plevel, oplength=plength;   /* save these variables */
        char nbuf[STRMAX+1];
        
        if (buf == str) {   /* copy to reentrant buffer if necessary */
            str = strcpy(nbuf, buf);
        }
        
        while (*str)        /* print string */
            xlputc(fptr, *str++);
 
        plevel = oplevel;   /* restore level and length */
        plength = oplength;
    }
    else
        while (*str)
            xlputc(fptr,*str++);
}


/* putsymbol - output a symbol */
#ifdef COMMONLISP
LOCAL VOID NEAR putsymbol(fptr,stri)
  LVAL fptr; char FAR *stri;
#else /* !defined(COMMONLISP) */
LOCAL VOID NEAR putsymbol(fptr,stri,escflag)
  LVAL fptr; char FAR *stri; int escflag;
#endif /* COMMONLISP */
{
    int downcase;
    LVAL type;
    char *p,c;
#ifdef MEDMEM
    char *str = buf;

    STRCPY(buf, stri);
#else /* !defined(MEDMEM) */
#define str stri
#endif /* MEDMEM */

#ifndef COMMONLISP
    /* check for printing without escapes */
    if (!escflag) {
	xlputstr(fptr,str);
	return;
    }
#endif /* COMMONLISP */
    /* check to see if symbol needs escape characters */
/*  if (tentry(*str) == k_const) {*/	/* always execute this code! TAA Mod*/
	for (p = str; *p; ++p)
	    if (islower(*p)
	    ||	((type = tentry(*p)) != k_const
	      && (!consp(type) || car(type) != k_nmacro))) {
		xlputc(fptr,'|');
		while (*str) {
		    if (*str == '\\' || *str == '|')
			xlputc(fptr,'\\');
		    xlputc(fptr,*str++);
		}
		xlputc(fptr,'|');
		return;
	    }
/*  } */

    /* get the case translation flag */
    downcase = (getvalue(s_printcase) == k_downcase);

    /* check for the first character being '#' */
    if (*str == '#' || isnumber(str,NULL))
	xlputc(fptr,'\\');

    /* output each character */
    while ((c = *str++) != 0) {
	/* don't escape colon until we add support for packages */
	if (c == '\\' || c == '|' /* || c == ':' */)
	    xlputc(fptr,'\\');
	xlputc(fptr,(downcase && isupper(c) ? tolower(c) : c));
    }
}
#ifndef MEDMEM
#undef str
#endif /* MEDMEM */

/* putstring - output a string */
/* rewritten to	 print strings containing nulls TAA mod*/
LOCAL VOID NEAR putstring(fptr,str)
  LVAL fptr,str;
{
    char FAR *p = getstring(str);
    unsigned len = getslength(str);

    /* output each character */
    while (len-- > 0) xlputc(fptr,*p++);
}

/* putqstring - output a quoted string */
/* rewritten to	 print strings containing nulls TAA mod*/
LOCAL VOID NEAR putqstring(fptr,str)
  LVAL fptr,str;
{
    char FAR *p = getstring(str);
    unsigned len = getslength(str);
    int ch;

    /* output the initial quote */
    xlputc(fptr,'"');

    /* output each character in the string */
    while (len-- > 0) {
	ch = *(unsigned char FAR *)p++;

	/* check for a control character */
	if (ch < 040 || ch == '\\' || ch == '"' || ch > 0176) { /* TAA MOD quote quote */
	    xlputc(fptr,'\\');
	    switch (ch) {
		case '\011':
		    xlputc(fptr,'t');
		    break;
		case '\012':
		    xlputc(fptr,'n');
		    break;
		case '\014':
		    xlputc(fptr,'f');
		    break;
		case '\015':
		    xlputc(fptr,'r');
		    break;
		case '\\':
		case '"':
		    xlputc(fptr,ch);
		    break;
		default:
		    putoct(fptr,ch);
		    break;
	    }
	}

		/* output a normal character */
	else
	    xlputc(fptr,ch);
    }


    /* output the terminating quote */
    xlputc(fptr,'"');
}

/* putatm - output an atom */
LOCAL VOID NEAR putatm(fptr,tag,val)
  LVAL fptr; char *tag; LVAL val;
{
    sprintf(buf,"#<%s: #",tag); xlputstr(fptr,buf);
    sprintf(buf,AFMT,val); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

/* putsubr - output a subr/fsubr */
LOCAL VOID NEAR putsubr(fptr,tag,val)
  LVAL fptr; char *tag; LVAL val;
{
/*    sprintf(buf,"#<%s-%s: #",tag,funtab[getoffset(val)].fd_name); */
    char *str;	    /* TAA mod */
    if ((str = funtab[getoffset(val)].fd_name) != NULL)
	sprintf(buf,"#<%s-%s: #",tag,str);
    else
	sprintf(buf,"#<%s: #",tag);
    xlputstr(fptr,buf);
    sprintf(buf,AFMT,val); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

/* putclosure - output a closure */
LOCAL VOID NEAR putclosure(fptr,val)
  LVAL fptr,val;
{
    LVAL name;
    if ((name = getname(val)) != NIL)
	sprintf(buf,"#<Closure-%s: #",getstring(getpname(name)));
    else
	strcpy(buf,"#<Closure: #");
    xlputstr(fptr,buf);
    sprintf(buf,AFMT,val); xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

/* putfixnum - output a fixnum */
LOCAL VOID NEAR putfixnum(fptr,n)
  LVAL fptr; FIXTYPE n;
{
    LVAL val;
#ifdef MEDMEM
    char fmt[STRMAX];
    STRCPY(fmt, ((val = getvalue(s_ifmt)) != NIL) &&
	stringp(val) && getslength(val) < STRMAX ?
	getstring(val) : (char FAR *)IFMT);
#else /* !defined(MEDMEM) */
    char *fmt;

    fmt = (((val = getvalue(s_ifmt)) != NIL) && stringp(val) ? getstring(val)
	: IFMT);
#endif /* MEDMEM */
    sprintf(buf,fmt,n);
    xlputstr(fptr,buf);
}

/* putflonum - output a flonum */
LOCAL VOID NEAR putflonum(fptr,n)
  LVAL fptr; FLOTYPE n;
{
#ifdef MEDMEM
    char fmt[STRMAX];
#else /* !defined(MEDMEM) */
    char *fmt;
#endif /* MEDMEM */
    LVAL val;
#ifdef IEEEFP
    union { FLOTYPE fpn; long intn[2]; } k/*ludge*/;

    k.fpn = n;
    if ((k.intn[1] & 0x7fffffffL) == 0x7ff00000L && k.intn[0] == 0) {
	xlputstr(fptr,k.intn[1]<0 ? "-INF" : "+INF");
	return;
    }
    if ((k.intn[1]&0x7ff00000L) == 0x7ff00000L &&
	((k.intn[1]&0xfffffL) != 0 || k.intn[0] != 0)) {
	xlputstr(fptr,"NaN");
	return;
    }
#endif /* IEEEFP */

#ifdef MEDMEM
    _fstrcpy(fmt, ((val = getvalue(s_ffmt)) != NIL) &&
	stringp(val) && getslength(val) < STRMAX ?
	getstring(val) : (char FAR *)"%g");
#else /* !defined(MEDMEM) */
    fmt = (((val = getvalue(s_ffmt)) != NIL) && stringp(val) ? getstring(val)
	: "%g");
#endif /* MEDMEM */
    sprintf(buf,fmt,n);
    xlputstr(fptr,buf);
}

/* putchcode - output a character */
/* modified to print control and meta characters TAA Mod */
LOCAL VOID NEAR putchcode(fptr,ch,escflag)
  LVAL fptr; int ch,escflag;
{
    if (escflag) {
	xlputstr(fptr,"#\\");
	if (ch > 127) {
	    ch -= 128;
	    xlputstr(fptr,"M-");
	}
	switch (ch) {
	    case '\n':
		xlputstr(fptr,"Newline");
		break;
	    case ' ':
		xlputstr(fptr,"Space");
		break;
	    case 127:
		xlputstr(fptr,"Rubout");
		break;
	    default:
		if (ch < 32) {
		    ch += '@';
		    xlputstr(fptr,"C-");
		}
		xlputc(fptr,ch);
		break;
	}
    }
    else xlputc(fptr,ch);
}

/* putoct - output an octal byte value */
LOCAL VOID NEAR putoct(fptr,n)
  LVAL fptr; int n;
{
    sprintf(buf,"%03o",n);
    xlputstr(fptr,buf);
}
