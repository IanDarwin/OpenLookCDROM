/* -*-C-*-
********************************************************************************
*
* File:         xlsym.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlsym.c,v 2.7 1994/06/06 15:59:26 npm Exp $
* Description:  symbol handling routines
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:40 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlsym.c,v 2.7 1994/06/06 15:59:26 npm Exp $";

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

/* external variables */
extern LVAL obarray,s_unbound;
extern LVAL xlenv,xlfenv;
extern LVAL true;	/* Bug fix TAA */

/* forward declarations */
#ifdef ANSI
LOCAL LVAL NEAR findprop(LVAL sym, LVAL prp); /* NPM: changed this to LOCAL */
#else
LOCAL FORWARD LVAL findprop();	/* NPM: changed this to LOCAL */
#endif

/* xlenter - enter a symbol into the obarray */
LVAL xlenter(name)
  char *name;
{
    LVAL sym,array;
    int i;

#ifndef NILSYMBOL
    /* check for nil */
    if (strcmp(name,"NIL") == 0)
	return (NIL);
#endif

    /* check for symbol already in table */
    array = getvalue(obarray);
    i = hash(name,HSIZE);
    for (sym = getelement(array,i); !null(sym); sym = cdr(sym))
	if (STRCMP(name,getstring(getpname(car(sym)))) == 0)
	    return (car(sym));

    /* make a new symbol node and link it into the list */
    xlsave1(sym);
    sym = consd(getelement(array,i));
#ifdef NILSYMBOL
    if (strcmp(name,"NIL") == 0) rplaca(sym, NIL);
    else
#endif
	rplaca(sym,xlmakesym(name));
    setelement(array,i,sym);
    xlpop();

    /* return the new symbol */
    return (car(sym));
}

/* xlmakesym - make a new symbol node */
LVAL xlmakesym(name)
  char *name;
{
    LVAL sym;
    sym = cvsymbol(name);
    if (*name == ':') {
	setvalue(sym,sym);
#ifdef SPECIALS
	setsflags(sym, F_CONSTANT);
#endif
    }
#ifdef SPECIALS
    else setsflags(sym, F_NORMAL);
#endif
    return (sym);
}

/* xlgetvalue - get the value of a symbol (with check) */
LVAL xlgetvalue(sym)
  LVAL sym;
{
    LVAL val;

    /* look for the value of the symbol */
    while ((val = xlxgetvalue(sym)) == s_unbound)
	xlunbound(sym);

    /* return the value */
    return (val);
}

/* xlxgetvalue - get the value of a symbol */
LVAL xlxgetvalue(sym)
  LVAL sym;
{
    register LVAL fp,ep;
    LVAL val;

    /* check the environment list */
    for (fp = xlenv; !null(fp); fp = cdr(fp))

	/* check for an instance variable */
	if (!null(ep = car(fp)) && objectp(car(ep))) {
	    if (xlobgetvalue(ep,sym,&val))
		return (val);
	}

	/* check an environment stack frame */
	else {
	    for (; !null(ep); ep = cdr(ep))
		if (sym == car(car(ep)))
		    return (cdr(car(ep)));
	}

    /* return the global value */
    return (getvalue(sym));
}

/* xlsetvalue - set the value of a symbol */
VOID xlsetvalue(sym,val)
  LVAL sym,val;
{
    register LVAL fp,ep;

#ifdef SPECIALS
    if (constantp(sym)) {
	xlnoassign(sym);
	/* never returns */
    }
#else
    if ( sym == true ||
	 sym == s_unbound ||
	 (getstring(getpname(sym)))[0] == ':') {    /* Bug FIX	TAA */
	xlerror( "constant value", sym );
	/* never returns */
    }
#endif

    /* look for the symbol in the environment list */
    for (fp = xlenv; !null(fp); fp = cdr(fp))

	/* check for an instance variable */
	if (!null(ep = car(fp)) && objectp(car(ep))) {
	    if (xlobsetvalue(ep,sym,val))
		return;
	}

	/* check an environment stack frame */
	else {
	    for (; !null(ep); ep = cdr(ep))
		if (sym == car(car(ep))) {
		    rplacd(car(ep),val);
		    return;
		}
	}

    /* store the global value */
    setvalue(sym,val);
}

/* xlgetfunction - get the functional value of a symbol (with check) */
LVAL xlgetfunction(sym)
  LVAL sym;
{
    LVAL val;

    /* look for the functional value of the symbol */
    while ((val = xlxgetfunction(sym)) == s_unbound)
	xlfunbound(sym);

    /* return the value */
    return (val);
}

/* xlxgetfunction - get the functional value of a symbol */
LVAL xlxgetfunction(sym)
  LVAL sym;
{
    register LVAL fp,ep;

    /* check the environment list */
    for (fp = xlfenv; !null(fp); fp = cdr(fp))
	for (ep = car(fp); !null(ep); ep = cdr(ep))
	    if (sym == car(car(ep)))
		return (cdr(car(ep)));

    /* return the global value */
    return (getfunction(sym));
}

/* xlsetfunction - set the functional value of a symbol */
VOID xlsetfunction(sym,val)
  LVAL sym,val;
{
    register LVAL fp,ep;

    /* look for the symbol in the environment list */
    for (fp = xlfenv; !null(fp); fp = cdr(fp))
	for (ep = car(fp); !null(ep); ep = cdr(ep))
	    if (sym == car(car(ep))) {
		rplacd(car(ep),val);
		return;
	    }

    /* store the global value */
    setfunction(sym,val);
}

/* xlgetprop - get the value of a property */
LVAL xlgetprop(sym,prp)
  LVAL sym,prp;
{
    LVAL p;
    return (null(p = findprop(sym,prp)) ? NIL : car(p));
}

/* xlputprop - put a property value onto the property list */
VOID xlputprop(sym,val,prp)
  LVAL sym,val,prp;
{
    LVAL pair;
    if (!null(pair = findprop(sym,prp)))
	rplaca(pair,val);
    else
	setplist(sym,cons(prp,cons(val,getplist(sym))));
}

/* xlremprop - remove a property from a property list */
VOID xlremprop(sym,prp)
  LVAL sym,prp;
{
    LVAL last,p;
    last = NIL;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(last)) {
	if (car(p) == prp)
	    if (!null(last))
		rplacd(last,cdr(cdr(p)));
	    else
		setplist(sym,cdr(cdr(p)));
	last = cdr(p);
    }
}

/* findprop - find a property pair */
LOCAL LVAL NEAR findprop(sym,prp)
  LVAL sym,prp;
{
    LVAL p;
    for (p = getplist(sym); consp(p) && consp(cdr(p)); p = cdr(cdr(p)))
	if (car(p) == prp)
	    return (cdr(p));
    return (NIL);
}

/* hash - hash a symbol name string */
int hash(str,len)
  char FAR *str;
  int len;
{
    int i;
    for (i = 0; *str; )
	i = (i << 2) ^ *str++;
    i %= len;
    return (i < 0 ? -i : i);
}

/* xlhash -- hash any xlisp object */
/* TAA extension */
int xlhash(obj,len)
    LVAL obj;
    int len;
{
    int i;
    unsigned long tot;
    union {FIXTYPE i; float j; unsigned FIXTYPE k;} swizzle;

    hashloop:	/* iterate on conses */
#ifndef NILSYMBOL
    if (null(obj)) return 0;
#endif
    switch (ntype(obj)) {
	case SYMBOL:
	    obj = getpname(obj);
	case STRING:
	    return hash(getstring(obj),len);
	case SUBR: case FSUBR:
	    return getoffset(obj) % len;
	case FIXNUM:
	    swizzle.i = getfixnum(obj);
	    return (int) (swizzle.k % len);
	case FLONUM:
	    swizzle.j = getflonum(obj);
	    return (int) (swizzle.k % len);
	case CHAR:
	    return getchcode(obj) % len;
	case CONS: case USTREAM:
	    obj = car(obj);	/* just base on CAR */
	    goto hashloop;
	case STREAM:
	    return 0;	/* nothing we can do on this */

#if (defined(UNIX) || defined(WINTERP))
	case XLTYPE_PIPE:
	    return (0);		/* nothing we can do on this */
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP 
	case XLTYPE_XtAccelerators:
	    swizzle.i = (FIXTYPE) get_xtaccelerators(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_XtTranslations:
	    swizzle.i = (FIXTYPE) get_xttranslations(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_XEvent:
	    return (0);
	case XLTYPE_Window:
	    swizzle.i = (FIXTYPE) get_window(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_Pixel:
	    swizzle.i = (FIXTYPE) get_xpixel(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_Pixmap:
	    swizzle.i = (FIXTYPE) get_pixmap(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_XImage:
	    swizzle.i = (FIXTYPE) get_ximage(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_XmString: {
            char* str;
            int   hashval;
            if (XmStringGetLtoR(get_xmstring(obj),
#if (XmVersion >= 1002)	/* Motif 1.2 -- NPM mod */
				XmFONTLIST_DEFAULT_TAG,
#else /* Motif 1.1 or 1.0 -- NPM mod */
				XmSTRING_DEFAULT_CHARSET,
#endif /* WINTERP_MOTIF_12 -- NPM mod */
				&str)) {
              hashval = hash(str, len);
              XtFree((char*) str);
              return (hashval);
            }
            else return (0);
	}
	case XLTYPE_XT_RESOURCE:
	    swizzle.i = (FIXTYPE) get_xtresource(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_TIMEOUTOBJ:
	    swizzle.i = (FIXTYPE) get_timeout_id(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_FDINPUTCBOBJ:
	    swizzle.i = (FIXTYPE) get_fdinputcb_id(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_WIDGETOBJ:
	    swizzle.i = (FIXTYPE) get_widgetobj_widgetID(obj);
	    return (int) (swizzle.k % len);
#ifdef WINTERP_XTANGO_WIDGET
	case XLTYPE_TANGOIMAGEOBJ:
	    swizzle.i = (FIXTYPE) get_tangoimageobj_timageID(obj);
	    return (int) (swizzle.k % len);
	case XLTYPE_TANGO_PATH:
	    swizzle.i = (FIXTYPE) get_tangopath(obj);
	    return (int) (swizzle.k % len);
        case XLTYPE_TANGO_TRANS:
	    swizzle.i = (FIXTYPE) get_tangotrans(obj);
	    return (int) (swizzle.k % len);
#endif /* WINTERP_XTANGO_WIDGET */
        case OBJECT:
        case VECTOR:
        case CLOSURE:
#ifdef STRUCTS
        case STRUCT:
#endif /* STRUCTS */
#ifdef COMPLX
        case COMPLEX:
#endif /* COMPLX */
	    /* for all "pure array" types */
	    for (i = getsize(obj), tot = 0; i-- > 0;)
		tot += (unsigned)xlhash(getelement(obj,i),len);
	    return (int)(tot % len);
        case XLTYPE_CALLBACKOBJ:
        case XLTYPE_EVHANDLEROBJ:
	case XLTYPE_PIXMAP_REFOBJ:
	    /*
	     * the above are hybrid arrays so they can't be processed as above.
	     * nor do they have Motif or Xt generated id's that can be used w/ 
	     * 'swizzle' union as in above...
	     */
            return (0);
        default:
	    return (0);

#else /* !defined(WINTERP) */

	default:    /* all array types */
	    for (i = getsize(obj), tot = 0; i-- > 0;)
		tot += (unsigned)xlhash(getelement(obj,i),len);
	    return (int)(tot % len);

#endif /* WINTERP */

    }
}

#ifdef SPECIALS
/* unbind a variable/constant */
LVAL xmakunbound()
{
    LVAL sym;

    sym = xlgasymbol();
    xllastarg();

    if (constantp(sym))
	xlerror("can't unbind constant", sym);

    setvalue(sym, s_unbound);
    setsflags(sym, F_NORMAL);
    return(sym);
}


/* define a constant -- useful in initialization */

VOID defconstant(sym, val)
  LVAL sym, val;
{
    setvalue(sym, val);
    setsflags(sym, F_CONSTANT | F_SPECIAL);
}

/* DEFCONSTANT DEFPARAMETER and DEFVAR */

LVAL xdefconstant()
{
    LVAL sym, val;

    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    /* evaluate constant value */
    val = xleval(val);

#ifdef NILSYMBOL
    if (null(sym)) xlfail("can't redefine NIL");
#endif

    if (specialp(sym)) {
	if (constantp(sym)) {
	    if (!eql(getvalue(sym),val)) {
		errputstr("WARNING-- redefinition of constant ");
		errprint(sym);
	    }
	}
	else xlerror("can't make special variable into a constant", sym);
    }

    defconstant(sym, val);

    return(sym);
}


LVAL xdefparameter()
{
    LVAL sym, val;

    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

    if (constantp(sym)) xlnoassign(sym);

    setvalue(sym, xleval(val));
    setsflags(sym, F_SPECIAL);
    return(sym);
}

LVAL xdefvar()
{
    LVAL sym, val=NIL;

    sym = xlgasymbol();
    if (moreargs()) {
	val = xlgetarg();
	xllastarg();
    }

    if (constantp(sym)) xlnoassign(sym);

    if (getvalue(sym) == s_unbound) setvalue(sym, xleval(val));
    setsflags(sym, F_SPECIAL);
    return(sym);
}

#endif

/* xlsinit - symbol initialization routine */
VOID xlsinit()
{
    LVAL array,p;

    /* initialize the obarray */
    obarray = xlmakesym("*OBARRAY*");
    array = newvector(HSIZE);
    setvalue(obarray,array);

    /* add the symbol *OBARRAY* to the obarray */
    p = consa(obarray);
    setelement(array,hash("*OBARRAY*",HSIZE),p);

}
