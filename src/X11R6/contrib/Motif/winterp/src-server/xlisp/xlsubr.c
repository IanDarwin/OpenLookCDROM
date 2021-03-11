/* -*-C-*-
********************************************************************************
*
* File:         xlsubr.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlsubr.c,v 2.6 1994/06/06 15:59:25 npm Exp $
* Description:  xlisp builtin function support routines
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:42 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlsubr.c,v 2.6 1994/06/06 15:59:25 npm Exp $";

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
extern LVAL k_test,k_tnot,s_eql;
#ifdef BETTERIO
extern LVAL true, s_termio, s_stdin, s_stdout;
#endif

/* xlsubr - define a builtin function */
#ifdef ANSI
LVAL xlsubr(char *sname, int type, LVAL (*fcn)(void),int offset)
#else
LVAL xlsubr(sname,type,fcn,offset)
  char *sname; int type; LVAL (*fcn)(); int offset;
#endif
{
    LVAL sym;
    sym = xlenter(sname);
    setfunction(sym,cvsubr(fcn,type,offset));
    return (sym);
}

/* xlgetkeyarg - get a keyword argument */
int xlgetkeyarg(key,pval)
  LVAL key,*pval;
{
    LVAL *argv=xlargv;
    int argc=xlargc;
    for (argv = xlargv, argc = xlargc; (argc -= 2) >= 0; argv += 2) {
	if (*argv == key) {
	    *pval = *++argv;

	    /* delete the used argument */
	    if (argc>0) memcpy(argv-1, argv+1, argc*sizeof(LVAL));
	    xlargc -=2;

	    return (TRUE);
	}
    }
    return (FALSE);
}

/* xlgkfixnum - get a fixnum keyword argument */
int xlgkfixnum(key,pval)
  LVAL key,*pval;
{
    if (xlgetkeyarg(key,pval)) {
	if (!fixp(*pval))
	    xlbadtype(*pval);
	return (TRUE);
    }
    return (FALSE);
}

/* xltest - get the :test or :test-not keyword argument */
VOID xltest(pfcn,ptresult)
  LVAL *pfcn; int *ptresult;
{
    if (xlgetkeyarg(k_test,pfcn))	/* :test */
	*ptresult = TRUE;
    else if (xlgetkeyarg(k_tnot,pfcn))	/* :test-not */
	*ptresult = FALSE;
    else {
	*pfcn = getfunction(s_eql);
	*ptresult = TRUE;
    }
}

/* xlgetfile - get a file or stream */
#ifdef BETTERIO
LVAL xlgetfile(outflag)
  int outflag;
{
    LVAL arg;

    /* get a file or stream (cons) or nil */
    if (null(arg = xlgetarg()))
	return getvalue(outflag ? s_stdout: s_stdin);
    else if (streamp(arg)) {
	if (getfile(arg) == CLOSED)
	    xlfail("file not open");
    }
    else if (arg == true)
	return getvalue(s_termio);
    else if (!ustreamp(arg))
	xlbadtype(arg);
    return arg;
}
#else
LVAL xlgetfile()
{
    LVAL arg;

    /* get a file or stream (cons) or nil */
    if (!null(arg = xlgetarg())) {
	if (streamp(arg)) {
	    if (getfile(arg) == NULL)
		xlfail("file not open");
	}
	else if (!ustreamp(arg))
	    xlbadtype(arg);
    }
    return (arg);
}
#endif

/* xlgetfname - get a filename */
LVAL xlgetfname()
{
    LVAL name;

    /* get the next argument */
    name = xlgetarg();

    /* get the filename string */
    if (symbolp(name))
	name = getpname(name);
    else if (!stringp(name))
	xlbadtype(name);

    if (getslength(name) >= FNAMEMAX)
	xlerror("file name too long", name);

    /* return the name */
    return (name);
}

/* needsextension - check if a filename needs an extension */
int needsextension(name)
  char *name;
{
    char *p;

    /* check for an extension */
    for (p = &name[strlen(name)]; --p >= &name[0]; )
	if (*p == '.')
	    return (FALSE);
	else if (!islower(*p) && !isupper(*p) && !isdigit(*p))
	    return (TRUE);

    /* no extension found */
    return (TRUE);
}

/* xlbadtype - report a "bad argument type" error */
LVAL xlbadtype(arg)
  LVAL arg;
{
    return xlerror("bad argument type",arg);
}

/* xltoofew - report a "too few arguments" error */
LVAL xltoofew()
{
    xlfail("too few arguments");
    return (NIL);   /* never returns */
}

/* xltoomany - report a "too many arguments" error */
VOID xltoomany()
{
    xlfail("too many arguments");
}

/* xltoolong - report a "too long to process" error */
VOID xltoolong()
{
    xlfail("too long to process");
}

#ifdef SPECIALS
/* xlnoassign - report a "can't assign/bind to constant" error */
VOID xlnoassign(arg)
   LVAL arg;
{
    xlerror("can't assign/bind to constant", arg);
}
#endif

#ifdef COMPLX
/* compare floating point for eql and equal */
/* This is by Tom Almy */
#ifdef ANSI
static int NEAR comparecomplex(LVAL arg1, LVAL arg2)
#else
LOCAL int comparecomplex(arg1, arg2)
LVAL arg1, arg2;
#endif
{
    LVAL r1=getelement(arg1,0), r2=getelement(arg2,0);
    LVAL i1=getelement(arg1,1), i2=getelement(arg2,1);

    if (ntype(r1) != ntype(r2)) return FALSE;
    else if (ntype(r1) == FIXNUM)
	return (getfixnum(r1)==getfixnum(r2)&&
		getfixnum(i1)==getfixnum(i2));
    else
	return (getflonum(r1)==getflonum(r2)&&
		getflonum(i1)==getflonum(i2));
}

#endif

/* eql - internal eql function */
int eql(arg1,arg2)
  LVAL arg1,arg2;
{
    /* compare the arguments */
    if (arg1 == arg2)
	return (TRUE);
    else if (arg1 != NIL) {
	switch (ntype(arg1)) {
	case FIXNUM:
	    return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
	case FLONUM:
	    return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
#ifdef COMPLX
	case COMPLEX:
	    return (complexp(arg2) ? comparecomplex(arg1,arg2) : FALSE);
#endif

#ifdef WINTERP /*!!! Modifications to the code in this #ifdef should also be reflected in equal() !!!*/
	    /* For some of the following it may be nonsense to check equality...
	       for others, we may need a more than a pointer compare. But X
	       may make it impossible, or difficult enough not to be worth it. */
	case XLTYPE_XtAccelerators:
	    return (xtaccelerators_p(arg2) ? get_xtaccelerators(arg1)==get_xtaccelerators(arg2) : FALSE);
	case XLTYPE_XtTranslations:
	    return (xttranslations_p(arg2) ? get_xttranslations(arg1)==get_xttranslations(arg2) : FALSE);
	case XLTYPE_XEvent:
	    return (xevent_p(arg2) ? get_xevent(arg1)==get_xevent(arg2) : FALSE); /* this doesn't make sense... this kind of compare is useless. */
	case XLTYPE_Window:
	    return (window_p(arg2) ? get_window(arg1)==get_window(arg2) : FALSE);
	case XLTYPE_Pixel:
	    return (pixel_p(arg2) ? get_xpixel(arg1)==get_xpixel(arg2) : FALSE);
	case XLTYPE_Pixmap:
	    return (pixmap_p(arg2) ? get_pixmap(arg1)==get_pixmap(arg2) : FALSE);
	case XLTYPE_XImage:
	    return (ximage_p(arg2) ? get_ximage(arg1)==get_ximage(arg2) : FALSE);
	case XLTYPE_XmString:
/*!!*/	    return (xmstring_p(arg2) ? XmStringByteCompare(get_xmstring(arg1), get_xmstring(arg2)) : FALSE);
	case XLTYPE_XT_RESOURCE:
	    return (xtresource_p(arg2) ? get_xtresource(arg1)==get_xtresource(arg2) : FALSE);
	case XLTYPE_TIMEOUTOBJ:
	    return (timeoutobj_p(arg2) ? get_timeout_id(arg1)==get_timeout_id(arg2) : FALSE);
	case XLTYPE_FDINPUTCBOBJ:
	    return (fdinputcbobj_p(arg2) ? get_fdinputcb_id(arg1)==get_fdinputcb_id(arg2) : FALSE);
	case XLTYPE_WIDGETOBJ:
	    return (widgetobj_p(arg2) ? get_widgetobj_widgetID(arg1)==get_widgetobj_widgetID(arg2) : FALSE);
	case XLTYPE_CALLBACKOBJ:
	    return (callbackobj_p(arg2) ?
		    ((strcmp(get_callback_name(arg1), get_callback_name(arg2)) == 0)
		      && (get_callback_proc(arg1)    == get_callback_proc(arg2))
		      && (get_callback_widget(arg1)  == get_callback_widget(arg2))
/*!!*/		      && (get_callback_closure(arg1) == get_callback_closure(arg2)))
		    : FALSE);
	case XLTYPE_EVHANDLEROBJ:
	    return (evhandlerobj_p(arg2) ?
		    (   (get_evhandler_mask(arg1)    == get_evhandler_mask(arg2))
		     && (get_evhandler_options(arg1) == get_evhandler_options(arg2))
		     && (get_evhandler_widget(arg1)  == get_evhandler_widget(arg2))
/*!!*/		     && (get_evhandler_closure(arg1) == get_evhandler_closure(arg2)))
		    : FALSE);
#ifdef WINTERP_XTANGO_WIDGET
	case XLTYPE_TANGOIMAGEOBJ:
	    return (tangoimageobj_p(arg2) ? get_tangoimageobj_timageID(arg1) == get_tangoimageobj_timageID(arg2) : FALSE);
        case XLTYPE_TANGO_PATH:
	    return (tangopath_p(arg2) ? get_tangopath(arg1) == get_tangopath(arg2) : FALSE);
        case XLTYPE_TANGO_TRANS:
	    return (tangotrans_p(arg2) ? get_tangotrans(arg1) == get_tangotrans(arg2) : FALSE);
#endif /* WINTERP_XTANGO_WIDGET */
     /*
      * the following don't have a Motif or Xt gen'd id used as key for equality comparison...
      * case XLTYPE_PIXMAP_REFOBJ:
      */
#endif /* WINTERP */

	default:
	    return (FALSE);
	}
    }
    else
	return (FALSE);
}

#ifdef ANSI
static int NEAR stringcmp(LVAL arg1, LVAL arg2)
#else
LOCAL stringcmp(arg1, arg2)	    /* compare two strings for equal */
LVAL arg1, arg2;		    /* Written by TAA. Compares strings */
				    /* with embedded nulls */
#endif
{
    char FAR *s1 = getstring(arg1), FAR *s2 = getstring(arg2);
    unsigned l = getslength(arg1);

    if (l != getslength(arg2)) return FALSE;

    while (l-- > 0) if (*s1++ != *s2++) return FALSE;

    return TRUE;
}

/* equal- internal equal function */
int equal(arg1,arg2)
  LVAL arg1,arg2;
{
    /* compare the arguments */
isItEqual:  /* turn tail recursion into iteration */
    if (arg1 == arg2)
	return (TRUE);
    else if (arg1 != NIL) {
	switch (ntype(arg1)) {
	case FIXNUM:
	    return (fixp(arg2) ? getfixnum(arg1)==getfixnum(arg2) : FALSE);
	case FLONUM:
	    return (floatp(arg2) ? getflonum(arg1)==getflonum(arg2) : FALSE);
#ifdef COMPLX
	case COMPLEX:
	    return (complexp(arg2) ? comparecomplex(arg1,arg2) : FALSE);
#endif
	case STRING:
	    return (stringp(arg2) ? stringcmp(arg1,arg2) : FALSE); /* TAA MOD */

#ifdef WINTERP /*!!! Modifications to the code in this #ifdef should also be reflected in eql() !!!*/
	    /* For some of the following it may be nonsense to check equality...
	       for others, we may need a more than a pointer compare. But X
	       may make it impossible, or difficult enough not to be worth it. */
	case XLTYPE_XtAccelerators:
	    return (xtaccelerators_p(arg2) ? get_xtaccelerators(arg1)==get_xtaccelerators(arg2) : FALSE);
	case XLTYPE_XtTranslations:
	    return (xttranslations_p(arg2) ? get_xttranslations(arg1)==get_xttranslations(arg2) : FALSE);
	case XLTYPE_XEvent:
	    return (xevent_p(arg2) ? get_xevent(arg1)==get_xevent(arg2) : FALSE); /* this doesn't make sense... this kind of compare is useless. */
	case XLTYPE_Window:
	    return (window_p(arg2) ? get_window(arg1)==get_window(arg2) : FALSE);
	case XLTYPE_Pixel:
	    return (pixel_p(arg2) ? get_xpixel(arg1)==get_xpixel(arg2) : FALSE);
	case XLTYPE_Pixmap:
	    return (pixmap_p(arg2) ? get_pixmap(arg1)==get_pixmap(arg2) : FALSE);
	case XLTYPE_XImage:
	    return (ximage_p(arg2) ? get_ximage(arg1)==get_ximage(arg2) : FALSE);
	case XLTYPE_XmString:
/*!!*/	    return (xmstring_p(arg2) ? XmStringCompare(get_xmstring(arg1), get_xmstring(arg2)) : FALSE);
	case XLTYPE_XT_RESOURCE:
	    return (xtresource_p(arg2) ? get_xtresource(arg1)==get_xtresource(arg2) : FALSE);
	case XLTYPE_TIMEOUTOBJ:
	    return (timeoutobj_p(arg2) ? get_timeout_id(arg1)==get_timeout_id(arg2) : FALSE);
	case XLTYPE_FDINPUTCBOBJ:
	    return (fdinputcbobj_p(arg2) ? get_fdinputcb_id(arg1)==get_fdinputcb_id(arg2) : FALSE);
	case XLTYPE_WIDGETOBJ:
	    return (widgetobj_p(arg2) ? get_widgetobj_widgetID(arg1)==get_widgetobj_widgetID(arg2) : FALSE);
	case XLTYPE_CALLBACKOBJ:
	    return (callbackobj_p(arg2) ?
		    ((strcmp(get_callback_name(arg1), get_callback_name(arg2)) == 0)
		      && (get_callback_proc(arg1)    == get_callback_proc(arg2))
		      && (get_callback_widget(arg1)  == get_callback_widget(arg2)))
		    : FALSE);
	case XLTYPE_EVHANDLEROBJ:
	    return (evhandlerobj_p(arg2) ?
		    (   (get_evhandler_mask(arg1)    == get_evhandler_mask(arg2))
		     && (get_evhandler_options(arg1) == get_evhandler_options(arg2))
		     && (get_evhandler_widget(arg1)  == get_evhandler_widget(arg2)))
		    : FALSE);
#ifdef WINTERP_XTANGO_WIDGET
	case XLTYPE_TANGOIMAGEOBJ:
	    return (tangoimageobj_p(arg2) ? get_tangoimageobj_timageID(arg1) == get_tangoimageobj_timageID(arg2) : FALSE);
        case XLTYPE_TANGO_PATH:
	    return (tangopath_p(arg2) ? get_tangopath(arg1) == get_tangopath(arg2) : FALSE);
        case XLTYPE_TANGO_TRANS:
	    return (tangotrans_p(arg2) ? get_tangotrans(arg1) == get_tangotrans(arg2) : FALSE);
#endif /* WINTERP_XTANGO_WIDGET */
     /*
      * the following don't have a Motif or Xt gen'd id used as key for equality comparison...
      * case XLTYPE_PIXMAP_REFOBJ:
      */
#endif /* WINTERP */

	case CONS:  /* TAA MOD turns tail recursion into iteration */
		    /* Not only is this faster, but greatly reduces chance */
		    /* of stack overflow */
	    if (consp(arg2) && equal(car(arg1),car(arg2))) {
		arg1 = cdr(arg1);
		arg2 = cdr(arg2);
		goto isItEqual;
	    }
	    return FALSE;
	default:
	    return (FALSE);
	}
    }
    else
	return (FALSE);
}


#ifdef KEYARG
/* TAA Addition */
/* xlkey - get the :key keyword argument */
extern LVAL k_key;

LVAL xlkey()
{
    LVAL kfcn;

    if (xlgetkeyarg(k_key,&kfcn)) return kfcn;
    return NIL;
}

/* xlapp1 - apply a function of a single argument */
LVAL xlapp1(fun,arg)
  LVAL fun,arg;
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)1));
    pusharg(arg);
    xlfp = newfp;

    /* return the result of applying the function */
    return xlapply(1);

}


/* dotest1 - call a test function with one argument */
int dotest1(arg,fun,kfun)
  LVAL arg,fun,kfun;
{
    FRAMEP newfp;

    if (kfun != NIL) arg = xlapp1(kfun,arg);

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)1));
    pusharg(arg);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(1) != NIL);

}

/* dotest2 - call a test function with two arguments */
int dotest2(arg1,arg2,fun,kfun)
  LVAL arg1,arg2,fun,kfun;
{
    FRAMEP newfp;

    if (kfun != NIL) arg2 = xlapp1(kfun,arg2);

    /* Speedup for default case TAA MOD */
    if (fun == getfunction(s_eql))
	return (eql(arg1,arg2));

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(arg1);
    pusharg(arg2);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(2) != NIL);

}

/* dotest2s - call a test function with two arguments, symmetrical */
int dotest2s(arg1,arg2,fun,kfun)
  LVAL arg1,arg2,fun,kfun;
{
    FRAMEP newfp;

    if (kfun != NIL) {
	arg1 = xlapp1(kfun,arg1);
	arg2 = xlapp1(kfun,arg2);
    }

    /* Speedup for default case TAA MOD */
    if (fun == getfunction(s_eql))
	return (eql(arg1,arg2));

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(arg1);
    pusharg(arg2);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(2) != NIL);

}

#else
/* dotest1 - call a test function with one argument */
int dotest1(arg,fun)
  LVAL arg,fun;
{
    FRAMEP newfp;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)1));
    pusharg(arg);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(1) != NIL);

}

/* dotest2 - call a test function with two arguments */
int dotest2(arg1,arg2,fun)
  LVAL arg1,arg2,fun;
{
    FRAMEP newfp;

    /* Speedup for default case TAA MOD */
    if (fun == getfunction(s_eql))
	return (eql(arg1,arg2));

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(arg1);
    pusharg(arg2);
    xlfp = newfp;

    /* return the result of applying the test function */
    return (xlapply(2) != NIL);

}

#endif

#ifdef COMPLX
/* return value of a number coerced to a FLOTYPE */
FLOTYPE makefloat(x)
     LVAL x;
{
    if (x != NIL) {
	if (ntype(x) == FIXNUM) return ((FLOTYPE) getfixnum(x));
	else if (ntype(x) == FLONUM) return getflonum(x);
    }
    xlerror("not a number", x);
    return 0.0; /* never reached */
}
#endif
