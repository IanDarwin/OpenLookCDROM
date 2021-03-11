/* -*-C-*-
********************************************************************************
*
* File:         xllist.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xllist.c,v 2.4 1994/06/06 15:59:19 npm Exp $
* Description:  xlisp built-in list functions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:08 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xllist.c,v 2.4 1994/06/06 15:59:19 npm Exp $";

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

/* forward declarations */
#ifdef ANSI
LOCAL LVAL NEAR cxr(char *adstr); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR nth(int charflag); /* NPM: changed this to LOCAL */
#ifdef KEYARG
LOCAL LVAL NEAR assoc(LVAL expr, LVAL alist, LVAL fcn, LVAL kfcn, int tresult);	/* NPM: changed this to LOCAL */
LOCAL LVAL NEAR subst(LVAL to, LVAL from, LVAL expr, LVAL fcn, LVAL kfcn, int tresult);	/* NPM: changed this to LOCAL */
LOCAL LVAL NEAR sublis(LVAL alist, LVAL expr, LVAL fcn, LVAL kfcn, int tresult); /* NPM: changed this to LOCAL */
#ifdef SETS
LOCAL LVAL NEAR membr(LVAL expr,LVAL list,LVAL fcn,LVAL kfcn,int tresult); /* NPM: changed this to LOCAL */
#endif /* SETS */
#else /* !defined(KEYARG) */
LOCAL LVAL NEAR assoc(LVAL expr, LVAL alist, LVAL fcn, int tresult); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR subst(LVAL to, LVAL from, LVAL expr, LVAL fcn, int tresult); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR sublis(LVAL alist, LVAL expr, LVAL fcn, int tresult); /* NPM: changed this to LOCAL */
#ifdef SETS
LOCAL LVAL NEAR membr(LVAL expr,LVAL list,LVAL fcn,int tresult); /* NPM: changed this to LOCAL */
#endif /* SETS */
#endif /* KEYARG */
LOCAL void NEAR splitlist(LVAL pivot,LVAL list, LVAL *psmaller, LVAL *plarger, LVAL fcn); /* NPM: changed this to LOCAL */
#else /* !defined(ANSI) */
LOCAL FORWARD LVAL cxr();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL nth(),assoc(); /* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL subst(),sublis(); /* NPM: changed this to LOCAL */
LOCAL FORWARD VOID splitlist();	/* NPM: changed this to LOCAL */
#endif /* ANSI */

/* external declarations  TAA MOD for circular list catching */
extern long nnodes;

/* xlcircular -- circular list error */
VOID NEAR xlcircular(VOIDP)
{
    xlfail("circular list");
}

/* xcar - take the car of a cons cell */
LVAL xcar()
{
    LVAL list;
    list = xlgalist();
    xllastarg();
    return (null(list) ? NIL : car(list));
}

/* xcdr - take the cdr of a cons cell */
LVAL xcdr()
{
    LVAL list;
    list = xlgalist();
    xllastarg();
    return (null(list) ? NIL : cdr(list));
}

/* cxxr functions */
LVAL xcaar() { return (cxr("aa")); }
LVAL xcadr() { return (cxr("da")); }
LVAL xcdar() { return (cxr("ad")); }
LVAL xcddr() { return (cxr("dd")); }

/* cxxxr functions */
LVAL xcaaar() { return (cxr("aaa")); }
LVAL xcaadr() { return (cxr("daa")); }
LVAL xcadar() { return (cxr("ada")); }
LVAL xcaddr() { return (cxr("dda")); }
LVAL xcdaar() { return (cxr("aad")); }
LVAL xcdadr() { return (cxr("dad")); }
LVAL xcddar() { return (cxr("add")); }
LVAL xcdddr() { return (cxr("ddd")); }

/* cxxxxr functions */
LVAL xcaaaar() { return (cxr("aaaa")); }
LVAL xcaaadr() { return (cxr("daaa")); }
LVAL xcaadar() { return (cxr("adaa")); }
LVAL xcaaddr() { return (cxr("ddaa")); }
LVAL xcadaar() { return (cxr("aada")); }
LVAL xcadadr() { return (cxr("dada")); }
LVAL xcaddar() { return (cxr("adda")); }
LVAL xcadddr() { return (cxr("ddda")); }
LVAL xcdaaar() { return (cxr("aaad")); }
LVAL xcdaadr() { return (cxr("daad")); }
LVAL xcdadar() { return (cxr("adad")); }
LVAL xcdaddr() { return (cxr("ddad")); }
LVAL xcddaar() { return (cxr("aadd")); }
LVAL xcddadr() { return (cxr("dadd")); }
LVAL xcdddar() { return (cxr("addd")); }
LVAL xcddddr() { return (cxr("dddd")); }

/* cxr - common car/cdr routine */
LOCAL LVAL NEAR cxr(adstr)
  char *adstr;
{
    LVAL list;

    /* get the list */
    list = xlgalist();

    xllastarg();

    /* perform the car/cdr operations */
    while (*adstr && consp(list))
	list = (*adstr++ == 'a' ? car(list) : cdr(list));

    /* make sure the operation succeeded */
    if (*adstr && !null(list))
	xlfail("bad argument");

    /* return the result */
    return (list);
}

/* xcons - construct a new list cell */
LVAL xcons()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* construct a new list element */
    return (cons(arg1,arg2));
}

/* xlist - built a list of the arguments */
/* Rewritten by TAA for compactness and speed */
LVAL xlist()
{
    LVAL val;
    int i=xlargc;

    /* protect a pointer */
    xlsave1(val);

    /* do the work */
    while (i-- > 0)
	val = cons(xlargv[i],val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

#ifdef COMMONLISPF

/* xliststar - built a list of the arguments */
/* by TAA */
LVAL xliststar()
{
    LVAL val;
    int i=xlargc;

    if (i==0) xltoofew();   /* must have at least one argument */

    /* protect a pointer */
    xlprot1(val);

    /* last argument is list tail */

    val = xlargv[--i];

    /* do the work */
    while (i-- > 0)
	val = cons(xlargv[i],val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

/* xbutlast -- copy list for all but last n */
/* Added function TAA */

LVAL xbutlast()
{
    LVAL val,list,last,next;
    FIXTYPE n=1,l=0;

    /* get argument(s) */
    list = xlgalist();
    if (moreargs()) {
	n = getfixnum(next=xlgafixnum());
	if (n<0) xlerror("bad index",next);
	xllastarg();
    }

    /* get length */
    for (next=list; consp(next);) {
	next=cdr(next);
	l++;
	if (l > nnodes) xlcircular();
    }

    /* calc final length */
    l-=n;
    if (l <= 0) return (NIL);	/* nothing left */

    /* do the first cons */

    val = consa(car(list));
    if (l-- == 1) return val;

    /* protect a pointer */
    xlprot1(val);

    /* do remaining conses */
    last = val;
    while (l-- > 0) {
	list = cdr(list);
	next = consa(car(list));
	rplacd(last,next);
	last = next;
    }


    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}
#endif


/* xappend - built-in function append */
LVAL xappend()
{
    LVAL list,last=NIL,next,val;

    /* protect some pointers */
    xlsave1(val);

    /* append each argument */
    if (moreargs()) {
	while (xlargc > 1) {

	    /* append each element of this list to the result list */
	    for (list = nextarg(); consp(list); list = cdr(list)) {
		next = consa(car(list));
		if (!null(val)) rplacd(last,next);
		else val = next;
		last = next;
	    }
	    if (!null(list)) xlbadtype(*--xlargv);  /*TAA added errormessage*/
	}

	/* handle the last argument */
	if (!null(val)) rplacd(last,nextarg());
	else val = nextarg();
    }

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}


#ifndef COMMONLISPF

/* xreverse - built-in function reverse */
LVAL xreverse()
{
    LVAL list,val;

    /* protect some pointers */
    xlsave1(val);

    /* get the list to reverse */
    list = xlgalist();
    xllastarg();

    /* append each element to the head of the result list */
    for (val = NIL; consp(list); list = cdr(list))
	val = cons(car(list),val);

    /* restore the stack */
    xlpop();

    /* return the list */
    return (val);
}

#endif

/* xlast - return the last cons of a list */
LVAL xlast()
{
    LVAL list;
    long l=0;

    /* get the list */
    list = xlgalist();
    xllastarg();

    /* find the last cons */
    if (consp(list))		/* TAA fix */
	while (consp(cdr(list))) {
	    list = cdr(list);
	    if (l++ > nnodes) xlcircular();
	}

    /* return the last element */
    return (list);
}

/* xmember - built-in function 'member' */
LVAL xmember()
{
    LVAL x,list,fcn,val;
    int tresult;
    long n=0;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    /* protect some pointers */
    xlsave1(fcn);
#endif

    /* get the expression to look for and the list */
    x = xlgetarg();
    list = xlgalist();
    xltest(&fcn,&tresult);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    /* look for the expression */
    for (val = NIL; consp(list); list = cdr(list)) {
#ifdef KEYARG
	if (dotest2(x,car(list),fcn,kfcn) == tresult)
#else
	if (dotest2(x,car(list),fcn) == tresult)
#endif
	{
	    val = list;
	    break;
	}
	if (n++ > nnodes) { val = NIL; break; } /* circular list */
    }

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (val);
}

/* xassoc - built-in function 'assoc' */
LVAL xassoc()
{
    LVAL x,alist,fcn,pair,val;
    int tresult;
    long n=0;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    /* protect some pointers */
    xlsave1(fcn);
#endif

    /* get the expression to look for and the association list */
    x = xlgetarg();
    alist = xlgalist();
    xltest(&fcn,&tresult);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    /* look for the expression */
    for (val = NIL; consp(alist); alist = cdr(alist)) {
	if ((!null(pair = car(alist))) && consp(pair))
#ifdef KEYARG
	    if (dotest2(x,car(pair),fcn,kfcn) == tresult)
#else
	    if (dotest2(x,car(pair),fcn) == tresult)
#endif
	    {
		val = pair;
		break;
	    }
	if (n++ > nnodes) { val = NIL; break; } /* circular list */
    }

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return result */
    return (val);
}

/* xsubst - substitute one expression for another */
LVAL xsubst()
{
    LVAL to,from,expr,fcn,val;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    /* protect some pointers */
    xlsave1(fcn);
#endif

    /* get the to value, the from value and the expression */
    to = xlgetarg();
    from = xlgetarg();
    expr = xlgetarg();
    xltest(&fcn,&tresult);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    /* do the substitution */
#ifdef KEYARG
    val = subst(to,from,expr,fcn,kfcn,tresult);
#else
    val = subst(to,from,expr,fcn,tresult);
#endif

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (val);
}

/* subst - substitute one expression for another */
#ifdef KEYARG
LOCAL LVAL NEAR subst(to,from,expr,fcn,kfcn,tresult)
  LVAL to,from,expr,fcn,kfcn; int tresult;
#else
LOCAL LVAL NEAR subst(to,from,expr,fcn,tresult)
  LVAL to,from,expr,fcn; int tresult;
#endif
{
    LVAL carval,cdrval;

#ifdef KEYARG
    if (dotest2(expr,from,fcn,kfcn) == tresult)
#else
    if (dotest2(expr,from,fcn) == tresult)
#endif
	return (to);
    else if (consp(expr)) {
	xlsave1(carval);
#ifdef KEYARG
	carval = subst(to,from,car(expr),fcn,kfcn,tresult);
	cdrval = subst(to,from,cdr(expr),fcn,kfcn,tresult);
#else
	carval = subst(to,from,car(expr),fcn,tresult);
	cdrval = subst(to,from,cdr(expr),fcn,tresult);
#endif
	xlpop();

/* the following TAA mod makes subst like COMMON LISP */

	if ((carval == car(expr)) && (cdrval == cdr(expr)))
	    return expr; /* no change */
	else
	    return (cons(carval,cdrval));
    }
    else
	return (expr);
}

/* xsublis - substitute using an association list */
LVAL xsublis()
{
    LVAL alist,expr,fcn,val;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    /* protect some pointers */
    xlsave1(fcn);
#endif

    /* get the assocation list and the expression */
    alist = xlgalist();
    expr = xlgetarg();
    xltest(&fcn,&tresult);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    /* do the substitution */
#ifdef KEYARG
    val = sublis(alist,expr,fcn,kfcn,tresult);
#else
    val = sublis(alist,expr,fcn,tresult);
#endif

    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the result */
    return (val);
}

/* sublis - substitute using an association list */
#ifdef KEYARG
LOCAL LVAL NEAR sublis(alist,expr,fcn,kfcn,tresult)
  LVAL alist,expr,fcn,kfcn; int tresult;
#else
LOCAL LVAL NEAR sublis(alist,expr,fcn,tresult)
  LVAL alist,expr,fcn; int tresult;
#endif
{
    LVAL carval,cdrval,pair;

#ifdef KEYARG
    if (!null(pair = assoc(expr,alist,fcn,kfcn,tresult)))
#else
    if (!null(pair = assoc(expr,alist,fcn,tresult)))
#endif
	return (cdr(pair));
    else if (consp(expr)) {
	xlsave1(carval);
#ifdef KEYARG
	carval = sublis(alist,car(expr),fcn,kfcn,tresult);
	cdrval = sublis(alist,cdr(expr),fcn,kfcn,tresult);
#else
	carval = sublis(alist,car(expr),fcn,tresult);
	cdrval = sublis(alist,cdr(expr),fcn,tresult);
#endif
	xlpop();
/* TAA MOD for making like common lisp */
	if ((car(expr) == carval) && (cdr(expr) == cdrval))
	    return (expr);
	else
	    return (cons(carval,cdrval));
    }
    else
	return (expr);
}

/* assoc - find a pair in an association list */
#ifdef KEYARG
LOCAL LVAL NEAR assoc(expr,alist,fcn,kfcn,tresult)
  LVAL expr,alist,fcn,kfcn; int tresult;
#else
LOCAL LVAL NEAR assoc(expr,alist,fcn,tresult)
  LVAL expr,alist,fcn; int tresult;
#endif
{
    LVAL pair;

    for (; consp(alist); alist = cdr(alist))
	if ((!null((pair = car(alist)))) && consp(pair))
#ifdef KEYARG
	    if (dotest2(expr,car(pair),fcn,kfcn) == tresult)
#else
	    if (dotest2(expr,car(pair),fcn) == tresult)
#endif
		return (pair);
    return (NIL);
}

#ifndef COMMONLISPF
/* xremove - built-in function 'remove' */
LVAL xremove()
{
    LVAL x,list,fcn,val,next;
    LVAL last = NULL;
    int tresult;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(val);

    /* get the expression to remove and the list */
    x = xlgetarg();
    list = xlgalist();
    xltest(&fcn,&tresult);

    xllastarg();

    /* remove matches */
    for (; consp(list); list = cdr(list))

	/* check to see if this element should be deleted */
	if (dotest2(x,car(list),fcn) != tresult) {
	    next = consa(car(list));
	    if (!null(val)) rplacd(last,next);
	    else val = next;
	    last = next;
	}

    /* restore the stack */
    xlpopn(2);

    /* return the updated list */
    return (val);
}

/* remif - common code for 'remove-if' and 'remove-if-not' */
#ifdef ANSI
static LVAL NEAR remif(int tresult)
#else
LOCAL LVAL remif(tresult)
  int tresult;
#endif
{
    LVAL list,fcn,val,last;
    LVAL next=NULL;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(val);

    /* get the expression to remove and the list */
    fcn = xlgetarg();
    list = xlgalist();
    xllastarg();

    /* remove matches */
    for (; consp(list); list = cdr(list))

	/* check to see if this element should be deleted */
	if (dotest1(car(list),fcn) != tresult) {
	    next = consa(car(list));
	    if (!null(val)) rplacd(last,next);
	    else val = next;
	    last = next;
	}

    /* restore the stack */
    xlpopn(2);

    /* return the updated list */
    return (val);
}

/* xremif - built-in function 'remove-if' */
LVAL xremif()
{
    return (remif(TRUE));
}

/* xremifnot - built-in function 'remove-if-not' */
LVAL xremifnot()
{
    return (remif(FALSE));
}
#endif

/* xnth - return the nth element of a list */
LVAL xnth()
{
    return (nth(TRUE));
}

/* xnthcdr - return the nth cdr of a list */
LVAL xnthcdr()
{
    return (nth(FALSE));
}

/* nth - internal nth function */
LOCAL LVAL NEAR nth(carflag)
  int carflag;
{
    LVAL list,num;
    FIXTYPE n;

    /* get n and the list */
    num = xlgafixnum();
/*  list = xlgacons(); */
    list = xlgalist();	    /* TAA fix */

    xllastarg();

    /* make sure the number isn't negative */
    if ((n = getfixnum(num)) < 0)
	xlfail("bad argument");

    /* find the nth element */
    while (consp(list) && --n >= 0)
	list = cdr(list);

    /* return the list beginning at the nth element */
    return (carflag && consp(list) ? car(list) : list);
}

/* xlength - return the length of a list or string */
LVAL xlength()
{
    FIXTYPE n;
    LVAL arg;

    /* get the list or string */
    arg = xlgetarg();
    xllastarg();

    /* find the length of a list */
    if (listp(arg))
	for (n = 0; consp(arg);) {
	    arg = cdr(arg);
	    n++;
	    if (n > nnodes) xlcircular();
	}

    /* find the length of a string */
    else if (stringp(arg))
	n = (FIXTYPE)getslength(arg);

    /* find the length of a vector */
    else if (vectorp(arg))
	n = (FIXTYPE)getsize(arg);

    /* otherwise, bad argument type */
    else
		xlbadtype(arg);

    /* return the length */
    return (cvfixnum(n));
}

/* map - internal mapping function */
#ifdef ANSI
static LVAL NEAR map(int carflag, int valflag)
#else
LOCAL LVAL NEAR map(carflag,valflag)
  int carflag,valflag;
#endif
{
    FRAMEP newfp;
    LVAL fun,lists,val,last,p,x,y;
    int argc;
    long n=0, nmax=nnodes;


    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fun);
    xlsave(lists);
    xlsave(val);

    /* get the function to apply and the first list */
    fun = xlgetarg();
    lists = xlgalist();

    /* initialize the result list */
    val = (valflag ? NIL : lists);

    /* build a list of argument lists */
    argc = 1;
    for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
	argc++;
	rplacd(last,cons(xlgalist(),NIL));
    }

    /* loop through each of the argument lists */
    for (;;) {

	if (n++ > nmax) xlcircular();

	/* build an argument list from the sublists */
	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(fun);
	pusharg(cvfixnum((FIXTYPE)argc));
	for (x = lists; (!null(x)) && (!null(y = car(x))) && consp(y); x = cdr(x)) {
	    pusharg(carflag ? car(y) : y);
	    rplaca(x,cdr(y));
	}

	/* quit if any of the lists were empty */
	if (!null(x)) {
	    xlsp = newfp;
	    break;
	}

	/* apply the function to the arguments */
	xlfp = newfp;
	if (valflag) {
	    p = consa(xlapply(argc));
	    if (!null(val)) rplacd(last,p);
	    else val = p;
	    last = p;
	}
	else
	    xlapply(argc);
    }

    /* restore the stack */
    xlpopn(3);

    /* return the last test expression value */
    return (val);
}

/* xmapc - built-in function 'mapc' */
LVAL xmapc()
{
    return (map(TRUE,FALSE));
}

/* xmapcar - built-in function 'mapcar' */
LVAL xmapcar()
{
    return (map(TRUE,TRUE));
}

/* xmapl - built-in function 'mapl' */
LVAL xmapl()
{
    return (map(FALSE,FALSE));
}

/* xmaplist - built-in function 'maplist' */
LVAL xmaplist()
{
    return (map(FALSE,TRUE));
}

/* xrplca - replace the car of a list node */
LVAL xrplca()
{
    LVAL list,newcar;

    /* get the list and the new car */
    list = xlgacons();
    newcar = xlgetarg();
    xllastarg();

    /* replace the car */
    rplaca(list,newcar);

    /* return the list node that was modified */
    return (list);
}

/* xrplcd - replace the cdr of a list node */
LVAL xrplcd()
{
    LVAL list,newcdr;

    /* get the list and the new cdr */
    list = xlgacons();
    newcdr = xlgetarg();
    xllastarg();

    /* replace the cdr */
    rplacd(list,newcdr);

    /* return the list node that was modified */
    return (list);
}

/* xnconc - destructively append lists */
LVAL xnconc()
{
    LVAL next,last=NIL,val=NIL;
    long l; /* TAA MOD */

    /* concatenate each argument */
    if (moreargs()) {
	while (xlargc > 1) {

	    /* TAA mod -- give error message if not a list */
	    if ((!null(next = nextarg())) && consp(next)) {

		/* concatenate this list to the result list */
		if (!null(val)) rplacd(last,next);
		else val = next;

		/* find the end of the list */
		l = 0;
		while (consp(cdr(next))) {
		    next = cdr(next);
		    if (l++ > nnodes) xlcircular();
		}
		last = next;
	    }
	    else if (!null(next)) xlbadtype(*--xlargv); /* TAA -- oops! */
	}

	/* handle the last argument */
	if (!null(val)) rplacd(last,nextarg());
	else val = nextarg();
    }

    /* return the list */
    return (val);
}
#ifndef COMMONLISPF
/* xdelete - built-in function 'delete' */
LVAL xdelete()
{
    LVAL x,list,fcn,last,val;
    int tresult;

    /* protect some pointers */
    xlsave1(fcn);

    /* get the expression to delete and the list */
    x = xlgetarg();
    list = xlgalist();
    xltest(&fcn,&tresult);

    xllastarg();

    /* delete leading matches */
    while (consp(list)) {
	if (dotest2(x,car(list),fcn) != tresult)
	    break;
	list = cdr(list);
    }
    val = last = list;

    /* delete embedded matches */
    if (consp(list)) {

	/* skip the first non-matching element */
	list = cdr(list);

	/* look for embedded matches */
	while (consp(list)) {

	    /* check to see if this element should be deleted */
	    if (dotest2(x,car(list),fcn) == tresult)
		rplacd(last,cdr(list));
	    else
		last = list;

	    /* move to the next element */
	    list = cdr(list);
	}
    }

    /* restore the stack */
    xlpop();

    /* return the updated list */
    return (val);
}

/* delif - common routine for 'delete-if' and 'delete-if-not' */
#ifdef ANSI
static LVAL NEAR delif(int tresult)
#else
LOCAL LVAL delif(tresult)
  int tresult;
#endif
{
    LVAL list,fcn,last,val;

    /* protect some pointers */
    xlsave1(fcn);

    /* get the expression to delete and the list */
    fcn = xlgetarg();
    list = xlgalist();
    xllastarg();

    /* delete leading matches */
    while (consp(list)) {
	if (dotest1(car(list),fcn) != tresult)
	    break;
	list = cdr(list);
    }
    val = last = list;

    /* delete embedded matches */
    if (consp(list)) {

	/* skip the first non-matching element */
	list = cdr(list);

	/* look for embedded matches */
	while (consp(list)) {

	    /* check to see if this element should be deleted */
	    if (dotest1(car(list),fcn) == tresult)
		rplacd(last,cdr(list));
	    else
		last = list;

	    /* move to the next element */
	    list = cdr(list);
	}
    }

    /* restore the stack */
    xlpop();

    /* return the updated list */
    return (val);
}

/* xdelif - built-in function 'delete-if' */
LVAL xdelif()
{
    return (delif(TRUE));
}

/* xdelifnot - built-in function 'delete-if-not' */
LVAL xdelifnot()
{
    return (delif(FALSE));
}
#endif


/*
    This sorting algorithm is based on a Modula-2 sort written by
    Richie Bielak and published in the February 1988 issue of
    "Computer Language" magazine in a letter to the editor.
*/


/* gluelists - glue the smaller and larger lists with the pivot */
#ifdef ANSI
static LVAL NEAR gluelists(LVAL smaller, LVAL pivot, LVAL larger)
#else
LOCAL LVAL gluelists(smaller,pivot,larger)
  LVAL smaller,pivot,larger;
#endif
{
    LVAL last;

    /* larger always goes after the pivot */
    rplacd(pivot,larger);

    /* if the smaller list is empty, we're done */
    if (null(smaller))
	return (pivot);

    /* append the smaller to the front of the resulting list */
    for (last = smaller; consp(cdr(last)); last = cdr(last))
	;
    rplacd(last,pivot);
    return (smaller);
}

/* sortlist - sort a list using quicksort */
#ifdef ANSI
static LVAL NEAR sortlist(LVAL list, LVAL fcn)
#else
LOCAL LVAL sortlist(list,fcn)
  LVAL list,fcn;
#endif
{
    LVAL smaller,pivot,larger;

    /* protect some pointers */
    xlstkcheck(3)
    xlsave(smaller);
    xlsave(pivot);
    xlsave(larger);

    /* lists with zero or one element are already sorted */
    if (consp(list) && consp(cdr(list))) {
	pivot = list; list = cdr(list);
	splitlist(pivot,list,&smaller,&larger,fcn);
	smaller = sortlist(smaller,fcn);
	larger = sortlist(larger,fcn);
	list = gluelists(smaller,pivot,larger);
    }

    /* cleanup the stack and return the sorted list */
    xlpopn(3);
    return (list);
}

/* splitlist - split the list around the pivot */
LOCAL VOID NEAR splitlist(pivot,list,psmaller,plarger,fcn)
  LVAL pivot,list,*psmaller,*plarger,fcn;
{
    LVAL next;
#ifdef KEYARG
    LVAL tmp=car(pivot);
#endif

    /* initialize the result lists */
    *psmaller = *plarger = NIL;

    /* In case of garbage collection TAA Mod thanx to Neal Holtz */
#ifdef KEYARG
    xlstkcheck(3);
    xlprotect(tmp);
#else
    xlstkcheck(2);
#endif
    xlprotect(list);
    xlsave(next);

#ifdef KEYARG
    if (!null(cdr(fcn))) tmp = xlapp1(cdr(fcn),tmp);
#endif

    /* split the list */
    for (; consp(list); list = next) {
	next = cdr(list);
#ifdef KEYARG
	if (dotest2((!null(cdr(fcn)))?xlapp1(cdr(fcn),car(list)):car(list),
	    tmp,car(fcn),NIL) )
#else
	if (dotest2(car(list),car(pivot),fcn))
#endif
	{
	    rplacd(list,*psmaller);
	    *psmaller = list;
	}
	else {
	    rplacd(list,*plarger);
	    *plarger = list;
	}
    }

    /* restore the stack */
#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif
}

/* xsort - built-in function 'sort' */
LVAL xsort()
{
    LVAL list,fcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(list);
    xlsave(fcn);

    /* get the list to sort and the comparison function */
    list = xlgalist();
#ifdef KEYARG
    fcn = cons(NIL,NIL);
    rplaca(fcn,xlgetarg());
    rplacd(fcn,xlkey());
#else
    fcn = xlgetarg();
#endif
    xllastarg();

    /* sort the list */
    list = sortlist(list,fcn);

    /* restore the stack and return the sorted list */
    xlpopn(2);
    return (list);
}

#ifdef SETS
/* These functions have the following copyright notice:    */
/* from XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney */

/* membr - internal MEMBER for set functions TAA */
#ifdef KEYARG
LOCAL LVAL NEAR membr(expr,list,fcn,kfcn,tresult)
  LVAL expr,list,fcn,kfcn; int tresult;
{
    xlprot1(expr);
    if (!null(kfcn)) expr = xlapp1(kfcn,expr);
    for (; consp(list); list = cdr(list))
	if (dotest2(expr,car(list),fcn,kfcn) == tresult) {
	    xlpop();
	    return (list);
	}
    xlpop();
    return (NIL);
}

#else
LOCAL LVAL NEAR membr(expr,list,fcn,tresult)
  LVAL expr,list,fcn; int tresult;
{
    for (; consp(list); list = cdr(list))
	if (dotest2(expr,car(list),fcn) == tresult)
		return (list);
    return (NIL);
}
#endif

extern LVAL true;

/* Common Lisp ADJOIN function */
LVAL xadjoin()
{
    LVAL x, list, fcn;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fcn);
    xlsave(kfcn);
#else
    xlsave1(fcn);
#endif

    /* get the lists and key arguements, if any */
    x = xlgetarg();
    list = xlgalist();
    xltest(&fcn,&tresult);
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

#ifdef KEYARG
    if (null(membr(x,list,fcn,kfcn,tresult))) list = cons(x,list) ;
    xlpopn(2);
#else
    if (null(membr(x,list,fcn,tresult))) list = cons(x,list) ;
    xlpop();
#endif

    return list;
}

#ifdef ANSI
static LVAL NEAR set_op(int which)
#else
LOCAL LVAL set_op(which)
	int which;
#endif
{
    LVAL x, list1, list2, result, fcn;
    int tresult;
#ifdef KEYARG
    LVAL kfcn;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(kfcn);
#else

    /* protect some pointers */
    xlstkcheck(2);
#endif
    xlsave(fcn);
    xlsave(result);

    /* get the lists and key arguements, if any */
    list1 = xlgalist();
    list2 = xlgalist();
    xltest(&fcn,&tresult);
#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    switch(which) {
	case 'U':
	    for (result = list1; consp(list2); list2 = cdr(list2)) {
		x = car(list2);
#ifdef KEYARG
		if (null(membr(x,list1,fcn,kfcn,tresult)))
#else
		if (null(membr(x,list1,fcn,tresult)))
#endif
		    result = cons(x, result);
	    }
	    break;
	case 'I':
	    for (result = NIL; consp(list2); list2 = cdr(list2)) {
		x = car(list2);
#ifdef KEYARG
		if (!null(membr(x,list1,fcn,kfcn,tresult)))
#else
		if (!null(membr(x,list1,fcn,tresult)))
#endif
		    result = cons(x, result);
	    }
	    break;
	case 'D':
	    for (result = NIL; consp(list1); list1 = cdr(list1)) {
		x = car(list1);
#ifdef KEYARG
		if (null(membr(x,list2,fcn,kfcn,tresult)))
#else
		if (null(membr(x,list2,fcn,tresult)))
#endif
		    result = cons(x, result);
	    }
	    break;
	case 'S':
	    for (result = true; consp(list1); list1 = cdr(list1)) {
		x = car(list1);
#ifdef KEYARG
		if (null(membr(x,list2,fcn,kfcn,tresult)))
#else
		if (null(membr(x,list2,fcn,tresult)))
#endif
		{
		    result = NIL;
		    break;
		}
	    }
	    break;
    }

#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif
    return(result);
}

LVAL xunion()	       { return(set_op('U')); }
LVAL xintersection()   { return(set_op('I')); }
LVAL xset_difference() { return(set_op('D')); }
LVAL xsubsetp()	       { return(set_op('S')); }

#endif


/* HASH TABLES ARE IMPLEMENTED AS STRUCTS, WITHOUT ACCESSING FCNS */

#ifdef HASHFCNS
extern LVAL a_hashtable, k_size, k_test, s_eql;

/* Hash table functions from Ken Whedbee */
LVAL xmakehash()    /* rewritten by TAA */
{
    LVAL size, testfcn, result;
    FIXTYPE len;

    if (xlgetkeyarg(k_size,&size)) {
	if (!fixp(size) || (len=getfixnum(size)) < 1) xlbadtype(size);
    }
    else len = 31;

    if (!xlgetkeyarg(k_test,&testfcn)) testfcn = getfunction(s_eql);

    xllastarg();

    xlprot1(testfcn);

    result = newstruct(a_hashtable,(int)len+1);

    setelement(result, 1, testfcn);

    xlpop();

    return result;
}

LVAL xgethash()
{
    LVAL alist,val,key,table,def=NIL;

    key = xlgetarg();
    table = xlgastruct();
    if (moreargs()) {
	def = xlgetarg();
	xllastarg();
    }
    if (getelement(table, 0) != a_hashtable)
	xlbadtype(table);

    alist = getelement(table,
	xlhash(key,getsize(table)-2) + 2);

#ifdef KEYARG
    val = assoc(key,alist,getelement(table,1),NIL,TRUE);
#else
    val = assoc(key,alist,getelement(table,1),TRUE);
#endif

    /* return result */
    return (null(val) ? def : cdr(val));
}

LVAL xremhash()
/* By TAA -- can't use assoc here*/
{
    LVAL alist,key,table,last;

    int idx;

    key = xlgetarg();
    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
	xlbadtype(table);

    idx = xlhash(key,getsize(table)-2) + 2;

    alist = getelement(table,idx);

    if (null(alist))
	return NIL;

#ifdef KEYARG
    else if (dotest2(key,car(car(alist)),getelement(table,1),NIL)==TRUE)
#else
    else if (dotest2(key,car(car(alist)),getelement(table,1))==TRUE)
#endif
	{
	setelement(table,idx,cdr(alist));   /* matches first element */
	return true;
    }
    else {
	last = alist;
	alist = cdr(alist);
	while (consp(alist)) {
#ifdef KEYARG
	    if (dotest2(key,car(car(alist)),getelement(table,1),NIL)==TRUE)
#else
	    if (dotest2(key,car(car(alist)),getelement(table,1))==TRUE)
#endif
	    {
		rplacd(last,cdr(alist));
		return true;
	    }
	    last = alist;
	    alist = cdr(alist);
	}
    }

    return NIL;
}

VOID xlsetgethash(key,table,value)
LVAL key,table,value;
{
    LVAL alist,oldval;
    int idx;

    if (getelement(table, 0) != a_hashtable)
	xlbadtype(table);

    idx = xlhash(key,getsize(table)-2) + 2;

    alist = getelement(table,idx);

#ifdef KEYARG
    if (!null(oldval = assoc(key,alist,getelement(table,1),NIL,TRUE)))
#else
    if (!null(oldval = assoc(key,alist,getelement(table,1),TRUE)))
#endif
	rplacd(oldval,value);
    else {
	alist = cons(cons(key,value),alist);
	setelement(table,idx,alist);
    }
}

/* function clrhash  TAA */

LVAL xclrhash()
{
    LVAL table;
    int i;

    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
	xlbadtype(table);

    for (i = getsize(table)-1; i > 1; i--) setelement(table,i,NIL);

    return (table);

}

/* function hash-table-count  TAA */

LVAL xhashcount()
{
    LVAL table, element;
    int i;
    FIXTYPE cnt = 0;

    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
	xlbadtype(table);

    for (i = getsize(table)-1; i > 1; i--)
	for (element=getelement(table,i);consp(element);element=cdr(element))
	    cnt++;

    return (cvfixnum(cnt));
}

/* function maphash  TAA */

LVAL xmaphash()
{
    FRAMEP newfp;
    LVAL fun, table, arg, element;
    int i;

    fun = xlgetarg();
    table = xlgastruct();
    xllastarg();

    if (getelement(table, 0) != a_hashtable)
	xlbadtype(table);

    xlstkcheck(2);
    xlprotect(fun);
    xlprotect(table);

    for (i = getsize(table)-1; i > 1; i--)
	for (element=getelement(table,i); consp(element);) {
	    arg = car(element);
	    element = cdr(element); /* in case element is deleted */
	    newfp =xlsp;
	    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	    pusharg(fun);
	    pusharg(cvfixnum((FIXTYPE) 2));
	    pusharg(car(arg));
	    pusharg(cdr(arg));
	    xlfp = newfp;
	    xlapply(2);
	}

    xlpopn(2);

    return (NIL);
}

#endif
