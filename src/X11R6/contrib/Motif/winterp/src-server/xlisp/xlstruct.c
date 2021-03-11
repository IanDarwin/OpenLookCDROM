/* -*-C-*-
********************************************************************************
*
* File:         xlstruct.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlstruct.c,v 2.4 1994/06/06 15:59:25 npm Exp $
* Description:  the defstruct facility
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:45 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlstruct.c,v 2.4 1994/06/06 15:59:25 npm Exp $";

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
#ifdef STRUCTS

/* external variables */
extern LVAL xlenv,xlfenv;
extern LVAL s_lambda,s_quote,lk_key,true;
extern LVAL s_strtypep, s_mkstruct, s_cpystruct, s_strref, s_strset;
extern LVAL s_x, s_s, s_sslots, s_setf;
extern LVAL k_concname, k_include;

/* forward declarations */
#ifdef ANSI
LOCAL void NEAR addslot(LVAL slotname,LVAL defexpr,int slotn,LVAL *pargs, LVAL *pbody); /* NPM: changed this to LOCAL */
LOCAL void NEAR updateslot(LVAL args,LVAL slotname,LVAL defexpr); /* NPM: changed this to LOCAL */
#else
LOCAL FORWARD void addslot(); /* NPM: changed this to LOCAL */
LOCAL FORWARD void updateslot(); /* NPM: changed this to LOCAL */
#endif


/* local variables */
static	char prefix[STRMAX+1];
#ifdef MEDMEM
static char makestr[] = "MAKE-%Fs";
#else
static char makestr[] = "MAKE-%s";
#endif

/* xmkstruct - the '%make-struct' function */
LVAL xmkstruct()
{
    LVAL type,val;
    int i;

    /* get the structure type */
    type = xlgasymbol();

    /* make the structure */
    val = newstruct(type,xlargc);

    /* store each argument */
    for (i = 1; moreargs(); ++i)
	setelement(val,i,nextarg());
    xllastarg();

    /* return the structure */
    return (val);
}

/* xcpystruct - the '%copy-struct' function */
LVAL xcpystruct()
{
    LVAL str,val;
    int size,i;
    str = xlgastruct();
    xllastarg();
    size = getsize(str);
    val = newstruct(getelement(str,0),size-1);
    for (i = 1; i < size; ++i)
	setelement(val,i,getelement(str,i));
    return (val);
}

/* xstrref - the '%struct-ref' function */
LVAL xstrref()
{
    LVAL str,val;
    int i;
    str = xlgastruct();
    val = xlgafixnum(); i = (int)getfixnum(val);
    xllastarg();
    if (i >= getsize(str)) /* wrong structure TAA MOD fix*/
	xlerror("bad structure reference",str);
    return (getelement(str,i));
}

/* xstrset - the '%struct-set' function */
LVAL xstrset()
{
    LVAL str,val;
    int i;
    str = xlgastruct();
    val = xlgafixnum(); i = (int)getfixnum(val);
    val = xlgetarg();
    xllastarg();
    if (i >= getsize(str)) /* wrong structure TAA MOD fix*/
	xlerror("bad structure reference",str);
    setelement(str,i,val);
    return (val);
}

/* xstrtypep - the '%struct-type-p' function */
LVAL xstrtypep()
{
    LVAL type,val;
    type = xlgasymbol();
    val = xlgetarg();
    xllastarg();
    return (structp(val) && getelement(val,0) == type ? true : NIL);
}

/* xdefstruct - the 'defstruct' special form */
LVAL xdefstruct()
{
    LVAL structname,slotname,defexpr,sym,tmp,args,body;
    LVAL options,oargs,slots;
    char FAR *pname;
    int slotn;

    /* protect some pointers */
    xlstkcheck(6);
    xlsave(structname);
    xlsave(slotname);
    xlsave(defexpr);
    xlsave(args);
    xlsave(body);
    xlsave(tmp);

    /* initialize */
    args = body = NIL;
    slotn = 0;

    /* get the structure name */
    tmp = xlgetarg();
    if (symbolp(tmp)) {
	structname = tmp;
	pname = getstring(getpname(structname));
#ifdef MEDMEM
	sprintf(prefix, "%Fs-", pname);
#else
	sprintf(prefix, "%s-", pname);
#endif
    }

    /* get the structure name and options */
    else if (consp(tmp) && symbolp(car(tmp))) {
	structname = car(tmp);
	pname = getstring(getpname(structname));
#ifdef MEDMEM
	sprintf(prefix, "%Fs-", pname);
#else
	sprintf(prefix, "%s-", pname);
#endif

	/* handle the list of options */
	for (options = cdr(tmp); consp(options); options = cdr(options)) {

	    /* get the next argument */
	    tmp = car(options);

	    /* handle options that don't take arguments */
	    if (symbolp(tmp)) {
		xlerror("unknown option",tmp);
	    }

	    /* handle options that take arguments */
	    else if (consp(tmp) && symbolp(car(tmp))) {
		oargs = cdr(tmp);

		/* check for the :CONC-NAME keyword */
		if (car(tmp) == k_concname) {

		    /* get the name of the structure to include */
		    if (!consp(oargs) || !symbolp(car(oargs)))
			xlerror("expecting a symbol",oargs);

		    /* save the prefix */
		    STRCPY(prefix,getstring(getpname(car(oargs))));
		}

		/* check for the :INCLUDE keyword */
		else if (car(tmp) == k_include) {

		    /* get the name of the structure to include */
		    if (!consp(oargs) || !symbolp(car(oargs)))
			xlerror("expecting a structure name",oargs);
		    tmp = car(oargs);
		    oargs = cdr(oargs);

		    /* add each slot from the included structure */
		    slots = xlgetprop(tmp,s_sslots);
		    for (; consp(slots); slots = cdr(slots)) {
			if (consp(car(slots)) && consp(cdr(car(slots)))) {

			    /* get the next slot description */
			    tmp = car(slots);

			    /* create the slot access functions */
			    addslot(car(tmp),car(cdr(tmp)),++slotn,&args,&body);
			}
		    }

		    /* handle slot initialization overrides */
		    for (; consp(oargs); oargs = cdr(oargs)) {
			tmp = car(oargs);
			if (symbolp(tmp)) {
			    slotname = tmp;
			    defexpr = NIL;
			}
			else if (consp(tmp) && symbolp(car(tmp))) {
			    slotname = car(tmp);
			    defexpr = (consp(cdr(tmp)) ? car(cdr(tmp)) : NIL);
			}
			else
			    xlerror("bad slot description",tmp);
			updateslot(args,slotname,defexpr);
		    }
		}
		else
		    xlerror("unknown option",tmp);
	    }
	    else
		xlerror("bad option syntax",tmp);
	}
    }

    /* get each of the structure members */
    while (moreargs()) {

	/* get the slot name and default value expression */
	tmp = xlgetarg();
	if (symbolp(tmp)) {
	    slotname = tmp;
	    defexpr = NIL;
	}
	else if (consp(tmp) && symbolp(car(tmp))) {
	    slotname = car(tmp);
	    defexpr = (consp(cdr(tmp)) ? car(cdr(tmp)) : NIL);
	}
	else
	    xlerror("bad slot description",tmp);

	/* create a closure for non-trival default expressions */
	if (defexpr != NIL) {
	    tmp = newclosure(NIL,s_lambda,xlenv,xlfenv);
	    setbody(tmp,cons(defexpr,NIL));
	    tmp = cons(tmp,NIL);
	    defexpr = tmp;
	}

	/* create the slot access functions */
	addslot(slotname,defexpr,++slotn,&args,&body);
    }

    /* store the slotnames and default expressions */
    xlputprop(structname,args,s_sslots);

    /* enter the MAKE-xxx symbol */
    sprintf(buf, makestr, pname);
    sym = xlenter(buf);

    /* make the MAKE-xxx function */
    args = cons(lk_key,args);
    tmp = cons(structname,NIL);
    tmp = cons(s_quote,tmp);
    body = cons(tmp,body);
    body = cons(s_mkstruct,body);
    body = cons(body,NIL);
    setfunction(sym,
		xlclose(sym,s_lambda,args,body,xlenv,xlfenv));

    /* enter the xxx-P symbol */
#ifdef MEDMEM
    sprintf(buf,"%Fs-P", pname);
#else
    sprintf(buf,"%s-P", pname);
#endif
    sym = xlenter(buf);

    /* make the xxx-P function */
    args = cons(s_x,NIL);
    body = cons(s_x,NIL);
    tmp = cons(structname,NIL);
    tmp = cons(s_quote,tmp);
    body = cons(tmp,body);
    body = cons(s_strtypep,body);
    body = cons(body,NIL);
    setfunction(sym,
		xlclose(sym,s_lambda,args,body,NIL,NIL));

    /* enter the COPY-xxx symbol */
#ifdef MEDMEM
    sprintf(buf,"COPY-%Fs", pname);
#else
    sprintf(buf,"COPY-%s", pname);
#endif
    sym = xlenter(buf);

    /* make the COPY-xxx function */
    args = cons(s_x,NIL);
    body = cons(s_x,NIL);
    body = cons(s_cpystruct,body);
    body = cons(body,NIL);
    setfunction(sym,
		xlclose(sym,s_lambda,args,body,NIL,NIL));

    /* restore the stack */
    xlpopn(6);

    /* return the structure name */
    return (structname);
}

/* xlrdstruct - convert a list to a structure (used by the reader) */
/* Modified by TAA to quote arguments and accept leading colons on keys */
LVAL xlrdstruct(list)
  LVAL list;
{
    LVAL structname,slotname,expr,last,val;

    /* protect the new structure */
    xlsave1(expr);

    /* get the structure name */
    if (!consp(list) || !symbolp(car(list)))
        xlerror("bad structure initialization list",list);
    structname = car(list);
    list = cdr(list);

    /* enter the MAKE-xxx symbol */
    sprintf(buf, makestr, getstring(getpname(structname)));

    /* initialize the MAKE-xxx function call expression */
    expr = cons(xlenter(buf),NIL);
    last = expr;

    /* turn the rest of the initialization list into keyword arguments */
    while (consp(list) && consp(cdr(list))) {

    /* get the slot keyword name */
    slotname = car(list);
    if (!symbolp(slotname))
	xlerror("expecting a slot name",slotname);


    /* add the slot keyword */
    if (*(getstring(getpname(slotname))) != ':') { /* add colon */
#ifdef MEDMEM
	sprintf(buf,":%Fs",getstring(getpname(slotname)));
#else
	sprintf(buf,":%s",getstring(getpname(slotname)));
#endif
	rplacd(last,cons(xlenter(buf),NIL));
    }
    else {
	rplacd(last,cons(slotname,NIL));
    }
    last = cdr(last);
    list = cdr(list);

    /* add the value expression	 -- QUOTED (TAA MOD) */
    rplacd(last,cons(NIL,NIL));
    last = cdr(last);
    rplaca(last, (slotname = cons(s_quote,NIL)));
    rplacd(slotname, cons(car(list), NIL));
    list = cdr(list);
    }

    /* make sure all of the initializers were used */
    if (consp(list))
    xlerror("bad structure initialization list",list);

    /* invoke the creation function */
    val = xleval(expr);

    /* restore the stack */
    xlpop();

    /* return the new structure */
    return (val);
}

/* xlprstruct - print a structure (used by printer) */
void xlprstruct(fptr,vptr,flag)
 LVAL fptr,vptr; int flag;
{
    LVAL next;
    int i,n;
    xlputstr(fptr,"#S(");   /* TAA MOD */
    xlprint(fptr,getelement(vptr,0),flag);
    next = xlgetprop(getelement(vptr,0),s_sslots);
    for (i = 1, n = getsize(vptr) - 1; i <= n && consp(next); ++i) {
	if (consp(car(next))) { /* should always succeed */
	    xlputc(fptr,' ');	/* Alternate, could print " :" */
	    xlprint(fptr,car(car(next)),flag);
	    xlputc(fptr,' ');
	    xlprint(fptr,getelement(vptr,i),flag);
	}
	next = cdr(next);
    }
    xlputc(fptr,')');
}

/* addslot - make the slot access functions */
LOCAL void NEAR addslot(slotname,defexpr,slotn,pargs,pbody)
 LVAL slotname,defexpr; int slotn; LVAL *pargs,*pbody;
{
    LVAL sym,args,body,tmp;

    /* protect some pointers */
    xlstkcheck(4);
    xlsave(sym);
    xlsave(args);
    xlsave(body);
    xlsave(tmp);

    /* construct the update function name */
#ifdef MEDMEM
    sprintf(buf,"%s%Fs",prefix,getstring(getpname(slotname)));
#else
    sprintf(buf,"%s%s",prefix,getstring(getpname(slotname)));
#endif
    sym = xlenter(buf);

    /* make the access function */
    args = cons(s_s,NIL);
    body = cons(cvfixnum((FIXTYPE)slotn),NIL);
    body = cons(s_s,body);
    body = cons(s_strref,body);
    body = cons(body,NIL);
    setfunction(sym,
		xlclose(sym,s_lambda,args,body,NIL,NIL));

    /* make the update function */
    args = cons(s_x,NIL);
    args = cons(s_s,args);
    body = cons(s_x,NIL);
    body = cons(cvfixnum((FIXTYPE)slotn),body);
    body = cons(s_s,body);
    body = cons(s_strset,body);
    body = cons(body,NIL);
    xlputprop(sym,
	      xlclose(NIL,s_lambda,args,body,NIL,NIL),
	      s_setf);

    /* add the slotname to the make-xxx keyword list */
    tmp = cons(defexpr,NIL);
    tmp = cons(slotname,tmp);
    tmp = cons(tmp,NIL);
    if ((args = *pargs) == NIL)
	*pargs = tmp;
    else {
	while (cdr(args) != NIL)
	    args = cdr(args);
	rplacd(args,tmp);
    }

    /* add the slotname to the %make-xxx argument list */
    tmp = cons(slotname,NIL);
    if ((body = *pbody) == NIL)
	*pbody = tmp;
    else {
	while (cdr(body) != NIL)
	    body = cdr(body);
	rplacd(body,tmp);
    }

    /* restore the stack */
    xlpopn(4);
}

/* updateslot - update a slot definition */
LOCAL void NEAR updateslot(args,slotname,defexpr)
 LVAL args,slotname,defexpr;
{
    LVAL tmp;
    for (; consp(args); args = cdr(args))
	if (slotname == car(car(args))) {
	    if (defexpr != NIL) {
		xlsave1(tmp);
		tmp = newclosure(NIL,s_lambda,xlenv,xlfenv);
		setbody(tmp,cons(defexpr,NIL));
		tmp = cons(tmp,NIL);
		defexpr = tmp;
		xlpop();
	    }
	    rplaca(cdr(car(args)),defexpr);
	    break;
	}
    if (args == NIL)
	xlerror("unknown slot name",slotname);
}

#endif /* STRUCTS */
