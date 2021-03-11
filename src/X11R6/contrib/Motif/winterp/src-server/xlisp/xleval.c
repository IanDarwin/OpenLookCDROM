/* -*-C-*-
********************************************************************************
*
* File:         xleval.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xleval.c,v 2.4 1994/06/06 15:59:14 npm Exp $
* Description:  xlisp evaluator
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:34 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xleval.c,v 2.4 1994/06/06 15:59:14 npm Exp $";

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

/* macro to check for lambda list keywords */
#define iskey(s) ((s) == lk_optional \
	       || (s) == lk_rest \
	       || (s) == lk_key \
	       || (s) == lk_aux \
	       || (s) == lk_allow_other_keys)

/* macros to handle tracing */
#define trenter(sym,argc,argv) {if (!null(sym)) doenter(sym,argc,argv);}
#define trexit(sym,val) {if (!null(sym)) doexit(sym,val);}

/* external variables */
extern LVAL xlenv,xlfenv,xldenv,xlvalue,true;
extern LVAL lk_optional,lk_rest,lk_key,lk_aux,lk_allow_other_keys;
extern LVAL s_evalhook,s_applyhook,s_tracelist;
extern LVAL s_lambda,s_macro;
extern LVAL s_unbound;
extern int xlsample;
#ifdef DISPMACRO
extern LVAL s_dispmacros;
#endif


/* local forward declarations */
#ifdef ANSI
LOCAL VOID NEAR badarglist(void); /* NPM: changed this to LOCAL */
LOCAL VOID NEAR doenter(LVAL sym, int argc, FRAMEP argv); /* NPM: changed this to LOCAL */
LOCAL VOID NEAR doexit(LVAL sym, LVAL val); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR evalhook(LVAL expr); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR evform(LVAL form); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR evfun(LVAL fun, int argc, FRAMEP argv);	/* NPM: changed this to LOCAL */
LOCAL int  NEAR evpushargs(LVAL fun,LVAL args);	/* NPM: changed this to LOCAL */
#ifdef WINTERP
      int xlmember(LVAL x, LVAL list); /* needed by ../w_callbacks.c */
#define member xlmember		/* needed for refs to member() in this file */
#else /* !defined(WINTERP */
LOCAL int  NEAR member(LVAL x, LVAL list); /* NPM: changed this to LOCAL */
#endif /* WINTERP */
#ifdef APPLYHOOK
LOCAL LVAL NEAR applyhook(LVAL fun, LVAL args);	/* NPM: changed this to LOCAL */
#endif
#else
LOCAL FORWARD VOID badarglist(); /* NPM: changed this to LOCAL */
LOCAL FORWARD VOID doenter();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID doexit();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL evalhook();  /* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL evform();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL evfun();	/* NPM: changed this to LOCAL */
#ifdef WINTERP
      FORWARD int xlmember();	/* needed by ../w_callbacks.c */
#define member xlmember		/* needed for refs to member() in this file */
#else /* !defined(WINTERP */
LOCAL FORWARD int member();
#endif /* WINTERP */
#ifdef APPLYHOOK
LOCAL FORWARD LVAL applyhook(); /* NPM: changed this to LOCAL */
#endif
#endif

#ifdef ANSI
static LVAL NEAR xlbadfunction(LVAL arg)
#else
LOCAL LVAL xlbadfunction(arg)
LVAL arg;
#endif
{
	return xlerror("bad function",arg);
}

/* xleval - evaluate an xlisp expression (checking for *evalhook*) */
LVAL xleval(expr)
  LVAL expr;
{
    /* check for control codes */
    if (--xlsample <= 0) {
	xlsample = SAMPLE;
	oscheck();
    }

    /* check for *evalhook* */
    if (!null(getvalue(s_evalhook)))
	return (evalhook(expr));

#ifndef NILSYMBOL
    /* check for nil */
    if (null(expr))
	return (NIL);
#endif

    /* dispatch on the node type */
    switch (ntype(expr)) {
    case CONS:
	return (evform(expr));
    case SYMBOL:
	return (xlgetvalue(expr));
    default:
	return (expr);
    }
}

/* xlxeval - evaluate an xlisp expression (bypassing *evalhook*) */
LVAL xlxeval(expr)
  LVAL expr;
{
#ifndef NILSYMBOL
    /* check for nil */
    if (null(expr))
	return (NIL);
#endif

    /* dispatch on node type */
    switch (ntype(expr)) {
    case CONS:
	return (evform(expr));
    case SYMBOL:
	return (xlgetvalue(expr));
    default:
	return (expr);
    }
}

/* xlapply - apply a function to arguments (already on the stack) */
LVAL xlapply(argc)
  int argc;
{
    LVAL fun,val;

    /* get the function */
    fun = xlfp[1];

    /* get the functional value of symbols */
    if (symbolp(fun)) {
	while ((val = getfunction(fun)) == s_unbound)
	    xlfunbound(fun);
	fun = xlfp[1] = val;
    }

    /* check for nil */
    if (null(fun))
	xlbadfunction(fun);

    /* dispatch on node type */
    switch (ntype(fun)) {
    case SUBR: {
		FRAMEP oldargv;
		int oldargc;
		oldargc = xlargc;
		oldargv = xlargv;
		xlargc = argc;
		xlargv = xlfp + 3;
		val = (*getsubr(fun))();
		xlargc = oldargc;
		xlargv = oldargv;
		break;
	}
    case CONS:
	if (!consp(cdr(fun)))
	    xlbadfunction(fun);
	if (car(fun) == s_lambda)
	    fun =   xlfp[1]	    /* TAA fix (vanNiekerk) */
		=   xlclose(NIL,
			  s_lambda,
			  car(cdr(fun)),
			  cdr(cdr(fun)),
			  xlenv,xlfenv);
	else
	    xlbadfunction(fun);
	/**** fall through into the next case ****/
    case CLOSURE:
	if (gettype(fun) != s_lambda)
	    xlbadfunction(fun);
	val = evfun(fun,argc,xlfp+3);
	break;
    default:
	xlbadfunction(fun);
    }

    /* remove the call frame */
    xlsp = xlfp;
    xlfp = xlfp - (int)getfixnum(*xlfp);

    /* return the function value */
    return (val);
}

/* evform - evaluate a form */
LOCAL LVAL NEAR evform(form)
  LVAL form;
{
    LVAL fun,args,val;
    LVAL tracing=NIL;
    FRAMEP argv;
    int argc;


    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    /* get the function and the argument list */
    fun = car(form);
    args = cdr(form);

    /* get the functional value of symbols */
    if (symbolp(fun)) {
	if (!null(getvalue(s_tracelist)) && member(fun,getvalue(s_tracelist)))
	    tracing = fun;
	fun = xlgetfunction(fun);
    }

    /* check for nil */
    if (null(fun))
	xlbadfunction(NIL);


    /* dispatch on node type */
    switch (ntype(fun)) {
    case SUBR:
#ifdef APPLYHOOK
	/* check for *applyhook* */
	if (!null(getvalue(s_applyhook))) {
	    val = (applyhook(fun,args));
	    break;
	}
#endif
	argv = xlargv;
	argc = xlargc;
	xlargc = evpushargs(fun,args);
	xlargv = xlfp + 3;
	trenter(tracing,xlargc,xlargv);
	val = (*getsubr(fun))();
	trexit(tracing,val);
	xlsp = xlfp;
	xlfp = xlfp - (int)getfixnum(*xlfp);
	xlargv = argv;
	xlargc = argc;
	break;
    case FSUBR:
	argv = xlargv;
	argc = xlargc;
	xlargc = pushargs(fun,args);
	xlargv = xlfp + 3;
	val = (*getsubr(fun))();
	xlsp = xlfp;
	xlfp = xlfp - (int)getfixnum(*xlfp);
	xlargv = argv;
	xlargc = argc;
	break;
    case CONS:
	if (!consp(cdr(fun)))
	    xlbadfunction(fun);
	if ((/* type = */ car(fun)) == s_lambda)
	    fun = xlclose(NIL,
			  s_lambda,
			  car(cdr(fun)),
			  cdr(cdr(fun)),
			  xlenv,xlfenv);
	else
	    xlbadfunction(fun);
	/**** fall through into the next case ****/
    case CLOSURE:
	if (gettype(fun) == s_lambda) {
#ifdef APPLYHOOK
	    /* check for *applyhook* */
	    if (!null(getvalue(s_applyhook))) {
		val = (applyhook(fun,args));
		break;
	    }
#endif
	    argc = evpushargs(fun,args);
	    argv = xlfp + 3;
	    trenter(tracing,argc,argv);
	    val = evfun(fun,argc,argv);
	    trexit(tracing,val);
	    xlsp = xlfp;
	    xlfp = xlfp - (int)getfixnum(*xlfp);
	}
	else {
	    macroexpand(fun,args,&fun);
#ifdef DISPMACRO
	    if (!null(getvalue(s_dispmacros)) && consp(fun)) {
		/* substitute back into original fcn */
		rplaca(form, car(fun));
		rplacd(form, cdr(fun));
	    }
#endif
	    val = xleval(fun);
	}
	break;
    default:
	xlbadfunction(fun);
    }

    /* restore the stack */
    xlpopn(2);

    /* return the result value */
    return (val);
}

/* xlexpandmacros - expand macros in a form */
LVAL xlexpandmacros(form)
  LVAL form;
{
    LVAL fun,args;

    /* protect some pointers */
    xlstkcheck(3);
    xlprotect(form);
    xlsave(fun);
    xlsave(args);

    /* expand until the form isn't a macro call */
    while (consp(form)) {
	fun = car(form);		/* get the macro name */
	args = cdr(form);		/* get the arguments */
	if (!symbolp(fun) || !fboundp(fun))
	    break;
	fun = xlgetfunction(fun);	/* get the expansion function */
	if (!macroexpand(fun,args,&form))
	    break;
    }

    /* restore the stack and return the expansion */
    xlpopn(3);
    return (form);
}

/* macroexpand - expand a macro call */
int macroexpand(fun,args,pval)
  LVAL fun,args,*pval;
{
    FRAMEP argv;
    int argc;

    /* make sure it's really a macro call */
    if (!closurep(fun) || gettype(fun) != s_macro)
	return (FALSE);

    /* call the expansion function */
    argc = pushargs(fun,args);
    argv = xlfp + 3;
    *pval = evfun(fun,argc,argv);
    xlsp = xlfp;
    xlfp = xlfp - (int)getfixnum(*xlfp);
    return (TRUE);
}

/* evalhook - call the evalhook function */
LOCAL LVAL NEAR evalhook(expr)
  LVAL expr;
{
    FRAMEP newfp;
    LVAL olddenv,val;

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getvalue(s_evalhook));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(expr);
    pusharg(cons(xlenv,xlfenv));
    xlfp = newfp;

    /* rebind the hook functions to nil */
    olddenv = xldenv;
    xldbind(s_evalhook,NIL);
    xldbind(s_applyhook,NIL);

    /* call the hook function */
    val = xlapply(2);

    /* unbind the symbols */
    xlunbind(olddenv);

    /* return the value */
    return (val);
}

#ifdef APPLYHOOK
/* applyhook - call the applyhook function */
LOCAL LVAL NEAR applyhook(fun,args)
  LVAL fun,args;
{
    FRAMEP newfp;
    LVAL olddenv,val,last,next;

    xlsave1(val);   /* protect against GC */

    if (consp(args)) { /* build argument list -- if there are any */
	/* we will pass evaluated arguments, with hooks enabled */
	/* so argument evaluation will be hooked too */
	val = last = consa(xleval(car(args)));
	args = cdr(args);
	while (consp(args)) { /* handle any more in loop */
	    next = consa(xleval(car(args)));
	    rplacd(last,next);
	    last = next;
	    args = cdr(args);
	}
    }

    /* create the new call frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(getvalue(s_applyhook));
    pusharg(cvfixnum((FIXTYPE)2));
    pusharg(fun);
    pusharg(val);
    xlfp = newfp;

    /* rebind hook functions to NIL */

    olddenv = xldenv;
    xldbind(s_evalhook,NIL);
    xldbind(s_applyhook,NIL);


    /* call the hook function */
    val = xlapply(2);

    /* unbind the symbols */
    xlunbind(olddenv);

    /* return the value */
    return (val);
}
#endif

/* evpushargs - evaluate and push a list of arguments */
LOCAL int NEAR evpushargs(fun,args)
  LVAL fun,args;
{
    FRAMEP newfp;
    int argc;

    /* protect the argument list */
    xlprot1(args);

    /* build a new argument stack frame */
    newfp = xlsp;

    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL); /* will be argc */

    /* evaluate and push each argument */
    for (argc = 0; consp(args); args = cdr(args), ++argc)
	pusharg(xleval(car(args)));

    /* establish the new stack frame */

    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* restore the stack */
    xlpop();

    /* return the number of arguments */
    return (argc);
}

/* pushargs - push a list of arguments */
int pushargs(fun,args)
  LVAL fun,args;
{
    FRAMEP newfp;
    int argc;

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(fun);
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; consp(args); args = cdr(args), ++argc)
	pusharg(car(args));

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* return the number of arguments */
    return (argc);
}

/* makearglist - make a list of the remaining arguments */
LVAL makearglist(argc,argv)
  int argc; LVAL *argv;
{
    LVAL list,this,last;
    xlsave1(list);
    for (last = NIL; --argc >= 0; last = this) {
	this = cons(*argv++,NIL);
	if (!null(last)) rplacd(last,this);
	else list = this;
     /* last = this; */ /* NPM: commented this out at request of jsp@glia.biostr.washington.edu (Jeff Prothero) */ 
    }
    xlpop();
    return (list);
}

/* evfun - evaluate a function */
#ifdef ANSI
static LVAL NEAR evfun(LVAL fun, int argc, FRAMEP argv)
#else
LOCAL LVAL evfun(fun,argc,argv)
  LVAL fun; int argc; FRAMEP argv;
#endif
{
    LVAL oldenv,oldfenv,cptr,val;
#ifdef SPECIALS
    LVAL olddenv=xldenv;
#endif
    CONTEXT cntxt;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(cptr);

    /* create a new environment frame */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlframe(getenvi(fun));
    xlfenv = getfenv(fun);

    /* bind the formal parameters */
    xlabind(fun,argc,argv);

    /* setup the implicit block */
    if (!null(getname(fun)))
	xlbegin(&cntxt,CF_RETURN,getname(fun));

    /* execute the block */
    if (!null(getname(fun)) && setjmp(cntxt.c_jmpbuf))
	val = xlvalue;
    else
	for (val = NIL, cptr = getbody(fun); consp(cptr); cptr = cdr(cptr)) {

		/* check for control codes */
		if (--xlsample <= 0) {
			xlsample = SAMPLE;
			oscheck();
		}

		val = car(cptr);

		/* check for *evalhook* */
		if (!null(getvalue(s_evalhook))) {
			val = evalhook(val);
			continue;
		}

		/* check for nil */
		if (null(val)) {
			val = NIL;
			continue;
		}

		/* dispatch on the node type */
		switch (ntype(val)) {
			case CONS:
				val = evform(val);
				break;
			case SYMBOL:
				val = xlgetvalue(val);
				break;
			default: /* nothing */
				break;
		}
	}
/*		val = xleval(car(cptr)); */

    /* finish the block context */
    if (!null(getname(fun)))
	xlend(&cntxt);

    /* restore the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;
#ifdef SPECIALS
    xlunbind(olddenv);
#endif

    /* restore the stack */
    xlpopn(3);

    /* return the result value */
    return (val);
}

/* xlclose - create a function closure */
LVAL xlclose(name,type,fargs,body,env,fenv)
  LVAL name,type,fargs,body,env,fenv;
{
    LVAL closure,key,arg,def,svar,new,last;
    char keyname[STRMAX+2];

    /* protect some pointers */
    xlsave1(closure);

    /* create the closure object */
    closure = newclosure(name,type,env,fenv);
    setlambda(closure,fargs);
    setbody(closure,body);

    /* handle each required argument */
    last = NIL;
    while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	/* make sure the argument is a symbol */
	if (!symbolp(arg))
	    badarglist();

	/* create a new argument list entry */
	new = cons(arg,NIL);

	/* link it into the required argument list */
	if (!null(last))
	    rplacd(last,new);
	else
	    setargs(closure,new);
	last = new;

	/* move the formal argument list pointer ahead */
	fargs = cdr(fargs);
    }

    /* check for the '&optional' keyword */
    if (consp(fargs) && car(fargs) == lk_optional) {
	fargs = cdr(fargs);

	/* handle each optional argument */
	last = NIL;
	while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	    /* get the default expression and specified-p variable */
	    def = svar = NIL;
	    if (consp(arg)) {
		if (!null(def = cdr(arg)))
		    if (consp(def)) {
			if (!null(svar = cdr(def)))
			    if (consp(svar)) {
				svar = car(svar);
				if (!symbolp(svar))
				    badarglist();
			    }
			    else
				badarglist();
			def = car(def);
		    }
		    else
			badarglist();
		arg = car(arg);
	    }

	    /* make sure the argument is a symbol */
	    if (!symbolp(arg))
		badarglist();

	    /* create a fully expanded optional expression */
	    new = cons(cons(arg,cons(def,cons(svar,NIL))),NIL);

	    /* link it into the optional argument list */
	    if (!null(last))
		rplacd(last,new);
	    else
		setoargs(closure,new);
	    last = new;

	    /* move the formal argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&rest' keyword */
    if (consp(fargs) && car(fargs) == lk_rest) {
	fargs = cdr(fargs);

	/* get the &rest argument */
	if (consp(fargs) && (!null((arg = car(fargs)))) && !iskey(arg) && symbolp(arg))
	    setrest(closure,arg);
	else
	    badarglist();

	/* move the formal argument list pointer ahead */
	fargs = cdr(fargs);
    }

    /* check for the '&key' keyword */
    if (consp(fargs) && car(fargs) == lk_key) {
	fargs = cdr(fargs);

	/* handle each key argument */
	last = NIL;
	while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	    /* get the default expression and specified-p variable */
	    def = svar = NIL;
	    if (consp(arg)) {
		if (!null(def = cdr(arg)))
		    if (consp(def)) {
			if (!null(svar = cdr(def)))
			    if (consp(svar)) {
				svar = car(svar);
				if (!symbolp(svar))
				    badarglist();
			    }
			    else
				badarglist();
			def = car(def);
		    }
		    else
			badarglist();
		arg = car(arg);
	    }

	    /* get the keyword and the variable */
	    if (consp(arg)) {
		key = car(arg);
		/* TAA MOD -- symbol must be keyword! */
		if ((!symbolp(key)) || getstring(getpname(key))[0] != ':')
		    badarglist();
		if (!null(arg = cdr(arg)))
		    if (consp(arg))
			arg = car(arg);
		    else
			badarglist();
	    }
	    else if (symbolp(arg)) {
		strcpy(keyname,":");
		STRCAT(keyname,getstring(getpname(arg)));
		key = xlenter(keyname);
	    }

	    /* make sure the argument is a symbol */
	    if (!symbolp(arg))
		badarglist();

	    /* create a fully expanded key expression */
	    new = cons(cons(key,cons(arg,cons(def,cons(svar,NIL)))),NIL);

	    /* link it into the optional argument list */
	    if (!null(last))
		rplacd(last,new);
	    else
		setkargs(closure,new);
	    last = new;

	    /* move the formal argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* check for the '&allow-other-keys' keyword */
    if (consp(fargs) && car(fargs) == lk_allow_other_keys)
#ifdef ALLOWOTHER
    {
	/* save marker that other keys are allowed */
	setkargs(closure,cons(lk_allow_other_keys,getkargs(closure)));
	fargs = cdr(fargs);
    }
#else
	fargs = cdr(fargs);	/* this is the default anyway */
#endif

    /* check for the '&aux' keyword */
    if (consp(fargs) && car(fargs) == lk_aux) {
	fargs = cdr(fargs);

	/* handle each aux argument */
	last = NIL;
	while (consp(fargs) && (!null(arg = car(fargs))) && !iskey(arg)) {

	    /* get the initial value */
	    def = NIL;
	    if (consp(arg)) {
		if (!null(def = cdr(arg)))
		    if (consp(def))
			def = car(def);
		    else
			badarglist();
		arg = car(arg);
	    }

	    /* make sure the argument is a symbol */
	    if (!symbolp(arg))
		badarglist();

	    /* create a fully expanded aux expression */
	    new = cons(cons(arg,cons(def,NIL)),NIL);

	    /* link it into the aux argument list */
	    if (!null(last))
		rplacd(last,new);
	    else
		setaargs(closure,new);
	    last = new;

	    /* move the formal argument list pointer ahead */
	    fargs = cdr(fargs);
	}
    }

    /* make sure this is the end of the formal argument list */
    if (!null(fargs))
	badarglist();

    /* restore the stack */
    xlpop();

    /* return the new closure */
    return (closure);
}

/* xlabind - bind the arguments for a function */
VOID xlabind(fun,argc,argv)
  LVAL fun; int argc; LVAL *argv;
{
    LVAL *kargv,fargs,key,arg,def,svar,p;
#ifdef ALLOWOTHER
    int keycount=0;
#endif
    int rargc,kargc;

    /* protect some pointers */
    xlsave1(def);

    /* bind each required argument */
    for (fargs = getargs(fun); !null(fargs); fargs = cdr(fargs)) {

	/* make sure there is an actual argument */
	if (--argc < 0)
	    xlfail("too few arguments");

#ifdef SPECIALS
	if (constantp(car(fargs))) xlnoassign(car(fargs));
#endif

	/* bind the formal variable to the argument value */
	xlbind(car(fargs),*argv++);
    }

    /* bind each optional argument */
    for (fargs = getoargs(fun); !null(fargs); fargs = cdr(fargs)) {

	/* get argument, default and specified-p variable */
	p = car(fargs);
	arg = car(p); p = cdr(p);
	def = car(p); p = cdr(p);
	svar = car(p);

#ifdef SPECIALS
	if (constantp(arg)) xlnoassign(arg);
	if ((!null(svar)) && constantp(svar)) xlnoassign(svar);
#endif

	/* bind the formal variable to the argument value */
	if (--argc >= 0) {
	    xlbind(arg,*argv++);
	    if (!null(svar)) xlbind(svar,true);
	}

	/* bind the formal variable to the default value */
	else {
	    if (!null(def)) def = xleval(def);
	    xlbind(arg,def);
	    if (!null(svar)) xlbind(svar,NIL);
	}
    }

    /* save the count of the &rest of the argument list */
    rargc = argc;

    /* handle '&rest' argument */
    if (!null(arg = getrest(fun))) {
#ifdef SPECIALS
	if (constantp(arg)) xlnoassign(arg);
#endif
	def = makearglist(argc,argv);
	xlbind(arg,def);
	argc = 0;
    }

    /* handle '&key' arguments */
    if (!null(fargs = getkargs(fun))) {
#ifdef ALLOWOTHER
	if (car(fargs) == lk_allow_other_keys)
	    fargs = cdr(fargs);	    /* toss marker */
	else
	    keycount = (rargc+1)/2; /* number of keyword arguments */
#endif
	for (; !null(fargs); fargs = cdr(fargs)) {

	    /* get keyword, argument, default and specified-p variable */
	    p = car(fargs);
	    key = car(p); p = cdr(p);
	    arg = car(p); p = cdr(p);
	    def = car(p); p = cdr(p);
	    svar = car(p);
#ifdef SPECIALS
	    if (constantp(arg)) xlnoassign(arg);
	    if (!null(svar) && constantp(svar)) xlnoassign(svar);
#endif

	    /* look for the keyword in the actual argument list */
	    for (kargv = argv, kargc = rargc; (kargc -= 2) >= 0; kargv += 2)
		if (*kargv == key)
		    break;

	    /* bind the formal variable to the argument value */
	    if (kargc >= 0) {
#ifdef ALLOWOTHER
		keycount--;
#endif
		xlbind(arg,*++kargv);
		if (!null(svar)) xlbind(svar,true);
	    }

	    /* bind the formal variable to the default value */
	    else {
		if (!null(def)) def = xleval(def);
		xlbind(arg,def);
		if (!null(svar)) xlbind(svar,NIL);
	    }
	}
#ifdef ALLOWOTHER
	if (keycount > 0) {
	    /* some keyword args were left over, and ! &allow-other-keys */
	    xlfail("too many keyword arguments");
	}
#endif
	argc = 0;
    }

    /* check for the '&aux' keyword */
    for (fargs = getaargs(fun); !null(fargs); fargs = cdr(fargs)) {

	/* get argument and default */
	p = car(fargs);
	arg = car(p); p = cdr(p);
	def = car(p);

#ifdef SPECIALS
	if (constantp(arg)) xlnoassign(arg);
#endif

	/* bind the auxiliary variable to the initial value */
	if (!null(def)) def = xleval(def);
	xlbind(arg,def);
    }

    /* make sure there aren't too many arguments */
    if (argc > 0)
	xlfail("too many arguments");

    /* restore the stack */
    xlpop();
}

/* doenter - print trace information on function entry */
#ifdef ANSI
static void NEAR doenter(LVAL sym, int argc, FRAMEP argv)
#else
LOCAL VOID doenter(sym,argc,argv)
  LVAL sym; int argc; FRAMEP argv;
#endif
{
    extern int xltrcindent;
    int i;

    /* indent to the current trace level */
    for (i = 0; i < xltrcindent; ++i)
	trcputstr(" ");
    ++xltrcindent;

    /* display the function call */
#ifdef MEDMEM
    sprintf(buf,"Entering: %Fs, Argument list: (",getstring(getpname(sym)));
#else
    sprintf(buf,"Entering: %s, Argument list: (",getstring(getpname(sym)));
#endif
    trcputstr(buf);
    while (--argc >= 0) {
	trcprin1(*argv++);
	if (argc) trcputstr(" ");
    }
    trcputstr(")\n");
}

/* doexit - print trace information for function/macro exit */
LOCAL VOID NEAR doexit(sym,val)
  LVAL sym,val;
{
    extern int xltrcindent;
    int i;

    /* indent to the current trace level */
    --xltrcindent;
    for (i = 0; i < xltrcindent; ++i)
	trcputstr(" ");

    /* display the function value */
#ifdef MEDMEM
    sprintf(buf,"Exiting: %Fs, Value: ",getstring(getpname(sym)));
#else
    sprintf(buf,"Exiting: %s, Value: ",getstring(getpname(sym)));
#endif
    trcputstr(buf);
    trcprin1(val);
    trcputstr("\n");
}

/* member - is 'x' a member of 'list'? */
#ifdef WINTERP
      int xlmember(x,list)	/* needed by ../w_callbacks.c */
#else /* !defined(WINTERP */
LOCAL int NEAR member(x,list)
#endif /* WINTERP */
  LVAL x,list;
{
    for (; consp(list); list = cdr(list))
	if (x == car(list))
	    return (TRUE);
    return (FALSE);
}

/* xlunbound - signal an unbound variable error */
VOID xlunbound(sym)
  LVAL sym;
{
    xlcerror("try evaluating symbol again","unbound variable",sym);
}

/* xlfunbound - signal an unbound function error */
VOID xlfunbound(sym)
  LVAL sym;
{
    xlcerror("try evaluating symbol again","unbound function",sym);
}

/* xlstkoverflow - signal a stack overflow error */
VOID xlstkoverflow()
{
    xlabort("evaluation stack overflow");
}

/* xlargstkoverflow - signal an argument stack overflow error */
VOID xlargstkoverflow()
{
    xlabort("argument stack overflow");
}

/* badarglist - report a bad argument list error */
LOCAL VOID NEAR badarglist()
{
    xlfail("bad formal argument list");
}
