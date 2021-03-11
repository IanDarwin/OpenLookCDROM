/* -*-C-*-
********************************************************************************
*
* File:         xlobj.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlobj.c,v 2.4 1994/06/06 15:59:21 npm Exp $
* Description:  xlisp object functions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:59 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlobj.c,v 2.4 1994/06/06 15:59:21 npm Exp $";

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
extern LVAL xlenv,xlfenv,xlvalue;
extern LVAL s_stdout,s_lambda;

/* local variables */
static LVAL s_self=0,k_new=0,k_isnew=0;
#ifdef OBJPRNT
static LVAL k_prin1,k_fix2;
#endif
static LVAL class=0,object=0;

#ifdef WINTERP
/*
 * instance variable numbers for the class 'Class'. WINTERP needs these in 
 * ../t_classes.c and ../w_classes.c so consolidate them to a single file.
 *  Remember to update these if you change numbers below
 */
#include "xlobj.h"
#else /* !defined(WINTERP) */
/* instance variable numbers for the class 'Class' */
#define MESSAGES	0	/* list of messages */
#define IVARS		1	/* list of instance variable names */
#define CVARS		2	/* list of class variable names */
#define CVALS		3	/* list of class variable values */
#define SUPERCLASS	4	/* pointer to the superclass */
#define IVARCNT		5	/* number of class instance variables */
#define IVARTOTAL	6	/* total number of instance variables */
#ifdef OBJPRNT
#define PNAME		7	/* print name TAA Mod */
#endif
/* number of instance variables for the class 'Class' */
#ifdef OBJPRNT
#define CLASSSIZE	8	/* TAA mod */
#else
#define CLASSSIZE	7
#endif
#endif /* WINTERP */    

/* forward declarations */
#ifdef ANSI
LOCAL LVAL NEAR entermsg(LVAL cls, LVAL msg); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR sendmsg(LVAL obj, LVAL cls, LVAL sym); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR evmethod(LVAL obj, LVAL msgcls, LVAL method); /* NPM: changed this to LOCAL */
#ifndef WINTERP			/* NPM: see xlisp.h -- need to export this for use by w_classes.c */
LOCAL int  NEAR getivcnt(LVAL cls, int ivar); /* NPM: changed this to LOCAL */
#endif /* !defined(WINTERP) */
LOCAL int  NEAR listlength(LVAL list); /* NPM: changed this to LOCAL */
#else /* !defined(ANSI) */
LOCAL FORWARD LVAL entermsg();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL sendmsg();	/* NPM: changed this to LOCAL */
LOCAL FORWARD LVAL evmethod();	/* NPM: changed this to LOCAL */
#ifndef WINTERP			/* NPM: see xlisp.h -- need to export this for use by w_classes.c */
LOCAL FORWARD int getivcnt();	/* NPM: this was missing from !defined(ANSI) */
#endif /* !defined(WINTERP) */
LOCAL FORWARD int  listlength(); /* NPM: this was missing from !defined(ANSI) */
#endif /* ANSI */

#ifdef OBJPRNT
/* routine to print an object for PRINx */
#ifdef ANSI
static VOID NEAR xputobj(LVAL fptr, LVAL val)
#else
LOCAL VOID xputobj(fptr,val)
  LVAL fptr; LVAL val;
#endif
{
    LVAL temp;
    if ((temp = getclass(val)) == class) { /* this is a class */
	if (null(temp = getivar(val,PNAME)) || (ntype(temp) != STRING) ) {
	    /* but nameless */
	    xlputstr(fptr,"#<class ???: #");
	}
	else {
#ifdef MEDMEM
	    sprintf(buf,"#<class %Fs: #",getstring(temp));
#else
	    sprintf(buf,"#<class %s: #",getstring(temp));
#endif
	    xlputstr(fptr,buf);
	}
    }
    else { /* not a class */
	if (null(temp = getivar(temp,PNAME)) || (ntype(temp) != STRING) ) {
	    /* but nameless */
	    xlputstr(fptr,"#<a ??? object: #");
	}
	else {
#ifdef MEDMEM
	    sprintf(buf,"#<a %Fs: #",getstring(temp));
#else
	    sprintf(buf,"#<a %s: #",getstring(temp));
#endif
	    xlputstr(fptr,buf);
	}
    }
    sprintf(buf,AFMT,val);
    xlputstr(fptr,buf);
    xlputc(fptr,'>');
}

#endif

/* xsend - send a message to an object */
LVAL xsend()
{
    LVAL obj;
    obj = xlgaobject();
    return (sendmsg(obj,getclass(obj),xlgasymbol()));
}

/* xsendsuper - send a message to the superclass of an object */
LVAL xsendsuper()
{
    LVAL env,p;
    for (env = xlenv; !null(env); env = cdr(env))
	if ((!null(p = car(env))) && objectp(car(p)))
	    return (sendmsg(car(p),
			    getivar(cdr(p),SUPERCLASS),
			    xlgasymbol()));
    xlfail("not in a method");
    return (NIL);   /* fake out compiler warning */
}

/* xlclass - define a class */
#ifdef ANSI
#ifndef WINTERP			/* export this for WINTERP... used in ../w_classes.c and ../wc_WIDGET.c */
static
#endif /* WINTERP */
LVAL NEAR xlclass(char *name, int vcnt)
#else /* !defined(ANSI) */
#ifndef WINTERP			/* export this for WINTERP... used in ../w_classes.c and ../wc_WIDGET.c */
LOCAL
#endif /* WINTERP */
LVAL xlclass(name,vcnt)
  char *name; int vcnt;
#endif /* ANSI */
{
    LVAL sym,cls;

    /* create the class */
    sym = xlenter(name);
    cls = newobject(class,CLASSSIZE);
    defconstant(sym,cls);   /* TAA MOD -- was setvalue */

    /* set the instance variable counts */
    setivar(cls,IVARCNT,cvfixnum((FIXTYPE)vcnt));
    setivar(cls,IVARTOTAL,cvfixnum((FIXTYPE)vcnt));

#ifdef OBJPRNT
    /* set the class name   TAA Mod */
    setivar(cls,PNAME,cvstring(name));
#endif

    /* set the superclass to 'Object' */
    setivar(cls,SUPERCLASS,object);

    /* return the new class */
    return (cls);
}

/* xlclass_p -- check if object is a class object as created by xlclass() */
#ifdef WINTERP
#ifdef ANSI
int xlclass_p(LVAL o_class)
#else /* !defined(ANSI) */
int xlclass_p(o_class)
     LVAL o_class;		/* for o_class, assume type==OBJECT */
#endif /* ANSI */
{
  return (getclass(o_class) == class);
}
#endif /* WINTERP */

/* xladdivar - enter an instance variable */
#ifdef ANSI
#ifndef WINTERP			/* export this for WINTERP... used in ../wc_WIDGET.c */
static
#endif /* WINTERP */
VOID NEAR xladdivar(LVAL cls, char *var)
#else /* !defined(ANSI) */
#ifndef WINTERP			/* export this for WINTERP... used in ../wc_WIDGET.c */
LOCAL
#endif /* WINTERP */
VOID xladdivar(cls,var)
  LVAL cls; char *var;
#endif /* ANSI */
{
    setivar(cls,IVARS,cons(xlenter(var),getivar(cls,IVARS)));
}

/* xladdmsg - add a message to a class */
#ifdef ANSI
#ifndef WINTERP			/* export this for WINTERP... used in ../wc_*.c */
static
#endif /* WINTERP */
VOID NEAR xladdmsg(LVAL cls, char *msg, int offset)
#else
#ifndef WINTERP			/* export this for WINTERP... used in ../wc_*.c */
LOCAL
#endif /* WINTERP */
VOID xladdmsg(cls,msg,offset)
  LVAL cls; char *msg; int offset;
#endif
{
    extern FUNDEF funtab[];
    LVAL mptr;

    /* enter the message selector */
    mptr = entermsg(cls,xlenter(msg));

    /* store the method for this message */
    rplacd(mptr,cvsubr(funtab[offset].fd_subr,funtab[offset].fd_type,offset));
}

/* xlobgetvalue - get the value of an instance variable */
int xlobgetvalue(pair,sym,pval)
  LVAL pair,sym,*pval;
{
    LVAL cls,names;
    int ivtotal,n;

    /* find the instance or class variable */
    for (cls = cdr(pair); objectp(cls); cls = getivar(cls,SUPERCLASS)) {

	/* check the instance variables */
	names = getivar(cls,IVARS);
	ivtotal = getivcnt(cls,IVARTOTAL);
	for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
	    if (car(names) == sym) {
		*pval = getivar(car(pair),n);
		return (TRUE);
	    }
	    names = cdr(names);
	}

	/* check the class variables */
	names = getivar(cls,CVARS);
	for (n = 0; consp(names); ++n) {
	    if (car(names) == sym) {
		*pval = getelement(getivar(cls,CVALS),n);
		return (TRUE);
	    }
	    names = cdr(names);
	}
    }

    /* variable not found */
    return (FALSE);
}

/* xlobsetvalue - set the value of an instance variable */
int xlobsetvalue(pair,sym,val)
  LVAL pair,sym,val;
{
    LVAL cls,names;
    int ivtotal,n;

    /* find the instance or class variable */
    for (cls = cdr(pair); objectp(cls); cls = getivar(cls,SUPERCLASS)) {

	/* check the instance variables */
	names = getivar(cls,IVARS);
	ivtotal = getivcnt(cls,IVARTOTAL);
	for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
	    if (car(names) == sym) {
		setivar(car(pair),n,val);
		return (TRUE);
	    }
	    names = cdr(names);
	}

	/* check the class variables */
	names = getivar(cls,CVARS);
	for (n = 0; consp(names); ++n) {
	    if (car(names) == sym) {
		setelement(getivar(cls,CVALS),n,val);
		return (TRUE);
	    }
	    names = cdr(names);
	}
    }

    /* variable not found */
    return (FALSE);
}

/* obisnew - default 'isnew' method */
LVAL obisnew()
{
    LVAL self;
    self = xlgaobject();
    xllastarg();
    return (self);
}

/* obclass - get the class of an object */
LVAL obclass()
{
    LVAL self;
    self = xlgaobject();
    xllastarg();
    return (getclass(self));
}

/* obshow - show the instance variables of an object */
LVAL obshow()
{
    LVAL self,fptr,cls,names;
    int ivtotal,n;

    /* get self and the file pointer */
    self = xlgaobject();
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* get the object's class */
    cls = getclass(self);

    /* print the object and class */
    xlputstr(fptr,"Object is ");
    xlprint(fptr,self,TRUE);
    xlputstr(fptr,", Class is ");
    xlprint(fptr,cls,TRUE);
    xlterpri(fptr);

    /* print the object's instance variables */
    for (; !null(cls); cls = getivar(cls,SUPERCLASS)) {
	names = getivar(cls,IVARS);
	ivtotal = getivcnt(cls,IVARTOTAL);
	for (n = ivtotal - getivcnt(cls,IVARCNT); n < ivtotal; ++n) {
	    xlputstr(fptr,"  ");
	    xlprint(fptr,car(names),TRUE);
	    xlputstr(fptr," = ");
	    xlprint(fptr,getivar(self,n),TRUE);
	    xlterpri(fptr);
	    names = cdr(names);
	}
    }

    /* return the object */
    return (self);
}

/* clnew - create a new object instance */
LVAL clnew()
{
    LVAL self;
    self = xlgaobject();
    return (newobject(self,getivcnt(self,IVARTOTAL)));
}

/* clisnew - initialize a new class */
LVAL clisnew()
{
    LVAL self,ivars,cvars,super;
    int n;

    /* get self, the ivars, cvars and superclass */
    self = xlgaobject();
    ivars = xlgalist();
    cvars = (moreargs() ? xlgalist() : NIL);
    super = (moreargs() ? xlgaobject() : object);
    xllastarg();

    /* store the instance and class variable lists and the superclass */
    setivar(self,IVARS,ivars);
    setivar(self,CVARS,cvars);
    setivar(self,CVALS,(!null(cvars) ? newvector(listlength(cvars)) : NIL));
    setivar(self,SUPERCLASS,super);

    /* compute the instance variable count */
    n = listlength(ivars);
    setivar(self,IVARCNT,cvfixnum((FIXTYPE)n));
    n += getivcnt(super,IVARTOTAL);
    setivar(self,IVARTOTAL,cvfixnum((FIXTYPE)n));

    /* return the new class object */
    return (self);
}

/* clanswer - define a method for answering a message */
LVAL clanswer()
{
    LVAL self,msg,fargs,code,mptr;

    /* message symbol, formal argument list and code */
    self = xlgaobject();
    msg = xlgasymbol();
    fargs = xlgalist();
    code = xlgalist();
    xllastarg();

    /* make a new message list entry */
    mptr = entermsg(self,msg);

    /* setup the message node */
    xlprot1(fargs);
    fargs = cons(s_self,fargs); /* add 'self' as the first argument */
	/* The following TAA MOD is by Niels Mayer, at HP */
	/* it sets the lexical environment to be correct (non-global) */
/*    rplacd(mptr,xlclose(msg,s_lambda,fargs,code,NIL,NIL)); */
    rplacd(mptr,xlclose(msg,s_lambda,fargs,code,xlenv,xlfenv));
    xlpop();

    /* return the object */
    return (self);
}

/* entermsg - add a message to a class */
LOCAL LVAL NEAR entermsg(cls,msg)
  LVAL cls,msg;
{
    LVAL lptr,mptr;

    /* lookup the message */
    for (lptr = getivar(cls,MESSAGES); !null(lptr); lptr = cdr(lptr))
	if (car(mptr = car(lptr)) == msg)
	    return (mptr);

    /* allocate a new message entry if one wasn't found */
    xlsave1(mptr);
    mptr = consa(msg);
    setivar(cls,MESSAGES,cons(mptr,getivar(cls,MESSAGES)));
    xlpop();

    /* return the symbol node */
    return (mptr);
}

/* sendmsg - send a message to an object */
LOCAL LVAL NEAR sendmsg(obj,cls,sym)
  LVAL obj,cls,sym;
{
    LVAL msg,msgcls,method,val,p;

    /* look for the message in the class or superclasses */
    for (msgcls = cls; !null(msgcls); ) {

	/* lookup the message in this class */
	for (p = getivar(msgcls,MESSAGES); !null(p); p = cdr(p))
	    if ((!null(msg = car(p))) && car(msg) == sym)
		goto send_message;

	/* look in class's superclass */
	msgcls = getivar(msgcls,SUPERCLASS);
    }

    /* message not found */
    xlerror("no method for this message",sym);

send_message:

    /* insert the value for 'self' (overwrites message selector) */
    *--xlargv = obj;
    ++xlargc;

#ifdef WINTERP
      *(xlargv - 1) = sym;
#endif /* WINTERP */    

    /* invoke the method */
    if (null(method = cdr(msg)))
	xlerror("bad method",method);
    switch (ntype(method)) {
    case SUBR:
	val = (*getsubr(method))();
	break;
    case CLOSURE:
	if (gettype(method) != s_lambda)
	    xlerror("bad method",method);
	val = evmethod(obj,msgcls,method);
	break;
    default:
	xlerror("bad method",method);
    }

    /* after creating an object, send it the ":isnew" message */
    if (car(msg) == k_new && (val!=NIL)) { /* NPM: 4/22/94 -- "(val!=NIL)" -- stole from xlisp 2.1f */
	xlprot1(val);
	sendmsg(val,getclass(val),k_isnew);
	xlpop();
    }

    /* return the result value */
    return (val);
}

/* evmethod - evaluate a method */
LOCAL LVAL NEAR evmethod(obj,msgcls,method)
  LVAL obj,msgcls,method;
{
    LVAL oldenv,oldfenv,cptr,name,val;
    CONTEXT cntxt;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(cptr);

    /* create an 'object' stack entry and a new environment frame */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = cons(cons(obj,msgcls),getenvi(method));
    xlenv = xlframe(xlenv);
    xlfenv = getfenv(method);

    /* bind the formal parameters */
    xlabind(method,xlargc,xlargv);

    /* setup the implicit block */
    if (!null(name = getname(method)))
	xlbegin(&cntxt,CF_RETURN,name);

    /* execute the block */
    if (name && setjmp(cntxt.c_jmpbuf))
	val = xlvalue;
    else
	for (cptr = getbody(method); consp(cptr); cptr = cdr(cptr))
	    val = xleval(car(cptr));

    /* finish the block context */
    if (!null(name))
	xlend(&cntxt);

    /* restore the environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(3);

    /* return the result value */
    return (val);
}

/* getivcnt - get the number of instance variables for a class */
#ifdef ANSI
#ifndef WINTERP			/* NPM: need to export this for use by w_classes.c */
static
#endif /* !defined(WINTERP) */
int NEAR getivcnt(LVAL cls, int ivar)
#else
#ifndef WINTERP			/* NPM: need to export this for use by w_classes.c */
LOCAL
#endif /* !defined(WINTERP) */
int getivcnt(cls,ivar)
  LVAL cls; int ivar;
#endif
{
    LVAL cnt;
    if (null(cnt = getivar(cls,ivar)) || !fixp(cnt))
	xlfail("bad value for instance variable count");
    return ((int)getfixnum(cnt));
}

/* listlength - find the length of a list */
#ifdef ANSI
static int NEAR listlength(LVAL list)
#else
LOCAL int listlength(list)
  LVAL list;
#endif
{
    int len;
    for (len = 0; consp(list); len++)
	list = cdr(list);
    return (len);
}

/* obsymbols - initialize symbols */
VOID obsymbols()
{
    /* enter the object related symbols */
    s_self  = xlenter("SELF");
    k_new   = xlenter(":NEW");
    k_isnew = xlenter(":ISNEW");
#ifdef OBJPRNT
    k_prin1 = xlenter(":PRIN1");
#endif

    /* get the Object and Class symbol values */
    object = getvalue(xlenter("OBJECT"));
    class  = getvalue(xlenter("CLASS"));
}

/* xloinit - object function initialization routine */
VOID xloinit()
{
    /* create the 'Class' object */
    class = xlclass("CLASS",CLASSSIZE);
    setelement(class,0,class);

    /* create the 'Object' object */
    object = xlclass("OBJECT",0);

    /* finish initializing 'class' */
    setivar(class,SUPERCLASS,object);
#ifdef OBJPRNT
    xladdivar(class,"PNAME");		/* ivar number 7  TAA Mod */
#endif
    xladdivar(class,"IVARTOTAL");	/* ivar number 6 */
    xladdivar(class,"IVARCNT");		/* ivar number 5 */
    xladdivar(class,"SUPERCLASS");	/* ivar number 4 */
    xladdivar(class,"CVALS");		/* ivar number 3 */
    xladdivar(class,"CVARS");		/* ivar number 2 */
    xladdivar(class,"IVARS");		/* ivar number 1 */
    xladdivar(class,"MESSAGES");	/* ivar number 0 */
    xladdmsg(class,":NEW",FT_CLNEW);
    xladdmsg(class,":ISNEW",FT_CLISNEW);
    xladdmsg(class,":ANSWER",FT_CLANSWER);

    /* finish initializing 'object' */
    setivar(object,SUPERCLASS,NIL);
    xladdmsg(object,":ISNEW",FT_OBISNEW);
    xladdmsg(object,":CLASS",FT_OBCLASS);
    xladdmsg(object,":SHOW",FT_OBSHOW);
#ifdef OBJPRNT
    xladdmsg(object,":PRIN1",FT_OBPRIN1);

    /* other stuff needed in this module */
    k_fix2 = cvfixnum((FIXTYPE)2);	/* so we don't have to recompute it */
#endif
}

#ifdef OBJPRNT
/* default :PRIN1 method for objects */
LVAL obprin1()
{
    LVAL self,fptr;

    /* get self and the file pointer */
    self = xlgaobject();
#ifdef BETTERIO
    fptr = (moreargs() ? xlgetfile(TRUE) : getvalue(s_stdout));
#else
    fptr = (moreargs() ? xlgetfile() : getvalue(s_stdout));
#endif
    xllastarg();

    /* print it */
    xputobj(fptr,self);

    /* return the object */
    return (self);
}

/* called by xlprint to tell an object to print itself by faking
   a call like (send obj :prin1 fptr) */
VOID putobj(fptr,obj)
    LVAL fptr,obj;
{
    FRAMEP oldargv;
    int oldargc;

    /* check if there's room for the new call frame (5 slots needed) */
    if (xlsp >= (xlargstktop-5)) xlargstkoverflow();

    /* create a new (dummy) call frame. dummy because (1) stack backtraces
     * won't work anyway since if there's an error when PRINTing an object,
     * that error will probably occur again during the backtrace, and
     * (2) sendmsg() trashes the message selector slot.
     */
    *xlsp   = cvfixnum((FIXTYPE)(xlsp - xlfp));
    xlfp    = xlsp++;	/* new frame pointer */
    *xlsp++ = NIL;	/* dummy function */
    *xlsp++ = k_fix2;	/* we have two arguments */
    *xlsp++ = k_prin1; /* 1st arg: the message (trashed by sendmsg()) */
    *xlsp++ = fptr;	/* 2nd arg: the file/stream */

    /* save old xlargc and xlargv. set up new ones */
    oldargc = xlargc;
    oldargv = xlargv;
    xlargc  = 1;	/* one arg to be picked up */
    xlargv  = xlfp + 4; /* points at 2nd arg: the file/stream */

    /* do it */
    sendmsg(obj,getclass(obj),k_prin1);

    /* restore xlargc and xlargv */
    xlargc  = oldargc;
    xlargv  = oldargv;

    /* remove call frame */
    xlsp    = xlfp;
    xlfp   -= (int)getfixnum(*xlfp);
}
#endif

