/* -*-C-*-
********************************************************************************
*
* File:         xlbfun.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlbfun.c,v 2.4 1994/06/06 15:59:11 npm Exp $
* Description:  xlisp basic built-in functions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:45 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlbfun.c,v 2.4 1994/06/06 15:59:11 npm Exp $";

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
extern LVAL xlenv,xlfenv,xldenv,true;
extern LVAL s_evalhook,s_applyhook;
extern LVAL s_unbound, s_quote;
extern char gsprefix[];
extern FIXTYPE gsnumber;

/* forward declarations */
#ifdef ANSI
LOCAL LVAL NEAR makesymbol(int iflag);
#else
LOCAL FORWARD LVAL makesymbol();
#endif

#if 0	/* original version uses current environment */
/* xeval - the built-in function 'eval' */
LVAL xeval()
{
    LVAL expr;

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    /* evaluate the expression */
    return (xleval(expr));
}
#else /* Common Lisp compatible version uses global environment */
/* xeval - the built-in function 'eval' */
LVAL xeval()
{
    LVAL expr,oldenv,oldfenv;

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* get the expression to evaluate */
    expr = xlgetarg();
    xllastarg();

    /*establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;

    /* evaluate the expression */
    expr = xleval(expr);

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    /* return evaluated expression */
    return (expr);
}
#endif

/* xapply - the built-in function 'apply' */
/* TAA MOD -- when COMMONLISP, allows multiple arguments as in CL */
/* Algorithm based on Luke Tierney's XLISP-STAT */
#ifdef COMMONLISP
LVAL xapply()
{
    LVAL fun,arglist;
    int n;

    if (xlargc < 2) xltoofew();
    if (! listp(xlargv[xlargc - 1])) xlfail("last argument must be a list");

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(arglist);
    xlprotect(fun);

    fun = xlgetarg();
    n = xlargc - 1;
    arglist = xlargv[n];
    while (n-- > 0) arglist = cons(xlargv[n], arglist);

    /* restore the stack */
    xlpopn(2);

    return xlapply(pushargs(fun, arglist));
}
#else
LVAL xapply()
{
    LVAL fun,arglist;

    /* get the function and argument list */
    fun = xlgetarg();
    arglist = xlgalist();
    xllastarg();

    /* apply the function to the arguments */
    return (xlapply(pushargs(fun,arglist)));
}
#endif

/* xfuncall - the built-in function 'funcall' */
LVAL xfuncall()
{
    FRAMEP newfp;
    int argc;

    /* build a new argument stack frame */
    newfp = xlsp;
    pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
    pusharg(xlgetarg());
    pusharg(NIL); /* will be argc */

    /* push each argument */
    for (argc = 0; moreargs(); ++argc)
	pusharg(nextarg());

    /* establish the new stack frame */
    newfp[2] = cvfixnum((FIXTYPE)argc);
    xlfp = newfp;

    /* apply the function to the arguments */
    return (xlapply(argc));
}

/* xmacroexpand - expand a macro call repeatedly */
LVAL xmacroexpand()
{
    LVAL form;
    form = xlgetarg();
    xllastarg();
    return (xlexpandmacros(form));
}

/* x1macroexpand - expand a macro call */
LVAL x1macroexpand()
{
    LVAL form,fun,args;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(args);

    /* get the form */
    form = xlgetarg();
    xllastarg();

    /* expand until the form isn't a macro call */
    if (consp(form)) {
	fun = car(form);		/* get the macro name */
	args = cdr(form);		/* get the arguments */
	if (symbolp(fun) && fboundp(fun)) {
	    fun = xlgetfunction(fun);	/* get the expansion function */
	    macroexpand(fun,args,&form);
	}
    }

    /* restore the stack and return the expansion */
    xlpopn(2);
    return (form);
}

/* xatom - is this an atom? */
LVAL xatom()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (atom(arg) ? true : NIL);
}

/* xsymbolp - is this an symbol? */
LVAL xsymbolp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
#ifdef NILSYMBOL
    return (symbolp(arg) ? true : NIL);
#else
    return (null(arg) || symbolp(arg) ? true : NIL);
#endif
}

/* xnumberp - is this a number? */
LVAL xnumberp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
#ifdef COMPLX
    return (numberp(arg) || complexp(arg) ? true : NIL);
#else
    return (fixp(arg) || floatp(arg) ? true : NIL);
#endif
}

#ifdef COMPLX
/* xcomplexp - is this a complex number? */
LVAL xcomplexp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (complexp(arg) ? true : NIL);
}
#endif

/* xintegerp - is this an integer? */
LVAL xintegerp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (fixp(arg) ? true : NIL);
}

/* xfloatp - is this a float? */
LVAL xfloatp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (floatp(arg) ? true : NIL);
}

/* xcharp - is this a character? */
LVAL xcharp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (charp(arg) ? true : NIL);
}

/* xstringp - is this a string? */
LVAL xstringp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (stringp(arg) ? true : NIL);
}

/* xarrayp - is this an array? */
LVAL xarrayp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (vectorp(arg) ? true : NIL);
}

/* xstreamp - is this a stream? */
LVAL xstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (streamp(arg) || ustreamp(arg) ? true : NIL);
}

#ifdef BETTERIO

/* xopenstreamp - is this an open stream? */
LVAL xopenstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return true;
    if (streamp(arg)) return (getfile(arg) != CLOSED ? true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xinputstreamp - is this an input stream? */
LVAL xinputstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return true;
    if (streamp(arg))
	return (getfile(arg)!=CLOSED && (arg->n_sflags&S_FORREADING)?
	    true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

/* xoutputstreamp - is this an output stream? */
LVAL xoutputstreamp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    if (ustreamp(arg)) return true;
    if (streamp(arg))
	return (getfile(arg)!=CLOSED && (arg->n_sflags&S_FORWRITING)?
	    true : NIL);
    xlbadtype(arg);
    return NIL; /* never executes */
}

#endif

/* xobjectp - is this an object? */
LVAL xobjectp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (objectp(arg) ? true : NIL);
}

/* xboundp - is this a value bound to this symbol? */
LVAL xboundp()
{
    LVAL sym;
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();
#ifdef NILSYMBOL
    return (boundp(sym) ? true : NIL);
#else
    return (null(sym) || boundp(sym) ? true : NIL);
#endif
}

/* xfboundp - is this a functional value bound to this symbol? */
LVAL xfboundp()
{
    LVAL sym;
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();
#ifdef NILSYMBOL
    return (fboundp(sym) ? true : NIL);
#else
    return (sym != NIL && fboundp(sym) ? true : NIL);
#endif
}

#ifdef SPECIALS
/* xconstantp - is this constant? TAA addition*/
LVAL xconstantp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();

    if ((!null(arg)) &&
	(((ntype(arg)==CONS) && (car(arg) != s_quote)) ||
	 ((ntype(arg)==SYMBOL) && (!constantp(arg)))))
	return (NIL);
    return (true);
}
#endif


/* xnull - is this null? */
LVAL xnull()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (null(arg) ? true : NIL);
}

/* xlistp - is this a list? */
LVAL xlistp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (listp(arg) ? true : NIL);
}

/* xendp - is this the end of a list? */
LVAL xendp()
{
    LVAL arg;
    arg = xlgalist();
    xllastarg();
    return (null(arg) ? true : NIL);
}

/* xconsp - is this a cons? */
LVAL xconsp()
{
    LVAL arg;
    arg = xlgetarg();
    xllastarg();
    return (consp(arg) ? true : NIL);
}

/* xeq - are these equal? */
LVAL xeq()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (arg1 == arg2 ? true : NIL);
}

/* xeql - are these equal? */
LVAL xeql()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (eql(arg1,arg2) ? true : NIL);
}

/* xequal - are these equal? (recursive) */
LVAL xequal()
{
    LVAL arg1,arg2;

    /* get the two arguments */
    arg1 = xlgetarg();
    arg2 = xlgetarg();
    xllastarg();

    /* compare the arguments */
    return (equal(arg1,arg2) ? true : NIL);
}

/* xset - built-in function set */
LVAL xset()
{
    LVAL sym,val;

    /* get the symbol and new value */
    sym = xlgasymbol();
    val = xlgetarg();
    xllastarg();

#ifdef SPECIALS
    if (constantp(sym)) {
	xlnoassign(sym);
    }
#endif

    /* assign the symbol the value of argument 2 and the return value */
    setvalue(sym,val);

    /* return the result value */
    return (val);
}

/* xgensym - generate a symbol */
LVAL xgensym()
{
    char sym[STRMAX+11]; /* enough space for prefix and number */
    LVAL x;

    /* get the prefix or number */
    if (moreargs()) {
	x = xlgetarg();
	switch (null(x)? CONS : ntype(x)) { /* was ntype(x)   TAA Mod */
	case SYMBOL:
		x = getpname(x);
	case STRING:
		STRNCPY(gsprefix,getstring(x),STRMAX);
		gsprefix[STRMAX] = '\0';
		break;
	case FIXNUM:
		gsnumber = getfixnum(x);
		break;
	default:
		xlbadtype(x);
	}
    }
    xllastarg();

    /* create the pname of the new symbol */
    sprintf(sym,"%s%d",gsprefix,gsnumber++);

    /* make a symbol with this print name */
    return (xlmakesym(sym));
}

/* xmakesymbol - make a new uninterned symbol */
LVAL xmakesymbol()
{
    return (makesymbol(FALSE));
}

/* xintern - make a new interned symbol */
LVAL xintern()
{
    return (makesymbol(TRUE));
}

/* makesymbol - make a new symbol */
LOCAL LVAL NEAR makesymbol(iflag)
  int iflag;
{
    LVAL pname;
	int i;

    /* get the print name of the symbol to intern */
    pname = xlgastring();
    xllastarg();

    /* check for containing only printable characters */
    i = getslength(pname);
    if (i >= STRMAX)
	xlerror("too long", pname);
    while (i-- > 0) if (pname->n_string[i] < 32 )
	xlerror("non-printing characters",pname);

    /* make the symbol */
#ifdef MEDMEM
    STRCPY(buf, getstring(pname));
    return (iflag ? xlenter(buf)
		  : xlmakesym(buf));
#else
    return (iflag ? xlenter(getstring(pname))
		  : xlmakesym(getstring(pname)));
#endif
}

/* xsymname - get the print name of a symbol */
LVAL xsymname()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();

#ifndef NILSYMBOL
    /* handle NIL, which is not internally represented as a symbol */
    if (null(sym)) {
	sym = newstring(3);
	strcpy((char *)getstring(sym), "NIL");
	return sym;
    }
#endif

    /* return the print name */
    return (getpname(sym));
}

/* xsymvalue - get the value of a symbol */
LVAL xsymvalue()
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();

#ifndef NILSYMBOL
    /* handle NIL */
    if (null(sym)) return (NIL);
#endif


    /* get the global value */
    while ((val = getvalue(sym)) == s_unbound)
	xlunbound(sym);

    /* return its value */
    return (val);
}

/* xsymfunction - get the functional value of a symbol */
LVAL xsymfunction()
{
    LVAL sym,val;

    /* get the symbol */
    sym = xlgasymornil();	/* TAA fix */
    xllastarg();

#ifndef NILSYMBOL
    /* handle NIL */
    if (null(sym)) {
	while (1)
	    xlfunbound(sym);
    }
#endif


    /* get the global value */
    while ((val = getfunction(sym)) == s_unbound)
	xlfunbound(sym);

    /* return its value */
    return (val);
}

/* xsymplist - get the property list of a symbol */
LVAL xsymplist()
{
    LVAL sym;

    /* get the symbol */
    sym = xlgasymornil();   /* TAA fix */
    xllastarg();

    /* return the property list */
#ifdef NILSYMBOL
    return (getplist(sym));
#else
    return (null(sym) ? NIL : getplist(sym));
#endif
}

/* xget - get the value of a property */
LVAL xget()
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
#ifdef COMMONLISP
    prp = xlgetarg();
#else
    prp = xlgasymbol();
#endif
    xllastarg();

    /* retrieve the property value */
    return (xlgetprop(sym,prp));
}

/* xputprop - set the value of a property */
LVAL xputprop()
{
    LVAL sym,val,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
    val = xlgetarg();
#ifdef COMMONLISP
    prp = xlgetarg();
#else
    prp = xlgasymbol();
#endif
    xllastarg();

    /* set the property value */
    xlputprop(sym,val,prp);

    /* return the value */
    return (val);
}

/* xremprop - remove a property value from a property list */
LVAL xremprop()
{
    LVAL sym,prp;

    /* get the symbol and property */
    sym = xlgasymbol();
#ifdef COMMONLISP
    prp = xlgetarg();
#else
    prp = xlgasymbol();
#endif
    xllastarg();

    /* remove the property */
    xlremprop(sym,prp);

    /* return nil */
    return (NIL);
}

/* xhash - compute the hash value of a string or symbol */
/* TAA Modified to hash anything */
LVAL xhash()
{
    LVAL len,val;
    int n;

    /* get the object and the table length */
    val = xlgetarg();
    len = xlgafixnum(); n = (int)getfixnum(len);
    xllastarg();

    /* check for hash arg out of range */
    if (n <= 0) xlbadtype(len);

    /* return the hash index */
    return (cvfixnum((FIXTYPE)xlhash(val,n)));
}

/* xaref - array reference function */
LVAL xaref()
{
    LVAL array,index;
    FIXTYPE i;		/* TAA fix */

    /* get the array (may be a string) and the index */
#ifdef COMMONLISP	/* allows strings to work with AREF */
    array = xlgetarg();
#else
    array = xlgavector();
#endif
    index = xlgafixnum(); i = /*(int) */ getfixnum(index);	/* TAA fix */
    xllastarg();

#ifdef COMMONLISP
    if (stringp(array)) {   /* extension -- allow fetching chars from string*/
	if (i < 0 || i >= getslength(array))
	    xlerror("string index out of bounds",index);
	return (cvchar(getstringch(array,(int)i)));
    }

    if (!vectorp(array)) xlbadtype(array);  /* type must be array */
#endif

    /* range check the index */
    if (i < 0 || i >= getsize(array))
	xlerror("array index out of bounds",index);

    /* return the array element */
    return (getelement(array,(int)i));	/* TAA fix -- casting */
}

/* xmkarray - make a new array */
LVAL xmkarray()
{
    LVAL size;
    FIXTYPE n;

    /* get the size of the array */
    size = xlgafixnum() ; n = getfixnum(size);
    if (n < 0 || n > MAXSLEN )
	xlerror("out of range",size);
    xllastarg();

    /* create the array */
    return (newvector((unsigned)n));
}

/* xvector - make a vector */
LVAL xvector()
{
    LVAL val;
    int i;

    /* make the vector */
    val = newvector(xlargc);

    /* store each argument */
    for (i = 0; moreargs(); ++i)
	setelement(val,i,nextarg());
    xllastarg();

    /* return the vector */
    return (val);
}

/******************************************************************************
 * (copy-array <src> <dest> [<pos>]) --> returns <dest>
 * This function copies from array <src> into the preallocated array <dest>
 * (allocate with 'make-array'). If the optional arg <pos> is given, then
 * elements from <src> will be written into <dest> at index <pos>, otherwise
 * <pos> defaults to 0. 
 *
 * This function was added to xlisp by Niels Mayer.
 ******************************************************************************/
LVAL Prim_COPY_ARRAY()
{
  register int size;
  register LVAL *src, *dest;
  LVAL src_array, dest_array, lval_pos;

  src_array = xlgavector();	/* get <src> */
  dest_array = xlgavector();	/* get <dest> */
  if moreargs()
    lval_pos = xlgafixnum();	/* get optional <pos> */
  else
    lval_pos = NIL;
  xllastarg();

  src = src_array->n_vdata;
  dest = dest_array->n_vdata;

  if (getsize(src_array) < getsize(dest_array))	/* which is shortest? */
    size = getsize(src_array);
  else
    size = getsize(dest_array);

  if (lval_pos != NIL) {
    int pos = getfixnum(lval_pos);
    int len = getsize(dest_array) - pos;
    if ((len <= 0) || (pos < 0))
      xlerror("Array position out of bounds.", lval_pos);    
    if (len < size)
      size = len;
    dest = dest + pos;
  }

  while (size--)
    *dest++ = *src++;

  return (dest_array);
}

/******************************************************************************
 * (array-insert-pos <array> <pos> <elt>) --> returns the new <array>
 * inserts <elt> at index <pos> in <array>. if <pos> < 0, then <elt> is
 * appended to the end of <array>.
 *
 * This function was added to xlisp by Niels Mayer.
 ******************************************************************************/
LVAL Prim_ARRAY_INSERT_POS()
{
  register int i;
  register LVAL *src, *dest;
  LVAL src_array, dest_array, elt, lval_position;
  int src_size, position;

  src_array = xlgavector();	/* get <array> */
  lval_position = xlgafixnum();	/* get <pos>, a fixnum */
  elt = nextarg();		/* get <elt>, which can be any lisp type */
  xllastarg();

  src_size = getsize(src_array);
  position = getfixnum(lval_position);
  if (position >= src_size)
    xlerror("Array insertion position out of bounds.", lval_position);
  dest_array = newvector(src_size + 1);

  src = src_array->n_vdata;
  dest = dest_array->n_vdata;

  if (position < 0) {		/* append <elt> to end of array */
    i = src_size;
    while (i--)
      *dest++ = *src++;
    *dest = elt;
  }
  else {			/* insert <elt> at <position> */
    i = position;
    while (i--)
      *dest++ = *src++;
    *dest++ = elt;
    i = src_size - position;
    while (i--)
      *dest++ = *src++;
  }
  return (dest_array);
}

/******************************************************************************
 * (array-delete-pos <array> <pos>) --> returns the new <array>
 * deletes the element at index <pos> in <array>. If <pos>==-1, then it
 * will delete the last element in the array. 
 * Note that this function is destructive. It reuses the old <array>'s
 * elements.
 *
 * This function was added to xlisp by Niels Mayer.
 ******************************************************************************/
LVAL Prim_ARRAY_DELETE_POS()
{
  register int i;
  register LVAL *src, *dest;
  LVAL src_array, dest_array, lval_position;
  int src_size, position;

  src_array = xlgavector();	/* get <array> */
  lval_position = xlgafixnum();	/* get <pos>, a fixnum */
  xllastarg();

  src_size = getsize(src_array);
  position = getfixnum(lval_position);
  if (position >= src_size)
    xlerror("Array insertion position out of bounds.", lval_position);
  if ((src_size - 1) > 0)
    dest_array = newvector(src_size - 1);
  else
    return (NIL);

  src = src_array->n_vdata;
  dest = dest_array->n_vdata;

  if (position < 0) {		/* remove last element of array */
    i = src_size - 1;
    while (i--)
      *dest++ = *src++;
  }
  else {			/* remove <elt> at <position> */
    i = position;
    while (i--)
      *dest++ = *src++;
    src++;			/* don't copy the deleted elt */
    i = src_size - (position + 1);
    while (i--)
      *dest++ = *src++;
  }
  return (dest_array);
}

/* xerror - special form 'error' */
LVAL xerror()
{
    LVAL emsg,arg;

    /* get the error message and the argument */
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    return (xlerror(getstring(emsg),arg));
}

/* xcerror - special form 'cerror' */
LVAL xcerror()
{
    LVAL cmsg,emsg,arg;

    /* get the correction message, the error message, and the argument */
    cmsg = xlgastring();
    emsg = xlgastring();
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* signal the error */
    xlcerror(getstring(cmsg),getstring(emsg),arg);

    /* return nil */
    return (NIL);
}

/* xbreak - special form 'break' */
LVAL xbreak()
{
    LVAL emsg,arg;

    /* get the error message */
    emsg = (moreargs() ? xlgastring() : NIL);
    arg = (moreargs() ? xlgetarg() : s_unbound);
    xllastarg();

    /* enter the break loop */
    xlbreak((!null(emsg) ? getstring(emsg) : (char FAR *)"**BREAK**"),arg);

    /* return nil */
    return (NIL);
}

/* xcleanup - special form 'clean-up' */
LVAL xcleanup()
{
    xllastarg();
    xlcleanup();
    return (NIL);
}

/* xtoplevel - special form 'top-level' */
LVAL xtoplevel()
{
    xllastarg();
    xltoplevel();
    return (NIL);
}

/* xcontinue - special form 'continue' */
LVAL xcontinue()
{
    xllastarg();
    xlcontinue();
    return (NIL);
}

/* xevalhook - eval hook function */
LVAL xevalhook()
{
    LVAL expr,newehook,newahook,newenv,oldenv,oldfenv,olddenv,val;

    /* protect some pointers */
    xlstkcheck(3);
#if 0	/* old way (see below) */
    xlsave(oldenv);
    xlsave(oldfenv);
    xlsave(newenv);
#else /* TAA MOD -- see below */
    xlprotect(oldenv);
    xlprotect(oldfenv);
    xlprotect(newenv);
#endif

    /* get the expression, the new hook functions and the environment */
    expr = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    newenv = (moreargs() ? xlgalist() : NIL);
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* establish the environment for the hook function */
#if 0	/* old way, if env is NIL then uses current environment */
    if (!null(newenv)) {
	oldenv = xlenv;
	oldfenv = xlfenv;
	xlenv = car(newenv);
	xlfenv = cdr(newenv);
    }
#else	/* TAA MOD -- if env is NIL then uses global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    if (!null(newenv)) {
	xlenv = car(newenv);
	xlfenv = cdr(newenv);
    }
    else {
	xlenv = xlfenv = NIL;
    }
#endif
    /* evaluate the expression (bypassing *evalhook*) */
    val = xlxeval(expr);

    /* restore the old environment */
    xlunbind(olddenv);
#if 0
    if (!null(newenv)) {
	xlenv = oldenv;
	xlfenv = oldfenv;
    }
#else
    xlenv = oldenv;
    xlfenv = oldfenv;
#endif

    /* restore the stack */
    xlpopn(3);

    /* return the result */
    return (val);
}

#ifdef APPLYHOOK
/* xapplyhook - apply hook function */
LVAL xapplyhook()
{
    LVAL fcn,args,newehook,newahook,olddenv,val;

    /* get the function, arguments, and the new hook functions */
    fcn = xlgetarg();
    args = xlgetarg();
    newehook = xlgetarg();
    newahook = xlgetarg();
    xllastarg();

    /* bind *evalhook* and *applyhook* to the hook functions */
    olddenv = xldenv;
    xldbind(s_evalhook,newehook);
    xldbind(s_applyhook,newahook);

    /* apply function (apply always bypasses hooks) */
    val = xlapply(pushargs(fcn,args));

    /* restore the old environment */
    xlunbind(olddenv);

    /* return the result */
    return (val);
}

#endif
