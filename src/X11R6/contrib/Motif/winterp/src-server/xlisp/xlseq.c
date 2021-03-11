/* -*-C-*-
********************************************************************************
*
* File:         xlseq.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlseq.c,v 2.4 1994/06/06 15:59:23 npm Exp $
* Description:  xlisp sequence functions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:50 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlseq.c,v 2.4 1994/06/06 15:59:23 npm Exp $";

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

#ifdef COMMONLISPF

/* external variables */
extern LVAL k_start,k_end,k_1start,k_1end,k_2start,k_2end;
extern LVAL true;

/* this is part of the COMMON LISP extension: */
/* (elt seq index)  -- generic sequence reference function */
/* (map type fcn seq1 [seq2 ...]) -- generic sequence mapping function */
/*   type is one of cons, array, string, or nil */
/* (some fcn seq1 [seq2 ...]) -- apply fcn until non-nil */
/*    also every notany and notevery */
/* (concatenate type seq1 [seq2 ...]) -- sequence concatenation function */
/*    type is one of cons, array, or string. */
/* (position-if pred seq) -- returns position of first match */
/* (search seq1 seq1 &key :test :test-not :start1 :end1 :start2 :end2) --
    generic sequence searching function. */
/* subseq reverse remove remove-if remove-if-not delete delete-if
   delete-if-not -- rewritten to allow all sequence types */
/* find-if count-if -- previous Common Lisp extension, rewritten to allow
   all sequence types */
/* the keyword arguments :start and :end are now valid for the remove, delete,
   find position and count functions */
/* the keyword argument :key is also valid where appropriate */

/* The author, Tom Almy, appologizes for using "goto" several places in
   this code. */

#ifdef ANSI
static unsigned NEAR getlength(LVAL seq)
#else
LOCAL unsigned getlength(seq)
LVAL seq;
#endif
{
    unsigned len;

    if (null(seq)) return 0;

    switch (ntype(seq)) {
	case STRING:
	    return (unsigned)(getslength(seq));
	case VECTOR:
	    return (unsigned)(getsize(seq));
	case CONS:
	    len = 0;
	    while (consp(seq)) {
		len++;
		if (len > MAXSLEN) xltoolong();
		seq = cdr(seq);
	    }
	    return len;
	default:
	    xlbadtype(seq);
	    return (0); /* ha ha */
	}
}


#ifdef ANSI
static void NEAR getseqbounds(unsigned *start, unsigned *end,
		    unsigned length, LVAL startkey, LVAL endkey)
#else
LOCAL VOID getseqbounds(start,end,length,startkey,endkey)
unsigned *start, *end, length;
LVAL startkey, endkey;
#endif
{
    LVAL arg;
    FIXTYPE temp;

    if (xlgkfixnum(startkey,&arg)) {
	temp = getfixnum(arg);
	if (temp < 0 || temp > length ) goto rangeError;
	*start = (unsigned)temp;
    }
    else *start = 0;

    if (xlgetkeyarg(endkey, &arg) && !null(arg)) {
	if (!fixp(arg)) xlbadtype(arg);
	temp = getfixnum(arg);
	if (temp < *start  || temp > length) goto rangeError;
	*end = (unsigned)temp;
    }
    else *end = length;

    return;
    /* else there is a range error */

rangeError:
    xlerror("range error",arg);
}


/* xelt - sequence reference function */
LVAL xelt()
{
    LVAL seq,index;
    FIXTYPE i;

    /* get the sequence and the index */

    seq = xlgetarg();

    index = xlgafixnum(); i = getfixnum(index);
    if (i < 0) goto badindex;

    xllastarg();

    if (listp(seq)) { /* do like nth, but check for in range */
	/* find the ith element */
	while (consp(seq)) {
	    if (i-- == 0) return (car(seq));
	    seq = cdr(seq);
	}
	goto badindex;	/* end of list reached first */
    }


    if (ntype(seq) == STRING) {
	if (i >= getslength(seq)) goto badindex;
	return (cvchar(getstringch(seq,(int)i)));
    }

    if (ntype(seq)!=VECTOR) xlbadtype(seq); /* type must be array */

    /* range check the index */
    if (i >= getsize(seq)) goto badindex;

    /* return the array element */
    return (getelement(seq,(int)i));

badindex:
    xlerror("index out of bounds",index);
    return (NIL);   /* eliminate warnings */
}

#ifdef MAPFCNS
/* xmap -- map function */

LVAL xmap()
{
    FRAMEP newfp;
    LVAL fun, lists, val, last, x, y;
    unsigned len,temp, i;
    int argc, typ;

    /* protect some pointers */
    xlstkcheck(3);
    xlsave(fun);
    xlsave(lists);
    xlsave(val);

    /* get the type of resultant */
    if (null((last = xlgetarg()))) {	/* nothing is returned */
	typ = 0;
    }
    else if ((typ = xlcvttype(last)) != CONS &&
		typ != STRING && typ != VECTOR) {
	xlerror("invalid result type", last);
    }

    /* get the function to apply and argument sequences */
    fun = xlgetarg();
    val = NIL;
    lists = xlgetarg();
    len = getlength(lists);
    argc = 1;

    /* build a list of argument lists */
    for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
	val = xlgetarg();
	if ((temp = getlength(val)) < len) len = temp;
	argc++;
	rplacd(last,(cons(val,NIL)));
    }

    /* initialize the result list */
    switch (typ) {
	case VECTOR:
	    val = newvector(len);
	    break;
	case STRING:
	    val = newstring(len);
	    val->n_string[len] = 0;
	    break;
	default:
	    val = NIL;
	    break;
    }


    /* loop through each of the argument lists */
    for (i=0;i<len;i++) {

	/* build an argument list from the sublists */
	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(fun);
	pusharg(cvfixnum((FIXTYPE)argc));
	for (x = lists; !null(x) ; x = cdr(x)) {
	    y = car(x);
	    switch (ntype(y)) {
		case CONS:
		    pusharg(car(y));
		    rplaca(x,cdr(y));
		    break;
		case VECTOR:
		    pusharg(getelement(y,i));
		    break;
		case STRING:
		    pusharg(cvchar(getstringch(y,i)));
		    break;
	    }
	}

	/* apply the function to the arguments */
	xlfp = newfp;
	x = xlapply(argc);

	switch (typ) {
	    case CONS:
		y = consa(x);
		if (!null(val)) rplacd(last,y);
		else val = y;
		last = y;
		break;
	    case VECTOR:
		setelement(val,i,x);
		break;
	    case STRING:
		if (!charp(x))
		    xlerror("map function returned non-character",x);
		val->n_string[i] = getchcode(x);
		break;
	}

    }

    /* restore the stack */
    xlpopn(3);

    /* return the last test expression value */
    return (val);
    }

/* every, some, notany, notevery */

#define EVERY 0
#define SOME 1
#define NOTEVERY 2
#define NOTANY 3

#ifdef ANSI
static LVAL NEAR xlmapwhile(int cond)
#else
LOCAL LVAL xlmapwhile(cond)
int cond;
#endif
{
    int exitcond;
    FRAMEP newfp;
    LVAL fun, lists, val, last, x, y;
    unsigned len,temp,i;
    int argc;

    /* protect some pointers */
    xlstkcheck(2);
    xlsave(fun);
    xlsave(lists);

    /* get the function to apply and argument sequences */
    fun = xlgetarg();
    lists = xlgetarg();
    len = getlength(lists);
    argc = 1;

    /* build a list of argument lists */
    for (lists = last = consa(lists); moreargs(); last = cdr(last)) {
	val = xlgetarg();
	if ((temp = getlength(val)) < len) len = temp;
	argc++;
	rplacd(last,(cons(val,NIL)));
    }

    switch (cond) {
	case SOME:
	case NOTANY:
	    exitcond = TRUE;
	    val = NIL;
	    break;
	case EVERY:
	case NOTEVERY:
	    exitcond = FALSE;
	    val = true;
	    break;
    }


    /* loop through each of the argument lists */
    for (i=0;i<len;i++) {

	/* build an argument list from the sublists */
	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(fun);
	pusharg(cvfixnum((FIXTYPE)argc));
	for (x = lists; !null(x); x = cdr(x)) {
	    y = car(x);
	    switch (ntype(y)) {
		case CONS:
		    pusharg(car(y));
		    rplaca(x,cdr(y));
		    break;
		case VECTOR:
		    pusharg(getelement(y,i));
		    break;
		case STRING:
		    pusharg(cvchar(getstringch(y,i)));
		    break;
	    }
	}

	/* apply the function to the arguments */
	xlfp = newfp;
	val = xlapply(argc);
	if (null(val) ^ exitcond) break;
    }

    if ((cond == NOTANY) | (cond == NOTEVERY))
	val = (null(val) ? true : NIL);


    /* restore the stack */
    xlpopn(2);

    /* return the last test expression value */
    return (val);
    }


LVAL xevery()
{
    return xlmapwhile(EVERY);
}

LVAL xsome()
{
    return xlmapwhile(SOME);
}

LVAL xnotany()
{
    return xlmapwhile(NOTANY);
}

LVAL xnotevery()
{
    return xlmapwhile(NOTEVERY);
}
#endif

/* xconcatenate - concatenate a bunch of sequences */
/* replaces (and extends) strcat, now a macro */
LOCAL unsigned NEAR calclength(VOIDP)
{
    LVAL tmp;
    FRAMEP saveargv;
    int saveargc;
    long len;

    /* save the argument list */
    saveargv = xlargv;
    saveargc = xlargc;

    /* find the length of the new string or vector */
    for (len = 0; moreargs(); ) {
	tmp = xlgetarg();
	len += getlength(tmp);

	if (len>MAXSLEN) xltoolong();  /*check for overflow*/
    }

    /* restore the argument list */
    xlargv = saveargv;
    xlargc = saveargc;

    return (unsigned)len;
}


LOCAL LVAL NEAR cattostring(VOIDP)
{
    LVAL tmp,temp,val;
    char FAR *str;
    unsigned len,i;

    /* find resulting length -- also validates argument types */
    len = calclength();

    /* create the result string */
    val = newstring(len);
    str = getstring(val);

    /* combine the strings */
    while (moreargs()) {
	tmp = nextarg();
	if (!null(tmp)) switch (ntype(tmp)) {
	    case STRING:
		len = getslength(tmp);
		MEMCPY(str, getstring(tmp), len);
		str += len;
		break;
	    case VECTOR:
		len = getsize(tmp);
		for (i = 0; i < len; i++) {
		    temp = getelement(tmp,i);
		    if (!charp(temp)) goto failed;
		    *str++ = getchcode(temp);
		}
		break;
	    case CONS:
		while (consp(tmp)) {
		    temp = car(tmp);
		    if (!charp(temp)) goto failed;
		    *str++ = getchcode(temp);
		    tmp = cdr(tmp);
		}
		break;
	}
    }

    *str = 0;	/* delimit string */

    /* return the new string */
    return (val);

failed:
    xlerror("can't make into string", tmp);
    return (NIL);   /* avoid warning message */
}


LOCAL LVAL NEAR cattovector(VOIDP)
{
    LVAL tmp,val;
    LVAL FAR *vect;
    unsigned len,i;

    /* find resulting length -- also validates argument types */
    len = calclength();

    /* create the result vector */
    val = newvector(len);
    vect = &val->n_vdata[0];

    /* combine the vectors */
    while (moreargs()) {
	tmp = nextarg();
	if (!null(tmp)) switch (ntype(tmp)) {
	    case VECTOR:
		len = getsize(tmp);
		MEMCPY(vect, &getelement(tmp,0), len*sizeof(LVAL));
		vect += len;
		break;
	    case STRING:
		len = getslength(tmp);
		for (i = 0; i < len; i++) {
		    *vect++ = cvchar(getstringch(tmp,i));
		}
		break;
	    case CONS:
		while (consp(tmp)) {
		    *vect++ = car(tmp);
		    tmp = cdr(tmp);
		}
		break;
	}
    }

    /* return the new vector */
    return (val);
}


LOCAL LVAL NEAR cattocons(VOIDP)
{
    LVAL val,tmp,next,last=NIL;
    unsigned len,i;

    xlsave1(val);	/* protect against GC */

    /* combine the lists */
    while (moreargs()) {
	tmp = nextarg();
	if (!null(tmp)) switch (ntype(tmp)) {
	    case CONS:
		while (consp(tmp)) {
		    next = consa(car(tmp));
		    if (!null(val)) rplacd(last,next);
		    else val = next;
		    last = next;
		    tmp = cdr(tmp);
		}
		break;
	    case VECTOR:
		len = getsize(tmp);
		for (i = 0; i<len; i++) {
		    next = consa(getelement(tmp,i));
		    if (!null(val)) rplacd(last,next);
		    else val = next;
		    last = next;
		}
		break;
	    case STRING:
		len = getslength(tmp);
		for (i = 0; i < len; i++) {
		    next = consa(cvchar(getstringch(tmp,i)));
		    if (!null(val)) rplacd(last,next);
		    else val = next;
		    last = next;
		}
		break;
	    default:
		xlbadtype(tmp); break; /* need default because no precheck*/
	}
    }

    xlpop();

    return (val);

}


LVAL xconcatenate()
{
    LVAL tmp;

    switch (xlcvttype(tmp = xlgetarg())) {  /* target type of data */
	case CONS:	return cattocons();
	case STRING:	return cattostring();
	case VECTOR:	return cattovector();
	default:	xlerror("invalid result type", tmp);
			return (NIL);	/* avoid warning */
    }
}

/* xsubseq - return a subsequence -- new version */

LVAL xsubseq()
{
    unsigned start,end,len;
    FIXTYPE temp;
    int srctype;
    LVAL src,dst;
    LVAL next,last=NIL;

    /* get sequence */
    src = xlgetarg();
    if (listp(src)) srctype = CONS;
    else srctype=ntype(src);


    /* get length */
    switch (srctype) {
	case STRING:
	    len = getslength(src);
	    break;
	case VECTOR:
	    len = getsize(src);
	    break;
	case CONS:	/* BADLY INEFFICIENT! */
	    dst = src;	/* use dst as temporary */
	    len = 0;
	    while (consp(dst)) {
		dst = cdr(dst);
		len++;
		if (len > MAXSLEN) xltoolong();
	    }
	    break;
	default:
	    xlbadtype(src);
    }

    /* get the starting position */
    dst = xlgafixnum(); temp = getfixnum(dst);
    if (temp < 0 || temp > len)
	xlerror("sequence index out of bounds",dst);
    start = (unsigned) temp;

    /* get the ending position */
    if (moreargs()) {
	dst = nextarg();
	if (null(dst)) end = len;
	else if (fixp(dst)) {
	    temp = getfixnum(dst);
	    if (temp < start || temp > len)
		xlerror("sequence index out of bounds",dst);
	    end = (unsigned) temp;
	}
	else xlbadtype(dst);
    }
    else
	end = len;
    xllastarg();

    len = end - start;

    switch (srctype) {	/* do the subsequencing */
	case STRING:
	    dst = newstring(len);
	    MEMCPY(getstring(dst), getstring(src)+start, len);
	    dst->n_string[len] = 0;
	    break;
	case VECTOR:
	    dst = newvector(len);
	    MEMCPY(dst->n_vdata, &src->n_vdata[start], sizeof(LVAL)*len);
	    break;
	case CONS:
	    xlsave1(dst);
	    while (start--) src = cdr(src);
	    while (len--) {
		next = consa(car(src));
		if (!null(dst)) rplacd(last,next);
		else dst = next;
		last = next;
		src = cdr(src);
	    }
	    xlpop();
	    break;
    }

    /* return the substring */
    return (dst);
}


/* xnreverse -- built-in function nreverse (destructive reverse) */
LVAL xnreverse()
{
    LVAL seq,val,next;
    unsigned int i,j;
    int ival;

    /* get the sequence to reverse */
    seq = xlgetarg();
    xllastarg();

    if (null(seq)) return (NIL);    /* empty argument */

    switch (ntype(seq)) {
	case CONS:
	    val = NIL;
	    while (consp(seq)) {
		next = cdr(seq);
		rplacd(seq,val);
		val = seq;
		seq = next;
	    }
	    break;
	case VECTOR:
	    if (getsize(seq) > 1)
		for (i = 0, j = getsize(seq)-1; i < j; i++, j--) {
		    val = getelement(seq,i);
		    setelement(seq,i,getelement(seq,j));
		    setelement(seq,j,val);
		}
	    return seq;
	case STRING:
	    if (getslength(seq) > 2)
		for (i = 0, j=getslength(seq)-1 ; i < j; i++, j--) {
		    ival = seq->n_string[i];
		    seq->n_string[i] = seq->n_string[j];
		    seq->n_string[j] = ival;
		}
	    return seq;
	default:
	    xlbadtype(seq); break;
    }

    /* return the sequence */
    return (val);
}

/* xreverse - built-in function reverse -- new version */
LVAL xreverse()
{
    LVAL seq,val;
    unsigned i,len;

    /* get the sequence to reverse */
    seq = xlgetarg();
    xllastarg();

    if (null(seq)) return (NIL);    /* empty argument */

    switch (ntype(seq)) {
	case CONS:
	    /* protect pointer */
	    xlsave1(val);

	    /* append each element to the head of the result list */
	    for (val = NIL; consp(seq); seq = cdr(seq))
		val = cons(car(seq),val);

	    /* restore the stack */
	    xlpop();
	    break;
	case VECTOR:
	    len = getsize(seq);
	    val = newvector(len);
	    for (i = 0; i < len; i++)
		setelement(val,i,getelement(seq,len-i-1));
	    break;
	case STRING:
	    len = getslength(seq);
	    val = newstring(len);
	    for (i = 0; i < len; i++)
		val->n_string[i] = seq->n_string[len-i-1];
	    val->n_string[len] = 0;
	    break;
	default:
	    xlbadtype(seq); break;
    }

    /* return the sequence */
    return (val);
}


/* remif - common code for 'remove', 'remove-if', and 'remove-if-not' */
#ifdef ANSI
static LVAL NEAR remif(int tresult, int expr)
#else
LOCAL LVAL remif(tresult,expr)
  int tresult,expr;
#endif
{
    LVAL x,seq,fcn,val,next;
    LVAL last=NULL;
    unsigned i,j,l;
    unsigned start,end;
    long s;

#ifdef KEYARG
    LVAL kfcn;
#endif


    if (expr) {
	/* get the expression to remove and the sequence */
	x = xlgetarg();
	seq = xlgetarg();
	xltest(&fcn,&tresult);
    }
    else {
	/* get the function and the sequence */
	fcn = xlgetarg();
	seq = xlgetarg();
    }

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef KEYARG
    kfcn=xlkey();
#endif

    xllastarg();

    if (null(seq)) return NIL;

    /* protect some pointers */

#ifdef KEYARG
    xlstkcheck(3);
    xlprotect(kfcn);
#else
    xlstkcheck(2);
#endif
    xlprotect(fcn);
    xlsave(val);

    /* remove matches */

    switch (ntype(seq)) {
	case CONS:
	    for (s=start; end-- > 0; seq = cdr(seq)) {
			/* check to see if this element should be deleted */

#ifdef KEYARG
		if (s-- > 0 ||
		    (expr?dotest2(x,car(seq),fcn,kfcn)
		    :dotest1(car(seq),fcn,kfcn)) != tresult)
#else
		if (s-- > 0 ||
		    (expr?dotest2(x,car(seq),fcn)
		    :dotest1(car(seq),fcn)) != tresult)
#endif
		{
		    next = consa(car(seq));
		    if (!null(val)) rplacd(last,next);
		    else val = next;
		    last = next;
		}
	    }
	    /* copy to end */
	    while (consp(seq)) {
		next = consa(car(seq));
		if (!null(val)) rplacd(last,next);
		else val = next;
		last = next;
		seq = cdr(seq);
	    }
	    break;
	case VECTOR:
	    val = newvector(l=getsize(seq));
	    for (i=j=0; i < l; i++) {
#ifdef KEYARG
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
		    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,getelement(seq,i),fcn)
		    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
		{
		    setelement(val,j++,getelement(seq,i));
		}
	    }
	    if (l != j) { /* need new, shorter result -- too bad */
		fcn = val; /* save value in protected cell */
		val = newvector(j);
		MEMCPY(val->n_vdata, fcn->n_vdata, j*sizeof(LVAL));
	    }
	    break;
	case STRING:
	    l = getslength(seq);
	    val = newstring(l);
	    for (i=j=0; i < l; i++) {
#ifdef KEYARG
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
		    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
		    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
		{
		    val->n_string[j++] = seq->n_string[i];
		}
	    }
	    if (l != j) { /* need new, shorter result -- too bad */
		fcn = val; /* save value in protected cell */
		val = newstring(j);
		MEMCPY(val->n_string, fcn->n_string, j*sizeof(char));
		val->n_string[j] = 0;
	    }
	    break;
	default:
	    xlbadtype(seq); break;
    }


    /* restore the stack */
#ifdef KEYARG
    xlpopn(3);
#else
    xlpopn(2);
#endif

    /* return the updated sequence */
    return (val);
}

/* xremif - built-in function 'remove-if' -- enhanced version */
LVAL xremif()
{
    return (remif(TRUE,FALSE));
}

/* xremifnot - built-in function 'remove-if-not' -- enhanced version */
LVAL xremifnot()
{
    return (remif(FALSE,FALSE));
}

/* xremove - built-in function 'remove' -- enhanced version */

LVAL xremove()
{
    return (remif(TRUE,TRUE));
}


/* delif - common code for 'delete', 'delete-if', and 'delete-if-not' */
#ifdef ANSI
static LVAL NEAR delif(int tresult, int expr)
#else
LOCAL LVAL delif(tresult,expr)
  int tresult,expr;
#endif
{
    LVAL x,seq,fcn,last,val;
    unsigned i,j,l;
    unsigned start,end;

#ifdef KEYARG
    LVAL kfcn;
#endif


    if (expr) {
	/* get the expression to delete and the sequence */
	x = xlgetarg();
	seq = xlgetarg();
	xltest(&fcn,&tresult);
    }
    else {
	/* get the function and the sequence */
	fcn = xlgetarg();
	seq = xlgetarg();
    }

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);


#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    if (null(seq)) return NIL;

    /* protect a pointer */

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    /* delete matches */

    switch (ntype(seq)) {
	case CONS:
	    end -= start; /* gives length */
	    /* delete leading matches, only if start is 0 */
	    if (start == 0)
		while (consp(seq) && end > 0) {
		    end--;
#ifdef KEYARG
		    if ((expr?dotest2(x,car(seq),fcn,kfcn)
			:dotest1(car(seq),fcn,kfcn)) != tresult)
#else
		    if ((expr?dotest2(x,car(seq),fcn)
			:dotest1(car(seq),fcn)) != tresult)
#endif
			    break;
		    seq = cdr(seq);
		}

	    val = last = seq;

	    /* delete embedded matches */
	    if (consp(seq) && end > 0) {

		/* skip the first non-matching element, start == 0 */
		if (start == 0) seq = cdr(seq);

		/* skip first elements if start > 0, correct "last" */
		for (;consp(seq) && start-- > 0;last=seq, seq=cdr(seq)) ;

		/* look for embedded matches */
		while (consp(seq) && end-- > 0) {

		    /* check to see if this element should be deleted */
#ifdef KEYARG
		    if ((expr?dotest2(x,car(seq),fcn,kfcn)
			:dotest1(car(seq),fcn,kfcn)) == tresult)
#else
		    if ((expr?dotest2(x,car(seq),fcn)
			:dotest1(car(seq),fcn)) == tresult)
#endif
			rplacd(last,cdr(seq));
		    else
			last = seq;

		    /* move to the next element */
		    seq = cdr(seq);
		}
	    }
	    break;
	case VECTOR:
	    l = getsize(seq);
	    for (i=j=0; i < l; i++) {
#ifdef KEYARG
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,getelement(seq,i),fcn,kfcn)
		    :dotest1(getelement(seq,i),fcn,kfcn)) != tresult)
#else
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,getelement(seq,i),fcn)
		    :dotest1(getelement(seq,i),fcn)) != tresult)
#endif
	    {
		    if (i != j) setelement(seq,j,getelement(seq,i));
		    j++;
		}
	    }
	    if (l != j) { /* need new, shorter result -- too bad */
		fcn = seq; /* save value in protected cell */
		seq = newvector(j);
		MEMCPY(seq->n_vdata, fcn->n_vdata, j*sizeof(LVAL));
	    }
	    val = seq;
	    break;
	case STRING:
	    l = getslength(seq);
	    for (i=j=0; i < l; i++) {
#ifdef KEYARG
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn,kfcn)
		    :dotest1(cvchar(getstringch(seq,i)),fcn,kfcn))!=tresult)
#else
		if (i < start || i >= end ||	/* copy if out of range */
		    (expr?dotest2(x,cvchar(getstringch(seq,i)),fcn)
		    :dotest1(cvchar(getstringch(seq,i)),fcn)) != tresult)
#endif
		{
		    if (i != j) seq->n_string[j] = seq->n_string[i];
		    j++;
		}
	    }
	    if (l != j) { /* need new, shorter result -- too bad */
		fcn = seq; /* save value in protected cell */
		seq = newstring(j);
		MEMCPY(seq->n_string, fcn->n_string, j*sizeof(char));
		seq->n_string[j] = 0;
	    }
	    val = seq;
	    break;
	default:
	    xlbadtype(seq); break;
    }


    /* restore the stack */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    /* return the updated sequence */
    return (val);
}

/* xdelif - built-in function 'delete-if' -- enhanced version */
LVAL xdelif()
{
    return (delif(TRUE,FALSE));
}

/* xdelifnot - built-in function 'delete-if-not' -- enhanced version */
LVAL xdelifnot()
{
    return (delif(FALSE,FALSE));
}

/* xdelete - built-in function 'delete' -- enhanced version */

LVAL xdelete()
{
    return (delif(TRUE,TRUE));
}

#ifdef POSFCNS
/* xcountif - built-in function 'count-if     TAA MOD addition */
LVAL xcountif()
{
    unsigned counter=0;
    unsigned start,end;
    LVAL seq, fcn;

#ifdef KEYARG
    LVAL kfcn;
#endif


    /* get the arguments */
    fcn = xlgetarg();
    seq = xlgetarg();

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    if (null(seq)) return (cvfixnum((FIXTYPE)0));

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    /* examine arg and count */
    switch (ntype(seq)) {
	case CONS:
	    end -= start;
	    for (; consp(seq) && start-- > 0; seq = cdr(seq)) ;
	    for (; end-- > 0; seq = cdr(seq))
#ifdef KEYARG
		if (dotest1(car(seq),fcn,kfcn)) counter++;
#else
		if (dotest1(car(seq),fcn)) counter++;
#endif
	    break;
	case VECTOR:
	    for (; start < end; start++)
#ifdef KEYARG
		if (dotest1(getelement(seq,start),fcn,kfcn)) counter++;
#else
		if (dotest1(getelement(seq,start),fcn)) counter++;
#endif
	    break;
	case STRING:
	    for (; start < end; start++)
#ifdef KEYARG
		if(dotest1(cvchar(getstringch(seq,start)),fcn,kfcn))counter++;
#else
		if (dotest1(cvchar(getstringch(seq,start)),fcn)) counter++;
#endif
	    break;
	default:
	    xlbadtype(seq); break;
    }

#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (cvfixnum((FIXTYPE)counter));
}

/* xfindif - built-in function 'find-if'    TAA MOD */
LVAL xfindif()
{
    LVAL seq, fcn, val;
    unsigned start,end;


#ifdef KEYARG
    LVAL kfcn;
#endif

    fcn = xlgetarg();
    seq = xlgetarg();

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);


#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    if (null(seq)) return NIL;

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    switch (ntype(seq)) {
	case CONS:
	    end -= start;
	    for (; consp(seq) && start-- > 0; seq = cdr(seq)) ;
	    for (; end-- > 0; seq = cdr(seq)) {
#ifdef KEYARG
		if (dotest1(val=car(seq), fcn, kfcn)) goto fin;
#else
		if (dotest1(val=car(seq), fcn)) goto fin;
#endif
	    }
	    break;
	case VECTOR:
	    for (; start < end; start++)
#ifdef KEYARG
		if (dotest1(val=getelement(seq,start),fcn,kfcn)) goto fin;
#else
		if (dotest1(val=getelement(seq,start),fcn)) goto fin;
#endif
	    break;
	case STRING:
	    for (; start < end; start++)
#ifdef KEYARG
		if (dotest1(val=cvchar(getstringch(seq,start)),fcn,kfcn))
		    goto fin;
#else
		if (dotest1(val=cvchar(getstringch(seq,start)),fcn)) goto fin;
#endif
	    break;
	default:
	    xlbadtype(seq); break;
    }

    val = NIL;	/* not found */

fin:
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif
    return (val);
}

/* xpositionif - built-in function 'position-if'    TAA MOD */
LVAL xpositionif()
{
    LVAL seq, fcn;
    unsigned start,end,count;

#ifdef KEYARG
    LVAL kfcn;
#endif

    fcn = xlgetarg();
    seq = xlgetarg();

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    if (null(seq)) return NIL;

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    count = start;

    switch (ntype(seq)) {
	case CONS:
	    end -= count;
	    for (; consp(seq) && start-- > 0; seq = cdr(seq)) ;
	    for (; end-- > 0; seq = cdr(seq)) {
#ifdef KEYARG
		if (dotest1(car(seq), fcn, kfcn)) goto fin;
#else
		if (dotest1(car(seq), fcn)) goto fin;
#endif
		count++;
	    }
	    break;
	case VECTOR:
	    for (; count < end; count++)
#ifdef KEYARG
		if (dotest1(getelement(seq,count),fcn,kfcn)) goto fin;
#else
		if (dotest1(getelement(seq,count),fcn)) goto fin;
#endif
	    break;
	case STRING:
	    for (; count < end; count++)
#ifdef KEYARG
		if (dotest1(cvchar(getstringch(seq,count)),fcn,kfcn))goto fin;
#else
		if (dotest1(cvchar(getstringch(seq,count)),fcn)) goto fin;
#endif
	    break;
	default:
	    xlbadtype(seq); break;
    }

		/* not found */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif
    return(NIL);

fin:		/* found */
#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (cvfixnum((FIXTYPE)count));
}
#endif

#ifdef SRCHFCN
/* xsearch -- search function */

LVAL xsearch()
{
    LVAL seq1, seq2, fcn, temp1, temp2;
    unsigned start1, start2, end1, end2, len1, len2;
    unsigned i,j;
    int tresult,typ1, typ2;
#ifdef KEYARG
    LVAL kfcn;
#endif

    /* get the sequences */
    seq1 = xlgetarg();
    len1 = getlength(seq1);
    seq2 = xlgetarg();
    len2 = getlength(seq2);

    /* test/test-not args? */
    xltest(&fcn,&tresult);

    /* check for start/end keys */
    getseqbounds(&start1,&end1,len1,k_1start,k_1end);
    getseqbounds(&start2,&end2,len2,k_2start,k_2end);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    /* calculate the true final search string location that needs to
	be checked (end2) */

    if (end2 - start2 < end1 - start1	    /* nothing to compare */
	|| end2 - start2 == 0)
	    return (NIL);

    len1 = end1 - start1;   /* calc lengths of sequences to test */
    end2 -= len1;	    /* we don't need to compare with start loc
				beyond this value */

    typ1 = ntype(seq1);
    typ2 = ntype(seq2);

#ifdef KEYARG
    xlstkcheck(2);
    xlprotect(kfcn);
#else
    xlstkcheck(1);
#endif
    xlprotect(fcn);

    if (typ1 == CONS) { /* skip leading section of sequence 1 if a cons */
	j = start1;
	while (j--) seq1 = cdr(seq1);
    }

    if (typ2 == CONS) { /* second string is cons */
	i = start2;	/* skip leading section of string 2 */
	while (start2--) seq2 = cdr(seq2);

	for (;i<=end2;i++) {
	    temp2 = seq2;
	    if (typ1 == CONS) {
		temp1 = seq1;
		for (j = start1; j < end1; j++) {
#ifdef KEYARG
		    if (dotest2s(car(temp1),car(temp2),fcn,kfcn) != tresult)
			goto next1;
#else
		    if (dotest2(car(temp1),car(temp2),fcn) != tresult)
			goto next1;
#endif
		    temp1 = cdr(temp1);
		    temp2 = cdr(temp2);
		}
	    }
	    else {
		for (j = start1; j < end1; j++) {
#ifdef KEYARG
		    if (dotest2s(typ1 == VECTOR ? getelement(seq1,j) :
			cvchar(getstringch(seq1,j)), car(temp2), fcn, kfcn)
			!=tresult)
#else
		    if (dotest2(typ1 == VECTOR ? getelement(seq1,j) :
		       cvchar(getstringch(seq1,j)), car(temp2), fcn)!=tresult)
#endif
			goto next1;
		    temp2 = cdr(temp2);
		}
	    }
#ifdef KEYARG
	    xlpopn(2);
#else
	    xlpop();
#endif
	    return cvfixnum(i);
	    next1: /* continue */
	    seq2 = cdr(seq2);
	}
    }

    else for (i = start2; i <= end2 ; i++) { /* second string is array/string */
	if (typ1 == CONS) {
	    temp1 = seq1;
	    for (j = 0; j < len1; j++) {
#ifdef KEYARG
		if (dotest2s(car(temp1),
			    typ2 == VECTOR ? getelement(seq2,i+j)
					   : cvchar(getstringch(seq2,i+j)),
			    fcn,kfcn) != tresult)
#else
		if (dotest2(car(temp1),
			    typ2 == VECTOR ? getelement(seq2,i+j)
					   : cvchar(getstringch(seq2,i+j)),
			    fcn) != tresult)
#endif
		    goto next2;
		temp1 = cdr(temp1);
	    }
	}
	else for (j=start1; j < end1; j++) {
#ifdef KEYARG
	    if (dotest2s(typ1 == VECTOR ?
		getelement(seq1,j) :
		cvchar(getstringch(seq1,j)),
		typ2 == VECTOR ?
		getelement(seq2,i+j-start1) :
		cvchar(getstringch(seq2,i+j-start1)), fcn, kfcn)
		!= tresult)
#else
	    if (dotest2(typ1 == VECTOR ?
		getelement(seq1,j) :
		cvchar(getstringch(seq1,j)),
		typ2 == VECTOR ?
		getelement(seq2,i+j-start1) :
		cvchar(getstringch(seq2,i+j-start1)), fcn)
		!= tresult)
#endif
		    goto next2;
	}
#ifdef KEYARG
	xlpopn(2);
#else
	xlpop();
#endif
	return cvfixnum(i);
	next2:; /* continue */
    }

#ifdef KEYARG
    xlpopn(2);
#else
    xlpop();
#endif

    return (NIL);   /*no match*/

}
#endif

#ifdef TIERNEY
extern LVAL k_ivalue;

/* The following is based on code with the following copyright message: */
/* XLISP-STAT 2.0 Copyright (c) 1988, by Luke Tierney		       */

/* Extended by Tom Almy to put in a single C function, allow :start and
   :end keywords, correctly  handle case of null(seq), and case where
   sequence is a string */

/* Common Lisp REDUCE function */
LVAL xreduce()
{
    LVAL fcn, seq, initial_value;
    LVAL next, args, result;
    int has_init;
    unsigned start, end;

    fcn = xlgetarg();
    seq = xlgetarg();
    has_init = xlgetkeyarg(k_ivalue, &initial_value);
    getseqbounds(&start, &end, getlength(seq), k_start, k_end);
    xllastarg();

    /* protect some pointers */
    xlstkcheck(4);
    xlsave(next);
    xlsave(args);
    xlsave(result);
    xlprotect(fcn);

    args = cons(NIL, cons(NIL,NIL));

    if (null(seq) || start==end) {
	result = has_init ? initial_value : xlapply(pushargs(fcn, NIL));
    }
    else switch (ntype(seq)) {
	case CONS:
	    end -= start;
	    while (start-- > 0) seq = cdr(seq); /* skip to start */
	    next = seq;
	    if (has_init) result = initial_value;
	    else {
		result = car(next);
		next = cdr(next);
		end--;
	    }
	    for (; end-- > 0; next = cdr(next)) {
		rplaca(args, result);
		rplaca(cdr(args), car(next));
		result = xlapply(pushargs(fcn, args));
	    }
	    break;
	case VECTOR:
	    if (has_init)
		result = initial_value;
	    else {
		result = getelement(seq, start);
		start++;
	    }
	    for (; start < end; start++) {
		rplaca(args, result);
		rplaca(cdr(args), getelement(seq, start));
		result = xlapply(pushargs(fcn, args));
	    }
	    break;
	case STRING:	/* for completeness, darned if I can think of a use */
	    if (has_init)
		result = initial_value;
	    else {
		result = cvchar(getstringch(seq, start));
		start++;
	    }
	    for (; start < end; start++) {
		rplaca(args, result);
		rplaca(cdr(args), cvchar(getstringch(seq, start)));
		result = xlapply(pushargs(fcn, args));
	    }
	    break;
	default:
	    xlbadtype(seq);
	}

    /* restore the stack frame */
    xlpopn(4);

    return(result);
}


#endif

#ifdef REMDUPS

/* Common Lisp REMOVE-DUPLICATES function */
/* by Tom Almy */
/* unlike xllist.c version, this one works on all sequences and
   allows the :start and :end keywords. */

LVAL xremove_duplicates()
{
    LVAL seq,fcn,val,next,tmp;
    LVAL last=NULL;
    unsigned i,j,l,k;
    unsigned start,end;
    int tresult;

#ifdef KEYARG
    LVAL kfcn,item;
#endif

    /* get the sequence */
    seq = xlgetarg();

    /* get any optional args */
    xltest(&fcn,&tresult);

    getseqbounds(&start,&end,getlength(seq),k_start,k_end);

#ifdef KEYARG
    kfcn = xlkey();
#endif

    xllastarg();

    if (null(seq)) return NIL;

    /* protect some pointers */
#ifdef KEYARG
    xlstkcheck(4);
    xlprotect(kfcn);
    xlsave(item);
#else
    xlstkcheck(2);
#endif
    xlprotect(fcn);
    xlsave(val);

    /* remove matches */

    switch (ntype(seq)) {
	case CONS:
	    end -= start;   /* length of valid subsequence */
	    while (start-- > 0) {   /* copy leading part intact */
		next = consa(car(seq));
		if (!null(val)) rplacd(last,next);
		else val=next;
		last= next;
	    }

	    for (; end-- > 1; seq = cdr(seq)) {
		/* check to see if this element should be deleted */
#ifdef KEYARG
		item = car(seq);
		if (!null(kfcn)) item = xlapp1(kfcn,item);
		for (l=end,tmp=cdr(seq); l-- >0; tmp = cdr(tmp))
		    if (dotest2(item,car(tmp),fcn,kfcn)==tresult)
			goto cons_noxfer;
#else
		for (l=end,tmp=cdr(seq); l-- >0; tmp = cdr(tmp))
		    if (dotest2(car(seq),car(tmp),fcn)==tresult)
			goto cons_noxfer;
#endif
		next = consa(car(seq));
		if (!null(val)) rplacd(last,next);
		else val = next;
		last = next;
		cons_noxfer:;
	    }
	    /* now copy to end */
	    while (consp(seq)) {
		next = consa(car(seq));
		if (!null(val)) rplacd(last,next);
		else val = next;
		last = next;
		seq = cdr(seq);
	    }
	    break;
	case VECTOR:
	    val = newvector(l=getsize(seq));

	    if (start>0)
		MEMCPY(&getelement(val,0),&getelement(seq,0),start*sizeof(LVAL));

	    for (i=j=0; i < start; i++) setelement(val,j++,getelement(seq,i));

	    for (i=j=start; i < end; i++) {
#ifdef KEYARG
		item = getelement(seq,i);
		if (!null(kfcn)) item = xlapp1(kfcn,item);
		for (k=i+1; k<end; k++)
		    if (dotest2(item,getelement(seq,k),fcn,kfcn)==tresult)
			goto vector_noxfer;
#else
		for (k=i+1; k<end; k++)
		    if (dotest2(getelement(seq,i),getelement(seq,k),fcn)==tresult)
			goto vector_noxfer;
#endif
		setelement(val,j++,getelement(seq,i));
		vector_noxfer:;
	    }

	    if (l-end > 0)
		MEMCPY(&getelement(val,end),
		       &getelement(seq,end),
		       (l-end)*sizeof(LVAL));

	    if (l != j) { /* need new, shorter result -- too bad */
		fcn = val; /* save value in protected cell */
		val = newvector(j);
		MEMCPY(val->n_vdata, fcn->n_vdata, j*sizeof(LVAL));
	    }
	    break;
	case STRING:
	    l = getslength(seq);
	    val = newstring(l);

	    MEMCPY(&val->n_string,&seq->n_string,start*sizeof(char));

	    for (i=j=start; i < end; i++) {
#ifdef KEYARG
		item = cvchar(getstringch(seq,i));
		if (!null(kfcn)) item = xlapp1(kfcn,item);
		for (k=i+1; k<end; k++)
		    if (dotest2(item,cvchar(getstringch(seq,k)),fcn,kfcn)==tresult)
			goto string_noxfer;
#else
		tmp = cvchar(getstringch(seq,i));
		for (k=i+1; k<end; k++)
		    if (dotest2(tmp,cvchar(getstringch(seq,k)),fcn)==tresult)
			goto string_noxfer;
#endif
		setstringch(val,j++,getstringch(seq,i));
		string_noxfer:;
	    }

	    MEMCPY(&val->n_string[end],&seq->n_string[end],(l-end)*sizeof(char));

	    if (l != j) { /* need new, shorter result -- too bad */
		fcn = val; /* save value in protected cell */
		val = newstring(j);
		MEMCPY(val->n_string,
		       fcn->n_string,
		       j*sizeof(char));
		val->n_string[j] = 0;
	    }
	    break;
	default:
	    xlbadtype(seq); break;
    }


    /* restore the stack */
#ifdef KEYARG
    xlpopn(4);
#else
    xlpopn(2);
#endif

    /* return the updated sequence */
    return (val);
}

#endif

#endif

