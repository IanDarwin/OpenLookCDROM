/* -*-C-*-
********************************************************************************
*
* File:         xlsys.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlsys.c,v 2.6 1994/06/06 15:59:27 npm Exp $
* Description:  xlisp builtin system functions
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:03:33 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlsys.c,v 2.6 1994/06/06 15:59:27 npm Exp $";

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
extern FILEP tfp;

/* external symbols */
extern LVAL a_subr,a_fsubr,a_cons,a_symbol;
extern LVAL a_fixnum,a_flonum,a_string,a_object,a_stream;
extern LVAL a_vector,a_closure,a_char,a_ustream;
extern LVAL k_verbose,k_print;
extern LVAL true;
#ifdef COMPLX
extern LVAL a_complex;
#endif /* COMPLX */
#ifdef COMMONLISPF
extern LVAL a_list, a_number, a_null, a_atom, a_anystream;
extern LVAL s_and, s_or, s_not, s_satisfies, s_member;
#ifdef STRUCTS
extern LVAL a_struct;
#endif /* STRUCTS */
#ifdef HASHFCNS
extern LVAL a_hashtable;
#endif /* HASHFCNS */
extern LVAL s_lambda, s_function;
#endif /* COMMONLISPF */
#if (defined(UNIX) || defined(WINTERP))
extern LVAL a_PIPE;
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
extern LVAL a_XtAccelerators, a_XtTranslations, a_XEvent, a_Window, a_Pixel,
  a_Pixmap, a_XImage, a_XmString, a_XmFontList, a_XT_RESOURCE, a_CALLBACKOBJ,
  a_TIMEOUTOBJ, a_PIXMAP_REFOBJ, a_WIDGETOBJ, a_EVHANDLEROBJ, a_FDINPUTCBOBJ;
#ifdef WINTERP_XTANGO_WIDGET
extern LVAL a_TANGOIMAGEOBJ, a_TANGO_PATH, a_TANGO_TRANS;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */

extern LVAL xlenv,xlfenv; /* Added for XLOAD mod */

/* xload - read and evaluate expressions from a file */
LVAL xload()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif
    int vflag,pflag;
    LVAL oldenv,oldfenv;    /* TAA MOD-- code sections using these variables
			       forces global environment on LOAD
			       Change based on Luke Tierney's XLISP-STAT */
    LVAL arg;

    /* protect some pointers */
    xlstkcheck(2);
    xlprotect(oldenv);
    xlprotect(oldfenv);

    /* establish global environment */
    oldenv = xlenv;
    oldfenv = xlfenv;
    xlenv = xlfenv = NIL;


    /* get the file name */
#ifdef MEDMEM
    _fstrncpy(name, getstring(xlgetfname()), STRMAX);
    name[STRMAX-1] = '\0';
#else
    name = getstring(xlgetfname());
#endif
    /* get the :verbose flag */ /* TAA MOD to simplify */
    vflag = xlgetkeyarg(k_verbose,&arg) ? (arg != NIL) : TRUE;

    /* get the :print flag */ /* TAA MOD to simplify */
    pflag = xlgetkeyarg(k_print,&arg) ? (arg != NIL) : FALSE;

    xllastarg();

    /* load the file, check for success */
    arg = xlload(name,vflag,pflag) ? true : NIL;

    /* restore environment */
    xlenv = oldenv;
    xlfenv = oldfenv;

    /* restore the stack */
    xlpopn(2);

    /* return success flag */
    return arg;

}

/* xtranscript - open or close a transcript file */
LVAL xtranscript()
{
#ifdef MEDMEM
    char name[STRMAX];
#else
    char *name;
#endif

    /* get the transcript file name */
#ifdef MEDMEM
    if (moreargs()) {
	_fstrncpy(name, getstring(xlgetfname()), STRMAX);
	name[STRMAX-1] = '\0';
    }
    else {
	name[0] = '\0';
    }
#else
    name = (moreargs() ? getstring(xlgetfname()) : NULL);
#endif
    xllastarg();

    /* close the current transcript */
    if (tfp != CLOSED) OSCLOSE(tfp);

    /* open the new transcript */
#ifdef MEDMEM
    tfp = (name[0] != '\0' ? OSAOPEN(name,CREATE_WR) : CLOSED);
#else
    tfp = (name != NULL ? OSAOPEN(name,CREATE_WR) : CLOSED);
#endif

    /* return T if a transcript is open, NIL otherwise */
    return (tfp != CLOSED ? true : NIL);
}

/* xtype - return type of a thing */
LVAL xtype()
{
    LVAL arg;

    arg = xlgetarg();
    xllastarg();    /* TAA MOD -- this was missing */

#ifndef NILSYMBOL
    if (arg == NIL) return (NIL);   /* An outright lie, but XLISP originally
				       did this. When NILSYMBOL, we will
				       return SYMBOL */
#endif

    switch (ntype(arg)) {
    case SUBR:		return (a_subr);
    case FSUBR:		return (a_fsubr);
    case CONS:		return (a_cons);
#if defined(NILSYMBOL) && defined(COMMONLISPF)
    case SYMBOL:	return (null(arg) ? a_list : a_symbol); /* different
                                            from XLISP 2.1 */
#else
    case SYMBOL:	return (a_symbol);
#endif
    case FIXNUM:	return (a_fixnum);
    case FLONUM:	return (a_flonum);
    case STRING:	return (a_string);
    case OBJECT:	return (a_object);
    case STREAM:	return (a_stream);
    case VECTOR:	return (a_vector);
    case CLOSURE:	return (a_closure);
    case CHAR:		return (a_char);
    case USTREAM:	return (a_ustream);
#ifdef STRUCTS
    case STRUCT:	return (getelement(arg,0));
#endif
#ifdef COMPLX
    case COMPLEX:	return (a_complex);
#endif
#if (defined(UNIX) || defined(WINTERP))
    case XLTYPE_PIPE:		return (a_PIPE);
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
    case XLTYPE_XtAccelerators: return (a_XtAccelerators);
    case XLTYPE_XtTranslations: return (a_XtTranslations);
    case XLTYPE_XEvent:		return (a_XEvent);
    case XLTYPE_Window:         return (a_Window);
    case XLTYPE_Pixel:          return (a_Pixel);
    case XLTYPE_Pixmap:         return (a_Pixmap);
    case XLTYPE_XImage:         return (a_XImage);
    case XLTYPE_XmString:	return (a_XmString);
    case XLTYPE_XmFontList:     return (a_XmFontList);
    case XLTYPE_XT_RESOURCE:    return (a_XT_RESOURCE);
    case XLTYPE_CALLBACKOBJ:    return (a_CALLBACKOBJ);
    case XLTYPE_TIMEOUTOBJ:     return (a_TIMEOUTOBJ);
    case XLTYPE_PIXMAP_REFOBJ:  return (a_PIXMAP_REFOBJ);
    case XLTYPE_WIDGETOBJ:      return (a_WIDGETOBJ);
    case XLTYPE_EVHANDLEROBJ:   return (a_EVHANDLEROBJ);
    case XLTYPE_FDINPUTCBOBJ:	return (a_FDINPUTCBOBJ);
#ifdef WINTERP_XTANGO_WIDGET
    case XLTYPE_TANGOIMAGEOBJ:  return (a_TANGOIMAGEOBJ);
    case XLTYPE_TANGO_PATH:     return (a_TANGO_PATH);
    case XLTYPE_TANGO_TRANS:    return (a_TANGO_TRANS);
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */
    default:		xlfail("bad node type");
			return (NIL); /* eliminate warning message */
    }
}

#ifdef COMMONLISPF
int xlcvttype(arg)  /* find type of argument and return it */
LVAL arg;
{
/*sorted into roughly most-likely-used-first order*/
    if (arg == a_cons)	    return CONS;
    if (arg == a_list)	    return CONS;    /* Synonym here */
    if (arg == a_vector)    return VECTOR;
    if (arg == a_string)    return STRING;
    if (arg == a_symbol)    return SYMBOL;
    if (arg == a_subr)	    return SUBR;
    if (arg == a_fsubr)	    return FSUBR;
    if (arg == a_fixnum)    return FIXNUM;
    if (arg == a_flonum)    return FLONUM;
    if (arg == a_object)    return OBJECT;
    if (arg == a_stream)    return STREAM;
    if (arg == a_closure)   return CLOSURE;
    if (arg == a_char)	    return CHAR;
    if (arg == a_ustream)   return USTREAM;
#ifdef STRUCTS
    if (arg == a_struct)    return STRUCT;
#endif /* STRUCTS */
#ifdef COMPLX
    if (arg == a_complex)   return COMPLEX;
#endif /* COMPLX */
#if (defined(UNIX) || defined(WINTERP))
    if (arg == a_PIPE)			return XLTYPE_PIPE;
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
    if (arg == a_WIDGETOBJ)		return XLTYPE_WIDGETOBJ;
    if (arg == a_XmString)		return XLTYPE_XmString;
    if (arg == a_CALLBACKOBJ)		return XLTYPE_CALLBACKOBJ;
    if (arg == a_TIMEOUTOBJ)		return XLTYPE_TIMEOUTOBJ;
    if (arg == a_EVHANDLEROBJ)		return XLTYPE_EVHANDLEROBJ;
    if (arg == a_FDINPUTCBOBJ)		return XLTYPE_FDINPUTCBOBJ;
    if (arg == a_XEvent)		return XLTYPE_XEvent;
    if (arg == a_Window)		return XLTYPE_Window;
    if (arg == a_XtAccelerators)	return XLTYPE_XtAccelerators;
    if (arg == a_XtTranslations)	return XLTYPE_XtTranslations;
    if (arg == a_Pixel)			return XLTYPE_Pixel;
    if (arg == a_Pixmap)		return XLTYPE_Pixmap;
    if (arg == a_XImage)		return XLTYPE_XImage;
    if (arg == a_XmFontList)		return XLTYPE_XmFontList;
    if (arg == a_XT_RESOURCE)		return XLTYPE_XT_RESOURCE;
    if (arg == a_PIXMAP_REFOBJ)		return XLTYPE_PIXMAP_REFOBJ;
#ifdef WINTERP_XTANGO_WIDGET
    if (arg == a_TANGOIMAGEOBJ)		return XLTYPE_TANGOIMAGEOBJ;
    if (arg == a_TANGO_PATH)		return XLTYPE_TANGO_PATH;
    if (arg == a_TANGO_TRANS)		return XLTYPE_TANGO_TRANS;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */
    if (arg == true)	    return -1;	/* Fix for coerce */
    return 0;
}

/* typep -- check type of thing */
#ifdef ANSI
static int NEAR xltypep(LVAL arg, LVAL typ)
#else
LOCAL xltypep(arg, typ)
  LVAL arg, typ;
#endif
{

#ifndef NILSYMBOL
    if (null(typ)) return FALSE;
#endif

    if (symbolp(typ)) {
	int cvtype;

	/* everything is type T */

	if (typ == true) return TRUE;

	/* only NIL is NULL */

	if (typ == a_null) return null(arg);

	/* only atoms are ATOM */

	if (typ == a_atom) return atom(arg);

	/* two types of streams */

	if (typ == a_anystream)
	    return (streamp(arg) || ustreamp(arg));

	/* many ways to be a function */

	if (typ == s_function)
	    return (subrp(arg) || closurep(arg) || symbolp(arg) ||
		(consp(arg) && car(arg) == s_lambda));

	/* NIL is type LIST or SYMBOL */

	if (null(arg)) return (typ==a_list || typ==a_symbol);

	cvtype = xlcvttype(typ);

#ifdef STRUCTS
	/* Structures are type STRUCT or the structure type */

	if (ntype(arg) == STRUCT)
	    return ((cvtype==STRUCT
#ifdef HASHFCNS
		&& getelement(arg,0) != a_hashtable
#endif
		)|| getelement(arg,0) == typ);
#endif

	/* If typename is NUMBER, then arg can be any numeric type */

	if (typ == a_number)
	    return (cvtype==FIXNUM || cvtype == FLONUM
#ifdef COMPLX
		|| cvtype == COMPLEX
#endif
		);

	    /* otherwise the typename must be the same as the type of the
		object (as would be returned by TYPE-OF) */

	return (ntype(arg) == cvtype);
    }
    /* type specifier is a list */
    if (consp(typ)) {
	LVAL fn = car(typ);
	LVAL lst = cdr(typ);

	if (fn == s_not) {  /* (not spec) */
	    if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
	    return !xltypep(arg, car(lst));
	}
	if (fn == s_satisfies) { /* (satisfies predicatefn) */
	    if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
#ifdef KEYARG
	    return dotest1(arg, car(lst), NIL);
#else
	    return dotest1(arg, car(lst));
#endif
	}
	if (fn == a_object) { /* (object class) */
	    if (!consp(lst) || !atom(cdr(lst))) goto bad_type;
	    lst = car(lst);
	    return (objectp(arg) &&
		(symbolp(lst) ? getvalue(lst) : lst) == getclass(arg));
	}
	if (fn == s_and) {  /* (and {spec}) */
	    for (; consp(lst); lst = cdr(lst))
		if (!xltypep(arg,car(lst))) return FALSE;
	    return TRUE;
	}
	if (fn == s_or) {   /* (or {spec}) */
	    for (; consp(lst); lst = cdr(lst))
		if (xltypep(arg,car(lst))) return TRUE;
	    return FALSE;
	}
	if (fn == s_member) {	/* (member {args}) */
	    for (; consp(lst); lst = cdr(lst))
		if (eql(car(lst),arg)) return TRUE;
	    return FALSE;
	}
    }
bad_type:
    xlerror("bad type specifier", typ);
    return FALSE; /* keep compilers happy */
}

LVAL xtypep()
{
    LVAL arg, typ;

    arg = xlgetarg();
    typ = xlgetarg();
    xllastarg();

    return (xltypep(arg, typ) ? true : NIL);
}




#ifdef ANSI
static LVAL NEAR listify(LVAL arg)  /* arg must be vector or string */
#else
LOCAL LVAL listify(arg) /* arg must be vector or string */
LVAL arg;
#endif
{
    LVAL val;
    unsigned i;

    xlsave1(val);

    if (ntype(arg) == VECTOR) {
	for (i = getsize(arg); i-- > 0; )
	    val = cons(getelement(arg,i),val);
    }
    else {  /* a string */
	for (i = getslength(arg); i-- > 0; )
	    val = cons(cvchar(getstringch(arg,i)),val);
    }

    xlpop();
    return (val);
}

#ifdef ANSI
static LVAL NEAR vectify(LVAL arg)  /* arg must be string or cons */
#else
LOCAL LVAL vectify(arg) /* arg must be string or cons */
LVAL arg;
#endif
{
    LVAL val,temp;
    unsigned i,l;

    if (ntype(arg) == STRING) {
	l = getslength(arg);
	val = newvector(l);
	for (i=0; i < l; i++) setelement(val,i,cvchar(getstringch(arg,i)));
    }
    else {  /* a cons */
	val = arg;
	for (l = 0; consp(val);) { /* get length */
	    val = cdr(val);
	    l++;
	    if (l > MAXSLEN) xltoolong();
	}
	val = newvector(l);
	temp = arg;
	for (i = 0; i < l; i++) {
	    setelement(val,i,car(temp));
	    temp = cdr(temp);
	}
    }
	return val;
}

#ifdef ANSI
static LVAL NEAR stringify(LVAL arg)
#else
LOCAL LVAL stringify(arg)   /* arg must be vector or cons */
LVAL arg;
#endif
{
    LVAL val,temp;
    unsigned i,l;

    if (ntype(arg) == VECTOR) {
	l = getsize(arg);
	val = newstring(l);
	for (i=0; i < l; i++) {
	    temp = getelement(arg,i);
	    if (ntype(temp) != CHAR) goto failed;
	    val->n_string[i] = getchcode(temp);
	}
	val->n_string[l] = 0;
	return val;
    }
    else {  /* must be cons */
	val = arg;
	for (l = 0; consp(val);) {
	    if (ntype(car(val)) != CHAR) goto failed;
	    val = cdr(val); /* get length */
	    l++;
	    if (l > MAXSLEN) xltoolong();
	}

	val = newstring(l);
	temp = arg;
	for (i = 0; i < l; i++) {
	    val->n_string[i] = getchcode(car(temp));
	    temp = cdr(temp);
	}
	val->n_string[l] = 0;
	return val;
    }
failed:
    xlerror("can't make into string", arg);
    return (NIL);   /* avoid compiler warnings */
}



/* coerce function */
LVAL xcoerce()
{
    LVAL type, arg, temp;
    int newtype,oldtype;

    arg = xlgetarg();
    type = xlgetarg();
    xllastarg();

    if ((newtype = xlcvttype(type)) == 0) goto badconvert;

    oldtype = (arg==NIL? CONS: ntype(arg)); /* TAA fix */

    if (newtype == -1 || oldtype == newtype) return (arg);  /* easy case! */

    switch (newtype) {
	case CONS:
	    if ((oldtype == STRING)||(oldtype == VECTOR))
		return (listify(arg));
	    break;
	case STRING:
	    if ((oldtype == CONS)||(oldtype == VECTOR))
		return (stringify(arg));
	    break;
	case VECTOR:
	    if ((oldtype == STRING)||(oldtype == CONS))
		return (vectify(arg));
	    break;
	case CHAR:
	    if (oldtype == FIXNUM) return cvchar((int)getfixnum(arg));
	    else if ((oldtype == STRING) && (getslength(arg) == 1))
		return cvchar(getstringch(arg,0));
	    else if (oldtype == SYMBOL) {
		temp = getpname(arg);
		if (getslength(temp) == 1) return cvchar(getstringch(temp,0));
	    }
	    break;
	case FLONUM:
	    if (oldtype == FIXNUM) return (cvflonum(1.0*(int)getfixnum(arg)));
	    break;
#ifdef COMPLX
	case COMPLEX:
	    if (oldtype == FIXNUM)
		return newicomplex(getfixnum(arg), (FIXTYPE) 0);
	    else if (oldtype == FLONUM)
		return newdcomplex(getflonum(arg), (FLOTYPE) 0.0);
	    break;
#endif
    }


badconvert:
    xlerror("illegal coersion",arg);
    return (NIL);   /* avoid compiler warnings */
}


#endif


#ifdef ADDEDTAA
/* xgeneric - get generic representation of thing */
/* TAA addition */
LVAL xgeneric()
{
    LVAL arg,acopy;
#ifdef WINTERP
    int i;
#endif /* WINTERP */

    arg = xlgetarg();
    xllastarg();
#ifndef NILSYMBOL
    if (arg == NIL)  return (NIL);
#endif

    switch (ntype(arg)) {
    case CONS: case USTREAM:
	return (cons(car(arg),cdr(arg)));
    case SYMBOL: case OBJECT: case VECTOR: case CLOSURE:
#ifdef STRUCTS
    case STRUCT:
#endif
#ifdef COMPLX
    case COMPLEX:
#endif
#ifdef WINTERP
    case XLTYPE_PIXMAP_REFOBJ:
#endif /* WINTERP */	
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	return (acopy);

#ifdef WINTERP
      case XLTYPE_Window:
	return (cvfixnum((FIXTYPE) get_window(arg))); 
      case XLTYPE_CALLBACKOBJ:
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	for (i = 0; (i < CALLBACKOBJ_IDX_OF_FIRST_LVAL); i++) 
	  setelement(acopy, i, NIL); /* make non-LVAL parts of original be printable/GCable in copy */
	set_callback_name(acopy, cvstring(get_callback_name(arg))); /* 'name' was char*; in copy, make it an LVAL */
	/* get_callback_proc() field is NIL in copy */
	return (acopy);
      case XLTYPE_TIMEOUTOBJ:
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	for (i = 0; (i < TIMEOUTOBJ_IDX_OF_FIRST_LVAL); i++) 
	  setelement(acopy, i, NIL); /* make non-LVAL parts of original be printable/GCable in copy */
	/* get_timeout_id() field is NIL in copy */
	return (acopy);
      case XLTYPE_EVHANDLEROBJ:
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	for (i = 0; (i < EVHANDLEROBJ_IDX_OF_FIRST_LVAL); i++) 
	  setelement(acopy, i, NIL); /* make non-LVAL parts of original be printable/GCable in copy */
	/* TODO -- return s-expr in place of EventMask held in get_evhandler_mask(), currently NIL. */
	/* TODO -- return tuple w/ :RAW or :NONMASKABLE for 'long' held in get_evhandler_options(), currently NIL */
	return (acopy);
      case XLTYPE_FDINPUTCBOBJ:
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	for (i = 0; (i < FDINPUTCBOBJ_IDX_OF_FIRST_LVAL); i++) 
	  setelement(acopy, i, NIL); /* make non-LVAL parts of original be printable/GCable in copy */
	/* get_fdinputcb_id() field is NIL in copy */
	return (acopy);
      case XLTYPE_WIDGETOBJ:
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	for (i = 1; (i < WIDGETOBJ_IDX_OF_FIRST_LVAL); i++) 
	  setelement(acopy, i, NIL); /* make non-LVAL parts of original be printable/GCable in copy */
	/* getclass() field in copy is an OBJECT lval, returned due to 'i = 1' in 'for' loop */
	/* get_widgetobj_widgetID() field is NIL in copy */
	return (acopy);
#ifdef WINTERP_XTANGO_WIDGET
      case XLTYPE_TANGOIMAGEOBJ:
	acopy = newvector(getsize(arg));
	MEMCPY(acopy->n_vdata, arg->n_vdata, getsize(arg)*sizeof(LVAL));
	for (i = 1; (i < TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL); i++) 
	  setelement(acopy, i, NIL); /* make non-LVAL parts of original be printable/GCable in copy */
	/* getclass() field in copy is an OBJECT lval, returned due to 'i = 1' in 'for' loop */
	/* get_tangoimageobj_timageID() and get_tangoimageobj_context() fields are NIL in copy */
	return (acopy);
      case XLTYPE_TANGO_PATH:
	return (cvfixnum((FIXTYPE) get_tangopath(arg))); 
      case XLTYPE_TANGO_TRANS:
	return (cvfixnum((FIXTYPE) get_tangotrans(arg)));
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */

    case STRING: /* make a copy of the string */
	acopy = newstring(getslength(arg));
	MEMCPY(getstring(acopy), getstring(arg), getslength(arg)+1);
	return (acopy);
    case FIXNUM: case FLONUM: case CHAR:
	return (arg); /* it hardly matters to copy these */
    default:	xlbadtype(arg);
	return (NIL);	/* avoid compiler warnings */
    }
}

#endif


/* xbaktrace - print the trace back stack */
LVAL xbaktrace()
{
    LVAL num;
    int n;

    if (moreargs()) {
	num = xlgafixnum();
	n = (int)getfixnum(num);
    }
    else
	n = -1;
    xllastarg();
    xlbaktrace(n);
    return (NIL);
}

/* xexit - get out of xlisp */
LVAL xexit()
{
    xllastarg();
    wrapup();
    return (NIL); /* never returns */
}

/* xpeek - peek at a location in memory */
LVAL xpeek()
{
    LVAL num;
    OFFTYPE *adr;   /* TAA MOD so that data fetched is sizeof(LVAL *) */

    /* get the address */
    num = xlgafixnum(); adr = (OFFTYPE *)getfixnum(num);
    xllastarg();

    /* return the value at that address */
    return (cvfixnum((FIXTYPE)*adr));
}

/* xpoke - poke a value into memory */
LVAL xpoke()
{
    LVAL val;
    OFFTYPE *adr;   /* TAA MOD so that data fetched is sizeof(LVAL *) */

    /* get the address and the new value */
    val = xlgafixnum(); adr = (OFFTYPE *)getfixnum(val);
    val = xlgafixnum();
    xllastarg();

    /* store the new value */
    *adr = (OFFTYPE)getfixnum(val);

    /* return the new value */
    return (val);
}

/* xaddrs - get the address of an XLISP node */
LVAL xaddrs()
{
    LVAL val;

    /* get the node */
    val = xlgetarg();
    xllastarg();

    /* return the address of the node */
    return (cvfixnum((FIXTYPE)val));
}

#ifdef RANDOM

extern LVAL a_randomstate, s_randomstate, k_data;


LVAL newrandom(seed)
 long seed;
{
    LVAL result;

    result = newstruct(a_randomstate, 1);
    xlprot1(result);

    setelement(result, 1, cvfixnum((FIXTYPE)seed));

    xlpop();

    return result;
}


/* make-random-state function */
LVAL xmakerandom()
{
    LVAL arg;

    /*argument is either random state, t for randomize, or nil/absent
	to use *random-state* */

    /* secret agenda: there could also be no regular arguments but a
	single keyword argument (:DATA) which is the seed!
	I'll leave it to the curious to figure out why. */

    if (moreargs()) {
	arg = xlgetarg();
	if (arg == k_data) {
	    arg = xlgafixnum();
	    xllastarg();
	    return newrandom((long)getfixnum(arg));
	}
	xllastarg();
	if (arg == true) return newrandom(real_tick_count());
	if (null(arg)) arg = getvalue(s_randomstate);
    }
    else arg = getvalue(s_randomstate);

    if ((!structp(arg)) || getelement(arg,0) != a_randomstate
	|| !fixp(getelement(arg,1))) {
	xlbadtype(arg);
    }

    return newrandom((long)getfixnum(getelement(arg,1)));
}

/* RANDOM Function */

LVAL xrand()
{
    LVAL state, value;
    long rand;
    int isfixed;

    value = xlgetarg();

    if (fixp(value)) {
	isfixed = TRUE;
	if (getfixnum(value) <= 0) xlerror("range error", value);
    }
    else if (floatp(value)) {
	isfixed = FALSE;
	if (getflonum(value) <= 0.0) xlerror("range error", value);
    }
    else xlbadtype(value);

    if (moreargs()) {	/* seed provided */
	state = xlgetarg();
	xllastarg();
    }
    else {  /* use global seed */
	state = getvalue(s_randomstate);
    }

    if ((!structp(state)) || getelement(state,0) != a_randomstate
	|| !fixp(getelement(state,1))) {
	xlbadtype(state);
    }

    rand = osrand((long)getfixnum(getelement(state,1))); /* generate number*/

    setelement(state, 1, cvfixnum((FIXTYPE)rand)); /* put seed away */

    if (isfixed)
	return cvfixnum((FIXTYPE)rand % getfixnum(value));
    else
	/* I'm tossing the upper 7 bits which, while it increases granularity,
	    will make the numbers more "random", I hope */
	return cvflonum((FLOTYPE)(rand&0xffffffL)/(FLOTYPE)0x1000000L*getflonum(value));
}
#endif
