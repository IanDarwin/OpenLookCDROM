/* -*-C-*-
********************************************************************************
*
* File:         xldmem.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xldmem.c,v 2.6 1994/06/06 15:59:13 npm Exp $
* Description:  xlisp dynamic memory management routines.
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:39 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xldmem.c,v 2.6 1994/06/06 15:59:13 npm Exp $";

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

/* node flags */
#ifdef WINTERP
#ifdef JGC
#define MARK	0x40		/* bit 2^6 */
#define LEFT	0x80		/* bit 2^7 */
#else /* !defined(JGC) */
#define MARK	1
#define LEFT	2
#endif /* JGC */
#else /* !defined(WINTERP) */
#ifdef JGC
#define MARK	0x20
#define LEFT	0x40
#else /* !defined(JGC) */
#define MARK	1
#define LEFT	2
#endif /* JGC */
#endif /* WINTERP */

/* macro to compute the size of a segment */
#define segsize(n) (sizeof(SEGMENT)+((n)-1)*sizeof(struct node))

/* external variables */
extern LVAL obarray,s_gcflag,s_gchook,s_unbound,s_debugio,true;
extern LVAL xlenv,xlfenv,xldenv;

/* variables local to xldmem.c and xlimage.c */
SEGMENT *segs,*lastseg,*fixseg,*charseg;
int anodes,nsegs,gccalls;
long nnodes,nfree,total;
LVAL fnodes = NIL;

/* forward declarations */
#ifdef ANSI
#ifdef JMAC
LOCAL FORWARD LVAL NEAR Newnode(int type); /* NPM: changed this to LOCAL */
#else  /* !defined(JMAC) */
LOCAL FORWARD LVAL NEAR newnode(int type); /* NPM: changed this to LOCAL */
#endif /* JMAC */
LOCAL FORWARD char * NEAR stralloc(unsigned int size); /* NPM: changed this to LOCAL */
LOCAL FORWARD VOID NEAR mark(LVAL ptr);	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID NEAR sweep(void); /* NPM: changed this to LOCAL */
LOCAL FORWARD VOID NEAR findmem(void); /* NPM: changed this to LOCAL */
LOCAL FORWARD int  NEAR addseg(void); /* NPM: changed this to LOCAL */
#else /* !defined(ANSI) */
#ifdef JMAC
LOCAL FORWARD LVAL Newnode();	/* NPM: changed this to LOCAL */
#else /* !defined(JMAC) */
LOCAL FORWARD LVAL newnode();	/* NPM: changed this to LOCAL */
#endif /* JMAC */
LOCAL FORWARD char *stralloc();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID mark();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID sweep();	/* NPM: changed this to LOCAL */
LOCAL FORWARD VOID findmem();	/* NPM: changed this to LOCAL */
#endif /* ANSI */


#ifdef JMAC
LVAL _nnode = NIL;
FIXTYPE _tfixed = 0;
int _tint = 0;

#define newnode(type) (((_nnode = fnodes) != NIL) ? \
	    ((fnodes = cdr(_nnode)), \
	     nfree--, \
	     (_nnode->n_type = type), \
	     rplacd(_nnode,NIL), \
	     _nnode) \
	    : Newnode(type))

#endif

#if (defined(UNIX) || defined(WINTERP))
/* cv_pipe - allocate and initialize a new XLTYPE_PIPE node, which uses same node structure as STREAM (see cvfile())  */
#ifdef BETTERIO
LVAL cv_pipe(fp, iomode)
     FILE *fp;
     int  iomode;
{
  LVAL val;
  val = newnode(XLTYPE_PIPE);
  setfile(val,fp);
  setsavech(val,'\0');
  val->n_sflags = iomode;
  val->n_cpos = 0;
  return (val);
}
#else /* !defined(BETTERIO) */
LVAL cv_pipe(fp)
     FILE *fp;
{
  LVAL val;
  val = newnode(XLTYPE_PIPE);
  setfile(val,fp);
  setsavech(val,'\0');
  return (val);
}
#endif /* BETTERIO */
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP

extern LVAL v_savedobjs;	/* w_savedobjs.c and xlglob.c */
extern void Wxms_Garbage_Collect_XmString(); /* w_XmString.c */
extern void Wpm_Decr_Refcount_Or_Free_Pixmap();	/* w_pixmap.c */

#ifdef WINTERP_XTANGO_WIDGET
extern void Xtango_Context_Add_TANGO_TRANS(/* LVAL lval */); /* t_utils.c */
extern void Xtango_Context_Remove_TANGO_PATH(/* LVAL lval */); /* t_utils.c */
extern void Xtango_Context_Remove_TANGO_TRANS(/* LVAL lval */); /* t_utils.c */
#endif /* WINTERP_XTANGO_WIDGET */

/* cv_xtresource - convert a xtresource instance to an XLTYPE_XT_RESOURCE node */
LVAL cv_xtresource(res)
     struct _Resource_Instance *res;
{
  LVAL val;
  val = newnode(XLTYPE_XT_RESOURCE);
  val->n_xtresource = res;
  return (val);
}

/* cv_pixel - convert a Pixel to an XLTYPE_Pixel node */
LVAL cv_pixel(pixel)
     Pixel pixel;
{
  LVAL val;
  val = newnode(XLTYPE_Pixel);
  val->n_pixel = pixel;
  return (val);
}

/* cv_pixmap - allocate and initialize a new XLTYPE_Pixmap node */
LVAL cv_pixmap(pixmap)
     Pixmap pixmap;
{
  LVAL val;
  val = newnode(XLTYPE_Pixmap);
  val->n_pixmap = pixmap;
  val->n_pixmap_color_info = (WINTERP_GIF_COLOR_INFO) NULL;
  return (val);
}

/* cv_pixmap - allocate and initialize a new XLTYPE_Pixmap node */
LVAL cv_pixmap_allocd_colors(pixmap, color_info)
     Pixmap pixmap;
     WINTERP_GIF_COLOR_INFO color_info;	/* Array of Pixels alloc'd to create GIF-Pixmap... */
{
  LVAL val;
  val = newnode(XLTYPE_Pixmap);
  val->n_pixmap = pixmap;
  val->n_pixmap_color_info = color_info;
  return (val);
}

/* cv_ximage -- allocate and initialize a new XLTYPE_XImage node */
LVAL cv_ximage(ximage)
     XImage *ximage;
{
  LVAL val;
  val = newnode(XLTYPE_XImage);
  val->n_ximage = ximage;
  return (val);
}

/* cv_xmstring -- allocate and initialize a new XLTYPE_XmString node */
LVAL cv_xmstring(xmstr)
     XmString xmstr;
{
  LVAL val;
  val = newnode(XLTYPE_XmString);
  val->n_xmstring = xmstr;
  return (val);
}

/* cv_xevent_ptr -- allocate and initialize a new XLTYPE_XEvent node */
LVAL cv_xevent(xevp)
     XEvent *xevp;
{
  LVAL val;
  val = newnode(XLTYPE_XEvent);
  val->n_xevent = xevp;
  return (val);
}

/* cv_window -- allocate and initialize a new XLTYPE_Window node */
LVAL cv_window(win)
     Window win;
{
  LVAL val;
  val = newnode(XLTYPE_Window);
  val->n_window = win;
  return (val);
}

/* cv_xtaccelerators -- allocate and initialize a new XLTYPE_XtAccelerators node */
LVAL cv_xtaccelerators(axl)
     XtAccelerators axl;
{
  LVAL val;
  val = newnode(XLTYPE_XtAccelerators);
  val->n_xtaccelerators = axl;
  return (val);
}

/* cv_xttranslations -- allocate and initialize a new XLTYPE_XtTranslations node */
LVAL cv_xttranslations(txl)
     XtTranslations txl;
{
  LVAL val;
  val = newnode(XLTYPE_XtTranslations);
  val->n_xttranslations = txl;
  return (val);
}

/* cv_string - allocate and initialize a new STRING node.     */
/* WARNING: use cvstring() to make a copy of the string ...   */
/* the string passed will get freed during garbage collection */
LVAL cv_string(str)
     char* str;
{
  LVAL val;
  val = newnode(STRING);
  val->n_strlen = strlen(str);
  val->n_string = str;
  return (val);
}

#ifdef WINTERP_XTANGO_WIDGET

/* cv_tangopath -- allocate and initialize a new XLTYPE_TANGO_PATH node */
LVAL cv_tangopath(path)
     TANGO_PATH path;
{
  LVAL val;
  val = newnode(XLTYPE_TANGO_PATH);
  val->n_xtangopath = path;
  /* Xtango_Context_Add_XLTYPE_TANGO_PATH(val); */
  return (val);
}

/* cv_tangotrans -- allocate and initialize a new XLTYPE_TANGO_TRANS node */
LVAL cv_tangotrans(trans, context)
     TANGO_TRANS trans;
     WINTERP_TANGO_CONTEXT context; /* type def'd in ../tango.h */
{
  LVAL val;
  val = newnode(XLTYPE_TANGO_TRANS);
  val->n_xtangotrans = trans;
  val->n_xtangotrans_context = context;
  Xtango_Context_Add_TANGO_TRANS(val); /* when context gets destroyed, this allows the TANGO_TRANS to be marked as invalid, s.t. get_tangotrans(x)==NULL. */
  return (val);
}
#endif /* WINTERP_XTANGO_WIDGET */

/* new_pixrefobj() -- allocate and initialize a new XLTYPE_PIXMAP_REFOBJ */
LVAL new_pixrefobj()
{
  LVAL val;
  val = newvector(PIXMAP_REFOBJ_SIZE);
  val->n_type = XLTYPE_PIXMAP_REFOBJ;
  return (val);
}

/* new_callbackobj() -- allocate and initialize a new XLTYPE_CALLBACKOBJ */
LVAL new_callbackobj()
{
  LVAL val;
  val = newvector(CALLBACKOBJ_SIZE);
  val->n_type = XLTYPE_CALLBACKOBJ;
  return (val);
}

/* new_timeoutobj() -- allocate and initialize a new XLTYPE_TIMEOUTOBJ */
LVAL new_timeoutobj()
{
  LVAL val;
  val = newvector(TIMEOUTOBJ_SIZE);
  val->n_type = XLTYPE_TIMEOUTOBJ;
  return (val);
}

/* new_evhandlerobj() -- allocate and initialize a new XLTYPE_EVHANDLEROBJ */
LVAL new_evhandlerobj()
{
  LVAL val;
  val = newvector(EVHANDLEROBJ_SIZE);
  val->n_type = XLTYPE_EVHANDLEROBJ;
  return (val);
}

/* new_fdinputcbobj() -- allocate and initialize a new XLTYPE_FDINPUTCBOBJ */
LVAL new_fdinputcbobj()
{
  LVAL val;
  val = newvector(FDINPUTCBOBJ_SIZE);
  val->n_type = XLTYPE_FDINPUTCBOBJ;
  return (val);
}
#endif /* WINTERP */


/* xlminit - initialize the dynamic memory module */
VOID xlminit()
{
    LVAL p;
    int i;

    /* initialize our internal variables */
    segs = lastseg = NULL;
    nnodes = nfree = total = 0L;
    nsegs = gccalls = 0;
    anodes = NNODES;
    fnodes = NIL;

    /* allocate the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
	xlfatal("insufficient memory");

    /* initialize the fixnum segment */
    p = &fixseg->sg_nodes[0];
    for (i = SFIXMIN; i <= SFIXMAX; ++i) {
	p->n_type = FIXNUM;
	p->n_fixnum = i;
	++p;
    }

    /* allocate the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
	xlfatal("insufficient memory");

    /* initialize the character segment */
    p = &charseg->sg_nodes[0];
    for (i = CHARMIN; i <= CHARMAX; ++i) {
	p->n_type = CHAR;
	p->n_chcode = i;
	++p;
    }

    /* initialize structures that are marked by the collector */
    obarray = NULL;
    xlenv = xlfenv = xldenv = NIL;
    s_gcflag = s_gchook = NULL;
#ifdef WINTERP
    v_savedobjs = NULL;
#endif /* WINTERP */

#ifdef STATICSTK
    /* allocate the evaluation stack */
    xlstack = xlstktop;

    /* allocate the argument stack */
    xlfp = xlsp = xlargstkbase;
#else
    /* allocate the evaluation stack */
    if ((xlstkbase = (LVAL **)MALLOC(EDEPTH * sizeof(LVAL *))) == NULL)
	xlfatal("insufficient memory");
    xlstack = xlstktop = xlstkbase + EDEPTH;

    /* allocate the argument stack */
    if ((xlargstkbase = (LVAL *)MALLOC(ADEPTH * sizeof(LVAL))) == NULL)
	xlfatal("insufficient memory");
    xlargstktop = xlargstkbase + ADEPTH;
    xlfp = xlsp = xlargstkbase;
#endif
    *xlsp++ = NIL;

#ifdef NILSYMBOL
    /* we have to make a NIL symbol before continuing */

    p = xlmakesym("NIL");
    memcpy(NIL, p, sizeof(struct node));    /* we point to this! */
    defconstant(NIL, NIL);
    p->n_type = FREE;			    /* don't collect "garbage" */

#endif




}

/* cons - construct a new cons node */
LVAL cons(x,y)
  LVAL x,y;
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
	xlstkcheck(2);
	xlprotect(x);
	xlprotect(y);
	findmem();
	if ((nnode = fnodes) == NIL)
	    xlabort("insufficient node space");
	xlpop();
	xlpop();
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    --nfree;

    /* initialize the new node */
    nnode->n_type = CONS;
    rplaca(nnode,x);
    rplacd(nnode,y);

    /* return the new node */
    return (nnode);
}

/* cvstring - convert a string to a string node */
LVAL cvstring(str)
  char *str;
{
    LVAL val;
    xlsave1(val);
    val = newnode(STRING);
    val->n_strlen = strlen(str);
    val->n_string = stralloc(getslength(val)+1);
    strcpy((char *)getstring(val),str);
    xlpop();
    return (val);
}

/* newstring - allocate and initialize a new string */
LVAL newstring(size)
  unsigned size;
{
    LVAL val;
    xlsave1(val);
    val = newnode(STRING);
    val->n_strlen = size;
    val->n_string = stralloc(size+1);
    val->n_string[0] = 0;
    xlpop();
    return (val);
}

/* cvsymbol - convert a string to a symbol */
LVAL cvsymbol(pname)
  char *pname;
{
    LVAL val;
    xlsave1(val);
    val = newvector(SYMSIZE);
    val->n_type = SYMBOL;
    setvalue(val,s_unbound);
    setfunction(val,s_unbound);
    setpname(val,cvstring(pname));
    xlpop();
    return (val);
}

/* cvsubr - convert a function to a subr or fsubr */
#ifdef ANSI
LVAL cvsubr(LVAL (*fcn)(void), int type, int offset)
#else
LVAL cvsubr(fcn,type,offset)
  LVAL (*fcn)(); int type,offset;
#endif
{
    LVAL val;
    val = newnode(type);
    val->n_subr = fcn;
    val->n_offset = offset;
    return (val);
}

/* cvfile - convert a file pointer to a stream */
#ifdef BETTERIO
LVAL cvfile(fp, iomode)
  FILEP fp;
  int  iomode;
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
    val->n_sflags = iomode;
    val->n_cpos = 0;
    return (val);
}
#else
LVAL cvfile(fp)
  FILE* fp;
{
    LVAL val;
    val = newnode(STREAM);
    setfile(val,fp);
    setsavech(val,'\0');
    return (val);
}
#endif

#ifdef JMAC

/* cvfixnum - convert an integer to a fixnum node */
LVAL Cvfixnum(n)
  FIXTYPE n;
{
    LVAL val;
    val = newnode(FIXNUM);
    val->n_fixnum = n;
    return (val);
}
#else
/* cvfixnum - convert an integer to a fixnum node */
LVAL cvfixnum(n)
  FIXTYPE n;
{
    LVAL val;
    if (n >= SFIXMIN && n <= SFIXMAX)
	return (&fixseg->sg_nodes[(int)n-SFIXMIN]);
    val = newnode(FIXNUM);
    val->n_fixnum = n;
    return (val);
}
#endif

/* cvflonum - convert a floating point number to a flonum node */
LVAL cvflonum(n)
  FLOTYPE n;
{
    LVAL val;
    val = newnode(FLONUM);
    val->n_flonum = n;
    return (val);
}

/* cvchar - convert an integer to a character node */
#ifdef JMAC
LVAL Cvchar(n)
  int n;
{
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return(NIL);    /* never executed */
}
#else
LVAL cvchar(n)
  int n;
{
    if (n >= CHARMIN && n <= CHARMAX)
	return (&charseg->sg_nodes[n-CHARMIN]);
    xlerror("character code out of range",cvfixnum((FIXTYPE)n));
    return 0;	/* never executed but gets rid of warning message */
}
#endif

/* newustream - create a new unnamed stream */
LVAL newustream()
{
    LVAL val;
    val = newnode(USTREAM);
    sethead(val,NIL);
    settail(val,NIL);
    return (val);
}

/* newobject - allocate and initialize a new object */
LVAL newobject(cls,size)
  LVAL cls; int size;
{
    LVAL val;
    val = newvector(size+1);
    val->n_type = OBJECT;
    setelement(val,0,cls);
    return (val);
}

/* newclosure - allocate and initialize a new closure */
LVAL newclosure(name,type,env,fenv)
  LVAL name,type,env,fenv;
{
    LVAL val;
    val = newvector(CLOSIZE);
    val->n_type = CLOSURE;
    setname(val,name);
    settype(val,type);
    setenvi(val,env);
    setfenv(val,fenv);
    return (val);
}

#ifdef STRUCTS
/* newstruct - allocate and initialize a new structure node */
LVAL newstruct(type,size)
 LVAL type; int size;
{
    LVAL val;
    val = newvector(size+1);
    val->n_type = STRUCT;
    setelement(val,0,type);
    return (val);
}
#endif


/* newvector - allocate and initialize a new vector node */
LVAL newvector(size)
  unsigned size;
{
    LVAL vect;
    int i;
    long bsize = size * sizeof(LVAL *);

    if (size > MAXVLEN) xlfail("array too large");

    xlsave1(vect);

    vect = newnode(VECTOR);
    vect->n_vsize = 0;

    if (size != 0) {
#ifdef NILSYMBOL    /* We must clear to a nonzero value */
	if ((vect->n_vdata = (LVAL *)MALLOC((unsigned int)bsize)) == NULL) {
	    gc();   /*	TAA Mod -- was findmem(), but this would
			cause undesired memory expansion */
	    if ((vect->n_vdata = (LVAL *)MALLOC((unsigned int)bsize)) == NULL)
		xlfail("insufficient vector space");
	}
	for (i = size; i-- > 0;) setelement(vect, i, NIL);
#else	/* CALLOC will clear for us */
	if ((vect->n_vdata = (LVAL *)CALLOC(1,(unsigned int)bsize)) == NULL) {
	    gc();   /*	TAA Mod -- was findmem(), but this would
			cause undesired memory expansion */
	    if ((vect->n_vdata = (LVAL *)CALLOC(1,(unsigned int)bsize)) == NULL)
		xlfail("insufficient vector space");
	}
#endif
	vect->n_vsize = size;
	total += bsize;
    }
    xlpop();
    return (vect);
}

/* newnode - allocate a new node */
#ifdef JMAC
LOCAL LVAL NEAR Newnode(type)
  int type;
{
    LVAL nnode;

    /* get a free node */
    findmem();
    if ((nnode = fnodes) == NIL)
	xlabort("insufficient node space");

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;

    /* initialize the new node */
    nnode->n_type = type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}
#else
LOCAL LVAL NEAR newnode(type)
  int type;
{
    LVAL nnode;

    /* get a free node */
    if ((nnode = fnodes) == NIL) {
	findmem();
	if ((nnode = fnodes) == NIL)
	    xlabort("insufficient node space");
    }

    /* unlink the node from the free list */
    fnodes = cdr(nnode);
    nfree -= 1L;

    /* initialize the new node */
    nnode->n_type = type;
    rplacd(nnode,NIL);

    /* return the new node */
    return (nnode);
}
#endif

/* stralloc - allocate memory for a string */
LOCAL char * NEAR stralloc(size)
  unsigned int size;
{
    char *sptr;

    /* allocate memory for the string copy */
    if ((sptr = (char *)MALLOC(size)) == NULL) {
	gc();
	if ((sptr = (char *)MALLOC(size)) == NULL)
	    xlfail("insufficient string space");
    }
    total += (long)size;

    /* return the new string memory */
    return (sptr);
}

/* findmem - find more memory by collecting then expanding */
LOCAL VOID NEAR findmem()
{
    gc();
    if (nfree < (long)anodes)
	addseg();
}

/* gc - garbage collect (only called here and in xlimage.c) */
VOID gc()
{
    register LVAL **p,*ap,tmp;
    FRAMEP newfp;
    LVAL fun;

    /* print the start of the gc message */
    if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
#ifdef BETTERIO /* print message on a fresh line */
	xlfreshline(getvalue(s_debugio));
#endif
	sprintf(buf,"[ gc: total %ld, ",nnodes);
	dbgputstr(buf); /* TAA MOD -- was std output */
    }

#ifdef WINTERP
    /* mark the callback-obj's, timeout-obj's, etc that are "referenced"
       inside Motif/Xtoolkit implementation. */
    if (v_savedobjs != NULL)
        mark(v_savedobjs);
#endif /* WINTERP */

    /* mark the obarray, the argument list and the current environment */
    if (obarray != NULL)
	mark(obarray);
    if (xlenv != NIL)
	mark(xlenv);
    if (xlfenv != NIL)
	mark(xlfenv);
    if (xldenv != NIL)
	mark(xldenv);

#ifdef NILSYMBOL
    mark(NIL);
#endif

    /* mark the evaluation stack */
    for (p = xlstack; p < xlstktop; ++p)
	if ((tmp = **p) != NIL)
	    mark(tmp);

    /* mark the argument stack */
    for (ap = xlargstkbase; ap < xlsp; ++ap)
	if ((tmp = *ap) != NIL)
	    mark(tmp);

    /* sweep memory collecting all unmarked nodes */
    sweep();

#ifdef NILSYMBOL
#ifdef JGC
    NIL->n_type &= ~MARK;
#else
    NIL->n_flags &= ~MARK;
#endif
#endif

    /* count the gc call */
    ++gccalls;

    /* call the *gc-hook* if necessary */
    if (s_gchook != NULL && ((fun = getvalue(s_gchook)) != NIL) ) {

	/* rebind hook function to NIL	TAA MOD */
	tmp = xldenv;
	xldbind(s_gchook,NIL);

	newfp = xlsp;
	pusharg(cvfixnum((FIXTYPE)(newfp - xlfp)));
	pusharg(fun);
	pusharg(cvfixnum((FIXTYPE)2));
	pusharg(cvfixnum((FIXTYPE)nnodes));
	pusharg(cvfixnum((FIXTYPE)nfree));
	xlfp = newfp;
	xlapply(2);

	/* unbind the symbol TAA MOD */
	xlunbind(tmp);
    }

    /* print the end of the gc message */
    if (s_gcflag != NULL && getvalue(s_gcflag) != NIL) {
	sprintf(buf,"%ld free ]\n",nfree);
	dbgputstr(buf); /* TAA MOD -- was std output */
    }
}

/* mark - mark all accessible nodes */
LOCAL VOID NEAR mark(ptr)
  LVAL ptr;
{
    register LVAL this,prev,tmp;
#ifdef JGC
    int i,n;
#else
    int type,i,n;
#endif
    /* initialize */
    prev = NIL;
    this = ptr;

    /* mark this list */
    for (;;) {
#ifdef JGC

/* descend as far as we can */
    while (!(this->n_type & MARK))

	/* check cons and symbol nodes */
	if (((i = (this->n_type |= MARK) & TYPEFIELD) == CONS)||
	    (i == USTREAM)) {
	    if ((tmp = car(this)) != NIL) {
		this->n_type |= LEFT;
		rplaca(this,prev);
	    }
	    else if ((tmp = cdr(this)) != NIL)
		rplacd(this,prev);
	    else		/* both sides nil */
		break;
	    prev = this;	    /* step down the branch */
	    this = tmp;
	}
	else if ((i & ARRAY) != 0) {
		for (i = 0, n = getsize(this); i < n;)
		    if ((tmp = getelement(this,i++)) != NIL)
			if ((tmp->n_type & (ARRAY|MARK)) == ARRAY ||
			    tmp->n_type == CONS ||
			    tmp->n_type == USTREAM
#ifdef WINTERP
			    || (tmp->n_type & (HYBRID_ARRAY|ARRAY|MARK)) == HYBRID_ARRAY
#endif /* WINTERP */
			    )
			    mark(tmp);
			else tmp->n_type |= MARK;
		break;
	      }
#ifdef WINTERP
        else {
	  switch (i) {
#if 0
	  case XLTYPE_PIXMAP_REFOBJ: 
	    /* PIXMAP_REFOBJ constituents are all LVALs, therefore, they were mark'd normally, in ARRAY portion above */
#endif
	  case XLTYPE_CALLBACKOBJ:
	    i = CALLBACKOBJ_IDX_OF_FIRST_LVAL;
	    break;
	  case XLTYPE_TIMEOUTOBJ:
	    i = TIMEOUTOBJ_IDX_OF_FIRST_LVAL;
	    break;
	  case XLTYPE_EVHANDLEROBJ:
	    i = EVHANDLEROBJ_IDX_OF_FIRST_LVAL;
	    break;
	  case XLTYPE_FDINPUTCBOBJ:
	    i = FDINPUTCBOBJ_IDX_OF_FIRST_LVAL;
	    break;
	  case XLTYPE_WIDGETOBJ:
	    /* mark the special class LVAL at slot 0 ref'd by getclass(x)
	       note: don't really need the full test below...
	       we know it's a class object (ARRAY), so we could just mark that... */
	    if ((tmp = getelement(this,0)) != NIL)
	      if ((tmp->n_type & (ARRAY|MARK)) == ARRAY ||
		  tmp->n_type == CONS                   ||
		  tmp->n_type == USTREAM
		  || (tmp->n_type & (HYBRID_ARRAY|ARRAY|MARK)) == HYBRID_ARRAY)
		mark(tmp);
	      else tmp->n_type |= MARK;
	    i = WIDGETOBJ_IDX_OF_FIRST_LVAL; /* mark ivars at idx >= 2 (ivars from subclasses) */
	    break;
#ifdef WINTERP_XTANGO_WIDGET
	  case XLTYPE_TANGOIMAGEOBJ:
	    /* except for 'i = TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL' this is same as XLTYPE_WIDGETOBJ above */
	    if ((tmp = getelement(this,0)) != NIL)
	      if ((tmp->n_type & (ARRAY|MARK)) == ARRAY ||
		  tmp->n_type == CONS                   ||
		  tmp->n_type == USTREAM
		  || (tmp->n_type & (HYBRID_ARRAY|ARRAY|MARK)) == HYBRID_ARRAY)
		mark(tmp);
	      else tmp->n_type |= MARK;
	    i = TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL; /* mark ivars at idx >= 3 (ivars from subclasses) */
	    break;
#endif /* WINTERP_XTANGO_WIDGET */
	  default:
	    i = -1;	/* sentinel for no switch() cases matching */
	    break;
	  }

	  if (i != -1)	/* don't do anything if no switch() cases matched */
	    for (n = getsize(this); i < n;)
	      if ((tmp = getelement(this,i++)) != NIL)
		if ((tmp->n_type & (ARRAY|MARK)) == ARRAY ||
		    tmp->n_type == CONS                   ||
		    tmp->n_type == USTREAM
		    || (tmp->n_type & (HYBRID_ARRAY|ARRAY|MARK)) == HYBRID_ARRAY)
		  mark(tmp);
		else tmp->n_type |= MARK;
	  
	  break;
	}
#endif /* WINTERP */

	/* backup to a point where we can continue descending */
	for (;;)

	    /* make sure there is a previous node */
	    if (prev != NIL) {
		if (prev->n_type & LEFT) {	/* came from left side */
		    prev->n_type &= ~LEFT;
		    tmp = car(prev);
		    rplaca(prev,this);
		    if ((this = cdr(prev)) != NIL) {
			rplacd(prev,tmp);
			break;
		    }
		}
		else {				/* came from right side */
		    tmp = cdr(prev);
		    rplacd(prev,this);
		}
		this = prev;			/* step back up the branch */
		prev = tmp;
	    }

#else /* !defined(JGC) */

	/* descend as far as we can */
	while (!(this->n_flags & MARK))

	    /* check cons and symbol nodes */
	    if ((type = ntype(this)) == CONS || type == USTREAM ) { /* TAA fix*/
		if ((tmp = car(this)) != NIL) {
		    this->n_flags |= MARK|LEFT;
		    rplaca(this,prev);
		}
		else if ((tmp = cdr(this)) != NIL) {
		    this->n_flags |= MARK;
		    rplacd(this,prev);
		}
		else {				/* both sides nil */
		    this->n_flags |= MARK;
		    break;
		}
		prev = this;			/* step down the branch */
		this = tmp;
	    }

#ifdef WINTERP

	    /* mark other node types */
	    else {
		this->n_flags |= MARK;
		switch (type) {
		case XLTYPE_CALLBACKOBJ:
		  i = CALLBACKOBJ_IDX_OF_FIRST_LVAL;
		  break;
		case XLTYPE_TIMEOUTOBJ:
		  i = TIMEOUTOBJ_IDX_OF_FIRST_LVAL;
		  break;
		case XLTYPE_EVHANDLEROBJ:
		  i = EVHANDLEROBJ_IDX_OF_FIRST_LVAL;
		  break;
		case XLTYPE_FDINPUTCBOBJ:
		  i = FDINPUTCBOBJ_IDX_OF_FIRST_LVAL;
		  break;
		case XLTYPE_WIDGETOBJ:
		  /* 
		   * An XLTYPE_WIDGETOBJ is just like a OBJECT node with slot 0
		   * being the class, and the other slots being instance
		   * variables. class WIDGET_CLASS defines a special instance
		   * variable at slot 1 holding the WidgetID. Since that slot
		   * isn't an LVAL, it should not be mark()'d. Any additional
		   * slots means that the WIDGETOBJ was subclassed and new
		   * instance variables were added in the subclass which need to
		   * be marked.
		   */
		  if ((tmp = getelement(this, 0)) != NIL)
		    mark(tmp);
		  i = WIDGETOBJ_IDX_OF_FIRST_LVAL; /* mark ivars at idx >= 2 (ivars from subclasses) */
		  break;
#ifdef WINTERP_XTANGO_WIDGET
		case XLTYPE_TANGOIMAGEOBJ:
		  /* see comments for XLTYPE_WIDGETOBJ above... this is similar... */
		  if ((tmp = getelement(this, 0)) != NIL)
		    mark(tmp);
		  i = TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL; /* mark ivars at idx >= 3 (ivars from subclasses) */
		  break;
#endif /* WINTERP_XTANGO_WIDGET */
		case XLTYPE_PIXMAP_REFOBJ:
		case SYMBOL:
		case OBJECT:
		case VECTOR:
		case CLOSURE:
#ifdef STRUCTS
		case STRUCT:
#endif
#ifdef COMPLX
		case COMPLEX:
#endif
		  i = 0;	/* start marking LVAL's at index 0 */
		  break;
		default:
		  i = -1;	/* sentinel for no switch() cases matching */
		  break;
		}
		if (i != -1)	/* don't do anything if no switch() cases matched */
		  for (n = getsize(this); i < n;)
		    if ((tmp = getelement(this,i++)) != NIL)
		      mark(tmp);
		break;
	    }

#else /* !defined(WINTERP) */

	    /* mark other node types */
	    else {
		this->n_flags |= MARK;
		switch (type) {
		case SYMBOL:
		case OBJECT:
		case VECTOR:
		case CLOSURE:
#ifdef STRUCTS
		case STRUCT:
#endif
#ifdef COMPLX
		case COMPLEX:
#endif

		    for (i = 0, n = getsize(this); --n >= 0; ++i)
			if ((tmp = getelement(this,i)) != NIL)
			    mark(tmp);
		    break;
		}
		break;
	    }

#endif /* WINTERP */

	/* backup to a point where we can continue descending */
	for (;;)

	    /* make sure there is a previous node */
	    if (prev != NIL) {
		if (prev->n_flags & LEFT) {	/* came from left side */
		    prev->n_flags &= ~LEFT;
		    tmp = car(prev);
		    rplaca(prev,this);
		    if ((this = cdr(prev)) != NIL) {
			rplacd(prev,tmp);
			break;
		    }
		}
		else {				/* came from right side */
		    tmp = cdr(prev);
		    rplacd(prev,this);
		}
		this = prev;			/* step back up the branch */
		prev = tmp;
	}
#endif /* JGC */

	    /* no previous node, must be done */
	    else
		return;
    }
}

/* sweep - sweep all unmarked nodes and add them to the free list */
LOCAL VOID NEAR sweep()
{
    SEGMENT *seg;
    LVAL p;
    int n;

    /* empty the free list */
    fnodes = NIL;
    nfree = 0L;

    /* add all unmarked nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	if (seg == fixseg || seg == charseg)
#ifdef JGC
	    {
	    /* remove marks from segments */
	    p = &seg->sg_nodes[0];
	    for (n = seg->sg_size; --n >= 0;)
		(p++)->n_type &= ~MARK;
	    continue;
	}
#else
	    continue; /* don't sweep fixed segments */
#endif
	p = &seg->sg_nodes[0];
#ifdef JGC
	for (n = seg->sg_size; --n >= 0;)
	    if (p->n_type & MARK)
		(p++)->n_type &= ~MARK;
	    else {
		switch (ntype(p)&TYPEFIELD) {
#else
	for (n = seg->sg_size; --n >= 0; ++p)
	    if (!(p->n_flags & MARK)) {
		switch (ntype(p)) {
#endif
		case STRING:
			if (getstring(p) != NULL) {
			    total -= (long)getslength(p)+1;
			    MFREE(getstring(p));
			}
			break;
		case STREAM:
			if (getfile(p) != CLOSED
			    && getfile(p) != STDIN
			    && getfile(p) != STDOUT
			    && getfile(p) != CONSOLE)/* taa fix - dont close stdio */
			    OSCLOSE(getfile(p));
			break;
#if (defined(UNIX) || defined(WINTERP))
		case XLTYPE_PIPE: /* same as STREAM, except that pipes must be closed w/ pclose() */
			if (getfile(p) != CLOSED)
			  pclose(getfile(p));	
			break;
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
		case XLTYPE_XmString:
			Wxms_Garbage_Collect_XmString(p);
			break;
		case XLTYPE_Pixmap:
			Wpm_Decr_Refcount_Or_Free_Pixmap(p); /* Tell Motif that the X11 Pixmap is no longer ref'd */
			break;
#ifdef WINTERP_XTANGO_WIDGET
                case XLTYPE_TANGO_PATH:
			Xtango_Context_Remove_TANGO_PATH(p); /* deallocate TANGO_PATH structure */
			break;
                case XLTYPE_TANGO_TRANS:
			Xtango_Context_Remove_TANGO_TRANS(p); /* deallocate TANGO_TRANS structure, remove lval from WINTERP_TANGO_CONTEXT */
			break;
#endif /* WINTERP_XTANGO_WIDGET */
	        case XLTYPE_FDINPUTCBOBJ:
			if (get_fdinputcb_readbuf(p) != (char*) NULL)
			  XtFree((char*) get_fdinputcb_readbuf(p));
			/* fall through to rest of vector-based type treatment */
                case XLTYPE_WIDGETOBJ:
#ifdef WINTERP_XTANGO_WIDGET
	        case XLTYPE_TANGOIMAGEOBJ:
#endif /* WINTERP_XTANGO_WIDGET */
	        case XLTYPE_TIMEOUTOBJ:
		case XLTYPE_CALLBACKOBJ:
		case XLTYPE_PIXMAP_REFOBJ:
                case XLTYPE_EVHANDLEROBJ:
#endif /* WINTERP */
		case SYMBOL:
		case OBJECT:
		case VECTOR:
		case CLOSURE:
#ifdef STRUCTS
		case STRUCT:
#endif
#ifdef COMPLX
		case COMPLEX:
#endif
			if (p->n_vsize) {
			    total -= (long)p->n_vsize * sizeof(LVAL);
			    MFREE(p->n_vdata);
			}
			break;
		}
		p->n_type = FREE;
		rplaca(p,NIL);
		rplacd(p,fnodes);
#ifdef JGC
		fnodes = p++;
		nfree++;
	    }
#else
		fnodes = p;
		nfree += 1L;
	    }
	    else
		p->n_flags &= ~MARK;
#endif
    }
}

/* addseg - add a segment to the available memory */
LOCAL int NEAR addseg()
{
    SEGMENT *newseg;
    LVAL p;
    int n;

    /* allocate the new segment */
    if (anodes == 0 || (newseg = newsegment(anodes)) == NULL)
	return (FALSE);

    /* add each new node to the free list */
    p = &newseg->sg_nodes[0];
    for (n = anodes; --n >= 0; ++p) {
	rplacd(p,fnodes);
	fnodes = p;
    }

    /* return successfully */
    return (TRUE);
}

/* newsegment - create a new segment (only called here and in xlimage.c) */
SEGMENT *newsegment(n)
  int n;
{
    SEGMENT *newseg;

    /* allocate the new segment */
    if ((newseg = (SEGMENT *)CALLOC(1,segsize(n))) == NULL)
	return (NULL);

    /* initialize the new segment */
    newseg->sg_size = n;
    newseg->sg_next = NULL;
    if (segs != NULL)
	lastseg->sg_next = newseg;
    else
	segs = newseg;
    lastseg = newseg;

    /* update the statistics */
    total += (long)segsize(n);
    nnodes += (long)n;
    nfree += (long)n;
    ++nsegs;

    /* return the new segment */
    return (newseg);
}

/* stats - print memory statistics */
#ifdef ANSI
static void NEAR stats(void)
#else
LOCAL VOID stats()
#endif
{
    sprintf(buf,"Nodes:	      %ld\n",nnodes); stdputstr(buf);
    sprintf(buf,"Free nodes:  %ld\n",nfree);  stdputstr(buf);
    sprintf(buf,"Segments:    %d\n",nsegs);   stdputstr(buf);
    sprintf(buf,"Allocate:    %d\n",anodes);  stdputstr(buf);
    sprintf(buf,"Total:	      %ld\n",total);  stdputstr(buf);
    sprintf(buf,"Collections: %d\n",gccalls); stdputstr(buf);
}

/* xgc - xlisp function to force garbage collection */
LVAL xgc()
{
    /* make sure there aren't any arguments */
    xllastarg();

    /* garbage collect */
    gc();

    /* return nil */
    return (NIL);
}

/* xexpand - xlisp function to force memory expansion */
LVAL xexpand()
{
    LVAL num;
    FIXTYPE n,i;

    /* get the new number to allocate */
    if (moreargs()) {
	num = xlgafixnum();
	n = getfixnum(num);
	/* make sure there aren't any more arguments */
	xllastarg();
    }
    else
	n = 1;

    /* allocate more segments */
    for (i = 0; i < n; i++)
	if (!addseg())
	    break;

    /* return the number of segments added */
    return (cvfixnum((FIXTYPE)i));
}

/* xalloc - xlisp function to set the number of nodes to allocate */
LVAL xalloc()
{
    FIXTYPE n;	/* TAA MOD -- prevent overflow */
    int oldn;

    /* get the new number to allocate */
    n = getfixnum(xlgafixnum());

    /* make sure there aren't any more arguments */
    if (xlargc > 1) xltoomany();    /* but one more is OK, TAA MOD */

    /* Place limits on argument by clipping to reasonable values  TAA MOD */
    if (n > ((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node))
	n = ((long)MAXSLEN - sizeof(SEGMENT))/sizeof(struct node);
    else if (n < 1000)
	n = 1000;   /* arbitrary */

    /* set the new number of nodes to allocate */
    oldn = anodes;
    anodes = (int)n;

    /* return the old number */
    return (cvfixnum((FIXTYPE)oldn));
}

/* xmem - xlisp function to print memory statistics */
LVAL xmem()
{
    /* allow one argument for compatiblity with common lisp */
    if (xlargc > 1) xltoomany();    /* TAA Mod */

    /* print the statistics */
    stats();

    /* return nil */
    return (NIL);
}

#ifdef SAVERESTORE
/* xsave - save the memory image */
LVAL xsave()
{
    char *name;

    /* get the file name, verbose flag and print flag */
    name = getstring(xlgetfname());
    xllastarg();

    /* save the memory image */
    return (xlisave(name) ? true : NIL);
}

/* xrestore - restore a saved memory image */
LVAL xrestore()
{
    extern jmp_buf top_level;
    char *name;

    /* get the file name, verbose flag and print flag */
    name = getstring(xlgetfname());
    xllastarg();

    /* restore the saved memory image */
    if (!xlirestore(name))
	return (NIL);

    /* return directly to the top level */
    dbgputstr("[ returning to the top level ]\n");  /* TAA MOD --was std out*/
    longjmp(top_level,1);
    return (NIL);   /* never executed, but avoids warning message */
}

#endif

#ifdef COMPLX
/* From XLISP-STAT, Copyright (c) 1988 Luke Tierney */

LVAL newicomplex(real, imag)
	FIXTYPE real, imag;
{
  LVAL val;

  if (imag == 0) val = cvfixnum(real);
  else {
    xlsave1(val);
    val = newvector(2);
    val->n_type = COMPLEX;
    setelement(val, 0, cvfixnum(real));
    setelement(val, 1, cvfixnum(imag));
    xlpop();
  }
  return(val);
}

LVAL newdcomplex(real, imag)
	double real, imag;
{
  LVAL val;

  xlsave1(val);
  val = newvector(2);
  val->n_type = COMPLEX;
  setelement(val, 0, cvflonum((FLOTYPE) real));
  setelement(val, 1, cvflonum((FLOTYPE) imag));
  xlpop();
  return(val);
}

/* newcomplex - allocate and initialize a new object */
LVAL newcomplex(real,imag)
  LVAL real,imag;
{
  if (fixp(real) && fixp(imag))
    return(newicomplex(getfixnum(real), getfixnum(imag)));
  else
    return(newdcomplex(makefloat(real), makefloat(imag)));
}

#endif
