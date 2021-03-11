/* -*-C-*-
********************************************************************************
*
* File:         xlimage.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlimage.c,v 2.5 1994/06/06 15:59:17 npm Exp $
* Description:  xlisp memory image save/restore functions
*		Note: For xlisp 2.1c TAA (?) modified so that offset is in
*		sizeof(node) unit.
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:21 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlimage.c,v 2.5 1994/06/06 15:59:17 npm Exp $";

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

#ifdef SAVERESTORE

#define FILENIL ((OFFTYPE)0)	/* value of NIL in a file */

/* external variables */
extern LVAL obarray,xlenv,xlfenv,xldenv,s_gchook,s_gcflag;
extern long nnodes,nfree,total;
extern int anodes,nsegs,gccalls;
extern struct segment *segs,*lastseg,*fixseg,*charseg;
extern CONTEXT *xlcontext;
extern LVAL fnodes;
extern int ftabsize;	/* TAA MOD -- added validity check */

/* local variables */
static OFFTYPE off,foff;
static FILEP fp;

/* forward declarations */
#ifdef ANSI
LOCAL OFFTYPE NEAR readptr(void); /* NPM: changed this to LOCAL */
LOCAL OFFTYPE NEAR cvoptr(LVAL p); /* NPM: changed this to LOCAL */
LOCAL LVAL NEAR cviptr(OFFTYPE o); /* NPM: changed this to LOCAL */
LOCAL void NEAR freeimage(void); /* NPM: changed this to LOCAL */
LOCAL void NEAR setoffset(void); /* NPM: changed this to LOCAL */
LOCAL void NEAR writenode(LVAL node); /* NPM: changed this to LOCAL */
LOCAL void NEAR writeptr(OFFTYPE off); /* NPM: changed this to LOCAL */
LOCAL void NEAR readnode(int type, LVAL node); /* NPM: changed this to LOCAL */
#else
LOCAL OFFTYPE readptr();	/* NPM: changed this to LOCAL */
LOCAL OFFTYPE cvoptr();		/* NPM: changed this to LOCAL */
LOCAL LVAL cviptr();		/* NPM: changed this to LOCAL */
LOCAL VOID freeimage();		/* NPM: changed this to LOCAL */
LOCAL VOID setoffset();		/* NPM: changed this to LOCAL */
LOCAL VOID writenode();		/* NPM: changed this to LOCAL */
LOCAL VOID writeptr();		/* NPM: changed this to LOCAL */
LOCAL VOID readnode();		/* NPM: changed this to LOCAL */
#endif

/* xlisave - save the memory image */
int xlisave(fname)
  char *fname;
{
    char fullname[STRMAX+1];
    SEGMENT *seg;
    int n,i,max;
    LVAL p;

    /* default the extension */
    if (needsextension(fname)) {
	strcpy(fullname,fname);
	strcat(fullname,".wks");
	fname = fullname;
    }

    /* open the output file */

    if ((fp = OSBOPEN(fname,CREATE_WR)) == CLOSED)
	return (FALSE);

    /* first call the garbage collector to clean up memory */
    gc();

    /* write out size of ftab (used as validity check) TAA MOD */
#ifdef NILSYMBOL
    writeptr((OFFTYPE)ftabsize);
#else
    writeptr((OFFTYPE)(ftabsize+2));
#endif

    /* write out the pointer to the *obarray* symbol */
    writeptr(cvoptr(obarray));

#ifdef NILSYMBOL
    /* write out components of NIL other than value, which must be NIL */
    writeptr(cvoptr(getfunction(NIL)));
    writeptr(cvoptr(getplist(NIL)));
    writeptr(cvoptr(getpname(NIL)));
#endif

    /* setup the initial file offsets */
    off = foff = (OFFTYPE)2;

    /* write out all nodes that are still in use */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	p = &seg->sg_nodes[0];
	for (n = seg->sg_size; --n >= 0; ++p, off++)
	    switch (ntype(p)) {
	    case FREE:
		break;
	    case CONS:
	    case USTREAM:
		setoffset();
		OSPUTC(p->n_type,fp);
		writeptr(cvoptr(car(p)));
		writeptr(cvoptr(cdr(p)));
		foff++;
		break;
#if 0 /* WINTERP comment */
	      case XLTYPE_PIPE:
		/* punt -- write out closed pipe/file node */
		break;
	      case XLTYPE_XtAccelerators:
		/* punt -- write out NULL accelerator table ??? safe ??? */
		break;
	      case XLTYPE_XtTranslations:
		/* punt -- write out NIL accelerator table ??? safe ??? */
		break;
	      case XLTYPE_XEvent:
		/* punt -- write out NULL event ??? safe ??? */
		break;
	      case XLTYPE_Window:
		/* punt -- write out NULL window ??? safe ??? */
		break;
	      case XLTYPE_Pixel:
		/* punt -- write out NULL Pixel ??? safe ??? */
		break;
	      case XLTYPE_Pixmap:
		/* punt -- write out NULL Pixmap ??? safe ??? */
		break;
	      case XLTYPE_XImage:
		/* punt -- write out NULL XImage ??? safe ??? */
		break;
	      case XLTYPE_XmString:
		/* punt -- write out NULL XmString ??? safe ??? */
		/* later -- write out using XmCvtXmStringToCT(),
		   read back in with XmCvtCTToXmString() */
		break;
	      case XLTYPE_XmFontList:
		/* punt -- write out NULL XmFontList ??? safe ??? */
		break;
	      case XLTYPE_XT_RESOURCE:
		/* write out symbol assoc'd with resource. */
		break;
#ifdef WINTERP_XTANGO_WIDGET
	      case XLTYPE_TANGO_PATH:
		/* write out tango path description, similar to existing TANGOpath_store()/TANGOpath_load() */
		break;
	      case XLTYPE_TANGO_TRANS:
		/* punt -- write out NULL TANGO_TRANS ??? safe ??? */
		break;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP comment */
	    default:
		setoffset();
		writenode(p);
		break;
	}
    }

    /* write the terminator */
    OSPUTC(FREE,fp);
    writeptr((OFFTYPE)0);

    /* write out data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	p = &seg->sg_nodes[0];
	for (n = seg->sg_size; --n >= 0; ++p)
	    switch (ntype(p)) {
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
#if 0 /* WINTERP comment */
	      case XLTYPE_CALLBACKOBJ:
		i = CALLBACKOBJ_IDX_OF_FIRST_LVAL;
		/* write out NULLs for idx 0 thru i-1,
		 * and begin writing LVALs for i <= idx < length */
		break;
	      case XLTYPE_TIMEOUTOBJ:
		i = TIMEOUTOBJ_IDX_OF_FIRST_LVAL;
		/* write out NULLs for idx 0 thru i-1,
		 * and begin writing LVALs for i <= idx < length */
		break;
	      case XLTYPE_EVHANDLEROBJ:
		i = EVHANDLEROBJ_IDX_OF_FIRST_LVAL;
		/* write out NULLs for idx 0 thru i-1,
		 * and begin writing LVALs for i <= idx < length */
		break;
	      case XLTYPE_FDINPUTCBOBJ:
		i = FDINPUTCBOBJ_IDX_OF_FIRST_LVAL;
		/* write out NULLs for idx 0 thru i-1,
		 * and begin writing LVALs for i <= idx < length */
		break;
	      case XLTYPE_WIDGETOBJ:
		if ((tmp = getelement(this, 0)) != NIL)
		  ???writenode???(tmp);
		i = WIDGETOBJ_IDX_OF_FIRST_LVAL;
		/* 
		 * write out first LVAL at idx 0,
		 * write out NULLs for idx 1 thru i-1,
		 * write out LVALs for i <= idx < length
		 */
		break;
#ifdef WINTERP_XTANGO_WIDGET
	      case XLTYPE_TANGOIMAGEOBJ:
		if ((tmp = getelement(this, 0)) != NIL)
		  ???writenode???(tmp);
		i = TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL;
		/* 
		 * write out first LVAL at idx 0,
		 * write out NULLs for idx 1 thru i-1,
		 * write out LVALs for i <= idx < length
		 */
		break;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP comment */
		max = getsize(p);
		for (i = 0; i < max; ++i)
		    writeptr(cvoptr(getelement(p,i)));
		break;
	    case STRING:
		max = getslength(p)+1;
		OSWRITE(getstring(p),1,max,fp);
		break;
#if defined(BETTERIO) && defined(FILETABLE)
	    case STREAM:
		if (getfile(p) > CONSOLE ) {
		    OSWRITE(filetab[getfile(p)].name,1,FNAMEMAX,fp);
		    *(long *)buf = OSTELL(getfile(p));
		    OSWRITE(buf,1,sizeof(long),fp);
		}
		break;
#endif
	}
    }

    /* close the output file */
    OSCLOSE(fp);

    /* return successfully */
    return (TRUE);
}

/* xlirestore - restore a saved memory image */
int xlirestore(fname)
  char *fname;
{
    extern FUNDEF funtab[];
    char fullname[STRMAX+1];
    int n,i,max,type;
    SEGMENT *seg;
    LVAL p;

    /* default the extension */
    if (needsextension(fname)) {
	strcpy(fullname,fname);
	strcat(fullname,".wks");
	fname = fullname;
    }

    /* open the file */
#ifdef PATHNAMES
    if ((fp = ospopen(fname,FALSE)) == CLOSED)
#else
    if ((fp = OSBOPEN(fname,OPEN_RO)) == CLOSED)
#endif
	return (FALSE);

    /* Check for file validity	TAA MOD */
#ifdef NILSYMBOL
    if (readptr() != (OFFTYPE) ftabsize)
#else
    if (readptr() != (OFFTYPE) (ftabsize+2))
#endif
	{
	OSCLOSE(fp);	/* close it -- we failed */
	return (FALSE);
    }

    /* free the old memory image */
    freeimage();

    /* initialize */
    off = (OFFTYPE)2;
    total = nnodes = nfree = 0L;
    fnodes = NIL;
    segs = lastseg = NULL;
    nsegs = gccalls = 0;
    xlenv = xlfenv = xldenv = s_gchook = s_gcflag = NIL;
    xlstack = xlstkbase + EDEPTH;
    xlfp = xlsp = xlargstkbase;
    *xlsp++ = NIL;
    xlcontext = NULL;

    /* create the fixnum segment */
    if ((fixseg = newsegment(SFIXSIZE)) == NULL)
	xlfatal("insufficient memory - fixnum segment");

    /* create the character segment */
    if ((charseg = newsegment(CHARSIZE)) == NULL)
	xlfatal("insufficient memory - character segment");

    /* read the pointer to the *obarray* symbol */
    obarray = cviptr(readptr());

#ifdef NILSYMBOL
    /* read components of NIL other than value, which must be NIL */
    setfunction(NIL, cviptr(readptr()));
    setplist(NIL, cviptr(readptr()));
    setpname(NIL, cviptr(readptr()));
#endif

    /* read each node */
    while ((type = OSGETC(fp)) >= 0)
	switch (type) {
	case FREE:
	    if ((off = readptr()) == (OFFTYPE)0)
		goto done;
	    break;
	case CONS:
	case USTREAM:
	    p = cviptr(off);
	    p->n_type = type;
#ifndef JGC
	    p->n_flags = 0;
#endif
	    rplaca(p,cviptr(readptr()));
	    rplacd(p,cviptr(readptr()));
	    off++;
	    break;
	default:
	    readnode(type,cviptr(off));
	    off++;
	    break;
	}
done:

    /* read the data portion of SYMBOL/VECTOR/OBJECT/STRING/CLOSURE nodes */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
    p = &seg->sg_nodes[0];
    for (n = seg->sg_size; --n >= 0; ++p)
	switch (ntype(p)) {
#if 0 /* WINTERP comment */
	case XLTYPE_CALLBACKOBJ:
	  i = CALLBACKOBJ_IDX_OF_FIRST_LVAL;
	  /* read in NULLs for idx 0 thru i-1,
	   * and begin writing LVALs for i <= idx < length */
	  break;
	case XLTYPE_TIMEOUTOBJ:
	  i = TIMEOUTOBJ_IDX_OF_FIRST_LVAL;
	  /* read in NULLs for idx 0 thru i-1,
	   * and begin writing LVALs for i <= idx < length */
	  break;
	case XLTYPE_EVHANDLEROBJ:
	  i = EVHANDLEROBJ_IDX_OF_FIRST_LVAL;
	  /* read in NULLs for idx 0 thru i-1,
	   * and begin writing LVALs for i <= idx < length */
	  break;
	case XLTYPE_FDINPUTCBOBJ:
	  i = FDINPUTCBOBJ_IDX_OF_FIRST_LVAL;
	  /* read in NULLs for idx 0 thru i-1,
	   * and begin writing LVALs for i <= idx < length */
	  break;
	case XLTYPE_WIDGETOBJ:
	  if ((tmp = getelement(this, 0)) != NIL)
	    ???readnode???(tmp);
	  i = WIDGETOBJ_IDX_OF_FIRST_LVAL;
	  /* 
	   * read in first LVAL at idx 0,
	   * read in NULLs for idx 1 thru i-1,
	   * read in LVALs for i <= idx < length
	   */
	  break;
#ifdef WINTERP_XTANGO_WIDGET
	case XLTYPE_TANGOIMAGEOBJ:
	  if ((tmp = getelement(this, 0)) != NIL)
	    ???readnode???(tmp);
	  i = TANGOIMAGEOBJ_IDX_OF_FIRST_LVAL;
	  /* 
	   * read in first LVAL at idx 0,
	   * read in NULLs for idx 1 thru i-1,
	   * read in LVALs for i <= idx < length
	   */
	  break;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP comment */
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
	    max = getsize(p);
	    if ((p->n_vdata = (LVAL *)MALLOC(max * sizeof(LVAL))) == NULL)
		xlfatal("insufficient memory - vector");
	    total += (long)(max * sizeof(LVAL));
	    for (i = 0; i < max; ++i)
		setelement(p,i,cviptr(readptr()));
	    break;
	case STRING:
	    max = getslength(p)+1;
	    if ((p->n_string = (char *)MALLOC(max)) == NULL)
		xlfatal("insufficient memory - string");
	    total += (long)max;
	    if (OSREAD(getstring(p),1,max,fp) != max)
		xlfatal("image file corrupted");
	    break;
	case STREAM:
#if (defined(UNIX) || defined(WINTERP))
	case XLTYPE_PIPE:	/* won't work w/ FILETABLE ... */
#endif /* (defined(UNIX) || defined(WINTERP)) */
#if defined(BETTERIO) && defined(FILETABLE)
	    if (getfile(p) > CONSOLE) { /* actual file to modify */
		unsigned long fpos;
		FILEP f;

		if (OSREAD(buf, 1, FNAMEMAX, fp) != FNAMEMAX ||
		    OSREAD(&fpos, 1, sizeof(long), fp) != sizeof(long))
			xlfatal("image file corrupted");
		/* open file in same type, file must exist to succeed */
		f = ((p->n_sflags & S_BINARY)? OSBOPEN : OSAOPEN)
		    (buf, (p->n_sflags&S_FORWRITING)? OPEN_UPDATE: OPEN_RO);
		setfile(p, f);
		if (fp != CLOSED) { /* position to same point,
					or end if file too short */
		    OSSEEKEND(f);
		    if (OSTELL(f) > fpos) OSSEEK(f, fpos);
		}
	    }
	    break;
#else
	    setfile(p,NULL);
	    break;
#endif
	case SUBR:
	case FSUBR:
	    p->n_subr = funtab[getoffset(p)].fd_subr;
	    break;
	}
    }

    /* close the input file */
    OSCLOSE(fp);

    /* collect to initialize the free space */
    gc();

    /* lookup all of the symbols the interpreter uses */
    xlsymbols();

    /* return successfully */
    return (TRUE);
}

/* freeimage - free the current memory image */
LOCAL VOID NEAR freeimage()
{
    SEGMENT *seg,*next;
    FILEP fp;
    LVAL p;
    int n;

    /* free the data portion of SYMBOL/VECTOR/OBJECT/STRING nodes */
    for (seg = segs; seg != NULL; seg = next) {
    p = &seg->sg_nodes[0];
    for (n = seg->sg_size; --n >= 0; ++p)
	switch (ntype(p)) {
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
#ifdef WINTERP
	case XLTYPE_TIMEOUTOBJ:
	case XLTYPE_CALLBACKOBJ:
	case XLTYPE_PIXMAP_REFOBJ:
	case XLTYPE_WIDGETOBJ:
#ifdef WINTERP_XTANGO_WIDGET
	case XLTYPE_TANGOIMAGEOBJ:
#endif /* WINTERP_XTANGO_WIDGET */
	case XLTYPE_EVHANDLEROBJ:
	case XLTYPE_FDINPUTCBOBJ:
#endif /* WINTERP */
	    if (p->n_vsize)
		MFREE(p->n_vdata);
	    break;
	case STRING:
	    if (getstring(p)!=NULL)
		MFREE(getstring(p));
	    break;
	case STREAM:
	    if (((fp = getfile(p)) != CLOSED) &&
		(fp != STDIN && fp != STDOUT && fp != CONSOLE))	 /* TAA BUG FIX */
	    OSCLOSE(fp);
	    break;
#if (defined(UNIX) || defined(WINTERP))
	  case XLTYPE_PIPE:
	    if (fp = getfile(p))
	      pclose(getfile(p));
	    break;
#endif /* (defined(UNIX) || defined(WINTERP)) */
#if 0 /* WINTERP comment */
    /* perhaps the best thing to do here would be go through v_savedobjs[] 
     * and destroy all widgets found within (except for *TOPLEVEL_WIDGET*) ...
     * that would take care of ensuring that none of the following are
     * referenced in a widget, thereby allowing us to free pixmaps, pixels, etc..
     */
	  case XLTYPE_XtAccelerators:
	    /* how do you destroy an accelerator? */
	    break;
	  case XLTYPE_XtTranslations:
	    /* how do you destroy a translation? */
		break;
	  case XLTYPE_XEvent:
	    /* do nothing. */
	    break;
	  case XLTYPE_Window:
	    /* do nothing? */
	    break;
	  case XLTYPE_Pixel:
	    /* do nothing? or Free the color? */;
	  case XLTYPE_Pixmap:
	    /* this fn should check to make sure of dealloc method for pixmap */
	    Wpm_Decr_Refcount_Or_Free_Pixmap(p); /* Tell Motif that the X11 Pixmap is no longer ref'd */
	    break;
	  case XLTYPE_XImage:
	    /* do nothing? free the pixmap? */
	    break;
	  case XLTYPE_XmString:
	    Wxms_Garbage_Collect_XmString(p);
	    break;
	  case XLTYPE_XmFontList:
	    /* free it?? */
	    break;
	  case XLTYPE_XT_RESOURCE:
	    /* do nothing. */
	    break;
#ifdef WINTERP_XTANGO_WIDGET
	  case XLTYPE_TANGO_PATH:
	    Xtango_Context_Remove_TANGO_PATH(p); /* deallocate TANGO_PATH structure */
	    break;
	  case XLTYPE_TANGO_TRANS:
	    Xtango_Context_Remove_TANGO_TRANS(p); /* deallocate TANGO_TRANS structure, remove lval from WINTERP_TANGO_CONTEXT */
	    break;
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP comment */
	}
    next = seg->sg_next;
    MFREE(seg);
    }
}

/* setoffset - output a positioning command if nodes have been skipped */
LOCAL VOID NEAR setoffset()
{
    if (off != foff) {
	OSPUTC(FREE,fp);
	writeptr(off);
	foff = off;
    }
}

/* writenode - write a node to a file */
LOCAL VOID NEAR writenode(node)
  LVAL node;
{
    OSPUTC(node->n_type,fp);
    OSWRITE(&node->n_info, sizeof(union ninfo), 1, fp);
#if defined(ALIGN32) & defined(SPECIALS)
    if (node->n_type == SYMBOL) OSPUTC(node->n_spflags,fp);
#endif
    foff++;
}

/* writeptr - write a pointer to a file */
LOCAL VOID NEAR writeptr(off)
  OFFTYPE off;
{
    OSWRITE(&off, sizeof(OFFTYPE), 1, fp);
}

/* readnode - read a node */
LOCAL VOID NEAR readnode(type,node)
  int type; LVAL node;
{
    node->n_type = type;
#ifndef JGC
    node->n_flags = 0;
#endif
    if (OSREAD(&node->n_info, sizeof(union ninfo), 1, fp) != 1)
	xlfatal("image file corrupted");
#if defined(ALIGN32) & defined(SPECIALS)
    if (type == SYMBOL) node->n_spflags = OSGETC(fp);
#endif
}

/* readptr - read a pointer */
LOCAL OFFTYPE NEAR readptr()
{
    OFFTYPE off;
    if(OSREAD(&off, sizeof(OFFTYPE), 1, fp) != 1)
	xlfatal("image file corrupted");
    return (off);
}

/* cviptr - convert a pointer on input */
LOCAL LVAL NEAR cviptr(o)
  OFFTYPE o;
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;

    /* check for nil */
    if (o == FILENIL)
	return (NIL);

    /* compute a pointer for this offset */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	if (o < off + (OFFTYPE)seg->sg_size)
	    return (seg->sg_nodes + (unsigned int)(o - off));
	off += (OFFTYPE)seg->sg_size;
    }

    /* create new segments if necessary */
    for (;;) {

    /* create the next segment */
	if ((seg = newsegment(anodes)) == NULL)
	    xlfatal("insufficient memory - segment");

    /* check to see if the offset is in this segment */
	if (o < off + (OFFTYPE)seg->sg_size)
	    return (seg->sg_nodes + (unsigned int)(o - off));
	off += (OFFTYPE)seg->sg_size;
    }
}

/* cvoptr - convert a pointer on output */
LOCAL OFFTYPE NEAR cvoptr(p)
  LVAL p;
{
    OFFTYPE off = (OFFTYPE)2;
    SEGMENT *seg;
    OFFTYPE np = CVPTR(p);

    /* check for nil */
    if (null(p))
	return (FILENIL);

    /* compute an offset for this pointer */
    for (seg = segs; seg != NULL; seg = seg->sg_next) {
	if (np >= CVPTR(&seg->sg_nodes[0]) &&
	    np <  CVPTR(&seg->sg_nodes[seg->sg_size]))
		return (off+ ((np-CVPTR(seg->sg_nodes))/sizeof(struct node)));
	    off += (OFFTYPE)seg->sg_size;
    }

    /* pointer not within any segment */
    xlerror("bad pointer found during image save",p);
    return (0); /* fake out compiler warning */
}
#endif


