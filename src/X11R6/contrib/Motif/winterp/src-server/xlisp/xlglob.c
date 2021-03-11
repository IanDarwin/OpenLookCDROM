/* -*-C-*-
********************************************************************************
*
* File:         xlglob.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlglob.c,v 2.5 1994/06/06 15:59:16 npm Exp $
* Description:  xlisp global variables
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:24 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlglob.c,v 2.5 1994/06/06 15:59:16 npm Exp $";

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

#if (defined(UNIX) || defined(WINTERP))
LVAL a_PIPE=NULL;
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP

/* additional type names for WINTERP */
LVAL a_XtAccelerators=NULL, a_XtTranslations=NULL, a_XEvent=NULL, a_Window=NULL,
  a_Pixel=NULL, a_Pixmap=NULL, a_XImage=NULL, a_XmString=NULL, a_XmFontList=NULL,
  a_XT_RESOURCE=NULL, a_CALLBACKOBJ=NULL, a_TIMEOUTOBJ=NULL, a_PIXMAP_REFOBJ=NULL,
  a_WIDGETOBJ=NULL, a_EVHANDLEROBJ=NULL, a_FDINPUTCBOBJ=NULL;

#ifdef WINTERP_XTANGO_WIDGET
LVAL a_TANGOIMAGEOBJ=NULL, a_TANGO_PATH=NULL, a_TANGO_TRANS=NULL;
#endif /* WINTERP_XTANGO_WIDGET */


/* A vector that is marked so that winterp objects referenced inside MOTIF
   or Xtoolkit code don't get garbage collected (see w_savedobjs.c) */
LVAL v_savedobjs=NULL;

/* *ERROR-HOOK* is a hook (ala *eval-hook*, *apply-hook*, *gc-hook*) which
   calls the defined hook closure when an error occurs. (xldbug.c) */
LVAL s_errhook=NULL;

#endif /* WINTERP */

/* symbols */
#ifdef NILSYMBOL
struct node isnil;
#endif

LVAL true=NULL,obarray=NULL;
LVAL s_unbound=NIL,s_dot=NULL;
LVAL s_quote=NULL,s_function=NULL;
LVAL s_bquote=NULL,s_comma=NULL,s_comat=NULL;
LVAL s_evalhook=NULL,s_applyhook=NULL,s_tracelist=NULL;
LVAL s_lambda=NULL,s_macro=NULL;
LVAL s_stdin=NULL,s_stdout=NULL,s_stderr=NULL,s_debugio=NULL,s_traceout=NULL;
LVAL s_rtable=NULL;
LVAL s_tracenable=NULL,s_tlimit=NULL,s_breakenable=NULL;
LVAL s_setf=NULL,s_car=NULL,s_cdr=NULL,s_nth=NULL,s_aref=NULL,s_get=NULL;
LVAL s_svalue=NULL,s_sfunction=NULL,s_splist=NULL;
LVAL s_eql=NULL,s_gcflag=NULL,s_gchook=NULL;
LVAL s_ifmt=NULL,s_ffmt=NULL;
LVAL s_1plus=NULL,s_2plus=NULL,s_3plus=NULL;
LVAL s_1star=NULL,s_2star=NULL,s_3star=NULL;
LVAL s_minus=NULL,s_printcase=NULL;
#ifdef PRINDEPTH
LVAL s_printlevel=NULL, s_printlength=NULL;
#endif
#ifdef DOSINPUT
LVAL s_dosinput=NULL;
#endif
#ifdef DISPMACRO
LVAL s_dispmacros=NULL;
#endif
#ifdef COMMONLISPF
LVAL s_elt = NULL;
LVAL a_list=NULL, a_number=NULL, a_null=NULL, a_atom=NULL, a_anystream=NULL;
LVAL s_and=NULL, s_or=NULL, s_not=NULL, s_satisfies=NULL, s_member=NULL;
#ifdef STRUCTS
LVAL a_struct = NULL;
#endif
#endif
#ifdef HASHFCNS
LVAL s_gethash = NULL, a_hashtable = NULL, k_size = NULL;
#endif
#ifdef COMPLX
LVAL a_complex = NULL;
#endif
#ifdef STRUCTS
LVAL s_strtypep=NULL, s_mkstruct=NULL, s_cpystruct=NULL;
LVAL s_strref=NULL, s_strset=NULL;
LVAL s_x=NULL, s_s=NULL, s_sslots=NULL;
#endif
#ifdef RANDOM
LVAL s_randomstate=NULL, a_randomstate=NULL, k_data=NULL;
#endif

/* keywords */
LVAL k_test=NULL,k_tnot=NULL;
LVAL k_wspace=NULL,k_const=NULL,k_nmacro=NULL,k_tmacro=NULL;
LVAL k_sescape=NULL,k_mescape=NULL;
LVAL k_direction=NULL,k_input=NULL,k_output=NULL;
LVAL k_start=NULL,k_end=NULL,k_1start=NULL,k_1end=NULL;
LVAL k_2start=NULL,k_2end=NULL,k_count=NULL;
LVAL k_verbose=NULL,k_print=NULL;
LVAL k_upcase=NULL,k_downcase=NULL;
#ifdef BETTERIO
LVAL k_io=NULL, k_elementtype=NULL;
LVAL s_termio=NULL, k_exist=NULL, k_nexist=NULL, k_error=NULL;
LVAL k_rename=NULL, k_newversion=NULL, k_overwrite=NULL, k_append=NULL;
LVAL k_supersede=NULL, k_rendel=NULL, k_probe=NULL, k_create=NULL;
#endif
#ifdef TIERNEY
LVAL k_ivalue=NULL;
#endif
#ifdef KEYARG
LVAL k_key=NULL;
#endif
#ifdef STRUCTS
LVAL k_concname=NULL, k_include=NULL;
#endif


/* lambda list keywords */
LVAL lk_optional=NULL,lk_rest=NULL,lk_key=NULL,lk_aux=NULL;
LVAL lk_allow_other_keys=NULL;

/* type names */
LVAL a_subr=NULL,a_fsubr=NULL;
LVAL a_cons=NULL,a_symbol=NULL,a_fixnum=NULL,a_flonum=NULL;
LVAL a_string=NULL,a_object=NULL,a_stream=NULL,a_vector=NULL;
LVAL a_closure=NULL,a_char=NULL,a_ustream=NULL;

#ifdef STATICSTK
/* evaluation variables */
LVAL * NEAR xlstkbase[EDEPTH];
LVAL * NEAR *xlstack = NULL;
LVAL xlenv=NULL,xlfenv=NULL,xldenv=NULL;

/* argument stack */
LVAL NEAR xlargstkbase[ADEPTH]; /* argument stack */
LVAL NEAR *xlfp = NULL;		/* argument frame pointer */
LVAL NEAR *xlsp = NULL;		/* argument stack pointer */
LVAL NEAR *xlargv = NULL;	/* current argument vector */
#else
/* evaluation variables */
LVAL **xlstack = NULL,**xlstkbase = NULL,**xlstktop = NULL;
LVAL xlenv=NULL,xlfenv=NULL,xldenv=NULL;

/* argument stack */
LVAL *xlargstkbase = NULL;  /* argument stack base */
LVAL *xlargstktop = NULL;   /* argument stack top */
LVAL *xlfp = NULL;	/* argument frame pointer */
LVAL *xlsp = NULL;	/* argument stack pointer */
LVAL *xlargv = NULL;	    /* current argument vector */
#endif
int xlargc = 0;		/* current argument count */

/* exception handling variables */
CONTEXT *xlcontext = NULL;  /* current exception handler */
CONTEXT *xltarget = NULL;   /* target context (for xljump) */
LVAL xlvalue=NULL;	/* exception value (for xljump) */
int xlmask=0;		/* exception type (for xljump) */

/* debugging variables */
int xldebug = 0;	/* debug level */
int xlsample = 0;	/* control character sample rate */
int xltrcindent = 0;	    /* trace indent level */

/* gensym variables */
char gsprefix[STRMAX+1] = { 'G',0 };	/* gensym prefix string */
FIXTYPE gsnumber = 1;	    /* gensym number */

/* i/o variables */
int xlfsize = 0;	/* flat size of current print call */
FILEP tfp = CLOSED;	/* transcript file pointer */
int redirectout = FALSE;    /* output is redirected */
int redirectin = FALSE;	    /* input is redirected */

/* general purpose string buffer */
char buf[STRMAX+1] = { 0 };

#ifdef FILETABLE
FILETABLETYPE filetab[FTABSIZE] =
{{stdin,"(stdin)"},
 {stdout,"(stdout)"},
 {stderr,"(console)"},
 {0,""}};
#endif
