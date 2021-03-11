/* -*-C-*-
********************************************************************************
*
* File:         xlinit.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlinit.c,v 2.5 1994/06/06 15:59:17 npm Exp $
* Description:  xlisp initialization module
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:18 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlinit.c,v 2.5 1994/06/06 15:59:17 npm Exp $";

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
extern LVAL true,s_dot,s_unbound,obarray;
extern LVAL s_quote,s_function,s_bquote,s_comma,s_comat;
extern LVAL s_lambda,s_macro;
extern LVAL s_stdin,s_stdout,s_stderr,s_debugio,s_traceout;
extern LVAL s_evalhook,s_applyhook,s_tracelist;
extern LVAL s_tracenable,s_tlimit,s_breakenable;
extern LVAL s_setf,s_car,s_cdr,s_nth,s_aref,s_get,s_eql;
extern LVAL s_svalue,s_sfunction,s_splist;
extern LVAL s_rtable,k_wspace,k_const,k_nmacro,k_tmacro;
extern LVAL k_sescape,k_mescape;
extern LVAL s_ifmt,s_ffmt,s_printcase;
extern LVAL s_1plus,s_2plus,s_3plus,s_1star,s_2star,s_3star,s_minus;
extern LVAL k_test,k_tnot;
extern LVAL k_direction,k_input,k_output;
#ifdef BETTERIO
extern LVAL k_io, k_elementtype;
extern LVAL s_termio, k_exist, k_nexist, k_error, k_rename, k_newversion;
extern LVAL k_overwrite, k_append, k_supersede, k_rendel, k_probe, k_create;
#endif
extern LVAL k_start,k_end,k_1start,k_1end,k_2start,k_2end;
extern LVAL k_verbose,k_print,k_count,k_upcase,k_downcase;
extern LVAL lk_optional,lk_rest,lk_key,lk_aux,lk_allow_other_keys;
extern LVAL a_subr,a_fsubr,a_cons,a_symbol;
extern LVAL a_fixnum,a_flonum,a_string,a_stream,a_object;
extern LVAL a_vector,a_closure,a_char,a_ustream;
extern LVAL s_gcflag,s_gchook;
#ifdef COMMONLISPF
extern LVAL s_elt;
extern LVAL a_list, a_number, a_null, a_atom, a_anystream;
extern LVAL s_and, s_or, s_not, s_satisfies, s_member;
#ifdef STRUCTS
extern LVAL a_struct;
#endif
#endif
#ifdef HASHFCNS
extern LVAL s_gethash, a_hashtable, k_size;
#endif
#ifdef TIERNEY
extern LVAL k_ivalue;
#endif
#ifdef KEYARG
extern LVAL k_key;
#endif
#ifdef COMPLX
extern LVAL a_complex;
#endif
#ifdef DISPMACRO
extern LVAL s_dispmacros;
#endif
#ifdef PRINDEPTH
extern LVAL s_printlevel,s_printlength;
#endif
#ifdef DOSINPUT
extern LVAL s_dosinput;	    /* TAA mod */
#endif
#ifdef STRUCTS	/* TAA mod -- make constants to save xlenters */
extern LVAL s_strtypep, s_mkstruct, s_cpystruct, s_strref, s_strset;
extern LVAL s_x, s_s, s_sslots;
extern LVAL k_concname, k_include;
#endif
#ifdef RANDOM
extern LVAL s_randomstate, a_randomstate, k_data;
#endif

extern FUNDEF funtab[];

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
extern LVAL s_errhook;
#endif /* WINTERP */

/* Forward declarations */
#ifdef ANSI
LOCAL FORWARD VOID NEAR initwks(void); /* NPM: changed this to LOCAL */
#else
LOCAL FORWARD VOID initwks();	/* NPM: changed this to LOCAL */
#endif

/* TAA MOD -- most compilers I use will generate better code calling
   a static function. Because we have many calls of xlenter here, (which
   will only execute once per session), I'm calling xlenter through a
   static function senter() */

#ifdef ANSI
LVAL NEAR senter(char NEAR *str)
{
    return xlenter(str);
}
#else
#define senter(x) xlenter(x)
#endif


/* xlinit - xlisp initialization routine */
int xlinit(resfile) /* TAA Mod -- return true if load of init.lsp needed */
	char *resfile;
{
    /* initialize xlisp (must be in this order) */
    xlminit();	/* initialize xldmem.c */
    xldinit();	/* initialize xldbug.c */

/* finish initializing */
#ifdef SAVERESTORE
    if (*resfile=='\0' || !xlirestore(resfile)) {
	initwks();
	return TRUE;
    }
    return FALSE;
#else
    initwks();
    return TRUE;
#endif
}

/* initwks - build an initial workspace */
LOCAL VOID NEAR initwks()
{
    FUNDEF *p;
    int i;

    xlsinit();	/* initialize xlsym.c */
    xlsymbols();/* enter all symbols used by the interpreter */
    xlrinit();	/* initialize xlread.c */
    xloinit();	/* initialize xlobj.c */

    /* setup defaults */
#ifdef NILSYMBOL
    /*can't mark as unbound until *unbound* created*/
    setfunction(s_unbound, s_unbound);
    setfunction(obarray, s_unbound);
    setfunction(NIL, s_unbound);
#endif

#ifdef WINTERP
    setsvalue(s_errhook,NIL);		/* no errhook function */
#endif /* WINTERP */

    setsvalue(s_evalhook,NIL);		/* no evalhook function */
    setsvalue(s_applyhook,NIL);		/* no applyhook function */
    setsvalue(s_tracelist,NIL);		/* no functions being traced */
    setsvalue(s_tracenable,NIL);	/* traceback disabled */
    setsvalue(s_tlimit,NIL);		/* trace limit infinite */
    setsvalue(s_breakenable,NIL);	/* don't enter break loop on errors */
    setsvalue(s_gcflag,NIL);		/* don't show gc information */
    setsvalue(s_gchook,NIL);		/* no gc hook active */

/* it seems just a waste of memory to set these to their defaults -- TAA MOD*/
/*  setsvalue(s_ifmt,cvstring(IFMT));	*//* integer print format */
/*  setsvalue(s_ffmt,cvstring("%g"));	*//* float print format */

#ifdef RANDOM
    setsvalue(s_randomstate, newrandom(1L));	/* random state */
#endif
    setsvalue(s_printcase,k_upcase);	/* upper case output of symbols */
#ifdef PRINDEPTH
    setsvalue(s_printlevel,NIL);	/* printing depth is infinite */
    setsvalue(s_printlength,NIL);	/* printing length is infinite */
#endif
#ifdef DOSINPUT
    setsvalue(s_dosinput,NIL);		/* use XLISP line editing */
#endif
#ifdef DISPMACRO
    setsvalue(s_dispmacros,NIL);	/* don't displace macros */
#endif

    /* install the built-in functions and special forms */
    for (i = 0, p = funtab; (p->fd_subr) != (LVAL(*)())NULL; ++i, ++p)
	if (p->fd_name != NULL)
	    xlsubr(p->fd_name,p->fd_type,p->fd_subr,i);

    /* add some synonyms */
    setfunction(senter("NOT"),getfunction(senter("NULL")));
    setfunction(senter("FIRST"),getfunction(senter("CAR")));
    setfunction(senter("SECOND"),getfunction(senter("CADR")));
    setfunction(senter("THIRD"),getfunction(senter("CADDR")));
    setfunction(senter("FOURTH"),getfunction(senter("CADDDR")));
    setfunction(senter("REST"),getfunction(senter("CDR")));
}

/* xlsymbols - enter all of the symbols used by the interpreter */
VOID xlsymbols()
{
    LVAL sym;

    /* enter the unbound variable indicator (must be first) */
    s_unbound = senter("*UNBOUND*");
    defconstant(s_unbound,s_unbound);	/* TAA mod -- was setvalue */

#ifdef NILSYMBOL
    senter("NIL");			    /* put NIL in oblist */
#endif

    /* enter the 't' symbol */
    true = senter("T");
    defconstant(true,true); /* TAA mod -- was setvalue */

    /* enter some other constants */

#ifdef TIMES
    sym = senter("INTERNAL-TIME-UNITS-PER-SECOND");
    defconstant(sym, cvfixnum((FIXTYPE) ticks_per_second()));
#endif
#ifdef COMPLX
    sym = senter("PI");
    defconstant(sym, cvflonum((FLOTYPE) PI));
#endif


    /* enter some important symbols */
    s_dot	= senter(".");
    s_quote	= senter("QUOTE");
    s_function	= senter("FUNCTION");
    s_bquote	= senter("BACKQUOTE");
    s_comma	= senter("COMMA");
    s_comat	= senter("COMMA-AT");
    s_lambda	= senter("LAMBDA");
    s_macro	= senter("MACRO");
    s_eql	= senter("EQL");
    s_ifmt	= senter("*INTEGER-FORMAT*");
    s_ffmt	= senter("*FLOAT-FORMAT*");

    /* symbols set by the read-eval-print loop */
    s_1plus	= senter("+");
    s_2plus	= senter("++");
    s_3plus	= senter("+++");
    s_1star	= senter("*");
    s_2star	= senter("**");
    s_3star	= senter("***");
    s_minus	= senter("-");

    /* enter setf place specifiers */
    s_setf	= senter("*SETF*");
    s_car	= senter("CAR");
    s_cdr	= senter("CDR");
    s_nth	= senter("NTH");
    s_aref	= senter("AREF");
    s_get	= senter("GET");
    s_svalue	= senter("SYMBOL-VALUE");
    s_sfunction = senter("SYMBOL-FUNCTION");
    s_splist	= senter("SYMBOL-PLIST");
#ifdef COMMONLISPF
    s_elt   = senter("ELT");
#endif
#ifdef HASHFCNS
    s_gethash	= senter("GETHASH");
#endif

    /* enter the readtable variable and keywords */
    s_rtable	= senter("*READTABLE*");
    k_wspace	= senter(":WHITE-SPACE");
    k_const	= senter(":CONSTITUENT");
    k_nmacro	= senter(":NMACRO");
    k_tmacro	= senter(":TMACRO");
    k_sescape	= senter(":SESCAPE");
    k_mescape	= senter(":MESCAPE");

    /* enter parameter list keywords */
    k_test	= senter(":TEST");
    k_tnot	= senter(":TEST-NOT");

    /* "open" keywords */
    k_direction = senter(":DIRECTION");
    k_input	= senter(":INPUT");
    k_output	= senter(":OUTPUT");
#ifdef BETTERIO
    k_io	= senter(":IO");
    k_probe	= senter(":PROBE");
    k_elementtype = senter(":ELEMENT-TYPE");
    k_exist	= senter(":IF-EXISTS");
    k_nexist	= senter(":IF-DOES-NOT-EXIST");
    k_error	= senter(":ERROR");
    k_rename	= senter(":RENAME");
    k_newversion = senter(":NEW-VERSION");
    k_overwrite = senter(":OVERWRITE");
    k_append	= senter(":APPEND");
    k_supersede = senter(":SUPERSEDE");
    k_rendel	= senter(":RENAME-AND-DELETE");
    k_create	= senter(":CREATE");
#endif

    /* enter *print-case* symbol and keywords */
    s_printcase = senter("*PRINT-CASE*");
    k_upcase	= senter(":UPCASE");
    k_downcase	= senter(":DOWNCASE");

    /* more printing symbols */
#ifdef PRINDEPTH
    s_printlevel= senter("*PRINT-LEVEL*");
    s_printlength = senter("*PRINT-LENGTH*");
#endif
#ifdef DOSINPUT
    s_dosinput	= senter("*DOS-INPUT*");
#endif

    /* other keywords */
    k_start	= senter(":START");
    k_end	= senter(":END");
    k_1start	= senter(":START1");
    k_1end	= senter(":END1");
    k_2start	= senter(":START2");
    k_2end	= senter(":END2");
    k_verbose	= senter(":VERBOSE");
    k_print	= senter(":PRINT");
    k_count	= senter(":COUNT");

#ifdef KEYARG
    k_key	= senter(":KEY");
#endif

#ifdef TIERNEY
    k_ivalue	= senter(":INITIAL-VALUE");
#endif

#ifdef STRUCTS
    k_concname	= senter(":CONC-NAME");
    k_include	= senter(":INCLUDE");
#endif

#ifdef HASHFCNS
    k_size = senter(":SIZE");
#endif

#ifdef RANDOM
    k_data = senter(":DATA");
#endif


    /* enter lambda list keywords */
    lk_optional = senter("&OPTIONAL");
    lk_rest	= senter("&REST");
    lk_key	= senter("&KEY");
    lk_aux	= senter("&AUX");
    lk_allow_other_keys = senter("&ALLOW-OTHER-KEYS");

    /* enter *standard-input*, *standard-output* and *error-output* */
    /* TAA Modified so that stderr (CONSOLE) is used if no redirection */

#ifdef BETTERIO
    s_stderr = senter("*ERROR-OUTPUT*");
    setsvalue(s_stderr,cvfile(CONSOLE,S_FORREADING|S_FORWRITING));
    s_termio = senter("*TERMINAL-IO*");
    setsvalue(s_termio,getvalue(s_stderr));
    s_stdin = senter("*STANDARD-INPUT*");
    setsvalue(s_stdin,redirectin ?
	cvfile(STDIN,S_FORREADING): getvalue(s_stderr));
    s_stdout = senter("*STANDARD-OUTPUT*");
    setsvalue(s_stdout,redirectout ?
	cvfile(STDOUT,S_FORWRITING): getvalue(s_stderr));
#else
    s_stderr = senter("*ERROR-OUTPUT*");
    setsvalue(s_stderr,cvfile(CONSOLE));
    s_stdin = senter("*STANDARD-INPUT*");
    setsvalue(s_stdin,redirectin ? cvfile(STDIN): getvalue(s_stderr));
    s_stdout = senter("*STANDARD-OUTPUT*");
    setsvalue(s_stdout,redirectout ? cvfile(STDOUT): getvalue(s_stderr));
#endif

    /* enter *debug-io* and *trace-output* */
    s_debugio = senter("*DEBUG-IO*");
    setsvalue(s_debugio,getvalue(s_stderr));
    s_traceout = senter("*TRACE-OUTPUT*");
    setsvalue(s_traceout,getvalue(s_stderr));

#ifdef WINTERP
    s_errhook = senter("*ERRHOOK*");
#endif /* WINTERP */

    /* enter the eval and apply hook variables */
    s_evalhook = senter("*EVALHOOK*");
    s_applyhook = senter("*APPLYHOOK*");

    /* enter the symbol pointing to the list of functions being traced */
    s_tracelist = senter("*TRACELIST*");

    /* enter the error traceback and the error break enable flags */
    s_tracenable = senter("*TRACENABLE*");
    s_tlimit = senter("*TRACELIMIT*");
    s_breakenable = senter("*BREAKENABLE*");

    /* enter symbols to control printing of garbage collection messages */
    s_gcflag = senter("*GC-FLAG*");
    s_gchook = senter("*GC-HOOK*");

    /* enter symbol to control displacing of macros with expanded version */
#ifdef DISPMACRO
    s_dispmacros = senter("*DISPLACE-MACROS*");
#endif

#ifndef WINTERP
    /* enter a copyright notice into the oblist */
    sym = senter("**Copyright-1988-by-David-Betz**");
    setsvalue(sym,true);
#endif /* WINTERP */

    /* enter type names */
    a_subr	= senter("SUBR");
    a_fsubr	= senter("FSUBR");
    a_cons	= senter("CONS");
    a_symbol	= senter("SYMBOL");
    a_fixnum	= senter("FIXNUM");
    a_flonum	= senter("FLONUM");
    a_string	= senter("STRING");
    a_object	= senter("OBJECT");
    a_stream	= senter("FILE-STREAM");
    a_vector	= senter("ARRAY");
    a_closure	= senter("CLOSURE");
    a_char	= senter("CHARACTER");
    a_ustream	= senter("UNNAMED-STREAM");
#ifdef COMPLX
    a_complex	= senter("COMPLEX");
#endif
#ifdef COMMONLISPF
    a_list  = senter("LIST");
    a_number = senter("NUMBER");
    a_null = senter("NULL");
    a_atom = senter("ATOM");
    a_anystream = senter("STREAM");
    s_and = senter("AND");
    s_or = senter("OR");
    s_not = senter("NOT");
    s_satisfies = senter("SATISFIES");
    s_member = senter("MEMBER");
#ifdef STRUCTS
    a_struct = senter("STRUCT");
#endif
#endif
#ifdef HASHFCNS
    a_hashtable = senter("%HASHTABLE");
#endif

#ifdef STRUCTS
    s_strtypep	= senter("%STRUCT-TYPE-P");
    s_mkstruct	= senter("%MAKE-STRUCT");
    s_cpystruct = senter("%COPY-STRUCT");
    s_strref	= senter("%STRUCT-REF");
    s_strset	= senter("%STRUCT-SET");
    s_x		= senter("X");
    s_s		= senter("S");
    s_sslots	= senter("*STRUCT-SLOTS*");
#endif

#ifdef RANDOM
    s_randomstate = senter("*RANDOM-STATE*");
    a_randomstate = senter("RANDOM-STATE");
    sym = cons(NIL,NIL);    /* add to *struct-slots* property ((data nil)) */
    sym = cons(senter("DATA"),sym);
    sym = consa(sym);
    xlputprop(a_randomstate,sym,s_sslots);
#endif

#if (defined(UNIX) || defined(WINTERP))
    a_PIPE          = senter("PIPE");
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP
    a_XtAccelerators = senter("XT_ACCELERATORS");
    a_XtTranslations = senter("XT_TRANSLATIONS");
    a_XEvent         = senter("XEVENT");
    a_Window         = senter("WINDOW");
    a_Pixel          = senter("PIXEL");
    a_Pixmap         = senter("PIXMAP");
    a_XImage         = senter("XIMAGE");
    a_XmString       = senter("XM_STRING");
    a_XmFontList     = senter("XM_FONT_LIST");
    a_XT_RESOURCE    = senter("XT_RESOURCE");
    a_CALLBACKOBJ    = senter("CALLBACK_OBJ");
    a_TIMEOUTOBJ     = senter("TIMEOUT_OBJ");
    a_PIXMAP_REFOBJ  = senter("PIXMAP_REFOBJ");
    a_WIDGETOBJ      = senter("WIDGET_OBJ");
#ifdef WINTERP_XTANGO_WIDGET
    a_TANGOIMAGEOBJ  = senter("TANGOIMAGE_OBJ");
    a_TANGO_PATH     = senter("TANGO_PATH");
    a_TANGO_TRANS    = senter("TANGO_TRANS");
#endif /* WINTERP_XTANGO_WIDGET */
    a_EVHANDLEROBJ   = senter("EVHANDLER_OBJ");
    a_FDINPUTCBOBJ   = senter("FDINPUTCB_OBJ");
#endif /* WINTERP */

    /* add the object-oriented programming symbols and os specific stuff */
    obsymbols();	/* object-oriented programming symbols */
    ossymbols();	/* os specific symbols */
}
