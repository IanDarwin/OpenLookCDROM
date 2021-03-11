/* -*-C-*-
********************************************************************************
*
* File:         xlftab.c
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlftab.c,v 2.4 1994/06/06 15:59:15 npm Exp $
* Description:  xlftab.c - xlisp function table -- for 'xlisp' not 'winterp'
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:30 1994 (Niels Mayer) npm@indeed
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
static char rcs_identity[] = "@(#)$Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlftab.c,v 2.4 1994/06/06 15:59:15 npm Exp $";

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

/* include system dependant definitions */
#include "osdefs.h"

/* SUBR/FSUBR indicator */
#define S   SUBR
#define F   FSUBR

/* xnotimp - function table entries that are currently not implemented */
LOCAL LVAL xnotimp()
{
    xlfail("function not implemented");
    return NIL;
}

/* the function table */
FUNDEF funtab[] = {
/* DO NOT ALTER ENTRIES UNTIL AFTER OBPRIN1 */
    /* read macro functions */
{   NULL,		S, rmhash	},
{   NULL,		S, rmquote	},
{   NULL,		S, rmdquote	},
{   NULL,		S, rmbquote	},
{   NULL,		S, rmcomma	},
{   NULL,		S, rmlpar	},
{   NULL,		S, rmrpar	},
{   NULL,		S, rmsemi	},
{   NULL,		S, xnotimp	},
{   NULL,		S, xnotimp	},

    /* methods */
{   NULL,		S, clnew	},
{   NULL,		S, clisnew	},
{   NULL,		S, clanswer	},
{   NULL,		S, obisnew	},
{   NULL,		S, obclass	},
{   NULL,		S, obshow	},
#ifdef OBJPRNT
{   NULL,		S, obprin1	},
#endif

/* Empty slots not needed beyond this point */

    /* evaluator functions */
{   "EVAL",		S, xeval	},
{   "APPLY",		S, xapply	},
{   "FUNCALL",		S, xfuncall	},
{   "QUOTE",		F, xquote	},
{   "FUNCTION",		F, xfunction	},
{   "BACKQUOTE",	F, xbquote	},
{   "LAMBDA",		F, xlambda	},

    /* symbol functions */
{   "SET",		S, xset		},
{   "SETQ",		F, xsetq	},
{   "SETF",		F, xsetf	},
{   "DEFUN",		F, xdefun	},
{   "DEFMACRO",		F, xdefmacro	},
{   "GENSYM",		S, xgensym	},
{   "MAKE-SYMBOL",	S, xmakesymbol	},
{   "INTERN",		S, xintern	},
{   "SYMBOL-NAME",	S, xsymname	},
{   "SYMBOL-VALUE",	S, xsymvalue	},
{   "SYMBOL-PLIST",	S, xsymplist	},
{   "GET",		S, xget		},
{   "PUTPROP",		S, xputprop	},
{   "REMPROP",		S, xremprop	},
{   "HASH",		S, xhash	},

    /* array functions */
{   "MAKE-ARRAY",	S, xmkarray	},
{   "AREF",		S, xaref	},

    /* list functions */
{   "CAR",		S, xcar		},
{   "CDR",		S, xcdr		},

{   "CAAR",		S, xcaar	},
{   "CADR",		S, xcadr	},
{   "CDAR",		S, xcdar	},
{   "CDDR",		S, xcddr	},

{   "CAAAR",		S, xcaaar	},
{   "CAADR",		S, xcaadr	},
{   "CADAR",		S, xcadar	},
{   "CADDR",		S, xcaddr	},
{   "CDAAR",		S, xcdaar	},
{   "CDADR",		S, xcdadr	},
{   "CDDAR",		S, xcddar	},
{   "CDDDR",		S, xcdddr	},

{   "CAAAAR",		S, xcaaaar	},
{   "CAAADR",		S, xcaaadr	},
{   "CAADAR",		S, xcaadar	},
{   "CAADDR",		S, xcaaddr	},
{   "CADAAR",		S, xcadaar	},
{   "CADADR",		S, xcadadr	},
{   "CADDAR",		S, xcaddar	},
{   "CADDDR",		S, xcadddr	},
{   "CDAAAR",		S, xcdaaar	},
{   "CDAADR",		S, xcdaadr	},
{   "CDADAR",		S, xcdadar	},
{   "CDADDR",		S, xcdaddr	},
{   "CDDAAR",		S, xcddaar	},
{   "CDDADR",		S, xcddadr	},
{   "CDDDAR",		S, xcdddar	},
{   "CDDDDR",		S, xcddddr	},

{   "CONS",		S, xcons	},
{   "LIST",		S, xlist	},
#ifdef COMMONLISPF
{   "LIST*",		S, xliststar	},
#endif
{   "APPEND",		S, xappend	},
{   "REVERSE",		S, xreverse	},
{   "LAST",		S, xlast	},
{   "NTH",		S, xnth		},
{   "NTHCDR",		S, xnthcdr	},
{   "MEMBER",		S, xmember	},
{   "ASSOC",		S, xassoc	},
{   "SUBST",		S, xsubst	},
{   "SUBLIS",		S, xsublis	},
{   "REMOVE",		S, xremove	},
{   "LENGTH",		S, xlength	},
{   "MAPC",		S, xmapc	},
{   "MAPCAR",		S, xmapcar	},
{   "MAPL",		S, xmapl	},
{   "MAPLIST",		S, xmaplist	},

    /* destructive list functions */
{   "RPLACA",		S, xrplca	},
{   "RPLACD",		S, xrplcd	},
{   "NCONC",		S, xnconc	},
{   "DELETE",		S, xdelete	},

    /* predicate functions */
{   "ATOM",		S, xatom	},
{   "SYMBOLP",		S, xsymbolp	},
{   "NUMBERP",		S, xnumberp	},
{   "BOUNDP",		S, xboundp	},
{   "NULL",		S, xnull	},
{   "LISTP",		S, xlistp	},
{   "CONSP",		S, xconsp	},
{   "MINUSP",		S, xminusp	},
{   "ZEROP",		S, xzerop	},
{   "PLUSP",		S, xplusp	},
{   "EVENP",		S, xevenp	},
{   "ODDP",		S, xoddp	},
{   "EQ",		S, xeq		},
{   "EQL",		S, xeql		},
{   "EQUAL",		S, xequal	},

    /* special forms */
{   "COND",		F, xcond	},
{   "CASE",		F, xcase	},
{   "AND",		F, xand		},
{   "OR",		F, xor		},
{   "LET",		F, xlet		},
{   "LET*",		F, xletstar	},
{   "IF",		F, xif		},
{   "PROG",		F, xprog	},
{   "PROG*",		F, xprogstar	},
{   "PROG1",		F, xprog1	},
{   "PROG2",		F, xprog2	},
{   "PROGN",		F, xprogn	},
{   "GO",		F, xgo		},
{   "RETURN",		F, xreturn	},
{   "DO",		F, xdo		},
{   "DO*",		F, xdostar	},
{   "DOLIST",		F, xdolist	},
{   "DOTIMES",		F, xdotimes	},
{   "CATCH",		F, xcatch	},
{   "THROW",		F, xthrow	},

    /* debugging and error handling functions */
{   "ERROR",		S, xerror	},
{   "CERROR",		S, xcerror	},
{   "BREAK",		S, xbreak	},
{   "CLEAN-UP",		S, xcleanup	},
{   "TOP-LEVEL",	S, xtoplevel	},
{   "CONTINUE",		S, xcontinue	},
{   "ERRSET",		F, xerrset	},
{   "BAKTRACE",		S, xbaktrace	},
{   "EVALHOOK",		S, xevalhook	},

    /* arithmetic functions */
{   "TRUNCATE",		S, xfix		},
{   "FLOAT",		S, xfloat	},
{   "+",		S, xadd		},
{   "-",		S, xsub		},
{   "*",		S, xmul		},
{   "/",		S, xdiv		},
{   "1+",		S, xadd1	},
{   "1-",		S, xsub1	},
{   "REM",		S, xrem		},
{   "MIN",		S, xmin		},
{   "MAX",		S, xmax		},
{   "ABS",		S, xabs		},
{   "SIN",		S, xsin		},
{   "COS",		S, xcos		},
{   "TAN",		S, xtan		},
{   "EXPT",		S, xexpt	},
{   "EXP",		S, xexp		},
{   "SQRT",		S, xsqrt	},
{   "RANDOM",		S, xrand	},

    /* bitwise logical functions */
{   "LOGAND",		S, xlogand	},
{   "LOGIOR",		S, xlogior	},
{   "LOGXOR",		S, xlogxor	},
{   "LOGNOT",		S, xlognot	},

    /* numeric comparison functions */
{   "<",		S, xlss		},
{   "<=",		S, xleq		},
{   "=",		S, xequ		},
{   "/=",		S, xneq		},
{   ">=",		S, xgeq		},
{   ">",		S, xgtr		},

    /* string functions */
#ifdef COMMONLISPF
{   "CONCATENATE",	S, xconcatenate },
#else
{   "STRCAT",		S, xstrcat	},
#endif
{   "SUBSEQ",		S, xsubseq	},
{   "STRING",		S, xstring	},
{   "CHAR",		S, xchar	},

    /* I/O functions */
{   "READ",		S, xread	},
{   "PRINT",		S, xprint	},
{   "PRIN1",		S, xprin1	},
{   "PRINC",		S, xprinc	},
{   "TERPRI",		S, xterpri	},
{   "FLATSIZE",		S, xflatsize	},
{   "FLATC",		S, xflatc	},

    /* file I/O functions */
{   "OPEN",		S, xopen	},
{   "FORMAT",		S, xformat	},
{   "CLOSE",		S, xclose	},
{   "READ-CHAR",	S, xrdchar	},
{   "PEEK-CHAR",	S, xpkchar	},
{   "WRITE-CHAR",	S, xwrchar	},
{   "READ-LINE",	S, xreadline	},

    /* system functions */
{   "LOAD",		S, xload	},
{   "DRIBBLE",		S, xtranscript	},

/* functions specific to xldmem.c */
{   "GC",		S, xgc		},
{   "EXPAND",		S, xexpand	},
{   "ALLOC",		S, xalloc	},
{   "ROOM",		S, xmem		},
#ifdef SAVERESTORE
{   "SAVE",		S, xsave	},
{   "RESTORE",		S, xrestore	},
#endif
/* end of functions specific to xldmem.c */

{   "TYPE-OF",		S, xtype	},
{   "EXIT",		S, xexit	},
{   "PEEK",		S, xpeek	},
{   "POKE",		S, xpoke	},
{   "ADDRESS-OF",	S, xaddrs	},

    /* new functions and special forms */
{   "VECTOR",		S, xvector	},
{   "BLOCK",		F, xblock	},
{   "RETURN-FROM",	F, xrtnfrom	},
{   "TAGBODY",		F, xtagbody	},
{   "PSETQ",		F, xpsetq	},
{   "FLET",		F, xflet	},
{   "LABELS",		F, xlabels	},
{   "MACROLET",		F, xmacrolet	},
{   "UNWIND-PROTECT",	F, xunwindprotect},
{   "PPRINT",		S, xpp		},
{   "STRING<",		S, xstrlss	},
{   "STRING<=",		S, xstrleq	},
{   "STRING=",		S, xstreql	},
{   "STRING/=",		S, xstrneq	},
{   "STRING>=",		S, xstrgeq	},
{   "STRING>",		S, xstrgtr	},
{   "STRING-LESSP",	S, xstrilss	},
{   "STRING-NOT-GREATERP",S,xstrileq	},
{   "STRING-EQUAL",	S, xstrieql	},
{   "STRING-NOT-EQUAL", S, xstrineq	},
{   "STRING-NOT-LESSP", S, xstrigeq	},
{   "STRING-GREATERP",	S, xstrigtr	},
{   "INTEGERP",		S, xintegerp	},
{   "FLOATP",		S, xfloatp	},
{   "STRINGP",		S, xstringp	},
{   "ARRAYP",		S, xarrayp	},
{   "STREAMP",		S, xstreamp	},
{   "OBJECTP",		S, xobjectp	},
{   "STRING-UPCASE",	S, xupcase	},
{   "STRING-DOWNCASE",	S, xdowncase	},
{   "NSTRING-UPCASE",	S, xnupcase	},
{   "NSTRING-DOWNCASE", S, xndowncase	},
{   "STRING-TRIM",	S, xtrim	},
{   "STRING-LEFT-TRIM", S, xlefttrim	},
{   "STRING-RIGHT-TRIM",S, xrighttrim	},
{   "WHEN",		F, xwhen	},
{   "UNLESS",		F, xunless	},
{   "LOOP",		F, xloop	},
{   "SYMBOL-FUNCTION",	S, xsymfunction },
{   "FBOUNDP",		S, xfboundp	},
{   "SEND",		S, xsend	},
{   "SEND-SUPER",	S, xsendsuper	},
{   "PROGV",		F, xprogv	},
{   "CHARACTERP",	S, xcharp	},
{   "CHAR-INT",		S, xcharint	},
{   "INT-CHAR",		S, xintchar	},
{   "READ-BYTE",	S, xrdbyte	},
{   "WRITE-BYTE",	S, xwrbyte	},
{   "MAKE-STRING-INPUT-STREAM", S, xmkstrinput	    },
{   "MAKE-STRING-OUTPUT-STREAM",S, xmkstroutput	    },
{   "GET-OUTPUT-STREAM-STRING", S, xgetstroutput    },
{   "GET-OUTPUT-STREAM-LIST",	S, xgetlstoutput    },
{   "GCD",		S, xgcd		},
{   "GET-LAMBDA-EXPRESSION",	S, xgetlambda	    },
{   "MACROEXPAND",	S, xmacroexpand },
{   "MACROEXPAND-1",	S, x1macroexpand},
{   "CHAR<",		S, xchrlss	},
{   "CHAR<=",		S, xchrleq	},
{   "CHAR=",		S, xchreql	},
{   "CHAR/=",		S, xchrneq	},
{   "CHAR>=",		S, xchrgeq	},
{   "CHAR>",		S, xchrgtr	},
{   "CHAR-LESSP",	S, xchrilss	},
{   "CHAR-NOT-GREATERP",S, xchrileq	},
{   "CHAR-EQUAL",	S, xchrieql	},
{   "CHAR-NOT-EQUAL",	S, xchrineq	},
{   "CHAR-NOT-LESSP",	S, xchrigeq	},
{   "CHAR-GREATERP",	S, xchrigtr	},
{   "UPPER-CASE-P",	S, xuppercasep	},
{   "LOWER-CASE-P",	S, xlowercasep	},
{   "BOTH-CASE-P",	S, xbothcasep	},
{   "DIGIT-CHAR-P",	S, xdigitp	},
{   "ALPHANUMERICP",	S, xalphanumericp},
{   "CHAR-UPCASE",	S, xchupcase	},
{   "CHAR-DOWNCASE",	S, xchdowncase	},
{   "DIGIT-CHAR",	S, xdigitchar	},
{   "CHAR-CODE",	S, xcharcode	},
{   "CODE-CHAR",	S, xcodechar	},
{   "ENDP",		S, xendp	},
{   "REMOVE-IF",	S, xremif	},
{   "REMOVE-IF-NOT",	S, xremifnot	},
{   "DELETE-IF",	S, xdelif	},
{   "DELETE-IF-NOT",	S, xdelifnot	},
{   "TRACE",		F, xtrace	},
{   "UNTRACE",		F, xuntrace	},
{   "SORT",		S, xsort	},
#ifdef ADDEDTAA
{   "GENERIC",		S, xgeneric	},
#endif
#ifdef TIMES
{   "TIME",		F, xtime	},
{   "GET-INTERNAL-RUN-TIME",	S, xruntime  },
{   "GET-INTERNAL-REAL-TIME",	S, xrealtime },
#endif
/* extra table entries */
#ifdef COMMONLISPF
#ifdef POSFCNS
{   "COUNT-IF",		S, xcountif	},
{   "FIND-IF",		S, xfindif	},
{   "POSITION-IF",	S, xpositionif	},
#endif
{   "COERCE",		S, xcoerce	},
{   "ELT",		S, xelt		},
#ifdef SRCHFCN
{   "SEARCH",		S, xsearch	},
#endif
#ifdef MAPFCNS
{   "MAP",		S, xmap		},
{   "SOME",		S, xsome	},
{   "EVERY",		S, xevery	},
{   "NOTANY",		S, xnotany	},
{   "NOTEVERY",		S, xnotevery	},
#endif
#endif
#ifdef BETTERIO
{   "FILE-POSITION",	S, xfileposition},
{   "FILE-LENGTH",	S, xfilelength	},
{   "FRESH-LINE",	S, xfreshline	},
{   "OPEN-STREAM-P",	S, xopenstreamp },
{   "INPUT-STREAM-P",	S, xinputstreamp},
{   "OUTPUT-STREAM-P",	S, xoutputstreamp},
#endif
#ifdef STRUCTS
{   "DEFSTRUCT",	F, xdefstruct	},
{   "%STRUCT-TYPE-P",	S, xstrtypep	},
{   "%MAKE-STRUCT",	S, xmkstruct	},
{   "%COPY-STRUCT",	S, xcpystruct	},
{   "%STRUCT-REF",	S, xstrref	},
{   "%STRUCT-SET",	S, xstrset	},
#endif
#if defined(STRUCTS) | defined(COMPLX)
{   "ASIN",		S, xasin	},
{   "ACOS",		S, xacos	},
{   "ATAN",		S, xatan	},
#endif
#ifdef APPLYHOOK
{   "APPLYHOOK",	S, xapplyhook	},
#endif

#ifdef COMMONLISPF
{   "NREVERSE",		S, xnreverse	},
{   "BUTLAST",		S, xbutlast	},
{   "TYPEP",		S, xtypep	},
#ifdef TIERNEY
{   "REDUCE",		S, xreduce	},
#endif
#ifdef REMDUPS
{   "REMOVE-DUPLICATES",S, xremove_duplicates },
#endif
#endif

#ifdef SETS
{   "ADJOIN",		S, xadjoin	    },
{   "UNION",		S, xunion	    },
{   "INTERSECTION",	S, xintersection    },
{   "SET-DIFFERENCE",	S, xset_difference  },
{   "SUBSETP",		S, xsubsetp	    },
#endif

#ifdef HASHFCNS
{   "GETHASH",		S, xgethash	    },
{   "REMHASH",		S, xremhash	    },
{   "MAKE-HASH-TABLE",	S, xmakehash	    },
{   "CLRHASH",		S, xclrhash	    },
{   "MAPHASH",		S, xmaphash	    },
{   "HASH-TABLE-COUNT", S, xhashcount	    },
#endif

#ifdef COMPLX
{   "COMPLEXP",		S, xcomplexp	    },
{   "COMPLEX",		S, xcomplex	    },
{   "CONJUGATE",	S, xconjugate	    },
{   "REALPART",		S, xrealpart	    },
{   "IMAGPART",		S, ximagpart	    },
{   "LOG",		S, xlog		    },
{   "FLOOR",		S, xfloor	    },
{   "CEILING",		S, xceil	    },
{   "ROUND",		S, xround	    },
{   "PHASE",		S, xphase	    },
{   "LCM",		S, xlcm		    },
#endif

#ifdef SPECIALS
{   "DEFCONSTANT",	F, xdefconstant	    },
{   "CONSTANTP",	S, xconstantp	    },
{   "DEFPARAMETER",	F, xdefparameter    },
{   "DEFVAR",		F, xdefvar	    },
{   "MAKUNBOUND",	S, xmakunbound	    },
#endif

#ifdef RANDOM
{   "MAKE-RANDOM-STATE",S, xmakerandom	    },
#endif

/* added by Niels Mayer to file xlbfun.c */
{   "COPY-ARRAY",	S, Prim_COPY_ARRAY	}, /* NPM  */
{   "ARRAY-INSERT-POS",	S, Prim_ARRAY_INSERT_POS}, /* NPM */
{   "ARRAY-DELETE-POS",	S, Prim_ARRAY_DELETE_POS}, /* NPM */

    /* include system dependent function pointers */
#include "osptrs.h"

{0,0,0} /* end of table marker */

};

int ftabsize = sizeof(funtab); /* TAA MOD -- added validity check */
