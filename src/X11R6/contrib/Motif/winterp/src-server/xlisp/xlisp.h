/* -*-C-*-
********************************************************************************
*
* File:         xlisp.h
* RCS:          $Header: /users/npm/src/winterp/src-server/xlisp/RCS/xlisp.h,v 2.6 1994/06/06 15:59:18 npm Exp $
* Description:  libXlisp.a external interfaces
* Author:       David Michael Betz. WINTERP portions by Niels Mayer;
*		XLISP-PLUS by Tom Almy with contributions from Johnny
*		Greenblatt, Neal Holtz, Niels Mayer, Blake McBride, Mikael
*		Pettersson, Luke Tierney, Ken Whedbee, Pete Yadlowsky.
* Created:      
* Modified:     Mon Jun  6 03:04:12 1994 (Niels Mayer) npm@indeed
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

/* system specific definitions */

#include <stdio.h>
#include <ctype.h>
#include <setjmp.h>

#ifdef WINTERP
/*
 * Note that Xm/Xm.h includes X11/Intrinsic.h which includes X11/Xos.h...
 * X11/Xos.h uses imake defines (e.g. -DSYSV) in order to make certain
 * unportable constructs more portable. For example "#include <string.h>"
 * versus "#include <strings.h>"; index() versus strchr(); rindex() versus
 * strrchr(), etc.
 * Note also that most of Xm/Xm.h isn't used here, but is used by xldmem.h
 * (included below) when compiling with -DWINTERP...
 */
#include <Xm/Xm.h>		/* Motif/Xtoolkit/X externals */

#ifdef WINTERP_XTANGO_WIDGET
#include "../xtango/xtango.h"	/* define opaque ptrs TANGO_LOC, TANGO_IMAGE, TANGO_PATH, TANGO_TRANS */
typedef struct _TangoWidgetAssoc_Node *WINTERP_TANGO_CONTEXT; /* define opaque ptr WINTERP_TANGO_CONTEXT w/o including ../tango.h */
#endif /* WINTERP_XTANGO_WIDGET */

#else /* !defined(WINTERP) */
#include <string.h>		/* an unportable construct from Almy's xlisp 2.1c */
#endif /* WINTERP */

/************ Notice to anyone attempting modifications ****************/
/* Compared to standard versions, length of strings in an LVAL exclude the
   terminating null. When appropriate, characters are consistantly treated
   as unsigned, and the null, \0, character is allowed. Don't write any new
   code that assumes NULL and/or NIL are zero */

/********************** PREFERENCE OPTIONS ****************/

/* There are many different preference options. If you turn them all off
   then you get "standard" xlisp 2.0. On a small system you may want
   to leave out unused functions */
/* You can also use dynamic array allocation by substituting dldmem.c
   and dlimage.c for xldmem.c and xlimage.c. Using this alternative
   adds 1184 bytes of code */
/* FOR-WINTERP: dldmem.c and dlimage.c have not been tested -- NPM */
/* Costs indicated for Borland Turbo C++ V1.0 (as a C compiler) */

/* Not all permutations of these choices have been tested, but luckily most
   won't interract. */


/* these first three modify performance, but don't affect execution of
   application programs (other than speed) */
#define JMAC	    /* performance enhancing macros, Johnny Greenblatt
			(7.5K at full config). Don't bother for 16 bit
			MSDOS compilers. */

#define JGC	    /* improved garbage collection, Johnny Greenblatt
			(48 bytes! Up to 10% less heap!) I never compile
			without this, so bugs might crop up if turned off*/

#define STATICSTK   /* Statically declared stack -- seems to make sense,
		       especially with 16 bit 8086 compilers. (Saves about
		       3k, and increases performance 5% with TURBO-C) */

/* This option is necessary for Microsoft Windows 3.0, but can be used
   under MS-DOS as well. Borland C++ and TopSpeed C provide adequate library
   support for MS-DOS use. For other compilers, additional functions would
   need to be written (not supplied). Windows provides the necessary
   functions, so any Windows-compliant compiler should suffice, with the
   exception that the code assumes that sprintf can accept the %Fs specifier
   for far strings, which Zortech does not allow (as of v2.1, at least).
   When using this option, you must compile all modules with the medium
   memory model, and you must also use the dldmem/dlimage pair of files
   rather than the xldmem/xlimage pair of files. Be sure to define
   STATICSTK and NILSYMBOL as well */

/*#define MEDMEM*/	/* Medium memory model */


/* This option is necessary for Microsoft Windows 3.0. It handles file
   streams using a local table of file defining structures. For non-windows
   use, the benefits are file streams can print their associated file names
   and files are saved across saves. */

#define FILETABLE

/* The remainder of options will affect some programs */

#define NILSYMBOL   /* NIL is actually a symbol as opposed to (LVAL *)0 */
		    /* If you use this, define SPECIALS as well, otherwise
			there's nothing keeping you from such problems as
			assigning NIL the value T */
		    /* This feature requires that all code that checks for
			NIL explicitly compare with NIL, something that lazy
			programmers haven't done. There is a good chance that
			an improper comparison is lurking in the code some-
			where. On the other hand, the code is lots cleaner,
			except for init/save/restore/gc kludges
			(saves 3040 bytes!)*/

#define PRINDEPTH   /* added ability to control print depth (736 bytes)*/

#define OBJPRNT	    /* friendly object printing feature TAA and
			Mikael Pettersson, Dept. of Computer and Info.
			Science, University of Linkoping, Sweden
			(944 bytes) */
#define ENHFORMAT   /* enhanced FORMAT function (Neal Holtz,TAA)(2112 bytes)*/

#define BETTERIO    /* improved io (binary files, file positioning,)
			OPEN meets Common-Lisp requirements
			FRESH-LINE function
			File access direction enforced.
			& and T format directives with ENHFORMAT
			Adds *terminal-io*
			NIL and T for *standard-??put* and *terminal-io* in
			  io functions.
			(3760 bytes with ENHFORMAT) */

#define DISPMACRO   /* if *displace-macros* is non-nil then macros that
			returns conses replace the original macro code.
			(144 bytes) */

#define ALLOWOTHER  /* Makes &allow-other-keys actuall work (i.e., other
			keys are *disallowed* when this is missing) This
			makes things consistant with SUBRS which now complain
			about excess keyword arguments (144 bytes) */

#define SPECIALS    /* True special variables and constants
		       defines DEFVAR DEFPARAM DEFCONST MAKUNBOUND CONSTANTP
				(~3500 bytes + potentially 1 byte/LVAL, but
				    not for most compilers)
			*/

#define COMMONLISP  /* NIL means end of string for ending indexes, string fcns
		       string functions take symbols or strings as args
		       Symbol properties can be other than symbols.
		       APPLY takes multiple arguments
		       AREF indexes strings.
		       Uninterned symbol print with leading #:
		       #. read macro enabled
		       (880 bytes)*/

#define COMMONLISPF /* defines common code which is then used for
		       additional options SRCHFCNS MAPFCNS POSFCNS
		       REMDUPS TIERNEY and KEYARG */
		    /* defines new versions of REVERSE REMOVE* DELETE*
			which handle all sequence types */
		    /* adds functions ELT COERCE NREVERSE BUTLAST CONCATENATE
			LIST* TYPEP;
			function STRCAT is deleted (~8000 bytes)*/
#define SRCHFCN	    /* SEARCH
		       Define COMMONLISPF first (1040 bytes)*/
#define MAPFCNS	    /* SOME EVERY NOTANY NOTEVERY MAP
		       Define COMMONLISPF first (2352 bytes)*/
#define POSFCNS	    /* POSITION-IF COUNT-IF FIND-IF
		       Define COMMONLISPF first (1504 bytes)*/
#define REMDUPS	    /* REMOVE-DUPLICATES
		       Define COMMONLISPF first (1440 bytes)*/
#define TIERNEY	    /* REDUCE, by Luke Tierney (with modifications).
		       Define COMMONLISPF first (1008 bytes)*/
#define KEYARG	    /* adds :KEY arguments where appropriate
		       Define COMMONLISPF first. (up to 1840 bytes) */

#define ADDEDTAA    /* added function by TAA: GENERIC (336 bytes) */

#define TIMES	    /* time functions TIME GET-INTERNAL-RUN-TIME
		       GET-INTERNAL-REAL-TIME and constant
		       INTERNAL-TIME-UNITS-PER-SECOND (5286 bytes)*/

#define SETS	    /* Luke Tierney's set functions ADJOIN UNION INTERSECTION
			SET-DIFFERENCE SUBSETP (1328 bytes)*/

#define RANDOM	    /* Add RANDOM-NUMBER-STATE type, *RANDOM-STATE*, and
		       function MAKE-RANDOM-STATE
		       You must also define STRUCTS and TIMES (736 bytes)*/

#define HASHFCNS    /* Hash table functions (Ken Whedbee):
		       SETHASH (SETF (SETHASH..)), MAKE-HASH-TABLE,
		       TAA's REMHASH, MAPHASH, CLRHASH, HASH-TABLE-COUNT
		       You must also define STRUCTS (2608 bytes)*/

#define STRUCTS	    /* DEFSTRUCT ASIN ACOS ATAN
		       (for xlisp 2.1 compatability) (7.5k)*/

#define APPLYHOOK   /* adds applyhook support, strangely missing before
		       (1312 bytes)*/

#define COMPLX	    /* complex numbers&more math from Luke Tierney:
			COMPLEX, COMPLEXP, IMAGPART, REALPART, CONJUGATE,
			PHASE, LOG, FLOOR, CEILING, ROUND, and PI.
			Also LCM (by Ken Whedbee) (15k bytes)*/

#define PATHNAMES "XLPATH"
		    /* Use environmental variable of same name as a search
		       path for LOAD and RESTORE commands */

#define SAVERESTORE
		    /* SAVE and RESTORE commands (an original option!)
			(3936 bytes) */

/* The following option only available for certain compilers noted
   below */

#define GRAPHICS    /* add graphics commands
			MODE COLOR MOVE DRAW MOVEREL DRAWREL
		       and screen commands CLS CLEOL GOTO-XY
			(3k) */


/*#define SMALLSIZE */ /* undefine some bulky and not very useful extensions*/


/************ END OF PREFERENCE OPTIONS **************/


/* handle dependencies */


#ifdef SMALLSIZE    /* Editorial comments: */
#undef GRAPHICS	    /* icing on cake */
#undef APPLYHOOK    /* Who really would use this? */
#undef RANDOM
#undef HASHFCNS
#undef STRUCTS	    /* should use objects */
#undef SAVERESTORE  /* A generally underutilized feature -- especially
			considering the original distribution didn't work! */
/*#undef COMPLX*/	/* Many will want to eliminate this as well */
#endif

#if defined(HASHFCNS) || defined(RANDOM)
#undef  STRUCTS /* NPM: prevents "STRUCTS redefined" cpp warning on ULTRIX */
#define STRUCTS
#endif

#ifdef RANDOM
#undef  TIMES /* NPM: prevents "TIMES redefined" cpp warning on ULTRIX */
#define TIMES
#endif

#if defined(SRCHFCN)||defined(MAPFCNS)||defined(POSFCNS)||defined(REMDUPS)||defined(TIERNEY)||defined(KEYARG)
#define COMMONLISPF
#endif

#ifdef NILSYMBOL
#undef  SPECIALS /* NPM: prevents "NILSYMBOL redefined" cpp warning on ULTRIX */
#define SPECIALS
#endif


/*************** COMPILER/ENVIRONMENT OPTIONS ****************/



/* Default compiler options: */
/* NNODES	number of nodes to allocate in each request (2000) */
/* VSSIZE	number of vector nodes to allocate in each request (6000) */
/* EDEPTH	evaluation stack depth (650) */
/* ADEPTH	argument stack depth (1000) */
/* FORWARD	type of a forward declaration () */
/* LOCAL	type of a local function (static) */
/* NEAR		function is is same segment (8086 processors) () */
/* AFMT		printf format for addresses ("%x") */
/* FIXTYPE	data type for fixed point numbers (long) */
/* MAXFIX	maximum positive value of an integer (0x7fffffffL) */
/* MAXSLEN	maximum sequence length, <= maximum unsigned, on 16 bit
		systems should be the maximum string length that can be
		malloc'ed (1000000)*/
/* MAXVLEN	maximum vector length, should normally be MAXSLEN, but on
		16 bit systems needs to be the maximum vector size that can
		be malloc'ed (MAXSLEN) */
/* ITYPE	fixed point input conversion routine type (long atol()) */
/* ICNV		fixed point input conversion routine (atol) */
/* IFMT		printf format for fixed point numbers ("%ld") */
/* FLOTYPE	data type for floating point numbers (double) */
/* OFFTYPE	number the size of an address (int) */
/* CVPTR	macro to convert an address to an OFFTYPE. We have to go
		through hoops for some MS-DOS compilers that like to
		normalize pointers. In these days of Windows, compilers
		seem to be better behaved. Change to default definition
		only after extensive testing. This is no big deal as it
		only effects the SAVE command. (OFFTYPE)(x) */
/* ALIGN32	Compiler has 32 bit ints and 32 bit alignment of struct
		elements */
/* DOSINPUT	OS specific code can read using OS's line input functon */
/* IEEEFP	IEEE FP -- proper printing of +-INF and NAN
		       for compilers that can't hack it.
		       Currently for little-endian systems. */
/* CDECL	C style declaration, for compilers that can also generate
		Pascal style, to allow calling of main() ([nothing])*/
/* ANSI		define for ANSI C compiler */
/* FNAMEMAX	Maximum size of file name strings (63) */

/* STDIO and MEM and certain STRING calls can be overridden as needed
   for various compilers or environments. By default, the standard
   library functions are used. Any substitute function must mimic the
   standard function in terms of arguments and return values */

/* OSAOPEN	Open ascii file (fopen) */
/* OSBOPEN	Open binary file (fopen) */
/* MODETYPE	Type of open mode (const char *) */
/* OPEN_RO	Open mode for read only ("r") */
/* OPEN_UPDATE	Open mode for update ("r+") */
/* CREATE_WR	Open mode for create for writing ("w") */
/* CREATE_UPDATE Open mode for create update ("w+") */
/* CLOSED	Closed file, or return value when open fails (NULL) */
/* OSGETC	Character read (fgetc) */
/* OSPUTC	Character write (fputc) */
/* OSREAD	Binary read of file (fread) */
/* OSWRITE	Binary write of file (fwrite) */
/* OSCLOSE	Close the file (fclose) */
/* OSSEEK	Seek in file (fseek(fp,loc,SEEK_SET)) */
/* OSSEEKCUR	Seek for changing direction (fseek(fp,loc,SEEK_CUR)) */
/* OSSEEKEND	Seek to end  (fseek(fp,0L,SEEK_END)) */
/* OSTELL	Tell file location (ftell) */
/* FILEP	File pointer type (FILE *),
		used in all the above functions */
/* STDIN	Standard input (a FILEP) (stdin) */
/* STDOUT	Standard output (stdout) */
/* CONSOLE	Console (stderr) */

/* MALLOC	Memory allocation (malloc) */
/* CALLOC	Memory allocation (calloc) */
/* MFREE	Memory allocation (free) */

/* These are needed in case far pointer override is necessary: */

/* STRCMP	String compare (strcmp) */
/* STRCPY	String copy (strcpy) */
/* STRNCPY	String copy (strncpy) */
/* STRCAT	String concatenate (strcat) */
/* STRLEN	String length (strlen) */
/* MEMCPY	Memory copy (memcpy) */

/******************************************************************************/
/******************************** WINTERP *************************************/
/******************************************************************************/
#ifdef WINTERP			/* WINTERP assumes UNIX, >= 32bit CPU */

#define XLISP_VERSION_INT  2	/* used in ../winterp.c */
#define XLISP_REVISION_INT 11	/* used in ../winterp.c */

#define NNODES		   2000
#define VSSIZE		   20000 /* NPM -- used only by dldmem.c which hasn't been tested with WINTERP */
#define EDEPTH		   4000
#define ADEPTH		   6000
#define AFMT		   "%lx"

/* ??? MAXSLEN ??? -- should this be 2^(sizeof(caddr_t)), not 1000000 */
/* ??? MAXVLEN ??? -- should this be 2^(sizeof(caddr_t)) == MAXSLEN */
/*	Note: MAXSLEN and MAXVLEN could be longer in WINTERP, but I'm not
	really 	sure that allocating more than a million nodes is a cool
	thing to do anyways, thus I'm leaving the default. (FYI, allocating a
	million node anything in WINTERP/XLISP will take up at least 10
	megabytes of swap...)  */

#define OFFTYPE		   long

#ifdef PATH_MAX
#define FNAMEMAX PATH_MAX	/* from <limits.h> */
#else /* !defined(PATH_MAX) */
#define FNAMEMAX 1023
#endif /* PATH_MAX */

#define OSAOPEN osaopen
extern FILE* osaopen(); /* in unixstuf.c -- fopen() w/ call to close-on-exec */

#define OSBOPEN osbopen
extern FILE* osbopen(); /* in unixstuf.c -- fopen() w/ call to close-on-exec */

/* NPM SEZ DON'T DO THIS: #define MALLOC XtMalloc */
/* NPM SEZ DON'T DO THIS: #define CALLOC XtCalloc */
/* NPM SEZ DON'T DO THIS: #define MFREE  XtFree   */

#define ALIGN32

#ifndef SEEK_SET /* NPM: prevents "SEEK_SET redefined" cpp warning on ULTRIX/Irix */
#define SEEK_SET		0
#endif /* !defined(SEEK_SET) */

#ifndef SEEK_CUR /* NPM: prevents "SEEK_CUR redefined" cpp warning on ULTRIX/Irix */
#define SEEK_CUR		1
#endif /* !defined(SEEK_CUR) */

#ifndef SEEK_END /* NPM: prevents "SEEK_END redefined" cpp warning on ULTRIX/Irix */
#define SEEK_END		2
#endif /* !defined(SEEK_END) */

/* ??? MAXFIX ??? */
/* Note: For MAXFIX, the default value for this seems ok, only used in ABS
   (xlmath2.c) */ 

#ifndef OBJPRNT			/* OBJPRNT *must* be defined for WINTERP! */
#define OBJPRNT
#endif /* OBJPRNT */

#ifndef PATHNAMES		/* PATHNAMES *must* be defined for WINTERP! -- see ospopen() in w_utils.c */
#define PATHNAMES "XLPATH"
#endif /* PATHNAMES */

#undef GRAPHICS			/* for MSDOS only */
#undef MEDMEM			/* for MSDOS only */
#undef FILETABLE		/* not ported */
#undef RANDOM			/* don't need this for UI stuff */
#undef TIMES			/* not really needed, tell of compile option */

#undef  COMPLX /* NPM: prevents "COMPLX redefined" cpp warning on ULTRIX */
#define COMPLX			/* required for storing xtango locations */

/*
 * NOTE: WINTERP breaks save/restore functionality because I haven't
 * implemented a way to make Xtoolkit/Motif objects (WIDGETOBJs, PIXMAPs,
 * PIXELs, CALLBACKOBJs, TIMEOUTOBJs, EVHANDLEROBJs, FDINPUTCBOBJs) persist
 * across invocations (difficult to do given part of state is kept in X server).
 * Currently all the code in xlimage.c just assumes that
 * all pointers are xlisp pointers and doesn't try to handle pointers to
 * objects created outside of xlisp. Implementing save/restore for WINTERP is
 * doable, but is not high on my priority list right now. Note that WINTERP
 * doesn't break save/restore for standard xlisp objects. However, since it
 * doesn't work correctly I've disabled it for now... I plan to get it working
 * in the next release...
 */
#undef SAVERESTORE

 /*
  * Use whatever is provided by imake and X Consortium Configuration Files
  * To determine whether we're using an ANSI compiler. In XLISP, #ifdef ANSI
  * is used to determine whether to use prototypes in headers, which is 
  * precisely what _NO_PROTO is for.
  */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
#define ANSI
#else /* defined(_NO_PROTO) */
#undef ANSI
#endif /* _NO_PROTO */

#endif /* WINTERP */
/******************************************************************************/
/******************************** WINTERP *************************************/
/******************************************************************************/

/* for Zortech C  -- Versions 2.0 and above, please */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/25 */
#ifdef __ZTC__
#define MAXSLEN		(65519U)
#define MAXVLEN		(16379U)
#define ANSI
#define AFMT		"%lx"
#define OFFTYPE		unsigned long
/* #define CVPTR(x)	((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))*/
#define IEEEFP
#define CDECL	_cdecl
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#define NEAR _near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#undef MEDMEM	/* doesn't work, as of V2.1 */
#endif

/* for the Turbo C compiler - MS-DOS, large or medium model */
/* Version 1.5 and 2.0.	 1.5 won't compile with TIMES */
/* Also for Turbo/Borland C++, as a C compiler */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/25 */
#ifdef __TURBOC__
#define MAXSLEN		(65519U)
#define MAXVLEN		(16383U)
#define ANSI
#define AFMT		"%lx"
#define OFFTYPE		unsigned long
#ifdef MEDMEM
#define CVPTR(x)	(unsigned long)(x)
#else
#define CVPTR(x)	((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#endif
#if __TURBOC__ < 0x297
#define IEEEFP		/* Borland C++ V2.0 or later handles this */
#endif
#define CDECL _Cdecl
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#define NEAR near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _Cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif

/* for the JPI TopSpeed C Compiler, Medium or Large memory model */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/25 */
#ifdef __TSC__
#pragma data(heap_size=>4096,stack_size=>16384)
#define IEEEFP
#define MAXSLEN		(65519U)
#define MAXVLEN		(16379U)
#define ANSI
#define AFMT		"%lx"
#define OFFTYPE		unsigned long
#ifdef MEDMEM
#define CVPTR(x)	(unsigned long)(x)
#else
#define CVPTR(x)	((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#endif
#define CDECL		/* don't use CDECL with this compiler */
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#define NEAR near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE *osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#endif

/* for the Microsoft C compiler - MS-DOS, large model */
/* Version 5.0.	 Avoid optimizations. Should work with earlier as well. */
/* Version 6.0A. Most opts ok. Avoid those that conflict with longjump */
/* GRAPHICS ok */
/* EDEPTH should be stacksize/25 */
#ifdef MSC
#define MAXSLEN		(65519U)
#define MAXVLEN		(16379U)
#define ANSI
#define AFMT		"%lx"
#define OFFTYPE		long
#define CVPTR(x)	((((unsigned long)(x) >> 16) << 4) + ((unsigned) x))
#define CDECL _cdecl
#define DOSINPUT
#undef JMAC	    /* not worth effort if cramped for space */
#define NEAR _near
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE * _cdecl osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#undef MEDMEM	    /* Except for Windows, in the future */
#endif

/* for 80386, Metaware High-C386 */
/* GRAPHICS ok -- Special fast graphics code, this
   version works only for EGA/VGA/Enhanced EorVGA modes! */
/* Tested with Versions 1.3, 1.4, and 1.5 */
#ifdef __HIGHC__
/* default EDEPTH=2000, at stacksize/34, requires stack of 68000 */
#define EDEPTH 4000
#define ADEPTH 6000
#define VSSIZE 20000
#define ALIGN32
#define ANSI
#define DOSINPUT
extern long myftell(FILE *fp);	/* ftell is broken at least through v1.62) */
#ifdef FILETABLE
#define OSTELL(f) myftell(filetab[f].fp)
#else
#define OSTELL myftell
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE *osbopen(const char *name, const char *mode);   /* open binary file */
#endif
#undef MEDMEM
#endif

/* for NDP386 system */
/* tested with version 1.4 */
#ifdef NDP386
#define ADEPTH	3000
#define EDEPTH	2000
#define VSSIZE 20000
/* these definitions point out the deficiencies of NDP */
extern void *malloc();
extern void *calloc();
extern void free();
#define	 SEEK_CUR 1
#define	 SEEK_END 2
#define	 SEEK_SET 0
#undef GRAPHICS
#define DOSINPUT
#ifndef FILETABLE
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE *osbopen(); /* open binary file */
#endif
#undef MEDMEM
#undef FILETABLE	/* not ported */
#endif

/* For GCC on MSDOS (see GCCSTUFF.C) */
/* for now graphics is pretty clunky, as well */
#ifdef GCC
#define EDEPTH 4000
#define ADEPTH 6000
#define VSSIZE 20000
#define ALIGN32
#define ANSI
#define	 SEEK_CUR 1
#define	 SEEK_END 2
#define	 SEEK_SET 0
#define IEEEFP
#ifdef FILETABLE
#ifdef BETTERIO
#define OSGETC osgetc
#define OSPUTC osputc
extern int osgetc(int), osputc(int,int);
#endif
#else
#define OSAOPEN osaopen /* special mode for ASCII files */
extern FILE *osaopen(const char *name, const char *mode);   /* open ASCII file */
#define OSBOPEN osbopen /* special mode for binary files */
extern FILE *osbopen(const char *name, const char *mode);   /* open binary file */
#ifdef BETTERIO
/* library improperly handles ASCII files re lseek() */
#define OSGETC osgetc
#define OSPUTC osputc
extern int osgetc(FILE*), osputc(int,FILE*);
#endif
#endif
#define DOSINPUT
#undef MEDMEM
#endif

/* for BSD & SYSV Unix. */
/* Also define BSD in BSD or SUN systems */
#ifdef UNIX
#define EDEPTH 4000
#define ADEPTH 6000
#define ALIGN32
#define AFMT			"%lx"
#define SEEK_SET		0
#define SEEK_CUR		1
#define SEEK_END		2
#undef GRAPHICS
#undef MEDMEM
#undef FILETABLE	/* not ported */
#endif

/*>>>>>>> For other systems -- You are on your own! */

/* Handle the FILETABLE specification -- non-windows */
#ifdef FILETABLE
#define FTABSIZE 13
#define FILEP int
#define CLOSED (-1)	/* because FILEP is now table index */
#define STDIN (0)
#define STDOUT (1)
#define CONSOLE (2)
#ifndef OSAOPEN
#define OSAOPEN osaopen
extern FILEP osaopen(const char *name, const char *mode);
#endif
#ifndef OSBOPEN
#define OSBOPEN osbopen
extern FILEP osbopen(const char *name, const char *mode);
#endif
#ifndef OSGETC
#define OSGETC(f) fgetc(filetab[f].fp)
#endif
#ifndef OSPUTC
#define OSPUTC(i,f) fputc(i,filetab[f].fp)
#endif
#ifndef OSREAD
#define OSREAD(x,y,z,f) fread(x,y,z,filetab[f].fp)
#endif
#ifndef OSWRITE
#define OSWRITE(x,y,z,f) fwrite(x,y,z,filetab[f].fp)
#endif
#ifndef OSCLOSE
#define OSCLOSE osclose
extern void osclose(int i); /* we must define this */
#endif
#ifndef OSSEEK
#define OSSEEK(f,loc) fseek(filetab[f].fp,loc,SEEK_SET)
#endif
#ifndef OSSEEKEND
#define OSSEEKEND(f) fseek(filetab[f].fp,0L,SEEK_END)
#endif
#ifndef OSSEEKCUR
#define OSSEEKCUR(f,loc) fseek(filetab[f].fp,loc,SEEK_CUR)
#endif
#ifndef OSTELL
#define OSTELL(f) ftell(filetab[f].fp)
#endif
#endif /* FILETABLE */


/* Handle the MEDMEM specification */
#ifdef MEDMEM
#include <alloc.h>
#define NILSYMBOL   /* We will need this */
#define STATICSTK   /* We will need this */
#define FAR far
#define STRCMP _fstrcmp
#define STRCPY _fstrcpy
#define STRNCPY _fstrncpy
#define STRCAT _fstrcat
#define STRLEN _fstrlen
#define MEMCPY _fmemcpy
#ifdef __TSC__
#define MALLOC _fmalloc
#define CALLOC _fcalloc
#define MFREE  _ffree
#endif
#ifdef __TURBOC__
#define MALLOC farmalloc
#define CALLOC farcalloc
#define MFREE farfree
#endif
#endif /* MEDMEM */

/************ DEFAULT DEFINITIONS  ******************/
#ifndef NNODES
#define NNODES		2000
#endif
#ifndef VSSIZE
#define VSSIZE		6000
#endif
#ifndef EDEPTH
#define EDEPTH		650
#endif
#ifndef ADEPTH
#define ADEPTH		1000
#endif
#ifndef FORWARD
#define FORWARD
#endif
#ifndef LOCAL
#define LOCAL		static
#endif
#ifndef AFMT
#define AFMT		"%x"
#endif
#ifndef FIXTYPE
#define FIXTYPE		long
#endif
#ifdef ANSI /* ANSI C Compilers already define this! */
#include <limits.h>
#define MAXFIX	LONG_MAX
#else
#ifndef MAXFIX
#define MAXFIX		(0x7fffffffL)
#endif
#endif
#ifndef MAXSLEN
#define MAXSLEN		(1000000)   /* no sequences longer than this */
#endif
#ifndef MAXVLEN
#define MAXVLEN		MAXSLEN
#endif
#ifndef WINTERP			/* ITYPE not used anywhere, conflicts w/ ../xtango/xtangosup.h */
#ifndef ITYPE
#define ITYPE		long atol()
#endif
#endif /* WINTERP */
#ifndef ICNV
#define ICNV(n)		atol(n)
#endif
#ifndef IFMT
#define IFMT		"%ld"
#endif
#ifndef FLOTYPE
#define FLOTYPE		double
#endif
#ifndef OFFTYPE
#define OFFTYPE		int
#endif
#ifndef CVPTR
#define CVPTR(x)	((OFFTYPE)(x))
#endif
#ifndef VOID
#define VOID		void
#endif
#ifdef ANSI
#define VOIDP	void
#else
#define VOIDP
#endif
#ifndef CDECL
#define CDECL
#endif
#ifndef NEAR
#define NEAR
#endif
#ifndef FAR
#define FAR
#endif
#ifndef FNAMEMAX
#define FNAMEMAX 63
#endif
#ifndef OSAOPEN
#define OSAOPEN fopen
#endif
#ifndef OSBOPEN
#define OSBOPEN fopen
#endif
#ifndef MODETYPE
#define MODETYPE const char *
#endif
#ifndef OPEN_RO
#define OPEN_RO "r"
#endif
#ifndef OPEN_UPDATE
#define OPEN_UPDATE "r+"
#endif
#ifndef CREATE_WR
#define CREATE_WR "w"
#endif
#ifndef CREATE_UPDATE
#define CREATE_UPDATE "w+"
#endif
#ifndef CLOSED
#define CLOSED NULL
#endif
#ifndef OSGETC
#define OSGETC fgetc
#endif
#ifndef OSPUTC
#define OSPUTC fputc
#endif
#ifndef OSREAD
#define OSREAD fread
#endif
#ifndef OSWRITE
#define OSWRITE fwrite
#endif
#ifndef OSCLOSE
#define OSCLOSE fclose
#endif
#ifndef OSSEEK
#define OSSEEK(fp,loc) fseek(fp,loc,SEEK_SET)
#endif
#ifndef OSSEEKEND
#define OSSEEKEND(fp) fseek(fp,0L,SEEK_END)
#endif
#ifndef OSSEEKCUR
#define OSSEEKCUR(fp,loc) fseek(fp,loc,SEEK_CUR)
#endif
#ifndef OSTELL
#define OSTELL ftell
#endif
#ifndef FILEP
#define FILEP FILE *
#endif
#ifndef STDIN
#define STDIN stdin
#endif
#ifndef STDOUT
#define STDOUT stdout
#endif
#ifndef CONSOLE
#define CONSOLE stderr
#endif
#ifndef MALLOC
#define MALLOC malloc
#endif
#ifndef CALLOC
#define CALLOC calloc
#endif
#ifndef MFREE
#define MFREE free
#endif
#ifndef STRCMP
#define STRCMP strcmp
#endif
#ifndef STRCPY
#define STRCPY strcpy
#endif
#ifndef STRNCPY
#define STRNCPY strncpy
#endif
#ifndef STRCAT
#define STRCAT strcat
#endif
#ifndef STRLEN
#define STRLEN strlen
#endif
#ifndef MEMCPY
#define MEMCPY memcpy
#endif

/* useful definitions */
#ifndef TRUE
#define TRUE	1
#endif
#ifndef FALSE
#define FALSE	0
#endif
#ifndef NIL
#define NIL	(LVAL )0
#endif

#ifdef COMPLX
#define PI 3.14159265358979323846
#endif

#ifdef ANSI
#include <stdlib.h>
#endif

/************* END OF COMPILER/ENVIRONMENT OPTIONS ************/



/* include the dynamic memory definitions */
#include "xldmem.h"

/* program limits */
#ifdef WINTERP
#define STRMAX		BUFSIZ          /* increase size of buffer to accommodate longer error messages in WINTERP... should be at least length of winterp.c:temptext[] */
#define HSIZE		1019		/* increase size of symbol hash table to accommodate more symbols/fns/methods */
#else
#define STRMAX		100		/* maximum length of a string constant */
#define HSIZE		199		/* symbol hash table size */
#endif /* WINTERP */
#define SAMPLE		100		/* control character sample rate */

/* function table offsets for the initialization functions */
#define FT_RMHASH	0
#define FT_RMQUOTE	1
#define FT_RMDQUOTE	2
#define FT_RMBQUOTE	3
#define FT_RMCOMMA	4
#define FT_RMLPAR	5
#define FT_RMRPAR	6
#define FT_RMSEMI	7
#define FT_CLNEW	10
#define FT_CLISNEW	11
#define FT_CLANSWER	12
#define FT_OBISNEW	13
#define FT_OBCLASS	14
#define FT_OBSHOW	15
#ifdef OBJPRNT
#define FT_OBPRIN1	16
#endif

/* macro to push a value onto the argument stack */
#define pusharg(x)	{if (xlsp >= xlargstktop) xlargstkoverflow();\
			 *xlsp++ = (x);}

/* macros to protect pointers */
#define xlstkcheck(n)	{if (xlstack - (n) < xlstkbase) xlstkoverflow();}
#define xlsave(n)	{*--xlstack = &n; n = NIL;}
#define xlprotect(n)	{*--xlstack = &n;}

/* check the stack and protect a single pointer */
#define xlsave1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
			 *--xlstack = &n; n = NIL;}
#define xlprot1(n)	{if (xlstack <= xlstkbase) xlstkoverflow();\
			 *--xlstack = &n;}

/* macros to pop pointers off the stack */
#define xlpop()		{++xlstack;}
#define xlpopn(n)	{xlstack+=(n);}

/* macros to manipulate the lexical environment */
#define xlframe(e)	cons(NIL,e)
#ifndef SPECIALS
#define xlbind(s,v)	xlpbind(s,v,xlenv)
#endif
#define xlfbind(s,v)	xlpbind(s,v,xlfenv);
#define xlpbind(s,v,e)	{rplaca(e,cons(cons(s,v),car(e)));}

/* macros to manipulate the dynamic environment */
#define xldbind(s,v)	{xldenv = cons(cons(s,getvalue(s)),xldenv);\
			 setvalue(s,v);}
#define xlunbind(e)	{for (; xldenv != (e); xldenv = cdr(xldenv))\
			   setvalue(car(car(xldenv)),cdr(car(xldenv)));}

/* macro to manipulate dynamic and lexical environment */

#ifdef SPECIALS
#define xlbind(s,v) {if (specialp(s)) xldbind(s,v) else xlpbind(s,v,xlenv)}
#define xlpdbind(s,v,e) {e = cons(cons(s,getvalue(s)),e);\
			 setvalue(s,v);}
#endif /* SPECIALS */

/* type predicates */
#if defined(__TURBOC__) && defined(NILSYMBOL)
#define null(x)		(((unsigned)(void _seg *)(x)) == ((unsigned)(void _seg *) NIL))
#else
#if defined(MSC) && defined(NILSYMBOL)
#define null(x)		(((unsigned)(_segment *)(x)) == ((unsigned)(_segment *) NIL))
#else
#define null(x)		((x) == NIL)
#endif
#endif
#define atom(x)		(null(x) || ntype(x) != CONS)
#define listp(x)	(null(x) || ntype(x) == CONS)

#ifdef NILSYMBOL

#define consp(x)	(ntype(x) == CONS)
#define subrp(x)	(ntype(x) == SUBR)
#define fsubrp(x)	(ntype(x) == FSUBR)
#define stringp(x)	(ntype(x) == STRING)
#define symbolp(x)	(ntype(x) == SYMBOL)
#if (defined(UNIX) || defined(WINTERP))
#define streamp(x)	((ntype(x) == STREAM) || (ntype(x) == XLTYPE_PIPE))
#else /* !(defined(UNIX) || defined(WINTERP)) */
#define streamp(x)	(ntype(x) == STREAM)
#endif /* defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
#ifdef WINTERP_XTANGO_WIDGET
#define objectp(x)	((ntype(x) == OBJECT) || (ntype(x) == XLTYPE_WIDGETOBJ) || (ntype(x) == XLTYPE_TANGOIMAGEOBJ))
#else
#define objectp(x)	((ntype(x) == OBJECT) || (ntype(x) == XLTYPE_WIDGETOBJ))
#endif /* WINTERP_XTANGO_WIDGET */
#else /* !defined(WINTERP) */
#define objectp(x)	(ntype(x) == OBJECT)
#endif /* WINTERP */
#define fixp(x)		(ntype(x) == FIXNUM)
#define floatp(x)	(ntype(x) == FLONUM)
#ifdef COMPLX
#define complexp(x)	(ntype(x) == COMPLEX)
#define numberp(x)	(ntype(x) == FIXNUM || ntype(x) == FLONUM)
#endif /* COMPLX */
#define vectorp(x)	(ntype(x) == VECTOR)
#define closurep(x)	(ntype(x) == CLOSURE)
#define charp(x)	(ntype(x) == CHAR)
#define ustreamp(x)	(ntype(x) == USTREAM)
#ifdef STRUCTS
#define structp(x)	(ntype(x) == STRUCT)
#endif /* STRUCTS */
#if (defined(UNIX) || defined(WINTERP))
#define pipe_p(x)	(ntype(x) == XLTYPE_PIPE)
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
#define xtresource_p(x)  (ntype(x) == XLTYPE_XT_RESOURCE)
#define pixel_p(x)       (ntype(x) == XLTYPE_Pixel)
#define pixmap_p(x)      (ntype(x) == XLTYPE_Pixmap)
#define ximage_p(x)      (ntype(x) == XLTYPE_XImage)
#define callbackobj_p(x) (ntype(x) == XLTYPE_CALLBACKOBJ)
#define timeoutobj_p(x)  (ntype(x) == XLTYPE_TIMEOUTOBJ)
#define widgetobj_p(x)   (ntype(x) == XLTYPE_WIDGETOBJ)
#define xmstring_p(x)	 (ntype(x) == XLTYPE_XmString)
#define xevent_p(x)      (ntype(x) == XLTYPE_XEvent)
#define xtaccelerators_p(x) (ntype(x) == XLTYPE_XtAccelerators)
#define xttranslations_p(x) (ntype(x) == XLTYPE_XtTranslations)
#define evhandlerobj_p(x)   (ntype(x) == XLTYPE_EVHANDLEROBJ)
#define fdinputcbobj_p(x)   (ntype(x) == XLTYPE_FDINPUTCBOBJ)
#define window_p(x)	  (ntype(x) == XLTYPE_Window)
#ifdef WINTERP_XTANGO_WIDGET
#define tangoimageobj_p(x) (ntype(x) == XLTYPE_TANGOIMAGEOBJ)
#define tangopath_p(x)     (ntype(x) == XLTYPE_TANGO_PATH)
#define tangotrans_p(x)    (ntype(x) == XLTYPE_TANGO_TRANS)
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */

#else /* !defined(NILSYMBOL) */

#define consp(x)	((x) && ntype(x) == CONS)
#define subrp(x)	((x) && ntype(x) == SUBR)
#define fsubrp(x)	((x) && ntype(x) == FSUBR)
#define stringp(x)	((x) && ntype(x) == STRING)
#define symbolp(x)	((x) && ntype(x) == SYMBOL)
#if (defined(UNIX) || defined(WINTERP))
#define streamp(x)	((x) && ((ntype(x) == STREAM) || (ntype(x) == XLTYPE_PIPE)))
#else /* !(defined(UNIX) || defined(WINTERP)) */
#define streamp(x)	((x) && ntype(x) == STREAM)
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
#ifdef WINTERP_XTANGO_WIDGET
#define objectp(x)	((x) && ((ntype(x) == OBJECT) || (ntype(x) == XLTYPE_WIDGETOBJ) || (ntype(x) == XLTYPE_TANGOIMAGEOBJ)))
#else
#define objectp(x)	((x) && ((ntype(x) == OBJECT) || (ntype(x) == XLTYPE_WIDGETOBJ)))
#endif /* WINTERP_XTANGO_WIDGET */
#else /* !defined(WINTERP) */
#define objectp(x)	((x) && ntype(x) == OBJECT)
#endif /* WINTERP */
#define fixp(x)		((x) && ntype(x) == FIXNUM)
#define floatp(x)	((x) && ntype(x) == FLONUM)
#ifdef COMPLX
#define complexp(x)	((x) && ntype(x) == COMPLEX)
#define numberp(x)	((x) && (ntype(x) == FIXNUM || ntype(x) == FLONUM))
#endif /* COMPLX */
#define vectorp(x)	((x) && ntype(x) == VECTOR)
#define closurep(x)	((x) && ntype(x) == CLOSURE)
#define charp(x)	((x) && ntype(x) == CHAR)
#define ustreamp(x)	((x) && ntype(x) == USTREAM)
#ifdef STRUCTS
#define structp(x)	((x) && ntype(x) == STRUCT)
#endif /* STRUCTS */
#if (defined(UNIX) || defined(WINTERP))
#define pipe_p(x)       ((x) && (ntype(x) == XLTYPE_PIPE))
#endif /* (defined(UNIX) || defined(WINTERP)) */
#ifdef WINTERP
#define xtresource_p(x)  ((x) && (ntype(x) == XLTYPE_XT_RESOURCE))
#define pixel_p(x)       ((x) && (ntype(x) == XLTYPE_Pixel))
#define pixmap_p(x)      ((x) && (ntype(x) == XLTYPE_Pixmap))
#define ximage_p(x)      ((x) && (ntype(x) == XLTYPE_XImage))
#define callbackobj_p(x) ((x) && (ntype(x) == XLTYPE_CALLBACKOBJ))
#define timeoutobj_p(x)  ((x) && (ntype(x) == XLTYPE_TIMEOUTOBJ))
#define widgetobj_p(x)   ((x) && (ntype(x) == XLTYPE_WIDGETOBJ))
#define xmstring_p(x)	 ((x) && (ntype(x) == XLTYPE_XmString))
#define xevent_p(x)      ((x) && (ntype(x) == XLTYPE_XEvent))
#define xtaccelerators_p(x) ((x) && (ntype(x) == XLTYPE_XtAccelerators))
#define xttranslations_p(x) ((x) && (ntype(x) == XLTYPE_XtTranslations))
#define evhandlerobj_p(x)   ((x) && (ntype(x) == XLTYPE_EVHANDLEROBJ))
#define fdinputcbobj_p(x) ((x) && (ntype(x) == XLTYPE_FDINPUTCBOBJ))
#define window_p(x)	  ((x) && (ntype(x) == XLTYPE_Window))
#ifdef WINTERP_XTANGO_WIDGET
#define tangoimageobj_p(x) ((x) && (ntype(x) == XLTYPE_TANGOIMAGEOBJ))
#define tangopath_p(x)     ((x) && (ntype(x) == XLTYPE_TANGO_PATH))
#define tangotrans_p(x)    ((x) && (ntype(x) == XLTYPE_TANGO_TRANS))
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */
#endif /* NILSYMBOL */

#define boundp(x)	(getvalue(x) != s_unbound)
#define fboundp(x)	(getfunction(x) != s_unbound)

/* shorthand functions */
#define consa(x)	cons(x,NIL)
#define consd(x)	cons(NIL,x)

/* argument list parsing macros */
#define xlgetarg()	(testarg(nextarg()))
#define xllastarg()	{if (xlargc != 0) xltoomany();}
#define testarg(e)	(moreargs() ? (e) : xltoofew())
#define typearg(tp)	(tp(*xlargv) ? nextarg() : xlbadtype(*xlargv))
#define nextarg()	(--xlargc, *xlargv++)
#define moreargs()	(xlargc > 0)

/* macros to get arguments of a particular type */
#define xlgacons()	(testarg(typearg(consp)))
#define xlgalist()	(testarg(typearg(listp)))
#define xlgasymbol()	(testarg(typearg(symbolp)))
#ifdef NILSYMBOL
#define xlgasymornil()	(testarg(typearg(symbolp)))
#else
#define xlgasymornil()	(*xlargv==NIL || symbolp(*xlargv) ? nextarg() : xlbadtype(*xlargv))
#endif
#define xlgastring()	(testarg(typearg(stringp)))
#ifdef COMMONLISP
#define xlgastrorsym()	(testarg(symbolp(*xlargv) ? getpname(nextarg()) : typearg(stringp)))
#else
#define xlgastrorsym()	xlgastring()
#endif
#define xlgaobject()	(testarg(typearg(objectp)))
#define xlgafixnum()	(testarg(typearg(fixp)))
#define xlgaflonum()	(testarg(typearg(floatp)))
#define xlgachar()	(testarg(typearg(charp)))
#define xlgavector()	(testarg(typearg(vectorp)))
#define xlgastream()	(testarg(typearg(streamp)))
#define xlgaustream()	(testarg(typearg(ustreamp)))
#define xlgaclosure()	(testarg(typearg(closurep)))
#ifdef STRUCTS
#define xlgastruct()	(testarg(typearg(structp)))
#endif

#if (defined(UNIX) || defined(WINTERP))
#define xlga_pipe()	(testarg(typearg(pipe_p)))
#endif /* (defined(UNIX) || defined(WINTERP)) */

#ifdef WINTERP
#define xlga_timeoutobj()	(testarg(typearg(timeoutobj_p)))
#define xlga_callbackobj()	(testarg(typearg(callbackobj_p)))
#define xlga_ximage()		(testarg(typearg(ximage_p)))
#define xlga_widgetobj()	(testarg(typearg(widgetobj_p)))
#define xlga_xevent()		(testarg(typearg(xevent_p)))
#define xlga_xttranslations()	(testarg(typearg(xttranslations_p)))
#define xlga_xtaccelerators()	(testarg(typearg(xtaccelerators_p)))
#define xlga_evhandlerobj()	(testarg(typearg(evhandlerobj_p)))
#define xlga_xmstring()		(testarg(typearg(xmstring_p)))
#define xlga_pixel()		(testarg(typearg(pixel_p)))
#define xlga_fdinputcbobj()	(testarg(typearg(fdinputcbobj_p)))
#define xlga_window()           (testarg(typearg(window_p)))
#ifdef WINTERP_XTANGO_WIDGET
#define xlga_tangoimageobj()	(testarg(typearg(tangoimageobj_p)))
#define xlga_tangopath()	(testarg(typearg(tangopath_p)))
#define xlga_tangotrans()	(testarg(typearg(tangotrans_p)))
#endif /* WINTERP_XTANGO_WIDGET */
#endif /* WINTERP */

/* FILETABLE specification -- non-windows */
#ifdef FILETABLE
typedef struct {
    FILE *fp;
    char name[FNAMEMAX];
} FILETABLETYPE;
extern FILETABLETYPE filetab[FTABSIZE];
#endif

/* function definition structure */
typedef struct {
    char *fd_name;	/* function name */
    int fd_type;	/* function type */
    LVAL (*fd_subr)();	/* function entry point */
} FUNDEF;

/* execution context flags */
#define CF_GO		0x0001
#define CF_RETURN	0x0002
#define CF_THROW	0x0004
#define CF_ERROR	0x0008
#define CF_CLEANUP	0x0010
#define CF_CONTINUE	0x0020
#define CF_TOPLEVEL	0x0040
#define CF_BRKLEVEL	0x0080
#define CF_UNWIND	0x0100

/* execution context */
#ifdef STATICSTK
typedef LVAL NEAR *FRAMEP;
typedef struct context {
    int c_flags;			/* context type flags */
    LVAL c_expr;			/* expression (type dependent) */
    jmp_buf c_jmpbuf;			/* longjmp context */
    struct context *c_xlcontext;	/* old value of xlcontext */
    LVAL * NEAR *c_xlstack;		/* old value of xlstack */
    LVAL NEAR *c_xlargv;		/* old value of xlargv */
    int c_xlargc;			/* old value of xlargc */
    LVAL NEAR *c_xlfp;			/* old value of xlfp */
    LVAL NEAR *c_xlsp;			/* old value of xlsp */
    LVAL c_xlenv;			/* old value of xlenv */
    LVAL c_xlfenv;			/* old value of xlfenv */
    LVAL c_xldenv;			/* old value of xldenv */
} CONTEXT;
#else
typedef LVAL *FRAMEP;
typedef struct context {
    int c_flags;			/* context type flags */
    LVAL c_expr;			/* expression (type dependent) */
    jmp_buf c_jmpbuf;			/* longjmp context */
    struct context *c_xlcontext;	/* old value of xlcontext */
    LVAL * *c_xlstack;			/* old value of xlstack */
    LVAL *c_xlargv;			/* old value of xlargv */
    int c_xlargc;			/* old value of xlargc */
    LVAL *c_xlfp;			/* old value of xlfp */
    LVAL *c_xlsp;			/* old value of xlsp */
    LVAL c_xlenv;			/* old value of xlenv */
    LVAL c_xlfenv;			/* old value of xlfenv */
    LVAL c_xldenv;			/* old value of xldenv */
} CONTEXT;
#endif


/* external variables */

#ifdef STATICSTK
extern LVAL * NEAR xlstkbase[];	    /* evaluation stack */
extern LVAL * NEAR *xlstack;		/* evaluation stack pointer */
#define xlstktop (&xlstkbase[EDEPTH])	/* top of the evaluation stack */
extern LVAL NEAR xlargstkbase[];	/* base of the argument stack */
#define xlargstktop (&xlargstkbase[ADEPTH]) /* top of the argument stack */
extern LVAL NEAR *xlfp;		    /* argument frame pointer */
extern LVAL NEAR *xlsp;		    /* argument stack pointer */
extern LVAL NEAR *xlargv;	    /* current argument vector */
#else /* !defined(STATICSTK) */
extern LVAL **xlstktop;		/* top of the evaluation stack */
extern LVAL **xlstkbase;	/* base of the evaluation stack */
extern LVAL **xlstack;		/* evaluation stack pointer */
extern LVAL *xlargstkbase;	/* base of the argument stack */
extern LVAL *xlargstktop;	/* top of the argument stack */
extern LVAL *xlfp;		/* argument frame pointer */
extern LVAL *xlsp;		/* argument stack pointer */
extern LVAL *xlargv;		/* current argument vector */
#endif /* STATICSTK */
extern int xlargc;		/* current argument count */

#ifdef ANSI
/* We need to be more thorough here!*/
/* OS system interface */
extern VOID oscheck(void);	/* check for control character during exec */
extern VOID osinit(char *banner);   /* initialize os interface */
extern VOID osfinish(void);	/* restore os interface */
extern VOID osflush(void);	/* flush terminal input buffer */
extern long  osrand(long);	/* next random number in sequence */
#ifdef PATHNAMES
extern FILEP ospopen(char *name, int ascii);	/* open file using path */
#endif /* PATHNAMES */
extern VOID xoserror(char *msg);/* print an error message */
extern int  ostgetc(void);	/* get a character from the terminal */
extern VOID ostputc(int ch);	/* put a character to the terminal */
#ifdef TIMES
extern unsigned long
    ticks_per_second(void), run_tick_count(void), real_tick_count(void);
#endif /* TIMES */
#ifdef BETTERIO
extern int renamebackup(char *filename);
#endif /* BETTERIO */

/* for xlisp.c */
extern void xlrdsave(LVAL expr);
extern void xlevsave(LVAL expr);
extern void xlfatal(char *msg);
extern void wrapup(void);

/* for xleval */
extern LVAL xlxeval(LVAL expr);
extern void xlabind(LVAL fun, int argc, LVAL *argv);
extern void xlfunbound(LVAL sym);
extern void xlargstkoverflow(void);
extern int  macroexpand(LVAL fun, LVAL args, LVAL *pval);
extern int  pushargs(LVAL fun, LVAL args);
extern LVAL makearglist(int argc, LVAL *argv);
extern void xlunbound(LVAL sym);
extern void xlstkoverflow(void);

/* for xlio */
extern int xlgetc(LVAL fptr);
extern void xlungetc(LVAL fptr, int ch);
extern int xlpeek(LVAL fptr);
extern void xlputc(LVAL fptr, int ch);
extern void xlflush(void);
extern void stdprint(LVAL expr);
extern void stdputstr(char *str);
extern void errprint(LVAL expr);
extern void errputstr(char *str);
extern void dbgprint(LVAL expr);
extern void dbgputstr(char *str);
extern void trcprin1(LVAL expr);
extern void trcputstr(char *str);

/* for xlprin */
extern void xlputstr(LVAL fptr, char *str);
extern void xlprint(LVAL fptr, LVAL vptr, int flag);
#ifdef PRINDEPTH
extern void xlprintl(LVAL fptr, LVAL vptr, int flag);
#endif /* PRINDEPTH */
#ifdef BETTERIO
extern int  xlgetcolumn(LVAL fptr);
extern int  xlfreshline(LVAL fptr);
#endif /* BETTERIO */
extern void xlterpri(LVAL fptr);
extern void xlputstr(LVAL fptr, char* str);

/* for xljump */
extern void xljump(CONTEXT *target, int mask, LVAL val);
extern void xlbegin(CONTEXT *cptr, int flags, LVAL expr);
extern void xlend(CONTEXT *cptr);
extern void xlgo(LVAL label);
extern void xlreturn(LVAL name, LVAL val);
extern void xlthrow(LVAL tag, LVAL val);
extern void xlsignal(char FAR *emsg, LVAL arg);
extern void xltoplevel(void);
extern void xlbrklevel(void);
extern void xlcleanup(void);
extern void xlcontinue(void);

/* for xllist */
#ifdef HASHFCNS
extern VOID xlsetgethash(LVAL key, LVAL table, LVAL value);
#endif /* HASHFCNS */

/* for xlsubr */
extern int xlgetkeyarg(LVAL key, LVAL *pval);
extern int xlgkfixnum(LVAL key, LVAL *pval);
extern void xltest(LVAL *pfcn, int *ptresult);
extern int needsextension(char *name);
extern int eql(LVAL arg1, LVAL arg2);
extern int equal(LVAL arg, LVAL arg2);
#ifdef KEYARG
extern LVAL xlkey(void);
extern LVAL xlapp1(LVAL fun, LVAL arg);
extern int dotest1(LVAL arg1, LVAL fun, LVAL kfun);
extern int dotest2(LVAL arg1, LVAL arg2, LVAL fun, LVAL kfun);
extern int dotest2s(LVAL arg1, LVAL arg2, LVAL fun, LVAL kfun);
#else /* !defined(KEYARG) */
extern int dotest1(LVAL arg1, LVAL fun);
extern int dotest2(LVAL arg1, LVAL arg2, LVAL fun);
#endif /* KEYARG */
#ifdef COMPLX
extern FLOTYPE makefloat(LVAL arg);
#endif /* COMPLX */

/* for xlobj */
extern int xlobsetvalue(LVAL pair, LVAL sym, LVAL val);
extern int xlobgetvalue(LVAL pair, LVAL sym, LVAL *pval);
#ifdef OBJPRNT
extern void putobj(LVAL fptr, LVAL obj);
#endif /* OBJPRNT */
#ifdef WINTERP
extern LVAL xlclass(char *name, int vcnt); /* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern int  xlclass_p(LVAL o_class); /* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern int  getivcnt(LVAL cls, int ivar); /* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern VOID xladdivar(LVAL cls, char *var); /* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern VOID xladdmsg(LVAL cls, char *msg, int offset); /* from xlobj.c -- was declared 'static' but needed by WINTERP */
#endif /* WINTERP */

/* for xlread */
extern LVAL tentry(int ch);
extern int xlload(char *fname, int vflag, int pflag);
extern int xlread(LVAL fptr, LVAL *pval);
extern int isnumber(char *str, LVAL *pval);

#ifdef STRUCTS
/* for xlstruct */
extern LVAL xlrdstruct(LVAL list);
extern void xlprstruct(LVAL fptr, LVAL vptr, int flag);
#endif /* STRUCTS */

/* save/restore functions */
#ifdef SAVERESTORE
extern int xlirestore(char *fname);
extern int xlisave(char *fname);
#endif /* SAVERESTORE */

/* external procedure declarations */
extern VOID obsymbols(void);	/* initialize oop symbols */
extern VOID ossymbols(void);	/* initialize os symbols */
extern VOID xlsymbols(void);	/* initialize interpreter symbols */
extern VOID xloinit(void);	/* initialize object functions */
extern VOID xlsinit(void);	/* initialize xlsym.c */
extern VOID xlrinit(void);	/* initialize xlread.c */
extern VOID xlminit(void);	/* init xldmem */
extern VOID xldinit(void);	/* initialize debugger */
extern	int xlinit(char *resfile);  /* xlisp initialization routine */
extern LVAL xleval(LVAL expr);	/* evaluate an expression */
extern LVAL xlapply(int argc);	/* apply a function to arguments */
extern LVAL xlsubr(char *sname, int type, LVAL (*fcn)(void),int offset);
				/* enter a subr/fsubr */
extern LVAL xlenter(char *name);/* enter a symbol */
extern LVAL xlmakesym(char *name);  /* make an uninterned symbol */
extern LVAL xlgetvalue(LVAL sym);   /* get value of a symbol (checked) */
extern void xlsetvalue(LVAL sym, LVAL val); /* set the value of symbol */
extern LVAL xlxgetvalue(LVAL sym);  /* get value of a symbol */
extern LVAL xlgetfunction(LVAL sym);/* get functional value of a symbol */
extern LVAL xlxgetfunction(LVAL sym);
			    /* get functional value of a symbol (checked) */
extern void xlsetfunction(LVAL sym, LVAL val);	/* set the functional value */
extern LVAL xlexpandmacros(LVAL form);	    /* expand macros in a form */
extern LVAL xlgetprop(LVAL sym, LVAL prp);  /* get the value of a property */
extern void xlputprop(LVAL sym, LVAL val, LVAL prp); /*set value of property*/
extern void xlremprop(LVAL sym, LVAL prp);  /* remove a property */
extern LVAL xlclose(LVAL name, LVAL type, LVAL fargs, LVAL body, LVAL env, LVAL fenv);
				/* create a function closure */
extern int hash(char FAR *str, int len);    /* Hash the string */
extern int xlhash(LVAL obj, int len);	/* Hash anything */

#ifdef RANDOM
extern LVAL newrandom(long);		/* create a random-state */
#endif /* RANDOM */

/* argument list parsing functions */
#ifdef BETTERIO
extern LVAL xlgetfile(int outflag);
#else /* !defined(BETTERIO) */
extern LVAL xlgetfile(void);	/* get a file/stream argument */
#endif /* BETTERIO */
extern LVAL xlgetfname(void);	/* get a filename argument */

/* error reporting functions (don't *really* return at all) */
extern LVAL xltoofew(void);	/* report "too few arguments" error */
extern void xltoomany(void);	/* report "too many arguments" error */
extern void xltoolong(void);	/* too long to process error */
extern LVAL xlbadtype(LVAL arg);/* report "bad argument type" error */
extern LVAL xlerror(char FAR *emsg, LVAL arg);	/* report arbitrary error */
extern void xlcerror(char FAR *cmsg, char FAR *emsg, LVAL arg); /*recoverable error*/
extern void xlerrprint(char *hdr,char FAR *cmsg, char FAR *emsg, LVAL arg);
extern void xlbaktrace(int n);	/* do a backtrace */
extern void xlabort(char *emsg);    /* serious error handler */
extern void xlfail(char *emsg);	    /* xlisp error handler */
extern void xlbreak(char FAR *emsg, LVAL arg);	/* enter break loop */
#ifdef SPECIALS
extern void xlnoassign(LVAL arg);   /* report assignment to constant error */
#endif /* SPECIALS */
#ifdef COMMONLISPF
extern int xlcvttype(LVAL arg);
#endif /* COMMONLISPF */

#ifdef WINTERP
extern void placeform(LVAL place, LVAL value); /* from xlcont.c -- needed by w_resources.c:Wres_GetValues_ArgList_To_Lisp */
extern int xlmember(LVAL x, LVAL list); /* from xleval.c -- needed by ../w_callbacks.c and ../w_txlations.c */
#endif /* WINTERP */

#else /* !defined(ANSI) */

#ifdef WINTERP
extern LVAL xlclass();		/* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern int xlclass_p();		/* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern int getivcnt();		/* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern VOID xladdivar();	/* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern VOID xladdmsg();		/* from xlobj.c -- was declared 'static' but needed by WINTERP */
extern int xlgetkeyarg();	/* from xlsubr.c -- not externed #if !defined(ANSI) ANSI, used by WINTERP */
extern void placeform();	/* from xlcont.c -- needed by w_resources.c:Wres_GetValues_ArgList_To_Lisp */
extern int xlmember();		/* from xleval.c -- needed by ../w_callbacks.c and ../w_txlations.c */
#endif /* WINTERP */

/* io interface */
extern long osrand();	/* random number between 0 and n-1 */
#ifdef PATHNAMES
extern FILEP ospopen(); /* open file using path */
#endif /* PATHNAMES */
#ifdef TIMES
extern unsigned long
    ticks_per_second(), run_tick_count(), real_tick_count();
#endif /* TIMES */

/* for xlisp.c */
extern VOID xlrdsave();
extern VOID xlevsave();
extern VOID xlfatal();
extern VOID wrapup();

/* for xleval */
extern LVAL xlxeval();
extern VOID xlabind();
extern VOID xlfunbound();
extern VOID xlargstkoverflow();
extern VOID xlstkoverflow();
extern LVAL makearglist();
extern VOID xlunbound();

/* for xlprin */
extern VOID xlputstr();

/* for xljump */
extern VOID xljump();

#ifdef HASHFCNS
/* for xllist */
extern VOID xlsetgethash();
#endif /* HASHFCNS */

#ifdef KEYARG
/* for xlsubr */
extern LVAL xlkey(), xlapp1();
#endif /* KEYARG */
#ifdef COMPLX
extern FLOTYPE makefloat();
#endif /* COMPLX */

/* for xlread */
extern LVAL tentry();

/* for xlstruct */
extern LVAL xlrdstruct();

/* external procedure declarations */
extern VOID oscheck();		/* check for control character during exec */
extern VOID xlsymbols();	/* initialize symbols */
extern LVAL xleval();		/* evaluate an expression */
extern LVAL xlapply();		/* apply a function to arguments */
extern LVAL xlsubr();		/* enter a subr/fsubr */
extern LVAL xlenter();		/* enter a symbol */
extern LVAL xlmakesym();	/* make an uninterned symbol */
extern LVAL xlgetvalue();	/* get value of a symbol (checked) */
extern LVAL xlxgetvalue();	/* get value of a symbol */
extern LVAL xlgetfunction();	/* get functional value of a symbol */
extern LVAL xlxgetfunction();	/* get functional value of a symbol (checked)*/
extern LVAL xlexpandmacros();	/* expand macros in a form */
extern LVAL xlgetprop();	/* get the value of a property */
extern LVAL xlclose();		/* create a function closure */

#ifdef RANDOM
extern LVAL newrandom();	/* create a random-state */
#endif /* RANDOM */

/* argument list parsing functions */
extern LVAL xlgetfile();	/* get a file/stream argument */
extern LVAL xlgetfname();	/* get a filename argument */

/* error reporting functions (don't *really* return at all) */
extern LVAL xltoofew();		/* report "too few arguments" error */
extern VOID xltoomany();	/* report too many arguments error */
extern VOID xltoolong();	/* too long to process error */
extern LVAL xlbadtype();	/* report "bad argument type" error */
extern LVAL xlerror();		/* report arbitrary error */
extern VOID xlerrprint();	/* print an error message */
extern VOID xlbaktrace();	/* do a backtrace */
#ifdef SPECIALS
extern VOID xlnoassign();	/* report assignment to constant error */
#endif /* SPECIALS */
#endif /* ANSI */

extern int redirectin, redirectout; /* input/output redirection */
extern char buf[];		/* temporary character buffer */

#ifdef NILSYMBOL
#undef NIL			/* want to do it differently */
extern struct node isnil;
#define NIL (&isnil)
#endif /* NILSYMBOL */

#ifndef WINTERP
/* externs in this file only needed by ../w_funtab.c, makes symbol
   table unecessarily large when combined w/ Motif/Xt/X */
#include "xlftab.h"
#endif /* !defined(WINTERP) */
