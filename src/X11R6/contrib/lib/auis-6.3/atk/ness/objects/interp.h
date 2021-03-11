/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

/* 
	interp.h

	external declarations for the Ness run-time environment.
*/

#ifndef _ness_interp_h_
#define _ness_interp_h_





#include "error.h"

#define  ExtendShortSign(x)  (((x) ^ 0x8000) - 0x8000)

typedef unsigned char TStackRef;
typedef unsigned short TSysMarkRef;
typedef unsigned long TCodeRef;
typedef long TType;

#define longHdr	((TType) 9101 )
#define boolHdr	((TType) 9102 )
#define dblHdr	((TType) 9103 )
#define ptrHdr	((TType) 9104 )
#define frameHdr	((TType) 9105 )


/* frameelt is here only to allow sizeof(struct frameelt) in computing
	offsets of local variables */
struct frameelt {			/* stack frame */
	TType hdr;
	unsigned char *returnaddress;	/* (position in ObjectCode) */
	struct frameelt *prevframe;
};


void initializeEnvt();		/*  initialize the run-time environment */
struct errornode *interpretNess(/*func, ness*/);	/*  interpret a call on func with the current stack  */

#endif /* _ness_interp_h_ */


/*
 * $Log: interp.h,v $
 * Revision 1.12  1994/03/01  23:05:54  rr2b
 * Commented out text after #endif's
 * BUG
 *
 * Revision 1.11  1993/05/04  01:23:55  susan
 * RCS Tree Split
 *
 * Revision 1.9.1.1  1993/02/02  03:01:42  rr2b
 * new R6tape branch
 *
 * Revision 1.9  1992/12/14  23:20:33  rr2b
 * add $Logs back after disclaimerization took them out
 *
 * Revision 1.7  1992/11/26  02:55:09  wjh
 * converted CorrectGetChar to GetUnsignedChar
 * moved ExtendShortSign to interp.h
 * remove xgetchar.h; use simpletext_GetUnsignedChar
 * nessrun timing messages go to stderr
 * replaced curNess with curComp
 * replaced genPush/genPop with struct compilation
 * created compile.c to handle compilation
 * moved scope routines to compile.c
 * converted from lex to tlex
 * convert to use lexan_ParseNumber
 * truncated logs to 1992 only
 * use bison and gentlex instead of yacc and lexdef/lex
 *
 * .
 *
 * Revision 1.6  91/09/12  19:44:00  bobg
 * Update copyright notice
 * 
 * Revision 1.5  1989/06/23  17:24:28  wjh
 * (Items that should not be sent to downstairs are marked "R4".)
 *
 * Added a call to CheckForInterrupt in the function calls and internal gotos.  Thus loops can be terminated with ^G.   (interp.c)  R4
 *
 * Changed dokeys() so it uses im_DoKeySequence.  This means it is now possible to send a key sequence which involves the message line.  (interp.c)  R4
 *
 * Implemented DoMenu(object, menustring).   This function causes the same behavior as if the user selected the menu option.  At present the menu string must be exactly as originally defined;  see the warning above for im_DoMenu().  (interp.c, call.c)  R4
 *
 * Changed access to Ness library routines so they are always compiled.  (They used ot get the default of NoCompilation, so they were useless.)  (call.c)
 *
 * Removed a superflous {} pair.  {This is purely a cosmetic change.}  (nessmark.c)
 *
 * Fixed value_GetStringValue.  Formerly it was getting an invalid initial value.  {The fix was done by adding a call to nessmark_Initialize() in the stackString section.}  (nevent.c)
 *
 * Modified the data stream so errors will not occur when a ness object is the outermost object.  The fix was to add two bytes, "00", at the end of the origin string to prevent the former occurence of a spurious "\}".  (ness.c)
 *
 * Fixed menu handling so Ness and child menus get posted when there is a mouse click in an inset within the Ness.  Formerly neither set of menus was posted. (nessv.c)
 *
 * Fixed dostmt, the function called from ness-load, which is recommended to be bound to ESC-ESC.  It was using a NULL pointer, so people were getting core dumps if they typed ESC-ESC before doing a compile.  (ness.c)
 *
 * Avoided an infinite loop which occurred if a library function referred to a non-existent entry point within itself.  Did this by checking to see if the library function is already Compiling just before trying to compile it.  (call.c call.hn, call.h)
 *
 * Revised system marker allocation so the compilation will not get a subsequent error.  (gen.c)
 *
 * Revised system marker allocation so it expands the space available if necessary. This will make it possible to compile larger programs.  (gen.c)
 *
 * Changed the type of TType to long from struct object *.  This will allow compilation on stricter compilers.  (interp.h)
 *
 * Fixed nessmark_FinalizeObject so it would not reference a NULL pointer.  {Somehow the assembler noticed this bug!}  (nessmark.c)
 *
 * Changed functions which deal with constant strings to have (char *) as there argument type (SaveError, exprnode_Create, ReportError, ExprError, RunError, LocateErrorFunc, QueryReadOnly, makeConst, printallerrors) or return type (Freeze, argcounterr, argtypeerr).  This prevents compile errors on picky compilers.  (interp.c, error.c, call.c, ness.c, nessv.c, search.c, nevent.c, nessmark.c, nessrun.ci)  R4
 *
 * Changed Imakefile to store Imakefile in checkin rule.  (Imakefile)
 *
 * Revision 1.4  89/06/01  16:02:58  wjh
 * campus release version
 * 
 * Revision 1.1  88/10/21  10:58:52  wjh
 * Initial revision
 * 
 * Creation 0.0  88/04/07 11:00  wjh
 * Initial creation by WJHansen
 * 
*/
