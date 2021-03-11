/* -*-C-*-
********************************************************************************
*
* File:         w_XmString.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/w_XmString.h,v 2.5 1994/06/06 15:41:02 npm Exp $
* Description:  Declare Puclic Interfaces to XmString routines
* Author:       Niels Mayer
* Created:      Sun Nov  5 14:46:20 1989
* Modified:     Sun Jun  5 14:32:27 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, or Niels Mayer not be used in advertising or
* publicity pertaining to distribution of the software without specific,
* written prior permission. Enterprise Integration Technologies, Hewlett-Packard
* Company, and Niels Mayer makes no representations about the suitability of
* this software for any purpose.  It is provided "as is" without express or
* implied warranty.
* 
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY AND NIELS MAYER
* DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE
* INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY OR NIELS MAYER BE LIABLE
* FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER
* RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
* CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
* CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
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

typedef struct _Deallocator_Pair {
    XtPointer pointer;
    void    (*deallocator)();
} Deallocator_Pair;

typedef struct _SuperXmStringTable {
  XmString* xmstrtab;		/* public */
  int xmstrtab_end_idx;		/* public -- the number of elts in array */
  Deallocator_Pair *freeables;	/* private */
  int freeables_end_idx;	/* private */
  int freeables_size;		/* private */
} * SuperXmStringTable;


extern XmString
Get_String_or_XmString_Arg_Returning_XmString
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL *item
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void
Wxms_Garbage_Collect_XmString
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lval_xmstring
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern LVAL
Wxms_XmStringTable_To_Lisp_Vector
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 XmStringTable xmstrtab,
 int           size
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern SuperXmStringTable
Wxms_Cvt_LispStringSequence_to_SuperXmStringTable
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 LVAL lisp_val
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );

extern void Wxms_Free_SuperXmStringTable
(
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
 SuperXmStringTable superstrtab
#endif /* !defined(_NO_PROTO) ==> ANSI */
 );
