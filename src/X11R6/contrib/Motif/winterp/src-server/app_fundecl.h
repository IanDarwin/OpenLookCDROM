/* -*-C-*-
********************************************************************************
*
* File:         app_fundecl.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/app_fundecl.h,v 2.8 1994/06/06 15:41:15 npm Exp $
* Description:  This file is #included in w_funtab.c. It allows applications 
*		built upon winterp to add new functions to the xlisp function
*		table set up in w_funtab.c. Any entries in this file must have
*		corresponding entry in file app_funidx.h, and should also be
*		declared "extern LVAL function();" in app_funextn.h.
* Author:       Niels Mayer
* Created:      Fri Dec  1 16:39:14 1989
* Modified:     Sun Jun  5 14:18:17 1994 (Niels Mayer) npm@indeed
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

/*
	FORMAT FOR ENTRIES IN THIS FILE:

{"THE_FIRST_APPLICATION_PRIMITIVE", S, The_First_Application_Primitive},
{"THE_SECOND_APPLICATION_PRIMITIVE", S, The_Second_Application_Primitive},
	.
	.
	.
  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                                |--- Note: Name of lisp function must be uppercase.

  Note: see also app_funidx.h, app_funidx.h and w_funtab.c...
*/

/*
{NULL, S, },
{NULL, S, },
{NULL, S, },
*/
