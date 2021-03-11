/* -*-C-*-
********************************************************************************
*
* File:         app_funidx.h
* RCS:          $Header: /users/npm/src/winterp/src-server/RCS/app_funidx.h,v 2.8 1994/06/06 15:41:14 npm Exp $
* Description:  This file is #included by w_funtab.h. It allows applications
*		built upon winterp to add new  functions to the xlisp function
*		table set up in w_funtab.c. Any entries in this file must have
*		corresponding function table entries in app_fundecl.h.
* Author:       Niels Mayer
* Created:      Fri Dec  1 16:39:10 1989
* Modified:     Sun Jun  5 14:19:06 1994 (Niels Mayer) npm@indeed
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
	this is just a sequence of identifiers, all terminated by commas ','.
	The name of the identifiers is composed of the C method or function 
	name (added in app_fundecl.h) prepended by 'FTAB_'.

 FTAB_The_First_Application_Primitive,
 FTAB_The_Second_Application_Primitive,

 Note: see also app_fundecl.h, app_funextn.h and w_funtab.h.
*/

/*
FTAB_ ,
FTAB_ ,
FTAB_ ,
FTAB_ ,
*/
