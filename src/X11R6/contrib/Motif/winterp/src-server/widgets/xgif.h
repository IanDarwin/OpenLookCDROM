/* -*-C-*-
********************************************************************************
*
* File:         xgif.h
* RCS:          $Header: /users/npm/src/winterp/src-server/widgets/RCS/xgif.h,v 2.1 1994/06/06 15:48:00 npm Exp $
* Description:  Header for GIF_To_XImage() -- see xgif.c
* Author:       Niels Mayer (mayer@netcom.com).
* Created:      6 Mar 92 12:47:22 GMT
* Modified:     Sun Jun  5 04:13:02 1994 (Niels Mayer) npm@indeed
* Language:     C
* Package:      N/A
* Status:       X11r6 contrib release
*
* Copyright (C) 1994, Enterprise Integration Technologies Corp. and Niels Mayer.
* WINTERP 1.15-1.99, Copyright (c) 1993, Niels P. Mayer.
* WINTERP 1.0-1.14, Copyright (c) 1989-1992 Hewlett-Packard Co. and Niels Mayer.
* gif2ras.c Copyright (c) 1988, 1989 by Patrick J. Naughton (naughton@wind.sun.com)
* 
* Permission to use, copy, modify, distribute, and sell this software and its
* documentation for any purpose is hereby granted without fee, provided that
* the above copyright notice appear in all copies and that both that
* copyright notice and this permission notice appear in supporting
* documentation, and that the name of Enterprise Integration Technologies,
* Hewlett-Packard Company, Niels Mayer, Alfred Kayser, John Bradley and
* Patrick J. Naughton not be used in advertising or publicity pertaining to
* distribution of the software without specific, written prior permission. 
* Enterprise Integration Technologies, Hewlett-Packard Company, Niels Mayer,
* Alfred Kayser, John Bradley and Patrick J. Naughton makes no representations 
* about the suitability of this software for any purpose. It is provided "as is" 
* without express or implied warranty.
*
* ENTERPRISE INTEGRATION TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER,
* ALFRED KAYSER, JOHN BRADLEY AND PATRICK J. NAUGHTON DISCLAIMS ALL
* WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
* OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL ENTERPRISE INTEGRATION
* TECHNOLOGIES, HEWLETT-PACKARD COMPANY, NIELS MAYER, ALFRED KAYSER, JOHN
* BRADLEY OR PATRICK J. NAUGHTON BE LIABLE FOR ANY SPECIAL, INDIRECT OR
* CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
* DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
* TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
* PERFORMANCE OF THIS SOFTWARE. THE AUTHORS SHALL HAVE NO LIABILITY WITH
* RESPECT TO THE INFRINGEMENT OF COPYRIGHTS, TRADE SECRETS OR ANY PATENTS
* BY THIS file OR ANY PART THEREOF.
*
********************************************************************************
*/

#include  <X11/Xlib.h>
#include  <X11/Intrinsic.h>

extern Bool GIF_To_XImage(	/* from widgets/xgif.c */
#ifndef _NO_PROTO		/* <Xm/Xm.h> or Makefile says we have ANSI C Fn. Prototypes */
     Display*       disp,	/* IN: display */
     Screen*        scrn,	/* IN: screen */
     Colormap       cmap,	/* IN: colormap */
     char*          fname,	/* IN: filename of GIF */
     Bool           quiet_p,	/* IN: True if don't want debug info printed, else False */
     char*          *err_string,     /* OUT: if routine returns False, returns a string w/ error message */
     XImage*        *ximage,	     /* OUT: if routine returns True, returns XImage* of GIF */
     int            *num_alloc_cols, /* OUT: if routine returns True, gives the number of colors allocated by XAllocColor() */
     unsigned long* *alloc_cols      /* OUT: if routine returns True, gives the array of pixel values alloc'd by XAllocColor() */
#endif /* !defined(_NO_PROTO) ==> ANSI */
  );
