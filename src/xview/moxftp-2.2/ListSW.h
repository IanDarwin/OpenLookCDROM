/* $XConsortium: List.h,v 1.20 91/07/26 20:07:51 converse Exp $
 *
 * Copyright 1989 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of M.I.T. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  M.I.T. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL M.I.T.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 */

/*  This is the List widget, it is useful to display a list, without the
 *  overhead of having a widget for each item in the list.  It allows 
 *  the user to select an item in a list and notifies the application through
 *  a callback function.
 *
 *	Created: 	8/13/88
 *	By:		Chris D. Peterson
 *                      MIT X Consortium
 */


/*
 * $Id: ListSW.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/ListSW.h,v $
 *
 * $Log: ListSW.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 * Revision 1.1  1993/02/11  00:41:51  jones
 * Initial revision
 *
 */

#ifndef _MyXawSWList_h
#define _MyXawSWList_h

/***********************************************************************
 *
 * List Widget
 *
 ***********************************************************************/

#if defined(XAW3D)
#include <X11/Xaw3d/Simple.h>
#else
#if defined(XAW)
#include <X11/Xaw/Simple.h>
#endif
#endif

#if defined(MOTIF)
#include <Xm/Xm.h>
#endif

#if defined(OPENWINDOW)
#include <Xol/OpenLook.h>
#endif

#include "Xfuncproto.h"

/* Resources:

 Name		     Class		RepType		Default Value
 ----		     -----		-------		-------------
 background	     Background		Pixel		XtDefaultBackground
 border		     BorderColor	Pixel		XtDefaultForeground
 borderWidth	     BorderWidth	Dimension	1
 callback            Callback           XtCallbackList  NULL       **6
 cursor		     Cursor		Cursor		left_ptr
 destroyCallback     Callback		Pointer		NULL 
 foreground	     Foreground		Pixel		XtDefaultForeground
 height		     Height		Dimension	0          **1
 insensitiveBorder   Insensitive	Pixmap		Gray
 mappedWhenManaged   MappedWhenManaged	Boolean		True
 sensitive	     Sensitive		Boolean		True
 width		     Width		Dimension	0          **1
 x		     Position		Position	0
 y		     Position		Position	0

*/

#define XtNlabelw "labelw"

extern WidgetClass MylistSWWidgetClass;
typedef struct _ListSWClassRec *MyListSWWidgetClass;
typedef struct _ListSWRec      *MyListSWWidget;

#endif /* _MyXawListSW_h */
/* DON'T ADD STUFF AFTER THIS #endif */
