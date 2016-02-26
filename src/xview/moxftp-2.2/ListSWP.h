/*
 * $XConsortium: ListP.h,v 1.12 89/12/11 15:09:04 kit Exp $
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
 *
 * Author:  Chris D. Peterson, MIT X Consortium
 */


/* 
 * ListP.h - Private definitions for List widget
 * 
 * This is the List widget, it is useful to display a list, without the
 * overhead of having a widget for each item in the list.  It allows 
 * the user to select an item in a list and notifies the application through
 * a callback function.
 *
 *	Created: 	8/13/88
 *	By:		Chris D. Peterson
 *                      MIT - Project Athena
 */

/*
 * $Id: ListSWP.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/ListSWP.h,v $
 *
 * $Log: ListSWP.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 * Revision 1.1  1993/02/11  00:41:51  jones
 * Initial revision
 *
 */

#ifndef _MyXawSWListP_h
#define _MyXawSWListP_h

/***********************************************************************
 *
 * List Widget Private Data
 *
 ***********************************************************************/

#if defined(XAW3D)
#include <X11/Xaw3d/SimpleP.h>
#else
#if defined(XAW)
#include <X11/Xaw/SimpleP.h>
#endif
#endif

#if defined(MOTIF)
#include <Xm/XmP.h>
#if XmREVISION==2
#include <Xm/ManagerP.h>
#endif
#endif

#if defined(OPENWINDOW)
#include <Xol/OpenLookP.h>
#include <Xol/Manager.h>
#include <Xol/ManagerP.h>
#endif

#include "ListSW.h"

/* New fields for the List widget class record */

typedef struct {int foo;} MyListSWClassPart;

/* Full class record declaration */
typedef struct _MyListSWClassRec {
    CoreClassPart	 core_class;
    CompositeClassPart   composite_class;
#if defined(MOTIF)
    ConstraintClassPart  constraint_class;
    XmManagerClassPart   manager_class;
#endif
#if defined(OPENWINDOW)
    ConstraintClassPart  constraint_class;
    ManagerClassPart	 manager_class;
#endif
    MyListSWClassPart	 Mylistsw_class;
} MyListSWClassRec;

extern MyListSWClassRec MylistSWClassRec;

/* New fields for the List widget record */
typedef struct {
    /* resources */
    Widget      vbar, hbar; 	/* scroll bars */ 
    Widget	List;		/* list widget */
    Widget	label;		/* label widget */
    Boolean	labelw;		/* Create label */ 
    int		wait;		/* Don't resize children */
    int		allowresize;	/* Allow resize */
    int		resizeing;      /* Try to git rid of resize osolations */
    int		vbar_max;
    int		vbar_page;
    int		hbar_max;
    int		hbar_page;
    int		list_x;
    int		list_y;
    int		list_w;
    int		list_h;
    int		vbar_x;
    int		vbar_y;
    int		vbar_w;
    int		vbar_h;
    int		hbar_x;
    int		hbar_y;
    int		hbar_w;
    int		hbar_h;
    int		label_x;
    int		label_y;
    int		label_w;
    int		label_h;
#if defined(XAW)||defined(OPENWINDOW)
    XFontStruct *font;
#else
    XmFontList   font;
#endif
    XFontStruct *fs;

} MyListSWPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _ListSWRec {
    CorePart	    core;
    CompositePart   composite;
#if defined(MOTIF)
    ConstraintPart constraint;
    XmManagerPart   manager;
#endif
#if defined(OPENWINDOW)
    ConstraintPart constraint;
    ManagerPart	    manager;
#endif
    MyListSWPart listsw;
} MyListSWRec;

#endif /* _MyXawListP_h */
