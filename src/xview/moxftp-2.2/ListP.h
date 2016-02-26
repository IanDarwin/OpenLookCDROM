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
 * $Id: ListP.h,v 1.1 1994/03/14 18:57:41 jones Exp $
 * $Source: /mintaka/u0/xx/ss/jones/jones.old/mftp.src/mftpnew/RCS/ListP.h,v $
 *
 * $Log: ListP.h,v $
 * Revision 1.1  1994/03/14  18:57:41  jones
 * Initial revision
 *
 * Revision 1.1  1993/02/11  00:41:51  jones
 * Initial revision
 *
 */

#ifndef _MyXawListP_h
#define _MyXawListP_h

/***********************************************************************
 *
 * List Widget Private Data
 *
 ***********************************************************************/

#include "List.h"

#if defined(OPENWINDOW)
#include <Xol/OpenLookP.h>
#include <Xol/Primitive.h>
#include <Xol/PrimitiveP.h>
#endif


#if defined(XAW3D)
#include <X11/Xaw3d/SimpleP.h>
#include <X11/Xaw3d/ThreeDP.h>
#else
#if defined(XAW)
#include <X11/Xaw/SimpleP.h>
#endif
#endif

#if defined(MOTIF)
#include <Xm/XmP.h>
#if XmREVISION==2
#include <Xm/PrimitiveP.h>
#endif
#endif


#define NO_HIGHLIGHT            XAW_LIST_NONE
#define OUT_OF_RANGE            -1
#define OKAY                     0
#define PAD		 	 2
#if defined(XAW)
#define SCROLL_PAD	 	 5
#endif
#if defined(MOTIF)
#define SCROLL_PAD		 2
#endif

/* New fields for the List widget class record */

typedef struct {int foo;} MyListClassPart;

/* Full class record declaration */
typedef struct _MyListClassRec {
    CoreClassPart	core_class;
#if defined(XAW)
    SimpleClassPart	simple_class;
#if defined(XAW3D)
    ThreeDClassPart    	threeD_class;
#endif
#endif
#if defined(MOTIF)
    XmPrimitiveClassPart primitive_class;
#endif
#if defined(OPENWINDOW)
    PrimitiveClassPart  primitive_class;
#endif
    MyListClassPart	Mylist_class;
} MyListClassRec;

extern MyListClassRec MylistClassRec;

/* New fields for the List widget record */
typedef struct {
    /* resources */
    Pixel	foreground;
    Dimension	internal_width,
        	internal_height,
                column_space,
                row_space,
		clip_top_margin,
		clip_right_margin,
		clip_width,
		clip_height;
    int         default_cols;
    Boolean     force_cols,
                paste,
                vertical_cols,
		mulitselect;
    int         longest,
		longest_str;
    int         nitems;		/* number of items in the list. */
#if defined(XAW)||defined(OPENWINDOW)
    XFontStruct	*font;
    XFontStruct	*fontbold;
#if defined(XAW3D)
    int	        redisplay;
#endif
#else
    XmFontList  font;
    XmFontList  fontbold;
#endif
    String *    list;
    XtCallbackList  callback;
    XtCallbackList  scallback;

    /* private state */

    int         bold;
    int	   *	selected;
    int         is_highlighted,	/* set to the item currently highlighted. */
                highlight,	/*set to the item that should be highlighted.*/
                col_width,	/* width of each column. */
                row_height,	/* height of each row. */
                nrows,		/* number of rows in the list. */
                ncols,		/* number of columns in the list. */
		resizeing;
    GC		normgc,		/* a couple o' GC's. */
		boldgc,
                revgc,
		revboldgc,
                graygc;		/* used when inactive. */
    XFontStruct *fs;
    XFontStruct *fsbold;
#if defined(OPENWINDOW)
    Widget	lastfocus;
#endif
    int		xoffset,
		yoffset,
		top,
		real_top;

} MyListPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _ListRec {
    CorePart	        core;
#if defined(XAW)
    SimplePart	        simple;
#if defined(XAW3D)
    ThreeDPart          threeD;
#endif
#endif
#if defined(MOTIF)
    XmPrimitivePart    	primitive;
#endif
#if defined(OPENWINDOW)
    PrimitivePart      	primitive;
#endif
    MyListPart		list;
} MyListRec;

#endif /* _MyXawListP_h */
