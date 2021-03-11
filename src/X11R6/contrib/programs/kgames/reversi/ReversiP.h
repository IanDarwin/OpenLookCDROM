/*
* $XConsortium: ReversiP.h,v 1.24 89/06/08 18:05:01 swick Exp $
*/


/***********************************************************
Copyright 1987, 1988 by Digital Equipment Corporation, Maynard, Massachusetts,
and the Massachusetts Institute of Technology, Cambridge, Massachusetts.

                        All Rights Reserved

Permission to use, copy, modify, and distribute this software and its 
documentation for any purpose and without fee is hereby granted, 
provided that the above copyright notice appear in all copies and that
both that copyright notice and this permission notice appear in 
supporting documentation, and that the names of Digital or MIT not be
used in advertising or publicity pertaining to distribution of the
software without specific, written prior permission.  

DIGITAL DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
DIGITAL BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

******************************************************************/

/* 
 * ReversiP.h - Private definitions for Reversi widget
 * 
 */

#ifndef _XawReversiP_h
#define _XawReversiP_h

/***********************************************************************
 *
 * Reversi Widget Private Data
 *
 ***********************************************************************/

#include "Reversi.h"
#include "transform.h"
#include <X11/Xaw/SimpleP.h>

/* New fields for the Reversi widget class record */

typedef struct {int foo;} ReversiClassPart;

/* Full class record declaration */
typedef struct _ReversiClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    ReversiClassPart	reversi_class;
} ReversiClassRec;

extern ReversiClassRec reversiClassRec;

typedef enum _AnimateState { AnimateNone, AnimateA, AnimateB } AnimateState;

typedef struct _Animate {
    AnimateState    state;
    ReversiStone    A, B;
    ReversiWidget   rw;
    int		    x, y;
    unsigned long   delay;
    int		    togo;
    XtIntervalId    timeout;
} Animate;

#define BOARD_WIDTH	8
#define BOARD_HEIGHT	8

/* New fields for the Reversi widget record */
typedef struct {
    /* resources */
    Pixel	    white;
    Pixel	    black;
    Pixel	    grid;
    XtCallbackList  callbacks;

    /* private state */
    GC		    white_GC;
    GC		    black_GC;
    GC		    grid_GC;
    ReversiStone    board[BOARD_WIDTH][BOARD_HEIGHT];
    Animate	    animate[BOARD_WIDTH][BOARD_HEIGHT];
    Transform	    t;
} ReversiPart;


/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _ReversiRec {
    CorePart	core;
    SimplePart	simple;
    ReversiPart	reversi;
} ReversiRec;

#endif /* _XawReversiP_h */
