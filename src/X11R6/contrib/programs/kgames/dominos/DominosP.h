/*
 * $NCD$
 *
 * Copyright 1992 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of NCD. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  NCD. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * NCD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NCD.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Keith Packard, Network Computing Devices
*/

#ifndef _XtDominosP_h
#define _XtDominosP_h

#include <X11/Xaw/SimpleP.h>
#include "Dominos.h"
#include "dominos.h"

/************************************
 *
 *  Class structure
 *
 ***********************************/

/*
 * New fields for the Dominos widget class record
 */

typedef struct _DominosClass {
    int	    makes_compiler_happy;  /* not used */
} DominosClassPart;

/*
 * Full class record declaration
 */

typedef struct _DominosClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    DominosClassPart	cards_class;
} DominosClassRec;

extern DominosClassRec cardsClassRec;

typedef struct {
    /* resources */
    Pixel	    pips_pixel;
    Pixel	    face_pixel;
    Boolean	    round_dominos;
    Dimension	    size;
    XtCallbackList  input_callback;	/* func called on button press */
    /* private state */
    GC		    pips_gc;
    GC		    face_gc;
    GC		    bg_gc;
    DominoPtr	    *board;
    Pixmap	    tmp_map;
    Position	    x_off, y_off;
} DominosPart;

/*
 * Full widget declaration
 */

typedef struct _DominosRec {
    CorePart	core;
    SimplePart	simple;
    DominosPart	dominos;
} DominosRec;

#endif /* _XtDominosP_h */
