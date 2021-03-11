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

#ifndef _XtCardsP_h
#define _XtCardsP_h

#include "Cards.h"
#include "HandP.h"

/************************************
 *
 *  Class structure
 *
 ***********************************/

/*
 * New fields for the Cards widget class record
 */

typedef struct _CardsClass {
    int	    makes_compiler_happy;  /* not used */
} CardsClassPart;

/*
 * Full class record declaration
 */

typedef struct _CardsClassRec {
    CoreClassPart	core_class;
    SimpleClassPart	simple_class;
    HandClassPart	hand_class;
    CardsClassPart	cards_class;
} CardsClassRec;

extern CardsClassRec cardsClassRec;

typedef struct {
    /* resources */
    Boolean	    round_cards;    /* rounded corners */
    Boolean	    small_cards;    /* small cards */
    Boolean	    use_tile;	    /* use tiles instead of stipples */
    CardsOverlap    overlap;
    Pixmap	    back;
    Pixmap	    trademark;
    Pixel	    obverse_color;
    Pixel	    black_color;
    Pixel	    red_color;
    Pixel	    inverse_color;
    Pixel	    empty_color;
    Boolean	    color;
    /* private state */
    int		    offset_face;
    int		    offset_other;
    GC		    redgc;
    Boolean	    redHasClip;
    GC		    blackgc;
    Boolean	    blackHasClip;
    GC		    whitegc;
    Boolean	    whiteHasClip;
    GC		    backgc;
    Boolean	    backHasClip;
    GC		    emptygc;
    Boolean	    emptyHasClip;
    Pixmap	    backTile;
    int		    back_delta_x;
    int		    back_delta_y;
} CardsPart;

/*
 * Full widget declaration
 */

typedef struct _CardsRec {
    CorePart	core;
    SimplePart	simple;
    HandPart	hand;
    CardsPart	cards;
} CardsRec;

#endif /* _XtCardsP_h */
