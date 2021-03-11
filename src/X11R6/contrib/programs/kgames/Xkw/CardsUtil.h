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

typedef struct _Card *CardPtr;

typedef enum _CardFace { CardFaceUp, CardFaceDown } CardFace;

typedef enum _CardDisplay { 
    CardDisplayTop, CardDisplayBottom, CardDisplayAll, 
    CardDisplaySome, CardDisplayNone
} CardDisplay;

typedef struct _Card {
    CardPtr	    next, prev;
    CardsCardRec    card;
    CardsCardRec    display;
    CardFace	    face;
    Boolean	    shouldBeUp;
    Boolean	    isUp;
    Widget	    widget;
    int		    row, col;
    XtPointer	    data;
} CardRec;

typedef struct _CardStack  *CardStackPtr;

typedef struct _CardStack {
    Widget	    widget;
    Boolean	    horizontal;
    CardDisplay	    display;
    int		    position;
    int		    basePosition;
    CardPtr	    first, last;
    CardRec	    empty;
} CardStackRec;

#define New(t) (t *) malloc(sizeof (t))
#define Dispose(p)  free((char *) p)
#define Some(t,n)   (t*) malloc(sizeof(t) * n)
#define More(p,t,n) ((p)? (t *) realloc((char *) p, sizeof(t)*n):Some(t,n))

extern Boolean	CardIsInOrder (/* CardPtr, CardPtr */);
extern Boolean	CardIsInSuitOrder (/* CardPtr, CardPtr */);
extern Boolean	CardIsInAlternatingSuitOrder (/* CardPtr, CardPtr */);
extern CardPtr	CardInOrder (/* CardPtr */);
extern CardPtr	CardInSuitOrder (/* CardPtr */);
extern CardPtr	CardInAlternatingSuitOrder (/* CardPtr */);

extern CardPtr	CardInReverseOrder (/* CardPtr */);
extern CardPtr	CardInReverseSuitOrder (/* CardPtr */);
extern CardPtr	CardInReverseAlternatingSuitOrder (/* CardPtr */);

extern void	CardDisplayStack (/* CardStackPtr */);

extern void	CardTurn (/* CardPtr, CardFace, Boolean */);
extern void	CardMove (/* CardStackPtr, CardPtr, CardStackPtr, Boolean */);
extern void	CardMoveCards (/* CardStackPtr, CardPtr, CardPtr, CardStackPtr, CardPtr, Boolean */);
extern void	CardRecordHistoryCallback (/* void (*)(), char * */);
extern Boolean	CardUndo (/* void */);

extern void	CardInitStack (/* CardStackPtr, Widget, CardsSuit, Boolean, Boolean, int */);
extern void	CardGenerateStandardDeck (/* CardPtr */);
extern void	CardShuffle (/* CardStackPtr */);
extern void	CardInitHistory (/* void */);
extern int	CardNextHistory (/* void */);
