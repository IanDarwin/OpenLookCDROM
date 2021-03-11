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

#ifndef _XtCards_h
#define _XtCards_h
#include "Hand.h"

typedef enum _CardsRank {
    CardsAce = 1,
    Cards2 = 2,
    Cards3 = 3,
    Cards4 = 4,
    Cards5 = 5,
    Cards6 = 6,
    Cards7 = 7,
    Cards8 = 8,
    Cards9 = 9,
    Cards10 = 10,
    CardsJack = 11, 
    CardsQueen = 12, 
    CardsKing = 13
} CardsRank;

typedef enum _CardsOverlap {
    CardsOverlapNeither = 0,
    CardsOverlapVertical = 1,
    CardsOverlapHorizontal = 2,
    CardsOverlapBoth = 3
} CardsOverlap;

#define CardsRankToInt(c)   ((int) (c) - 1)
#define IntToCardsRank(i)   ((CardsRank) ((i) + 1))

typedef enum _CardsSuit {
    CardsClub, CardsDiamond, CardsHeart, CardsSpade, 
    CardsBack, CardsEmpty, CardsNone
} CardsSuit;

#define CardsSuitToInt(s)   ((int) (s))
#define IntToCardsSuit(i)   ((int) (i))

typedef struct {
    CardsRank	rank;
    CardsSuit	suit;
} CardsCardRec, *CardsCardPtr;

/* define exposed functions */

extern XtPointer    CardsAddCard(/* Widget, CardsCardPtr, int row, int col */);
extern void	    CardsReplaceCard(/* Widget, XtPointer, CardsCardPtr */);

/* Add aliases to stuff which is simply inherited from the Hand widget */

#define CardsRemoveCard	HandRemoveCard
#define CardsRectangleForCard	HandRectangleForCard
#define CardsRectangleForPos	HandRectangleForPos
#define CardsRemoveAllCards	HandRemoveAllCards
#define CardsXYToPos		HandXYToPos
#define CardsUpdateDisplay	HandUpdateDisplay
#define CardsInputRec		HandInputRec
#define CardsInputPtr		HandInputPtr

typedef struct _CardsRec *CardsWidget;
typedef struct _CardsClassRec *CardsWidgetClass;

extern WidgetClass  cardsWidgetClass;

#define XtNroundCards "roundCards"
#define XtCRoundCards "RoundCards"
#define XtNsmallCards "smallCards"
#define XtCSmallCards "SmallCards"
#define XtNback "back"
#define XtCBack "Back"
#define XtNtrademark "trademark"
#define XtCTrademark "Trademark"
#define XtNobverseColor "obverseColor"
#define XtCObverseColor "ObverseColor"
#define XtNblackColor "blackColor"
#define XtCBlackColor "BlackColor"
#define XtNredColor "redColor"
#define XtCRedColor "RedColor"
#define XtNinverseColor "inverseColor"
#define XtCInverseColor "InverseColor"
#define XtNcolor "color"
#define XtNoverlap "overlap"
#define XtCOverlap "Overlap"
#define XtNuseTile "useTile"
#define XtCUseTile "UseTile"

#define XtRCardsOverlap "CardsOverlap"
#endif /* _XtCards_h */
