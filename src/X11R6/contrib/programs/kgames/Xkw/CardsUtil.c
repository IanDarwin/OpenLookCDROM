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

# include	<X11/Intrinsic.h>
# include	<X11/StringDefs.h>
# include	<X11/Xos.h>
# include	<X11/Xutil.h>
# include	"Cards.h"
# include	"CardsUtil.h"

typedef struct _CardHistory	*CardHistoryPtr;

typedef enum {
    HistoryMove, HistoryTurn, HistoryCallback, HistoryShuffle 
} CardHistoryType;

typedef struct _CardHistory {
    CardHistoryPtr	prev;
    CardHistoryType	type;
    int			serial;
    union {
	struct {
	    CardPtr	    card;
	    CardFace	    face;
	} turn;
	struct {
	    CardStackPtr    source;
	    CardStackPtr    dest;
	    CardPtr	    before;
	    CardPtr	    first;
	    CardPtr	    last;
	} move;
	struct {
	    void    	    (*func)();
	    char	    *closure;
	} callback;
	struct {
	    CardStackPtr    stack;
	    CardPtr	    *cards;
	    int		    numCards;
	} shuffle;
    } u;
} CardHistoryRec;

static CardHistoryPtr	history;
static int		historySerial;
static Boolean		canAnimate = True;

static void
SetShoulds (stack)
    CardStackPtr    stack;
{
    CardPtr card;
    if (!stack->first)
	return;
    
    switch (stack->display) {
    case CardDisplayTop:
	card = stack->last;
	card->shouldBeUp = True;
	for (card = card->prev; card; card = card->prev)
	    card->shouldBeUp = False;
	break;
    case CardDisplayBottom:
	card = stack->first;
	card->shouldBeUp = True;
	for (card = card->next; card; card = card->next)
	    card->shouldBeUp = False;
	break;
    case CardDisplayAll:
	for (card = stack->first; card; card = card->next)
	    card->shouldBeUp = True;
	break;
    case CardDisplayNone:
	for (card = stack->first; card; card = card->next)
	    card->shouldBeUp = False;
	break;
    }
}

static void
DisplayCards (stack, c)
    CardStackPtr    stack;
    CardPtr	    c;
{
    int		    row, col;
    int		    pos;
    CardsSuit	    suit;
    Boolean	    animated = !canAnimate;
    int		    mode;
    
    if (stack->horizontal)
	row = stack->position;
    else
	col = stack->position;
    pos = stack->basePosition;;
    while (c) 
    {
	if (stack->horizontal)
	    col = pos;
	else
	    row = pos;
	if (c->face == CardFaceDown)
	    suit = CardsBack;
	else
	    suit = c->card.suit;
	if (c->isUp && c->shouldBeUp && 
	    c->widget == stack->widget &&
	    row == c->row && col == c->col && c->data)
	{
	    if (suit != c->display.suit)
	    {
		c->display.suit = suit;
		CardsReplaceCard (c->widget, c->data, &c->display);
	    }
	}
	else
	{
	    if (c->data)
	    {
		if (c->shouldBeUp && !animated)
		{
		    Animate (c->widget, c->row, c->col,
			     stack->widget, row, col);
		    animated = True;
		}
		CardsRemoveCard (c->widget, c->data);
		c->data = 0;
	    }
	    if (c->shouldBeUp)
	    {
		c->display.suit = suit;
		c->display.rank = c->card.rank;
		c->row = row;
		c->col = col;
		c->widget = stack->widget;
		mode = Below;
		c->data = CardsAddCard (c->widget, &c->display, row, col);
	    }
	    c->isUp = c->shouldBeUp;
	}
	if (c->isUp)
	    pos++;
	c = c->next;
    }
}

Boolean
CardIsInOrder (a, b)
    CardPtr	a, b;
{
    return a->face == b->face &&
           a->card.rank + 1 == b->card.rank;
}

Boolean
CardIsInSuitOrder (a, b)
    CardPtr	a, b;
{
    return a->face == b->face &&
	   a->card.suit == b->card.suit && 
	   CardIsInOrder (a, b);
}

CardPtr
CardInSuitOrder (card)
    CardPtr	card;
{
    while (card->next && CardIsInSuitOrder (card->next, card))
	card = card->next;
    return card;
}

CardPtr
CardInOrder (card)
    CardPtr	card;
{
    while (card->next && CardIsInOrder (card->next, card))
	card = card->next;
    return card;
}
    
CardPtr
CardInReverseSuitOrder (card)
    CardPtr	card;
{
    while (card->prev && CardIsInSuitOrder (card, card->prev))
	card = card->prev;
    return card;
}

CardPtr
CardInReverseOrder (card)
    CardPtr	card;
{
    while (card->prev && CardIsInOrder (card, card->prev))
	card = card->prev;
    return card;
}

static Boolean
IsAlternateSuit (a, b)
    CardsSuit	a, b;
{
    return (a == CardsSpade || a == CardsClub) ^ 
	   (b == CardsSpade || b == CardsClub);
}

Boolean
CardIsInAlternatingSuitOrder (a, b)
    CardPtr a, b;
{
    return CardIsInOrder (a,b) && 
	   IsAlternateSuit (a->card.suit, b->card.suit);
}

CardPtr
CardInAlternatingSuitOrder (card)
    CardPtr card;
{
    while (card->next && CardIsInAlternatingSuitOrder (card->next, card))
	card = card->next;
    return card;
}

CardPtr
CardInReverseAlternatingSuitOrder (card)
    CardPtr card;
{
    while (card->prev && CardIsInAlternatingSuitOrder (card, card->prev))
	card = card->prev;
    return card;
}

void
CardSetAnimate (animate)
    Boolean animate;
{
    canAnimate = animate;
}

void
CardDisplayStack (stack)
    CardStackPtr    stack;
{
    stack->empty.shouldBeUp = stack->first || 
			stack->empty.card.suit == CardsNone ? False : True;
    SetShoulds (stack);
    DisplayCards (stack, stack->first);
    DisplayCards (stack, &stack->empty);
}

void
CardTurn (card, face, remember)
    CardPtr	card;
    CardFace	face;
    Boolean	remember;
{
    if (remember)
    {
	CardHistoryPtr	h = New(CardHistoryRec);
	
	h->u.turn.card = card;
	h->u.turn.face = card->face;
	h->type = HistoryTurn;
	h->prev = history;
	h->serial = historySerial;
	history = h;
    }

    card->face = face;
}

void 
CardMove (from_stack, from_card, to_stack, remember)
    CardStackPtr    from_stack;
    CardPtr	    from_card;
    CardStackPtr    to_stack;
    Boolean	    remember;
{
    CardMoveCards (from_stack, from_card, from_stack->last, to_stack, to_stack->last, remember);
}

static void
RemoveStack (stack, first, last)
    CardStackPtr    stack;
    CardPtr	    first, last;
{
    if (first->prev)
	first->prev->next = last->next;
    else
	stack->first = last->next;
    if (last->next)
	last->next->prev = first->prev;
    else
	stack->last = first->prev;
    first->prev = 0;
    last->next = 0;
}

static void
AddStack (stack, first, last, before)
    CardStackPtr    stack;
    CardPtr	    first, last, before;
{
    CardPtr after;
    
    first->prev = before;
    if (before)
    {
	last->next = before->next;
	before->next = first;
    }
    else
    {
	last->next = stack->first;
	stack->first = first;
    }
    if (last->next)
	last->next->prev = last;
    else
	stack->last = last;
}

void
CardMoveCards (from_stack, first_card, last_card, to_stack, to_card, remember)
    CardStackPtr    from_stack;
    CardPtr	    first_card, last_card;
    CardStackPtr    to_stack;
    CardPtr	    to_card;
    Boolean	    remember;
{
    if (remember)
    {
	CardHistoryPtr	h = New(CardHistoryRec);

	h->u.move.source = from_stack;
	h->u.move.dest = to_stack;
	h->u.move.before = first_card->prev;
	h->u.move.first = first_card;
	h->u.move.last = last_card;
	h->type = HistoryMove;
	h->prev = history;
	h->serial = historySerial;
	history = h;
    }
    RemoveStack (from_stack, first_card, last_card);
    AddStack (to_stack, first_card, last_card, to_card);
}

void
CardInitStack (stack, widget, emptySuit, horizontal, position, display)
    CardStackPtr    stack;
    Widget	    widget;
    CardsSuit	    emptySuit;
    Boolean	    horizontal;
    int		    position;
    CardDisplay	    display;
{
    CardPtr   empty;

    stack->first = stack->last = 0;
    stack->widget = widget;
    stack->horizontal = horizontal;
    stack->position = position;
    stack->basePosition = 0;
    stack->display = display;
    empty = &stack->empty;
    empty->card.suit = emptySuit;
    empty->card.rank = CardsAce;
    empty->display.suit = emptySuit;
    empty->display.rank = CardsAce;
    empty->isUp = False;
    empty->face = CardFaceUp;
    empty->next = 0;
    empty->prev = 0;
    empty->row = 0;
    empty->col = 0;
    empty->data = 0;
}

void
CardGenerateStandardDeck (card)
    CardPtr	card;
{
    CardsSuit	suit;
    CardsRank	rank;

    for (suit = CardsClub; suit <= CardsSpade; suit++) 
    {
	for (rank = CardsAce; rank <= CardsKing; rank++) 
	{
	    card->next = card + 1;
	    card->prev = card - 1;
	    card->card.suit = suit;
	    card->card.rank = rank;
	    card->row = -1;
	    card->data = 0;
	    card->face = CardFaceDown;
	    card->shouldBeUp = True;
	    card->isUp = False;
	    card++;
	}
    }
    card[-1].next = 0;
    card[-52].prev = 0;
}

typedef struct _CardShuffle {
    int		value;
    CardPtr	card;
} CardShuffleRec, *CardShufflePtr;

static int
ShuffleCompare (a,b)
    CardShufflePtr  a,b;
{
    return a->value - b->value;
}

void
CardShuffle (stack, remember)
    CardStackPtr    stack;
    Boolean	    remember;
{
    CardPtr	    card, *save;
    int		    numCards;
    CardShufflePtr  shuffle, shuf;
    int		    i;
    
    numCards = 0;
    for (card = stack->first; card; card = card->next)
	++numCards;
    save = 0;
    if (remember)
    {
	CardHistoryPtr	h = New(CardHistoryRec);
	
	h->u.shuffle.stack = stack;
	h->u.shuffle.cards = Some (CardPtr, numCards);
	h->u.shuffle.numCards = numCards;
	h->type = HistoryShuffle;
	h->prev = history;
	h->serial = historySerial;
	history = h;
	save = h->u.shuffle.cards;
    }
    shuffle = Some (CardShuffleRec, numCards);
    shuf = shuffle;
    for (card = stack->first; card; card = card->next)
    {
	if (save)
	    *save++ = card;
	shuf->card = card;
	shuf->value = random ();
	++shuf;
    }
    qsort ((char *) shuffle, numCards, sizeof *shuffle, ShuffleCompare);
    shuf = shuffle;
    stack->first = shuffle[0].card;
    stack->last = shuffle[numCards-1].card;
    for (i = 0; i < numCards; i++)
    {
	if (i < numCards - 1)
	    shuf[0].card->next = shuf[1].card;
	else
	    shuf[0].card->next = 0;
	if (i > 0)
	    shuf[0].card->prev = shuf[-1].card;
	else
	    shuf[0].card->prev = 0;
	shuf++;
    }
}

static void
Unshuffle (stack, cards, numCards)
    CardStackPtr    stack;
    CardPtr	    *cards;
    int		    numCards;
{
    int	    i;

    stack->first = cards[0];
    stack->last = cards[numCards-1];
    for (i = 0; i < numCards; i++, cards++)
    {
	if (i > 0)
	    cards[0]->prev = cards[-1];
	else
	    cards[0]->prev = 0;
	if (i < numCards-1)
	    cards[0]->next = cards[1];
	else
	    cards[0]->next = 0;
    }
}

void
CardInitHistory ()
{
    CardHistoryPtr  h, p;

    for (h = history; h; h = p)
    {
	p = h->prev;
	Dispose (h);
    }
    history = 0;
    historySerial = 0;
}

void
CardRecordHistoryCallback (func, closure)
    void    (*func) ();
    char    *closure;
{
    CardHistoryPtr	h = New(CardHistoryRec);
    
    h->u.callback.func = func;
    h->u.callback.closure = closure;
    h->type = HistoryCallback;
    h->prev = history;
    h->serial = historySerial;
    history = h;
}

Boolean
CardUndo ()
{
    CardHistoryPtr	h, p;
    int			serial;

    if (!history)
	return False;
    serial = history->serial;
    for (h = history; h && h->serial == serial; h = p)
    {
	p = h->prev;
	    
	switch (h->type)
	{
	case HistoryMove:
	    CardMoveCards (h->u.move.dest, h->u.move.first, h->u.move.last,
			   h->u.move.source, h->u.move.before, False);
	    break;
	case HistoryTurn:
	    h->u.turn.card->face = h->u.turn.face;
	    break;
	case HistoryCallback:
	    (*h->u.callback.func) (h->u.callback.closure);
	    break;
	case HistoryShuffle:
	    Unshuffle (h->u.shuffle.stack, h->u.shuffle.cards, h->u.shuffle.numCards);
	    Dispose (h->u.shuffle.cards);
	    break;
	}
	Dispose (h);
    }
    history = h;
    return True;
}

int
CardNextHistory ()
{
    return historySerial++;
}
