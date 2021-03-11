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
# include	<Xkw/Cards.h>
# include	<Xkw/SuitCards.h>

SuitCardsInit (s, under, widget, emptySuit, horizontal, row, col, display)
    SuitCardsPtr    s;
    CardStackPtr    under;
    Widget	    widget;
    CardsSuit	    emptySuit;
    Boolean	    horizontal;
    int		    row, col;
    CardDisplay	    display;
{
    int		    position;
    int		    basePosition;
    CardsSuit	    suit;
    CardStackPtr    stack;

    if (horizontal)
    {
	position = col;
	basePosition = row;
    }
    else
    {
	position = row;
	basePosition = col;
    }
    for (suit = CardsClub; suit <= CardsSpade; suit++)
    {
	stack = &s->suits[CardsSuitToInt(suit)];
	CardInitStack (stack, under, widget, emptySuit, 
		       !horizontal, position, display);
	stack->basePosition = basePosition;
	under = stack;
	position++;
    }
    s->aceHigh = True;
}

Boolean
SuitRankGreater (a, b, aceHigh)
    CardsRank	a, b;
    Boolean	aceHigh;
{
    if (a == b)
	return False;
    if (a == CardsAce)
	return True;
    if (b == CardsAce)
	return False;
    return a > b;
}

SuitCardsMoveFromStack (from_stack, card, to_suit, remember)
    CardStackPtr    from_stack;
    CardPtr	    card;
    SuitCardsPtr    to_suit;
    Boolean	    remember;
{
    CardStackPtr    to_stack;
    CardPtr	    to_card, next_card;
    CardsSuit	    suit;
    CardsRank	    rank;

    suit = card->card.suit;
    rank = card->card.rank;
    to_stack = &to_suit->suits[CardsSuitToInt(suit)];
    to_card = 0;
    for (next_card = to_stack->first; next_card; next_card = next_card->next)
    {
	if (SuitRankGreater (rank, next_card->card.rank, to_suit->aceHigh))
	    break;
	to_card = next_card;
    }
    CardMoveCards (from_stack, card, card, to_stack, to_card, remember);
}

SuitCardsMove (from_suit, card, to_suit, remember)
    SuitCardsPtr    from_suit;
    CardPtr	    card;
    SuitCardsPtr    to_suit;
    Boolean	    remember;
{
    CardStackPtr    from_stack;
    CardsSuit	    suit;

    suit = card->card.suit;
    from_stack = &from_suit->suits[CardsSuitToInt(suit)];
    SuitCardsMoveFromStack (from_stack, to_suit, card, remember);
}

SuitCardsMoveToStack (from_suit, card, to_stack, to_card, remember)
    SuitCardsPtr    from_suit;
    CardPtr	    card;
    CardStackPtr    to_stack;
    CardPtr	    to_card;
    Boolean	    remember;
{
    CardStackPtr    from_stack;
    CardsSuit	    suit;

    suit = card->card.suit;
    from_stack = &from_suit->suits[CardsSuitToInt(suit)];
    CardMoveCards (from_stack, card, card, to_stack, to_card, remember); 
}

SuitCardsDisplay (s)
    SuitCardsPtr    s;
{
    CardsSuit	suit;

    for (suit = CardsClub; suit <= CardsSpade; suit++)
	CardDisplayStack (&s->suits[CardsSuitToInt(suit)]);
}

CardPtr
SuitCardsHandInputToCard (s, input)
    SuitCardsPtr    s;
    HandInputPtr    input;
{
    CardsSuit	    suit;
    int		    suitPosition;
    CardStackPtr    stack;
    CardPtr	    card;

    if (s->suits[CardsClub].horizontal)
	suitPosition = input->row;
    else
	suitPosition = input->col;
    suit = IntToCardsSuit (suitPosition - s->suits[CardsClub].position);
    stack = &s->suits[suit];
    for (card = stack->last; card; card = card->prev)
	if (card->shouldBeUp &&
	    card->row == input->row && card->col == input->col)
	    return card;
    return 0;
}
