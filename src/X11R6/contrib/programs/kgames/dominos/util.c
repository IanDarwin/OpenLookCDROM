/*
 * $NCDId$
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

#include "dominos.h"

DominoPtr
MakeDomino(a, b)
    Pips    a, b;
{
    DominoPtr	domino;

    domino = New(DominoRec);
    domino->pips[0] = a;
    domino->pips[1] = b;
    domino->peer[North] = 0;
    domino->peer[East] = 0;
    domino->peer[South] = 0;
    domino->peer[West] = 0;
    domino->orientation = North;
    return domino;
}

DominoPtr
InitDominos (max)
    Pips    max;
{
    Pips	r, c;
    DominoPtr	dominos, d, *prev;

    prev = &dominos;
    *prev = d = MakeDomino(0, 0);
    prev = &d->peer[LinkPeer];
    for (r = 0; r <= max; r++)
	for (c = r; c <= max; c++)
	{
	    *prev = d = MakeDomino(r, c);
	    prev = &d->peer[LinkPeer];
	}
    return dominos;
}

typedef struct _DominoMix {
    int		value;
    DominoPtr	domino;
} DominoMixRec, *DominoMixPtr;

static int
MixCompare (a,b)
    DominoMixPtr    a, b;
{
    return a->value - b->value;
}

DominoPtr
MixDominos (dominos)
    DominoPtr	dominos;
{
    int		    numDominos;
    DominoPtr	    d;
    DominoMixPtr    mix, m;
    int		    i;
    
    numDominos = 0;
    for (d = dominos; d; d = d->peer[LinkPeer])
	numDominos++;
    m = mix = Some(DominoMixRec, numDominos);
    for (d = dominos; d; d = d->peer[LinkPeer])
    {
	m->value = random ();
	m->domino = d;
	m++;
    }
    qsort ((char *) mix, numDominos, sizeof *mix, MixCompare);
    m = mix;
    dominos = m->domino;
    for (i = 0; i < numDominos; i++)
    {
	d = m->domino;
	if (i < numDominos - 1)
	    d->peer[LinkPeer] = m[1].domino;
	else
	    d->peer[LinkPeer] = 0;
	m++;
    }
    return dominos;
}

DominoPtr   
PickDomino (dominos)
    DominoPtr	*dominos;
{
    DominoPtr	d;

    d = *dominos;
    if (d)
    {
	*dominos = d->peer[LinkPeer];
	d->peer[LinkPeer] = 0;
    }
    return d;
}

DisposeDominos (domino)
    DominoPtr	domino;
{
    Direction	dir;
    
    for (dir = North; dir <= West; dir++)
	if (domino->peer[dir])
	    DisposeDominos(domino->peer[dir]);
    Dispose (domino);
}

int
TraverseDominos (d, func, data)
    DominoPtr	d;
    int		(*func)();
    pointer	data;
{
    Direction	dir;
    
    if (!(*func) (d, data))
	return FALSE;
    for (dir = North; dir <= West; dir++)
	if (d->peer[dir])
	{
	    if (!TraverseDominos (d->peer[dir], func, data))
		return FALSE;
	}
    return TRUE;
}
