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

int	    NumPlayers = 2;
DominoPtr   pile;
DominoPtr   player[MAX_PLAYERS];
DominoPtr   board;
UndoPtr	    undoList;

DisposeGame ()
{
    Player	p;
    
    if (pile)
    {
	DisposeDominos(pile);
	pile = 0;
    }
    for (p = 0; p < NumPlayers; p++)
	if(player[p])
	{
	    DisposeDominos(player[p]);
	    player[p] = 0;
	}
    if (board)
    {
	DisposeDominos(board);
	board = 0;
    }
    if (undoList)
	DisposeUndoList ();
}

ResetGame ()
{
    Player	p;
    int		i;
    
    DisposeGame ();
    pile = InitDominos(MAX_DOMINO_PIP);
    pile = MixDominos(pile);
    for (p = 0; p < NumPlayers; p++)
	for (i = 0; i < PLAYER_START; i++)
	    PlayerDraw (&player[p], FALSE);
}

int
PlayerDraw(p, remember)
    DominoPtr	*p;
    int		remember;
{
    DominoPtr	d;
    DominoPtr	*prev;
    UndoPtr	undo;

    d = PickDomino(&pile);
    if (!d)
	return FALSE;
    for (prev = p; *prev; prev = &(*prev)->peer[LinkPeer])
	/*SUPPRESS 530*/
	;
    if (remember)
    {
	undo = New(UndoRec);
	if (!undo)
	    return FALSE;
	undo->player = p;
	undo->next = undoList;
	undoList = undo;
	undo->dest = prev;
	undo->source = &pile;
	undo->domino = d;
	undo->orientation = d->orientation;
    }
    *prev = d;
    return TRUE;
}

DominoPtr *
PlayerExtract (p, source)
    DominoPtr	*p;
    DominoPtr	source;
{
    while (*p != source)
	p = &(*p)->peer[LinkPeer];
    *p = source->peer[LinkPeer];
    source->peer[LinkPeer] = 0;
    return p;
}

int
PlayerMove (p, source, target, dir, orientation)
    DominoPtr	*p, source, target;
    Direction	dir;
    Direction	orientation;
{
    UndoPtr undo;

    undo = New(UndoRec);
    if (!undo)
	return FALSE;
    undo->next = undoList;
    undoList = undo;
    undo->player = p;
    undo->dest = &target->peer[dir];
    undo->domino = source;
    undo->orientation = source->orientation;
    undo->source = PlayerExtract (p, source);
    target->peer[dir] = source;
    if (IsDouble (source))
    {
	orientation = target->orientation + 1;
	if (orientation > West)
	    orientation = North;
    }
    source->orientation = orientation;
    return TRUE;
}

PlayerFirstMove (p, source)
    DominoPtr	*p;
    DominoPtr	source;
{
    UndoPtr undo;

    undo = New (UndoRec);
    if (!undo)
	return;
    undo->next = undoList;
    undoList = undo;
    undo->player = p;
    undo->dest = &board;
    undo->source = PlayerExtract (p, source);
    undo->domino = source;
    undo->orientation = source->orientation;
    board = source;
    source->orientation = East;
}

PlayerUndo ()
{
    UndoPtr undo;

    undo = undoList;
    if (!undo)
	return FALSE;
    undoList = undo->next;
    *undo->dest = 0;
    undo->domino->peer[LinkPeer] = *undo->source;
    *undo->source = undo->domino;
    undo->domino->orientation = undo->orientation;
    Dispose (undo);
    return TRUE;
}

DisposeUndoList ()
{
    UndoPtr	undo;
    
    while (undoList) 
    {
	undo = undoList;
	undoList = undo->next;
	Dispose (undo);
    }
}
		    
int
IsDouble(d)
    DominoPtr	d;
{
    return d->pips[0] == d->pips[1];
}

OtherDir (dir)
    Direction	dir;
{
    switch (dir) {
    case North: return South;
    case South: return North;
    case East: return West;
    case West: return East;
    }
    /*NOTREACHED*/
}

int
CanUseEdge (d, dir, orientation)
    DominoPtr	d;
    Direction	dir, orientation;
{
    if (IsDouble (d))
	return TRUE;
    if (dir == orientation || dir == OtherDir(orientation))
	return TRUE;
    return FALSE;
}

Pips
EdgePips (d, dir, orientation)
    DominoPtr	d;
    Direction	dir;
{
    if (IsDouble (d))
	return d->pips[0];
    if (dir == orientation)
	return d->pips[0];
    if (dir == OtherDir(orientation))
	return d->pips[1];
    return (Pips) -1;
}

int
CanPlay (source, target, dir, orientation)
    DominoPtr	source, target;
    Direction	dir, orientation;
{
    if (target->peer[dir])
	return FALSE;
    if (!CanUseEdge (target, dir, target->orientation))
	return FALSE;
    if (!CanUseEdge (source, OtherDir(dir), orientation))
	return FALSE;
    if (EdgePips (source, OtherDir(dir), orientation) !=
	EdgePips (target, dir, target->orientation))
	return FALSE;
    return TRUE;
}

