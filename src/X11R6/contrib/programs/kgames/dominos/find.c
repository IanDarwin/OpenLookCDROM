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

typedef struct _findRec {
    DominoPtr	domino;
    int		(*func)();
    pointer	data;
} FindRec, *FindPtr;

static int
ForEachEdge (d, dir, f)
    DominoPtr	d;
    Direction	dir;
    FindPtr	f;
{
    Direction	orientation;

    for (orientation = North; orientation <= West; orientation++)
	if (CanPlay (f->domino, d, dir, orientation))
	    if (!(*f->func) (f->domino, d, dir, orientation, f->data))
		return FALSE;
    return TRUE;
}

static int
FindEdge (d, search_dir, f)
    DominoPtr	d;
    Direction	search_dir;
    FindPtr	f;
{
    Direction	dir;
    
    if (IsDouble (d))
    {
	for (dir = North; dir <= West; dir++)
	    if (dir != OtherDir(search_dir))
	    {
		if (d->peer[dir])
		{
		    if (!FindEdge (d->peer[dir], dir, f))
			return FALSE;
		}
		else
		{
		    if (!ForEachEdge (d, dir, f))
			return FALSE;
		}
	    }
	return TRUE;
    }
    else
    {
	if (d->peer[search_dir])
	    return FindEdge (d->peer[search_dir], search_dir, f);
	return ForEachEdge (d, search_dir, f);
    }
}

FindPlays (board, domino, func, data)
    DominoPtr	board;
    DominoPtr	domino;
    int		(*func)();
    pointer	data;
{
    FindRec	f;
    Direction	dir;

    f.domino = domino;
    f.func = func;
    f.data = data;
    for (dir = North; dir <= West; dir++)
	if (IsDouble (board) ||
	    dir == board->orientation ||
	    dir == OtherDir(board->orientation))
	{
	    if (!FindEdge (board, dir, &f))
		return FALSE;
	}
    return TRUE;
}
