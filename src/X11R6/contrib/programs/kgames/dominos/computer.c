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

int	PlayLevel = 10;

int
MakeFirstPlay (source, target, dir, orientation, data)
    DominoPtr	source, target;
    Direction	dir, orientation;
    pointer	data;
{
    PlayPtr	play = (PlayPtr) data;

    play->source = source;
    play->target = target;
    play->dir = dir;
    play->orientation = orientation;
    return FALSE;
}

int
FindPlay (player, play)
    DominoPtr	*player;
    PlayPtr	play;
{
    DominoPtr	source;
    int		ret = FALSE;

    play->player = player;
    for (source = *player; source; source = source->peer[LinkPeer])
    {
	if (!FindPlays (board, source, MakeFirstPlay, 
			(pointer) play))
	{
	    ret = TRUE;
	    break;
	}
    }
    return ret;
}
