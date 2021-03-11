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

#define DOMINO_MAJOR_WIDTH  17
#define DOMINO_MINOR_WIDTH  9
#define DOMINO_MAJOR_HEIGHT 9
#define DOMINO_MINOR_HEIGHT 5
#define DominoUpright(d)    ((d)->orientation == North || (d)->orientation == South)
#define DominoWidth(d)	    (DominoUpright(d) ? DOMINO_MINOR_WIDTH : DOMINO_MAJOR_WIDTH)
#define DominoHeight(d)	    (DominoUpright(d) ? DOMINO_MAJOR_HEIGHT : DOMINO_MINOR_HEIGHT)
#define DominoX(d)	    (DominoUpright(d) ? DOMINO_MINOR_WIDTH / 2 : DOMINO_MAJOR_WIDTH / 2)
#define DominoY(d)	    (DominoUpright(d) ? DOMINO_MAJOR_HEIGHT / 2 : DOMINO_MINOR_HEIGHT / 2)

PeerX(d, dir)
    DominoPtr	d;
    Direction	dir;
{
    switch (dir) {
    case North:
    case South:
	return 0;
    case East:
	return -DominoX(d) - DominoWidth(d->peer[dir]) + DominoX(d->peer[dir]);
    case West:
	return -DominoX(d) + DominoWidth(d) + DominoX(d->peer[dir]);
    }
}

PeerY(d, dir)
    DominoPtr	d;
    Direction	dir;
{
    switch (dir) {
    case East:
    case West:
	return 0;
    case North:
	return -DominoY(d) - DominoHeight(d->peer[dir]) + DominoY(d->peer[dir]);
    case South:
	return -DominoY(d) + DominoHeight(d) + DominoY(d->peer[dir]);
    }
}

BoardSize (b, r, x, y)
    DominoPtr	b;
    RectPtr	r;
    int		x, y;
{
    RectRec	sub;
    Direction	dir;

    r->x1 = x - DominoX(b);
    r->y1 = y - DominoY(b);
    r->x2 = r->x1 + DominoWidth(b);
    r->y2 = r->y1 + DominoHeight(b);
    for (dir = North; dir <= West; dir++)
    {
	if (b->peer[dir])
	{
	    BoardSize (b->peer[dir], &sub, x + PeerX(b,dir), y + PeerY(b, dir));
	    if (sub.x1 < r->x1)
		r->x1 = sub.x1;
	    if (sub.x2 > r->x2)
		r->x2 = sub.x2;
	    if (sub.y1 < r->y1)
		r->y1 = sub.y1;
	    if (sub.y2 > r->y2)
		r->y2 = sub.y2;
	}
    }
}
    
DrawDominos (b, x, y)
    DominoPtr	b;
    int		x, y;
{
    Direction	dir;

    DrawDomino (b, x - DominoX(b), y - DominoY(b));
    for (dir = North; dir <= West; dir++)
	if (b->peer[dir])
	    DrawDominos (b->peer[dir], x + PeerX(b,dir), y + PeerY(b, dir));
}

DrawBoard (b)
    DominoPtr	b;
{
    RectRec	size;
    int		xoff, yoff;

    BoardSize (b, &size, 0, 0);
    InitDraw (&size);
    xoff = -size.x1;
    yoff = -size.y1;
    DrawDominos (b, xoff, yoff);
    DoneDraw (&size);
}

DrawDomino(d, x, y)
    DominoPtr	d;
    int		x, y;
{
    if (d->orientation == North || d->orientation == East)
	DrawEnd (d->pips[0], x, y);
    else
	DrawEnd (d->pips[1], x, y);
    if (d->orientation == North || d->orientation == South)
	y += DOMINO_MINOR_HEIGHT-1;
    else
	x += DOMINO_MINOR_WIDTH-1;
    if (d->orientation == North || d->orientation == East)
	DrawEnd (d->pips[1], x, y);
    else
	DrawEnd (d->pips[0], x, y);
}


char	screen[512][512];

InitDraw (r)
    RectPtr r;
{
    int	    x, y;
    int	    height, width;

    height = r->y2 - r->y1;
    width = r->x2 - r->x1;
    for (y = 0; y < height; y++)
	for (x = 0; x < width; x++)
	    screen[y][x] = ' ';
}

DoneDraw (r)
    RectPtr r;
{
    int	    x, y;
    int	    height, width;

    height = r->y2 - r->y1;
    width = r->x2 - r->x1;
    for (y = 0; y < height; y++)
    {
	for (x = 0; x < width; x++)
	    putchar (screen[y][x]);
	putchar ('\n');
    }
}

char	picture[][DOMINO_MINOR_HEIGHT][DOMINO_MINOR_WIDTH] = {
    {
	"+-------+",
	"|       |",
	"|       |",
	"|       |",
	"+-------+",
    },
    {
	"+-------+",
	"|       |",
	"|   *   |",
	"|       |",
	"+-------+",
    },
    {
	"+-------+",
	"| *     |",
	"|       |",
	"|     * |",
	"+-------+",
    },
    {
	"+-------+",
	"| *     |",
	"|   *   |",
	"|     * |",
	"+-------+",
    },
    {
	"+-------+",
	"| *   * |",
	"|       |",
	"| *   * |",
	"+-------+",
    },
    {
	"+-------+",
	"| *   * |",
	"|   *   |",
	"| *   * |",
	"+-------+",
    },
    {
	"+-------+",
	"| *   * |",
	"| *   * |",
	"| *   * |",
	"+-------+",
    },
    {
	"+-------+",
	"| *   * |",
	"| * * * |",
	"| *   * |",
	"+-------+",
    },
    {
	"+-------+",
	"| * * * |",
	"| *   * |",
	"| * * * |",
	"+-------+",
    },
    {
	"+-------+",
	"| * * * |",
	"| * * * |",
	"| * * * |",
	"+-------+",
    },
};

DrawEnd(pips, x, y)
    Pips    pips;
    int	    x, y;
{
    int	    row;

    for (row = 0; row < DOMINO_MINOR_HEIGHT; row++)
	bcopy (&picture[pips][row][0], &screen[y + row][x], DOMINO_MINOR_WIDTH);
}
