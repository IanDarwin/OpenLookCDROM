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

# include <stdio.h>
# include "dominos.h"


static void
WriteIndent (file, level)
    FILE    *file;
    int	    level;
{
    while (level--)
	putc (' ', file);
}

static void
WriteDomino (file, d, level)
    FILE	*file;
    DominoPtr	d;
{
    Direction	dir;
    
    WriteIndent (file, level);
    fprintf (file, "(");
    if (d)
    {
	fprintf (file, "%d %d %d",
		 d->pips[0], d->pips[1], d->orientation);
	fprintf (file, "\n");
	for (dir = North; dir <= West; dir++)
	    WriteDomino (file, d->peer[dir], level + 1);
	WriteIndent (file, level);
    }
    fprintf (file, ")\n");
}
	     
WriteDominos (file, d)
    FILE	*file;
    DominoPtr	d;
{
    WriteDomino (file, d, 0);
}

#define TOKEN_EOF	-1
#define TOKEN_OP	0
#define TOKEN_CP	1
#define TOKEN_NUMBER	2

#define STATE_BEGIN	0
#define STATE_DIGIT	1

#define MAX_TOKEN	256

static char DominoToken[MAX_TOKEN];

int	    DominoErrno;

static void
syntax ()
{
    if (!DominoErrno)
	FileError ("Syntax error in file");
    DominoErrno = 1;
}

static int
LexDomino (file)
    FILE    *file;
{
    int	    c;
    int	    state = STATE_BEGIN;
    char    *tokenp = DominoToken;

    for (;;)
    {
	c = getc (file);
	switch (state) {
	case STATE_BEGIN:
	    switch (c) {
	    case EOF:
		return TOKEN_EOF;
	    case '(':
		return TOKEN_OP;
	    case ')':
		return TOKEN_CP;
	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
		state = STATE_DIGIT;
		*tokenp++ = c;
		break;
	    case ' ': case '\t': case '\n':
		break;
	    default:
		syntax ();
		return TOKEN_EOF;
	    }
	    break;
	case STATE_DIGIT:
	    switch (c) {
	    case '(':
	    case ')':
	    case ' ':
	    case '\t':
	    case '\n':
		ungetc (c, file);
	    case EOF:
		*tokenp = '\0';
		return TOKEN_NUMBER;
	    case '0': case '1': case '2': case '3': case '4':
	    case '5': case '6': case '7': case '8': case '9':
		if (tokenp >= DominoToken + sizeof DominoToken - 1)
		{
		    syntax ();
		    return TOKEN_EOF;
		}
		*tokenp++ = c;
		break;
	    default:
		syntax ();
		return TOKEN_EOF;
	    }
	}
    }
}

static DominoPtr
ReadDomino (file)
    FILE    *file;
{
    DominoPtr	d = 0;
    Direction	dir;
    Pips	pips[2];
    int		i;

    switch (LexDomino (file)) {
    case TOKEN_OP:
	break;
    default:
	syntax ();
	return 0;
    }
    for (i = 0; i <  2; i++) {
	switch (LexDomino (file)) {
	case TOKEN_NUMBER:
	    pips[i] = atoi(DominoToken);
	    break;
	case TOKEN_CP:
	    if (i == 0)
		return 0;
	default:
	    syntax ();
	    return 0;
	}
    }
    d = MakeDomino (pips[0], pips[1]);
    switch (LexDomino (file)) {
    case TOKEN_NUMBER:
	d->orientation = atoi (DominoToken);
	break;
    default:
	syntax ();
	DisposeDominos (d);
	return 0;
    }
    for (dir = North; dir <= West; dir++)
	d->peer[dir] = ReadDomino (file);
    switch (LexDomino (file)) {
    case TOKEN_CP:
	return d;
    default:
	syntax ();
	DisposeDominos (d);
	return 0;
    }
}

DominoPtr
ReadDominos (file)
    FILE    *file;
{
    DominoErrno = 0;
    return ReadDomino (file);
}

WriteScores (file, scores, num)
    FILE    *file;
    int	    *scores;
    int	    num;
{
    int	    i;

    for (i = 0; i < num; i++)
	fprintf (file, "%d ", scores[i]);
    fprintf (file, "\n");
}

ReadScores (file, scores, num)
    FILE    *file;
    int	    *scores;
    int	    num;
{
    int	    i;

    for (i = 0; i < num; i++)
    {
	switch (LexDomino (file)) {
	case TOKEN_NUMBER:
	    scores[i] = atoi(DominoToken);
	    break;
	default:
	    syntax ();
	    return 0;
	}
    }
    return num;
}

WriteInt (file, i)
    FILE    *file;
    int	    i;
{
    fprintf (file, "%d\n", i);
}

ReadInt (file, i)
    FILE    *file;
    int	    *i;
{
    switch (LexDomino (file)) {
    case TOKEN_NUMBER:
	*i = atoi (DominoToken);
	return TRUE;
    default:
	syntax ();
	return FALSE;
    }
}
