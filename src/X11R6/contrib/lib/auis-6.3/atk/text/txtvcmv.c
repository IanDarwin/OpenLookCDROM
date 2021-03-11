/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/text/RCS/txtvcmv.c,v 1.11 1993/10/26 22:43:58 gk5g Exp $";
#endif

#include <andrewos.h>
#include <class.h>
#include <ctype.h>
#include <txtvcmds.h>
#include <txtvinfo.h>
#include <text.ih>
#include <im.ih>
#include <message.ih>
#include <mark.ih>

#define AUXMODULE 1
#include <textv.eh>

void textview_EndOfWordCmd (self)
    register struct textview *self;
{/**/
    register int j, ct, pos, dlen, testType;
    register struct text *d;
    char	c;
    /****/
    pos = textview_CollapseDot(self);
    ct = im_Argument(textview_GetIM(self));
    d=Text(self);
    dlen= text_GetLength(d);
    for ( j = 0; j < ct; j++ )
    {
 	while ( pos < dlen && (testType = charType(c = text_GetChar(d, pos))) == WHITESPACE )
	{
	    if ( c == '\n' && text_GetChar(d, pos + 1) == '\n' )
	    {
		/* stop at blank lines */
		break;
	    }
	    pos++;
	}
	pos++;

	if ( c != '\n' )
	    while (pos < dlen && charType(text_GetChar(d, pos))== testType ) pos++;
    }
    textview_SetDotPosition(self,pos);
    textview_FrameDot(self, pos);
    textview_WantUpdate(self, self);
}

void textview_ForwardWordCmd (self)
    register struct textview *self;
{
    register int j, count, pos, dlen;
    register struct text *d;

    pos = textview_CollapseDot(self);
    count = im_Argument(textview_GetIM(self));
    d=Text(self);
    dlen= text_GetLength(d);
    for ( j = 0; j < count; j++ )
    {
	if ( self->editor == EMACS )
	{
	    while (pos<dlen && isalnum(text_GetChar(d, pos))==0) pos++;
	    while (pos<dlen && isalnum(text_GetChar(d,pos))!=0) pos++;
	}
	else {
	    register int  testType;
	    char	c;

	    if ( (testType = charType(text_GetChar(d, pos))) == WHITESPACE )
	    {
		while ( pos < dlen && (testType = charType(c = text_GetChar(d, pos))) == WHITESPACE )
		{
		    if ( c == '\n' && charType(text_GetChar(d, pos - 1)) == WHITESPACE )
		    {
			/* stop at end of trailing white space */
			break;
		    }
		    pos++;
		}
	    }
	    else
	    {
		while ( pos < dlen && charType(text_GetChar(d, pos)) == testType ) pos++;
		while ( pos < dlen && charType(c = text_GetChar(d, pos)) == WHITESPACE )
		{
		    if ( c == '\n' )
			if ( text_GetChar(d, pos + 1) == '\n' )
			{
			    /* stop at blank lines */
			    pos++;
			    break;
			}
			else
			    if ( charType(text_GetChar(d, pos - 1)) == WHITESPACE )
			    {
				/* stop at end of trailing white space */
				break;
			    }
		    pos++;
		}
	    }
	}
    }

    textview_SetDotPosition(self,pos);
    textview_FrameDot(self, pos);
    textview_WantUpdate(self, self);
}

void textview_BackwardWordCmd (self)
    register struct textview *self;
{
    register int j, count, pos;
    register struct text *d;

    pos = textview_CollapseDot(self);
    count = im_Argument(textview_GetIM(self));
    d=Text(self);
    for ( j = 0; j < count; j++ )
    {
	if ( self->editor == EMACS )
	{
	    while (pos>0 && isalnum(text_GetChar(d,pos-1))==0)  pos--;
	    while (pos>0 && isalnum(text_GetChar(d,pos-1))!=0) pos--;
	}
	else
	{
	    register int testType;
	    char	c;

	    pos--;
	    while ( pos > 0 && (testType = charType(c = text_GetChar(d, pos))) == WHITESPACE )
	    {
		if ( c == '\n' && text_GetChar(d, pos - 1) == '\n' )
		{
		    /* stop at blank lines */
		    break;
		}
		pos--;
	    }
	    if ( c != '\n' )
		while ( pos > 0 && charType(text_GetChar(d, pos - 1))== testType )  pos--;
	}
    }
    textview_SetDotPosition(self,pos);
    textview_FrameDot(self, pos);
    textview_WantUpdate(self, self);
}

void textview_LineToTopCmd(self)
register struct textview *self;
{
    long pos;

    pos = textview_GetDotPosition(self) + textview_GetDotLength(self);
    pos = textview_MoveBack(self, pos, 0, textview_MoveByLines, 0, 0);

    textview_SetTopPosition(self, pos);
    textview_WantUpdate(self, self);
}

void textview_ForwardParaCmd(self)
register struct textview *self;
{
    register int j, ct, pos, dlen;
    register struct text *d;

    j = 0;
    ct = im_Argument(textview_GetIM(self));
    d = Text(self);
    dlen = text_GetLength(d);
    pos =textview_GetDotPosition(self);
    while (j<ct) {
	while (pos<dlen) {
	    pos++;	/* always move at least one character */
	    if (text_GetChar(d,pos) == '\n') break;
	}
	textview_SetDotPosition(self, pos);
	if (pos < dlen) pos++;
	j++;
    }
    textview_WantUpdate(self, self);
}

void textview_BackwardParaCmd(self)
register struct textview *self;
{
    register int j, ct, pos;
    register struct text *d;

    j = 0;
    ct = im_Argument(textview_GetIM(self));
    d=Text(self);
    pos=textview_GetDotPosition(self);
    while (j<ct)  {
	if (pos > 0) pos--;
	while (pos>0 && text_GetChar(d,pos-1) != '\n') pos--;
	j++;
    }
    textview_SetDotPosition(self,pos);
    textview_WantUpdate(self, self);
}

void textview_GotoParagraphCmd(self)
register struct textview *self;
{
    char temp[100];
    int line, gf;
    register int pos;
    register struct text *d;

    d = Text(self);

    if (im_ArgProvided(textview_GetIM(self)))
        line = im_Argument(textview_GetIM(self));
    else {
        gf = message_AskForString(self, 0, "What paragraph? ", 0, temp, 100);
        if (gf < 0)
            return;
        line = atoi(temp);
    }

    pos = text_GetPosForLine(d, line);

    textview_SetDotPosition(self, pos);
    textview_SetDotLength(self, 0);
    textview_FrameDot(self, pos);
    textview_WantUpdate(self, self);
}

void textview_WhatParagraphCmd (v)
register struct textview *v;
{
    char temp[100];
    register int i, pos;
    register struct text *d;

    d=Text(v);
    pos = textview_GetDotPosition(v);

    i = text_GetLineForPos(d, pos);
    
    sprintf(temp,"Paragraph %d.",i);
    message_DisplayString(v, 0, temp);
}

void textview_ViGlitchUpCmd(self)
register struct textview *self;
{
    register int n;
    register int pos;
    long 	dotPos;
    long 	dist, lines;

    n = im_Argument(self->header.view.imPtr);
    pos = textview_GetTopPosition(self);
    pos = textview_MoveBack(self, pos, 0, textview_MoveByLines, 0, 0);
    pos = textview_MoveForward(self, pos, n, textview_MoveByLines, 0, 0);
    if (self->scroll == textview_ScrollBackward)
	self->scroll = textview_MultipleScroll;
    dotPos = textview_GetDotPosition(self);
    if ( textview_Visible(self, dotPos) )
	dotPos = textview_MoveForward(self, dotPos, n, textview_MoveByLines, &dist, &lines);
    else
	dotPos = pos;
    textview_SetDotPosition(self, pos);
    textview_SetTopPosition(self, pos);
    textview_WantUpdate(self, self);
}

void textview_ViGlitchDownCmd(self)
register struct textview *self;
{
    register int n,pos;
    long	dotPos;
    long 	dist, lines;

    n = im_Argument(self->header.view.imPtr);
    pos = textview_GetTopPosition(self);
    pos = textview_MoveBack(self, pos, n, textview_MoveByLines, &dist, &lines);
    if (self->scroll == textview_ScrollForward)
	self->scroll = textview_MultipleScroll;
    else {
	if (self->scrollDist == -1) {
	    self->scrollDist = dist;
	    self->scrollLine = lines;
	}
	else {
	    self->scrollDist += dist;
	    if (self->scrollDist >= textview_GetLogicalHeight(self))
		self->scrollDist = -1;
	    else
		self->scrollLine += lines;
	}
    }
    dotPos = textview_GetDotPosition(self);
    if ( textview_Visible(self, dotPos) )
	dotPos = textview_MoveBack(self, dotPos, n, textview_MoveByLines, &dist, &lines);
    else
	dotPos = pos;
    textview_SetDotPosition(self, pos);
    textview_SetTopPosition(self, pos);
    textview_WantUpdate(self, self);
}

void textview_DownCmd(self)
    register struct textview *self;
{
    if ( im_Argument(self->header.view.imPtr) == 1 )
    {
	/* default is half the screen */
	(self->header.view.imPtr)->argState.argProvided = TRUE;
	(self->header.view.imPtr)->argState.argument  = textview_GetLines(self)/2;
    }
    textview_ViGlitchUpCmd(self);
}

void textview_UpCmd(self)
    register struct textview *self;
{
    if ( im_Argument(self->header.view.imPtr) == 1 )
    {
	/* default is half the screen */
	(self->header.view.imPtr)->argState.argProvided = TRUE;
	(self->header.view.imPtr)->argState.argument  = textview_GetLines(self)/2;
    }
    textview_ViGlitchDownCmd(self);
}

void textview_GlitchUpCmd(self)
register struct textview *self;
{
    register int n;
    register int pos;

    n = im_Argument(textview_GetIM(self));
    pos = textview_GetTopPosition(self);
    pos = textview_MoveBack(self, pos, 0, textview_MoveByLines, 0, 0);
    pos = textview_MoveForward(self, pos, n, textview_MoveByPseudoLines, 0, 0);
    if (self->scroll == textview_ScrollBackward) {
        self->scroll = textview_MultipleScroll;
    }
    textview_SetTopOffTop(self, pos, self->pixelsComingOffTop);
    textview_WantUpdate(self, self);
}

void textview_GlitchDownCmd(self)
register struct textview *self;
{
    register int n,pos;
    long dist, lines;

    n = im_Argument(textview_GetIM(self));
    pos = textview_GetTopPosition(self);
    pos = textview_MoveBack(self, pos, n, textview_MoveByPseudoLines, &dist, &lines);
    if (self->scroll == textview_ScrollForward) {
        self->scroll = textview_MultipleScroll;
    }
    else {
	if (self->scrollDist == -1) {
	    self->scrollDist = dist;
	    self->scrollLine = lines;
	}
	else {
	    self->scrollDist += dist;
	    if (self->scrollDist >= textview_GetLogicalHeight(self))
		self->scrollDist = -1;
	    else
		self->scrollLine += lines;
	}
    }
	
    textview_SetTopOffTop(self, pos, self->pixelsComingOffTop);
    textview_WantUpdate(self, self);
}

static long PageOverlap(viewHeight)
long viewHeight;
{
    return (viewHeight < 147) ? viewHeight / 3 : 49;
}

void textview_NextScreenCmd(self)
struct textview *self;
{
    int argument = im_Argument(textview_GetIM(self));
    int count;

    im_ClearArg(textview_GetIM(self));
    for (count = 0; count < argument; ++count) {
        int numLines = textview_GetLines(self);
        long pos;
        long overlap;
        long viewHeight;

        if (numLines == 0) {
            /* Do nothing; there is no text on the screen. */
            return;
        }

        viewHeight = textview_GetLogicalHeight(self) - self->by;
        overlap = PageOverlap(viewHeight);

        pos = textview_GetTopPosition(self);
        /* get line aligned */
        pos = textview_MoveBack(self, pos, 0, textview_MoveByLines, 0, 0);
        pos = textview_MoveForward(self, pos, viewHeight - overlap, textview_MoveByPixels, 0, 0);
        if (self->scroll == textview_ScrollBackward) {
            self->scroll = textview_MultipleScroll;
        }
        textview_SetTopOffTop(self, pos, self->pixelsComingOffTop);

        if ( self->editor == VI ) {
            textview_SetDotPosition(self, pos);
        }
    }
    textview_WantUpdate(self, self);
}

void textview_PrevScreenCmd(self)
register struct textview *self;
{
    int argument = im_Argument(textview_GetIM(self));
    int count;

    im_ClearArg(textview_GetIM(self));

    for (count = 0; count < argument; ++count) {
        long numLines = textview_GetLines(self);
        long pos;
        long viewHeight;
        long dist;
        long lines;
        long overlapHeight;

        viewHeight = textview_GetLogicalHeight(self) - self->by;
        if (numLines == 0) {
            overlapHeight = 0;
        }
        else {
            overlapHeight = PageOverlap(viewHeight);
        }

        pos = textview_GetTopPosition(self);
        pos = textview_MoveBack(self, pos, viewHeight - overlapHeight, textview_MoveByPixels, &dist, &lines);

        if (self->scroll == textview_ScrollForward)
            self->scroll = textview_MultipleScroll;
        else if (self->scrollDist == -1)  {
            self->scrollDist = dist;
            self->scrollLine = lines;
        }
        else  {
            self->scrollDist = -1;
        }

        textview_SetTopOffTop(self, pos, self->pixelsComingOffTop);

        if ( self->editor == VI ) {
            textview_SetDotPosition(self, textview_MoveForward(self, pos, lines + 1, textview_MoveByLines, &dist, &lines));
        }
    }
    textview_WantUpdate(self, self);
}

void textview_StartOfParaCmd (self)
register struct textview *self;
{
    register struct text *d;
    register int pos;

    d = Text(self);
    pos = textview_GetDotPosition(self);
    pos = text_GetBeginningOfLine(d, pos);
    textview_SetDotPosition(self, pos);
    textview_SetDotLength(self, 0);
    textview_WantUpdate(self, self);
}

void textview_EndOfParaCmd (self)
register struct textview *self;
{
    register struct text *d;
    register int pos;

    d = Text(self);
    pos = text_GetEndOfLine(d, textview_GetDotPosition(self));
    textview_SetDotPosition(self, pos);
    textview_SetDotLength(self, 0);
    textview_WantUpdate(self, self);
}

void textview_SelectRegionCmd(self)
register struct textview *self;
{
    register int i;
    register int dot, mark;

    mark = mark_GetPos(self->atMarker);
    dot = textview_GetDotPosition(self);
    if (mark > dot) {
	i = dot;
	dot = mark;
	mark = i;
    }
    /* Now assume that mark <= dot */
    textview_SetDotLength(self,dot-mark);
    textview_SetDotPosition(self,mark);
    textview_WantUpdate(self, self);
}

void textview_CtrlAtCmd(self)
register struct textview *self;
{
    mark_SetPos(self->atMarker,textview_GetDotPosition(self));
    mark_SetLength(self->atMarker,textview_GetDotLength(self));
    message_DisplayString(self, 0, "Mark set.");
}

void textview_BackwardCmd(self)
register struct textview *self;
{
    register long endpos, len;
    register struct text *d;

    endpos = textview_CollapseDot(self);
    d = Text(self);
    len = im_Argument(textview_GetIM(self));

    if (endpos == 0)
        return;
    if (endpos - len < 0)
	len = endpos;

    if ( self->editor == VI )
	for ( ; endpos > 0 && len > 0 && text_GetChar(d, endpos - 1) != '\n' ; endpos--)
	    len--;
    else
	endpos -= len;

    textview_SetDotPosition(self,endpos);
    textview_FrameDot(self, endpos);
    textview_WantUpdate(self, self);
}

void textview_ForwardCmd(self)
register struct textview *self;
{
    register long pos;
    register long newPos;

    pos = textview_CollapseDot(self);
    textview_SetDotPosition(self, (newPos = pos + im_Argument(textview_GetIM(self))));
    if (pos != textview_GetDotPosition(self)) {
        textview_FrameDot(self, newPos);
        textview_WantUpdate(self, self);
    }
}

void textview_PreviousLineCmd (self)
register struct textview *self;  /**/
{
    register int npos, j;
    int xpos;
    struct mark tm;	/* note this mark is not on the document's marker chain */
    long pos, dist, lines, cumLines, currentline, startPos, nlPos, prevPos;
    struct text		*text;

    startPos = textview_GetDotPosition(self);
    pos = textview_CollapseDot(self);
    text	= Text(self);
    if (im_GetLastCmd(textview_GetIM(self)) == lcMove)
	xpos = self->movePosition;
    else
	self->movePosition = xpos = self->cexPos;
    if ( self->editor == VI )
    {
	currentline = textview_FindLineNumber(self, startPos);
	/* ignore wrap-around lines */
	cumLines	=	lines	= 0;
	npos	= startPos;
	for (j = im_Argument(self->header.view.imPtr); j > 0; j--)
	{
	    nlPos = text_GetBeginningOfLine(text, text_GetBeginningOfLine(text, npos) - 1);
	    do
	    {
		prevPos = npos;
		npos = textview_MoveBack(self, prevPos, 1, textview_MoveByLines, &dist, &lines);
		cumLines++;
	    } while ( npos > nlPos && prevPos != npos);
	}
    }
    else {
	currentline = textview_FindLineNumber(self, pos);
	npos = textview_MoveBack(self, pos, im_Argument(textview_GetIM(self)), textview_MoveByLines, &dist, &cumLines);
    }
    if (cumLines > currentline)  {
	/* Have moved back off the screen */
	if (self->scroll == textview_ScrollForward)
	    self->scroll = textview_MultipleScroll;
	if (self->scrollDist == -1 )  {
	    if (currentline != -1)  {
		self->scrollDist = dist - (self->lines[currentline].y - self->lines[0].y);
		self->scrollLine = cumLines - currentline;
		textview_SetTopPosition(self, npos);
	    }
	    else  {
		long topPos;

		currentline = textview_FindLineNumber(self, startPos);
		if (currentline != -1 || (startPos <= (topPos = textview_GetTopPosition(self)) && startPos >= topPos))
		{
		    if ( self->editor == VI )
		    {
			(self->header.view.imPtr)->argState.argProvided = TRUE;
			(self->header.view.imPtr)->argState.argument  = cumLines;
			textview_ViGlitchDownCmd(self);
		    }
		    else
			textview_FrameDot(self, npos);
		}
	    }
	}
	else  {
	    self->scrollDist += dist;
	    self->scrollLine += cumLines;
	    textview_SetTopPosition(self, npos);
	}
    }
    else
	textview_FrameDot(self, npos);
    mark_SetPos(&tm, npos);
    mark_SetLength(&tm, 0);
    if (self->csxPos != -1)  {
	struct formattinginfo info;

	npos = textview_LineRedraw (self, textview_GetPosition, &tm, self->bx,
	    self->by, textview_GetLogicalWidth(self) - 2 * self->bx,
	    textview_GetLogicalHeight(self) - 2 * self->by, xpos, (int *) 0, NULL, &info);
    }
    textview_SetDotPosition(self, npos);
    im_SetLastCmd(self->header.view.imPtr, lcMove);
    textview_WantUpdate(self, self);
}

void textview_NextLineCmd (self)
register struct textview *self; /**/
{
    register int npos, j;
    int xpos;
    struct mark tm;	/* note this mark is not on the document's marker chain */
    long currentline, pos, lines, nlines, newline, startPos, nlPos, dsize;
    static int linetomove;

    startPos = textview_GetDotPosition(self);
    pos = textview_CollapseDot(self);
    dsize	= text_GetLength(Text(self));

    if (im_GetLastCmd(self->header.view.imPtr) == lcMove)
	xpos = self->movePosition;
    else
	self->movePosition = xpos = self->cexPos;
    if ( self->editor == VI )
    {
	currentline = textview_FindLineNumber(self, startPos);
	npos = textview_MoveBack(self,startPos,0, textview_MoveByLines, NULL, NULL);

	/* ignore wrap-around lines */
	nlines	= 0;
	npos	= startPos;
	for (j = im_Argument(textview_GetIM(self)); j > 0; j--)
	{
	    if ( (nlPos = text_GetEndOfLine(Text(self), npos) + 1) == dsize )
		break;
	    do
	    {
		npos = textview_MoveForward(self,npos, 1, textview_MoveByLines, NULL, &lines);
		nlines++;

	    } while ( npos < nlPos && npos < dsize );
	}
    }
    else {
	currentline = textview_FindLineNumber(self, pos);
	npos = textview_MoveBack(self,pos,0, textview_MoveByLines, NULL, NULL);
	npos = textview_MoveForward(self,npos, nlines = im_Argument(textview_GetIM(self)), textview_MoveByLines, NULL, NULL);
    }
    if (currentline != -1)  {
	/* Current Position is on the screen */
	
	newline = textview_FindLineNumber(self, npos);
	if (newline == -1)  {
	    linetomove = nlines - (self->nLines - currentline - 1);
	    
	    if (linetomove < self->nLines && linetomove > 0)  {
		if (self->scroll == textview_ScrollBackward) self->scroll = textview_MultipleScroll;
		textview_SetTopPosition(self, mark_GetPos(self->lines[linetomove].data));
	    }
	    else
		if ( self->editor == VI )
		{
		    (self->header.view.imPtr)->argState.argProvided = TRUE;
		    (self->header.view.imPtr)->argState.argument  = nlines;
		    textview_ViGlitchUpCmd(self);
		}
		else
		    textview_FrameDot(self, npos);
	}
    }
    else  {
	/* Current Position is off the screen */
	if (self->scroll == textview_ScrollForward)  {
	    linetomove += nlines;
	    if (linetomove < self->nLines)
		textview_SetTopPosition(self, mark_GetPos(self->lines[linetomove].data));
	    else
		textview_FrameDot(self, npos);
	}
	else  {
	    long topPos;

	    currentline = textview_FindLineNumber(self, startPos);
	    if (currentline != -1 || (startPos <= (topPos = textview_GetTopPosition(self)) && startPos >= topPos))
		textview_FrameDot(self, npos);
	}
    }

    mark_SetPos(&tm, npos);
    mark_SetLength(&tm, 0);
    if (self->csxPos != -1)  {
	struct formattinginfo info;

	npos = textview_LineRedraw (self, textview_GetPosition, &tm, self->bx,
	    self->by, textview_GetLogicalWidth(self)-2*self->bx,
	    textview_GetLogicalHeight(self)-2*self->by, xpos, (int *) 0, NULL, &info);
    }
    textview_SetDotPosition(self,npos);
    im_SetLastCmd(textview_GetIM(self), lcMove);
    textview_WantUpdate(self, self);
}

void textview_EndOfTextCmd(self)
register struct textview *self;
{
    register struct text *d;
    register int e;

    textview_CtrlAtCmd(self);
    d = Text(self);
    e = text_GetLength(d);
    textview_SetDotPosition(self,e);
    textview_SetDotLength(self,0);
    textview_FrameDot(self,e);
    textview_WantUpdate(self, self);
}

void textview_BeginningOfTextCmd(self)
register struct textview *self;
{
    textview_CtrlAtCmd(self);
    textview_SetDotPosition(self,0);
    textview_SetDotLength(self,0);
    textview_FrameDot(self,0);
    textview_WantUpdate(self, self);
}

void textview_EndOfLineCmd (self)
register struct textview *self;
{
    register int startpos, npos, dsize;
    register struct text *d;
    /* lie for now */

    d = Text(self);

    if ( self->editor == VI )
	npos = text_GetEndOfLine(d, textview_GetDotPosition(self));
    else {
	if ((dsize = textview_GetDotLength(self)) > 0)  {
	    textview_SetDotPosition(self, dsize+textview_GetDotPosition(self));
	    textview_SetDotLength(self, 0);
	    textview_WantUpdate(self, self);
	    return;
	}
	dsize = text_GetLength(d);
	npos = textview_MoveBack(self, startpos=textview_GetDotPosition(self), 0 , textview_MoveByLines, NULL, NULL);
	npos = textview_MoveForward(self, npos, 1, textview_MoveByLines, NULL, NULL);
	if (npos > 0)  {
	    if (npos == dsize)  {
		if (text_GetChar(d, npos-1) == '\n') npos--;
		if (npos < startpos) npos = startpos;
	    }
	    else if (text_GetChar(d, npos-1) == ' ' )
		while (npos > startpos && (text_GetChar(d, npos-1)) == ' ') npos--;
	    else if (text_GetChar(d, npos-1) == '\n') npos--;
	}
    }
    textview_SetDotPosition(self, npos);
    textview_SetDotLength(self, 0);
    textview_FrameDot(self, npos);
    textview_WantUpdate(self, self);
}

void textview_BeginningOfLineCmd(self)
register struct textview *self;
{
    register int pos;

    if ( self->editor == VI )
    {
	pos	= text_GetBeginningOfLine(Text(self), textview_GetDotPosition(self));
	textview_SetDotPosition(self, pos);
	textview_SetDotLength(self, 0);
    }
    else {
	if (textview_GetDotLength(self) != 0) {
	    textview_SetDotLength(self,0);
	    pos = textview_GetDotPosition(self);
	}
	else {
	    pos = textview_MoveBack(self, textview_GetDotPosition(self),0, textview_MoveByLines, 0, 0);
	    textview_SetDotPosition(self,pos);
	}
    }
    textview_FrameDot(self, pos);
    textview_WantUpdate(self, self);
}

void textview_EndOfWSWordCmd(self)
    register struct textview *self;
{/**/
    long	pos, textSize;
    register int	j, ct;
    /***/
    ct	= im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self));
    textSize = text_GetLength(Text(self));
    for (j = 0; j < ct; j++)
    {
	do
	{
	    textview_EndOfWordCmd(self);
	    pos = textview_GetDotPosition(self);
	} while ( pos < textSize && charType(text_GetChar(Text(self), pos)) != WHITESPACE );
    }
}

void textview_ForwardWSWordCmd(self)
    register struct textview *self;
{/**/
    long	pos, textSize;
    register int	j, ct;
    /***/
    ct	= im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self));
    textSize = text_GetLength(Text(self));
    for (j = 0; j < ct; j++)
    {
	do
	{
	    textview_ForwardWordCmd(self);
	    pos = textview_GetDotPosition(self);
	} while ( pos < textSize && charType(text_GetChar(Text(self), pos - 1)) != WHITESPACE );
    }
}

void textview_BackwardWSWordCmd(self)
    register struct textview *self;
{/**/
    long		pos;
    register int	j, ct;
    /***/
    ct	= im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self));
    for (j = 0; j < ct; j++)
    {
	do
	{
	    textview_BackwardWordCmd(self);
	    pos = textview_GetDotPosition(self);
	} while ( pos > 0 && charType(text_GetChar(Text(self), pos - 1)) != WHITESPACE );
    }
}

void textview_BeginningOfFirstWordCmd(self)
    struct textview *self;
{
    register struct text *d;
    char	c;

    im_ClearArg(textview_GetIM(self)); 
    d=Text(self);

    textview_BeginningOfLineCmd(self);
    c = text_GetChar(d, textview_GetDotPosition(self));
    if ( charType(c) == WHITESPACE && c != '\n' )
	textview_ForwardWordCmd(self);
}

void textview_BeginningOfPreviousLineCmd(self)
    struct textview *self;
{
    int	ct;
    register int    j;

    ct = im_Argument(textview_GetIM(self));
    im_ClearArg(textview_GetIM(self)); 

    for (j = 0; j < ct; j++)
    {
    textview_PreviousLineCmd(self);
    textview_BeginningOfFirstWordCmd(self);
    }
}

void textview_BeginningOfNextLineCmd(self)
    struct textview *self;
{
    int	ct;
    register int    j;

    ct = im_Argument(self->header.view.imPtr);
    im_ClearArg(self->header.view.imPtr); 

    for (j = 0; j < ct; j++)
    {
    textview_NextLineCmd(self);
    textview_BeginningOfFirstWordCmd(self);
    }
}

void textview_CursorToLine(self, line)
struct textview *self;
long line;
{
    if (line > 0 && line <= self->nLines) {
        textview_SetDotPosition(self, mark_GetPos(self->lines[line - 1].data));
        textview_SetDotLength(self, 0);
        textview_WantUpdate(self, self);
    }
}

/* Useful commands for our editor. */

void textview_CursorToTop(self, key)
struct textview *self;
long key;
{
    textview_CursorToLine(self, 1);
}

void textview_CursorToCenter(self, key)
struct textview *self;
long key;
{
    textview_CursorToLine(self, self->nLines / 2);
}

void textview_CursorToBottom(self, key)
struct textview *self;
long key;
{
    textview_CursorToLine(self, self->nLines);
}

void textview_GoToLineCmd(self)
    register struct textview *self;
{

    int argument, pos;
    register struct text *text;

    argument = im_Argument(textview_GetIM(self));
    if ( !im_ArgProvided(textview_GetIM(self)) )
	textview_EndOfTextCmd(self);
    else
    {
	text = (struct text *) self->header.view.dataobject;
	textview_SetDotLength(self, 0);
	pos = text_GetPosForLine(text, argument);
	textview_SetDotPosition(self, pos);
	textview_FrameDot(self, pos);
	textview_WantUpdate(self, self);
    }
}


