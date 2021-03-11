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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/console/lib/RCS/draw.c,v 2.8 1992/12/15 21:30:21 rr2b R6tape $";
#endif


 

#include <class.h>
#include <im.ih>
#include <conclass.ih>
#include <view.ih>
#include <fontdesc.ih>
#include <cursor.ih>
#include <graphic.ih>
#include <rect.h>
#include <math.h>
#include <mktrig.h>
#include <console.h>
#include <sys/param.h>

extern InvertWindow();
extern PromptToWindow();
extern InitPstrings();
extern int Pposx, Pposy;
extern char Pstring1[256], Pstring2[256], Pstring3[256], Pstring4[MAXPATHLEN];


DrawDebug(self,Op, disp)
struct consoleClass *self;
int Op;
struct display *disp;
{
    mydbg(("entering: DrawDebug\n"));
    if (Op == NEWVAL) {
        mydbg(("NEWVAL %d on display %s with text %s\n", disp->WhatToDisplay->Value, disp->label, disp->WhatToDisplay->RawText));
    }
    if (Op == REDRAW) {
        mydbg(("REDRAW on display %s\n", disp->label));
    }
}

RingAlarm(self, Op, indexed)
struct consoleClass *self;
int Op, indexed;
{
    static char *BufPtr;

    mydbg(("entering: RingAlarm\n"));
    if (Op == NEWVAL) {
        PauseEnqueuedEvents = TRUE;
        if (RingingAlarm == FALSE){
            RingingAlarm = TRUE;
	    BufPtr = Numbers[ALARM].RawText;
            while(*BufPtr != ':') *BufPtr++;/* Alarm: */
            *BufPtr++;
            while(*BufPtr != ':') *BufPtr++;/* 12:00 */
            *BufPtr++;
            while(*BufPtr != ':') *BufPtr++;/* AM: */
	    *BufPtr++;
	    InitPstrings();
	    sprintf(Pstring1, "%s", Numbers[CLOCKALL].RawText);
	    sprintf(Pstring3, "%s", BufPtr);
	    PromptToWindow(self);
	}
	InvertWindow(self);
        PauseEnqueuedEvents = FALSE;
    }
}

DrawGauge(self, Op, disp)
struct consoleClass *self;
int Op;
struct display *disp;
{
    int xo, yo, xc, yc, xoff, yoff, diam,
        DialPosition,
	yb;
    struct rectangle clpRect;

    mydbg(("entering: DrawGauge\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
	rectangle_SetRectSize(&clpRect, disp->Xmin, disp->Ymin, disp->Width, disp->FullHeight);
	consoleClass_SetClippingRect(self, &clpRect);
	
        xc = (disp->Xmax - disp->Xmin) / 2;
        yc = (disp->Ymax - disp->Ymin) / 2;
        diam = (xc > (yc * 2))? ((yc * 4) - 3): ((xc * 2) - 3);
	if (diam < 1) return(0); 
	xc = xc + disp->Xmin;
	yc = yc + disp->Ymin;
	yb = disp->Ymax;
	yoff = SineMult[160] * diam / 2 / 10000;
	xoff = CosineMult[160] * diam / 2 / 10000;
	xo = xc - diam / 2;
	yo = yb - diam / 2;

        DialPosition = 150 - (120 * disp->WhatToDisplay->Value) / disp->ValueMax;
        if (DialPosition < 30)
            DialPosition = 30;

	if (Op == REDRAW) {
	    consoleClass_SetTransferMode(self, graphic_BLACK);
	    consoleClass_DrawArcSize(self,xo, yo, diam, diam, 290, 140);
	    consoleClass_MoveTo(self, xc, yb);
	    consoleClass_DrawLineTo(self, xc - xoff, yb - yoff);
            consoleClass_MoveTo(self, xc, yb);
            consoleClass_DrawLineTo(self, xc + xoff, yb - yoff);
        }
        if (Op == NEWVAL) {
            consoleClass_SetTransferMode(self, graphic_WHITE);
            consoleClass_MoveTo(self, xc, yb);
            consoleClass_DrawLineTo(self, xc + CosineMult[disp->displayparam1] * diam / 2  / 11000, yb - SineMult[disp->displayparam1] * diam / 2 / 11000);
            consoleClass_SetTransferMode(self, graphic_BLACK);
            if (disp->displayparam1 > 90 || disp->displayparam1 < 10) {
                consoleClass_MoveTo(self, xc, yb);
                consoleClass_DrawLineTo(self, xc - xoff, yb - yoff);
                consoleClass_MoveTo(self, xc, yb);
                consoleClass_DrawLineTo(self, xc + xoff, yb - yoff);
            }
        }
        if (Op == REDRAW || Op == NEWVAL) {
            consoleClass_MoveTo(self, xc, yb);
            consoleClass_DrawLineTo(self, xc + CosineMult[DialPosition] * diam / 2  / 11000, yb - SineMult[DialPosition] * diam / 2 / 11000);
            disp->displayparam1 = DialPosition;
        }
    }
}

SetStandardCursor(self, cursor)
struct consoleClass *self;
short cursor;
{
    static struct cursor *cp;

    mydbg(("entering: SetStandardCursor\n"));
    if (cp == NULL) cp  = cursor_Create(self);
    cursor_SetStandard(cp, cursor);
    im_SetWindowCursor(view_GetIM((struct view *)self), cp);
}

SignalTrouble(self, Op, disp)
struct consoleClass *self;
int Op;
struct display *disp;
{
    mydbg(("entering: SignalTrouble\n"));
    if (Op == REDRAW || Op == NEWVAL) {
        disp->Trouble = TRUE;
    }
}


DrawTitle(self,Op, disp)
struct consoleClass *self;
int Op;
struct display *disp;
{
    char    TextDum[50];

    mydbg(("entering: DrawTitle\n"));
    maketext(self, TextDum, disp, 0);
    im_SetTitle(view_GetIM((struct view *) self), TextDum);
}

draw_corners(self, x1,y1,x2,y2,inc)
struct consoleClass *self;
int x1,y1,x2,y2,inc;
{
    mydbg(("entering: draw_corners\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
        consoleClass_MoveTo(self, x1, y1);
        consoleClass_DrawLineTo(self, x1 + inc, y1 + inc);
        consoleClass_MoveTo(self, x1, y2);
        consoleClass_DrawLineTo(self, x1 + inc, y2 - inc);
        consoleClass_MoveTo(self, x2, y2);
        consoleClass_DrawLineTo(self, x2 - inc, y2 - inc);
        consoleClass_MoveTo(self, x2, y1);
        consoleClass_DrawLineTo(self, x2 - inc, y1 + inc);
    }
}



DrawTics1(self, xc,yc,x1,y1,x2,y2)
struct consoleClass *self;
int xc,yc,x1,y1,x2,y2;
{
    mydbg(("entering: DrawTics1\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
        consoleClass_MoveTo(self,(xc + x1),(yc + y1));
        consoleClass_DrawLineTo(self,(xc + x2),(yc + y2));
        consoleClass_MoveTo(self,(xc + x1),(yc - y1));
        consoleClass_DrawLineTo(self,(xc + x2),(yc - y2));
        consoleClass_MoveTo(self,(xc - x1),(yc - y1));
        consoleClass_DrawLineTo(self,(xc - x2),(yc - y2));
        consoleClass_MoveTo(self,(xc - x1),(yc + y1));
        consoleClass_DrawLineTo(self,(xc - x2),(yc + y2));
    }
}


DrawTics0(self, xc,yc,x1,y1,x2,y2)
struct consoleClass *self;
int xc,yc,x1,y1,x2,y2;
{
    mydbg(("entering: DrawTics0\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
        consoleClass_MoveTo(self,(xc + x1),(yc));
        consoleClass_DrawLineTo(self,(xc + x2),(yc));
        consoleClass_MoveTo(self,(xc),(yc - y1));
        consoleClass_DrawLineTo(self,(xc),(yc - y2));
        consoleClass_MoveTo(self,(xc),(yc + y1));
        consoleClass_DrawLineTo(self,(xc),(yc + y2));
        consoleClass_MoveTo(self,(xc - x1),(yc));
        consoleClass_DrawLineTo(self,(xc - x2),(yc));
    }
}


SetupTics(self, xc, yc, r1, r2, r3, r4, IsRound, MaxValue)
struct consoleClass *self;
int xc, yc, r1, r2, r3, r4, MaxValue;
boolean IsRound;
{
    int     x1, y1, x2, y2, Limit, FewTics, theta;
    double    Theta, LotsOfTics, inc;

    mydbg(("entering: SetupTics\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
	if ((MaxValue % 4) != 0){
	    arrgh(("Console: Don't know how to divide dial for MaxValue: %d -- I'll guess.\n", MaxValue));
	}
	if (MaxValue < 61){
	    FewTics = 30;
	}
	else{
	    FewTics = 18;
	}
	Limit = (int) ((2 * MaxValue) / 3);
	LotsOfTics = 360.0 / MaxValue;
	inc = (r1 < Limit) ? (double)FewTics : LotsOfTics;
	for (Theta = 0.0; Theta <= 45.0; Theta += inc) {
	    theta = (int) Theta;
	    if ((theta % FewTics) == 0) {
		if (theta == 0) {
		    x1 = y1 = r4;
		    x2 = y2 = r2;
		}
		else {
		    if (IsRound){
			x1 = (int)(r4 * CosineMult[theta] / 10000);
			x2 = (int)(r2 * CosineMult[theta] / 10000);
			y1 = (int)(r4 * SineMult[theta] / 10000);
			y2 = (int)(r2 * SineMult[theta] / 10000);
		    }
		    else{
			x1 = r4;
			x2 = r2;
			y1 = (int)(((double) SineMult[theta] / (double) CosineMult[theta]) * x1);
			y2 = (int)(((double) SineMult[theta] / (double) CosineMult[theta]) * x2);
		    }
		}
	    }
	    else {
		if (IsRound){
		    x1 = (int)(r3 * CosineMult[theta] / 10000);
		    x2 = (int)(r2 * CosineMult[theta] / 10000);
		    y1 = (int)(r3 * SineMult[theta] / 10000);
		    y2 = (int)(r2 * SineMult[theta] / 10000);
		}
		else{
		    x1 = r3;
		    x2 = r2;
		    y1 = (int)(((double) SineMult[theta] / (double) CosineMult[theta]) * x1);
		    y2 = (int)(((double) SineMult[theta] / (double) CosineMult[theta]) * x2);
		}
	    }
	    if (theta == 0) {
		DrawTics0(self, xc, yc, x1, y1, x2, y2);
	    }
	    else {
		DrawTics1(self, xc, yc, x1, y1, x2, y2);
		DrawTics1(self, xc, yc, y1, x1, y2, x2);
	    }
	}
    }
}




draw_dial(self, xc, yc, r1, IsRound, MaxValue)
struct consoleClass *self;
int xc, yc, r1, MaxValue;
boolean IsRound;
{
    int     r2, r3, r4, corner;

    mydbg(("entering: draw_dial\n"));
    if (!PauseEnqueuedEvents && !RingingAlarm){
        r2 = (int)(r1 * 0.91);
        r3 = (int)(r2 * 0.95);
	r4 = (int)(r2 * 0.85);
	if (r1 < (int)(2 * MaxValue / 3)){
	    r2 = r1+2;
	}
	if (IsRound){
	    int d = r1 * 2;
	    consoleClass_DrawOvalSize(self, xc - r1, yc - r1, d, d);
	    d = r2 * 2;
	    consoleClass_DrawOvalSize(self, xc - r2, yc - r2, d, d);
	}
	else{
	    consoleClass_DrawRectSize(self, xc - r1, yc - r1, r1 * 2, r1 * 2);
	    consoleClass_DrawRectSize(self, xc - r2, yc - r2, r2 * 2, r2 * 2);
	    corner = (r1 - r2);
	    draw_corners(self, xc - r1, yc - r1, xc + r1, yc + r1, corner);
	}
	SetupTics(self, xc, yc, r1, r2, r3, r4, IsRound, MaxValue);
    }
}


