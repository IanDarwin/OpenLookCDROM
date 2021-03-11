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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/lookz/RCS/tabrulv.c,v 1.8 1993/05/05 19:49:43 susan Exp $";
#endif


#include <class.h>
#include <graphic.ih>
#include <view.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <tabs.ih>
#define class_StaticEntriesOnly
#include <style.ih>
#undef  class_StaticEntriesOnly
#include <im.ih>
#include <rect.h>

#include <lprruler.ih>

#include <tabrulv.eh>

#define	IconString	"/"
#define	ClearString	"AllClear"
#define	CancelString	"Cancel"

static void RemoveIcon(), RepaintIcon(), RedrawPark(), RedrawIcons(), RedrawCommands(), RedrawText(), RedrawRuler();

#define ICONHEIGHT 14	/*C*//* height of area where icons move */
#define PARKWIDTH  19	/*C*//* width of icon parking area */
#define tabrulerHEIGHT 22 /*C*//* bottom line = topline + tabrulerHEIGHT */

static struct fontdesc *TextFont, *IconFont;


struct TickTbl {
    short majorpix, minorpix;	/* number of pixels for major and minor cycles */
    short one;			/* increment for -zero- value */
    short ht[10];		/* height of each tick 
				 cyclecnt==0 is the first tick 
				 and is where numeric value is displayed */
    char fmt[10];		/* format for text area */
    char unitstring[10];	/* units name to be displayed */
};

static struct TickTbl InchTbl = {72, 9, 1, {6, 3, 5, 3, 7, 3, 5, 3}, "%+5.2f i", "In."};
static struct TickTbl PointTbl = {36, 6, 36, {6, 3, 3, 5, 3, 3}, "%+3.0f p", "Pts."};
static struct TickTbl CMTbl = {30, 6, 1, {6, 3, 3, 3, 3}, "%+5.2f c", "Cm."};


static boolean
BogusCallFromParent(self, where, msg)
register struct tabrulerview *self;
char *where, *msg;
{
    fprintf(stderr, "<tabrulerview>Bogus call to %s, %s\n", where, msg);
    return FALSE;
}

static boolean
CheckWindow(self, where)
register struct tabrulerview *self;
char *where;
{
    register struct graphic *g
      = (struct graphic *)tabrulerview_GetDrawable(self);
    if ( ! g) return BogusCallFromParent(self, where, "No Graphic");
    return TRUE;
}



boolean
tabrulerview__InitializeClass(ClassID)
struct classhdr *ClassID;
{
    TextFont = fontdesc_Create("andysans", fontdesc_Bold, 12);
    IconFont = fontdesc_Create("icon", fontdesc_Plain, 12);
    return TRUE;
}

boolean
tabrulerview__InitializeObject(ClassID, self)
struct classhdr *ClassID;
register struct tabrulerview  *self;
{
    self->OnScreen = FALSE;
    self->tabrulerchanged = self->iconschanged = self->textchanged = TRUE;
    self->unit = style_Inches;
    self->TickTbl = &InchTbl;
    self->ValueChangeProc = NULL;
    self->tabs = NULL;
    self->Moving = FALSE;
    self->mul = environ_GetProfileInt("TabScalingMultiplier", 14);
    self->div = environ_GetProfileInt("TabScalingDivisor", 12);
    return TRUE;
}

void
tabrulerview__FinalizeObject(ClassID, self)
struct classhdr *ClassID;
register struct tabrulerview  *self;
{
}

void
tabrulerview__ObservedChanged(self, dobj, status)
register struct tabrulerview  *self;
struct tabruler *dobj;
long status;
{
    if (status == lprruler_DATACHANGED) 
	self->iconschanged = self->textchanged = TRUE;
    else if (status == observable_OBJECTDESTROYED)
	return;

    tabrulerview_WantUpdate(self, self);
}

static void
MoveIcon(self, newx)
struct tabrulerview *self;
register long newx;
{
    RemoveIcon(self, self->Movex);
    self->Movex = newx;
    RepaintIcon(self, newx, graphic_COPY);
}
      

static void
RepaintIcon(self, position, color)
register struct tabrulerview  *self;
register long position;
short color;
{
    tabrulerview_SetTransferMode(self, color);
    tabrulerview_MoveTo(self, position*self->mul/self->div, self->icony);
    tabrulerview_DrawText(self, IconString, 1, graphic_NOMOVEMENT);
}

static void
RemoveIcon(self, pos) 
register struct tabrulerview *self;
register long pos;
{
    register long dx;
    register int i;

    RepaintIcon(self, pos, graphic_WHITE);
    for (i = 0; i < self->tabs->number; i++) {
	dx = self->tabs->Positions[i] - pos;
	if (dx < 10 && dx > -10) /*C*/
	    RepaintIcon(self, self->tabs->Positions[i], graphic_COPY);
    }
}


/* Draw tick marks in a tabruler for -self- on the topline, from -left- to -right-
    the value zero would be plotted at location zeroloc, which may be off
    the tabruler to either end
    the length of tick marks is given by -tbl-, with one entry for
	each minor cycle within a major cycle.
	The lengths of the cycles are given by -major- and -minor-
	assume major % minor == 0 */
static void
DoTicks(self, zeroloc, left, right, tbl)
register struct tabrulerview *self;
short zeroloc, right;
register short left;
register struct TickTbl *tbl;
{
    short cycmax = tbl->majorpix / tbl->minorpix;	/* number of minor cycles in a major */
    register short tickloc;					/* where to place next tick */
    register short cyclecnt;				/* count minor ticks within major cycle */
    short ordloc;						/* where to first plot an ordinate value */
    short ordval;						/* next ordinate value to plot */
    register short x;

    /* since division and remainder are ill-defined for negative operands:
	the "<<12" items arrange that the %'s are done on positive operands
the divisions always come out to exact values 
*/
    tickloc = left + (zeroloc + (tbl->minorpix << 12) - left) % tbl->minorpix;
    ordloc = left + (zeroloc + (tbl->majorpix << 12) - left) % tbl->majorpix;
    ordval = (ordloc - zeroloc) / tbl->majorpix * tbl->one;
    cyclecnt = cycmax - (ordloc-tickloc)/tbl->minorpix;
    if (cyclecnt == cycmax) cyclecnt = 0;

    tabrulerview_SetFont(self, TextFont);
    for ( ; tickloc < right; tickloc += tbl->minorpix) {
	x = tickloc * self->mul / self->div;
	tabrulerview_MoveTo(self, x, self->topline);
	tabrulerview_DrawLineTo(self, x, self->topline + tbl->ht[cyclecnt]);
	if (cyclecnt == 0) {
	    /* draw digit */
	    char buf[10];
	    if (ordval > 0)
		sprintf(buf, "%+d", ordval);
	    else sprintf(buf, "%d", ordval);	/* no sign if ordval==0 */
	    tabrulerview_MoveTo(self, x-1, self->topline + 7);	/*C*/
	    tabrulerview_DrawString(self, buf,
				    graphic_BETWEENLEFTANDRIGHT | graphic_ATTOP);
	    ordval += tbl->one;
	}
	if (++cyclecnt >= cycmax) cyclecnt = 0;
    }
}

static void
RecomputeAndRedraw(self)
register struct tabrulerview *self;
{
    struct rectangle r;

    tabrulerview_GetLogicalBounds(self, &r);

    self->leftline = 0;
    self->rightline = r.width;
    /* Assuming icons are square... */
    self->textloc = self->leftline + ICONHEIGHT*4 + 4;   /* with icon for unit change to its left */
    self->cleartxt = (self->rightline - self->leftline) / 2 + self->leftline;
    self->canceltxt = self->rightline - 4;

    self->topline = 4 + ICONHEIGHT;
    self->bottomline = self->topline + tabrulerHEIGHT;
    self->icony = self->topline - 2;
    self->leftzero = self->leftline;

    tabrulerview_SetTransferMode(self, graphic_COPY);
    tabrulerview_FillRect(self, &r, self->WhitePattern);

    RedrawRuler(self);
    RedrawPark(self);
    RedrawIcons(self);
    RedrawText(self, 0.0, FALSE);
    RedrawCommands(self);

    self->tabrulerchanged = FALSE;
}

static void
RedrawRuler(self)
struct tabrulerview *self;
{
    struct rectangle r;
    r.left   = self->leftline;
    r.top    = self->topline;
    r.width  = self->rightline - self->leftline;
    r.height = self->bottomline - self->topline;

    /* Clean the area */
    tabrulerview_SetTransferMode(self, graphic_COPY);
    tabrulerview_FillRect(self, &r, self->WhitePattern);

    /* draw outline */
    tabrulerview_MoveTo(self, self->leftline, self->topline);
    tabrulerview_DrawLineTo(self, self->rightline, self->topline);
    tabrulerview_DrawLineTo(self, self->rightline, self->bottomline);
    tabrulerview_DrawLineTo(self, self->leftline, self->bottomline);
    tabrulerview_DrawLineTo(self, self->leftline, self->topline);

    DoTicks(self, self->leftline, self->leftline,
	     self->rightline, self->TickTbl);

}

static void
RedrawText(self, number, flag)
register struct tabrulerview *self;
float number;
boolean flag;
{
    struct rectangle r;
    char buf[10];
    long buttony;

    r.top = self->bottomline + 2, r.left = self->textloc;
    r.height = ICONHEIGHT + 4/*C*/;
    r.width = self->clearpos - self->textloc - ICONHEIGHT;
    buttony = self->bottomline + (ICONHEIGHT>>1) + 4;
    tabrulerview_SetTransferMode(self, graphic_COPY);
    tabrulerview_FillRect(self, &r, self->WhitePattern);
    tabrulerview_SetFont(self, TextFont);
    tabrulerview_MoveTo(self, self->textloc, self->bottomline + 2);
    if (flag)
	/* We want to display a number here */
	sprintf(buf, self->TickTbl->fmt, number);
    else
	strcpy(buf, self->TickTbl->unitstring);

    tabrulerview_DrawString(self, buf, graphic_ATLEFT |  graphic_ATTOP);

    /* display button icons */
    tabrulerview_SetFont(self, IconFont);
    tabrulerview_MoveTo(self, self->textloc-ICONHEIGHT, buttony);
    tabrulerview_DrawText(self, "\'", 1, graphic_NOMOVEMENT);
    
    self->textchanged = FALSE;
}    

static void
RedrawCommands(self)
register struct tabrulerview *self;
{
    long x, y, buttony;
    struct rectangle r;

    /*
      fontdesc_StringSize(TextFont, (struct graphic *) self, ClearString, &x,&y);
      */
    x = 60; /* x = Width of ClearString */
    self->clearpos = self->cleartxt - (x>>1) - ICONHEIGHT;
    /*
      fontdesc_StringSize(TextFont, (struct graphic *) self, CancelString, &x,&y);
      */
    x = 50; /* x = Width of CancelString */
    self->cancelpos = self->canceltxt - x - ICONHEIGHT;

    r.top = self->bottomline + 2;
    r.left = self->clearpos - ICONHEIGHT;
    r.height = ICONHEIGHT + 4/*C*/;
    r.width = self->rightline - r.left;
    buttony = self->bottomline + (ICONHEIGHT>>1) + 4;

    tabrulerview_SetTransferMode(self, graphic_COPY);
    tabrulerview_FillRect(self, &r, self->WhitePattern);
    tabrulerview_SetFont(self, TextFont);

    /* The text for AllClear and Cancel */
    tabrulerview_MoveTo(self, self->cleartxt, self->bottomline + 2);
    tabrulerview_DrawString(self, ClearString, graphic_BETWEENLEFTANDRIGHT | graphic_ATTOP);

    tabrulerview_MoveTo(self, self->canceltxt, self->bottomline + 2);
    tabrulerview_DrawString(self, CancelString, graphic_ATRIGHT | graphic_ATTOP);

    /* display button icons */
    tabrulerview_SetFont(self, IconFont);
    tabrulerview_MoveTo(self, self->cancelpos, buttony);
    tabrulerview_DrawText(self, "\'", 1, graphic_NOMOVEMENT);
    tabrulerview_MoveTo(self, self->clearpos, buttony);
    tabrulerview_DrawText(self, "\'", 1, graphic_NOMOVEMENT);
}

static void
RedrawPark(self)
register struct tabrulerview *self;
{
    struct rectangle r;

    /* Redraw the parking lot... */
    r. height = ICONHEIGHT+4, r.width = PARKWIDTH;	/*C*/
    r.top = self->bottomline + 2;		/*C*/
    r.left = self->leftline + 2;
    tabrulerview_SetTransferMode(self, graphic_COPY);
    tabrulerview_FillRect(self, &r, self->Grey25Pattern);

    /* If we are moving the icon, then we leave the park empty */
    if (!self->Moving) {
	tabrulerview_MoveTo(self, self->leftline+PARKWIDTH/2 + 2,
			    self->bottomline+ICONHEIGHT + 4); /*C*/
	tabrulerview_DrawText(self, IconString, 1, graphic_NOMOVEMENT);
    }
}

static void
RedrawIcons(self)
register struct tabrulerview  *self;
{
    register int i;
    struct rectangle r;

    /* Clear the tabstops */
    r.top = self->topline - ICONHEIGHT - 4/*C*/, r.left = 0;
    r.height = ICONHEIGHT + 3/*C*/, r.width = self->rightline;	
    tabrulerview_SetTransferMode(self, graphic_COPY);
    tabrulerview_FillRect(self, &r, self->WhitePattern);
    tabrulerview_SetFont(self, IconFont);

    if (self->tabs)
	for (i = 0; i < self->tabs->number; i++)
	    RepaintIcon(self, self->tabs->Positions[i] +
			self->leftline, graphic_COPY);

    /* The moving icon... */
    if (self->Moving)
	RepaintIcon(self, self->Movex, graphic_COPY);

    RedrawPark(self);

    self->iconschanged = FALSE;
}

void 
tabrulerview__FullUpdate( self, type, left, top, width, height )
register struct tabrulerview  *self;
register enum view_UpdateType  type;
register long  left, top, width, height;
{
    if (type == view_Remove) {
	self->OnScreen = FALSE;
	return;
    }
    if ( ! CheckWindow(self, "FullUpdate")) return;
    if ((type != view_FullRedraw 
	  && type != view_LastPartialRedraw)
	 || tabrulerview_GetLogicalWidth(self) == 0 
	 || tabrulerview_GetLogicalHeight(self) == 0) 
	return;
    self->OnScreen = TRUE;
    if (type == view_FullRedraw) {
	/* must recompute graphics info because image
	 may be on different display hardware */
	self->Grey25Pattern = tabrulerview_GrayPattern(self, 4, 16);
	self->WhitePattern = tabrulerview_WhitePattern(self);
    }
    self->tabrulerchanged = self->textchanged = self->iconschanged = TRUE;
    RecomputeAndRedraw(self);
}


void 
tabrulerview__Update( self )
register struct tabrulerview *self;
{
    if (! self->OnScreen || ! CheckWindow(self, "Update")) return;
    if (self->tabrulerchanged) RedrawRuler(self);
    if (self->textchanged) RedrawText(self, 0.0, FALSE);
    if (self->iconschanged) RedrawIcons(self); 
}

struct view *
tabrulerview__Hit(self, action, x, y, num_clicks)
register struct tabrulerview  *self;
register enum view_MouseAction  action;
register long  x, y, num_clicks;
{
    if (action == view_NoMouseEvent)
	return (struct view *) self;
    if (!self->OnScreen || !CheckWindow(self, "Hit"))
	return NULL;

    if (action == view_LeftDown || action == view_RightDown) {
	if (self->Moving) { 
	    /* Cancel */
	    RemoveIcon(self, self->Movex);
	    self->Moving = FALSE;
	    RedrawPark(self);
	} else {
	    short dx, dy;
	    dx = x - (self->textloc - ICONHEIGHT);
	    dy = y - (self->bottomline + ICONHEIGHT/2 + 4);
	    if (dy < 10 && dy > -10) {
		/* Vertically, we're in the right place */
		if (dx < 10 && dx > -10) {
		    /* change units button */
		    switch(self->unit) {
			case style_CM:
			    self->TickTbl = &PointTbl;
			    self->unit = style_Points; 
			    break;
			case style_Points: 
			    self->TickTbl = &InchTbl;
			    self->unit = style_Inches; 
			    break;
			default: 
			    self->TickTbl = &CMTbl;
			    self->unit = style_CM; 
			    break;
		    }
		    RedrawRuler(self);
		    RedrawText(self, 0.0, FALSE);
		} else if (self->tabs) {
		    if ((dx = x - self->cancelpos) < 10 && dx >-10) {
			/* The cancel button */
			self->tabs = tabs_Create();
			RedrawIcons(self);
			RedrawText(self, 0.0, FALSE);
			if (self->ValueChangeProc)
			    /* notify client of new value */
			    /* cannot have -ve tabstops, so we use it
			     * to indicate cancel code
			     */
			    (self->ValueChangeProc)(self, 
						    self->ValueChangeRock, 
						    -1L, style_AllClear,
						    style_Points);
		    } else if ((dx = x - self->clearpos) < 10 && dx > -10) {
			/* The AllClear button */
			self->tabs = tabs_Clear(self->tabs);
			RedrawIcons(self);
			RedrawText(self, 0.0, FALSE);
			if (self->ValueChangeProc)
			    /* notify client of new value */
			    (self->ValueChangeProc)(self, 
						    self->ValueChangeRock, 
						    0L, style_AllClear,
						    style_Points);
		    } else if (x < PARKWIDTH) {
			/* We are in the park! - create a new tab in process */
			self->Moving = TRUE;
			self->Movex = x;
			self->oldtab = -1; /* Cannot have -ve tabs */
			RepaintIcon(self, x, graphic_COPY);
			RedrawPark(self);
		    }
		}
	    } else if (y < self->topline) {
		/* Playing with existing tabstops */
		int i = FindClosestTab(self, (x - self->leftzero)*self->div / self->mul);
		if (i >= 0) {
		    /* Only do something if we are close enough to a tab */
		    float f = ((float) self->tabs->Positions[i]) /
		      self->TickTbl->majorpix *
		      self->TickTbl->one;
		    if (action == view_LeftDown) {
			/* Hit on a tab - let's move it... */
			self->Moving = TRUE;
			self->Movex = self->tabs->Positions[i];
			/* remove it from its old position */
			self->tabs = tabs_Delete(self->tabs, i);
			self->oldtab = self->Movex;
			RedrawText(self, f, TRUE);
		    } else if (action == view_RightDown) {
			/* we want to remove a tab */
			long x = self->tabs->Positions[i];
			self->tabs = tabs_Delete(self->tabs, i);
			RemoveIcon(self, x);
			RedrawText(self, 0.0, FALSE);
			if (self->ValueChangeProc)
			    /* notify client of new value */
			    (self->ValueChangeProc)(self, 
						    self->ValueChangeRock, 
						    x, style_TabClear,
						    style_Points);
		    }
		}
	    }
	}
    } else {
	/* Either move or up event */
	if (action == view_LeftUp && self->Moving) {
	    float f;
	    /* Plant a tab here */
	    int i;
	    x = (x - self->leftline) * self->div / self->mul;
	    i = FindClosestTab(self, x);
	    self->Moving = FALSE;
	    if (i == -1) {
		f = ((float) (x - self->leftline)) /
		  self->TickTbl->majorpix *
		  self->TickTbl->one;
		if (self->oldtab == x) {
		    /* Seems user has moved an old tab nowhere */
		    MoveIcon(self, x);
		    self->tabs=tabs_Add(self->tabs, x, style_LeftAligned /*C*/);
		} else {
		    /* We have just created a new tab */
		    /* ONLY LEFTALIGNED TABS ALLOWED!!! XXX */
		    MoveIcon(self, x);
		    if (self->ValueChangeProc) {
			/* notify client of new value */
			(self->ValueChangeProc)(self, self->ValueChangeRock, x,
						style_LeftAligned, /*C*/
						style_Points);
			if (self->oldtab >= 0)
			    (self->ValueChangeProc)(self, self->ValueChangeRock, 
						    self->oldtab, style_TabClear,
						    style_Points);
		    }
		    self->tabs = tabs_Add(self->tabs, x, style_LeftAligned /*C*/);
		}
	    } else {
		f = ((float) (self->tabs->Positions[i] - self->leftline)) /
		  self->TickTbl->majorpix *
		  self->TickTbl->one;
		RemoveIcon(self, x);
		/* If we were moving a tab around, then there's more work */
		if (self->oldtab >= 0)
		    if (self->ValueChangeProc)
			(self->ValueChangeProc)(self, self->ValueChangeRock, 
					self->oldtab,style_TabClear,
					style_Points);
	    }
	    RedrawPark(self);
	    RedrawText(self, f, TRUE);
	} else if (self->Moving &&
		   (action == view_LeftMovement ||
		    action == view_RightMovement)) {
	    /* We are still moving the new tab to its position */
	    float f;
	    if (x - self->leftline >= 0) {
		x = (x - self->leftline) *self->div / self->mul;
		f = ((float) (x - self->leftline)) /
		  self->TickTbl->majorpix *
		  self->TickTbl->one;
		MoveIcon(self, x);
		RedrawText(self, f, TRUE);
	    }
	}
    }
    if (action == view_LeftDown || action == view_RightDown)
	tabrulerview_WantInputFocus(self, self);
    return (struct view *) self;
}

enum view_DSattributes
tabrulerview__DesiredSize( self, width, height, pass, 
			   desiredWidth, desiredHeight ) 
register struct tabrulerview *self;
long width;
long height;
enum view_DSpass pass;
long *desiredWidth;
long *desiredHeight;
{
    *desiredHeight = 80;
    *desiredWidth = 700;
    return view_Fixed;
}

void
tabrulerview__SetValues(self, tabs)
register struct tabrulerview *self;
struct tabs *tabs;
{
    self->tabs = tabs;
    self->iconschanged = self->textchanged = TRUE;
    tabrulerview_WantUpdate(self, self);
}

void
tabrulerview__GetValues(self, tabs)
register struct tabrulerview *self;
struct tabs **tabs;
{
    *tabs = self->tabs;
}

int
FindClosestTab(self, pos)
register struct tabrulerview *self;
register long pos;
{
    int i;
    long dx;

    if (self->tabs)
	for (i = 0; i < self->tabs->number; i++) {
	    dx = pos - self->tabs->Positions[i];
	    if (dx < 0)
		dx = -dx;
	    if (dx < 6) /* Cannot have tabs closer than 6 pts apart */
		return i;
	}
    return -1;
}   
