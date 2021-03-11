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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/lprrulv.c,v 2.15 1993/05/05 19:49:43 susan Exp $";
#endif


 

/* lprrulv.c	

	The view module for the lprruler dataobject

deferred features:
	adjust the location of zero depending on the values
	click ruler to set margin to that position
	click icon to just display its value
	click park to select icon and display its value
	keystrokes for fine adjustment of the most recently moved icon
	the text of the size number should be editable as simpletext
	the conversion between CM and PTS is unstable so the values get smaller
	should an icon "jump" when button is pressed down?
		We can avoid this jump by computing an offset from the x 
		and using that offset while the button is down 

*/

#include <class.h>
#include <graphic.ih>
#include <view.ih>
#include <fontdesc.ih>

#define class_StaticEntriesOnly
#include <style.ih>
#undef  class_StaticEntriesOnly


/*
#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
*/

#include <im.ih>
#include <rect.h>

#include <lprruler.ih>
#include <lprrulv.eh>

/*
static struct menulist  *class_menulist;
static struct keymap  *class_keymap;
static struct keystate  *class_keystate;
*/


static void RemoveIcon(), RecomputeIconX();

#define GAP 10		/*C*//* width of gap between rulers */
#define ICONHEIGHT 14	/*C*//* height of area where icons move */
#define PARKWIDTH  19	/*C*//* width of icon parking area */
#define RULERHEIGHT 22 /*C*//* bottom line = topline + RULERHEIGHT */

static struct fontdesc *TextFont, *IconFont;


struct TickTbl {
	short majorpix, minorpix;/* number of pixels for major and minor cycles */
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
	struct lprrulerview *self;
	char *where, *msg;
{
	fprintf(stderr, "<lprrulerview>Bogus call to %s, %s\n", where, msg);
	return FALSE;
}

static boolean
CheckWindow(self, where)
	struct lprrulerview *self;
	char *where;
{
	struct graphic *g
		= (struct graphic *)lprrulerview_GetDrawable(self);
	if ( ! g) return BogusCallFromParent(self, where, "No Graphic");
	return TRUE;
}



boolean
lprrulerview__InitializeClass(ClassID)
	struct classheader *ClassID;
{
	TextFont = fontdesc_Create("andysans", fontdesc_Bold, 12);
	IconFont = fontdesc_Create("icon", fontdesc_Plain, 12);
	return TRUE;
}

boolean
lprrulerview__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	struct lprrulerview  *self;
{
	struct icondata *i;
	long cnt;
	self->OnScreen = FALSE;
	self->rulerchanged = self->iconschanged = self->textchanged = TRUE;
	self->unit = style_Inches;
	self->TickTbl = &InchTbl;
	self->ValueChangeProc = NULL;
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++) {
		i->value = lprrulerview_NoValue;
		i->parkdirty = TRUE;
		i->icondirty = TRUE;
		i->isBlack = FALSE;
		RecomputeIconX(self, i - self->iconloc);
	}
	LEFT.icon = '(';   LEFT.parkoffset = 4;	/*C*/
	RIGHT.icon = ')';   RIGHT.parkoffset = -4;	/*C*/
	return TRUE;
}

void
lprrulerview__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	struct lprrulerview  *self;
{
}

void
lprrulerview__ObservedChanged(self, dobj, status)
	struct lprrulerview  *self;
	struct lprruler *dobj;
	long status;
{
	if (status == lprruler_DATACHANGED) 
		self->iconschanged = self->textchanged = TRUE;
	else if (status == observable_OBJECTDESTROYED)
		return;
	lprrulerview_WantUpdate(self, self);
}

static void
RepaintIcon(self, icon, color)
	struct lprrulerview  *self;
	enum iconcode icon;
	short color;
{
	struct icondata *i = &(self->iconloc[(short)icon]);
	lprrulerview_SetTransferMode(self, color);
	lprrulerview_MoveTo(self, i->x, self->icony);
	lprrulerview_DrawText(self, &i->icon, 1, graphic_NOMOVEMENT);
}
static void
RepaintPark(self, icon, color)
	struct lprrulerview  *self;
	enum iconcode icon;
	short color;
{
	struct icondata *i = &(self->iconloc[(short)icon]);
	struct rectangle r;		/* rectangle for parking lot */
	r. height = ICONHEIGHT+4, r.width = PARKWIDTH;	/*C*/
	r.top = self->icony - (ICONHEIGHT>>1) - 1;		/*C*/
	r.left = i->parkx - (PARKWIDTH>>1);
	lprrulerview_SetTransferMode(self, color);
	switch (color) {
	    case graphic_COPY:
		lprrulerview_FillRect(self, &r, self->Grey25Pattern);
		break;
	    case graphic_WHITE:
		lprrulerview_FillRect(self, &r, self->WhitePattern);
		break;
	    case graphic_INVERT:
	    case graphic_BLACK:
	    default:
		lprrulerview_FillRect(self, &r, self->BlackPattern);
		break;
	}
}

/* compute the x coordinate for an icon */
static void
RecomputeIconX(self, icon)
	struct lprrulerview *self;
	enum iconcode icon;
{
	struct icondata *i = &(self->iconloc[(short)icon]);
	if (icon == paraIcon) {
		long pv = PARA.value;
		if (pv > lprrulerview_NoValue && pv < 0L) {
			/* para exdent icon */
			PARA.icon = '+';
			PARA.parkoffset = -4;
		}
		else {
			/* para indent icon */
			PARA.icon = '*';
			PARA.parkoffset = 0;
		}
	}
	if (i->value <= lprrulerview_NoValue || i->isBlack)
		i->x = i->parkx + i->parkoffset;
	else if (icon == paraIcon && LEFT.value > lprrulerview_NoValue)
		i->x = LEFT.x +  (i->value>>16);
	else
		i->x = i->zero + (i->value>>16);
}
/* set the range bounds for an icon */
static void
BoundIcon(self, icon, zeroloc, left, right)
	struct lprrulerview *self;
	enum iconcode icon;
	short zeroloc, left, right;
{
	struct icondata *i = &(self->iconloc[(short)icon]);
	i->zero = zeroloc;
	i->left = left;
	i->right = right;
}
/* check each value to set isBlack for the icon.  
	Remove icon if blackness changes */
static void
CheckBounds(self)
	struct lprrulerview *self;
{
	struct icondata *i;
	long cnt;
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++) {
		boolean wasBlack = i->isBlack;
		if (i->value <= lprrulerview_NoValue)
			i->isBlack = FALSE;
		else
			i->isBlack = (i->x < i->left || i->x > i->right);
		if (i->isBlack != wasBlack) {
			enum iconcode icon = (enum iconcode)(i - self->iconloc);
			RemoveIcon(self, icon);
			RecomputeIconX(self, icon);
			i->parkdirty = TRUE;
		}
	}
			
}

/* check dirty bits and repaint all parks and icons that claim to need it 
	never modifies isBlack */
static void
CleanUpIconArea(self)
	struct lprrulerview *self;
{
	struct icondata *i;
	long cnt;
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++)
		if (i->parkdirty)
			RepaintPark(self, i - self->iconloc, 
				(i->isBlack) ? graphic_WHITE : graphic_COPY);
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++)
		if (i->icondirty) {
			i->icondirty = FALSE;
			RepaintIcon(self, i - self->iconloc, graphic_COPY);
		}
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++)
		if (i->parkdirty) {
			i->parkdirty = FALSE;
			if (i->isBlack)
				RepaintPark(self, i - self->iconloc, graphic_INVERT);
		}
	self->iconschanged = FALSE;
}

/* paint an icon white and set dirty bits for all icons and parks that may be affected */
static void
RemoveIcon(self, icon) 
	struct lprrulerview *self;
	enum iconcode icon;
{
	struct icondata *i;
	long cnt;
	short xlo, xhi;
	RepaintIcon(self, icon, graphic_WHITE);
	i = &(self->iconloc[(short)icon]);
	i->icondirty = TRUE;
	xlo = i->x - 20;		/*C*/
	xhi = i->x + 20;		/*C*/
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++) {
		/* in the general case we here have an N-squared problem.
			We have to check whether replotting any one symbol
			requires replotting any other.  For now we cheat and 
			and assume that the only overlap is between parks
			and their own icons. */
		if (xlo <= i->x && i->x <= xhi)   i->icondirty = TRUE;
		if (xlo <= i->parkx && i->parkx <= xhi)   i->parkdirty = TRUE;
		if (i->icondirty != i->parkdirty 
				&& i->parkx - 20 < i->x && i->x < i->parkx + 20)
			i->parkdirty = i->icondirty = TRUE;
	}
}

static void
MoveLeftIcon(self, newx)
	struct lprrulerview *self;
	long newx;
{
	long deltav = (newx - LEFT.x)<<16;
	RemoveIcon(self, leftIcon);
	LEFT.value += deltav;
	RecomputeIconX(self, leftIcon);
	if (PARA.value > lprrulerview_NoValue) {
		/* don't update PARA.x.  It is set from LEFT.x+PARA.value */
		RemoveIcon(self, paraIcon);
		RecomputeIconX(self, paraIcon);
	}
}
static void
MoveRightIcon(self, newx)
	struct lprrulerview *self;
	long newx;
{
	long deltav = (newx - RIGHT.x)<<16;
	RemoveIcon(self, rightIcon);
	RIGHT.value += deltav;
	RecomputeIconX(self, rightIcon);
}
static void
MoveParaIcon(self, newx)
	struct lprrulerview *self;
	long newx;
{
	long deltav = (newx - PARA.x)<<16;
	RemoveIcon(self, paraIcon);
	PARA.value += deltav;
	RecomputeIconX(self, paraIcon);
}


/* Draw tick marks in a ruler for -self- on the topline, from -left- to -right-
	the value zero would be plotted at location zeroloc, which may be off
		the ruler to either end
	the length of tick marks is given by -tbl-, with one entry for
		each minor cycle within a major cycle.
	The lengths of the cycles are given by -major- and -minor-
	assume major % minor == 0 */
static void
DoTicks(self, zeroloc, left, right, tbl)
	struct lprrulerview *self;
	short zeroloc, right;
	short left;
	struct TickTbl *tbl;
{
	short cycmax = tbl->majorpix / tbl->minorpix;	/* number of minor cycles in a major */
	short tickloc;					/* where to place next tick */
	short cyclecnt;				/* count minor ticks within major cycle */
	short ordloc;						/* where to first plot an ordinate value */
	short ordval;						/* next ordinate value to plot */

	/* since division and remainder are ill-defined for negative operands:
		 the "<<12" items arrange that the %'s are done on positive operands
		the divisions always come out to exact values 
	*/
	tickloc = left + (zeroloc + (tbl->minorpix << 12) - left) % tbl->minorpix;
	ordloc = left + (zeroloc + (tbl->majorpix << 12) - left) % tbl->majorpix;
	ordval = (ordloc - zeroloc) / tbl->majorpix * tbl->one;
	cyclecnt = cycmax - (ordloc-tickloc)/tbl->minorpix;
	if (cyclecnt == cycmax) cyclecnt = 0;

	lprrulerview_SetFont(self, TextFont);
	for ( ; tickloc < right; tickloc += tbl->minorpix) {
		lprrulerview_MoveTo(self, tickloc, self->topline);
		lprrulerview_DrawLineTo(self, tickloc, self->topline + tbl->ht[cyclecnt]);
		if (cyclecnt == 0) {
			/* draw digit */
			char buf[10];
			if (ordval > 0)
				sprintf(buf, "%+d", ordval);
			else sprintf(buf, "%d", ordval);	/* no sign if ordval==0 */
			lprrulerview_MoveTo(self, tickloc-1, self->topline + 7);	/*C*/
			lprrulerview_DrawString(self, buf,
				graphic_BETWEENLEFTANDRIGHT | graphic_ATTOP);
			ordval += tbl->one;
		}
		if (++cyclecnt >= cycmax) cyclecnt = 0;
	}
}

static void
RedrawRuler(self)
	struct lprrulerview *self;
{
	struct rectangle r;
	short extra;
	short middle, left, right;	/* bounds for tick marks NOT FOR ICON PAINTING */

	lprrulerview_GetLogicalBounds(self, &r);

	/* the following section computes the layout parameters
		it is full of artificial constants *//*C*/

	if (r.width < 200) r.width = 200;	/* truncate from the right if too narrow */
	/* leave space at the left for "parking" two icons and text underneath them
		leave space at the right for parking one icon */
	self->leftline = 70;
	self->rightline = r.width - PARKWIDTH - 10;
	self->middle = (r.width - 80 - PARKWIDTH - 10) * 15 / 24;
	self->textloc = 20;   /* with icon for unit change to its left */
	
	/* leave at least 16 pixels above the ruler for the icons */
	extra = r.height - ICONHEIGHT - 2 - RULERHEIGHT;
	if (extra < 0) extra = 0;		/* truncate at bottom if too short */
	self->topline = ICONHEIGHT + 2 + (extra>>1);	
	self->bottomline = self->topline + RULERHEIGHT;
	self->icony = self->topline - (ICONHEIGHT>>1) - 4;
	LEFT.parkx = self->leftline - 3 - (PARKWIDTH>>1);
	PARA.parkx = LEFT.parkx - 2 - PARKWIDTH;
	RIGHT.parkx = self->rightline + 3 + (PARKWIDTH>>1);
	
	lprrulerview_SetTransferMode(self, graphic_COPY);
	lprrulerview_FillRect(self, &r, self->WhitePattern);

	/* draw lprruler pieces */
	lprrulerview_MoveTo(self, self->leftline, self->topline);
	lprrulerview_DrawLineTo(self, self->middle, self->topline);
	lprrulerview_DrawLineTo(self, self->middle+4, self->topline + 10);
	lprrulerview_DrawLineTo(self, self->middle-2, self->topline + 15);
	lprrulerview_DrawLineTo(self, self->middle, self->bottomline);
	lprrulerview_DrawLineTo(self, self->leftline, self->bottomline);

	lprrulerview_MoveTo(self, self->rightline, self->topline);
	lprrulerview_DrawLineTo(self, self->middle+GAP, self->topline);
	lprrulerview_DrawLineTo(self, self->middle+GAP+4, self->topline + 10);
	lprrulerview_DrawLineTo(self, self->middle+GAP-2, self->topline + 15);
	lprrulerview_DrawLineTo(self, self->middle+GAP, self->bottomline);
	lprrulerview_DrawLineTo(self, self->rightline, self->bottomline);

	left = self->leftline+2;			/*C*/
	middle = self->middle-2;		/*C*/
	right = self->rightline-2;		/*C*/
	self->leftzero = (middle - left) * 3 / 10 + self->leftline;
	self->rightzero = (right - (middle + 4/*C*/ + GAP)) * 6 / 10 + middle + GAP + 2;

	DoTicks(self, self->leftzero, left, middle, self->TickTbl);
	DoTicks(self, self->rightzero, middle + 4/*C*/ + GAP, right, self->TickTbl);
	self->rulerchanged = FALSE;
}
static void
RedrawText(self)
	struct lprrulerview  *self;
{
	struct rectangle r;
	r.top = self->topline, r.left = self->textloc;
	r.height = RULERHEIGHT, r.width = self->leftline - self->textloc;	
	lprrulerview_SetTransferMode(self, graphic_COPY);
	lprrulerview_FillRect(self, &r, self->WhitePattern);
	lprrulerview_SetFont(self, TextFont);
	lprrulerview_MoveTo(self, self->textloc, self->bottomline - 6/*C*/);
	lprrulerview_DrawString(self, self->TickTbl->unitstring, 
				graphic_ATLEFT |  graphic_ATBASELINE);

	/* display icon for changing unit */
	lprrulerview_SetFont(self, IconFont);
	lprrulerview_MoveTo(self, self->textloc - 12, self->bottomline - 12);		/*C*/
	lprrulerview_DrawText(self, "\'", 1, graphic_NOMOVEMENT);
	self->textchanged = FALSE;
}
static void
RedrawIcons(self)
	struct lprrulerview  *self;
{
	struct icondata *i;
	long cnt;
	struct rectangle r;
	r.top = self->topline - ICONHEIGHT - 4/*C*/, r.left = 0;
	r.height = ICONHEIGHT + 4/*C*/, r.width = self->rightline + PARKWIDTH + 4/*C*/;	
	lprrulerview_SetTransferMode(self, graphic_COPY);
	lprrulerview_FillRect(self, &r, self->WhitePattern);
	lprrulerview_SetFont(self, IconFont);
	BoundIcon(self, leftIcon, self->leftzero, self->leftline, self->middle);
	BoundIcon(self, paraIcon, self->leftzero, self->leftline, self->middle);
	BoundIcon(self, rightIcon, self->rightzero, self->middle+GAP, self->rightline);
	
	for (i = &(self->iconloc[(short)leftIcon]), cnt = 3; cnt--; i++) {
		i->parkdirty = i->icondirty = TRUE;
		i->isBlack = FALSE;
		RecomputeIconX(self, i - self->iconloc);
	}
	CheckBounds(self);
	CleanUpIconArea(self);
}

void 
lprrulerview__FullUpdate( self, type, left, top, width, height )
	struct lprrulerview  *self;
	enum view_UpdateType  type;
	long  left, top, width, height;
{
	if (type == view_Remove) {
		self->OnScreen = FALSE;
		return;
	}
	if ( ! CheckWindow(self, "FullUpdate")) return;
	if ((type != view_FullRedraw 
				&& type != view_LastPartialRedraw)
			|| lprrulerview_GetLogicalWidth(self) == 0 
			|| lprrulerview_GetLogicalHeight(self) == 0) 
		return;
	self->OnScreen = TRUE;
	if (type == view_FullRedraw) {
		/* must recompute graphics info because image
			may be on different display hardware */
		self->WhitePattern = lprrulerview_WhitePattern(self);
		self->Grey25Pattern = lprrulerview_GrayPattern(self, 4, 16);
		self->BlackPattern = lprrulerview_BlackPattern(self);
	}
	self->rulerchanged = self->textchanged = self->iconschanged = TRUE;
	RedrawRuler(self);
	RedrawText(self);
	RedrawIcons(self);
}


void 
lprrulerview__Update( self )
	struct lprrulerview *self;
{
	if (! self->OnScreen || ! CheckWindow(self, "Update")) return;
	if (self->rulerchanged) RedrawRuler(self);
	if (self->textchanged) RedrawText(self);
	if (self->iconschanged) RedrawIcons(self);
}

struct view *
lprrulerview__Hit(self, action, x, y, num_clicks)
	struct lprrulerview  *self;
	enum view_MouseAction  action;
	long  x, y, num_clicks;
{
	if (action == view_NoMouseEvent)
		return (struct view *)self;
	if (! self->OnScreen || ! CheckWindow(self, "Hit")) return NULL;
	if (self->MovingIcon == noIcon  &&  y > self->topline) {
		/* must be in text area or ruler */
		if (action == view_LeftDown || action == view_RightDown ) {
			/* iconAt(self, self->textloc - 12, self->bottomline - 12, "'"); */
			short delx, dely;
			delx = x - (self->textloc -12);
			dely = y - (self->bottomline - 12);
			if (delx<0) delx = -delx;
			if (dely<0) dely = -dely;
			if (delx < 10 && dely < 10) {
				/* change unit */
				switch (self->unit) {
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
				RedrawIcons(self);
				RedrawText(self);
			}
		}
	}
	else {
		/* above topline,  assume trying to move an icon 
			Allow LEFT BUTTON only */

		lprrulerview_SetFont(self, IconFont);

		if (action == view_LeftUp) {
			struct icondata *i = &(self->iconloc[(short)self->MovingIcon]);
			long t = 0;	/* passed to the ValueChangeProc */
			switch (self->MovingIcon) {
				case leftIcon: 
					MoveLeftIcon(self, x);  
					t = i->value;
					break;
				case rightIcon: 
					MoveRightIcon(self, x);  
					t = - i->value;
					break;
				case paraIcon: 
					MoveParaIcon(self, x); 
					t = i->value; 
					break;
			}
			if (i->parkx - 11 <= i->x  && i->x <= i->parkx + 11) {
				t = i->value = lprrulerview_NoValue;
				RecomputeIconX(self, i - self->iconloc);
				if (self->MovingIcon == leftIcon)
					RecomputeIconX(self, paraIcon);
			}
			
			if (self->ValueChangeProc)
				/* notify client of new value */
				(self->ValueChangeProc)(self, 
						self->ValueChangeRock, 
						self->MovingIcon, 
						t);
			self->MovingIcon = noIcon;
			CheckBounds(self);
			CleanUpIconArea(self);
		}
		else {  /* Down or Move */
			short relx;	/* used later to display numeric value */
			if (action == view_LeftDown) {
				struct icondata *i;
				long cnt;
				self->MovingIcon = noIcon;
				for (i = &(self->iconloc[(short)paraIcon]), cnt = 3; cnt--; i--) 
					/* NOTE REVERSED ORDER of loop 
						so paraIcon will be
						found first even if it is close to leftICon */
					/* check for closeness to icon i */
					if (i->x - 12 < x  &&  x < i->x + 12) {
						self->MovingIcon = (enum iconcode)
								(i - self->iconloc);
						if (i->isBlack 
								|| i->value 
								<= lprrulerview_NoValue) {
							i->isBlack = FALSE;
							i->value = (i->x - i->zero)<<16;
						}
						if (self->MovingIcon == leftIcon 
								&& PARA.isBlack)
							PARA.isBlack = FALSE;
						/* no need to remove icons here
							they will be removed by
							MoveXxxxIcon below */
						break;
					}
			}

			/* Here for mouse down and mouse movement
				relx will be used for displaying the numeric value */
			switch (self->MovingIcon) {
				case leftIcon:
					MoveLeftIcon(self, x);
					relx = LEFT.x - self->leftzero;
					break;
				case rightIcon:
					MoveRightIcon(self, x);
					relx = RIGHT.x - self->rightzero;
					break;
				case paraIcon:
					MoveParaIcon(self, x);
					if (LEFT.value <= lprrulerview_NoValue)
						relx = PARA.x - self->leftzero;
					else
						relx = PARA.x - LEFT.x;
					break;
			}
			CleanUpIconArea(self);
			if (self->MovingIcon != noIcon) {
				/* plot the numeric value  (this is where we use relx) */
				struct rectangle r;
				char buf[10];
				r.top = self->topline, r.left = self->textloc;
				r.height = RULERHEIGHT, r.width = self->leftline - self->textloc;	
				lprrulerview_SetTransferMode(self, graphic_COPY);
				lprrulerview_FillRect(self, &r, self->WhitePattern);
				if (self->leftline <= x && x <= self->rightline)
					sprintf(buf, self->TickTbl->fmt, 
							(relx + 0.0) / self->TickTbl->majorpix * self->TickTbl->one);
				else 
					strcpy(buf, self->TickTbl->unitstring);
				lprrulerview_SetFont(self, TextFont);
				lprrulerview_MoveTo(self, self->textloc, self->bottomline - 6/*C*/);
				lprrulerview_DrawString(self, buf, 
					graphic_ATLEFT |  graphic_ATBASELINE);
		 	}
		} /* end "down or move */
	} /* end "above topline" */
	if (action == view_LeftDown || action == view_RightDown)
		lprrulerview_WantInputFocus(self, self);
	return ( struct view * )self;		/* where to send subsequent hits */
}

enum view_DSattributes
lprrulerview__DesiredSize( self, width, height, pass, 
				desiredWidth, desiredHeight ) 
	struct lprrulerview *self;
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



/* icon position values are pixel positions expressed as long int's with the binary point at 1<<16 */

/* set the values for the icon positions.  Values lprrulerview_NoValue (-999<<16) and lower indicate
			that no value is to be displayed */
void
lprrulerview__SetValues(self, leftmargin, rightmargin, paraindent)
	struct lprrulerview *self;
	long leftmargin, rightmargin, paraindent;
{
	LEFT.value = leftmargin;
	RIGHT.value =  (rightmargin <= lprrulerview_NoValue) ? rightmargin : - rightmargin;
	PARA.value = paraindent;
	self->iconschanged = self->textchanged = TRUE;
	lprrulerview_WantUpdate(self, self);
}

void
lprrulerview__GetValues(self, leftmargin, rightmargin, paraindent)
		/* sets the three parameters to the values of the icon positions */
	struct lprrulerview *self;
	long *leftmargin, *rightmargin, *paraindent;
{
	*leftmargin = LEFT.value;
	*rightmargin = (RIGHT.value <= lprrulerview_NoValue) ? RIGHT.value : - RIGHT.value;
	*paraindent = PARA.value;
}

