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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/lpair.c,v 2.22 1993/12/10 20:38:13 susan Exp $";
#endif


 


#include <andrewos.h>
#include <class.h>
#include <lpair.eh>

#include <im.ih>
#include <graphic.ih>
#include <view.ih>
#include <cursor.ih>
#include <rect.h>


/* In theory, BARWIDTH could be nuked, but it should be optimized out anyway and it may be useful "someday." */
#define BARWIDTH 0

/* 
 * <<Wed Apr 12 14:48:48 1989>>
 * Actually there is at least one BARWIDTH use (in __Hit) that would 
 * be wrong if BARWIDTH were non-0.  Be careful if today is "someday".
 * 
 * 						-t
 */

#define GRAVITY 1

/* Forward Declarations */
static void lpair_ComputeSizesFromTotal ();
static void lpair_ComputeSizes ();
static void lpair_ResetDimensions ();


/* For use in ComputeSizes below. */
#define min(a, b) ((a < b) ? a : b)
#define max(a, b) ((a < b) ? b : a)

/* Basically, the only reason this routine exists is because the FullUpdate signature does not use rectangles. This routine expects its redrawRectangle argument to be valid know matter what the type argument is. The type arg is just passed through to the children that need to be redrawn. All in all, this, Update, and FullUpdate can probably be simplified. -Z- */
static void DoFullUpdate(self, type, redrawRectangle)
struct lpair *self;
enum view_UpdateType type;
struct rectangle *redrawRectangle;
{
	/*  All this code needs changed */
	register int	x, y;
	int	offset; /* Used to get the bar line in the right place. */
	register struct view *leftTopObject = self->obj[0];
	register struct view *rightBottomObject = self->obj[1];
	struct rectangle childRectangle;

	self->needsfull = 0;

	lpair_ComputeSizes(self);
	lpair_ResetDimensions(self);	/* reset the child lpair sizes */
	x = 0; 
	y = 0;
	if (self->typex == lpair_VERTICAL)
		x += self->objcvt[0];
	else
		y += self->objcvt[0];

	lpair_SetTransferMode(self, graphic_BLACK);
	offset = (self->movable) ? BARWIDTH : 0; /* If not movable, don't put extra space around bar. */
	if (self->typex == lpair_VERTICAL) {
		lpair_MoveTo(self, x + offset, 0);
		lpair_DrawLineTo(self, x + offset, lpair_GetLogicalHeight(self)/*-1*/);
	} else {
		lpair_MoveTo(self, 0, y + offset);
		lpair_DrawLineTo(self, lpair_GetLogicalWidth(self)/*-1*/, y + offset);
	}

	if (leftTopObject != NULL) {
		view_GetEnclosedBounds(leftTopObject, &childRectangle);
		rectangle_IntersectRect(&childRectangle, &childRectangle, redrawRectangle);
		if (!rectangle_IsEmptyRect(&childRectangle)) {
			/* lpair_RetractViewCursors(self, leftTopObject); */
			view_FullUpdate(leftTopObject, type,
			    view_EnclosedXToLocalX(leftTopObject,
			    rectangle_Left(&childRectangle)),
			    view_EnclosedYToLocalY(leftTopObject,
			    rectangle_Top(&childRectangle)),
			    rectangle_Width(&childRectangle),
			    rectangle_Height(&childRectangle));
		}
	}

	if (rightBottomObject != NULL) {
		view_GetEnclosedBounds(rightBottomObject, &childRectangle);
		rectangle_IntersectRect(&childRectangle, &childRectangle, redrawRectangle);
		if (!rectangle_IsEmptyRect(&childRectangle)) {
			/* lpair_RetractViewCursors(self, rightBottomObject); */
			view_FullUpdate(rightBottomObject, type,
			    view_EnclosedXToLocalX(rightBottomObject,
			    rectangle_Left(&childRectangle)),
			    view_EnclosedYToLocalY(rightBottomObject,
			    rectangle_Top(&childRectangle)),
			    rectangle_Width(&childRectangle),
			    rectangle_Height(&childRectangle));
		}
	}

	if (self->movable) {
		struct rectangle cursorRegion;

		if (self->typex == lpair_VERTICAL)
			rectangle_SetRectSize(&cursorRegion,
			    x - GRAVITY,
			    0,
			    2 * (BARWIDTH + GRAVITY) + 1,
			    lpair_GetLogicalHeight(self));
		else
			rectangle_SetRectSize(&cursorRegion,
			    0,
			    y - GRAVITY,
			    lpair_GetLogicalWidth(self),
			    2 * (BARWIDTH + GRAVITY) + 1);

		lpair_PostCursor(self, &cursorRegion, self->cursor);
	}
}


void lpair__Update(self)
struct lpair *self;
{
	if (self->needsfull) {

		struct rectangle redrawRectangle;
		struct graphic *pat;

		lpair_SetTransferMode(self, graphic_COPY);
		lpair_GetLogicalBounds(self, &redrawRectangle);
		if (!(self->needsfull & 1)) {
			if (self->typex == lpair_HORIZONTAL)
				rectangle_SetTop(&redrawRectangle, self->objcvt[0] + BARWIDTH);
			else
				rectangle_SetLeft(&redrawRectangle, self->objcvt[0] + BARWIDTH);
		}
		if (!(self->needsfull & 2)) {
			if (self->typex == lpair_HORIZONTAL)
				rectangle_SetBottom(&redrawRectangle, self->objcvt[0] - BARWIDTH);
			else
				rectangle_SetRight(&redrawRectangle, self->objcvt[0] - BARWIDTH);
		}
		pat = lpair_WhitePattern(self);
		lpair_FillRect(self, &redrawRectangle, pat);
		/* I intentionally pass in view_FullRedraw here knowing that the DoFullUpdate
 * will still take the rectangle into account.
 */
		DoFullUpdate(self, view_FullRedraw, &redrawRectangle);
	}
}


void lpair__FullUpdate(self, type, left, top, width, height)
struct lpair *self;
enum view_UpdateType type;
long	left;
long	top;
long	width;
long	height;
{

	struct rectangle redrawRectangle;

	/* This really shouldn't be neccesary since the other redraw types should be required to fill in an appropriate recangle. However there are a few places (drawtxtv.c) where view_FullRedraw and view_Remove types get sent with 0 by 0 rectangles. */
	switch (type) {
	case view_PartialRedraw:
	case view_LastPartialRedraw:
		rectangle_SetRectSize(&redrawRectangle, left, top, width, height);
		break;
	case view_MoveNoRedraw:
		/* Need to reinsert views so they get correct coordinate system. */
		lpair_ResetDimensions(self);
		/* Intentional fall through. */
	case view_Remove:
		if (self->obj[0] != NULL)
			view_FullUpdate(self->obj[0], type, 0, 0, 0, 0);
		if (self->obj[1] != NULL)
			view_FullUpdate(self->obj[1], type, 0, 0, 0, 0);
		return;
		break;
	default:
		lpair_GetVisualBounds(self, &redrawRectangle);
		break;
	}
	DoFullUpdate(self, type, &redrawRectangle);
}


#define ABS(x) ((x) > 0 ? (x) : -(x))

struct view *lpair__Hit(self, action, x, y, numberOfClicks)
struct lpair *self;
enum view_MouseAction action;
long	x, y;
long	numberOfClicks;
{

	register long	dim;

	if (self->typex == lpair_VERTICAL) {
		dim = x;
	} else {
		dim = y;
	}

	if (self->movable && 
	    (action == view_RightDown || action == view_LeftDown) && 
	    (ABS(dim - self->objcvt[0] - BARWIDTH) <= BARWIDTH + GRAVITY)) {
		self->lasthit = dim;
		im_SetWindowCursor(view_GetIM((struct view *) self), self->cursor);
		self->ismoving = TRUE;
		return((struct view *) self); /* Our hit, return us. */
	}

	if (self->ismoving) {
		if ((action == view_RightUp || action == view_LeftUp)) {
			if (!self->maybeZero)
				if (self->typex == lpair_VERTICAL) {
					x = max(x, BARWIDTH + GRAVITY);
					x = min(x,
					    lpair_GetLogicalWidth(self) - BARWIDTH - GRAVITY);
				} 
				else {
					y = max(y, BARWIDTH + GRAVITY);
					y = min(y,
					    lpair_GetLogicalHeight(self) - BARWIDTH - GRAVITY);
				}

			if ((abs(self->lasthit - dim) > BARWIDTH + GRAVITY)) {
				if (self->typex == lpair_VERTICAL)
					if (self->sizeform == lpair_PERCENTAGE)
						self->objsize[1] = 100 - 
						    ((100 * x) / lpair_GetLogicalWidth(self));
					else if (self->sizeform == lpair_BOTTOMFIXED)
						self->objsize[1] = lpair_GetLogicalWidth(self) - x;
					else
						self->objsize[0] = x;
				else if (self->sizeform == lpair_PERCENTAGE)
					self->objsize[1] = 100 - 
					    ((100 * y) / lpair_GetLogicalHeight(self));
				else if (self->sizeform == lpair_BOTTOMFIXED)
					self->objsize[1] = lpair_GetLogicalHeight(self) - y;
				else
					self->objsize[0] = y;
				if (self->sizeform == lpair_PERCENTAGE)
					if (self->objsize[1] > 100)
						self->objsize[1] = 100;
					else if (self->objsize[1] < 0)
						self->objsize[1] = 0;
				self->needsfull = 3;
				lpair_WantUpdate(self, self);
			}
			im_SetWindowCursor(lpair_GetIM(self), NULL);
			self->ismoving = FALSE;
		}
		return((struct view *) self);
	}
	if (self->typex == lpair_VERTICAL)
		if (x < self->objcvt[0]) {
			if (self->obj[0] != NULL)
				return view_Hit(self->obj[0], action, view_EnclosedXToLocalX(self->obj[0], x),
				    view_EnclosedYToLocalY(self->obj[0], y), numberOfClicks);
		}
		else {
			if (self->obj[1] != NULL)
				return view_Hit(self->obj[1], action, view_EnclosedXToLocalX(self->obj[1], x),
				    view_EnclosedYToLocalY(self->obj[1], y), numberOfClicks);
		}
	else if (y < self->objcvt[0]) {
		if (self->obj[0] != NULL)
			return view_Hit(self->obj[0], action, view_EnclosedXToLocalX(self->obj[0], x),
			    view_EnclosedYToLocalY(self->obj[0], y), numberOfClicks);
	} else {
		if (self->obj[1] != NULL)
			return view_Hit(self->obj[1], action, view_EnclosedXToLocalX(self->obj[1], x),
			    view_EnclosedYToLocalY(self->obj[1], y), numberOfClicks);
	}
	return NULL; /* Catches the case where one of the lpair's views is NULL. */
}


/* This routineis special to prevent children from getting updates before a
 * FullUpdate. 
*/
void lpair__WantUpdate(self, requestor)
struct lpair *self;
struct view *requestor;
{

	/* If we are about to FullUpdate the view requesting an update, throw away the
 * request. This prevents views from getting updates before FullUpdates.
 */
	if (self->header.view.parent != NULL && 
	    !((self->needsfull & 1) && self->obj[0] != NULL && 
	    view_IsAncestor(requestor, self->obj[0]) || 
	    (self->needsfull & 2) && self->obj[1] != NULL && 
	    view_IsAncestor(requestor, self->obj[1])) && 
	    self->header.view.parent != NULL)
		view_WantUpdate(self->header.view.parent, requestor);
}


#define STARTHEIGHT 256
enum view_DSattributes lpair__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct lpair *self;
long	width, height;
enum view_DSpass pass;
long	*desiredwidth, *desiredheight;
{
	long	d0, d1, c0, c1;
	c0 = self->objcvt[0];
	c1 = self->objcvt[1];
	if (self->obj[0] && self->obj[1]) {
		if (self->typex == lpair_VERTICAL) {
			if (pass != view_HeightSet) {
				lpair_ComputeSizesFromTotal (self, width);
				view_DesiredSize(self->obj[0], self->objcvt[0], height, view_WidthSet, desiredwidth, &d0);
				view_DesiredSize(self->obj[1], self->objcvt[1], height, view_WidthSet, desiredwidth, &d1);
				*desiredwidth = width;
				self->objcvt[0] = c0;
				self->objcvt[1] = c1;
				if (d1 > 2048) {
					if (d0 > 2048) {
						*desiredheight = STARTHEIGHT;
						return(view_Fixed);
					}
					*desiredheight = d0;
					return(view_Fixed);
				}
				if (d0 > 2048) {
					*desiredheight = d1;
					return(view_Fixed);
				}
				*desiredheight = max(d0, d1);
				return(view_Fixed);
			}
		} else {
			if (pass != view_WidthSet) {
				lpair_ComputeSizesFromTotal (self, height);
				view_DesiredSize(self->obj[0], width, self->objcvt[0], view_HeightSet, &d0, desiredheight);
				view_DesiredSize(self->obj[1], width, self->objcvt[1], view_HeightSet, &d1, desiredheight);
				self->objcvt[0] = c0;
				self->objcvt[1] = c1;
				*desiredheight = (height > 2048) ? STARTHEIGHT : height;
				*desiredwidth = max(d0, d1);
				return(view_Fixed);
			}
		}
	}
	*desiredwidth = width;
	*desiredheight = (height > 2048) ? STARTHEIGHT : height;
	return(view_Fixed);
}


static void lpair_ComputeSizesFromTotal (l, totalsize)
register struct lpair *l;
int	totalsize;
{
	int	i = 0;

	if (l->movable)
		totalsize -= 2 * BARWIDTH + 1;		/* If movable allocate an area for move mouse hits. */
	else
		totalsize -= 1;	/* Make room for the bar in the middle. */

	switch (l->sizeform) { /* Allocate space for the 'hard' allocated view. */
	case lpair_PERCENTAGE:
		i = 1;
		l->objcvt[i] = (l->objsize[i] * totalsize) / 100;
		break;
	case lpair_BOTTOMFIXED:
		i = 1;
		l->objcvt[i] = min(totalsize, l->objsize[i]);
		break;
	case lpair_TOPFIXED:
		i = 0;
		l->objcvt[i] = min(totalsize, l->objsize[i]);
		break;
	}
	/* Give the rest to the other view. */
	l->objcvt[1-i] = totalsize - l->objcvt[i];
}


static void lpair_ComputeSizes (l)
register struct lpair *l;
{

	int	totalsize;

	/* Find out how much space the two must share. */
	if (l->typex == lpair_VERTICAL)
		totalsize = lpair_GetLogicalWidth(l);
	else
		totalsize = lpair_GetLogicalHeight(l);

	/* See if we don't have any space -- actually we should be testing to see if children can fit at all, but for, now a simple test for zero */
	if (totalsize == 0) {
		l->objcvt[0] = l->objcvt[1] = 0;
		return;
	}
	lpair_ComputeSizesFromTotal (l, totalsize);
}


static void lpair_ResetDimensions(self)
register struct lpair *self;
{

	register int	i, x, y;
	register struct view *child;
	struct rectangle enclosingRect;

	x = 0; 
	y = 0;
	for (i = 0; i < 2; i++) { /* Loop over the two halves of the lpair. */
		child = (struct view *) self->obj[i];
		if (child != NULL)
			if (self->typex == lpair_VERTICAL) {
				rectangle_SetRectSize(&enclosingRect, x, y, self->objcvt[i], lpair_GetLogicalHeight(self));
				view_InsertView(child, self, &enclosingRect);
				x += self->objcvt[i] + 2 * BARWIDTH + 1;
			}
			else {
				rectangle_SetRectSize(&enclosingRect, x, y, lpair_GetLogicalWidth(self),  self->objcvt[i]);
				view_InsertView(child, self, &enclosingRect);
				y += self->objcvt[i] + 2 * BARWIDTH + 1;
			}
	}
}


boolean lpair__InitializeObject (classID, self)
struct classheader *classID;
register struct lpair *self;
{

	self->obj[0] = NULL;
	self->obj[1] = NULL;
	self->typex = lpair_HORIZONTAL;
	self->ismoving = FALSE;
	self->movable = TRUE;
	self->lasthit = 0;
	self->cursor = cursor_Create(self);
	cursor_SetStandard(self->cursor, Cursor_HorizontalBars);
	self->needsfull = 0;
	self->maybeZero = FALSE;

	return TRUE;
}


void lpair__FinalizeObject(classID, self)
struct classheader *classID;
struct lpair *self;
{

	if (self->obj[0] != NULL)
		view_UnlinkTree(self->obj[0]);
	if (self->obj[1] != NULL)
		view_UnlinkTree(self->obj[1]);
	cursor_Destroy(self->cursor);
}


struct lpair *lpair__Create (classID, l1, l2, x)
struct classheader *classID;
struct view *l1, *l2;
long	x;
{

	struct lpair *newl;

	newl = lpair_New();
	lpair_Init(newl, l1, l2, x);
	return newl;
}


void lpair__Init(newl, l1, l2, x)
struct lpair *newl;
struct view *l1, *l2;
long	x;
{

	if (x < 0)
		lpair_VSplit(newl, l1, l2, -x, 1);
	else
		lpair_VFixed(newl, l1, l2, x, 1);
	newl->needsfull = 3;
}


struct lpair *lpair__SetUp(self, l1, l2, bsize, porf, vorh, moveable)
struct lpair *self; /* The lpair view to modify. */
struct view *l1, *l2; /* This lpairs prospective children. */
int	bsize, porf, vorh; /* Size of bottom (right) view area, size is percent or pixels, vertical or horizontal. */
{

	lpair_SetNth(self, 0, l1);
	lpair_SetNth(self, 1, l2);

	if (porf == lpair_TOPFIXED) {
		self->objsize[0] = bsize;
		self->objsize[1] = 0;
	} else {
		self->objsize[0] = 0;
		self->objsize[1] = bsize;
	}

	self->sizeform = porf;
	if (self->typex != vorh) {
		if ((self->typex = vorh) == lpair_VERTICAL)
			cursor_SetStandard(self->cursor, Cursor_VerticalBars);
		else
			cursor_SetStandard(self->cursor, Cursor_HorizontalBars);
	}
	self->movable = moveable;
	lpair_ComputeSizes(self);
	if (self->objcvt[0] == 0 || self->objcvt[1] == 0)
		self->maybeZero = TRUE;
	return self;
}


struct lpair *lpair__VFixed (tl, l1, l2, bsize, moveable)
struct view *l1, *l2;
struct lpair *tl;
int	bsize;
{

	lpair_SetUp(tl, l1, l2, bsize, lpair_BOTTOMFIXED, lpair_HORIZONTAL, moveable);
	return tl;
}


struct lpair *lpair__VTFixed (tl, l1, l2, bsize, moveable)
struct view *l1, *l2;
struct lpair *tl;
int	bsize;
{

	lpair_SetUp(tl, l1, l2, bsize, lpair_TOPFIXED, lpair_HORIZONTAL, moveable);
	return tl;
}


struct lpair *lpair__VSplit(tl, l1, l2, pct, moveable)
struct view *l1, *l2;
int	pct;
register struct lpair *tl;
{
	lpair_SetUp(tl, l1, l2, pct, lpair_PERCENTAGE, lpair_HORIZONTAL, moveable);
	return tl;
}


struct lpair *lpair__HFixed (tl, l1, l2, bsize, moveable)
struct view *l1, *l2;
struct lpair *tl;
int	bsize;
{

	lpair_SetUp(tl, l1, l2, bsize, lpair_BOTTOMFIXED, lpair_VERTICAL, moveable);
	return tl;
}


struct lpair *lpair__HTFixed (tl, l1, l2, bsize, moveable)
struct view *l1, *l2;
struct lpair *tl;
int	bsize;
{

	lpair_SetUp(tl, l1, l2, bsize, lpair_TOPFIXED, lpair_VERTICAL, moveable);
	return tl;
}


struct lpair *lpair__HSplit(tl, l1, l2, pct, moveable)
struct view *l1, *l2;
int	pct;
register struct lpair *tl;
{
	lpair_SetUp(tl, l1, l2, pct, lpair_PERCENTAGE, lpair_VERTICAL, moveable);
	return tl;
}


void lpair__SetLPState(tl, porf, vorh, movable)
struct lpair *tl; /* Pointer to the lpair we wish to modify. */
int	porf, vorh, movable; /* Percent or fixed, Vertical or Horizontal, movable bar. * */
{

	if (porf == lpair_PERCENTAGE || porf == lpair_BOTTOMFIXED || porf == lpair_TOPFIXED)
		tl->sizeform = porf;
	if (vorh == lpair_VERTICAL || vorh == lpair_HORIZONTAL)
		if (tl->typex != vorh) {
			if ((tl->typex = vorh) == lpair_VERTICAL)
				cursor_SetStandard(tl->cursor, Cursor_VerticalBars);
			else
				cursor_SetStandard(tl->cursor, Cursor_HorizontalBars);
		}
	if (movable != lpair_NOCHANGE)
		tl->movable = movable;
	tl->needsfull = 3;
	lpair_WantUpdate(tl, tl);
}


void lpair__GetLPState(tl, porf, vorh, movable)
struct lpair *tl;
int	*porf, *vorh, *movable;
{

	*porf = tl->sizeform;
	*vorh = tl->typex;
	*movable = tl->movable;
}


void lpair__SetMovable(lp, i)
struct lpair *lp;
int	i;
{

	lp->movable = i;
}


struct view *lpair__GetNth(l, ai)
register struct lpair *l;
int	ai;
{

	return l->obj[ai];
}


void lpair__SetNth(self, ai, x)
register struct lpair *self;
register struct view *x;
int	ai;
{

	if (ai >= 0 && ai <= 1 && self->obj[ai] != x) {
		int	other = 1 -ai;

		if (self->obj[other] == x) {
			lpair_SetNth(self, other, NULL);
		}

		if (self->obj[ai] != NULL)
			view_UnlinkTree(self->obj[ai]);

		if ((self->obj[ai] = x) != NULL)
			view_LinkTree(self->obj[ai], self);

		self->needsfull |= (1 << ai);
		lpair_WantUpdate(self, self);
	}
}


void lpair__LinkTree(self, parent)
struct lpair *self;
struct view *parent;
{

	super_LinkTree(self, parent);
	if (self->obj[0] != NULL)
		view_LinkTree(self->obj[0], self);
	if (self->obj[1] != NULL)
		view_LinkTree(self->obj[1], self);
}


void lpair__InitChildren(self)
struct lpair *self;
{
	if (self->obj[0] != NULL)
		view_InitChildren(self->obj[0]);
	if (self->obj[1] != NULL)
		view_InitChildren(self->obj[1]);
}


