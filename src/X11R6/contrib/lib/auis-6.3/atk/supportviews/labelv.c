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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/labelv.c,v 2.15 1992/12/15 21:42:39 rr2b R6tape $";
#endif


 

/* labelv.c	

	The view module for the label dataobject


*/


/* sys/types.h in AIX PS2 defines "struct label", causing a type name clash.
   Avoid this by temporarily redefining "label" to be something else in the preprocessor. */
#define label gezornenplatz
#include <andrewos.h> /* strings.h */
#undef label
#include <class.h>
#include <graphic.ih>
#include <view.ih>
#include <fontdesc.ih>

#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <bind.ih>

#include <im.ih>
#include <rect.h>

#include <label.ih>
#include <labelv.eh>


	static boolean
BogusCallFromParent(self, where, msg)
	register struct labelview *self;
	char *where, *msg;
{
	fprintf(stderr, "<labelview>Bogus call to %s, %s\n", where, msg);
	return FALSE;
}

	static boolean
CheckWindow(self, where)
	register struct labelview *self;
	char *where;
{
	register struct graphic *g
		= (struct graphic *)labelview_GetDrawable(self);
	if ( ! g) return BogusCallFromParent(self, where, "No Graphic");
	return TRUE;
}

	boolean
labelview__InitializeObject(ClassID, self)
	struct classheader *ClassID;
	register struct labelview  *self;
{
	self->GaveSize = FALSE;
	self->embedded = TRUE;
	self->OnScreen = FALSE;
	self->inverted = FALSE;
	self->hitproc = NULL;
	return TRUE;
}

	void
labelview__FinalizeObject(ClassID, self)
	struct classheader *ClassID;
	register struct labelview  *self;
{
}

	void
labelview__ObservedChanged(self, dobj, status)
	register struct labelview  *self;
	struct observable *dobj;
	long status;
{
	if (status == label_DATACHANGED) {
		self->GaveSize = FALSE;
		labelview_WantNewSize(self, self);
	}
	else if (status == observable_OBJECTDESTROYED) 
		return;	/* deleting it */
	labelview_WantUpdate(self, self);
}

	struct labelview *
labelview__GetApplicationLayer(self)
	register struct labelview *self;
{
	self->embedded = FALSE;
	return self;
}

	static void
RedrawTable(self)
	register struct labelview *self;
{
	register struct label *st 
			= (struct label *)self->header.view.dataobject;
	struct rectangle r;
	int x, y, oldwidth;
	
	labelview_SetTransferMode(self, graphic_COPY);
	labelview_GetLogicalBounds(self, &r);
	labelview_FillRect(self, &r, self->WhitePattern);

	if (st->flags & label_BOXED) {
	    labelview_SetTransferMode(self, graphic_INVERT);
	    r.top++;
	    r.left++;
	    r.width -= 2;
	    r.height -= 2;
	    labelview_DrawRect(self,&r);
#if 0
	    oldwidth = labelview_GetLineWidth(self);
	    labelview_SetLineWidth(self, 2);
	    labelview_SetLineWidth(self, oldwidth);
#endif
	}
	if (st->text) {
		labelview_SetFont(self, st->font);
		if (*(st->text+1)) {
			/* more than one character */
		        x = (st->flags & label_RIGHTJUST) ? r.left + r.width - 3 :
			    (st->flags & label_HCENTERJUST) ? r.left + (r.width / 2) :
				r.left + 3;
			y = (st->flags & label_BOTTOMJUST) ? r.top + r.height - 3 :
			    (st->flags & label_VCENTERJUST) ? r.top + (r.height / 2):
				r.top + 3;
			labelview_MoveTo(self, x, y);
			labelview_DrawString(self, st->text,
				 (st->flags == 0) ? graphic_ATLEFT | graphic_ATTOP :
					            st->flags & ~label_BOXED);
		}
		else {
			/* plot one character.  Use character size rather than font sizes
				because font sizes for icon12 are terrible for most icons 
				center the character in the rectangle */
			struct fontdesc_charInfo charInfo;
			fontdesc_CharSummary(labelview_GetFont(self), 
					labelview_GetDrawable(self), *st->text, &charInfo);
			labelview_MoveTo(self, 
				r.left + charInfo.xOriginOffset 
					+ ((r.width-charInfo.width)>>1),
				r.top + charInfo.yOriginOffset
					+ ((r.height-charInfo.height)>>1)
			);
			labelview_DrawString(self, st->text, graphic_NOMOVEMENT);
		}
	}
	if (self->inverted) {
		labelview_SetTransferMode(self, graphic_INVERT);
		labelview_FillRect(self, &r, self->BlackPattern);
	}
}

	void 
labelview__FullUpdate(self, type, left, top, width, height)
	register struct labelview  *self;
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
			|| labelview_GetLogicalWidth(self) == 0 
			|| labelview_GetLogicalHeight(self) == 0) 
		return;
	self->OnScreen = TRUE;
	if (type == view_FullRedraw  || type == view_LastPartialRedraw) {
		self->WhitePattern = labelview_WhitePattern(self);
		self->BlackPattern = labelview_BlackPattern(self);
	}
	if ( ! self->GaveSize)
		labelview_WantNewSize(self, self);
	RedrawTable(self);
}


	void 
labelview__Update(self)
	register struct labelview *self;
{
	if (! self->OnScreen || ! CheckWindow(self, "Update")) return;
	RedrawTable(self);
}

	struct view *
labelview__Hit(self, action, x, y, num_clicks)
	register struct labelview  *self;
	register enum view_MouseAction  action;
	register long  x, y, num_clicks;
{
	if (action == view_NoMouseEvent)
		return (struct view *)self;
	if (! self->OnScreen || ! CheckWindow(self, "Hit")) return NULL;
	if (self->hitproc)
		(self->hitproc)(self, action, self->hitrock);
	return (struct view *)self;		/* where to send subsequent hits */
}

	enum view_DSattributes
labelview__DesiredSize( self, width, height, pass, 
				desiredWidth, desiredHeight ) 
	register struct labelview *self;
	long width;
	long height;
	enum view_DSpass pass;
	long *desiredWidth;
	long *desiredHeight;
{
	if ( ! self->GaveSize ) {
		long w, h;
		char *tail;
		register struct label *st 
			= (struct label *)self->header.view.dataobject;
		register struct graphic *g;
		struct FontSummary *FS;
		if ( ! CheckWindow(self, "DSize")) {
			/* can't compute size without a window (we need a font)
				give dummy values */
			*desiredWidth = 144;
			*desiredHeight = 20;
			return view_HeightFlexible | view_WidthFlexible;
		}
		self->minwidth = self-> minheight = 0;
		g = labelview_GetDrawable(self);
		FS = fontdesc_FontSummary(st->font, g);
		tail = (char *)label_GetText((struct label *)self->header.view.dataobject);
		while (tail) {
			register char *tend = index(tail, '\n');
			if (tend) {
				fontdesc_TextSize(st->font, g, tail, tend-tail, &w, &h);
				tail = tend+1;
			}
			else {
				fontdesc_StringSize(st->font, g, tail, &w, &h);
				tail = NULL;
			}
			if (w > self->minwidth)  self->minwidth = w;
			self->minheight += FS->maxHeight;
		}
		self->GaveSize = TRUE;
	}

	*desiredWidth = self->minwidth+6;
	*desiredHeight = self->minheight+6;
/*
fprintf(stderr, "input %dx%d    output %dx%d\n", 
width, height, *desiredWidth, *desiredHeight); fflush(stderr);
*/
	return view_HeightLarger | view_WidthLarger;
}

	void
labelview__Print( self, file, processor, format, level )
	struct labelview 	 *self;
	FILE   *file;
	char  	 *processor;
	char  	 *format;
	boolean  	level;
{
	/* XXX sigh */
}

	void
labelview__SetHitProc(self, proc, rock)
	register struct labelview *self;
	void (*proc)();
	char *rock;
{
	self->hitproc = proc;
	self->hitrock = rock;
}

	char *
labelview__GetHitRock(self)
	register struct labelview *self;
{
	return self->hitrock;
}

	void
labelview__SetInversion(self, invert)
	register struct labelview *self;
	boolean invert;
{
	if (self->inverted == invert)
		return;
	self->inverted = invert;
	labelview_WantUpdate(self, self);
}

	boolean
labelview__GetInversion(self)
	register struct labelview *self;
{
	return self->inverted;
}
