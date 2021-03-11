static char *doodview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/doodle/RCS/doodview.c,v 1.1 1992/10/06 22:10:36 susan R6tape $";

/* **************************************************** *\
Copyright 1989 Nathaniel S. Borenstein
Permission to use, copy, modify, and distribute this software and its
documentation for any purpose and without fee is hereby granted,
provided that the above copyright notice appear in all copies and
that both that copyright notice and this permission notice appear in
supporting documentation, and that the name of 
Nathaniel S. Borenstein not be used in
advertising or publicity pertaining to distribution of the software
without specific, written prior permission. 

Nathaniel S. Borenstein DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
Nathaniel S. Borenstein BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY
DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER
IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING
OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
\* ***************************************************** */
#include <doodview.eh>
#include <view.ih>
#include <graphic.ih>

boolean doodview__InitializeObject(c, self)
struct classheader *c;
struct doodview *self;
{
    self->downhitX = self->downhitY = self->lastuphitX = self->lastuphitY = self->linewidth = 0;
    return(TRUE);
}

void doodview__FinalizeObject(c, self)
struct classheader *c;
struct doodview *self;
{
}

struct view *
doodview__Hit(self, action, x, y, numclicks)
struct doodview *self;
enum view_MouseAction action;
long x, y, numclicks;  
{
    boolean IsUpHit = FALSE;

    switch(action) {
	    case view_LeftDown:
	    case view_RightDown:
		self->linewidth= (2*numclicks) - 1;
		self->downhitX = x;
		self->downhitY = y;
		self->lastuphitX = self->lastuphitY = -1;
		break;
	    case view_LeftUp:
	    case view_LeftMovement:
		doodview_SetLineWidth(self, self->linewidth);
		if (self->lastuphitX >= 0) {
		    /* white out old line */
		    doodview_SetTransferMode(self,
			graphic_WHITE);
		    doodview_MoveTo(self, self->downhitX,
			self->downhitY);
		    doodview_DrawLineTo(self,
			self->lastuphitX, self->lastuphitY);
		}
		/* draw new line */
		doodview_SetTransferMode(self, graphic_BLACK);
		doodview_MoveTo(self, self->downhitX,
				  self->downhitY);
		doodview_DrawLineTo(self, x, y);
		self->lastuphitX = x;
		self->lastuphitY = y;
		break;
	    case view_RightUp:
		IsUpHit = TRUE;
		/* Drop through to next case */
	    case view_RightMovement:
		doodview_SetLineWidth(self, self->linewidth);
		if (self->lastuphitX >= 0) {
		    /* white out old rectangle */
		    DrawMyRect(self, graphic_WHITE, FALSE);
		}
		/* draw new rectangle */
		self->lastuphitX = x;
		self->lastuphitY = y;
		DrawMyRect(self, graphic_BLACK, IsUpHit);
		break;
    }
    return((struct view *) self);
}

static DrawMyRect(self, transfermode, FillTheRectangle)
struct doodview *self;
short transfermode;
boolean FillTheRectangle;
{
    struct rectangle Rect;

    if (self->lastuphitX > self->downhitX) {
	Rect.left = self->downhitX;
	Rect.width = self->lastuphitX - self->downhitX;
    } else {
	Rect.left = self->lastuphitX;
	Rect.width = self->downhitX - self->lastuphitX;
    }
    if (self->lastuphitY > self->downhitY) {
	Rect.top = self->downhitY;
	Rect.height = self->lastuphitY - self->downhitY;
    } else {
	Rect.top = self->lastuphitY;
	Rect.height = self->downhitY - self->lastuphitY;
    }
    doodview_SetTransferMode(self, transfermode);
    doodview_DrawRect(self, &Rect);
    if (FillTheRectangle) {
	doodview_FillRect(self, &Rect,
		doodview_BlackPattern(self));
    }
}
