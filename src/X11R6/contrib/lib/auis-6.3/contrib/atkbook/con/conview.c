static char *conview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/conview.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

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
#include <conview.eh>
#include <conob.ih>
#include <message.ih>

extern char *malloc(), *realloc();

boolean conview__InitializeObject(c, self)
struct classinfo *c;
struct conview *self;
{
    self->laststring = NULL;
    return(TRUE);
}

void conview__FinalizeObject(c, self)
struct classheader *c;
struct conview *self;
{
    if (self->laststring != NULL) {
	free(self->laststring);
	self->laststring = NULL;
    }
}

enum view_DSattributes conview__DesiredSize(self, width, height, pass, desiredwidth, desiredheight)
struct conview *self;
long width, height;
enum view_DSpass pass;
long *desiredwidth, *desiredheight;

{
    *desiredwidth = 200;
    *desiredheight = 50;
    return(view_WidthFlexible | view_HeightFlexible);
}

void conview__SetLastString(self, buf)
struct conview *self;
char *buf;
{
    if (buf == NULL) {
	if (self->laststring != NULL) free(self->laststring);
	self->laststring = NULL;
	return;
    }
    if (self->laststring == NULL) {
	self->laststring = malloc(1+strlen(buf));
    } else {
	self->laststring = realloc(self->laststring,
				   1+strlen(buf));
    }
    if (self->laststring != NULL) strcpy(self->laststring, buf);
}

struct view *conview__Hit(self, action, x, y, nc)
struct conview *self;
enum view_MouseAction action;
long x, y, nc;
{
    if (action == view_LeftDown || action == view_RightDown) {
	char buf[1000];
	struct conob *c;

	conview_WantInputFocus(self, self);
	c = (struct conob *) conview_GetDataObject(self);
	conob_GetStringToDisplay(c, buf, sizeof(buf), TRUE);
	message_DisplayString(self, 10, buf);
    }
    return((struct view *) self);
}

void conview__FullUpdate(self, type, left, top, width, height)
struct conview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;
    struct conob *co;

    conview_GetLogicalBounds(self, &Rect);
    co = (struct conob *) conview_GetDataObject(self);
    if (co != NULL && co->Boxed) {
	conview_DrawRectSize(self, Rect.left,	Rect.top,
			Rect.width - 1, Rect.height - 1);
    }
    Rect.left += 1;
    Rect.top +=	1;
    Rect.width -= 2;
    Rect.height -= 2;
    if (Rect.width <= 0 || Rect.height <= 0) return;
    conview_DrawMyself(self, &Rect, TRUE);
}

void conview__Update(self)
struct conview *self;  
 {
    struct rectangle Rect;

    conview_GetLogicalBounds(self, &Rect);
    Rect.left += 1;
    Rect.top +=	1;
    Rect.width -= 2;
    Rect.height -= 2;
    if (Rect.width <= 0 || Rect.height <= 0) return;
    conview_DrawMyself(self, &Rect, FALSE);
}

void conview__DrawMyself(self, r, IsFullUpdate)
struct conview *self;
struct rectangle *r;
boolean IsFullUpdate;
{
    char buf[1000];
    struct conob *co = (struct conob *)
      conview_GetDataObject(self);

    if (co == NULL) {
	strcpy(buf, "<No object>");
    } else {
	conob_GetStringToDisplay(co, buf, sizeof(buf), FALSE);
    }
    if (!IsFullUpdate) {
	if (co == NULL) return;
	if ((self->laststring != NULL)
	    && !strcmp(self->laststring, buf)) return;
	/* White out and force full update */
	conview_EraseRect(self, r);
    }	
    conview_MoveTo(self, (r->left + r->width) / 2,
		      (r->top + r->height) / 2);
    conview_DrawString(self, buf,
	graphic_BETWEENLEFTANDRIGHT
	| graphic_BETWEENTOPANDBASELINE);
    conview_SetLastString(self, buf);
}

