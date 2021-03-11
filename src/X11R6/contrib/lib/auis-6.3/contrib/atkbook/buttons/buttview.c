static char *buttview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/buttons/RCS/buttview.c,v 1.1 1992/10/06 22:07:54 susan R6tape $";

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
#include <buttview.eh>
#include <butt.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <cursor.ih>

extern char *malloc();

boolean
buttview__InitializeObject(c, self)
struct classheader *c;
struct buttview *self;
{
    self->myfontdesc = fontdesc_Create("andy",
				fontdesc_Bold, 12);
    self->mycursor = cursor_Create(self);
    self->lasttext = NULL;
    if (self->myfontdesc == NULL || self->mycursor == NULL) {
	return (FALSE);
    }
    cursor_SetStandard(self->mycursor, Cursor_Octagon);
    self->HitFunction = NULL;
    self->rock1 = 0;
    self->rock2 = 0;
    return(TRUE);
}

void
buttview__FinalizeObject(c, self)
struct classheader *c;
struct buttview *self;
{
    cursor_Destroy(self->mycursor);
    fontdesc_Destroy(self->myfontdesc);
    if (self->lasttext) free(self->lasttext);
}

void
buttview__FullUpdate(self, type, left, top, width, height)
struct buttview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;
    struct butt *b = (struct butt *)
      buttview_GetDataObject(self);
    char *txt = NULL;

    buttview_GetLogicalBounds(self, &Rect);
    buttview_EraseRect(self, &Rect);
    buttview_PostCursor(self, &Rect, self->mycursor);
    buttview_MoveTo(self, (Rect.left + Rect.width) / 2,
		       (Rect.top + Rect.height) / 2);
    buttview_SetFont(self, self->myfontdesc);
    if (b != NULL) {
	txt = butt_GetText(b);
	if (self->lasttext != NULL) free(self->lasttext);
	if (txt != NULL) {
	    self->lasttext = malloc(1+strlen(txt));
	    if (self->lasttext != NULL) {
		strcpy(self->lasttext, txt);
	    }
	} else {
	    self->lasttext = NULL;
	}
    }
    if (txt == NULL) txt = "<BEEP>";
    buttview_DrawString(self, txt,
	graphic_BETWEENLEFTANDRIGHT
	| graphic_BETWEENTOPANDBASELINE);
}

void
buttview__Update(self)
struct buttview *self;  
{
    struct rectangle r;
    struct butt *b = (struct butt *)
      buttview_GetDataObject(self);
    boolean HasChanged = FALSE;
    char *btext = butt_GetText(b);

    if (btext == NULL) {
	if (self->lasttext != NULL) HasChanged = TRUE;
    } else if (self->lasttext == NULL
		|| strcmp(btext, self->lasttext)) {
	HasChanged = TRUE;
    }
    if (HasChanged) {
	buttview_GetLogicalBounds(self, &r);
	buttview_FullUpdate(self, 0, r.left, r.top,
			      r.width, r.height);
    } /* Otherwise the text didn't change, no need to redraw */
}

struct view *
buttview__Hit(self, action, x, y, numclicks)
struct buttview *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct butt *b = (struct butt *)
      buttview_GetDataObject(self);

    if (buttview_GetHitFunction(self)) {
	buttview_GetHitFunction(self)
	  (buttview_GetRock1(self),
	   buttview_GetRock2(self), b, action);
    }
    return((struct view *)self);
}

void
buttview__SetHitFunction(self, f)
struct buttview *self;
int (*f)();
{
    self->HitFunction = f;
}

void
buttview__SetRocks(self, r1, r2)
struct buttview *self;
long r1, r2;
{
    self->rock1 = r1;
    self->rock2 = r2;
}

