static char *butt_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hggh/RCS/butt.c,v 1.1 1992/10/06 22:14:59 susan R6tape $";

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
#include <butt.eh>
#include <fontdesc.ih>
#include <graphic.ih>
#include <cursor.ih>

extern char *malloc();

boolean
butt__InitializeObject(c, self)
struct classheader *c;
struct butt *self;
{
    self->text = NULL;
    self->HitFunction = NULL;
    self->rock1 = 0;
    self->rock2 = 0;
    self->myfontdesc = fontdesc_Create("andy", fontdesc_Bold, 12);
    self->mycursor = cursor_Create(self);
    cursor_SetStandard(self->mycursor, Cursor_Octagon);
    return(TRUE);
}

void
butt__FinalizeObject(c, self)
struct classheader *c;
struct butt *self;
{
    cursor_Destroy(self->mycursor);
    fontdesc_Destroy(self->myfontdesc);
    if (self->text) free(self->text);
}

struct view *
butt__Hit(self, action, x, y, numclicks)
struct butt *self;
enum view_MouseAction action;
long x, y, numclicks;  
{
    if (self->HitFunction) {
	self->HitFunction(self->rock1, self->rock2,
			  self, action);
    }
    return((struct view *)self);
}

void
butt__Update(self)
struct butt *self;  
{
    struct rectangle r;

    butt_GetLogicalBounds(self, &r);
    butt_EraseRect(self, &r);
    butt_FullUpdate(self, view_FullRedraw, r.left,
		       r.top, r.width, r.height);
}

void
butt__FullUpdate(self, type, left, top, width, height)
struct butt *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;
    char *dtext;

    butt_GetLogicalBounds(self, &Rect);
    butt_SetFont(self, self->myfontdesc);
    butt_MoveTo(self, (Rect.left + Rect.width) / 2,
		   (Rect.top + Rect.height) / 2);
    if (self->text == NULL) {
	dtext = "<BEEP>";
    } else {
	dtext = self->text;
    }
    butt_DrawString(self, dtext,
		       graphic_BETWEENLEFTANDRIGHT
		       | graphic_BETWEENTOPANDBASELINE);
    butt_PostCursor(self, &Rect, self->mycursor);
}

void
butt__SetText(self, txt)
struct butt *self;
char *txt;
{
    if (self->text) free(self->text);
    self->text = malloc(1+strlen(txt));
    strcpy(self->text, txt);
    butt_WantUpdate(self, self);
}

void
butt__SetButtonFont(self, f)
struct butt *self;
struct fontdesc *f;
{
    if (self->myfontdesc != NULL) {
	fontdesc_Destroy(self->myfontdesc);
    }
    self->myfontdesc = f;
    butt_WantUpdate(self, self);
}

void
butt__SetHitFunction(self, f)
struct butt *self;
int (*f)();
{
    self->HitFunction = f;
}

void
butt__SetRocks(self, r1, r2)
struct butt *self;
long r1, r2;
{
    self->rock1 = r1;
    self->rock2 = r2;
}

