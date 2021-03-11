static char *clogview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/clogview.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

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
#include <clogview.eh>
#include <scroll.ih>
#include <text.ih>
#include <textv.ih>
#include <conob.ih>

extern char *index();

boolean clogview__InitializeObject(c, self)
struct classinfo *c;
struct clogview *self;
{
    self->s = scroll_New();
    self->t = text_New();
    self->tv = textview_New();
    if (self->s == NULL || self->t == NULL
	 || self->tv == NULL) return(FALSE);
    textview_SetDataObject(self->tv, self->t);
    scroll_SetView(self->s, self->tv);
    text_SetReadOnly(self->t, TRUE);
    return(TRUE);
}
 
void clogview__FinalizeObject(c, self)
struct classheader *c;
struct clogview *self;
{
    textview_UnlinkTree(self->tv);
    scroll_UnlinkTree(self->s);
    text_Destroy(self->t);
    scroll_Destroy(self->s);
    textview_Destroy(self->tv);
}

struct view *
clogview__Hit(self, action, x, y, numclicks)
struct clogview *self;
enum view_MouseAction action;
long x, y, numclicks;  
{
    return(scroll_Hit(self->s, action, x, y, numclicks));
}

void
clogview__FullUpdate(self, type, left, top, width, height)
struct clogview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;
    clogview_GetVisualBounds(self, &Rect);
    scroll_InsertView(self->s, self, &Rect);
    scroll_FullUpdate(self->s, type, left, top, width, height);
}

void
clogview__Update(self)
struct clogview *self;  
{
    char buf[1000];
    int textlen, buflen;
    struct conob *co = (struct conob *)
      clogview_GetDataObject(self);

    if (co) {
	conob_GetStringToDisplay(co, buf, sizeof(buf)-1, FALSE);
	if (index(buf, '\n') == NULL) strcat(buf, "\n");
	if ((clogview_GetLastString(self) != NULL)
	    && !strcmp(clogview_GetLastString(self), buf)) {
	    return;
	}
	buflen = strlen(buf);
	textlen = text_GetLength(self->t);
	text_AlwaysInsertCharacters(self->t, textlen,
				    buf, buflen);
	clogview_SetLastString(self, buf);
	textview_FrameDot(self->tv, textlen+buflen-1);
	textview_Update(self->tv);
	if (textlen+buflen > 10000) {
	    text_AlwaysDeleteCharacters(self->t, 0, 1000);
	}
    }
}

void clogview__LinkTree(self, v)
struct clogview *self;
struct view *v;
{
    super_LinkTree(self, v);
    scroll_LinkTree(self->s, self);
}

