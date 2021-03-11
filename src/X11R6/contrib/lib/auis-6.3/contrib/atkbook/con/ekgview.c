static char *ekgview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/con/RCS/ekgview.c,v 1.1 1992/10/06 22:08:31 susan R6tape $";

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
#include <ekgview.eh>
#include <conob.ih>

boolean ekgview__InitializeObject(c, self)
struct classinfo *c;
struct ekgview *self;
{
    self->histctr = 0;
    return(TRUE);
}

void ekgview__FinalizeObject(c, self)
struct classheader *c;
struct ekgview *self;
{
}

#define CALCX(i) (r->left + (((i) * r->width) / HISTSIZE))
#define CALCY(i) (r->top + r->height - ((r->height * (self->history[(i)] - co->displaymin)) / (co->displaymax - co->displaymin)))

void ekgview__DrawMyself(self, r, IsFullUpdate)
struct ekgview *self;
struct rectangle *r;
boolean IsFullUpdate;
{
    int i;
    struct conob *co = (struct conob *)
      ekgview_GetDataObject(self);

    if (!IsFullUpdate) {
	self->history[self->histctr++] = conob_GetNumval(co);
	if (self->histctr >= HISTSIZE) {
	    /* White out, advance history,
	         then fall through to fullupdate */
	    for(i=0; i<HISTSIZE-SLIDEBY; ++i) {
		self->history[i] = self->history[i+SLIDEBY];
	    }
	    self->histctr -= SLIDEBY;
	    ekgview_EraseRect(self, r);
	} else { /* Really NOT a full update */
	    if (self->histctr <= 1) {
		ekgview_MoveTo(self, CALCX(0), CALCY(0));
	    } else {
		ekgview_MoveTo(self, CALCX(self->histctr-2),
			       CALCY(self->histctr-2));
	    }
	    ekgview_DrawLineTo(self, CALCX(self->histctr-1),
			       CALCY(self->histctr-1));
	    return; /* Done with Update operation */
	}
    }
    ekgview_MoveTo(self, CALCX(0), CALCY(0));
    for (i=1; i<self->histctr; ++i) {
	ekgview_DrawLineTo(self, CALCX(i), CALCY(i));
    }
}
