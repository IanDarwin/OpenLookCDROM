static char *hanimapp_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hwanim/RCS/hanimapp.c,v 1.1 1992/10/06 22:18:43 susan R6tape $";

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
#include <hanimapp.eh>
#include <im.ih>
#include <event.ih>
#include <text.ih>

void AnimateText();
#define ANIMATIONDELAY event_MSECtoTU(100)

boolean hanimapp__InitializeObject(c, self)
struct classheader *c;
struct hanimapp *self;
{
    self->GoingForward = TRUE;
    return(TRUE);
}

void hanimapp__FinalizeObject(c, self)
struct classheader *c;
struct hanimapp *self;
{
}

boolean hanimapp__Start(self)
struct hanimapp *self;
{
    if (!super_Start(self)) return(FALSE);
    im_EnqueueEvent(AnimateText, self, ANIMATIONDELAY);
    return(TRUE);
}

void AnimateText(self)
struct hanimapp *self;
{
    struct text *t = hanimapp_GetText(self);
    int len = text_GetLength(t);

    if (self->GoingForward) {
	if (len > 60) self->GoingForward = FALSE;
	text_InsertCharacters(t, 0, " ", 1);
    } else {
	if (len < 20) self->GoingForward = TRUE;
	text_DeleteCharacters(t, 0, 1);
    }
    text_NotifyObservers(t, observable_OBJECTCHANGED); 
    im_EnqueueEvent(AnimateText, self, ANIMATIONDELAY);
}
