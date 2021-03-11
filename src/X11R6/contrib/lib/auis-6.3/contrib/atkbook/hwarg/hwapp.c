static char *hwapp_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hwarg/RCS/hwapp.c,v 1.1 1992/10/06 22:19:11 susan R6tape $";

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
#include <hwapp.eh>
#include <im.ih>
#include <text.ih>
#include <textv.ih>

boolean hwapp__InitializeObject(c, self)
struct classheader *c;
struct hwapp *self;
{
    self->t = NULL;
    self->tv = NULL;
    hwapp_SetMajorVersion(self, 1);
    hwapp_SetMinorVersion(self, 0);
    return(TRUE);
}

void hwapp__FinalizeObject(c, self)
struct classheader *c;
struct hwapp *self;
{
    if (self->t != NULL) text_Destroy(self->t);
    if (self->tv != NULL) textview_Destroy(self->tv);
}

boolean hwapp__Start(self)
struct hwapp *self;
{
    struct im *im = im_Create(NULL);

    self->t = text_New();
    self->tv = textview_New();
    if (self->t == NULL ||
	 self->tv == NULL
	 || im == NULL) return(FALSE);
    textview_SetDataObject(self->tv, self->t);
    text_InsertCharacters(self->t, 0, "Hello world.", 12); 
    im_SetView(im, self->tv);
    textview_WantInputFocus(self->tv, self->tv);
    return(TRUE);
}

struct text *hwapp__GetText(self)
struct hwapp *self;
{
    return(self->t);
}

