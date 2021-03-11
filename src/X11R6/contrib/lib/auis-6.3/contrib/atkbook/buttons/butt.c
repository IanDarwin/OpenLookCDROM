static char *butt_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/buttons/RCS/butt.c,v 1.1 1992/10/06 22:07:54 susan R6tape $";

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

extern char *malloc();

boolean
butt__InitializeObject(c, self)
struct classheader *c;
struct butt *self;
{
    self->text = NULL;
    return(TRUE);
}

void
butt__FinalizeObject(c, self)
struct classheader *c;
struct butt *self;
{
    if (self->text) free(self->text);
}

void
butt__SetText(self, txt)
struct butt *self;
char *txt;
{
    if (self->text) free(self->text);
    self->text = malloc(1+strlen(txt));
    strcpy(self->text, txt);
}

