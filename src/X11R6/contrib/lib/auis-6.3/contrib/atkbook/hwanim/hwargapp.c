static char *hwargapp_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hwanim/RCS/hwargapp.c,v 1.1 1992/10/06 22:18:43 susan R6tape $";

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
#include <hwargapp.eh>
#include <im.ih>
#include <text.ih>
#include <textv.ih>

boolean hwargapp__InitializeObject(c, self)
struct classheader *c;
struct hwargapp *self;
{
    self->whattosay = NULL;
    return(TRUE);
}

void hwargapp__FinalizeObject(c, self)
struct classheader *c;
struct hwargapp *self;
{
}

boolean hwargapp__ParseArgs(self, argc, argv)
struct hwargapp *self;
int argc;
char **argv;
{
    if (argc > 1) self->whattosay = argv[1];
    return(TRUE);
}

boolean hwargapp__Start(self)
struct hwargapp *self;
{
    if (!super_Start(self)) return(FALSE);
    if (self->whattosay) {
	struct text *t;

	t = hwargapp_GetText(self);
	text_Clear(t);
	text_InsertCharacters(t, 0, self->whattosay, strlen(self->whattosay)); 
    }
    return(TRUE);
}
