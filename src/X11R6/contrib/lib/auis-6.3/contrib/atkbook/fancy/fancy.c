static char *fancy_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/fancy/RCS/fancy.c,v 1.1 1992/10/06 22:11:07 susan R6tape $";

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
#include <fancy.eh>
#include <stylesht.ih>
#include <style.ih>
#include <envrment.ih>

static char *InsertionText = "\n\n\n\nHello, world!\n\nYou are viewing a demonstration of how the Andrew Toolkit can be used to produce interesting and complex multi-font formatted text.\n\nThis should prove useful in understanding how to make fancy text.\n(At least, that's the intent.)\n";

boolean fancy__InitializeObject(c, self)
struct classheader *c;
struct fancy *self;
{
    struct style *styletmp;
    struct environment *envtmp;

    fancy_InsertCharacters(self, 0, InsertionText,
				strlen(InsertionText));
    fancy_ReadTemplate(self, "help", FALSE);
    styletmp = stylesheet_Find(
	fancy_GetStyleSheet(self), "center");
    fancy_SetGlobalStyle(self, styletmp);
    styletmp = stylesheet_Find(
	fancy_GetStyleSheet(self), "bold");
    envtmp = fancy_AddStyle(self, 4, 5, styletmp);
    environment_SetStyle(envtmp, TRUE, TRUE);
    return(TRUE);
}

void fancy__FinalizeObject(c, self)
struct classheader *c;
struct fancy *self;
{
}

char *fancy__ViewName(self)
struct fancy *self;
{
    return("textview");
}
