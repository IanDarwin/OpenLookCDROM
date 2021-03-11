static char *fancier_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/fancy/RCS/fancier.c,v 1.1 1992/10/06 22:11:07 susan R6tape $";

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
#include <fancier.eh>
#include <style.ih>
#include <stylesht.ih>
#include <fontdesc.ih>

boolean fancier__InitializeObject(c, self)
struct classheader *c;
struct fancier *self;
{
    struct style *newstyle;
    
    newstyle = style_New();
    style_AddNewFontFace(newstyle, fontdesc_Bold);
    style_AddNewFontFace(newstyle, fontdesc_Italic);
    style_SetFontSize(newstyle, style_ConstantFontSize, 20);
    style_SetJustification(newstyle, style_RightJustified);
    style_SetNewLeftMargin(newstyle, style_PreviousIndentation,
			    2, style_Inches);
    fancier_AddStyle(self, 152, 65, newstyle);
    /* Wrapped this style around 65 specific characters */

    style_SetName(newstyle, "stupidstyle");
    style_SetMenuName(newstyle, "Stupidity,That's My Style");
    stylesheet_Add(fancier_GetStyleSheet(self), newstyle);

    return(TRUE);
}

void fancier__FinalizeObject(c, self)
struct classheader *c;
struct fancier *self;
{
}

