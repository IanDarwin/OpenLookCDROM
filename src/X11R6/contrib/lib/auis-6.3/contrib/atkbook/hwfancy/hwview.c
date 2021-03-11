static char *hwview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hwfancy/RCS/hwview.c,v 1.1 1992/10/06 22:19:30 susan R6tape $";

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
#include <hwview.eh>
#include <graphic.ih>
#include <fontdesc.ih>

void hwview__FullUpdate(self, type, left, top, width, height)
struct hwview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    struct rectangle Rect;
    struct fontdesc *font;

    hwview_GetVisualBounds(self, &Rect);
    hwview_MoveTo(self, Rect.left + Rect.width/2,
		   Rect.top + Rect.height/2);
    hwview_DrawString(self, "Hello, world!",
	graphic_BETWEENTOPANDBASELINE
	| graphic_BETWEENLEFTANDRIGHT);

    font = fontdesc_Create("andy", fontdesc_Bold, 20);
    /* Note that creating this font on every 
      FullUpdate leaks core.  We should create
      it in InitializeObject and re-use it. */
    hwview_SetFont(self, font);

    hwview_MoveTo(self, Rect.left + Rect.width/2,
		   Rect.top + Rect.height/2 + 20);
    hwview_DrawText(self, "(big stuff)garbage", 11,
	graphic_BETWEENTOPANDBASELINE
	| graphic_BETWEENLEFTANDRIGHT);

    Rect.left = Rect.left + Rect.width/4;
    Rect.width = Rect.width/2;
    Rect.top = Rect.top + Rect.height/4;
    Rect.height = Rect.height/2;
    hwview_DrawRect(self, &Rect);

    hwview_SetLineWidth(self, 4);
    hwview_DrawRectSize(self, 10, 20, 25, 40);
    hwview_SetLineWidth(self, 1);

    hwview_DrawOvalSize(self, 30, 30, 25, 25);

    hwview_DrawRRectSize(self, 50, 50, 100, 50, 8, 8);

    hwview_DrawArc(self, &Rect, 45, 90);

    hwview_MoveTo(self, 5, 5);
    hwview_DrawLineTo(self, 25, 25);

    hwview_FillRectSize(self, Rect.left, 0, Rect.width,
		Rect.top, hwview_GrayPattern(self, 3, 10));
    hwview_FillRectSize(self, Rect.left + Rect.width/4,
		Rect.top/4, Rect.width/2, Rect.top/2,
		hwview_BlackPattern(self));

    hwview_MoveTo(self, Rect.left+Rect.width/4, Rect.top/4);
    hwview_SetTransferMode(self, graphic_WHITE);
    hwview_DrawString(self, "White Left",
		       graphic_ATLEFT | graphic_ATTOP);

    hwview_SetTransferMode(self, graphic_INVERT);
    hwview_MoveTo(self, Rect.left+(3*Rect.width)/4 - 30,
		   Rect.top/4);
    hwview_DrawString(self, "Inverted text example",
		       graphic_ATLEFT | graphic_ATTOP);
    
}


