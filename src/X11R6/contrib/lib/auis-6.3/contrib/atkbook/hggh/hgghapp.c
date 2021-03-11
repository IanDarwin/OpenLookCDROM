static char *hgghapp_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hggh/RCS/hgghapp.c,v 1.1 1992/10/06 22:14:59 susan R6tape $";

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
#include <hgghapp.eh>
#include <im.ih>
#include <view.ih>
#include <text.ih>
#include <textv.ih>
#include <lpair.ih>
#include <butt.ih>

void ToggleLpairViews();

boolean hgghapp__InitializeObject(c, self)
struct classheader *c;
struct hgghapp *self;
{
    hgghapp_SetMajorVersion(self, 1);
    hgghapp_SetMinorVersion(self, 0);
    return(TRUE);
}

void hgghapp__FinalizeObject(c, self)
struct classheader *c;
struct hgghapp *self;
{
}

boolean hgghapp__Start(self)
struct hgghapp *self;
{
    struct im *im = im_Create(NULL);
    struct butt *b = butt_New();
    struct text *t1 = text_New();
    struct text *t2 = text_New();
    struct textview *tv1 = textview_New();
    struct textview *tv2 = textview_New();
    struct lpair *lp1 = lpair_New();
    struct lpair *lp2 = lpair_New();

    if (im == NULL || b == NULL || t1 == NULL || t2 == NULL
	 || tv1 == NULL || tv2 == NULL
	 || lp1 == NULL || lp2 == NULL) {
	return(FALSE);
    }
    textview_SetDataObject(tv1, t1);
    textview_SetDataObject(tv2, t2);
    text_InsertCharacters(t1, 0, "Hello, world!", 13);
    text_InsertCharacters(t2, 0, "Goodbye, world!", 15);
    butt_SetText(b, "Toggle");
    butt_SetHitFunction(b, ToggleLpairViews);
    butt_SetRocks(b, self, lp1);
    lpair_SetUp(lp1, tv1, tv2, 50, lpair_PERCENTAGE,
		 lpair_HORIZONTAL, TRUE);
    lpair_SetUp(lp2, b, lp1, 25, lpair_TOPFIXED,
		 lpair_HORIZONTAL, TRUE);
    im_SetView(im, lp2);
    textview_WantInputFocus(tv1, tv1);
    return(TRUE);
}

static void ToggleLpairViews(self, lp, b, action)
struct hgghapp *self;
struct lpair *lp;
struct butt *b;
enum view_MouseAction action;
{
    struct view *v1, *v2;

    if (action == view_LeftDown || action == view_RightDown) {
	v1 = lpair_GetNth(lp, 0);
	v2 = lpair_GetNth(lp, 1);
	lpair_SetNth(lp, 0, v2);
	lpair_SetNth(lp, 1, v1);
	lpair_WantUpdate(lp, lp);
	view_WantInputFocus(v2, v2);
    }
}
