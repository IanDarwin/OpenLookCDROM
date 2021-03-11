static char *hgghview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/hgghmenu/RCS/hgghview.c,v 1.1 1992/10/06 22:17:20 susan R6tape $";

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
#include <hgghview.eh>
#include <im.ih>
#include <view.ih>
#include <text.ih>
#include <textv.ih>
#include <lpair.ih>
#include <butt.ih>
#include <buttview.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>

void ToggleProc(), ToggleLpairViews();

static struct keymap *hgghview_keymap = NULL;
static struct menulist *hgghview_menulist = NULL;

boolean hgghview__InitializeClass(c)
struct classheader *c;
{
    struct proctable_Entry *proc = NULL;

    hgghview_keymap = keymap_New();
    hgghview_menulist = menulist_New();

    proc = proctable_DefineProc("hgghview-toggle", ToggleProc,
		&hgghview_classinfo, NULL,
		"toggles the two parts of the hgghview.");

    keymap_BindToKey(hgghview_keymap, "!", proc, 0);
    menulist_AddToML(hgghview_menulist, "Toggle Me!",
		      proc, NULL, 0);
    menulist_AddToML(hgghview_menulist, "HGGH,Toggle Me!",
		      proc, NULL, 0);
    return(TRUE);
}

boolean hgghview__InitializeObject(c, self)
struct classheader *c;
struct hgghview *self;
{
    struct text *t1 = text_New();
    struct text *t2 = text_New();
    struct textview *tv1 = textview_New();
    struct textview *tv2 = textview_New();
    struct butt *b = butt_New();
    struct buttview *bv = buttview_New();

    self->lp = lpair_New();
    textview_SetDataObject(tv1, t1);
    textview_SetDataObject(tv2, t2);
    buttview_SetDataObject(bv, b);
    text_InsertCharacters(t1, 0, "Hello, world!", 13);
    text_InsertCharacters(t2, 0, "Goodbye, world!", 15);
    butt_SetText(b, "Toggle");
    buttview_SetHitFunction(bv, ToggleLpairViews);
    buttview_SetRocks(bv, self, NULL);
    lpair_VSplit(self->lp, tv1, tv2, 50, 1);
    hgghview_VTFixed(self, bv, self->lp, 25, 1);
    textview_WantInputFocus(tv1, tv1);
    self->ks = keystate_Create(self, hgghview_keymap);
    self->ml = menulist_DuplicateML(hgghview_menulist, self);
    return(TRUE);
}

void hgghview__FinalizeObject(c, self)
struct classheader *c;
struct hgghview *self;
{
    lpair_Destroy(self->lp);
    keystate_Destroy(self->ks);
    menulist_Destroy(self->ml);
}

void
hgghview__PostKeyState(self, ks)
struct hgghview *self;
struct keystate *ks;
{
    self->ks->next = NULL;
    keystate_AddBefore(self->ks, ks);
    super_PostKeyState(self, self->ks);
}

void hgghview__PostMenus(self, ml)
struct hgghview *self;
struct menulist *ml;
{
    menulist_ClearChain(self->ml);
    if (ml) menulist_ChainAfterML(self->ml, ml, ml);
    super_PostMenus(self, self->ml);
}

static void ToggleProc(self, param)
struct hgghview *self;
long param;
{
    struct view *v1, *v2;

    v1 = lpair_GetNth(self->lp, 0);
    v2 = lpair_GetNth(self->lp, 1);
    lpair_SetNth(self->lp, 0, v2);
    lpair_SetNth(self->lp, 1, v1);
    lpair_WantUpdate(self->lp, self->lp);
    view_WantInputFocus(v2, v2);
}

static void ToggleLpairViews(self, ignored, b, action)
struct hgghview *self;
long ignored;
struct butt *b;
enum view_MouseAction action;
{
    if (action == view_LeftDown || action == view_RightDown) {
	ToggleProc(self, 0);
    }
}
