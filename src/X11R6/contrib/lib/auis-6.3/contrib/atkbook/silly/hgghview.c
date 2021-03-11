static char *hgghview_rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/atkbook/silly/RCS/hgghview.c,v 1.1 1992/10/06 22:20:14 susan R6tape $";

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
#include <message.ih>
#include <hggh.ih>

enum message_CompletionCode CompleteChoice();
void ToggleProc(), ToggleLpairViews(), ChangeButton(), ChangeFromShortList(), ChangeFromLongList(), HelpChoice();

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

    proc = proctable_DefineProc("hgghview-change-button",
		ChangeButton, &hgghview_classinfo, NULL,
		"Changes the text in the toggling button.");
    menulist_AddToML(hgghview_menulist,
		"HGGH,Change Button Text", proc, NULL, 0);

    proc = proctable_DefineProc("hgghview-change-short-list",
		ChangeFromShortList, &hgghview_classinfo, NULL,
		"Change toggling text from short list.");
    menulist_AddToML(hgghview_menulist,
		"HGGH,Change From Short List", proc, NULL, 0);

    proc = proctable_DefineProc("hgghview-change-long-list",
		ChangeFromLongList, &hgghview_classinfo, NULL,
		"Changes the toggling text using a long list.");
    menulist_AddToML(hgghview_menulist,
		"HGGH,Change From Long List", proc, NULL, 0);

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

    self->bv = buttview_New();
    self->b = butt_New();
    self->lp = lpair_New();
    textview_SetDataObject(tv1, t1);
    textview_SetDataObject(tv2, t2);
    buttview_SetDataObject(self->bv, self->b);
    text_InsertCharacters(t1, 0, "Hello, world!", 13);
    text_InsertCharacters(t2, 0, "Goodbye, world!", 15);
    butt_SetText(self->b, "Toggle");
    buttview_SetHitFunction(self->bv, ToggleLpairViews);
    buttview_SetRocks(self->bv, self, NULL);
    lpair_VSplit(self->lp, tv1, tv2, 50, 1);
    hgghview_VTFixed(self, self->bv, self->lp, 25, 1);
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
    butt_Destroy(self->b);
    buttview_Destroy(self->bv);
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

static void ChangeButton(self, param)
struct hgghview *self;
long param;
{
    char Buf[250];

    if (message_AskForString(self, 50,
		"Enter new text for button: ",
		NULL, Buf, sizeof(Buf)) >= 0) {
	butt_SetText(self->b, Buf);
	butt_NotifyObservers(self->b,
		observable_OBJECTCHANGED);
	message_DisplayString(self, 10,
		"Changed the button text as requested.");
    }
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
    view_WantInputFocus(v2,v2);
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


static void ChangeFromShortList(self, param)
struct hgghview *self;
long param;
{
    long result;
    static char *ShortList[] = {
	"hello",
	"goodbye",
	"kick me",
	"press here",
	"I am a button",
	"Impeach Nixon",
	NULL
    };

    if (message_MultipleChoiceQuestion(self, 50,
		"Choose new text for button: ",
		1, &result, ShortList, NULL) >= 0) {
	butt_SetText(self->b, ShortList[result]);
	butt_NotifyObservers(self->b,
		observable_OBJECTCHANGED);
	message_DisplayString(self, 10,
		"Changed the button text as requested.");
    }
}

static void ChangeFromLongList(self, param)
struct hgghview *self;
long param;
{
    char Buf[500];

    if (message_AskForStringCompleted(self, 50,
		"Enter new text for button: ",
		NULL, Buf, sizeof(Buf), NULL, CompleteChoice,
		HelpChoice, 0, message_MustMatch) >= 0) {
	butt_SetText(self->b, Buf);
	butt_NotifyObservers(self->b,
		observable_OBJECTCHANGED);
	message_DisplayString(self, 10,
		"Changed the button text as requested.");
    }
}

static char  *LongList[] = {
    "red",
    "yellow",
    "blue",
    "black",
    "brown",
    "beige",
    "mauve",
    "purple",
    "pink",
    "green",
    "silver",
    "orange",
    "gray",
    "white",
    "blue-green",
};
#define LONGLISTSIZE (sizeof(LongList) / sizeof(char *))

static void HelpChoice(partial, dummy, helpTextFunction, helpTextRock)
char *partial;
long dummy; /* This was the tenth (penultimate)
	      argument to AskForStringCompleted */
int (*helpTextFunction)();
long helpTextRock;
{
    int i, len;
    len = strlen(partial);
    for (i=0; i<LONGLISTSIZE;++i) {
	if (!strncmp(partial, LongList[i], len)) {
	    (*helpTextFunction)(helpTextRock,
		message_HelpListItem, LongList[i], NULL);
	}
    }
}
static enum message_CompletionCode CompleteChoice(part, dummy, buf, size)
char *part, *buf;
long dummy, size;
{
    int matches = 0, i, plen, minmatch = -1, minmatchlen, newct;
    int DidMatch[LONGLISTSIZE];
    char partial[1000];

    strncpy(partial, part, sizeof(partial));
    /* previous line is in case part == buf */
    *buf = '\0';
    plen = strlen(partial);
    /* First, go through marking (in DidMatch) which ones
      match at all, keeping track of the shortest match
      in minmatch/minmatchlen. */
    for (i=0; i<LONGLISTSIZE; ++i) {
	DidMatch[i] = !strncmp(partial, LongList[i], plen);
	if (DidMatch[i]) {
	    ++matches;
	    if (minmatch < 0) {
		minmatch = i;
		minmatchlen = strlen(LongList[i]);
	    } else {
		newct = matchct(LongList[i],
				LongList[minmatch]);
		if (newct < minmatchlen) minmatchlen = newct;
		if (strlen(LongList[i]) <
		    strlen(LongList[minmatch])) {
		    minmatch = i;
		}
	    }
	}
    }
    if (matches < 1) {
	/* If there were no matches, we look for the
	    longest partial match and use it to set the
	    string before returning message_Invalid */
	int bestmatch = 0, dum;

	for (i=0; i<LONGLISTSIZE; ++i) {
	    dum = matchct(partial, LongList[i]);
	    if (dum > bestmatch) {
		bestmatch = dum;
		if (bestmatch>size) bestmatch = size;
		strncpy(buf, LongList[i], bestmatch);
		buf[bestmatch] = '\0';
	    }
	}
	return(message_Invalid);
    } else if (matches == 1) {
	/* If there is only one match, we copy it
	 and return message_Complete */
	strncpy(buf, LongList[minmatch], size);
	buf[size] = '\0';
	return(message_Complete);
    } else {
	/* If there are multiple matches, we want to
	 return the shortest matching string.  If it is
	 completely matched that string, we want to return
	 message_CompleteValid, otherwise message_Valid. */
	int lim = (minmatchlen > size) ? size : minmatchlen;
	strncpy(buf, LongList[minmatch], lim);
	buf[lim] = '\0';
	if (minmatchlen == strlen(LongList[minmatch])) {
	    return(message_CompleteValid);
	}
	return(message_Valid);
    }
}

static int matchct(s1, s2)
char *s1, *s2;
{
    int ans = 0;
    while (*s1 && (*s1++ == *s2++)) ++ans;
    return(ans);
}


