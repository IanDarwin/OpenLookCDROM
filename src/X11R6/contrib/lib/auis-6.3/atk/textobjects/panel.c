/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
 * Permission to use, copy, modify, and distribute this software and its 
 * documentation for any purpose is hereby granted without fee, 
 * provided that the above copyright notice appear in all copies and that 
 * both that copyright notice, this permission notice, and the following 
 * disclaimer appear in supporting documentation, and that the names of 
 * IBM, Carnegie Mellon University, and other copyright holders, not be 
 * used in advertising or publicity pertaining to distribution of the software 
 * without specific, written prior permission.
 * 
 * IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
 * DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
 * ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
 * SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
 * DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
 * WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 *  $
*/

#ifndef NORCSID
#define NORCSID
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/textobjects/RCS/panel.c,v 1.14 1993/12/14 00:58:29 rr2b Exp $";
#endif

#include <class.h>

#include <cursor.ih>
#include <envrment.ih>
#include <fontdesc.ih>
#include <graphic.ih>
#include <bind.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <nstdmark.ih>
#include <proctbl.ih>
#include <style.ih>
#include <text.ih>
#include <textv.ih>
#include <im.ih>
#include <view.ih>
#include <message.ih>

#include <panel.eh>

static struct keymap *classKeymap;
static struct style *defaultHighlightStyle;
static struct style *defaultOverallStyle;
static struct fontdesc *defaultIconFont = NULL;
static char defaultIcon;

/*
 * Statics
 */

static void DestroyPanelList(pe)
register struct panel_Entry *pe;
{
    while (pe != NULL) {
        register struct panel_Entry *ne;
        ne = pe->next;
        free(pe);
        pe = ne;
    }
}

static void DestroyKeyList(ke)
register struct key_Entry *ke;
{
    while (ke != NULL) {
        register struct key_Entry *ne;
        ne = ke->next;
        free(ke);
        ke = ne;
    }
}

static void ClearHighlight(self)
register struct panel *self;
{
    register long pos, len;

    if (self->highlightEnv == NULL)
        return;

    pos = environment_Eval(self->highlightEnv);
    len = environment_GetLength(self->highlightEnv);

    environment_Delete(self->highlightEnv);
    text_RegionModified(self->text, pos, len);

    self->highlightEnv = NULL;
    self->highlightEntry = NULL;
}

static void SetupHighlight(self, entry)
register struct panel *self;
struct panel_Entry *entry;
{
    self->highlightEnv =
      text_AddStyle(self->text,
        entry->pos, entry->len,
          self->highlightStyle);

    self->highlightEntry = entry;
}

static void SelectAtPos(self, pos)
struct panel *self;
long pos;
{
    register struct panel_Entry *pe;

    for (pe = self->panelList; pe != NULL; pe = pe->next)
        if (pos >= pe->pos && pos <= pe->pos + pe->len)
            break;

    if (pe == NULL)
        return;

    ClearHighlight(self);
    SetupHighlight(self, pe);

    panel_SetDotPosition(self, pe->pos);
    panel_FrameDot(self, pe->pos);
    panel_SetDotLength(self, 0);
    panel_LoseInputFocus(self);
    im_ForceUpdate();

    if(self->handler)
	(*self->handler)(self->globalTag, pe->tag, self);
}

static void KeyDispatch(self, rock)
struct panel *self;
long rock;
{
    struct key_Entry *k = self->keyList;
    char c = (char) rock;

    while (k != NULL) {
        if (k->key == c)
            break;
        k = k->next;
    }

    if (k == NULL)
        return;

    (*k->proc)(k->rock, self, c);
}

static void ProcNext(rock, self, c)
long rock;
struct panel *self;
char c;
{
    panel_SelectNext(self);
}

static void ProcPrev(rock, self, c)
long rock;
struct panel *self;
char c;
{
    panel_SelectPrevious(self);
}

/*
 * Class Procedures
 */

boolean panel__InitializeClass(classID)
struct classheader *classID;
{
    defaultHighlightStyle = style_New();
    style_AddNewFontFace(defaultHighlightStyle, fontdesc_Bold);

    defaultOverallStyle = style_New();
    style_SetJustification(defaultOverallStyle, style_LeftJustified);
    style_SetFontSize(defaultOverallStyle, style_ConstantFontSize, 10);
    style_SetNewLeftMargin(defaultOverallStyle, style_LeftMargin,             16384, style_Inches);
    style_SetNewIndentation(defaultOverallStyle, style_LeftEdge,             -16384, style_Inches);

    defaultIconFont = fontdesc_Create("icon", fontdesc_Plain, 12);
    defaultIcon = 'R';

    classKeymap = keymap_New();

    {
        struct proctable_Entry *pte;
        unsigned char s[2];

        pte = proctable_DefineProc("key-dispatch",
          KeyDispatch, &panel_classinfo, NULL, NULL);

        s[1] = '\0';
        for (s[0] = '\0'; s[0] < 128; s[0]++)
            keymap_BindToKey(classKeymap, s, pte, s[0]);
    }

    return TRUE;
}

boolean panel__InitializeObject(classID, self)
struct classheader *classID;
register struct panel *self;
{
    struct style *newover=style_New();
    if(newover && defaultOverallStyle) style_Copy(defaultOverallStyle, newover);
    self->panelList = NULL;
    self->keyList = NULL;
    self->keystate = keystate_Create(self, classKeymap);
    self->text = text_New();
    super_SetDataObject(self,self->text);
    self->ourText = TRUE;
    self->handler = NULL;
    self->iconFont = defaultIconFont;
    self->icon = defaultIcon;
    self->cursor = cursor_Create(self);
    self->highlightStyle = defaultHighlightStyle;
    self->highlightEntry = NULL;
    self->highlightEnv = NULL;

    panel_SetBorder(self, 5, 5);
    if(newover) panel_SetDefaultStyle(self, newover);

    panel_AssignKey(self, 'P' - 64, ProcPrev, 0);
    panel_AssignKey(self, 'P', ProcPrev, 0);
    panel_AssignKey(self, 'p', ProcPrev, 0);
    panel_AssignKey(self, 'B', ProcPrev, 0);
    panel_AssignKey(self, 'b', ProcPrev, 0);
    panel_AssignKey(self, 'N' - 64, ProcNext, 0);
    panel_AssignKey(self, 'N', ProcNext, 0);
    panel_AssignKey(self, 'n', ProcNext, 0);
    panel_AssignKey(self, 'F', ProcNext, 0);
    panel_AssignKey(self, 'f', ProcNext, 0);

    return TRUE;
}

void panel__FinalizeObject(ClassID, self)
register struct classheader *ClassID;
register struct panel *self;
{
    ClearHighlight(self);   /* clears env */

    DestroyPanelList(self->panelList);
    DestroyKeyList(self->keyList);

    keystate_Destroy(self->keystate);
    if(self->ourText) text_Destroy(self->text);
    cursor_Destroy(self->cursor);
}

/*
 * Methods
 */

struct panel_Entry *panel__Add(self, item, tag, showNow)
register struct panel *self;
char *item;
char *tag;
int showNow;			/* make new selection visible now? */
{
    register struct panel_Entry *new;
    register long len;
    register long textlen;
    register struct text *text = NULL;
    char c = '\n';

    text = self->text;
    new = (struct panel_Entry*) malloc(sizeof (struct panel_Entry));
    if (!new)
	return (struct panel_Entry *)NULL;

    len = strlen(item);
    textlen = text_GetLength(text);

    new->pos = textlen;
    new->tag = tag;
    new->len = len;
    new->next = self->panelList;

    self->panelList = new;

    text_AlwaysInsertCharacters(text, textlen, item, len);
    text_AlwaysInsertCharacters(text, textlen + len, &c, 1);
    if (showNow)
	panel_FrameDot(self, textlen);
    text_NotifyObservers(text, 0);

    return new;
}

void panel__Remove(self, entry)
register struct panel *self;
register struct panel_Entry *entry;
{
    register long len;
    register struct panel_Entry *pe, **le;

    /* Find and unlink from list */

    le = &self->panelList;
    for (pe = *le; pe; le = &pe->next, pe = *le)
        if (pe == entry)
            break;

    if (pe == NULL)
        return;     /* Invalid entry */

    *le = entry->next;

    /* Clear highlight if the entry to be deleted is highlighted */
    if (entry == self->highlightEntry)
	ClearHighlight(self);

    /* Remove from display and deallocate */

    len = entry->len + 1;
    text_AlwaysDeleteCharacters(self->text, entry->pos, len);
    panel_WantUpdate(self, self);

    for (pe = self->panelList; pe != NULL; pe = pe->next)
        if (pe->pos >= entry->pos)
            pe->pos -= len;

    free(entry);
}

void panel__RemoveAll(self)
struct panel *self;
{
    ClearHighlight(self);
    DestroyPanelList(self->panelList);
    self->panelList = NULL;
    text_Clear(self->text);
    panel_WantUpdate(self, self);
}

void panel__SelectNext(self)
struct panel *self;
{
    long pos;

    if (self->highlightEnv == NULL)
        return;

    pos = environment_Eval(self->highlightEnv) +
      environment_GetLength(self->highlightEnv) + 1;

    SelectAtPos(self, pos);     /* Handles end of doc okay. */
}

void panel__SelectPrevious(self)
struct panel *self;
{
    long pos;

    if (self->highlightEnv == NULL)
        return;

    pos = environment_Eval(self->highlightEnv) - 2;

    SelectAtPos(self, pos);     /* Handles beg. of doc okay. */
}

void panel__ClearSelection(self)
struct panel *self;
{
    ClearHighlight(self);
    panel_WantUpdate(self, self);
}

void panel__MakeSelection(self, entry)
register struct panel *self;
register struct panel_Entry *entry;
{
    ClearHighlight(self);

    if (entry == (struct panel_Entry *)NULL)
	return;

    SetupHighlight(self, entry);
    panel_SetDotPosition(self, entry->pos);
    panel_SetDotLength(self, 0);
    panel_LoseInputFocus(self);
    panel_WantUpdate(self, self);
}

void panel__AssignKey(self, c, proc, rock)
struct panel *self;
char c;
void (*proc)();
long rock;
{
    struct key_Entry *k;

    k = self->keyList;

    while (k != NULL) {
        if (k->key == c)
            break;
        k = k->next;
    }

    if (k == NULL) {
        k = (struct key_Entry *) malloc(sizeof (struct key_Entry));
        k->next = self->keyList;
        self->keyList = k;
    }

    k->key = c;
    k->proc = proc;
    k->rock = rock;
}

/*
 * Overrides
 */

void panel__FullUpdate(self, type, x, y, w, h)
register struct panel *self;
enum view_UpdateType type;
long x, y, w, h;
{
    register struct graphic *graphic;

    graphic = panel_GetDrawable(self);
    cursor_SetGlyph(self->cursor, self->iconFont, self->icon);
    panel_PostCursor(self, &graphic->visualBounds, self->cursor);

    super_FullUpdate(self, type, x, y, w, h);
}

void panel__PostMenus(self, ml)
struct panel *self;
struct menulist *ml;
{
    /* Discard child menu postings */
    super_PostMenus(self,NULL);
}

void panel__PostKeyState(self, ks)
struct panel *self;
struct keystate *ks;
{
    /* Post our own keystate, discarding keystate from child */

    self->keystate->next = NULL;
    view_PostKeyState(self->header.view.parent, self->keystate);
}

struct view *panel__Hit(self, action, x, y, numberOfClicks)
register struct panel *self;
enum view_MouseAction action;
long x, y, numberOfClicks;
{
    super_Hit(self, action, x, y, numberOfClicks);
    SelectAtPos(self, panel_GetDotPosition(self));
    panel_SetDotLength(self, 0);
    panel_LoseInputFocus(self);
    return (struct view *)NULL;
}

void panel__FreeAllTags(self)
struct panel *self;
{
    register struct panel_Entry *e;
    register char *tag;
    
    if((e = panel_EntryRoot(self)) != NULL) 
        for(;e != NULL;e = panel_EntryNext(self,e))
            if((tag = panel_EntryTag(self,e)) != NULL)
                free(tag);
}

static void
AddLabels( self )
  struct panel		*self;
{
  static char		 answer[100];

  while(1) {
      *answer = '\0';
    if(message_AskForString(self,0,"Labels : ",NULL,answer,
			     sizeof(answer)) == -1)
	break;
    if((*answer == '\0') || !strcmp(answer,""))
	break;
    panel_Add(self,answer,0,FALSE);
  }
}

void
panel__SetDataObject( self, dataObj )
  struct panel	    *self;
  struct dataobject *dataObj;
{
  struct text	    *text = NULL;

  if(dataObj) {
    if((text = (struct text*)panel_GetDataObject(self)) != NULL && 
       (self->ourText == TRUE)) {
      self->ourText = FALSE;
      panel_FreeAllTags(self);
      panel_RemoveAll(self);
      text_Destroy(text);
    }
    super_SetDataObject(self, dataObj);
    self->text = (struct text *)dataObj;
  }
}
