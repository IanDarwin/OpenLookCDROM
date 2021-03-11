/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

#include <style.ih>
#include <envrment.ih>
#include <text.ih>
#include <cursor.ih>

/*
 * A call to panel_SetHandler(panel, HandlerProc, globalTag) is required.
 * The handler procedure receives three arguments
 * and must be defined like this:
 *
 * void HandlerProc(globalTag, tag, self)
 * char *globalTag, *tag;
 * struct panel *self;
 */

struct panel_Entry {
    struct panel_Entry *next;
    long pos;
    long len;
    char *tag;              /* general mem address */
};

struct key_Entry {
    struct key_Entry *next;
    char key;
    void (*proc)();
    long rock;
};

#define panel_VERSION   1

class panel: textview[textv] {

classprocedures:
    InitializeClass() returns boolean;
    InitializeObject(struct panel *self) returns boolean;
    FinalizeObject(struct panel *self);

methods:
    Add(char *item, char *tag, int showNow) returns struct panel_Entry *;
    Remove(struct panel_Entry *entry);
    RemoveAll();
    FreeAllTags(); /* destroys the client tag(rock) associated w/ a panel_Entry */

    /* MakeSelection highlights but does not call handler. */
    /* User clicking on item highlights and calls handler. */

    SelectNext();
    SelectPrevious();
    ClearSelection();
    MakeSelection(struct panel_Entry *entry);

    /* Each panel has its own key handlers for simple single-keystrokes. */
    /* Default keys may be reassigned or cancelled using NULL;  they are: */
    /* ^P, P, p, B, and b are bound to do a SelectPrevious. */
    /* ^N, N, n, F, and f are bound to do a SelectNext. */
    /* The proc must be defined like: */
    /*   void Proc(whatever rock, struct panel *receivedBy, char c) */
    /* Things bound to ESC or ^X will not work! */

    AssignKey(char c, void (*proc)(), long rock);

macromethods:
    SetHandler(void (*proc)(), char *gt) \
        ((self)->handler = (proc), (self)->globalTag = (gt))

    /* New highlight style takes effect upon next selection */

    SetHighlightStyle(struct style *s) ((self)->highlightStyle = (s))
    SetCursorFont(struct fontdesc *cf) ((self)->iconFont = (cf))
    SetCursorIcon(char c) ((self)->icon = (c))

    /* Returns currently selected panel entry, NULL if */
    /* nothing is currently selected. */

    CurrentHighlight() ((self)->highlightEntry)

    /* Note: SetBorder, SetCursorFont, SetCursorIcon, and */
    /* SetDefaultStyle require a FullUpdate and should usually */
    /* be used only during initialization. */

    /* Useful inheritance:  SetDefaultStyle(pv, style) */
    /* Useful inheritance: SetBorder(pv, hpix, vpix); */

    /* panel_Entry access macros */

    EntryRoot() ((self)->panelList)
    EntryNext(pe) ((pe)->next)
    EntryTag(pe) ((pe)->tag)

overrides:
    Hit(enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;
    FullUpdate(enum view_UpdateType type, long x, long y, long w, long h);
    PostMenus(struct menulist *ml);
    PostKeyState(struct keystate *ks);
    SetDataObject(struct dataobject *dataObj);

data:
    struct panel_Entry *panelList;
    struct key_Entry *keyList;

    void (*handler)();
    char *globalTag;

    struct keystate *keystate;

    struct text *text;
    boolean ourText;    /* This is TRUE if text is (de)allocated by panel.  If the client sets his own text dataobj. we do nothing with self->text and self->ourText is FALSE.  This is for backward compatibility with existing applications that rely on panel being a text/textview pair-- which is, of course, a no-no in an object-oriented environ that supports multiple views on a single dataobject. */

    struct fontdesc *iconFont;
    char icon;
    struct cursor *cursor;

    struct style *highlightStyle;
    struct panel_Entry *highlightEntry;
    struct environment *highlightEnv;
};
