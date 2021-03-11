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


 



enum palette_location {palette_LEFT = 0, palette_TOP = 1, palette_RIGHT = 2, palette_BOTTOM = 3};

enum palette_autoselect {palette_NONE = 0, palette_TOGGLE = 1, palette_FOLLOWMOUSE = 2, palette_EXCLUSIVE = 3};

enum palette_itemtype {palette_ICON = 0, palette_VIEW = 1, palette_STRING = 2};

struct palette_item {
    struct palette *palette;                /* The palette we are in. */
    enum palette_itemtype type;             /* The type of item. */
    union palette_iteminfo {
        struct {                            /* For palette_ICON: */
            struct fontdesc *font;          /*   The font this icon is in. */
            char ch;                        /*   The char in that font. */
        } icon;
        struct {                            /* For palette_VIEW: */
            struct view *view;              /*   The view we should display. */
            boolean wants_update:1;         /*   If the view wants an update. */
        } view;
        struct {                            /* For palette_STRING: */
            struct fontdesc *font;          /*   The font to use. */
            char *str;                      /*   The string to display. */
        } str;      
    } u;
    int pos;                                /* Relative position parameter. */
    long x, y, w, h;                        /* Rect for icon. */
    void (*fn)();                           /* Function to call. */
    long rock;                              /* Rock to pass to the function */
    enum palette_autoselect autoselect;     /* Automatic selection style */
    boolean selected:1;                     /* True if the item is selected. */
    boolean new_selected:1;                 /* True if the item should be selected. */
    
    struct palette_item *next;
};


class palette: view {

    classprocedures:
    Create(struct view *child, enum palette_location loc) returns struct palette *;
    InitializeObject(struct palette *self) returns boolean;
    FinalizeObject(struct palette *self);

    DestroyItem(struct palette_item *item);
    SelectItem(struct palette_item *item);
    DeselectItem(struct palette_item *item);

    overrides:
    FullUpdate(enum view_UpdateType type, long left, long top, long width, long height);
    Update();
    Hit (enum view_MouseAction action, long x, long y, long numberOfClicks) returns struct view *;

    WantUpdate(struct view *requestor);

    methods:
    SetLocation(enum palette_location loc);
    SetChild(struct view *child);

    AddIcon(struct fontdesc *font, int ch, int pos, void (*fn)(), long rock, enum palette_autoselect autoselect) returns struct palette_item *;
    AddView(struct view *view, int pos, void (*fn)(), long rock, enum palette_autoselect autoselect) returns struct palette_item *;
    AddString(struct fontdesc *font, char *str, int pos, void (*fn)(), long rock, enum palette_autoselect autoselect) returns struct palette_item *;

    macros:
    GetRock(item) ((item)->rock)
    SetRock(item, newrock) ((item)->rock = (newrock))
    GetAutoSelect(item) ((item)->auto)
    SetAutoSelect(item, sel) ((item)->auto = (sel))
    Selected(item) ((item)->selected)

    macromethods:
    GetLocation() ((self)->loc)
    GetChild() ((self)->child)

    data:
    enum palette_location loc;
    struct palette_item *items, *hit_item;
    struct view *child;
    long border;
    boolean needs_full:1;
};
