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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/supportviews/RCS/palette.c,v 2.11 1992/12/15 21:42:39 rr2b R6tape $";
#endif


 

#include <class.h>
#include <fontdesc.ih>
#include <graphic.ih>
#include <view.ih>
#include <palette.eh>

static struct palette_item *FreeList;

boolean palette__InitializeObject(classID, self)
struct classheader *classID;
struct palette *self;
{
    self->loc = palette_LEFT;
    self->items = NULL;
    self->hit_item = NULL;
    self->child = NULL;
    self->border = -1;
    self->needs_full = 0;

    return TRUE;
}

struct palette *palette__Create(classID, child, loc)
struct classheader *classID;
struct view *child;
enum palette_location loc;
{
    struct palette *new = palette_New();

    palette_SetLocation(new, loc);
    palette_SetChild(new, child);

    return new;
}

void palette__FinalizeObject(classID, self)
struct classheader *classID;
struct palette *self;
{
    struct palette_item *p;

    if ((p = self->items) != NULL)
        while (1)
            if (p->next != NULL) {
                p->next = FreeList;
                FreeList = self->items;
                break;
            }
            else
                p = p->next;

    palette_SetChild(self, NULL);
}


void palette__DestroyItem(classID, item)
struct classheader *classID;
struct palette_item *item;
{
    struct palette *self = item->palette;
    struct palette_item *ptr, **prev;

    if (self == NULL)
        /* Already been deleted. */
        return;

    for (prev = &self->items; (ptr = *prev) != NULL; prev = &ptr->next)
        if (ptr == item) {
            *prev = ptr->next;

            ptr->next = FreeList;
            FreeList = ptr;
            ptr->palette = NULL;

            if (self->hit_item == item)
                self->hit_item = NULL;

            self->needs_full = 1;
            palette_WantUpdate(self, self);

            return;
        }

    /* Something is very wrong. The item said it was in the palette, but the palette didn't know about the item. */
    abort();
}

void palette__SelectItem(classID, item)
struct classheader *classID;
struct palette_item *item;
{
    if (item->new_selected)
        return;
    
    item->new_selected = TRUE;
    palette_WantUpdate(item->palette, item->palette);
}

void palette__DeselectItem(classID, item)
struct classheader *classID;
struct palette_item *item;
{
    if (!item->new_selected)
        return;
    
    item->new_selected = FALSE;
    palette_WantUpdate(item->palette, item->palette);
}

void palette__FullUpdate(self, type, left, top, w, h)
struct palette *self;
enum view_UpdateType type;
long left, top, w, h;
{
    struct fontdesc_charInfo info;
    int maxwidth, maxheight, items, columns, col, x, y, tmp;
    long width, height, strwidth, strheight;
    struct palette_item *item;
    struct rectangle child;
    struct graphic *black;

    if (type != view_FullRedraw && type != view_LastPartialRedraw)
        return;

    self->needs_full = 0;

    width = palette_GetLogicalWidth(self);
    height = palette_GetLogicalHeight(self);

    maxwidth = maxheight = 20;
    items = 0;
    for (item = self->items; item != NULL; item = item->next) {
        switch (item->type) {
            case palette_ICON:
                fontdesc_CharSummary(item->u.icon.font, palette_GetDrawable(self), item->u.icon.ch, &info);
                if (info.width > maxwidth)
                    maxwidth = info.width;
                if (info.height > maxheight)
                    maxheight = info.height;
                break;
            case palette_VIEW:
                break;
            case palette_STRING:
                fontdesc_StringSize(item->u.str.font, palette_GetDrawable(self), item->u.str.str, &strwidth, &strheight);
                maxwidth += strwidth;
                break;
        }
        items++;
    }
    maxwidth += 2;
    maxheight += 2;

    if (height > 0 && width > 0 && items > 0) {
        palette_SetTransferMode(self, graphic_COPY);
        switch (self->loc) {
            case palette_LEFT:
                columns = (items * maxheight + height - 1) / height;
                self->border = maxwidth*columns;
                palette_MoveTo(self, self->border, 0);
                palette_DrawLineTo(self, self->border, height);
                rectangle_SetRectSize(&child, self->border + 1, 0, width - self->border - 1, height);
                col = 1;
                x = 0;
                y = 0;
                for (item = self->items; item != NULL; item = item->next) {
                    item->x = x;
                    item->y = y;
                    item->w = maxwidth;
                    item->h = maxheight;
                    if (++col > columns) {
                        x = 0;
                        y += maxheight;
                        col = 1;
                    }
                    else
                        x += maxwidth;
                }
                break;
            case palette_TOP:
                tmp = width/maxwidth;
                columns = (items + tmp - 1) / tmp;
                self->border = maxheight*columns;
                palette_MoveTo(self, 0, self->border);
                palette_DrawLineTo(self, width, self->border);
                rectangle_SetRectSize(&child, 0, self->border + 1, width, height - self->border - 1);
                x = 0;
                y = 0;
                for (item = self->items; item != NULL; item = item->next) {
                    item->x = x;
                    item->y = y;
                    item->w = maxwidth;
                    item->h = maxheight;
                    x += maxwidth;
                    if (x > width - maxwidth) {
                        y += maxheight;
                        x = 0;
                    }
                }
                break;
            case palette_RIGHT:
                columns = (items * maxheight + height - 1) / height;
                self->border = width - maxwidth*columns;
                palette_MoveTo(self, self->border, 0);
                palette_DrawLineTo(self, self->border, height);
                rectangle_SetRectSize(&child, 0, 0, self->border, height);
                col = 1;
                x = self->border + 1;
                y = 0;
                for (item = self->items; item != NULL; item = item->next) {
                    item->x = x;
                    item->y = y;
                    item->w = maxwidth;
                    item->h = maxheight;
                    if (++col > columns) {
                        x = self->border + 1;
                        y += maxheight;
                        col = 1;
                    }
                    else
                        x += maxwidth;
                }
                break;
            case palette_BOTTOM:
                tmp = width/maxwidth;
                columns = (items + tmp - 1) / tmp;
                self->border = height - maxheight*columns;
                palette_MoveTo(self, 0, self->border);
                palette_DrawLineTo(self, width, self->border);
                rectangle_SetRectSize(&child, 0, 0, width, self->border);
                x = 0;
                y = self->border + 1;
                for (item = self->items; item != NULL; item = item->next) {
                    item->x = x;
                    item->y = y;
                    item->w = maxwidth;
                    item->h = maxheight;
                    x += maxwidth;
                    if (x > width - maxwidth) {
                        y += maxheight;
                        x = 0;
                    }
                }
                break;
        }
        black = palette_BlackPattern(self);
        for (item = self->items; item != NULL; item = item->next) {
            switch (item->type) {
                case palette_ICON:
                    palette_MoveTo(self, item->x + item->w/2, item->y + item->h/2);
                    palette_SetFont(self, item->u.icon.font);
                    palette_DrawText(self, &item->u.icon.ch, 1, graphic_NOMOVEMENT);
                    break;
                case palette_VIEW:
                    item->u.view.wants_update = FALSE;
                    view_InsertViewSize(item->u.view.view, self, item->x, item->y, item->w, item->h);
                    view_FullUpdate(item->u.view.view, view_FullRedraw, 0, 0, item->w, item->h);
                    break;
                case palette_STRING:
                    palette_MoveTo(self, item->x + item->w/2, item->y + item->h/2);
                    palette_SetFont(self, item->u.str.font);
                    palette_DrawString(self, item->u.str.str, graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE);
                    break;
            }
            if (item->selected = item->new_selected) {
                palette_SetTransferMode(self, graphic_INVERT);
                palette_FillRectSize(self, item->x, item->y, item->w,   item->h, black);
                palette_SetTransferMode(self, graphic_COPY);
            }
        }
    }
    else {
        self->border = -1;
        rectangle_SetRectSize(&child, 0, 0, width, height);
    }

    view_InsertView(self->child, self, &child);
    view_FullUpdate(self->child, view_FullRedraw, 0, 0, rectangle_Width(&child), rectangle_Height(&child));
}

void palette__Update(self)
struct palette *self;
{
    struct palette_item *item;
    struct graphic *black;

    if (self->needs_full) {
        palette_SetTransferMode(self, graphic_SOURCE);
        palette_EraseVisualRect(self);
        palette_FullUpdate(self, view_FullRedraw, 0, 0, palette_GetLogicalWidth(self), palette_GetLogicalHeight(self));
    }
    else {
        palette_SetTransferMode(self, graphic_INVERT);
        black = palette_BlackPattern(self);
        for (item = self->items; item != NULL; item = item->next) {
            if (item->type == palette_VIEW && item->u.view.wants_update) {
                /* If they want an update, we need to re-invert them. */
                item->u.view.wants_update = FALSE;
                if (item->selected) {
                    palette_FillRectSize(self, item->x, item->y, item->w, item->h, black);
                    item->selected = FALSE;
                }
                view_Update(item->u.view.view);
            }
            if (item->selected != item->new_selected) {
                palette_FillRectSize(self, item->x, item->y, item->w, item->h, black);
                item->selected = item->new_selected;
            }
        }
    }
}

struct view *palette__Hit(self, action, x, y, numclicks)
struct palette *self;
enum view_MouseAction action;
long x, y, numclicks;
{
    struct palette_item *item, *ptr;

    switch (action) {
        case view_LeftDown:
        case view_RightDown:
            switch (self->loc) {
                case palette_LEFT:
                    if (x > self->border)
                        if (self->child != NULL)
                            return view_Hit(self->child, action, (x - self->border - 1), y, numclicks);
                        else
                            return (struct view *)self;
                    break;
                case palette_TOP:
                    if (y > self->border)
                        if (self->child != NULL)
                            return view_Hit(self->child, action, x, (y - self->border - 1), numclicks);
                        else
                            return (struct view *)self;
                    break;
                case palette_RIGHT:
                    if (x <= self->border)
                        if (self->child != NULL)
                            return view_Hit(self->child, action, x, y, numclicks);
                        else
                            return (struct view *)self;
                    break;
                case palette_BOTTOM:
                    if (y <= self->border)
                        if (self->child != NULL)
                            return view_Hit(self->child, action, x, y, numclicks);
                        else
                            return (struct view *)self;
                    break;
                default:
                    if (self->child != NULL)
                        return view_Hit(self->child, action, x, y, numclicks);
                    else
                        return (struct view *)self;
                    /* break; */
            }

            for (item = self->items; item != NULL; item = item->next)
                if (x >= item->x && y >= item->y && x < (item->x + item->w) && y < (item->y + item->h))
                    break;

            self->hit_item = item;

            if (item != NULL) {
                switch (item->autoselect) {
                    case palette_FOLLOWMOUSE:
                        palette_SelectItem(item);
                        break;
                    case palette_TOGGLE:
                        if (palette_Selected(item))
                            palette_DeselectItem(item);
                        else
                            palette_SelectItem(item);
                        break;
                    case palette_EXCLUSIVE:
                        for (ptr = self->items; ptr != NULL; ptr = ptr->next)
                            if (palette_Selected(ptr))
                                palette_DeselectItem(ptr);
                        palette_SelectItem(item);
                        break;
                    default:
                        break;
                }
                (*item->fn)(item->rock, item, action, numclicks);
            }

            return (struct view *)self;
            /* break; */

        case view_LeftUp:
        case view_RightUp:
            if ((item = self->hit_item) != NULL) {
                if (item->autoselect == palette_FOLLOWMOUSE)
                    palette_DeselectItem(item);
                (*item->fn)(item->rock, item, action, numclicks);
            }
            return NULL;
            /* break; */

        default:
            return (struct view *)self;
    }
}

void palette__WantUpdate(self, requestor)
struct palette *self;
struct view *requestor;
{
    struct palette_item *item;

    for (item = self->items; item != NULL; item = item->next)
        if (item->type == palette_VIEW && item->u.view.view == requestor) {
            item->u.view.wants_update = TRUE;
            super_WantUpdate(self, self);
            return;
        }
    super_WantUpdate(self, requestor);
}

void palette__SetLocation(self, loc)
struct palette *self;
enum palette_location loc;
{
    if (self->loc != loc) {
        self->loc = loc;
        self->needs_full = 1;
        palette_WantUpdate(self, self);
    }
}

void palette__SetChild(self, child)
struct palette *self;
struct view *child;
{
    if (self->child != NULL)
        self->child->parent = NULL;

    self->child = child;

    if (child != NULL)
        child->parent = (struct view *)self;
}

static struct palette_item *palette_AddItem(self, info, pos, fn, rock, autoselect)
struct palette *self;
union palette_iteminfo info;
int pos;
void (*fn)();
long rock;
enum palette_autoselect autoselect;
{
    struct palette_item *item, *ptr, **prev;

    if (FreeList == NULL)
        item = (struct palette_item *)malloc(sizeof(struct palette_item));
    else {
        item = FreeList;
        FreeList = FreeList->next;
    }

    item->palette = self;
    item->u = info;
    item->pos = pos;
    item->x = item->y = item->w = item->h = 0;
    item->fn = fn;
    item->rock = rock;
    item->autoselect = autoselect;
    item->selected = item->new_selected = FALSE;

    for (prev = &self->items; (ptr = *prev) != NULL; prev = &ptr->next)
        if (pos < ptr->pos)
            break;

    item->next = ptr;
    *prev = item;

    self->needs_full = TRUE;
    palette_WantUpdate(self, self);

    return item;
}

struct palette_item *palette__AddIcon(self, font, ch, pos, fn, rock, autoselect)
struct palette *self;
struct fontdesc *font;
int ch, pos;
void (*fn)();
long rock;
enum palette_autoselect autoselect;
{
    union palette_iteminfo info;

    info.icon.font = font;
    info.icon.ch = ch;

    return palette_AddItem(self, info, pos, fn, rock, autoselect);
}

struct palette_item *palette__AddView(self, view, pos, fn, rock, autoselect)
struct palette *self;
struct view *view;
int pos;
void (*fn)();
long rock;
enum palette_autoselect autoselect;
{
    union palette_iteminfo info;

    info.view.view = view;
    info.view.wants_update = FALSE;

    return palette_AddItem(self, info, pos, fn, rock, autoselect);
}

struct palette_item *palette__AddString(self, font, str, pos, fn, rock, autoselect)
struct palette *self;
struct fontdesc *font;
char *str;
int pos;
void (*fn)();
long rock;
enum palette_autoselect autoselect;
{
    union palette_iteminfo info;

    info.str.font = font;
    info.str.str = str;

    return palette_AddItem(self, info, pos, fn, rock, autoselect);
}
