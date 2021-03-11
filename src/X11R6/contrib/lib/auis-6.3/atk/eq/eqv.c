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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/eq/RCS/eqv.c,v 2.11 1992/12/15 21:32:53 rr2b R6tape $";
#endif


 

/*
 * eqv.c
 * This module handles the view for eq.
 */


#include <class.h>
#include <eqv.eh>

#include <eq.ih>
#include <rectlist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <graphic.ih>
#include <view.ih>
#include <mark.ih>
#include <im.ih>

static int fudge = 5;		/* extra space: for visual balance and for cursor */
static struct keymap *eqviewKeymap;
static struct menulist *eqviewMenus, *eqviewCutMenus;
static struct graphic *pat;

static int debug_flag = 0;
#define debug(f) if (debug_flag) { printf f; fflush(stdout); }

boolean eqview__InitializeObject(classID, self)
struct classheader *classID;
struct eqview *self;
{
    debug(("Init here\n"));

    self->off_x = self->off_y = 0;
    self->changed = EQVIEW_nothing;
    self->caret_x = self->caret_y = 0;
    self->selection_width = self->selection_height = -1;
    self->hasinputfocus = FALSE;
    self->embedded = TRUE;
    self->keystate = keystate_Create(self, eqviewKeymap);
    self->normalMenus = menulist_DuplicateML(eqviewMenus, self);
    self->cutMenus = menulist_DuplicateML(eqviewCutMenus, self);
    self->dot = NULL;
    self->filename = NULL;

    return TRUE;
}

void eqview__FinalizeObject(classID, self)
struct classheader *classID;
struct eqview *self;
{
    if (self->keystate)
	keystate_Destroy(self->keystate);
    if (self->normalMenus)
	menulist_Destroy(self->normalMenus);
    if (self->cutMenus)
	menulist_Destroy(self->cutMenus);
    if (self->filename)
	free(self->filename);
    mark_Destroy(self->dot);
}

void eqview__SetDataObject(self, dataObject)
struct eqview *self;
struct dataobject *dataObject;
{
    if (!class_IsTypeByName(class_GetTypeName(dataObject), "eq"))  {
	fprintf(stderr, "Incompatible dataobject associated with eqview\n");
	return;
    }
    super_SetDataObject(self, dataObject);
    self->dot = eq_CreateMark((struct eq *) dataObject, 0, 0);
    mark_SetStyle(self->dot, FALSE, FALSE);
    eqview_SetDotPosition(self, 4);
}

void eqview__FullUpdate(self, type, left, top, width, height)
struct eqview *self;
enum view_UpdateType type;
long left, top, width, height;
{
    debug(("FullUpdate here\n"));

    if(type == view_MoveNoRedraw){
	self->changed = EQVIEW_caret;
    } 
    else {
	self->changed = EQVIEW_everything;
    }
    eqview_Update(self);
}


void eqview__Update(self)
struct eqview *self;
{
    struct eq *eqptr = Eq(self);
    struct formula *first = eq_Access(eqptr, 0);
    struct rectangle rect;

    debug(("Update here\n"));

    /* undraw */
    switch(self->changed) {
    case EQVIEW_everything:
	eqview_SetTransferMode(self, graphic_COPY);
	rectangle_SetRectSize(&rect, eqview_GetLogicalLeft(self), eqview_GetLogicalTop(self), eqview_GetLogicalWidth(self), eqview_GetLogicalHeight(self));
	pat = eqview_WhitePattern(self);
	eqview_FillRect(self, &rect, pat);
	break;
    case EQVIEW_eq:
	eqview_DrawCaret(self);	/* sigh - caret extends outside following box */
	eqview_SetTransferMode(self, graphic_COPY);
	rectangle_SetRectSize(&rect, self->off_x+first->pos.x+first->min.x-3,
	    self->off_y+first->pos.y+first->min.y-1,
	    first->max.x-first->min.x+4,  first->max.y-first->min.y+2);
	pat = eqview_WhitePattern(self);
	eqview_FillRect(self, &rect, pat);
	break;
    case EQVIEW_caret:
	eqview_DrawCaret(self);
	break;
    case EQVIEW_nothing:
	break;
    }
	
    /* draw */
    switch (self->changed) {
    case EQVIEW_everything:
    case EQVIEW_eq:
	eqview_Format(self, eqptr, NIL, eq_Access(eqptr, 0), NIL, T_EQSTYLE);
	if (self->changed == EQVIEW_eq && first->max.y-first->min.y+2*fudge+1 > eqview_GetLogicalHeight(self)) {
	    eqview_WantNewSize(self, self);
	    self->changed = EQVIEW_everything;
	    return;
	}
	self->off_x = eqview_GetLogicalLeft(self) + fudge;
	self->off_y = eqview_GetLogicalTop(self) + fudge+(first->max.y-first->min.y)/2;
	first->hot.x =  self->off_x;
	first->hot.y = self->off_y;

	eqview_SetTransferMode(self, graphic_INVERT);
	eqview_Draw(self, eqptr, first, self->off_x, self->off_y);
	eqview_CalculateCaret(self);
	eqview_DrawCaret(self);
	break;
    case EQVIEW_caret:
	eqview_CalculateCaret(self);
	eqview_DrawCaret(self);
	break;
    case EQVIEW_nothing:
	break;
    }
    if (self->hasinputfocus) {
	if (mark_GetLength(self->dot) > 0) {
	    menulist_ClearChain(self->cutMenus);
	    eqview_PostMenus(self, self->cutMenus);
	}
	else if (mark_GetLength(self->dot) == 0) {
	    menulist_ClearChain(self->normalMenus);
	    eqview_PostMenus(self, self->normalMenus);
	}
    }
    self->changed = EQVIEW_nothing;
}

void eqview__SetDotPosition(self, newp)
struct eqview *self;
long newp;
{
    long len = eq_Size(Eq(self));

    if (newp < 0)
	newp = 0;
    else {
	if (newp > len)
	    newp = len;
    }
    mark_SetPos(self->dot, newp);
    eqview_Changed(self, EQVIEW_caret);
    eqview_WantUpdate(self, self);
}

void eqview__SetDotLength(self, newl)
struct eqview *self;
long newl;
{
    if (newl < 0)
	newl = 0;
    mark_SetLength(self->dot, newl);
    eqview_Changed(self, EQVIEW_caret);
    eqview_WantUpdate(self, self);
}

long eqview__GetDotPosition(self)
struct eqview *self;
{
    return mark_GetPos(self->dot);
}

long eqview__GetDotLength(self)
struct eqview *self;
{
    return mark_GetLength(self->dot);
}

struct eqview *eqview__Hit(self, action, x, y, clicks)
struct eqview *self;
enum view_MouseAction action;
long x, y, clicks;
{
    int i, pos, len;
    struct eq *eqptr = Eq(self);

    debug(("Hit here\n"));

    if (action == view_LeftDown) {
	i = eqview_Find(self, eqptr, x, y, 0);
	eqview_SetDotPosition(self, i);
	eqview_SetDotLength(self, 0);
    }
    else if (action == view_LeftUp || action == view_RightDown) {
	pos = eqview_GetDotPosition(self);
	len = eqview_GetDotLength(self);
	i = eqview_Find(self, eqptr, x, y, pos);
	if (i < pos+len/2)
	    len = pos + len - i,  pos = i;
	else
	    len = i - pos;
	eqview_SetDotPosition(self, pos);
	eqview_SetDotLength(self, len);
    }

    eqview_Changed(self, EQVIEW_caret);
    eqview_WantUpdate(self, self);
    eqview_WantInputFocus(self, self);

    return self;
}

void eqview__ReceiveInputFocus(self)
struct eqview *self;
{
    debug(("ReceiveInputFocus here\n"));

    self->hasinputfocus = TRUE;
    eqview_Changed(self, EQVIEW_caret);
    self->keystate->next = NULL;
    eqview_PostKeyState(self, self->keystate);
    eqview_WantUpdate(self, self);
}


void eqview__LoseInputFocus(self)
struct eqview *self;
{
    debug(("LoseInputFocus here\n"));

    self->hasinputfocus = FALSE;
    eqview_Changed(self, EQVIEW_caret);
    eqview_WantUpdate(self, self);
}


enum view_DSattributes eqview__DesiredSize(self, width, height, pass, widthp, heightp)
struct eqview *self;
long width, height;
enum view_DSpass pass;
long *widthp, *heightp;
{
    struct formula *first;
    struct eq *eqptr = Eq(self);

    if (eqptr == NULL)
	eqptr = eq_New();

    first = eq_Access(eqptr, 0);
    eqview_Format(self, eqptr, NIL, first, NIL, T_EQSTYLE);

    debug(("DesiredSize here\n"));

    *widthp = width;
    *heightp = first->max.y - first->min.y + 1 + 2*fudge;

    return(view_Fixed);
}

boolean eqview__InitializeClass(classID)
struct classheader *classID;
{
    extern struct keymap *eqview_InitKeyMap();

    eqviewKeymap = eqview_InitKeyMap(&eqview_classinfo, &eqviewMenus, &eqviewCutMenus);

    return TRUE;
}

void eqview__Print(self, file, process, final, toplevel)
struct eqview *self;
FILE *file;
char *process;
char *final;
int toplevel;
{
    struct eq *eqptr = Eq(self);

    if (strcmp(process, "troff") == 0) {
	eq_Parse(eqptr, file, 'e');
	fprintf(file,".EQ\ndelim off\n.EN\n");
    }    
}

struct view *eqview__GetApplicationLayer(self)
struct eqview *self;
{
    self->embedded = FALSE;
    eqview_WantInputFocus(self, self);

    return (struct view *) self;
}
