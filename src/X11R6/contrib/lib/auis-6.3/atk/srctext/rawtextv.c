/* File rawtextv.c created by R S Kemmetmueller
  
   RawtextView, a Text mode with overstrike. */
/* Copyright 1988,1994 Carnegie Mellon University and IBM. All rights reserved.
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
static char rcsid[] = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/srctext/RCS/rawtextv.c,v 1.6 1994/02/22 20:14:18 rr2b Exp $";
#endif


#include <andrewos.h>
#include <class.h>

#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <proctbl.ih>
#include <bind.ih>
#include <message.ih>

#include "rawtext.ih"
#include "rawtextv.eh"

static struct keymap *raw_Map;
static struct menulist *raw_Menus;

void toggleOverstrike(); /*RSK92overstrike*/

static struct bind_Description rawtextBindings[]={
    {"rawtextview-toggle-overstrike-mode", "\033\034",0, NULL,0,0, toggleOverstrike, "Turn overstrike mode on or off."}, /*RSK92overstrike*/
    NULL
};

boolean rawtextview__InitializeClass(classID)
struct classheader *classID;
{
    raw_Menus = menulist_New();
    raw_Map = keymap_New();
    bind_BindList(rawtextBindings,raw_Map,raw_Menus,&rawtextview_classinfo);
    return TRUE;
}

boolean rawtextview__InitializeObject(classID, self)
struct classheader *classID;
struct rawtextview *self;
{
    self->raw_state = keystate_Create(self, raw_Map);
    self->raw_menus = menulist_DuplicateML(raw_Menus, self);
    return TRUE;
}

void rawtextview__FinalizeObject(classID, self)
struct classheader *classID;
struct rawtextview *self;
{
    keystate_Destroy(self->raw_state);
    menulist_Destroy(self->raw_menus);
}

void rawtextview__ReceiveInputFocus(self)
struct rawtextview *self;
{
    self->header.textview.hasInputFocus = TRUE;
    self->header.textview.keystate->next= NULL;
    self->raw_state->next= NULL;
    rawtextview_PostKeyState(self, keystate_AddBefore(self->raw_state, self->header.textview.keystate)); /* Yech, this makes no sense to me, but works */
    rawtextview_PostMenus(self, self->header.textview.menus); /* I find this particularly nauseating, but it seems to do the trick */
    menulist_SetMask(self->raw_menus, textview_NoMenus);
    if (rawtextview_GetEditor(self) == VI)
	if (rawtextview_GetVIMode(self) == COMMAND)
	    message_DisplayString(self, 0, "Command Mode");
        else
	    message_DisplayString(self, 0, "Input Mode");
    rawtextview_WantUpdate(self, self);
}

void rawtextview__PostMenus(self, menulist)
struct rawtextview *self;
struct menulist *menulist;
{
    menulist_ChainAfterML(self->raw_menus, menulist, 0);
    super_PostMenus(self, self->raw_menus);
}

/*RSK92overstrike*/
void toggleOverstrike(self,key)
struct rawtextview *self;
long key;
{
    struct rawtext *d = (struct rawtext *)self->header.view.dataobject;
    if (rawtext_IsInOverstrikeMode(d)) {
	rawtext_ChangeOverstrikeMode(d,FALSE);
	message_DisplayString(self,0,"Normal (insert) mode.");
    }
    else {
	rawtext_ChangeOverstrikeMode(d,TRUE);
	message_DisplayString(self,0,"Overstrike mode.");
    }
    rawtext_NotifyObservers(d,observable_OBJECTCHANGED);
}

/* Change Log
 
$Log: rawtextv.c,v $
 * Revision 1.6  1994/02/22  20:14:18  rr2b
 * Updated to the latest version.
 *
 * Revision 1.2  1993/12/02  17:02:01  rskm
 * bound keys the same (weird) way srctext does, so insets work (in review)
 *
 * Revision 1.1  1992/06/08  16:03:11  rskm
 * Initial revision
 *

*/
