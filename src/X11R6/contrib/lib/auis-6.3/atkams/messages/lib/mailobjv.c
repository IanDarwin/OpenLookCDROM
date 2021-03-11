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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/mailobjv.c,v 1.3 1992/12/15 21:48:53 rr2b R6tape $";
#endif


 
#include <class.h>

#include <sbutton.ih>
#include "mailobjv.eh"
#include <mailobj.ih>
#include <bind.ih>
#include <menulist.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <ams.ih>
#include <complete.ih>
#include <message.ih>

static struct keymap *mailobjv_standardkeymap;
static struct menulist *mailobjv_standardmenulist;

void ChangeContents(self)
struct mailobjv *self;
{
    char ctype[300], fname[1000], Label[200];
    FILE *fp;
    struct mailobj *mo = (struct mailobj*) mailobjv_GetDataObject(self);

    if (mo->RawData
	&& !ams_GetBooleanFromUser(ams_GetAMS(), "Do you want to overwrite the data that is already here", FALSE)) {
	message_DisplayString(self, 10, "Use the LEFT mouse button to activate this object.");
	return;
    }
    if (completion_GetFilename(self, "Name of raw data file: ", "", fname, sizeof(fname), FALSE, TRUE) == -1 ) {
	return;
    }
    if (message_AskForString(self, 50, "Content-type: ", NULL, ctype, sizeof(ctype)) < 0) {
	return;
    }
    if (message_AskForString(self, 50, "Content-Description: ", NULL, Label, sizeof(Label)) < 0) {
	return;
    }
    fp = fopen(fname, "r");
    if (!fp) return;
    mailobj_ReadAlienMail(mo, ctype, NULL, fp, 0);
    mailobj_SetLabel(mo, 0, Label);
    fclose(fp);
}

static struct bind_Description mailobjv_standardbindings [] = {
    /* procname, keysequenece, key rock, menu string, menu rock, menu mask, proc, docstring, dynamic autoload */
    {"mailobjv-change-contents", "c", NULL, "Mail Object,Change Contents", NULL, NULL, ChangeContents, "Change the contents of a mailobj"},
    {NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL},
};

boolean mailobjv__InitializeClass(c) 
struct classheader *c;
{
    mailobjv_standardmenulist = menulist_New();
    mailobjv_standardkeymap = keymap_New();
    bind_BindList(mailobjv_standardbindings, mailobjv_standardkeymap, mailobjv_standardmenulist, &mailobjv_classinfo);
    return(TRUE);
}

void
mailobjv__PostKeyState(self, ks)
struct mailobjv *self;
struct keystate *ks;
{
    if (!ks) return;
    keystate_AddBefore(self->mykeys, ks);
    super_PostKeyState(self, self->mykeys);
}

void mailobjv__PostMenus(self, ml)
struct mailobjv *self;
struct menulist *ml;
{
    menulist_ClearChain(self->mymenulist);
    if (ml) menulist_ChainAfterML(self->mymenulist, ml, ml);
    super_PostMenus(self, self->mymenulist);
}

boolean mailobjv__InitializeObject(c, self)
struct classheader *c;
struct mailobjv *self;
{
    self->mykeys = keystate_Create(self, mailobjv_standardkeymap);
    self->mymenulist = menulist_DuplicateML(mailobjv_standardmenulist, self);
    return(TRUE);
}

void mailobjv__FinalizeObject(c, self)
struct classheader *c;
struct mailobjv *self;
{
    menulist_Destroy(self->mymenulist);
    keystate_Destroy(self->mykeys);
}

boolean mailobjv__Touch(self, ind, action)
struct mailobjv *self;
int ind;
enum view_MouseAction action;
{
    struct mailobj *mo;

    struct sbutton *b=mailobjv_ButtonData(self);

    sbutton_GetHitFuncRock(b)=action;
    
    mo = (struct mailobj*) mailobjv_GetDataObject(self);
    if (action == view_LeftUp  && mo->RawData) {
	mailobj_RunMetamail(mo);
    } else if(action == view_LeftUp || action == view_RightUp) {
	if (action == view_RightUp || ams_GetBooleanFromUser(ams_GetAMS(), "There is no data here yet.  Do you want to read in data from a file", FALSE)) {
	    ChangeContents(self);
	}
    }
    return super_Touch(self, ind, action);
}
