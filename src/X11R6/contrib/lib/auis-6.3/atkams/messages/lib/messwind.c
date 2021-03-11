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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/messwind.c,v 1.13 1992/12/15 21:48:03 rr2b R6tape $";
#endif


 

#include <stdio.h>

#include <andrewos.h>
#include <cui.h>
#include <fdphack.h>
#include <hdrparse.h>
#include <mailconf.h>

#include <class.h>
#include <lpair.ih>
#include <message.ih>
#include <environ.ih>
#include <captions.ih>
#include <folders.ih>
#include <ams.ih>
#include <amsutil.ih>
#include <text.ih>
#include <text822v.ih>
#include <messwind.eh>
#include <sys/param.h>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <im.ih>
#include <view.ih>

static void DuplicateWindow(self)
struct messwind *self;
{
    struct messwind *m = (struct messwind *) messwind_New();

    ams_InstallInNewWindow(m, "messages", "Messages", environ_GetProfileInt("messages.width", -1), environ_GetProfileInt("messages.height", -1), m->folders);
}

static struct keymap *messwind_standardkeymap;
static struct menulist *messwind_standardmenulist;

boolean messwind__InitializeObject(c, mess)
struct classheader *c;
struct messwind *mess;
{
    int hbsplit;
    struct text *t;

    mess->mykeys = keystate_Create(mess, messwind_standardkeymap);
    mess->mymenulist = menulist_DuplicateML(messwind_standardmenulist, mess);

    t = text_New();
    text_SetReadOnly(t, TRUE);
    mess->bview = t822view_New();
    t822view_SetDataObject(mess->bview, t);
    mess->folders = folders_New();
    mess->captions = captions_New();
    folders_SetCaptions(mess->folders, mess->captions);
    captions_SetFolders(mess->captions, mess->folders);
    captions_SetBodies(mess->captions, mess->bview);
    t822view_SetCaptions(mess->bview, mess->captions);
    mess->SideBySide = amsutil_GetOptBit(EXP_SIDEBYSIDE) ? 1 : 0;

    hbsplit = environ_GetProfileInt("messages.headbodysplit", 50);
    if (hbsplit < 0) hbsplit *= -1;
    if (hbsplit > 100) hbsplit = 50;
    mess->capbodylp =lpair_New();
    lpair_VSplit(mess->capbodylp, captions_GetApplicationLayer(mess->captions), t822view_GetApplicationLayer(mess->bview), hbsplit, 1);
    mess->foldapplayer = folders_GetApplicationLayer(mess->folders);
    CheckVerticalHorizontal(mess);
    return(TRUE);
}

CheckVerticalHorizontal(mess)
struct messwind *mess;
{
    int foldpix = environ_GetProfileInt("messages.folderpixels", mess->SideBySide ? 200 : 80);

    if (mess->SideBySide) {
	messwind_HTFixed(mess, mess->foldapplayer, mess->capbodylp, foldpix, 1);
    } else {
	messwind_VTFixed(mess, mess->foldapplayer, mess->capbodylp, foldpix, 1);
    }
    folders_SetVeryNarrow(mess->folders, mess->SideBySide);
}

void messwind__ToggleSideBySide(mess)
struct messwind *mess;
{
    struct im *myim = messwind_GetIM(mess);
    struct view *v = myim ? im_GetInputFocus(myim) : NULL;

    mess->SideBySide = !mess->SideBySide;
    CheckVerticalHorizontal(mess);
    if (v) view_WantInputFocus(v, v);
    if (myim) im_RedrawWindow(myim);
}

void messwindCompound(mess, cmds)
struct messwindn *mess;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), mess, "messwind", cmds);
}

void messwindFoldersCommand(mess, cmds)
struct messwind *mess;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), mess->folders, "folders", cmds);
}
static char lastWindowWarning[] =
  "This is the last window.";
static char *lastWindowChoices[] = {
	"Continue Running",
	"Quit Messages",
	NULL};

#define lastWindow_CANCEL 0
#define lastWindow_QUIT   1

static void DeleteWindow(self)
struct messwind *self;
{
    if (ams_CountAMSViews() > 3) {
	ams_CommitState(FALSE, FALSE, FALSE, FALSE);
	if (messwind_GetIM(self)) im_Destroy(messwind_GetIM(self));
	messwind_Destroy(self);
    }
    else {
	long answer;
	if (message_MultipleChoiceQuestion(NULL, 0,
					   lastWindowWarning, lastWindow_CANCEL,
					   &answer, lastWindowChoices, NULL)
	    == -1)
	    return;
	switch(answer){
	    case lastWindow_CANCEL:
		return;

	    case lastWindow_QUIT :
		ams_CommitState(TRUE, FALSE, TRUE, TRUE);
	}
    }

}

static struct bind_Description messwind_standardbindings [] = {
    /* procname, keysequenece, key rock, menu string, menu rock, menu mask, proc, docstring, dynamic autoload */
    {"messwind-delete-window", "\030\004", NULL, "Delete Window~89", 0, NULL, DeleteWindow, "Delete messages window"},
    {"messwind-duplicate", "\0302", NULL, NULL, NULL, 0, DuplicateWindow, "Open another messages window"},
    {"messwind-compound-operation", NULL, NULL, NULL, NULL, 0, messwindCompound, "Execute a compound messwind operation"},
    {"messwind-folders-compound", NULL, NULL, NULL, NULL, 0, messwindFoldersCommand, "Execute a compound 'captions' operation on the folders"},
    {"messwind-toggle-layout", NULL, NULL, NULL, NULL, 0, messwind__ToggleSideBySide, "Toggle the vertical/horizontal positioning of folders & captions"},
    {NULL, NULL, NULL, NULL, NULL, 0, NULL, NULL, NULL},
};

boolean messwind__InitializeClass(c) 
struct classheader *c;
{
    messwind_standardmenulist = menulist_New();
    messwind_standardkeymap = keymap_New();
    bind_BindList(messwind_standardbindings, messwind_standardkeymap, messwind_standardmenulist, &messwind_classinfo);
    return(TRUE);
}

void
messwind__PostKeyState(self, ks)
struct messwind *self;
struct keystate *ks;
{
    if (!ks) return;
    if (amsutil_GetOptBit(EXP_KEYSTROKES)
		&& text_GetReadOnly((struct text *)self->bview->header.view.dataobject)) {
        keystate_AddBefore(self->mykeys, ks);
	super_PostKeyState(self, self->mykeys);
    } else {
	super_PostKeyState(self, ks);
    }
}

void messwind__PostMenus(mess, ml)
struct messwind *mess;
struct menulist *ml;
{
    menulist_ClearChain(mess->mymenulist);
    if (ml) menulist_ChainAfterML(mess->mymenulist, ml, ml);
    super_PostMenus(mess, mess->mymenulist);
}

void messwind__FinalizeObject(c, self)
struct classheader *c;
struct messwind *self;
{
    messwind_SetNth(self, 0, NULL);
    messwind_SetNth(self, 1, NULL);
    lpair_Destroy(self->capbodylp);
    captions_Destroy(self->captions);
    folders_Destroy(self->folders);
    t822view_Destroy(self->bview);
    menulist_Destroy(self->mymenulist);
    keystate_Destroy(self->mykeys);
}
