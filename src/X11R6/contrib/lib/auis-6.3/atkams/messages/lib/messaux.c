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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atkams/messages/lib/RCS/messaux.c,v 1.16 1993/04/06 21:42:04 Zarf Exp $";
#endif


 

#include <andrewos.h>
#include <sys/param.h>
#include <cui.h>
#include <fdphack.h>
#include <errprntf.h>
#include <class.h>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
/* #include <bind.ih> */
#include <text.ih>
#include <textv.ih>
#include <environ.ih>
#include <envrment.ih>
#include <message.ih>
#include <ams.ih>
#include <amsutil.ih>
#include <text822.ih>
#include <text822v.ih>
#include <captions.ih>
#include <folders.ih>
#include <sendmsg.ih>
#define AUXMODULE 1
#include <messages.eh>
#undef AUXMODULE
#include <options.ih>


extern         void BSM_CheckNewPlease();
extern         void BSM_ReadMailPlease();
extern         void BSM_ShowAllPlease();
extern         void BSM_ShowHelp();
extern         void BSM_ShowNewPlease();
extern         void BSM_ShowPersonalPlease();
extern         void BSM_ShowSubscribedPlease();
extern         boolean ClearSM();
extern         void messages_DuplicateWindow();
extern         void FSearchFPlease();
extern         void FSearchRPlease();
extern         struct t822view *GetBodies();
extern         struct captions *GetCaptions();
extern         struct captions *GetCaptionsNoCreate();
extern         struct folders *GetFolders();
extern         void MessagesFocusFolders();
extern         void MessagesFoldersCommand();
extern         void MessagesSendmessageCommand();

void sm_SetMessagesOptions(self)
struct messages *self;
{
    captions_ResetVisibleCaption(GetCaptions(self));
    options_SetMessagesOptions(GetBodies(self));
}

extern  int  (*messtextv_ForwardSearchCmd)(), (*messtextv_ReverseSearchCmd)();

void messages__PostKeyState(self, ks)
struct messages *self;
struct keystate *ks;
{
    struct keystate *tmp;
    if (!ks) return;
    if (amsutil_GetOptBit(EXP_KEYSTROKES) 
		&& (self->WhatIAm != WHATIAM_BODIES
		    || text_GetReadOnly(
			(struct text *) t822view_GetDataObject(GetBodies(self))
		))) {
	tmp = keystate_AddBefore(self->mypermkeys, ks);
	tmp = keystate_AddBefore(self->mykeys, tmp);
	super_PostKeyState(self, tmp);
    } else {
	tmp = keystate_AddBefore(self->mypermkeys, ks);
	super_PostKeyState(self, tmp);
    }
}

void messages__FinalizeObject(c, self)
struct classheader *c;
struct messages *self;
{
    menulist_ClearChain(self->mymenulist);
    menulist_ClearChain(self->mypermmenulist);
    menulist_Destroy(self->mymenulist);
    menulist_Destroy(self->mypermmenulist);
    keystate_Destroy(self->mykeys);
    keystate_Destroy(self->mypermkeys);
    if (self->fileintomenulist) menulist_Destroy(self->fileintomenulist);
}

void messages__PostMenus(mess, ml)
struct messages *mess;
struct menulist *ml;
{
    CheckMenuMasks(mess);
    menulist_ClearChain(mess->mymenulist);

    menulist_ChainAfterML(mess->mymenulist, mess->mypermmenulist, mess->mypermmenulist);
    if (ml) menulist_ChainAfterML(mess->mymenulist, ml, ml);
    if (mess->fileintomenulist) {
	menulist_ChainAfterML(mess->mymenulist, mess->fileintomenulist, mess->fileintomenulist);
    }
    super_PostMenus(mess, mess->mymenulist);
}

struct captions *GetCaptions(self)
struct messages *self;
{
    struct captions *c = NULL;
    switch(self->WhatIAm) {
	case WHATIAM_FOLDERS:
	    c = folders_GetCaptions((struct folders *) self);
	    break;
	case WHATIAM_CAPTIONS:
	    c = (struct captions *) self;
	    break;
	case WHATIAM_BODIES:
	    c = t822view_GetCaptions((struct t822view *) self);
	    break;
    }
    if (!c) {
	ams_ReportError(ams_GetAMS(), "Out of memory; a core dump is imminent.", ERR_FATAL, FALSE, 0);
    }
    return(c);
}

struct t822view *GetBodies(self)
struct messages *self;
{
    struct t822view *tv = NULL;
    switch(self->WhatIAm) {
	case WHATIAM_FOLDERS:
	    tv = captions_GetBodView(folders_GetCaptions((struct folders *) self));
	    break;
	case WHATIAM_CAPTIONS:
	    tv = captions_GetBodView((struct captions *) self);
	    break;
	case WHATIAM_BODIES:
	    tv = (struct t822view *) self;
	    break;
    }
    if (!tv) {
	ams_ReportError(ams_GetAMS(), "Out of memory; a core dump is imminent.", ERR_FATAL, FALSE, 0);
    }
    return(tv);
}

void MessagesFocusFolders(self)
struct messages *self;
{
    struct folders *f = GetFolders(self);
    folders_WantInputFocus(f, f);
}

void MessagesSendmessageCommand(self, cmds)
struct messages *self;
char *cmds;
{
    struct sendmessage *sm = folders_ExposeSend(GetFolders(self));
    if (sm) {
	ams_GenericCompoundAction(ams_GetAMS(), sm, "sendmessage", cmds);
    }
}

void BSM_ShowHelp(self)
struct messages *self;
{
    if (GetCaptions(self)->FullName) {
	folders_ExplainDir(GetFolders(self), GetCaptions(self)->FullName, GetCaptions(self)->ShortName);
    } else {
	t822view_ShowHelp(GetBodies(self), NULL);
    }
}

boolean ClearSM(self)
struct captions *self;
{
    struct sendmessage *sm = folders_ExposeSend(captions_GetFolders(self));
    if (!sm) return(TRUE);
    sendmessage_Reset(sm);
    if (sendmessage_HasChanged(sm)) return(TRUE);
    return(FALSE);
}

void BSM_CheckNewPlease(self)
struct messages *self;
{
    folders_UpdateMsgs(GetFolders(self), 0, NULL, TRUE);
}

void BSM_ReadMailPlease(self)
struct messages *self;
{
    if(environ_GetProfileSwitch("ReadMailFolders", FALSE))
	folders_UpdateMsgs(GetFolders(self), TRUE, NULL, TRUE);
    else
	folders_ReadMail(GetFolders(self), TRUE);
}

void BSM_ShowNewPlease(self)
struct messages *self;
{
    folders_Reconfigure(GetFolders(self), LIST_NEWONLY);
}

void BSM_ShowPersonalPlease(self)
struct messages *self;
{
    folders_Reconfigure(GetFolders(self), LIST_MAIL_FOLDERS);
}

void BSM_ShowAllPlease(self)
struct messages *self;
{
    folders_Reconfigure(GetFolders(self), LIST_ALL_FOLDERS);
}

void BSM_ShowSubscribedPlease(self)
struct messages *self;
{
    folders_Reconfigure(GetFolders(self), LIST_SUBSCRIBED);
}

void messages_DuplicateWindow(self)
struct messages *self;
{
    if (self->WhatIAm == WHATIAM_FOLDERS) {
	struct folders *f = folders_New();

	ams_InstallInNewWindow(folders_GetApplicationLayer(f), "messages-folders", "Message Folders", environ_GetProfileInt("folders.width", 600), environ_GetProfileInt("folders.height", 120), f);
    } else if (self->WhatIAm == WHATIAM_CAPTIONS) {
	struct captions *f = captions_New();

	ams_InstallInNewWindow(captions_GetApplicationLayer(f), "messages-captions", "Message Captions", environ_GetProfileInt("captions.width", 600), environ_GetProfileInt("captions.height", 120), f);
    } else if (self->WhatIAm == WHATIAM_BODIES) {
	struct t822view *f = t822view_New();
	struct text *t = text_New();

	t822view_SetDataObject(f, t);
	ams_InstallInNewWindow(t822view_GetApplicationLayer(f), "messages-bodies", "Message Bodies", environ_GetProfileInt("bodies.width", 600), environ_GetProfileInt("bodies.height", 120), f);
    }
	
}

struct captions *GetCaptionsNoCreate(self)
struct messages *self;
{
    struct captions *c = NULL;
    switch(self->WhatIAm) {
	case WHATIAM_FOLDERS:
	    c = ((struct folders *) self)->mycaps;
	    break;
	case WHATIAM_CAPTIONS:
	    c = (struct captions *) self;
	    break;
	case WHATIAM_BODIES:
	    c = ((struct t822view *) self)->mycaps;
	    break;
    }
    return(c);
}

struct folders *GetFolders(self)
struct messages *self;
{
    struct folders *f = NULL;
    switch(self->WhatIAm) {
	case WHATIAM_FOLDERS:
	    f = (struct folders *) self;
	    break;
	case WHATIAM_CAPTIONS:
	    f = captions_GetFolders((struct captions *) self);
	    break;
	case WHATIAM_BODIES:
	    f = captions_GetFolders(t822view_GetCaptions((struct t822view *) self));
	    break;
    }
    if (!f) {
	ams_ReportError(ams_GetAMS(), "Out of memory; a core dump is imminent.", ERR_FATAL, FALSE, 0);
    }
    return(f);
}

void MessagesFoldersCommand(self, cmds)
struct messages *self;
char *cmds;
{
    ams_GenericCompoundAction(ams_GetAMS(), GetFolders(self), "folders", cmds);
}

void FSearchFPlease(self)
struct messages *self;
{
    messtextv_ForwardSearchCmd((struct textview *) GetFolders(self));
    messages_WantInputFocus(self, self);
}

void FSearchRPlease(self)
struct messages *self;
{
    messtextv_ReverseSearchCmd((struct textview *) GetFolders(self));
    messages_WantInputFocus(self, self);
}
