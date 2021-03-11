/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/metextv.c,v 2.11 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 
#include <class.h>

static struct keymap *ssmap;
#include <keystate.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <menttext.ih>
#include <valuev.ih>
#include <message.ih>
#include <menulist.ih>
#include <metextv.eh>
#define Text(A) ((struct mentertext *)(A->header.view.dataobject))
void metextview_CancelCommand(self)
register struct metextview *self; 
{
    struct mentertext *txt = Text(self);
    if(mentertext_Changed(txt)){
	mentertext_ClearLine(txt,metextview_GetDotPosition(self));
	mentertext_NotifyObservers(txt,0);
    }
}
void metextview_ReturnCommand(self)
register struct metextview *self; 
{
    long np;
    char resp[64];
    struct mentertext *txt = Text(self);
    long pos = metextview_GetDotPosition(self);
    long len = mentertext_GetLength(txt);
    np = mentertext_Index(txt,pos,'\n',len - pos);
    if(np != EOF){
	pos =  mentertext_Index(txt,np + 1,'\n',len - (np + 1));
	if(pos == EOF) {
	    if(len > np) np = len ;
	}
	else np = pos;
	metextview_SetDotPosition(self,np);
	metextview_FrameDot(self,np);
    }
    else{
	if(message_AskForString(self,0,"Confirm[y] ",0,resp,63) == -1)
	    return;
	if(*resp != 'n' &&  *resp != 'N') {
	    mentertext_updatebuf(txt);
	}
    }
}
void metextview_ClearCommand(self)
register struct metextview *self; 
{
    struct mentertext *txt = Text(self);
    mentertext_Clear(txt);
    mentertext_NotifyObservers(txt,0);
}

void metextview__ReceiveInputFocus(self)
register struct metextview *self; 
{
    self->header.textview.hasInputFocus = TRUE;
    self->keystate->next = NULL;
    self->header.textview.keystate->next = NULL; /* Unforgivably bogus... */
    keystate_AddBefore(self->keystate, self->header.textview.keystate); /* Slightly bogus. */
    metextview_PostKeyState(self , self->keystate);
    if(Text(self) && self->ClearOnRIF) {
	mentertext_Clear(Text(self));
	mentertext_NotifyObservers(Text(self),0);
    }
    else metextview_WantUpdate(self,self);
    if(self->valueview)
	valueview_Highlight(self->valueview);
    menulist_SetMask(self->header.textview.menus, textview_NoMenus);
/*    metextview_PostMenus(self,self->header.textview.menus); */
}
void metextview__LoseInputFocus(self)
register struct metextview *self; 
{
    if(self->valueview)
	valueview_Dehighlight(self->valueview);
    if(Text(self) && self->ResetOnLIF) {
	metextview_CancelCommand(self);
    }
    super_LoseInputFocus(self);
}
boolean metextview__InitializeObject(classID,self)
struct classheader *classID;
struct metextview *self;
{
    self->keystate = keystate_Create(self, ssmap);
    self->ClearOnRIF = FALSE;
    self->ResetOnLIF = FALSE;
    self->valueview = NULL;
    return TRUE;
}

boolean metextview__InitializeClass(classID)
struct classheader *classID;
{    
    struct classinfo *classInfo = &metextview_classinfo;
    struct proctable_Entry *tempProc;

    ssmap = keymap_New();
    tempProc=proctable_DefineProc("metextview-return-cmd", metextview_ReturnCommand, classInfo, NULL, "Handle menter key");
    keymap_BindToKey(ssmap, "\015", tempProc, 0);
/*
    tempProc=proctable_DefineProc("metextview-cancel-cmd", metextview_CancelCommand, classInfo, NULL, "Handle ^G");
    keymap_BindToKey(ssmap, "\007", tempProc, 0);
    tempProc=proctable_DefineProc("metextview-clear-cmd", metextview_ClearCommand, classInfo, NULL, "Handle ^U");
    keymap_BindToKey(ssmap, "\025", tempProc, 0);
*/
    return TRUE;
}
void metextview__ObservedChanged(self,changed,value)
register struct metextview *self;
struct observable *changed;
register long value;
{
    super_ObservedChanged(self,changed,value);
    if(value != observable_OBJECTDESTROYED){
	if(metextview_GetDotPosition(self) == 0 && 
	   (value = mentertext_GetLength(Text(self))) > 0 &&
	   (value = mentertext_Index(Text(self),0,'\n',value)) != EOF)
	    metextview_SetDotPosition(self,value);
    }
}
