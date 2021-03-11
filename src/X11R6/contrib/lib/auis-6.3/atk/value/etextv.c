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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/etextv.c,v 2.10 1992/12/15 21:46:29 rr2b R6tape $";
#endif


 
#include <class.h>

static struct keymap *ssmap;
#include <keystate.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <entrtext.ih>
#include <valuev.ih>
#include <etextv.eh>
#define Text(A) ((struct entertext *)(A->header.view.dataobject))
void etextview_CancelCommand(self)
register struct etextview *self; 
{
    struct entertext *txt = Text(self);
    if(entertext_Changed(txt)){
	if(entertext_buflen(txt))
	    entertext_SetChars(txt,entertext_GetString(txt) ,entertext_buflen(txt) - 1);
	else entertext_Clear(txt);
	entertext_NotifyObservers(txt,0);
    }
}
void etextview_ReturnCommand(self)
register struct etextview *self; 
{
    struct entertext *txt = Text(self);
    etextview_SetDotPosition(self,0);
    etextview_SetDotLength(self,0);
    entertext_updatebuf(txt);
}
void etextview_ClearCommand(self)
register struct etextview *self; 
{
    struct entertext *txt = Text(self);
    entertext_Clear(txt);
    entertext_NotifyObservers(txt,0);
}

void etextview__ReceiveInputFocus(self)
register struct etextview *self; 
{
    self->header.textview.hasInputFocus = TRUE;
    self->keystate->next = NULL;
    self->header.textview.keystate->next = NULL; /* Unforgivably bogus... */
    keystate_AddBefore(self->keystate, self->header.textview.keystate); /* Slightly bogus. */
    etextview_PostKeyState(self , self->keystate);
    if(Text(self) && self->ClearOnRIF) {
	entertext_Clear(Text(self));
	entertext_NotifyObservers(Text(self),0);
    }
    else etextview_WantUpdate(self,self);
    if(self->valueview)
	valueview_Highlight(self->valueview);
}
void etextview__LoseInputFocus(self)
register struct etextview *self; 
{
    if(self->valueview)
	valueview_Dehighlight(self->valueview);
    if(Text(self) && self->ResetOnLIF) {
	etextview_CancelCommand(self);
    }
    super_LoseInputFocus(self);
}
boolean etextview__InitializeObject(classID,self)
struct classheader *classID;
struct etextview *self;
{
    self->keystate = keystate_Create(self, ssmap);
    self->ClearOnRIF = FALSE;
    self->ResetOnLIF = FALSE;
    self->valueview = NULL;
    return TRUE;
}

boolean etextview__InitializeClass(classID)
struct classheader *classID;
{    
    struct classinfo *classInfo = &etextview_classinfo;
    struct proctable_Entry *tempProc;

    ssmap = keymap_New();
    tempProc=proctable_DefineProc("etextview-return-cmd", etextview_ReturnCommand, classInfo, NULL, "Handle enter key");
    keymap_BindToKey(ssmap, "\015", tempProc, 0);
    tempProc=proctable_DefineProc("etextview-cancel-cmd", etextview_CancelCommand, classInfo, NULL, "Handle ^G");
    keymap_BindToKey(ssmap, "\007", tempProc, 0);
    tempProc=proctable_DefineProc("etextview-clear-cmd", etextview_ClearCommand, classInfo, NULL, "Handle ^U");
    keymap_BindToKey(ssmap, "\025", tempProc, 0);
    return TRUE;
}
