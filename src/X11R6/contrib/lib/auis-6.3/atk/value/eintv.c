/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1989 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */
/* $Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/eintv.c,v 1.1 1993/08/20 20:05:59 susan Exp $ */
/* $ACIS: $ */
/* $Source: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/eintv.c,v $ */

#ifndef lint
static char *rcsid = "$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/value/RCS/eintv.c,v 1.1 1993/08/20 20:05:59 susan Exp $ ";
#endif /* lint */
#include <class.h>

static struct keymap *ssmap;
#include <keystate.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <entrint.ih>
#include <valuev.ih>
#include <eintv.eh>
#define Text(A) ((struct enterint *)(A->header.view.dataobject))
void eintview_CancelCommand(self)
register struct eintview *self; 
{
    struct enterint *txt = Text(self);
    if(enterint_Changed(txt)){
	if(enterint_buflen(txt))
	    enterint_SetChars(txt,enterint_GetString(txt) ,enterint_buflen(txt) - 1);
	else enterint_Clear(txt);
	enterint_NotifyObservers(txt,0);
    }
}
void eintview_ReturnCommand(self)
register struct eintview *self; 
{
    struct enterint *txt = Text(self);
    eintview_SetDotPosition(self,0);
    eintview_SetDotLength(self,0);
    enterint_updatebuf(txt);
}
void eintview_ClearCommand(self)
register struct eintview *self; 
{
    struct enterint *txt = Text(self);
    enterint_Clear(txt);
    enterint_NotifyObservers(txt,0);
}

void eintview__ReceiveInputFocus(self)
register struct eintview *self; 
{
    self->header.textview.hasInputFocus = TRUE;
    self->keystate->next = NULL;
    self->header.textview.keystate->next = NULL; /* Unforgivably bogus... */
    keystate_AddBefore(self->keystate, self->header.textview.keystate); /* Slightly bogus. */
    eintview_PostKeyState(self , self->keystate);
    if(Text(self) && self->ClearOnRIF) {
	enterint_Clear(Text(self));
	enterint_NotifyObservers(Text(self),0);
    }
    else eintview_WantUpdate(self,self);
    if(self->valueview)
	valueview_Highlight(self->valueview);
}
void eintview__LoseInputFocus(self)
register struct eintview *self; 
{
    if(self->valueview)
	valueview_Dehighlight(self->valueview);
    if(Text(self) && self->ResetOnLIF) {
	eintview_CancelCommand(self);
    }
    super_LoseInputFocus(self);
}
boolean eintview__InitializeObject(classID,self)
struct classheader *classID;
struct eintview *self;
{
    self->keystate = keystate_Create(self, ssmap);
    self->ClearOnRIF = FALSE;
    self->ResetOnLIF = FALSE;
    self->valueview = NULL;
    return TRUE;
}

boolean eintview__InitializeClass(classID)
struct classheader *classID;
{    
    struct classinfo *classInfo = &eintview_classinfo;
    struct proctable_Entry *tempProc;

    ssmap = keymap_New();
    tempProc=proctable_DefineProc("eintview-return-cmd", eintview_ReturnCommand, classInfo, NULL, "Handle enter key");
    keymap_BindToKey(ssmap, "\015", tempProc, 0);
    tempProc=proctable_DefineProc("eintview-cancel-cmd", eintview_CancelCommand, classInfo, NULL, "Handle ^G");
    keymap_BindToKey(ssmap, "\007", tempProc, 0);
    tempProc=proctable_DefineProc("eintview-clear-cmd", eintview_ClearCommand, classInfo, NULL, "Handle ^U");
    keymap_BindToKey(ssmap, "\025", tempProc, 0);
    return TRUE;
}
