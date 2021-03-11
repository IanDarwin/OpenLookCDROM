/* Copyright 1992 by the Andrew Toolkit Consortium and Carnegie Mellon University. All rights Reserved. */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalbv.c,v 1.5 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "pvalbv.eh"
#include "prefval.ih"
#include <sbutton.ih>
#include "prefsbv.ih"
#include <view.ih>

#define DATA(self) ((struct prefval *)pvalbv_GetDataObject(self))

#define ON 1
#define OFF 0

boolean pvalbv__InitializeObject(classID, self)
struct classheader *classID;
struct pvalbv *self;
{
    struct sbutton *t=pvalbv_GetSButton(self);
    sbutton_SetLabel(t, OFF, "No");
    sbutton_SetLabel(t, ON, "Yes");
    sbutton_SetLayout(t, 1, 2, sbutton_GrowColumns);
    self->activated=(-1);
    return TRUE;
    
}

void pvalbv__FinalizeObject(classID, self)
struct classheader *classID;
struct pvalbv *self;
{
    /* the wrapv class takes care of destroying everything */
}


struct view *pvalbv__Hit(self, action, x, y, numberOfClicks)
struct pvalbv *self;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    struct view *ret;
    boolean val;

    if(action==view_LeftDown) self->activated=pvalbv_GetSButton(self)->activated;

    ret=super_Hit(self, action, x, y, numberOfClicks);
    
    if(prefval_GetValue(DATA(self))==NULL) return (struct view *)self;

    if((action == view_LeftUp) && (self->activated!=pvalbv_GetSButton(self)->activated)) {

	prefval_GetValue(DATA(self))->v.bval=(pvalbv_GetSButton(self)->activated==ON);
	prefval_SetModified(DATA(self));

	prefval_NotifyObservers(DATA(self), prefval_ValuesChanged);
    }
    return (struct view *)self;    
}


void pvalbv__UpdateSButton(self)
struct pvalbv *self;
{
    if(prefval_GetType(DATA(self))==prefval_Boolean) {
	if(prefval_GetValue(DATA(self))) {
	    if(prefval_GetValue(DATA(self))->v.bval) sbutton_ActivateButton(pvalbv_GetSButton(self), ON);
	    else sbutton_ActivateButton(pvalbv_GetSButton(self), OFF);
	}
    }
}
