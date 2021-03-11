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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalsbv.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "pvalsbv.eh"
#include "prefval.ih"
#include <sbutton.ih>
#include "prefsbv.ih"
#include <view.ih>

#define DATA(self) ((struct prefval *)pvalsbv_GetDataObject(self))

boolean pvalsbv__InitializeObject(classID, self)
struct classheader *classID;
struct pvalsbv *self;
{
    struct sbutton *t;
    struct prefsbv *tv;
    
    t=sbutton_New();
    if(t==NULL) return FALSE;
    
    tv=prefsbv_New();
    if(tv==NULL) {
	sbutton_Destroy(t);
	return FALSE;
    }

    pvalsbv_SetData(self, t);
    pvalsbv_SetView(self, tv);
    pvalsbv_SetInterfaceView(self, tv);
    prefsbv_SetDataObject(tv, t);
    return TRUE;
    
}

void pvalsbv__FinalizeObject(classID, self)
struct classheader *classID;
struct pvalsbv *self;
{
    /* the wrapv class takes care of destroying everything */
}

void pvalsbv__UpdateSButton(self)
struct pvalsbv *self;
{
}

void pvalsbv__SetDataObject(self, d)
struct pvalsbv *self;
struct prefval *d;
{
    super_SetDataObject(self, d);
    pvalsbv_UpdateSButton(self);
}

void pvalsbv__ObservedChanged(self, changed, val)
struct pvalsbv *self;
struct prefval *changed;
long val;
{
    super_ObservedChanged(self, changed, val);
    if(changed==DATA(self)) {
	pvalsbv_UpdateSButton(self);
    }
}
