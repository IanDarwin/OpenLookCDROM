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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvaltvfc.c,v 1.4 1993/12/09 00:09:25 gk5g Exp $";
#endif

#include <andrewos.h>
#include <math.h>
#include <class.h>
#include <prefval.ih>
#include <proctbl.ih>
#include <keymap.ih>
#include <keystate.ih>
#include <menulist.ih>
#include <bind.ih>
#include <observe.ih>
#include <text.ih>
#include <textv.ih>
#include "pvaltvfc.eh"

#define DATA(self) ((struct prefval *)pvaltvfc_GetDataObject(self))
#define TEXT(self) (pvaltvfc_GetText(self))

static void pvaltvfc__Select(self, ind)
struct pvaltvfc *self;
int ind;
{
    if(ind<prefval_GetChoiceListSize(DATA(self))) {
	prefval_SetIndexValue(DATA(self), prefval_GetCurrentItem(DATA(self)),  &DATA(self)->cvalues[prefval_GetChoiceListSize(DATA(self)) - ind - 1]);
	prefval_NotifyObservers(DATA(self), prefval_ValuesChanged);
    }
}
    
boolean pvaltvfc__InitializeClass(classID)
struct classheader *classID;
{
    return TRUE;
}

boolean pvaltvfc__InitializeObject(classID, self)
struct classheader *classID;
struct pvaltvfc *self;
{
    return TRUE;
}

void pvaltvfc__FinalizeObject(classID, self)
struct classheader *classID;
struct pvaltvfc *self;
{
}

