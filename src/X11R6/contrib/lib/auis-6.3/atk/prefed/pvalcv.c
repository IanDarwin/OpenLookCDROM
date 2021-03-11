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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalcv.c,v 1.7 1993/05/19 18:43:36 rr2b Exp $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "pvalcv.eh"
#include "prefval.ih"
#include <sbutton.ih>
#include "prefsbv.ih"
#include <view.ih>
#include <atom.ih>
#include <observe.ih>

#define DATA(self) ((struct prefval *)pvalcv_GetDataObject(self))

boolean pvalcv__InitializeObject(classID, self)
struct classheader *classID;
struct pvalcv *self;
{
    struct sbutton *sb=pvalcv_GetSButton(self);
    sbutton_SetLabel(sb, 0, "      ");
    sbutton_GetStyle(sbutton_GetDefaultPrefs(sb))= sbutton_PLAINBOX;
    return TRUE;
}

void pvalcv__FinalizeObject(classID, self)
struct classheader *classID;
struct pvalcv *self;
{
    /* the wrapv class takes care of destroying everything */
}

void pvalcv__UpdateSButton(self)
struct pvalcv *self;
{
    struct sbutton *sb=pvalcv_GetSButton(self);
    char *ps=prefval_IndexValueString(DATA(self), 0);
    struct atom *a;

    if(ps==NULL) return;

    a=atom_Intern(ps);
    if(a==NULL) return;
    
    sbutton_GetTop(sbutton_GetDefaultPrefs(sb)) = atom_Name(a);

    sbutton_SetChangeFlag(sb,  sbutton_ALLCHANGED|sbutton_SIZECHANGED);
    sbutton_NotifyObservers(sb, observable_OBJECTCHANGED);
}
