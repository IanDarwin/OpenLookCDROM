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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/pvalfv.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "pvalfv.eh"
#include "prefval.ih"
#include <sbutton.ih>
#include "prefsbv.ih"
#include <view.ih>
#include <fontdesc.ih>

#define DATA(self) ((struct prefval *)pvalfv_GetDataObject(self))

boolean pvalfv__InitializeObject(classID, self)
struct classheader *classID;
struct pvalfv *self;
{
    struct sbutton *sb=pvalfv_GetSButton(self);
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(sb);
    sbutton_SetLabel(sb, 0, "SampleText");
    sbutton_GetStyle(prefs)= sbutton_PLAIN;
    return TRUE;
    
}

void pvalfv__FinalizeObject(classID, self)
struct classheader *classID;
struct pvalfv *self;
{
    /* the wrapv class takes care of destroying everything */
}

void pvalfv__UpdateSButton(self)
struct pvalfv *self;
{
    struct sbutton *sb=pvalfv_GetSButton(self);
    struct sbutton_prefs *prefs=sbutton_GetDefaultPrefs(sb);
    struct fontdesc *font=NULL;
    char *name=prefval_IndexValueString(DATA(self), 0);
    char buf2[1024];
    long style, size;
    if(name==NULL) return;
    if(fontdesc_ExplodeFontName(name, buf2, sizeof(buf2), &style, &size))	{
	font=fontdesc_Create(buf2,style,size);
	sbutton_GetFont(prefs) = font;
    }
    sbutton_SetChangeFlag(sb, sbutton_ALLCHANGED|sbutton_FONTCHANGED);
    sbutton_NotifyObservers(sb, observable_OBJECTCHANGED);
}

