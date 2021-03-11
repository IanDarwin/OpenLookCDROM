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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/prefed/RCS/textintv.c,v 1.4 1992/12/15 21:39:11 rr2b R6tape $";
#endif



 

#include <andrewos.h>
#include <class.h>

#include "textintv.eh"

#include <text.ih>
#include <textv.ih>
#include "titextv.ih"

boolean textintv__InitializeObject(classID, self)
struct classheader *classID;
struct textintv *self;
{
    struct text *t;
    struct textview *tv;
    
    t=text_New();
    if(t==NULL) return FALSE;
    
    tv=(struct textview *)titextv_New();
    if(tv==NULL) {
	text_Destroy(t);
	return FALSE;
    }

    titextv_SetTextIntv((struct titextv *)tv, self);
    textintv_SetData(self, t);
    textintv_SetView(self, tv);
    textintv_SetInterfaceView(self, tv);
    textview_SetDataObject(tv, t);
    return TRUE;
    
}

void textintv__FinalizeObject(classID, self)
struct classheader *classID;
struct textintv *self;
{
    /* the wrapv class takes care of destroying everything */
}


void textintv__SetDotPosition(self, pos)
struct textintv *self;
long pos;
{
}


