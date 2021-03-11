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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex13/RCS/helloa.c,v 2.8 1992/12/15 21:33:36 rr2b R6tape $";
#endif

#include <class.h>

#include "helloa.eh"

#include "dataobj.ih"
#include "view.ih"
#include "frame.ih"
#include "im.ih"
#include "hello.ih"

static boolean makeWindow(dobj)
struct dataobject *dobj;
{
    struct view *v;
    struct view *applayer;
    struct frame *frame;
    struct im *im;

    v=(struct view *)class_NewObject(dataobject_ViewName(dobj));
    if(v==NULL)
	return FALSE;

    applayer=view_GetApplicationLayer(v);
    if(applayer==NULL) {
	view_Destroy(v);
	return FALSE;
    }

    frame=frame_New();
    if(frame==NULL) {
	view_DeleteApplicationLayer(v,applayer);
	return FALSE;
    }

    im=im_Create(NULL);
    if(im==NULL) {
	frame_Destroy(frame);
	return FALSE;
    }

    view_SetDataObject(v,dobj);
    frame_SetView(frame,applayer);
    im_SetView(im,frame);

    view_WantInputFocus(v,v);

    return TRUE;

}

boolean helloworldapp__Start(hwapp)
struct helloworldapp *hwapp;
{
    struct helloworld *hw;

    if(!super_Start(hwapp))
	return FALSE;

    hw=helloworld_New();
    if(hw==NULL)
	return FALSE;

    if(!makeWindow((struct dataobject *)hw) ||
       !makeWindow((struct dataobject *)hw)){
	helloworld_Destroy(hw);
	return FALSE;
    }

    return TRUE;
}
