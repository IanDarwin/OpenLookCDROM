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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex11/RCS/helloa.c,v 2.8 1992/12/15 21:32:53 rr2b R6tape $";
#endif

#include <class.h>

#include "helloa.eh"

#include "im.ih"
#include "frame.ih"
#include "hello.ih"
#include "hellov.ih"

boolean helloworldapp__Start(hwapp)
struct helloworldapp *hwapp;
{
    struct helloworld *hw;
    struct helloworldview *hwv;
    struct view *applayer;
    struct frame *frame;
    struct im *im;

    if(!super_Start(hwapp))
	return FALSE;

    hw=helloworld_New();
    if(hw==NULL)
	return FALSE;

    hwv=helloworldview_New();
    if(hwv==NULL) {
	helloworld_Destroy(hw);
	return FALSE;
    }

    applayer=helloworldview_GetApplicationLayer(hwv);
    if(applayer==NULL) {
	helloworldview_Destroy(hwv);
	return FALSE;
    }

    frame=frame_New();
    if(frame==NULL) {
	helloworldview_DeleteApplicationLayer(hwv,applayer);
	return FALSE;
    }

    im=im_Create(NULL);
    if(im==NULL) {
	frame_Destroy(frame);
	return FALSE;
    }

    helloworldview_SetDataObject(hwv,hw);
    frame_SetView(frame,applayer);
    im_SetView(im,frame);

    helloworldview_WantInputFocus(hwv,hwv);

    return TRUE;

}

