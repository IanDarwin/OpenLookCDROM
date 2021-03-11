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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex3/RCS/hellov.c,v 2.8 1992/12/15 21:34:22 rr2b R6tape $";
#endif

#include <class.h>
#include "hellov.eh"
#include "graphic.ih"

#define POSUNDEF -1

boolean helloworldview__InitializeObject(classID, hwv)
struct classheader *classID;
struct helloworldview *hwv;
{
    hwv->x = POSUNDEF;
    hwv->y = POSUNDEF;
    hwv->HaveDownTransition = FALSE;
    return TRUE;
}


void helloworldview__FullUpdate(hwv, type, left, top, width, height)
    struct helloworldview *hwv;
    enum view_UpdateType type;
    long left;
    long top;
    long width;
    long height;
{
    struct rectangle myVisualRect; 

    if (hwv->x == POSUNDEF)  {
	helloworldview_GetVisualBounds(hwv,&myVisualRect);
	hwv->x = rectangle_Left(&myVisualRect) + rectangle_Width(&myVisualRect)/2;
	hwv->y = rectangle_Top(&myVisualRect) + rectangle_Height(&myVisualRect)/2;
    }

    helloworldview_MoveTo(hwv,hwv->x,hwv->y);
    helloworldview_DrawString(hwv,"hello world",
	graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);    
}

    
void helloworldview__Update(hwv)
struct helloworldview *hwv;
{
   
    if (hwv->newX != hwv->x ||  hwv->newY != hwv->y) {
        helloworldview_SetTransferMode(hwv,graphic_INVERT);

        helloworldview_MoveTo(hwv, hwv->x , hwv->y);
        helloworldview_DrawString(hwv, "hello world", graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);

        hwv->x = hwv->newX;
        hwv->y = hwv->newY;

        helloworldview_MoveTo(hwv, hwv->x , hwv->y);
        helloworldview_DrawString(hwv, "hello world", graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);
    
    }
}


struct view *helloworldview__Hit(hwv,action,x,y,numberOfClicks)
struct helloworldview *hwv;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    if(hwv->HaveDownTransition)
	switch(action){
	    case view_RightUp:
		hwv->HaveDownTransition=FALSE;
		/* fall through */
	    case view_RightMovement:
		hwv->newX=x-hwv->distX;
		hwv->newY=y-hwv->distY;
		break;
	    case view_LeftUp:
		hwv->HaveDownTransition=FALSE;
		hwv->newX=x;
		hwv->newY=y;
		break;
	    case view_LeftMovement:
		/* do nothing */
		break;
	    default:
		/* re-synchronize mouse */
		hwv->HaveDownTransition=FALSE;
	}

    if(!hwv->HaveDownTransition)
	switch(action){
	    case view_RightDown:
		hwv->distX=x-hwv->x;
		hwv->distY=y-hwv->y;
		/* fall through */
	    case view_LeftDown:
		hwv->HaveDownTransition=TRUE;
		helloworldview_WantInputFocus(hwv,hwv);
		break;
	}

    helloworldview_WantUpdate(hwv,hwv);

    return (struct view *)hwv;
}
