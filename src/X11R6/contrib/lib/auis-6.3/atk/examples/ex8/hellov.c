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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex8/RCS/hellov.c,v 2.8 1992/12/15 21:35:02 rr2b R6tape $";
#endif

#include <class.h>

#include "hellov.eh"

#include "graphic.ih"
#include "rect.h"
#include "keymap.ih"
#include "keystate.ih"
#include "menulist.ih"
#include "scroll.ih"
#include "bind.ih"
#include "message.ih"

#define POSUNDEF -1
#define TOTALSIZE 1500

static void xgetinfo(), xsetframe(), ygetinfo(), ysetframe();
static long xwhat(), ywhat();

static struct scrollfns horizInterface = {
    xgetinfo, xsetframe, NULL, xwhat
};

static struct scrollfns vertInterface = {
    ygetinfo, ysetframe, NULL, ywhat
};

static struct keymap *helloworldviewKeymap;
static struct menulist *helloworldviewMenulist;

boolean helloworldview__InitializeObject(classID, hwv)
struct classheader *classID;
struct helloworldview *hwv;   
{
    hwv->x = POSUNDEF;
    hwv->y = POSUNDEF;
    hwv->HaveDownTransition = FALSE;
    hwv->haveInputFocus = FALSE;
    hwv->keystate = keystate_Create(hwv, helloworldviewKeymap);
    hwv->blackOnWhite = TRUE;
    hwv->newBlackOnWhite = TRUE;
    hwv->menulist = menulist_DuplicateML(helloworldviewMenulist, hwv);
    hwv->newFrameX = hwv->newFrameY = 0;
    return TRUE;
}

struct view *helloworldview__GetApplicationLayer(hwv)
struct textview *hwv;
{
    return (struct view *)scroll_Create(hwv,scroll_LEFT+scroll_BOTTOM);
}

void helloworldview__DeleteApplicationLayer(hwv,scrollbar)
struct textview *hwv;
struct scroll *scrollbar;
{
    scroll_Destroy(scrollbar);
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

    helloworldview_GetVisualBounds(hwv,&myVisualRect);
    hwv->vrWidth=rectangle_Width(&myVisualRect);
    hwv->vrHeight=rectangle_Height(&myVisualRect);

    if (hwv->newFrameX + hwv->vrWidth > TOTALSIZE)
	hwv->newFrameX = TOTALSIZE - hwv->vrWidth;
    if (hwv->newFrameY + hwv->vrHeight > TOTALSIZE)
	hwv->newFrameY = TOTALSIZE - hwv->vrHeight;

    hwv->frameX = hwv->newFrameX;
    hwv->frameY = hwv->newFrameY;

    if (hwv->x == POSUNDEF)  {
	hwv->x = hwv->frameX + hwv->vrWidth / 2;
	hwv->y = hwv->frameY + hwv->vrHeight / 2;
	hwv->newX = hwv->x;
	hwv->newY = hwv->y;
    }
    else {
	hwv->x = hwv->newX;
	hwv->y = hwv->newY;
    }

    helloworldview_SetTransferMode(hwv, graphic_COPY);

    if (hwv->blackOnWhite)  {
	helloworldview_FillRect(hwv, &myVisualRect, helloworldview_WhitePattern(hwv));
    }
    else  {
	helloworldview_FillRect(hwv, &myVisualRect, helloworldview_BlackPattern(hwv));
    }

    helloworldview_SetTransferMode(hwv, graphic_INVERT);

    helloworldview_MoveTo(hwv, hwv->x - hwv->frameX, hwv->y - hwv->frameY);
    helloworldview_DrawString(hwv,"hello world", graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);    
}


void helloworldview__Update(hwv)
struct helloworldview *hwv;
{    
    /* TransferMode is graphic_INVERT from the last FullUpdate */

    if (hwv->newBlackOnWhite != hwv->blackOnWhite)  {
	struct rectangle vr;

	helloworldview_GetVisualBounds(hwv,&vr);
	helloworldview_FillRect(hwv, &vr, helloworldview_BlackPattern(hwv));
	hwv->blackOnWhite = hwv->newBlackOnWhite;
    }

    if (hwv->newX != hwv->x || hwv->frameX != hwv->newFrameX || hwv->newY != hwv->y || hwv->frameY != hwv->newFrameY) {
	if(hwv->x!=hwv->newX || hwv->y!=hwv->newY){
	    static char buf[100];
	    sprintf(buf,"Hello world at (%d,%d)",hwv->newX,hwv->newY);
	    message_DisplayString(hwv,0,buf);
	}	    

	helloworldview_MoveTo(hwv, hwv->x - hwv->frameX, hwv->y - hwv->frameY);
	helloworldview_DrawString(hwv, "hello world", graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);

	hwv->x = hwv->newX;
	hwv->y = hwv->newY;
	hwv->frameX = hwv->newFrameX;
	hwv->frameY = hwv->newFrameY;

	helloworldview_MoveTo(hwv, hwv->x - hwv->frameX, hwv->y - hwv->frameY);
	helloworldview_DrawString(hwv, "hello world", graphic_BETWEENTOPANDBASELINE | graphic_BETWEENLEFTANDRIGHT);
    }
}


struct view *helloworldview__Hit(hwv, action, x, y, numberOfClicks)
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


void helloworldview__ReceiveInputFocus(hwv)
struct helloworldview *hwv;
{
    hwv->haveInputFocus = TRUE;
    hwv->keystate->next = NULL;
    helloworldview_PostKeyState(hwv, hwv->keystate);
    helloworldview_PostMenus(hwv, hwv->menulist);
}


void helloworldview__LoseInputFocus(hwv)
struct helloworldview *hwv;
{
    hwv->haveInputFocus = FALSE;
}


static void Center(hwv, rock)
struct helloworldview *hwv;
long rock;
{
    hwv->newX = hwv->newFrameX + hwv->vrWidth / 2;
    hwv->newY = hwv->newFrameY + hwv->vrHeight / 2;
    helloworldview_WantUpdate(hwv, hwv);
}


static void Invert(hwv, rock)
struct helloworldview *hwv;
long rock;
{
    hwv->newBlackOnWhite = ! hwv->newBlackOnWhite;
    helloworldview_WantUpdate(hwv, hwv);
}


static void xgetinfo(hwv, total, seen, dot)
struct helloworldview *hwv;
struct range *total, *seen, *dot;
{
    total->beg = 0;
    total->end = TOTALSIZE;
    seen->beg = hwv->frameX;
    seen->end = hwv->frameX + hwv->vrWidth;
    dot->beg = dot->end = hwv->x;
}

static void ygetinfo(hwv, total, seen, dot)
struct helloworldview *hwv;
struct range *total, *seen, *dot;
{
    total->beg = 0;
    total->end = TOTALSIZE;
    seen->beg = hwv->frameY;
    seen->end = hwv->frameY + hwv->vrHeight;
    dot->beg = dot->end = hwv->y;
}

static void xsetframe(hwv, posn, cord, outof)
struct helloworldview *hwv;
int posn;
long cord, outof;
{
    hwv->newFrameX = posn - hwv->vrWidth * cord / outof;
    if (hwv->newFrameX + hwv->vrWidth > TOTALSIZE)
	hwv->newFrameX = TOTALSIZE - hwv->vrWidth;
    else if (hwv->newFrameX < 0)
	hwv->newFrameX = 0;
    helloworldview_WantUpdate(hwv, hwv);
}

static void ysetframe(hwv, posn, cord, outof)
struct helloworldview *hwv;
int posn;
long cord, outof;
{
    hwv->newFrameY = posn - hwv->vrHeight * cord / outof;
    if (hwv->newFrameY + hwv->vrHeight > TOTALSIZE)
	hwv->newFrameY = TOTALSIZE - hwv->vrHeight;
    else if (hwv->newFrameY < 0)
	hwv->newFrameY = 0;
    helloworldview_WantUpdate(hwv, hwv);
}

static long xwhat(hwv, cord, outof)
struct helloworldview *hwv;
long cord, outof;
{
    return hwv->frameX + hwv->vrWidth * cord / outof;
}

static long ywhat(hwv, cord, outof)
struct helloworldview *hwv;
long cord, outof;
{
    return hwv->frameY + hwv->vrHeight * cord / outof;
}


char *helloworldview__GetInterface(hwv, type)
struct helloworldview *hwv;
char *type;
{
    if (strcmp(type, "scroll,vertical") == 0)
	return (char *) &vertInterface;
    else if (strcmp(type, "scroll,horizontal") == 0)
	return (char *) &horizInterface;
    else
	return NULL;
}


static struct bind_Description helloworldviewBindings[]={
    {"helloworld-center", "\003",0, "Hello World,Center",0,0, Center, "Center the helloworld string."},
    {"helloworld-invert", "\011",0, "Hello World,Invert",0,0, Invert, "Invert the helloworld string."},
    NULL
};

boolean helloworldview__InitializeClass(classID)
    struct classheader *classID;
{
    helloworldviewMenulist=menulist_New();
    helloworldviewKeymap=keymap_New();
    bind_BindList(helloworldviewBindings, helloworldviewKeymap,helloworldviewMenulist,&helloworldview_classinfo);
    return TRUE;
}
