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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/examples/ex13/RCS/hellov.c,v 2.8 1992/12/15 21:33:36 rr2b R6tape $";
#endif

#include <stdio.h>
#include <class.h>

#include "hellov.eh"

#include "graphic.ih"
#include "fontdesc.ih"
#include "rect.h"
#include "keymap.ih"
#include "keystate.ih"
#include "menulist.ih"
#include "scroll.ih"
#include "bind.ih"
#include "message.ih"
#include "im.ih"

#include "hello.ih"

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

static struct fontdesc *bold, *italic;

boolean helloworldview__InitializeObject(classID,hwv)
struct classheader *classID;
struct helloworldview *hwv;   
{
    hwv->haveInputFocus = FALSE;
    hwv->HaveDownTransition=FALSE;
    hwv->keystate = keystate_Create(hwv, helloworldviewKeymap);
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

void drawHW(hwv)
struct helloworldview *hwv;
{
    helloworldview_MoveTo(hwv,
			   hwv->x-hwv->frameX,hwv->y-hwv->frameY);
    helloworldview_SetFont(hwv,italic);
    helloworldview_DrawString(hwv, "hello ",
			       graphic_BETWEENTOPANDBASELINE |
			       graphic_ATRIGHT);
    helloworldview_MoveTo(hwv,
			   hwv->x-hwv->frameX,hwv->y-hwv->frameY);
    helloworldview_SetFont(hwv,bold);
    helloworldview_DrawString(hwv, "world",
			       graphic_BETWEENTOPANDBASELINE |
			       graphic_ATLEFT);
}

void helloworldview__FullUpdate(hwv, type, left, top, width, height)
struct helloworldview *hwv;
enum view_UpdateType type;
long left;
long top;
long width;
long height; 
{
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;
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

    if (hw->x == POSUNDEF) {
	hw->x = hwv->frameX + hwv->vrWidth / 2;
	hw->y = hwv->frameY + hwv->vrHeight / 2;
    }

    hwv->x=hw->x;
    hwv->y=hw->y;
    hwv->blackOnWhite=hw->blackOnWhite;

    helloworldview_SetTransferMode(hwv, graphic_COPY);

    if (hw->blackOnWhite)
	helloworldview_FillRect(hwv, &myVisualRect, helloworldview_WhitePattern(hwv));
    else
	helloworldview_FillRect(hwv, &myVisualRect, helloworldview_BlackPattern(hwv));

    helloworldview_SetTransferMode(hwv,graphic_INVERT);

    drawHW(hwv);
}


void helloworldview__Update(hwv)
struct helloworldview *hwv;
{    
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;

    helloworldview_SetTransferMode(hwv, graphic_INVERT);

    if (hw->blackOnWhite!=hwv->blackOnWhite) {
	struct rectangle vr;
	helloworldview_GetVisualBounds(hwv,&vr);
	helloworldview_FillRect(hwv,&vr,helloworldview_BlackPattern(hwv));
	hwv->blackOnWhite=hw->blackOnWhite;
    }

    if (hwv->x!=hw->x || hwv->y!=hw->y || hwv->frameX!=hwv->newFrameX || hwv->frameY!=hwv->newFrameY) {
	if(hwv->x!=hw->x || hwv->y!=hw->y){
	    static char buf[100];
	    sprintf(buf,"Hello world at (%d,%d)",hw->x,hw->y);
	    message_DisplayString(hwv,0,buf);
	}	    

	drawHW(hwv);
  
	hwv->x=hw->x;
	hwv->y=hw->y;
  	hwv->frameX = hwv->newFrameX;
  	hwv->frameY = hwv->newFrameY;
  
	drawHW(hwv);
    }
}


struct view *helloworldview__Hit(hwv, action, x, y, numberOfClicks)
struct helloworldview *hwv;
enum view_MouseAction action;
long x;
long y;
long numberOfClicks;
{
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;

    if(hwv->HaveDownTransition)
	switch(action){
	    case view_RightUp:
		hwv->HaveDownTransition=FALSE;
		/* fall through */
	    case view_RightMovement:
		hw->x+=x-hwv->hitX;
		hw->y+=y-hwv->hitY;
		hwv->hitX=x;
		hwv->hitY=y;
		break;
	    case view_LeftUp:
		hwv->HaveDownTransition=FALSE;
		hw->x=x+hwv->frameX;
		hw->y=y+hwv->frameY;
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
		hwv->hitX=x;
		hwv->hitY=y;
		/* fall through */
	    case view_LeftDown:
		hwv->HaveDownTransition=TRUE;
		helloworldview_WantInputFocus(hwv,hwv);
		break;
	}

    helloworld_NotifyObservers(hw,0);

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
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;

    hw->x = hwv->newFrameX + hwv->vrWidth / 2;
    hw->y = hwv->newFrameY + hwv->vrHeight / 2;

    helloworld_NotifyObservers(hw,0);
}


static void Invert(hwv, rock)
struct helloworldview *hwv;
long rock;
{
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;

    hw->blackOnWhite = !hw->blackOnWhite;
    helloworld_NotifyObservers(hw,0);
}


static void relocate(hwv,rock)
struct helloworldview *hwv;
long rock;
{
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;
    char buf[100];
    int x,y;

    message_AskForString(hwv,0,"New location (x,y): ",NULL,buf,sizeof(buf));

    if(sscanf(buf,"%d,%d",&x,&y)!=2)
	message_DisplayString(hwv,1,"Bad format; must be: number,number");
    else{
	hw->x=x;
	hw->y=y;

	helloworld_NotifyObservers(hw,0);
    }
}


static void readHW(hwv,rock)
struct helloworldview *hwv;
long rock;
{
    char file[100], msgBuf[100];
    FILE *fp;

    message_AskForString(hwv,0,"Read file: ",NULL,file,sizeof(file));
    fp=fopen(file,"r");
    if(fp==NULL){
	sprintf(msgBuf,"Couldn't open %s for reading.", file);
	message_DisplayString(hwv,1,msgBuf);
    }else{
	char header[100];

	if(fgets(header,sizeof(header),fp)==NULL){
	    sprintf(msgBuf,"Premature end-of-file in %s.",file);
	    message_DisplayString(hwv,1,msgBuf);
	}else{
	    char name[20];
	    int id;

	    if(sscanf(header,"\\begindata{%[^,],%d}\n",name,&id)!=2){
		sprintf(msgBuf,
			"%s doesn't contain a valid datastream header.",
			file);
		message_DisplayString(hwv,1,msgBuf);
	    }else{
		struct helloworld *hw=
		  (struct helloworld *)hwv->header.view.dataobject;

		if(strcmp(name,class_GetTypeName(hw))!=0){
		    sprintf(msgBuf,
			    "%s doesn't contain a helloworld dataobj.",
			    file);
		    message_DisplayString(hwv,1,msgBuf);
		}else{
		    /* FINALLY, read the object in... */
		    helloworld_Read(hw,fp,id);
		    fclose(fp);
		    helloworld_NotifyObservers(hw,0);
		}
	    }
	}
    }
}


static void writeHW(hwv,rock)
struct helloworldview *hwv;
long rock;
{
    char file[100], msgBuf[100];
    FILE *fp;

    message_AskForString(hwv,0,"Write file: ",NULL,file,sizeof(file));
    fp=fopen(file,"w");
    if(fp==NULL){
	sprintf(msgBuf,"Couldn't open %s for writing.",file);
	message_DisplayString(hwv,1,msgBuf);
    }else{
	struct helloworld *hw=
	  (struct helloworld *)hwv->header.view.dataobject;
	
	helloworld_Write(hw,fp,im_GetWriteID(),0);
	fclose(fp);
    }
}


static void xgetinfo(hwv, total, seen, dot)
struct helloworldview *hwv;
struct range *total, *seen, *dot;
{
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;

    total->beg = 0;
    total->end = TOTALSIZE;
    seen->beg = hwv->frameX;
    seen->end = hwv->frameX + hwv->vrWidth;
    dot->beg = dot->end = hw->x;
}

static void ygetinfo(hwv, total, seen, dot)
struct helloworldview *hwv;
struct range *total, *seen, *dot;
{
    struct helloworld *hw=(struct helloworld *)hwv->header.view.dataobject;

    total->beg = 0;
    total->end = TOTALSIZE;
    seen->beg = hwv->frameY;
    seen->end = hwv->frameY + hwv->vrHeight;
    dot->beg = dot->end = hw->y;
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
    {"helloworld-center", "\003",0, "Hello World,Center",0,0, Center, "Center the helloworldview string."},
    {"helloworld-invert", "\011",0, "Hello World,Invert",0,0, Invert, "Invert the helloworld string."},
    {"helloworld-relocate", "\022",0, "Hello World,Relocate",0,0, relocate, "Relocate the helloworld string."},
    {"helloworld-read", NULL,0, "Hello World,Read",0,0, readHW, "Read in a new hello world."},
    {"helloworld-write", NULL,0, "Hello World,Write",0,0, writeHW, "Write out the current hello world to a file."},
    NULL
};

boolean helloworldview__InitializeClass(classID)
struct classheader *classID;
{
    bold=fontdesc_Create("andysans",fontdesc_Bold,12);
    italic=fontdesc_Create("andysans",fontdesc_Italic,12);

    helloworldviewMenulist=menulist_New();
    helloworldviewKeymap=keymap_New();
    bind_BindList(helloworldviewBindings,
		   helloworldviewKeymap,
		   helloworldviewMenulist,
		   &helloworldview_classinfo);

    return TRUE;
}
