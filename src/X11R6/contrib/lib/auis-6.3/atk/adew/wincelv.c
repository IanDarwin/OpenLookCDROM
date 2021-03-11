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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/adew/RCS/wincelv.c,v 1.9 1992/12/15 21:25:24 rr2b R6tape $";
#endif

#include <class.h>
#include <cel.ih>
#include <arbiterv.ih>
#include <celv.ih>
#include <view.ih>
#include <im.ih>
#include <rect.h>
#include <wincelv.eh>

#define DataObject(A) (A->header.view.dataobject)
#define Cel(A) ((struct cel *) DataObject(A))

struct view *wincelview__Hit(self,action,mousex,mousey,numberOfClicks) 
struct wincelview *self;
enum view_MouseAction action;
long mousex, mousey, numberOfClicks;
{
    if(action == view_LeftUp){
	if(self->celview == NULL)wincelview_pushchild(self);
	else wincelview_popchild(self);
    }	
    return (struct view *) self;
}
void wincelview__pushchild(self)
struct wincelview *self;
{
    struct arbiterview *abv;
    struct cel *pc = Cel(self);
    abv = arbiterview_FindArb(self);
    if(pc && abv != NULL){
	if(self->celview == NULL){
	    if((self->celview = celview_New()) == NULL) {
		fprintf(stderr,"Could not allocate enough memory.\n");
		return;
	    }
	    celview_AddObserver(self->celview,self);
	    celview_SetDataObject(self->celview,pc);
	    celview_SetRemoteArb(self->celview,abv);
	    if ((self->window = im_Create(NULL)) != NULL) {
		im_SetView(self->window, self->celview);
		im_AddObserver(self->window,self);
	    }
	    else celview_Destroy(self->celview);
	}
    }
}
void wincelview__popchild(self)
struct wincelview *self;
{
    if(self->celview != NULL && self->window != NULL){
	im_Destroy(self->window);
    }
}

static DoUpdate(self)
struct wincelview *self;
{
    struct rectangle enclosingRect;
    long xsize,ysize;
    struct point pt[5];
    enclosingRect.top = 0; enclosingRect.left = 0;
    enclosingRect.width  = wincelview_GetLogicalWidth(self) -1 ;
    enclosingRect.height = wincelview_GetLogicalHeight(self) -1 ;
    wincelview_SetTransferMode(self,graphic_WHITE);
    wincelview_EraseRect(self,&(enclosingRect));
    wincelview_SetTransferMode(self,graphic_INVERT);
    enclosingRect.left = enclosingRect.width / 3;
    enclosingRect.top =enclosingRect.height / 3;
    enclosingRect.width  =  enclosingRect.width / 2 ;
    enclosingRect.height = enclosingRect.height / 2 ;
    ysize = enclosingRect.height - enclosingRect.top;
    xsize = enclosingRect.width - enclosingRect.left;
    wincelview_DrawRect(self,&(enclosingRect));
    pt[0].x = enclosingRect.left - 1;
    pt[0].y = enclosingRect.height + enclosingRect.top - ysize;
    pt[1].x = pt[0].x - xsize;
    pt[1].y = pt[0].y ;
    pt[2].x = pt[1].x ;
    pt[2].y = pt[0].y  + ysize + ysize;
    pt[3].x = pt[0].x + xsize;
    pt[3].y = pt[2].y;
    pt[4].x = pt[3].x;
    pt[4].y = enclosingRect.top + enclosingRect.height + 1;
    wincelview_DrawPath(self,pt,5);
}
void wincelview__ObservedChanged(self, changed, value)
struct wincelview *self;
struct observable *changed;
long value;
{
    if(value == observable_OBJECTDESTROYED){
	if(changed == (struct observable *)self->window){
	    self->window = NULL;
	    celview_RemoveObserver(self->celview,self);
	    celview_Destroy(self->celview);
	}
	self->celview = NULL;
	self->window = NULL;
    }
}
void wincelview__FullUpdate(self,type,left,top,width,height)
struct wincelview *self;
enum view_UpdateType type;
long left,top,width,height;
{
    DoUpdate(self);
}
boolean wincelview__InitializeObject(classID,self)
struct classheader *classID;
struct wincelview *self;
{
    self->celview = NULL;
    return TRUE;
}

