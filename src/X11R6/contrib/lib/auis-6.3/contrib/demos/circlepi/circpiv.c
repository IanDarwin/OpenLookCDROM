/* ********************************************************************** *\
 *         Copyright IBM Corporation 1991 - All Rights Reserved           *
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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/demos/circlepi/RCS/circpiv.c,v 1.4 1992/12/15 21:49:28 rr2b R6tape $";
#endif

#include <stdio.h>
#include <math.h>
#include <class.h>
#include <circpiv.eh>
#include <circpi.ih>
#include <andrewos.h>
#include <cursor.ih>
#include <graphic.ih>
#include <menulist.ih>
#include <message.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <view.ih>

/* Defined constants and macros */

/* External Declarations */

/* Forward Declarations */
static void LimitProc();

/* Global Variables */
static struct menulist *menulist = NULL;


boolean
circlepiview__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/
    struct proctable_Entry *proc = NULL;
    char menubuf[80];
    int i;

    if ((menulist = menulist_New()) == NULL) return(FALSE);

    if ((proc = proctable_DefineProc("circpi-set-depth-limit", LimitProc, &circlepiview_classinfo, NULL, "Takes the integer rock and uses it to set the depth limit on recursion.")) == NULL) return(FALSE);
    for (i = 1; i <= 10; ++i) {
	sprintf(menubuf, "Depth Limit~10,%d~%d", i, i);
	menulist_AddToML(menulist, menubuf, proc, (long)i, 0);
    }
    
    return(TRUE);
}


boolean
circlepiview__InitializeObject(c, self)
struct classheader *c;
struct circlepiview *self;
{
/*
  Set up the data for each instance of the object.
*/
  if (!(self->cursor = cursor_Create(self))) return(FALSE);
  cursor_SetStandard(self->cursor, Cursor_Gunsight);
  self->color = 0;		/* assume monochrome, for now */
  self->ml = menulist_DuplicateML(menulist, self);

  return(TRUE);
}


void
circlepiview__FinalizeObject(c, self)
struct classheader *c;
struct circlepiview *self;
{
  if (self->cursor) cursor_Destroy(self->cursor);
  self->cursor = NULL;
  return;
}


void
circlepiview__ObservedChanged(self, b, v)
     struct circlepiview *self;
     struct circlepi *b;
     long v;
{
    super_ObservedChanged(self, b, v);
    if (v == observable_OBJECTCHANGED) {
	self->need_full_update = TRUE;
	circlepiview_WantUpdate(self, self);
    }
}


void
circlepiview__PostMenus(self, ml)
     struct circlepiview *self;
     struct menulist *ml;
{
    /*
      Enable the menus for this object.
      */

    menulist_ClearChain(self->ml);
    if (ml) menulist_ChainAfterML(self->ml, ml, ml);
    super_PostMenus(self, self->ml);
}


void
circlepiview__LinkTree(self, parent)
     struct circlepiview *self;
     struct view *parent;
{
    super_LinkTree(self, parent);

    if (circlepiview_GetIM(self) != NULL) {
	if (circlepiview_DisplayClass(self) & graphic_Color) {
	    self->color = 1;
	} else {
	    self->color = 0;
	}
    }
}


void
circlepiview__FullUpdate(self, type, left, top, width, height)
struct circlepiview *self;
enum view_UpdateType type;
long left, top, width, height;
{
/*
  Do an update.
*/

  if ((type == view_FullRedraw) || (type == view_LastPartialRedraw)) {
    self->need_full_update = TRUE;
    circlepiview_WantUpdate(self, self);
  }
}

int inside_circle(x,y)
     double x,y;
{
  return((x*x + y*y) <= 1.0);
}



static void step(self, x, y, side, inside, outside, depth, depth_limit)
     struct circlepiview *self;  
     double x, y, side;
     double *inside, *outside;
     int depth, depth_limit;
{
  struct rectangle vbounds, fill;
  struct graphic *tile;

  circlepiview_GetVisualBounds(self, &vbounds);
  if (depth < depth_limit) {
    if (inside_circle(x, y)) {
      *inside += (side*side)/(double)(4.0);
      if (self->color) {
	  circlepiview_SetFGColor(self, 0.0, 0.0, 1.0);
	  circlepiview_SetTransferMode(self, graphic_COPY);
	  tile = NULL;
      } else {
	  tile = circlepiview_GrayPattern(self, 2, 2);
      }

      fill.left = vbounds.left + (long)((x-side/(double)(2.0))*(double)(vbounds.width) + (double)(0.5));
      fill.top = vbounds.top + (long)((y-side/(double)(2.0))*(double)(vbounds.height) + (double)(0.5));
      fill.width = (long)((double)(vbounds.width) * side/(double)(2.0) + (double)(0.5));
      fill.height = (long)((double)(vbounds.height) * side/(double)(2.0)+ (double)(0.5));
      circlepiview_FillRect(self, &fill, tile);

      /* recurse on NE quadrant */
      step(self, x+side/(double)(4.0), y+side/(double)(4.0), side/(double)(2.0), inside, outside, depth+1, depth_limit);
    } else {
      *outside += (side*side)/(double)(4.0);
      if (self->color) {
	  circlepiview_SetFGColor(self, 1.0, 0.0, 0.0);
	  circlepiview_SetTransferMode(self, graphic_COPY);
	  tile = NULL;
      } else {
	  tile = circlepiview_GrayPattern(self, 0, 2);
      }
      fill.left = vbounds.left + (long)(x*(double)(vbounds.width) + (double)(0.5));
      fill.top = vbounds.top + (long)(y*(double)(vbounds.height) + (double)(0.5));
      fill.width = (long)((double)(vbounds.width) * side/(double)(2.0) + (double)(0.5));
      fill.height = (long)((double)(vbounds.height) * side/(double)(2.0)+ (double)(0.5));
      circlepiview_FillRect(self, &fill, tile);

      /* recurse on SW quadrant */
      step(self, x-side/(double)(4.0), y-side/(double)(4.0), side/(double)(2.0), inside, outside, depth+1, depth_limit);
    }
    step(self, x-side/(double)(4.0), y+side/(double)(4.0), side/(double)(2.0), inside, outside, depth+1, depth_limit);
    step(self, x+side/(double)(4.0), y-side/(double)(4.0), side/(double)(2.0), inside, outside, depth+1, depth_limit);
    
  } /* if (depth < depth_limit) */

  return;
}


void
circlepiview__Update(self)
struct circlepiview *self;  
{
/*
  Redisplay this object.
*/
  struct circlepi *b = (struct circlepi *) circlepiview_GetDataObject(self);
  struct graphic *tile;
  struct rectangle vbounds;
  double inside = 0.0, outside = 0.0;
  char msg[80];

  if (self->need_full_update) {
    circlepiview_EraseVisualRect(self);
    if (self->color) {
	circlepiview_SetFGColor(self, 0.0, 1.0, 0.0);
	circlepiview_SetTransferMode(self, graphic_COPY);
	tile = NULL;
    } else {
	tile = circlepiview_GrayPattern(self, 1, 2);
    }
    circlepiview_GetVisualBounds(self, &vbounds);
    circlepiview_FillRect(self, &vbounds, tile);

    step(self, 0.5, 0.5, 1.0, &inside, &outside, 0, circlepi_GetDepth(b));

    sprintf(msg, "pi, inside:  %f;  outside:  %f;  unknown:  %3f%%.",
	    inside * 4.0,
	    (1.0 - outside) * 4.0,
	    (1.0 - (inside + outside))/1.0*100.0);
    message_DisplayString(self, 1, msg);

    self->need_full_update = FALSE;
  } else {
				/* compute delta */
  }
}



struct view *
circlepiview__Hit(self, action, x, y, numclicks)
struct circlepiview *self;
long x, y;
enum view_MouseAction action;
long numclicks;  
{
/*
  Handle the button event.  Currently, semantics are:
  left up  --  receive input focus (to post menus)
*/
    struct circlepi *b = (struct circlepi *) circlepiview_GetDataObject(self);  

    switch (action) {
      case view_LeftUp:
	circlepiview_WantInputFocus(self,self);
	break;
      default:
	break;
    }
    return((struct view *)self);
}


static void
LimitProc(self, param)
     struct circlepiview *self;
     long param;
{
    struct circlepi *c = (struct circlepi *)circlepiview_GetDataObject(self);

    circlepi_SetDepth(c, param);
}
