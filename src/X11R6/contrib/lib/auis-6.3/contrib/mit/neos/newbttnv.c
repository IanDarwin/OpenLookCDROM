/* $Author: rr2b $ */

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/mit/neos/RCS/newbttnv.c,v 1.3 1992/12/15 21:55:51 rr2b R6tape $";
#endif


 
/*
 * newbuttonv.c
*/

/* ************************************************************
 *  Copyright (C) 1989, 1990, 1991
 *  by the Massachusetts Institute of Technology
 *   For full copyright information see:'mit-copyright.h'     *
 *  Adapted from ATK pushbuttonview by Nick Williams, July 1989
 *************************************************************/

#include <mit-copyright.h>
#include <stdio.h>
#include <class.h>
#include <newbttnv.eh>
#include <pshbttn.ih>
#include <andrewos.h>
#include <buffer.ih>
#include <complete.ih>
#include <cursor.ih>
#include <environ.ih>
#include <fontdesc.ih>
#include <frame.ih>
#include <graphic.ih>
#include <im.ih>
#include <menulist.ih>
#include <message.ih>
#include <observe.ih>
#include <proctbl.ih>
#include <view.ih>

#include "eos.h"

/* Defined constants and macros */
#define NO_MSG "Newbutton (v1)"
#define THREEDEE 2
#define ARBCON 3

#define PROMPTFONT "andy12b"
#define FONTTYPE fontdesc_Bold
#define FONTSIZE 12
#define BUTTONPRESSDEPTH 2
#define TEXTPAD 2

/* External Declarations */

/* Forward Declarations */

/* Global Variables */
static struct atom *pushedtrigger;

boolean newbuttonview__InitializeClass(c)
struct classheader *c;
{
/* 
  Initialize all the class data.
*/

  pushedtrigger = atom_Intern("buttonpushed");
  return(pushedtrigger != NULL);
}


boolean newbuttonview__InitializeObject(c, self)
struct classheader *c;
struct newbuttonview *self;
{
/*
  Set up the data for each instance of the object.
*/
    self->lit = 0;

    if (!(self->cursor = cursor_Create(self))) return(FALSE);
    cursor_SetStandard(self->cursor, Cursor_Arrow);

    self->bfont = (char *) malloc(33);
    self->depth = environ_GetProfileInt("buttondepth", 4);
    strcpy(self->bfont, "helvetica");
    self->bfontsize = 12;
    self->enabled = TRUE;
    observable_DefineTrigger(self, pushedtrigger);

    return(TRUE);
}

void newbuttonview__FinalizeObject(c, self)
struct classheader *c;
struct newbuttonview *self;
{
  if (self->cursor) cursor_Destroy(self->cursor);
  self->cursor = NULL;
  free(self->bfont);
  self->bfont = NULL;
  return;
}

/* The code below is from the value buttonview class */
void db(self,foo,fo)
struct newbuttonview * self;
struct rectangle *foo,*fo;
{
    newbuttonview_DrawRect(self, fo);
    newbuttonview_DrawRect(self,foo);   
    newbuttonview_MoveTo( self, fo->left, fo->top );
    newbuttonview_DrawLineTo( self, foo->left, foo->top);
    newbuttonview_MoveTo( self, fo->left + fo->width, fo->top );
    newbuttonview_DrawLineTo( self, foo->left + foo->width, foo->top);
    newbuttonview_MoveTo( self, fo->left , fo->top + fo->height);
    newbuttonview_DrawLineTo( self, foo->left, foo->top + foo->height);
    newbuttonview_MoveTo( self, fo->left + fo->width, fo->top + fo->height);
    newbuttonview_DrawLineTo( self, foo->left + foo->width, foo->top + foo->height);

}



void newbuttonview__FullUpdate(self, type, left, top, width, height)
struct newbuttonview *self;
enum view_UpdateType type;
long left, top, width, height;
{
/*
  Redisplay this object.  Specifically, set my font, and put my text label
  in the center of my display box.
*/

  struct rectangle Rect, Rect2, Rect3;
  struct pushbutton *b = (struct pushbutton *) newbuttonview_GetDataObject(self);
  struct fontdesc *my_fontdesc;
  struct graphic *my_graphic;
  struct FontSummary *my_FontSummary;
  int r2_bot, r_bot;
  int tx = 0, ty = 0;
  short t_op;
  char text[128];
  int style;

  if (b) {
    style = pushbutton_GetStyle(b);
    newbuttonview_GetLogicalBounds(self, &Rect);
    newbuttonview_PostCursor(self, &Rect, self->cursor);
    my_graphic = (struct graphic *)newbuttonview_GetDrawable(self);
    if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
      my_fontdesc= fontdesc_Create(self->bfont, FONTTYPE, self->bfontsize);
    }
    if (my_fontdesc) {
      newbuttonview_SetFont(self, my_fontdesc);
      my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
    }

    newbuttonview_SetTransferMode(self, graphic_SOURCE);

    t_op = graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE;
    strcpy(text, pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG);
    if (!self->enabled) {
        char *temporary;
        for (temporary = text; *temporary != '\0'; *temporary++ = ' ');
    } 
      /* Rect2 is the inner (Text) region */
      Rect2.top    = Rect.top + self->depth;
      Rect2.left   = Rect.left + self->depth;
      Rect2.width  = Rect.width - 2*self->depth;
      Rect2.height = Rect.height - 2*self->depth;
      Rect3.top    = Rect2.top;
      Rect3.left   = Rect2.left + self->depth;
      Rect3.width  = Rect2.width - 2*self->depth;
      Rect3.height = Rect2.height - 2*self->depth;
      r2_bot = (Rect2.top)+(Rect2.height);
      r_bot = (Rect.top)+(Rect.height);
      tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;

    switch (style) {
    case THREEDEE:
      ty = Rect2.top + Rect2.height/2;

      newbuttonview_FillRectSize(self, Rect.left, Rect.top, self->depth, Rect.height, newbuttonview_GrayPattern(self, 1, 4));	/* left bar */

      newbuttonview_FillRectSize(self, Rect.left + Rect.width - self->depth, Rect.top, self->depth, Rect.height, newbuttonview_GrayPattern(self, 3, 4)); /* right bar */

      newbuttonview_FillTrapezoid(self, Rect2.left, r2_bot, Rect2.width, Rect.left, r_bot, Rect.width, newbuttonview_GrayPattern(self, 3, 4)); /* lower trapz */

      newbuttonview_FillTrapezoid(self, Rect.left, Rect.top, Rect.width, Rect2.left, Rect2.top, Rect2.width, newbuttonview_GrayPattern(self, 1, 4)); /* upper trapz */

      newbuttonview_FillRect(self, &Rect2, newbuttonview_GrayPattern(self,1,2)); /* the middle box */

      newbuttonview_SetTransferMode(self, graphic_WHITE);
      newbuttonview_MoveTo(self, tx+1, ty);
      newbuttonview_DrawString(self, text, t_op);
      newbuttonview_MoveTo(self, tx, ty+1);
      newbuttonview_DrawString(self, text, t_op);
      newbuttonview_MoveTo(self, tx+1, ty+1);
      newbuttonview_DrawString(self, text, t_op);
      newbuttonview_SetTransferMode(self, graphic_BLACK);
      newbuttonview_MoveTo(self, tx, ty);
      newbuttonview_DrawString(self, text, t_op);
      break;
    case ARBCON:
      newbuttonview_SetTransferMode(self, graphic_COPY);
      newbuttonview_FillRect(self, &Rect, newbuttonview_GrayPattern(self, 4, 16));
      newbuttonview_SetTransferMode(self, graphic_WHITE);
      newbuttonview_FillRect(self, &Rect2, 0);
      newbuttonview_SetTransferMode(self, graphic_COPY);      
      db(self, &Rect3, &Rect2);
      newbuttonview_MoveTo(self, Rect3.left + Rect3.width/2, Rect3.top + Rect3.height/2);
      newbuttonview_DrawString(self, text, t_op);
      break;
    default:
      tx = (Rect.left + Rect.width) / 2;
      if (my_FontSummary)
	ty = (Rect.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);

      newbuttonview_SetTransferMode(self, graphic_SOURCE);
      newbuttonview_MoveTo(self, tx, ty);
      newbuttonview_DrawString(self, text, t_op);
      break;
    } /* switch (style) */

  } /* if (b) */
}


void newbuttonview__Update(self)
struct newbuttonview *self;  
{
/*
  Do an update.  Just set up the call to FullUpdate method.
*/
    struct rectangle r;

    newbuttonview_EraseVisualRect(self);
    newbuttonview_GetLogicalBounds(self, &r);
    newbuttonview_FullUpdate(self, 0, r.left, r.top, r.width, r.height);
}


static int RectEnclosesXY(r, x, y)
struct rectangle *r;
long x, y;
{
  return(   ( ((r->top)  <= y) && ((r->top + r->height) >= y) )
	 && ( ((r->left) <= x) && ((r->left + r->width) >= x) )
	 );
}


static void HighlightButton(self)
struct newbuttonview *self;
{
  struct pushbutton *b = (struct pushbutton *) newbuttonview_GetDataObject(self);
  struct rectangle Rect, Rect2, Rect3;
  int style;
  struct fontdesc *my_fontdesc;
  int tx, ty;
  short t_op;
  char *text;
  
  if (!(self->lit)) {
    style = pushbutton_GetStyle(b);
    newbuttonview_GetLogicalBounds(self, &Rect);
    
    Rect2.top    = Rect.top + self->depth;
    Rect2.left   = Rect.left + self->depth;
    Rect2.width  = Rect.width - 2*self->depth;
    Rect2.height = Rect.height - 2*self->depth;
    Rect3.top    = Rect2.top;
    Rect3.left   = Rect2.left + self->depth;
    Rect3.height = Rect2.height - 2*self->depth;
    Rect3.width  = Rect2.width - 2*self->depth;
    if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
        my_fontdesc= fontdesc_Create(self->bfont, FONTTYPE, self->bfontsize);
    }
    text = pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG;
    if (my_fontdesc)
	newbuttonview_SetFont(self, my_fontdesc);
    t_op = graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE;
	  
    switch(style) {

    case THREEDEE:
      tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;
      ty = Rect2.top + Rect2.height/2;
      newbuttonview_SetTransferMode(self, graphic_WHITE);
      newbuttonview_MoveTo(self, tx, ty);
      newbuttonview_DrawString(self, text, t_op);
      break;

    case ARBCON:
      Rect3.top    = Rect2.top;
      Rect3.left   = Rect2.left + self->depth;
      Rect3.height = Rect2.height - 2*self->depth;
      Rect3.width  = Rect2.width - 2*self->depth;
      my_fontdesc = fontdesc_Create(self->bfont, fontdesc_Bold, self->bfontsize);
      newbuttonview_SetFont(self, my_fontdesc);
      newbuttonview_SetTransferMode(self, graphic_WHITE);
      newbuttonview_FillRect(self, &Rect3, 0);
      newbuttonview_SetTransferMode(self, graphic_COPY);      
      db(self, &Rect3, &Rect2);
      newbuttonview_MoveTo(self, Rect3.left + Rect3.width/2, Rect3.top + Rect3.height/2);
      newbuttonview_DrawString(self, text, t_op);
      break;
      
    default:
      newbuttonview_SetTransferMode(self, graphic_INVERT);
      newbuttonview_FillRect(self,&Rect,newbuttonview_BlackPattern(self));
      break;
    }
  }
  self->lit = 1;
}


static void UnhighlightButton(self)
struct newbuttonview *self;
{
  struct pushbutton *b = (struct pushbutton *) newbuttonview_GetDataObject(self);
  struct rectangle Rect, Rect2, Rect3;
  int style;
  struct fontdesc *my_fontdesc;
  int tx, ty;
  short t_op;
  char *text;
  
  if (self->lit) {
    style = pushbutton_GetStyle(b);
    newbuttonview_GetLogicalBounds(self, &Rect);
    text = pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG;
    /* Rect2 is the inner (Text) region */
    Rect2.top = Rect.top + self->depth;
    Rect2.left = Rect.left + self->depth;
    Rect2.width = Rect.width - 2*self->depth;
    Rect2.height = Rect.height - 2*self->depth;
    if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
	my_fontdesc= fontdesc_Create(self->bfont, FONTTYPE, self->bfontsize);
    }
    if (my_fontdesc)
	newbuttonview_SetFont(self, my_fontdesc);
    t_op = graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBASELINE;
   
    switch(style) {
    case THREEDEE:
      tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;
      ty = Rect2.top + Rect2.height/2;
      newbuttonview_SetTransferMode(self, graphic_BLACK);
      newbuttonview_MoveTo(self, tx, ty);
      newbuttonview_DrawString(self, text, t_op);
      break;

    case ARBCON:
      Rect3.top    = Rect2.top;
      Rect3.left   = Rect2.left + self->depth;
      Rect3.height = Rect2.height - 2*self->depth;
      Rect3.width  = Rect2.width - 2*self->depth;
      my_fontdesc = fontdesc_Create(self->bfont, fontdesc_Plain, self->bfontsize);
      newbuttonview_SetFont(self, my_fontdesc);
      newbuttonview_SetTransferMode(self, graphic_WHITE);
      newbuttonview_FillRect(self, &Rect3, 0);
      newbuttonview_SetTransferMode(self, graphic_COPY);      
      db(self, &Rect3, &Rect2);
      newbuttonview_MoveTo(self, Rect3.left + Rect3.width/2, Rect3.top + Rect3.height/2);
      newbuttonview_DrawString(self, text, t_op);
      break;
      
    default:
      newbuttonview_SetTransferMode(self, graphic_INVERT);
      newbuttonview_FillRect(self,&Rect,newbuttonview_BlackPattern(self));
      break;
    }
  }
  self->lit = 0;
}


struct view *newbuttonview__Hit(self, action, x, y, numclicks)
struct newbuttonview *self;
long x, y;
enum view_MouseAction action;
long numclicks;  
{
/*
  Handle the button event.  Currently, semantics are:
    
    Left Down  -- Draw button pressed
    Right Down -- select button (Receive input focus, for menuing without activating)
    Left Up    -- draw button at rest, pull trigger
    Right Up   -- No Op
    Left Movement     -- unhighlight if moved off, highlight if moved on
    Right Movement -- No Op
*/
  struct cursor *wait_cursor;
  if (!self->enabled) return (struct view *) self;

  switch (action) {
  case view_LeftDown: 
    HighlightButton(self);
    newbuttonview_WantInputFocus(self,self);
    break;
  case view_LeftMovement:
    {
      struct rectangle r;

      newbuttonview_GetVisualBounds(self, &r);
      if (RectEnclosesXY(&r, x, y))
	HighlightButton(self);
      else
	UnhighlightButton(self);
    }
    break;
  case view_LeftUp:
    {
      short litp = self->lit;

      UnhighlightButton(self);
      if (litp) {
	if (wait_cursor = cursor_Create(self)) {
	  cursor_SetStandard(wait_cursor, Cursor_Wait);
	  im_SetProcessCursor(wait_cursor);
	  newbuttonview_PullTrigger(self, pushedtrigger);
	  im_SetProcessCursor(NULL);
	  cursor_Destroy(wait_cursor);
	}
      }
    }
    break;
  case view_RightDown:
    newbuttonview_WantInputFocus(self, self);
    break;
  }
  return((struct view *)self);
}


enum view_DSattributes newbuttonview__DesiredSize(self, width, height, pass, desired_width, desired_height)
struct newbuttonview *self;
long width;
long height;
enum view_DSpass pass;
long *desired_width;
long *desired_height;
{
/* 
  Tell parent that this object  wants to be as big as the box around its
  text string.  For some reason IM allows resizing of this object. (BUG)
*/

  struct fontdesc *my_fontdesc;
  struct FontSummary *my_FontSummary;
  struct graphic *my_graphic;
  struct pushbutton *b = (struct pushbutton *) newbuttonview_GetDataObject(self);
  int style;

  style = pushbutton_GetStyle(b);

  my_graphic = (struct graphic *)newbuttonview_GetDrawable(self);
  if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
    my_fontdesc= fontdesc_Create(self->bfont, FONTTYPE, self->bfontsize);
  }
  if (my_fontdesc) {
    fontdesc_StringSize(my_fontdesc, my_graphic, pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG, desired_width, desired_height);
    my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
  }

  switch (style) {
  case THREEDEE:
    *desired_width = *desired_width + 2*TEXTPAD + 2*self->depth;
    if (my_FontSummary)
      *desired_height = my_FontSummary->maxHeight + 2*TEXTPAD + 2*self->depth;
    break;
  default:
    if (my_FontSummary)
      *desired_height = my_FontSummary->maxHeight;
    break;
  }

/*
  (BUG) I don't check to see if I can specify a size, I just do it.
  Will this break things?  What if I can't change my size?  Will I be
  Ugly?  What to do, what to do....
*/

  return(view_Fixed); /* (BUG) should disable user sizing, but this doesn't */
}


void newbuttonview__GetOrigin(self, width, height, originX, originY)
struct newbuttonview *self;
long width, height;
long *originX, *originY;
{
/*
  We want this object to sit in-line with text, not below the baseline.
  Simply, we could negate the height as the originX, but then our
  text would be too high.  So, instead, we use the height of
  our font above the baseline
*/

  struct FontSummary *my_FontSummary;
  struct fontdesc *my_fontdesc;
  struct graphic *my_graphic;
  struct pushbutton *b = (struct pushbutton *) newbuttonview_GetDataObject(self);
  int style;

  style = pushbutton_GetStyle(b);

  my_graphic = (struct graphic *)newbuttonview_GetDrawable(self);
  if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
    my_fontdesc= fontdesc_Create(self->bfont, FONTTYPE, self->bfontsize);
  }
  if (my_fontdesc) {
      my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
  }

  *originX = 0;
  switch(style) {
  case THREEDEE:
    if (my_FontSummary) 
      *originY = (my_FontSummary->maxHeight) - (my_FontSummary->maxBelow) + 1 + TEXTPAD + self->depth;
    break;
  default:
    if (my_FontSummary)
      *originY = (my_FontSummary->maxHeight) - (my_FontSummary->maxBelow) + 1;
    break;
  }
  return;
}

void newbuttonview__Enable(self, want_to_enable)
struct newbuttonview *self;
boolean want_to_enable;
{
    struct pushbutton *b;
    if (self->enabled == want_to_enable) return;

    self->enabled = want_to_enable;
    b = (struct pushbutton *) newbuttonview_GetDataObject(self);

    if (want_to_enable)
        pushbutton_EnableTrigger(b, pushedtrigger);
    else
        pushbutton_DisableTrigger(b, pushedtrigger);

    newbuttonview_WantUpdate(self, self);
}

boolean newbuttonview__IsEnabled(self)
struct newbuttonview *self;
{
    return self->enabled;
}
