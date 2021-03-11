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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/hyplink/RCS/pshbttnv.c,v 1.27 1994/01/10 21:02:56 rr2b Exp $";
#endif

#include <sys/param.h>	/* for MAXPATHLEN */
#include <stdio.h>
#include <class.h>
#include <andrewos.h>
#include <pshbttn.ih>
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
#include <txttroff.ih>
#include <pshbttnv.eh>

/* Defined constants and macros */
#define CURSORON 1		/* yes, use crosshairs cursor over button */
#define NOROUNDRECT 1		/* round rect is broken */
#if 0
#define DEBUG 1			/* turn on debugging */
#endif
#ifdef DEBUG
#define DBG(x) fprintf(stderr, "\nDebug: %s.", x);fflush(stderr);
#else
#define DBG(x) ;
#endif

#define NO_MSG "Push Me"

#define PROMPTFONT "andysans12b"
#define FONT "andysans"
#define FONTTYPE fontdesc_Bold
#define FONTSIZE 12
#define BUTTONDEPTH 4
#define ULSHADE 0.25 /* upper & left sides */
#define LRSHADE 0.75 /* lower & right sides */
#define TOPSHADE 0.50 /* face of button */
#define MOTIFBUTTONDEPTH 2
#define MOTIFULSHADE 0.10
#define MOTIFLRSHADE 0.50
#define MOTIFTOPSHADE 0.25
#define BUTTONPRESSDEPTH 2
#define TEXTPAD 2

/* External Declarations */

/* Forward Declarations */
static void LabelProc(), FontProc(), StyleProc(), ColorProc();

/* Global Variables */
static struct atom *pushedtrigger;
static struct menulist *menulist = NULL;


static void
pushbuttonview_setShade(self, pct)
     struct pushbuttonview *self;
     double pct;			/* 0.0 -> 1.0 */
{
    pushbuttonview_SetFGColor(self, 
			      self->foreground_color[0]*pct 
			      + self->background_color[0]*(1.0-pct), 
			      self->foreground_color[1]*pct 
			      + self->background_color[1]*(1.0-pct), 
			      self->foreground_color[2]*pct 
			      + self->background_color[2]*(1.0-pct));

} /* pushbuttonview_setShade */


boolean
pushbuttonview__InitializeClass(c)
     struct classheader *c;
{
    /* 
      Initialize all the class data.
      Set up the proc table entries and the menu list 
      (which is cloned for each instance of this class).
      */
    struct proctable_Entry *proc = NULL;

    if ((pushedtrigger = atom_Intern("buttonpushed")) == NULL) return(FALSE);

    if ((menulist = menulist_New()) == NULL) return(FALSE);

    if ((proc = proctable_DefineProc("pushbuttonview-set-label-text", LabelProc, &pushbuttonview_classinfo, NULL, "Prompts for user to set the text string of the pushbutton.")) == NULL) return(FALSE);
    menulist_AddToML(menulist, "Pushbutton~1,Set Label~11", proc, NULL, 0);
    
    if ((proc = proctable_DefineProc("pushbuttonview-set-font", FontProc, &pushbuttonview_classinfo, NULL, "Prompts for user to set the font of the pushbutton.")) == NULL) return(FALSE);
    menulist_AddToML(menulist, "Pushbutton~1,Set Font~12", proc, NULL, 0);
    
    if ((proc = proctable_DefineProc("pushbuttonview-set-style", StyleProc, &pushbuttonview_classinfo, NULL, "Prompts for user to set the appearance of the pushbutton.")) == NULL) return(FALSE);
    menulist_AddToML(menulist, "Pushbutton~1,Set Style~13", proc, NULL, 0);
    
    if ((proc = proctable_DefineProc("pushbuttonview-set-color", ColorProc, &pushbuttonview_classinfo, NULL, "Prompts for user to set the foreground and background color of the pushbutton.")) == NULL) return(FALSE);
#ifndef PL8			/* users of PL8 can't set color */
    menulist_AddToML(menulist, "Pushbutton~1,Set Color~14", proc, NULL, 0);
#endif /* PL8 */
    
    return(TRUE);
}


boolean
pushbuttonview__InitializeObject(c, self)
struct classheader *c;
struct pushbuttonview *self;
{
/*
  Set up the data for each instance of the object.
*/
    int i;

    self->lit = 0;
    self->awaitingUpdate = 0;
    self->ml = menulist_DuplicateML(menulist, self);
    self->cached_label = "NOT YET CACHED";
    self->cached_style = -1;
    self->cached_fontdesc = NULL;
    for (i = 0 ; i < 3; i++) {
	self->foreground_color[i] = 0.0;
	self->background_color[i] = 0.0;
    }
    
#if CURSORON
    if (!(self->cursor = cursor_Create(self))) return(FALSE);
    cursor_SetStandard(self->cursor, Cursor_Gunsight);
#endif /* CURSORON */
    observable_DefineTrigger(self, pushedtrigger);
    return(TRUE);
}


void
pushbuttonview__PostMenus(self, ml)
     struct pushbuttonview *self;
     struct menulist *ml;
{
    /*
      Enable the menus for this object.
      */

    menulist_ClearChain(self->ml);
    if (ml) menulist_ChainAfterML(self->ml, ml, ml);
    super_PostMenus(self, self->ml);
}


static void
pushbuttonview_CacheSettings(self, b, updateflag)
     struct pushbuttonview *self;
     struct pushbutton *b;
     int updateflag;		/* call WantUpdate if necessary */
{
    char *fgcolor, *bgcolor;
    unsigned char fg_rgb[3], bg_rgb[3];
    double get_rgb[3];
    int i;
    char *pb_label;
    int pb_style;
    struct fontdesc *pb_font;
    int UpdateNeeded, NewSize;

    if (b && pushbuttonview_GetIM(self)) {
	    /* these calls don't make sense if there is no IM or button, 
	       (in fact they seg fault!) */

	UpdateNeeded = 0;
	NewSize = 0;		/* A new size requires an update */

	pb_label = pushbutton_GetText(b);
	if (pb_label != self->cached_label) {
	    self->cached_label = pb_label;
	    NewSize = 1;
	}

	pb_style = pushbutton_GetStyle(b);
	if (pb_style != self->cached_style) {
	    self->cached_style = pb_style;
	    NewSize = 1;
	}

	pb_font = pushbutton_GetButtonFont(b);
	if (pb_font != self->cached_fontdesc) {
	    self->cached_fontdesc = pb_font;
	    NewSize = 1;
	}

	fgcolor = pushbutton_GetFGColor(b, fg_rgb);
	bgcolor = pushbutton_GetBGColor(b, bg_rgb);

	/* We need to set the colors to find out their interpretation 
	   on the window server. */
	pushbuttonview_SetForegroundColor(self, 
					  fgcolor, 
					  fg_rgb[0]*256L, 
					  fg_rgb[1]*256L,
					  fg_rgb[2]*256L);
	pushbuttonview_SetBackgroundColor(self,
					  bgcolor,
					  bg_rgb[0]*256L,
					  bg_rgb[1]*256L,
					  bg_rgb[2]*256L);
	
	pushbuttonview_GetFGColor(self, 
				  &(get_rgb[0]), 
				  &(get_rgb[1]), 
				  &(get_rgb[2]));
	for(i = 0; i < 3; i++) {
	    if (get_rgb[i] != self->foreground_color[i]) {
		self->foreground_color[i] = get_rgb[i];
		UpdateNeeded = 1; /* color changed, not size */
	    }
	}

	pushbuttonview_GetBGColor(self, 
				  &(get_rgb[0]), 
				  &(get_rgb[1]), 
				  &(get_rgb[2]));
	for(i = 0; i < 3; i++) {
	    if (get_rgb[i] != self->background_color[i]) {
		self->background_color[i] = get_rgb[i];
		UpdateNeeded = 1; /* color changed, not size */
	    }
	}

	if (NewSize) {
	    pushbuttonview_WantNewSize(self,self);
	}
	if (updateflag && (NewSize || UpdateNeeded)) {
	    pushbuttonview_WantUpdate(self, self);
	}
    } /* if (b && im) */

}     


void
pushbuttonview__LinkTree(self, parent)
     struct pushbuttonview *self;
     struct view *parent;
{
    super_LinkTree(self, parent);

    pushbuttonview_CacheSettings(self, (struct pushbutton *) pushbuttonview_GetDataObject(self), 0);

} /* pushbuttonview__LinkTree */


void
pushbuttonview__FinalizeObject(c, self)
struct classheader *c;
struct pushbuttonview *self;
{
#if CURSORON
  if (self->cursor) cursor_Destroy(self->cursor);
  self->cursor = NULL;
#endif /* CURSORON */
  if(self->ml) menulist_Destroy(self->ml);
  return;
}


void
pushbuttonview__FullUpdate(self, type, left, top, width, height)
struct pushbuttonview *self;
enum view_UpdateType type;
long left, top, width, height;
{
/*
  Redisplay this object.  Specifically, set my font, and put my text label
  in the center of my display box.
*/

  struct rectangle Rect, Rect2;
  struct pushbutton *b = (struct pushbutton *) pushbuttonview_GetDataObject(self);
  int bdepth, r2_bot, r_bot;
  double ulshade, lrshade, topshade;
  int tx = 0, ty = 0;
  short t_op;
  char *text;
  int style;
  struct fontdesc *my_fontdesc;
  struct graphic *my_graphic;
  struct FontSummary *my_FontSummary;
  int redraw;

  self->awaitingUpdate = 0;

  pushbuttonview_GetLogicalBounds(self, &Rect);

  switch (type) {
    case view_FullRedraw:
    case view_LastPartialRedraw:
      pushbuttonview_RetractCursor(self, self->cursor);
      pushbuttonview_PostCursor(self, &Rect, self->cursor);
      redraw = 1;
      break;
    case view_MoveNoRedraw:
#if CURSORON
      pushbuttonview_RetractCursor(self, self->cursor);
      pushbuttonview_PostCursor(self, &Rect, self->cursor);
#endif /* CURSORON */
      redraw = 0;
      break;
    case view_PartialRedraw:
      redraw = 0;
      break;
    case view_Remove:
#if CURSORON
      pushbuttonview_RetractCursor(self, self->cursor);
#endif /* CURSORON */
      redraw = 0;
      break;
    default:
      redraw = 1;
  }


  if (b && redraw) {
    style = pushbutton_GetStyle(b);
    my_graphic = (struct graphic *)pushbuttonview_GetDrawable(self);
    if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
      my_fontdesc= fontdesc_Create(FONT, FONTTYPE, FONTSIZE);
    }
    if (my_fontdesc) {
      pushbuttonview_SetFont(self, my_fontdesc);
      my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
    }

    pushbuttonview_SetTransferMode(self, graphic_SOURCE);
    if ((style != pushbutton_THREEDEE) && (style != pushbutton_MOTIF)) {
	/* Erase with BG color, only if style is not 3-D (3-D draws all bits) */
	pushbuttonview_setShade(self, 0.0);
	pushbuttonview_FillRect(self, &Rect, NULL);
    }
    pushbuttonview_setShade(self, 1.0);

    t_op = graphic_BETWEENLEFTANDRIGHT | graphic_ATBASELINE;
    text = pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG;
    ty = 0;			/* default, in case my_FontSummary is NULL */

    switch (style) {
    case pushbutton_BOXEDRECT:
      Rect.width--;
      Rect.height--;
      /* Rect2 is the inner rect */
      Rect2.top = Rect.top + BUTTONDEPTH;
      Rect2.left = Rect.left + BUTTONDEPTH;
      Rect2.width = Rect.width - 2*BUTTONDEPTH;
      Rect2.height = Rect.height - 2*BUTTONDEPTH;
      tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;
      if (my_FontSummary)
	ty = TEXTPAD + (Rect2.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);

      pushbuttonview_SetTransferMode(self, graphic_COPY);
      pushbuttonview_DrawRect(self, &Rect);
      pushbuttonview_DrawRect(self, &Rect2);
      pushbuttonview_MoveTo(self, tx, ty);
      pushbuttonview_DrawString(self, text, t_op);
      break;

    case pushbutton_ROUNDRECT:
      /* THIS CODE IS STILL BROKEN, DUE TO A LACK OF DOCUMENTATION ON RRect! */
      /* Rect2 is the inner rect for DrawRRect */
      Rect2.top = Rect.top;
      Rect2.left = Rect.left;
      Rect2.height = BUTTONDEPTH;
      Rect2.width = BUTTONDEPTH;
      Rect.width -= 1;
      Rect.height -= BUTTONDEPTH;

      tx = TEXTPAD + (Rect.left + Rect.width) / 2;
      if (my_FontSummary)
	ty = TEXTPAD + (Rect.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);

      pushbuttonview_SetTransferMode(self, graphic_COPY);
      pushbuttonview_DrawRRect(self, &Rect, &Rect2);
      pushbuttonview_MoveTo(self, tx, ty);
      pushbuttonview_DrawString(self, text, t_op);
      break;

    case pushbutton_MOTIF:
    case pushbutton_THREEDEE:
      if (style == pushbutton_MOTIF) {
	  bdepth = MOTIFBUTTONDEPTH;
	  ulshade = MOTIFULSHADE;
	  lrshade = MOTIFLRSHADE;
	  topshade = MOTIFTOPSHADE;
      } else {
	  bdepth = BUTTONDEPTH;
	  ulshade = ULSHADE;
	  lrshade = LRSHADE;
	  topshade = TOPSHADE;
      }


      /* Rect2 is the inner (Text) region */
      Rect2.top = Rect.top + bdepth;
      Rect2.left = Rect.left + bdepth;
      Rect2.width = Rect.width - 2*bdepth;
      Rect2.height = Rect.height - 2*bdepth;
      r2_bot = (Rect2.top)+(Rect2.height);
      r_bot = (Rect.top)+(Rect.height);

      tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;
      if (my_FontSummary) {
	ty = TEXTPAD + (Rect2.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);
      }

#ifdef WM_ENV
      if (im_WhichWS(pushbuttonview_GetIM(self))[0] == 'w') {
          pushbuttonview_FillRectSize(self, Rect.left, Rect.top, bdepth, Rect.height, pushbuttonview_GrayPattern(self, 1, 4));	/* left bar */

          pushbuttonview_FillRectSize(self, Rect.left + Rect.width - bdepth, Rect.top, bdepth, Rect.height, pushbuttonview_GrayPattern(self, 3, 4)); /* right bar */

          pushbuttonview_FillTrapezoid(self, Rect2.left, r2_bot, Rect2.width, Rect.left, r_bot, Rect.width, pushbuttonview_GrayPattern(self, 3, 4)); /* lower trapz */

          pushbuttonview_FillTrapezoid(self, Rect.left, Rect.top, Rect.width, Rect2.left, Rect2.top, Rect2.width, pushbuttonview_GrayPattern(self, 1, 4)); /* upper trapz */

          pushbuttonview_FillRect(self, &Rect2, pushbuttonview_GrayPattern(self,1,2)); /* the middle box */
      }
      else
#endif /* WM_ENV */
      {
          pushbuttonview_SetTransferMode(self, graphic_COPY);
          pushbuttonview_setShade(self, ulshade);
          pushbuttonview_FillRectSize(self, Rect.left, Rect.top, bdepth, Rect.height, NULL);	/* left bar */

          pushbuttonview_setShade(self, lrshade);
          pushbuttonview_FillRectSize(self, Rect.left + Rect.width - bdepth, Rect.top, bdepth, Rect.height, NULL); /* right bar */
          pushbuttonview_FillTrapezoid(self, Rect2.left, r2_bot, Rect2.width, Rect.left, r_bot, Rect.width, NULL); /* lower trapz */

          pushbuttonview_setShade(self, ulshade);
          pushbuttonview_FillTrapezoid(self, Rect.left, Rect.top, Rect.width, Rect2.left, Rect2.top, Rect2.width, NULL); /* upper trapz */

          pushbuttonview_setShade(self, topshade);
          pushbuttonview_FillRect(self, &Rect2, NULL); /* the middle box */

      }

      pushbuttonview_SetTransferMode(self, graphic_BLACK);
      if (style != pushbutton_MOTIF) {
          pushbuttonview_setShade(self, 0.0);
          pushbuttonview_MoveTo(self, tx+1, ty);
          pushbuttonview_DrawString(self, text, t_op);
          pushbuttonview_MoveTo(self, tx, ty+1);
          pushbuttonview_DrawString(self, text, t_op);
          pushbuttonview_MoveTo(self, tx+1, ty+1);
          pushbuttonview_DrawString(self, text, t_op);
      }
      pushbuttonview_setShade(self, 1.0);
      pushbuttonview_MoveTo(self, tx, ty);
      pushbuttonview_DrawString(self, text, t_op);
      break;

    case pushbutton_PLAINBOX:
      Rect.width--;
      Rect.height--;
      pushbuttonview_DrawRect(self, &Rect);

      tx = Rect.left + (Rect.width / 2);
      ty = Rect.top + (Rect.height / 2);

      pushbuttonview_MoveTo(self, tx, ty);
      t_op = graphic_BETWEENLEFTANDRIGHT | graphic_BETWEENTOPANDBOTTOM;
      pushbuttonview_DrawString(self, text, t_op);

      break;
 
   default: /* PLAIN */
     tx = (Rect.left + Rect.width) / 2;
      if (my_FontSummary)
	ty = (Rect.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);

      pushbuttonview_SetTransferMode(self, graphic_COPY);
      pushbuttonview_MoveTo(self, tx, ty);
      pushbuttonview_DrawString(self, text, t_op);
      break;
    } /* switch (style) */

  } /* if (b && redraw) */
  self->lit = 0;
}


void
pushbuttonview__Update(self)
struct pushbuttonview *self;  
{
/*
  Do an update.  Just set up the call to FullUpdate method.
*/
    struct rectangle r;

#if 0
    /* Shouldn't need this 'cuz full update fills the BG already  */
    pushbuttonview_EraseVisualRect(self);
#endif /* 0 */
    pushbuttonview_GetLogicalBounds(self, &r);
    pushbuttonview_FullUpdate(self, view_FullRedraw, r.left, r.top, r.width, r.height);
}


static int
RectEnclosesXY(r, x, y)
struct rectangle *r;
long x, y;
{
  return(   ( ((r->top)  <= y) && ((r->top + r->height) >= y) )
	 && ( ((r->left) <= x) && ((r->left + r->width) >= x) )
	 );
}


static void
HighlightButton(self)
struct pushbuttonview *self;
{
  struct pushbutton *b = (struct pushbutton *) pushbuttonview_GetDataObject(self);
  struct rectangle Rect, Rect2;
  int style;
  struct fontdesc *my_fontdesc;
  struct graphic *my_graphic;
  struct FontSummary *my_FontSummary;
  int tx, ty;
  short t_op;
  char *text;
  int bdepth, r2_bot, r_bot;
  double ulshade, lrshade;
  
  if (!(self->lit)) {
    style = pushbutton_GetStyle(b);
    pushbuttonview_GetLogicalBounds(self, &Rect);
    
    switch (style) {
    case pushbutton_PLAIN:
    case pushbutton_PLAINBOX:
      pushbuttonview_SetTransferMode(self, graphic_INVERT);
      pushbuttonview_FillRect(self,&Rect,pushbuttonview_BlackPattern(self));
      break;

    case pushbutton_BOXEDRECT:
      /* Rect2 is the inner rect */
      Rect2.top = Rect.top + BUTTONDEPTH;
      Rect2.left = Rect.left + BUTTONDEPTH;
      Rect2.width = Rect.width - 2*BUTTONDEPTH;
      Rect2.height = Rect.height - 2*BUTTONDEPTH;

      pushbuttonview_SetTransferMode(self, graphic_INVERT);
      pushbuttonview_FillRect(self, &Rect,pushbuttonview_BlackPattern(self));
      pushbuttonview_FillRect(self, &Rect2,pushbuttonview_BlackPattern(self));

      break;
    case pushbutton_ROUNDRECT:
      /* Rect2 is the inner rect for DrawRRect */
      Rect2.top = Rect.top;
      Rect2.left = Rect.left;
      Rect2.height = BUTTONDEPTH;
      Rect2.width = BUTTONDEPTH;
      Rect.width -= 1;
      Rect.height -= BUTTONDEPTH;

      pushbuttonview_SetTransferMode(self, graphic_INVERT);
      pushbuttonview_FillRRect(self, &Rect, &Rect2, pushbuttonview_BlackPattern(self));

      break;
  
    case pushbutton_MOTIF:
    case pushbutton_THREEDEE:
      if (style == pushbutton_MOTIF) {
	  bdepth = MOTIFBUTTONDEPTH;
	  ulshade = MOTIFLRSHADE;
	  lrshade = MOTIFULSHADE;
      } else {
	  bdepth = BUTTONDEPTH;
	  ulshade = ULSHADE;
	  lrshade = LRSHADE;
      }

      /* Rect2 is the inner (Text) region */
      Rect2.top = Rect.top + bdepth;
      Rect2.left = Rect.left + bdepth;
      Rect2.width = Rect.width - 2*bdepth;
      Rect2.height = Rect.height - 2*bdepth;
      r2_bot = (Rect2.top)+(Rect2.height);
      r_bot = (Rect.top)+(Rect.height);

      if (style == pushbutton_MOTIF) {
          pushbuttonview_SetTransferMode(self, graphic_COPY);
          pushbuttonview_setShade(self, ulshade);
          pushbuttonview_FillRectSize(self, Rect.left, Rect.top, bdepth, Rect.height, NULL);	/* left bar */
	  
          pushbuttonview_setShade(self, lrshade);
          pushbuttonview_FillRectSize(self, Rect.left + Rect.width - bdepth, Rect.top, bdepth, Rect.height, NULL); /* right bar */
          pushbuttonview_FillTrapezoid(self, Rect2.left, r2_bot, Rect2.width, Rect.left, r_bot, Rect.width, NULL); /* lower trapz */
	  
          pushbuttonview_setShade(self, ulshade);
          pushbuttonview_FillTrapezoid(self, Rect.left, Rect.top, Rect.width, Rect2.left, Rect2.top, Rect2.width, NULL); /* upper trapz */
	  
          pushbuttonview_setShade(self, 1.0);
	  
      } else {
	  my_graphic = (struct graphic *)pushbuttonview_GetDrawable(self);
	  if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
	      my_fontdesc= fontdesc_Create(FONT, FONTTYPE, FONTSIZE);
	  }
	  if (my_fontdesc) {
	      pushbuttonview_SetFont(self, my_fontdesc);
	      my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
	  }
	  t_op = graphic_BETWEENLEFTANDRIGHT | graphic_ATBASELINE;
	  text = pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG;
	  tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;
	  if (my_FontSummary)
	    ty = TEXTPAD + (Rect2.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);
	  
	  pushbuttonview_setShade(self, 0.0);
	  pushbuttonview_SetTransferMode(self, graphic_BLACK);
	  pushbuttonview_MoveTo(self, tx, ty);
	  pushbuttonview_DrawString(self, text, t_op);
	  pushbuttonview_setShade(self, 1.0);
      } /* MOTIF? */
      
      break;
    }
  }
  self->lit = 1;
}


static void
UnhighlightButton(self)
struct pushbuttonview *self;
{
  struct pushbutton *b = (struct pushbutton *) pushbuttonview_GetDataObject(self);
  struct rectangle Rect, Rect2;
  int style;
  struct fontdesc *my_fontdesc;
  struct graphic *my_graphic;
  struct FontSummary *my_FontSummary;
  int tx, ty;
  short t_op;
  char *text;
  int bdepth, r2_bot, r_bot;
  double ulshade, lrshade;
  
  if (self->lit) {
    style = pushbutton_GetStyle(b);
    pushbuttonview_GetLogicalBounds(self, &Rect);
    
    switch (style) {
    case pushbutton_PLAIN:
    case pushbutton_PLAINBOX:
      pushbuttonview_SetTransferMode(self, graphic_INVERT);
      pushbuttonview_FillRect(self,&Rect,pushbuttonview_BlackPattern(self));
      break;

    case pushbutton_BOXEDRECT:
      /* Rect2 is the inner rect */
      Rect2.top = Rect.top + BUTTONDEPTH;
      Rect2.left = Rect.left + BUTTONDEPTH;
      Rect2.width = Rect.width - 2*BUTTONDEPTH;
      Rect2.height = Rect.height - 2*BUTTONDEPTH;

      pushbuttonview_SetTransferMode(self, graphic_INVERT);
      pushbuttonview_FillRect(self, &Rect,pushbuttonview_BlackPattern(self));
      pushbuttonview_FillRect(self, &Rect2,pushbuttonview_BlackPattern(self));

      break;
    case pushbutton_ROUNDRECT:
      /* Rect2 is the inner rect for DrawRRect */
      Rect2.top = Rect.top;
      Rect2.left = Rect.left;
      Rect2.height = BUTTONDEPTH;
      Rect2.width = BUTTONDEPTH;
      Rect.width -= 1;
      Rect.height -= BUTTONDEPTH;

      pushbuttonview_SetTransferMode(self, graphic_INVERT);
      pushbuttonview_FillRRect(self, &Rect, &Rect2, pushbuttonview_BlackPattern(self));

      break;

    case pushbutton_MOTIF:
    case pushbutton_THREEDEE:
      if (style == pushbutton_MOTIF) {
	  bdepth = MOTIFBUTTONDEPTH;
	  ulshade = MOTIFULSHADE;
	  lrshade = MOTIFLRSHADE;
      } else {
	  bdepth = BUTTONDEPTH;
	  ulshade = ULSHADE;
	  lrshade = LRSHADE;
      }


      /* Rect2 is the inner (Text) region */
      Rect2.top = Rect.top + bdepth;
      Rect2.left = Rect.left + bdepth;
      Rect2.width = Rect.width - 2*bdepth;
      Rect2.height = Rect.height - 2*bdepth;
      r2_bot = (Rect2.top)+(Rect2.height);
      r_bot = (Rect.top)+(Rect.height);

      if (style == pushbutton_MOTIF) {
          pushbuttonview_SetTransferMode(self, graphic_COPY);
          pushbuttonview_setShade(self, ulshade);
          pushbuttonview_FillRectSize(self, Rect.left, Rect.top, bdepth, Rect.height, NULL);	/* left bar */
	  
          pushbuttonview_setShade(self, lrshade);
          pushbuttonview_FillRectSize(self, Rect.left + Rect.width - bdepth, Rect.top, bdepth, Rect.height, NULL); /* right bar */
          pushbuttonview_FillTrapezoid(self, Rect2.left, r2_bot, Rect2.width, Rect.left, r_bot, Rect.width, NULL); /* lower trapz */
	  
          pushbuttonview_setShade(self, ulshade);
          pushbuttonview_FillTrapezoid(self, Rect.left, Rect.top, Rect.width, Rect2.left, Rect2.top, Rect2.width, NULL); /* upper trapz */
	  
          pushbuttonview_setShade(self, 1.0);
	  
      } else {
	  my_graphic = (struct graphic *)pushbuttonview_GetDrawable(self);
	  if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
	      my_fontdesc= fontdesc_Create(FONT, FONTTYPE, FONTSIZE);
	  }
	  if (my_fontdesc) {
	      pushbuttonview_SetFont(self, my_fontdesc);
	      my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
	  }
	  t_op = graphic_BETWEENLEFTANDRIGHT | graphic_ATBASELINE;
	  text = pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG;
	  tx = TEXTPAD + (Rect2.left + Rect2.width) / 2;
	  if (my_FontSummary)
	    ty = TEXTPAD + (Rect2.top + my_FontSummary->maxHeight - my_FontSummary->maxBelow);
	  
	  pushbuttonview_SetTransferMode(self, graphic_BLACK);
	  pushbuttonview_MoveTo(self, tx, ty);
	  pushbuttonview_DrawString(self, text, t_op);
      } /* MOTIF? */
      
      break;
    }
  }
  self->lit = 0;
}


struct view *
pushbuttonview__Hit(self, action, x, y, numclicks)
struct pushbuttonview *self;
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
  
  switch (action) {
  case view_LeftDown: 
    HighlightButton(self);
    pushbuttonview_WantInputFocus(self,self);
    break;
  case view_LeftMovement:
    {
      struct rectangle r;

      pushbuttonview_GetVisualBounds(self, &r);
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
	  pushbutton_PullTrigger((struct pushbutton *) pushbuttonview_GetDataObject(self), pushedtrigger);
	  pushbuttonview_PullTrigger(self, pushedtrigger);
	  im_SetProcessCursor(NULL);
	  cursor_Destroy(wait_cursor);
	}
      }
    }
    break;
  case view_RightDown:
    pushbuttonview_WantInputFocus(self, self);
    break;
  }

  return((struct view *)self);
}


void
pushbuttonview__ObservedChanged(self, b, v)
     struct pushbuttonview *self;
     struct pushbutton *b;
     long v;
{
    super_ObservedChanged(self, b, v);
    pushbuttonview_CacheSettings(self, b, 1);
}


enum view_DSattributes 
pushbuttonview__DesiredSize(self, width, height, pass, desired_width, desired_height)
struct pushbuttonview *self;
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
  struct pushbutton *b = (struct pushbutton *) pushbuttonview_GetDataObject(self);
  int style;

  style = pushbutton_GetStyle(b);

  my_graphic = (struct graphic *)pushbuttonview_GetDrawable(self);
  if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
    my_fontdesc= fontdesc_Create(FONT, FONTTYPE, FONTSIZE);
  }
  if (my_fontdesc) {
    fontdesc_StringSize(my_fontdesc, my_graphic, pushbutton_GetText(b) ? pushbutton_GetText(b) : NO_MSG, desired_width, desired_height);
    my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
  }

  switch (style) {
  case pushbutton_PLAIN:
  case pushbutton_PLAINBOX:
    if (my_FontSummary)
      *desired_height = my_FontSummary->maxHeight;
    break;
  case pushbutton_MOTIF:
    *desired_width = *desired_width + 2*TEXTPAD + 2*MOTIFBUTTONDEPTH;
    if (my_FontSummary)
      *desired_height = my_FontSummary->maxHeight + 2*TEXTPAD + 2*MOTIFBUTTONDEPTH;
    break;
  case pushbutton_BOXEDRECT:
  case pushbutton_ROUNDRECT:
  case pushbutton_THREEDEE:
    *desired_width = *desired_width + 2*TEXTPAD + 2*BUTTONDEPTH;
    if (my_FontSummary)
      *desired_height = my_FontSummary->maxHeight + 2*TEXTPAD + 2*BUTTONDEPTH;
    break;
  }

/*
  (BUG) I don't check to see if I can specify a size, I just do it.
  Will this break things?  What if I can't change my size?  Will I be
  Ugly?  What to do, what to do....
*/

  return(view_Fixed); /* (BUG) should disable user sizing, but this doesn't */
}


void
pushbuttonview__GetOrigin(self, width, height, originX, originY)
struct pushbuttonview *self;
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
  struct pushbutton *b = (struct pushbutton *) pushbuttonview_GetDataObject(self);
  int style;

  style = pushbutton_GetStyle(b);

  my_graphic = (struct graphic *)pushbuttonview_GetDrawable(self);
  if (!(my_fontdesc = pushbutton_GetButtonFont(b))) {
    my_fontdesc= fontdesc_Create(FONT, FONTTYPE, FONTSIZE);
  }
  if (my_fontdesc) {
    my_FontSummary =  fontdesc_FontSummary(my_fontdesc, my_graphic);
  }

  *originX = 0;
  switch (style) {
    case pushbutton_PLAIN:
    case pushbutton_PLAINBOX:
      if (my_FontSummary)
	*originY = (my_FontSummary->maxHeight) - (my_FontSummary->maxBelow) + 1;
      break;
    case pushbutton_MOTIF:
      if (my_FontSummary) 
	*originY = (my_FontSummary->maxHeight) - (my_FontSummary->maxBelow) + 1 + TEXTPAD + MOTIFBUTTONDEPTH;
      break;
    case pushbutton_BOXEDRECT:
    case pushbutton_ROUNDRECT:
    case pushbutton_THREEDEE:
      if (my_FontSummary) 
	*originY = (my_FontSummary->maxHeight) - (my_FontSummary->maxBelow) + 1 + TEXTPAD + BUTTONDEPTH;
      break;
  }
  return;
}


static void
LabelProc(self, param)
     struct pushbuttonview *self;
     long param;
{
    /*
      This is the routine which asks the user for a new text label.
      (BUG) Should not allow newlines (or should it?)
      */

    char buf[MAXPATHLEN];
    struct pushbutton *b = (struct pushbutton *)pushbuttonview_GetDataObject(self);
    char *oldtext;

    oldtext = pushbutton_GetText(b);
    if (message_AskForString(self,50,"Enter new text for button: ",
			     oldtext, buf, sizeof(buf)) >= 0) {
	pushbutton_SetText(b, buf);
	message_DisplayString(self, 10, "Changed button text.");
			     }
}


static void
FontProc(self, param)
struct pushbuttonview *self;
long param;
{
/*
  This is the routine which asks the user for a new font.
  It sucks, but I don't know how to smoothly integrate this button
  with a textview-like font change.  Oh well.
*/

  char buf[MAXPATHLEN], name[MAXPATHLEN];
  long style, size;
  struct pushbutton *b = (struct pushbutton *)pushbuttonview_GetDataObject(self);
  struct fontdesc *fd;

  if (message_AskForString(self,50,"Enter new fontname for pushbutton: ", PROMPTFONT, buf, sizeof(buf)) >= 0) {
      if (!fontdesc_ExplodeFontName(buf, name, sizeof(name), &style, &size)) {
	  message_DisplayString(self, 50, "Couldn't parse fontname.");
	  return;
      }
      if ((fd = fontdesc_Create(name,style,size))!=NULL) {
	  pushbutton_SetButtonFont(b, fd);
	  message_DisplayString(self, 10, "Changed font.");
      } else {
	  message_DisplayString(self, 50, "Font change failed.  Using old font.");
      }
  }
}


static void
StyleProc(self, param)
     struct pushbuttonview *self;
     long param;
{
    /*
      This is the routine which asks the user for a new pushbutton appearance.
      */

    struct pushbutton *b = (struct pushbutton *)pushbuttonview_GetDataObject(self);
    static char *style_menu[] = {
	"Plain Text",
	"Boxed Text",
	"Three Dimensional",
#ifndef NOROUNDRECT
	"Rounded Rect",
#endif
        "Simple Boxed Text",
        "OSF/Motif",
	NULL
    };
    int choice;

    if (message_MultipleChoiceQuestion(self,99,"Pick a new style:", pushbutton_GetStyle(b), &choice, style_menu, NULL)>= 0) {
#ifdef NOROUNDRECT
        if (choice >= pushbutton_ROUNDRECT) {
            choice++;
        }
#endif
	pushbutton_SetStyle(b, choice);
	message_DisplayString(self, 10, "Changed button style.");
    } else {
	message_DisplayString(self, 10, "Choice cancelled.");
    }
}


static void
ColorProc(self, param)
     struct pushbuttonview *self;
     long param;
{
    /*
      This is the routine which asks the user for a new pushbutton colors.
      */

    char buf[MAXPATHLEN], rgb_buf[10];
    struct pushbutton *b = (struct pushbutton *)pushbuttonview_GetDataObject(self);
    char *oldcolor;
    unsigned char rgb[3];

    oldcolor = pushbutton_GetFGColor(b, rgb);
    if (oldcolor == NULL) {
	sprintf(rgb_buf, "0x%02x%02x%02x", rgb[0], rgb[1], rgb[2]);
	oldcolor = rgb_buf;
    }
    if (message_AskForString(self,50,"Enter new foreground color for button: ", oldcolor, buf, sizeof(buf)) >= 0) {
	if ((strncmp(buf,"0x",2) != 0) 
	    && (strncmp(buf,"0X",2) != 0)) {
	    pushbutton_SetFGColor(b, buf, 0, 0, 0);
	    message_DisplayString(self, 10, "Changed foreground color.");
	} else {
	    pushbutton_ParseRGB(b, buf, rgb);
	    pushbutton_SetFGColor(b, NULL, rgb[0], rgb[1], rgb[2]);
	    sprintf(buf, "Changed button foreground color to 0x%x%x%x.", rgb[0], rgb[1], rgb[2]);
	    message_DisplayString(self, 10, buf);
	}
    }

    oldcolor = pushbutton_GetBGColor(b, rgb);
    if (oldcolor == NULL) {
	sprintf(rgb_buf, "0x%02x%02x%02x", rgb[0], rgb[1], rgb[2]);
	oldcolor = rgb_buf;
    }
    if (message_AskForString(self,50,"Enter new background color for button: ", oldcolor, buf, sizeof(buf)) >= 0) {
	if ((strncmp(buf,"0x",2) != 0) 
	    && (strncmp(buf,"0X",2) != 0)) {
	    pushbutton_SetBGColor(b, buf, 0, 0, 0);
	    message_DisplayString(self, 10, "Changed background color.");
	} else {
	    pushbutton_ParseRGB(b, buf, rgb);
	    pushbutton_SetBGColor(b, NULL, rgb[0], rgb[1], rgb[2]);
	    sprintf(buf, "Changed button background color to 0x%x%x%x.", rgb[0], rgb[1], rgb[2]);
	    message_DisplayString(self, 10, buf);
	}
    }
}


void pushbuttonview__WantUpdate(self, requestor)
     struct pushbuttonview *self;
     struct view *requestor;
{
    if ((struct view *) self == requestor) {
	if (self->awaitingUpdate) {
	    return;
	}
	self->awaitingUpdate = TRUE;
    }
    super_WantUpdate(self, requestor);
} /* pushbuttonview__WantUpdate */

static void OutputLabel(f, l)
FILE *f;
char *l;
{
    if(l==NULL) l=NO_MSG;
    while(*l) {
	if(l[0]=='\\') fprintf(f, "\\\\\\\\");
	else if(l[0]=='\"') fprintf(f, "\"\"");
	else fprintf(f, "%c", l[0]);
	l++;
    }
}

void pushbuttonview__Print(self, file, processor, format, topLevel)
register struct pushbuttonview  *self;
register FILE  *file;
char   *processor;
char   *format;
boolean   topLevel;
{
    int count;
    register struct pushbutton *dobj = (struct pushbutton *)self->header.view.dataobject;

    if (strcmp(processor, "troff") == 0) {
	/* output to troff */
	if (topLevel)
	    /* take care of initial troff stream */
	    texttroff_BeginDoc(file);

	fprintf(file, ".de bx\n\\(br\\|\\\\$1\\|\\(br\\l'|0\\(rn'\\l'|0\\(ul'\n..\n");
	fprintf(file, ".bx \"");
	OutputLabel(file, pushbutton_GetText(dobj));
	fprintf(file, "\"");
	fprintf(file, "\n");
	if (topLevel)
	    texttroff_EndDoc(file);

    } else {
	/* guess we're trying to write in postscript */
	struct fontdesc *fd = pushbutton_GetButtonFont(dobj);
	char *ffam = FONT;
	long fsiz = FONTSIZE;
	long fsty = FONTTYPE;
	char *mod1, *mod2, *psfam, *prefix;
	boolean adddash = FALSE, addroman = FALSE;

	if (strcmp(format, "troff") == 0)
	    prefix = "\\!  ";
	else prefix = "";

	if (fd) {
	    ffam = fontdesc_GetFontFamily(fd);
	    fsiz = fontdesc_GetFontSize(fd);
	    fsty = fontdesc_GetFontStyle(fd);
	}

	if (!strcmp(ffam, "andy")) {
	    psfam = "Times";
	    addroman = TRUE;
	}
	else if (!strcmp(ffam, "andytype"))
	    psfam = "Courier";
	else
	    psfam = "Helvetica";

	mod1 = "";
	mod2 = "";
	if (fsty & fontdesc_Bold) {
	    mod1 = "Bold";
	    adddash = TRUE;
	}
	if (fsty & fontdesc_Italic) {
	    if (addroman)
		mod2 = "Italic";
	    else
		mod2 = "Oblique";
	    adddash = TRUE;
	}
	if (fsty == fontdesc_Plain && addroman) {
	    mod1 = "Roman";
	    adddash = TRUE;
	}

	fprintf(file, "%s  /%s%c%s%s findfont %d scalefont setfont\n", prefix, psfam, (adddash ? '-' : ' '), mod1, mod2, fsiz);

	fprintf(file, "%s  0 0 moveto (", prefix);	
	OutputLabel(file, pushbutton_GetText(dobj));
	fprintf(file, ") show\n");
    }
}
