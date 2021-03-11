

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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/image/RCS/cmapv.c,v 1.4 1992/12/15 21:36:09 rr2b R6tape $";
#endif
#include <andrewos.h>
#include <view.ih>
#include <rect.h>
#include <message.ih>
#include <lpair.ih>
#include <suite.h>
#include <suite.ih>
#include <slider.ih>
#include <sliderv.ih>
#include <color.ih>
#include <colorv.ih>
#include <cmap.ih>
#include <cmapv.eh>

struct view *Color_Choice();
struct view *Control_Choice();

static suite_Specification cmap_entries[] = {
    suite_HitHandler( Color_Choice ),
    suite_ItemCaptionFontName( "andysans12b" ),
    suite_Arrangement( suite_Matrix | suite_Fixed ),
    suite_Rows( 16 ), suite_Columns( 16 ),
    suite_ItemHighlightStyle( suite_None ),
    NULL
};

#define NUMSLIDERS 3
#define redCode 0
#define greenCode 1
#define blueCode 2

static suite_Specification redSlider[] = {
    suite_ItemDatum(redCode),
    suite_ItemForegroundColor("red"),
    suite_ItemBackgroundColor("white"),
    suite_ItemTitleCaption("00000"),
    NULL
};

static suite_Specification greenSlider[] = {
    suite_ItemDatum(greenCode),
    suite_ItemForegroundColor("green"),
    suite_ItemBackgroundColor("white"),
    suite_ItemTitleCaption("00000"),
    NULL
};

static suite_Specification blueSlider[] = {
    suite_ItemDatum(blueCode),
    suite_ItemForegroundColor("blue"),
    suite_ItemBackgroundColor("white"),
    suite_ItemTitleCaption("00000"),
    NULL
};

static suite_Specification control_entries[] = {
    suite_HitHandler( Control_Choice ),
    suite_ItemCaptionFontName( "andysans12b" ),
    suite_Arrangement( suite_Row ),
    suite_ItemHighlightStyle( suite_None ),
    suite_Item(redSlider),
    suite_Item(greenSlider),
    suite_Item(blueSlider),
    NULL
};

struct sliderCBData {
    struct colormapv *cmapv;
    struct suite_item *item;
};

boolean
colormapv__InitializeClass( classID )
  struct classheader *classID;
{
    return(TRUE);
}


boolean
colormapv__InitializeObject( classID, self )
  struct classheader *classID;
  struct colormapv *self;
{
  self->map = suite_Create(cmap_entries, self);
  self->controlPanel = suite_Create(control_entries, self);
  self->top = lpair_New();
  lpair_VSplit(self->top, (struct view*) self->map, (struct view*) self->controlPanel, 30, TRUE);
  lpair_LinkTree(self->top, (struct view *) self);
  self->currentColor = NULL;
  return(TRUE);
}

void
colormapv__FinalizeObject( classID, self )
  struct classheader *classID;
  struct colormapv *self;
{
}

static long
SliderChanged( rock, inten )
    struct sliderCBData *rock;
    long inten;
{
    struct colormapv *self = rock->cmapv;
    register struct suite *s = self->controlPanel;
    register struct suite_item *item = rock->item;
    struct colormap *cmap = (struct colormap *) colormapv_GetDataObject(self);
    struct color *c;
    if(c = self->currentColor) {
	unsigned int R, G, B;
	char title[128];
	color_GetRGB(c, R, G, B);
	switch(suite_ItemAttribute(s, item, suite_ItemDatum(0))) {
	    case redCode:
		if(R != inten)
		    color_SetRGB(c, inten, G, B);
		break;
	    case greenCode:
		if(G != inten)
		    color_SetRGB(c, R, inten, B);
		break;
	    case blueCode:
		if(G != inten) 
		    color_SetRGB(c, R, G, inten);
		break;
	}
	colormap_ChangeColor(cmap, c);
	colormap_SetModified(cmap);
	sprintf(title, "%ld", inten);
	suite_ChangeItemAttribute(s, item, suite_ItemTitleCaption(title));
    }
}

void
colormapv__FullUpdate( self, type, left, top, width, height )
  struct colormapv *self;
  enum view_UpdateType type;
  long left, top, width, height;
{
  struct rectangle r;
  struct colormap *cmap = (struct colormap*) colormapv_GetDataObject(self);
  boolean firstTime = FALSE;

  colormapv_GetVisualBounds(self, &r);
  if(suite_ItemCount(self->map) == 0) {
      int size = colormap_Used(cmap);
      register int i, j;
      struct suite_item *item;

      firstTime = TRUE;
      for( i = 0; i < size; i++ ) {
	  struct color *c = colormap_NthColor(cmap, i);
	  struct colorv *cv = colorv_New();
	  colorv_SetDataObject(cv, c);
	  item = suite_CreateItem(self->map, NULL, i);
	  suite_ItemDataObject(self->map, item) = (struct dataobject *) c;
	  suite_ItemViewObject(self->map, item) = (struct view *) cv;
      }

      for( i = 0; i < NUMSLIDERS; i++ ) {
	  struct suite *s = self->controlPanel;
	  item = suite_ItemOfDatum(s, i);
	  if(item) {
	      struct slider *slider = slider_New();
	      struct sliderv *sliderv = sliderv_New();
	      struct sliderCBData *sliderCBRock;
	      suite_ItemDataObject(s, item) = (struct dataobject *) slider;
	      suite_ItemViewObject(s, item) = (struct view *) sliderv;
	      sliderv->curval = 0;
	      sliderv->range = 65535;
	      sliderv_SetDataObject(sliderv, slider);
	      sliderCBRock = (struct sliderCBData*) malloc(sizeof(struct sliderCBData));
	      sliderCBRock->cmapv = self;
	      sliderCBRock->item = item;
	      sliderv_SetCallback(sliderv, SliderChanged, sliderCBRock);
	  }
      }	
  }

  lpair_InsertView(self->top, self, &r);
  lpair_FullUpdate(self->top, type, left, top, width, height);

#if 0
  if(firstTime)
      suite_HighlightItem(self->map, suite_ItemAtPosition(self->map, 1));
#endif
}

void
colormapv__Update( self )
  struct colormapv *self;
{
  super_Update(self);
}

struct view *
Color_Choice( self, suite, item, type, action, x, y, clicks )
  struct colormapv *self;
  register struct suite *suite;
  register struct suite_item *item;
  enum view_UpdateType type;
  enum view_MouseAction action;
  long x, y, clicks;
{
  if(item && action == view_LeftDown) {
      struct suite *s = self->controlPanel;
      struct color *c;
      unsigned int R, G, B;
      register int i;

      c = (struct color *) suite_ItemDataObject(self->map, item);
      color_GetRGB(c, R, G, B);
      self->currentColor = c;
      for(i = 0; i < NUMSLIDERS; i++) {
	  struct suite_item *item = suite_ItemOfDatum(s, i);
	  struct sliderv *sliderv = (struct sliderv *) suite_ItemViewObject(s, item);
	  switch(i) {
	      case redCode:
		  sliderv_SetCurval(sliderv, R);
		  break;
	      case greenCode:
		  sliderv_SetCurval(sliderv, G);
		  break;
	      case blueCode:
		  sliderv_SetCurval(sliderv, B);
		  break;
	  }
      }
  }
  return(NULL);
}

struct view *
Control_Choice( self, suite, item, type, action, x, y, clicks )
  struct colormapv *self;
  register struct suite *suite;
  register struct suite_item *item;
  enum view_UpdateType type;
  enum view_MouseAction action;
  long x, y, clicks;
{
  if(item && action == view_LeftDown) {
      message_DisplayString(self, 0, suite_ItemAttribute(suite, item, suite_ItemCaption(0)));
  }
  return(NULL);
}

void
colormapv__PostMenus( self, menulist )
    struct colormapv *self;
    struct menulist *menulist;
{
  super_PostMenus(self, menulist);
}

struct view *
colormapv__Hit( self, action, x, y, numberOfClicks)
    struct colormapv *self;
    enum view_MouseAction action;
    long x;
    long y;
    long numberOfClicks;
{
    return((struct view *) lpair_Hit(self->top, action, x, y, numberOfClicks));
}
