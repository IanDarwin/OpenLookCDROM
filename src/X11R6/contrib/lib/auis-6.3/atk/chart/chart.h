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


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Chart View Internal Symbolics

MODULE	chart.h

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	Symbolics for Chart View & Palette

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  08/28/89	Created (TCP)

END-SPECIFICATION  ************************************************************/

#define  menu_default		  (1<<0)
#define  menu_application	  (1<<1)
#define  menu_applicationlayer	  (1<<2)
#define  menu_palette_hidden	  (1<<3)
#define  menu_palette_exposed	  (1<<4)

struct chartv_instance
  {
  struct chart		     *data_object;
  struct chartobj	     *data_viewer;
  struct suite	/*===*/	     *form_object, *sort_palette_object;
  struct suite		     *control_suite_object, *sort_position_suite_object,
			     *sort_value_suite_object, *sort_label_suite_object,
			     *type_suite_object, *title_suite_object;
  struct im		     *im_view_object;
  struct frame		     *frame_view_object;
  struct text		     *description_text;
  struct textview	     *description_textview;
  struct view		     *description_textview_scroll;
  struct lpair		     *pair_view;
  struct view		     *client_anchor;
  struct fontdesc	     *title_font, *item_font, *label_font, *scale_font;
  struct keystate	     *keystate;
  struct rectangle	      bounds, control_bounds, pair_bounds;
  struct menulist	     *menu;
  struct view		    *(*hit_handler)(), (*title_hit_handler)(),
			     (*title_data_object_handler)(),
			     (*title_view_object_handler)();
  struct chart_item	     *current_item;
  long			      client_datum, last_modified, description_last_modified;
  char			      arrangement, background_shade, chart_cursor_byte,
			     *title_font_name, *label_font_name, *scale_font_name,
  			     *chart_cursor_font_name, *item_font_name,
			      item_border_style, item_border_size, item_highlight_style,
			      border_style, border_size,
			      title_border_style, title_border_size,
			      title_highlight_style, title_placement, title_alignment,
			      visual_orientation;
  boolean		      initialized, form_initialized,
			      expose_form, form_exposed,
			      input_focus, description_exposed,
			      read_only, ignore_fullupdate, ignore_loseinputfocus,
			      application, application_layer;
  };

#define  DeletePhrase		"Delete"
#define  SavePhrase		"Save"
#define  AscendPhrase		"Ascend"
#define  DescendPhrase		"Descend"
#define  PrintPhrase		"Print"
#define  TopPhrase		"Top"
#define  BottomPhrase		"Bottom"
#define  LeftPhrase		"Left"
#define  RightPhrase		"Right"
#define  TitlesPhrase		"Titles"

#define  TitleFontNamePhrase	"andysans12b"
#define  ItemCaptionFontNamePhrase "andysans10b"

#define  Menu			  (self->instance->menu)

#define  Keystate		  (self->instance->keystate)
#define  Bounds			  (&self->instance->bounds)
#define  Left			  (self->instance->bounds.left)
#define  Top			  (self->instance->bounds.top)
#define  Width			  (self->instance->bounds.width)
#define  Height			  (self->instance->bounds.height)

#define  PairBounds		  (&self->instance->pair_bounds)
#define  ControlBounds		  (&self->instance->control_bounds)

#define  Chart			  (self->instance->data_object)
#define  ChartViewer		  (self->instance->data_viewer)
#define  PairView		  (self->instance->pair_view)

#define  Arrangement		  (self->instance->arrangement)
#define  BorderStyle		  (self->instance->border_style)
#define  BorderSize		  (self->instance->border_size)
#define  TitleBorderStyle	  (self->instance->title_border_style)
#define  TitleBorderSize	  (self->instance->title_border_size)
#define  TitleFontName		  (self->instance->title_font_name)
#define  TitleHighlightStyle	  (self->instance->title_highlight_style)
#define  TitlePlacement		  (self->instance->title_placement)
#define  TitleAlignment		  (self->instance->title_alignment)
#define  TitleHitHandler	  (self->instance->title_hit_handler)
#define  TitleDataObjectHandler	  (self->instance->title_data_object_handler)
#define  TitleViewObjectHandler	  (self->instance->title_view_object_handler)
#define  ClientAnchor		  (self->instance->client_anchor)
#define  ClientDatum		  (self->instance->client_datum)
#define  HitHandler		  (self->instance->hit_handler)
#define  ItemBorderStyle	  (self->instance->item_border_style)
#define  ItemBorderSize		  (self->instance->item_border_size)
#define  ItemHighlightStyle	  (self->instance->item_highlight_style)
#define  ItemFontName		  (self->instance->item_font_name)
#define  ItemFont		  (self->instance->item_font)
#define  CurrentItem		  (self->instance->current_item)
#define  LabelFontName		  (self->instance->label_font_name)
#define  ScaleFontName		  (self->instance->scale_font_name)

#define  ChartCursor		  (self->instance->chart_cursor)
#define  ChartCursorByte	  (self->instance->chart_cursor_byte)
#define  ChartCursorFont	  (self->instance->chart_cursor_font)
#define  ChartCursorFontName	  (self->instance->chart_cursor_font_name)

#define  VisualOrientation	(self->instance->visual_orientation)
#define  VisualOrientationBU	(0)
#define  VisualOrientationTD	(1)
#define  VisualOrientationLR	(2)
#define  VisualOrientationRL	(3)

#define  BackgroundShade	  (self->instance->background_shade)
#define  BackgroundPattern	  (self->instance->background_pattern)
#define  BackgroundWhite	  (self->instance->background_shade == 1)
#define  BackgroundNonWhite	  (self->instance->background_shade != 1)

#define  Initialized		  (self->instance->initialized)
#define  PaletteInitialized	  (self->instance->form_initialized)
#define  IgnoreFullUpdate	  (self->instance->ignore_fullupdate)
#define  IgnoreLoseInputFocus	  (self->instance->ignore_loseinputfocus)

#define  InputFocus		  (self->instance->input_focus)
#define  ExposePalette		  (self->instance->expose_form)
#define  PaletteExposed		  (self->instance->form_exposed)
#define  ControlSuite		  (self->instance->control_suite_object)
#define  SortValueSuite		  (self->instance->sort_value_suite_object)
#define  SortLabelSuite		  (self->instance->sort_label_suite_object)
#define  SortPositionSuite	  (self->instance->sort_position_suite_object)
#define  TypeSuite		  (self->instance->type_suite_object)
#define  TitleSuite		  (self->instance->title_suite_object)
#define  Palette		  (self->instance->form_object)/*===*/
#define  SortForm		  (self->instance->sort_palette_object)/*===*/
#define  PaletteIm		  (self->instance->im_view_object)
#define  PaletteFrame		  (self->instance->frame_view_object)
#define  Application		  (self->instance->application)
#define  ApplicationLayer	  (self->instance->application_layer)
#define  Description		  (self->instance->description_text)
#define  DescriptionView	  (self->instance->description_textview)
#define  DescriptionViewScroll	  (self->instance->description_textview_scroll)
#define  DescriptionExposed	  (self->instance->description_exposed)

#define  ReadOnly		  (self->instance->read_only)
#define  LastModified		  (self->instance->last_modified)
#define  DescriptionLastModified  (self->instance->description_last_modified)
