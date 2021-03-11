/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Chart View-object Abstract-class

MODULE	chartobj.ch

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support Chart parochial View-objects.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/23/89	Created (TCP)
  05/08/89	Use UnlinkNotification (TCP)
  05/10/89	Precompute data-points into Shadows (TCP)
  06/01/89	Add ChartBaseLine macro-method (TCP)
  09/06/89	Add CurrentItem method (TCP)

END-SPECIFICATION  ************************************************************/


#define  chartobj_VERSION		    1

#define  Data			    ((struct chart *)(self)->header.chartobj.data_object)

/***  Options  ***/

#define  chartobj_SuppressScales    (1<<0)
#define  chartobj_SuppressLabels    (1<<1)
#define  chartobj_SuppressGrids	    (1<<2)


struct chart_item_shadow
  {
  struct chart_item_shadow	    *next;
  struct chart_item		    *item;
  long				     datum;
  struct rectangle		     bounds;
  short				     x, y;
  };


class chartobj : aptv
  {
overrides:

  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  SetDataObject( struct char *);
  Hit( enum view_MouseAction action, long x, long y, long n )	returns struct view *;
  ObservedChanged( struct view *changed, long value );
  UnlinkNotification( struct chartobj *self );
  Print( FILE *file, char *processor, char *finalFormat, boolean topLevel );

methods:

  WhichItem( x, y )				returns struct chart_item_shadow *;
  SetChartOptions( options );
  SetDebug( boolean state );

  /***  The following Methods are to be Overriden by sub-classes  ***/
  DrawChart();
  PrintChart();
  HitChart( action, x, y, clicks )		returns struct view *;
  ObserveChart( change );
  Moniker()					returns char *;

macromethods:

  PixelsPerUnit()		(self->pixels_per_unit_value)
  PixelsPerInterval()		(self->pixels_per_value_interval)
  Items()			(self->item_shadows)
  ItemBounds( shadow )		(&(shadow)->bounds)
  ItemLeft( shadow )		((shadow)->bounds.left)
  ItemTop( shadow )		((shadow)->bounds.top)
  ItemWidth( shadow )		((shadow)->bounds.width)
  ItemHeight( shadow )		((shadow)->bounds.height)
  ItemCenter( shadow )		(ItemLeft(shadow)+ItemWidth(shadow)/2)
  ItemMiddle( shadow )		(ItemTop(shadow)+ItemHeight(shadow)/2)
  ItemRight( shadow )		(ItemLeft(shadow)+ItemWidth(shadow))
  ItemBottom( shadow )		(ItemTop(shadow)+ItemHeight(shadow))
  ItemX( shadow )		((shadow)->x)
  ItemY( shadow )		((shadow)->y)
  ItemDatum( shadow )		((shadow)->datum)
  NextItem( shadow )		((shadow)->next)
  ChartBounds()			(&self->chart_bounds)
  ChartLeft()			(self->chart_bounds.left)
  ChartTop()			(self->chart_bounds.top)
  ChartWidth()			(self->chart_bounds.width)
  ChartHeight()			(self->chart_bounds.height)
  ChartCenter()			(chartobj_ChartLeft(self) + chartobj_ChartWidth(self)/2)
  ChartRight()			(chartobj_ChartLeft(self) + chartobj_ChartWidth(self) - 1)
  ChartMiddle()			(chartobj_ChartTop(self)  + chartobj_ChartHeight(self)/2)
  ChartBottom()			(chartobj_ChartTop(self)  + chartobj_ChartHeight(self) - 1)
  ChartBaseLine()		(self->baseline)
  ChartBaseline()		(self->baseline)
  CurrentItem()			(self->current_item)

classprocedures:

  InitializeObject( struct chartobj *self ) returns boolean;
  FinalizeObject( struct chartobj *self );

data:

  struct chart			 *data_object;
  float				  pixels_per_unit_value,
				  pixels_per_value_interval;
  char				  scale_positions, label_positions;
  long				  scale_tick;
  struct rectangle		  chart_bounds,
				  left_scale, right_scale, top_scale, bottom_scale,
				  left_labels, right_labels, top_labels, bottom_labels;
  struct fontdesc		 *scale_font, *label_font, *graphic_font;
  char				 *scale_font_name, *label_font_name;
  struct graphic		 *dotted_graphic, *dashed_graphic;
  struct chart_item_shadow	 *item_shadows;
  struct chart_item		 *current_item;
  short				  baseline;
  boolean			  suppress_scales, suppress_labels, suppress_grids;
  };
