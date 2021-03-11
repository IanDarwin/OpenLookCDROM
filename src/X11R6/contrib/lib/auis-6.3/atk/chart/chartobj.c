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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartobj.c,v 1.20 1993/11/18 02:36:04 gk5g Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Chart View-object Abstract-class

MODULE	chartobj.c

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
  05/08/89	Add UnlinklNotification (RetractViewCursors) (TCP)
  05/10/89	Precompute data-points into Shadows (TCP)
  06/01/89	Improve Scale-tick numbers (TCP)
		Handle negative values
		Provide generic Hit method
  06/02/89	Improve Scale-tick printing (TCP)
  06/08/89	Honor suppression of Labels & Scales in printing (TCP)
  06/26/89	Correct BaseLine setting (TCP)
  09/05/89	Upgrade to V1.0 (TCP)

END-SPECIFICATION  ************************************************************/

#include <math.h>
#include "graphic.ih"
#include "observe.ih"
#include "view.ih"
#include "fontdesc.ih"
#include "im.ih"
#include "rect.h"
#include "apt.h"
#include "aptv.ih"
#include "chart.ih"
#include "chartv.ih"
#include "chartobj.eh"
#include <ctype.h>

int chartobj_debug = 0;

#define debug chartobj_debug

#define  Chart			(self->data_object)

#define  Shadows		(self->item_shadows)
#define  ShadowX(s)		(s->x)
#define  ShadowY(s)		(s->y)
#define  ShadowItem(s)		(s->item)
#define  ShadowDatum(s)		(s->datum)
#define  ShadowBounds(s)	(&s->bounds)
#define  ShadowLeft(s)		(s->bounds.left)
#define  ShadowTop(s)		(s->bounds.top)
#define  ShadowWidth(s)		(s->bounds.width)
#define  ShadowHeight(s)	(s->bounds.height)
#define  ShadowCenter(s)	(ShadowLeft(s)+ShadowWidth(s)/2)
#define  ShadowMiddle(s)	(ShadowTop(s)+ShadowHeight(s)/2)
#define  NextItem(s)		(s->next)
#define  CurrentItem		(self->current_item)

#define  Bounds			(chartobj_BodyBounds(self))
#define  Left			(chartobj_BodyLeft(self))
#define  Top			(chartobj_BodyTop(self))
#define  Width			(chartobj_BodyWidth(self))
#define  Height			(chartobj_BodyHeight(self))
#define  Right			(chartobj_BodyRight(self))
#define  Center			(chartobj_BodyCenter(self))
#define  Middle			(chartobj_BodyMiddle(self))
#define  Bottom			(chartobj_BodyBottom(self))

#define  ChartBounds		(&self->chart_bounds)
#define  ChartLeft		(self->chart_bounds.left)
#define  ChartTop		(self->chart_bounds.top)
#define  ChartWidth		(self->chart_bounds.width)
#define  ChartHeight		(self->chart_bounds.height)
#define  ChartRight		(self->chart_bounds.left+self->chart_bounds.width-1)
#define  ChartBottom		(self->chart_bounds.top+self->chart_bounds.height-1)
#define  ChartBaseLine		(self->baseline)

#define  PixelsPerUnit		(chartobj_PixelsPerUnit(self))
#define  PixelsPerInterval	(chartobj_PixelsPerInterval(self))

#define  LeftScaleLeft		(self->left_scale.left)
#define  LeftScaleTop		(self->left_scale.top)
#define  LeftScaleWidth		(self->left_scale.width)
#define  LeftScaleHeight	(self->left_scale.height)
#define  LeftScaleRight		(LeftScaleLeft+LeftScaleWidth)
#define  LeftScaleBottom	(LeftScaleTop+LeftScaleHeight-1)
#define  LeftScaleBarX		(LeftScaleRight-10)
#define  LeftScaleBarY		(LeftScaleTop-10)

#define  RightScaleLeft		(self->right_scale.left)
#define  RightScaleTop		(self->right_scale.top)
#define  RightScaleWidth	(self->right_scale.width)
#define  RightScaleHeight	(self->right_scale.height)
#define  RightScaleRight	(RightScaleLeft+RightScaleWidth)
#define  RightScaleBottom	(RightScaleTop+RightScaleHeight-1)
#define  RightScaleBarX		(RightScaleRight-10)
#define  RightScaleBarY		(RightScaleTop-10)

#define  TopScaleLeft		(self->top_scale.left)
#define  TopScaleTop		(self->top_scale.top)
#define  TopScaleWidth		(self->top_scale.width)
#define  TopScaleHeight		(self->top_scale.height)
#define  TopScaleRight		(TopScaleLeft+TopScaleWidth)
#define  TopScaleBottom		(TopScaleTop+TopScaleHeight-1)
#define  TopScaleBarX		(TopScaleRight-10)
#define  TopScaleBarY		(TopScaleTop-10)

#define  BottomScaleLeft	(self->bottom_scale.left)
#define  BottomScaleTop		(self->bottom_scale.top)
#define  BottomScaleWidth	(self->bottom_scale.width)
#define  BottomScaleHeight	(self->bottom_scale.height)
#define  BottomScaleRight	(BottomScaleLeft+BottomScaleWidth)
#define  BottomScaleBottom	(BottomScaleTop+BottomScaleHeight-1)
#define  BottomScaleBarX	(BottomScaleRight-10)
#define  BottomScaleBarY	(BottomScaleTop-10)

#define  ScalePositions		(self->scale_positions)
#define  ScaleFont		(self->scale_font)
#define  ScaleFontName		(self->scale_font_name)
#define  ScaleTick		(self->scale_tick)

#define  LeftLabelsLeft		(self->left_labels.left)
#define  LeftLabelsTop		(self->left_labels.top)
#define  LeftLabelsWidth	(self->left_labels.width)
#define  LeftLabelsHeight	(self->left_labels.height)
#define  LeftLabelsRight	(LeftLabelsLeft+LeftLabelsWidth)
#define  LeftLabelsBottom	(LeftLabelsTop+LeftLabelsHeight-1)
#define  LeftLabelsCenter	(LeftLabelsLeft+LeftLabelsWidth/2)
#define  LeftLabelsMiddle	(LeftLabelsTop+LeftLabelsHeight/2)

#define  RightLabelsLeft	(self->right_labels.left)
#define  RightLabelsTop		(self->right_labels.top)
#define  RightLabelsWidth	(self->right_labels.width)
#define  RightLabelsHeight	(self->right_labels.height)
#define  RightLabelsRight	(RightLabelsLeft+RightLabelsWidth)
#define  RightLabelsBottom	(RightLabelsTop+RightLabelsHeight-1)
#define  RightLabelsCenter	(RightLabelsLeft+RightLabelsWidth/2)
#define  RightLabelsMiddle	(RightLabelsTop+RightLabelsHeight/2)

#define  TopLabelsLeft		(self->top_labels.left)
#define  TopLabelsTop		(self->top_labels.top)
#define  TopLabelsWidth		(self->top_labels.width)
#define  TopLabelsHeight	(self->top_labels.height)
#define  TopLabelsRight		(TopLabelsLeft+TopLabelsWidth)
#define  TopLabelsBottom	(TopLabelsTop+TopLabelsHeight-1)
#define  TopLabelsCenter	(TopLabelsLeft+TopLabelsWidth/2)
#define  TopLabelsMiddle	(TopLabelsTop+TopLabelsHeight/2)

#define  BottomLabelsLeft	(self->bottom_labels.left)
#define  BottomLabelsTop	(self->bottom_labels.top)
#define  BottomLabelsWidth	(self->bottom_labels.width)
#define  BottomLabelsHeight	(self->bottom_labels.height)
#define  BottomLabelsRight	(BottomLabelsLeft+BottomLabelsWidth)
#define  BottomLabelsBottom	(BottomLabelsTop+BottomLabelsHeight-1)
#define  BottomLabelsCenter	(BottomLabelsLeft+BottomLabelsWidth/2)
#define  BottomLabelsMiddle	(BottomLabelsTop+BottomLabelsHeight/2)

#define  LabelFont		(self->label_font)
#define  LabelFontName		(self->label_font_name)
#define  LabelPositions		(self->label_positions)
#define  GraphicFont		(self->graphic_font)
#define  DottedGraphic		(self->dotted_graphic)
#define  DottedIcon		('5')
#define  DashedGraphic		(self->dashed_graphic)
#define  DashedIcon		('5')

#define  RightTop		(view_ATRIGHT | view_ATTOP)
#define  RightBottom		(view_ATRIGHT | view_ATBOTTOM)
#define  RightMiddle		(view_ATRIGHT | view_BETWEENTOPANDBOTTOM)
#define  Balanced	        (view_BETWEENLEFTANDRIGHT | view_BETWEENTOPANDBOTTOM)

#define  ScalesSuppressed	(self->suppress_scales)
#define  LabelsSuppressed	(self->suppress_labels)
#define  GridsSuppressed	(self->suppress_grids)

#define  abs(x)			(((x)<0) ? -(x) : (x))

boolean 
chartobj__InitializeObject( classID, self)
  register struct classheader	      *classID;
  register struct chartobj	      *self;
  {
  IN(chartobj_InitializeObject);
  DEBUGst(RCSID,rcsid);
  chartobj_SetShrinkIcon( self, 'e', "icon12", "Chart", "andysans10b" );
  chartobj_SetHelpFileName( self, "/usr/andy/help/chart.help"/*=== ===*/ );
  chartobj_SetOptions( self, aptv_SuppressControl );
  Shadows = NULL;
  Chart = NULL;
  ScalePositions = LabelPositions = NULL;
  ScalesSuppressed = LabelsSuppressed = GridsSuppressed = false;
  PixelsPerUnit = PixelsPerInterval = 0;
  ScaleTick = 1;
/*===*/ScalePositions = chartv_Left;
/*===*/LabelPositions = chartv_Bottom;
  ScaleFontName = "andysans8";
  ScaleFont = chartobj_BuildFont( self, ScaleFontName, NULL );
  LabelFontName = "andysans8";
  LabelFont = chartobj_BuildFont( self, LabelFontName, NULL );
  GraphicFont = chartobj_BuildFont( self, "shape10", NULL );
  DottedGraphic = DashedGraphic = NULL;
  CurrentItem = NULL;
  OUT(chartobj_InitializeObject);
  return  TRUE;
  }

void 
chartobj__FinalizeObject( classID, self )
  register struct classheader	 *classID;
  register struct chartobj	 *self;
  {
  IN(chartobj_FinalizeObject);
  Free_Shadows( self );
/*===*/
  OUT(chartobj_FinalizeObject);
  }

void 
chartobj__UnlinkNotification( self, linkee )
  register struct chartobj	 *self;
  register struct view		 *linkee;
  {
  IN(chartobj_UnlinkNotification);
  if ( linkee == (struct view *)self )
    chartobj_RetractViewCursors( self, self );
/*===*/
  OUT(chartobj_UnlinkNotification);
  }

void
chartobj__ObservedChanged( self, changed, value )
  register struct chartobj	     *self;
  register struct observable	     *changed;
  register long			      value;
  {
  IN(chartobj_ObservedChanged);
  switch ( value )
    {
    case chart_ItemValueChanged:
/*===*/
      break;
    case chart_ItemsSorted:
/*===*/
      break;
    }
  chartobj_ClearBody( self );
  chartobj_FullUpdate( self, view_FullRedraw, Left, Top, Width, Height );
  OUT(chartobj_ObservedChanged);
  }

void
chartobj__ObserveChart( self, change )
  register struct chartobj	   *self;
  register long			    change;
  {
  IN(chartobj_ObserveChart);
  chartobj_SetFont( self, chartobj_BuildFont( self, "andysans10b", NULL ) );
  chartobj_MoveTo( self, Center, Middle );
  chartobj_DrawString( self, "MISSING ObserveChart METHOD", Balanced );
  OUT(chartobj_ObserveChart);
  }

void
chartobj__SetDebug( self, state )
  register struct chartobj  *self;
  register char		     state;
  {
  IN(chartobj_SetDebug);
  debug = state;
  OUT(chartobj_SetDebug);
  }

void
chartobj__SetDataObject( self, data )
  register struct chartobj   *self;
  register struct chart	     *data;
  {
  IN(chartobj_SetDataObject);
  super_SetDataObject( self, Chart = data );
  DEBUGst(ChartType,chart_Type( data ));
  OUT(chartobj_SetDataObject);
  }

void
chartobj__SetChartOptions( self, options )
  register struct chartobj   *self;
  register long		      options;
  {
  IN(chartobj_SetChartOptions);

  if ( options & chartobj_SuppressScales )
    ScalesSuppressed = true;
  if ( options & chartobj_SuppressLabels )
    LabelsSuppressed = true;
  OUT(chartobj_SetChartOptions);
  }

char *
chartobj__Moniker( self )
  register struct chartobj   *self;
  {
  IN(chartobj_Moniker);
  OUT(chartobj_Moniker);
  return  "UNKNOWN";
  }

void 
chartobj__FullUpdate( self, type, left, top, width, height )
  register struct chartobj	 *self;
  register enum view_UpdateType	  type;
  register long			  left, top, width, height;
  {
  char				  value_string[25];
  long				  W, H;

  IN(chartobj_FullUpdate);
  super_FullUpdate( self, type, left, top, width, height );
  if ( ! chartobj_BypassUpdate(self)  &&  Height > 0  &&
	(type == view_FullRedraw || type == view_LastPartialRedraw) )
    { DEBUG(Not Bypassed);
    ChartLeft = Left;  ChartTop = Top;  ChartWidth = Width;  ChartHeight = Height;
    Generate_Shadows( self );
    LeftScaleLeft    = LeftScaleTop    = LeftScaleWidth    = LeftScaleHeight    = 0;
    RightScaleLeft   = RightScaleTop   = RightScaleWidth   = RightScaleHeight   = 0;
    TopScaleLeft     = TopScaleTop     = TopScaleWidth     = TopScaleHeight     = 0;
    BottomScaleLeft  = BottomScaleTop  = BottomScaleWidth  = BottomScaleHeight  = 0;
    LeftLabelsLeft   = LeftLabelsTop   = LeftLabelsWidth   = LeftLabelsHeight   = 0;
    RightLabelsLeft  = RightLabelsTop  = RightLabelsWidth  = RightLabelsHeight  = 0;
    TopLabelsLeft    = TopLabelsTop    = TopLabelsWidth    = TopLabelsHeight    = 0;
    BottomLabelsLeft = BottomLabelsTop = BottomLabelsWidth = BottomLabelsHeight = 0;
    if ( !ScalesSuppressed )
      { DEBUG(Prepare for Scales);
      DottedGraphic = fontdesc_CvtCharToGraphic( GraphicFont,
			    chartobj_GetDrawable( self ), DottedIcon );
      sprintf( value_string, "%d", chart_ItemValueGreatest( Chart) );
      fontdesc_StringSize( ScaleFont, chartobj_GetDrawable( self ),
			   value_string, &W, &H );
      if ( ScalePositions & chartv_Left )
	{
	LeftScaleLeft    = ChartLeft;
	LeftScaleTop     = ChartTop;
	LeftScaleWidth   = W + 20;
	LeftScaleHeight  = ChartHeight;
	ChartLeft	+= LeftScaleWidth;
	ChartWidth	-= LeftScaleWidth;
	}
      if ( ScalePositions & chartv_Right )
	{
	RightScaleTop     = ChartTop;
	RightScaleWidth   = W + 20;
	RightScaleLeft    = ChartRight;
	RightScaleHeight  = ChartHeight;
	ChartWidth	 -= RightScaleWidth;
	}
      if ( ScalePositions & chartv_Top )
	{
	TopScaleLeft      = ChartLeft;
	TopScaleTop       = ChartTop;
	TopScaleWidth     = ChartWidth;
	TopScaleHeight    = H + 20;
	ChartHeight	 -= TopScaleHeight;
	LeftScaleTop     += TopScaleHeight;
	RightScaleTop    += TopScaleHeight;
	LeftScaleHeight  -= TopScaleHeight;
	RightScaleHeight -= TopScaleHeight;
	}
      if ( ScalePositions & chartv_Bottom )
	{
	BottomScaleLeft   = ChartLeft;
	BottomScaleWidth  = ChartWidth;
	BottomScaleHeight = H + 20;
	BottomScaleTop    = ChartBottom - BottomScaleHeight;
	ChartHeight	 -= BottomScaleHeight;
	LeftScaleHeight  -= TopScaleHeight;
	RightScaleHeight -= TopScaleHeight;
	}
      }
    if ( !LabelsSuppressed )
      { DEBUG(Prepare for Labels);
      fontdesc_StringSize( LabelFont, chartobj_GetDrawable( self ),
			   "M", &W, &H );
      if ( LabelPositions & chartv_Left )
	{
	LeftLabelsLeft    = ChartLeft;
	LeftLabelsTop     = ChartTop;
	LeftLabelsWidth   = W + 20;
	LeftLabelsHeight  = ChartHeight;
	ChartLeft	 += LeftLabelsWidth;
	ChartWidth	 -= LeftLabelsWidth;
	}
      if ( LabelPositions & chartv_Right )
	{
	RightLabelsTop     = ChartTop;
	RightLabelsWidth   = W + 20;
	RightLabelsLeft    = ChartRight;
	RightLabelsHeight  = ChartHeight;
	ChartWidth	  -= RightLabelsWidth;
	}
      if ( LabelPositions & chartv_Top )
	{
	TopLabelsLeft      = ChartLeft;
	TopLabelsTop       = ChartTop;
	TopLabelsWidth     = ChartWidth;
	TopLabelsHeight    = H + 20;
	ChartHeight	  -= TopLabelsHeight;
	LeftLabelsTop     += TopLabelsHeight;
	RightLabelsTop    += TopLabelsHeight;
	LeftLabelsHeight  -= TopLabelsHeight;
	RightLabelsHeight -= TopLabelsHeight;
	}
      if ( LabelPositions & chartv_Bottom )
	{
	BottomLabelsLeft   = ChartLeft;
	BottomLabelsWidth  = ChartWidth;
	BottomLabelsHeight = H + 20;
	BottomLabelsTop    = ChartBottom - BottomLabelsHeight;
	ChartHeight	  -= BottomLabelsHeight;
	LeftLabelsHeight  -= TopLabelsHeight;
	RightLabelsHeight -= TopLabelsHeight;
	}
      }
    DEBUGdt(ItemValueSpan,chart_ItemValueSpan(Chart));
    PixelsPerUnit = (1.0*ChartHeight) /
	(1.0*((chart_ItemValueSpan( Chart ) > 0) ?
	     chart_ItemValueSpan( Chart ) : 1));
    DEBUGgt(PixelsPerUnit,PixelsPerUnit);
    PixelsPerInterval = PixelsPerUnit * chart_ItemValueRangeInterval( Chart );
    DEBUGgt(PixelsPerInterval,PixelsPerInterval);
    ChartBaseLine = ChartTop + chart_ItemValueGreatest( Chart )*PixelsPerInterval;
    if ( !ScalesSuppressed )
      {
      LeftScaleHeight  -= BottomLabelsHeight + 1;
      RightScaleHeight -= BottomLabelsHeight + 1;
      Draw_Scales( self );
      }
    if ( !LabelsSuppressed )
      {
      Draw_Labels( self );
      }
    Set_Shadows( self );
    chartobj_DrawChart( self );
    }
  OUT(chartobj_FullUpdate);
  }

void
chartobj__DrawChart( self )
  register struct chartobj	   *self;
  {
  IN(chartobj_DrawChart);
  chartobj_SetFont( self, chartobj_BuildFont( self, "andysans10b", NULL ) );
  chartobj_MoveTo( self, Center, Middle );
  chartobj_DrawString( self, "MISSING DrawChart METHOD", Balanced );
  OUT(chartobj_DrawChart);
  }

struct view *
chartobj__Hit( self, action, x, y, clicks )
  register struct chartobj	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register struct view		     *hit;

  IN(chartobj_Hit );
  if ( (hit = super_Hit( self, action, x, y, clicks )) == NULL )
    hit = (struct view *) chartobj_HitChart( self, action, x, y, clicks );
  OUT(chartobj_Hit );
  return  hit;
  }

struct view *
chartobj__HitChart( self, action, x, y, clicks )
  register struct chartobj	   *self;
  register enum view_MouseAction    action;
  register long			    x, y, clicks;
  {
  static struct chart_item_shadow  *shadow;
  register long			    delta;
  static long			    value, value_original, initial_y, prior_y;
  static char			    value_string[25], *name;

  IN(chartobj_HitChart);
  if ( shadow  ||
	 (action == view_LeftDown  &&  (shadow = chartobj_WhichItem( self, x, y ))) )
    {
    CurrentItem = shadow->item;
    if ( y > Bottom )	y = Bottom;
    if ( y < Top )	y = Top;
    switch ( action )
      {
      case  view_LeftDown:
	chartobj_UseInvisibleCursor( self );
        chartobj_SetTransferMode( self, graphic_INVERT );
	initial_y = y;
	y = ShadowMiddle(shadow);
	chartobj_MoveTo( self, Left, prior_y = y );
	chartobj_DrawLineTo( self, Right, y );
	name = (char *) chart_ItemAttribute( Chart, shadow->item, chart_ItemName(0) );
	value = value_original = chart_ItemAttribute( Chart, shadow->item, chart_ItemValue(0) );
	DEBUGdt(Initial-value,value);
        break;
      case  view_LeftMovement:
	chartobj_MoveTo( self, Left, prior_y );
	chartobj_DrawLineTo( self, Right, prior_y );
	if ( abs(delta = prior_y - y) > PixelsPerInterval )
	  value += (delta / PixelsPerInterval) * chart_ItemValueRangeInterval( Chart );
	chartobj_MoveTo( self, Left, prior_y = y );
	chartobj_DrawLineTo( self, Right, y );
        break;
      case  view_LeftUp:
	chartobj_MoveTo( self, Left, prior_y );
	chartobj_DrawLineTo( self, Right, prior_y );
	if ( abs(delta = initial_y - y) > PixelsPerInterval )
	  {
	  value_original += (delta / PixelsPerInterval) * chart_ItemValueRangeInterval( Chart );
	  DEBUGdt(Final-value,value);
	  chart_SetItemAttribute( Chart, shadow->item,
	    chart_ItemValue( (value = value_original) ) );
	  chart_SetModified( Chart );
	  chart_NotifyObservers( Chart, chart_ItemValueChanged );
	  }
	shadow = NULL;
	chartobj_UseNormalCursor( self );
        break;
      }
    sprintf( value_string, "%s:  Value = %d", name, value );
    chartobj_Announce( self, value_string );
    }
  OUT(chartobj_HitChart);
  return  (struct view *) self;
  }

static
Generate_Shadows( self )
  register struct chartobj	   *self;
  {
  register struct chart_item_shadow   *shadow = NULL, *prior = NULL;
  register struct chart_item	   *chart_item = chart_ItemAnchor( Chart );

  IN(Generate_Shadows);
  if ( Shadows )
    Free_Shadows( self );
  while ( chart_item )
    {
    shadow = (struct chart_item_shadow *) calloc( 1, sizeof(struct chart_item_shadow) );
    if ( prior )
      prior->next = shadow;
      else
      Shadows = shadow;
    prior = shadow;
    ShadowItem(shadow) = chart_item;
    chart_item = chart_NextItem( Chart, chart_item );
    }
  DEBUGdt(ItemValueRangeLow,chart_ItemValueRangeLow(Chart));
  DEBUGdt(ItemValueRangeInterval,chart_ItemValueRangeInterval(Chart));
  DEBUGdt(ItemValueRangeHigh,chart_ItemValueRangeHigh(Chart));
  DEBUGdt(ItemValueSpan,chart_ItemValueSpan(Chart));
  OUT(Generate_Shadows);
  }

static
Set_Shadows( self )
  register struct chartobj	     *self;
  {
  register long			      i, x, y, width, high_adjust = 0, low_adjust = 0,
				      count = chart_ItemCount( Chart ),
				      excess, fudge;
  register struct chart_item_shadow  *shadow = Shadows;

  IN(Set_Shadows);
  width = (ChartWidth / ((count) ? count : 1));
  x = ChartLeft + width/2;
  if ( excess = (ChartWidth - (width * count)) / 2 )
    excess = count / excess;
  DEBUGdt(width,width);
  if (  (ScaleTick > 0) && (chart_ItemValueGreatest( Chart ) % ScaleTick ) )
    high_adjust = (ScaleTick - (chart_ItemValueGreatest( Chart ) % ScaleTick)) * PixelsPerUnit;
  DEBUGdt(HighAdjust,high_adjust);
  if ( chart_ItemValueLeast( Chart) < 0 )
    {
    low_adjust = abs(chart_ItemValueLeast( Chart ) * PixelsPerUnit);
    ChartBaseLine = ChartBottom - low_adjust;
    if (  (ScaleTick > 0) && (chart_ItemValueLeast( Chart ) % ScaleTick ) )
      low_adjust += (ScaleTick + (chart_ItemValueLeast( Chart ) % ScaleTick)) * PixelsPerUnit;
    }
  DEBUGdt(LowAdjust,low_adjust);
  DEBUGdt(BaseLine,ChartBaseLine);  DEBUGdt(Bottom,ChartBottom);
  for ( i = 0; i < count  &&  shadow; i++ )
    {
    DEBUGdt(Value,chart_ItemAttribute( Chart, ShadowItem(shadow), chart_ItemValue(0) ));
    if ( (y = (ChartBottom -
	(chart_ItemAttribute( Chart, ShadowItem(shadow), chart_ItemValue(0) ) *
	PixelsPerUnit)) + high_adjust - low_adjust) > ChartBottom - 2 )
      y = ChartBottom - 2;
    ShadowY(shadow) = y;
    ShadowTop(shadow) = y - 5;
    fudge = (excess) ? ((i % excess) ? 0 : 1) : 0;
    ShadowX(shadow) = x + fudge;
    ShadowLeft(shadow) = x + fudge - 5;
    x += width + fudge;
    ShadowWidth(shadow) = ShadowHeight(shadow) = 10;
    shadow = NextItem(shadow);
    }
  OUT(Set_Shadows);
  }

static
Free_Shadows( self )
  register struct chartobj	     *self;
  {
  register struct chart_item_shadow  *shadow = Shadows, *next;

  while ( shadow )
    {
    next = NextItem(shadow);
    free( shadow );
    shadow = next;
    }
  Shadows = NULL;
  }

struct chart_item_shadow *
chartobj__WhichItem( self, x, y )
  register struct chartobj	     *self;
  register long			      x, y;
  {
  register struct chart_item_shadow  *shadow = Shadows;

  while ( shadow )
    {
    if ( chartobj_Within( self, x, y, ShadowBounds(shadow) ) )
      break;
    shadow = NextItem(shadow);
    }
  return  shadow;
  }

void
Printer( self, file, processor, format, level, printer )
  register struct chartobj	     *self;
  register FILE			     *file;
  register char			     *processor;
  register char			     *format;
  register boolean		      level;
  {
  IN(Printer);
  if ( !ScalesSuppressed )
    Print_Scales( self );
  if ( !LabelsSuppressed )
    Print_Labels( self );
  chartobj_PrintChart( self );
  OUT(Printer);
  }

void
chartobj__PrintChart( self )
  register struct chartobj	   *self;
  {
  IN(chartobj_PrintChart);
  chartobj_SetFont( self, chartobj_BuildFont( self, "andysans10b", NULL ) );
  chartobj_MoveTo( self, Center, Middle );
  chartobj_DrawString( self, "MISSING PrintChart METHOD", Balanced );
  OUT(chartobj_PrintChart);
  }

void
chartobj__Print( self, file, processor, format, top_level )
  register struct chartobj	     *self;
  register FILE			     *file;
  register char			     *processor;
  register char			     *format;
  register boolean		      top_level;
  {
  IN(chartobj_Print);
  if ( top_level )
    {
    chartobj_SetPrintOptions( self, aptv_PrintFillPage |
				    aptv_PrintLandScape );
    chartobj_SetPrintUnitDimensions( self, 8.5, 11.0 );
    chartobj_SetPrintPageDimensions( self, 8.5, 11.0 );
    }
  super_PrintObject( self, file, processor, format, top_level, Printer );
  OUT(chartobj_Print);
  }

static
Draw_Labels( self )
  register struct chartobj	 *self;
  {
  IN(Draw_Labels);
  chartobj_SetFont( self, LabelFont );
  if ( LabelPositions & chartv_Left )
    Draw_Vertical_Labels( self, LeftLabelsLeft,  LeftLabelsTop,
				LeftLabelsWidth, LeftLabelsHeight );
  if ( LabelPositions & chartv_Right )
    Draw_Vertical_Labels( self, RightLabelsLeft,  RightLabelsTop,
				RightLabelsWidth, RightLabelsHeight );
  if ( LabelPositions & chartv_Top )
    Draw_Horizontal_Labels( self, TopLabelsLeft,  TopLabelsTop,
				  TopLabelsWidth, TopLabelsHeight );
  if ( LabelPositions & chartv_Bottom )
    Draw_Horizontal_Labels( self, BottomLabelsLeft,  BottomLabelsTop,
				  BottomLabelsWidth, BottomLabelsHeight );
  OUT(Draw_Labels);
  }

static
Draw_Horizontal_Labels( self, left, top, width, height )
  register struct chartobj	 *self;
  {
  register struct chart_item	 *chart_item = chart_ItemAnchor( Chart );
  register short		  x, x_increment, y, excess, fudge, i = 0;

  IN(Draw_Horizontal_Labels);
  chartobj_SetTransferMode( self, graphic_WHITE );
  chartobj_FillRectSize( self, left, top, width, height, graphic_WHITE );
  chartobj_SetTransferMode( self, graphic_BLACK );
  if ( chart_ItemCount( Chart ) )
    {
    x_increment = width / chart_ItemCount( Chart );
    x = left + x_increment/2;
    y = top + height/2;
    if ( excess = (width - (x_increment * chart_ItemCount( Chart ))) / 2 )
      excess = chart_ItemCount( Chart ) / excess;
    while ( chart_item )
      {
      chartobj_MoveTo( self, x, y );
      fudge = (excess) ? ((i % excess) ? 0 : 1) : 0;
      x += x_increment + fudge;
      chartobj_DrawString( self,
	chart_ItemAttribute( Chart, chart_item, chart_ItemName(0) ), Balanced );
      chart_item = chart_NextItem( Chart, chart_item );
      i++;
      }
    }
  OUT(Draw_Horizontal_Labels);
  }

static
Draw_Vertical_Labels( self, left, top, width, height )
  register struct chartobj	 *self;
  {
  IN(Draw_Left_Labels);
/*===*/
  OUT(Draw_Left_Labels);
  }

static
Draw_Scales( self )
  register struct chartobj	 *self;
  {
  IN(Draw_Scales);
  chartobj_SetFont( self, ScaleFont );
  if ( ScalePositions & chartv_Left )	Draw_Left_Scale( self );
  if ( ScalePositions & chartv_Right )	Draw_Right_Scale( self );
  if ( ScalePositions & chartv_Top )	Draw_Top_Scale( self );
  if ( ScalePositions & chartv_Bottom )	Draw_Bottom_Scale( self );
  OUT(Draw_Scales);
  }

static
Draw_Left_Scale( self )
  register struct chartobj	 *self;
  {
  register long			  value, Y, adjust;
  register float		  y, y_increment;
  long				  half_y_increment;
  char				  value_string[25];

  IN(Draw_Left_Scale); /*=== NEEDS WORK ===*/
  Prepare_Vertical_Scale( self );
  chartobj_SetTransferMode( self, graphic_WHITE );
  chartobj_FillRectSize( self, LeftScaleLeft,  LeftScaleTop,
			       LeftScaleWidth, LeftScaleHeight, graphic_WHITE );
  chartobj_SetTransferMode( self, graphic_COPY );
  chartobj_MoveTo( self, LeftScaleBarX, LeftScaleTop );
  chartobj_DrawLineTo( self, LeftScaleBarX, LeftScaleBottom );
  chartobj_MoveTo( self, LeftScaleBarX, LeftScaleTop );
  chartobj_DrawLineTo( self, LeftScaleRight, LeftScaleTop );
  chartobj_MoveTo( self, LeftScaleBarX, LeftScaleBottom );
  chartobj_DrawLineTo( self, LeftScaleRight, LeftScaleBottom );
  chartobj_SetLineDash( self, "\001\004", 0, graphic_LineOnOffDash);
  chartobj_MoveTo( self, LeftScaleRight, LeftScaleTop );
  chartobj_DrawLineTo( self, LeftScaleRight+ChartWidth, LeftScaleTop );
  chartobj_MoveTo( self, LeftScaleRight, LeftScaleBottom );
  chartobj_DrawLineTo( self, LeftScaleRight+ChartWidth, LeftScaleBottom );
  chartobj_SetLineDash( self, NULL, 0, graphic_LineSolid);
  if ( (ScaleTick > 0) && (adjust = (value = chart_ItemValueLeast(Chart)) % ScaleTick ) )
    value -= ScaleTick + adjust;
  sprintf( value_string, "%d", value );
  chartobj_MoveTo( self, LeftScaleBarX-5, LeftScaleBottom );
  chartobj_DrawString( self, value_string, RightBottom );
  if ( (ScaleTick > 0) && (adjust = (value = chart_ItemValueGreatest(Chart)) % ScaleTick ) )
    value += ScaleTick - adjust;
  sprintf( value_string, "%d", value );
  chartobj_MoveTo( self, LeftScaleBarX-5, LeftScaleTop );
  chartobj_DrawString( self, value_string, RightTop );
  if ( y_increment = PixelsPerUnit * ScaleTick )
    {
    half_y_increment = y_increment / 2;
    Y = y = LeftScaleTop + y_increment;
    chartobj_MoveTo( self, LeftScaleBarX, Y - half_y_increment );
    chartobj_DrawLineTo( self, LeftScaleRight - 5, Y - half_y_increment );
    while ( Y < (LeftScaleBottom - half_y_increment) )
      {
      chartobj_MoveTo( self, LeftScaleBarX, Y );
      chartobj_DrawLineTo( self, LeftScaleRight, Y );
      chartobj_SetLineDash( self, "\001\004", 0, graphic_LineOnOffDash);
      chartobj_MoveTo( self, LeftScaleRight, Y );
      chartobj_DrawLineTo( self, LeftScaleRight+ChartWidth, Y );
      chartobj_SetLineDash( self, NULL, 0, graphic_LineSolid);
      chartobj_MoveTo( self, LeftScaleBarX, Y + half_y_increment );
      chartobj_DrawLineTo( self, LeftScaleRight - 5, Y + half_y_increment );
      value -= ScaleTick;
      sprintf( value_string, "%d", value );
      chartobj_MoveTo( self, LeftScaleBarX-5, Y );
      chartobj_DrawString( self, value_string, RightMiddle );
      Y = y = y + y_increment;
      }
    if ( y < (LeftScaleBottom - half_y_increment ) )
      {
      chartobj_MoveTo( self, LeftScaleBarX, Y = (y + half_y_increment) );
      chartobj_DrawLineTo( self, LeftScaleRight - 5, Y = (y + half_y_increment) );
      }
    }
  OUT(Draw_Left_Scale); /*=== NEEDS WORK ===*/
  }

static
Draw_Right_Scale( self )
  register struct chartobj	 *self;
  {
  IN(Draw_Right_Scale);
  Prepare_Vertical_Scale( self );
/*===*/
  OUT(Draw_Right_Scale);
  }

static
Prepare_Vertical_Scale( self )
  register struct chartobj	 *self;
  {
  long  ValueSpanScale = 1;
  long  ScaledValueSpan;
  static int nice_tick_values[] = {1, 5, 10, 25, 50, 100, 0};
  int tick, tick_index;

  IN(Prepare_Vertical_Scale);
  /* First scale the value span to < 1000. */
  DEBUGdt(Value Span,chart_ItemValueSpan( Chart ));
  ScaledValueSpan = chart_ItemValueSpan( Chart );
  while ( ScaledValueSpan >= 100 ) {
    ValueSpanScale *= 10;
    ScaledValueSpan /= 10;
  }
  DEBUGdt(ValueSpanScale, ValueSpanScale);
  /* Now find a tick size that is >= 20 pixels high */
  tick = nice_tick_values[0];
  tick_index = 0;
  while ( tick != 0 && PixelsPerUnit*tick*ValueSpanScale < 20 )
    tick = nice_tick_values[++tick_index];
  if (tick == 0)
    tick = nice_tick_values[tick_index-1];  /* use last nice value (can't happen) */
  DEBUGdt(tick, tick);
  ScaleTick = tick*ValueSpanScale;
  if (ScaleTick > chart_ItemValueSpan(Chart))
    ScaleTick = chart_ItemValueSpan(Chart);
  DEBUGdt(ScaleTick,ScaleTick);
  OUT(Prepare_Vertical_Scale);
  }

static
Draw_Top_Scale( self )
  register struct chartobj	 *self;
  {
  IN(Draw_Top_Scale);
  Prepare_Horizontal_Scale( self );
/*===*/
  OUT(Draw_Top_Scale);
  }

static
Draw_Bottom_Scale( self )
  register struct chartobj	 *self;
  {
  IN(Draw_Bottom_Scale);
  Prepare_Horizontal_Scale( self );
/*===*/
  OUT(Draw_Bottom_Scale);
  }

static
Prepare_Horizontal_Scale( self )
  register struct chartobj	 *self;
  {
  IN(Prepare_Horizontal_Scale);
/*===*/
  OUT(Prepare_Horizontal_Scale);
  }

static
Print_Scales( self )
  register struct chartobj	 *self;
  {
  IN(Print_Scales);
  chartobj_SetPrintLineWidth( self, 1 );
  chartobj_SetPrintFont( self, ScaleFontName );
  if ( ScalePositions & chartv_Left )	Print_Left_Scale( self );
/*===
  if ( ScalePositions & chartv_Right )	Print_Right_Scale( self );
  if ( ScalePositions & chartv_Top )	Print_Top_Scale( self );
  if ( ScalePositions & chartv_Bottom )	Print_Bottom_Scale( self );
===*/
  OUT(Print_Scales);
  }

static
Print_Left_Scale( self )
  register struct chartobj	 *self;
  {
  register long			  value, y, adjust,
				  y_increment, half_y_increment;
  char				  value_string[25];

  IN(Print_Left_Scale);
  Prepare_Vertical_Scale( self );
  chartobj_PrintLine( self, LeftScaleBarX, LeftScaleTop, LeftScaleBarX, LeftScaleBottom );
  chartobj_PrintLine( self, LeftScaleBarX, LeftScaleTop, LeftScaleRight, LeftScaleTop );
/*  chartobj_FillTrapezoid( self, LeftScaleRight, LeftScaleTop, ChartWidth,
				LeftScaleRight, LeftScaleTop, ChartWidth, DottedGraphic );*/
  chartobj_PrintLine( self, LeftScaleBarX, LeftScaleBottom, LeftScaleRight, LeftScaleBottom );
/*  chartobj_FillTrapezoid( self, LeftScaleRight, LeftScaleBottom, ChartWidth,
				LeftScaleRight, LeftScaleBottom, ChartWidth, DottedGraphic );*/
  if ( adjust = (value = chart_ItemValueLeast( Chart)) % ScaleTick )
    value -= ScaleTick + adjust;
  sprintf( value_string, "%d", value );
  chartobj_PrintString( self, LeftScaleBarX-5, LeftScaleBottom, value_string, RightBottom );
  if ( adjust = (value = chart_ItemValueGreatest( Chart)) % ScaleTick )
    value += ScaleTick - adjust;
  sprintf( value_string, "%d", value );
  chartobj_PrintString( self, LeftScaleBarX-5, LeftScaleTop, value_string, RightTop );
  if ( y_increment = PixelsPerUnit * ScaleTick )
    {
    half_y_increment = y_increment / 2;
    y = LeftScaleTop + y_increment;
    chartobj_PrintLine( self, LeftScaleBarX, y - half_y_increment,
			      LeftScaleRight - 5, y - half_y_increment );
    while ( y < (LeftScaleBottom - y_increment) )
      {
      chartobj_PrintLine( self, LeftScaleBarX, y, LeftScaleRight, y );
/*      chartobj_FillTrapezoid( self, LeftScaleRight, y, ChartWidth,
				    LeftScaleRight, y, ChartWidth, DottedGraphic );*/
      chartobj_PrintLine( self, LeftScaleBarX, y + half_y_increment,
				LeftScaleRight - 5, y + half_y_increment );
      value = value -= ScaleTick;
      sprintf( value_string, "%d", value );
      chartobj_PrintString( self, LeftScaleBarX-5, y, value_string, RightMiddle );
      y += y_increment;
      }
    if ( y < (LeftScaleBottom - half_y_increment ) )
      {
      chartobj_PrintLine( self, LeftScaleBarX, y + half_y_increment,
				LeftScaleRight - 5, y + half_y_increment );
      }
    }
  OUT(Print_Left_Scale);
  }

static
Print_Labels( self )
  register struct chartobj	 *self;
  {
  IN(Print_Labels);
  chartobj_SetPrintFont( self, LabelFontName );
/*===
  if ( LabelPositions & chartv_Left )	Print_Left_Labels( self );
  if ( LabelPositions & chartv_Right )	Print_Right_Labels( self );
===*/
  if ( LabelPositions & chartv_Top )
    Print_Horizontal_Labels( self, TopLabelsLeft, TopLabelsWidth, TopLabelsMiddle );
  if ( LabelPositions & chartv_Bottom )
    Print_Horizontal_Labels( self, BottomLabelsLeft, BottomLabelsWidth, BottomLabelsMiddle );
  OUT(Print_Labels);
  }

static
Print_Horizontal_Labels( self, left, width, middle )
  register struct chartobj	 *self;
  {
  register struct chart_item	 *chart_item = chart_ItemAnchor( Chart );
  register short		  x, x_increment, y;

  IN(Print_Horizontal_Labels);
  if ( chart_ItemCount( Chart ) )
    {
    x_increment = width / chart_ItemCount( Chart );
    x = left + x_increment/2;
    y = middle;
    while ( chart_item )
      {
      chartobj_PrintString( self, x, y,
	chart_ItemAttribute( Chart, chart_item, chart_ItemName(0) ), Balanced );
      x += x_increment;
      chart_item = chart_NextItem( Chart, chart_item );
      }
    }
  OUT(Print_Horizontal_Labels);
  }

