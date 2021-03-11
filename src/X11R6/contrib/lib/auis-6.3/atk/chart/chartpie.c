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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartpie.c,v 1.12 1993/11/18 02:34:33 gk5g Exp $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Pie Chart View-object

MODULE	chartpie.c

VERSION	0.0

AUTHOR	TC Peters
	Information Technology Center, Carnegie-Mellon University 

DESCRIPTION
	This is the suite of Methods that support the Chart View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/28/89	Created (TCP)
  09/05/89	Upgrade to V1.0 (TCP)
  09/07/89	Set CurrentItem (TCP)

END-SPECIFICATION  ************************************************************/

#include <math.h>
#include "graphic.ih"
#include "view.ih"
#include "apt.h"
#include "aptv.ih"
#include "chartobj.ih"
#include "chart.ih"
#include "chartpie.eh"
#include <ctype.h>

int chartpie_debug = 0;

#define debug chartpie_debug

struct chartpie_item
  {
  struct chart_item		*item;
  long				 x, y;
  long				 x_label, y_label;
  double			 percent;
  };

struct chartpie_drawing
  {
  struct rectangle		 bounds;
  long				 x_center;
  long				 y_center;
  double			 sum;
  long				 count;
  struct chartpie_item		 item[1];
  };

#define  Drawing		((self)->drawing)
#define  DrawingSum		Drawing->sum
#define  DrawingCount		Drawing->count
#define  DrawingBounds		&Drawing->bounds
#define  DrawingLeft		Drawing->bounds.left
#define  DrawingTop		Drawing->bounds.top
#define  DrawingWidth		Drawing->bounds.width
#define  DrawingHeight		Drawing->bounds.height
#define  DrawingX		Drawing->x_center
#define  DrawingY		Drawing->y_center
#define  DrawingItem(i)		Drawing->item[i].item
#define  DrawingItemX(i)	Drawing->item[i].x
#define  DrawingItemY(i)	Drawing->item[i].y
#define  DrawingItemLabelX(i)	Drawing->item[i].x_label
#define  DrawingItemLabelY(i)	Drawing->item[i].y_label
#define  DrawingItemPercent(i)	Drawing->item[i].percent

#define  Screen			1
#define  Paper			2

#define  Bounds			(chartpie_ChartBounds(self))
#define  Left			(chartpie_ChartLeft(self))
#define  Top			(chartpie_ChartTop(self))
#define  Width			(chartpie_ChartWidth(self))
#define  Height			(chartpie_ChartHeight(self))
#define  Right			(chartpie_ChartRight(self))
#define  Bottom			(chartpie_ChartBottom(self))
#define  Middle			(chartpie_ChartMiddle(self))
#define  Center			(chartpie_ChartCenter(self))


boolean 
chartpie__InitializeObject( classID, self)
  register struct classheader	 *classID;
  register struct chartpie	 *self;
  {
  IN(chartpie_InitializeObject);
  chartpie_SetShrinkIcon( self, 'e', "icon12", "PieChart", "andysans10b" );
  chartpie_SetHelpFileName( self, "/usr/andy/help/ez.help"/*=== ===*/ );
  chartpie_SetChartOptions( self, chartobj_SuppressScales | chartobj_SuppressLabels );
  Drawing = NULL;
  OUT(chartpie_InitializeObject);
  return  TRUE;
  }

void 
chartpie__FinalizeObject( classID, self )
  register struct classheader	 *classID;
  register struct chartpie	 *self;
  {
  IN(chartpie_FinalizeObject);
  if ( Drawing )  free( Drawing );
  OUT(chartpie_FinalizeObject);
  }

void
chartpie__SetDebug( self, state )
  register struct chartpie	 *self;
  register char			  state;
  {
  IN(chartpie_SetDebug);
  super_SetDebug( self, debug = state );
  OUT(chartpie_SetDebug);
  }

struct view *
chartpie__HitChart( self, action, x, y, clicks )
  register struct chartpie	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  static long			      prior_x, prior_y, candidate;
  register double		      radius = DrawingX - DrawingLeft,
				      hyp, x_delta, y_delta, delta = 0,
				      start_angle = 0, end_angle, angle;
  register struct view		     *hit;

  IN(chartpie_HitChart);
  hit = (struct view *) self;
  x_delta = x - DrawingX;  y_delta = y - DrawingY;
  hyp = hypot( x_delta, y_delta );
  angle = -(360.0 * (atan2( y_delta, x_delta ) / 6.28318));
  if ( angle <= 0 )    angle += 360.0; /*===messy===*/
  switch ( action )
    {
    case  view_LeftDown:
      chartpie_SetFont( self, chartpie_IconFont(self) );
      chartpie_SetTransferMode( self, graphic_INVERT );
      for ( candidate = 0; candidate < DrawingCount; candidate++ )
	{
	end_angle = start_angle + (3.6 * DrawingItemPercent(candidate));
	if ( angle >= start_angle  &&  angle <= end_angle )
	  {
	  chartpie_CurrentItem( self ) = DrawingItem(candidate);
	  break;
	  }
	start_angle = end_angle;
	}
      chartpie_MoveTo( self, prior_x = DrawingItemX(candidate),
			     prior_y = DrawingItemY(candidate) );
      chartpie_DrawString( self, "E", NULL );
      break;
    case  view_LeftMovement:
      chartpie_MoveTo( self, prior_x, prior_y );
      chartpie_DrawString( self, "E", NULL );
      chartpie_MoveTo( self, prior_x = DrawingX + (radius * (x_delta / hyp)),
			     prior_y = DrawingY + (radius * (y_delta / hyp)) );
      chartpie_DrawString( self, "E", NULL );
      break;
    case  view_LeftUp:
      chartpie_MoveTo( self, prior_x, prior_y );
      chartpie_DrawString( self, "E", NULL );
      if ( delta )/*===*/
        chart_NotifyObservers( Data, chart_ItemValueChanged );
      break;
    }
  OUT(chartpie_HitChart);
  return  hit;
  }

void
chartpie__DrawChart( self )
  register struct chartpie	     *self;
  {
  IN(chartpie_DrawChart);
  Show_Pie_Chart( self, Screen );
  OUT(chartpie_DrawChart);
  }

void
chartpie__PrintChart( self )
  register struct chartpie	     *self;
  {
  IN(chartpie_PrintChart);
  Show_Pie_Chart( self, Paper );
  OUT(chartpie_PrintChart);
  }

static
Show_Pie_Chart( self, medium )
  register struct chartpie	     *self;
  register long			      medium;
  {
  register long			      i;
  short				      height;
  char				      percent_string[257];
  register struct chart_item	     *item;
  register double		      degrees, current_degree = 0.0;
  register struct fontdesc	     *font;

  IN(Show_Pie_Chart);
  Compute_Pie_Points( self );
  chartpie_SetFont( self, font = chartpie_BuildFont( self, "andysans10", &height ) );
  chartpie_SetTransferMode( self, graphic_BLACK );
  if ( medium == Screen )
    chartpie_DrawOval( self, DrawingBounds );
  else if ( medium == Paper )
      chartpie_PrintCircle(self, DrawingX, DrawingY,
			   MIN(DrawingX - DrawingLeft, DrawingY - DrawingTop));
  item = chart_ItemAnchor( Data );
  for ( i = 0; i < chart_ItemCount(Data)  &&  item; i++ )
    {
    if ( medium == Screen )
      {
      chartpie_MoveTo( self, DrawingX, DrawingY );
      chartpie_DrawLineTo( self, DrawingItemX(i), DrawingItemY(i) );
      }
      else
      {
      degrees = 360.0 *
	(chart_ItemAttribute( Data, item, chart_ItemValue(0) ) / DrawingSum);
      chartpie_PrintSlice( self, DrawingX, DrawingY, DrawingX - DrawingLeft,
			     current_degree, current_degree += degrees,
			     i, chart_ItemCount(Data), 0 );
      }
    if ( chart_ItemAttribute( Data, item, chart_ItemName(0) ) )
      { DEBUGst(Name,chart_ItemAttribute( Data, item, chart_ItemName(0) ));
      if ( chart_ItemFontName( Data ) )
	{
	font = chartpie_BuildFont( self,
		    chart_ItemFontName( Data ), &height );
	if ( medium == Screen )
          chartpie_SetFont( self, font );
	  else
	  chartpie_SetPrintFont( self, chart_ItemFontName( Data ) );
	}
      sprintf( percent_string, "%.2f%%", DrawingItemPercent(i) );
      if ( medium == Screen )
	{
	chartpie_MoveTo( self, DrawingItemLabelX(i), DrawingItemLabelY(i) );
	chartpie_DrawString( self, chart_ItemAttribute( Data, item, chart_ItemName(0) ),
	    view_BETWEENLEFTANDRIGHT | view_BETWEENTOPANDBOTTOM );
	chartpie_MoveTo( self, DrawingItemLabelX(i), DrawingItemLabelY(i) + height );
	chartpie_DrawString( self, percent_string,
	    view_BETWEENLEFTANDRIGHT | view_BETWEENTOPANDBOTTOM );
	}
	else
	{
	chartpie_SetPrintGrayLevel( self, 0.0 );
	chartpie_PrintString( self, DrawingItemLabelX(i), DrawingItemLabelY(i),
				chart_ItemAttribute( Data, item, chart_ItemName(0) ), 0 );
	chartpie_PrintString( self, DrawingItemLabelX(i), DrawingItemLabelY(i) + height,
				percent_string, 0 );
	}
      }
    item = chart_NextItem( Data, item );
    }
  OUT(Show_Pie_Chart);
  }

static
Compute_Pie_Points( self )
  register struct chartpie	     *self;
  {
  register long			      i, count = 0;
  register struct chart_item	     *item;
  register double		      sum = 0, radian,
				      degrees, current_degree = 90.0, radius;

  IN(Compute_Pie_Points);
  item = chart_ItemAnchor( Data );
  for ( i = 0; i < chart_ItemCount( Data )  &&  item; i++ )
    {
    count++;
    sum += chart_ItemAttribute( Data, item, chart_ItemValue(0) );
    item = chart_NextItem( Data, item );
    }
  if ( Drawing )    free( Drawing );
  Drawing = (struct chartpie_drawing *)
	malloc( sizeof(struct chartpie_drawing) + count * sizeof(struct chartpie_item) );
  DrawingCount = count;
  DEBUGdt(Count,DrawingCount);
  DrawingSum = sum;
  DEBUGgt(Sum,DrawingSum);
  DrawingTop = Top;
  if ( Width > Height )
    DrawingWidth = DrawingHeight = Height - 1;
    else
    {
    DrawingWidth = DrawingHeight = Width - 1;
    DrawingTop = Middle - DrawingHeight / 2;
    }
  DrawingLeft = Center - DrawingWidth / 2;
  DEBUGdt(Left,DrawingLeft);   DEBUGdt(Top,DrawingTop);
  DEBUGdt(Width,DrawingWidth); DEBUGdt(Height,DrawingHeight);
  radius = DrawingWidth / 2;
  DrawingX = Center;
  DrawingY = DrawingTop + radius;
  item = chart_ItemAnchor( Data );
  for ( i = 0; i < chart_ItemCount(Data)  &&  item; i++ )
    {
    DrawingItem(i) = item;
    DEBUGdt(Value,chart_ItemAttribute( Data, item, chart_ItemValue(0) ));
    DrawingItemPercent(i) =
	(chart_ItemAttribute( Data, item, chart_ItemValue(0) ) / DrawingSum) * 100;
    degrees = 3.6 * DrawingItemPercent(i);
    radian = 6.28318 * ((current_degree + degrees)/360.0);
    DrawingItemX(i) = DrawingX + radius * sin( radian );
    DrawingItemY(i) = DrawingY + radius * cos( radian );
    radian = 6.28318 * ((current_degree + (degrees/2.0))/360.0);
    DrawingItemLabelX(i) = DrawingX + (radius * 0.75) * sin( radian );
    DrawingItemLabelY(i) = DrawingY + (radius * 0.75) * cos( radian );
    current_degree += degrees;
    item = chart_NextItem( Data, item );
    }
  OUT(Compute_Pie_Points);
  }
