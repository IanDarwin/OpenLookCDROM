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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/chartlin.c,v 1.13 1992/12/15 21:29:20 rr2b R6tape $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Line Chart View-object

MODULE	chartlin.c

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
  05/05/89	Improve positioning of line segments (TCP)
  05/08/89	Further improve positioning (TCP)
  05/10/89	Depend upon pre-computed data-points in DrawChart (TCP)
  05/31/89	Add classID parameter in FinalizeObject (TCP)
  06/02/89	Use super-class Hit method (TCP)
  09/05/89	Upgrade to V1.0 (TCP)

END-SPECIFICATION  ************************************************************/

int chartlin_debug = 0;

#define debug chartlin_debug



#include <math.h>
#include "graphic.ih"
#include "observe.ih"
#include "view.ih"
#include "im.ih"
#include "rect.h"
#include "apt.h"
#include "aptv.ih"
#include "chartobj.ih"
#include "chart.ih"
#include "chartlin.eh"
#include <ctype.h>

#define  Screen			1
#define  Paper			2

#define  Bounds			(chartlin_ChartBounds(self))
#define  Left			(chartlin_ChartLeft(self))
#define  Top			(chartlin_ChartTop(self))
#define  Width			(chartlin_ChartWidth(self))
#define  Height			(chartlin_ChartHeight(self))
#define  Right			(chartlin_ChartRight(self))
#define  Bottom			(chartlin_ChartBottom(self))

#define  Items			(chartlin_Items(self))
#define  ItemBounds(s)		(chartlin_ItemBounds(self,(s)))
#define  ItemLeft(s)		(chartlin_ItemLeft(self,(s)))
#define  ItemTop(s)		(chartlin_ItemTop(self,(s)))
#define  ItemWidth(s)		(chartlin_ItemWidth(self,(s)))
#define  ItemHeight(s)		(chartlin_ItemHeight(self,(s)))
#define  ItemCenter(s)		(chartlin_ItemCenter(self,(s)))
#define  ItemMiddle(s)		(chartlin_ItemMiddle(self,(s)))
#define  ItemX(s)		(chartlin_ItemX(self,(s)))
#define  ItemY(s)		(chartlin_ItemY(self,(s)))
#define  NextItem(s)		(chartlin_NextItem(self,(s)))

#define  PixelsPerUnit		(chartlin_PixelsPerUnit( self ))
#define  PixelsPerInterval	(chartlin_PixelsPerInterval( self ))

boolean 
chartlin__InitializeObject( classID, self)
  register struct classheader	 *classID;
  register struct chartlin	 *self;
  {
  IN(chartlin_InitializeObject);
  chartlin_SetShrinkIcon( self, 'e', "icon12", "LineChart", "andysans10b" );
  chartlin_SetHelpFileName( self, "/usr/andy/help/ez.help"/*=== ===*/ );
  OUT(chartlin_InitializeObject);
  return  TRUE;
  }

void
chartlin__FinalizeObject( classID, self )
  register struct classheader	 *classID;
  register struct chartlin	 *self;
  {}

void
chartlin__SetDebug( self, state )
  register struct chartlin	 *self;
  register char			  state;
  {
  IN(chartlin_SetDebug);
  super_SetDebug( self, debug = state );
  OUT(chartlin_SetDebug);
  }

char *
chartlin__Moniker( self )
  register struct chartlin   *self;
  {
  IN(chartlin_Moniker);
  OUT(chartlin_Moniker);
  return  "Line";
  }

void
chartlin__DrawChart( self )
  register struct chartlin	     *self;
  {
  register struct chart_item_shadow  *shadow = Items;
  register short		      prior_x, prior_y;

  IN(chartlin_DrawChart);
  prior_x = 0;
  while ( shadow )
    {
    DEBUGdt(Value,chart_ItemAttribute( Data, shadow->item, chart_ItemValue(0) ));
    if ( prior_x )
      {
      chartlin_MoveTo( self, prior_x, prior_y );
      chartlin_DrawLineTo( self, ItemX(shadow), ItemY(shadow) );
      }
    prior_x = ItemX(shadow);
    prior_y = ItemY(shadow);
    shadow = NextItem(shadow);
    }
  OUT(chartlin_DrawChart);
  }

void
chartlin__PrintChart( self )
  register struct chartlin	     *self;
  {
  register long			      i, left, top, width,
				      count = chart_ItemCount( Data),
				      excess, fudge;
  register struct chart_item	     *chart_item = chart_ItemAnchor( Data );
  struct point			      prior_point;

  IN(chartlin_PrintChart);
  DEBUGdt(ItemCount,count);
  width = (Width / ((count) ? count : 1));
  left = Left + width/2;
  if ( excess = (Width - (width * count)) / 2 )
    excess = count / excess;
  chartlin_SetPrintLineWidth( self, 1 );
  chartlin_SetPrintGrayLevel( self, 0.0 );
  prior_point.x = 0;
  for ( i = 0; i < count  &&  chart_item; i++ )
    {
    DEBUGdt(Value,chart_ItemAttribute( Data, chart_item, chart_ItemValue(0) ));
    top = Bottom - (chart_ItemAttribute( Data, chart_item, chart_ItemValue(0) ) * PixelsPerUnit);
    if ( prior_point.x )
      chartlin_PrintLine( self, prior_point.x, prior_point.y, left, top );
    fudge = (excess) ? ((i % excess) ? 0 : 1) : 0;
    prior_point.x = left + fudge;
    prior_point.y = top;
    left += width + fudge;
    chart_item = chart_NextItem( Data, chart_item );
    }
  OUT(chartlin_PrintChart);
  }
