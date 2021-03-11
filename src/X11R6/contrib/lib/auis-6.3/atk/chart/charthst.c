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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/atk/chart/RCS/charthst.c,v 1.11 1992/12/15 21:29:20 rr2b R6tape $";
#endif

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Histogram Chart View-object

MODULE	charthst.c

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
  05/10/89	Utilize pre-computed data-points per chartobj (TCP)
  05/31/89	Add classID parameter in FinalizeObject (TCP)
  06/01/89	Accomodate negative values (TCP)
  06/02/89	Fix Printing of negative values (TCP)
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
#include "charthst.eh"
#include <ctype.h>

int charthst_debug = 0;

#define debug charthst_debug

#define  Screen			1
#define  Paper			2

#define  Bounds			(charthst_ChartBounds(self))
#define  Left			(charthst_ChartLeft(self))
#define  Top			(charthst_ChartTop(self))
#define  Width			(charthst_ChartWidth(self))
#define  Height			(charthst_ChartHeight(self))
#define  Right			(charthst_ChartRight(self))
#define  Bottom			(charthst_ChartBottom(self))
#define  BaseLine		(charthst_ChartBaseLine(self))

#define  PixelsPerUnit		(charthst_PixelsPerUnit( self ))
#define  PixelsPerInterval	(charthst_PixelsPerInterval( self ))

#define  Items			(charthst_Items(self))
#define  ItemBounds(shadow)	(charthst_ItemBounds(self,(shadow)))
#define  ItemX(shadow)		(charthst_ItemX(self,(shadow)))
#define  ItemY(shadow)		(charthst_ItemY(self,(shadow)))
#define  ItemLeft(shadow)	(charthst_ItemLeft(self,(shadow)))
#define  ItemTop(shadow)	(charthst_ItemTop(self,(shadow)))
#define  ItemWidth(shadow)	(charthst_ItemWidth(self,(shadow)))
#define  ItemHeight(shadow)	(charthst_ItemHeight(self,(shadow)))
#define  ItemCenter(shadow)	(charthst_ItemCenter(self,(shadow)))
#define  ItemMiddle(shadow)	(charthst_ItemMiddle(self,(shadow)))
#define  NextItem(shadow)	(charthst_NextItem(self,(shadow)))

#define  abs(x)			(((x) < 0) ? -(x): (x))

boolean 
charthst__InitializeObject( classID, self)
  register struct classheader	 *classID;
  register struct charthst	 *self;
  {
  IN(charthst_InitializeObject);
  charthst_SetShrinkIcon( self, 'e', "icon12", "HistogramChart", "andysans10b" );
  charthst_SetHelpFileName( self, "/usr/andy/help/ez.help"/*=== ===*/ );
  OUT(charthst_InitializeObject);
  return  TRUE;
  }

void
charthst__FinalizeObject( classID, self )
  register struct classheader	 *classID;
  register struct charthst	 *self;
  {}

char *
charthst__Moniker( self )
  register struct charthst   *self;
  {
  IN(charthst_Moniker);
  OUT(charthst_Moniker);
  return  "Histogram";
  }

void
charthst__SetDebug( self, state )
  register struct charthst     *self;
  register char		       state;
  {
  IN(charthst_SetDebug);
  super_SetDebug( self, debug = state );
  OUT(charthst_SetDebug);
  }

struct view *
charthst__HitChart( self, action, x, y, clicks )
  register struct charthst	     *self;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  register struct view		     *hit;
  static struct chart_item_shadow    *shadow;
  register long			      delta;
  static long			      initial_y, prior_y, y_offset,
				      value, value_original;
  static char			      value_string[257], *name;

  IN(charthst_HitChart);
  hit = (struct view *) self;
  if ( shadow  ||
	 (action == view_LeftDown  &&  (shadow = charthst_WhichItem( self, x, y ))) )
    {
    charthst_CurrentItem(self) = shadow->item;
    if ( y > Bottom ) y = Bottom;
    if ( y < Top )    y = Top;
    y += y_offset;
    switch ( action )
      {
      case  view_LeftDown:
	charthst_UseInvisibleCursor( self );
        charthst_SetTransferMode( self, graphic_INVERT );
	y_offset = ItemY(shadow) - y;
	initial_y = prior_y = y = ItemY(shadow);
	charthst_MoveTo( self, Left, y );
	charthst_DrawLineTo( self, Right, y );
	name = (char *) chart_ItemAttribute( Data, shadow->item, chart_ItemName(0) );
	value = value_original = chart_ItemAttribute( Data, shadow->item, chart_ItemValue(0) );
	DEBUGdt(Initial-value,value);
        break;
      case  view_LeftMovement:
	charthst_MoveTo( self, Left, prior_y );
	charthst_DrawLineTo( self, Right, prior_y );
	if ( (abs(delta = prior_y - y)) > PixelsPerInterval )
	  value += (delta / PixelsPerInterval) * chart_ItemValueRangeInterval( Data );
	charthst_MoveTo( self, Left, prior_y = y );
	charthst_DrawLineTo( self, Right, y );
        break;
      case  view_LeftUp:
	charthst_MoveTo( self, Left, prior_y );
	charthst_DrawLineTo( self, Right, prior_y );
	if ( abs(delta = initial_y - y) > PixelsPerInterval )
	  {
	  value_original += (delta / PixelsPerInterval) *
				chart_ItemValueRangeInterval( Data );
	  DEBUGdt(Final-value,value);
	  chart_SetItemAttribute( Data, shadow->item, chart_ItemValue((value = value_original)) );
	  chart_SetModified( Data );
	  chart_NotifyObservers( Data, chart_ItemValueChanged );
	  }
	shadow = NULL;
	charthst_UseNormalCursor( self );
	y_offset = 0;
        break;
      }
    sprintf( value_string, "%s:  Value = %d", name, value );
    charthst_Announce( self, value_string );
    }
  OUT(charthst_HitChart);
  return  hit;
  }

void
charthst__DrawChart( self )
  register struct charthst	     *self;
  {
  register long			      left, width, top, height, i = 0,
				      count = chart_ItemCount( Data ),
				      excess, fudge;
  register struct chart_item_shadow  *shadow = Items;

  IN(charthst_DrawChart);
  width = (Width / ((count) ? count : 1)) - 1;
  DEBUGdt(width,width);
  left = Left;
  if ( excess = (Width - ((width + 1) * count)) / 2 )
    excess = count / excess;
  DEBUGdt(excess,excess);
  while ( shadow )
    {
    DEBUGdt(Value,chart_ItemAttribute( Data, shadow->item, chart_ItemValue(0) ));
    fudge = (excess) ? ((i % excess) ? 0 : 1) : 0;
    top = (ItemY(shadow) < BaseLine) ? ItemY(shadow) : BaseLine;
    height = abs(BaseLine - ItemY(shadow));
    charthst_FillRectSize( self, ItemLeft(shadow) = left, ItemTop(shadow) = top,
	ItemWidth(shadow) = width+fudge, ItemHeight(shadow) = height, graphic_BLACK );
    ItemTop(shadow) -= 5;
    ItemHeight(shadow) += 10;
    left += width + 1 + fudge;
    shadow = NextItem(shadow);
    i++;
    }
  OUT(charthst_DrawChart);
  }

void
charthst__PrintChart( self )
  register struct charthst	     *self;
  {
  register long			      i, left, top, width, height,
				      count = chart_ItemCount( Data ),
				      excess, fudge;
  register struct chart_item_shadow  *shadow = Items;
  register struct aptv_path	     *path;
  register struct aptv_point	     *path_point;

  IN(charthst_PrintChart);
  path = (struct aptv_path *)
     malloc( sizeof(struct aptv_path) + 5 * sizeof(struct aptv_point) );
  width = (Width / ((count) ? count : 1)) - 1;
  left = Left;
  if ( excess = (Width - ((width + 1) * count)) / 2 )
    excess = count / excess;
  charthst_SetPrintLineWidth( self, 1 );
  path->count = 5;
  path_point = &path->points[0];
  for ( i = 0; i < count; i++ )
    {
    top = (ItemY(shadow) < BaseLine) ? ItemY(shadow) : BaseLine;
    height = abs(BaseLine - ItemY(shadow));
    fudge = (excess) ? ((i % excess) ? 0 : 1) : 0;
    path_point = &path->points[0];
    path_point->x = left;		    path_point->y = top;	    path_point++;
    path_point->x = left + width + fudge;   path_point->y = top;	    path_point++;
    path_point->x = left + width + fudge;   path_point->y = top + height;   path_point++;
    path_point->x = left;		    path_point->y = top + height;   path_point++;
    path_point->x = left;		    path_point->y = top;
    charthst_SetPrintGrayLevel( self, 0.7 );
    charthst_PrintPathFilled( self, path );
    charthst_SetPrintGrayLevel( self, 0.0 );
    charthst_PrintPath( self, path );
    left += width + 1 + fudge;
    shadow = NextItem(shadow);
    }
  charthst_SetPrintLineWidth( self, 1 );
  if ( path )  free( path );
  OUT(charthst_PrintChart);
  }
