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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/ziporect.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* ziporect.c	Zip Object -- Rectangle					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: ziporect.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:00:54  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:58:46  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.13  1992/09/10  00:52:12  gk5g
 * added #include <view.ih> ... a new view methods isn't being found for these subclasses of view!?!?!
 * .
 *
 * Revision 2.12  1991/09/12  16:43:42  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.11  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.10  1990/08/21  14:38:09  sg08
 * Fixed history log
 * ,
 *
 * Revision 2.9  90/08/21  14:34:53  sg08
 * Use Ensure_Attribute on Draw and Print
 * 
 * Revision 2.8  89/09/08  17:42:30  ghoti
 * removal of unused variables
 * 
 * Revision 2.7  89/08/25  18:53:01  sg08
 * reduce TransferMode activity, modify build
 * 
 * Revision 2.6  89/05/01  22:14:13  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.5  89/02/08  16:51:27  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  20:05:03  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/18  21:12:35  tom
 * Handle variable line-widths.
 * 
 * Revision 2.2  88/10/11  20:34:52  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:17:37  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:43:49  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:46:12  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Rectangle

MODULE	ziporect.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)
   08/24/89	Remove excess SetTransferMode() activity in Draw() (SCG)
   08/24/89	Modify Build to handle non-refresh of pane on build completion (SCG)
   08/14/90	Add Ensure_Attribute on Draw and Print (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <view.ih>
#include <fontdesc.ih>
#include <zipobj.ih>
#include <ziporect.eh>

static Draw();
static Compute_Handle_Positions();

static char				*rect_attributes[] =
					{
					"icon", "B",
					"iconfontname", IconFontName,
					NULL
					};
char
ziporect__Object_Attributes( self, attributes )
  register struct ziporect		 *self;
  register char				**attributes[];
  {
  IN(ziporect__Object_Attributes);
  *attributes = rect_attributes;
  OUT(ziporect__Object_Attributes);
  return  zip_ok;
  }

char
ziporect__Object_Icon( self )
  register struct ziporect		 *self;
  {
  IN(ziporect__Object_Icon);
  OUT(ziporect__Object_Icon);
  return  'G';
  }

char
ziporect__Object_Icon_Cursor( self )
  register struct ziporect		 *self;
  {
  IN(ziporect__Object_Icon_Cursor);
  OUT(ziporect__Object_Icon_Cursor);
  return  'B';
  }

char
ziporect__Object_Datastream_Code( self )
  register struct ziporect		 *self;
  {
  IN(ziporect__Object_Datastream_Code);
  OUT(ziporect__Object_Datastream_Code);
  return  'G';
  }

long
ziporect__Show_Object_Properties( self, pane, figure )
  register struct ziporect		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw Rectangle from Upper-left to Lower-right." );
  return  zip_ok;
  }

long
ziporect__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct ziporect		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  int					  position = 0; /*===*/
  register zip_type_figure		  figure;
  register long				  status = zip_ok;

  IN(ziporect__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
        zip_Create_Figure( Data, &CurrentFigure, NULL, zip_rectangle_figure,
			   CurrentImage, position )) == zip_ok )
	{
        ziporect_Set_Object_Point( self, CurrentFigure,	zip_figure_origin_point, X, Y );
        ziporect_Set_Object_Point( self, CurrentFigure,	zip_figure_auxiliary_point, X, Y );
	CurrentFigure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
        pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	zip_Set_Figure_Shade( Data, CurrentFigure,
			      pane->zip_pane_edit->zip_pane_edit_current_shade );
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	}
      break;
    case view_LeftUp:
      if ( figure = CurrentFigure )
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	if ( figure_x_point == figure_x_points(0)  &&
	     figure_y_point == figure_y_points(0) )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
	  }
        else
          {
	  zipview_Draw_Figure( View, CurrentFigure, pane );
  	  }
	}
      break;
    case view_LeftMovement:
      if ( CurrentFigure )
	{
	zipview_Draw_Figure( View, CurrentFigure, pane );
        ziporect_Set_Object_Point( self, CurrentFigure,	zip_figure_auxiliary_point, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	}
      break;
    }
  OUT(ziporect__Build_Object);
  return  status;
  }

long
ziporect__Draw_Object( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziporect__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane, zip_draw );
  OUT(ziporect__Draw_Object);
  return  status;
  }

long
ziporect__Clear_Object( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziporect__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane, zip_clear );
  OUT(ziporect__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane, action )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register short			  action;
  {
  register long				  status = zip_ok,
					  left = window_x_point, top = window_y_point,
					  width = window_x_points(0) - left,
					  height = window_y_points(0) - top;
  register unsigned char		  pattern = NULL, shade;

  IN(Draw);
  if ( width < 0 )
    { left = window_x_points(0);  width = -width; }
  if ( height < 0 )
    { top = window_y_points(0);   height = -height;  }
  if ( (figure->zip_figure_mode.zip_figure_mode_shaded  ||
        figure->zip_figure_mode.zip_figure_mode_patterned) )
    {
    if ( View->mouse_action != view_LeftMovement  &&  action == zip_draw )
      {
      if ( figure->zip_figure_mode.zip_figure_mode_patterned  &&
	   (pattern = zip_Contextual_Figure_Pattern( Data, figure )) )
        { DEBUGct(Pattern,pattern);
        zipview_FillTrapezoid(View, left,top,width, left,top+height+1,width+1,
	    zipview_Define_Graphic( View,
	    zipview_Select_Contextual_Figure_Font( View, figure ), pattern ));
        }
        else
        /* Shade of '0' means Transparent --- Shade of '1' means White */
        if ( (shade = zip_Contextual_Figure_Shade( Data, figure )) >= 1  &&
	      shade <= 100 )
	  { DEBUGdt(Shade,shade);
	  if ( (shade = ('0' + ((shade + 10) / 10)) - 1) > '9' )  shade = '9';
	  DEBUGdt(Shade-index,shade);
          zipview_Ensure_Fill_Attributes( View, figure );
	  zipview_FillRectSize( View, left,top, width+1,height+1,
	    zipview_Define_Graphic( View, zip_Define_Font( Data, ShadeFontName, NULL ), shade ));
	  }
      }
      else
      if ( action == zip_clear )
	{ DEBUG(Clear Action);
	zipview_EraseRectSize( View, left+1, top+1, width-1, height-1 );
	}
    }
  if ( zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
    zipview_DrawRectSize( View, left, top, width, height );
  if ( ExposePoints )
    ziporect_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )
    ziporect_Highlight_Object_Points( self, figure, pane );
  OUT(Draw);
  return  status;
  }

long
ziporect__Print_Object( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register long				  left, right, top, bottom;

  IN(ziporect__Print_Object);
  left = print_x_point;       top = print_y_point;
  right = print_x_points(0);  bottom = print_y_points(0);
  if ( right < left )
    { DEBUG(X-Flipped);
    left = right;
    right = print_x_point;
    }
  if ( bottom < top )
    { DEBUG(Y-Flipped);
    top = bottom;
    bottom = print_y_point;
    }
  zipprint_Set_Shade( Print, figure->zip_figure_fill.zip_figure_shade ); 
  zipprint_Ensure_Line_Attributes( Print, figure );
  zipprint_Draw_Rectangle( Print, left, top, right, bottom );
  OUT(ziporect__Print_Object);
  return  status;
  }

long
ziporect__Proximate_Object_Points( self, figure, pane, x, y )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(ziporect__Proximate_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y1, x, y ) )
    point = zip_figure_origin_point;	    /* Upper Left Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y3, x, y ) )
    point = zip_figure_auxiliary_point;	    /* Lower Right Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y1, x, y ) )
    point = zip_figure_auxiliary_point + 1; /* Upper Right Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y3, x, y ) )
    point = zip_figure_auxiliary_point + 2; /* Lower Left Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y1, x, y ) )
    point = zip_figure_auxiliary_point + 3; /* 12 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y2, x, y ) )
    point = zip_figure_auxiliary_point + 4; /* 3 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y3, x, y ) )
    point = zip_figure_auxiliary_point + 5; /* 6 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y2, x, y ) )
    point = zip_figure_auxiliary_point + 6; /* 9 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y2, x, y ) )
    point = zip_figure_auxiliary_point + 7; /* Center */
  OUT(ziporect__Proximate_Object_Points);
  return  point;
  }

long
ziporect__Within_Object( self, figure, pane, x, y )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register long				  distance = -1,
					  X1, Y1, X2, Y2,
					  X, Y, XA, XB, YA, YB;

  IN(ziporect__Within_Object);
  X1 = window_x_point;	    Y1 = window_y_point;
  X2 = window_x_points(0);  Y2 = window_y_points(0);
  if ( x >= X1  &&  x <= X2  &&  y >= Y1  &&  y <= Y2 )
    {
    if ( (XA = abs(x - X1)) < (XB = abs(x - X2)) )
      X = XA;
      else
      X = XB;
    if ( (YA = abs(y - Y1)) < (YB = abs(y - Y2)) )
      Y = YA;
      else
      Y = YB;
    distance = (X * X) + (Y * Y);
    }
  OUT(ziporect__Within_Object);
  return  distance;
  }

boolean
ziporect__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = false;
  register zip_type_pixel		  X1, Y1, X2, Y2;

  IN(ziporect__Enclosed_Object);
  X1 = window_x_point;	    Y1 = window_y_point;
  X2 = window_x_points(0);  Y2 = window_y_points(0);
  if ( X1 > x  &&  Y1 > y  &&  X2 < (x + w)  &&  Y2 < (y + h) )
    enclosed = true;
  OUT(ziporect__Enclosed_Object);
  return  enclosed;
  }

long
ziporect__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(ziporect__Object_Enclosure);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  *x = X1;  *y = Y1;  *w = abs(X3 - X1);  *h = abs(Y3 - Y1);
  OUT(ziporect__Object_Enclosure);
  return  zip_ok;
  }

long
ziporect__Highlight_Object_Points( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(ziporect__Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Highlight_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(ziporect__Highlight_Object_Points);
  return  status;
  }

long
ziporect__Normalize_Object_Points( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(ziporect__Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Normalize_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(ziporect__Normalize_Object_Points);
  return  status;
  }

long
ziporect__Expose_Object_Points( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziporect__Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point,     figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_points(0), figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_points(0), figure_y_points(0) );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point,     figure_y_points(0) );
  OUT(ziporect__Expose_Object_Points);
  return  status;
  }

long
ziporect__Hide_Object_Points( self, figure, pane )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziporect__Hide_Object_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point,     figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_points(0), figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_points(0), figure_y_points(0) );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point,     figure_y_points(0) );
  OUT(ziporect__Hide_Object_Points);
  return  status;
  }

long
ziporect__Set_Object_Point( self, figure, point, x, y )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;

  IN(ziporect__Set_Object_Point);
  if ( figure->zip_figure_points == NULL  &&
       (status = zip_Allocate_Figure_Points_Vector(
		    Data, &figure->zip_figure_points )) == zip_ok )
    {
    figure->zip_figure_points->zip_points_count = 1;
    }
  if ( status == zip_ok )
    {
    switch ( point )
      {
      case zip_figure_origin_point:
        figure_x_point = x;
        figure_y_point = y;
        break;
      case  zip_figure_auxiliary_point:
        figure_x_points(0) = x;
        figure_y_points(0) = y;
	break;
      case zip_figure_auxiliary_point + 1: /* Upper Right Corner */
        figure_x_points(0) = x;
	figure_y_point = y;
        break;
      case zip_figure_auxiliary_point + 2: /* Lower Left Corner */
        figure_x_point = x;
	figure_y_points(0) = y;
	break;
      case zip_figure_auxiliary_point + 3: /* 12 O'Clock */
	figure_y_points(0) -= y - figure_y_point;
	figure_y_point = y;
	break;
      case zip_figure_auxiliary_point + 4: /* 3 O'Clock */
	figure_x_point -= x - figure_x_points(0);
	figure_x_points(0) = x;
	break;
      case zip_figure_auxiliary_point + 5: /* 6 O'Clock */
	figure_y_point += figure_y_points(0) - y;
	figure_y_points(0) = y;
	break;
      case zip_figure_auxiliary_point + 6: /* 9 O'Clock */
	figure_x_points(0) += figure_x_point - x;
	figure_x_point = x;
	break;
      case zip_figure_auxiliary_point + 7: /* Center */
	ziporect_Adjust_Object_Point_Suite( self, figure,
	    x - (figure_x_point + (figure_x_points(0) - figure_x_point)/2),
	    y - (figure_y_point - (figure_y_point - figure_y_points(0))/2) );
	break;
      default: status = zip_failure; /*=== zip_invalid_point_type ===*/
      }
    if ( status == zip_ok )
      {
      zip_Set_Image_Extrema( Data, figure->zip_figure_image, x, y );
/*===handle both extrema 7/20/86===*/
/*===have extrema check for REDUCTIONS as well as EXPANSIONS 7/20/86===*/
      zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			      figure->zip_figure_image );
      }
    }
  OUT(ziporect__Set_Object_Point);
  return  status;
  }

long/*=== MUST RATIONALIZE POINTS VS HANDLES ===*/
ziporect__Object_Point( self, figure, point, x, y )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register long				  point, *x, *y;
  {
  register long				  status = zip_ok;

  *x = 0;
  *y = 0;
  switch( point )
    {
    case 1:  *x = figure_x_point; *y = figure_y_point;      break;
    default: status = zip_failure;
    }
  return  status;
  }

long
ziporect__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(ziporect__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  figure_x_points(0) += x_delta;
  figure_y_points(0) += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_points(0), figure_y_points(0) );
/*===have extrema check for REDUCTIONS as well as EXPANSIONS 7/20/86===*/
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(ziporect__Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3,
					 *Y1, *Y2, *Y3;
  {
  *X1 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point );
  *X2 = *X1 + (window_x_points(0) - window_x_point)/2;
  *X3 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_points(0) );
  *Y1 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point );
  *Y2 = *Y1 + (window_y_points(0) - window_y_point)/2;
  *Y3 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_points(0) );
  }


long
ziporect__Set_Object_Shade( self, figure, shade )
  register struct ziporect		 *self;
  register zip_type_figure		  figure;
  {
  IN(ziporect__Set_Object_Shade);
  figure->zip_figure_fill.zip_figure_shade = shade;
  if ( shade >= 1  &&  shade <= 100 )
    figure->zip_figure_mode.zip_figure_mode_shaded = on;
    else
    figure->zip_figure_mode.zip_figure_mode_shaded = off;
  OUT(ziporect__Set_Object_Shade);
  return  zip_ok;
  }

boolean
ziporect__Contains( self, figure, pane, x, y )
  register struct ziporect		*self;
  register zip_type_figure		figure;
  register zip_type_pane		pane;
  register zip_type_pixel		x, y;

  {
  register boolean			status = FALSE;
  register int				x1, y1, x2, y2;

  IN( ziporect__Contains )
  x1 = window_x_point;
  y1 = window_y_point;
  x2 = window_x_points(0);
  y2 = window_y_points(0);
  if ( figure->zip_figure_mode.zip_figure_mode_shaded )
  {
     if ( y1 <= y && y <= y2 && x1 <= x && x <= x2 )
       status = TRUE;
  }
  else if ((( x == x1 || x == x2 ) && y1 <= y && y <= y2 ) ||
     (( y == y1 || y == y2 ) && x1 <= x && x <= x2 ))
    status = TRUE;
  OUT( ziporect__Contains )
  return status;
  }
