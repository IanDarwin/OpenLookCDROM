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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipoarc.c,v 1.3 1992/12/15 21:57:55 rr2b R6tape $";
#endif

/* zipoarc.c	Zip Object -- Arc					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Arc

MODULE	zipoarc.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  11/18/88	Recognize Line-width (TCP)
   08/14/90	Use Ensure_Line_Attributes on Draw and Print (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include <math.h>
#include "zipobj.ih"
#include "zipoarc.eh"

/*LIBS: -lm
*/
static char two = 2; /* To quieten Compiler */
static Draw();
static Set_Points();
static Compute_Handle_Positions();

char
zipoarc__Object_Icon( self )
  register struct zipoarc		 *self;
  {
  IN(zipoarc__Object_Icon);
  OUT(zipoarc__Object_Icon);
  return  'L';
  }


char
zipoarc__Object_Icon_Cursor( self )
  register struct zipoarc		 *self;
  {
  IN(zipoarc__Object_Icon_Cursor);
  OUT(zipoarc__Object_Icon_Cursor);
  return  'P';
  }

char
zipoarc__Object_Datastream_Code( self )
  register struct zipoarc		 *self;
  {
  IN(zipoarc__Object_Datastream_Code);
  OUT(zipoarc__Object_Datastream_Code);
  return  'M';
  }

long
zipoarc__Show_Object_Properties( self, pane, figure )
  register struct zipoarc		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw Arc Clockwise: Noon to 3:00, 3:00 to 6:00, etc." );
  return  zip_ok;
  }

long
zipoarc__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipoarc		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  register zip_type_figure		  figure;
  register long				  position = 0;/*===*/
  register zip_type_point		  X_origin = 0, Y_origin = 0,
					  X_start, Y_start;

  IN(zipoarc__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
	zip_Create_Figure( Data, &pane->zip_pane_current_figure, NULL, zip_arc_figure,
			   pane->zip_pane_current_image, position )) == zip_ok )
        {
	figure = pane->zip_pane_current_figure;
        zipoarc_Set_Object_Point( self, figure,
		zip_figure_origin_point, X, Y ); /* Center-point */
	figure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
	pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	}
      break;
    case view_LeftUp:
      if ( figure = pane->zip_pane_current_figure )
        {
	if ( figure_x_points(two) == 0  ||  figure_y_points(two) == 0 )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
          break;
	  }
	}
      /* Fall-thru */
    case view_LeftMovement:
      if ( figure = pane->zip_pane_current_figure )
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	zipview_Draw_Figure( View, figure, pane );
	X_start = figure_x_points(0);  Y_start = figure_y_points(0);
	if ( X_start < X  &&  Y_start < Y )
	  { X_origin = X;       Y_origin = Y_start; }
	else
	if ( X_start < X  &&  Y_start > Y )
	  { X_origin = X_start; Y_origin = Y; }
	else
	if ( X_start > X  &&  Y_start > Y )
	  { X_origin = X;       Y_origin = Y_start; }
	else
	if ( X_start > X  &&  Y_start < Y )
	  { X_origin = X_start; Y_origin = Y; }
	/* Origin */	figure_x_point = X_origin;
			figure_y_point = Y_origin;
	/* End */	figure_x_points(1) = X;
			figure_y_points(1) = Y;
	/* Radii */	figure_x_points(two) = abs(X_start - X);
			figure_y_points(two) = abs(Y_start - Y);
	zipview_Draw_Figure( View, figure, pane );
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	}
      break;
    }
  OUT(zipoarc__Build_Object);
  return  status;
  }

long
zipoarc__Draw_Object( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoarc__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane );
  OUT(zipoarc__Draw_Object);
  return  status;
  }

long
zipoarc__Clear_Object( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoarc__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane );
  OUT(zipoarc__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register long				  x_radius, y_radius;
  register long				  left, top, width, height;
  register short			  start_angle, offset_angle;
  register double			  theta, angle, x, y;
  register unsigned char		  lwidth;

  IN(Draw); /*=== CLEAN UP THIS OLD MIGRATORY MESS... ===*/
  if ( zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
    {
    x_radius = abs( zipview_X_Point_To_Pixel( View, pane, figure,
     figure_x_point + figure_x_points(two) ) - window_x_point );
    y_radius = abs( zipview_Y_Point_To_Pixel( View, pane, figure,
     figure_y_point + figure_y_points(two) ) - window_y_point );
    if ( x_radius  &&  y_radius )
      {
      left =  window_x_point - x_radius;
      top  =  window_y_point - y_radius;
      width  = 2 * x_radius;    height = 2 * y_radius;
      x = window_x_points(0) - window_x_point;
      y = window_y_point - window_y_points(0);
      theta = atan2( y, x);
      start_angle = angle = -(360.0 * (theta / (2.0 * 3.14159))) + 90.0;
      x = window_x_points(1) - window_x_point;
      y = window_y_point - window_y_points(1);
      theta = atan2( y, x );
      offset_angle = (-(360.0 * (theta / (2.0 * 3.14159))) + 90.0) - angle;
      if ( offset_angle <= 0 )  offset_angle += 360;
      zipview_DrawArcSize( View, left, top, width, height, start_angle, offset_angle );
      zipview_MoveTo( View, window_x_points(1), window_y_points(1) );
      if ( ExposePoints )    zipoarc_Expose_Object_Points( self, figure, pane );
      if ( HighlightPoints ) zipoarc_Highlight_Object_Points( self, figure, pane );
      }
    }
  OUT(Draw);
  return  status;
  }

long
zipoarc__Print_Object( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  long x, y, xlen, ylen, x0, y0, x1, y1;

  IN(zipoarc__Print_Object);
  x = print_x_point;
  y = print_y_point;
  xlen = print_x_lengths(two);
  ylen = print_y_lengths(two);
  x0 = print_x_points(0);
  y0 = print_y_points(0);
  x1 = print_x_points(1);
  y1 = print_y_points(1);
  zipprint_Ensure_Line_Attributes( Print, figure );
  zipprint_Draw_Arc( Print, x, y, abs(xlen), abs(ylen), x0, y0, x1, y1);
  OUT(zipoarc__Print_Object);
  return  status;
  }

long
zipoarc__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3, XS, YS, XE, YE;

  IN(zipoarc__Proximate_Object_Points);
  Compute_Handle_Positions( self, figure, pane,
                	   &X1, &X2, &X3, &Y1, &Y2, &Y3, &XS, &YS, &XE, &YE );
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y2, x, y ))
    point = zip_figure_origin_point;	    /* 1 -- Origin */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, XS, YS, x, y ))
    point = zip_figure_auxiliary_point;	    /* 2 -- Start */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, XE, YE, x, y ))
    point = zip_figure_auxiliary_point + 1; /* 3 -- End */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y2, x, y ))
    point = zip_figure_auxiliary_point + 2; /* 4 -- 3 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y3, x, y ))
    point = zip_figure_auxiliary_point + 3; /* 5 -- 6 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y2, x, y ))
    point = zip_figure_auxiliary_point + 4; /* 6 -- 9 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y1, x, y ))
    point = zip_figure_auxiliary_point + 5; /* 7 -- 12 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y1, x, y ))
    point = zip_figure_auxiliary_point + 6; /* 8 -- Upper-Left */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y1, x, y ))
    point = zip_figure_auxiliary_point + 7; /* 9 -- Upper-Right */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y3, x, y ))
    point = zip_figure_auxiliary_point + 8; /* 10 -- Lower-Right */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y3, x, y ))
    point = zip_figure_auxiliary_point + 9; /* 11 -- Lower-Left */
  OUT(zipoarc__Proximate_Object_Points);
  return  point;
  }

boolean
zipoarc__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = false;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3, XS, YS, XE, YE;

  IN(zipoarc__Enclosed_Object);
  Compute_Handle_Positions( self, figure, pane,
                	   &X1, &X2, &X3, &Y1, &Y2, &Y3, &XS, &YS, &XE, &YE );
  if ( X1 > x  &&  Y1 > y  &&  X3 < (x + w)  &&  Y3 < (y + h) )
    enclosed = true;
  OUT(zipoarc__Enclosed_Object);
  return  enclosed;
  }

long
zipoarc__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3, XS, YS, XE, YE;

  IN(zipoarc__Object_Enclosure);
  Compute_Handle_Positions( self, figure, pane,
			    &X1, &X2, &X3, &Y1, &Y2, &Y3, &XS, &YS, &XE, &YE );
  *x = X1;  *y = Y1;  *w = abs(X3 - X1);  *h = abs(Y3 - Y1);
  OUT(zipoarc__Object_Enclosure);
  return  zip_ok;
  }

long
zipoarc__Highlight_Object_Points( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3, XS, YS, XE, YE;
  register long				  status = zip_ok;

  IN(zipoarc__Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane,
                	    &X1, &X2, &X3, &Y1, &Y2, &Y3, &XS, &YS, &XE, &YE );
  zipedit_Highlight_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  if ( XS != X1  &&  XS != X2  &&  XS != X3 )
    zipedit_Highlight_Point( Edit, pane, XS, YS );
  if ( XE != X1  &&  XE != X2  &&  XE != X3 )
    zipedit_Highlight_Point( Edit, pane, XE, YE );
  OUT(zipoarc__Highlight_Object_Points);
  return  status;
  }

long
zipoarc__Normalize_Object_Points( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3, XS, YS, XE, YE;
  register long				  status = zip_ok;

  IN(zipoarc__Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane,
                	   &X1, &X2, &X3, &Y1, &Y2, &Y3, &XS, &YS, &XE, &YE );
  zipedit_Normalize_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  if ( XS != X1  &&  XS != X2  &&  XS != X3 )
    zipedit_Normalize_Point( Edit, pane, XS, YS );
  if ( XE != X1  &&  XE != X2  &&  XE != X3 )
    zipedit_Normalize_Point( Edit, pane, XE, YE );
  OUT(zipoarc__Normalize_Object_Points);
  return  status;
  }

long
zipoarc__Expose_Object_Points( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoarc__Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  OUT(zipoarc__Expose_Object_Points);
  return  status;
  }

long
zipoarc__Hide_Object_Points( self, figure, pane )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoarc__Hide_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  OUT(zipoarc__Hide_Points);
  return  status;
  }

static
Set_Points( figure, x_center, y_center, x_radius, y_radius,
		xs_delta, ys_delta, xe_delta, ye_delta )
  register zip_type_figure		  figure;
  register float			  x_center, y_center, x_radius, y_radius,
					  xs_delta, ys_delta, xe_delta, ye_delta;
  {
  figure_x_point = x_center;
  figure_y_point = y_center;
  figure_x_points(0) = (x_center - x_radius) + xs_delta;
  figure_y_points(0) = (y_center + y_radius) - ys_delta;
  figure_x_points(1) = (x_center - x_radius) + xe_delta;
  figure_y_points(1) = (y_center + y_radius) - ye_delta;
  figure_x_points(two) = abs(x_radius);
  figure_y_points(two) = abs(y_radius);
  }

long
zipoarc__Set_Object_Point( self, figure, point, x, y )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;
  register float			  x_radius, y_radius, span,
					  X1, X2, X3, Y1, Y2, Y3,
					  XS, YS, XE, YE,
					  XS_delta, YS_delta, XE_delta, YE_delta;
  register double			  theta;

  IN(zipoarc__Set_Object_Point);
  if ( figure->zip_figure_points == NULL )
    if ( (status = zip_Allocate_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_ok )
      if ( (status = zip_Enlarge_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_ok  &&
	   (status = zip_Enlarge_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_ok )
        {
	figure->zip_figure_points->zip_points_count = 3;
        figure_x_point = x;	/* Center  (Origin) */
        figure_y_point = y;
        figure_x_points(0) = x;	/* Start */
        figure_y_points(0) = y;
        figure_x_points(1) = x;	/* End */
        figure_y_points(1) = y;
        figure_x_points(two) = figure_y_points(two) = 0;	/* Radii */
	}
  x_radius = abs( x - figure_x_point );
  y_radius = abs( y - figure_y_point );
  X2 = figure_x_point;
  X1 = X2 - figure_x_points(two);
  X3 = X2 + figure_x_points(two);
  Y2 = figure_y_point;
  Y1 = Y2 + figure_y_points(two);
  Y3 = Y2 - figure_y_points(two);
  XS = figure_x_points(0);
  YS = figure_y_points(0);
  XE = figure_x_points(1);
  YE = figure_y_points(1);
  if ( (span = X3 - X1) == 0 )  span = 1;
  XS_delta = ((XS - X1)/span) * (2*x_radius);
  XE_delta = ((XE - X1)/span) * (2*x_radius);
  if ( (span = Y3 - Y1) == 0 )  span = 1;
  YS_delta = ((YS - Y1)/span) * (2*y_radius);
  YE_delta = ((YE - Y1)/span) * (2*y_radius);
  if ( status == zip_ok  &&  x_radius  &&  y_radius )
    {
    switch ( point )
      {
      case zip_figure_origin_point:	    /* 1 -- Center X/Y */
	zipoarc_Adjust_Object_Point_Suite( self, figure,
		x - figure_x_point, y - figure_y_point );
        break;
      case zip_figure_auxiliary_point:	    /* 2 -- Start X/Y */
	theta = atan2( 1.0 * (y - Y2), 1.0 * (x - X2) );
	figure_x_points(0) = X2 + (figure_x_points(two) * cos( theta ));
	figure_y_points(0) = Y2 + (figure_y_points(two) * sin( theta ));
	break;
      case zip_figure_auxiliary_point + 1:  /* 3 -- End X/Y */
	theta = atan2( 1.0 * (y - Y2), 1.0 * (x - X2) );
	figure_x_points(1) = X2 + (figure_x_points(two) * cos( theta ));
	figure_y_points(1) = Y2 + (figure_y_points(two) * sin( theta ));
	break;
      case zip_figure_auxiliary_point + 2:  /* 4 --  3 O'Clock */
      case zip_figure_auxiliary_point + 4:  /* 6 --  9 O'Clock */
	figure_x_points(0) = (X2 - x_radius) + XS_delta;
	figure_x_points(1) = (X2 - x_radius) + XE_delta;
	figure_x_points(two) = x_radius;
	break;
      case zip_figure_auxiliary_point + 3:  /* 5 --  6 O'Clock */
      case zip_figure_auxiliary_point + 5:  /* 7 -- 12 O'Clock */
	figure_y_points(0) = (Y2 + y_radius) - YS_delta;
	figure_y_points(1) = (Y2 + y_radius) - YE_delta;
	figure_y_points(two) = y_radius;
	break;
      case zip_figure_auxiliary_point + 6:  /* 8  -- Upper-Left  */
	Set_Points( figure, X3 - x_radius, Y3 + y_radius,
		x_radius, y_radius, XS_delta, YS_delta,	XE_delta, YE_delta );
	break;
      case zip_figure_auxiliary_point + 7:  /* 9  -- Upper-Right */
	Set_Points( figure, X1 + x_radius, Y3 + y_radius,
		x_radius, y_radius, XS_delta, YS_delta,	XE_delta, YE_delta );
	break;
      case zip_figure_auxiliary_point + 8:  /* 10 -- Lower-Right */
	Set_Points( figure, X1 + x_radius, Y1 - y_radius,
		x_radius, y_radius, XS_delta, YS_delta,	XE_delta, YE_delta );
	break;
      case zip_figure_auxiliary_point + 9:  /* 11 -- Lower-Left  */
	Set_Points( figure, X3 - x_radius, Y1 - y_radius,
		x_radius, y_radius, XS_delta, YS_delta,	XE_delta, YE_delta );
	break;
      default:
        status = zip_failure; /*=== zip_invalid_point_type ===*/
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
  OUT(zipoarc__Set_Object_Point);
  return  status;
  }

long
zipoarc__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipoarc__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  figure_x_points(0) += x_delta;
  figure_y_points(0) += y_delta;
  figure_x_points(1) += x_delta;
  figure_y_points(1) += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipoarc__Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, Y1, Y2, Y3, XS, YS, XE, YE )
  register struct zipoarc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3, *Y1, *Y2, *Y3,
					 *XS, *YS, *XE, *YE;
  {
  *X1 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point - figure_x_points(two) );
  *X2 = window_x_point;
  *X3 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point + figure_x_points(two) );
  *Y1 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point + figure_y_points(two) );
  *Y2 = window_y_point;
  *Y3 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point - figure_y_points(two) );
  *XS = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_points(0) );
  *YS = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_points(0) );
  *XE = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_points(1) );
  *YE = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_points(1) );
  }
