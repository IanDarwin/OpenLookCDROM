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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipocirc.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipocirc.c	Zip Object -- Circle					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipocirc.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:58:01  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:57:55  rr2b
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
 * Revision 2.12  1991/09/12  16:42:42  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.11  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.10  1990/08/21  14:28:05  sg08
 * Add Ensure_Attributes usage. Set both x and y radii in Set_Object_Point
 *
 * Revision 2.9  90/05/09  16:03:13  gk5g
 * Changes in support of i386_mach.
 * 
 * Revision 2.8  89/09/08  17:41:44  ghoti
 * removal of unused variables
 * 
 * Revision 2.7  89/08/30  16:25:15  sg08
 * Removed excess SetTransferMode activity. Accomodate non-refresh on builds.
 * 
 * Revision 2.6  89/05/01  22:15:05  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.5  89/02/08  16:50:19  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  19:34:39  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/18  21:09:51  tom
 * Handle variable line-widths.
 * 
 * Revision 2.2  88/10/11  20:29:02  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:15:00  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:36:31  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:44:58  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Circle

MODULE	zipocirc.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)
   08/24/89	Remove excess SetTransferMode() activity in Draw() (SCG)
   08/25/89	Modify Build to handle non-refresh of pane on build completion (SCG)
     08/16/90	Add Ensure_Attributes usage. Set both x and y radii in Set_Object_Point (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <view.ih>
#include <zipobj.ih>
#include <zipocirc.eh>

/*LIBS: -lm
*/

static Draw();
static Compute_Handle_Positions();

char
zipocirc__Object_Icon( self )
  register struct zipocirc		 *self;
  {
  IN(zipocirc__Object_Icon);
  OUT(zipocirc__Object_Icon);
  return  'J';
  }

char
zipocirc__Object_Icon_Cursor( self )
  register struct zipocirc		 *self;
  {
  IN(zipocirc__Object_Icon_Cursor);
  OUT(zipocirc__Object_Icon_Cursor);
  return  'G';
  }

char
zipocirc__Object_Datastream_Code( self )
  register struct zipocirc		 *self;
  {
  IN(zipocirc__Object_Datastream_Code);
  OUT(zipocirc__Object_Datastream_Code);
  return  'J';
  }

long
zipocirc__Show_Object_Properties( self, pane, figure )
  register struct zipocirc		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw Circle from Center outward." );
  return  zip_ok;
  }

long
zipocirc__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipocirc		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  register long				  radial_point = 0, position = 0; /*===*/
  register zip_type_figure		  figure;

  IN(zipocirc__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
	zip_Create_Figure( Data, &CurrentFigure, NULL, zip_circle_figure,
			   CurrentImage, position )) == zip_ok )
        {
        zipocirc_Set_Object_Point( self, CurrentFigure,	zip_figure_origin_point, X, Y );
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
	if ( figure_x_points(0) == 0 )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
	  }
          else
          {
          zipview_Draw_Figure( View, figure, pane );
	  }
	}
	break;
    case view_LeftMovement:
      if ( figure = CurrentFigure )
	{
	zipview_Draw_Figure( View, figure, pane );
        if ( abs( X - figure_x_point ) < abs( Y - figure_y_point ) )
	  radial_point = figure_x_point + abs( Y - figure_y_point );
	  else
	  radial_point = X;
        zipocirc_Set_Object_Point( self, figure,
		zip_figure_auxiliary_point, radial_point, 0 );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	}
      break;
    }
  OUT(zipocirc__Build_Object);
  return  status;
  }

long
zipocirc__Read_Object( self, figure )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status = zip_ok;
  IN(zipocirc__Read_Object);
  status = zip_Read_Figure( Data, figure );
  zip_Set_Image_Extrema( Data, figure->zip_figure_image,
			 figure_x_point - abs(figure_x_points(0)),
			 figure_y_point + abs(figure_x_points(0)) );
  zip_Set_Image_Extrema( Data, figure->zip_figure_image,
			 figure_x_point + abs(figure_x_points(0)),
			 figure_y_point - abs(figure_x_points(0)) );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			  figure->zip_figure_image );
  OUT(zipocirc__Read_Object);
  return  status;
  }

long
zipocirc__Draw_Object( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocirc__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane, zip_draw );
  OUT(zipocirc__Draw_Object);
  return  status;
  }

long
zipocirc__Clear_Object( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocirc__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane, zip_clear );
  OUT(zipocirc__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane, action )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register long				  action;
  {
  register long				  status = zip_ok;
  register int				  side, radius;
  register unsigned char		  shade;

  IN(Draw);
  side = zipview_X_Point_To_Pixel( View, pane, figure,
				   figure_x_point + figure_x_points(0) );
  radius = abs( side - window_x_point );
  if ( figure->zip_figure_mode.zip_figure_mode_shaded )
    { DEBUGdt(Shade,figure->zip_figure_fill.zip_figure_shade);
    if ( View->mouse_action != view_LeftMovement  &&  action == zip_draw )
      {
      /* Shade of '0' means Transparent --- Shade of '1' means White */
      if ( (shade = figure->zip_figure_fill.zip_figure_shade) >= 1  &&
	    shade <= 100 )
        {
        if ( (shade = ('0' + ((shade + 10) / 10)) - 1) > '9' )  shade = '9';
        DEBUGdt(Shade-index,shade);
#if (!defined(MACH) || !defined(i386))
	zipview_Ensure_Fill_Attributes( View, figure );
        zipview_FillOvalSize( View, window_x_point - radius, window_y_point - radius,
			    (radius << 1)+1, (radius << 1)+1,
			    zipview_Define_Graphic( View,zip_Define_Font(
				    Data, ShadeFontName, NULL ), shade ) );
#endif
        }
      }
      else
      if ( action == zip_clear )
	{
#if (!defined(MACH) || !defined(i386))
        zipview_FillOvalSize( View, window_x_point - radius, window_y_point - radius,
			      (radius << 1)+1, (radius << 1)+1, graphic_WHITE );
#endif
	}
    }
  if ( 	zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
    zipview_DrawOvalSize( View, window_x_point - radius, window_y_point - radius,
			radius << 1, radius << 1 );
  if ( ExposePoints )	    zipocirc_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )    zipocirc_Highlight_Object_Points( self, figure, pane );
  OUT(Draw);
  return  status;
  }

long
zipocirc__Print_Object( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocirc__Print_Object);
  zipprint_Set_Shade( Print, figure->zip_figure_fill.zip_figure_shade );
  zipprint_Ensure_Line_Attributes( Print, figure );
  zipprint_Draw_Circle( Print, print_x_point, print_y_point, print_x_lengths(0) );
  OUT(zipocirc__Print_Object);
  return  status;
  }

long
zipocirc__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipocirc__Proximate_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y2, x, y ) )
    point = zip_figure_origin_point;	    /* Origin */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y2, x, y ) )
    point = zip_figure_auxiliary_point;     /* 3 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y3, x, y ) )
    point = zip_figure_auxiliary_point + 1; /* 6 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y2, x, y ) )
    point = zip_figure_auxiliary_point + 2; /* 9 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y1, x, y ) )
    point = zip_figure_auxiliary_point + 3; /* 12 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y1, x, y ) )
    point = zip_figure_auxiliary_point + 4; /* Upper-Left */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y1, x, y ) )
    point = zip_figure_auxiliary_point + 5; /* Upper-Right */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y3, x, y ) )
    point = zip_figure_auxiliary_point + 6; /* Lower-Right */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y3, x, y ) )
    point = zip_figure_auxiliary_point + 7; /* Lower-Left */

  OUT(zipocirc__Proximate_Object_Points);
  return  point;
  }

boolean
zipocirc__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = false;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipocirc__Enclosed_Object);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( X1 > x  &&  Y1 > y  &&  X3 < (x + w)  &&  Y3 < (y + h) )
    enclosed = true;
  OUT(zipocirc__Enclosed_Object);
  return  enclosed;
  }

long
zipocirc__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipocirc__Object_Enclosure);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  *x = X1;  *y = Y1;  *w = abs(X3 - X1);  *h = abs(Y3 - Y1);
  OUT(zipocirc__Object_Enclosure);
  return  zip_ok;
  }

long
zipocirc__Highlight_Object_Points( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipocirc__Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Highlight_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipocirc__Highlight_Object_Points);
  return  status;
  }

long
zipocirc__Normalize_Object_Points( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipocirc__Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Normalize_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipocirc__Normalize_Object_Points);
  return  status;
  }

long
zipocirc__Expose_Object_Points( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocirc__Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point + figure_x_points(0), figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point + figure_x_points(0) );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point - figure_x_points(0), figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point - figure_x_points(0) );

  OUT(zipocirc__Expose_Object_Points);
  return  status;
  }

long
zipocirc__Hide_Object_Points( self, figure, pane )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocirc__Hide_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point + figure_x_points(0), figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point + figure_x_points(0) );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point - figure_x_points(0), figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point - figure_x_points(0) );
  OUT(zipocirc__Hide_Points);
  return  status;
  }

long
zipocirc__Set_Object_Point( self, figure, point, x, y )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;
  register zip_type_point		  x_radius, y_radius, radius,
					  X1, X2, X3, R,
					  Y1, Y2, Y3;

  IN(zipocirc__Set_Object_Point);
  if ( figure->zip_figure_points == NULL  &&
       (status = zip_Allocate_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_ok )
    {
    figure->zip_figure_points->zip_points_count = 1;
    figure_x_point = x;
    figure_y_point = y;
    figure_x_points(0) = figure_y_points(0) = 0;
    }
  x_radius = abs( x - figure_x_point );
  y_radius = abs( y - figure_y_point );
  radius = (x_radius > y_radius) ? x_radius : y_radius;
  R  = figure_x_points(0);
  X2 = figure_x_point;
  X1 = X2 - R;
  X3 = X2 + R;
  Y2 = figure_y_point;
  Y1 = Y2 + R;
  Y3 = Y2 - R;
  figure_x_points(0) = radius;
  if ( status == zip_ok )
    switch ( point )
      {
      case zip_figure_origin_point:
        figure_x_point = x;
        figure_y_point = y;
	figure_x_points(0) = figure_y_points(0) = R;
	break;
      case zip_figure_auxiliary_point:	    /*  3 O'Clock */
      case zip_figure_auxiliary_point + 2:  /*  9 O'Clock */
	figure_x_points(0) = figure_y_points(0) = x_radius;
	break;
      case zip_figure_auxiliary_point + 1:  /*  6 O'Clock */
      case zip_figure_auxiliary_point + 3:  /* 12 O'Clock */
	figure_x_points(0) = figure_y_points(0) = y_radius;
	break;
      case zip_figure_auxiliary_point + 4:  /* Upper-Left */
	figure_x_point = X3 - radius;
	figure_y_point = Y3 + radius;
	break;
      case zip_figure_auxiliary_point + 5:  /* Upper-Right */
	figure_x_point = X1 + radius;
	figure_y_point = Y3 + radius;
	break;
      case zip_figure_auxiliary_point + 6:  /* Lower-Right */
	figure_x_point = X1 + radius;
	figure_y_point = Y1 - radius;
	break;
      case zip_figure_auxiliary_point + 7:  /* Lower-Left */
	figure_x_point = X3 - radius;
	figure_y_point = Y1 - radius;
	break;
      default:
	status = zip_failure; /*=== zip_invalid_point_position ===*/
      }
/*=== SET EXTREMA === */
  OUT(zipocirc__Set_Object_Point);
  return  status;
  }

long
zipocirc__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipocirc__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipocirc__Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3, *Y1, *Y2, *Y3;
  {
  *X1 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point - figure_x_points(0) );
  *X2 = window_x_point;
  *X3 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point + figure_x_points(0) );
  *Y1 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point + figure_x_points(0) );
  *Y2 = window_y_point;
  *Y3 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point - figure_x_points(0) );
  }


long
zipocirc__Set_Object_Shade( self, figure, shade )
  register struct zipocirc		 *self;
  register zip_type_figure		  figure;
  {
  IN(zipocirc__Set_Object_Shade);
  figure->zip_figure_fill.zip_figure_shade = shade;
  if ( shade >= 1  &&  shade <= 100 )
    figure->zip_figure_mode.zip_figure_mode_shaded = on;
    else
    figure->zip_figure_mode.zip_figure_mode_shaded = off;
  OUT(zipocirc__Set_Object_Shade);
  return  zip_ok;
  }

boolean
zipocirc__Contains( self, figure, pane, x, y )
  register struct zipocirc		*self;
  register zip_type_figure		figure;
  register zip_type_pane		pane;
  register zip_type_pixel		x, y;

  {
  register boolean			status = FALSE;
  register long				x_radius, y_radius, x_origin, y_origin, result, tolerance;

  IN( zipocirc__Contains )
  x_origin = window_x_point;
  y_origin = window_y_point;
  x_radius = abs( zipview_X_Point_To_Pixel( View, pane, figure,
            	              figure_x_point + figure_x_points(0)) - window_x_point );
  y_radius = abs( zipview_Y_Point_To_Pixel( View, pane, figure,
            	              figure_y_point + figure_y_points(0)) - window_y_point );
  result = ( square( y_radius ) * square( x-x_origin ))
	   + ( square( x_radius ) * square( y-y_origin ))
           - ( square( y_radius ) * square( x_radius ));
  if ( figure->zip_figure_mode.zip_figure_mode_shaded && result <= 0 )
     status = TRUE;
  else if ( result == 0 ) status = TRUE;
  OUT( zipocirc__Contains )
  return status;
  }
