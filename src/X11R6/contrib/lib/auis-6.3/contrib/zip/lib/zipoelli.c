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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipoelli.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipoelli.c	Zip Object -- Ellipse					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipoelli.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:58:20  rr2b
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
 * Revision 2.12  1992/09/10  00:52:12  gk5g
 * added #include <view.ih> ... a new view methods isn't being found for these subclasses of view!?!?!
 * .
 *
 * Revision 2.11  1991/09/12  16:42:49  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.10  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.9  1990/08/21  14:29:43  sg08
 * Add Ensure_Attribute usage on Draw and Print
 *
 * Revision 2.8  89/09/08  17:41:49  ghoti
 * removal of unused variables
 * 
 * Revision 2.7  89/08/30  16:26:11  sg08
 * Removed excess SetTransferMode activity. Accomodate non-refresh on builds.
 * 
 * Revision 2.6  89/05/01  22:14:55  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.5  89/02/08  16:50:31  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  19:38:45  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/18  21:10:19  tom
 * Handle variable line-widths.
 * 
 * Revision 2.2  88/10/11  20:33:06  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:15:24  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:37:24  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:45:05  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Ellipse

MODULE	zipoelli.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)
  08/24/89	Remove excess SetTransferMode() activity in Draw() (SCG)
  08/25/89	Modify Build to handle non-refresh of pane on build completion (SCG)
   08/16/90	Add Ensure_Attribute usage on Draw and Print (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <view.ih>
#include <zipobj.ih>
#include <zipoelli.eh>

/*LIBS: -lm
*/

static Draw();
static Compute_Handle_Positions();

char
zipoelli__Object_Icon( self )
  register struct zipoelli		 *self;
  {
  IN(zipoelli__Object_Icon);
  OUT(zipoelli__Object_Icon);
  return  'K';
  }

char
zipoelli__Object_Icon_Cursor( self )
  register struct zipoelli		 *self;
  {
  IN(zipoelli__Object_Icon_Cursor);
  OUT(zipoelli__Object_Icon_Cursor);
  return  'H';
  }

char
zipoelli__Object_Datastream_Code( self )
  register struct zipoelli		 *self;
  {
  IN(zipoelli__Object_Datastream_Code);
  OUT(zipoelli__Object_Datastream_Code);
  return  'L';
  }

long
zipoelli__Show_Object_Properties( self, pane, figure )
  register struct zipoelli		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw Ellipse from Center outward." );
  return  zip_ok;
  }

long
zipoelli__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipoelli		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  register long				  position = 0; /*===*/
  register zip_type_figure		  figure;

  IN(zipoelli__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
	zip_Create_Figure( Data, &pane->zip_pane_current_figure, NULL, zip_ellipse_figure,
			   pane->zip_pane_current_image, position )) == zip_ok )
        {
        zipoelli_Set_Object_Point( self, pane->zip_pane_current_figure,
		zip_figure_origin_point, X, Y );
	pane->zip_pane_current_figure->zip_figure_zoom_level =
	    pane->zip_pane_zoom_level;
	pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	zip_Set_Figure_Shade( Data, CurrentFigure,
			      pane->zip_pane_edit->zip_pane_edit_current_shade );
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	}
      break;
    case view_LeftUp:
      if ( figure = pane->zip_pane_current_figure )
        {
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	if ( figure_x_points(0) == 0  ||  figure_y_points(0) == 0 )
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
      if ( figure = pane->zip_pane_current_figure )
	{
	zipview_Draw_Figure( View, figure, pane );
	if ( X == figure->zip_figure_point.zip_point_x )
	  X += (Y - figure_y_point)/2;
        zipoelli_Set_Object_Point( self, figure, zip_figure_auxiliary_point, X, 0 );
	if ( Y == figure_y_point )
	  Y += (X - figure_x_point)/2;
        zipoelli_Set_Object_Point( self, figure, zip_figure_auxiliary_point + 1, 0, Y );
	zipview_Draw_Figure( View, figure, pane );
	}
      break;
    }
  OUT(zipoelli__Build_Object);
  return  status;
  }

long
zipoelli__Read_Object( self, figure )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Read_Object);
  status = zip_Read_Figure( Data, figure );
  zip_Set_Image_Extrema( Data, figure->zip_figure_image,
			 figure_x_point - abs(figure_x_points(0)),
			 figure_y_point + abs(figure_y_points(0)));
  zip_Set_Image_Extrema( Data, figure->zip_figure_image,
			 figure_x_point + abs(figure_x_points(0)),
			 figure_y_point - abs(figure_y_points(0)));
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			  figure->zip_figure_image );
  OUT(zipoelli__Read_Object);
  return  status;
  }

long
zipoelli__Write_Object( self, figure )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Write_Object);
  status = zip_Write_Figure( Data, figure );
  OUT(zipoelli__Write_Object);
  return  status;
  }

long
zipoelli__Draw_Object( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane, zip_draw );
  OUT(zipoelli__Draw_Object);
  return  status;
  }

long
zipoelli__Clear_Object( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane, zip_clear );
  OUT(zipoelli__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane, action )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register long				  action;
  {
  register long				  status = zip_ok;
  register int				  x_start_end, minor_radius, major_radius;
  register unsigned char		  shade;

  IN(Draw);
  x_start_end = zipview_X_Point_To_Pixel( View, pane, figure,
                                          figure_x_point + figure_x_points(0) );
  minor_radius = abs( x_start_end - window_x_point );
  major_radius = abs( zipview_Y_Point_To_Pixel( View, pane, figure,
            	              figure_y_point + figure_y_points(0)) - window_y_point );
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
	zipview_Ensure_Fill_Attributes( View, figure );
	zipview_FillOvalSize( View, window_x_point - minor_radius, window_y_point - major_radius,
				(minor_radius << 1)+1, (major_radius << 1)+1,
				zipview_Define_Graphic( View,zip_Define_Font(
				    Data, ShadeFontName, NULL ), shade ) );
	}
      }
      else
      if ( action == zip_clear )
	{
	zipview_FillOvalSize( View, window_x_point - minor_radius, window_y_point - major_radius,
			      (minor_radius << 1)+1, (major_radius << 1)+1, graphic_WHITE );
	}
    }
  if ( zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
    zipview_DrawOvalSize( View, window_x_point - minor_radius, window_y_point - major_radius, minor_radius << 1, major_radius << 1 );
  if ( ExposePoints )	    zipoelli_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )    zipoelli_Highlight_Object_Points( self, figure, pane );
  OUT(Draw);
  return  status;
  }

long
zipoelli__Print_Object( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Print_Object);
  zipprint_Set_Shade( Print, figure->zip_figure_fill.zip_figure_shade ); 
  zipprint_Ensure_Line_Attributes( Print, figure );
  zipprint_Draw_Ellipse( Print, print_x_point, print_y_point,
			print_x_lengths(0), print_y_lengths(0) );
  OUT(zipoelli__Print_Object);
  return  status;
  }

long
zipoelli__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipoelli__Proximate_Object_Points);
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

  OUT(zipoelli__Proximate_Object_Points);
  return  point;
  }

boolean
zipoelli__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = false;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipoelli__Enclosed_Object);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( X1 > x  &&  Y1 > y  &&  X3 < (x + w)  &&  Y3 < (y + h) )
    enclosed = true;
  OUT(zipoelli__Enclosed_Object);
  return  enclosed;
  }

long
zipoelli__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipoelli__Object_Enclosure);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  *x = X1;  *y = Y1;  *w = abs(X3 - X1);  *h = abs(Y3 - Y1);
  OUT(zipoelli__Object_Enclosure);
  return  zip_ok;
  }

long
zipoelli__Highlight_Object_Points( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipoelli__Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Highlight_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipoelli__Highlight_Object_Points);
  return  status;
  }

long
zipoelli__Normalize_Object_Points( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipoelli__Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Normalize_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipoelli__Normalize_Object_Points);
  return  status;
  }

long
zipoelli__Expose_Object_Points( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point + figure_x_points(0), figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point + figure_x_points(0) );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point - figure_x_points(0), figure_y_point );
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point - figure_x_points(0) );

  OUT(zipoelli__Expose_Object_Points);
  return  status;
  }

long
zipoelli__Hide_Object_Points( self, figure, pane )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Hide_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point + figure_x_points(0), figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point + figure_x_points(0) );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point - figure_x_points(0), figure_y_point );
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point - figure_x_points(0) );
  OUT(zipoelli__Hide_Points);
  return  status;
  }

long
zipoelli__Set_Object_Point( self, figure, point, x, y )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;
  register zip_type_point		  x_radius, y_radius, Rx, Ry,
					  X1, X2, X3,
					  Y1, Y2, Y3;

  IN(zipoelli__Set_Object_Point);
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
  Rx = figure_x_points(0);
  Ry = figure_y_points(0);
  X2 = figure_x_point;
  X1 = X2 - Rx;
  X3 = X2 + Rx;
  Y2 = figure_y_point;
  Y1 = Y2 + Ry;
  Y3 = Y2 - Ry;
  if ( status == zip_ok )
    switch ( point )
      {
      case zip_figure_origin_point:
        figure_x_point = x;
        figure_y_point = y;
	break;
      case zip_figure_auxiliary_point:	    /*  3 O'Clock */
      case zip_figure_auxiliary_point + 2:  /*  9 O'Clock */
	figure_x_points(0) = x_radius;
	break;
      case zip_figure_auxiliary_point + 1:  /*  6 O'Clock */
      case zip_figure_auxiliary_point + 3:  /* 12 O'Clock */
	figure_y_points(0) = y_radius;
	break;
      case zip_figure_auxiliary_point + 4:  /* Upper-Left */
	figure_x_point = X3 - x_radius;
	figure_y_point = Y3 + y_radius;
	figure_x_points(0) = x_radius;
	figure_y_points(0) = y_radius;
        break;
      case zip_figure_auxiliary_point + 5:  /* Upper-Right */
	figure_x_point = X1 + x_radius;
	figure_y_point = Y3 + y_radius;
	figure_x_points(0) = x_radius;
	figure_y_points(0) = y_radius;
	break;
      case zip_figure_auxiliary_point + 6:  /* Lower-Right */
	figure_x_point = X1 + x_radius;
	figure_y_point = Y1 - y_radius;
	figure_x_points(0) = x_radius;
	figure_y_points(0) = y_radius;
	break;
      case zip_figure_auxiliary_point + 7:  /* Lower-Left */
	figure_x_point = X3 - x_radius;
	figure_y_point = Y1 - y_radius;
	figure_x_points(0) = x_radius;
	figure_y_points(0) = y_radius;
	break;
      default:
	status = zip_failure; /*=== zip_invalid_point_position ===*/
      }
/*=== SET EXTREMA === */

  OUT(zipoelli__Set_Object_Point);
  return  status;
  }

long
zipoelli__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipoelli__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipoelli__Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct zipoelli		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3, *Y1, *Y2, *Y3;
  {
  *X1 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point - figure_x_points(0) );
  *X2 = window_x_point;
  *X3 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point + figure_x_points(0) );
  *Y1 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point + figure_y_points(0) );
  *Y2 = window_y_point;
  *Y3 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point - figure_y_points(0) );
  }
