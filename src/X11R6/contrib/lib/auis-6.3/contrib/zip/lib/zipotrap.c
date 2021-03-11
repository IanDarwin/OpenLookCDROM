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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipotrap.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipotrap.c	Zip Object -- Trapezoid					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipotrap.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:01:32  rr2b
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
 * Revision 2.11  1992/09/10  00:52:12  gk5g
 * added #include <view.ih> ... a new view methods isn't being found for these subclasses of view!?!?!
 * .
 *
 * Revision 2.10  1991/09/12  16:44:06  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.9  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.8  1989/09/08  17:42:48  ghoti
 * removal of unused variables
 *
 * Revision 2.7  89/08/30  16:28:39  sg08
 * Removed excess SetTransferMode activity.
 * 
 * Revision 2.6  89/06/14  17:45:00  sg08
 * Made Draw conform to use Shading attribute (for the benefit of GAHM)
 * 
 * Revision 2.5  89/02/08  16:51:48  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  20:18:30  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/16  18:37:47  tom
 * Optimize Drawing sequence.
 * 
 * Revision 2.2  88/10/11  20:34:30  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:18:21  ghoti
 * adjusting rcs #
 * 
 * Revision 1.3  88/09/17  23:30:45  dba
 * simplified expressions too complex for the Vax compiler
 * 
 * Revision 1.2  88/09/15  17:45:47  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:46:50  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Trapezoid

MODULE	zipotrap.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

	    HANDLES

         X1****X2****X3		Y1
         *            *
        *              *
      X4        X5      X6	Y2
      *                  *
     *                    *
   X7************X8********X9   Y3


HISTORY
  04/13/88	Created (TC Peters)
  10/27/88	Optimize drawing times (TCP)
   06/14/89	Made Draw() use Shade attribute rather than pattern attribute,
                            plus symbolic font name (SCG)
   08/24/89	Remove excess SetTransferMode() activity in Draw() (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <view.ih>
#include <fontdesc.ih>
#include <zipobj.ih>
#include <zipotrap.eh>

static Draw();
static Compute_Handle_Positions();

char
zipotrap__Object_Icon( self )
  register struct zipotrap		 *self;
  {
  IN(zipotrap__Object_Icon);
  OUT(zipotrap__Object_Icon);
/*===  return  'P';*/
  return  NULL;
  }

char
zipotrap__Object_Icon_Cursor( self )
  register struct zipotrap		 *self;
  {
  IN(zipotrap__Object_Icon_Cursor);
  OUT(zipotrap__Object_Icon_Cursor);
/*===  return  'A';*/
  return  NULL;
  }

char
zipotrap__Object_Datastream_Code( self )
  register struct zipotrap		 *self;
  {
  IN(zipotrap__Object_Datastream_Code);
  OUT(zipotrap__Object_Datastream_Code);
  return  'F';
  }

long
zipotrap__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipotrap		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register int				  status = zip_success;
  register long				  position = 0; /*===*/
  static zip_type_point			  initial_Y;
  register zip_type_figure		  figure;

  IN(zipotrap__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
	zip_Create_Figure( Data, &CurrentFigure, NULL, zip_trapezoid_figure,
			   CurrentImage, position )) == zip_success )
        {
        zipotrap_Set_Object_Point( self, CurrentFigure,	zip_figure_origin_point, X, Y );
        zipotrap_Set_Object_Point( self, CurrentFigure,	zip_figure_auxiliary_point, X, Y );
	CurrentFigure->zip_figure_zoom_level =  pane->zip_pane_zoom_level;
	zip_Set_Figure_Pattern( Data, CurrentFigure, '5' /*===*/ );
	zip_Set_Figure_Font( Data, CurrentFigure, "shape10" /*===*/ );
	initial_Y = Y;
	}
      break;
    case view_LeftUp:
      if ( figure = CurrentFigure )
        {
	if ( figure_x_point == 0  ||  figure_y_point == 0 )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
	  break;
	  }
	}
      /* Fall-thru */
    case view_LeftMovement:
      if ( CurrentFigure )
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	zipview_Draw_Figure( View, CurrentFigure, pane );
        zipotrap_Set_Object_Point( self, CurrentFigure,
		zip_figure_auxiliary_point + 1, X, initial_Y );
        zipotrap_Set_Object_Point( self, CurrentFigure,
		zip_figure_auxiliary_point + 2, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	}
      break;
    }
  OUT(zipotrap__Build_Object);
  return  status;
  }

long
zipotrap__Draw_Object( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipotrap__Draw_Object);
/*===
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane );
===*/
  if ( figure->zip_figure_zoom_level <= pane->zip_pane_zoom_level )
    status = Draw( self, figure, pane );
  OUT(zipotrap__Draw_Object);
  return  status;
  }

long
zipotrap__Clear_Object( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipotrap__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane );
  OUT(zipotrap__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok, shade;
  register long				  i, L1, L2, X1, Y1, X2, Y2,
					  PaneLeft = zipview_Pane_Left( View, pane ),
					  PaneTop= zipview_Pane_Top( View, pane ),
					  PaneRight = zipview_Pane_Right( View, pane ),
					  PaneBottom = zipview_Pane_Bottom( View, pane );
  register struct graphic		 *graphic;
  register float			  SM = pane->zip_pane_stretch_zoom_multiplier,
					  SD = pane->zip_pane_stretch_divisor;

  IN(Draw);
  if ( shade = zip_Contextual_Figure_Shade( Data, figure ))
    {
    if ( (shade = ('0' + ((shade + 10) / 10)) - 1) > '9' )  shade = '9';
    graphic = zipview_Define_Graphic( View, zip_Define_Font( Data, ShadeFontName, NULL ), shade );    X1 = window_x_points(0);
    X2 = window_x_points(1);
    Y1 = window_y_points(0);
    Y2 = window_y_points(1);
    L1 = figure->zip_figure_point.zip_point_x * SM / SD;
    L2 = figure->zip_figure_point.zip_point_y * SM / SD;
    zipview_FillTrapezoid( View, X1, Y1, L1, X2, Y2, L2, graphic );
    for ( i = 2; i < figure->zip_figure_points->zip_points_count; i += 3  )
      {
      X1 = window_x_points(i + 1);
      X2 = window_x_points(i + 2);
      if ( X1 > PaneRight  &&  X2 > PaneRight )
        continue;
      Y1 = window_y_points(i + 1);
      Y2 = window_y_points(i + 2);
      if ( Y1 > PaneBottom ||  Y2 < PaneTop )
        continue;
      L1 = figure_x_points(i) * SM / SD;
      L2 = figure_y_points(i) * SM / SD;
      if ( (X1 + L1) < PaneLeft  &&  (X2 + L2) < PaneLeft )
        continue;
      zipview_FillTrapezoid( View, X1, Y1, L1, X2, Y2, L2, graphic );
      }
    if ( ExposePoints )
      zipotrap_Expose_Object_Points( self, figure, pane );
    if ( HighlightPoints )
      zipotrap_Highlight_Object_Points( self, figure, pane );
    }
  OUT(Draw);
  return  status;
  }

long
zipotrap__Print_Object( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  char					  pattern = NULL;
  int					  i;

  IN(zipotrap__Print_Object);
  if ( pattern = zip_Contextual_Figure_Pattern( Data, figure ) )
  {
      long x1,y1,x2,y2,xlen,ylen;

      x1 = print_x_points(0);
      y1 = print_y_points(0);
      x2 = print_x_points(1);
      y2 = print_y_points(1);
      xlen = print_x_length;
      ylen = print_y_length;

    zipprint_Fill_Trapezoid( Print, x1, y1, x2, y2, xlen, ylen, pattern);

    for ( i = 2; i < figure->zip_figure_points->zip_points_count; i += 3  )  {
      x1 = print_x_points(i+1);
      y1 = print_y_points(i+1);
      x2 = print_x_points(i+2);
      y2 = print_y_points(i+2);
      xlen = print_x_lengths(i);
      ylen = print_y_lengths(i);

     zipprint_Fill_Trapezoid( Print, x1, y1, x2, y2, xlen, ylen, pattern);
    }
    }
    
  OUT(zipotrap__Print_Object);
  return  status;
  }

long
zipotrap__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, X4, X5, X6, X7, X8, X9, Y1, Y2, Y3;

  IN(zipotrap__Proximate_Object_Points);
  Compute_Handle_Positions( self, figure, pane,
    &X1, &X2, &X3, &X4, &X5, &X6, &X7, &X8, &X9, &Y1, &Y2, &Y3 );
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y1, x, y ))
    point = zip_figure_origin_point;		/* Upper Left Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X7, Y3, x, y ) )
    point = zip_figure_auxiliary_point;		/* Lower Left Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y1, x, y ))
    point = zip_figure_auxiliary_point + 1;	/* Upper Right Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X9, Y3, x, y ))
    point = zip_figure_auxiliary_point + 2;	/* Lower Right Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y1, x, y ))
    point = zip_figure_auxiliary_point + 3;	/* 12 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X6, Y2, x, y ))
    point = zip_figure_auxiliary_point + 4;	/* 3 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X8, Y3, x, y ))
    point = zip_figure_auxiliary_point + 5;	/* 6 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X4, Y2, x, y ))
    point = zip_figure_auxiliary_point + 6;	/* 9 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X5, Y2, x, y ))
    point = zip_figure_auxiliary_point + 7;	/* Center */
  OUT(zipotrap__Proximate_Object_Points);
  return  point;
  }

long
zipotrap__Highlight_Object_Points( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, X4, X5, X6, X7, X8, X9, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipotrap__Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane,
    &X1, &X2, &X3, &X4, &X5, &X6, &X7, &X8, &X9, &Y1, &Y2, &Y3 );
  zipedit_Highlight_Point( Edit, pane, X1, Y1 );
  zipedit_Highlight_Point( Edit, pane, X2, Y1 );
  zipedit_Highlight_Point( Edit, pane, X3, Y1 );
  zipedit_Highlight_Point( Edit, pane, X4, Y2 );
  zipedit_Highlight_Point( Edit, pane, X5, Y2 );
  zipedit_Highlight_Point( Edit, pane, X6, Y2 );
  zipedit_Highlight_Point( Edit, pane, X7, Y3 );
  zipedit_Highlight_Point( Edit, pane, X8, Y3 );
  zipedit_Highlight_Point( Edit, pane, X9, Y3 );
  OUT(zipotrap__Highlight_Object_Points);
  return  status;
  }

long
zipotrap__Normalize_Object_Points( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, X4, X5, X6, X7, X8, X9, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipotrap__Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane,
    &X1, &X2, &X3, &X4, &X5, &X6, &X7, &X8, &X9, &Y1, &Y2, &Y3 );
  zipedit_Normalize_Point( Edit, pane, X1, Y1 );
  zipedit_Normalize_Point( Edit, pane, X2, Y1 );
  zipedit_Normalize_Point( Edit, pane, X3, Y1 );
  zipedit_Normalize_Point( Edit, pane, X4, Y2 );
  zipedit_Normalize_Point( Edit, pane, X5, Y2 );
  zipedit_Normalize_Point( Edit, pane, X6, Y2 );
  zipedit_Normalize_Point( Edit, pane, X7, Y3 );
  zipedit_Normalize_Point( Edit, pane, X8, Y3 );
  zipedit_Normalize_Point( Edit, pane, X9, Y3 );
  OUT(zipotrap__Normalize_Object_Points);
  return  status;
  }

long
zipotrap__Expose_Object_Points( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipotrap__Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  OUT(zipotrap__Expose_Object_Points);
  return  status;
  }

long
zipotrap__Hide_Object_Points( self, figure, pane )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipotrap__Hide_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  OUT(zipotrap__Hide_Points);
  return  status;
  }

long
zipotrap__Set_Object_Point( self, figure, point, x, y )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;
  register long				  delta;

  IN(zipotrap__Set_Object_Point);
  if ( figure->zip_figure_points == NULL )
    if ( (status = zip_Allocate_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_success )
      if ( (status = zip_Enlarge_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_success )
	{
	figure->zip_figure_points->zip_points_count = 2;
	figure_x_point = 0;
	figure_y_point = 0;
	figure_x_points(0) = x;
	figure_y_points(0) = y;
	figure_x_points(1) = x;
	figure_y_points(1) = y;
	}
  if ( status == zip_success )
    {
    switch ( point )
      {
      case zip_figure_origin_point:		/* 1 => Upper Left */
	delta = x - figure_x_points(0);
	figure_x_point += abs( delta );
	figure_x_points(0) += delta;
	delta = y - figure_y_points(0);
	figure_y_points(0) += delta;
        break;
      case zip_figure_auxiliary_point:		/* 2 => Lower Left */
	delta = x - figure_x_points(1);
	figure_y_point += abs( delta );
	figure_x_points(1) += delta;
	delta = y - figure_y_points(1);
	figure_y_points(1) += delta;
	break;
      case zip_figure_auxiliary_point + 1:	/* 3 => Upper Right */
	delta = x - (figure_x_points(0) + figure_x_point );
	figure_x_point += abs( delta );
	delta = y - figure_y_points(0);
	figure_y_points(0) += delta;
        break;
      case zip_figure_auxiliary_point + 2:	/* 4 => Lower Right */
	delta = x - (figure_x_points(1) + figure_y_point );
	figure_y_point += abs( delta );
	delta = y - figure_y_points(1);
	figure_y_points(1) += delta;
	break;
      default:
	status = zip_failure;
      }
    if ( status == zip_success )
      {
      zip_Set_Image_Extrema( Data, figure->zip_figure_image,
			     figure->zip_figure_point.zip_point_x +
				       figure->zip_figure_points->zip_points[0].zip_point_x,
			     figure->zip_figure_points->zip_points[0].zip_point_y );
      zip_Set_Image_Extrema( Data, figure->zip_figure_image,
			     figure->zip_figure_point.zip_point_y +
				       figure->zip_figure_points->zip_points[1].zip_point_x,
			     figure->zip_figure_points->zip_points[1].zip_point_y );

      zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			      figure->zip_figure_image );
      }
    }
    if ( status == zip_success )
      {
      zip_Set_Image_Extrema( Data, figure->zip_figure_image, x, y );
/*===handle both extrema 7/20/86===*/
/*===have extrema check for REDUCTIONS as well as EXPANSIONS 7/20/86===*/
      zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			      figure->zip_figure_image );
      }
  OUT(zipotrap__Set_Object_Point);
  return  status;
  }

long
zipotrap__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipotrap__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  figure_x_points(0) += x_delta;
  figure_y_points(0) += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipotrap__Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, X4, X5, X6, X7, X8, X9, Y1, Y2, Y3 )
  register struct zipotrap		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3, *X4, *X5, *X6, *X7, *X8, *X9,
					 *Y1, *Y2, *Y3;
  {
  *X1 = window_x_points(0);
  *X3 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_points(0) + figure_x_point );
  *X2 = *X1 + ((*X3 - *X1) / 2);

  *X7 = window_x_points(1);
  *X9 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_points(1) + figure_y_point );
  *X8 = *X7 + ((*X9 - *X7) / 2);

  *X4 = *X7 + ((*X1 - *X7) / 2);
  *X6 = *X9 + ((*X3 - *X9) / 2);
  *X5 = *X4 + ((*X6 - *X4) / 2);

  *Y1 = window_y_points(0);
  *Y3 = window_y_points(1);
  *Y2 = *Y1 + ((*Y3 - *Y1) / 2);
  }
