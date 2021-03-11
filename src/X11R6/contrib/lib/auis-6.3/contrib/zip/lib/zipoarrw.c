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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipoarrw.c,v 1.3 1992/12/15 21:57:55 rr2b R6tape $";
#endif

/* zipoarrw.c	Zip Object -- Arrow					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Arrow

MODULE	zipoarrw.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  11/18/88	Recognize Line-width (TCP)
  05/01/89	Use symbolic font-names (TCP)
  05/16/89	Destroy zero-length arrows when built (TCP)
   08/14/90	Use Ensure_Line_Attributes on Draw and Print (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <math.h>
#include <view.ih>
#include <environ.ih>
#include <zipobj.ih>
#include <zipoarrw.eh>

static Draw();
static Draw_Basic_Style();
static Draw_Basic_Body();
static Feather_Points();


boolean
zipoarrow__InitializeObject( classID, self )
  register struct classheader	         *classID;
  register struct zipoarrow	         *self;
  {
  IN(zipoarrow_InitializeObject);
  self->tolerance = environ_GetProfileInt( "ZipCreateTolerance", 10 );
  DEBUGdt(Tolerance,self->tolerance);
  OUT(zipoarrow_InitializeObject);
  return  true;
  }

char
zipoarrow__Object_Icon( self )
  register struct zipoarrow		 *self;
  {
  IN(zipoarrow__Object_Icon);
  OUT(zipoarrow__Object_Icon);
  return  'O';
  }

char
zipoarrow__Object_Icon_Cursor( self )
  register struct zipoarrow		 *self;
  {
  IN(zipoarrow__Object_Icon_Cursor);
  OUT(zipoarrow__Object_Icon_Cursor);
  return  'c';
  }

char
zipoarrow__Object_Datastream_Code( self )
  register struct zipoarrow		 *self;
  {
  IN(zipoarrow__Object_Datastream_Code);
  OUT(zipoarrow__Object_Datastream_Code);
  return  'O';
  }

long
zipoarrow__Show_Object_Properties( self, pane, figure )
  register struct zipoarrow		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw Arrow from Tail to Head." );
  return  zip_ok;
  }

long
zipoarrow__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipoarrow		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  int					  position = 0; /*===*/
  static zip_type_point			  prior_X, prior_Y;
  static zip_type_pixel			  prior_x, prior_y;
  register zip_type_figure		  figure;

  IN(zipoarrow__Build_Object);
  if ( action == (long)view_LeftDown  &&
       (abs(x - prior_x) < self->tolerance  &&  abs(y - prior_y) < self->tolerance) )
    {
    action = NULL;
    CurrentFigure = NULL;
    prior_x = prior_y = prior_X = prior_Y = 0;
    pane->zip_pane_edit->zip_pane_edit_build_figure = true;
    }
  switch ( action )
    {
    case view_LeftDown:
      prior_x = x;  prior_y = y;
      if ( pane->zip_pane_edit->zip_pane_edit_build_figure  ||
	   CurrentFigure == NULL )
	{
	prior_X = X;  prior_Y = Y;
	if ( (status = zip_Create_Figure( Data, &CurrentFigure, NULL,
		zip_arrow_figure, CurrentImage, position )) == zip_ok )
          {
          zipoarrow_Set_Object_Point( self, CurrentFigure, zip_figure_origin_point, X, Y );
          zipoarrow_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
  	  CurrentFigure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
	  pane->zip_pane_edit->zip_pane_edit_build_figure = false;
	  pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	  }
	}
	else
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	if ( X != prior_X  ||  Y != prior_Y )
	  {
	  if ( pane->zip_pane_edit->zip_pane_edit_last_point_id == zip_figure_auxiliary_point  &&
	       CurrentFigure->zip_figure_points->zip_points[0].zip_point_x ==
		 CurrentFigure->zip_figure_point.zip_point_x &&
	       CurrentFigure->zip_figure_points->zip_points[0].zip_point_y ==
		 CurrentFigure->zip_figure_point.zip_point_y )
	      pane->zip_pane_edit->zip_pane_edit_last_point_id--;
          zipoarrow_Set_Object_Point( self, CurrentFigure,
		   ++pane->zip_pane_edit->zip_pane_edit_last_point_id,
		   prior_X = X, prior_Y = Y );
	  zipview_Draw_Figure( View, CurrentFigure, pane );
	  zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	  }
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	}
      break;
    case view_LeftUp:
      if ( figure = CurrentFigure )
	{
	if ( figure_x_point == figure_x_points(0)  &&
	     figure_y_point == figure_y_points(0) )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
          break;
	  }
	  else
	  {
          prior_x = x;  prior_y = y;
          zipview_Draw_Figure( View, CurrentFigure, pane );
	  }
	}
      break;
    case view_LeftMovement:
      if ( CurrentFigure  &&  (X != prior_X  ||  Y != prior_Y) )
	{
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	zipview_Draw_Figure( View, CurrentFigure, pane );
        zipoarrow_Set_Object_Point( self, CurrentFigure,
	    pane->zip_pane_edit->zip_pane_edit_last_point_id, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	}
      break;
    }
  OUT(zipoarrow__Build_Object);
  return  status;
  }

long
zipoarrow__Draw_Object( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane );
  OUT(zipoarrow__Draw_Object);
  return  status;
  }

long
zipoarrow__Clear_Object( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipoarrow__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane );
  OUT(zipoarrow__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register unsigned char		  width;

  IN(Draw);
  if ( zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
    {
    switch ( figure->zip_figure_style )
      {
      case 0: Draw_Basic_Style(self, figure, pane, NULL );   break;
      case 1: Draw_Basic_Style(self, figure, pane, '9' );    break;
      case 2: Draw_Basic_Style(self, figure, pane, '0' );    break;
      }
    if ( ExposePoints )    zipoarrow_Expose_Object_Points( self, figure, pane );
    if ( HighlightPoints ) zipoarrow_Highlight_Object_Points( self, figure, pane );
    }
  OUT(Draw);
  return  status;
  }

static
Draw_Basic_Style( self, figure, pane, fill )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register char				  fill;
  {
  long					  current_x, current_y, prior_x, prior_y;
  struct point				  points[3];
  register short			  transfer_mode;

  IN(Draw_Basic_Style);
  Draw_Basic_Body( self, figure, pane, &current_x, &current_y, &prior_x, &prior_y );
  if ( prior_x != current_x  ||  prior_y != current_y )
    {
    Feather_Points( current_x, current_y, prior_x, prior_y,
		    &points[0].x, &points[0].y, &points[1].x, &points[1].y );
    points[2].x = current_x; points[2].y = current_y;
    if ( fill )
      {
      transfer_mode = zipview_GetTransferMode( View );
      zipview_SetTransferMode( View, graphic_COPY );
      zipview_FillPolygon( View, points, 3,
	zipview_Define_Graphic( View, zip_Define_Font( Data, ShadeFontName, NULL ), fill ) );
      zipview_SetTransferMode( View, transfer_mode );
      zipview_DrawLineTo( View, points[0].x, points[0].y );
      zipview_DrawLineTo( View, points[1].x, points[1].y );
      zipview_DrawLineTo( View, current_x, current_y );
      }
      else
      {
      zipview_DrawLineTo( View, points[0].x, points[0].y );
      zipview_MoveTo( View, current_x, current_y );
      zipview_DrawLineTo( View, points[1].x, points[1].y  );
      }
    }
  OUT(Draw_Basic_Style);
  }

static
Draw_Basic_Body( self, figure, pane, current_x, current_y, prior_x, prior_y )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *current_x, *current_y, *prior_x, *prior_y;
  {
  register long				  i;

  zipview_MoveTo( View, *prior_x = window_x_point, *prior_y = window_y_point );
  for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
    {
    zipview_DrawLineTo( View, *current_x = window_x_points(i),
			      *current_y = window_y_points(i) );
    if ( figure->zip_figure_points->zip_points_count > (i+1) )
      { *prior_x = *current_x;  *prior_y = *current_y; }
    }
  }

static
Feather_Points( current_x, current_y, prior_x, prior_y, x1, y1, x2, y2 )
  register zip_type_pixel		  current_x, current_y, prior_x, prior_y,
					 *x1, *y1, *x2, *y2;
  {
  register double			  theta, cos_theta, sin_theta;

  IN(Feather_Points);
  DEBUGdt(Current-x,current_x);  DEBUGdt(Current-y,current_y);
  DEBUGdt(Prior-x,prior_x);      DEBUGdt(Prior-y,prior_y);
  theta = atan2( 1.0 * ((current_y - prior_y) ? (current_y - prior_y) : 1),
		 1.0 * ((prior_x - current_x) ? (prior_x - current_x) : 1) );
  DEBUGgt(Theta,theta);
  cos_theta = cos( theta );  DEBUGgt(Cos-Theta,cos_theta);
  sin_theta = sin( theta );  DEBUGgt(Sin-Theta,sin_theta);
  *x1 = current_x + (5.0 * cos_theta - 2.5 * sin_theta);  DEBUGdt(X1,x1);
  *y1 = current_y - (5.0 * sin_theta + 2.5 * cos_theta);  DEBUGdt(Y1,y1);
  *x2 = current_x + (5.0 * cos_theta - (-2.5 * sin_theta));  DEBUGdt(X2,x2);
  *y2 = current_y - (5.0 * sin_theta + (-2.5 * cos_theta));  DEBUGdt(Y2,y2);
  OUT(Feather_Points);
  }

long
zipoarrow__Print_Object( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  pc, status = zip_ok, x1, y1, x2, y2,
					  current_x = 0, current_y = 0, prior_x, prior_y;
  zip_type_point_pairs	    		  points = NULL;
  register double			  theta, cos_theta, sin_theta;
/*===debug=1;===*/
  IN(zipoarrow__Print_Object);
  if ( (status = zip_Allocate_Figure_Points_Vector( Data, &points )) == zip_ok )
    {
    points->zip_points_count = 0;
    prior_x = print_x_point;  prior_y = print_y_point;
    for ( pc = 0; pc < figure->zip_figure_points->zip_points_count; pc++ )
      {
      if ( status == zip_ok )
        {
        points->zip_points[pc].zip_point_x = current_x = print_x_points(pc);
        points->zip_points[pc].zip_point_y = current_y = print_y_points(pc);
        points->zip_points_count++;
        status = zip_Enlarge_Figure_Points_Vector( Data, &points );
	if ( figure->zip_figure_points->zip_points_count > (pc+1) )
	  { prior_x = current_x;  prior_y = current_y; }
        }
      }
    DEBUGdt(Count,points->zip_points_count);
    theta = atan2( 1.0 * ((current_y - prior_y) ? (current_y - prior_y) : 1),
		   1.0 * ((prior_x - current_x) ? (prior_x - current_x) : 1) );
    DEBUGgt(Theta,theta);
    cos_theta = cos( theta );  DEBUGgt(Cos-Theta,cos_theta);
    sin_theta = sin( theta );  DEBUGgt(Sin-Theta,sin_theta);
    x1 = current_x + (750.0 * cos_theta - 375.0 * sin_theta);     DEBUGdt(X1,x1);
    y1 = current_y - (750.0 * sin_theta + 375.0 * cos_theta);     DEBUGdt(Y1,y1);
    x2 = current_x + (750.0 * cos_theta - (-375.0 * sin_theta));  DEBUGdt(X2,x2);
    y2 = current_y - (750.0 * sin_theta + (-375.0 * cos_theta));  DEBUGdt(Y2,y2);
    points->zip_points[pc].zip_point_x = x1;
    points->zip_points[pc].zip_point_y = y1;
    pc++;  points->zip_points_count++;
    status = zip_Enlarge_Figure_Points_Vector( Data, &points );
    points->zip_points[pc].zip_point_x = current_x;
    points->zip_points[pc].zip_point_y = current_y;
    pc++;  points->zip_points_count++;
    status = zip_Enlarge_Figure_Points_Vector( Data, &points );
    points->zip_points[pc].zip_point_x = x2;
    points->zip_points[pc].zip_point_y = y2;
    zipprint_Ensure_Line_Attributes( Print, figure );
    zipprint_Draw_Multi_Line( Print,
			      figure->zip_figure_points->zip_points_count + 3,
			      print_x_point, print_y_point, points );
    free( points );
    }
  OUT(zipoarrow__Print_Object);
  return  status;
  }

long
zipoarrow__Within_Object( self, figure, pane, x, y )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register long				  distance = -1;

  IN(zipoarrow__Within_Object);
/*===*/
  OUT(zipoarrow__Within_Object);
  return  distance;
  }

boolean
zipoarrow__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = true;
  register zip_type_pixel		  X, Y;
  register long				  i;

  IN(zipoarrow__Enclosed_Object);
  X = window_x_point;  Y = window_y_point;
  if ( X < x  ||  X > (x + w)  ||  Y < y  ||  Y > (y + h) )
    enclosed = false;
  for ( i = 0; enclosed == true  &&  i < figure->zip_figure_points->zip_points_count; i++ )
    {
    X = window_x_points(i);  Y = window_y_points(i);
    if (X < x  ||  X > (x + w)  ||  Y < y  ||  Y > (y + h) )
      enclosed = false;
    }
  OUT(zipoarrow__Enclosed_Object);
  return  enclosed;
  }

long
zipoarrow__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  register zip_type_pixel		  max_x, min_x, max_y, min_y, X, Y;
  register long				  i;

  IN(zipoarrow__Object_Enclosure);
  max_x = min_x = window_x_point;  max_y = min_y = window_y_point;
  for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
    {
    if ( (X = window_x_points(i)) > max_x )
      max_x = X;
      else
      if ( X < min_x )
        min_x = X;
    if ( (Y = window_y_points(i)) > max_y )
      max_y = Y;
      else
      if ( Y < min_y )
        min_y = Y;
    }
  *x = min_x;  *y = min_y;
  *w = abs(max_x - min_x);
  *h = abs(max_y - min_y);
  OUT(zipoarrow__Object_Enclosure);
  return  zip_ok;
  }

long
zipoarrow__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register long				  point = 0, i;

  IN(zipoarrow__Proximate_Object_Points);
  if ( zipview_Proximate_Figure_Point( View, pane, figure,
				       window_x_point, window_y_point, x, y ) )
    point = zip_figure_origin_point;
    else
    for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
      if (  zipview_Proximate_Figure_Point( View, pane, figure,
				     window_x_points(i),
				     window_y_points(i), x, y ) )
	{
	point = zip_figure_auxiliary_point + i;
        break;
	}
  OUT(zipoarrow__Proximate_Object_Points);
  return  point;
  }

long
zipoarrow__Highlight_Object_Points( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok, i;

  IN(zipoarrow__Highlight_Object_Points);
  zipedit_Highlight_Point( Edit, pane, window_x_point, window_y_point );
    for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
      zipedit_Highlight_Point( Edit, pane, window_x_points(i), window_y_points(i) );
  OUT(zipoarrow__Highlight_Object_Points);
  return  status;
  }

long
zipoarrow__Normalize_Object_Points( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok, i;

  IN(zipoarrow__Normalize_Object_Points);
  zipedit_Normalize_Point( Edit, pane, window_x_point, window_y_point );
    for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
      zipedit_Normalize_Point( Edit, pane, window_x_points(i), window_y_points(i) );
  OUT(zipoarrow__Normalize_Object_Points);
  return  status;
  }

long
zipoarrow__Expose_Object_Points( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok, i;

  IN(zipoarrow__Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
      zipedit_Expose_Point( Edit, pane, figure, figure_x_points(i), figure_y_points(i) );
  OUT(zipoarrow__Expose_Object_Points);
  return  status;
  }

long
zipoarrow__Hide_Object_Points( self, figure, pane )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok, i;

  IN(zipoarrow__Hide_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
    for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
     zipedit_Hide_Point( Edit, pane, figure, figure_x_points(i), figure_y_points(i) );
  OUT(zipoarrow__Hide_Points);
  return  status;
  }

long
zipoarrow__Set_Object_Point( self, figure, point, x, y )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;

  IN(zipoarrow__Set_Object_Point);
  if ( point == zip_figure_origin_point )
    {
    figure->zip_figure_point.zip_point_x = x;
    figure->zip_figure_point.zip_point_y = y;
    }
  else
  if ( point == zip_figure_auxiliary_point )
    {
    if ( figure->zip_figure_points == NULL )
      if ( (status = zip_Allocate_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_ok )
	figure->zip_figure_points->zip_points_count = 1;
    if ( status == zip_ok )
      {
      figure->zip_figure_points->zip_points[0].zip_point_x = x; 
      figure->zip_figure_points->zip_points[0].zip_point_y = y;
      }
    }
  else
  if ( (point - zip_figure_auxiliary_point) < figure->zip_figure_points->zip_points_count )
    {
    figure->zip_figure_points->zip_points[point -
	zip_figure_auxiliary_point].zip_point_x = x;
    figure->zip_figure_points->zip_points[point -
	zip_figure_auxiliary_point].zip_point_y = y;
    }
  else
  if ( (point - zip_figure_auxiliary_point) == figure->zip_figure_points->zip_points_count )
    {
    if ( (status = zip_Enlarge_Figure_Points_Vector( Data, &figure->zip_figure_points )) == zip_ok )
      {
      figure->zip_figure_points->zip_points[point - zip_figure_auxiliary_point].zip_point_x = x;
      figure->zip_figure_points->zip_points[point - zip_figure_auxiliary_point].zip_point_y = y;
      figure->zip_figure_points->zip_points_count++;
      }
    }
  else status = zip_failure; /*=== zip_invalid_point_type (more than 1 beyond allocation ===*/
  if ( status == zip_ok )
    {
    zip_Set_Image_Extrema( Data, figure->zip_figure_image, x, y );
    zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
    }
  OUT(zipoarrow__Set_Object_Point);
  return  status;
  }

long
zipoarrow__Object_Point( self, figure, point, x, y )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register long				  point;
  register zip_type_point		  *x, *y;
  {
  register long				  status = zip_ok;

  IN(zipoarrow__Object_Point);
/*===*/*x = *y = 0;
  OUT(zipoarrow__Object_Point);
  return  status;
  }

long
zipoarrow__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipoarrow		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok, i;

  IN(zipoarrow__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  for ( i = 0; i < figure->zip_figure_points->zip_points_count; i++ )
    {
    figure->zip_figure_points->zip_points[i].zip_point_x += x_delta;
    figure->zip_figure_points->zip_points[i].zip_point_y += y_delta;
    zip_Set_Image_Extrema( Data, figure->zip_figure_image,
		figure->zip_figure_points->zip_points[i].zip_point_x,
		figure->zip_figure_points->zip_points[i].zip_point_y  );
/*===handle both extrema 7/20/86===*/
/*===have extrema check for REDUCTIONS as well as EXPANSIONS 7/20/86===*/
    zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
    }
  OUT(zipoarrow__Adjust_Object_Point_Suite);
  return  status;
  }

