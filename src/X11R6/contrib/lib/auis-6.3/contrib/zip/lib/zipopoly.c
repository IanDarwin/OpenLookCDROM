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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipopoly.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipopoly.c	Zip Object -- Polygon				      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipopoly.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:00:16  rr2b
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
 * Revision 2.12  1991/09/12  16:43:28  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.11  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.10  1990/08/21  14:33:45  sg08
 * Use Ensure_Attribute on Draw and Print
 *
 * Revision 2.9  90/05/09  16:02:23  gk5g
 * Changes in support of i386_mach.
 * 
 * Revision 2.8  89/09/08  17:42:22  ghoti
 * removal of unused variables
 * 
 * Revision 2.7  89/08/30  16:27:51  sg08
 * Removed excess SetTransferMode activity. Accomodate non-refresh on builds.
 * 
 * Revision 2.6  89/05/01  22:13:36  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.5  89/02/08  16:51:06  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  19:58:21  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/18  21:11:48  tom
 * Handle variable line-widths;
 * 
 * Revision 2.2  88/10/11  20:34:24  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:17:08  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:42:18  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:45:57  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Polygon

MODULE	zipopoly.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)
  08/24/89	Remove excess SetTransferMode() activity in Draw() (SCG)
  08/24/89	Modify Build to handle non-refresh of pane on build completion (SCG)
   08/16/90	Use Ensure_Attribute on Draw and Print (SCG)

END-SPECIFICATION  ************************************************************/

#include <math.h>
#include <class.h>
#include <view.ih>
#include <zipobj.ih>
#include <zipopoly.eh>

static long				  sides;


static Draw();

char
zipopolygon__Object_Icon( self )
  register struct zipopolygon		 *self;
  {
  IN(zipopolygon__Object_Icon);
  OUT(zipopolygon__Object_Icon);
  return  'N';
  }

char
zipopolygon__Object_Icon_Cursor( self )
  register struct zipopolygon		 *self;
  {
  IN(zipopolygon__Object_Icon_Cursor);
  OUT(zipopolygon__Object_Icon_Cursor);
  return  'G';
  }

char
zipopolygon__Object_Datastream_Code( self )
  register struct zipopolygon		 *self;
  {
  IN(zipopolygon__Object_Datastream_Code);
  OUT(zipopolygon__Object_Datastream_Code);
  return  'E';
  }

long
zipopolygon__Show_Object_Properties( self, pane, figure )
  register struct zipopolygon		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  char					 *response;

  zipview_Set_Pane_Cursor( View, pane, '@', IconFontName );
  sides = 0;
  CurrentFigure = NULL;
  zipview_Query( View, "Enter Number of Polygon Sides [3]: ",
					NULL, &response );
  DEBUGst(Response,response);
  sides = atoi( response );
  DEBUGdt(Sides,sides);
  if ( sides < 3 )  sides = 3;
  zipview_Announce( View, "Draw Polygon from Center outward." );
  zipview_Set_Pane_Cursor( View, pane, 'G', CursorFontName );
  return  zip_ok;
  }

long
zipopolygon__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipopolygon		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok,
					  radial_point = 0;
  register zip_type_figure		  figure;

  IN(zipopolygon__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
        zip_Create_Figure( Data, &CurrentFigure, NULL, zip_polygon_figure,
			   CurrentImage, 0 )) == zip_success )
	{ DEBUG(Created);
        zipopolygon_Set_Object_Point( self, CurrentFigure,
				      zip_figure_origin_point, X, Y );
	zipopolygon_Set_Object_Point( self, CurrentFigure,
				      zip_figure_auxiliary_point, X, Y );
	figure = CurrentFigure;
	figure_x_points(0) = figure_y_points(0) = 0;
	figure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
        pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	zip_Set_Figure_Shade( Data, figure,
			      pane->zip_pane_edit->zip_pane_edit_current_shade );
	zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	}
      break;
    case view_LeftUp:
      if ( figure = CurrentFigure )
	{
	DEBUGdt(Radius,figure_x_points(0));
	DEBUGdt(Sides,figure_y_points(0));
        zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
	if ( figure_x_points(0) == 0  ||  figure_y_points(0) < 3 )
	  {
	  zipview_Clear_Figure( View, figure, pane );
	  zip_Destroy_Figure( Data, figure );
	  CurrentFigure = NULL;
	  }
          else
          {
	    zipview_Draw_Figure( View, figure, pane );
	  }
	}
	break;
    case view_LeftMovement:
      if ( (figure = CurrentFigure)  &&  status == zip_ok )
	{
	zipview_Draw_Figure( View, figure, pane );
        if ( abs( X - figure_x_point ) < abs( Y - figure_y_point ) )
	  radial_point = figure_x_point + abs( Y - figure_y_point );
	  else
	  radial_point = X;
        zipopolygon_Set_Object_Point( self, figure,
				      zip_figure_auxiliary_point, radial_point, 0 );
	figure_y_points(0) = sides;
	zipview_Draw_Figure( View, figure, pane );
	}
      break;
    }
  OUT(zipopolygon__Build_Object);
  return  status;
  }

long
zipopolygon__Draw_Object( self, figure, pane )
  register struct zipopolygon		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipopolygon__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane, zip_draw );
  OUT(zipopolygon__Draw_Object);
  return  status;
  }

long
zipopolygon__Clear_Object( self, figure, pane )
  register struct zipopolygon		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipopolygon__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane, zip_clear );
  OUT(zipopolygon__Clear_Object);
  return  status;
  }

#if (defined(MACH) && defined(i386))
int calltosavecompile (pane, figure, self)
zip_type_pane pane;
zip_type_figure figure;
struct zipopolygon *self;
{
return (abs( zipview_X_Point_To_Pixel( View, pane, figure,
		figure_x_point + figure_x_points(0) ) - window_x_point ));
}
#endif

static
Draw( self, figure, pane, action )
  register struct zipopolygon		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register short			  action;
  {
  register long				  status = zip_ok, i,
					  sides, radius;
  register unsigned char		  pattern = NULL, shade,
                                          allocated = false, width;
  register short			  even = true;
  struct point				  point_vector[9];
  register struct point			 *points = point_vector;
  register double			  theta = 0.0, angle;

  IN(Draw);
  DEBUGdt(Sides,sides);
  if ( (sides = figure_y_points(0)) >= 3 )
    {
    if ( sides > 8 )
      {
      allocated = true;
      points = (struct point *) malloc( (sides + 1) * sizeof(struct point) );
      }
    if ( sides % 2 )
      { DEBUG(Odd);
      even = false;
      theta = -1.57079;
      }
#if (defined(MACH) && defined(i386))
    radius = calltosavecompile(pane,figure,self);
#else
    radius = abs( zipview_X_Point_To_Pixel( View, pane, figure,
		figure_x_point + figure_x_points(0) ) - window_x_point );
#endif
    points[0].x = points[sides].x = window_x_point + ((even) ? radius : 0);
    DEBUGdt(X,points[0].x);
    points[0].y = points[sides].y = window_y_point - ((even) ? 0 : radius);
    DEBUGdt(Y,points[0].y);
    angle = (2.0 * 3.14159) / sides;  DEBUGgt(Angle,angle);
    for ( i = 1; i < sides; i++ )
      {
      theta += angle;  DEBUGgt(Theta,theta);
      points[i].x = window_x_point + radius * cos(theta); DEBUGdt(X,points[i].x);
      points[i].y = window_y_point + radius * sin(theta); DEBUGdt(Y,points[i].y);
      }
    if ( (figure->zip_figure_mode.zip_figure_mode_shaded  ||
          figure->zip_figure_mode.zip_figure_mode_patterned) )
      {
      if ( View->mouse_action != view_LeftMovement  &&  action == zip_draw )
        {
        if ( figure->zip_figure_mode.zip_figure_mode_patterned  &&
	     (pattern = zip_Contextual_Figure_Pattern( Data, figure )) )
          { DEBUGct(Pattern,pattern);
          zipview_FillPolygon( View, points, sides + 1,
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
	    zipview_FillPolygon( View, points, sides + 1, 
	      zipview_Define_Graphic( View, zip_Define_Font( Data, ShadeFontName, NULL ), shade ) );
	    }
        }
        else
        if ( action == zip_clear )
	  { DEBUG(Clear Action);
	  zipview_FillPolygon( View, points, sides + 1, graphic_WHITE );
	  }
      }
      if ( zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
      {
      zipview_DrawPolygon( View, points, sides + 1 );
      if ( ExposePoints )
        zipopolygon_Expose_Object_Points( self, figure, pane );
      if ( HighlightPoints )
        zipopolygon_Highlight_Object_Points( self, figure, pane );
      }
    if ( allocated )   free( points );
    }
  OUT(Draw);
  return  status;
  }

long
zipopolygon__Print_Object( self, figure, pane )
  register struct zipopolygon		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  pc, status = zip_ok,
					  sides, radius, even = true;
  zip_type_point_pairs	    		  points = NULL;
  register double			  theta = 0.0, angle;

  IN(zipopolygon__Print_Object);
  sides = figure_y_points(0);
  radius = print_x_lengths(0);
  if ( (status = zip_Allocate_Figure_Points_Vector( Data, &points )) == zip_ok )
    {
    points->zip_points_count = 0;
    angle = (2.0 * 3.14159) / sides;
    if ( sides % 2 )
      {
      even = false;
      theta = -1.57079;
      }
    for ( pc = 0; pc < sides - 1  &&  status == zip_ok; pc++ )
      {
      if ( status == zip_success )
        {
        theta += angle;
        points->zip_points[pc].zip_point_x = print_x_point + radius * cos(theta);
        points->zip_points[pc].zip_point_y = print_y_point + radius * sin(theta);
        points->zip_points_count++;
        status = zip_Enlarge_Figure_Points_Vector( Data, &points );
        }
      }
    points->zip_points[sides-1].zip_point_x = print_x_point + ((even) ? radius : 0);
    points->zip_points[sides-1].zip_point_y = print_y_point - ((even) ? 0 : radius);
    zipprint_Set_Shade( Print, figure->zip_figure_fill.zip_figure_shade ); 
    zipprint_Ensure_Line_Attributes( Print, figure );
    zipprint_Draw_Multi_Line( Print, sides,
			      print_x_point + ((even) ? radius : 0),
			      print_y_point - ((even) ? 0 : radius), points );
    free( points );
    }
  OUT(zipopolygon__Print_Object);
  return  status;
  }
