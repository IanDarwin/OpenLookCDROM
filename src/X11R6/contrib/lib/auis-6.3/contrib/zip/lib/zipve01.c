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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipve01.c,v 1.5 1993/05/04 01:51:05 susan Exp $";
#endif


 

/* zipve01.c	Zip EditView-object  -- Grids			      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */



/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip EditView-object  --  Grids

MODULE	zipve01.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Editing facilities
	of the Zip View-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/31/88	Created (TCP)
  01/18/88	Improve Grid speed by not drawing any dots beyond 1st level (TCP)
   09/26/89	Fix point delta calculations in Draw_Pane_Grid() (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "fontdesc.ih"
#include "zip.ih"
#include "zipv.ih"
#include "zipedit.ih"
#include "zipedit.h"

static Draw_Pane_Coordinate_Marks();
static Draw_Pane_Coordinate_Ticks();
static Compute_Pane_Coordinate_Deltas();

int
zipedit__Draw_Pane_Coordinates( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipedit__Draw_Pane_Coordinates);
  pane->zip_pane_state.zip_pane_state_coordinates_exposed = true;
  pane->zip_pane_border_thickness = ZIP_pane_coordinate_thickness;
/*===  zipedit_Compute_Pane_Stretch_Factors( self, pane );===*/
  zipview_Set_Clip_Area( View, pane, PaneLeft, PaneTop, PaneWidth, PaneHeight );
  zipview_SetTransferMode( View, graphic_BLACK );
  zipview_MoveTo( View, PaneLeft, PaneTop );
  if ( zipview_GetLineWidth( View ) != 1 )
    zipview_SetLineWidth( View, 1 );
  zipview_DrawLineTo( View, PaneRight - 1, PaneTop );
  zipview_DrawLineTo( View, PaneRight - 1, PaneBottom - 1 );
  zipview_DrawLineTo( View, PaneLeft, PaneBottom - 1 );
  zipview_DrawLineTo( View, PaneLeft, PaneTop );
  zipview_MoveTo( View, PaneLeft + ZIP_pane_coordinate_thickness - 1,
		  PaneTop + ZIP_pane_coordinate_thickness - 1);
  zipview_DrawLineTo( View, PaneRight - ZIP_pane_coordinate_thickness,
		      PaneTop + ZIP_pane_coordinate_thickness - 1 );
  zipview_DrawLineTo( View, PaneRight - ZIP_pane_coordinate_thickness,
		      PaneBottom - ZIP_pane_coordinate_thickness );
  zipview_DrawLineTo( View, PaneLeft + ZIP_pane_coordinate_thickness - 1,
		      PaneBottom - ZIP_pane_coordinate_thickness );
  zipview_DrawLineTo( View, PaneLeft + ZIP_pane_coordinate_thickness - 1,
		      PaneTop + ZIP_pane_coordinate_thickness - 1);
  Compute_Pane_Coordinate_Deltas( self, pane );
  Draw_Pane_Coordinate_Marks( self, pane );
  zipedit_Draw_Pane_Grid( self, pane );
  zipview_Set_Clip_Area( View,  pane, PaneLeft + ZIP_pane_coordinate_thickness + 2,
		       PaneTop  + ZIP_pane_coordinate_thickness + 2,
		       PaneWidth - (2 * ZIP_pane_coordinate_thickness) - 2,
		       PaneHeight- (2 * ZIP_pane_coordinate_thickness) - 2 );
  OUT(zipedit__Draw_Pane_Coordinates);
  return zip_success;
  }

zipedit__Draw_Pane_Grid( self, pane )
  register struct zipedit	 *self;
  register zip_type_pane	  pane;
  {
  register long			  i, j, k, point_spacing,
				  X_int = 0, Y_int = 0;
  register float		  X, Y, point_delta_SMSD;
  /* register float		    grid_factor, delta */
  register float		  SM, SD, point_delta,
                                  L, T, R, B,
				  x_center, y_center;
  struct fontdesc		 *current_font =
				    zipview_GetFont( View );
  char				  big_dot = 'C';
  register struct graphic	 *black = zipview_BlackPattern( View );;

  IN(zipedit__Draw_Pane_Grid);
  if ( pane->zip_pane_edit->zip_pane_edit_coordinate_grid )
    {
    Compute_Pane_Coordinate_Deltas( self, pane );
    point_spacing = pane->zip_pane_edit->zip_pane_edit_mark_point_spacing;
    point_delta = pane->zip_pane_edit->zip_pane_edit_mark_point_delta;
    zipview_Set_Clip_Area( View, pane,
	 PaneLeft + pane->zip_pane_border_thickness + 2,
	 PaneTop  + pane->zip_pane_border_thickness + 2,
	 PaneWidth - (2 * pane->zip_pane_border_thickness) - 2,
	 PaneHeight- (2 * pane->zip_pane_border_thickness) - 2 );
    if ( self->dots_font == NULL )
      self->dots_font = (struct fontdesc *) zip_Define_Font( Data, zip_dot_font_name, NULL );
    zipview_SetFont( View, self->dots_font );
    if ( pane->zip_pane_zoom_level >= 0 )
      SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
      else
      SM = pane->zip_pane_stretch_multiplier / (abs( pane->zip_pane_zoom_level ) + 1);
    SD = pane->zip_pane_stretch_divisor;
    point_delta_SMSD = (point_delta * SM) / SD;
/*    delta =  (point_delta_SMSD = (point_delta * SM) / SD)/
	    pane->zip_pane_edit->zip_pane_edit_coordinate_grid; */
    L = PaneLeft;
    R = PaneRight;
    T = PaneTop;
    B = PaneBottom;
    x_center = pane->zip_pane_x_origin + pane->zip_pane_x_offset;
    y_center = pane->zip_pane_y_origin - pane->zip_pane_y_offset;
    zipview_SetTransferMode( View, graphic_BLACK );
/*    grid_factor = pane->zip_pane_edit->zip_pane_edit_coordinate_grid; */
    for ( i = 0;
	  x_center + (i * point_spacing) <= R  ||
	  x_center - ((i+1) * point_spacing) >= L  ||
	  y_center + (i * point_spacing) <= B  ||
	  y_center - ((i+1) * point_spacing) >= T;
	  i++ )
      {
      for ( j = -i; j <= i; j++ )
	{
	X_int = X = x_center + (j * point_delta_SMSD );
	if ( X_int < x_center ) X++; /*=== FUDGE ===*/
	for ( k = -i; k <= i; k++ )
	  {
	  Y_int = Y = y_center - (k * point_delta_SMSD );
	  if ( Y_int < y_center ) Y++; /*=== FUDGE ===*/
	  if ( ( (j == -i  ||  j == i)  ||  (k == -i  ||  k == i) )  &&
		X >= L  &&  X <= R  &&  Y >= T  &&  Y <= B )
	    {
	    zipview_FillRectSize( View, X_int, Y_int, 1,1, black );
/*===
	    if ( grid_factor  &&  point_delta > grid_factor )
	      for ( r = 0; r < grid_factor; r++ )
	        for ( c = 0; c < grid_factor; c++ )
		  if ( c > 0  ||  r > 0 )
		    zipview_FillRectSize( View, (int)(X + c * delta),
				     (int)(Y + r * delta), 1,1, black );
===*/
	    }
	  }
	}
      if ( i == 0 )
        {
        zipview_MoveTo( View, X_int, Y_int );
        zipview_DrawText( View, &big_dot, 1, NULL );
	}
      }
    zipview_SetFont( View, current_font );
    }
  OUT(zipedit__Draw_Pane_Grid);
  return zip_success;
  }

static
Clear_Pane_Mark_Areas( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  zipview_SetTransferMode( View, graphic_WHITE );
  zipview_FillRectSize( View, PaneLeft + 1, PaneTop + 1,
    PaneWidth - 2, pane->zip_pane_border_thickness - 2, zipview_WhitePattern( View ) );
  zipview_FillRectSize( View, PaneRight - pane->zip_pane_border_thickness + 1,
    PaneTop + 1,
    pane->zip_pane_border_thickness - 2, PaneHeight - 2, zipview_WhitePattern( View ) );
  zipview_FillRectSize( View, PaneLeft + 1, PaneBottom - pane->zip_pane_border_thickness + 1,
    PaneWidth - 2, pane->zip_pane_border_thickness - 2, zipview_WhitePattern( View ) );
  zipview_FillRectSize( View, PaneLeft + 1, PaneTop + 1,
    pane->zip_pane_border_thickness - 2, PaneHeight - 2, zipview_WhitePattern( View ) );
  zipview_SetTransferMode( View, graphic_BLACK );
  return zip_success;
  }

static
Draw_Pane_Coordinate_Marks( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register struct fontdesc		 *current_font = zipview_GetFont( View );

  IN(Draw_Pane_Coordinate_Marks);
  zipview_Set_Clip_Area( View, pane, PaneLeft, PaneTop, PaneWidth, PaneHeight );
  Clear_Pane_Mark_Areas( self, pane );
  Draw_Pane_Coordinate_Ticks( self, pane );
  zipview_Set_Clip_Area( View,  pane, PaneLeft + ZIP_pane_coordinate_thickness + 2,
		       PaneTop  + ZIP_pane_coordinate_thickness + 2,
		       PaneWidth - (2 * ZIP_pane_coordinate_thickness) - 2,
		       PaneHeight- (2 * ZIP_pane_coordinate_thickness) - 2 );
  zipview_SetFont( View, current_font );
  OUT(Draw_Pane_Coordinate_Marks);
  return zip_success;
  }

static
Draw_Pane_Coordinate_Ticks( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register float			  SM, SD;
  register int				  i, center, middle, edge;
  char					  point_value[256];
  short				     font = NULL;

  IN(Draw_Pane_Coordinate_Ticks);
  zip_Define_Font( Data, ZIP_pane_coordinate_font_name, &font );
  zipview_SetFont( View, Fonts->zip_fonts_vector[font].zip_fonts_table_font );
  center = PaneLeft + pane->zip_pane_x_offset + PaneWidth/2;
  middle = PaneTop + ZIP_pane_coordinate_thickness/2;
  edge = PaneTop + ZIP_pane_coordinate_thickness - 3;
  zipview_Set_Clip_Area( View,  pane, PaneLeft + ZIP_pane_coordinate_thickness,
		       PaneTop, PaneWidth - (2 * ZIP_pane_coordinate_thickness),
		       ZIP_pane_coordinate_thickness );
  if ( pane->zip_pane_zoom_level >= 0 )
    SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
    else
    SM = pane->zip_pane_stretch_multiplier / (abs( pane->zip_pane_zoom_level ) + 1);
  SD = pane->zip_pane_stretch_divisor;
  for ( i = 0;
	center + (i * pane->zip_pane_edit->zip_pane_edit_mark_point_spacing) <=
		 PaneRight  ||
	center - (i * pane->zip_pane_edit->zip_pane_edit_mark_point_spacing) >=
		 PaneLeft;
	i++ )
    {
    sprintf( point_value, "%d", i * (int)pane->zip_pane_edit->zip_pane_edit_mark_point_delta );
    zipview_MoveTo( View, (int)(center +
		       ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)),  middle );
    zipview_DrawString( View, point_value,
			((i%2) ? view_ATTOP : view_ATBOTTOM) | view_BETWEENLEFTANDRIGHT );
    if ( i == 0 )
      zipview_FillRectSize( View, center - 1, edge - 2, 3, 5, zipview_WhitePattern( View ) ); 
      else
      zipview_FillRectSize( View, (int)(center +
		        ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)),
		      edge, 1,3, zipview_WhitePattern( View ) ); 
    zipview_MoveTo( View, (int)(center - ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)),
		   middle );
    zipview_DrawString( View, point_value,
			((i%2) ? view_ATTOP : view_ATBOTTOM) | view_BETWEENLEFTANDRIGHT );
    zipview_FillRectSize( View, (int)(center - ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)),
		    edge, 1, 3, zipview_WhitePattern( View ) ); 
    }
  center = PaneLeft + ZIP_pane_coordinate_thickness/2;
  middle = PaneTop - pane->zip_pane_y_offset + PaneHeight/2;
  edge = PaneLeft + ZIP_pane_coordinate_thickness - 3;
  zipview_Set_Clip_Area( View,  pane, PaneLeft,
		       PaneTop + ZIP_pane_coordinate_thickness,
		       ZIP_pane_coordinate_thickness,
		       PaneHeight - (2 * ZIP_pane_coordinate_thickness ) );
  for ( i = 0;
	middle + (i * pane->zip_pane_edit->zip_pane_edit_mark_point_spacing) <=
	     PaneBottom  ||
	middle - (i * pane->zip_pane_edit->zip_pane_edit_mark_point_spacing) >=
	     PaneTop;
	i++ )
    {
    sprintf( point_value, "%d", i * (int)pane->zip_pane_edit->zip_pane_edit_mark_point_delta );
    zipview_MoveTo( View,  center,
		    (int)(middle + ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)) );
    zipview_DrawString( View, point_value,
		    view_BETWEENTOPANDBOTTOM | view_BETWEENLEFTANDRIGHT );
    if ( i == 0 )
      zipview_FillRectSize( View, edge - 2, middle - 1, 5, 3, zipview_WhitePattern( View ) ); 
      else
      zipview_FillRectSize( View, edge,
	 (int)(middle + ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)),
	 3, 1, zipview_WhitePattern( View ) ); 
    zipview_MoveTo( View, center,
		    (int)(middle - ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD))  );
    zipview_DrawString( View, point_value, view_BETWEENTOPANDBOTTOM | view_BETWEENLEFTANDRIGHT );
    zipview_FillRectSize( View, edge,
	 (int)(middle - ((i * pane->zip_pane_edit->zip_pane_edit_mark_point_delta) * SM / SD)),
	 3,1, zipview_WhitePattern( View ) ); 
    }
  zipview_Set_Clip_Area( View,  pane, PaneLeft + ZIP_pane_coordinate_thickness + 2,
		       PaneTop  + ZIP_pane_coordinate_thickness + 2,
		       PaneWidth - (2 * ZIP_pane_coordinate_thickness) - 2,
		       PaneHeight- (2 * ZIP_pane_coordinate_thickness) - 2 );
  OUT(Draw_Pane_Coordinate_Ticks);
  return zip_success;
  }

static
Compute_Pane_Coordinate_Deltas( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register float			  SM, SD, delta = 1.0, limit = 10.0;
  register int				  done = false, low;

  if ( pane->zip_pane_zoom_level >= 0 )
    SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
    else
    SM = pane->zip_pane_stretch_multiplier / (abs( pane->zip_pane_zoom_level ) + 1);
  SD = pane->zip_pane_stretch_divisor;
  if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source )
    low = ((pane->zip_pane_source.zip_pane_stream->zip_stream_greatest_x / 20.0) * SM) / SD;
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_image_source )
    low = ((pane->zip_pane_source.zip_pane_image->zip_image_greatest_x / 20.0) * SM) / SD;
  else /*=== deal with figure extrema ===*/
    low = (20.0 * SM) / SD;
/*=== make intelligent ===*/
  pane->zip_pane_edit->zip_pane_edit_mark_point_delta = 100.0;

  if ( (pane->zip_pane_edit->zip_pane_edit_mark_point_spacing =
	    ((pane->zip_pane_edit->zip_pane_edit_mark_point_delta * SM) / SD )) <
		 low )
    while ( ! done )
      {
      for ( pane->zip_pane_edit->zip_pane_edit_mark_point_delta = delta;
          pane->zip_pane_edit->zip_pane_edit_mark_point_delta <= limit;
	  pane->zip_pane_edit->zip_pane_edit_mark_point_delta += delta )
	{
        if ( (pane->zip_pane_edit->zip_pane_edit_mark_point_spacing =
	    ((pane->zip_pane_edit->zip_pane_edit_mark_point_delta * SM) / SD )) >=
		 low  &&
	       ((int)pane->zip_pane_edit->zip_pane_edit_mark_point_delta % 2) == 0 )
	  done = true;
	}
        delta = limit;
        limit = 10.0 * limit;
      }
  return zip_success;
  }

zipedit__Lighten_Pane( self, pane, density )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register char				  density;
  {
  zipview_Set_Pane_Clip_Area( View, pane );
  zipview_SetTransferMode( View, graphic_WHITE );
  if ( density == NULL )
    density = 'G';
  zipview_FillTrapezoid( View,
	PaneLeft, PaneTop, PaneWidth,
	PaneLeft, PaneBottom, PaneWidth,
zipview_WhitePattern(View)/*===tile===*/ );
  zipview_FlushGraphics( View );
  zipview_SetTransferMode( View, graphic_BLACK );
  }

