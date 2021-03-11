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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipvp02.c,v 1.3 1992/12/15 21:58:46 rr2b R6tape $";
#endif


 

/* zipvp02.c	Zip View-object	-- Panning			      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */



/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object -- Panning	

MODULE	zipvp02.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Pane Cursor facilities
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
  12/08/88	Utilize panning_precision of pane when panning (TCP)
   08/16/90	Add Normalize_Line_Attributes on Initiate_Panning (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "zip.ih"
#include "zipv.ih"

#define	 Data			      (self->data_object)
#define	 View			      (self)

#define  BorderThickness	      (pane->zip_pane_border_thickness)


#define  panning_factor			  16

static Set_Crosshairs();
static int Blit_Pane();

long
zipview__Pan_Pane( self, pane, x_offset, y_offset )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x_offset, y_offset;
  {
  register int				  status = zip_success;

  IN(zipview_Pan_Pane);
  DEBUGdt(X-Offset,x_offset);
  DEBUGdt(Y-Offset,y_offset);
  ZIP_EFN(zip_Pan_Pane_EFN);
  if ( pane )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    if ( x_offset == 0  &&  y_offset == 0 )
      {
      Blit_Pane( self, pane, - pane->zip_pane_x_offset,
			    - pane->zip_pane_y_offset );
      pane->zip_pane_x_offset = 0;
      pane->zip_pane_y_offset = 0;
      pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin;
      pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin;
      }
      else
      {
      long  factor = (pane->zip_pane_panning_precision > 0 ) ?
			 pane->zip_pane_panning_precision : 1;
      x_offset = (x_offset / factor) * factor;
      y_offset = (y_offset / factor) * factor;
      pane->zip_pane_x_offset += x_offset;
      pane->zip_pane_y_offset += y_offset;
      pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin + pane->zip_pane_x_offset;
      pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin - pane->zip_pane_y_offset;
      Blit_Pane( self, pane, x_offset, y_offset );
      }
/*===
    if ( pane->zip_pane_state.zip_pane_state_coordinates_exposed )
      ZIP_Draw_Pane_Coordinate_Marks( pane );
    zipview_FlushGraphics( self );
===*/
    zipview_Draw_Pane( self, pane );
    }
    else
    {
    status = zip_pane_non_existent;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Pan_Pane);
  return status;
  }

long
zipview__Pan_Pane_To_Edge( self, pane, edge )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  edge;
  {
  register int				  status = zip_success;
  register int				  x_offset = 0, y_offset = 0;
  register zip_type_stream		  stream;

  IN(zip_Pan_Pane_To_Edge);
  ZIP_EFN(zip_Pan_Pane_To_Edge_EFN);
  if ( pane  &&  (stream = (zip_type_stream)zipview_Pane_Stream( self, pane )) )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    if ( edge & zipview_pane_top_edge )
      y_offset =   ((zipview_Pane_Y_Origin( self, pane ) -
		     pane->zip_pane_y_offset) - stream->zip_stream_greatest_y *
	((pane->zip_pane_zoom_level >= 0) ?
	 pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1)
	 :
	 pane->zip_pane_stretch_multiplier / abs(pane->zip_pane_zoom_level))
	 /
	 pane->zip_pane_stretch_divisor);
    else
    if ( edge & zipview_pane_bottom_edge )
      y_offset =   (((zipview_Pane_Y_Origin( self, pane ) -
	pane->zip_pane_y_offset) - stream->zip_stream_least_y *
	((pane->zip_pane_zoom_level >= 0) ?
	 pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1)
	 :
	 pane->zip_pane_stretch_multiplier / abs(pane->zip_pane_zoom_level))
	 /
	 pane->zip_pane_stretch_divisor) - zipview_Pane_Height( self, pane ) + 1);
    if ( edge & zipview_pane_left_edge )
      x_offset = - ((zipview_Pane_X_Origin( self, pane ) +
	pane->zip_pane_x_offset) + stream->zip_stream_least_x *
	((pane->zip_pane_zoom_level >= 0) ?
	 pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1)
	 :
	 pane->zip_pane_stretch_multiplier / abs(pane->zip_pane_zoom_level))
         /
	 pane->zip_pane_stretch_divisor);
    else
    if ( edge & zipview_pane_right_edge )
      x_offset = - (((zipview_Pane_X_Origin( self, pane ) +
	pane->zip_pane_x_offset) + stream->zip_stream_greatest_x *
	((pane->zip_pane_zoom_level >= 0) ?
	 pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1)
	 :
	 pane->zip_pane_stretch_multiplier / abs(pane->zip_pane_zoom_level))
	 /
	 pane->zip_pane_stretch_divisor) - zipview_Pane_Width( self, pane ) + 1);
    Blit_Pane( self, pane, x_offset, y_offset );
    pane->zip_pane_x_offset += x_offset;
    pane->zip_pane_y_offset += y_offset;
    pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin + pane->zip_pane_x_offset;
    pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin - pane->zip_pane_y_offset;
/*===
    if ( pane->zip_pane_state.zip_pane_state_coordinates_exposed )
      ZIP_Draw_Pane_Coordinate_Marks( pane );
    zipview_FlushGraphics( self );
===*/
    zipview_Draw_Pane( self, pane );
    }
    else
    {
    status = zip_pane_non_existent;
    }
  zipview_FlushGraphics( self );
  OUT(zip_Pan_Pane_To_Edge);
  ZIP_STATUS();
  return status;
  }

static zip_type_pixel			  initial_x, initial_y,
					  hair_x, hair_y;


long
zipview__Handle_Panning( self, pane, x, y, x_delta, y_delta )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  x, y, x_delta, y_delta;
  {
return zip_ok;
  }

long
zipview__Initiate_Panning( self, pane, x, y, mode )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  x, y, mode;
  {
  IN(zipview__Initiate_Panning);
  zipview_Normalize_Line_Attributes( self );
  Set_Crosshairs( self, pane, initial_x = x, initial_y = y );
  Set_Crosshairs( self, pane, hair_x = x + 1, hair_y = y + 1 );
  OUT(zipview__Initiate_Panning);
  return zip_ok;
  }


long
zipview__Continue_Panning( self, pane, x, y )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  x, y;
  {
  register long				  precision =
				pane->zip_pane_panning_precision;

  IN(zipview__Continue_Panning);
  if ( x > hair_x + precision  ||  x < hair_x - precision  ||
       y > hair_y + precision  ||  y < hair_y - precision  )
    {
    Set_Crosshairs( self, pane, hair_x, hair_y );
    Set_Crosshairs( self, pane, hair_x = x, hair_y = y );
    }
  OUT(zipview__Continue_Panning);
  return zip_ok;
  }

long
zipview__Terminate_Panning( self, pane, x, y, x_delta, y_delta, draw )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  x, y;
  register long				 *x_delta, *y_delta;
  register long				  draw;
  {
  register long				  precision =
				pane->zip_pane_panning_precision;

  IN(zipview__Terminate_Panning);
  Set_Crosshairs( self, pane, initial_x, initial_y );
  Set_Crosshairs( self, pane, hair_x, hair_y );
  if ( x_delta )  *x_delta = 0;
  if ( y_delta )  *y_delta = 0;
  if ( x > initial_x + precision ||  x < initial_x - precision ||
       y > initial_y + precision ||  y < initial_y - precision  )
    {
    if ( draw )
      {
      zipview_Pan_Pane( self, pane, x - initial_x, -(y - initial_y) );
      if ( x_delta )
        *x_delta = pane->zip_pane_x_offset;
      if ( y_delta )
        *y_delta = pane->zip_pane_y_offset;
      }
      else
      {
      if ( x_delta )
	*x_delta = x - initial_x;
      if ( y_delta )
	*y_delta = y - initial_y;
      }
    }
  OUT(zipview__Terminate_Panning);
  return zip_ok;
  }

static
Set_Crosshairs( self, pane, x, y )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register int				  x, y;
  {
  IN(Set_Crosshairs);
  zipview_SetTransferMode( self, graphic_INVERT );
  zipview_MoveTo( self, zipview_Pane_Left( self, pane ), y );
  if ( zipview_GetLineWidth( self ) != 1 )
    zipview_SetLineWidth( self, 1 );
  zipview_DrawLineTo( self, zipview_Pane_Right( self, pane ), y );
  zipview_MoveTo( self, x, zipview_Pane_Top( self, pane ) );
  zipview_DrawLineTo( self, x, zipview_Pane_Bottom( self, pane ) );
  zipview_FlushGraphics( self );
  OUT(Set_Crosshairs);
  }

static int
Blit_Pane( self, pane, x_offset, y_offset )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register int				  x_offset, y_offset;
  {
  struct rectangle			  rectangle;
  struct point				  point;
  register struct graphic		 *graphic_op;

  IN(Blit_Pane);
  zipview_SetTransferMode( self, graphic_COPY );
  rectangle.left =
	(x_offset > 0) ?
	   zipview_Pane_Left( self, pane ) + BorderThickness
	   :
	   zipview_Pane_Left( self, pane ) + BorderThickness + abs(x_offset);
  rectangle.top = 
	(y_offset > 0) ?
	   zipview_Pane_Top(self,  pane )  + BorderThickness + abs(y_offset)
	   :
	   zipview_Pane_Top( self, pane )  + BorderThickness;
  point.x =
	(x_offset > 0) ?
	   zipview_Pane_Left( self, pane ) + BorderThickness + abs(x_offset)
	   :
	   zipview_Pane_Left( self, pane ) + BorderThickness;
  point.y =
	(y_offset > 0) ?
	   zipview_Pane_Top( self, pane )  + BorderThickness
	   :
	   zipview_Pane_Top( self, pane )  + BorderThickness + abs(y_offset);
  rectangle.width =
	 zipview_Pane_Width( self, pane )  - (abs(x_offset) + (2 * BorderThickness) );
  rectangle.height =
	 zipview_Pane_Height( self, pane ) - (abs(y_offset) + (2 * BorderThickness) );
  zipview_BitBlt( self, &rectangle, self, &point, NULL );
  if ( pane->zip_pane_state.zip_pane_state_inverted )
    {
    zipview_SetTransferMode( self, graphic_BLACK );
    graphic_op = zipview_BlackPattern( self );
    }
    else
    {
    zipview_SetTransferMode( self, graphic_WHITE );
    graphic_op = zipview_WhitePattern( self );
    }
  zipview_FillRectSize( self, 
	(x_offset > 0) ?
	   zipview_Pane_Left( self, pane ) + BorderThickness
	   :
	   zipview_Pane_Right( self, pane ) - (BorderThickness + abs(x_offset)),
	zipview_Pane_Top( self, pane ) + BorderThickness,
	abs(x_offset),
	zipview_Pane_Height( self, pane ) - 2 * BorderThickness, graphic_op );
  zipview_FillRectSize( self, 
	zipview_Pane_Left( self, pane ) + BorderThickness,
	(y_offset > 0) ?
	   zipview_Pane_Bottom( self, pane ) - (BorderThickness + abs(y_offset))
	   :
	   zipview_Pane_Top( self, pane ) + BorderThickness,
	zipview_Pane_Width( self, pane ) - 2 * BorderThickness,
	abs(y_offset), graphic_op );
  zipview_SetTransferMode( self, graphic_BLACK );
  OUT(Blit_Pane);
  return zip_success;
  }
