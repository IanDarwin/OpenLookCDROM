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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipobj.c,v 1.5 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipobj.c	Zip Object -- Super-class				      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipobj.c,v $
 * Revision 1.5  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.4.1.1  1993/02/02  06:57:22  rr2b
 * new R6tape branch
 *
 * Revision 1.4  1993/01/08  16:36:06  rr2b
 * cutting down on duplicate global symbols
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
 * Revision 2.11  1991/09/12  16:42:30  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.10  1990/08/21  14:26:33  sg08
 * Add Contains stub. Remove Object_Visible stub
 *
 * Revision 2.9  90/07/12  16:04:02  sg08
 * fix Object_Point to return at least SOME information (ie the first coordinate)
 * 
 * Revision 2.8  89/09/11  08:17:15  ghoti
 * fix enumeration type clashes - specifically those dealing with mouse actions
 * 
 * Revision 2.7  89/09/08  17:41:33  ghoti
 * removal of unused variables
 * 
 * Revision 2.6  89/08/23  16:33:55  tom
 * Override GetModified to check for changes to Imbedded objects
 * 
 * Revision 2.5  89/07/20  13:12:04  sg08
 * introduced Object_Visible method
 * 
 * Revision 2.4  89/05/01  22:13:52  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.3  89/02/08  16:50:03  ghoti
 * change copyright notice
 * 
 * Revision 2.2  89/02/07  19:25:53  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.1  88/09/27  18:14:25  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:34:48  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:44:39  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Super-class

MODULE	zipobj.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  07/12/89	Added Object_Visible stub (SCG)
  08/07/89	Add Object_Modified stub (TCP)
  07/12/90	Have Object_Point return the first coordinate (SCG)
   08/14/90	Add Contains stub. Remove Object_Visible stub (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "zip.ih"
#include "zipv.ih"
#include "zipedit.ih"
#include "zipprint.ih"
#include "zipobj.eh"

static boolean debug=FALSE;

#define  Data				(self->data_object)
#define  View				(self->view_object)
#define  Edit				(self->edit_object)
#define  Print				(self->print_object)


boolean
zipobject__InitializeObject( classID, self )
  register struct classheader	         *classID;
  register struct zipobject	         *self;
  {
/*===debug=1;===*/
  IN(zipobject__InitializeObject);
  Data  = NULL;
  View  = NULL;
  Edit  = NULL;
  Print = NULL;
  OUT(zipobject__InitializeObject);
  return  true;
  }

void
zipobject__Destroy_Object( self, object )
  register struct zipobject	         *self;
  register zip_type_figure		  object;
  {
  IN(zipobject__Destroy_Object);
  /*=== NULL ===*/
  OUT(zipobject__Destroy_Object);
  }

void
zipobject__Set_Data_Object( self, data_object )
  register struct zipobject	         *self;
  register struct zip		         *data_object;
  {
  IN(zipobject__Set_Data_Object);
  Data = data_object;
  OUT(zipobject__Set_Data_Object);
  }

void
zipobject__Set_View_Object( self, view_object )
  register struct zipobject		 *self;
  register struct zipview	         *view_object;
  {
  IN(zipobject__Set_View_Object);
  View = view_object;
  OUT(zipobject__Set_View_Object);
  }

void
zipobject__Set_Edit_Object( self, edit_object )
  register struct zipobject		 *self;
  register struct zipedit	         *edit_object;
  {
  IN(zipobject__Set_Edit_Object);
  Edit = edit_object;
  OUT(zipobject__Set_Edit_Object);
  }

void
zipobject__Set_Print_Object( self, print_object )
  register struct zipobject		 *self;
  register struct zipprint	         *print_object;
  {
  IN(zipobject__Set_Print_Object);
  Print = print_object;
  OUT(zipobject__Set_Print_Object);
  }

void
zipobject__Set_Debug( self, state )
  register struct zipobject		 *self;
  register char				  state;
  {
  IN(zipobject__Set_Debug);
  debug = state;
  OUT(zipobject__Set_Debug);
  }

long
zipobject__Object_Modified( self, object )
  register struct zipobject	 *self;
  register zip_type_figure	  object;
  {
  IN(zipobject_Object_Modified);
  OUT(zipobject_Object_Modified);
  return  0;
  }

enum view_DSattributes
zipobject__Object_DesiredSize( self, given_width, given_height,
		        pass, desired_width, desired_height )
  register struct zipobject	     *self;
  register long			      given_width, given_height;
  register enum view_DSpass	      pass;
  register long			     *desired_width, *desired_height;
  {
  IN(zipobject__Object_DesiredSize);
  /*===*/
  OUT(zipobject__Object_DesiredSize);
  return  0;
  }

void 
zipobject__Object_FullUpdate( self, type, left, top, width, height )
  register struct zipobject	     *self;
  register enum view_UpdateType	      type;
  register long			      left, top, width, height;
  {
  IN(zipobject__Object_FullUpdate);
  /*=== NULL ===*/
  OUT(zipobject__Object_FullUpdate);
  }

void 
zipobject__Object_Update( self )
  register struct zipobject	   *self;
  {
  IN(zipobject__Object_Update);
  /*=== NULL ===*/
  OUT(zipobject__Object_Update);
  }

struct view *
zipobject__Object_Hit( self, figure, action, x, y, clicks )
  register struct zipobject	     *self;
  register zip_type_figure	      figure;
  register enum view_MouseAction      action;
  register long			      x, y, clicks;
  {
  IN(zipobject__Object_Hit );
  /*=== NULL ===*/
  OUT(zipobject__Object_Hit );
  return  NULL;
  }

char
zipobject__Object_Icon( self )
  register struct zipobject		 *self;
  {
  IN(zipobject__Object_Icon);
  /****   NULL  ****/
  OUT(zipobject__Object_Icon);
  return  NULL;
  }

char *
zipobject__Object_Icon_Font_Name( self )
  register struct zipobject		 *self;
  {
  IN(zipobject__Object_Icon_Font_Name);
  OUT(zipobject__Object_Icon_Font_Name);
  return  IconFontName;
  }

char
zipobject__Object_Icon_Cursor( self )
  register struct zipobject		 *self;
  {
  IN(zipobject__Object_Icon_Cursor);
  /****   NULL  ****/
  OUT(zipobject__Object_Icon_Cursor);
  return  NULL;
  }

char *
zipobject__Object_Icon_Cursor_Font_Name( self )
  register struct zipobject		 *self;
  {
  IN(zipobject__Object_Icon_Cursor_Font_Name);
  OUT(zipobject__Object_Icon_Cursor_Font_Name);
  return  CursorFontName;
  }

char
zipobject__Object_Datastream_Code( self )
  register struct zipobject		 *self;
  {
  IN(zipobject__Object_Datastream_Code);
  /****   NULL  ****/
  OUT(zipobject__Object_Datastream_Code);
  return  NULL;
  }

long
zipobject__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipobject		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y;
  register zip_type_point		  X, Y;
  {
  IN(zipobject__Build_Object);
  /****   NULL  ****/
  OUT(zipobject__Build_Object);
  return zip_failure;
  }

long
zipobject__Show_Object_Properties( self, pane, figure )
  register struct zipobject		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  IN(zipobject__Show_Object_Properties);
  /****   NULL  ****/
  OUT(zipobject__Show_Object_Properties);
  return  zip_ok;
  }

long
zipobject__Read_Object( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status;

  IN(zipobject__Read_Object);
  status = zip_Read_Figure( Data, figure );
  OUT(zipobject__Read_Object);
  return  status;
  }

long
zipobject__Read_Object_Stream( self, figure, file, id )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register FILE				 *file;
  register long				  id;
  {
  IN(zipobject__Read_Object_Stream);
  /****   NULL  ****/
  OUT(zipobject__Read_Object_Stream);
  return  zip_failure;
  }

long
zipobject__Write_Object( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  register long				  status = zip_ok;

  IN(zipobject__Write_Object);
  status = zip_Write_Figure( Data, figure );
  OUT(zipobject__Write_Object);
  return  status;
  }

long
zipobject__Draw_Object( self, figure, pane )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  IN(zipobject__Draw_Object);
  /****   NULL  ****/
  OUT(zipobject__Draw_Object);
  return zip_failure;
  }

long
zipobject__Clear_Object( self, figure, pane )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  IN(zipobject__Clear_Object);
  /****   NULL  ****/
  OUT(zipobject__Clear_Object);
  return zip_failure;
  }

long
zipobject__Print_Object( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  IN(zipobject__Print_Object);
  /****   NULL  ****/
  OUT(zipobject__Print_Object);
  return zip_failure;
  }

long
zipobject__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  IN(zipobject__Proximate_Object_Points);
  /****   NULL  ****/
  OUT(zipobject__Proximate_Object_Points);
  return zip_failure;
  }

long
zipobject__Within_Object( self, figure, pane, x, y )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  IN(zipobject__Within_Object);
  /****   NULL   ****/
  OUT(zipobject__Within_Object);
  return  -1;
  }

boolean
zipobject__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  IN(zipobject__Enclosed_Object);
  /****   NULL   ****/
  OUT(zipobject__Enclosed_Object);
  return  false;
  }

long
zipobject__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  *x, *y, *w, *h;
  {
  IN(zipobject__Object_Enclosure);
  /****   NULL   ****/
  *x = *y = *w = *h = 0;
  OUT(zipobject__Object_Enclosure);
  return  zip_failure;
  }

long
zipobject__Highlight_Object_Points( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  IN(zipobject__Highlight_Object_Points);
  /****   NULL  ****/
  OUT(zipobject__Highlight_Object_Points);
  return zip_failure;
  }

long
zipobject__Normalize_Object_Points( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  IN(zipobject__Normalize_Object_Points);
  /****   NULL  ****/
  OUT(zipobject__Normalize_Object_Points);
  return zip_failure;
  }

long
zipobject__Expose_Object_Points( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  IN(zipobject__Expose_Object_Points);
  /****   NULL  ****/
  OUT(zipobject__Expose_Object_Points);
  return zip_failure;
  }

long
zipobject__Hide_Object_Points( self, figure )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  {
  IN(zipobject__Hide_Object_Points);
  /****   NULL  ****/
  OUT(zipobject__Hide_Object_Points);
  return zip_failure;
  }

long
zipobject__Set_Object_Shade( self, figure, shade )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register long				  shade;
  {
  IN(zipobject__Set_Object_Shade);
  /****   NULL  ****/
  OUT(zipobject__Set_Object_Shade);
  return  zip_failure;
  }

long
zipobject__Set_Object_Font( self, figure, font )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register short			  font;
  {
  IN(zipobject__Set_Object_Font);
  /****   NULL  ****/
  OUT(zipobject__Set_Object_Font);
  return  zip_failure;
  }

/*  Generic Object Point Setting

    Object-points are "set" by specifying a "Handle" and the x/y coordinate
    at which the Handle is to be placed. Handles form a rectangular perimeter 
    that govern the position of the figure bounded by the Handles. Handles
    are designated thus --

	9 ***** 2 ***** 3
	*		*
	*		*
	8	1	4
	*		*
	*		*
	7 ***** 6 ***** 5

    (that is, Handle-1 is the centroid, Handle-2 is at Noon, Handle-3 at
    the Upper-Right corner, Handle-4 at 3 O'clock, etc -- proceeding in a
    clockwise direction). Creation of a figure entails setting of the
    appropriate Handles; eg, creation of a Rectangle figure is usually
    effected by setting Handle-9 and Handle-5, the Upper-left and the Lower-right
    Handles.

*/

long
zipobject__Set_Object_Point( self, figure, point, x, y )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register long				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;

  IN(zipobject__Set_Object_Point);
  if ( figure->zip_figure_points == NULL  &&
       (status = zip_Allocate_Figure_Points_Vector(
		    Data, &figure->zip_figure_points )) == zip_ok )
    figure->zip_figure_points->zip_points_count = 1;
  if ( status == zip_ok )
    {
    switch ( point )
      {
      case 1:	/* Center */
	zipobject_Adjust_Object_Point_Suite( self, figure, 
		x - (figure_x_point + (figure_x_points(0) - figure_x_point)/2),
		y - (figure_y_point - (figure_y_point - figure_y_points(0))/2) );
	break;
      case 2:	/* 12 O'Clock */
	figure_y_points(0) += figure_y_point - y;
	figure_y_point = y;
	break;
      case 3:	/* Upper-Right Corner */
	figure_x_points(0) = x;
	figure_y_point = y;
	break;
      case 4:	/* 3 O'Clock */
	figure_x_point += figure_x_points(0) - x;
	figure_x_points(0) = x;
	break;
      case 5:	/* Lower-Right Corner */
	figure_x_points(0) = x;
	figure_y_points(0) = y;
	break;
      case 6:	/* 6 O'Clock */
	figure_y_point += figure_y_points(0) - y;
	figure_y_points(0) = y;
	break;
      case 7:	/* Lower-Left Corner */
	figure_x_point = x;
	figure_y_points(0) = y;
	break;
      case 8:	/* 9 O'Clock */
	figure_x_points(0) += figure_x_point - x;
	figure_x_point = x;
	break;
      case 9:	/* Upper-Left Corner */
	figure_x_point = x;
	figure_y_point = y;
	break;
      default: status = zip_failure; /*=== s/b zip_invalid_point ===*/
      }
    if ( status == zip_ok )
      {
      zip_Set_Image_Extrema( Data, figure->zip_figure_image, x, y );
      zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			      figure->zip_figure_image );
      }
    }
  OUT(zipobject__Set_Object_Point);
  return  status;
  }

long
zipobject__Object_Point( self, figure, point, x, y )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register long				  point;
  register zip_type_point		  *x, *y;
  {
  IN(zipobject__Object_Point);
  if ( figure )
    { /* just return the first coordinate */
    *x = figure_x_point;
    *y = figure_y_point;
    }
  else *x = *y = 0;
  OUT(zipobject__Object_Point);
  return zip_failure;
  }

long
zipobject__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipobject		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipobject__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  figure_x_points(0) += x_delta;
  figure_y_points(0) += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipobject__Adjust_Object_Point_Suite);
  return  status;
  }

boolean
zipobject__Contains( self, figure, pane, x, y )
  register struct zipobject		*self;
  register zip_type_figure		figure;
  register zip_type_pane		pane;
  register zip_type_pixel		x, y;

  {
    return FALSE;
  }
