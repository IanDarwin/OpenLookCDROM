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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipoplin.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipoplin.c	Zip Object -- Ploy-Line					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipoplin.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:59:56  rr2b
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
 * Revision 2.8  1991/09/12  16:43:23  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.7  1989/09/11  08:17:34  ghoti
 * fix enumeration type clashes - specifically those dealing with mouse actions
 *
 * Revision 2.6  89/09/08  17:42:16  ghoti
 * removal of unused variables
 * 
 * Revision 2.5  89/08/30  16:27:21  sg08
 * Accomodate non-refresh on builds.
 * 
 * Revision 2.4  89/02/08  16:51:04  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/07  19:55:36  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/10/11  20:33:26  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:16:54  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:41:32  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:45:50  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- PolyLine

MODULE	zipoplin.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "zipobj.ih"
#include "zipoplin.eh"
#include "environ.ih"

boolean
zipoplin__InitializeObject( classID, self )
  register struct classheader	         *classID;
  register struct zipoplin	         *self;
  {
  IN(zipoplin_InitializeObject);
  self->tolerance = environ_GetProfileInt( "ZipCreateTolerance", 10 );
  DEBUGdt(Tolerance,self->tolerance);
  OUT(zipoplin_InitializeObject);
  return  true;
  }

char
zipoplin__Object_Icon( self )
  register struct zipoplin		 *self;
  {
  IN(zipoplin__Object_Icon);
  OUT(zipoplin__Object_Icon);
  return  'D';
  }


char
zipoplin__Object_Icon_Cursor( self )
  register struct zipoplin		 *self;
  {
  IN(zipoplin__Object_Icon_Cursor);
  OUT(zipoplin__Object_Icon_Cursor);
  return  'J';
  }

char
zipoplin__Object_Datastream_Code( self )
  register struct zipoplin		 *self;
  {
  IN(zipoplin__Object_Datastream_Code);
  OUT(zipoplin__Object_Datastream_Code);
  return  'D';
  }

long
zipoplin__Show_Object_Properties( self, pane, figure )
  register struct zipoplin		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw PolyLine in Segments." );
  return  zip_ok;
  }

long
zipoplin__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipoplin		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  long					  position = 0;
  static long				  initial_x, initial_y,
					  initial_X, initial_Y,
					  prior_X, prior_Y,
					  prior_x, prior_y;

  IN(zipoplin__Build_Object);
  if ( action == (long)view_LeftDown  &&
       (abs(x - prior_x) < self->tolerance  &&  abs(y - prior_y) < self->tolerance) )
    {
    action = NULL;
    CurrentFigure = NULL;
    prior_x = prior_y = prior_X = prior_Y = initial_x = initial_y = initial_X = initial_Y = 0;
    pane->zip_pane_edit->zip_pane_edit_build_figure = true;
    }
  switch ( action )
    {
    case view_LeftDown:
      prior_x = x;  prior_y = y;
      zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
      if ( pane->zip_pane_edit->zip_pane_edit_build_figure )
	{
        initial_x = x;  initial_y = y;
        initial_X = prior_X = X;  initial_Y = prior_Y = Y;
	if ( (status = zip_Create_Figure( Data, &CurrentFigure, NULL,
		zip_poly_line_figure, CurrentImage, position )) == zip_success )
          {
          zipoplin_Set_Object_Point( self, CurrentFigure,
		zip_figure_origin_point, X, Y );
          zipoplin_Set_Object_Point( self, CurrentFigure,
		zip_figure_auxiliary_point, X, Y );
  	  CurrentFigure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
	  pane->zip_pane_edit->zip_pane_edit_build_figure = false;
	  pane->zip_pane_edit->zip_pane_edit_last_point_id = zip_figure_auxiliary_point;
	  zip_Set_Figure_Shade( Data, CurrentFigure,
			      pane->zip_pane_edit->zip_pane_edit_current_shade );
	  }
	}
	else
	{
	if ( X != prior_X  ||  Y != prior_Y )
	  {
	  if ( abs(initial_x - x) <= self->tolerance  &&  abs(initial_y - y) <= self->tolerance )
	    { DEBUG(Close Path);
	    X = initial_X;  Y = initial_Y;
	    }
          zipoplin_Set_Object_Point( self, CurrentFigure,
		   ++pane->zip_pane_edit->zip_pane_edit_last_point_id,
		   prior_X = X, prior_Y = Y );
	  zipview_Draw_Figure( View, CurrentFigure, pane );
/*	  if ( X == initial_X && Y == initial_Y )
	    pane->zip_pane_edit->zip_pane_edit_build_figure = true; */
	  }
	}
      break;
    case view_LeftUp:
      if ( CurrentFigure )
	{
        prior_x = x;  prior_y = y;
        zipview_Draw_Figure( View, CurrentFigure, pane );
	}
      break;
    case view_LeftMovement:
      if ( CurrentFigure  &&  !pane->zip_pane_edit->zip_pane_edit_build_figure  )
	{
        zipview_Set_Pane_Painting_Mode( View, pane, zipview_paint_inverted );
	zipview_Draw_Figure( View, CurrentFigure, pane );
        zipoplin_Set_Object_Point( self, CurrentFigure,
		     pane->zip_pane_edit->zip_pane_edit_last_point_id, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );

	}
      break;
    }
  zipview_Set_Pane_Painting_Mode( View, pane, zip_default );
  OUT(zipoplin__Build_Object);
  return  status;
  }
