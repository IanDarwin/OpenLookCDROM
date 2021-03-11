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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/ziporang.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* ziporang.c	Zip Object -- Roundangle					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: ziporang.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:00:33  rr2b
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
 * Revision 2.11  1991/09/12  16:43:37  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.10  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.9  1990/08/21  14:34:23  sg08
 * Modify Build to handle non-refresh of pane on build completion
 *
 * Revision 2.8  89/09/08  17:42:26  ghoti
 * removal of unused variables
 * 
 * Revision 2.7  89/08/30  16:28:14  sg08
 * Removed excess SetTransferMode activity. Accomodate non-refresh on builds.
 * 
 * Revision 2.6  89/05/01  22:14:47  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.5  89/02/08  16:51:15  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  20:00:42  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/18  21:12:20  tom
 * Handle variable line-widths.
 * 
 * Revision 2.2  88/10/11  20:32:47  tom
 * Change Printing interface to remove pane and figure args.
 * 
 * Revision 2.1  88/09/27  18:17:21  ghoti
 * adjusting rcs #
 * 
 * Revision 1.3  88/09/17  23:03:43  dba
 * simplified expressions that were too complex
 * for the Vax compiler
 * 
 * Revision 1.2  88/09/15  17:43:02  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:46:05  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Roundangle

MODULE	ziporang.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)
  08/24/89	Remove excess SetTransferMode() activity in Draw() (SCG)
   08/28/89	Modify Build to handle non-refresh of pane on build completion (SCG)

END-SPECIFICATION  ************************************************************/

#include <class.h>
#include <view.ih>
#include <zipobj.ih>
#include <ziporang.eh>


static Draw();

char
ziporang__Object_Icon( self )
  register struct ziporang		 *self;
  {
  IN(ziporang__Object_Icon);
  OUT(ziporang__Object_Icon);
  return  'I';
  }

char
ziporang__Object_Icon_Cursor( self )
  register struct ziporang		 *self;
  {
  IN(ziporang__Object_Icon_Cursor);
  OUT(ziporang__Object_Icon_Cursor);
  return  'S';
  }

char
ziporang__Object_Datastream_Code( self )
  register struct ziporang		 *self;
  {
  IN(ziporang__Object_Datastream_Code);
  OUT(ziporang__Object_Datastream_Code);
  return  'N';
  }

long
ziporang__Show_Object_Properties( self, pane, figure )
  register struct ziporang		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw RoundAngle from Upper-left to Lower-right." );
  return  zip_ok;
  }

long
ziporang__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct ziporang		 *self;
  register zip_type_pane		  pane;
  register long				  action, x, y, clicks;
  register zip_type_point		  X, Y;
  {
  register long				  status = zip_ok;
  int					  position = 0; /*===*/
  register zip_type_figure		  figure;

  IN(ziporang__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
        zip_Create_Figure( Data, &CurrentFigure, NULL, zip_roundangle_figure,
			 CurrentImage, position )) == zip_success )
	{
        ziporang_Set_Object_Point( self, CurrentFigure,	zip_figure_origin_point, X, Y );
        ziporang_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
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
	if ( figure_x_point == figure_x_points(0)  &&
	     figure_y_point == figure_y_points(0) )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane ); 
	  }
          else
	  {
	  zipview_Draw_Figure( View, CurrentFigure, pane );
	  }
	}
        break;
    case view_LeftMovement:
      if ( CurrentFigure )
	{
	zipview_Draw_Figure( View, CurrentFigure, pane );
        ziporang_Set_Object_Point( self, CurrentFigure, zip_figure_auxiliary_point, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	}
      break;
    }
  OUT(ziporang__Build_Object);
  return  status;
  }

long
ziporang__Draw_Object( self, figure, pane )
  register struct ziporang		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane, zip_draw );
  OUT(ziporang__Draw_Object);
  return  status;
  }

long
ziporang__Clear_Object( self, figure, pane )
  register struct ziporang		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(ziporang__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane, zip_clear );
  OUT(ziporang__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane, action )
  register struct ziporang		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register long				  action;
  {
  register long				  status = zip_ok;
  register long				  radius, left, top, width, height;
  register unsigned char		  pattern = NULL, shade;

  IN(Draw);
  left   = window_x_point;		top    = window_y_point;
  width  = window_x_points(0) - left;	height = window_y_points(0) - top;
  if ( width < 0 )
    { left = window_x_points(0);  width = -width; }
  if ( height < 0 )
    { top = window_y_points(0);   height = -height; }
  if ( abs(figure_x_point - figure_x_points(0)) >
       abs(figure_y_point - figure_y_points(0)) )
    radius =  abs(window_y_point - window_y_points(0) ) >> 2;
    else
    radius =  abs(window_x_point - window_x_points(0) ) >> 2;
  if ( figure->zip_figure_mode.zip_figure_mode_shaded  ||
       figure->zip_figure_mode.zip_figure_mode_patterned )
    {
    if ( View->mouse_action != view_LeftMovement  &&  action == zip_draw )
      {
      if ( figure->zip_figure_mode.zip_figure_mode_patterned  &&
	   (pattern = zip_Contextual_Figure_Pattern( Data, figure )) )
        { DEBUGct(Pattern,pattern);
	zipview_FillTrapezoid(View, left,top,width, left,top+height+1,width+1,
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
	  zipview_FillRRectSize( View, left, top, width+1, height+1, radius, radius,
				 zipview_Define_Graphic( View,zip_Define_Font(
				    Data, ShadeFontName, NULL ), shade ) );
	  }
      }
      else
      if ( action == zip_clear )
	{
	zipview_FillRRectSize( View, left, top, width, height, radius, radius, graphic_WHITE );
	}
    }
  if ( zipview_Ensure_Line_Attributes( View, figure ) == zip_ok )
    zipview_DrawRRectSize( View, left, top, width, height, radius, radius );
  if ( ExposePoints )	    ziporang_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )    ziporang_Highlight_Object_Points( self, figure, pane );
  OUT(Draw);
  return  status;
  }

long
ziporang__Print_Object( self, figure, pane )
  register struct ziporang		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register long				  radius, left, right, top, bottom;
  long                                    temp1, temp2;

  IN(ziporang__Print_Object);
  left = print_x_point;       top = print_y_point;
  right = print_x_points(0);  bottom = print_y_points(0);
  if ( right < left )
    { DEBUG(X-Flipped);
    left = right;
    right = print_x_point;
    }
  if ( bottom < top )
    { DEBUG(Y-Flipped);
    top = bottom;
    bottom = print_y_point;
    }
  if ( abs(figure_x_point - figure_x_points(0)) >
       abs(figure_y_point - figure_y_points(0)) ) {
    temp1 = print_y_point;
    temp2 = print_y_points(0);
    radius =  abs(temp1 - temp2) >> 2;
  }
  else  {
      temp1 = print_x_point;
      temp2 = print_x_points(0);
      radius =  abs(temp1 - temp2) >> 2;
  }
  zipprint_Set_Shade( Print, figure->zip_figure_fill.zip_figure_shade ); 
  zipprint_Ensure_Line_Attributes( Print, figure );
  zipprint_Draw_Round_Rectangle( Print, left, top, right, bottom, radius, radius );
  OUT(ziporang__Print_Object);
  return  status;
  }
