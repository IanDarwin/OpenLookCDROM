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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipve00.c,v 1.5 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipve00.c	Zip EditView-object				      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipve00.c,v $
 * Revision 1.5  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:03:34  rr2b
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
 * Revision 2.9  1991/09/12  16:44:45  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.8  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.7  1989/08/30  16:33:49  sg08
 * Flag Figures regardless of visibility on Highlight/Normalize
 *
 * Revision 2.6  89/06/08  18:56:59  tom
 * Use Drawable instead of View in fontdesc_StringSize.
 * 
 * Revision 2.5  89/02/17  18:09:45  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.4  89/02/08  16:52:18  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/07  20:40:55  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/11/16  18:43:06  tom
 * Optimize point Highlight/Normalize drawing.
 * 
 * Revision 2.1  88/09/27  18:19:45  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:49:37  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:47:30  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip EditView-object

MODULE	zipve00.c

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
  10/26/88	Optimize point Highlight/Normalize (TCP)
  06/08/89	Use Drawable in fontdesc_StringSize (TCP)
   08/22/89	Flag Figures regardless of visibility on Highlight/Normalize (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "im.ih"
#include "fontdesc.ih"
#include "zip.ih"
#include "zipv.ih"
#include "zipobj.ih"
#include "zipedit.ih"
#include "zipedit.h"

static int Delete_Inferior_Image( );
static int Undelete_Inferior_Image();
static int Normalize_Inferior_Image_Points();

long
zipedit__Set_Pane_Highlight_Icon( self, pane, icon )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register char				  icon;
  {
  register long				  status = zip_ok;

  IN(zipedit__Set_Pane_Highlight_Icon);
  if ( pane )
    pane->zip_pane_highlight_icon = icon;
    else 
    status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Set_Pane_Highlight_Icon);
  return status;
  }

long
zipedit__Highlight_Pane_Points( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Normalize_Pane_Points( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Hide_Pane_Points( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Hide_Pane_Points);
  if ( pane )
    {
    if ( pane->zip_pane_state.zip_pane_state_points_exposed )
      {
      pane->zip_pane_state.zip_pane_state_points_exposed = false;
      status = zipview_Display_Pane( View, pane );
      }
      else  status = zip_pane_not_exposed; /*=== pane_points ===*/
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Hide_Pane_Points);
  return status;
  }

long
zipedit__Expose_Pane_Points( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Expose_Pane_Points);
  if ( pane )
    {
    if ( ! pane->zip_pane_state.zip_pane_state_points_exposed )
      {
      pane->zip_pane_state.zip_pane_state_points_exposed = true;
      status = zipview_Draw_Pane( View, pane );
      }
      else status = zip_pane_not_hidden; /*=== pane_points ===*/
    } 
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Expose_Pane_Points);
  return status;
  }

long
zipedit__Expose_Pane_Grid( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Expose_Pane_Grid);
  if ( pane )
    {
    if ( pane->zip_pane_edit->zip_pane_edit_coordinate_grid == 0 )
      {
      pane->zip_pane_edit->zip_pane_edit_coordinate_grid = 1;
      status = zipedit_Draw_Pane_Grid( self, pane );
      }
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Expose_Pane_Grid);
  return status;
  }

long
zipedit__Hide_Pane_Grid( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Hide_Pane_Grid);
  if ( pane )
    {
    if ( pane->zip_pane_edit->zip_pane_edit_coordinate_grid > 0 )
      {
      pane->zip_pane_edit->zip_pane_edit_coordinate_grid = 0;
      status = zipview_Display_Pane( View, pane );
      }
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Hide_Pane_Grid);
  return status;
  }

long
zipedit__Halve_Pane_Grid( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Halve_Pane_Grid);
  if ( pane )
    {
    if ( pane->zip_pane_edit->zip_pane_edit_coordinate_grid >= 2 )
      {
      pane->zip_pane_edit->zip_pane_edit_coordinate_grid /= 2;
      status = zipview_Display_Pane( View, pane );
      }
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Halve_Pane_Grid);
  return status;
  }

long
zipedit__Double_Pane_Grid( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Double_Pane_Grid);
  if ( pane )
    {
    if ( pane->zip_pane_edit->zip_pane_edit_coordinate_grid > 0 )
      {
      pane->zip_pane_edit->zip_pane_edit_coordinate_grid *= 2;
      status = zipedit_Draw_Pane_Grid( self, pane );
      }
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Double_Pane_Grid);
  return status;
  }

long
zipedit__Expose_Pane_Coordinates( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Expose_Pane_Coordinates);
  if ( pane )
    {
    if ( ! pane->zip_pane_state.zip_pane_state_coordinates_exposed )
      {
      pane->zip_pane_state.zip_pane_state_coordinates_exposed = true;
      pane->zip_pane_preserved_border_thickness = pane->zip_pane_border_thickness;
      pane->zip_pane_border_thickness = ZIP_pane_coordinate_thickness;
      status = zipview_Display_Pane( View, pane );
      }
    } 
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Expose_Pane_Coordinates);
  return status;
  }

long
zipedit__Hide_Pane_Coordinates( self, pane )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Hide_Pane_Coordinates);
  if ( pane )
    {
    if ( pane->zip_pane_state.zip_pane_state_coordinates_exposed )
      {
      pane->zip_pane_state.zip_pane_state_coordinates_exposed = false;
      pane->zip_pane_border_thickness = pane->zip_pane_preserved_border_thickness;
      status = zipview_Display_Pane( View, pane );
      }
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Hide_Pane_Coordinates);
  return status;
  }

long
zipedit__Delete_Figure( self, figure, pane )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status= zip_ok;

  IN(zipedit__Delete_Figure);
  if ( figure )
    {
    if ( pane  &&  figure->zip_figure_visibility == zip_figure_exposed )
      {
      zipview_Set_Pane_Clip_Area( View, pane );
      if ( figure->zip_figure_state.zip_figure_state_points_highlighted )
        zipedit_Normalize_Figure_Points( self, figure, pane );
      status = zipobject_Clear_Object( Objects(figure->zip_figure_type), figure, pane );
      }
    figure->zip_figure_visibility = zip_figure_hidden;
    figure->zip_figure_state.zip_figure_state_deleted = true;
    if ( figure == CurrentFigure )  CurrentFigure = NULL;
    zip_SetModified( Data );
    figure->zip_figure_image->zip_image_stream->zip_stream_states.zip_stream_state_modified = 1;
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Delete_Figure);
  return status;
  }

long
zipedit__Undelete_Figure( self, figure, pane )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
   register long				  status= zip_ok;

  IN(zipedit__Undelete_Figure);
  if ( figure )
    {
    if ( pane  &&  figure->zip_figure_visibility == zip_figure_hidden )
      {
      zipview_Set_Pane_Clip_Area( View, pane );
      status = zipobject_Draw_Object( Objects(figure->zip_figure_type), figure, pane );
      figure->zip_figure_visibility = zip_figure_exposed;
      }
    figure->zip_figure_state.zip_figure_state_deleted = false;
    }
    else  status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Undelete_Figure);
  return status;
 }

long
zipedit__Which_Figure_Point( self, figure, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		 *pane;
  register long				  x, y;
  {
  register long				  point = 0;
  register long				  status = zip_ok;

  IN(zipedit__Which_Figure_Point);
  if ( figure )
    {
    DEBUGst(Figure-name,figure->zip_figure_name);
    DEBUGdt(Figure-type,figure->zip_figure_type);
    point = zipobject_Proximate_Object_Points( Objects(figure->zip_figure_type),
					       figure, pane, x, y );
    }
  OUT(zipedit__Which_Figure_Point);
  ZIP_STATUS();
  return point;
  }

long
zipedit__Highlight_Figure_Points( self, figure, pane )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Highlight_Figure_Points);
  if ( pane  &&  figure  &&  figure->zip_figure_visibility == zip_figure_exposed  &&
       ! figure->zip_figure_state.zip_figure_state_points_highlighted )
    { 
    zipview_Set_Pane_Clip_Area( View, pane );
    status = zipobject_Highlight_Object_Points( Objects(figure->zip_figure_type), figure, pane );
    }
  if ( figure ) figure->zip_figure_state.zip_figure_state_points_highlighted = true;
  ZIP_STATUS();
  OUT(zipedit__Highlight_Figure_Points);
  return  status;
  }

long
zipedit__Normalize_Figure_Points( self, figure, pane )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Normalize_Figure_Points);
  if ( pane  &&  figure  &&  figure->zip_figure_visibility == zip_figure_exposed  &&
       figure->zip_figure_state.zip_figure_state_points_highlighted )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    status = zipobject_Normalize_Object_Points( Objects(figure->zip_figure_type), figure, pane );
    }
  if ( figure ) figure->zip_figure_state.zip_figure_state_points_highlighted = false;
  ZIP_STATUS();
  OUT(zipedit__Normalize_Figure_Points);
  return zip_ok;
  }

long
zipedit__Hide_Figure_Points( self, figure, pane )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Hide_Figure_Points);
  if ( pane  &&  figure  &&  figure->zip_figure_state.zip_figure_state_points_exposed )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    figure->zip_figure_state.zip_figure_state_points_exposed = false;
    zipview_Display_Pane( View, pane );
    }
  ZIP_STATUS();
  OUT(zipedit__Hide_Figure_Points);
  return zip_ok;
  }

long
zipedit__Expose_Figure_Points( self, figure, pane )
  register struct zipedit		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipedit__Expose_Figure_Points);
  if ( pane  &&  figure  &&  ! figure->zip_figure_state.zip_figure_state_points_exposed )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    figure->zip_figure_state.zip_figure_state_points_exposed = true;
    zipview_Draw_Figure( View, figure, pane );
    }
  ZIP_STATUS();
  OUT(zipedit__Expose_Figure_Points);
  return zip_ok;
  }

long
zipedit__Delete_Image( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register zip_type_figure		  figure_ptr;

  IN(zipedit__Delete_Image);
  if ( image )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    image->zip_image_visibility = zip_image_hidden;
    image->zip_image_state.zip_image_state_deleted = true;
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_ok )
      {
      status = zipedit_Delete_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
      if ( image->zip_image_inferior  &&  status == zip_ok )
    status = Delete_Inferior_Image( self, image->zip_image_inferior, pane );
    }
    else
    status = zip_image_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Delete_Image);
  return status;
  }

static int
Delete_Inferior_Image( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register zip_type_figure		  figure_ptr;

  IN(Delete_Inferior_Image);
  image->zip_image_visibility = zip_image_hidden;
  image->zip_image_state.zip_image_state_deleted = true;
  figure_ptr = image->zip_image_figure_anchor;
  while ( figure_ptr  &&  status == zip_ok )
    {
    status = zipedit_Delete_Figure( self, figure_ptr, pane );
    figure_ptr = figure_ptr->zip_figure_next;
    }
  if ( image->zip_image_inferior  &&  status == zip_ok )
    status = Delete_Inferior_Image( self, image->zip_image_inferior, pane );
  if ( image->zip_image_peer  &&  status == zip_ok )
    status = Delete_Inferior_Image( self, image->zip_image_peer, pane );
  OUT(Delete_Inferior_Image);
  return status;
  }

long
zipedit__Undelete_Image( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register long						  status = zip_ok;
  register zip_type_figure				  figure_ptr;

  IN(zipedit__Undelete_Image);
  if ( image )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    image->zip_image_visibility = zip_image_exposed;
    image->zip_image_state.zip_image_state_deleted = false;
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_ok )
      {
      status = zipedit_Undelete_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_ok )
      status = Undelete_Inferior_Image( self, image->zip_image_inferior, pane );
    }
    else
    status = zip_image_non_existent;
  ZIP_STATUS();
  OUT(zipedit__Undelete_Image);
  return status;
  }

static int
Undelete_Inferior_Image( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register zip_type_figure		  figure_ptr;

  IN(Undelete_Inferior_Image);
  image->zip_image_visibility = zip_image_exposed;
  image->zip_image_state.zip_image_state_deleted = false;
  figure_ptr = image->zip_image_figure_anchor;
  while ( figure_ptr  &&  status == zip_ok )
    {
    status = zipedit_Undelete_Figure( self, figure_ptr, pane );
    figure_ptr = figure_ptr->zip_figure_next;
    }
  if ( image->zip_image_inferior  &&  status == zip_ok )
    status = Undelete_Inferior_Image( self, image->zip_image_inferior, pane );
  if ( image->zip_image_peer  &&  status == zip_ok )
    status = Undelete_Inferior_Image( self, image->zip_image_peer, pane );
  OUT(Undelete_Inferior_Image);
  return status;
  }

long
zipedit__Highlight_Image_Points( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr;
  register long				  status = zip_ok;

  IN(zipedit__Highlight_Image_Points);
  if ( image  &&  pane  &&   image->zip_image_visibility == zip_image_exposed )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_ok )
      {
      status = zipedit_Highlight_Figure_Points( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_ok )
      status = Highlight_Inferior_Image_Points( self, image->zip_image_inferior, pane );
    }
  ZIP_STATUS();
  OUT(zipedit__Highlight_Image_Points);
  return status;
  }


int
Highlight_Inferior_Image_Points( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr;
  register long				  status = zip_ok;

  IN(zip_Highlight_Inferior_Image_Points);
  if ( image->zip_image_visibility == zip_image_exposed )
    {
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_ok )
      {
      status = zipedit_Highlight_Figure_Points( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_ok )
      status = Highlight_Inferior_Image_Points( self, image->zip_image_inferior, pane );
    if ( image->zip_image_peer  &&  status == zip_ok )
      status = Highlight_Inferior_Image_Points( self, image->zip_image_peer, pane );
    }
  ZIP_STATUS();
  OUT(zip_Highlight_Inferior_Image_Points);
  return status;
  }

long
zipedit__Normalize_Image_Points( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr;
  register long				  status = zip_ok;

  IN(zip_Normalize_Image_Points);
  if ( image  &&  pane  &&  image->zip_image_visibility == zip_image_exposed )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_ok )
      {
      status = zipedit_Normalize_Figure_Points( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_ok )
      status = Normalize_Inferior_Image_Points( self, image->zip_image_inferior, pane );
    }
  ZIP_STATUS();
  OUT(zip_Normalize_Image_Points);
  return zip_ok;
  }

static int
Normalize_Inferior_Image_Points( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr;
  register long				  status = zip_ok;

  IN(Normalize_Inferior_Image_Points);
  if ( image->zip_image_visibility == zip_image_exposed )
    {
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_ok )
      {
      status = zipedit_Normalize_Figure_Points( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_ok )
      status = Normalize_Inferior_Image_Points( self, image->zip_image_inferior, pane );
    if ( image->zip_image_peer  &&  status == zip_ok )
      status = Normalize_Inferior_Image_Points( self, image->zip_image_peer, pane );
    }
  ZIP_STATUS();
  OUT(Normalize_Inferior_Image_Points);
  return status;
  }

long
zipedit__Hide_Image_Points( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr;
  register long				  status = zip_ok;

  IN(zip_Hide_Image_Points);
  if ( image  &&  pane )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr )
      {
      zipedit_Hide_Figure_Points( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    }
  ZIP_STATUS();
  OUT(zip_Hide_Image_Points);
  return zip_ok;
  }

long
zipedit__Expose_Image_Points( self, image, pane )
  register struct zipedit		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr;
  register long				  status = zip_ok;

  IN(zip_Expose_Image_Points);
  if ( image  &&  pane )
    {
    zipview_Set_Pane_Clip_Area( View, pane );
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr )
      {
      zipedit_Expose_Figure_Points( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    }
  ZIP_STATUS();
  OUT(zip_Expose_Image_Points);
  return zip_ok;
  }


long
zipedit__Delete_Stream( self, stream, pane )
  register struct zipedit		 *self;
  register zip_type_stream		  stream;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Undelete_Stream( self, stream, pane )
  register struct zipedit		 *self;
  register zip_type_stream		  stream;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Highlight_Stream_Points( self, stream, pane )
  register struct zipedit		 *self;
  register zip_type_stream		  stream;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Normalize_Stream_Points( self, stream, pane )
  register struct zipedit		 *self;
  register zip_type_stream		  stream;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Hide_Stream_Points( self, stream, pane )
  register struct zipedit		 *self;
  register zip_type_stream		  stream;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

long
zipedit__Expose_Stream_Points( self, stream, pane )
  register struct zipedit		 *self;
  register zip_type_stream		  stream;
  register zip_type_pane		  pane;
  {
return zip_ok;
  }

zipedit__Expose_Point( self, pane, figure, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register zip_type_pixel		  x, y;
  {
  char					  points[100];
  int					  xp, yp;
  register struct fontdesc		 *current_font =
					    zipview_GetFont( View );

  IN(zipedit__Expose_Point);
  sprintf( points, "(%d,%D)", x, y );  /*=== optimize ===*/
  zipview_SetTransferMode( View, graphic_BLACK );
  zipview_MoveTo( View, ((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))) - 3,
			((OriginY) - (y * (Flop) * (Multiplier) / (Divisor))) - 3 );
  zipview_DrawLineTo( View, ((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))) + 3,
			    ((OriginY) - (y * (Flop) * (Multiplier) / (Divisor))) - 3 );
  zipview_DrawLineTo( View, ((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))) + 3,
			    ((OriginY) - (y * (Flop) * (Multiplier) / (Divisor))) + 3 );
  zipview_DrawLineTo( View, ((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))) - 3,
			    ((OriginY) - (y * (Flop) * (Multiplier) / (Divisor))) + 3 );
  zipview_DrawLineTo( View, ((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))) - 3,
			    ((OriginY) - (y * (Flop) * (Multiplier) / (Divisor))) - 3 );
  if ( PointsFont == NULL )
    {
    char			      family_name[257];
    long			      font_style;
    long			      font_size;

    fontdesc_ExplodeFontName( zip_points_font_name, family_name,
			      sizeof( family_name ), &font_style, & font_size );
    PointsFont = fontdesc_Create( family_name, font_style, font_size );
    }
  zipview_SetFont( View, PointsFont );
  fontdesc_StringSize( zipview_GetFont( View ), im_GetDrawable(View), points, &xp, &yp );
  zipview_SetTransferMode( View, graphic_WHITE );
  zipview_FillRectSize( View,
			((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))) + 5,
			((OriginY) - (y * (Flop) * (Multiplier) / (Divisor))) - 5,
			xp , 10, zipview_WhitePattern( View ) );
			 /*=== compute width and height ===*/
  zipview_SetTransferMode( View, graphic_BLACK );
  zipview_MoveTo( View,
		  5 + ((OriginX) + (x * (Flip) * (Multiplier) / (Divisor))),
		  (OriginY) - (y * (Flop) * (Multiplier) / (Divisor)) );
  zipview_DrawString( View, graphic_ATLEFT | graphic_BETWEENTOPANDBOTTOM, points );
  zipview_MoveTo( View,
		  (OriginX) + (x * (Flip) * (Multiplier) / (Divisor)),
		  (OriginY) - (y * (Flop) * (Multiplier) / (Divisor)) );
  zipview_SetFont( View, current_font );
  IN(zipedit__Expose_Point);
  }


zipedit__Hide_Point( self, pane, figure, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register zip_type_pixel		  x, y;
  {
/*=== needed ?  ===*/
  }

int
zipedit__Highlight_Handles( self, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  X1, X2, X3, Y1, Y2, Y3;
  {
  IN(zipedit__Highlight_Handles);
  zipedit_Highlight_Point(  self, pane, X1, Y1 );
  zipedit_Highlight_Handle( self, pane, X1, Y2 );
  zipedit_Highlight_Point(  self, pane, X1, Y3 );
  zipedit_Highlight_Handle( self, pane, X2, Y1 );
  zipedit_Highlight_Point(  self, pane, X2, Y2 );
  zipedit_Highlight_Handle( self, pane, X2, Y3 );
  zipedit_Highlight_Point(  self, pane, X3, Y1 );
  zipedit_Highlight_Handle( self, pane, X3, Y2 );
  zipedit_Highlight_Point(  self, pane, X3, Y3 );
  OUT(zipedit__Highlight_Handles);
  return zip_ok;
  }

struct highlights
  {
  zip_type_pixel	     x, y;
  };

static struct highlights    *highlights;
static long		     highlights_count;

static boolean
Ratify_Highlighting( self, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register long				  i;
  register boolean			  ratified = true;

  IN(Ratify_Highlighting);
  if ( x > 0  &&  y > 0 )
    {
    for ( i = 0; i < highlights_count; i++ )
      if ( abs(x - highlights[i].x ) < 5  &&  abs(y - highlights[i].y ) < 5 )
	{ DEBUG(Not Ratified);
	ratified = false;
	break;
	}
    if ( ratified )
      { DEBUG(Ratified);
      if ( highlights_count )
	highlights = (struct highlights *) realloc( highlights,
			++highlights_count * sizeof(struct highlights) );
	else
	highlights = (struct highlights *) malloc( ++highlights_count * sizeof(struct highlights) );
      highlights[highlights_count-1].x = x;
      highlights[highlights_count-1].y = y;
      }
    }
  OUT(Ratify_Highlighting);
  return  ratified;
  }

static boolean
Ratify_Normalizing( self, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register long				  i;
  register boolean			  discard = true;
  register boolean			  ratified = false;

  IN(Ratify_Normalizing);
  if ( x > 0  &&  y > 0 )
    {
    for ( i = 0; i < highlights_count; i++ )
      if ( x == highlights[i].x  &&  y == highlights[i].y )
	{ DEBUG(Ratified);
	highlights[i].x = -1;
	ratified = true;
	break;
	}
    if ( ratified )
      { DEBUG(Attempt Purge);
      for ( i = 0; i < highlights_count; i++ )
        if ( highlights[i].x > 0 )
	  {
	  discard = false;
	  break;
	  }
      if ( discard )
        { DEBUG(Do Purge);
        free( highlights );
        highlights_count = 0;
        highlights = NULL;
        }
      }
    }
  OUT(Ratify_Normalizing);
  return  ratified;
  }

int
zipedit__Highlight_Point( self, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  IN(zipedit_Highlight_Point);
  if ( Ratify_Highlighting( self, pane, x, y ) )
    {
    if ( zipview_GetTransferMode( View ) != graphic_INVERT )
      zipview_SetTransferMode( View, graphic_INVERT );
    if ( IconFont == NULL )
      {
      char			      family_name[257];
      long			      font_style;
      long			      font_size;

      DEBUG(Create Icon Font);
      fontdesc_ExplodeFontName( zip_icon_font_name, family_name,
				sizeof( family_name ), &font_style, & font_size );
      IconFont = fontdesc_Create( family_name, font_style, font_size );
      }
    if ( zipview_GetFont( View ) != IconFont )
      zipview_SetFont( View, IconFont );
    zipview_MoveTo( View, x, y );
    zipview_DrawText( View, &pane->zip_pane_highlight_icon, 1, NULL );
    }
  OUT(zipedit_Highlight_Point);
  return zip_ok;
  }

 int
zipedit__Highlight_Handle( self, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  IN(zipedit_Highlight_Handle);
  if ( Ratify_Highlighting( self, pane, x, y ) )
    {
    if ( zipview_GetTransferMode( View ) != graphic_INVERT )
      zipview_SetTransferMode( View, graphic_INVERT );
    zipview_MoveTo( View, x, y );
    zipview_FillRectSize( View, x - 2, y - 2, 5, 5, zipview_WhitePattern( View ) );
    }
  OUT(zipedit_Highlight_Handle);
  return zip_ok;
  }

int
zipedit__Normalize_Handles( self, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  X1, X2, X3, Y1, Y2, Y3;
  {
  IN(zipedit__Normalize_Handles);
  zipedit_Normalize_Point(  self, pane, X1, Y1 );
  zipedit_Normalize_Handle( self, pane, X1, Y2 );
  zipedit_Normalize_Point(  self, pane, X1, Y3 );
  zipedit_Normalize_Handle( self, pane, X2, Y1 );
  zipedit_Normalize_Point(  self, pane, X2, Y2 );
  zipedit_Normalize_Handle( self, pane, X2, Y3 );
  zipedit_Normalize_Point(  self, pane, X3, Y1 );
  zipedit_Normalize_Handle( self, pane, X3, Y2 );
  zipedit_Normalize_Point(  self, pane, X3, Y3 );
  OUT(zipedit__Normalize_Handles);
  return zip_ok;
  }


int
zipedit__Normalize_Point( self, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_pixel		  x, y;
  register zip_type_pane		  pane;
  {
  IN(zipedit__Normalize_Point);
  if ( Ratify_Normalizing( self, pane, x, y ) )
    {
    if ( zipview_GetTransferMode( View ) != graphic_INVERT )
      zipview_SetTransferMode( View, graphic_INVERT );
    if ( IconFont == NULL )
      {
      char			      family_name[257];
      long			      font_style;
      long			      font_size;

      fontdesc_ExplodeFontName( zip_icon_font_name, family_name,
				sizeof( family_name ), &font_style, & font_size );
      IconFont = fontdesc_Create( family_name, font_style, font_size );
      }
    if ( zipview_GetFont( View ) != IconFont )
      zipview_SetFont( View, IconFont );
    zipview_MoveTo( View, x, y );
    zipview_DrawText( View, &pane->zip_pane_highlight_icon, 1, NULL );
    }
  OUT(zipedit__Normalize_Point);
  return zip_ok;
  }


 int
zipedit__Normalize_Handle( self, pane, x, y )
  register struct zipedit		 *self;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  IN(zipedit__Highlight_Handle);
  if ( Ratify_Normalizing( self, pane, x, y ) )
    {
    if ( zipview_GetTransferMode( View ) != graphic_INVERT )
      zipview_SetTransferMode( View, graphic_INVERT );
    zipview_MoveTo( View, x, y );
    zipview_FillRectSize( View, x - 2, y - 2, 5, 5, zipview_WhitePattern( View ) );
    }
  OUT(zipedit__Normalize_Handle);
  return zip_ok;
  }
