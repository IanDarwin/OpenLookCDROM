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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipvf00.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipvf00.c	Zip View-object	-- Figures			      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipvf00.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:04:14  rr2b
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
 * Revision 2.10  1991/09/12  16:45:20  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.9  1990/08/21  14:46:08  sg08
 * Suppress usage of unimplemented Object_Visible() (wait for "Intersects" method)
 *
 * Revision 2.8  89/07/20  13:12:38  sg08
 * introduced Figure_Visible method
 * 
 * Revision 2.7  89/06/14  17:45:58  sg08
 * Allow NULL Pane argument to Hide|Expose_Figure
 * 
 * Revision 2.6  89/05/19  20:40:01  tom
 * Improve performance by dropping FlushGraphics.
 * 
 * Revision 2.5  89/02/17  18:10:25  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.4  89/02/08  16:52:43  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/07  21:00:47  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/11/16  18:40:14  tom
 * Correct Which_Figure to check to exposed state.
 * 
 * Revision 2.1  88/09/27  18:20:34  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:52:16  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:48:01  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object -- Figures

MODULE	zipvf00.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Figure facilities
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
  11/01/88	Correct Which_Figure to check to exposed state (TCP)
  05/15/89	Improve performance by dropping FlushGraphics (TCP)
   06/14/89	Allow NULL Pane arg to Hide|Expose_Figure (SCG)
  07/12/89	Added Figure_Visible method (SCG)
   08/20/90	Suppress usage of unimplemented Object_Visible() (wait for "Intersects" method) (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "zip.ih"
#include "zipobj.ih"
#include "zipv.ih"

#define	 Data			      (self->data_object)
#define	 View			      (self)
#define  Objects(i)		    ((*self->objects)[i])


long
zipview__Display_Figure( self, figure, pane )
  register struct zipview		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;

  IN(zipview__Display_Figure);
  if ( figure  &&  pane )
    {
    zipview_Set_Pane_Figure( self, pane, figure );
    status = zipview_Display_Pane( self, pane );
    }
    else
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_stream_non_existent;
  ZIP_STATUS();
  OUT(zipview__Display_Figure);
  return status;
  }

long
zipview__Draw_Figure( self, figure, pane )
  register struct zipview		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview__Draw_Figure);
  if ( figure  &&  pane  &&  figure->zip_figure_visibility != zip_figure_hidden  &&
       !figure->zip_figure_state.zip_figure_state_deleted )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    figure->zip_figure_visibility = zip_figure_exposed;
    status = zipobject_Draw_Object( Objects(figure->zip_figure_type), figure, pane );
    }
    else 
    if ( pane == NULL )
      status = zip_pane_non_existent;
    else
    if ( figure == NULL )
      status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zipview__Draw_Figure);
  return status;
  }

long
zipview__Clear_Figure( self, figure, pane )
  register struct zipview		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview__Clear_Figure);
  if ( figure  &&  pane  &&  figure->zip_figure_visibility == zip_figure_exposed  &&
       !figure->zip_figure_state.zip_figure_state_deleted )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    figure->zip_figure_visibility = 0;
    status = zipobject_Clear_Object( Objects(figure->zip_figure_type), figure, pane );
    }
    else 
    if ( pane == NULL )
      status = zip_pane_non_existent;
    else
    if ( figure == NULL )
      status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zipview__Clear_Figure);
  return status;
  }

long
zipview__Hide_Figure( self, figure, pane )
  register struct zipview		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview__Hide_Figure);
  if ( figure &&
       !figure->zip_figure_state.zip_figure_state_deleted )
    {
    if ( pane )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
      if ( figure->zip_figure_visibility == zip_figure_exposed )
        status = zipobject_Clear_Object( Objects(figure->zip_figure_type), figure, pane );
      }
    figure->zip_figure_visibility = zip_figure_hidden;
    }
    else
    if ( figure == NULL )
      status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zipview__Hide_Figure);
  return status;
  }

long
zipview__Expose_Figure( self, figure, pane )
  register struct zipview		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;
  
  IN(zipview__Expose_Figure);
  if ( figure &&
       !figure->zip_figure_state.zip_figure_state_deleted )
    {
    if ( pane )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
      if ( figure->zip_figure_visibility != zip_figure_exposed )
        status = zipobject_Draw_Object( Objects(figure->zip_figure_type), figure, pane );
      }
    figure->zip_figure_visibility = zip_figure_exposed;
    }
    else
    if ( figure == NULL )
      status = zip_figure_non_existent;
  ZIP_STATUS();
  OUT(zipview__Expose_Figure);
  return status;
  }

static zip_type_figure
Which_Figure( self, x, y, pane )
  register struct zipview		 *self;
  register zip_type_pixel		  x, y;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure_ptr, figure = NULL;
  register zip_type_image		  image;
  register zip_type_stream		  stream = NULL;

  IN(Which_Figure);
  DEBUGst(Pane-name,pane->zip_pane_name);
  if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source )
      stream = pane->zip_pane_source.zip_pane_stream;
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_image_source )
      stream = pane->zip_pane_source.zip_pane_image->zip_image_stream;
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source )
      stream = pane->zip_pane_source.zip_pane_figure->zip_figure_image->zip_image_stream;
  if ( stream )
    {
    image = stream->zip_stream_image_anchor;
    DEBUGst(Image-name,image->zip_image_name);
    while ( image )
      {
      figure_ptr = image->zip_image_figure_anchor;
      while ( figure_ptr )
	{
	DEBUGst(Figure-name,figure_ptr->zip_figure_name);
        if ( figure_ptr->zip_figure_visibility == zip_figure_exposed  &&
	     zipobject_Proximate_Object_Points( Objects(figure_ptr->zip_figure_type),
						figure_ptr, pane, x, y ) )
	  {
	  figure = figure_ptr;
	  goto found;
	  }
        figure_ptr = figure_ptr->zip_figure_next;
	}
      image = zip_Next_Image( Data, image );
      }
    }
  found:
  OUT(Which_Figure);
  return  figure;
  }

zip_type_figure
zipview__Which_Figure( self, x, y )
  register struct zipview		 *self;
  register zip_type_pixel		  x, y;
  {
  register zip_type_figure		  figure = NULL;
  register zip_type_pane		  pane;

  IN(zipview__Which_Figure);
  if ( pane = zipview_Which_Pane( self, x, y ) )
    {
    DEBUGst(Pane-name,pane->zip_pane_name);
    figure = Which_Figure( self, x, y, pane );
    }
  OUT(zipview__Which_Figure);
  return  figure;
  }

zip_type_figure
zipview__Which_Pane_Figure( self, x, y, pane )
  register struct zipview		 *self;
  register zip_type_pixel		  x, y;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure = NULL;

  IN(zipview__Which_Figure);
  DEBUGst(Pane-name,pane->zip_pane_name);
  figure = Which_Figure( self, x, y, pane );
  OUT(zipview__Which_Figure);
  return  figure;
  }

zip_type_figure
zipview__Within_Which_Figure( self, x, y )
  register struct zipview	     *self;
  register long			      x, y;
  {
  register zip_type_stream	      stream;
  register zip_type_image	      image;
  register zip_type_figure	      figure, figure_ptr = NULL;
  register zip_type_pane	      pane;
  register long			      distance, best_distance = 999999;

  IN(zipview__Within_Which_Figure);
  if ( (pane  = zipview_Which_Pane( self, x, y ))  &&
       (stream = zipview_Pane_Stream( self, pane ))
     ) 
    {
    DEBUGst(Pane-name,pane->zip_pane_name);
    DEBUGst(Stream-name,stream->zip_stream_name);
    image = stream->zip_stream_image_anchor;
    while ( image )
      {
      DEBUGst(Image-name,image->zip_image_name);
      figure = image->zip_image_figure_anchor;
      while( figure )
	{
        DEBUGst(Figure-name,figure->zip_figure_name);
	if ( !figure->zip_figure_state.zip_figure_state_deleted )
	  if ( (distance = zipobject_Within_Object(
		Objects(figure->zip_figure_type), figure, pane, x, y )) > -1 )
	    {
	    if ( distance < best_distance  ||  figure->zip_figure_mode.zip_figure_mode_shaded )
	      { DEBUGdt(Best-distance,distance);
	      best_distance = distance;
              figure_ptr = figure;
	      }
	    }
	figure = figure->zip_figure_next;
	}
      image = zip_Next_Image( Data, image );
      }
    }
  OUT(zipview__Within_Which_Figure);
  return  figure_ptr;
  }

boolean
zipview__Figure_Visible( self, figure, pane )
  register struct zipview		    *self;
  register zip_type_figure		    figure;
  register zip_type_pane		    pane;
  {
  register boolean			    status = FALSE;

  IN( zipview__Figure_Visible );
  if ( figure && pane )
    {
      if ( zipview_Image_Visible( self, zip_Figure_Image( Data, figure ), pane )
             == FALSE )
        status = FALSE;
        else status = TRUE; /* === ===*/
/*         status = zipobject_Object_Visible( Objects( figure->zip_figure_type ), figure, pane ); */
    }
  OUT( zipview__Figure_Visible );
  return status;
  }
