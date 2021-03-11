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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipvi00.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipvi00.c	Zip View-object	-- Images			      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipvi00.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:04:23  rr2b
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
 * Revision 2.8  1991/09/12  16:45:25  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.7  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.6  1989/07/20  13:13:13  sg08
 * introduced Image_Visible method
 *
 * Revision 2.5  89/02/17  18:10:31  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.4  89/02/08  16:52:46  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/07  21:03:04  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/11/16  18:35:22  tom
 * Optimize Draw Figure by doing it directly from Image.
 * 
 * Revision 2.1  88/09/27  18:20:40  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:52:43  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:48:05  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object -- Images

MODULE	zipvi00.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Image facilities
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
  10/26/88	Optimize Draw Figure by doing it directly (TCP)
  07/12/89	Added Image_Visible method (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "zip.ih"
#include "zipobj.ih"
#include "zipv.ih"

#define	 Data			      (self->data_object)
#define	 View			      (self)
#define  Objects(i)		    ((*self->objects)[i])

static int Draw_Inferior_Image();
static int Draw_Figure();
static int Clear_Inferior_Image();
static int Hide_Inferior_Image();
static int Expose_Inferior_Image();

long
zipview__Display_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;

  IN(zipview__Display_Image);
  if ( image  &&  pane )
    {
    zipview_Set_Pane_Image( self, pane, image );
    status = zipview_Display_Pane( self, pane );
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_image_non_existent;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview__Display_Image);
  return status;
  }

long
zipview__Draw_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(zipview__Draw_Image);
  if ( image  &&  pane )
    {
    if ( image->zip_image_visibility != zip_image_hidden )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
      image->zip_image_visibility = zip_image_exposed;
      figure_ptr = image->zip_image_figure_anchor;
      while ( figure_ptr  &&  status == zip_success )
        {
        status = Draw_Figure( self, figure_ptr, pane );
        figure_ptr = figure_ptr->zip_figure_next;
        }
      }
    if ( image->zip_image_inferior  &&  status == zip_success )
      status = Draw_Inferior_Image( self, image->zip_image_inferior, pane );
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_image_non_existent;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview__Draw_Image);
  return status;
  }

static int
Draw_Inferior_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(Draw_Inferior_Image);
  if ( image->zip_image_visibility != zip_image_hidden )
    {
    image->zip_image_visibility = zip_image_exposed;
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_success )
      {
      status = Draw_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    }
  if ( image->zip_image_inferior  &&  status == zip_success )
    status = Draw_Inferior_Image( self, image->zip_image_inferior, pane );
  if ( image->zip_image_peer  &&  status == zip_success )
    status = Draw_Inferior_Image( self, image->zip_image_peer, pane );
  OUT(Draw_Inferior_Image);
  return status;
  }

static int
Draw_Figure( self, figure, pane )
  register struct zipview		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  if ( figure->zip_figure_visibility != zip_figure_hidden  &&
     !figure->zip_figure_state.zip_figure_state_deleted )
    {
    figure->zip_figure_visibility = zip_figure_exposed;
    status = zipobject_Draw_Object( Objects(figure->zip_figure_type), figure, pane );
    }
  return  status;
  }

long
zipview__Clear_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(zipview__Clear_Image);
  if ( image  &&  pane )
    {
    if ( image->zip_image_visibility == zip_image_exposed )
      {
      image->zip_image_visibility = 0;
      figure_ptr = image->zip_image_figure_anchor;
      while ( figure_ptr  &&  status == zip_success )
        {
        status = zipview_Clear_Figure( self, figure_ptr, pane );
        figure_ptr = figure_ptr->zip_figure_next;
        }
      }
    if ( image->zip_image_inferior  &&  status == zip_success )
      status = Clear_Inferior_Image( self, image->zip_image_inferior, pane );
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_image_non_existent;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview__Clear_Image);
  return status;
  }

static int
Clear_Inferior_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(Clear_Inferior_Image);
  if ( image->zip_image_visibility == zip_image_exposed )
    {
    image->zip_image_visibility = 0;
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_success )
      {
      status =zipview_Clear_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    }
  if ( image->zip_image_inferior  &&  status == zip_success )
    status = Clear_Inferior_Image( self, image->zip_image_inferior, pane );
  if ( image->zip_image_peer  &&  status == zip_success )
    status = Clear_Inferior_Image( self, image->zip_image_peer, pane );
  OUT(Clear_Inferior_Image);
  return status;
  }

long
zipview__Hide_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(zipview__Hide_Image);
  if ( image )
    {
    image->zip_image_visibility = zip_image_hidden;
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_success )
      {
      status = zipview_Hide_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_success )
      status = Hide_Inferior_Image( self, image->zip_image_inferior, pane );
    }
    else 
    status = zip_image_non_existent;
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview__Hide_Image);
  return status;
  }

static int
Hide_Inferior_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(Hide_Inferior_Image);
  image->zip_image_visibility = zip_image_hidden;
  figure_ptr = image->zip_image_figure_anchor;
  while ( figure_ptr  &&  status == zip_success )
    {
    status = zipview_Hide_Figure( self, figure_ptr, pane );
    figure_ptr = figure_ptr->zip_figure_next;
    }
  if ( image->zip_image_inferior  &&  status == zip_success )
    status = Hide_Inferior_Image( self, image->zip_image_inferior, pane );
  if ( image->zip_image_peer  &&  status == zip_success )
    status = Hide_Inferior_Image( self, image->zip_image_peer, pane );
  OUT(Hide_Inferior_Image);
  return status;
  }

long
zipview__Expose_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(zipview__Expose_Image);
  if ( image )
    {
    image->zip_image_visibility = zip_image_exposed;
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_success )
      {
      status = zipview_Expose_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_success )
      status = Expose_Inferior_Image( self, image->zip_image_inferior, pane );
    }
    else 
    status = zip_image_non_existent;
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview__Expose_Image);
  return status;
  }

static int
Expose_Inferior_Image( self, image, pane )
  register struct zipview		 *self;
  register zip_type_image		  image;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_success;
  register zip_type_figure		  figure_ptr;

  IN(Expose_Inferior_Image);
  image->zip_image_visibility = zip_image_exposed;
  figure_ptr = image->zip_image_figure_anchor;
  while ( figure_ptr  &&  status == zip_success )
    {
    status = zipview_Expose_Figure( self, figure_ptr, pane );
    figure_ptr = figure_ptr->zip_figure_next;
    }
  if ( image->zip_image_inferior  &&  status == zip_success )
    status = Expose_Inferior_Image( self, image->zip_image_inferior, pane );
  if ( image->zip_image_peer  &&  status == zip_success )
    status = Expose_Inferior_Image( self, image->zip_image_peer, pane );
  OUT(Expose_Inferior_Image);
  return status;
  }

zip_type_image
zipview__Which_Image( self, x, y )
  register struct zipview		 *self;
  register long				  x, y;
  {
  register zip_type_image		  image = NULL;
  register zip_type_figure		  figure;
  register int				  status = zip_success;

  IN(zippview_Which_Image);
  if ( figure = zipview_Which_Figure( self, x, y ) )
    image = figure->zip_figure_image;
  ZIP_STATUS();
  OUT(zippview_Which_Image);
  return image;
  }

zip_type_image
zipview__Within_Which_Image( self, x, y )
  register struct zipview		 *self;
  register long				  x, y;
  {
return zip_ok;
  }

boolean
zipview__Image_Visible( self, image, pane )
  register struct zipview		    *self;
  register zip_type_image		    image;
  register zip_type_pane		    pane;
  {
  register boolean			    status = FALSE;

  IN( zipview__Image_Visible );
  if ( image && pane )
    {
    if ( !(( zip_Image_Greatest_X( Data, image ) <
          zipview_X_Pixel_To_Point( self, pane, NULL, zipview_Pane_Left( self, pane ))) ||
        ( zip_Image_Greatest_Y( Data, image ) <
          zipview_Y_Pixel_To_Point( self, pane, NULL, zipview_Pane_Bottom( self, pane ))) ||
        ( zip_Image_Least_X( Data, image ) >
          zipview_X_Pixel_To_Point( self, pane, NULL, zipview_Pane_Right( self, pane ))) ||
        ( zip_Image_Least_Y( Data, image ) >
          zipview_Y_Pixel_To_Point( self, pane, NULL, zipview_Pane_Top( self, pane )))))
      status = TRUE;
    }
  OUT( zipview__Image_Visible );
  return status;
  }
