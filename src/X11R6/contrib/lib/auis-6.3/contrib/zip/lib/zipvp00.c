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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipvp00.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipvd00.c	Zip View-object	-- Pane				      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipvp00.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:04:33  rr2b
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
 * Revision 2.12  1991/09/12  16:45:34  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.11  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.10  1989/06/14  17:46:39  sg08
 * Suppress Mark_Pane_Objects_Unexposed in Assign_Pane_Stream_Attributes. Unclear why it was done in the first place, thus unclear what this might break. (But GAHM needs it this way)
 *
 * Revision 2.9  89/05/01  22:12:27  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.8  89/02/17  18:10:38  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.7  89/02/08  16:52:58  ghoti
 * change copyright notice
 * 
 * Revision 2.6  89/02/07  21:24:48  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.5  88/12/08  18:42:40  tom
 * Fix Panning to honor panning-precesion and use 8 as factor.
 * 
 * Revision 2.4  88/11/16  18:57:41  tom
 * Mark non-visible units not-exposed.
 * 
 * Revision 2.3  88/10/13  13:03:58  tom
 *  Fix setting of Block ptr in Create_Pane.
 * 
 * Revision 2.2  88/10/11  20:43:39  tom
 * Correct setting of Stream flags.
 * 
 * Revision 2.1  88/09/27  18:21:12  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:54:29  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:48:26  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object -- Pane

MODULE	zipvd00.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Pane facilities
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
  10/13/88	Fix setting of blaock cell in Pane creation (TCP)
  11/01/88	Mark non-visible units not-exposed (TCP)
  12/08/88	Set panning-precision to default 8 (TCP)
  04/03/89	Honor Panning-precision in preferences (TCP)
  05/01/89	Use symbolic font-names (TCP)
  06/14/89	Suppress Mark_Pane_Objects_Unexposed in Assign_Pane_Stream_Attributes (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "environ.ih"
#include "zip.ih"
#include "zipv.ih"


#define	 Data			      (self->data_object)
#define  View			      (self)

#define  PaneLeft(pane)		      (zipview_Pane_Left( self, pane ))
#define  PaneTop(pane)		      (zipview_Pane_Top( self, pane ))
#define  PaneWidth(pane)	      (zipview_Pane_Width( self, pane ))
#define  PaneHeight(pane)	      (zipview_Pane_Height( self, pane ))
#define  PaneBottom(pane)	      (zipview_Pane_Bottom( self, pane ))
#define  PaneRight(pane)	      (zipview_Pane_Right( self, pane ))

#define  PaneXOrigin(pane)	      (zipview_Pane_X_Origin( self, pane ))
#define  PaneYOrigin(pane)	      (zipview_Pane_Y_Origin( self, pane ))

#define  Block			      (View->block)

static int Allocate_Pane_Object();
static int Deallocate_Pane_Object();
static int Deallocate_Pane_Object();
static Assign_Pane_Stream_Attributes( );
static Assign_Pane_Image_Attributes();
static Assign_Pane_Figure_Attributes();
static Mark_Pane_Objects_Unexposed();
static Mark_Pane_Image_Unexposed();
static Coordinate_Block_Pane();
static Coordinate_Nested_Pane();
static Compute_Pane_Stream_Stretch_Factors();
static Compute_Pane_Image_Stretch_Factors();
static Compute_Pane_Figure_Stretch_Factors();

long
zipview__Create_Pane( self, pane, name, block, attributes )
  register struct zipview		 *self;
  register zip_type_pane		 *pane;
  register char				 *name;
  register struct rectangle		 *block;
  register long				  attributes;
  {
  register int				  status = zip_ok;

  IN(zipview_Create_Pane);
  if ( (status = Allocate_Pane_Object( self, pane, name, attributes )) == zip_ok )
    {
    (*pane)->zip_pane_attributes.zip_pane_attribute_block_area = true;
    if ( block )
      (*pane)->zip_pane_area.zip_pane_block = block;
      else
      (*pane)->zip_pane_area.zip_pane_block = &Block;
    Coordinate_Block_Pane( self, *pane );
    }
  ZIP_STATUS();
  DEBUGdt(Status,status);
  OUT(zipview_Create_Pane);
  return status;
  }

long
zipview__Create_Nested_Pane( self, pane, name, master_pane, attributes )
  register struct zipview		 *self;
  register zip_type_pane		 *pane;
  register char				 *name;
  register zip_type_pane		  master_pane;
  register long				  attributes;
  {
  register int				  status = zip_ok;

  IN(zipview_Create_Nested_Pane);
  if ( (status = Allocate_Pane_Object( self, pane, name, attributes )) == zip_ok )
    {
    (*pane)->zip_pane_attributes.zip_pane_attribute_pane_area = true;
    (*pane)->zip_pane_area.zip_pane_pane = master_pane;
    Coordinate_Nested_Pane( self, *pane );
    }
  ZIP_STATUS();
  DEBUGdt(Status,status);
  OUT(zipview_Create_Nested_Pane);
  return status;
  }

static int
Allocate_Pane_Object( self, pane, name, attributes )
  register struct zipview		 *self;
  register zip_type_pane		 *pane;
  register char				 *name;
  register int				  attributes; /*=== set attributes ===*/
  {
  register int				  status = zip_ok;
  register zip_type_pane_chain		  pane_link, prior_link;
  static				  pane_serial = 1;
  char					  pane_name[257];

  IN(Allocate_Pane_Object);
  if ( name == NULL  ||  *name == NULL )
    sprintf( pane_name, "Un-Named-Pane-%2d", pane_serial++ );
    else
    strcpy( pane_name, name );
  if (
     (*pane = (zip_type_pane) calloc( 1, sizeof(struct zip_pane) ) ) == NULL
      ||
     ((*pane)->zip_pane_name = (char *) malloc( strlen( pane_name ) + 1 )) == NULL
      ||
     (pane_link = (zip_type_pane_chain) calloc( 1, sizeof(struct zip_pane_chain))) == NULL
     )
    status = zip_insufficient_pane_space;
    else
    {
    pane_link->zip_pane_chain_next = NULL;
    pane_link->zip_pane_chain_ptr  = *pane;
    prior_link = (zip_type_pane_chain)	&PaneAnchor;
    while( prior_link )
      {
      if ( prior_link->zip_pane_chain_next == NULL )
        {
	prior_link->zip_pane_chain_next = pane_link;
        break;
	}
      prior_link = prior_link->zip_pane_chain_next;
      }
    strcpy( (*pane)->zip_pane_name, pane_name );
    (*pane)->zip_pane_scale = 1.0;
    (*pane)->zip_pane_x_flip = 1;
    (*pane)->zip_pane_y_flop = 1;
    if ( attributes & zip_balanced )
      (*pane)->zip_pane_attributes.zip_pane_attribute_balanced	  = true;
    if ( attributes & zip_overlay )
      (*pane)->zip_pane_attributes.zip_pane_attribute_overlay	  = true;
    if ( attributes & zip_inverted )
      (*pane)->zip_pane_attributes.zip_pane_attribute_inverted	  = true;
    if ( attributes & zip_transparent )
      (*pane)->zip_pane_attributes.zip_pane_attribute_transparent = true;
    (*pane)->zip_pane_x_origin_percent = 50;
    (*pane)->zip_pane_y_origin_percent = 50;
    (*pane)->zip_pane_width_percent    = 100;
    (*pane)->zip_pane_height_percent   = 100;
    if ( ((*pane)->zip_pane_panning_precision = 
	    environ_GetProfileInt( "ZipPanningPrecision", 0 )) == 0 )
      (*pane)->zip_pane_panning_precision = 8;
    zipview_Set_Pane_Cursor( self, *pane, 'A', CursorFontName );
    }
  DEBUGdt(Status,status);
  OUT(Allocate_Pane_Object);
  return status;
  }

static int
Deallocate_Pane_Object( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register zip_type_pane_chain		  pane_link, prior_link;

  IN(Deallocate_Pane_Object);
  if ( pane )
    {
    prior_link = (zip_type_pane_chain) &PaneAnchor;
    pane_link = PaneAnchor;
    while( pane_link )
      {
      if ( pane_link->zip_pane_chain_ptr == pane )
        {
	prior_link->zip_pane_chain_next = pane_link->zip_pane_chain_next;
        free( pane_link );
	if ( pane->zip_pane_name )  free( pane->zip_pane_name );
        free( pane );
        break;
	}
      prior_link = pane_link;
      pane_link = pane_link->zip_pane_chain_next;
      }
    }
  OUT(Deallocate_Pane_Object);
  return zip_ok;
  }

long
zipview__Destroy_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Destroy_Pane);
  if ( pane )
    {
    if ( pane->zip_pane_state.zip_pane_state_exposed )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
      zipview_Clear_Pane( self, pane );
      if ( pane->zip_pane_attributes.zip_pane_attribute_overlay )
        zipview_Restore_Overlay( self, pane );
      }
    Deallocate_Pane_Object( self, pane );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Destroy_Pane);
  return status;
  }

long
zipview__Remove_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Remove_Pane);
  if ( pane )
    {
    if ( zipview_Pane_Exposed( self, pane ) )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
/*===
      ZIPVIEW_WM_DefineRegion( ZIPVIEW_Pane_Region( pane ), 0, 0, 0,0 );
      ZIPVIEW_WM_ForgetRegion( ZIPVIEW_Pane_Region( pane ) );
      if ( pane->zip_pane_saved_region_id )
	{
	ZIPVIEW_WM_DefineRegion( ZIPVIEW_Pane_Saved_Region( pane ), 0, 0, 0,0 );    
	ZIPVIEW_WM_ForgetRegion( ZIPVIEW_Pane_Saved_Region( pane ) );
	}

      if ( pane->zip_pane_other_region_id )
	zipview_Restore_Overlay( self, pane );
	else===*/ if ( zipview_Pane_Exposed( self, pane ) )
	  zipview_Clear_Pane( self, pane );
      zipview_FlushGraphics( self );
      }
    pane->zip_pane_state.zip_pane_state_exposed = false;
    pane->zip_pane_state.zip_pane_state_hidden = false;
    pane->zip_pane_state.zip_pane_state_removed = true;
    }
  ZIP_STATUS();
  OUT(zipview_Remove_Pane);
  return status;
  }

zip_type_pane
zipview__Pane( self, name )
  register struct zipview		 *self;
  register char				 *name;
  {
  register zip_type_pane		  pane = NULL;
  register zip_type_pane_chain		  pane_link =
					    PaneAnchor;
  register int				  status = zip_ok;

  IN(zipview_Pane);
  while ( pane_link )
    {
    if ( pane_link->zip_pane_chain_ptr->zip_pane_name  &&
	 apt_MM_Compare( pane_link->zip_pane_chain_ptr->zip_pane_name, name ) == 0 )
      {
      DEBUGst( Found Named-Pane,pane_link->zip_pane_chain_ptr->zip_pane_name);
      pane = pane_link->zip_pane_chain_ptr;
      goto exit_point;
      }
    pane_link = pane_link->zip_pane_chain_next;
    }
  exit_point:
  ZIP_STATUS();
  OUT(zipview_Pane);
  return pane;
  }

zip_type_stream
zipview__Pane_Stream( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register zip_type_stream		  stream = NULL;
  register int				  status = zip_ok;

  IN(zipview_Pane_Stream);
  if ( pane )
    {
    if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source )
      stream = pane->zip_pane_source.zip_pane_stream;
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_image_source )
      stream = pane->zip_pane_source.zip_pane_image->zip_image_stream;
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source )
      stream = pane->zip_pane_source.zip_pane_figure->zip_figure_image->zip_image_stream;
    }
  ZIP_STATUS();
  OUT(zipview_Pane_Stream);
  return  stream;
  }

zip_type_image
zipview__Pane_Image( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register zip_type_image		  image = NULL;
  register int				  status = zip_ok;

  IN(zipview_Pane_Image);
  if ( pane )
    {
    if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source )
      image = pane->zip_pane_source.zip_pane_stream->zip_stream_image_anchor;
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_image_source )
      image = pane->zip_pane_source.zip_pane_image;
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source )
      image = pane->zip_pane_source.zip_pane_figure->zip_figure_image;
    }
  ZIP_STATUS();
  OUT(zipview_Pane_Image);
  return  image;
  }

zip_type_figure
zipview__Pane_Figure( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register zip_type_figure		  figure = NULL;
  register int				  status = zip_ok;

  IN(zipview_Pane_Figure);
  if ( pane )
    {
    if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source )
      figure = pane->zip_pane_source.zip_pane_stream->zip_stream_image_anchor->zip_image_figure_anchor;
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_image_source )
      figure = pane->zip_pane_source.zip_pane_image->zip_image_figure_anchor;
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source )
      figure = pane->zip_pane_source.zip_pane_figure;
    }
  ZIP_STATUS();
  OUT(zipview_Pane_Figure);
  return  figure;
  }

long
zipview__Clear_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Clear_Pane);
  ZIP_EFN(zip_Clear_Pane_EFN);
  if ( pane )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    if ( pane->zip_pane_state.zip_pane_state_inverted )
      zipview_SetTransferMode( self, graphic_BLACK );
      else
      zipview_SetTransferMode( self, graphic_WHITE );
    zipview_FillRectSize( self,
	  PaneLeft(pane) + pane->zip_pane_border_thickness,
	  PaneTop(pane)  + pane->zip_pane_border_thickness,
	  PaneWidth(pane)  - (2 * pane->zip_pane_border_thickness),
	  PaneHeight(pane) - (2 * pane->zip_pane_border_thickness),
	  zipview_WhitePattern( self )/*===*/ );
    zipview_SetTransferMode( self, graphic_BLACK );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Clear_Pane);
  return status;
  }

long
zipview__Invert_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Invert_Pane);
  ZIP_EFN(zip_Invert_Pane_EFN);
  if ( pane )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    zipview_SetTransferMode( self, graphic_INVERT );
    zipview_FillRectSize( self,
	  PaneLeft(pane) + pane->zip_pane_border_thickness,
	  PaneTop(pane)  + pane->zip_pane_border_thickness,
	  PaneWidth(pane)  - (2 * pane->zip_pane_border_thickness),
	  PaneHeight(pane) - (2 * pane->zip_pane_border_thickness),
	  zipview_WhitePattern( self ) /*===*/ );
    zipview_SetTransferMode( self, graphic_BLACK );
    pane->zip_pane_state.zip_pane_state_inverted ^= 1;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Invert_Pane);
  return status;
  }

long
zipview__Flip_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Flip_Pane);
  ZIP_EFN(zip_Flip_Pane_EFN);
  if ( pane )
    {
    pane->zip_pane_x_flip *= -1;
    zipview_Display_Pane( self, pane );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Flip_Pane);
  return status;
  }

long
zipview__Flop_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Flop_Pane);
  ZIP_EFN(zip_Flop_Pane_EFN);
  if ( pane )
    {
    pane->zip_pane_y_flop *= -1;
    zipview_Display_Pane( self, pane );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Flop_Pane);
  return  status;
  }

long
zipview__Normalize_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Normalize_Pane);
  if ( pane )
    {
    pane->zip_pane_zoom_level = 0;
    pane->zip_pane_x_offset = 0;
    pane->zip_pane_y_offset = 0;
    pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin;
    pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin;
    pane->zip_pane_x_flip = 1;
    pane->zip_pane_y_flop = 1;
    zipview_Set_Pane_Scale( self, pane, 1.0 );
    }
  OUT(zipview_Normalize_Pane);
  return  status;
  }


long
zipview__Center_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  return zipview_Pan_Pane( self, pane, 0, 0 );
  }

long
zipview__Balance_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Balance_Pane);
  ZIP_EFN(zip_Balance_Pane_EFN);
  if ( pane )
    {
/*=== implement Balance_Pane ===*/
    zipview_Display_Pane( self, pane );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Balance_Pane);
  return status;
  }

zip_type_pane
zipview__Which_Pane( self, x, y )
  register struct zipview		 *self;
  register long				  x, y;
  {
  register int				  status = zip_ok;
  register zip_type_pane		  pane = NULL, candidate;
  register zip_type_pane_chain		  pane_link = PaneAnchor;

  IN(zipview_Which_Pane);
  while ( pane_link )
    {
    candidate = pane_link->zip_pane_chain_ptr;
    if ( candidate->zip_pane_state.zip_pane_state_exposed  &&
	 x >= PaneLeft(candidate)  &&  x <= PaneRight(candidate)  &&
 	 y >= PaneTop(candidate)   &&  y <= PaneBottom(candidate) )
      {
      if ( pane )
	{
	/*DEBUG(Candidate);*/
	if (
	   PaneLeft(candidate)   >=  PaneLeft(pane)   &&
	   PaneTop(candidate)    >=  PaneTop(pane)    &&
	   PaneRight(candidate)  <=  PaneRight(pane)  &&
	   PaneBottom(candidate) <=  PaneBottom(pane)
	   )
	  {
	  pane = candidate;
	  }
	}
        else  pane = candidate;
      }
    pane_link = pane_link->zip_pane_chain_next;
    }
  ZIP_STATUS();
  OUT(zipview_Which_Pane);
  return pane;
  }

long
zipview__Set_Pane_Figure( self, pane, figure )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Figure);
  if ( pane  &&  figure )
    {
    pane->zip_pane_x_offset = 0;
    pane->zip_pane_y_offset = 0;
    pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin;
    pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin;
    pane->zip_pane_zoom_level = 0;
    status = Assign_Pane_Figure_Attributes( self, pane, figure );
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_figure_non_existent;
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Figure);
  return status;
  }

long
zipview__Reset_Pane_Figure( self, pane, figure )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  register int				  status = zip_ok;

  IN(zipview_Reset_Pane_Figure);
  if ( pane )
    status = Assign_Pane_Figure_Attributes( self, pane, figure );
  ZIP_STATUS();
  OUT(zipview_Reset_Pane_Figure);
  return status;
  }

long
zipview__Set_Pane_Image( self, pane, image )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_image		  image;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Image);
  if ( pane  &&  image )
    {
    pane->zip_pane_x_offset = 0;
    pane->zip_pane_y_offset = 0;
    pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin;
    pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin;
    pane->zip_pane_zoom_level = 0;
    status = Assign_Pane_Image_Attributes( self, pane, image );
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_image_non_existent;
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Image);
  return status;
  }

long
zipview__Reset_Pane_Image( self, pane, image )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_image		  image;
  {
  register int				  status = zip_ok;

  IN(zipview_Reset_Pane_Image);
  if ( pane )
    status = Assign_Pane_Image_Attributes( self, pane, image );
  ZIP_STATUS();
  OUT(zipview_Reset_Pane_Image);
  return status;
  }

long
zipview__Set_Pane_Stream( self, pane, stream )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_stream		  stream;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Stream);
  if ( pane  &&  stream )
    {
    pane->zip_pane_x_offset = 0;
    pane->zip_pane_y_offset = 0;
    pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin;
    pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin;
    pane->zip_pane_zoom_level = 0;
    status = Assign_Pane_Stream_Attributes( self, pane, stream );
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_stream_non_existent;
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Stream);
  return status;
  }

long
zipview__Reset_Pane_Stream( self, pane, stream )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_stream		  stream;
  {
  register int				  status = zip_ok;

  IN(zipview_Reset_Pane_Stream);
  if ( pane )
    status = Assign_Pane_Stream_Attributes( self, pane, stream );
  ZIP_STATUS();
  OUT(zipview_Reset_Pane_Stream);
  return status;
  }

long
zipview__Set_Pane_Auxiliary_Stream( self, pane, stream )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_stream		  stream;
  {
  register int				  status = zip_ok;
  register zip_type_pane_auxiliary_stream  new, old;

  IN(zipview_Set_Pane_Auxiliary_Stream);
  if ( pane  &&  stream )
    {
    old = pane->zip_pane_auxiliary_stream;
    while ( old )
     if ( old->zip_pane_auxiliary_stream_ptr != stream )
       old = old->zip_pane_auxiliary_stream_next;
       else /*=== stream is already auxiliary ===*/
       {
       status = zip_failure;
       break;
       }
    if ( !status )
      {
      if ( new = (zip_type_pane_auxiliary_stream) 
	  	    calloc( 1,sizeof(struct zip_pane_auxiliary_stream)) )
        {
        new->zip_pane_auxiliary_stream_ptr = stream;
        if ( old = pane->zip_pane_auxiliary_stream )
      	  {
    	    while ( old->zip_pane_auxiliary_stream_next )
	    old = old->zip_pane_auxiliary_stream_next;
 	    old->zip_pane_auxiliary_stream_next = new;
	  }
  	  else pane->zip_pane_auxiliary_stream = new;
	}
      else  status = zip_insufficient_pane_space;
      }
    }
    else 
    {
    if ( pane == NULL )
      status = zip_pane_non_existent;
      else
      status = zip_stream_non_existent;
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Auxiliary_Stream);
  return status;
  }

long
zipview__Reset_Pane_Auxiliary_Stream( self, pane, stream )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_stream		  stream;
  {
  register zip_type_pane_auxiliary_stream  next, prior;

  IN(zipview_Reset_Pane_Auxiliary_Stream);
  if ( pane  &&  pane->zip_pane_auxiliary_stream  &&  stream )
    {
    next = pane->zip_pane_auxiliary_stream;
    prior = (zip_type_pane_auxiliary_stream) &pane->zip_pane_auxiliary_stream;
    while ( next )
      {
      if ( next->zip_pane_auxiliary_stream_ptr == stream )
	{
        prior->zip_pane_auxiliary_stream_next = next->zip_pane_auxiliary_stream_next;
	free( next );
	break;
	}
        else
	{
	prior = next;
	next = next->zip_pane_auxiliary_stream_next;
	}
      }
    }
  OUT(zipview_Reset_Pane_Auxiliary_Stream);
  return  zip_ok;
  }

long
zipview__Reset_Pane_Auxiliary_Streams( self, pane )
  register struct zipview		*self;
  register zip_type_pane		 pane;
  {
  register zip_type_pane_auxiliary_stream next, prior;

  IN(zipview_Reset_Pane_Auxiliary_Streams);
  if ( pane  &&  pane->zip_pane_auxiliary_stream )
    {
    next = pane->zip_pane_auxiliary_stream;
    prior = ( zip_type_pane_auxiliary_stream ) &pane->zip_pane_auxiliary_stream;
    while ( next )
      {
      prior->zip_pane_auxiliary_stream_next = next->zip_pane_auxiliary_stream_next;
      free( next );
      next = prior->zip_pane_auxiliary_stream_next;
      }
    pane->zip_pane_auxiliary_stream = NULL;
    }
  OUT(zipview_Reset_Pane_Auxiliary_Streams);
  return  zip_ok;
  }

long
zipview__Set_Pane_Coordinates( self, pane, x_origin, y_origin, width, height )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Coordinates);
  if ( pane )
    {
    if ( x_origin >= 0  &&  x_origin <= 100  &&
	 y_origin >= 0  &&  y_origin <= 100  &&
	 width  >= 0  &&  width  <= 100  &&
	 height >= 0  &&  height <= 100 )
      {
      pane->zip_pane_x_origin_percent = abs(x_origin);
      pane->zip_pane_y_origin_percent = abs(y_origin);
      pane->zip_pane_width_percent    = abs(width);
      pane->zip_pane_height_percent   = abs(height);
      if ( pane->zip_pane_attributes.zip_pane_attribute_block_area )
        Coordinate_Block_Pane( self, pane );
      else
      if ( pane->zip_pane_attributes.zip_pane_attribute_pane_area )
        Coordinate_Nested_Pane( self, pane );
      }
      else status = zip_failure; /*=== s/b invalid placement===*/
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Coordinates);
  return status;
  }

long
zipview__Set_Pane_Border( self, pane, font_name, pattern, thickness )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register char				 *font_name;
  register char				  pattern;
  register long				  thickness;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Border);
  if ( pane )
    {
    if ( font_name )
      zip_Define_Font( Data, font_name, &pane->zip_pane_border_font );
      else
      pane->zip_pane_border_font = NULL;
    pane->zip_pane_border_pattern = pattern;
    pane->zip_pane_border_thickness = thickness;
    zipview_Compute_Pane_Stretch_Factors( self, pane );
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Border);
  return status;
  }

long
zipview__Set_Pane_Painting_Mode( self, pane, mode )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register char				  mode;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Painting_Mode);
  if ( pane )
    {
    if ( mode & zipview_paint_inverted )
      pane->zip_pane_state.zip_pane_state_paint_inverted = on;
      else
      pane->zip_pane_state.zip_pane_state_paint_inverted = off;
    if ( mode & zipview_paint_copy )
      pane->zip_pane_state.zip_pane_state_paint_copy = on;
      else
      pane->zip_pane_state.zip_pane_state_paint_copy = off;
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Painting_Mode);
  return status;
  }

long
zipview__Set_Pane_Scale( self, pane, scale )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register float			  scale;
  {
  register int				  status = zip_ok;

  IN(zipview_Set_Pane_Scale);
  if ( pane )
    {
    pane->zip_pane_scale = scale;
    zipview_Compute_Pane_Stretch_Factors( self, pane );
    }
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Scale);
  return status;
  }

long
zipview__Scale_Pane( self, pane, scale )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register float			  scale;
  {
  register int				  status = zip_ok;

  IN(zipview_Scale_Pane);
  ZIP_EFN(zip_Scale_Pane_EFN);
  if ( pane )
    {
    zipview_Set_Pane_Scale( self, pane, scale );
    zipview_Display_Pane( self, pane );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Scale_Pane);
  return status;
  }

long
zipview__Scale_Pane_To_Point( self, pane, x, y, scale, mode )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_point		  x, y;
  register float			  scale;
  register long				  mode;
  {
  register long				  status = zip_ok,
					  left, top, width, height,
					  center, middle, right, bottom;
  register zip_type_pixel		  X, Y, x_offset, y_offset;

  IN(zipview_Scale_Pane_To_Point);
  DEBUGdt(x,x); DEBUGdt(y,y);
  DEBUGgt(Scale,scale);
  DEBUGxt(Mode,mode);
  pane->zip_pane_x_offset = pane->zip_pane_y_offset = 0;
  zipview_Set_Pane_Scale( self, pane, scale );
  X = zipview_X_Point_To_Pixel( self, pane, NULL, x );
  Y = zipview_Y_Point_To_Pixel( self, pane, NULL, y );
  DEBUGdt(X,X); DEBUGdt(Y,Y);
  left  = PaneLeft(pane);  top    = PaneTop(pane);
  width = PaneWidth(pane); height = PaneHeight(pane);
  center = left + width/2; middle = top + height/2;
  right = PaneRight(pane); bottom = PaneBottom(pane);
  DEBUGdt(Center,center); DEBUGdt(Middle,middle);
  if ( mode & zip_left )	    x_offset = left - X;
  else  if ( mode & zip_right )	    x_offset = right - X;
  else				    x_offset = center - X;
  if ( mode & zip_top )		    y_offset = Y - top;
  else  if ( mode & zip_bottom )    y_offset = Y - bottom;
  else				    y_offset = Y - middle;
  DEBUGdt(X-offset,x_offset);  DEBUGdt(Y-offset,y_offset);
  pane->zip_pane_x_offset = x_offset;
  pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin + pane->zip_pane_x_offset;
  pane->zip_pane_y_offset = y_offset;
  pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin - pane->zip_pane_y_offset;
  status = zipview_Scale_Pane( self, pane, scale );
  OUT(zipview_Scale_Pane_To_Point);
  return status;
  }

long
zipview__Zoom_Pane( self, pane, level )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  level;
  {
  register int				  status = zip_ok;

  IN(zipview_Zoom_Pane);
  ZIP_EFN(zip_Zoom_Pane_EFN);
  if ( pane )
    {
    pane->zip_pane_zoom_level = level;
    if ( pane->zip_pane_zoom_level >= 0 )
      pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
      else
      pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier / (abs(pane->zip_pane_zoom_level) + 1);
    zipview_Display_Pane( self, pane );
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Zoom_Pane);
  return status;
  }

long
zipview__Zoom_Pane_To_Point( self, pane, x, y, level, mode )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_point		  x, y;
  register long				  level, mode;
  {
  register long				  status = zip_ok,
					  left, top, width, height,
					  center, middle, right, bottom;
  register zip_type_pixel		  X, Y, x_offset, y_offset;

  IN(zipview_Zoom_Pane_To_Point);
  DEBUGdt(x,x); DEBUGdt(y,y);
  DEBUGdt(Level,level);
  DEBUGxt(Mode,mode);
  pane->zip_pane_x_offset = pane->zip_pane_y_offset = 0;
  pane->zip_pane_zoom_level = level;
  X = zipview_X_Point_To_Pixel( self, pane, NULL, x );
  Y = zipview_Y_Point_To_Pixel( self, pane, NULL, y );
  DEBUGdt(X,X); DEBUGdt(Y,Y);
  left  = PaneLeft(pane);  top    = PaneTop(pane);
  width = PaneWidth(pane); height = PaneHeight(pane);
  center = left + width/2; middle = top + height/2;
  right = PaneRight(pane); bottom = PaneBottom(pane);
  DEBUGdt(Center,center); DEBUGdt(Middle,middle);
  if ( mode & zip_left )	    x_offset = left - X;
  else  if ( mode & zip_right )	    x_offset = right - X;
  else				    x_offset = center - X;
  if ( mode & zip_top )		    y_offset = Y - top;
  else  if ( mode & zip_bottom )    y_offset = Y - bottom;
  else				    y_offset = Y - middle;
  DEBUGdt(X-offset,x_offset);  DEBUGdt(Y-offset,y_offset);
  pane->zip_pane_x_offset = x_offset;
  pane->zip_pane_x_origin_offset = pane->zip_pane_x_origin + pane->zip_pane_x_offset;
  pane->zip_pane_y_offset = y_offset;
  pane->zip_pane_y_origin_offset = pane->zip_pane_y_origin - pane->zip_pane_y_offset;
  status = zipview_Zoom_Pane( self, pane, level );
  OUT(zipview_Zoom_Pane_To_Point);
  return status;
  }
 /*===             BEAUTIFY      ===*/
long
zipview__X_Pixel_To_Point( self, pane, figure, x )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register long				  x;
  {
  register int				  PX = 0, SM, SD;
  register int				  status = zip_ok;

  IN(zipview_X_Pixel_To_Point);
  if ( pane )
    {
    SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1); 
/*===
	 (pane->zip_pane_zoom_level -
		((figure) ? figure->zip_figure_zoom_level : 0) + 1 );
===*/
    SD = pane->zip_pane_stretch_divisor;
    PX = x - (PaneXOrigin(pane) + pane->zip_pane_x_offset);
    PX = ((PX * SD) / ((SM) ? SM : 1)) * pane->zip_pane_x_flip;
    }
  ZIP_STATUS();
  OUT(zipview_X_Pixel_To_Point);
  return PX;
  }

long
zipview__Y_Pixel_To_Point( self, pane, figure, y )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register long				  y;
  {
  register int				  PY = 0, SM, SD;
  register int				  status = zip_ok;

  IN(zipview_Y_Pixel_To_Point);
  if ( pane )
    {
    SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1); 
/*===
	 (pane->zip_pane_zoom_level -
		((figure) ? figure->zip_figure_zoom_level : 0) + 1 );
===*/
    SD = pane->zip_pane_stretch_divisor;
    PY = -(y - (PaneYOrigin(pane) - pane->zip_pane_y_offset));
    PY = ((PY * SD) / ((SM) ? SM : 1)) * pane->zip_pane_y_flop;
    }
  ZIP_STATUS();
  OUT(zipview_Y_Pixel_To_Point);
  return PY;
  }

long
zipview__X_Point_To_Pixel( self, pane, figure, x )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register long				  x;
  {
  register int				  SM, SD, offset;
  register int				  X = 0;
  register int				  status = zip_ok;

  IN(zipview_X_Point_To_Pixel);
  if ( pane )
    {
    SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1); 
/*===
	(pane->zip_pane_zoom_level - 
		((figure) ? figure->zip_figure_zoom_level : 0) + 1);
===*/
    SD = pane->zip_pane_stretch_divisor;
    offset = pane->zip_pane_x_origin + pane->zip_pane_x_offset;
    X = offset + x * pane->zip_pane_x_flip * ((SM) ? SM : 1) / ((SD) ? SD : 1);
    }
  ZIP_STATUS();
  OUT(zipview_X_Point_To_Pixel);
  return X;
  }

long
zipview__Y_Point_To_Pixel( self, pane, figure, y )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  register long				  y;
  {
  register int				  SM, SD, offset;
  register int				  Y = 0;
  register int				  status = zip_ok;

  IN(zipview_Y_Point_To_Pixel);
  if ( pane )
    {
    SM = pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1); 
/*===
	(pane->zip_pane_zoom_level - 
		((figure) ? figure->zip_figure_zoom_level : 0) + 1);
===*/
    SD = pane->zip_pane_stretch_divisor;
    offset = pane->zip_pane_y_origin - pane->zip_pane_y_offset;
    Y = offset - y * pane->zip_pane_y_flop * ((SM) ? SM : 1) / ((SD) ? SD : 1);
    }
  ZIP_STATUS();
  OUT(zipview_Y_Point_To_Pixel);
  return Y;
  }

long
zipview__X_Point_Delta( self, pane, x_delta )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  x_delta;
  {
  register int				  status = zip_ok;
  register int				  points = 0;

  IN(zipview_X_Point_Delta);
  if ( pane )
    {
    if ( pane->zip_pane_pixels_per_point > 0 )
      points = x_delta / pane->zip_pane_pixels_per_point;
      else
      points = x_delta * pane->zip_pane_points_per_pixel;
    }
  OUT(zipview_X_Point_Delta);
  ZIP_STATUS();
  return points;
  }

long
zipview__Y_Point_Delta( self, pane, y_delta )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  y_delta;
  {
  register int				  status = zip_ok;
  register int				  points = 0;

  IN(zipview_Y_Point_Delta);
  if ( pane )
    {
    if ( pane->zip_pane_pixels_per_point > 0 )
      points = y_delta / pane->zip_pane_pixels_per_point;
      else
      points = y_delta * pane->zip_pane_points_per_pixel;
    }
  OUT(zipview_Y_Point_Delta);
  ZIP_STATUS();
  return points;
  }

static
Assign_Pane_Stream_Attributes( self, pane, stream )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_stream		  stream;
  {
/*===  Mark_Pane_Objects_Unexposed( self, pane );===*/
  pane->zip_pane_attributes.zip_pane_attribute_stream_source = false;
  pane->zip_pane_attributes.zip_pane_attribute_image_source  = false;
  pane->zip_pane_attributes.zip_pane_attribute_figure_source = false;
  if ( stream )
    {
    pane->zip_pane_attributes.zip_pane_attribute_stream_source = true;
    pane->zip_pane_source.zip_pane_stream =
	pane->zip_pane_current_stream = stream;
    if ( pane->zip_pane_current_image = stream->zip_stream_image_anchor )
      pane->zip_pane_current_figure = stream->zip_stream_image_anchor->
					zip_image_figure_anchor;
    Compute_Pane_Stream_Stretch_Factors( self, pane, stream );
    }
    else
    {
    pane->zip_pane_current_stream = NULL;
    pane->zip_pane_current_image  = NULL;
    pane->zip_pane_current_figure = NULL;
    }
  return  zip_ok;
  }

static
Assign_Pane_Image_Attributes( self, pane, image )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_image		  image;
  {
  Mark_Pane_Objects_Unexposed( self, pane );
  pane->zip_pane_attributes.zip_pane_attribute_image_source  = false;
  pane->zip_pane_attributes.zip_pane_attribute_stream_source = false;
  pane->zip_pane_attributes.zip_pane_attribute_figure_source = false;
  if ( image )
    {
    pane->zip_pane_attributes.zip_pane_attribute_image_source  = true;
    pane->zip_pane_source.zip_pane_image = image;
    pane->zip_pane_current_stream = image->zip_image_stream;
    pane->zip_pane_current_image  = image;
    pane->zip_pane_current_figure = image->zip_image_figure_anchor;
    Compute_Pane_Image_Stretch_Factors( self, pane, image );
    }
    else
    {
    pane->zip_pane_current_stream = NULL;
    pane->zip_pane_current_image  = NULL;
    pane->zip_pane_current_figure = NULL;
    }
  return  zip_ok;
  }

static
Assign_Pane_Figure_Attributes( self, pane, figure )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  Mark_Pane_Objects_Unexposed( self, pane );
  pane->zip_pane_attributes.zip_pane_attribute_figure_source = false;
  pane->zip_pane_attributes.zip_pane_attribute_image_source  = false;
  pane->zip_pane_attributes.zip_pane_attribute_stream_source = false;
  if ( figure )
    {
    pane->zip_pane_attributes.zip_pane_attribute_figure_source = true;
    pane->zip_pane_source.zip_pane_figure = figure;
    pane->zip_pane_current_stream = figure->zip_figure_image->zip_image_stream;
    pane->zip_pane_current_image  = figure->zip_figure_image;
    pane->zip_pane_current_figure = figure;
    Compute_Pane_Figure_Stretch_Factors( self, pane, figure );
    }
    else
    {
    pane->zip_pane_current_stream = NULL;
    pane->zip_pane_current_image  = NULL;
    pane->zip_pane_current_figure = NULL;
    }
  return  zip_ok;
  }

static
Mark_Pane_Objects_Unexposed( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register zip_type_image		  image;

  IN(Mark_Pane_Objects_Unexposed);
  if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source  &&
       pane->zip_pane_source.zip_pane_stream )
    {
    image = pane->zip_pane_source.zip_pane_stream->zip_stream_image_anchor;
    while ( image )
      {
      Mark_Pane_Image_Unexposed( self, pane, image );
      image = zip_Next_Image( Data, image );
      }
    }
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_image_source  &&
       pane->zip_pane_source.zip_pane_image )
    {
    Mark_Pane_Image_Unexposed( self, pane, pane->zip_pane_source.zip_pane_image );
    }
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source  &&
       pane->zip_pane_source.zip_pane_figure )
    {
    pane->zip_pane_source.zip_pane_figure = 0;
    }
  else 
  {DEBUG(Unknown Source-type);}
  OUT(Mark_Pane_Objects_Unexposed);
  }

static
Mark_Pane_Image_Unexposed( self, pane, image )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_image		  image;
  {
  register zip_type_figure		  figure = image->zip_image_figure_anchor;

  IN(Mark_Pane_Image_Unexposed);
  image->zip_image_visibility = 0;
  while (figure )
    {
    figure->zip_figure_visibility = 0;
    figure = figure->zip_figure_next;
    }
  OUT(Mark_Pane_Image_Unexposed);
  }


zipview_Recoordinate_Panes( self )
  register struct zipview		 *self;
  {
  register zip_type_pane_chain		  pane_link = PaneAnchor;

  IN(zipview_Recoordinate_Panes);
  while ( pane_link )
    {
    if ( pane_link->zip_pane_chain_ptr->zip_pane_attributes.zip_pane_attribute_block_area )
      Coordinate_Block_Pane( self, pane_link->zip_pane_chain_ptr );
    else
    if ( pane_link->zip_pane_chain_ptr->zip_pane_attributes.zip_pane_attribute_pane_area )
      Coordinate_Nested_Pane( self, pane_link->zip_pane_chain_ptr );
    else
    {DEBUG(Unknown Attribute);}
    pane_link = pane_link->zip_pane_chain_next;
    }
  OUT(zipview_Recoordinate_Panes);
  return  zip_ok;
  }

static
Coordinate_Block_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  IN(Coordinate_Block_Pane);
  pane->zip_pane_x_origin = pane->zip_pane_x_origin_offset =
    pane->zip_pane_area.zip_pane_block->left +
    (pane->zip_pane_area.zip_pane_block->width *
     zipview_Pane_X_Percent_Origin( self, pane )) / 100;
  pane->zip_pane_x_origin_offset += pane->zip_pane_x_offset;
  pane->zip_pane_y_origin = pane->zip_pane_y_origin_offset =
    pane->zip_pane_area.zip_pane_block->top +
     (pane->zip_pane_area.zip_pane_block->height *
      zipview_Pane_Y_Percent_Origin( self, pane )) / 100;
  pane->zip_pane_y_origin_offset -= pane->zip_pane_y_offset;
  pane->zip_pane_width  = (pane->zip_pane_area.zip_pane_block->width *
			zipview_Pane_Percent_Width( self, pane ))    / 100;
  pane->zip_pane_height = (pane->zip_pane_area.zip_pane_block->height *
			zipview_Pane_Percent_Height( self, pane ))   / 100;
  pane->zip_pane_left   = PaneXOrigin(pane) - (PaneWidth(pane)  / 2);
  pane->zip_pane_top    = PaneYOrigin(pane) - (PaneHeight(pane) / 2);
  zipview_Compute_Pane_Stretch_Factors( self, pane );
  OUT(Coordinate_Block_Pane);
  }

static
Coordinate_Nested_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  IN(Coordinate_Nested_Pane);
  pane->zip_pane_x_origin = pane->zip_pane_x_origin_offset =
    PaneLeft(pane->zip_pane_area.zip_pane_pane) +
     (PaneWidth(pane->zip_pane_area.zip_pane_pane) *
      zipview_Pane_X_Percent_Origin( self, pane )) / 100;
  pane->zip_pane_x_origin_offset += pane->zip_pane_x_offset;
  pane->zip_pane_y_origin = pane->zip_pane_y_origin_offset =
    PaneTop(pane->zip_pane_area.zip_pane_pane ) +
     (PaneHeight(pane->zip_pane_area.zip_pane_pane ) *
      zipview_Pane_Y_Percent_Origin( self, pane )) / 100;
  pane->zip_pane_y_origin_offset -= pane->zip_pane_y_offset;
  pane->zip_pane_width  = (PaneWidth(pane->zip_pane_area.zip_pane_pane) *
			zipview_Pane_Percent_Width( self, pane ))    / 100;
  pane->zip_pane_height = (PaneHeight(pane->zip_pane_area.zip_pane_pane) *
			zipview_Pane_Percent_Height( self, pane ))   / 100;
  pane->zip_pane_left   = PaneXOrigin(pane) - (PaneWidth(pane)  / 2);
  pane->zip_pane_top    = PaneYOrigin(pane) - (PaneHeight(pane) / 2);
  zipview_Compute_Pane_Stretch_Factors( self, pane );
  OUT(Coordinate_Nested_Pane);
  }

zipview_Compute_Pane_Stretch_Factors( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  IN(zipview_Compute_Pane_Stretch_Factors);
  if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source  &&
       pane->zip_pane_source.zip_pane_stream )
    Compute_Pane_Stream_Stretch_Factors( self, pane,
		pane->zip_pane_source.zip_pane_stream );
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_image_source  &&
       pane->zip_pane_source.zip_pane_image )
    Compute_Pane_Image_Stretch_Factors( self, pane,
		pane->zip_pane_source.zip_pane_image );
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source  &&
       pane->zip_pane_source.zip_pane_figure )
    Compute_Pane_Figure_Stretch_Factors( self, pane,
		pane->zip_pane_source.zip_pane_figure );
  else 
  {DEBUG(Unknown Source-type);}
  OUT(zipview_Compute_Pane_Stretch_Factors);
  }

static
Compute_Pane_Stream_Stretch_Factors( self, pane, stream )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_stream		  stream;
  {
  register long				  width, height;

  IN(Compute_Pane_Stream_Stretch_Factors);
  width  = (pane->zip_pane_object_width)  ?
     pane->zip_pane_object_width  : PaneWidth(pane);
  height = (pane->zip_pane_object_height) ?
     pane->zip_pane_object_height : PaneHeight(pane);
  if ( width  *
	 ((stream->zip_stream_greatest_y -
	   stream->zip_stream_least_y) + 1) >
       height *
	 ((stream->zip_stream_greatest_x -
	   stream->zip_stream_least_x) + 1) )
    {
    pane->zip_pane_stretch_multiplier = pane->zip_pane_scale *
	(height - (2 * pane->zip_pane_border_thickness));
    pane->zip_pane_stretch_divisor =
     (stream->zip_stream_greatest_y - stream->zip_stream_least_y) + 1;
    }
    else
    {
    pane->zip_pane_stretch_multiplier = pane->zip_pane_scale *
	(width - (2 * pane->zip_pane_border_thickness));
    pane->zip_pane_stretch_divisor =
     (stream->zip_stream_greatest_x - stream->zip_stream_least_x) + 1;
    }
  if ( pane->zip_pane_zoom_level >= 0 )
    pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
    else
    pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier / (abs(pane->zip_pane_zoom_level) + 1);
  pane->zip_pane_x_origin_offset =
    pane->zip_pane_x_origin + pane->zip_pane_x_offset;
  pane->zip_pane_y_origin_offset =
    pane->zip_pane_y_origin - pane->zip_pane_y_offset;

  pane->zip_pane_pixels_per_point = width /
	((2 * stream->zip_stream_greatest_x) ? (2 * stream->zip_stream_greatest_x) : 1);
  pane->zip_pane_points_per_pixel = (2 * stream->zip_stream_greatest_x) /
	((width > 0 ) ? width : 1);
  OUT(Compute_Pane_Stream_Stretch_Factors);
  }

static
Compute_Pane_Image_Stretch_Factors( self, pane, image )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_image		  image;
  {
  register long				  width, height;

  IN(Compute_Pane_Image_Stretch_Factors);
  width  = (pane->zip_pane_object_width)  ?
     pane->zip_pane_object_width  : PaneWidth(pane);
  height = (pane->zip_pane_object_height) ?
     pane->zip_pane_object_height : PaneHeight(pane);
  if ( width  *
	 ((image->zip_image_greatest_y -
	   image->zip_image_least_y) + 1) >
       height *
	 ((image->zip_image_greatest_x -
	   image->zip_image_least_x) + 1) )
    {
    pane->zip_pane_stretch_multiplier = pane->zip_pane_scale *
	(height - (2 * pane->zip_pane_border_thickness));
    pane->zip_pane_stretch_divisor =
     (image->zip_image_greatest_y - image->zip_image_least_y) + 1;
    }
    else
    {
    pane->zip_pane_stretch_multiplier = pane->zip_pane_scale *
	(width - (2 * pane->zip_pane_border_thickness));
    pane->zip_pane_stretch_divisor =
     (image->zip_image_greatest_x - image->zip_image_least_x) + 1;
    }
  if ( pane->zip_pane_zoom_level >= 0 )
    pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
    else
    pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier / (abs(pane->zip_pane_zoom_level) + 1);
  pane->zip_pane_x_offset =
    -((((image->zip_image_greatest_x - image->zip_image_least_x) / 2) +
     image->zip_image_least_x) *
	pane->zip_pane_stretch_multiplier)/pane->zip_pane_stretch_divisor;
  pane->zip_pane_x_origin_offset =
    pane->zip_pane_x_origin + pane->zip_pane_x_offset;
  pane->zip_pane_y_offset =
    -((((image->zip_image_greatest_y - image->zip_image_least_y) / 2) +
     image->zip_image_least_y) *
	pane->zip_pane_stretch_multiplier)/pane->zip_pane_stretch_divisor;
  pane->zip_pane_y_origin_offset =
    pane->zip_pane_y_origin - pane->zip_pane_y_offset;

  pane->zip_pane_pixels_per_point = width /
	((2 * image->zip_image_greatest_x) ? (2 * image->zip_image_greatest_x) : 1);
  pane->zip_pane_points_per_pixel = (2 * image->zip_image_greatest_x) /
	((width > 0 ) ? width : 1);
  OUT(Compute_Pane_Image_Stretch_Factors);
  }

static
Compute_Pane_Figure_Stretch_Factors( self, pane, figure )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  { 
  IN(Compute_Pane_Figure_Stretch_Factors);
/*===
printf("Compute_Pane_Figure_Stretch_Factors NOT YET READY\n");
===*/
  pane->zip_pane_stretch_multiplier = pane->zip_pane_scale * 1;
  pane->zip_pane_stretch_divisor = 1;
  if ( pane->zip_pane_zoom_level >= 0 )
    pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier * (pane->zip_pane_zoom_level + 1);
    else
    pane->zip_pane_stretch_zoom_multiplier =
	pane->zip_pane_stretch_multiplier / (abs(pane->zip_pane_zoom_level) + 1);
  OUT(Compute_Pane_Figure_Stretch_Factors);
  }
