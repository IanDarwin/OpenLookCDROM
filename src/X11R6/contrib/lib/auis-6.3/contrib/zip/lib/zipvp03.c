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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipvp03.c,v 1.5 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipvp03.c	Zip View-object	-- Display/Draw Facilities	      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipvp03.c,v $
 * Revision 1.5  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  07:05:01  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  22:00:08  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.12  1991/09/12  16:45:52  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.11  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.10  1990/08/21  14:47:50  sg08
 * Add Normalize_Line_Attributes in Draw_Pane_Border
 *
 * Revision 2.9  89/08/30  16:36:20  sg08
 * Use zipview_Condition when drawing Pane Borders 
 * 
 * Revision 2.8  89/07/20  13:14:50  sg08
 * utilize new Stream_Visible method to optimize drawing of Auxiliary Streams
 * 
 * Revision 2.7  89/02/24  18:35:28  ghoti
 * hc fixes
 * 
 * Revision 2.6  89/02/17  18:11:03  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.5  89/02/08  16:53:12  ghoti
 * change copyright notice
 * 
 * Revision 2.4  89/02/07  21:32:05  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.3  88/11/16  18:59:14  tom
 * Add use of Pre/Post/Processors to Draw as well as Display.
 * 
 * Revision 2.2  88/10/11  20:44:11  tom
 * Correct setting of Stream flags.
 * 
 * Revision 2.1  88/09/27  18:21:42  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:55:55  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:48:46  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object -- Display/Draw Facilities	

MODULE	zipvp03.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Pane Display/Draw facilities
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
  10/18/88	Add use of Pre/Post/Processors to Draw as well as Display (TCP)
  07/12/89	Invoke Stream_Visible to optimize Draw_Auxiliary_Streams (SCG)
  08/30/89	Use zipview_Condition when drawing Pane Borders (SCG)
   08/16/90	Add Normalize_Line_Attributres in Draw_Pane_Border (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "fontdesc.ih"
#include "zip.ih"
#include "zipv.ih"
#include "zipedit.ih"


#define	 Data			      (self->data_object)
#define	 View			      (self)
#define	 Edit			      (self->edit_object)

#define  PaneLeft		      (zipview_Pane_Left( self, pane ))
#define  PaneTop		      (zipview_Pane_Top( self, pane ))
#define  PaneWidth		      (zipview_Pane_Width( self, pane ))
#define  PaneHeight		      (zipview_Pane_Height( self, pane ))
#define  PaneBottom		      (zipview_Pane_Bottom( self, pane ))
#define  PaneRight		      (zipview_Pane_Right( self, pane ))


static int Show_Pane();
static int Pane_Suite_Member();
static int Draw_Auxiliary_Streams();

long
zipview__Display_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Display_Pane);
  if ( pane )
    {
    if ( zipview_Pane_Overlaying( self, pane )  &&
         ! zipview_Pane_Exposed( self, pane ) )
      zipview_Preserve_Overlay( self, pane );
    if ( ! zipview_Pane_Transparent( self, pane ) )
      zipview_Clear_Pane( self, pane );
    zipview_Compute_Pane_Stretch_Factors( self, pane );
    status = Show_Pane( self, pane, zip_display_action );
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipview_Display_Pane);
  return status;
  }

long
zipview__Draw_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Draw_Pane);
  if ( pane )
    {
    status = Show_Pane( self, pane, zip_draw_action );
    }
    else 
    status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipview_Draw_Pane);
  return status;
  }

static int
Show_Pane( self, pane, action )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register long				  action;
  {
  register int				  status = zip_ok;

  IN(Show_Pane);
  zipview_Mark_Pane_Exposed( self, pane );
  zipview_Set_Pane_Clip_Area( self, pane );
  if ( pane->zip_pane_display_preprocessor )
    { DEBUG(>>> Display Preprocessor);
    status = (*pane->zip_pane_display_preprocessor)
		( pane->zip_pane_display_processor_anchor, pane, action );
    DEBUG(<<< Display Preprocessor);
    }
  if ( pane->zip_pane_display_processor )
    { DEBUG(>>> Display Processor);
    status = (*pane->zip_pane_display_processor)
		( pane->zip_pane_display_processor_anchor, pane, action );
    DEBUG(<<< Display Processor);
    }
    else
      {
      Draw_Auxiliary_Streams( self, pane );
      if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source  &&
           pane->zip_pane_source.zip_pane_stream  &&
           pane->zip_pane_source.zip_pane_stream->zip_stream_image_anchor )
        status = zipview_Draw_Stream( self, pane->zip_pane_source.zip_pane_stream, pane );
      else
      if ( pane->zip_pane_attributes.zip_pane_attribute_image_source  &&
           pane->zip_pane_source.zip_pane_image  &&
           pane->zip_pane_source.zip_pane_image->zip_image_figure_anchor )
        status = zipview_Draw_Image( self, pane->zip_pane_source.zip_pane_image, pane );
      else
      if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source  &&
           pane->zip_pane_source.zip_pane_figure )
        status = zipview_Draw_Figure( self, pane->zip_pane_source.zip_pane_figure, pane );
      else
        {
        status = zipview_Clear_Pane( self, pane ); /*=== missing  pane-source ===*/
        }
      if ( status == zip_ok )
        if ( pane->zip_pane_border_thickness )
          zipview_Draw_Pane_Border( self, pane );
	  else
	  { DEBUG(Border Zero);
	  if ( pane->zip_pane_state.zip_pane_state_coordinates_exposed )
            zipedit_Draw_Pane_Coordinates( Edit, pane );
            else
            if ( pane->zip_pane_edit  &&  pane->zip_pane_edit->zip_pane_edit_coordinate_grid )
              zipedit_Draw_Pane_Grid( Edit, pane );
	  }
      if ( status == zip_ok  &&  pane->zip_pane_cursor_glyph )
        zipview_Post_Pane_Cursor( self, pane, pane->zip_pane_cursor_glyph );
      zipview_FlushGraphics( self );
      }
  if ( pane->zip_pane_display_postprocessor )
    { DEBUG(>>> Display Postprocessor);
    status = (*pane->zip_pane_display_postprocessor)
		( pane->zip_pane_display_processor_anchor, pane, action );
    DEBUG(<<< Display Postprocessor);
    }
  OUT(Show_Pane);
  return  status;
  }

long
zipview__Redisplay_Panes( self )
  register struct zipview		 *self;
  {
  register int				  status = zip_ok;
  register zip_type_pane_chain		  pane_link = PaneAnchor;

  IN(zipview_Redisplay_Panes);
  status = zipview_Recoordinate_Panes( self );
  while ( pane_link  &&  status == zip_ok )
    {
    if ( ! (zipview_Pane_Hidden( self,  pane_link->zip_pane_chain_ptr ) ||
	    zipview_Pane_Removed( self, pane_link->zip_pane_chain_ptr ) ) )
      {
      zipview_Pane_Exposed( self, pane_link->zip_pane_chain_ptr ) = false;
      status = zipview_Display_Pane( self, pane_link->zip_pane_chain_ptr );
      }
    pane_link = pane_link->zip_pane_chain_next;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Redisplay_Panes);
  return status;
  }

long
zipview__Redraw_Panes( self )
  register struct zipview		 *self;
  {
  register int				  status = zip_ok;
  register zip_type_pane_chain		  pane_link =
			    PaneAnchor;

  IN(zipview_Redraw_Panes);
  zipview_Recoordinate_Panes( self );
  while ( pane_link  &&  status == zip_ok )
    {
    if ( ! (zipview_Pane_Hidden( self,  pane_link->zip_pane_chain_ptr ) ||
	    zipview_Pane_Removed( self, pane_link->zip_pane_chain_ptr ) ) )
      {
      zipview_Pane_Exposed( self, pane_link->zip_pane_chain_ptr ) = false;
      status = zipview_Draw_Pane( self, pane_link->zip_pane_chain_ptr );
      }
    pane_link = pane_link->zip_pane_chain_next;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Redraw_Panes);
  return status;
  }

long
zipview__Redisplay_Pane_Suite( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;
  register zip_type_pane_chain		  pane_link =
			    PaneAnchor;

  IN(zipview_Redisplay_Pane_Suite);
  zipview_Recoordinate_Panes( self );
  while ( pane_link  &&  status == zip_ok )
    {
    if ( Pane_Suite_Member( self, pane, pane_link->zip_pane_chain_ptr ) )
      {
      zipview_Pane_Exposed( self, pane_link->zip_pane_chain_ptr ) = false;
      status = zipview_Display_Pane( self, pane_link->zip_pane_chain_ptr );
      }
    pane_link = pane_link->zip_pane_chain_next;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Redisplay_Pane_Suite);
  return status;
  }

long
zipview__Redraw_Pane_Suite( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;
  register zip_type_pane_chain		  pane_link = PaneAnchor;

  IN(zipview_Redraw_Pane_Suite);
  zipview_Recoordinate_Panes( self );
  while ( pane_link  &&  status == zip_ok )
    {
    if ( Pane_Suite_Member( self, pane, pane_link->zip_pane_chain_ptr ) )
      {
      zipview_Pane_Exposed( self, pane_link->zip_pane_chain_ptr ) = false;
      status = zipview_Draw_Pane( self, pane_link->zip_pane_chain_ptr );
      }
    pane_link = pane_link->zip_pane_chain_next;
    }
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Redraw_Pane_Suite);
  return status;
  }

static int
Pane_Suite_Member( self, major_pane, candidate_pane )
  register struct zipview		 *self;
  register zip_type_pane		  major_pane;
  register zip_type_pane		  candidate_pane;
  {
  register int				  status = false;
  register zip_type_pane		  superior_pane;

  IN(Pane_Suite_Member);
  if ( candidate_pane == major_pane  &&
       !(zipview_Pane_Hidden( self,  candidate_pane ) ||
	 zipview_Pane_Removed( self, candidate_pane ) ) )
    status = true;
    else
    {
    if ( candidate_pane->zip_pane_attributes.zip_pane_attribute_pane_area  &&
	 !(zipview_Pane_Hidden( self,  candidate_pane ) ||
	   zipview_Pane_Removed( self, candidate_pane ) ) )
      {
      if ( candidate_pane->zip_pane_area.zip_pane_pane == major_pane )
	status = true;
	{
	superior_pane = candidate_pane->zip_pane_area.zip_pane_pane;
	while ( superior_pane )
	  {
	  if ( superior_pane->zip_pane_area.zip_pane_pane == major_pane )
	    {
	    status = true;
	    break;
	    }
	    else
	    if ( superior_pane->zip_pane_attributes.zip_pane_attribute_pane_area )
	      superior_pane = superior_pane->zip_pane_area.zip_pane_pane;
	      else break;
	  }
	}
      }
    }
  OUT(Pane_Suite_Member);
  return status;
  }

static int
Draw_Auxiliary_Streams( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;
  register zip_type_pane_auxiliary_stream ptr;

  IN(Draw_Auxiliary_Streams);
  if ( pane->zip_pane_auxiliary_stream )
    {
    zipview_Set_Pane_Clip_Area( self, pane );
    ptr = pane->zip_pane_auxiliary_stream;
    while ( status == zip_ok  &&  ptr )
      {
      if ( ptr->zip_pane_auxiliary_stream_ptr  &&
	   ptr->zip_pane_auxiliary_stream_visibility != zip_pane_hidden  &&
           zipview_Stream_Visible( self, ptr->zip_pane_auxiliary_stream_ptr, pane ) == TRUE &&
	  (status =  zipview_Draw_Image( self, ptr->zip_pane_auxiliary_stream_ptr->
				    zip_stream_image_anchor, pane )) == zip_ok )
        {
	ptr->zip_pane_auxiliary_stream_visibility = zip_pane_exposed;
	if ( pane->zip_pane_auxiliary_stream->zip_pane_auxiliary_stream_density )
	  zipedit_Lighten_Pane( Edit, pane,
	     pane->zip_pane_auxiliary_stream->zip_pane_auxiliary_stream_density );
	}
      ptr = ptr->zip_pane_auxiliary_stream_next;
      }
    }
  DEBUGdt(Status,status);
  OUT(Draw_Auxiliary_Streams);
  return status;
  }

zipview_Draw_Pane_Border( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;
  register struct graphic		 *graphic;
  register int				  thickness =
				    pane->zip_pane_border_thickness;
  register short		          font = 
				    pane->zip_pane_border_font;
  register char				  pattern =
				    pane->zip_pane_border_pattern;

  IN(zipview_Draw_Pane_Border);
  if ( thickness > 0  &&
      ! pane->zip_pane_state.zip_pane_state_coordinates_exposed )
    { DEBUGdt(Thickness,thickness);
    zipview_Set_Clip_Area( self, pane, PaneLeft, PaneTop,
			   PaneWidth, PaneHeight );
    zipview_Set_Pane_Painting_Mode( self, pane, zip_default );
    zipview_Normalize_Line_Attributes( self );
    zipview_Condition( self, pane, NULL, zip_draw );
    if ( pane->zip_pane_border_pattern )
      {
      graphic = zipview_Define_Graphic( self,
	    Fonts->zip_fonts_vector[font].zip_fonts_table_font,
	    pattern );
      zipview_FillTrapezoid( self, /* Top */
	PaneLeft, PaneTop, PaneWidth,
	PaneLeft, PaneTop + thickness,	PaneWidth, graphic );
      zipview_FillTrapezoid( self, /* Bottom */
	PaneLeft, PaneBottom - thickness, PaneWidth,
	PaneLeft, PaneBottom, PaneWidth, graphic );
      zipview_FillTrapezoid( self, /* Left */
	PaneLeft, PaneTop + thickness, thickness,
	PaneLeft, PaneBottom - thickness, thickness, graphic );
      zipview_FillTrapezoid( self, /* Right */
	PaneRight - thickness, PaneTop + thickness, thickness,
	PaneRight - thickness, PaneBottom - thickness, thickness, graphic );
      }
      else
      {
      zipview_FillRectSize( self,
	 PaneLeft, PaneTop,
	 PaneWidth, thickness, zipview_BlackPattern( self ) );
      zipview_FillRectSize( self,
	 PaneRight - thickness, PaneTop,
	 thickness, PaneHeight, zipview_BlackPattern( self ) );
      zipview_FillRectSize( self,
	 PaneLeft, PaneBottom - thickness,
	 PaneWidth, thickness, zipview_BlackPattern( self ) );
      zipview_FillRectSize( self,
	 PaneLeft, PaneTop,
	 thickness, PaneHeight, zipview_BlackPattern( self ) );
      }
    if ( pane->zip_pane_edit  &&  pane->zip_pane_edit->zip_pane_edit_coordinate_grid )
      zipedit_Draw_Pane_Grid( Edit, pane );
/*    zipview_SetTransferMode( self, graphic_BLACK ); */
    zipview_Set_Pane_Clip_Area( self, pane );
    }
    else
    { DEBUG(Coordinates/Grid);
    if ( pane->zip_pane_state.zip_pane_state_coordinates_exposed )
      zipedit_Draw_Pane_Coordinates( Edit, pane );
      else
      if ( pane->zip_pane_edit  &&  pane->zip_pane_edit->zip_pane_edit_coordinate_grid )
        zipedit_Draw_Pane_Grid( Edit, pane );
    }
  OUT(zipview_Draw_Pane_Border);
  return  status;
  }

long
zipview__Hide_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int						  status = zip_ok;

  IN(zipview_Hide_Pane);
  if ( pane )
    {
    if ( zipview_Pane_Exposed( self, pane ) )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
/*===      ZIP_WM_DefineRegion( pane->zip_pane_region_id, 0, 0, 0,0 );===*/
/*===      ZIP_WM_ForgetRegion( pane->zip_pane_region_id );===*/
/*===
      if ( pane->zip_pane_saved_region_id )
        {
       ZIP_WM_DefineRegion( pane->zip_pane_saved_region_id, 0, 0, 0,0 );
        ZIP_WM_ForgetRegion( pane->zip_pane_saved_region_id ); 
        }
===*/
/*===      pane->zip_pane_saved_region_id = ZIP_Region_ID( pane );===*/
      zipview_Set_Pane_Clip_Area( self, pane );
/*===      ZIP_WM_SaveRegion( pane->zip_pane_saved_region_id, 
		       PaneLeft,  PaneTop,
		       PaneWidth, PaneHeight );===*/
      zipview_Set_Pane_Clip_Area( self, pane );
      if ( 0 /*===pane->zip_pane_other_region_id===*/ )
        zipview_Restore_Overlay( self, pane );
        else
        {
        zipview_SetTransferMode( self, graphic_WHITE );
        zipview_FillRectSize( self,
		    PaneLeft, PaneTop,
		    PaneWidth, PaneHeight,
		    zipview_WhitePattern( self ) /*===*/ );
        zipview_SetTransferMode( self, graphic_BLACK );
        }
      zipview_Mark_Pane_Hidden( self, pane );
      }
    }
    else  status = zip_pane_non_existent;
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Hide_Pane);
  return status;
  }

long
zipview__Expose_Pane( self, pane )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  {
  register int				  status = zip_ok;

  IN(zipview_Expose_Pane);
  if ( pane )
    {
    if ( zipview_Pane_Hidden( self, pane ) )
      {
      zipview_Set_Pane_Clip_Area( self, pane );
      if ( zipview_Pane_Overlaying( self, pane ) )
        zipview_Preserve_Overlay( self, pane );
      zipview_Set_Pane_Clip_Area( self, pane );
      zipview_SetTransferMode( self, graphic_COPY );
/*===      ZIP_WM_RestoreRegion( ZIP_Pane_Saved_Region( pane ),
		    PaneLeft(pane), PaneTop(pane) );===*/
      zipview_SetTransferMode( self, graphic_BLACK );
      zipview_Set_Pane_Clip_Area( self, pane );
      zipview_Mark_Pane_Exposed( self, pane );
      }
    } 
    else  status = zip_pane_non_existent;
  zipview_FlushGraphics( self );
  ZIP_STATUS();
  OUT(zipview_Expose_Pane);
  return status;
  }

int
zipview_Preserve_Overlay( self, pane )
register struct zipview		 *self;
register zip_type_pane		  pane;
{
    IN(zipview_Preserve_Overlay);
    /*===
      if ( pane->zip_pane_other_region_id )
      {
	  ZIP_WM_DefineRegion( ZIP_Pane_Saved_Region( pane ), 0, 0, 0,0 );
	  ZIP_WM_ForgetRegion( pane->zip_pane_other_region_id );
      }
      pane->zip_pane_other_region_id = ZIP_Region_ID( pane );
      zipview_Set_Clip_Area( self, pane,
			     PaneLeft(pane),
			     PaneTop(pane),
			     PaneWidth(pane),
			     PaneHeight(pane) );
      ZIP_WM_SaveRegion( pane->zip_pane_other_region_id, 
			 PaneLeft, PaneTop,
			 PaneWidth, PaneHeight );
      zipview_Set_Pane_Clip_Area( self, pane );
      ===*/
    OUT(zipview_Preserve_Overlay);
    return(0);
}


int
zipview_Restore_Overlay( self, pane )
register struct zipview		 *self;
register zip_type_pane		  pane;
{
    IN(zipview_Restore_Overlay);
    /*===
      zipview_Set_Clip_Area( self, pane,
			     PaneLeft(pane),  PaneTop(pane),
			     PaneWidth(pane), PaneHeight(pane) );
      zipview_SetTransferMode( self, graphic_COPY );
      ZIP_WM_RestoreRegion( pane->zip_pane_other_region_id,
			    PaneLeft(pane), PaneTop(pane) );
      zipview_SetTransferMode( self, graphic_BLACK );
      zipview_Set_Pane_Clip_Area( self, pane );
      ZIP_WM_DefineRegion( pane->zip_pane_other_region_id, 0, 0, 0,0 );
      ZIP_WM_ForgetRegion( pane->zip_pane_other_region_id );
      pane->zip_pane_other_region_id = NULL;
      ===*/
    OUT(zipview_Restore_Overlay);
    return(0);
}

