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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipvp01.c,v 1.3 1992/12/15 21:58:46 rr2b R6tape $";
#endif


 

/* zipvp01.c	Zip View-object	-- Pane	Cursors			      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */



/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip View-object -- Pane Cursors	

MODULE	zipvp01.c

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
  05/01/89	Use symbolic font-names (TCP)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "view.ih"
#include "im.ih"
#include "cursor.ih"
#include "fontdesc.ih"
#include "zip.ih"
#include "zipv.ih"

#define	 Data			      (self->data_object)
#define	 View			      (self)


long
zipview__Set_Pane_Cursor( self, pane, icon, font_name )
  register struct zipview		 *self;
  register zip_type_pane		  pane;
  register char				  icon;
  register char				 *font_name;
  {
  register int				  status = zip_ok;
  register struct fontdesc		 *old_font;

  IN(zipview_Set_Pane_Cursor);
  if ( pane )
    {
    if ( pane->zip_pane_cursor_glyph == NULL )
      pane->zip_pane_cursor_glyph = cursor_Create( self );
    if ( ! font_name )
      font_name = CursorFontName;
    old_font = pane->zip_pane_cursor_font;
    pane->zip_pane_cursor_font = (struct fontdesc *)
	zip_Define_Font( Data, font_name, NULL );
    if ( pane->zip_pane_cursor_font  &&
	  (old_font != pane->zip_pane_cursor_font ||
	   pane->zip_pane_cursor_icon != icon) )
	{
	pane->zip_pane_cursor_icon = icon;
        cursor_SetGlyph( pane->zip_pane_cursor_glyph,
			 pane->zip_pane_cursor_font, pane->zip_pane_cursor_icon );

	}
    zipview_Post_Pane_Cursor( self, pane, pane->zip_pane_cursor_glyph );
    }
    else  status = zip_pane_non_existent;
  ZIP_STATUS();
  OUT(zipview_Set_Pane_Cursor);
  return status;
  }

long
zipview__Use_Working_Pane_Cursors( self )
  register struct zipview	         *self;
  {
  static  struct cursor			 *glyph = NULL;

  IN(zipview__Use_Working_Pane_Cursors);
/*===  zipview_Use_Alternate_Pane_Cursors( self, 'H', "icon12" );===*/
  if ( glyph == NULL )
    {
    glyph = cursor_Create( self );
    cursor_SetGlyph( glyph, zip_Define_Font( Data, "icon12", NULL ), 'H' );
    }
  im_SetWindowCursor( zipview_GetIM( self ), glyph );
  OUT(zipview__Use_Working_Pane_Cursors);
  return zip_ok;
  }

long
zipview__Use_Normal_Pane_Cursors( self )
  register struct zipview	     *self;
  {
  register zip_type_pane_chain	      pane_chain = PaneAnchor;

  IN(zipview__Use_Normal_Pane_Cursors);
  im_SetWindowCursor( zipview_GetIM( self ), NULL );
  while ( pane_chain )
    {
    if ( pane_chain->zip_pane_chain_ptr->zip_pane_state.zip_pane_state_exposed )
      zipview_Post_Pane_Cursor( self, pane_chain->zip_pane_chain_ptr,
			        pane_chain->zip_pane_chain_ptr->zip_pane_cursor_glyph );
    pane_chain = pane_chain->zip_pane_chain_next;
    }
  OUT(zipview__Use_Normal_Pane_Cursors);
  return zip_ok;
  }

long
zipview__Use_Alternate_Pane_Cursors( self, icon, font_name )
  register struct zipview	     *self;
  register char			      icon;
  register char			     *font_name;
  {
  register struct fontdesc	     *font;
  register zip_type_pane_chain	      pane_chain = PaneAnchor;
  static  struct cursor	    	     *glyph = NULL;
  register int			      status = zip_ok;

  IN(zipview__Use_Alternate_Pane_Cursors);
  if ( glyph == NULL )
    glyph = cursor_Create( self );
  font = (struct fontdesc *)zip_Define_Font( Data, font_name, NULL );
  cursor_SetGlyph( glyph, font, icon );
  while ( pane_chain )
    {
    if ( pane_chain->zip_pane_chain_ptr->zip_pane_state.zip_pane_state_exposed )
      zipview_Post_Pane_Cursor( self, pane_chain->zip_pane_chain_ptr, glyph );
    pane_chain = pane_chain->zip_pane_chain_next;
    }
  ZIP_STATUS();
  OUT(zipview__Use_Alternate_Pane_Cursors);
  return zip_ok;
  }

zipview__Post_Pane_Cursor( self, pane, glyph )
  register struct zipview	     *self;
  register zip_type_pane	      pane;
  register struct cursor	     *glyph;
  {
  struct  rectangle		      rectangle;

  IN(zipview__Post_Pane_Cursor);
  if ( pane->zip_pane_state.zip_pane_state_exposed )
    {
    DEBUG(Exposed);
    rectangle.left   = zipview_Pane_Left( self, pane );
    rectangle.top    = zipview_Pane_Top( self, pane );
    rectangle.width  = zipview_Pane_Width( self, pane );
    rectangle.height = zipview_Pane_Height( self, pane );
    if ( cursor_IsPosted( glyph ) )
      zipview_RetractCursor( self, glyph );
    zipview_PostCursor( self, &rectangle, glyph );
    }
  OUT(zipview__Post_Pane_Cursor);
  }
