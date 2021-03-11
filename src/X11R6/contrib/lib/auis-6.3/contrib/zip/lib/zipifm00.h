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

/* zipifm00.h	Internal Macros						      */
/* Author	TC Peters & RM LeVine					      */
/* Information Technology Center		   Carnegie-Mellon University */


/*
    $Log: zipifm00.h,v $
 * Revision 1.3  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.2.1.1  1993/02/02  06:56:23  rr2b
 * new R6tape branch
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.4  1991/09/12  20:07:51  bobg
 * Update copyright notice
 *
 * Revision 2.3  1989/02/08  16:49:37  ghoti
 * change copyright notice
 *
 * Revision 2.2  89/02/07  19:11:57  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.1  88/09/27  18:13:14  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:31:55  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:44:07  tom
 * Initial revision
 * 
 * Revision 1.1  87/10/28  21:39:34  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Figure X

MODULE	zipifm00.h

NOTICE	IBM Internal Use Only

DESCRIPTION
	These facilities are the Internal Facility Macros.

HISTORY
  05/16/86	Created (RM LeVine)
  05/16/86	Added ZIP_... ( EEFN, FEFN, SEFN, IEFN, PEFN) (RML)
  05/18/86	Added ZIP_xSTATUS ( E, S, I, F, P ) (TCP/RML)
  07/04/86	Added ZIP_REFN and ZIP_RSTATUS (RML)
  09/20/86	Added xEFN check for null panes, etc (TCP)
  08/17/87	Migration: introduce ZIP_WM_... macros (TCP)
  03/31/88	Revise for ATK (TCP)

END-SPECIFICATION  ************************************************************/

#define ZIP_EFN( facility )\
	  Facility = facility;

#define ZIP_STATUS()\
	if ( (Status = status) != zip_success )\
	  {zip_Try_general_Exception_Handler( Data );}

#define ZIP_Select_Figure_Font( view, figure )\
	zipview_SetFont( view, Fonts->zip_fonts_vector[figure->zip_figure_font].zip_fonts_table_font)

#define ZIP_Select_Image_Font( view, image )\
	zipview_SetFont( view, Fonts->zip_fonts_vector[image->zip_image_font].zip_fonts_table_font )

#define ZIP_Select_Stream_Font( view, stream )\
	zipview_SetFont( view, Fonts->zip_fonts_vector[stream->zip_stream_font].zip_fonts_table_font )

#define ZIP_Select_Pane_Font( view, pane )\
	zipview_SetFont( view, Fonts->zip_fonts_vector[pane->zip_pane_font].zip_fonts_table_font )
