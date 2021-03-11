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

/* zip.h	Zip Subroutine Library general Header			      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zip.h,v $
 * Revision 1.5  1993/09/22  19:53:56  gk5g
 * made debugging macros ansi
 * BUILD
 *
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:53:35  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1993/01/08  16:36:06  rr2b
 * cutting down on duplicate global symbols
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.9  1991/09/12  20:07:18  bobg
 * Update copyright notice
 *
 * Revision 2.8  1990/08/21  14:12:59  sg08
 * Added zip_color_values and zip_color struct declarations
 *
 * Revision 2.7  89/05/05  21:08:02  tom
 * Correct define of zip_type_pixel: short s/b long.
 * 
 * Revision 2.6  89/05/01  22:08:25  tom
 * Use symbolic special font-names.
 * 
 * Revision 2.5  89/02/17  18:06:12  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 * 
 * Revision 2.4  89/02/08  16:48:34  ghoti
 * change copyright notice
 * 
 * Revision 2.3  89/02/04  18:40:07  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.2  88/10/11  21:10:09  tom
 * Added zip_display|draw_action.
 * 
 * Revision 2.1  88/09/27  18:10:15  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:24:37  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:43:00  tom
 * Initial revision
 * 
 * Revision 1.2  87/11/11  21:58:13  tom
 * Improve Figure Handles;
 * Add Absolute/Relative feature.
 * 
 * Revision 1.1  87/10/28  21:37:20  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	Zip Subroutine Library general Header

MODULE	zip.h

NOTICE	IBM Internal Use Only

DESCRIPTION
	This file is to be included in the compilation of both client-programs
	and the Zip Subroutine Library modules. It defines the Environment
	structure and general symbolics. It includes all other Zip header-files
	required for client-program compilation.

HISTORY
  06/03/84	Created (TCP)
  05/01/85	Converted to use in Subroutine Library environment (TCP)
  12/10/85	Eliminated Image and Figure Chain-units (TCP)
  12/31/85	Reference "zip/zip...h" (TCP)
  05/18/86	Add status and facility fields to environment (TCP/RML)
		Add general, Pane, Stream, Image, Figure Exception Handlers
  06/04/86	Added new (temporary) fields to environment object (RM LeVine)
  06/26/86	Added zipiff03.h and zipefn00.h to include suite. (RML)
  07/03/86	Removed the temporary print fields form object (RML)
  07/11/86	Added font table structure and font elments to environment structure (RML)
  07/18/86	Added include of zipefe00.h (Editing) (TCP)
  07/21/86	Added current-pane (TCP)
  07/29/86	Add zip_opaque, etc (TCP)
  08/29/86	Remove automatic includes of Internal header-files (TCP)
		(zipifm00.h and zipiff00.h)
  08/17/87	Migration: Add "graphic" ptr (TCP)
  10/19/87	Define apt_DEBUG suite iff undefined (TCP)
  03/31/88	Revise for ATK (TCP)
  10/11/87	Add DB ifdef, zip_display_action & zip_draw_action (TCP)
  05/01/89	Add symbolic names for zip fonts (name shortening) (TCP)
  05/05/89	Correct zip_type_pixel: short s/b long (TCP)
  08/14/90	Added zip_color_values and zip_color struct declarations (SCG)

END-SPECIFICATION  ************************************************************/

typedef  long					  zip_type_pixel;
typedef  short					  zip_type_percent;

#include <stdio.h>
#include "zipfig.h"
#include "zipimage.h"
#include "zipprint.h"
#include "zipstrm.h"
#include "zippane.h"
#include "zipefn00.h"
#include "zipefc00.h"
#include "zipiff00.h"
#include "zipifm00.h"


#ifndef DEBUG
/* char    debug; */
#ifdef DB
#ifndef __STDC__
#define  IN( x )	    if ( debug ) printf( ">x\n" );
#define  OUT( x )	    if ( debug ) printf( "<x\n" );
#define  DEBUG( y )	    if ( debug ) printf( " %s\n", "y" );
#define  DEBUGc( y )	    if ( debug ) printf( " '%c'\n", "y" );
#define  DEBUGct( t, y )    if ( debug ) printf( " t: '%c'\n", y );
#define  DEBUGs( y )	    if ( debug ) printf( " '%s'\n", "y" );
#define  DEBUGst( t, y )    if ( debug ) printf( " t: '%s'\n", y );
#define  DEBUGd( y )	    if ( debug ) printf( " %d\n", y );
#define  DEBUGdt( t, y )    if ( debug ) printf( " t: %d\n", y );
#define  DEBUGlt( t, y )    if ( debug ) printf( " t: %D\n", y );
#define  DEBUGgt( t, y )    if ( debug ) printf( " t: %g\n", y );
#define  DEBUGx( y )	    if ( debug ) printf( " %x\n", y );
#define  DEBUGxt( t, y )    if ( debug ) printf( " t: %x\n", y );
#else
#define  IN( x )	    if ( debug ) printf( ">" #x "\n" );
#define  OUT( x )	    if ( debug ) printf( "<" #x "\n" );
#define  DEBUG( y )	    if ( debug ) printf( " %s\n", #y );
#define  DEBUGc( y )	    if ( debug ) printf( " '%c'\n", #y );
#define  DEBUGct( t, y )    if ( debug ) printf( " t: '%c'\n", y );
#define  DEBUGs( y )	    if ( debug ) printf( " '%s'\n", #y );
#define  DEBUGst( t, y )    if ( debug ) printf( " t: '%s'\n", y );
#define  DEBUGd( y )	    if ( debug ) printf( " %d\n", y );
#define  DEBUGdt( t, y )    if ( debug ) printf( " t: %d\n", y );
#define  DEBUGlt( t, y )    if ( debug ) printf( " t: %D\n", y );
#define  DEBUGgt( t, y )    if ( debug ) printf( " t: %g\n", y );
#define  DEBUGx( y )	    if ( debug ) printf( " %x\n", y );
#define  DEBUGxt( t, y )    if ( debug ) printf( " t: %x\n", y );
#endif /* __STDC__ */
#else /* DB */
#define  IN( x )
#define  OUT( x )
#define  DEBUG( y )
#define  DEBUGc( y )
#define  DEBUGct( t, y )
#define  DEBUGs( y )
#define  DEBUGst( t, y )
#define  DEBUGd( y ) 
#define  DEBUGdt( t, y )
#define  DEBUGlt( t, y )
#define  DEBUGgt( t, y )
#define  DEBUGx( y ) 
#define  DEBUGxt( t, y )
#endif /* DB */
#endif /* DEBUG */


#define  ShadeFontName		      "zipshd16"
#define  DotFontName		      "zipdot20"
#define  IconFontName		      "zipicn20"
#define  CursorFontName		      "aptcsr20"
#define  TopoFontName		      "ziptop10"

#define  abs( x )			(((x)<0)?-(x):(x))

#define  yes	1
#define  no	0
#define  true	1
#define  false	0
#define  on	1
#define  off	0

#define	 PageCount		      Data->page_count
#define	 Stream			      Data->stream
#define	 StreamFileName		      Data->stream_file_name
#define  WriteStreamFile	      Data->write_stream_file
#define  WriteStreamId		      Data->write_stream_id
#define  WriteStreamLevel	      Data->write_stream_level
#define	 Id			      Data->id
#define	 DesiredWidth		      Data->desired_view_width
#define	 DesiredHeight		      Data->desired_view_height
#define	 DesiredMetric		      Data->desired_metric
#define	 ObjectWidth		      Data->object_width
#define	 ObjectHeight		      Data->object_height

#define  Paths			      Data->paths
#define  StreamAnchor		      Data->stream_anchor
#define  ImageAnchor		      Data->image_anchor
#define  FigureAnchor		      Data->figure_anchor
#define  MessageWriter		      Data->message_writer
#define  MessageClearer		      Data->message_clearer
#define  MessageAcknowledger	      Data->message_acknowledger
#define  generalExceptionHandler      Data->general_exception_handler
#define  StreamExceptionHandler       Data->stream_exception_handler
#define  ImageExceptionHandler        Data->image_exception_handler
#define  FigureExceptionHandler       Data->figure_exception_handler

#define  Facility		      Data->facility
#define  Status			      Data->status
#define  StatusAddenda		      Data->status_addenda

#define  Fonts			      Data->fonts

#define  PaneAnchor		      View->pane_anchor

#define  zip_default			(0)
#define  zip_opaque			(0)
#define  zip_non_balanced		(0)
#define  zip_normal			(0)
#define  zip_portrait			(0)
#define  zip_landscape			(1 << 0)
#define  zip_display_action		(1 << 0)
#define  zip_overlay			(1 << 1)
#define  zip_draw_action		(1 << 1)
#define  zip_read			(1 << 1)
#define  zip_inverted			(1 << 2)
#define  zip_write			(1 << 2)
#define  zip_centered			(1 << 3)
#define  zip_update			(1 << 3)
#define  zip_balanced			(1 << 4)
#define  zip_transparent		(1 << 5)

#define  zip_menu_palette		(1<<0)
#define  zip_figure_palette		(1<<1)
#define  zip_name_palette		(1<<2)
#define  zip_font_palette		(1<<3)
#define  zip_shade_palette		(1<<4)
#define  zip_color_palette		(1<<5)
#define  zip_attribute_palette		(1<<6)
#define  zip_hierarchy_palette		(1<<7)


struct zip_color
{
  double	red, green, blue;
};

struct zip_color_values
{
    struct zip_color	*line;
    struct zip_color	*fillfg;
    struct zip_color	*fillbg;
};

typedef  struct zip_paths			 *zip_type_paths;
typedef  char					 *zip_type_path;
struct zip_paths
  {
  long						  zip_paths_count;
  zip_type_path					  zip_paths_vector[1];
  };

typedef  struct zip_fonts			 *zip_type_fonts;
typedef  struct zip_fonts_table			  zip_type_font;
struct zip_fonts_table
  {
  struct fontdesc				 *zip_fonts_table_font;
  char						 *zip_fonts_table_name;
  };

struct zip_fonts
  {
  short						  zip_fonts_count;
  zip_type_font					  zip_fonts_vector[1];
  };
