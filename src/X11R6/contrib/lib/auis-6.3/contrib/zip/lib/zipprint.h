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

/* zipefr00.h	Zip  Subroutine Library Printing Objects Header			*/
/* Author	TC Peters & RM LeVine					        */
/* Information Technology Center		   Carnegie-Mellon University   */

/*
	$Log: zipprint.h,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.2.1.1  1993/02/02  07:02:19  rr2b
 * new R6tape branch
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.5  1991/09/12  20:09:13  bobg
 * Update copyright notice
 *
 * Revision 2.4  1990/08/21  14:41:41  sg08
 * Add fields for line styles to zip_printing struct
 *
 * Revision 2.3  89/02/08  16:52:02  ghoti
 * change copyright notice
 * 
 * Revision 2.2  88/10/11  20:38:22  tom
 * Handle Line-width.
 * 
 * Revision 2.1  88/09/27  18:18:59  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:47:26  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:47:09  tom
 * Initial revision
 * 
 * Revision 1.1  87/10/28  21:38:52  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	Zip Figure X

MODULE	zipefr00.h

NOTICE	IBM Internal Use Only

DESCRIPTION
	These facilities are the External structure definitions and some macros.

HISTORY
  07/04/86	Created (RM LeVine)
  08/07/86	Clean-up (TCP)
  09/22/87	Add Print Language (TCP)
   08/14/90	Add fields for line styles to zip_printing struct (SCG)
  
END-SPECIFICATION ************************************************************/

#define  zip_troff				1
#define  zip_postscript				2

#define  zip_printing_page_width_default 	8
#define  zip_printing_page_height_default	11
#define  zip_printing_resolution_default	7200

#define  Printing			  View->printing

typedef  struct zip_printing		 *zip_type_printing;

struct zip_printing
  {
  long					  zip_printing_pel_width;
  long					  zip_printing_pel_height;
  float					  zip_printing_inch_width;
  float					  zip_printing_inch_height;
  float					  zip_printing_resolution; /* Number of dots per inch */
  float					  zip_printing_stretch_divisor;
  float					  zip_printing_stretch_multiplier;
  float					  zip_printing_stretch_zoom_multiplier;
  zip_type_percent			  zip_printing_height_percent;
  zip_type_percent			  zip_printing_width_percent;
  zip_type_percent			  zip_printing_x_origin_percent;
  zip_type_percent			  zip_printing_y_origin_percent;
					  /* Page info */
  FILE					 *zip_printing_file;
  zip_type_pixel			  zip_printing_current_x;
  zip_type_pixel			  zip_printing_current_y;
  char					  zip_printing_language;
  char					  zip_printing_processor;
  boolean				  zip_printing_level;
  long					  zip_printing_orientation;
  char					 *zip_printing_prefix;
  char					  zip_printing_line_width;
  char					 *zip_printing_line_dash_pattern;
  int					  zip_printing_line_dash_offset;
  short					  zip_printing_line_cap;
  short					  zip_printing_line_join;
  float					  zip_printing_shade;
  };

#define  zipprint_Printing_File( Print )\
	(Printing->zip_printing_file)

#define  zipprint_Printing_Prefix( Print )\
	(Printing->zip_printing_prefix)

#define  zipprint_Printing_X_Origin( Print )\
	(Printing->zip_printing_pel_width/2)
#define  zipprint_Printing_Y_Origin( Print )\
	(Printing->zip_printing_pel_height/2)

#define  zipprint_Printing_Current_X( Print )\
	(Printing->zip_printing_current_x)
#define  zipprint_Printing_Current_Y( Print )\
	(Printing->zip_printing_current_y)

#define  zipprint_Printing_Line_Width( Print )\
	(Printing->zip_printing_line_width)

#define  zipprint_Printing_Shade( Print )\
	(Printing->zip_printing_shade)
