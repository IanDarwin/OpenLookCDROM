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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipdf01.c,v 1.4 1993/09/23 20:53:09 gk5g Exp $";
#endif


 

/* zipedf01.c	Zip Data-object	-- Figures  Generics		      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip Data-object -- Figures  Generics

MODULE	zipedf01.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Figure facilities
	of the Zip Data-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
    Such curiosities need be resolved prior to Project Completion...


HISTORY
  03/31/88	Created (TCP)
  11/17/88	Add Superior_Image_Line_Width (TCP/SCG)
  05/10/89	Improve behaviour of Superior_Image_Line_Width (SCG)
   08/14/90	Add Superior_Image_<Attribute> methods for color and line styles (SCG)

END-SPECIFICATION  ************************************************************/

#include <andrewos.h>
#include "class.h"
#include "dataobj.ih"
#include "zip.ih"
#include <sys/stat.h>
#include <ctype.h>

#define	 Data			      self


char
zip__Superior_Image_Pattern( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register char			      pattern = NULL;

  while ( image )
    {
    if ( image->zip_image_fill.zip_image_pattern )
      {
      pattern = image->zip_image_fill.zip_image_pattern;
      break;
      }
    image = image->zip_image_superior;
    }
  return pattern;
  }

char
zip__Superior_Image_Shade( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register char			      shade = NULL;

  while ( image )
    {
    if ( image->zip_image_fill.zip_image_shade )
      {
      shade = image->zip_image_fill.zip_image_shade;
      break;
      }
    image = image->zip_image_superior;
    }
  return shade;
  }

unsigned char
zip__Superior_Image_Line_Width( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register unsigned char	      width = 255;

  while ( image )
    {
    if ( image->zip_image_line_width != 255 )
      {
      width = image->zip_image_line_width;
      break;
      }
    image = image->zip_image_superior;
    }
  return width;
  }

long
zip__Superior_Image_Line_Dash( self, image, pattern, offset, type )
  register struct zip		     *self;
  register zip_type_image	      image;
  register char			     **pattern;
  register int			     *offset;
  register short		     *type;
  {
  register long			    status = zip_failure;

  while ( image )
    {
    if ( image->zip_image_line_dash_pattern )
      {
      *pattern = image->zip_image_line_dash_pattern;
      *offset = image->zip_image_line_dash_offset;
      *type = image->zip_image_line_dash_type;
      status = zip_ok;
      break;
      }
    image = image->zip_image_superior;
    }
  return status;
  }

short
zip__Superior_Image_Line_Cap( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register short	      cap = -1;

  while ( image )
    {
    if ( image->zip_image_line_cap != -1 )
      {
      cap = image->zip_image_line_cap;
      break;
      }
    image = image->zip_image_superior;
    }
  return cap;
  }

short
zip__Superior_Image_Line_Join( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register short	      join = -1;

  while ( image )
    {
    if ( image->zip_image_line_join != -1 )
      {
      join = image->zip_image_line_join;
      break;
      }
    image = image->zip_image_superior;
    }
  return join;
  }

struct zip_color *
zip__Superior_Image_Line_Color( self, image )
  register struct zip		    *self;
  register zip_type_image	    image;
  {
  register struct zip_color	    *color = NULL;

  while ( image )
    {
    if ( image->zip_image_color_values && image->zip_image_color_values->line )
      {
      color = image->zip_image_color_values->line;
      break;
      }
    image = image->zip_image_superior;
    }
  return color;
  }

struct zip_color *
zip__Superior_Image_FillFG_Color( self, image )
  register struct zip		    *self;
  register zip_type_image	    image;
  {
  register struct zip_color	    *color = NULL;

  while ( image )
    {
    if ( image->zip_image_color_values && image->zip_image_color_values->fillfg )
      {
      color = image->zip_image_color_values->fillfg;
      break;
      }
    image = image->zip_image_superior;
    }
  return color;
  }

struct zip_color *
zip__Superior_Image_FillBG_Color( self, image )
  register struct zip		    *self;
  register zip_type_image	    image;
  {
  register struct zip_color	    *color = NULL;

  while ( image )
    {
    if ( image->zip_image_color_values && image->zip_image_color_values->fillbg )
      {
      color = image->zip_image_color_values->fillbg;
      break;
      }
    image = image->zip_image_superior;
    }
  return color;
  }

char *
zip__Superior_Image_Text( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register char			     *text = NULL;

  while ( image )
    {
    if ( image->zip_image_text )
      {
      text = image->zip_image_text;
      break;
      }
    image = image->zip_image_superior;
    }
  return text;
  }


struct fontdesc *
zip__Superior_Image_Font( self, image )
  register struct zip		     *self;
  register zip_type_image	      image;
  {
  register struct fontdesc 	     *font = NULL;

  while ( image )
    {
    if ( image->zip_image_font )
      {
      font = Fonts->
		zip_fonts_vector[image->zip_image_font].zip_fonts_table_font;
      break;
      }
    image = image->zip_image_superior;
    }
  return font;
  }
