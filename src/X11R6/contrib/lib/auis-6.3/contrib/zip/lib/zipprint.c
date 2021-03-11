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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipprint.c,v 1.6 1993/05/04 01:51:05 susan Exp $";
#endif


 

/*LIBS:  -lm
*/

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/* zipprint.c	Zip PrintView-object				      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */


/*
    $Log: zipprint.c,v $
 * Revision 1.6  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.4.1.1  1993/02/02  07:01:59  rr2b
 * new R6tape branch
 *
 * Revision 1.4  1993/01/08  16:36:06  rr2b
 * cutting down on duplicate global symbols
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
 * Revision 2.7  1991/09/12  16:44:12  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.6  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.5  1991/08/26  22:30:21  gk5g
 * Patch submitted by Guy Harris:
 * ATK seems not to have made up its mind whether PostScript format in printing is called "postscript" or "PostScript", nor even whether "troff" format is called "troff" or "Troff". Attached are some patches that teach some insets confused about this subject that they're called "PostScript" and "troff", respectively.
 * The patches to "zip" succeeded in improving the quality of the PostScript it generated, by causing it to be properly preceded with "\!" or whatever so that "*roff" would know what to do with it.
 *
 * Revision 2.4  1989/02/17  18:08:51  ghoti
 * ifdef/endif,etc. label fixing - courtesy of Ness
 *
 * Revision 2.3  89/02/08  16:51:55  ghoti
 * change copyright notice
 * 
 * Revision 2.2  89/02/07  20:24:13  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.1  88/09/27  18:18:45  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:47:03  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:47:03  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip PrintView-object

MODULE	zipprint.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Zip PrintView-object.

    NB: The comment-symbol "===" indicates areas which are:
	    1 - Questionable
	    OR
	    2 - Arbitrary
	    OR
	    3 - Temporary Hacks
   Such curiosities need be resolved prior to Project Completion...

HISTORY
  03/31/88	Created for ATK (TCP)

END-SPECIFICATION  ************************************************************/

#include <stdio.h>
#include "class.h"
#include <math.h>
#include "graphic.ih"
#include "view.ih"
#include "im.ih"
#include "rect.h"
#include "environ.ih"
#include <ctype.h>
#include "fontdesc.ih"
#include "zip.ih"
#include "zipv.ih"
#include "zipobj.ih"
#include "zipprint.eh"

static boolean debug=FALSE;
#define	 Data			     (self->data_object)
#define  View			     (self->view_object)
#define	 Print			     (self)
#define	 Objects(i)		   ((*View->objects)[i])

#define  PelWidth		(Printing->zip_printing_pel_width)
#define  PelHeight		(Printing->zip_printing_pel_height)
#define  PelResolution		(Printing->zip_printing_resolution)
#define  InchWidth		(Printing->zip_printing_inch_width)
#define  InchHeight		(Printing->zip_printing_inch_height)

static Print_Figure();
static Print_Image();
static int Print_Inferior_Image();
static Compute_Printing_Slug_Stretch_Factors();
static Set_Printing_Characteristics();

boolean
zipprint__InitializeObject( classID, self)
  register struct classheader	      *classID;
  register struct zipprint	      *self;
  {
  IN(zipprint_InitializeObject );
/*===*/
  OUT(zipprint_InitializeObject );
  return TRUE;
  }

void 
zipprint__FinalizeObject( classID, self )
  register struct classheader	      *classID;
  register struct zipprint	      *self;
  {
  IN(zipprint_FinalizeObject );
/*===*/
  if ( Printing )  free( Printing );
  OUT(zipprint_FinalizeObject );
  }

void
zipprint__Set_Data_Object( self, data_object )
  register struct zipprint	      *self;
  register struct zip		      *data_object;
  {
  IN(zipprint_Set_Data_Object );
  Data = data_object;
  OUT(zipprint_Set_Data_Object );
  }

void
zipprint__Set_View_Object( self, view_object )
  register struct zipprint	     *self;
  register struct zipview	     *view_object;
  {
  IN(zipprint_Set_View_Object);
  View = view_object;
  if ( Printing =
		(zip_type_printing) calloc( 1, sizeof(struct zip_printing) ) )
    {
    Printing->zip_printing_resolution = zip_printing_resolution_default; 
    Printing->zip_printing_width_percent  = 100;
    Printing->zip_printing_height_percent = 100;
    Printing->zip_printing_x_origin_percent = 50;
    Printing->zip_printing_y_origin_percent = 50;
    Printing->zip_printing_inch_width = zip_printing_page_width_default;
    Printing->zip_printing_inch_height = zip_printing_page_height_default;
    Printing->zip_printing_pel_width =
	zip_printing_resolution_default * zip_printing_page_width_default;
    Printing->zip_printing_pel_height =
	zip_printing_resolution_default * zip_printing_page_height_default;
    }
  OUT(zipprint_Set_View_Object);
  }

void
zipprint__Set_Debug( self, state )
  register struct zipprint	      *self;
  register char			       state;
  {
  IN(zipprint_Set_Debug);
  debug = state;
  OUT(zipprint_Set_Debug);
  }

long
zipprint__Set_Print_Language( self, language )
  register struct zipprint		 *self;
  register char				 *language;
  {
  register char				  language_code = 0;

  IN(zipprint_Set_Print_Language);
  DEBUGst(Language,language);
  if ( apt_MM_Compare( language, "PostScript" ) == 0 )
    language_code = zip_postscript;
  else
  if ( apt_MM_Compare( language, "Troff" ) == 0 )
    language_code = zip_troff;
  Printing->zip_printing_language = language_code;
  OUT(zipprint_Set_Print_Language);
  return  zip_ok;
  }

long
zipprint__Set_Print_Processor( self, processor )
  register struct zipprint		 *self;
  register char				 *processor;
  {
  register char				  processor_code = 0;

  IN(zipprint_Set_Print_Processor);
  DEBUGst(Processor,processor);
  if ( apt_MM_Compare( processor, "PostScript" ) == 0 )
    processor_code = zip_postscript;
  else
  if ( apt_MM_Compare( processor, "troff" ) == 0 )
    processor_code = zip_troff;
  Printing->zip_printing_processor = processor_code;
  OUT(zipprint_Set_Print_Processor);
  return zip_ok;
  }

long
zipprint__Set_Print_Level( self, level )
  register struct zipprint		 *self;
  register long				  level;
  {
  IN(zipprint_Set_Print_Level);
  DEBUGdt(Level,level);
  Printing->zip_printing_level = level;
  OUT(zipprint_Set_Print_Level);
  return zip_ok;
  }

long
zipprint__Set_Print_File( self, file )
  register struct zipprint		 *self;
  register FILE				 *file;
  {
  IN(zipprint_Set_Print_File);
  Printing->zip_printing_file = file;
  OUT(zipprint_Set_Print_File);
  return zip_ok;
  }

long
zipprint__Set_Print_Resolution( self, resolution )
  register struct zipprint		 *self;
  register long				  resolution;
  {
  IN(zipprint_Set_Print_Resolution);
  DEBUGdt(Resolution,resolution);
  Printing->zip_printing_resolution = resolution;
  OUT(zipprint_Set_Print_Resolution);
  return zip_ok;
  }

long
zipprint__Set_Print_Dimensions( self, inch_width, inch_height )
  register struct zipprint		 *self;
  register float			  inch_width, inch_height;
  {
  IN(zipprint_Set_Print_Dimensions);
  DEBUGgt(Inch-Width,inch_width);
  DEBUGgt(Inch-Height,inch_height);
  Printing->zip_printing_inch_width = inch_width;
  Printing->zip_printing_inch_height = inch_height;
  OUT(zipprint_Set_Print_Dimensions);  
  return zip_ok;
  }

long
zipprint__Set_Print_Coordinates( self, x_origin, y_origin, width, height )
  register struct zipprint		 *self;
  register zip_type_percent		  x_origin, y_origin, width, height;
  {
  int					  status = zip_success;

  IN(zipprint_Set_Print_Coordinates);
  DEBUGdt(X-Origin-Pct,x_origin);
  DEBUGdt(Y-Origin-Pct,y_origin);
  DEBUGdt(Width-Pct,width);
  DEBUGdt(Height-Pct,height);
  Printing->zip_printing_x_origin_percent = x_origin;
  Printing->zip_printing_y_origin_percent = y_origin;
  Printing->zip_printing_height_percent = height;
  Printing->zip_printing_width_percent = width;
  OUT(zipprint_Set_Print_Coordinates);
  return status;
  }

long
zipprint__Set_Print_Orientation( self, orientation )
  register struct zipprint		 *self;
  register long				  orientation;
  {
  IN(zipprint_Set_Print_Orientation);
  Printing->zip_printing_orientation = orientation;
  OUT(zipprint_Set_Print_Orientation);
  return zip_ok;
  }

long
zipprint__Print_Figure( self, figure, pane )
  register struct zipprint	       *self;
  register zip_type_figure		figure;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(zipprint_Print_Figure);
  if ( figure )
    {
    Compute_Printing_Slug_Stretch_Factors( self, pane );
    zipprint_Write_Print_Datastream_Header( self );
    Print_Figure( self, figure, pane );
    zipprint_Write_Print_Datastream_Trailer( self );
    }
  OUT(zipprint_Print_Figure);
  return  status;
  }

static
Print_Figure( self, figure, pane )
  register struct zipprint	       *self;
  register zip_type_figure		figure;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(Print_Figure);
  if ( ! figure->zip_figure_state.zip_figure_state_deleted )
/*===figure->zip_figure_visibility == zip_figure_exposed===*/
    {
    Set_Printing_Characteristics( self, pane, figure );
    status = zipobject_Print_Object( Objects(figure->zip_figure_type), figure, pane );
    }
  OUT(Print_Figure);
  return  status;
  }


long
zipprint__Print_Image( self, image, pane )
  register struct zipprint	       *self;
  register zip_type_image		image;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(zipprint_Print_Image);
  if ( image )
    {
    Compute_Printing_Slug_Stretch_Factors( self, pane );
    zipprint_Write_Print_Datastream_Header( self );
    Print_Image( self, image, pane );
    zipprint_Write_Print_Datastream_Trailer( self );
    }
  OUT(zipprint_Print_Image);
  return  status;
  }

static
Print_Image( self, image, pane )
  register struct zipprint	       *self;
  register zip_type_image		image;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;
  register zip_type_figure		figure_ptr;

  IN(Print_Image);
  if ( 1 /*===image->zip_image_visibility == zip_image_exposed===*/ )
    {
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_success )
      {
      status = Print_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
     }
    if ( image->zip_image_inferior  &&  status == zip_success )
      status = Print_Inferior_Image( self, image->zip_image_inferior, pane );
    }
  OUT(Print_Image);
  return  status;
  }

static int
Print_Inferior_Image( self, image, pane )
  register struct zipprint	       *self;
  register zip_type_image	        image;
  register zip_type_pane		pane;
  {
  register int			        status = zip_success;
  register zip_type_figure		figure_ptr;

  IN(Print_Inferior_Image);
  if ( 1 /*===image->zip_image_visibility == zip_image_exposed===*/ )
    {
    figure_ptr = image->zip_image_figure_anchor;
    while ( figure_ptr  &&  status == zip_success )
      {
      status = Print_Figure( self, figure_ptr, pane );
      figure_ptr = figure_ptr->zip_figure_next;
      }
    if ( image->zip_image_inferior  &&  status == zip_success )
      status = Print_Inferior_Image( self, image->zip_image_inferior, pane );
    if ( image->zip_image_peer  &&  status == zip_success )
      status = Print_Inferior_Image( self, image->zip_image_peer, pane );
    }
  OUT(Print_Inferior_Image);
  return status;
  }

long
zipprint__Print_Stream( self, stream, pane )
  register struct zipprint	       *self;
  register zip_type_stream		stream;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(zipprint_Print_Stream);
  if ( stream )
    {
    Compute_Printing_Slug_Stretch_Factors( self, pane );
    zipprint_Write_Print_Datastream_Header( self );
    Print_Image( self, zip_Image_Root( Data, stream ), pane );
    zipprint_Write_Print_Datastream_Trailer( self );
    }
  OUT(zipprint_Print_Stream);
  return  status;
  }

long
zipprint__Print_Pane( self, pane )
  register struct zipprint	       *self;
  register zip_type_pane		pane;
  {
  register long			        status = zip_ok;

  IN(zipprint_Print_Pane);
  if ( pane )
    {
    if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source  &&
         pane->zip_pane_source.zip_pane_stream  &&
         pane->zip_pane_source.zip_pane_stream->zip_stream_image_anchor )
      status = zipprint_Print_Stream( self, pane->zip_pane_source.zip_pane_stream, pane );
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_image_source  &&
         pane->zip_pane_source.zip_pane_image )
      status = zipprint_Print_Image( self, pane->zip_pane_source.zip_pane_image, pane );
    else
    if ( pane->zip_pane_attributes.zip_pane_attribute_figure_source  &&
         pane->zip_pane_source.zip_pane_figure )
      status = zipprint_Print_Figure( self, pane->zip_pane_source.zip_pane_figure, pane );
    else status = zip_failure; /*=== s/b missing  pane-source ===*/
    }
  OUT(zipprint_Print_Pane);
  return  status;
  }

static
Compute_Printing_Slug_Stretch_Factors( self, pane )
  register struct zipprint	         *self;
  register zip_type_pane		  pane;
  {
  register long				  greatest_x=1, least_x=1, greatest_y=1, least_y=1;

  IN(Compute_Printing_Slug_Stretch_Factors);
  if ( pane->zip_pane_attributes.zip_pane_attribute_stream_source )
    {
    greatest_x = pane->zip_pane_source.zip_pane_stream->zip_stream_greatest_x;
    least_x    = pane->zip_pane_source.zip_pane_stream->zip_stream_least_x;
    greatest_y = pane->zip_pane_source.zip_pane_stream->zip_stream_greatest_y;
    least_y    = pane->zip_pane_source.zip_pane_stream->zip_stream_least_y;
    }
  else
  if ( pane->zip_pane_attributes.zip_pane_attribute_image_source )
    {
    greatest_x = pane->zip_pane_source.zip_pane_image->zip_image_greatest_x;
    least_x    = pane->zip_pane_source.zip_pane_image->zip_image_least_x;
    greatest_y = pane->zip_pane_source.zip_pane_image->zip_image_greatest_y;
    least_y    = pane->zip_pane_source.zip_pane_image->zip_image_least_y;
    }
  PelWidth  = PelResolution * InchWidth;
  PelHeight = PelResolution * InchHeight;
  if ( PelWidth  * ((greatest_y - least_y) + 1) >
       PelHeight * ((greatest_x - least_x) + 1) )
    {
    Printing->zip_printing_stretch_multiplier =
	 PelHeight;
    Printing->zip_printing_stretch_divisor    =
	 (greatest_y - least_y) + 1; 
    }
    else
    {
    Printing->zip_printing_stretch_multiplier =
	 PelWidth;
    Printing->zip_printing_stretch_divisor =
	 (greatest_x - least_x) + 1; 
    }
  OUT(Compute_Printing_Slug_Stretch_Factors);
  }

static
Set_Printing_Characteristics( self, pane, figure )
  register struct zipprint	         *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  IN(Set_Printing_Characteristics);
  if ( pane->zip_pane_zoom_level >= 0 )
    Printing->zip_printing_stretch_zoom_multiplier = pane->zip_pane_scale *
	 (Printing->zip_printing_stretch_multiplier
         * (pane->zip_pane_zoom_level - figure->zip_figure_zoom_level + 1 ));
    else
    Printing->zip_printing_stretch_zoom_multiplier = pane->zip_pane_scale *
	 (Printing->zip_printing_stretch_multiplier
        / (((abs(pane->zip_pane_zoom_level - figure->zip_figure_zoom_level)) > 0 ) ?
            (abs(pane->zip_pane_zoom_level - figure->zip_figure_zoom_level)): 2));
  OUT(Set_Printing_Characteristics);
  }
/*=== === ===*/

int 
apt_MM_Compare( s1, s2 )
  /* Assumes "s1" must be shifted to lower-case
             "s2" must be shifted to lower-case
  */
  register unsigned char		 *s1, *s2;
  {
  register unsigned char		  c1, c2;
  register int				  result = 0;

  do
    {
    c1 = isupper( *s1 ) ? tolower( *s1++ ) : *s1++;
    c2 = isupper( *s2 ) ? tolower( *s2++ ) : *s2++;
    if ( c1 != c2 )
      break;
    } while ( c1 );
  if ( c1 != c2 )
    if ( c1 > c2 )
      result = 1;
      else result = -1;
  return result;
  }

