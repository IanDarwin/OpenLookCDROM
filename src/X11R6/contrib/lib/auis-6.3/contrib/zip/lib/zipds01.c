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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipds01.c,v 1.5 1993/05/04 01:51:05 susan Exp $";
#endif


 

/* zipds01.c	Zip Data-object	-- Stream Input Parsing		      */
/* Author	TC Peters					      */
/* Information Technology Center	   Carnegie-Mellon University */



/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Zip Data-object -- Stream Input Parsing

MODULE	zipds01.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This is the suite of Methods that support the Stream facilities
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
   08/14/90	Add code to deparse color and line style attributes (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "dataobj.ih"
#include "zipobj.ih"
#include "zip.ih"
#include <ctype.h>


#define	 Data			      (self)
#define	 Objects(i)		      ((*self->objects)[i])

static  zip_type_stream		      stream;
static  zip_type_image		      image;
static  zip_type_figure		      figure;
static  char			      msg[512];

static  char			     *Unique_Name(),
				      NextChar(), PriorChar();
static  int			      position;
static double				Parse_Stream_Real();

static int Substitute_Referenced_Stream();
static int Parse_Figure_Unit_Attributes();
static int Parse_Stream_Figure();
static int Parse_Stream_Image_Beginning();
static int Parse_Stream_Image_Ending();
static int Parse_Image_Attributes();
static int Extract_Attribute();
static int Parse_Stream_Integer();
static Parse_Stream_Commentary();
static Equivalent_Token();
static Parse_Presentation_Parameter();

long
zip__Read_Figure( self, figure )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      status = zip_ok;

  IN(zip__Read_Figure);
  NextChar();
  if ( (status = zip_Parse_Figure_Point( self, figure,
		    &figure->zip_figure_point.zip_point_x,
		    &figure->zip_figure_point.zip_point_y )) == zip_ok )
    {
    zip_Set_Image_Extrema( self, zip_Figure_Image( self, figure ),
		    figure->zip_figure_point.zip_point_x,
		    figure->zip_figure_point.zip_point_y );
    Parse_Figure_Unit_Attributes( self, figure );
    if ( (status = zip_Parse_Figure_Attributes( self, figure )) == zip_ok )
      status = zip_Parse_Figure_Points( self, figure );
    }
  DEBUGdt(Status,status);
  OUT(zip__Read_Figure);
  return status;
  }

long
zip_Deparse_Stream( self, stream_object )
  register struct zip		     *self;
  register zip_type_stream	      stream_object;
  {
  register long			      status = zip_ok;
  register char			      c;
/*===debug=1;===*/
  IN(zip_Parse_Stream);
  figure = NULL;
  stream = stream_object;
  position = 0;
  if (( image = zip_Image_Root( self, stream_object )) == NULL )
    {
    if ( (status = zip_Create_Inferior_Image( self, &image,
		     "ZIP_ROOT_IMAGE", stream, NULL )) == zip_ok )
      image->zip_image_state.zip_image_state_transient = true;
    stream->zip_stream_image_anchor = image;
    }
  stream->zip_stream_greatest_x = 0;
  stream->zip_stream_greatest_y = 0;
/*===  stream->zip_stream_pseudo_x_offset = 0;
  stream->zip_stream_pseudo_y_offset = 0;===*/
  c = NextChar();
  while ( c  &&  status == zip_ok )
    {
    switch ( c )
      {
      case '*':
        status = Parse_Stream_Figure( self );
        break;
      case '{':
	status = Parse_Stream_Image_Beginning( self );
        break;
      case '}':
	status = Parse_Stream_Image_Ending( self );
        break;
      case '\\':
        status = zipobject_Read_Object_Stream(
		 Objects(figure->zip_figure_type), figure,
		 stream->zip_stream_file, 12345/*===*/ );
        break;
      case '%':
	status = Parse_Presentation_Parameter( self );
	break;
      case '!':
	status = Substitute_Referenced_Stream( self );
	break;
      default:
	if ( !Parse_Stream_Commentary( self, c ) )
          status = zip_stream_positioning_error;
      }
    c = NextChar();
    }
  if ( status == zip_ok )
    {
    if ( image  &&  image->zip_image_figure_anchor )
      zip_Set_Stream_Extrema( self, stream, image );
    }
    else
    {
    DEBUGdt( BAD status, status );
    DEBUGct( CURSOR AT, c);
    DEBUGdt( POSITION, position);
    }
  OUT(zip_Deparse_Stream);
  return status;
  }

static int
Substitute_Referenced_Stream( self )
  register struct zip		     *self;
  {
  register long			      status;
  char				     *file_name;

  IN(Substitute_Referenced_Stream);
  if ( (status = Extract_Attribute( self, &file_name )) == zip_ok )
    {
    DEBUGst(File-name,file_name);
    zip_Close_Stream_File( self, stream );
    if ( (status = zip_Set_Stream_File_Name( self, stream, file_name )) == zip_ok )
      {
      if ( (status = zip_Open_Stream_File( self, stream, zip_default )) == zip_ok )
	stream->zip_stream_attributes.zip_stream_attribute_reference = true;
      }
    }
  OUT(Substitute_Referenced_Stream);
  return  status;
  }

long
zip__Parse_Figure_Point( self, figure, x, y )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  register zip_type_point	     *x, *y;
  {
  register int			      negative;
  register int			      v;
  register char			      c;

  IN(zip__Parse_Figure_Point);
  c = NextChar();
    negative = false;
    if ( c == '-' )
      {
      negative = true;
      c = NextChar();
      }
    v = 0;
    while ( c >= '0'  &&  c <= '9' )
      {
      v = v * 10 + ( c - '0');
      c = NextChar();
      }
    if ( negative )
      *x = -v;
      else
      *x =  v;
    DEBUG(Half-Done);
    if ( c == ',' )
      c = NextChar();
    negative = false;
    if ( c == '-' )
      {
      negative = true;
      c = NextChar();
      }
    v = 0;
    while ( c >= '0'  &&  c <= '9' )
      {
      v = v * 10 + ( c - '0');
      c = NextChar();
      }
    if ( negative )
      *y = -v;
      else
      *y =  v;
  if ( c != '\n' )
    PriorChar( c );
  OUT(zip__Parse_Figure_Point);
  return zip_ok;
  }

long
zip__Parse_Figure_Points( self, figure )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      i = 0;
  register char			      c;
  register long			      status = zip_ok;

  IN(zip__Parse_Figure_Points);
  while ( Parse_Stream_Commentary( self, c = NextChar() ) ) ;
  if ( c == '>'  ||  c == ';' )
    {
    status = zip_Allocate_Figure_Points_Vector( self, &figure->zip_figure_points );
    while ( c  &&  (c == '>'  ||  c == ';')  &&  status == zip_ok )
      {
      if ( (i % zip_points_allocation) == 0 )
        status = zip_Enlarge_Figure_Points_Vector( self, &figure->zip_figure_points );
      if ( status == zip_ok  &&
	  (status = zip_Parse_Figure_Point( self, figure,
			&figure->zip_figure_points->zip_points[i].zip_point_x,
		 	&figure->zip_figure_points->zip_points[i].zip_point_y )) == zip_ok )
        {
        zip_Set_Image_Extrema( self, image, figure->zip_figure_points->zip_points[i].zip_point_x,
		 		 figure->zip_figure_points->zip_points[i].zip_point_y );
        figure->zip_figure_points->zip_points_count = ++i;
        }
      c = NextChar();
      }
    }
  if ( c != '\n' )
    PriorChar( c );
  DEBUGdt(Status,status);
  OUT(zip__Parse_Figure_Points);
  return status; 
  }

static int
Parse_Figure_Unit_Attributes( self, figure )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register char			      c;

  IN(Parse_Figure_Unit_Attributes);
  /* Skip over any obsolete syntactic elements */
  if ( (c = NextChar()) == ';' )
    while ( (c = NextChar()) != '\n' ) ;
    else  PriorChar( c );
  OUT(Parse_Figure_Unit_Attributes);
  return  zip_ok;
  }

long
zip__Parse_Figure_Attributes( self, figure )
  register struct zip		     *self;
  register zip_type_figure	      figure;
  {
  register int			      end_of_attributes, offset, i = 0;
  register long			      status = zip_ok;
  register char			      c, first, second;
  unsigned char			      element = 1;
  char				     *attribute_ptr,  pattern[10];
  register short			cap;
  register double			red, green, blue;

  IN(zip__Parse_Figure_Attributes);
  end_of_attributes = false;
  while ( ! end_of_attributes  &&  status == zip_ok )
    {
    switch ( c = NextChar() )
      {
      case  'A':	/* Annotation */
	DEBUG(Attribute A);
	status = Extract_Attribute( self, &attribute_ptr
			     /*===&figure->zip_figure_annotation===*/ );
	break;
      case  'C':	/* Color */
	DEBUG(Attribute C);
	c = NextChar();
        red = Parse_Stream_Real(); NextChar();
        green = Parse_Stream_Real(); NextChar();
        blue = Parse_Stream_Real();
	switch ( c )
        {
	    case 'L': zip_Set_Figure_Line_Color( self, figure, red, green, blue ); break;
       	    case 'F': zip_Set_Figure_FillFG_Color( self, figure, red, green, blue ); break;
       	    case 'B': zip_Set_Figure_FillBG_Color( self, figure, red, green, blue ); break;
	}
        break;
      case  'M':	/* Mode */
	DEBUG(Attribute M);
        first  = c = NextChar();/*===cleanse===*/
	DEBUGct(first,first);
        second = c = NextChar();
        DEBUGct(second,second);
	if ( first == 'T'  ||  first == 't'  || second == 'T'  || second == 't' )
	  figure->zip_figure_mode.zip_figure_mode_top = on;
	if ( first == 'M'  ||  first == 'm'  || second == 'M'  || second == 'm' )
	  figure->zip_figure_mode.zip_figure_mode_middle = on;
	if ( first == 'B'  ||  first == 'b'  || second == 'B'  || second == 'b' )
	  figure->zip_figure_mode.zip_figure_mode_bottom = on;
	if ( first == 'L'  ||  first == 'l'  || second == 'L'  || second == 'l' )
	  figure->zip_figure_mode.zip_figure_mode_left = on;
	if ( first == 'C'  ||  first == 'c'  || second == 'C'  || second == 'c' )
	  figure->zip_figure_mode.zip_figure_mode_center = on;
	if ( first == 'R'  ||  first == 'r'  || second == 'R'  || second == 'r' )
	  figure->zip_figure_mode.zip_figure_mode_right = on;
	if ( (c = NextChar()) == 'H' )
	  figure->zip_figure_mode.zip_figure_mode_halo = on;
	  else  PriorChar( c );
        break;
      case  'P':	/* Pattern */
	DEBUG(Attribute P);
	figure->zip_figure_mode.zip_figure_mode_patterned = on;
	figure->zip_figure_fill.zip_figure_pattern = NextChar();
        break;
      case  'G':	/* Grey-level Shade */
	DEBUG(Attribute G);
	figure->zip_figure_mode.zip_figure_mode_shaded = on;
	figure->zip_figure_fill.zip_figure_shade = Parse_Stream_Integer();
        break;
      case  'F':	/* Font */
	DEBUG(Attribute F);
	if ( (status = Extract_Attribute( self, &attribute_ptr )) == zip_ok )
	  {
	  zip_Define_Font( self, attribute_ptr, &figure->zip_figure_font );
	  free( attribute_ptr );
/*===Need to set extrema for Caption, but cannot now compute: need to be in View!===*/
	  }
        break;
      case  'N':	/* Name */
	DEBUG(Attribute N);
  	status = Extract_Attribute( self, &attribute_ptr );
        if ( attribute_ptr )
	  {
	  if ( (status = zip_Set_Figure_Name( self, figure, attribute_ptr )) ==
		    zip_duplicate_figure_name )
	    {
	    register int		  suffix = 0;
	    register char		 *new_name = NULL;

	    while( status == zip_duplicate_figure_name )
	      {
	      status = zip_Set_Figure_Name( self, figure, new_name =
			    Unique_Name( attribute_ptr, suffix++ ) );
	      }
/*===*/printf("Changed Dup Figure Name To '%s'\n", new_name);
	    status = zip_ok;
	    }
	  free( attribute_ptr );
	  }
        break;
      case  'R':	/* Reference */
	DEBUG(Attribute R);
  	status = Extract_Attribute( self, &figure->zip_figure_datum.zip_figure_text );
        break;
      case  'T':	/* Text */
	DEBUG(Attribute T);
	if ( (status = Extract_Attribute( self,
			    &figure->zip_figure_datum.zip_figure_text )) == zip_ok )
	  {
	  if ( figure->zip_figure_type == zip_caption_figure  &&
	       figure->zip_figure_font )
	    {
/*===Need to set extrema for Caption, but cannot now compute: need View!
	    zip_Select_Figure_Font( self, figure );
	    ZIP_WM_StringWidth( figure->zip_figure_datum.zip_figure_text,
				&string_width, &string_height );
            zip_Set_Image_Extrema( self, image, figure->zip_figure_point.zip_point_x +
					    string_width,
			                  figure->zip_figure_point.zip_point_y );
===*/
	    }
	  }
        break;
      case  'Z':	/* Zoom-level */
	DEBUG(Attribute Z);
        figure->zip_figure_zoom_level = Parse_Stream_Integer();
        break;
      case  'D':	/* Detail-level */
	DEBUG(Attribute D);
	figure->zip_figure_detail_level = Parse_Stream_Integer();
        break;
      case  'S':	/* Style */
	DEBUG(Attribute S);
	figure->zip_figure_style = Parse_Stream_Integer();
        break;
      case  'L':	/* Line ... */
	DEBUG(Attribute L);
	c = NextChar();
	switch ( c )
        {
	    case 'W': figure->zip_figure_line_width = Parse_Stream_Integer(); break;
	    case 'C': if (( c = NextChar()) == 'N' ) cap = graphic_CapNotLast;
		else if ( c == 'B' ) cap = graphic_CapButt;
		else if ( c == 'R' ) cap = graphic_CapRound;
		else if ( c == 'P' ) cap = graphic_CapProjecting;
		else { cap = graphic_CapButt; printf( "zip: unrecognized line cap attribute: '%c'\n", c ); }
		zip_Set_Figure_Line_Cap( self, figure, cap ); break;
	    case 'J': if (( c = NextChar()) == 'M' ) cap = graphic_JoinMiter;
		else if ( c == 'R' ) cap = graphic_JoinRound;
		else if ( c == 'B' ) cap = graphic_JoinBevel;
		else { cap = graphic_JoinMiter; printf( "zip: unrecognized line join attribute: '%c'\n", c ); }
		zip_Set_Figure_Line_Join( self, figure, cap ); break;
	    case 'D': if (( c = NextChar()) == 'S' ) cap = graphic_LineSolid;
	        else if ( c == 'D' ) cap = graphic_LineDoubleDash;
		else if ( c == 'O' ) cap = graphic_LineOnOffDash;
		NextChar(); offset = Parse_Stream_Integer(); NextChar();
		while( element != 0 )
		{
		    pattern[i++] = element = ( char ) Parse_Stream_Integer();
		    if ( element ) NextChar();
		}
		pattern[i] = 0;
		zip_Set_Figure_Line_Dash( self, figure, pattern, offset, cap );
		break;
	}
        break;
      default:
  	DEBUG(Attribute Default);
	if ( ! Parse_Stream_Commentary( self, c ) )
	  {
	  c = PriorChar( c );
	  if ( c != '*'  &&  c != '>'  &&
	       c != '{'  &&  c != '}'  &&
	       c != '\0' )
	    {
	    status = zip_unrecognized_stream_object_attribute;
	    sprintf( msg, "Unrecognized Figure Attribute '%c' At Position %d\n",
		     c, position );
/*===	    while ( apt_Acknowledge( msg ) == -1 );===*/
	    }
	  end_of_attributes = true;
	  }
	break;
      }
    }
  DEBUGdt(Status,status);
  OUT(zip__Parse_Figure_Attributes);
  return status;
  }

static int
Parse_Stream_Figure( self )
  register struct zip		     *self;
  {
  register long			      status;
  register char			      c;

  IN(Parse_Stream_Figure);
  c = NextChar();
  if ( (status = zip_Create_Figure( self, &figure, NULL, NULL, image, figure )) == zip_ok )
    {
    if ( isascii( c )  &&  isupper( c ) )
      figure->zip_figure_type = c - '@'; /* 'A' == '1', etc */
      else 
      figure->zip_figure_type = c - '0';
    DEBUGdt(Figure-type, figure->zip_figure_type);
    status = zipobject_Read_Object( Objects(figure->zip_figure_type), figure );
    }
  DEBUGdt(Status,status);
  OUT(Parse_Stream_Figure);
  return status;
  }

static int
Parse_Stream_Image_Beginning( self )
  register struct zip		     *self;
  {
  register int			      status, page_count;
  char				     *ptr;

  IN(Parse_Stream_Image_Beginning);
  figure = NULL;
  if ( (status = zip_Create_Inferior_Image( self, &image, NULL, stream, image )) ==
	     zip_ok )
    {
    if ( (status = Extract_Attribute( self, &ptr )) == zip_ok )
      {
      status = Parse_Image_Attributes( self );
      if ( status == zip_ok  &&  ptr )
	if ( (status = zip_Set_Image_Name( self, image, ptr )) == zip_duplicate_image_name )
	  {
	  register int		  suffix = 0;
	  register char		 *new_name = NULL;

	  while( status == zip_duplicate_image_name )
	    {
	    status = zip_Set_Image_Name( self, image, new_name =
			    Unique_Name( ptr, suffix++ ) );
	    }
/*===*/printf("Changed Dup Image Name To '%s'\n", new_name);
	  status = zip_ok;
	  }
	  else
	  if ( image->zip_image_name &&
	       strncmp( image->zip_image_name, "ZIP_PAGE_IMAGE_", 15 ) == 0 )
	    {
	    page_count = atoi( image->zip_image_name + 15 );
	    DEBUGdt(Page,page_count);
	    if ( page_count > PageCount )
	      PageCount = page_count;
	    }
      }
    }
  OUT(Parse_Stream_Image_Beginning);
  return status;
  }


static int
Parse_Stream_Image_Ending( self )
  register struct zip		     *self;
  {
  register int			      status = zip_ok;
  char				     *attribute_ptr;

  IN(Parse_Stream_Image_Ending);
  Extract_Attribute( self, &attribute_ptr );
  if ( image->zip_image_figure_anchor )
    zip_Set_Stream_Extrema( self, stream, image );
  image = image->zip_image_superior;
  figure = NULL;
  OUT(Parse_Stream_Image_Ending);
  return status;
  }

static int
Parse_Image_Attributes( self )
  register struct zip		     *self;
  {
  register short			cap;
  register boolean		      end_of_attributes;
  register int			      status = zip_ok, offset;
  char				     *attribute_ptr, pattern[10];
  register char			      c, element = 1;
  register long			      i = 0;
  register double			red, green, blue;

  IN(Parse_Image_Attributes);
  end_of_attributes = false;
  while ( ! end_of_attributes  &&  status == zip_ok )
    {
    switch ( c = NextChar() )
      {
      case  'A':	/* Annotation */
	DEBUG(Attribute A);
	status = Extract_Attribute( self, &attribute_ptr
		 /*=== &figure->zip_image_annotation===*/ );
	break;
      case  'C':	/* Color */
	DEBUG(Attribute C);
	c = NextChar();
        red = Parse_Stream_Real(); NextChar();
        green = Parse_Stream_Real(); NextChar();
        blue = Parse_Stream_Real();
	switch ( c )
        {
	    case 'L': zip_Set_Image_Line_Color( self, image, red, green, blue ); break;
       	    case 'F': zip_Set_Image_FillFG_Color( self, image, red, green, blue ); break;
       	    case 'B': zip_Set_Image_FillBG_Color( self, image, red, green, blue ); break;
	}
        break;
      case  'P':	/* Pattern */
	DEBUG(Attribute P);
	image->zip_image_mode.zip_image_mode_patterned = on;
	image->zip_image_fill.zip_image_pattern = NextChar();
        break;
      case  'G':	/* Grey-level Shade */
	DEBUG(Attribute G);
	image->zip_image_mode.zip_image_mode_shaded = on;
        while ( (c = NextChar())  &&  c >= '0'  &&  c <= '9' )
	  image->zip_image_fill.zip_image_shade =
	     image->zip_image_fill.zip_image_shade * 10 + (c - '0');
	PriorChar( c );
        break;
      case  'F':	/* Font */
	DEBUG(Attribute F)
	if ( (status = Extract_Attribute( self, &attribute_ptr )) == zip_ok )
	  {
	  zip_Define_Font( self, attribute_ptr, &image->zip_image_font );
	  free( attribute_ptr );
	  }
        break;
      case  'T':	/* Text */
	DEBUG(Attribute T);
	status = Extract_Attribute( self, &image->zip_image_text );
        break;
      case  'N':	/* Name */
	DEBUG(Attribute N);
  	status = Extract_Attribute( self, &image->zip_image_name );
	if ( strncmp( image->zip_image_name, "ZIP_PAGE_IMAGE_", 15 ) == 0 )
	  {
	  i = atoi( image->zip_image_name + 15 );
	  DEBUGdt(Page,i);
	  if ( i > PageCount )
	    PageCount = i;
	  }
        break;
      case  'Z':	/* Zoom-level */
	DEBUG(Attribute Z);
	image->zip_image_zoom_level = Parse_Stream_Integer();
        break;
      case  'D':	/* Detail-level */
	DEBUG(Attribute D);
	image->zip_image_detail_level = Parse_Stream_Integer();
        break;
      case  'S':	/* Style */
	DEBUG(Attribute S);
	image->zip_image_style = Parse_Stream_Integer();
        break;
      case  'L':	/* Line ... */
	DEBUG(Attribute L);
	c = NextChar();
	switch ( c )
        {
	    case 'W': image->zip_image_line_width = Parse_Stream_Integer(); break;
	    case 'C': if (( c = NextChar()) == 'N' ) cap = graphic_CapNotLast;
		else if ( c == 'B' ) cap = graphic_CapButt;
		else if ( c == 'R' ) cap = graphic_CapRound;
		else if ( c == 'P' ) cap = graphic_CapProjecting;
		else { cap = graphic_CapButt; printf( "zip: unrecognized line cap attribute: '%c'\n", c ); }
		zip_Set_Image_Line_Cap( self, image, cap ); break;
	    case 'J': if (( c = NextChar()) == 'M' ) cap = graphic_JoinMiter;
		else if ( c == 'R' ) cap = graphic_JoinRound;
		else if ( c == 'B' ) cap = graphic_JoinBevel;
		else { cap = graphic_JoinMiter; printf( "zip: unrecognized line join attribute: '%c'\n", c ); }
		zip_Set_Image_Line_Join( self, image, cap ); break;
	    case 'D': if (( c = NextChar()) == 'S' ) cap = graphic_LineSolid;
	        else if ( c == 'D' ) cap = graphic_LineDoubleDash;
		else if ( c == 'O' ) cap = graphic_LineOnOffDash;
		NextChar(); offset = Parse_Stream_Integer(); NextChar(); i = 0;
		while( element != 0 )
		{
		    pattern[i++] = element = ( char ) Parse_Stream_Integer();
		    if ( element ) NextChar();
		}
		pattern[i] = 0;
		zip_Set_Image_Line_Dash( self, image, pattern, offset, cap );
		break;
	}
        break;
      default:
	DEBUG(Attribute Default);
	if ( ! Parse_Stream_Commentary( self, c ) )
	  {
	  c = PriorChar( c );
	  if ( c != '*'  &&  c != '>'  &&
	       c != '{'  &&  c != '}'  &&
	       c != '\0' )
	    {
	    status = zip_unrecognized_stream_object_attribute;
	    sprintf( msg, "Unrecognized Image Attribute '%c' At Position %d\n",
		     c, position );
/*===	    while ( apt_Acknowledge( msg ) == -1 );===*/
	    }
	  end_of_attributes = true;
	  }
	break;
      }
    }
  DEBUGdt(Status,status);
  OUT(Parse_Image_Attributes);
  return status;
  }

static int
Extract_Attribute( self, attribute_ptr )
  register struct zip		     *self;
  register char			    **attribute_ptr;
  {
  register int			      status = zip_ok;
  register char			     *counter, c;
  char				      buffer[zip_default_buffer_size + 1];

  IN(Extract_Attribute);
  *attribute_ptr = NULL;
  counter = buffer;
  if ( (c = NextChar()) != '\n' )
    {
  *counter++ = c;
  while ( c  &&  counter - buffer < zip_default_buffer_size )
    {
    if ( (c = NextChar()) == '\n' )
      break;
      else
      *counter++ = c;
    }
  *counter = 0;
  if ( counter == buffer + zip_default_buffer_size )
    status = zip_insufficient_figure_space;
  else if ( *attribute_ptr = (char *) malloc( strlen( buffer ) + 1 ) )
    {
    counter = buffer;
    while ( *counter )
      { /*+++ Two statements because of HC bug on "++" +++*/
      *( *attribute_ptr + ( counter - buffer) ) = *counter; counter++;
      }
    *( *attribute_ptr + ( counter - buffer ) ) = 0;
    DEBUGst( Extracted Attribute,*attribute_ptr);
    }
    else status = zip_insufficient_figure_space;
    }
  OUT(Extract_Attribute);
  return status;
  }

static char *
Unique_Name( name, seed )
  register char			     *name;
  register int			      seed;
  {
  static char			      alternate[512];

  sprintf( alternate, "%s[%d]", name, seed );
  return alternate;
  }

static int
Parse_Stream_Integer()
  {
  register long			      number = 0;
  register char			      c;

  while ( (c = NextChar())  &&  c >= '0'  &&  c <= '9' )
    number = number * 10 + (c - '0');
  PriorChar( c );
  return  number;
  }

static double
Parse_Stream_Real()
  {
  register double		number = 0.0;
  char				value[10];
  register char			c, *p = value;
  extern double			atof();

  while ( (c = NextChar())  && (( c >= '0'  &&  c <= '9' ) || c == '.' ))
        *p++ = c;
  *p = '\0';
  number = atof( value );
  PriorChar( c );
  return  number;
  }

static
Parse_Stream_Commentary( self, c )
  register struct zip		     *self;
  register char			      c;
  {
  register int			      status = false;

  IN(Parse_Stream_Commentary);
  if ( c == '#'  ||  c == ' '  ||  c == '\t'  ||  c == '\n' )
    {
    status = true;
    while ( c  &&  c != '\n' )
      c = NextChar();
    }
  OUT(Parse_Stream_Commentary);
  return status;
  }

static
Equivalent_Token( self, token, table )
  register struct zip		     *self;
  register char			     *token;
  register char			     *table[];
  {
  register int			      result = 0;

  IN(Equivalent_Token);
  DEBUGst( Token, token );
  while ( token  &&  *token  &&  table  &&  *table )
    {
    DEBUGst( Table-item, *table );
    if ( ! apt_MM_Compare( token, *table++ ) )
      {
      result = 1;
      break;
      }
    }
  DEBUGdt( Result, result );
  OUT(Equivalent_Token);
  return  result;
  }

static
Parse_Presentation_Parameter( self )
  register struct zip		     *self;
  {
  register long			      status = zip_ok;
  char				     *token, *ptr;
  static char			     *vw[] =
    { "ViewWidth", "vw", NULL };
  static char			     *vh[] =
    { "ViewHeight", "vh", NULL };
  static char			     *ow[] =
    { "ObjectWidth", "ow", NULL };
  static char			     *oh[] =
    { "ObjectHeight", "oh", NULL };

  IN(Parse_Presentation_Parameter);
  Extract_Attribute( self, &token );
  if ( ptr = token )
    {
    while ( *ptr  &&  *ptr != ' '  &&  *ptr != '\t'  &&  *ptr != '\n' )
      ptr++;
    if ( *ptr == ' '  ||  *ptr == '\t' )
      *ptr++ = 0;
    if ( Equivalent_Token( self, token, vw ) )
      {
      DesiredWidth = atoi( ptr );
      DEBUGdt(Computed ViewWidth,DesiredWidth);
      }
    else
    if ( Equivalent_Token( self, token, vh ) )
      {
      DesiredHeight = atoi( ptr );
      DEBUGdt(Computed ViewHeight,DesiredHeight);
      }
    else
    if ( Equivalent_Token( self, token, ow ) )
      {
      ObjectWidth = atoi( ptr );
      DEBUGdt(Computed ObjectWidth,ObjectWidth);
      }
    else
    if ( Equivalent_Token( self, token, oh ) )
      {
      ObjectHeight = atoi( ptr );
      DEBUGdt(Computed ObjectHeight,ObjectHeight);
      }
    free( token );
    }
  OUT(Parse_Presentation_Parameter);
  return  status;
  }

static char
NextChar()
  {
  register int				     c;

  if ( (c = getc( stream->zip_stream_file )) == EOF )
    {
    DEBUG(EOF);
    c = 0;
    }
  DEBUGct(C,c);
  position++;
  return c;
  }

static char
PriorChar( c )
  register char				     c;
  {
  c = ungetc( c, stream->zip_stream_file );
  DEBUGct(C,c);
  position--;
  return c;
  }

