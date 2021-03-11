/* ********************************************************************** *\
 *         Copyright IBM Corporation 1988,1991 - All Rights Reserved      *
 *        For full copyright information see:'andrew/config/COPYRITE'     *
\* ********************************************************************** */

/*
	$Disclaimer: 
*Permission to use, copy, modify, and distribute this software and its 
*documentation for any purpose is hereby granted without fee, 
*provided that the above copyright notice appear in all copies and that 
*both that copyright notice, this permission notice, and the following 
*disclaimer appear in supporting documentation, and that the names of 
*IBM, Carnegie Mellon University, and other copyright holders, not be 
*used in advertising or publicity pertaining to distribution of the software 
*without specific, written prior permission.
*
*IBM, CARNEGIE MELLON UNIVERSITY, AND THE OTHER COPYRIGHT HOLDERS 
*DISCLAIM ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING 
*ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS.  IN NO EVENT 
*SHALL IBM, CARNEGIE MELLON UNIVERSITY, OR ANY OTHER COPYRIGHT HOLDER 
*BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY 
*DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, 
*WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS 
*ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
*OF THIS SOFTWARE.
* $
*/


 

/*
 * P_R_P_Q_# (C) COPYRIGHT IBM CORPORATION 1988
 * LICENSED MATERIALS - PROPERTY OF IBM
 * REFER TO COPYRIGHT INSTRUCTIONS FORM NUMBER G120-2083
 */
/*
zip.ch

  11/09/87	Support "Absolute" as well as "Relative" sizing (TCP)
  03/31/88	Revise for ATK (TCP)
  11/17/88	Add Line_Width methods (TCP/SCG)
  05/10/89	Have Contextual_Figure_Line_Width return "unsigned char" (SCG)
  08/07/89	Override GetModified to check for changes to Imbedded objects (TCP)
   08/14/90	Added numerous color and line style method declarations (SCG)
*/

#define  zip_VERSION    2

#include "zip.h"

class zip : dataobject[dataobj]
  {

overrides:

  Read( FILE *file, long id )					    returns long;
  Write( FILE *file, long id, long level )			    returns long;
  GetModified()							    returns long;

methods:

  Set_Debug( boolean state );

  Create_Figure( figure, name, type, image, peer )		    returns long;
  Destroy_Figure( figure )					    returns long;
  Hook_Figure( figure, peer_figure )				    returns long;
  Unhook_Figure( figure )					    returns long;
  Set_Figure_Name( figure, name )				    returns long;
  Set_Figure_Text( figure, text )				    returns long;
  Set_Figure_Pattern( figure, pattern )				    returns long;
  Set_Figure_Shade( figure, shade )				    returns long;
  Set_Figure_Line_Width( figure, width )			    returns long;
  Set_Figure_Line_Dash( figure, char *pattern, int offset, short type ) returns long;
  Set_Figure_Line_Cap( figure, short cap )			    returns long;
  Set_Figure_Line_Join( figure, short join )			    returns long;
  Set_Figure_Line_Color( figure, double red, double green, double blue )    returns long;
  Set_Figure_FillFG_Color( figure, double red, double green, double blue )    returns long;
  Set_Figure_FillBG_Color( figure, double red, double green, double blue )    returns long;
  Set_Figure_Font( figure, font_name )				    returns long;
  Set_Figure_Mode( figure, mode )				    returns long;
  Set_Figure_Point( figure, point, x, y )			    returns long;
  Adjust_Figure_Point_Suite( figure, x_delta, y_delta )		    returns long;
  Change_Figure_Point( figure, old_x, old_y, new_x, new_y )	    returns long;
  Remove_Figure_Point( figure, old_x, old_y )			    returns long;
  Add_Figure_Point( figure, new_x, new_y )			    returns long;
  Figure( char *name )						    returns struct zip_figure *;
  Image_Figure( struct zip_image *image, char *name )		    returns struct zip_figure *;
  Stream_Figure( struct zip_stream *stream, char *name )	    returns struct zip_figure *;

  Create_Peer_Image( image, name, stream, peer )		    returns long;
  Create_Inferior_Image( image, name, stream, superior )	    returns long;
  Destroy_Image( image )					    returns long;
  Hook_Peer_Image( image, peer_image )				    returns long;
  Hook_Inferior_Image( image, superior_image )			    returns long;
  Unhook_Image( image )						    returns long;
  Image_Left_Peer( image )					    returns struct zip_image *;
  Set_Image_Name( image, name )					    returns long;
  Set_Image_Text( image, text )					    returns long;
  Set_Image_Pattern( image, pattern )				    returns long;
  Set_Image_Shade( image, shade )				    returns long;
  Set_Image_Line_Width( image, width )				    returns long;
  Set_Image_Line_Dash( image, char *pattern, int offset, short type ) returns long;
  Set_Image_Line_Cap( image, short cap )			    returns long;
  Set_Image_Line_Join( image, short join )			    returns long;
  Set_Image_Line_Color( image, double red, double green, double blue )    returns long;
  Set_Image_FillFG_Color( image, double red, double green, double blue )    returns long;
  Set_Image_FillBG_Color( image, double red, double green, double blue )    returns long;
  Set_Image_Font( image, font_name )				    returns long;
  Superior_Image_Pattern( image )				    returns char;
  Superior_Image_Shade( image )					    returns char;
  Superior_Image_Line_Width( image )				    returns char;
  Superior_Image_Line_Dash( image, char **pattern, int *offset, short *type ) returns long;
  Superior_Image_Line_Cap( image )			    returns short;
  Superior_Image_Line_Join( image )			    returns short;
  Superior_Image_Line_Color( image )			    returns struct zip_color *;
  Superior_Image_FillFG_Color( image )			    returns struct zip_color *;
  Superior_Image_FillBG_Color( image )			    returns struct zip_color *;
  Superior_Image_Text( image )					    returns char *;
  Superior_Image_Font( image )					    returns struct fontdesc *;
  Adjust_Image_Point_Suite( image, x_delta, y_delta )		    returns long;
  Image( char *name )						    returns struct zip_image *;
  Stream_Image( stream, image_name )				    returns struct zip_image *;
  Next_Image( image )						    returns struct zip_image *;

  Open_Stream( struct zip_stream **stream, char *name, long mode )  returns long;
  Close_Stream( struct zip_stream *stream )			    returns long;
  Read_Stream( struct zip_stream *stream )			    returns long;
  Write_Stream( struct zip_stream *stream )			    returns long;
  Create_Stream( struct zip_stream **stream, name, mode )	    returns long;
  Set_Stream_Name( stream, name )				    returns long;
  Set_Stream_Pattern( stream, pattern )				    returns long;
  Set_Stream_Line_Width( stream, width )			    returns long;
  Set_Stream_Line_Dash( stream, char *pattern, int offset, short type ) returns long;
  Set_Stream_Line_Cap( stream, short cap )			    returns long;
  Set_Stream_Line_Join( stream, short join )			    returns long;
  Set_Stream_Line_Color( stream, double red, double green, double blue )    returns long;
  Set_Stream_FillFG_Color( stream, double red, double green, double blue )    returns long;
  Set_Stream_FillBG_Color( stream, double red, double green, double blue )    returns long;
  Set_Stream_Text( stream, text )				    returns long;
  Set_Stream_Font( stream, font_name )				    returns long;
  Set_Stream_Source( struct zip_stream *stream, char *name )	    returns long;
  Stream( char *name )						    returns struct zip_stream *;

  Contextual_Figure_Pattern( figure )				    returns char;
  Contextual_Figure_Shade( figure )				    returns char;
  Contextual_Figure_Line_Width( figure )			    returns unsigned char;
  Contextual_Figure_Line_Dash( figure, char **pattern, int *offset, short *type );
  Contextual_Figure_Line_Join( figure ) returns short;
  Contextual_Figure_Line_Cap( figure ) returns short;
  Contextual_Figure_Line_Color( figure, double *red, double *green, double *blue ) returns long;
  Contextual_Figure_FillFG_Color( figure, double *red, double *green, double *blue ) returns long;
  Contextual_Figure_FillBG_Color( figure, double *red, double *green, double *blue ) returns long;


  /****  Following Facilities For Sub-Class (Internal) Usage Only  ****/

  Allocate_Color_Values()					    returns struct zip_color_values *;
  Allocate_Color()						    returns struct zip_color *;
  Define_Font( font_name, *font_index )				    returns long;
  Read_Figure( figure )						    returns long;
  Write_Figure( figure )					    returns long;
  Parse_Figure_Point( figure, *x, *y )				    returns long;
  Parse_Figure_Points( figure )					    returns long;
  Parse_Figure_Attributes( figure )				    returns long;
  Allocate_Figure_Points_Vector( figure )			    returns long;
  Enlarge_Figure_Points_Vector( figure )			    returns long;
  Set_Image_Extrema( image, x, y )				    returns long;
  Set_Stream_Extrema( stream, image )				    returns long;
  Try_general_Exception_Handler()				    returns long;
  Try_Figure_Exception_Handler( figure )			    returns long;
  Try_Image_Exception_Handler( image )				    returns long;
  Try_Stream_Exception_Handler( stream )			    returns long;

macromethods:

  Figure_Name( figure )		    ((figure) ? (figure)->zip_figure_name : NULL )
  Figure_Type( figure )		    ((figure) ? (figure)->zip_figure_type : NULL )
  Figure_Text( figure )		    ((figure) ? (figure)->zip_figure_datum.zip_figure_text : NULL )
  Figure_Anchor( figure )	    ((figure) ? (figure)->zip_figure_datum.zip_figure_anchor : NULL )
  Figure_Pattern( figure )	    ((figure) ? (figure)->zip_figure_fill.zip_figure_pattern : NULL )
  Figure_Shade( figure )	    ((figure) ? (figure)->zip_figure_fill.zip_figure_shade : NULL )
  Figure_Image( figure )	    ((figure) ? (figure)->zip_figure_image : NULL )
  Figure_Zoom_Level( figure )	    ((figure) ? (figure)->zip_figure_zoom_level : NULL )
  Figure_Detail_Level( figure )	    ((figure) ? (figure)->zip_figure_detail_level : NULL )
  Next_Figure( figure )		    ((figure) ? (figure)->zip_figure_next : NULL )
  Figure_Root( image )		    ((image) ? (image)->zip_image_figure_anchor : NULL )

  Image_Root( stream )		    ((stream) ? stream->zip_stream_image_anchor : NULL )
  Image_Name( image )		    ((image) ? (image)->zip_image_name : NULL )
  Image_Text( image )		    ((image) ? (image)->zip_image_text : NULL )
  Image_Type( image )		    ((image) ? (image)->zip_image_type : NULL )
  Image_Pattern( image )	    ((image) ? (image)->zip_figure_fill.zip_image_pattern : NULL )
  Image_Shade( image )		    ((image) ? (image)->zip_figure_fill.zip_image_shade : NULL )
  Image_Datum( image )		    ((image) ? (image)->zip_image_client_data : NULL )
  Image_Zoom_Level( image )	    ((image) ? (image)->zip_image_zoom_level : NULL )
  Image_Detail_Level( image )	    ((image) ? (image)->zip_image_detail_level : NULL )
  Image_Superior( image )	    ((image) ? (image)->zip_image_superior : NULL )
  Image_Inferior( image )	    ((image) ? (image)->zip_image_inferior : NULL )
  Image_Right_Peer( image )	    ((image) ? (image)->zip_image_right_peer : NULL )
  Image_Least_X( image )	    ((image) ? (image)->zip_image_least_x : NULL )
  Image_Greatest_X( image )	    ((image) ? (image)->zip_image_greatest_x : NULL )
  Image_Least_Y( image )	    ((image) ? (image)->zip_image_least_y : NULL )
  Image_Greatest_Y( image )	    ((image) ? (image)->zip_image_greatest_y : NULL )

  Stream_Name( stream )		    ((stream) ? (stream)->zip_stream_name : NULL )

  Containing_Figure_Stream( figure ) \
    ((figure) ? (figure->zip_figure_image->zip_image_stream) : NULL)
  Containing_Figure_Image( figure ) \
    ((figure) ? (figure->zip_figure_image) : NULL)

  Containing_Image_Stream( image ) \
    ((image) ? (image->zip_image_stream) : NULL)
  Containing_Image_Image( image ) \
    ((image) ? (image->zip_image_superior) : NULL)

  Set_general_Exception_Handler( handler ) \
    {self->general_exception_handler = handler;}


classprocedures:

  InitializeClass() returns boolean;
  InitializeObject( struct zip *self )				    returns boolean;
  FinalizeObject( struct zip *self );

data:

  zip_type_stream		 stream;
  char				*stream_file_name;
  FILE				*write_stream_file;
  long				 write_stream_id;
  long				 write_stream_level;
  long				 id;
  long				 desired_view_width;
  long				 desired_view_height;
  char				 desired_view_metric;
  long				 object_width;
  long				 object_height;
  char				 object_metric;
  struct zipobject	       *((*objects)[]);
  long				 page_count;
  struct zip_stream_chain	*stream_anchor;
  struct zip_image		*image_anchor;
  struct zip_figure		*figure_anchor;
  struct zip_paths		*paths;
  struct zip_fonts		*fonts;
  long				 status;
  long				 status_addenda;
  char				*facility;
  long				(*general_exception_handler)();
  long				(*stream_exception_handler)();
  long				(*image_exception_handler)();
  long				(*figure_exception_handler)();
  long				(*message_acknowledger)();
  long				(*message_writer)();
  long				(*message_clearer)();
  };
