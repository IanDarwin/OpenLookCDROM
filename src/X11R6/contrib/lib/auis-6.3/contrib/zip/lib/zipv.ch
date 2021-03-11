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
zipview.H

  11/09/87	Support "Absolute" as well as "Relative" sizing (TCP)
  03/31/88	Revise for ATK (TCP)
  06/14/89	Add Pane_Hit_Processor access macromethod (SCG)
   08/16/90	Add {Ensure,Normalize}_{Line,Fill}_Attributes methods
                            Add square, zipmin, zipmax macros (SCG)
*/

#define  zipview_VERSION		    2

#define  zipview_invert			    (1<<1)

#define  zipview_default_block_width	    512
#define  zipview_default_block_height	    512


#define  zipview_pane_top_edge		    (1 << 1)
#define  zipview_pane_bottom_edge	    (1 << 2)
#define  zipview_pane_left_edge		    (1 << 3)
#define  zipview_pane_right_edge	    (1 << 4)

#define  zipview_paint_inverted		    (1 << 1)
#define  zipview_paint_copy		    (1 << 2)

#define	 square(x)			    ((x)*(x))
#define	 zipmin(x,y)			    ((x) < (y) ? (x) : (y))
#define	 zipmax(x,y)			    ((x) < (y) ? (y) : (x))

struct zipview_options
  {
  unsigned int				    manual_refresh	: 1;
  };

struct zipview_states
  {
  unsigned int				    inputfocus		: 1;
  unsigned int				    editing		: 1;
  unsigned int				    grid_exposed	: 1;
  unsigned int				    coordinates_exposed	: 1;
  unsigned int				    fonts_exposed	: 1;
  unsigned int				    figures_exposed	: 1;
  unsigned int				    first_time	    	: 1;
  unsigned int				    abeyant	    	: 1;
  unsigned int				    application	    	: 1;
  };


class zipview[zipv] : view
  {
overrides:

  DesiredSize( long width, long height, enum view_DSpass pass, long *dWidth, long *dheight )
								    returns enum view_DSattributes;
  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  Update();
  WantUpdate( struct view *requestor );
  Hit( enum view_MouseAction action, long x, long y, long n)	    returns struct view *;
  ReceiveInputFocus();
  LoseInputFocus();
  SetDataObject( struct zip * );
  ObservedChanged ( struct zipview *changed, long value );
  Print( FILE *file, char *processor, char *finalFormat, boolean topLevel );
  GetApplicationLayer()						    returns struct view *;
  GetInterface( char *interface_name )				    returns struct scrollfns *;
  PostMenus( menulist );
  PostKeyState( keystate );

methods:

  SetOptions( int );
  Set_Debug( boolean state );

  Display_Figure( figure, pane )				    returns long;
  Draw_Figure( figure, pane )					    returns long;
  Clear_Figure( figure, pane )	    				    returns long;
  Hide_Figure( figure, pane )					    returns long;
  Expose_Figure( figure, pane )					    returns long;
  Which_Figure( x, y )						    returns struct zip_figure *;
  Which_Pane_Figure( x, y, pane )				    returns struct zip_figure *;
  Within_Which_Figure( x, y )					    returns struct zip_figure *;
  Figure_Visible( figure, pane )				    returns boolean;

  Display_Image( image, pane )					    returns long;
  Draw_Image( image, pane )					    returns long;
  Clear_Image( image, pane )					    returns long;
  Hide_Image( image, pane )					    returns long;
  Expose_Image( image, pane )					    returns long;
  Which_Image( x, y )						    returns struct zip_image *;
  Image_Visible( image, pane )				    returns boolean;

  Display_Stream( stream, pane )				    returns long;
  Draw_Stream( stream, pane )					    returns long;
  Clear_Stream( stream, pane )					    returns long;
  Which_Stream( x, y )						    returns struct zip_stream *;
  Stream_Visible( stream, pane )				    returns boolean;

  Pane( name )							    returns struct zip_pane *;
  Create_Pane( pane, name, master_block, attributes )		    returns long;
  Create_Nested_Pane( pane, name, master_pane, attributes )	    returns long;
  Destroy_Pane( pane )						    returns long;
  Redisplay_Panes()						    returns long;
  Redraw_Panes()						    returns long;
  Redisplay_Pane_Suite( pane )					    returns long;
  Redraw_Pane_Suite( pane )					    returns long;

  Set_Pane_Coordinates( pane, x_origin, y_origin, width, height )   returns long;
  Set_Pane_Border( pane, font_name, pattern, thickness )	    returns long;
  Set_Pane_Stream( pane, stream )				    returns long;
  Reset_Pane_Stream( pane, stream )				    returns long;
  Pane_Stream( pane )						    returns struct zip_stream *;

  Set_Pane_Auxiliary_Stream( pane, stream )			    returns long;
  Reset_Pane_Auxiliary_Stream( pane, stream )			    returns long;
  Reset_Pane_Auxiliary_Streams( pane )				    returns long;
  Set_Pane_Image( pane, image )					    returns long;
  Reset_Pane_Image( pane, image )				    returns long;
  Pane_Image( pane )						    returns struct zip_image *;
  Set_Pane_Figure( pane, figure )				    returns long;
  Reset_Pane_Figure( pane, figure )				    returns long;
  Pane_Figure( pane )						    returns struct zip_figure *;
  Set_Pane_Scale( pane, scale )					    returns long;
  Set_Pane_Cursor( pane, cursor_icon, cursor_font )		    returns long;
  Set_Pane_Painting_Mode( pane, mode )				    returns long;

  Display_Pane( pane )						    returns long;
  Draw_Pane( pane )						    returns long;
  Clear_Pane( pane )						    returns long;
  Zoom_Pane( pane, level )					    returns long;
  Zoom_Pane_To_Point( pane, x, y, level, mode )			    returns long;
  Scale_Pane( pane, scale )					    returns long;
  Scale_Pane_To_Point( pane, x, y, scale, mode )		    returns long;
  Pan_Pane( pane, x_offset, y_offset )				    returns long;
  Pan_Pane_To_Edge( pane, edge )				    returns long;
  Flip_Pane( pane )						    returns long;
  Flop_Pane( pane )						    returns long;
  Invert_Pane( pane )						    returns long;
  Balance_Pane( pane )						    returns long;
  Center_Pane( pane )						    returns long;
  Normalize_Pane( pane )					    returns long;
  Hide_Pane( pane )						    returns long;
  Expose_Pane( pane )						    returns long;
  Remove_Pane( pane )						    returns long;
  Which_Pane( x, y )						    returns struct zip_pane *;

  X_Pixel_To_Point( pane, figure, x )				    returns long;
  Y_Pixel_To_Point( pane, figure, y )				    returns long;
  X_Point_To_Pixel( pane, figure, x )				    returns long;
  Y_Point_To_Pixel( pane, figure, y )				    returns long;
  X_Point_Delta( pane, x_delta )    				    returns long;
  Y_Point_Delta( pane, y_delta )    				    returns long;

  Print_Figure( figure, pane )					    returns long;
  Print_Image( image, pane )					    returns long;
  Print_Stream( stream, pane )					    returns long;
  Print_Pane( pane )						    returns long;
  Set_Print_Resolution( resolution )				    returns long;
  Set_Print_Dimensions( width, height )				    returns long;
  Set_Print_Coordinates( x_origin, y_origin, width, height )	    returns long;
  Set_Print_Orientation( orientation )				    returns long;
  Set_Print_Language( language )				    returns long;
  Set_Print_Processor( processor )				    returns long;
  Set_Print_Level( level )					    returns long;
  Set_Print_File( file )					    returns long;

  Use_Normal_Pane_Cursors()					    returns long;
  Use_Working_Pane_Cursors()					    returns long;
  Use_Alternate_Pane_Cursors( cursor_icon, cursor_font )   	    returns long;

  Handle_Panning( pane, x, y, x_delta, y_delta )		    returns long;
  Initiate_Panning( pane, x, y, mode )				    returns long;
  Continue_Panning( pane, x, y )				    returns long;
  Terminate_Panning( pane, x, y, x_delta, y_delta, draw )	    returns long;

  Contextual_Figure_Font( figure )				    returns struct fontdesc *;
  Select_Contextual_Figure_Font( figure )			    returns struct fontdesc *;

  /****  Following Facilities For Sub-Class (Internal) Usage Only  ****/

  Define_Graphic( font, pattern )				    returns struct graphic *;
  Set_Clip_Area( pane, left, top, width, height );
  Set_Pane_Clip_Area( pane );
  Reset_Pane_Clip_Area( pane );
  Post_Pane_Cursor( pane, glyph );
  Condition( pane, figure, action )				    returns boolean;
  Proximate_Figure_Point( pane, figure, X, Y, x, y )		    returns boolean;
  Proximate_Enclosure( pane, left, top, right, bottom, x, y )	    returns boolean;
  Try_Pane_Exception_Handler( pane )				    returns long;
  Query( query, default_response, response )			    returns long;
  Query_File_Name( query, response )				    returns long;
  Announce( message )						    returns long;
  Ensure_Fill_Attributes( figure );
  Ensure_Line_Attributes( figure )				    returns long;
  Normalize_Fill_Attributes();
  Normalize_Line_Attributes();

macromethods:

  Figure_Exposed(figure)	((figure)->zip_figure_visibility == zip_figure_exposed)
  Figure_Hidden(figure)		((figure)->zip_figure_visibility == zip_figure_hidden)

  Image_Exposed( image )	((image)->zip_image_visibility == zip_image_exposed)
  Image_Hidden( image )		((image)->zip_image_visibility == zip_image_hidden)
 
  Pane_Name(pane)		((pane)->zip_pane_name)
  Pane_Exposed(pane)		((pane)->zip_pane_state.zip_pane_state_exposed)
  Pane_Hidden(pane)		((pane)->zip_pane_state.zip_pane_state_hidden)
  Pane_Removed(pane)		((pane)->zip_pane_state.zip_pane_state_removed)
  Pane_Inverted(pane)		((pane)->zip_pane_state.zip_pane_state_inverted)
  Pane_Coordinates_Exposed(pane)((pane)->zip_pane_state.zip_pane_state_coordinates_exposed)
  Pane_Overlaying(pane)		((pane)->zip_pane_attributes.zip_pane_attribute_overlay)
  Pane_Transparent(pane)	((pane)->zip_pane_attributes.zip_pane_attribute_transparent)

  Pane_X_Percent_Origin(pane)	((zip_type_percent)(pane)->zip_pane_x_origin_percent)
  Pane_Y_Percent_Origin(pane)	((zip_type_percent)(pane)->zip_pane_y_origin_percent)
  Pane_Percent_Width(pane)	((zip_type_percent)(pane)->zip_pane_width_percent)
  Pane_Percent_Height(pane)	((zip_type_percent)(pane)->zip_pane_height_percent)
  Pane_X_Origin(pane)		((zip_type_pixel)(pane)->zip_pane_x_origin)
  Pane_Y_Origin(pane)		((zip_type_pixel)(pane)->zip_pane_y_origin)
  Pane_Left(pane)		((zip_type_pixel)(pane)->zip_pane_left)
  Pane_Right(pane)		((zip_type_pixel)(pane)->zip_pane_left + (pane)->zip_pane_width)
  Pane_Top(pane)		((zip_type_pixel)(pane)->zip_pane_top)
  Pane_Bottom(pane)		((zip_type_pixel)(pane)->zip_pane_top + (pane)->zip_pane_height)
  Pane_Width(pane)		((zip_type_pixel)(pane)->zip_pane_width)
  Pane_Height(pane)		((zip_type_pixel)(pane)->zip_pane_height)
  Pane_Object_Width(pane)	((zip_type_pixel)(pane)->zip_pane_object_width)
  Pane_Object_Height(pane)	((zip_type_pixel)(pane)->zip_pane_object_height)
  Pane_Scale(pane)		((pane)->zip_pane_scale)

  Pane_Datum(pane)		((pane)->zip_pane_client_data)

  Set_Pane_Object_Width( pane, width ) \
    {(pane)->zip_pane_object_width = width;}
  Set_Pane_Object_Height( pane, height ) \
    {(pane)->zip_pane_object_height = height;}

  Set_Pane_Panning_Precision( pane, precision ) \
    {(pane)->zip_pane_panning_precision = precision;}
  Set_Pane_Zoom_Level( pane, level ) \
    {(pane)->zip_pane_zoom_level = level;}
  Pane_Zoom_Level( pane ) \
     (pane)->zip_pane_zoom_level
  Set_Pane_Detail_Level( pane, level ) \
    {(pane)->zip_pane_detail_level = level;}
  Pane_Detail_Level( pane ) \
     (pane)->zip_pane_detail_level

  Set_Pane_Hit_Processor( pane, processor, anchor ) \
    {(pane)->zip_pane_hit_processor = \
    (long (*)())processor;(pane)->zip_pane_hit_processor_anchor = (long)anchor;}
  Pane_Hit_Processor( pane ) \
    (long (*)())(pane)->zip_pane_hit_processor
  Reset_Pane_Hit_Processor( pane ) \
    {(pane)->zip_pane_hit_processor = NULL;}

  Set_Pane_Display_Preprocessor( pane, processor, anchor ) \
    {(pane)->zip_pane_display_preprocessor = \
    (long (*)())processor;(pane)->zip_pane_display_processor_anchor = (long)anchor;}
  Reset_Pane_Display_Preprocessor( pane ) \
    {(pane)->zip_pane_display_preprocessor = NULL;}

  Set_Pane_Display_Processor( pane, processor, anchor ) \
    {(pane)->zip_pane_display_processor = \
    (long (*)())processor;(pane)->zip_pane_display_processor_anchor = (long)anchor;}
  Reset_Pane_Display_Processor( pane ) \
    {(pane)->zip_pane_display_processor = NULL;}

  Set_Pane_Display_Postprocessor( pane, processor, anchor ) \
    {(pane)->zip_pane_display_postprocessor = \
    (long (*)())processor;(pane)->zip_pane_display_processor_anchor = (long)anchor;}
  Reset_Pane_Display_Postreprocessor( pane ) \
    {(pane)->zip_pane_display_postprocessor = NULL;}

  Set_Pane_Exception_Handler( handler ) \
    {(self)->pane_exception_handler = handler;(pane)->zip_pane_hit_anchor = NULL;}
  Reset_Pane_Exception_Handler() \
    {(self)->pane_exception_handler = NULL;}

classprocedures:

  InitializeClass() returns boolean;
  InitializeObject( struct zipview *self )			    returns boolean;
  FinalizeObject( struct zipview *self );

data:

  struct zip			*data_object;
  struct zipedit		*edit_object;
  struct zipprint		*print_object;
  struct zipobject	       *((*objects)[]);
  struct zip_pane_chain		*pane_anchor;
  struct zip_printing		*printing; 
  struct menulist		*menu;
  struct keymap			*keymap;
  struct keystate		*keystate;
  struct cursor			*cursor;
  struct zipview_options	 options;
  struct zipview_states		 states;
  zip_type_pane			 pane, current_pane;
  struct rectangle		 block;
  long				 action;
  enum view_MouseAction		 mouse_action;
  long				 mouse_x,
				 mouse_y,
				 mouse_clicks;
  long				 edge;
  long				 width;
  long				 height;
  void			       (*pane_exception_handler)();
  struct fontdesc		*points_font,
				*icon_font,
				*dot_font,
				*shade_font;
  long				 current_page;
  long				 panning_precision;
  };
