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
zipprint.H

  03/31/88	Created for ATK (TCP)
   08/20/90	Add Ensure_Line_Attributes method (SCG)
*/

#define  zipprint_VERSION		    1

struct zipprint_options
  {
  unsigned int				    invert		: 1;
  };

struct zipprint_states
  {
  unsigned int				    xxxx		: 1;
  };


class zipprint
  {
overrides:

methods:

  Set_Debug( boolean state );
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

  Set_Data_Object( data_object );
  Set_View_Object( view_object );
  Set_Line_Width( line_width )					    returns long;
  Ensure_Line_Attributes( figure )				    returns long;
  Set_Shade( line_width )					    returns long;
  Move_To( x, y )						    returns long;
  Draw_To( x, y )						    returns long;
  Draw_Multi_Line( count, x, y, points )			    returns long;
  Draw_String( x, y, string, mode )				    returns long;
  Draw_Line( x1, y1, x2, y2 )					    returns long;
  Draw_Rectangle( x1, y1, x2, y2 )				    returns long;
  Draw_Round_Rectangle( x1, y1, x2, y2, xr, yr )		    returns long;
  Draw_Circle( x, y, radius )					    returns long;
  Draw_Ellipse( x, y, x_radius, y_radius )			    returns long;
  Draw_Arc( x, y, x_radius,y_radius, x_start,y_start, x_end,y_end ) returns long;
  Arc_To( x_center, y_center, x_radius,y_radius, x_start,y_start, x_end,y_end ) returns long;
  Fill_Trapezoid( x1, y1, x2, y2, l1, l2, pattern )		    returns long;

  Close_Path()							    returns long;
  Change_Font( font_index )					    returns long;
  Restore_Font()						    returns long;
  Try_Printing_Exception_Handler( printing )			    returns long;

macromethods:

classprocedures:

  InitializeObject( struct zipprint *self )			    returns boolean;
  FinalizeObject( struct zipprint *self );


data:

  struct zip			*data_object;
  struct zipview		*view_object;
  struct zipprint_options	 options;
  struct zipprint_states	 states;
  };
