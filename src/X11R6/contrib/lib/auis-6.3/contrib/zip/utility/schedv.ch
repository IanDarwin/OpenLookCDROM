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


/**  SPECIFICATION -- External Facility Suite  *********************************

TITLE	The Sched View-object Specification

MODULE	schedv.c

NOTICE	IBM Internal Use Only

DESCRIPTION
	This defines the suite of Methods that support the LightTable View-object.

HISTORY
  01/20/89	Created (TCP)

END-SPECIFICATION  ************************************************************/

#define  schedv_VERSION		    1


class schedv : view
  {
overrides:

  FullUpdate( enum view_UpdateType type, long left, long top, long width, long height );
  Hit( enum view_MouseAction action, long x, long y, long n )
	returns struct view *;
  SetDataObject( struct sched * );
  ReceiveInputFocus();
  LoseInputFocus();

methods:

  Set_Debug( debug );

classprocedures:

  InitializeClass( struct classheader *classID )			    returns boolean;
  InitializeObject( struct classheader *classID, struct schedv *self )   returns boolean;
  FinalizeObject( struct classheader *classID, struct schedv *self );

data:

  struct sched			    *data;
  struct zipview		    *zipview;
  struct zip_pane		    *chart_pane;
  struct suite			    *control_buttons;
  struct rectangle		     block, chart_block;
  struct menulist		    *menu;
  struct cursor			    *cursor;
  char				     line_width, line_style, figure_type;
  boolean			     tracking, modified, input_focus,
				     pending_question, pending_duplicate;
  struct zip_stream		    *stream;
  struct zip_image		    *image;
  struct zip_figure		    *figure,
				    *current_slot_figure,  *current_text_figure,
				    *previous_slot_figure, *previous_text_figure;
  long				     point;
  long				     initial_x_pixel, initial_y_pixel;
  unsigned char			     wm_type;
  };
