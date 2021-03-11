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
zipedit.H

  03/31/88	Created for ATK (TCP)
*/

#define  zipedit_VERSION		    1


struct zipedit_options
  {
  unsigned int				    invert		: 1;
  };

struct zipedit_states
  {
  unsigned int				    grid_exposed	: 1;
  unsigned int				    coordinates_exposed	: 1;
  unsigned int				    palettes_exposed	: 1;
  unsigned int				    background_exposed	: 1;
  unsigned int				    background_selected	: 1;
  unsigned int				    background_lightened: 1;
  unsigned int				    enclosure_exposed	: 1;
  unsigned int				    duplicate_selection	: 1;
  unsigned int				    foreground_panning	: 1;
  unsigned int				    moving		: 1;
  };


class zipedit
  {
overrides:

methods:

  Set_Debug( boolean state );
  Update();
  Accept_Hit( pane, action, x, y, n )				    returns long;

  Set_Data_Object( data_object )				    returns long;
  Set_View_Object( view_object )				    returns long;

  Delete_Figure( figure, pane )					    returns long;
  Undelete_Figure( figure, pane )				    returns long;

  Which_Figure_Point( figure, pane, x, y )			    returns long;
  Highlight_Figure_Points( figure, pane )			    returns long;
  Normalize_Figure_Points( figure, pane )			    returns long;
  Hide_Figure_Points( figure, pane )				    returns long;
  Expose_Figure_Points( figure, pane )				    returns long;

  Delete_Image( image, pane )					    returns long;
  Undelete_Image( image, pane )					    returns long;
  Highlight_Image_Points( image, pane )				    returns long;
  Normalize_Image_Points( image, pane )				    returns long;
  Hide_Image_Points( image, pane )				    returns long;
  Expose_Image_Points( image, pane )				    returns long;

  Set_Pane_Highlight_Icon( pane, icon )   			    returns long;

  Hide_Pane_Points( pane )					    returns long;
  Expose_Pane_Points( pane )					    returns long;
  Hide_Pane_Coordinates( pane )					    returns long;
  Expose_Pane_Coordinates( pane )				    returns long;
  Draw_Pane_Coordinates( pane )					    returns long;
  Hide_Pane_Grid( pane )					    returns long;
  Expose_Pane_Grid( pane )					    returns long;
  Draw_Pane_Grid( pane )					    returns long;
  Halve_Pane_Grid( pane )					    returns long;
  Double_Pane_Grid( pane )					    returns long;
  Lighten_Pane( pane, density )					    returns long;
  Align_Pane( pane )						    returns long;

  Handle_Editing( pane )					    returns long;
  Expose_Palettes( pane )					    returns long;
  Hide_Palettes( pane )						    returns long;
  Set_Palettes( pane, palette_mode )				    returns long;
  Redisplay_Panes( pane )					    returns long;
  Initialize_Editing( pane )					    returns long;
  Terminate_Editing( pane )					    returns long;


  /****  Following Facilities For Sub-class Usage Only  ****/
  Expose_Point( pane, figure, x, y )				    returns long;
  Hide_Point( pane, figure, x, y )				    returns long;
  Highlight_Point( pane, x, y )					    returns long;
  Normalize_Point( pane, x, y )					    returns long;
  Highlight_Handles( pane, x1, x2, x3, y1, y2, y3 )		    returns long;
  Highlight_Handle( pane, x, y )				    returns long;
  Normalize_Handles( pane, x1, x2, x3, y1, y2, y3 )		    returns long;
  Normalize_Handle( pane, x, y )				    returns long;

macromethods:

  Set_Keyboard_Processor( anchor, processor ) \
    {self->keyboard_processor = processor;self->keyboard_anchor = (long)anchor;}

  Set_Pending_Processor( anchor, processor ) \
    {self->pending_processor = processor;self->pending_anchor = (long)anchor;}

classprocedures:

  InitializeClass() returns boolean;
  InitializeObject( struct zipedit *self )			    returns boolean;
  FinalizeObject( struct zipedit *Self );


data:

  struct zip			 *data_object;
  struct zipview		 *view_object;
  struct zipedit_options	  options;
  struct zipedit_states		  states;
  long				  action;
  zip_type_pane			  pane;
  struct fontdesc		 *icon_font;
  struct fontdesc		 *points_font;
  struct fontdesc		 *dots_font;
  enum view_MouseAction		(*keyboard_processor)();
  long				  keyboard_anchor;
  struct keystate		 *keystate;
  struct menulist		 *menu;
  long				  prior_x, prior_y;
  char				  font_family;
  long				  font_height;
  char				  font_bold, font_italic;
  long				  font_vertical, font_horizontal;
  zip_type_pixel		  enclosure_left, enclosure_top,
				  enclosure_width, enclosure_height;
  zip_type_figure		 *enclosed_figures;
  long				(*pending_processor)();
  long				  pending_anchor;
  zip_type_pixel		  enclosure_shadow_start_x, enclosure_shadow_start_y,
				  enclosure_shadow_last_x, enclosure_shadow_last_y;
  };
