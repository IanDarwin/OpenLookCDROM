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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipocapt.c,v 1.3 1992/12/15 21:57:55 rr2b R6tape $";
#endif

/* zipocapt.c	Zip Object -- Captions					      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Captions

MODULE	zipocapt.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  04/13/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)
  06/08/89	Use Drawable in fontdesc_StringSize (TCP)
  07/24/89	Drop leading/trailing newline(s) upon build completion (TCP)
   08/14/90	Use Ensure_Line_Attributes on Draw (SCG)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "im.ih"
#include "fontdesc.ih"
#include "zipobj.ih"
#include "zipocapt.eh"

static Draw();
static Compute_Handle_Positions();
static enum view_MouseAction Accept_Caption_Character();

char
zipocapt__Object_Icon( self )
  register struct zipocapt		 *self;
  {
  IN(zipocapt_Object_Icon);
  OUT(zipocapt_Object_Icon);
  return  'A';
  }

char
zipocapt__Object_Icon_Cursor( self )
  register struct zipocapt		 *self;
  {
  IN(zipocapt_Object_Icon_Cursor);
  OUT(zipocapt_Object_Icon_Cursor);
  return  'D';
  }

char
zipocapt__Object_Datastream_Code( self )
  register struct zipocapt		 *self;
  {
  IN(zipocapt_Object_Datastream_Code);
  OUT(zipocapt_Object_Datastream_Code);
  return  'A';
  }

long
zipocapt__Show_Object_Properties( self, pane, figure )
  register struct zipocapt		 *self;
  register zip_type_pane		  pane;
  register zip_type_figure		  figure;
  {
  zipview_Announce( View, "Draw Caption by Selecting Start-point, then Typing." );
  return  zip_ok;
  }

long
zipocapt__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipocapt		 *self;
  register zip_type_pane		  pane;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  register zip_type_point		  X, Y;
  {
  zip_type_figure			  figure;
  register long				  status = zip_ok;
  int					  position = 0; /*===*/
  char					  text[4];

  IN(zipocapt_Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      if ( (status =
	zip_Create_Figure( Data, &figure, NULL, zip_caption_figure,
			   pane->zip_pane_current_image, position )) == zip_ok )
        {
	pane->zip_pane_current_figure = figure;
        zipocapt_Set_Object_Point( self, figure, 1, X, Y );
	text[0] = '|'; text[1] = 0;
        figure->zip_figure_font = pane->zip_pane_current_font;
        figure->zip_figure_mode = pane->zip_pane_current_mode;
        figure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
	ZIP_Select_Figure_Font( View, figure );
        zip_Set_Figure_Text( Data, figure, text );
        zipview_Draw_Figure( View, figure, pane );
        zipedit_Set_Keyboard_Processor( Edit, self, Accept_Caption_Character );
        zipview_Set_Pane_Cursor( View, pane, 'D', CursorFontName );
	}
      break;
    case view_LeftUp:
    case view_LeftMovement:
      break;
    }
  OUT(zipocapt_Build_Object);
  return  status;
  }

static enum view_MouseAction
Accept_Caption_Character( self, pane, c, action, x, y, clicks )
  register struct zipocapt		 *self;
  register zip_type_pane		  pane;
  register char				  c;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  {
  register zip_type_figure		  figure = pane->zip_pane_current_figure;
  char					  text[4097];/*===*/
  register char				 *text_cursor;

  IN(Accept_Caption_Character)
  zipedit_Normalize_Figure_Points( Edit, figure, pane );
  if ( action == view_LeftUp  ||  action == view_LeftMovement )
    action = 0;
    else
    {
    strcpy( text, figure->zip_figure_datum.zip_figure_text );/*===*/
    text_cursor = strlen( text ) + &text[0];
    zipview_Clear_Figure( View, figure, pane );
    switch ( c )
      {
      case 0:        DEBUG(Finished Typing);
	text_cursor--;
        if ( (text_cursor - text) >= 0  &&  *text_cursor == '|' )
	  *text_cursor-- = 0;
	while ( (text_cursor - text) > 0 )
	  if ( *(text_cursor-1) == '\\'  &&  *text_cursor == 'n' )
	    {*(text_cursor-1) = 0; text_cursor -= 2;}
	    else  break;
        if ( *text == 0 )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
	  figure = NULL;
	  }
        zipedit_Set_Keyboard_Processor( Edit, NULL, NULL );
        break;
      case '\012':
      case '\015':   DEBUG(NewLine);
	if ( text_cursor > text+1 )
	  {
          *(text_cursor-1) = '\\';
          *text_cursor = 'n';
          *(text_cursor+1) = '|';
          *(text_cursor+2) = 0;
	  }
        break;
      case '\010':
      case '\177':   DEBUG(BackSpace or Delete);
        if ( text_cursor > text+1 )
          { DEBUG(Not at front);
	  if ( (text_cursor - text) > 2  &&
	       *(text_cursor-3) == '\\'  &&  *(text_cursor-2) == 'n' )
	    text_cursor--;
          *(text_cursor-2) = '|';
          *(text_cursor-1) = 0;
          }
        break;
      default:      DEBUG(Regular);
        *(text_cursor-1) = c;
        *text_cursor = '|';
        *(text_cursor+1) = 0;
      }
    if ( *text )
      {
      zip_Set_Figure_Text( Data, figure, text );
      zipview_Draw_Figure( View, figure, pane );
      zipedit_Highlight_Figure_Points( Edit, figure, pane );
      }
    }
   OUT(Accept_Caption_Character);
  return  action;
  }

long
zipocapt__Draw_Object( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocapt_Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane );
  OUT(zipocapt_Draw_Object);
  return  status;
  }

long
zipocapt__Clear_Object( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocapt_Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane );
  OUT(zipocapt_Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register struct fontdesc		 *font, *current_font =
					    zipview_GetFont( View );
  char					 *text = NULL;
  register long				  mode = NULL, x, y, y_increment, 
					  left, left_offset = 0, top_offset, width;
  long					  xp, yp;
  register long			  transfer_mode;
  char					  buffer[4097];
  register char				 *cursor, *buffer_ptr;

  IN(Draw);
  font = zipview_Select_Contextual_Figure_Font( View, figure );
  if ( figure->zip_figure_datum.zip_figure_text )
    text = figure->zip_figure_datum.zip_figure_text;
  else
  if ( figure->zip_figure_image->zip_image_text )
    text = figure->zip_figure_image->zip_image_text;
  else
  if ( text = zip_Superior_Image_Text( Data, figure->zip_figure_image->zip_image_superior ) )
    {}
  else
  if ( figure->zip_figure_image->zip_image_stream->zip_stream_text )
    text = figure->zip_figure_image->zip_image_stream->zip_stream_text;

  x = window_x_point;  y = window_y_point;
  y_increment = fontdesc_GetFontSize( font ) + 5/*===fudge.s/b FontNewLine===*/;
  left = 2;  top_offset = -y_increment/2;
  if ( figure->zip_figure_mode.zip_figure_mode_top )
    { mode |= graphic_ATTOP; top_offset = 0; }
  else
  if ( figure->zip_figure_mode.zip_figure_mode_middle )
    { mode |= graphic_BETWEENTOPANDBOTTOM; top_offset = -y_increment/2; }
  else
  if ( figure->zip_figure_mode.zip_figure_mode_baseline )
    { mode |= graphic_BETWEENTOPANDBASELINE; top_offset = -y_increment/2; }
  else
  if ( figure->zip_figure_mode.zip_figure_mode_bottom )
    { mode |= graphic_ATBOTTOM; top_offset = -y_increment; }

  if ( figure->zip_figure_mode.zip_figure_mode_left )
    { mode |= graphic_ATLEFT; left = 1; }
  if ( figure->zip_figure_mode.zip_figure_mode_center )
    { mode |= graphic_BETWEENLEFTANDRIGHT; left = 2; }
  if ( figure->zip_figure_mode.zip_figure_mode_right )
    { mode |= graphic_ATRIGHT; left = 3; }

  if ( text )
    {
    cursor = text;
    zipview_Ensure_Line_Attributes( View, figure );
    while ( *cursor )
      {
      buffer_ptr = buffer;
      while ( *cursor  &&  !(*cursor == '\\'  &&  *(cursor+1) == 'n') )
        *buffer_ptr++ = *cursor++;
      *buffer_ptr = 0;
      if ( figure->zip_figure_mode.zip_figure_mode_halo  &&
	    (transfer_mode = zipview_GetTransferMode( View )) == graphic_BLACK )
	{
        width = fontdesc_StringSize( font, im_GetDrawable(View), buffer, &xp, &yp );
	DEBUGdt(Width,width);
	switch ( left )
	  {
	  case 1: left_offset = 0;	    break;
	  case 2: left_offset = -width/2;   break;
	  case 3: left_offset = -width;	    break;
	  }
        zipview_SetTransferMode( View, graphic_WHITE );
        zipview_EraseRectSize( View, x + left_offset, y + top_offset,
			       width + 2, y_increment );
        zipview_SetTransferMode( View, transfer_mode );
	}
      zipview_MoveTo( View, x, y );
      DEBUGdt(X,x);  DEBUGdt(Y,y);
      zipview_DrawString( View, buffer, mode );
      DEBUGxt(Mode,mode);
      if ( *cursor == '\\' )
	{
	cursor++;
	if ( *cursor == 'n' )
	  {
	  cursor++;
	  y += y_increment;
	  }
	}
      }
    }
  if ( ExposePoints )
    zipocapt_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )
    zipocapt_Highlight_Object_Points( self, figure, pane );
  zipview_SetFont( View, current_font );
  OUT(Draw);
  return  status;
  }

long
zipocapt__Print_Object( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  char					 *text = NULL;
  register struct fontdesc		 *font = NULL;
  register int				  y, y_increment;
  char					  buffer[4097];
  register char				 *cursor, *buffer_ptr;
  register long				  status = zip_ok;
  register long				  mode = NULL;

  IN(zipocapt_Print_Object);
  font = zipview_Contextual_Figure_Font( View, figure );
  if ( figure->zip_figure_datum.zip_figure_text )
    text = figure->zip_figure_datum.zip_figure_text;
  else
  if ( figure->zip_figure_image->zip_image_text )
    text = figure->zip_figure_image->zip_image_text;
  else
  if ( text = zip_Superior_Image_Text( Data, figure->zip_figure_image->zip_image_superior ) )
    {}
  else
  if ( figure->zip_figure_image->zip_image_stream->zip_stream_text )
    text = figure->zip_figure_image->zip_image_stream->zip_stream_text;

  if ( text )
    {
    mode =  ((figure->zip_figure_mode.zip_figure_mode_top)      ? zip_top      : 0) |
	    ((figure->zip_figure_mode.zip_figure_mode_middle)   ? zip_middle   : 0) |
	    ((figure->zip_figure_mode.zip_figure_mode_baseline) ? zip_baseline : 0) |
	    ((figure->zip_figure_mode.zip_figure_mode_bottom)   ? zip_bottom   : 0) |
	    ((figure->zip_figure_mode.zip_figure_mode_left)     ? zip_left     : 0) |
	    ((figure->zip_figure_mode.zip_figure_mode_center)   ? zip_center   : 0) |
	    ((figure->zip_figure_mode.zip_figure_mode_right)    ? zip_right    : 0);
    zipprint_Change_Font( Print, font );
    cursor = text;
    y = print_y_point;
    y_increment = 100 * fontdesc_GetFontSize( font );
    DEBUGdt(Y-Increment,y_increment);
    while ( *cursor )
      {
      buffer_ptr = buffer;
      while ( *cursor  &&  !(*cursor == '\\'  &&  *(cursor+1) == 'n') )
        *buffer_ptr++ = *cursor++;
      *buffer_ptr = 0;
      zipprint_Draw_String( Print, print_x_point, y, buffer, mode );
      if ( *cursor == '\\' )
	{
	cursor++;
	if ( *cursor == 'n' )
	  {
	  cursor++;
	  y += y_increment;
	  }
	}
      }
    zipprint_Restore_Font( Print ); 
    }
  OUT(zipocapt_Print_Object);
  return  status;
  }

long
zipocapt__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipocapt_Proximate_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y1, x, y ) )
    point = 9;	    /* Upper Left Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y3, x, y ) )
    point = 5;	    /* Lower Right Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y1, x, y ) )
    point = 3; /* Upper Right Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y3, x, y ) )
    point = 7; /* Lower Left Corner */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y1, x, y ) )
    point = 2; /* 12 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X3, Y2, x, y ) )
    point = 4; /* 3 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y3, x, y ) )
    point = 6; /* 6 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X1, Y2, x, y ) )
    point = 8; /* 9 O'Clock */
  else
  if ( zipview_Proximate_Figure_Point( View, pane, figure, X2, Y2, x, y ) )
    point = 1; /* Center */
  else
  if ( zipview_Proximate_Enclosure( View, pane, X1, Y1, X3, Y3, x, y ) )
    point = 1; /* Center */
  OUT(zipocapt_Proximate_Object_Points);
  return  point;
  }

boolean
zipocapt__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = false;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipocapt_Enclosed_Object);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( X1 > x  &&  Y1 > y  &&  X2 < (x + w)  &&  Y2 < (y + h) )
    enclosed = true;
  OUT(zipocapt_Enclosed_Object);
  return  enclosed;
  }

long
zipocapt__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipocapt_Object_Enclosure);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  *x = X1;  *y = Y1;  *w = abs(X3 - X1);  *h = abs(Y3 - Y1);
  OUT(zipocapt_Object_Enclosure);
  return  zip_ok;
  }

long
zipocapt__Highlight_Object_Points( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipocapt_Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Highlight_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipocapt_Highlight_Object_Points);
  return  status;
  }

long
zipocapt__Normalize_Object_Points( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;
  register long				  status = zip_ok;

  IN(zipocapt_Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Normalize_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipocapt_Normalize_Object_Points);
  return  status;
  }

long
zipocapt__Expose_Object_Points( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocapt_Expose_Object_Points);
  zipedit_Expose_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  OUT(zipocapt_Expose_Object_Points);
  return  status;
  }

long
zipocapt__Hide_Object_Points( self, figure, pane )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipocapt_Hide_Points);
  zipedit_Hide_Point( Edit, pane, figure, figure_x_point, figure_y_point );
  OUT(zipocapt_Hide_Points);
  return  status;
  }

long
zipocapt__Set_Object_Point( self, figure, point, x, y )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register long				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;

  IN(zipocapt_Set_Object_Point);
  switch ( point )
      {
      case 1: /* Center */
        figure_x_point = x; figure_y_point = y;
        break;
      case 2: /* 12 O'Clock */
        figure_x_point = x; figure_y_point = y;
	break;
      case 3: /* Upper Right Corner */
        figure_x_point = x; figure_y_point = y;
        break;
      case 4: /* 3 O'Clock */
        figure_x_point = x; figure_y_point = y;
	break;
      case 5: /* Lower Right Corner */
        figure_x_point = x; figure_y_point = y;
	break;
      case 6: /* 6 O'Clock */
        figure_x_point = x; figure_y_point = y;
	break;
      case 7: /* Lower Left Corner */
        figure_x_point = x; figure_y_point = y;
	break;
      case 8: /* 9 O'Clock */
        figure_x_point = x; figure_y_point = y;
	break;
      case 9: /* Upper Left Corner */
        figure_x_point = x; figure_y_point = y;
	break;
      default: status = zip_failure; /*=== zip_invalid_point_type ===*/
      }
    if ( status == zip_ok )
      {
      zip_Set_Image_Extrema( Data, figure->zip_figure_image, x, y );
      zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			      figure->zip_figure_image );
      }
  OUT(zipocapt_Set_Object_Point);
  return  status;
  }

long
zipocapt__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipocapt_Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipocapt_Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3,
					 *Y1, *Y2, *Y3;
  {
  long					  width = 0, height = 0,
					  font_height, w, h;
  register struct fontdesc		 *font;
  register zip_type_pixel		  x, y;
  register long				  line_count = 0;
  char					  string[4096];
  register char				 *text = figure->zip_figure_datum.zip_figure_text,
					 *line = NULL, *line_max = string + sizeof string;

  x = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point );
  y = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point );
  font_height = fontdesc_GetFontSize( font =
	 zipview_Select_Contextual_Figure_Font( View, figure ) ) + 5
					 /*===fudge. CREATE FontNewLine===*/;

  while ( text  &&  *text  &&  line != line_max )
    {
    line = string;
    while ( *text  &&  line != line_max )
      {
      if ( *text == '\\' )
	{
	if ( *(text+1) == '\\' )  text++;
	if ( *(text+1) == 'n' )
    	  {text++; break;}
	  else *line++ = *text++;
	}
	else *line++ = *text++;
      }
    *line = 0;
    fontdesc_StringSize( font, im_GetDrawable(View), string, &w, &h );
    if ( w > width )  width = w;
    line_count++;
    }
  height = line_count * font_height;
  /*===*/  width += 5; /* Fudge */

  if (  figure->zip_figure_mode.zip_figure_mode_left )
    *X1 = x;
    else
    if (  figure->zip_figure_mode.zip_figure_mode_center )
    *X1 = x - width/2;
    else
    if (  figure->zip_figure_mode.zip_figure_mode_right )
    *X1 = x - width;

  if (  figure->zip_figure_mode.zip_figure_mode_top )
    *Y1 = y;
    else
    if (  figure->zip_figure_mode.zip_figure_mode_middle )
    *Y1 = y - font_height/2;
    else
    if (  figure->zip_figure_mode.zip_figure_mode_baseline )
    *Y1 = y - font_height/2; /*===*/
    else
    if (  figure->zip_figure_mode.zip_figure_mode_bottom )
    *Y1 = y - font_height; /*===*/
    else
    *Y1 = y - font_height/2; /*===*/

  *X2 = *X1 + width/2;
  *X3 = *X1 + width;
  *Y2 = *Y1 + height/2;
  *Y3 = *Y1 + height;
  }



long
zipocapt__Set_Object_Font( self, figure, font )
  register struct zipocapt		 *self;
  register zip_type_figure		  figure;
  register short			  font;
  {
  IN(zipocapt_Set_Object_Font);
  figure->zip_figure_font = font;
  OUT(zipocapt_Set_Object_Font);
  return  zip_ok;
  }
