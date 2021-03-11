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
static char rcsid[]="$Header: /afs/cs.cmu.edu/project/atk-dist/auis-6.3/contrib/zip/lib/RCS/zipofcap.c,v 1.4 1993/05/04 01:51:05 susan Exp $";
#endif

/* zipofcap.c	Zip Object -- Flexible Captions				      */
/* Author	TC Peters						      */
/* Information Technology Center		   Carnegie-Mellon University */

/*
    $Log: zipofcap.c,v $
 * Revision 1.4  1993/05/04  01:51:05  susan
 * RCS Tree Split
 *
 * Revision 1.3.1.1  1993/02/02  06:58:38  rr2b
 * new R6tape branch
 *
 * Revision 1.3  1992/12/15  21:57:55  rr2b
 * more disclaimerization fixing
 *
 * Revision 1.2  1992/12/15  04:00:36  rr2b
 * disclaimerization
 *
 * Revision 1.1  1992/10/06  18:33:00  susan
 * Initial revision
 *
 * Revision 2.8  1991/09/12  16:42:55  bobg
 * Update copyright notice and rcsid
 *
 * Revision 2.7  1991/09/10  20:52:14  gk5g
 * Changes in support of SGI_4d platform.
 * Mostly added forward delcarations.
 *
 * Revision 2.6  1989/09/11  08:17:28  ghoti
 * fix enumeration type clashes - specifically those dealing with mouse actions
 *
 * Revision 2.5  89/09/08  17:41:54  ghoti
 * removal of unused variables
 * 
 * Revision 2.4  89/05/01  22:12:58  tom
 * Use special symbolic font-names.
 * 
 * Revision 2.3  89/02/08  16:50:34  ghoti
 * change copyright notice
 * 
 * Revision 2.2  89/02/07  19:43:18  ghoti
 * first pass porting changes: filenames and references to them
 * 
 * Revision 2.1  88/09/27  18:15:40  ghoti
 * adjusting rcs #
 * 
 * Revision 1.2  88/09/15  17:38:16  ghoti
 * copyright fix
 * 
 * Revision 1.1  88/09/14  17:45:15  tom
 * Initial revision
 * 
*/

/**  SPECIFICATION -- Internal Facility Suite  *********************************

TITLE	Zip Object -- Flexible Captions

MODULE	zipofcap.c

NOTICE	IBM Internal Use Only

DESCRIPTION

.......................

HISTORY
  06/23/88	Created (TC Peters)
  05/01/89	Use symbolic font-names (TCP)

END-SPECIFICATION  ************************************************************/

#include "class.h"
#include "fontdesc.ih"
#include "zipobj.ih"
#include "zipofcap.eh"

static enum view_MouseAction Accept_Caption_Character();
static Draw();
static Compute_Handle_Positions();

char
zipofcapt__Object_Icon( self )
  register struct zipofcapt		 *self;
  {
  IN(zipofcapt__Object_Icon);
  OUT(zipofcapt__Object_Icon);
/*===  return  'B';===*/
  return  NULL;
  }

char
zipofcapt__Object_Icon_Cursor( self )
  register struct zipofcapt		 *self;
  {
  IN(zipofcapt__Object_Icon_Cursor);
  OUT(zipofcapt__Object_Icon_Cursor);
  return  'B';
  }

char
zipofcapt__Object_Datastream_Code( self )
  register struct zipofcapt		 *self;
  {
  IN(zipofcapt__Object_Datastream_Code);
  OUT(zipofcapt__Object_Datastream_Code);
  return  'B';
  }

long
zipofcapt__Build_Object( self, pane, action, x, y, clicks, X, Y )
  register struct zipofcapt		 *self;
  register zip_type_pane		  pane;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  register zip_type_point		  X, Y;
  {
  zip_type_figure			  figure;
  register long				  status = zip_ok;
  enum view_MouseAction			  Accept_Caption_Character();
  int					  position = 0; /*===*/
  char					  text[4];
/*===debug=1;*/
  IN(zipofcapt__Build_Object);
  switch ( action )
    {
    case view_LeftDown:
      zipview_Set_Pane_Cursor( View, pane, 'C', CursorFontName );
      if ( (status =
        zip_Create_Figure( Data, &CurrentFigure, NULL, zip_flexible_caption_figure,
			   CurrentImage, position )) == zip_ok )
	{
        zipofcapt_Set_Object_Point( self, CurrentFigure, 9, X, Y );
        zipofcapt_Set_Object_Point( self, CurrentFigure, 5, X, Y );
	text[0] = '|'; text[1] = '\0';
        CurrentFigure->zip_figure_font = pane->zip_pane_current_font;
        CurrentFigure->zip_figure_mode = pane->zip_pane_current_mode;
	ZIP_Select_Figure_Font( View, CurrentFigure );
        zip_Set_Figure_Text( Data, CurrentFigure, text );
	CurrentFigure->zip_figure_zoom_level = pane->zip_pane_zoom_level;
        pane->zip_pane_edit->zip_pane_edit_last_point_id = 5;
	zip_Set_Figure_Shade( Data, CurrentFigure,
			      pane->zip_pane_edit->zip_pane_edit_current_shade );
	}
      break;
    case view_LeftUp:
      if ( figure = CurrentFigure )
	{
	if ( figure_x_point == figure_x_points(0)  &&
	     figure_y_point == figure_y_points(0) )
	  {
	  zipedit_Delete_Figure( Edit, figure, pane );
          break;
	  }
	  else  zipedit_Set_Keyboard_Processor( Edit, self, Accept_Caption_Character );
	}
      /* Fall-thru */
    case view_LeftMovement:
      if ( CurrentFigure )
	{
        zipedit_Normalize_Figure_Points( Edit, CurrentFigure, pane );
	zipview_Clear_Figure( View, CurrentFigure, pane );
        zipofcapt_Set_Object_Point( self, CurrentFigure, 5, X, Y );
	zipview_Draw_Figure( View, CurrentFigure, pane );
	if ( action != view_LeftUp )
	  zipedit_Highlight_Figure_Points( Edit, CurrentFigure, pane );
	}
      break;
    }
  OUT(zipofcapt__Build_Object);
  return  status;
  }

static enum view_MouseAction
Accept_Caption_Character( self, pane, c, action, x, y, clicks )
  register struct zipofcapt		 *self;
  register zip_type_pane		  pane;
  register char				  c;
  register enum view_MouseAction	  action;
  register long				  x, y, clicks;
  {
  register zip_type_figure		  figure = pane->zip_pane_current_figure;
  char					  text[4097];/*===*/
  register char				 *text_cursor;

  IN(Accept_Caption_Character)
  if ( action == view_LeftUp  ||  action == view_LeftMovement )
    action = 0;
    else if ( CurrentFigure )
      { DEBUG(Current Figure Exists);
      strcpy( text, figure->zip_figure_datum.zip_figure_text );/*===*/
      text_cursor = strlen( text ) + &text[0];
      zipview_Clear_Figure( View, figure, pane );
      switch ( c )
        {
	case 0:	    DEBUG(Finished Typing);
          if ( *text  &&  *(text_cursor-1) == '|' )
	    *(text_cursor-1) = 0;
          if ( *text == 0 )
	    zipedit_Delete_Figure( Edit, figure, pane );
          zipedit_Set_Keyboard_Processor( Edit, NULL, NULL );
          break;
        case '\012':
	case '\015':	DEBUG(NewLine);
          *(text_cursor-1) = '\\';
          *text_cursor = 'n';
          *(text_cursor+1) = '|';
          *(text_cursor+2) = 0;
          break;
        case '\010':
	case '\177':	DEBUG(BackSpace|Delete);
          if ( text_cursor > text+1 )
            { DEBUG(Not at front);
	    if ( (text_cursor - text) > 2  &&
	       *(text_cursor-3) == '\\'  &&  *(text_cursor-2) == 'n' )
	      text_cursor--;
            *(text_cursor-2) = '|';
            *(text_cursor-1) = 0;
            }
          break;
        default:   DEBUG(Regular);
          *(text_cursor-1) = c;
          *text_cursor = '|';
          *(text_cursor+1) = 0;
      }
    if ( *text )
      {
      zip_Set_Figure_Text( Data, figure, text );
      zipview_Draw_Figure( View, figure, pane );
      }
    }
    else
    {
    zipedit_Set_Keyboard_Processor( Edit, NULL, NULL );
    }
  OUT(Accept_Caption_Character);
  return  action;
  }

long
zipofcapt__Draw_Object( self, figure, pane )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipofcapt__Draw_Object);
  if ( zipview_Condition( View, pane, figure, zip_draw ) )
    status = Draw( self, figure, pane );
  OUT(zipofcapt__Draw_Object);
  return  status;
  }

long
zipofcapt__Clear_Object( self, figure, pane )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;

  IN(zipofcapt__Clear_Object);
  if ( zipview_Condition( View, pane, figure, zip_clear ) )
    status = Draw( self, figure, pane );
  OUT(zipofcapt__Clear_Object);
  return  status;
  }

static
Draw( self, figure, pane )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  register long				  status = zip_ok;
  register struct fontdesc		 *font, *current_font =
					    zipview_GetFont( View );
  char					 *text = NULL;
  register long				  mode = NULL,
					  width, wide, widest = 0,
					  height, tallest = 0,
					  x, y, y_increment, lines = 1;
  char					  buffer[4097], widest_buffer[4097];
  register char				 *cursor, *buffer_ptr, *font_family;
  register long				  font_style, font_size, prior_font_size,
					  adjusted = false,
					  too_small = 0, too_big = 0,
					  too_wide = 0, too_tall = 0,
					  too_narrow = 0, too_short = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3, xp, yp;
/*===debug=1;*/
  IN(Draw);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
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
    mode |= (figure->zip_figure_mode.zip_figure_mode_top)      ? graphic_ATTOP      : 0;
    mode |= (figure->zip_figure_mode.zip_figure_mode_middle)   ? graphic_BETWEENTOPANDBOTTOM   : 0;
    mode |= (figure->zip_figure_mode.zip_figure_mode_baseline) ? graphic_BETWEENTOPANDBASELINE : 0;
    mode |= (figure->zip_figure_mode.zip_figure_mode_bottom)   ? graphic_ATBOTTOM   : 0;
    mode |= (figure->zip_figure_mode.zip_figure_mode_left)     ? graphic_ATLEFT     : 0;
    mode |= (figure->zip_figure_mode.zip_figure_mode_center)   ? graphic_BETWEENLEFTANDRIGHT   : 0;
    mode |= (figure->zip_figure_mode.zip_figure_mode_right)    ? graphic_ATRIGHT    : 0;
    cursor = text;
    while ( *cursor )
      {
      if ( *cursor++ == '\\' )
	if ( *cursor++ == 'n' ) lines++;
      }
    DEBUGdt(Lines,lines);
    font = zipview_Select_Contextual_Figure_Font( View, figure );
    font_family = fontdesc_GetFontFamily( font );
    font_style = fontdesc_GetFontStyle( font );
    width = abs(window_x_points(0) - window_x_point);    DEBUGdt(Width,width);
    height = abs(window_y_points(0) - window_y_point);   DEBUGdt(Height,height);
    cursor = text;
    while ( *cursor )
      {
      buffer_ptr = buffer;
      while ( *cursor  &&  !(*cursor == '\\'  &&  *(cursor+1) == 'n') )
        *buffer_ptr++ = *cursor++;
      if ( *cursor == '\\' )
	{
	cursor++;
	if ( *cursor == 'n' )
	  cursor++;
	}
      *buffer_ptr = 0;
      if ( (wide = fontdesc_StringSize( font, View, buffer, &xp, &yp )) > widest )
	{
	widest = wide;
	strcpy( widest_buffer, buffer );
	}
      }
    font = fontdesc_Create( font_family, font_style, font_size = 4 );
    zipview_SetFont( View, font );
    while ( (! adjusted)  &&  font_size >= 4  &&  font_size <= 34/*===*/ )
      {
      prior_font_size = font_size;;
      DEBUGdt(Prior-Font-size,prior_font_size);
      widest = fontdesc_StringSize( font, View, widest_buffer, &xp, &yp );
      DEBUGdt(Widest,widest);
      tallest = lines * font_size;
      DEBUGdt(Tallest,tallest);
      if ( ! too_small )
	{ DEBUG(Too Big...);
	if ( widest > (width + 2) )       { DEBUG(Too Wide);  too_wide++; }
	if ( tallest > (height + 2) )     { DEBUG(Too Tall);  too_tall++; }
	if ( too_wide  ||  too_tall )
	  {
	  too_big++; too_wide = too_tall = false;
	  font = fontdesc_Create( font_family, font_style, font_size -= 2 );
          }
	}
      if ( ! too_big )
	{ DEBUG(Too Small...)
	if ( widest < (width - 2) )	  { DEBUG(Too Narrow);  too_narrow++; }
	if ( tallest < (height - 2) )	  { DEBUG(Too Short);   too_short++;  }
	if ( too_narrow  &&  too_short )
	  {
	  too_small++; too_narrow = too_short = false;
	  font = fontdesc_Create( font_family, font_style, font_size += 2 );
	  }
	}
      if ( !too_small  &&  !too_big )
        adjusted = true;
      zipview_SetFont( View, font );
      if ( (font_size = fontdesc_GetFontSize( font )) == prior_font_size )
	{ DEBUG(No change);
	adjusted = true;
	}
      }
    zipview_SetFont( View, font );
    DEBUGdt(Final Font-size,fontdesc_GetFontSize( font ));
    y = Y2;  x = X2;
    y_increment = fontdesc_GetFontSize( font ) + 5 /*===fudge.s/b FontNewLine===*/;
    if ( figure->zip_figure_mode.zip_figure_mode_top )
      y = Y1;
    else
    if ( figure->zip_figure_mode.zip_figure_mode_middle )
      y = y - ((lines>>1) * y_increment);
    else
    if ( figure->zip_figure_mode.zip_figure_mode_bottom )
      y = Y3;
    if ( figure->zip_figure_mode.zip_figure_mode_left )
      x = X1;
    else
    if ( figure->zip_figure_mode.zip_figure_mode_right )
      x = X3;
    zipview_Set_Clip_Area( View, pane, window_x_point, window_y_point, width, height );
    cursor = text;
    while ( *cursor )
      {
      buffer_ptr = buffer;
      while ( *cursor  &&  !(*cursor == '\\'  &&  *(cursor+1) == 'n') )
        *buffer_ptr++ = *cursor++;
      *buffer_ptr = 0;
      zipview_MoveTo( View, x, y );
      zipview_DrawString( View, buffer, mode );
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
    zipview_Set_Pane_Clip_Area( View, pane );
{
/*===hokey: should have fontdesc facility===*/
char font_style_name[5];
    *font_style_name = 0;
    if ( font_style & fontdesc_Bold )
      strcat( font_style_name, "b" );
    if ( font_style & fontdesc_Italic )
      strcat( font_style_name, "i" );
    sprintf( buffer, "%s%s%d", font_family, font_style_name, font_size );
}
    DEBUGst(Font-name,buffer);
    zip_Set_Figure_Font( Data, figure, buffer );
    }
  if ( ExposePoints )
    zipofcapt_Expose_Object_Points( self, figure, pane );
  if ( HighlightPoints )
    zipofcapt_Highlight_Object_Points( self, figure, pane );
  zipview_SetFont( View, current_font );
  OUT(Draw);
  return  status;
  }

long
zipofcapt__Set_Object_Point( self, figure, point, x, y )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register int				  point;
  register zip_type_point		  x, y;
  {
  register long				  status = zip_ok;

  IN(zipofcapt__Set_Object_Point);
  if ( figure->zip_figure_points == NULL  &&
       (status = zip_Allocate_Figure_Points_Vector(
		    Data, &figure->zip_figure_points )) == zip_ok )
    figure->zip_figure_points->zip_points_count = 1;
  if ( status == zip_ok )
    {
    switch ( point )
      {
      case 1: /* Center */
	zipofcapt_Adjust_Object_Point_Suite( self, figure,
	    x - (figure_x_point + (figure_x_points(0) - figure_x_point)/2),
	    y - (figure_y_point - (figure_y_point - figure_y_points(0))/2) );
        break;
      case 2: /* 12 O'Clock */
	figure_y_points(0) -= y - figure_y_point;
	figure_y_point = y;
	break;
      case 3: /* Upper Right Corner */
        figure_x_points(0) = x;
	figure_y_point = y;
        break;
      case 4: /* 3 O'Clock */
	figure_x_point -= x - figure_x_points(0);
	figure_x_points(0) = x;
	break;
      case  5: /* Lower Right Corner */
        figure_x_points(0) = x;
        figure_y_points(0) = y;
	break;
      case 6: /* 6 O'Clock */
	figure_y_point += figure_y_points(0) - y;
	figure_y_points(0) = y;
	break;
      case 7: /* Lower Left Corner */
        figure_x_point = x;
	figure_y_points(0) = y;
	break;
      case 8: /* 9 O'Clock */
	figure_x_points(0) += figure_x_point - x;
	figure_x_point = x;
	break;
      case 9: /* Upper Left Corner */
        figure_x_point = x;
        figure_y_point = y;
	break;
      default: status = zip_failure; /*=== zip_invalid_handle ===*/
      }
    if ( status == zip_ok )
      {
      zip_Set_Image_Extrema( Data, figure->zip_figure_image, x, y );
/*===handle both extrema 7/20/86===*/
/*===have extrema check for REDUCTIONS as well as EXPANSIONS 7/20/86===*/
      zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			      figure->zip_figure_image );
      }
    }
  OUT(zipofcapt__Set_Object_Point);
  return  status;
  }

long
zipofcapt__Proximate_Object_Points( self, figure, pane, x, y )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y;
  {
  register int				  point = 0;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipofcapt__Proximate_Object_Points);
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
  OUT(zipofcapt__Proximate_Object_Points);
  return  point;
  }

boolean
zipofcapt__Enclosed_Object( self, figure, pane, x, y, w, h )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		  x, y, w, h;
  {
  register boolean			  enclosed = false;
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipofcapt__Enclosed_Object);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  if ( X1 > x  &&  Y1 > y  &&  X2 < (x + w)  &&  Y2 < (y + h) )
    enclosed = true;
  OUT(zipofcapt__Enclosed_Object);
  return  enclosed;
  }

long
zipofcapt__Object_Enclosure( self, figure, pane, x, y, w, h )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *x, *y, *w, *h;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipofcapt__Object_Enclosure);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  *x = X1;  *y = Y1;  *w = abs(X3 - X1);  *h = abs(Y3 - Y1);
  OUT(zipofcapt__Object_Enclosure);
  return  zip_ok;
  }

long
zipofcapt__Highlight_Object_Points( self, figure, pane )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipofcapt__Highlight_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Highlight_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipofcapt__Highlight_Object_Points);
  return  zip_ok;
  }

long
zipofcapt__Normalize_Object_Points( self, figure, pane )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  {
  zip_type_pixel			  X1, X2, X3, Y1, Y2, Y3;

  IN(zipofcapt__Normalize_Object_Points);
  Compute_Handle_Positions( self, figure, pane, &X1, &X2, &X3, &Y1, &Y2, &Y3 );
  zipedit_Normalize_Handles( Edit, pane, X1, X2, X3, Y1, Y2, Y3 );
  OUT(zipofcapt__Normalize_Object_Points);
  return  zip_ok;
  }

long
zipofcapt__Adjust_Object_Point_Suite( self, figure, x_delta, y_delta )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_point		  x_delta, y_delta;
  {
  register long				  status = zip_ok;

  IN(zipofcapt__Adjust_Object_Point_Suite);
  figure_x_point += x_delta;
  figure_y_point += y_delta;
  figure_x_points(0) += x_delta;
  figure_y_points(0) += y_delta;
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_point, figure_y_point );
  zip_Set_Image_Extrema( Data, figure->zip_figure_image, figure_x_points(0), figure_y_points(0) );
/*===have extrema check for REDUCTIONS as well as EXPANSIONS 7/20/86===*/
  zip_Set_Stream_Extrema( Data, figure->zip_figure_image->zip_image_stream,
			    figure->zip_figure_image );
  OUT(zipofcapt__Adjust_Object_Point_Suite);
  return  status;
  }

static
Compute_Handle_Positions( self, figure, pane, X1, X2, X3, Y1, Y2, Y3 )
  register struct zipofcapt		 *self;
  register zip_type_figure		  figure;
  register zip_type_pane		  pane;
  register zip_type_pixel		 *X1, *X2, *X3,
					 *Y1, *Y2, *Y3;
  {
  *X1 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_point );
  *X2 = *X1 + (window_x_points(0) - window_x_point)/2;
  *X3 = zipview_X_Point_To_Pixel( View, pane, figure, figure_x_points(0) );
  *Y1 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_point );
  *Y2 = *Y1 + (window_y_points(0) - window_y_point)/2;
  *Y3 = zipview_Y_Point_To_Pixel( View, pane, figure, figure_y_points(0) );
  }
