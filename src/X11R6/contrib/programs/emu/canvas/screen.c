#if !defined(lint) && !defined(__clipper__)
 static char *rcsid = "screen.c,v 1.3 1994/06/02 10:55:58 me Exp";
#endif

/*
 * This file is part of the Emu system.
 *
 * Copyright 1990 by PCS Computer Systeme, GmbH. Munich, West Germany.
 * 
 * Copyright 1994 by Jordan K. Hubbard and Michael W. Elbel
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL PCS, THE AUTHORS, OR THEIR HOUSEPETS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. SO DON'T SUE US.
 * THANK YOU.
 */

/*
 * General screen manipulation routines
 *
 * Author: Michael Elbel
 * Date: March 28th, 1990.
 * Description: Here we have most of the routines that manipulate the canvas
 * 		window directly like scrolling and so on.
 *
 * Revision History:
 *
 * screen.c,v
 * Revision 1.3  1994/06/02  10:55:58  me
 * Fixed a bug where blinking double sized text wouldn't work properly
 *
 * Revision 1.2  1994/05/24  19:55:50  me
 * New Copyright
 * fixed bug where the text cursor dissappeared upon a move_abs
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 *
 * Revision 1.15  93/08/18  15:07:09  me
 * Fixed blinking text color
 * 
 * Revision 1.14  92/10/16  16:00:19  me
 * More fixes by Steve Crooks
 * 
 * Revision 1.13  92/05/16  06:25:38  jkh
 * Synchronization checkin.
 * 
 * Revision 1.12  92/02/26  11:33:00  me
 * Steve Crooks clix port and general code cleanup
 * 
 * Revision 1.1  90/04/12  14:29:34  me
 * Initial revision
 */

#include "canvas.h"

/*
 * Routine to turn the cursor off if necessary.
 */

Export void
flip_cursor_off(TermCanvasWidget w)
{
     char c;
     
     /* Should we really flip? */
     if (!w->term.cursor_on || !w->term.cursor_visible ||
	 (w->term.screen_cur.y < 0) || (w->term.screen_cur.x < 0))
	  return;
     
     if (w->term.curs_char.value == 0)
	  XFillRectangle(XtDisplay(w), XtWindow(w), w->term.clear_gc,
			 w->term.screen_cur.x,
			 w->term.screen_cur.y - w->term.cursor_height,
			 w->term.cursor_width, w->term.cursor_height);
     else {
	  c = w->term.curs_char.value;
#ifdef DOUBLE_FONTS
	  draw_string(w, w->term.cursor_rem_gc, w->term.curs_char.attributes,
		      w->term.screen_cur.x,
		      w->term.screen_cur.y - w->term.font_descent +
		      w->term.double_offset, &c, 1);
#else 
	  draw_string(w, w->term.cursor_rem_gc, w->term.curs_char.attributes,
		      w->term.screen_cur.x,
		      w->term.screen_cur.y - w->term.font_descent, &c, 1);
#endif /* DOUBLE_FONTS */ 
     }

     w->term.cursor_on = False;
}

/*
 * Routine to turn the cursor on if desired. If the cursor is off but there
 * is a timer set to turn it on, turn it on immediately and reset the timer.
 */

Export void
flip_cursor_on(TermCanvasWidget w)
{
     char c;
     
     /* Should we really flip? */
     if (w->term.cursor_on ||
	 (!w->term.cursor_visible && (w->term.timeout_id == 0)) ||
	 (w->term.screen_cur.y < 0) || (w->term.screen_cur.x < 0))
	  return;
     
     if (w->term.outline_cursor)
	  XDrawRectangle(XtDisplay(w), XtWindow(w), w->term.cursor_graph_gc,
			 w->term.screen_cur.x,
			 w->term.screen_cur.y - w->term.cursor_height,
			 w->term.cursor_width - 1, w->term.cursor_height -1);
     else
	  if (w->term.curs_char.value == 0)
	       XFillRectangle(XtDisplay(w), XtWindow(w), w->term.cursor_graph_gc,
			      w->term.screen_cur.x,
			      w->term.screen_cur.y - w->term.cursor_height,
			      w->term.cursor_width, w->term.cursor_height);
	  else {
	       c = w->term.curs_char.value;
#ifdef DOUBLE_FONTS
	       draw_string(w, w->term.cursor_text_gc,
			   w->term.curs_char.attributes,
			   w->term.screen_cur.x,
			   w->term.screen_cur.y - w->term.font_descent +
			   w->term.double_offset, &c, 1);
#else 
	       draw_string(w, w->term.cursor_text_gc,
			   w->term.curs_char.attributes,
			   w->term.screen_cur.x,
			   w->term.screen_cur.y - w->term.font_descent, &c, 1);
#endif
	  }
     
     if (!w->term.cursor_visible && (w->term.timeout_id != 0)) {
	  w->term.cursor_visible = True;
	  XtRemoveTimeOut(w->term.timeout_id);
	  w->term.timeout_id = XtAddTimeOut(w->term.blink_interval,
					    blink_cursor, w);
     }
     
     w->term.cursor_on = True;
}

/*
 * To switch the cursor off totally. Checks whether there is a timeout
 * set and turns that off if necessary
 */
Export void
switch_cursor_off(TermCanvasWidget w)
{
     if (w->term.timeout_id != 0) {
	      XtRemoveTimeOut(w->term.timeout_id);
	      w->term.timeout_id = 0;
     }

     if (w->term.cursor_visible) {
	  flip_cursor_off(w);
	  w->term.cursor_visible = False;
     }
}

/*
 * To switch the cursor on again. Checks whether the cursor should blink,
 * then activates the timeout again
 */
Export void
switch_cursor_on(TermCanvasWidget w)
{
     if (w->term.cursor_visible || (w->term.timeout_id != 0))
	  return;		/* cursor was already on */

     flip_cursor_on(w);
     w->term.cursor_visible = True;

     if (w->term.cursor_blinking &&
	 (w->term.focused || w->term.blink_wo_focus))
	  w->term.timeout_id = XtAddTimeOut(w->term.blink_interval,
					    blink_cursor, w);
}    

/*
 * Scroll the screen. The lines that were scrolled away get erased.
 */
Export void
scroll_screen(TermCanvasWidget w, int start_lin, int lines, int scroll_lines)
{
     int fh = w->term.cell_height;
     int fw = w->term.cell_width;

     if (scroll_lines >0) {
	  /* scroll up */
	  XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), w->term.gc,
		    0, (start_lin + scroll_lines) * fh,
		    w->term.columns * fw, (lines - scroll_lines) * fh,
		    0, start_lin * fh);
	  XFillRectangle(XtDisplay(w), XtWindow(w), w->term.clear_gc,
			 0, (start_lin + lines - scroll_lines)* fh,
			 w->term.columns * fw, fh * scroll_lines);
     } else {
	  /* scroll down */
	  XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), w->term.gc,
		    0, start_lin * fh,
		    w->term.columns * fw, (lines - scroll_lines) * fh,
		    0, (start_lin - scroll_lines) * fh);
	  XFillRectangle(XtDisplay(w), XtWindow(w), w->term.clear_gc,
			 0, start_lin * fh,
			 w->term.columns * fw, fh * (-scroll_lines));
     }
}

/*
 * Deletes characters in the given line starting with start_col by shifting
 * everything right of that region left and blanking the resulting space.
 */
Export void
delete_screen_chars(TermCanvasWidget w, int line, int start_col,
		    int no_chars, int line_len)
{
     int fh = w->term.cell_height;
     int fw = w->term.cell_width;

#ifdef DOUBLE_FONTS 
     if (w->term.line_flags[line])
	       fw *= 2;
#endif /* DOUBLE_FONTS */
     
#if 0 /* Probably not necessary, since X does the clipping itself */
     /* clip to the right border of the line */
     if ((start_col + no_chars) > line_len)
	  no_chars = line_len - start_col;
#endif 
     
     XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), w->term.gc,
	       (start_col + no_chars) * fw, line * fh,
	       (line_len - start_col - no_chars) * fw, fh,
	       start_col * fw, line * fh);
     XFillRectangle(XtDisplay(w), XtWindow(w), w->term.clear_gc,
		    (line_len - no_chars) * fw , line * fh,
		    no_chars * fw, fh);
}

/*
 * Erases characters in the given line starting with start_col by 
 * blanking the space.
 */
Export void
erase_screen_chars(TermCanvasWidget w, int line, int start_col,
		   int no_chars, int line_len)
{
     int fh = w->term.cell_height;
     int fw = w->term.cell_width;
     
#ifdef DOUBLE_FONTS 
     if (w->term.line_flags[line]) {
	       fw *= 2;
     }
#endif /* DOUBLE_FONTS */
     
#if 0 /* Probably not necessary, since X does the clipping itself */
     /* clip to the right border of the line */
     if ((start_col + no_chars) > line_len)
	  no_chars = line_len - start_col;
#endif 
     
     XFillRectangle(XtDisplay(w), XtWindow(w), w->term.clear_gc,
		    start_col * fw , line * fh,
		    no_chars * fw, fh);
}

/*
 * Clears a number of lines on the screen (e.g. for a clear screen operation)
 */
Export void
erase_screen_lines(TermCanvasWidget w, int start, int no)
{

     XFillRectangle(XtDisplay(w), XtWindow(w), w->term.clear_gc,
		    0, start * w->term.cell_height,
		    w->term.columns * w->term.cell_width,
		    no * w->term.cell_height);
}

/*
 * Vaildate the default GCs in the widget private structure.
 *
 * Normal Text without attributes and color 0/0 is always done via
 * 'term->norm_gc' so this doesnt need to be changed while 'term->spec_gc'
 * is used for text 'with' attributes. The attributes for the last
 * spec_gc are stored in 'term->old_attribs' and 'term->old_color'.
 *
 * validate_gc sets 'term->gc' to 'term->norm_gc' or 'term->spec_gc' and
 * returns the gc.
 */
Export GC
#ifdef DOUBLE_FONTS 
validate_gc(TermCanvasWidget w, unsigned char attrs, unsigned char color, 
	    LineFlagsElem lflags)
#else
validate_gc(TermCanvasWidget w, unsigned char attrs, unsigned char color)
#endif /* DOUBLE_FONTS */ 
{
     XGCValues val;
     unsigned char old, oldc;
#ifdef DOUBLE_FONTS
     LineFlagsElem oldl = w->term.old_lflags;
     XRectangle rect;
     w->term.act_lflags = lflags;
#endif /* DOUBLE_FONTS */ 
     w->term.act_attribs = attrs;
     w->term.act_color = color;
     
     /* Switching to normal ?? */
#ifdef DOUBLE_FONTS 
     if (((attrs & FONT_ATTS) == 0) && (color == 0) && (lflags == 0))
     {
	  w->term.double_offset = 0;
	  w->term.gc = w->term.norm_gc;
	  return(w->term.gc);
     }
#else 
     if (((attrs & FONT_ATTS) == 0) && (color == 0))
     {
	  w->term.gc = w->term.norm_gc;
	  return(w->term.gc);
     }
#endif /* DOUBLE_FONTS */ 

     oldc = w->term.old_color;
     old = w->term.old_attribs;
     
     /*
      * Just return the old gc if we're changing to the old attributes
      * and the color hasn't changed.
      */
#ifdef DOUBLE_FONTS 
     if ((old == attrs) && (color == oldc) && (lflags == oldl) &&
	 (lflags == 0))
#else 
     if ((old == attrs) && (color == oldc))
#endif /* DOUBLE_FONTS */ 
     {
	  w->term.gc = w->term.spec_gc;
	  return(w->term.spec_gc);
     }
     
#ifdef DOUBLE_FONTS
     if (lflags) {
	  /* Double wide / or high */
	  /* New font */
	  if (lflags & LINE_D_WIDE) {
	       /* double width single height */
	       if (attrs & ATT_BOLD) {
		    /* double wide single height bold */
		    if (w->term.d_wb_font == NULL) {
			 /* Not there, load it */
			 if ((w->term.d_wb_font =
			      XLoadQueryFont(XtDisplay(w),
					     w->term.d_wb_font_n)) == NULL)
				   /* Couldn't load it, use the normal font */
			      w->term.d_wb_font = w->term.normal_font;
		    }
		    XSetFont(XtDisplay(w), w->term.spec_gc,
			     w->term.d_wb_font->fid);
	       } else {
		    /* double wide single height normal */
		    if (w->term.d_w_font == NULL) {
			 /* Not there, load it */
			 if ((w->term.d_w_font =
			      XLoadQueryFont(XtDisplay(w),
					     w->term.d_w_font_n)) == NULL)
				   /* Couldn't load it, use the normal font */
			      w->term.d_w_font = w->term.normal_font;
		    }
		    XSetFont(XtDisplay(w), w->term.spec_gc,
			     w->term.d_w_font->fid);
	       }
	       w->term.double_offset = 0;
	       XSetClipMask(XtDisplay(w), w->term.spec_gc, None);
	  } else {
	       /* double width double height */
	       if (attrs & ATT_BOLD) {
		    /* bold */
		    if (w->term.d_hb_font == NULL) {
			 /* Not there, load it */
			 if ((w->term.d_hb_font =
			      XLoadQueryFont(XtDisplay(w),
					     w->term.d_hb_font_n)) == NULL)
				   /* Couldn't load it, use the normal font */
			      w->term.d_hb_font = w->term.normal_font;
		    }
		    XSetFont(XtDisplay(w), w->term.spec_gc,
			     w->term.d_hb_font->fid);
	       } else {
		    /* normal */
		    if (w->term.d_h_font == NULL) {
			 /* Not there, load it */
			 if ((w->term.d_h_font =
			      XLoadQueryFont(XtDisplay(w),
					     w->term.d_h_font_n)) == NULL) 
				   /* Couldn't load it, use the normal font */
			      w->term.d_h_font = w->term.normal_font;
		    }
		    XSetFont(XtDisplay(w), w->term.spec_gc,
			     w->term.d_h_font->fid);
	       }
	       w->term.double_offset = lflags & LINE_D_UPPER ?
		    w->term.cell_height - w->term.font_descent:
	            0 - w->term.font_descent;
		    
	       /* change the clip rectangle */
	       rect.x = 0;
	       rect.y = 0;
	       rect.width = w->term.columns * w->term.cell_width;
	       rect.height = w->term.cell_height;
	       
	       XSetClipRectangles(XtDisplay(w), w->term.spec_gc,
				  0, w->term.array_cur.lin
				  * w->term.cell_height,
				  &rect, 1, YXBanded);
	  }
     } else {
	  if (lflags != oldl) {
	       if (attrs & ATT_BOLD)
		    XSetFont(XtDisplay(w), w->term.spec_gc,
			     w->term.bold_font->fid);
	       else
		    XSetFont(XtDisplay(w), w->term.spec_gc,
			     w->term.normal_font->fid);
	       XSetClipMask(XtDisplay(w), w->term.spec_gc, None);
	       w->term.double_offset = 0;
	  } else {
	       if (attrs & ATT_BOLD) {
		    if(!(old & ATT_BOLD))
			 XSetFont(XtDisplay(w), w->term.spec_gc,
				  w->term.bold_font->fid);
	       }
	       else {
		    if(old & ATT_BOLD)
			 XSetFont(XtDisplay(w), w->term.spec_gc,
				  w->term.normal_font->fid);
	       }
	  }
     }
#else 
     if (attrs & ATT_BOLD) {
	  if(!(old & ATT_BOLD))
	       XSetFont(XtDisplay(w), w->term.spec_gc,
			w->term.bold_font->fid);
     }
     else {
	  if(old & ATT_BOLD)
	       XSetFont(XtDisplay(w), w->term.spec_gc,
			w->term.normal_font->fid);
     }
#endif /* DOUBLE_FONTS */ 
     
     if ((attrs & ATT_REVERSE) != (attrs & ATT_SELECTED)) {
	  if (((old & ATT_REVERSE) == (old & ATT_SELECTED))
	      || (color != oldc)) {
	       val.background = w->term.cit[GET_COLOR_FG(color)].fg.pix;
	       val.foreground = w->term.cit[GET_COLOR_BG(color)].bg.pix;
	       XChangeGC(XtDisplay(w), w->term.spec_gc,
			 GCForeground | GCBackground, &val);
	  }
     }
     else {
	  if (((old & ATT_REVERSE ) != (old & ATT_SELECTED))
	      || (color != oldc)) {
	       val.foreground = w->term.cit[GET_COLOR_FG(color)].fg.pix;
	       val.background = w->term.cit[GET_COLOR_BG(color)].bg.pix;
	       XChangeGC(XtDisplay(w), w->term.spec_gc,
			 GCForeground | GCBackground, &val);
	  }
     }
     w->term.old_attribs = attrs;
     w->term.old_color = color;
#ifdef DOUBLE_FONTS
     w->term.old_lflags = lflags;
#endif /* DOUBLE_FONTS */ 
     w->term.gc = w->term.spec_gc;
     return(w->term.spec_gc);
}

/*
 * Vaildate the cursor GCs in the widget private structure.
 */
Export void
#ifdef DOUBLE_FONTS
validate_cursor_gc(TermCanvasWidget w, Char cur_char, LineFlagsElem lflags)
#else
validate_cursor_gc(TermCanvasWidget w, Char cur_char)
#endif /* DOUBLE_FONTS */ 
{
     XGCValues val;
     short value = cur_char.value;
     unsigned char attrs = value == 0 ? 0 : cur_char.attributes;
     unsigned char color = value == 0 ? 0 : cur_char.color;
     unsigned char old_att = w->term.curs_char.attributes;
     unsigned char old_color = w->term.curs_char.color;
#ifdef DOUBLE_FONTS
     LineFlagsElem old_lflags = w->term.curs_lflags;
     if (value == 0) lflags = 0;

     /* If the attributes haven't changed, just come back */
     if ((old_att == attrs) && (old_color == color) && (old_lflags == lflags)){
	  w->term.curs_char = cur_char;
	  return;
     }
#else 
     /* If the attributes haven't changed, just come back */
     if ((old_att == attrs) && (old_color == color)) {
	  w->term.curs_char = cur_char;
	  return;
     }
#endif /* DOUBLE_FONTS */ 
     
     /* Normal display */
     if ((attrs & ATT_REVERSE) == (attrs & ATT_SELECTED)) {
	  if (((old_att & ATT_REVERSE) != (old_att & ATT_SELECTED))
	      || (color != old_color)) {
	       /* was the display inverted ? */
	       if (w->term.cursor_bg != 
		   w->term.cit[GET_COLOR_BG(color)].bg.pix) {
		    val.foreground = w->term.cursor_bg;
		    val.background = w->term.cursor_fg;
		    XChangeGC(XtDisplay(w), w->term.cursor_graph_gc,
			      GCForeground | GCBackground, &val);
		    val.foreground = w->term.cursor_fg;
		    val.background = w->term.cursor_bg;
		    XChangeGC(XtDisplay(w), w->term.cursor_text_gc,
			      GCForeground | GCBackground, &val);
	       }
	       else {
		    val.foreground = w->term.cursor_fg;
		    val.background = w->term.cursor_bg;
		    XChangeGC(XtDisplay(w), w->term.cursor_graph_gc,
			      GCForeground | GCBackground, &val);
		    val.foreground = w->term.cursor_bg;
		    val.background = w->term.cursor_fg;
		    XChangeGC(XtDisplay(w), w->term.cursor_text_gc,
			      GCForeground | GCBackground, &val);
	       }
	       val.foreground = w->term.cit[GET_COLOR_FG(color)].fg.pix;
	       val.background = w->term.cit[GET_COLOR_BG(color)].bg.pix;
	       XChangeGC(XtDisplay(w), w->term.cursor_rem_gc,
			 GCForeground | GCBackground, &val);
	  }
     }
     /* Inverted display */
     else {
	  /* was the display not inverted ? */
	  if (((old_att & ATT_REVERSE) == (old_att & ATT_SELECTED))
	      || (color != old_color)) {
	       if (w->term.cursor_bg == 
		   w->term.cit[GET_COLOR_FG(color)].fg.pix) {
		    val.foreground = w->term.cursor_fg;
		    val.background = w->term.cursor_bg;
		    XChangeGC(XtDisplay(w), w->term.cursor_graph_gc,
			      GCForeground | GCBackground, &val);
		    val.foreground = w->term.cursor_bg;
		    val.background = w->term.cursor_fg;
		    XChangeGC(XtDisplay(w), w->term.cursor_text_gc,
			      GCForeground | GCBackground, &val);
	       }
	       else {
		    val.foreground = w->term.cursor_bg;
		    val.background = w->term.cursor_fg;
		    XChangeGC(XtDisplay(w), w->term.cursor_graph_gc,
			      GCForeground | GCBackground, &val);
		    val.foreground = w->term.cursor_fg;
		    val.background = w->term.cursor_bg;
		    XChangeGC(XtDisplay(w), w->term.cursor_text_gc,
			      GCForeground | GCBackground, &val);
	       }
	       val.background = w->term.cit[GET_COLOR_FG(color)].fg.pix;
	       val.foreground = w->term.cit[GET_COLOR_BG(color)].bg.pix;
	       XChangeGC(XtDisplay(w), w->term.cursor_rem_gc,
			 GCForeground | GCBackground, &val);
	  }
     }
     
#ifdef DOUBLE_FONTS
     if (lflags) {
	  /* Double wide / or high */
	  if (lflags & LINE_D_WIDE) {
	       /* double width single height */
	       if (attrs & ATT_BOLD) {
		    /* double wide single height bold */
		    XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.d_wb_font->fid);
		    XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.d_wb_font->fid);
	       } else {
		    /* double wide single height normal */
		    XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.d_w_font->fid);
		    XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.d_w_font->fid);
	       }
	  } else {
	       /* double width double height */
	       if (attrs & ATT_BOLD) {
		    /* bold */
		    XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.d_hb_font->fid);
		    XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.d_hb_font->fid);
	       } else {
		    /* normal */
		    XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.d_h_font->fid);
		    XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.d_h_font->fid);
	       }
	  }
     } else {
	  if (lflags != old_lflags) {
	       if (attrs & ATT_BOLD) {
		    XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.bold_font->fid);
		    XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.bold_font->fid);
	       } else {
		    XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.normal_font->fid);
		    XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.normal_font->fid);
	       }
	  } else {
	       if (attrs & ATT_BOLD) {
		    if(!(old_att & ATT_BOLD)) {
			 XSetFont(XtDisplay(w), w->term.cursor_text_gc,
				  w->term.bold_font->fid);
			 XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
				  w->term.bold_font->fid);
		    }
	       }
	       else {
		    if(old_att & ATT_BOLD) {
			 XSetFont(XtDisplay(w), w->term.cursor_text_gc,
				  w->term.normal_font->fid);
			 XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
				  w->term.normal_font->fid);
		    }
	       }
	  }
     }

     w->term.curs_lflags = lflags;
#else
     if (attrs & ATT_BOLD) {
	  if(!(old_att & ATT_BOLD)) {
	       XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			w->term.bold_font->fid);
	       XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			w->term.bold_font->fid);
	  }
     }
     else {
	  if(old_att & ATT_BOLD) {
	       XSetFont(XtDisplay(w), w->term.cursor_text_gc,
			w->term.normal_font->fid);
	       XSetFont(XtDisplay(w), w->term.cursor_rem_gc,
			w->term.normal_font->fid);
	  }
     }
#endif /* DOUBLE_FONTS */ 
     
     w->term.curs_char = cur_char;

}

/*
 * Draw a string with the given attribs, using the given gc (validate it first
 * if necessary) at draw_x, draw_y.
 */
Export void
draw_string(TermCanvasWidget w, GC gc, unsigned char attr, int x, int y,
	    char *str, int len)
{
     XDrawImageString(XtDisplay(w), XtWindow(w), gc, x, y, str, len);
     
     /* underlining ? */
     if (attr & ATT_UNDERL) {
#ifdef DOUBLE_FONTS
	  if (w->term.act_lflags != 0)
	       XDrawLine(XtDisplay(w), XtWindow(w), gc, x,
			 y + w->term.ul_width,
			 x + w->term.cell_width * len * 2,
			 y + w->term.ul_width);
	  else
	       XDrawLine(XtDisplay(w), XtWindow(w), gc, x,
			 y + w->term.ul_width, x + w->term.cell_width * len,
			 y + w->term.ul_width);
#else
	  XDrawLine(XtDisplay(w), XtWindow(w), gc, x, y + w->term.ul_width,
		    x + w->term.cell_width * len, y + w->term.ul_width);
#endif /* DOUBLE_FONTS */ 
     }
}

/*
 * Shifts the given line to the right starting at the given column.
 * The resulting space isn't cleared !!
 */

Export void
shift_screen_line(TermCanvasWidget w, int line, int col, int cnt, int cols)
{
     XCopyArea(XtDisplay(w), XtWindow(w), XtWindow(w), w->term.gc,
	       col * w->term.cell_width, line * w->term.cell_height,
	       (cols - col - cnt) * w->term.cell_width, w->term.cell_height,
	       (col + cnt) * w->term.cell_width, line * w->term.cell_height);
}

/*
 * Routine to blink Text on the screen. It gets called via timer.
 * Turns itself off if there is no blinking text on the screen and the current
 * attribute is not blinking.
 */

/*ARGSUSED*/
Export void
blink_text(XtPointer client_data, XtIntervalId *id)
{
     TermCanvasWidget w = (TermCanvasWidget)client_data;
     int i, j, cnt, no;
     int draw_x, draw_y;
     char buffer[MAX_CHARS_IN_LINE];
     CharPtr c;
     unsigned char cur_attr = 0;
     unsigned char cur_color = 0;
     unsigned char save_attr, save_color;
     int fh = w->term.cell_height;
     int fw = w->term.cell_width;
     GC gc = w->term.norm_gc;
#ifdef DOUBLE_FONTS
     LineFlags lflags = w->term.line_flags;
     LineFlagsElem cur_lflags, save_lflags;
     int act_end = w->term.columns;
     
     /* save the current attributes and color */
     save_attr = w->term.act_attribs;
     save_lflags = w->term.act_lflags;
     save_color = w->term.act_color;
#else
#define act_end w->term.columns

     /* save the current attributes and color */
     save_attr = w->term.act_attribs;
     save_color = w->term.act_color;
#endif /* DOUBLE_FONTS */ 
     
     no = 0;

     for (i = 0; i < w->term.lines; i++) {
	  c = (w->term.char_array[i]);
#ifdef DOUBLE_FONTS
	  if ((lflags[i] !=  cur_lflags) || (lflags[i] != 0)) {
	       cur_lflags = lflags[i];
	       if (w->term.blink_text_on)
		    gc = validate_gc(w, cur_attr, cur_color, cur_lflags);
	       else
		    gc = validate_gc(w, cur_attr ^ ATT_REVERSE,
				     cur_color, cur_lflags);
	       if (cur_lflags != 0) {
		    fw = w->term.cell_width * 2;
		    act_end = w->term.columns / 2;
	       } else {
		    fw = w->term.cell_width;
		    act_end = w->term.columns;
	       }
	  }
	  draw_y = (i +1) * fh - w->term.font_descent + w->term.double_offset;
#else 
	  draw_y = (i +1) * fh - w->term.font_descent;
#endif /* DOUBLE_FONTS */
	  
	  for (j = 0; j < act_end; ) {
	       cnt = 0;
	       
	       /* skip over characters that aren't blinking */
	       for (; (j < w->term.columns) && ((c->value == 0)
		    || ((c->attributes & ATT_BLINK) == 0)) ; c++)
		    j++;
	       draw_x = j * fw;
	       
	       /* collect characters */
	       for (; (j < w->term.columns) && (c->value != 0)
		       && (c->attributes == cur_attr)
		       && (c->color == cur_color); c++) {
		    buffer[cnt++] = c->value;
		    no++;
		    j++;
	       }
	       
	       /* draw the string if any */
	       if (cnt != 0) {
		    buffer[cnt] = 0;
		    /* draw the string or remove it ?? */
		    if (w->term.blink_text_on)
			 draw_string(w, gc, cur_attr, draw_x, draw_y,
				     buffer, cnt);
		    else
			 XFillRectangle(XtDisplay(w), XtWindow(w),
					gc,
					draw_x, i * fh, cnt * fw, fh);
	       }
	       
	       /* do we have to change the current gc */
	       if ((j < w->term.columns) && (c->value != 0)) {
		    cur_attr = c->attributes;
		    cur_color = c->color;
#ifdef DOUBLE_FONTS
		    if (w->term.blink_text_on)
			 gc = validate_gc(w, cur_attr, cur_color, cur_lflags);
		    else
			 gc = validate_gc(w, cur_attr ^ ATT_REVERSE,
					  cur_color, cur_lflags);
#else
		    if (w->term.blink_text_on)
			 gc = validate_gc(w, cur_attr, cur_color);
		    else
			 gc = validate_gc(w, cur_attr ^ ATT_REVERSE,
					  cur_color);
#endif /* DOUBLE_FONTS */ 
	       }
	  }
     }

     /*
      * restore the current attributes
      */
#ifdef DOUBLE_FONTS
     (void) validate_gc(w, save_attr, save_color, save_lflags);
#else 
     (void) validate_gc(w, save_attr, save_color);
#endif /* DOUBLE_FONTS */
     
     /*
      * restart the timer if we've found any blinking characters or the
      * current attribute is set to blink
      */
     if ((no > 0) || ((w->term.act_attribs & ATT_BLINK) != 0))
	  w->term.blink_id = XtAddTimeOut(w->term.tblink_interval,
					  blink_text, w);
     else
	  w->term.blink_id = 0;
     
     /*
      * Flip the draw mode
      */
     w->term.blink_text_on = !w->term.blink_text_on;
}

/*
 * Scroll the Screen to start with the 'no'th Line of the save area from the
 * bottom.
 *
 * The normal area will be 'under' the saved.
 */
Export void
scroll_save_abs(TermCanvasWidget w, int no, int flag)
{
     scroll_save_rel(w, no - w->term.scroll_pos, flag);
}

/*
 * Scroll the Screen plus the save area the specified number of lines
 */
Export void
scroll_save_rel(TermCanvasWidget w, int no, int flag)
{
     int pos;

/*      printf("%d ", no); fflush(stdout); */
     
     /*
      * Check if we can really scroll that many lines
      */
     pos = w->term.scroll_pos + no;
     if (pos <= 0) {
	  no = - w->term.scroll_pos;
	  
#if 0
	  /*
	   * At this point, adjust the scrollbar no matter what, since
	   * the thumb starts leaving the bottom of the bar now.
	   */
	  if (w->term.adjust_scroll_bar != NULL)
	       w->term.adjust_scroll_bar(XtParent(w),
			(double)(w->term.save_size) /
			(double)(w->term.lines + w->term.save_size),
			(double)w->term.lines /
			(double)(w->term.lines + w->term.save_size));
#endif 
     }
	  
     if (pos > (int) w->term.save_size)
	  no = w->term.save_size - w->term.scroll_pos;

     if (no == 0)
	  return;
     
     /*
      * Unmark the selection to avoid trouble with the coordinates.
      */
     if (w->term.select_on)
	  unmark_selection(w);
     
     /* Set the new scroll position */
     w->term.scroll_pos += no;

     /* If we're on the bottom, we can switch the js_flag off */
     if (w->term.scroll_pos == 0)
	  w->term.js_flag = False;
     
     /*
      * Scrolling the screen only makes sense if we scroll less than
      * a screen full.
      */
     if (abs(no) < w->term.lines) {
	  scroll_screen(w, 0, w->term.lines, -no);

	  /* let redraw_rect do the new lines */
	  if (no > 0)
	       redraw_rect(w, w->term.char_array, 0, 0,
			   no - 1, w->term.columns - 1);
	  else
	       redraw_rect(w, w->term.char_array, w->term.lines + no, 0,
			   w->term.lines - 1, w->term.columns - 1);
     }
     /*
      * Just redraw the whole screen elsewhise
      */
     else {
	  XClearWindow(XtDisplay(w), XtWindow(w));
	  redraw_rect(w, w->term.char_array, 0, 0,
		      w->term.lines - 1, w->term.columns - 1);
     }
     
     /* Adjust the cursor position */
     if(w->term.array_cur.lin + w->term.scroll_pos >= w->term.lines)
	  w->term.screen_cur.y = -1;
     else {
	  int tmp = w->term.screen_cur.y;
	  
	  w->term.screen_cur.y = (w->term.array_cur.lin + w->term.scroll_pos
				  + 1) * w->term.cell_height;
	  
	  /* If the cursor reappears, put it back */
	  if ((tmp < 0) && (w->term.array_cur.lin + w->term.scroll_pos
			    < w->term.lines))
	       flip_cursor_on(w);
     }
     /*
      * Tell the Parent to adjust the scrollbar 
      */
     if ((flag != 0) && (w->term.adjust_scroll_bar != NULL))
	  w->term.adjust_scroll_bar(XtParent(w),
				 (double)(w->term.save_size - w->term.scroll_pos) /
				 (double)(w->term.lines + w->term.save_size),
				 (double)w->term.lines /
				 (double)(w->term.lines + w->term.save_size));
} 

/*
 * Set the current cursor positon to be lin and col.
 * Checks if the clip rectangle for the gc has to be changed and validates
 * the cursor gcs.
 */
Export void
set_cursor_pos(TermCanvasWidget w, int lin, int col)
{
     w->term.array_cur.lin = lin;
     w->term.array_cur.col = col;
     w->term.screen_cur.y = (lin + 1) * w->term.cell_height;
#ifdef DOUBLE_FONTS
     w->term.screen_cur.x = col * w->term.cell_width;
     if (w->term.act_lflags != 0) w->term.screen_cur.x *= 2;
     
     validate_cursor_gc(w, w->term.char_array[lin][col], w->term.act_lflags);
#else 
     validate_cursor_gc(w, w->term.char_array[lin][col]);
#endif /* DOUBLE_FONTS */
     
     if (w->term.no_block_cursor) {
	  XSetClipOrigin(XtDisplay(w), w->term.cursor_text_gc,
			 w->term.screen_cur.x,
			 w->term.screen_cur.y - w->term.cursor_height);
	  XSetClipOrigin(XtDisplay(w), w->term.cursor_rem_gc,
			 w->term.screen_cur.x,
			 w->term.screen_cur.y - w->term.cursor_height);
     }
#ifdef DOUBLE_FONTS
     else {
	  XSetClipOrigin(XtDisplay(w), w->term.cursor_text_gc,
			 w->term.screen_cur.x,
			 w->term.screen_cur.y - w->term.cell_height);
	  XSetClipOrigin(XtDisplay(w), w->term.cursor_rem_gc,
			 w->term.screen_cur.x,
			 w->term.screen_cur.y - w->term.cell_height);
     }
#endif /* DOUBLE_FONTS */
}

Export void
validate_cursor_size(TermCanvasWidget w)
{
     XRectangle rect;

     if ((w->term.cursor_width == 0) ||
	 (w->term.cursor_width > w->term.cell_width))
	  w->term.cursor_width = w->term.cell_width;
     if ((w->term.cursor_height == 0) ||
	 (w->term.cursor_height > w->term.cell_height))
	  w->term.cursor_height = w->term.cell_height;

     w->term.no_block_cursor = (w->term.cursor_height != w->term.cell_height)
	  || (w->term.cursor_width != w->term.cell_width);

     /* change the clip rectangle */
     if (w->term.no_block_cursor) {
	  rect.x = 0;
	  rect.y = 0;
	  rect.width = w->term.cursor_width;
	  rect.height = w->term.cell_height - w->term.cursor_height;
	  
	  XSetClipRectangles(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.screen_cur.x,
			     w->term.screen_cur.y - w->term.cursor_height,
			     &rect, 1, YXBanded);
#ifdef DOUBLE_FONTS
	  XSetClipRectangles(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.screen_cur.x,
			     w->term.screen_cur.y - w->term.cursor_height,
			     &rect, 1, YXBanded);
#endif
     } else {
#ifdef DOUBLE_FONTS
	  rect.x = 0;
	  rect.y = 0;
	  rect.width = w->term.cell_width;
	  rect.height = w->term.cell_height;
	  
	  XSetClipRectangles(XtDisplay(w), w->term.cursor_text_gc,
			     w->term.screen_cur.x,
			     w->term.screen_cur.y - w->term.cell_height,
			     &rect, 1, YXBanded);
	  XSetClipRectangles(XtDisplay(w), w->term.cursor_rem_gc,
			     w->term.screen_cur.x,
			     w->term.screen_cur.y - w->term.cursor_height,
			     &rect, 1, YXBanded);
#else 
	  XSetClipMask(XtDisplay(w), w->term.cursor_text_gc, None);
#endif /* DOUBLE_FONTS */
     }
}
