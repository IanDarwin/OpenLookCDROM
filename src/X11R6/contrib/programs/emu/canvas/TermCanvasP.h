#ifndef XP_TERMCANVASP_H
#define XP_TERMCANVASP_H

/* TermCanvasP.h,v 1.4 1994/06/02 10:57:37 me Exp */

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
 * Private Header file for the TermCanvas widget
 *
 * Author: Michael Elbel
 * Date: March 20th, 1990.
 *
 * Revision History:
 * TermCanvasP.h,v
 * Revision 1.4  1994/06/02  10:57:37  me
 * Added Log line
 *
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 */

#include <X11/IntrinsicP.h>
#include <X11/CoreP.h>

#include "common.h"
#include "TermCanvas.h"

typedef struct {
     int nothing;
} TermCanvasClassPart;

typedef struct _TermCanvasClassRec {
     CoreClassPart		core_class;
     TermCanvasClassPart	term_canvas_class;
} TermCanvasClassRec;

/*
 * Don't change this structure without absolutely
 * knowing what you do. Right now it fits in 32
 * Bits. If you add only a single byte, your compiler
 * will most likely add another 32 Bits to its size.
 */
typedef struct _Char {
     unsigned short	value;
     unsigned char	attributes;
     unsigned char	color;
} Char, *CharPtr;

#define GET_COLOR_FG(col) ((int)(col & 0xf))
#define GET_COLOR_BG(col) ((int)(col >> 4))
#define SET_COLOR_FG(col, fg) ((col & 0xf0) | (unsigned char)(fg & 0xf))
#define SET_COLOR_BG(col, bg) ((col & 0x0f) | (unsigned char)(bg << 4))

typedef Char *Line, **LinePtr;

typedef Line *CharArray;

#ifdef DOUBLE_FONTS
typedef unsigned char LineFlagsElem, *LineFlags;
#endif

typedef Boolean TabListElem, *TabList;

typedef struct _ArrayCursor {
     int	lin;
     int	col;
} ArrayCursor, *ArrayCursorPtr;

typedef struct _ScreenCursor {
     int	x;
     int	y;
} ScreenCursor, *ScreenCursorPtr;

/*
 * I hardcode the number of Colors 'cuz it depends on
 * the structure of Char - you'd have to do major changes
 * in the CharArray structure if you wanted to increase
 * the number.
 */
typedef struct _ColorIndex {
     Boolean	set;
     Pixel	pix;
} ColorIndex;

typedef struct _ColorIndexTable {
     ColorIndex	fg;
     ColorIndex bg;
} ColorIndexTable[16];

/*
 * A structure to hold the relevant properties from a font
 * we need to make a well formed font name for it.
 */
typedef struct {
     /* registry, foundry, family */
     char *beginning;
     char *width;
     /* slant, width, add_style */
     char *middle;
     int pixel_size;
     char *point_size;
     int res_x;
     int res_y;
     char *spacing;
     /* charset registry, charset encoding */
     char *end;
} FontNameProperties;

typedef struct {
     /*
      * Public Resources
      */
     Dimension		lines;		/* Lines in current array	     */
     Dimension		columns;	/* Columns in array		     */
     Dimension		cell_width;	/* Width of a character cell	     */
     Dimension		cell_height;	/* Heigth of a character cell	     */
     String		normal_font_n;	/* Name of Font	for ordinary Text    */
     String		bold_font_n;	/* Name of Font	for bold Text	     */
     String		default_font_n;	/* Name of the default Font	     */
#ifdef DOUBLE_FONTS
     String		d_h_font_n;	/* Font	for double high-wide Text    */
     String		d_w_font_n;	/* Font	for double wide Text	     */
     String		d_hb_font_n;	/* Font	for bold d. high-wide Text   */
     String		d_wb_font_n;	/* Font	for bold double wide Text    */
#endif
     Dimension		ul_width;	/* Width of underline		     */
     Pixel		cursor_fg;	/* color of the cursor foreground    */
     Pixel		cursor_bg;	/* color of the cursor background    */
     Pixel		text_color;	/* color of the text		     */
     Pixel		background_color;/*backup of the background color    */
     Dimension		cursor_width;	/* Cursor width in pixels	     */
     Dimension		cursor_height;	/* Cursor height in pixels	     */
     Boolean		cursor_blinking;/* Does the cursor blink ?	     */
     int		blink_interval;	/* How fast does it blink?	     */
     Boolean		blink_wo_focus; /* blink cursor w/o input focus	     */
     int		tblink_interval;/* How fast does text blink?	     */
     Boolean		wrap_around;	/* Wrap at the end of the line?	     */
     Boolean		insert_mode;	/* Insert or overwrite characters    */
     Dimension		bell_volume;	/* How loud to toot the horn	     */
     Dimension		def_tab_width;	/* Default tab width		     */
     String		term_type;	/* The emulation's name		     */
     Boolean		selection_inv;	/* Mark selections by inverting	     */
     Boolean		reverse_mode;	/* Screen in reverse mode ?	     */
     int		save_lines;	/* How many lines to save	     */
     int		multi_click_time;/* Maximum time between two	     */
					/* clicks in milliseconds	     */
     Dimension		js_lines;	/* How many lines to jump scroll     */
     /* Communication Functions */
     VoidFuncPtr	output;		/* Who takes the output		     */
     VoidFuncPtr	set_size;	/* To set the size		     */
     VoidFuncPtr	notify_first_map;/*Whom to call on the first map     */
     ComBlockPtr	com_block;	/* Used to call 'output' with	     */
     Boolean		com_block_alloc;/* Have we allocated it ourselves?   */
     /*
      * Private Resources, to be kept by every widget instance
      */
     /* X specific stuff */
     /* Text output stuff */
     GC			norm_gc;	/* GC for ordinary text		     */
     GC			spec_gc;	/* GC for text with attributes	     */
     GC			gc;		/* GC we're currently writing with   */
     int		font_descent;	/* Descent below the baseline	     */
     FontNameProperties	*n_font_props;	/* Normal fonts properties	     */
     XFontStruct	*normal_font;	/* Font for ordinary text	     */
     XFontStruct	*bold_font;	/* Font for bold text		     */
#ifdef DOUBLE_FONTS
     XFontStruct	*d_h_font;	/* Font	for double high-wide Text    */
     XFontStruct	*d_w_font;	/* Font	for double wide Text	     */
     XFontStruct	*d_hb_font;	/* Font	for bold d. high-wide Text   */
     XFontStruct	*d_wb_font;	/* Font	for bold double wide Text    */
     int		double_offset;	/* Y-offset while drawing double Text*/
#endif
     /* other X stuff */
     GC			cursor_graph_gc;/* GC for graphic cursor drawing     */
     GC			cursor_text_gc;	/* GC for textual cursor drawing     */
     GC			cursor_rem_gc;  /* GC for textual cursor removing    */
     GC			clear_gc;	/* GC for clearing the screen	     */
     XtIntervalId	timeout_id;	/* Timer ID for cursor blinking	     */
     XtIntervalId	blink_id;	/* Timer ID for text blinking	     */
     Boolean		focused;	/* True if widget has the focus	     */
     Boolean		mapped_yet;	/* Have we ever been mapped ?	     */
     Cursor		pointer;	/* The X pointer to use		     */
     String		pointer_color;	/* The pointer's foreground color    */
     XColor		pointer_fg;	/* The actual X color		     */
     XColor		window_fg;	/* The actual X color		     */
     XColor		window_bg;	/* The actual X color		     */
     int		color_cells;	/* The number of colors on the screen*/
     Boolean		pt_fg_eq_wd_fg; /* Is the pointer foreground the same*/
     					/* color as the window foreground?   */
     ColorIndexTable	cit;		/* The Color Index Table	     */
     /* Selection stuff */
     Position		sel_start_x;	/* Start of the selection X	     */
     Position		sel_start_y;	/* Start of the selection y	     */
     Position		sel_end_x;	/* End of the selection X	     */
     Position		sel_end_y;	/* End of the selection y	     */
     Position		sel_first_x;	/* first X point		     */
     Position		sel_first_y;	/* first Y point		     */
     Atom	        *sel_atoms;	/* list of selections we own	     */
     Cardinal		n_sel_atoms;	/* how many selections we own	     */
     Cardinal		n_sel_size;	/* how much room for selections	     */
     Boolean		select_on;	/* Have we marked the selection ?    */
     String		selection_text; /* The text the selection contains   */
     Dimension		selection_len;	/* The length of the selected text   */
     /* fixed  Layer specific */
     unsigned char	flut[256];	/* Font lookup table 		     */
     unsigned char	save_flut[256];	/* Saved font lookup table 	     */
     CharArray		char_array;	/* Points to the current array	     */
     CharArray		save_array;	/* Points to the save array	     */
#ifdef DOUBLE_FONTS
     LineFlags		line_flags;	/* Points to the line flags	     */
     LineFlags		s_line_flags;	/* Points to the saved line flags    */
     LineFlagsElem	act_lflags;	/* The line flags we're using	     */
     LineFlagsElem	old_lflags;	/* The last line flags 		     */
     LineFlagsElem	curs_lflags;	/* The LineFlags under the cursor    */
#endif /* DOUBLE_FONTS */ 
     Dimension		save_size;	/* How many lines in save array	     */
     Dimension		scroll_pos;	/* How much we are scrolled back     */
     Boolean		js_flag;	/* Are we jump-scrolled ?	     */
     ScrollFuncPtr	adjust_scroll_bar;/* To adjust the scroll bar	     */
     TabList		tabs;		/* Points to the current tab list    */
     ArrayCursor	array_cur;	/* Cursor coords in the array	     */
     ScreenCursor	screen_cur;	/* Ditto in the screen		     */
     Boolean		rel_cursor_pos; /* Cursor positioning relative	     */
     Boolean		cursor_visible;	/* Is the cursor on the screen?	     */
     Boolean		cursor_on;	/* Has the cursor been switched on?  */
     unsigned char	act_attribs;	/* Attributes we are writing with    */
     unsigned char	old_attribs;	/* The last special attributes	     */
     unsigned char	act_color;	/* The Colors we're using	     */
     unsigned char	old_color;	/* The last Colors		     */
     Char 		curs_char;	/* Attributes under the cursor	     */
     Boolean		blink_text_on;	/* Are blinking chars visible ?	     */
     Boolean		outline_cursor;	/* Just draw a outlined cursor	     */
     Boolean		no_block_cursor;/* The cursor is smaller than the
					 * character cell		     */
     Dimension		scroll_top;	/* Scrolling region top		     */
     Dimension		scroll_bottom;	/* Scrolling region bottom	     */
     Generic		motionHead;	/* motion description list	     */
} TermCanvasPart;

typedef struct _TermCanvasRec {
     CorePart		core;
     TermCanvasPart	term;
} TermCanvasRec;

extern TermCanvasClassRec termCanvasClassRec;

#endif /* XP_TERMCANVASP_H */
