#ifndef XP_CANVAS_H
#define XP_CANVAS_H

/* canvas.h,v 1.2 1994/05/24 19:55:33 me Exp */

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
 * Declarations and Definitions local to the TermCanvas Widget
 *
 * Author: Michael W. Elbel
 * Date: April 24th, 1990.
 * Description: Siehe oben.
 *
 * Revision History:
 *
 * canvas.h,v
 * Revision 1.2  1994/05/24  19:55:33  me
 * New Copyright
 * fixed bug where the text cursor dissappeared upon a move_abs
 *
 * Revision 1.1.1.1  1994/05/22  11:22:40  me
 * Initial import into CVS
 */

#include "TermCanvasP.h"

/* The bits that result in a 'non-normal' GC */
#define FONT_ATTS	(ATT_BOLD | ATT_REVERSE | ATT_SELECTED)

/*
 * Macro to make checking the type of an argument in a comblock easier
 */
#define check_arg_type(block, reg, type, mesg, retcode)			\
{									\
     if (cb_reg_type(block, reg) != type) {				\
          warn("%s - Expected '%s' type in register '%c', got '%s'",	\
	       mesg, reg_type_name(type), reg,				\
	       reg_type_name(cb_reg_type(block, reg)));			\
	  return(retcode);						\
     }									\
}

/* Jump vector for the canvas_handler */
Import IntFuncPtr jumptable[];

/* The type of a function used by canvasDoMotion */
typedef int (*XpTermMotionFunc)(TermCanvasWidget, ScreenCursorPtr, int *);

/* Function declarations from TermCanvas.c */
Import int XpTermCanvasDispatch(Widget, ComBlockPtr, Boolean);
Import FontNameProperties *get_font_name_props(Display *, XFontStruct *);
Import char *bold_font_name(FontNameProperties *);
Import char *special_font_name(FontNameProperties *, unsigned char,
			       LineFlagsElem);
Import int font_lookup_string(unsigned char *, int, unsigned char []);

/* Function declarations from char_array.c */
Import CharArray create_char_array(int, int);
Import CharArray resize_char_array(TermCanvasWidget, CharArray, int, int,
				   int, int);
#ifdef DOUBLE_FONTS 
Import LineFlags create_line_flags(int);
Import LineFlags resize_line_flags(LineFlags, int, int);
#endif /* DOUBLE_FONTS */ 

#ifdef DOUBLE_FONTS
Import void save_array_lines(TermCanvasWidget, LinePtr, LineFlags, int);
#else
Import void save_array_lines(TermCanvasWidget, LinePtr, int);
#endif
Import void resize_save_area(TermCanvasWidget, int, int);
Import void scroll_array(TermCanvasWidget, int, int, int, Boolean);
Import void delete_array_chars(TermCanvasWidget, int, int, int, int);
Import void erase_array_chars(TermCanvasWidget, int, int, int, int);
Import void erase_array_lines(TermCanvasWidget, int, int);
Import TabList create_tab_list(int);
Import void clear_tab_list(TabList, int);
Import void init_fixed_tabs(TabList, int, int);
Import TabList resize_tab_list(TabList, int, int, int);
Import void free_char_array(CharArray, int);
Import void shift_array_line(TermCanvasWidget, Line, int, int, int);
Import void put_string_in_array(TermCanvasWidget, char *, char, int, int,
				unsigned char, unsigned char);

/* Function declarations from screen.c */
Import void flip_cursor_off(TermCanvasWidget);
Import void flip_cursor_on(TermCanvasWidget);
Import void switch_cursor_off(TermCanvasWidget);
Import void switch_cursor_on(TermCanvasWidget);
Import void blink_cursor(XtPointer, XtIntervalId *);
Import void scroll_screen(TermCanvasWidget, int, int, int);
Import void delete_screen_chars(TermCanvasWidget, int, int, int, int);
Import void erase_screen_chars(TermCanvasWidget, int, int, int, int);
Import void erase_screen_lines(TermCanvasWidget, int, int);
#ifdef DOUBLE_FONTS 
Import GC validate_gc(TermCanvasWidget, unsigned char, unsigned char,
		      LineFlagsElem);
#else
Import GC validate_gc(TermCanvasWidget, unsigned char, unsigned char);
#endif /* DOUBLE_FONTS */ 
#ifdef DOUBLE_FONTS
Import void validate_cursor_gc(TermCanvasWidget, Char, LineFlagsElem);
#else
Import void validate_cursor_gc(TermCanvasWidget, Char);
#endif /* DOUBLE_FONTS */ 
Import void draw_string(TermCanvasWidget, GC, unsigned char, int, int,
			char *, int);
Import void shift_screen_line(TermCanvasWidget, int, int, int, int);
Import void blink_text(XtPointer, XtIntervalId *);
Import void set_cursor_pos(TermCanvasWidget, int, int);
Import void validate_cursor_size(TermCanvasWidget);

/* Function declarations from selection.c */
Import void start_selection(Widget, XEvent *, String *, Cardinal *);
Import void extend_selection(Widget, XEvent *, String *, Cardinal *);
Import void end_selection(Widget, XEvent *, String *, Cardinal *);
Import void paste_selection(Widget, XEvent *, String *, Cardinal *);
Import void unmark_selection(TermCanvasWidget);
Import Boolean in_selected_area(TermCanvasWidget, int, int, int);
Import void scroll_selection(TermCanvasWidget, int, int, int);

/* Function declarations from motion.c */
Import ScreenCursor canvasDoMotion(TermCanvasWidget, String, XpTermMotionFunc,
				   ScreenCursor);
Import void canvasFreeMotion(TermCanvasWidget);

/* Functions from functions.c */
Import int move_cursor(TermCanvasWidget, int, int);
Import int move_cursor_rel(TermCanvasWidget, int, int);
Import int insert(TermCanvasWidget, ComBlockPtr);
Import int move_abs(TermCanvasWidget, ComBlockPtr);
Import int move_rel(TermCanvasWidget, ComBlockPtr);
Import int move_abs_column(TermCanvasWidget, ComBlockPtr);
Import int move_abs_row(TermCanvasWidget, ComBlockPtr);
Import int move_rel_column(TermCanvasWidget, ComBlockPtr);
Import int move_rel_row(TermCanvasWidget, ComBlockPtr);
Import int move_rel_row_scrolled(TermCanvasWidget, ComBlockPtr);
Import int insert_mode(TermCanvasWidget, ComBlockPtr);
Import int overwrite_mode(TermCanvasWidget, ComBlockPtr);
Import int delete_chars(TermCanvasWidget, ComBlockPtr);
Import int delete_to_eol(TermCanvasWidget, ComBlockPtr);
Import int delete_lines(TermCanvasWidget, ComBlockPtr);
Import int delete_to_eoscr(TermCanvasWidget, ComBlockPtr);
Import int erase_chars(TermCanvasWidget, ComBlockPtr);
Import int erase_line_left(TermCanvasWidget, ComBlockPtr);
Import int erase_lines(TermCanvasWidget, ComBlockPtr);
Import int erase_from_toscr(TermCanvasWidget, ComBlockPtr);
Import int clear_screen(TermCanvasWidget, ComBlockPtr);
Import int insert_lines(TermCanvasWidget, ComBlockPtr);
Import int set_scroll_region(TermCanvasWidget, ComBlockPtr);
Import int ring_bell(TermCanvasWidget, ComBlockPtr);
Import int hor_tab(TermCanvasWidget, ComBlockPtr);
Import int set_tab_cur_col(TermCanvasWidget, ComBlockPtr);
Import int set_tab_col(TermCanvasWidget, ComBlockPtr);
Import int set_tabs_eq_width(TermCanvasWidget, ComBlockPtr);
Import int clear_tab_cur_col(TermCanvasWidget, ComBlockPtr);
Import int clear_tab_col(TermCanvasWidget, ComBlockPtr);
Import int clear_all_tabs(TermCanvasWidget, ComBlockPtr);
Import int set_attribute(TermCanvasWidget, ComBlockPtr);
Import int clear_attribute(TermCanvasWidget, ComBlockPtr);
Import int override_translations(TermCanvasWidget, ComBlockPtr);
Import int change_flut(TermCanvasWidget, ComBlockPtr);
Import int canvas_size(TermCanvasWidget, ComBlockPtr);
Import int canvas_cursor_pos(TermCanvasWidget, ComBlockPtr);
Import int canvas_attribs(TermCanvasWidget, ComBlockPtr);
Import int canvas_scroll_region(TermCanvasWidget, ComBlockPtr);
Import int canvas_wrap_mode(TermCanvasWidget, ComBlockPtr);
Import int canvas_reverse_mode(TermCanvasWidget, ComBlockPtr);
Import int canvas_cursor_on(TermCanvasWidget, ComBlockPtr);
Import int canvas_cursor_blinking(TermCanvasWidget, ComBlockPtr);
Import int canvas_cursor_size(TermCanvasWidget, ComBlockPtr);
Import int redraw_screen(TermCanvasWidget, ComBlockPtr);
Import int change_fonts(TermCanvasWidget, ComBlockPtr);
Import int scroll_screen_absolute(TermCanvasWidget, ComBlockPtr);
Import int scroll_screen_relative(TermCanvasWidget, ComBlockPtr);
Import int cursor_off(TermCanvasWidget, ComBlockPtr);
Import int cursor_on(TermCanvasWidget, ComBlockPtr);
Import int set_screen_size(TermCanvasWidget, ComBlockPtr);
Import int wrap_around(TermCanvasWidget, ComBlockPtr);
Import int dont_wrap(TermCanvasWidget, ComBlockPtr);
Import int cursor_pos_rel_to_scr_reg(TermCanvasWidget, ComBlockPtr);
Import int cursor_pos_absolute(TermCanvasWidget, ComBlockPtr);
Import int reverse_video(TermCanvasWidget, ComBlockPtr);
Import int normal_video(TermCanvasWidget, ComBlockPtr);
Import int save_flut(TermCanvasWidget, ComBlockPtr);
Import int restore_flut(TermCanvasWidget, ComBlockPtr);
Import int set_cursor_blink(TermCanvasWidget, ComBlockPtr);
Import int set_cursor_size(TermCanvasWidget, ComBlockPtr);
Import int change_fg_color(TermCanvasWidget, ComBlockPtr);
Import int change_bg_color(TermCanvasWidget, ComBlockPtr);
Import int set_cit_cell(TermCanvasWidget, ComBlockPtr);
Import int canvas_display_cells(TermCanvasWidget, ComBlockPtr);
Import int set_jump_scroll(TermCanvasWidget, ComBlockPtr);
Import int get_jump_scroll(TermCanvasWidget, ComBlockPtr);
Import int set_lflags(TermCanvasWidget, ComBlockPtr);
Import int dummy(TermCanvasWidget, ComBlockPtr);

Import void parse_and_change_flut(String, unsigned char [], int);
Import void redraw_rect(TermCanvasWidget, CharArray, int, int, int, int);
Import int font_lookup_string(unsigned char *, int, unsigned char []);
Import void set_flut(unsigned char [], int, int, int);
Import void scroll_save_abs(TermCanvasWidget, int, int);
Import void scroll_save_rel(TermCanvasWidget, int, int);

#endif /* XP_CANVAS_H */
