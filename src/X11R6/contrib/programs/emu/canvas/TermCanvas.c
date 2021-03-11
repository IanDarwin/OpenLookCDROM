#if !defined(lint) && !defined(__clipper__)
static char *rcsid = "TermCanvas.c,v 1.3 1994/05/26 21:01:04 me Exp";
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
 * Routines implementing the TermCanvas Widget used in emu
 *
 * Author: Michael Elbel
 * Date: March 20th, 1990.
 * Description: Public and private routines for the TermCanvas Widget
 *
 * Revision History:
 *
 * TermCanvas.c,v
 * Revision 1.3  1994/05/26  21:01:04  me
 * New copyright
 *
 * Revision 1.2  1994/05/24  19:55:21  me
 * New Copyright
 * fixed bug where the text cursor dissappeared upon a move_abs
 *
 * Revision 1.1.1.1  1994/05/22  11:22:41  me
 * Initial import into CVS
 *
 * Revision 1.24  93/08/18  15:06:04  me
 * Partly added Double Height/Width Stuff
 * corrected the text cursor behaviour when losing/getting focus and 
 * switched off cursor.
 * 
 * Revision 1.23  92/10/16  15:55:18  me
 * More fixes by Steve Crooks
 * 
 * [ ... ]
 *
 * Revision 1.1  90/04/12  14:26:11  me
 * Initial revision
 */

#include "canvas.h"
/* #include <X11/Quarks.h> */

#define offset(field) XtOffset(TermCanvasWidget, term.field)

/*
 * I'm using "black" and "white instead of XtDefaultForeground and
 * XtDefaultBackground because I have no way to detect where the color
 * comes from in the widget and the toolkit automatically switches
 * XtdefaultForeground and XtDefaultBackground if *reverseVideo is on
 */

Local XtResource resources[] = {
     { XtNforeground,	XtCForeground,	XtRPixel,	sizeof(Pixel),
	    offset(text_color),		XtRString,	"XtDefaultForeground"},
     { XtNbackground,	XtCBackground,	XtRPixel,	sizeof(Pixel),
	    offset(background_color),	XtRString,	"XtDefaultBackground"},
     { XpNcursorFg,	XtCForeground,	XtRPixel,	sizeof(Pixel),
	    offset(cursor_fg),		XtRString,	"XtDefaultForeground"},
     { XpNcursorBg,	XtCBackground,	XtRPixel,	sizeof(Pixel),
	    offset(cursor_bg),		XtRString,	"XtDefaultBackground"},
     { XtNreverseVideo,	XtCReverseVideo,XtRBoolean,	sizeof(Boolean),
	    offset(reverse_mode),	XtRString,	"False"		},
     { XtNfont,		XtCFont,	XtRString,	sizeof(String),
	    offset(normal_font_n),	XtRString,	XtDefaultFont	},
     { XpNboldFont,	XpCBoldFont,	XtRString,	sizeof(String),
	    offset(bold_font_n),	XtRString,	NULL	},
#ifdef DOUBLE_FONTS
     { XpNdWideFont,	XpCDWideFont,	XtRString,	sizeof(String),
	    offset(d_h_font_n),		XtRString,	NULL	},
     { XpNdWHighFont,	XpCDWHighFont,	XtRString,	sizeof(String),
	    offset(d_w_font_n),		XtRString,	NULL	},
     { XpNdWideBFont,	XpCDWideBFont,	XtRString,	sizeof(String),
	    offset(d_hb_font_n),	XtRString,	NULL	},
     { XpNdWHighBFont,	XpCDWHighBFont,	XtRString,	sizeof(String),
	    offset(d_wb_font_n),	XtRString,	NULL	},
#endif 
     { "defaultFont",	"DefaultFont",	XtRString,	sizeof(String),
	    offset(default_font_n),	XtRString,	"fixed"		},
     { XpNunderlineWidth,XpCUnderlineWidth,XtRDimension,sizeof(Dimension),
	    offset(ul_width),		XtRString,	"1"		},
     { XpNlines,	XpCLines,	XtRInt,		sizeof(int),
	    offset(lines),		XtRString,	"24"		},
     { XpNcolumns,	XpCColumns,	XtRDimension,	sizeof(Dimension),
	    offset(columns),		XtRString,	"80"		},
     { XpNcellWidth,	XpCCellWidth,	XtRDimension,	sizeof(Dimension),
	    offset(cell_width),		XtRString,	"0"		},
     { XpNcellHeight,	XpCCellHeight,	XtRDimension,	sizeof(Dimension),
	    offset(cell_height),	XtRString,	"0"		},
     { XpNcursorHeight,	XpCCursorHeight,XtRDimension,	sizeof(Dimension),
	    offset(cursor_height),	XtRString,	"0"		},
     { XpNcursorWidth,	XpCCursorWidth,	XtRDimension,	sizeof(Dimension),
	    offset(cursor_width),	XtRString,	"0"		},
     { XpNcursorBlinking, XpCCursorBlinking,XtRBoolean,	sizeof(Boolean),
#ifdef __XTERM_LIKE__ /* mimic xterm */
	    offset(cursor_blinking),	XtRString,	"False"		},
#else
	    offset(cursor_blinking),	XtRString,	"True"		},
#endif
     { XpNblinkInterval,XpCBlinkInterval,XtRInt,	sizeof(int),
	    offset(blink_interval), 	XtRString,	"500"		},
     { XpNblinkWOFocus,	XpCBlinkWOFocus,XtRBoolean,	sizeof(Boolean),
	    offset(blink_wo_focus), 	XtRString,	"False"		},
     { XpNtextBlinkInterval,XpCTextBlinkInterval,XtRInt,sizeof(int),
	    offset(tblink_interval),	XtRString, 	"500"		},
     { XpNwrapAround,	XpCWrapAround,	XtRBoolean,	sizeof(Boolean),
	    offset(wrap_around),	XtRString,	"True"		},
     { XpNinsertMode,	XpCInsertMode,	XtRBoolean,	sizeof(Boolean),
	    offset(insert_mode),	XtRString,	"False"		},
     { XpNbellVolume,	XpCBellVolume,	XtRDimension,	sizeof(Dimension),
	    offset(bell_volume), 	XtRString,	"0"		},
     { XpNdefTabWidth,	XpCDefTabWidth,	XtRDimension,	sizeof(Dimension),
	    offset(def_tab_width), 	XtRString,	"8"		},
     { XpNtermType,	XpCTermType,	XtRString,	sizeof(String),
	    offset(term_type),	XtRImmediate,		NULL		},
     { XpNsetSize,	XpCSetSize,	XtRFunction,	sizeof(VoidFuncPtr),
	    offset(set_size),		XtRImmediate,	NULL		},
     { XpNoutput,	XpCOutput,	XtRFunction,	sizeof(VoidFuncPtr),
	    offset(output),		XtRImmediate,	NULL		},
     { XpNnotifyFirstMap,XpCNotifyFirstMap,XtRFunction,	sizeof(VoidFuncPtr),
	    offset(notify_first_map),	XtRImmediate,	NULL		},
     { XpNcomBlock,	XpCComBlock,	XtRPointer,	sizeof(ComBlockPtr),
	    offset(com_block),		XtRImmediate,	NULL		},
     { XpNselectionInverse,XpCSelectionInverse,XtRBoolean,sizeof(Boolean),
	    offset(selection_inv), 	XtRString,	"False"		},
     { XpNpointerShape,	XpCPointerShape,XtRCursor,	sizeof(Cursor),
	    offset(pointer),		XtRString,	"xterm"		},
     { XpNpointerColor, XtCForeground,  XtRString,	sizeof(String),
	    offset(pointer_color),	XtRString,	NULL		},
     { XpNsaveLines,	XpCSaveLines,	XtRInt,		sizeof(int),
	    offset(save_lines),		XtRString,	"-1"		},
     { XpNadjustScrollBar,XpCAdjustScrollBar,XtRFunction,sizeof(VoidFuncPtr),
	    offset(adjust_scroll_bar),	XtRImmediate,	NULL		},
     { XpNmultiClickTime,XpCMultiClickTime,XtRInt,	sizeof(int),
	    offset(multi_click_time),	XtRString,	"300"		},
     { XpNjumpScrollLines,XpCJumpScrollLines,XtRDimension,sizeof(Dimension),
	    offset(js_lines),		XtRString,	"10"		},
};
#undef offset

Local void focus_in_handler(Widget, XEvent *, String *, Cardinal *);
Local void focus_out_handler(Widget, XEvent *, String *, Cardinal *);
Local void key_input_handler(Widget, XEvent *, String *, Cardinal *);
Local void call_parser(Widget, XEvent *, String *, Cardinal *);
Local void call_canvas(Widget, XEvent *, String *, Cardinal *);


Local XtActionsRec actions[] =
{
     { "focus-in",		focus_in_handler	},
     { "focus-out",		focus_out_handler	},
     { "key-input",		key_input_handler	},
     { "select-start",		start_selection		},
     { "select-extend",		extend_selection	},
     { "select-end",		end_selection		},
     { "insert-selection",	paste_selection		},
     { "call-parser",		call_parser		},
     { "call-canvas",		call_canvas		},
     { NULL,			NULL			}
};

Local char translations[] = "\
<FocusIn>:		focus-in()\n\
<FocusOut>:		focus-out()\n\
<Btn1Down>:		select-start()\n\
<Btn1Motion>:		select-extend()\n\
<Btn1Up>:		select-end(PRIMARY, CUT_BUFFER0)\n\
<Btn3Down>:		select-extend()\n\
<Btn3Motion>:		select-extend()\n\
<Btn3Up>:		select-end(PRIMARY, CUT_BUFFER0)\n\
<Btn2Up>:		insert-selection(PRIMARY, CUT_BUFFER0)\n\
Shift <Key>Prior:	call-canvas(46, \"\", a, 200, b, 1)\n\
Shift <Key>Next:	call-canvas(46, \"\", a, -200, b, 1)\n\
Shift <Key>Up:		call-canvas(46, \"\", a, 1, b, 1)\n\
Shift <Key>Down:	call-canvas(46, \"\", a, -1, b, 1)\n\
<Key>:			key-input()\n\
";

/* 46 is OP_SCROLL_SCREEN_RELATIVE */

Local void Initialize(Widget, Widget, ArgList, Cardinal *);
Local void Realize(Widget, Mask *, XSetWindowAttributes *);
Local void Canvas_Expose(Widget, XEvent *, Region);
Local void Resize(Widget);
Local void Destroy(Widget);

Export TermCanvasClassRec termCanvasClassRec = {
  { /* core fields */
    /* superclass		*/	(WidgetClass) &coreClassRec,
    /* class_name		*/	"TermCanvas",
    /* widget_size		*/	sizeof(TermCanvasRec),
    /* class_initialize		*/	NULL,
    /* class_part_initialize	*/	NULL,
    /* class_inited		*/	FALSE,
    /* initialize		*/	Initialize,
    /* initialize_hook		*/	NULL,
    /* realize			*/	Realize,
    /* actions			*/	actions,
    /* num_actions		*/	XtNumber(actions),
    /* resources		*/	resources,
    /* num_resources		*/	XtNumber(resources),
    /* xrm_class		*/	NULLQUARK,
    /* compress_motion		*/	TRUE,
    /* compress_exposure	*/	XtExposeCompressMaximal |
	                                XtExposeGraphicsExposeMerged,
    /* compress_enterleave	*/	TRUE,
    /* visible_interest		*/	FALSE,
    /* destroy			*/	Destroy,
    /* resize			*/	Resize,
    /* expose			*/	Canvas_Expose,
    /* set_values		*/	NULL,
    /* set_values_hook		*/	NULL,
    /* set_values_almost	*/	XtInheritSetValuesAlmost,
    /* get_values_hook		*/	NULL,
    /* accept_focus		*/	NULL,
    /* version			*/	XtVersion,
    /* callback_private		*/	NULL,
    /* tm_table			*/	translations,
    /* query_geometry		*/	XtInheritQueryGeometry,
    /* display_accelerator	*/	XtInheritDisplayAccelerator,
    /* extension		*/	NULL
  },
  { /* TermCanvas fields */
    /* empty			*/	0
  }
};

Export WidgetClass termCanvasWidgetClass = (WidgetClass)&termCanvasClassRec;

/*
 * Initialize Routine, gets called upon Creation of the Widget
 */
/*ARGSUSED*/
Local void
Initialize(Widget request, Widget new, ArgList av, Cardinal *ac)
{
     TermCanvasWidget w = (TermCanvasWidget)new;
     XFontStruct *fn;
     XFontStruct *fb;

     /* Initialize the Char Array */
     w->term.char_array = create_char_array(w->term.lines, w->term.columns);
     w->term.array_cur.lin = w->term.array_cur.col = 0;

     /* Initialize the Save Array */
     /* Default size ?? */
     if (w->term.save_lines == -1)
	  w->term.save_lines = 64 > w->term.lines ? 64 : w->term.lines;
     w->term.save_array = (CharArray)XtMalloc(w->term.save_lines *
					      sizeof(Line));
     bzero(w->term.save_array, w->term.save_lines * sizeof(Line));
     
     w->term.save_size = 0;

#ifdef DOUBLE_FONTS
     /* Initialize the LineFlags */
     w->term.line_flags = create_line_flags(w->term.lines);
     clear_line_flags(w->term.line_flags, w->term.lines);
     w->term.s_line_flags = create_line_flags(w->term.save_lines);
     clear_line_flags(w->term.s_line_flags, w->term.lines);
#endif /* DOUBLE_FONTS */ 

     /* Initialize the scroll position and length */
     w->term.scroll_pos = 0;
     w->term.js_flag = False;
     
     /* Initialize the Tab List */
     w->term.tabs = create_tab_list(w->term.columns);
     clear_tab_list(w->term.tabs, w->term.columns);
     init_fixed_tabs(w->term.tabs, w->term.columns, w->term.def_tab_width);

     /* Initialize the flut and saved flut */
     set_flut(w->term.flut, 0, 0, 256);
     set_flut(w->term.save_flut, 0, 0, 256);

     /*
      * Initialize the Fonts.
      *
      * I could have used converters, but since the Toolkit
      * Documentation doesn't specify what happens with the
      * allocated FontStructs, I can't rely on the current
      * behaviour.
      * So i'm partly reproducing functionality from 'CvtStringToFontStruct'
      * in lib/Xt/Converters.c.
      */
     
     /* Check for XtDefaultFont */
     if (strcmp(w->term.normal_font_n, XtDefaultFont) == 0) {
	  w->term.normal_font_n = w->term.default_font_n;
     }
     
     /* Now load them */
     if ((fn = XLoadQueryFont(XtDisplay(w), w->term.normal_font_n)) == NULL)
	  fatal("Couldn't even load the normal Font '%s'",
		w->term.normal_font_n);

     /*
      * Get the normal Font's name properties. From this we can try to
      * derive bold and double sized fonts.
      */
     if ((w->term.n_font_props = get_font_name_props(XtDisplay(w), fn))
	 == NULL)
	  warn("Couldn't get the normal font's font name properties");
     
     /*
      * We put some brains into loading the bold and double sized Fonts. If
      * they aren't specified, we try to make font names from the normal
      * font's name properties.
      */
     if ((w->term.bold_font_n == NULL) && (w->term.n_font_props != NULL))
	  w->term.bold_font_n = bold_font_name(w->term.n_font_props);

#ifdef DOUBLE_FONTS
     if ((w->term.d_w_font_n == NULL) && (w->term.n_font_props != NULL))
	  w->term.d_w_font_n = special_font_name(w->term.n_font_props,
						  0, LINE_D_WIDE);
     
     if ((w->term.d_h_font_n == NULL) && (w->term.n_font_props != NULL))
	  w->term.d_h_font_n = special_font_name(w->term.n_font_props,
						  0, LINE_D_UPPER);
     
     if ((w->term.d_wb_font_n == NULL) && (w->term.n_font_props != NULL))
	  w->term.d_wb_font_n = special_font_name(w->term.n_font_props,
						  ATT_BOLD, LINE_D_WIDE);

     if ((w->term.d_hb_font_n == NULL) && (w->term.n_font_props != NULL))
	  w->term.d_hb_font_n = special_font_name(w->term.n_font_props,
						  ATT_BOLD, LINE_D_UPPER);
     /*
      * We don't load the double sized fonts upon startup
      * in order not to waste memory
      */
     w->term.d_h_font = NULL;
     w->term.d_w_font = NULL;
     w->term.d_hb_font = NULL;
     w->term.d_wb_font = NULL;

#endif /* DOUBLE_FONTS */ 
     
     /*
      * Load the bold font
      */
     if ((fb = XLoadQueryFont(XtDisplay(w), w->term.bold_font_n)) == NULL) {
	  warn("Couldn't load the bold Font, using the normal one");
	  fb = fn;
     }
     
     w->term.normal_font = fn;
     w->term.bold_font = fb;
     
     if (fn->max_bounds.width != fn->min_bounds.width)
	  warn("Normal font is not of fixed width!");
     
     w->term.cell_width = fn->max_bounds.width;
     w->term.cell_height = fn->ascent + fn->descent;
     w->term.font_descent = fn->descent;
     
     if (fb->max_bounds.width != fb->min_bounds.width)
	  warn("Bold font is not of fixed width!");

     if (fn->max_bounds.width != fb->max_bounds.width)
	  warn("Normal and bold fonts are of different width (%d vs. %d)",
	       fn->max_bounds.width, fb->max_bounds.width);
     if ((fn->ascent + fn->descent) != (fb->ascent + fb->descent))
	  warn("Normal and bold fonts are of different height (%d vs. %d)",
	       fn->ascent + fn->descent, fb->ascent + fb->descent);
     if (fn->descent != fb->descent)
	  warn("Normal and bold fonts have different descent (%d vs. %d)",
	       fn->descent, fb->descent);

     /* Initialize the Cursor Variables */
     w->term.screen_cur.x = 0;
     w->term.screen_cur.y = w->term.cell_height;
     
     /* set up the scrolling region */
     w->term.scroll_top = 0;
     if (w->term.lines > 0) {
	  w->term.scroll_bottom = w->term.lines - 1;
     }
     else {
	  w->term.scroll_bottom = 0;
     }

     /* cursor positioning abolute */
     w->term.rel_cursor_pos = False;

     /* we don't own any selections yet */
     w->term.n_sel_atoms = w->term.n_sel_size = 0;
     w->term.sel_atoms = NULL;
     
     w->term.select_on = False;
     w->term.select_on = False;
     w->term.sel_start_x = w->term.sel_start_y = 0;
     w->term.sel_first_x = w->term.sel_first_y = 0;
     w->term.sel_end_x = w->term.sel_end_y = 0;

     /* we haven't been mapped yet */
     w->term.mapped_yet = False;

     /* Check if we have no size yet */
     if (w->core.width == 0)
	  w->core.width = w->term.columns * w->term.cell_width;
     if (w->core.height == 0)
	  w->core.height = w->term.lines * w->term.cell_height;

     /* Initialize the text blink TimeoutId */
     w->term.blink_id = 0;

     /* Initialize the selection text */
     w->term.selection_text = NULL;
     
     /* Initialize the motion head */
     w->term.motionHead = NULL;

     /* Initialize the current character under the cursor */
     w->term.curs_char.value = 0;
}

/*
 * Realize Routine, gets called when the widget is realized
 */
Local void
Realize(Widget widget, Mask *value_mask, XSetWindowAttributes *attributes)
{
     void blink_cursor();
     
     TermCanvasWidget w = (TermCanvasWidget)widget;
     XGCValues val;

     Arg args[5];
     int n;
     int i;
     
     Pixel fg, bg, cfg, cbg;
     
     /* Set up the mouse pointer */
     attributes->cursor = w->term.pointer;
     *value_mask |= CWCursor;
     
     /*
      * This didn't work, do it directly.
      *
      * w->term.color_cells = DisplayCells(XtDisplay(w), XtScreen(w));
      */
     w->term.color_cells = XtScreen(w)->root_visual->map_entries;
     
     /* Initialize the colors */
     w->term.window_fg.pixel = w->term.text_color;
     XQueryColor(XtDisplay(w), w->core.colormap, &(w->term.window_fg));
     w->term.window_bg.pixel = w->term.background_color;
     XQueryColor(XtDisplay(w), w->core.colormap, &(w->term.window_bg));
     
     /*
      * If we're on a black and white screen, it makes no sense to
      * set the pointer color, just ignore it.
      */
     if (w->term.color_cells > 2)  {
	  /* The default is the window_fg */
	  if (w->term.pointer_color != NULL) {
	       if (XParseColor(XtDisplay(w), w->core.colormap,
			       w->term.pointer_color, &(w->term.pointer_fg))
		   != 1) {
		    warn("Can't find pointer color \"%s\", using text color",
			 w->term.pointer_color);
		    w->term.pointer_fg = w->term.window_fg;
	       }
	  } else {
	       w->term.pointer_fg = w->term.window_fg;
	  }
	  
	  /*
	   * If the pointer Foreground is the same as the window
	   * foreground we have to be careful when switching to
	   * reverse video (Or get an invisible pointer) on color
	   * displays.
	   */
	  w->term.pt_fg_eq_wd_fg =
	       (w->term.window_fg.red == w->term.pointer_fg.red) &&
	       (w->term.window_fg.green == w->term.pointer_fg.green) &&
	       (w->term.window_fg.blue == w->term.pointer_fg.blue);
     }
     else {
	  w->term.pointer_fg = w->term.window_fg;
     }

 
     if (w->term.reverse_mode) {
	  fg = w->term.background_color;
	  bg = w->term.text_color;
	  w->term.background_color = bg;
	  w->term.text_color = fg;
	  
	  /* Set the mouse cursor */
	  if ((w->term.color_cells > 2) && !w->term.pt_fg_eq_wd_fg) {
	       XRecolorCursor(XtDisplay(w), w->term.pointer,
			      &(w->term.pointer_fg), &(w->term.window_fg));
	  }
	  else {
	       XRecolorCursor(XtDisplay(w), w->term.pointer,
			      &(w->term.window_bg), &(w->term.window_fg));
	  }
     }
     else {
	  fg = w->term.text_color;
	  bg = w->term.background_color;
	  
	  /* Change the cursor */
	  XRecolorCursor(XtDisplay(w), w->term.pointer,
			 &(w->term.pointer_fg), &(w->term.window_bg));
     }
     
     /*
      * Set the background pixel, so XtCreateWindow uses the right
      * color.
      */
     n = 0;
     XtSetArg(args[n], XtNbackground, bg);
     n++;
     XtSetValues(widget, args, n);

     /*
      * The text cursor
      *
      * If we're on a black and white display default to foreground
      * and background colors respectively.
      */
     if (w->term.color_cells > 2) {
	  if (w->term.cursor_fg == bg) {
	       cfg = w->term.cursor_bg;
	       cbg = w->term.cursor_fg;
	  }
	  else {
	       cfg = w->term.cursor_fg;
	       cbg = w->term.cursor_bg;
	  }
     }
     else {
	  w->term.cursor_fg = cfg = fg;
	  w->term.cursor_bg = cbg = bg;
     }
     
     /*
      * Initialize the cit,
      */ 
     for (i = 1; i < 16; i++) {
	  w->term.cit[i].fg.set = False;
	  w->term.cit[i].bg.set = False;
     }
     
     /*
      * Set up entry 0 of the to be the foreground and background
      * so emu will work without somebody explicitly setting any
      * colors
      */
     w->term.cit[0].fg.pix = fg;
     w->term.cit[0].bg.pix = bg;
     w->term.cit[0].fg.set = True;
     w->term.cit[0].bg.set = True;
     
     XtCreateWindow((Widget)w, (unsigned int)InputOutput,
		    (Visual *)CopyFromParent, *value_mask, attributes);

     /* create the text GCs */
     /* first normal Text */
     val.foreground = fg;
     val.background = bg;
     
     val.line_width = w->term.ul_width;
     val.function = GXcopy;
     val.font = w->term.normal_font->fid;
     
     w->term.norm_gc = XCreateGC(XtDisplay(w), XtWindow(w),
				 GCFunction | GCForeground | GCBackground
				 | GCFont | GCLineWidth,
				 &val);
     
     /* then special text, let's initialize it to bold */
     val.font = w->term.bold_font->fid;
     
     w->term.spec_gc = XCreateGC(XtDisplay(w), XtWindow(w),
				 GCFunction | GCForeground | GCBackground
				 | GCFont | GCLineWidth,
		&val);
     
     /* set the old_attribs accordingly */
     w->term.old_attribs = ATT_BOLD;
     
     /* clear the current and cursor attributes */
     w->term.act_attribs = w->term.curs_char.attributes = 0;
     w->term.gc = w->term.norm_gc;

     /* clear current and saved color */
     w->term.act_color = 0;
     w->term.old_color = 0;

#ifdef DOUBLE_FONTS
     /* clear current and saved line flags */
     w->term.act_lflags = 0;
     w->term.old_lflags = 0;
     w->term.double_offset = 0;
#endif /* DOUBLE_FONTS */ 
     
     /* create the clearing GC */
     val.foreground = bg;
	  
     w->term.clear_gc = XCreateGC(XtDisplay(w), XtWindow(w),
				  GCFunction|GCForeground|GCBackground,
				  &val);
	  
     /* create Cursor GCs */
     val.font = w->term.normal_font->fid;
     
     val.foreground = cfg;
     val.background = cbg;
     w->term.cursor_graph_gc = XCreateGC(XtDisplay(w), XtWindow(w),
				 GCFunction|GCForeground|GCBackground|GCFont,
					 &val);
     val.foreground = cbg;
     val.background = cfg;
     w->term.cursor_text_gc = XCreateGC(XtDisplay(w), XtWindow(w),
				GCFunction|GCForeground|GCBackground|GCFont,
					&val);
     
     val.foreground = fg;
     val.background = bg;
     w->term.cursor_rem_gc = XCreateGC(XtDisplay(w), XtWindow(w),
				GCFunction|GCForeground|GCBackground|GCFont,
				       &val);

     validate_cursor_size(w);

     /* have the cursor be drawn with the first expose event */
     w->term.cursor_visible = True;
     w->term.cursor_on = False;

     /* start the blinking cursor */
     if (w->term.cursor_blinking)
	  w->term.timeout_id = XtAddTimeOut(w->term.blink_interval,
					    blink_cursor, w);
     
     /* blinking text should be visible */
     w->term.blink_text_on = True;

     /* if the ComBlockPtr isn't set yet allocate our own */
     if (w->term.com_block == NULL) {
	  warn("TermCanvas: No ComBlockPtr supplied, creating my own");
	  w->term.com_block = (ComBlockPtr)(XtMalloc(sizeof(ComBlock)));
	  w->term.com_block_alloc = True;
     }
     else 
     {
	  w->term.com_block_alloc = False;
     }
     
#if 0
     /* Make an initial Resize Request */
     XtMakeResizeRequest(w, (Dimension)(w->term.cell_width * w->term.columns),
			 (Dimension)(w->term.cell_height * w->term.lines),
			 NULL, NULL);
#endif
}

/*
 * Resize Handler, will get called by the Intrinsics
 */
Local void
Resize(Widget widget)
{
     TermCanvasWidget w = (TermCanvasWidget)widget;
     Dimension lines, columns;

     lines = w->core.height / w->term.cell_height;
     columns = w->core.width / w->term.cell_width;
     
#ifdef DEBUG
     debug("canvas resize: Resizing to %d rows %d columns (%dx%d)",
	   lines, columns, w->core.width, w->core.height);
#endif
     
     if (w->term.set_size != NULL)
	  w->term.set_size(XtParent(w), lines, columns, w->core.width,
			   w->core.height);
     
     if ((lines == w->term.lines) && (columns == w->term.columns))
	  return;
     
     /*
      * Unmark the selection before resizing the char array to avoid
      * trouble with the coordinates.
      */
     if (w->term.select_on)
	  unmark_selection(w);
     
     w->term.char_array = resize_char_array(w, w->term.char_array,
					    w->term.lines, w->term.columns,
					    lines, columns);
#ifdef DOUBLE_FONTS
     w->term.line_flags = resize_line_flags(w->term.line_flags,
					    w->term.lines, lines);
#endif /* DOUBLE_FONTS */ 

     resize_save_area(w, w->term.columns, columns);
     
     w->term.tabs = resize_tab_list(w->term.tabs, w->term.columns, columns,
				    w->term.def_tab_width);

     /*
      * If the cursor would be beyond the end of the screen put it to the
      * border
      */
     if ((lines > 0) && (w->term.array_cur.lin >= lines)) {
	  w->term.array_cur.lin = lines - 1;
	  w->term.screen_cur.y = lines * w->term.cell_height;
     }
     if ((columns > 0) && (w->term.array_cur.col >= columns)) {
	  w->term.array_cur.col = columns - 1;
	  w->term.screen_cur.x = (columns - 1) * w->term.cell_width;
     }
     
     /*
      * Clear the screen and redraw everything, but only if we are
      * already realized.
      */
     /*
     if (XtIsRealized(w)) {
	  clear_whole_screen(w);
	  redraw_rect(w, w->term.char_array, 0, 0, lines - 1, columns - 1);
	  flip_cursor_on(w);
     }
     */

     /*
      * If the scrolling region was at the bottom of the screen
      * put it to the new bottom.
      */
     if ((w->term.scroll_bottom == w->term.lines - 1) ||
	 (w->term.lines == 0)) {
	  w->term.scroll_bottom = lines - 1;
     }
     if (w->term.scroll_top >= lines) {
	  w->term.scroll_top = lines - 1;
     }

     /* Set the new lines and columns */
     w->term.lines = lines;
     w->term.columns = columns;
}

/*
 * Destroy Procedure, Frees up allocated stuff and stops the blinking
 * cursor.
 */
Local void
Destroy(Widget _w)
{
     TermCanvasWidget w = (TermCanvasWidget)_w;

     if (w->term.timeout_id != 0) {
	  XtRemoveTimeOut(w->term.timeout_id);
	  w->term.timeout_id = 0;
     }
     
     /* Free the current array */
     free_char_array(w->term.char_array, w->term.lines);

     /* Free the save_array */
     free_char_array(w->term.save_array, w->term.save_lines);

     /* Free the tab list */
     XtFree(w->term.tabs);

     /* Destroy the ComBlock, if we have allocated it ourselves */
     if (w->term.com_block_alloc)
	  XtFree((caddr_t)w->term.com_block);

     /* Free the GCs */
     XFreeGC(XtDisplay(w), w->term.norm_gc);
     XFreeGC(XtDisplay(w), w->term.spec_gc);
     XFreeGC(XtDisplay(w), w->term.cursor_graph_gc);
     XFreeGC(XtDisplay(w), w->term.cursor_text_gc);
     XFreeGC(XtDisplay(w), w->term.cursor_rem_gc);
     XFreeGC(XtDisplay(w), w->term.clear_gc);

     /* Free the MotionList stuff */
     canvasFreeMotions(w);
}

/*
 * Expose Handler. Will get called by the Intrinsics
 */
/*ARGSUSED*/
Local void
Canvas_Expose(Widget widget, XEvent *event, Region region)
{
     TermCanvasWidget w = (TermCanvasWidget)widget;
     Position start_lin, start_col, end_lin, end_col;
     int tmp;

     /* calculate the character Rectangle to redraw */
     tmp = event->xexpose.y;
     start_lin = tmp / w->term.cell_height;
     tmp += event->xexpose.height;
     end_lin = (tmp - 1) / w->term.cell_height;
     if (end_lin >= w->term.lines)
	  end_lin = w->term.lines - 1;
	  
     tmp = event->xexpose.x;
     start_col = tmp / w->term.cell_width;
     tmp += event->xexpose.width;
     end_col = (tmp - 1) / w->term.cell_width + 1;
     if (end_col >= w->term.columns)
	  end_col = w->term.columns - 1;

     /* do the redrawing */
     redraw_rect(w, w->term.char_array, start_lin, start_col,
		 end_lin, end_col);

     /* if the cursor was inside the rectangle and is visible, draw it */
     if (w->term.cursor_visible
	 && ((w->term.array_cur.col >= start_col)
	     && (w->term.array_cur.col <= end_col))
	 && ((w->term.array_cur.lin >= start_lin)
	     && (w->term.array_cur.lin <= end_col)))
	  flip_cursor_on(w);
     
     /* if necessary call the notify_first_map routine */
     if (!w->term.mapped_yet) {
	  if (w->term.notify_first_map != NULL)
	       w->term.notify_first_map(XtParent(w));
	  w->term.mapped_yet = True;
     }
}

/* Routine to blink the cursor - gets called via XtAddTimeOut */
/*ARGSUSED*/
Export void
blink_cursor(XtPointer client_data, XtIntervalId *id)
{
     TermCanvasWidget w = (TermCanvasWidget)client_data;

     if (w->term.cursor_visible) {
	  flip_cursor_off(w);
	  w->term.cursor_visible = False;
     }
     else {
	  w->term.cursor_visible = True;
	  flip_cursor_on(w);
     }
     
     if (w->term.cursor_blinking)
	  w->term.timeout_id = XtAddTimeOut(w->term.blink_interval,
					    blink_cursor, w);
}

/* Generic Action Routines start here */

/* Routine that gets called when the Widget Loses the Input Focus */
/*ARGSUSED*/
Local void
focus_out_handler(Widget widget, XEvent *event, String *params,
		  Cardinal *num_params)
{
     TermCanvasWidget w = (TermCanvasWidget)widget;

     if (w->term.focused) {
	  if (XtIsRealized((Widget)w)) {
	       if (w->term.cursor_visible)
		    flip_cursor_off(w);
	       w->term.outline_cursor = True;
	       
	       if (!w->term.blink_wo_focus && w->term.cursor_blinking) {
		    if (w->term.timeout_id != 0) {
			 XtRemoveTimeOut(w->term.timeout_id);
			 w->term.timeout_id = 0;
			 w->term.cursor_visible = True;
		    }
		    
		    flip_cursor_on(w);	/* turn it on anyways */
	       } else {
		    if (w->term.cursor_visible)
			 flip_cursor_on(w);
	       }
	  } else {
	       w->term.outline_cursor = True;
	  }
	  w->term.focused = False;
     }
}
			 
/* Routine that gets called when the Widget gets the Input Focus */
/*ARGSUSED*/
Local void
focus_in_handler(Widget widget, XEvent *event, String *params,
		 Cardinal *num_params)
{
     TermCanvasWidget w = (TermCanvasWidget)widget;

     if (!w->term.focused) {
	  if (XtIsRealized(widget)) {
	       flip_cursor_off(w);
	       w->term.outline_cursor = False;
	       
	       if (!w->term.blink_wo_focus && w->term.cursor_blinking
		   && w->term.cursor_visible && w->term.timeout_id == 0)
		    w->term.timeout_id =
			 XtAddTimeOut(w->term.blink_interval, blink_cursor, w);
	       
	       /* Watch out for permanently switched off cursors */
	       if (w->term.cursor_visible)
		    flip_cursor_on(w);
	  } else
	       w->term.outline_cursor = False;
	  w->term.focused = True;
     }
}

			 
/*
 * Generic key input handling Routine.
 * Called without parameters it will translate the event into a Latin-1
 * String and call the reverse parser.
 * If mod1 is pressed during the event, the resulting string's high bits
 * get set to indicate a pressed Meta Key (we assume that Meta is bound
 * to mod1.
 * 
 * If a parameter is supplied this string is used instead.
 */
Local void
key_input_handler(Widget widget, XEvent *event, String *params,
		  Cardinal *num_params)
{
     TermCanvasWidget w = (TermCanvasWidget)widget;
     ComBlockPtr block = w->term.com_block;
     
     if (*num_params == 0) { /* look up the event */
	  char buffer[100];
	  int i, j;
	  KeySym keysym;

	  if ((i = XLookupString (&event->xkey, buffer, 100,
				  &keysym, NULL))
	      != 0) {
	       if ((event->xkey.state & Mod1Mask) != 0) {
		    /* set the high bits */
		    for (j = 0; j < i; j++) {
			 buffer[j] |= 0x80;
		    }
	       }
	       cb_opcode(block) = OP_INSERT;
	       cb_nbytes(block) = i;
	       strcpy((char *)cb_buffer(block), buffer);
	       if (w->term.output != NULL) {
		    w->term.output(XtParent(w), block);
	       }
	  }
     }
     else {
	  cb_opcode(block) = OP_INSERT;
	  cb_nbytes(block) = strlen(params[0]);
	  strcpy((char *)cb_buffer(block), (char *)params[0]);
	  if (w->term.output != NULL) {
	       w->term.output(XtParent(w), block);
	  }
     }
}

/*
 * Action Routine to call the reverse parser directly.
 *
 * The calling format is:
 *
 * call-parser(<opcode>, <buffer>, [<Register>, <int_val>], ...)
 * 	<opcode> is the opcode to call,
 * 	<buffer> the value for the comblock_buffer
 * 	<Register> <int_val> Pairs specify which Registers to set
 * 		with <Register> being the uppercase Letter for the Register,
 * 		<int_val> being the Value to put int that Reg.
 *
 * 	e.g. a translation of "call-parser (5, foobar, A, 12, B, 20)
 * 	will call the reverse parser with the opcode 5, string "foobar"
 * 	in the buffer and Registers A and B of the comblock set to 12 and 20.
 *
 * Note: No Error checking on the Register Names and values is done.
 */
/*ARGSUSED*/
Local void
call_parser(Widget _w, XEvent *event, String *params, Cardinal *num_params)
{
     TermCanvasWidget w = (TermCanvasWidget)_w;
     ComBlockPtr block = w->term.com_block;
     
     int i,reg;
     int np = *num_params;

     if (np < 2) {
	  warn("action 'call-parser' called with too few arguments (%d)", np);
	  return;
     }

     cb_opcode(block) = atoi(params[0]);
     strcpy((char *)cb_buffer(block), params[1]);
     cb_nbytes(block) = strlen(params[1]);
     
     params += 2;
     np -=2;

     for (i = 0; i < (np - 1); i += 2) {
	  reg = *params[i] & 0x7f;
	  
	  cb_reg_data(block, reg) = (Generic)atoi(params[i+1]);
	  cb_reg_type(block, reg) = CB_INT_TYPE;
     }
     if (w->term.output != NULL) {
	  w->term.output(XtParent(w), block);
     }
}
     
/*
 * Action Routine to call the canvas directly.
 *
 * The calling format is:
 *
 * call-canvas(<opcode>, <buffer>, [<Register>, <int_val>], ...)
 *
 * For the exact parameter description see cal-parser
 */

/*ARGSUSED*/
Local void
call_canvas(Widget widget, XEvent *event, String *params, Cardinal *num_params)
{
     ComBlock block;
     
     int i,reg;
     int np = *num_params;

     if (np < 2) {
	  warn("action 'call-canvas' called with too few arguments (%d)", np);
	  return;
     }
     cb_opcode(&block) = atoi(params[0]);
     strcpy((char *)cb_buffer(&block), params[1]);
     cb_nbytes(&block) = strlen(params[1]);
     
     params += 2;
     np -=2;

     for (i = 0; i < (np - 1); i += 2) {
	  reg = *params[i] & 0x7f;
	  
	  cb_reg_data(&block, reg) = (Generic)atoi(params[i+1]);
	  cb_reg_type(&block, reg) = CB_INT_TYPE;
     }
     /* Always put the cursor back */
     XpTermCanvasDispatch(widget, &block, True);
}

     
/*
 * Generic canvas dispatcher.
 * Requests of OP_NOP are just ignored.
 * The flag is used to determine whether the cursor should be put back
 */
Export int
XpTermCanvasDispatch(Widget widget, ComBlockPtr blockptr, Boolean flag)
{
     int opc = cb_opcode(blockptr);
     int retval = 99;	/* 99 = invalid */
     
     if ((opc >= 0) && (opc < OP_NUM)) {
#ifdef DEBUG 
	  printf("canvas dispatch: opcode = %2d\n", opc);
#endif 
	  retval = ((*jumptable[opc])(widget, blockptr));
     }    
     else if (opc != OP_NOP)
	  warn("XpTermCanvasDispatch: Illegal opcode %d", opc);

     if (flag) {
	  if (((TermCanvasWidget)widget)->term.js_flag &&
	      ((TermCanvasWidget)widget)->term.scroll_pos != 0)
	       scroll_save_abs((TermCanvasWidget)widget, 0, True);
	  flip_cursor_on((TermCanvasWidget)widget);
     }
     return(retval);
}

/*
 * Returns the fields from start to stop in a dash-
 * separated string.
 * This function will modify the source, putting
 * '\0's in the appropiate place and moving the
 * beginning forward to after the '\0'
 *
 * This will NOT work for the last field (but we won't need
 * it anyways)
 */
Local char *
n_fields(char **source, int start, int stop)
{
     int i;
     char *str , *str1;
     
     /*
      * find the start-1th dash
      */
     for (i = start-1, str = *source; i; i--, str++)
	  if ((str = strchr(str, '-')) == NULL)
	       return NULL;

     /*
      * find the stopth dash
      */
     for (i = stop - start + 1, str1 = str; i; i--, str1++)
	  if ((str1 = strchr(str1, '-')) == NULL)
	       return NULL;

     /*
      * put a \0 at the end of the fields
      */
     *(str1 - 1) = '\0';

     /*
      * move source forward
      */
     *source = str1;

     return str;
}


/*
 * gets the font properties from a given font structure.
 * We use the FONT name to find them out, since that
 * seems easier.
 * Returns a pointer to a static FontNameProperties structure
 * or NULL on error.
 */
Export FontNameProperties *
get_font_name_props(Display *dpy, XFontStruct *fs)
{
     Local FontNameProperties props;
     
     register XFontProp *fp;
     register int i;
     Atom fontatom = XInternAtom (dpy, "FONT", False);
     char *name;
     char *str;
     
     /*
      * first get the full font name
      */
     for (name = NULL, i = 0, fp = fs->properties;
	  i < fs->n_properties;
	  i++, fp++)
	  if (fp->name == fontatom)
	       name = XGetAtomName (dpy, fp->card32);
     
     if (name == NULL)
	  return NULL;

     /*
      * Now split it up into parts and put them in
      * their places. Since we are using parts of
      * the original string, we must not free the Atom Name
      */
     
     /* registry, foundry, family */
     if ((props.beginning = n_fields(&name, 1, 3)) == NULL)
	 return NULL;

     /* weight is the next */
     if ((props.width = n_fields(&name, 1, 1)) == NULL)
	  return NULL;
     
     /* slant, width, add style */
     if ((props.middle = n_fields(&name, 1, 3)) == NULL)
	  return NULL;
     
     /* pixel size */
     if ((str = n_fields(&name, 1, 1)) == NULL)
	  return NULL;
     if ((props.pixel_size = atoi(str)) == 0)
	  return NULL;
     
     /* point size */
     if ((props.point_size = n_fields(&name, 1, 1)) == NULL)
	  return NULL;

     /* res_x */
     if ((str = n_fields(&name, 1, 1)) == NULL)
	  return NULL;
     if ((props.res_x = atoi(str)) == 0)
	  return NULL;

     /* res_y */
     if ((str = n_fields(&name, 1, 1)) == NULL)
	  return NULL;
     if ((props.res_y = atoi(str)) == 0)
	  return NULL;
     
     /* spacing */
     if ((props.spacing = n_fields(&name, 1, 1)) == NULL)
	  return NULL;

     /* skip the average width */
     if ((str = n_fields(&name, 1, 1)) == NULL)
	  return NULL;

     /* the rest: charset registry and charset encoding */
     props.end = name;
     
     return &props;
}

/*
 * Take the given font props and try to make a
 * well formed font name specifying the same base font
 * and size and everything, but in bold. The return
 * value comes from a static variable, so be careful
 * if you reuse it.
 */
Export char *
bold_font_name(FontNameProperties *props)
{
     static char ret[200];

     /*
      * Put together something in the form
      * "<beginning>-bold-<middle>-<pixel_size>-<point_size>-<res_x>-<res_y>"\
      * "-<spacing>-*-<end>"
      */
     sprintf(ret, "%s-bold-%s-%d-%s-%d-%d-%s-*-%s",
	     props->beginning,
	     props->middle,
	     props->pixel_size,
	     props->point_size, 
	     props->res_x, 
	     props->res_y,
	     props->spacing, 
	     props->end);
     
     return ret;
}

#ifdef DOUBLE_FONTS
/*
 * Take the given font props and try to make a well formed
 * font name specifying the sme base font but changed depending
 * ont the given attributes and lflags.
 *
 * For double width fonts, we just double the X-resolution,
 * for double height fonts we double the pixel-size and Y-resolution
 */
Export char *
special_font_name(FontNameProperties *props, unsigned char atts,
		  LineFlagsElem lflags)
{
     char tmp[200];
     char *ret;

     char *width;
     int pixel_size = props->pixel_size;
     int res_x = props->res_x;
     int res_y = props->res_y;

     if (atts & ATT_BOLD)
	  width = "bold";
     else
	  width = props->width;
     
     if (lflags & LINE_D_WIDE)
	  res_x *= 2;
     
     if (lflags & (LINE_D_UPPER | LINE_D_LOWER)) {
	  res_x *= 2;
	  res_y *= 2;
	  pixel_size *= 2;
     }
     
     sprintf(tmp, "%s-%s-%s-%d-%s-%d-%d-%s-*-%s",
	     props->beginning,
	     width,
	     props->middle,
	     pixel_size,
	     props->point_size, 
	     res_x, 
	     res_y,
	     props->spacing, 
	     props->end);

     ret = XtMalloc(strlen(tmp) + 1);
     strcpy(ret, tmp);
     
     return ret;
}
#endif /* DOUBLE_FONTS */ 
