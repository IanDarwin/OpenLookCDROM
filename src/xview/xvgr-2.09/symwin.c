/* $Id: symwin.c,v 1.37 92/07/19 08:13:54 pturner Exp Locker: pturner $
 *
 * symbols, legends, and error bars
 *
 */

#include <stdio.h>
#include <math.h>

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/font.h>
#include <xview/cursor.h>

#include "globals.h"

void updatesymbols();
void updatelegendstr();
void updateerrbar();
void updatelegends();

void create_ledit_frame();
static void update_ledit_items();
static void load_ledit();

int cset = 0;

/*
extern Server_image w0_image, w1_image, w2_image, w3_image, w4_image, w5_image, w6_image, w7_image;
extern Server_image none_image, solid_image, dotted_image, sdashed_image, ldashed_image, ddashed_image;
*/

extern Frame main_frame;

Frame define_symbols_frame;
static Panel define_symbols_panel;

Frame define_errbar_frame;
static Panel define_errbar_panel;
void define_errbar_popup();
Panel_item errbar_size_item;
Panel_item errbar_width_item;
Panel_item errbar_lines_item;
Panel_item errbar_type_item;
Panel_item errbar_riser_item;
Panel_item errbar_riserlinew_item;
Panel_item errbar_riserlines_item;
Panel_item errbar_apply_item;


static Panel_item toggle_set_item;
static Panel_item symbols_apply_item;
static Panel_item toggle_symbols_item;
static Panel_item symchar_item;
static Panel_item symsize_item;
static Panel_item symskip_item;
static Panel_item symfill_item;
static Panel_item toggle_color_item;
static Panel_item toggle_width_item;
static Panel_item toggle_lines_item;
static Panel_item toggle_fill_item;
static Panel_item toggle_fillusing_item;
static Panel_item toggle_fillpat_item;
static Panel_item toggle_fillcol_item;
static Panel_item toggle_symset_item;
static Panel_item symmsg;

Frame define_legend_frame;
static Panel define_legend_panel;
void define_legend_popup();

Panel_item legend_x_panel;	/* needed in the canvas event proc */
Panel_item legend_y_panel;
static Panel_item toggle_legends_item;
static Panel_item toggle_legendloc_item;
static Panel_item legend_str_panel;
static Panel_item legends_gap_item;
static Panel_item legends_len_item;
static Panel_item legend_font_item;
static Panel_item legend_charsize_item;
static Panel_item legend_color_item;
static Panel_item legend_linew_item;
static Panel_item leglocbut;
static Panel_item legend_box_item;
static Panel_item legend_boxfill_item;
static Panel_item legend_boxfillusing_item;
static Panel_item legend_boxfillcolor_item;
static Panel_item legend_boxfillpat_item;
static Panel_item legend_boxlinew_item;
static Panel_item legend_boxlines_item;
static Panel_item legend_boxcolor_item;

Frame ledit_frame;
Panel ledit_panel;

void xv_setstr(p, s)
    Panel_item p;
    char *s;
{
    xv_set(p, PANEL_VALUE, s, NULL);
}

char *xv_getstr(p)
    Panel_item p;
{
    return (char *) xv_get(p, PANEL_VALUE);
}

/*
 * define symbols for the current set
 */

static void define_symbols(set_mode)
    int set_mode;
{
    int setno, sym, symchar, symskip, symfill, line, pen, wid, fill, fillpat, fillusing, fillcol, i;
    double symsize;
    char s[30];
    int value = (int) xv_get(symsize_item, PANEL_VALUE);

    symsize = value / 100.0;
    sym = (int) xv_get(toggle_symbols_item, PANEL_VALUE);
    pen = (int) xv_get(toggle_color_item, PANEL_VALUE);
    wid = (int) xv_get(toggle_width_item, PANEL_VALUE);
    line = (int) xv_get(toggle_lines_item, PANEL_VALUE);
    fill = (int) xv_get(toggle_fill_item, PANEL_VALUE);
    fillusing = (int) xv_get(toggle_fillusing_item, PANEL_VALUE) ? PATTERN : COLOR;
    fillpat = (int) xv_get(toggle_fillpat_item, PANEL_VALUE);
    fillcol = (int) xv_get(toggle_fillcol_item, PANEL_VALUE);
    symskip = (int) xv_get(symskip_item, PANEL_VALUE);
    symfill = (int) xv_get(symfill_item, PANEL_VALUE);
    strcpy(s, (char *) xv_get(symchar_item, PANEL_VALUE));
    symchar = s[0];
    strcpy(g[cg].l.str[cset].s, (char *) xv_get(legend_str_panel, PANEL_VALUE));
    load_ledit(cg, cset);
    if (set_mode == 0) {
	setno = cset;
    } else {
	setno = -1;
    }
    set_prop(cg, SET,
	     SETNUM, setno,
	     SYMBOL, TYPE, sym,
	     SYMBOL, FILL, symfill,
	     SYMBOL, SIZE, symsize,
	     SYMBOL, CHAR, symchar,
	     SKIP, symskip,
	     LINESTYLE, line,
	     LINEWIDTH, wid,
	     COLOR, pen,
	     FILL, TYPE, fill,
	     FILL, WITH, fillusing,
	     FILL, COLOR, fillcol,
	     FILL, PATTERN, fillpat,
	     0);
    updatesymbols(cg, cset);
    drawgraph();
}

static void define_symbols_proc()
{
    define_symbols((int) xv_get(symbols_apply_item, PANEL_VALUE));
}

/*
 * define colors incrementally
 */
static void setall_colors_proc()
{
    int i;

    for (i = 0; i < g[cg].maxplot; i++) {
	if (isactive(cg, i)) {
	    setplotcolor(cg, i, (i % maxcolors) + 1);
	}
    }
    updatesymbols(cg, cset);
    drawgraph();
}

/*
 * define symbols incrementally mod 10
 */
static void setall_sym_proc()
{
    int i;

    for (i = 0; i < g[cg].maxplot; i++) {
	if (isactive(cg, i)) {
	    setplotsym(cg, i, (i % 10) + 2);
	}
    }
    updatesymbols(cg, cset);
    drawgraph();
}

/*
 * define linewidths incrementally mod 7
 */
static void setall_linew_proc()
{
    int i;

    for (i = 0; i < g[cg].maxplot; i++) {
	if (isactive(cg, i)) {
	    setplotlinew(cg, i, (i % 7) + 1);
	}
    }
    updatesymbols(cg, cset);
    drawgraph();
}

/*
 * freshen up symbol items, generally after a parameter
 * file has been read
 */
void updatesymbols(gno, value)
    int gno, value;
{
    int iv;
    char s[2];

    if (value == -1) {
	value = cset;
    }
    if (define_symbols_frame && cset == value) {
	iv = 100.0 * g[gno].p[value].symsize;
	xv_set(symsize_item, PANEL_VALUE, iv, NULL);
	xv_set(toggle_symset_item, PANEL_VALUE, value, NULL);
	xv_set(symskip_item, PANEL_VALUE, g[gno].p[value].symskip, NULL);
	xv_set(symfill_item, PANEL_VALUE, g[gno].p[value].symfill, NULL);
	if (g[gno].p[value].symchar > ' ' && g[gno].p[value].symchar < 127) {
	    s[0] = g[gno].p[value].symchar;
	    s[1] = 0;
	} else {
	    s[0] = 0;
	}
	xv_set(symchar_item, PANEL_VALUE, s, NULL);
	xv_set(toggle_symbols_item, PANEL_VALUE, getsetplotsym(gno, value), NULL);
	xv_set(toggle_color_item, PANEL_VALUE, getsetcolor(gno, value), NULL);
	xv_set(toggle_width_item, PANEL_VALUE, getsetlinew(gno, value), NULL);
	xv_set(toggle_lines_item, PANEL_VALUE, getsetlines(gno, value), NULL);
	xv_set(toggle_fill_item, PANEL_VALUE, g[gno].p[value].fill, NULL);
	xv_set(toggle_fillusing_item, PANEL_VALUE, g[gno].p[value].fillusing == COLOR ? 0 : 1, NULL);
	xv_set(toggle_fillcol_item, PANEL_VALUE, g[gno].p[value].fillcolor, NULL);
	xv_set(toggle_fillpat_item, PANEL_VALUE, g[gno].p[value].fillpattern, NULL);
	updatelegendstr(gno);
	updateerrbar(gno, value);
    }
}

/*
 * set number changed so update everything
 */
/* ARGSUSED */
void set_cset_proc(w, cd)
    Panel_item w;
    int cd;
{
    cset = xv_get(toggle_symset_item, PANEL_VALUE);
    updatesymbols(cg, cset);
    updateerrbar(cg, cset);
}

/*
 * legends
 */

static int firstrun = TRUE;

/*
 * freshen up legend items, generally after a parameter
 * file has been read
 */

void updatelegendstr(gno)
    int gno;
{
    if (define_symbols_frame) {
	xv_set(legend_str_panel, PANEL_VALUE, g[gno].l.str[cset].s, NULL);
    }
}

/*
 * Cancel proc
 */
static void reset_symleg_proc()
{
}

/*
 * close the symbols/legends popup
 */
static void define_symbols_done_proc()
{
    xv_set(define_symbols_frame, WIN_SHOW, FALSE, 0);
}

/*
 * create the symbols popup
 */
void define_symbols_popup()
{
    int i;

    if (define_symbols_frame) {
	updatesymbols(cg, cset);
	xv_set(define_symbols_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    define_symbols_frame = xv_create(main_frame, FRAME,
				     XV_LABEL, "Symbols",
				     FRAME_SHOW_LABEL, TRUE,
		      WIN_ERROR_MSG, "Couldn't create define_symbols_frame",
				     NULL);
    define_symbols_panel = xv_create(define_symbols_frame, PANEL,
				     XV_HELP_DATA, "xvgr:symbols_panel",
				     PANEL_LAYOUT, PANEL_VERTICAL,
				     NULL);
    toggle_symset_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Select set:",
				     XV_HELP_DATA, "xvgr:symbols_selectset",
					   PANEL_NOTIFY_PROC, set_cset_proc,
						PANEL_CHOICE_NCOLS, 3,
						PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
		 "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
		 "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
						NULL,
				      XV_X, xv_col(define_symbols_panel, 0),
				      XV_Y, xv_row(define_symbols_panel, 0),
						NULL);
    xv_create(define_symbols_panel, PANEL_MESSAGE,
	      PANEL_LABEL_STRING, "Symbol:",
	      XV_X, xv_col(define_symbols_panel, 0),
	      XV_Y, xv_row(define_symbols_panel, 1),
	      NULL);
    toggle_symbols_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:symbols_selectsym",
						 PANEL_CHOICE_NCOLS, 4,
					      PANEL_LABEL_STRING, "Symbol:",
						 PANEL_CHOICE_STRINGS,
						 "No symbol",
						 "Dot",
						 "Circle",
						 "Square",
						 "Diamond",
						 "Triangle up",
						 "Triangle left",
						 "Triangle down",
						 "Triangle right",
						 "Plus",
						 "X",
						 "Star",
						 "Impulse at X",
						 "Impulse at Y",
						 "Vert line at X",
						 "Horiz line at Y",
						 "Histogram X",
						 "Histogram Y",
						 "Stair step X",
						 "Stair step Y",
						 "Bar X",
						 "Bar Y",
						 "Range",
						 "Loc",
						 "Set #",
						 "Set #, loc",
						 "Bar and whisker",
						 "Segments",
						 "Character",
						 "Tag first point",
						 "Tag last point",
						 "Tag center point",
						 "*String",
						 "Hi low X",
						 "Hi low Y",
						 "Open/close X",
						 "Open/close Y",
						 NULL,
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 10),
						 NULL);

    symfill_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "Fill:",
				       XV_HELP_DATA, "xvgr:symbols_symfill",
					  PANEL_CHOICE_STRINGS,
					  "None", "Filled", "Opaque",
					  NULL,
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 10),
					  NULL);
    symsize_item = (Panel_item) xv_create(define_symbols_panel, PANEL_SLIDER,
				       XV_HELP_DATA, "xvgr:symbols_symsize",
					  PANEL_SLIDER_WIDTH, 100,
					  PANEL_SHOW_VALUE, FALSE,
					  PANEL_SHOW_RANGE, FALSE,
					  PANEL_MIN_VALUE, 0,
					  PANEL_MAX_VALUE, 400,
					  PANEL_LABEL_STRING, "Size:",
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 10),
					  NULL);

    symchar_item = (Panel_item) xv_create(define_symbols_panel, PANEL_TEXT,
				       XV_HELP_DATA, "xvgr:symbols_symchar",
					  PANEL_LAYOUT, PANEL_HORIZONTAL,
					  PANEL_VALUE_DISPLAY_LENGTH, 3,
					  PANEL_LABEL_STRING, "Char:",
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 10),
					  NULL);

    symskip_item = (Panel_item) xv_create(define_symbols_panel, PANEL_NUMERIC_TEXT,
				       XV_HELP_DATA, "xvgr:symbols_symskip",
					  PANEL_LABEL_STRING, "Skip:",
					  PANEL_VALUE_DISPLAY_LENGTH, 4,
					  PANEL_MIN_VALUE, 0,
					  PANEL_MAX_VALUE, 500,
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 10),
					  NULL);
    legend_str_panel = (Panel_item) xv_create(define_symbols_panel, PANEL_TEXT,
				     XV_HELP_DATA, "xvgr:symbols_symlegend",
					      PANEL_VALUE_DISPLAY_LENGTH, 20,
					      PANEL_LABEL_STRING, "Legend:",
				      XV_X, xv_col(define_symbols_panel, 7),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 10),
					      NULL);
    symbols_apply_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE,
				    XV_HELP_DATA, "xvgr:symbols_symapplyto",
					    PANEL_LABEL_STRING, "Apply to:",
						PANEL_CHOICE_STRINGS,
						"selected set",
						"all sets",
						NULL,
				      XV_X, xv_col(define_symbols_panel, 9),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 12),
						NULL);
    xv_create(define_symbols_panel, PANEL_MESSAGE,
	      PANEL_LABEL_STRING, "Line:",
	      XV_X, xv_col(define_symbols_panel, 25),
	      XV_Y, xv_row(define_symbols_panel, 1),
	      NULL);
    toggle_lines_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:symbols_linestyle",
					       PANEL_LABEL_STRING, "Style:",
					       PANEL_CHOICE_STRINGS,
					       "None",
					       "Solid",
					       "Dotted",
					       "Dashed",
					       "Long Dashed",
					       "Dot-dashed",
					       NULL,
				      XV_Y, xv_row(define_symbols_panel, 2),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 32),
					       NULL);
    toggle_width_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:symbols_linewidth",
					       PANEL_LABEL_STRING, "Width:",
					       PANEL_CHOICE_NCOLS, 3,
					       PANEL_CHOICE_STRINGS,
					       "None",
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
/*
w0_image, w1_image, w2_image, w3_image, w4_image, w5_image, w6_image, w7_image,
*/
					       NULL,
				      XV_Y, xv_row(define_symbols_panel, 3),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 32),
					       NULL);
    toggle_color_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:symbols_linecolor",
					       PANEL_LABEL_STRING, "Color:",
					       PANEL_CHOICE_NCOLS, 4,
					       PANEL_CHOICE_STRINGS,
					       "0",
			  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
					       NULL,
				      XV_Y, xv_row(define_symbols_panel, 4),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 32),
					       NULL);
    xv_create(define_symbols_panel, PANEL_MESSAGE,
	      PANEL_LABEL_STRING, "Fill:",
	      XV_X, xv_col(define_symbols_panel, 45),
	      XV_Y, xv_row(define_symbols_panel, 1),
	      NULL);
    toggle_fill_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				      XV_HELP_DATA, "xvgr:symbols_filltype",
					      PANEL_LABEL_STRING, "Fill:",
					      PANEL_CHOICE_STRINGS,
					      "None",
					      "As polygon",
					      "To Y=0.0",
					      "To X=0.0",
					      "To X min",
					      "To X max",
					      "To Y min",
					      "To Y max",
					      NULL,
				      XV_Y, xv_row(define_symbols_panel, 2),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 54),
					      NULL);
    toggle_fillusing_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:symbols_fillusing",
					  PANEL_LABEL_STRING, "Fill using:",
						   PANEL_CHOICE_STRINGS,
						   "Color",
						   "Pattern",
						   NULL,
				      XV_Y, xv_row(define_symbols_panel, 3),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 54),
						   NULL);
    toggle_fillcol_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:symbols_fillcolor",
					       PANEL_LABEL_STRING, "Color:",
						 PANEL_CHOICE_NCOLS, 4,
						 PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
						 NULL,
				      XV_Y, xv_row(define_symbols_panel, 4),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 54),
						 NULL);
    toggle_fillpat_item = (Panel_item) xv_create(define_symbols_panel, PANEL_CHOICE_STACK,
				   XV_HELP_DATA, "xvgr:symbols_fillpattern",
					     PANEL_LABEL_STRING, "Pattern:",
						 PANEL_CHOICE_NCOLS, 4,
						 PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
						 NULL,
				      XV_Y, xv_row(define_symbols_panel, 5),
			    PANEL_VALUE_X, xv_col(define_symbols_panel, 54),
						 NULL);
    (void) xv_create(define_symbols_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:symbols_setallcolors",
		     PANEL_LABEL_STRING, "Set all colors",
		     PANEL_NOTIFY_PROC, setall_colors_proc,
		     XV_X, xv_col(define_symbols_panel, 40),
		     XV_Y, xv_row(define_symbols_panel, 6),
		     NULL);
    (void) xv_create(define_symbols_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:symbols_setallsymbols",
		     PANEL_LABEL_STRING, "Set all symbols",
		     PANEL_NOTIFY_PROC, setall_sym_proc,
		     XV_X, xv_col(define_symbols_panel, 40),
		     XV_Y, xv_row(define_symbols_panel, 7),
		     NULL);
    (void) xv_create(define_symbols_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:symbols_setalllinewidths",
		     PANEL_LABEL_STRING, "Set all linewidths",
		     PANEL_NOTIFY_PROC, setall_linew_proc,
		     XV_X, xv_col(define_symbols_panel, 40),
		     XV_Y, xv_row(define_symbols_panel, 8),
		     NULL);
    (void) xv_create(define_symbols_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:symbols_errorbar",
		     PANEL_LABEL_STRING, "Error bar props...",
		     PANEL_NOTIFY_PROC, define_errbar_popup,
		     XV_X, xv_col(define_symbols_panel, 15),
		     XV_Y, xv_row(define_symbols_panel, 10),
		     NULL);
    (void) xv_create(define_symbols_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:symbols_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, define_symbols_done_proc,
		     XV_X, xv_col(define_symbols_panel, 34),
		     XV_Y, xv_row(define_symbols_panel, 10),
		     NULL);
    (void) xv_create(define_symbols_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:symbols_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_symbols_proc,
		     XV_X, xv_col(define_symbols_panel, 5),
		     XV_Y, xv_row(define_symbols_panel, 10),
		     NULL);
    if (!g[cg].l.active) {
	for (i = 0; i < MAXPLOT; i++) {
	    g[cg].l.str[i].s[0] = '\0';
	}
    }
    updatesymbols(cg, cset);
    window_fit(define_symbols_panel);
    window_fit(define_symbols_frame);
    xv_set(define_symbols_frame, WIN_SHOW, TRUE, 0);
}

/*
 * define errbars for the current set
 */

static void define_errbar_proc()
{
    int i, itmp, applyto, nstart, nstop;

    applyto = (int) xv_get(errbar_apply_item, PANEL_VALUE);
    if (applyto) {
	nstart = 0;
	nstop = g[cg].maxplot - 1;
    } else {
	nstart = nstop = cset;
    }
    for (i = nstart; i <= nstop; i++) {
	itmp = (int) xv_get(errbar_type_item, PANEL_VALUE);
	switch (dataset_type(cg, i)) {
	case XYDX:
	case XYDXDX:
	    if (itmp == 0) {
		itmp = BOTH;;
	    } else if (itmp == 1) {
		itmp = LEFT;
	    } else {
		itmp = RIGHT;
	    }
	    break;
	case XYDY:
	case XYDYDY:
	    if (itmp == 0) {
		itmp = BOTH;;
	    } else if (itmp == 1) {
		itmp = TOP;
	    } else {
		itmp = BOTTOM;
	    }
	    break;
	default:
	    itmp = BOTH;
	    break;
	}
	set_prop(cg, SET,
		 SETNUM, i,
		 ERRORBAR, TYPE, itmp,
	    ERRORBAR, LENGTH, xv_get(errbar_size_item, PANEL_VALUE) / 100.0,
		 ERRORBAR, LINEWIDTH, (int) xv_get(errbar_width_item, PANEL_VALUE) + 1,
		 ERRORBAR, LINESTYLE, (int) xv_get(errbar_lines_item, PANEL_VALUE) + 1,
		 ERRORBAR, RISER, ACTIVE, (int) xv_get(errbar_riser_item, PANEL_VALUE) ? ON : OFF,
		 ERRORBAR, RISER, LINEWIDTH, (int) xv_get(errbar_riserlinew_item, PANEL_VALUE) + 1,
		 ERRORBAR, RISER, LINESTYLE, (int) xv_get(errbar_riserlines_item, PANEL_VALUE) + 1,
		 0);
    }
    drawgraph();
}

/*
 */
void updateerrbar(gno, value)
    int gno, value;
{
    int itmp;

    if (value == -1) {
	value = cset;
    }
    if (define_errbar_frame && cset == value) {
	xv_set(errbar_size_item, PANEL_VALUE, (int) (g[gno].p[value].errbarper * 100), NULL);
	switch (g[gno].p[value].errbarxy) {
	case BOTH:
	    itmp = 0;
	    break;
	case TOP:
	case LEFT:
	    itmp = 1;
	    break;
	case BOTTOM:
	case RIGHT:
	    itmp = 2;
	    break;
	}
	xv_set(errbar_type_item, PANEL_VALUE, itmp, NULL);
	xv_set(errbar_width_item, PANEL_VALUE, g[gno].p[value].errbar_linew - 1, NULL);
	xv_set(errbar_lines_item, PANEL_VALUE, g[gno].p[value].errbar_lines - 1, NULL);
	xv_set(errbar_riser_item, PANEL_VALUE, g[gno].p[value].errbar_riser == ON, NULL);
	xv_set(errbar_riserlinew_item, PANEL_VALUE, g[gno].p[value].errbar_riser_linew - 1, NULL);
	xv_set(errbar_riserlines_item, PANEL_VALUE, g[gno].p[value].errbar_riser_lines - 1, NULL);
    }
}

/*
 * close the errbar popup
 */
static void define_errbar_done_proc()
{
    xv_set(define_errbar_frame, WIN_SHOW, FALSE, 0);
}

/*
 * create the errbar popup
 */
void define_errbar_popup()
{
    if (define_errbar_frame) {
	updateerrbar(cg, cset);
	xv_set(define_errbar_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    define_errbar_frame = xv_create(main_frame, FRAME,
				    XV_LABEL, "Error bars",
				    FRAME_SHOW_LABEL, TRUE,
		       WIN_ERROR_MSG, "Couldn't create define_errbar_frame",
				    NULL);
    define_errbar_panel = xv_create(define_errbar_frame, PANEL,
				    XV_HELP_DATA, "xvgr:errbar_panel",
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    errbar_size_item = (Panel_item) xv_create(define_errbar_panel, PANEL_SLIDER,
					   XV_HELP_DATA, "xvgr:errbar_size",
					      PANEL_SLIDER_WIDTH, 100,
					      PANEL_SHOW_VALUE, TRUE,
					      PANEL_SHOW_RANGE, FALSE,
					      PANEL_MIN_VALUE, 0,
					      PANEL_MAX_VALUE, 400,
					      PANEL_LABEL_STRING, "Size:",
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
					      NULL);
    errbar_width_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHOICE_STACK,
					  XV_HELP_DATA, "xvgr:errbar_width",
					  PANEL_LABEL_STRING, "Line width:",
					       PANEL_CHOICE_NCOLS, 3,
					       PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
					       NULL,
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
					       NULL);
    errbar_lines_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHOICE_STACK,
					  XV_HELP_DATA, "xvgr:errbar_style",
					  PANEL_LABEL_STRING, "Line style:",
					       PANEL_CHOICE_STRINGS,
					       "Solid",
					       "Dotted",
					       "Dashed",
					       "Long Dashed",
					       "Dot-dashed",
					       NULL,
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
					       NULL);
    errbar_riser_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHECK_BOX,
				    XV_HELP_DATA, "xvgr:errbar_risertoggle",
					       PANEL_LABEL_STRING, "Riser:",
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
					       NULL);
    errbar_riserlinew_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:errbar_riserlinew",
				    PANEL_LABEL_STRING, "Riser line width:",
						    PANEL_CHOICE_NCOLS, 3,
						    PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
						    NULL,
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
						    NULL);
    errbar_riserlines_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:errbar_riserlines",
				    PANEL_LABEL_STRING, "Riser line style:",
						    PANEL_CHOICE_STRINGS,
						    "Solid",
						    "Dotted",
						    "Dashed",
						    "Long Dashed",
						    "Dot-dashed",
						    NULL,
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
						    NULL);
    errbar_type_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHOICE_STACK,
					XV_HELP_DATA, "xvgr:errbar_display",
					      PANEL_LABEL_STRING, "Display:",
					      PANEL_CHOICE_STRINGS,
					 "Both", "Top/left", "Bottom/right",
					      NULL,
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
					      NULL);
    errbar_apply_item = (Panel_item) xv_create(define_errbar_panel, PANEL_CHOICE,
					XV_HELP_DATA, "xvgr:errbar_applyto",
					    PANEL_LABEL_STRING, "Apply to:",
					       PANEL_CHOICE_STRINGS,
					       "selected set",
					       "all sets",
					       NULL,
			     PANEL_VALUE_X, xv_col(define_errbar_panel, 18),
					       NULL);
    (void) xv_create(define_errbar_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:errbar_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, define_errbar_done_proc,
		     XV_X, xv_col(define_errbar_panel, 15),
		     XV_Y, xv_row(define_errbar_panel, 10),
		     NULL);
    (void) xv_create(define_errbar_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:errbar_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_errbar_proc,
		     XV_X, xv_col(define_errbar_panel, 5),
		     XV_Y, xv_row(define_errbar_panel, 10),
		     NULL);
    updateerrbar(cg, cset);
    window_fit(define_errbar_panel);
    window_fit(define_errbar_frame);
    xv_set(define_errbar_frame, WIN_SHOW, TRUE, 0);
}

void updatelegends(gno)
{
    int iv;

    if (define_legend_frame) {
	iv = 100.0 * g[gno].l.charsize;
	xv_set(legend_charsize_item, PANEL_VALUE, iv, NULL);
	xv_set(toggle_legends_item, PANEL_VALUE, g[gno].l.active == ON, NULL);
	sprintf(buf, "%.9lg", g[gno].l.legx);
	xv_set(legend_x_panel, PANEL_VALUE, buf, NULL);
	sprintf(buf, "%.9lg", g[gno].l.legy);
	xv_set(legend_y_panel, PANEL_VALUE, buf, NULL);
	xv_set(legend_font_item, PANEL_VALUE, g[gno].l.font, NULL);
	xv_set(legend_color_item, PANEL_VALUE, g[gno].l.color, NULL);
	xv_set(legends_gap_item, PANEL_VALUE, g[gno].l.vgap - 1, NULL);
	xv_set(legends_len_item, PANEL_VALUE, g[gno].l.len - 1, NULL);
	xv_set(toggle_legendloc_item, PANEL_VALUE, g[gno].l.loctype == VIEW, NULL);
	xv_set(legend_box_item, PANEL_VALUE, g[gno].l.box == ON, NULL);
	xv_set(legend_boxfill_item, PANEL_VALUE, g[gno].l.boxfill == ON, NULL);
	xv_set(legend_boxfillusing_item, PANEL_VALUE, g[gno].l.boxfillusing == PATTERN, NULL);
	xv_set(legend_boxfillcolor_item, PANEL_VALUE, g[gno].l.boxfillcolor, NULL);
	xv_set(legend_boxfillpat_item, PANEL_VALUE, g[gno].l.boxfillpat, NULL);
	xv_set(legend_boxcolor_item, PANEL_VALUE, g[gno].l.boxlcolor, NULL);
	xv_set(legend_boxlinew_item, PANEL_VALUE, g[gno].l.boxlinew - 1, NULL);
	xv_set(legend_boxlines_item, PANEL_VALUE, g[gno].l.boxlines - 1, NULL);
	load_ledit(gno, -1);
    }
}

/*
 * define legends for all sets
 */
void define_legends_proc()
{
    char val[80];
    int value;

    if (define_legend_frame) {
	value = (int) xv_get(legend_charsize_item, PANEL_VALUE);
	g[cg].l.charsize = value / 100.0;
	g[cg].l.active = (int) xv_get(toggle_legends_item, PANEL_VALUE) ? ON : OFF;
	g[cg].l.vgap = (int) xv_get(legends_gap_item, PANEL_VALUE) + 1;
	g[cg].l.len = (int) xv_get(legends_len_item, PANEL_VALUE) + 1;
	g[cg].l.loctype = (int) xv_get(toggle_legendloc_item, PANEL_VALUE) ? VIEW : WORLD;
	strcpy(val, (char *) xv_get(legend_x_panel, PANEL_VALUE));
	g[cg].l.legx = atof(val);
	strcpy(val, (char *) xv_get(legend_y_panel, PANEL_VALUE));
	g[cg].l.legy = atof(val);
	g[cg].l.font = (int) xv_get(legend_font_item, PANEL_VALUE);
	g[cg].l.color = (int) xv_get(legend_color_item, PANEL_VALUE);
	g[cg].l.box = (int) xv_get(legend_box_item, PANEL_VALUE) ? ON : OFF;
	g[cg].l.boxfill = (int) xv_get(legend_boxfill_item, PANEL_VALUE) ? ON : OFF;
	g[cg].l.boxfillusing = (int) xv_get(legend_boxfillusing_item, PANEL_VALUE) ? PATTERN : COLOR;
	g[cg].l.boxfillcolor = (int) xv_get(legend_boxfillcolor_item, PANEL_VALUE);
	g[cg].l.boxfillpat = (int) xv_get(legend_boxfillpat_item, PANEL_VALUE);
	g[cg].l.boxlcolor = (int) xv_get(legend_boxcolor_item, PANEL_VALUE);
	g[cg].l.boxlinew = (int) xv_get(legend_boxlinew_item, PANEL_VALUE) + 1;
	g[cg].l.boxlines = (int) xv_get(legend_boxlines_item, PANEL_VALUE) + 1;
    }
    drawgraph();
}

/*
 * activate the legend location flag
 */
void legend_loc_proc()
{
    extern Xv_Cursor cursor_legloc;
    extern Canvas canvas;

    if (define_legend_frame) {
	g[cg].l.loctype = (int) xv_get(toggle_legendloc_item, PANEL_VALUE) ? VIEW : WORLD;
/*
	xv_set(define_legend_frame, WIN_SHOW, FALSE, 0);
*/
    }
    set_action(0);
    set_action(LEG_LOC);
}

/*
 * load legend strings from set comments
 */
void legend_load_proc()
{
    int i;

    for (i = 0; i < MAXPLOT; i++) {
	if (isactive(cg, i)) {
	    strcpy(g[cg].l.str[i].s, g[cg].p[i].comments);
	}
    }
    updatesymbols(cg, cset);
    load_ledit(cg, -1);
}

/*
 * close the legend popup
 */
static void define_legend_done_proc()
{
    xv_set(define_legend_frame, WIN_SHOW, FALSE, 0);
}

/*
 * create the legend popup
 */
void define_legend_popup()
{
    if (define_legend_frame) {
	updatelegends(cg);
	xv_set(define_legend_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    define_legend_frame = xv_create(main_frame, FRAME,
				    XV_LABEL, "Legends",
				    FRAME_SHOW_LABEL, TRUE,
		       WIN_ERROR_MSG, "Couldn't create define_legend_frame",
				    WIN_Y, 0,
				    0);
    define_legend_panel = xv_create(define_legend_frame, PANEL,
				    XV_HELP_DATA, "xvgr:legend_panel",
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    0);
    toggle_legends_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE,
					  XV_HELP_DATA, "xvgr:legend_onoff",
					      PANEL_LABEL_STRING, "Legend:",
						 PANEL_CHOICE_STRINGS,
						 "OFF",
						 "ON",
						 NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 0),
						 NULL);
    toggle_legendloc_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
					XV_HELP_DATA, "xvgr:legend_loctype",
				PANEL_LABEL_STRING, "Legend location type:",
						   PANEL_CHOICE_STRINGS,
						   "World coordinates",
						   "Viewport coordinates",
						   NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 1),
						   NULL);
    legend_font_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
					   XV_HELP_DATA, "xvgr:legend_font",
					      PANEL_LABEL_STRING, "Font:",
					      PANEL_CHOICE_STRINGS,
				"Times-Roman", "Times-Bold", "Times-Italic",
					    "Times-BoldItalic", "Helvetica",
				      "Helvetica-Bold", "Helvetica-Oblique",
				 "Helvetica-BoldOblique", "Greek", "Symbol",
					      NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 2),
					      NULL);
    legend_charsize_item = (Panel_item) xv_create(define_legend_panel, PANEL_SLIDER,
				       XV_HELP_DATA, "xvgr:legend_charsize",
						  PANEL_SLIDER_WIDTH, 100,
						  PANEL_SHOW_VALUE, TRUE,
						  PANEL_SHOW_RANGE, FALSE,
						  PANEL_MIN_VALUE, 0,
						  PANEL_MAX_VALUE, 400,
				      PANEL_LABEL_STRING, "Character size:",
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 3),
						  NULL);
    legend_color_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
					  XV_HELP_DATA, "xvgr:legend_color",
					       PANEL_LABEL_STRING, "Color:",
					       PANEL_CHOICE_NCOLS, 4,
					       PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
					       NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 4),
					       NULL);
    legends_gap_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
					    XV_HELP_DATA, "xvgr:legend_gap",
					  PANEL_LABEL_STRING, "Legend gap:",
					      PANEL_CHOICE_STRINGS,
					      "1", "2", "3", "4",
					      NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 5),
					      NULL);
    legends_len_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
					 XV_HELP_DATA, "xvgr:legend_length",
				       PANEL_LABEL_STRING, "Legend length:",
					      PANEL_CHOICE_STRINGS,
				     "1", "2", "3", "4", "5", "6", "7", "8",
					      NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 6),
					      NULL);
    legend_x_panel = (Panel_item) xv_create(define_legend_panel, PANEL_TEXT,
					    XV_HELP_DATA, "xvgr:legend_x",
					    PANEL_LAYOUT, PANEL_HORIZONTAL,
					    PANEL_VALUE_DISPLAY_LENGTH, 10,
					    PANEL_LABEL_STRING, "Legend X:",
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 7),
					    NULL);
    legend_y_panel = (Panel_item) xv_create(define_legend_panel, PANEL_TEXT,
					    XV_HELP_DATA, "xvgr:legend_y",
					    PANEL_LAYOUT, PANEL_HORIZONTAL,
					    PANEL_VALUE_DISPLAY_LENGTH, 10,
					    PANEL_LABEL_STRING, "Legend Y:",
			     PANEL_VALUE_X, xv_col(define_legend_panel, 23),
				       XV_Y, xv_row(define_legend_panel, 8),
					    NULL);
    legend_box_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHECK_BOX,
					     XV_HELP_DATA, "xvgr:legend_box",
					     PANEL_CHOICE_STRINGS,
					     "Frame",
					     NULL,
			      PANEL_VALUE_X, xv_col(define_legend_panel, 9),
				       XV_Y, xv_row(define_legend_panel, 9),
					     NULL);
    legend_boxfill_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHECK_BOX,
					XV_HELP_DATA, "xvgr:legend_boxfill",
						 PANEL_CHOICE_STRINGS,
						 "Fill frame",
						 NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 32),
				       XV_Y, xv_row(define_legend_panel, 9),
						 NULL);
    legend_boxfillusing_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
				   XV_HELP_DATA, "xvgr:legend_boxfillusing",
					  PANEL_LABEL_STRING, "Fill using:",
						      PANEL_CHOICE_STRINGS,
						      "Color", "Pattern",
						      NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 32),
				      XV_Y, xv_row(define_legend_panel, 10),
						      NULL);
    legend_boxfillcolor_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
				   XV_HELP_DATA, "xvgr:legend_boxfillcolor",
					       PANEL_LABEL_STRING, "Color:",
						      PANEL_CHOICE_NCOLS, 4,
						      PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
						      NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 32),
				      XV_Y, xv_row(define_legend_panel, 11),
						      NULL);
    legend_boxfillpat_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
				     XV_HELP_DATA, "xvgr:legend_boxfillpat",
					     PANEL_LABEL_STRING, "Pattern:",
						    PANEL_CHOICE_NCOLS, 4,
						    PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
						    NULL,
			     PANEL_VALUE_X, xv_col(define_legend_panel, 32),
				      XV_Y, xv_row(define_legend_panel, 12),
						    NULL);
    legend_boxcolor_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
				   XV_HELP_DATA, "xvgr:legend_boxlinecolor",
					       PANEL_LABEL_STRING, "Color:",
						  PANEL_CHOICE_NCOLS, 4,
						  PANEL_CHOICE_STRINGS,
		     "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
					       "11", "12", "13", "14", "15",
						  NULL,
			      PANEL_VALUE_X, xv_col(define_legend_panel, 9),
				      XV_Y, xv_row(define_legend_panel, 10),
						  NULL);
    legend_boxlinew_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
				   XV_HELP_DATA, "xvgr:legend_boxlinewidth",
					       PANEL_LABEL_STRING, "Width:",
						  PANEL_CHOICE_NCOLS, 3,
						  PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
						  NULL,
			      PANEL_VALUE_X, xv_col(define_legend_panel, 9),
				      XV_Y, xv_row(define_legend_panel, 11),
						  NULL);
    legend_boxlines_item = (Panel_item) xv_create(define_legend_panel, PANEL_CHOICE_STACK,
				   XV_HELP_DATA, "xvgr:legend_boxlinestyle",
					       PANEL_LABEL_STRING, "Style:",
						  PANEL_CHOICE_STRINGS,
						  "Solid",
						  "Dotted",
						  "Dashed",
						  "Long Dashed",
						  "Dot-dashed",
						  NULL,
			      PANEL_VALUE_X, xv_col(define_legend_panel, 9),
				      XV_Y, xv_row(define_legend_panel, 12),
						  NULL);

    (void) xv_create(define_legend_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:legend_accept",
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, define_legends_proc,
		     XV_X, xv_col(define_legend_panel, 0),
		     XV_Y, xv_row(define_legend_panel, 14),
		     NULL);
    (void) xv_create(define_legend_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:legend_place",
		     PANEL_LABEL_STRING, "Place",
		     PANEL_NOTIFY_PROC, legend_loc_proc,
		     XV_X, xv_col(define_legend_panel, 10),
		     XV_Y, xv_row(define_legend_panel, 14),
		     NULL);
    (void) xv_create(define_legend_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:legend_load",
		     PANEL_LABEL_STRING, "Load comments",
		     PANEL_NOTIFY_PROC, legend_load_proc,
		     XV_X, xv_col(define_legend_panel, 19),
		     XV_Y, xv_row(define_legend_panel, 14),
		     NULL);
    (void) xv_create(define_legend_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:legend_load",
		     PANEL_LABEL_STRING, "Edit...",
		     PANEL_NOTIFY_PROC, create_ledit_frame,
		     XV_X, xv_col(define_legend_panel, 36),
		     XV_Y, xv_row(define_legend_panel, 14),
		     NULL);
    (void) xv_create(define_legend_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:legend_cancel",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, define_legend_done_proc,
		     XV_X, xv_col(define_legend_panel, 46),
		     XV_Y, xv_row(define_legend_panel, 14),
		     NULL);
    updatelegends(cg);
    window_fit(define_legend_panel);
    window_fit(define_legend_frame);
    xv_set(define_legend_frame, WIN_SHOW, TRUE, NULL);
}

#define TPAGESIZE 10
#define NPAGES (MAXPLOT/TPAGESIZE)
static int tcurpage = 0;

static Panel_item ledit_item[MAXPLOT];

static ledit_Done_notify_proc()
{
    xv_set(ledit_frame, WIN_SHOW, FALSE, 0);
}

static void accept_ledit_proc()
{
    int iv, i, j;

    for (i = 0; i < MAXPLOT; i++) {
	strcpy(g[cg].l.str[i].s, (char *) xv_get(ledit_item[i], PANEL_VALUE));
    }
    drawgraph();
}

static void update_ledit_items(gno)
    int gno;
{
    tickmarks t;
    int i, j, itmp;

    if (ledit_frame) {
	if (tcurpage == 0) {
	    itmp = (NPAGES - 1) * TPAGESIZE;
	} else {
	    itmp = (tcurpage - 1) * TPAGESIZE;
	}
	for (i = tcurpage * TPAGESIZE; i < TPAGESIZE * (tcurpage + 1); i++) {
	    xv_set(ledit_item[itmp++], XV_SHOW, FALSE, NULL);
	    xv_set(ledit_item[i], XV_SHOW, TRUE, NULL);
	}
    }
}

static void load_ledit(gno, which)
    int gno, which;
{
    int i;

    if (ledit_frame) {
	if (which >= 0) {
	    xv_setstr(ledit_item[which], g[gno].l.str[which].s);
	} else {
	    for (i = 0; i < MAXPLOT; i++) {
		xv_setstr(ledit_item[i], g[gno].l.str[i].s);
	    }
	}
    }
}

static void page_ledit_notify_proc()
{
    tcurpage = (tcurpage + 1) % NPAGES;
    update_ledit_items(cg);
}

void create_ledit_frame()
{
    int i;
    char buf[128];

    if (ledit_frame) {
	update_ledit_items(cg);
	xv_set(ledit_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    ledit_frame = xv_create(main_frame, FRAME,
			    XV_LABEL, "Edit legend labels",
			    FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create ledit_frame",
			    NULL);
    ledit_panel = xv_create(ledit_frame, PANEL,
			    XV_HELP_DATA, "xvgr:ledit_panel",
			    PANEL_LAYOUT, PANEL_VERTICAL,
			    NULL);

    (void) xv_create(ledit_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Page",
		     XV_HELP_DATA, "xvgr:ledit_page",
		     PANEL_NOTIFY_PROC, page_ledit_notify_proc,
		     XV_X, xv_col(ledit_panel, 0),
		     XV_Y, xv_row(ledit_panel, 0),
		     NULL);
    (void) xv_create(ledit_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Legend labels:",
		     XV_HELP_DATA, "xvgr:leditticks_ticklableloc",
		     XV_X, xv_col(ledit_panel, 0),
		     XV_Y, xv_row(ledit_panel, 1),
		     NULL);

    for (i = 0; i < MAXPLOT; i++) {
	sprintf(buf, "Set %2d:", i + 1);
	ledit_item[i] = (Panel_item) xv_create(ledit_panel, PANEL_TEXT,
					       PANEL_LABEL_STRING, buf,
				      XV_HELP_DATA, "xvgr:leditticks_label",
					     PANEL_VALUE_DISPLAY_LENGTH, 30,
				      XV_SHOW, i < TPAGESIZE ? TRUE : FALSE,
					       XV_X, xv_col(ledit_panel, 0),
			       XV_Y, xv_row(ledit_panel, i % TPAGESIZE + 2),
					       NULL);
    }

    (void) xv_create(ledit_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:leditticks_cancel",
		     PANEL_NOTIFY_PROC, ledit_Done_notify_proc,
		     XV_X, xv_col(ledit_panel, 15),
		     XV_Y, xv_row(ledit_panel, 14),
		     NULL);
    (void) xv_create(ledit_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     XV_HELP_DATA, "xvgr:leditticks_accept",
		     PANEL_NOTIFY_PROC, accept_ledit_proc,
		     XV_X, xv_col(ledit_panel, 5),
		     XV_Y, xv_row(ledit_panel, 14),
		     NULL);
    load_ledit(cg, -1);
    update_ledit_items(cg);
    window_fit(ledit_panel);
    window_fit(ledit_frame);
    xv_set(ledit_frame, WIN_SHOW, TRUE, 0);
}
