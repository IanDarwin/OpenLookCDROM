
/*
	grtool.c - sunview stuff, Paul Turner - July 1988

	$Header: grtool.c,v 1.23 89/09/12 05:10:27 pturner Locked $
*/

extern int win_h, win_w;

static char buf[256];		/* string used here and there */

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sunwindow/attr.h>
#include <sunwindow/defaults.h>
#include <suntool/frame.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <suntool/scrollbar.h>
#include <pixrect/pixrect.h>
#include <suntool/tty.h>
#include <suntool/textsw.h>
#include <suntool/alert.h>
#include "globals.h"
#include "defines.h"
#include "objdefs.h"

static short grtool_icon[] = {
#include "icon.h"
};

DEFINE_ICON_FROM_IMAGE(frame_icon, grtool_icon);

#define PIX_XOR PIX_SRC^PIX_DST

#define define_select_set_panel3(panel,panel_item,x,y) \
				panel_item=panel_create_item( panel,\
				PANEL_CYCLE,\
				PANEL_LABEL_STRING,"Select set:",\
	 			PANEL_CHOICE_STRINGS,\
			"Set 0", "Set 1", "Set 2", "Set 3", "Set 4", "Set 5",\
			"Set 6", "Set 7", "Set 8", "Set 9", "Set 10", "Set 11",\
			"Set 12", "Set 13", "Set 14", 0,\
	    		PANEL_ITEM_X,x,\
	    		PANEL_ITEM_Y,y,\
			0 );

int auto_redraw = 1;		/* if auto draw when things change */
int force_redraw = 0;		/* if no auto draw and re-draw pressed */
int rectflag = 0;		/* if an xor'ed rectangle is drawn with mouse */
int rubber_flag = 0;		/* set rubber band line */
int mbox_flag = 0;		/* moving box attached to cursor */
int mline_flag = 0;		/* moving line attached to cursor */
int go_locateflag = 1;		/* locator */

static void create_defaults_popup();	/* set grtool defaults */
static void define_tics_popup();/* define tics */
static void define_symbols_popup();	/* define set symbols and entry to
					 * legends */
static void define_compute_popup();	/* transformations */

static void updateparms();	/* update world, view windows if any changes
				 * made during mouse controlled zoom */
static void updatetics();	/* update tic flags */

void drawgraph();		/* calls plotone(), called any time the graph
				 * /* needs to be re-drawn /* not used as a
				 * canvas redraw proc */

void define_edit_popup();	/* edit points */
extern Frame points_frame;

void define_strings_popup();	/* strings popup, routines in strwin.c */
extern Frame strings_frame;	/* in strwin.c */

void define_setops_popup();	/* set operations, routines in setwin.c */
void do_strings_popup();	/* a routine to set WIN_SHOW=TRUE */
extern Frame setops_frame;	/* in setwin.c */

/*
* canvas to draw graph and associated pixwin
*/
Canvas canvas;
Pixwin *pw;

Pixfont *winfont;		/* font to use for all of grtool */

Cursor cursor, cursor_save;	/* cursor for zoom */
Cursor cursor_move;
Cursor cursor_del;
Cursor cursor_wait;

static short int move_bits[] = {
#include <images/move.cursor>
};

static short int del_bits[] = {
#include <images/bullseye.cursor>
};

static short int wait_bits[] = {
#include <images/hglass.cursor>
};

mpr_static(move_cursor, 16, 16, 1, move_bits);
mpr_static(del_cursor, 16, 16, 1, del_bits);
mpr_static(wait_cursor, 16, 16, 1, wait_bits);

Window main_frame, main_panel;	/* main frame and panel */

static void create_tty_popup();	/* text window for status and reports */
static Textsw textsw;		/* for status */
static Frame tty_frame;
static Panel tty_panel;
static Panel session_fname_item;/* file name to save contents of status
				 * textsw */
static void do_tty_proc();

static void define_status_popup();
static void do_status_proc();
static Frame status_frame;
static Frame status_panel;
static Panel_item header_item1;
static Panel_item status_items_x[MAXPLOT];
static Panel_item status_items_y[MAXPLOT];

static Menu files_menu;
static void create_readp_popup();	/* read parameter files */
static void create_writep_popup();	/* write parameter files */
static void create_readdata_popup();	/* write parameter files */
static Frame readdata_frame;
static Panel readdata_panel;
static Panel readdata_fname_item;
static Panel readdata_dir_item;
static Panel toggle_readdata_item;

void define_compose_popup();	/* transformations, etc. */
extern Frame compose_frame;

/* world should really be window */
static void define_world_popup();	/* define the world dimensions */
static Frame define_world_frame;
static Panel define_world_panel;
static Panel_item define_world_xg1;
static Panel_item define_world_xg2;
static Panel_item define_world_yg1;
static Panel_item define_world_yg2;
static Panel_item define_view_xv1;
static Panel_item define_view_xv2;
static Panel_item define_view_yv1;
static Panel_item define_view_yv2;
static Panel_item autoscale_set_item;
static Panel_item autoscale_type_item;
static Panel_item auto_redraw_item;

static Frame define_tics_frame;
static Panel define_tics_panel;
static Panel_item define_tics_xmajor;
static Panel_item define_tics_xminor;
static Panel_item define_tics_ymajor;
static Panel_item define_tics_yminor;
static Panel_item define_tics_xform;
static Panel_item define_tics_yform;
static Panel_item define_xtics_angle;
static Panel_item toggle_xtics_item;
static Panel_item toggle_ytics_item;
static Panel_item title_item;
static Panel_item subtitle_item;
static Panel_item xaxis_item;
static Panel_item yaxis_item;

static Frame define_symbols_frame;
static Panel define_symbols_panel;
static Panel_item toggle_symbols_item[MAXPLOT];
static Panel_item toggle_pen_item[MAXPLOT];
static Panel_item toggle_lines_item[MAXPLOT];
static Panel_item toggle_legends_item;
static Panel_item legend_str_panel[MAXPLOT];
static Panel_item legends_gap_item;
static Panel_item legends_len_item;
static Panel_item legend_x_panel;
static Panel_item legend_y_panel;
static Panel_item error_from_set;
static Panel_item error_to_set;
static Panel_item error_xy_item;

static Panel locate_item;

static Menu draw_menu;
static Menu compose_menu;
static Menu devices_menu;
static Menu defaults_menu;

static Panel_item defaults_font_item;

/* lines and boxes flags */
int box_color = 0, line_color = 0, line_arrow = 0;

#ifdef LOCAL
int hpgl_loaded = 0;

#endif

bailout()
{
#ifdef LOCAL
    if (hpgl_loaded) {
	if (!yesno("Exit without loading the T3 fonts?", "", "YES", "NO")) {
	    system("dopcl");
	}
    }
#endif
    window_set(main_frame, FRAME_NO_CONFIRM, TRUE, 0);
    window_destroy(main_frame);
    exit(0);
}

static quit_main_proc(item, event)
    Panel_item item;
    Event *event;
{
    bailout();
}

void do_draw_pulldown(item, event)
    Panel_item item;
    Event *event;
{
    int choice;

    if ((event_id(event) == MS_RIGHT) && event_is_down(event)) {
	choice = (int) menu_show(draw_menu, main_panel, event, 0);
	switch (choice) {
	case 1:
	    window_set(define_world_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 2:
	    break;
	case 3:
	    window_set(define_tics_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 4:
	    window_set(define_symbols_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 5:
	    window_set(strings_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 7:
	    if (activeset()) {
		defaultgraph();
		defaulttics((int) panel_get_value(autoscale_type_item));
		updateparms();
		drawgraph();
	    } else
		errwin("No active sets!");
	    break;
	case 9:
	    set_action(0);
	    set_action(ZOOM_1ST);
	    window_set(canvas, WIN_CURSOR, cursor, 0);
	    break;
	case 11:
	    if (activeset()) {
		flipxy();
		drawgraph();
	    } else
		errwin("No active sets!");
	    break;
	default:
	    break;
	}
    } else
	panel_default_handle_event(item, event);
}

flipxy()
{
    int i, j;
    double *x, *y, *getx(), *gety();
    double x1, y1, x2, y2;

    for (i = 0; i < maxplot; i++) {
	if (isactive(i)) {
	    x = getx(i);
	    y = gety(i);
	    for (j = 0; j < getsetlength(i); j++) {
		fswap(&x[j], &y[j]);
	    }
	    getsetminmax(i, &y1, &y2, &x1, &x2);
	    setminmax(i, x1, x2, y1, y2);
	}
    }
    fswap(&xg1, &yg1);
    fswap(&xg2, &yg2);
    fswap(&xt1, &yt1);
    fswap(&xt2, &yt2);
    iswap(&xform, &yform);
    iswap(&fformx, &fformy);
    iswap(&xticsintflag, &yticsintflag);
    iswap(&xticlflag, &yticlflag);
    iswap(&xticslog, &yticslog);
    iswap(&xticflag, &yticflag);
    iswap(&xtopflag, &ytopflag);
    iswap(&xabsflag, &yabsflag);
    iswap(&xticinoutflag, &yticinoutflag);
    iswap(&xticopflag, &yticopflag);
    iswap(&xgridflag, &ygridflag);
    iswap(&xzflag, &yzflag);
    strcpy(buf, xlabel);
    strcpy(xlabel, ylabel);
    strcpy(ylabel, buf);
    (void) updateparms();
}

void do_files_pulldown(item, event)
    Panel_item item;
    Event *event;
{
    int choice;

    if (event_id(event) == MS_RIGHT && event_is_down(event)) {
	choice = (int) menu_show(files_menu, main_panel, event, 0);
	switch (choice) {
	case 1:
	    create_readdata_popup();
	    break;
	case 2:
	    create_readp_popup();
	    break;
	case 3:
	    create_writep_popup();
	    break;
	default:
	    break;
	}
    } else
	panel_default_handle_event(item, event);
}

void do_compose_pulldown(item, event)
    Panel_item item;
    Event *event;
{
    int choice;

    if (event_id(event) == MS_RIGHT && event_is_down(event)) {
	choice = (int) menu_show(compose_menu, main_panel, event, 0);
	switch (choice) {
	case 1:
	    window_set(compose_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 2:
	    window_set(setops_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 3:
	    window_set(points_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 4:
	    break;
	default:
	    break;
	}
    } else
	panel_default_handle_event(item, event);
}

void do_devices_pulldown(item, event)
    Panel_item item;
    Event *event;
{
    int choice;

    if (event_id(event) == MS_RIGHT && event_is_down(event)) {
	choice = (int) menu_show(devices_menu, main_panel, event, 0);
	switch (choice) {
	case 1:
	    hdevice = 1;	/* HP 7550 8.5x11 landscape */
	    break;
	case 2:
	    hdevice = 2;	/* HP 7550 8.5x11 portrait */
	    break;
	case 3:
	    hdevice = 3;	/* HP 7550 11x17 landscape */
	    break;
	case 4:
	    hdevice = 4;	/* HP 7550 11x17 portrait */
	    break;
	case 5:
	    hdevice = 5;	/* Generic */
	    break;
	case 6:
	    hdevice = 6;	/* PostScript landscape */
	    break;
	case 7:
	    hdevice = 7;	/* PostScript portrait */
	    break;
	case 8:
#ifdef LOCAL
	    if (!hpgl_loaded) {
		if (yesno("Load HPGL?", "", "YES", "NO")) {
		    system("dohpgl");
		}
	    }
	    hpgl_loaded = 1;
#endif
	    hdevice = 8;	/* HPGL cartridge in LaserJet II landscape */
	    break;
	case 9:
#ifdef LOCAL
	    if (!hpgl_loaded) {
		if (yesno("Load HPGL?", "", "YES", "NO")) {
		    system("dohpgl");
		}
	    }
	    hpgl_loaded = 1;
#endif
	    hdevice = 9;	/* HPGL cartridge in LaserJet II portrait */
	    break;
	case 10:
	    hdevice = 10;	/* Rasterfile landscape */
	    break;
	case 11:
	    hdevice = 11;	/* Rasterfile portrait */
	    break;
	default:
	    hdevice = 1;
	    break;
	}
    } else
	panel_default_handle_event(item, event);
}

void do_defaults_pulldown(item, event)
    Panel_item item;
    Event *event;
{
    static void results_file_proc();
    int choice;

    if (event_id(event) == MS_RIGHT && event_is_down(event)) {
	choice = (int) menu_show(defaults_menu, main_panel, event, 0);
	switch (choice) {
	case 1:
	    results_file_proc();
	    break;
	default:
	    break;
	}
    } else
	panel_default_handle_event(item, event);
}

void forcedrawgraph()
{
    force_redraw = 1;
    drawgraph();
}

void drawgraph()
{
    auto_redraw = !((int) panel_get_value(auto_redraw_item));
    if (auto_redraw || hardcopyflag || force_redraw) {
	if (!activeset()) {
	    errwin("Warning, no active sets");
	}
	if ((xv1 == xv2 || yv1 == yv2)) {
	    errwin("Viewport improperly defined");
	    hardcopyflag = FALSE;
	    return;
	}
	if ((yg1 == yg2 || xg1 == xg2)) {
	    errwin("World improperly defined");
	    hardcopyflag = FALSE;
	    return;
	}
	if (xt1 == 0.0) {
	    errwin("X-major tics = 0.0");
	    hardcopyflag = FALSE;
	    return;
	} else if ((xg2 - xg1) / xt1 > 100.0) {
	    if (yesno("Number of X-major tics >100, abort?", "", "YES", "NO")) {
		hardcopyflag = FALSE;
		return;
	    }
	}
	if (yt1 == 0.0) {
	    errwin("Y-major tics = 0.0");
	    hardcopyflag = FALSE;
	    return;
	} else if ((yg2 - yg1) / yt1 > 100.0) {
	    if (yesno("Number of Y-major tics >100, abort?", "", "YES", "NO")) {
		hardcopyflag = FALSE;
		return;
	    }
	}
	if (xt2 == 0.0) {
	    errwin("X-minor tics = 0.0");
	    hardcopyflag = FALSE;
	    return;
	} else if ((xg2 - xg1) / xt2 > 200.0) {
	    if (yesno("Number of X-minor tics >200, abort?", "", "YES", "NO")) {
		hardcopyflag = FALSE;
		return;
	    }
	}
	if (yt2 == 0.0) {
	    errwin("Y-minor tics = 0.0");
	    hardcopyflag = FALSE;
	    return;
	} else if ((yg2 - yg1) / yt2 > 200.0) {
	    if (yesno("Number of Y-minor tics >200, abort?", "", "YES", "NO")) {
		hardcopyflag = FALSE;
		return;
	    }
	}
	plotone();
	hardcopyflag = FALSE;
    }
    force_redraw = 0;
}

select_line(pw, x1, y1, x2, y2)
    struct pixwin *pw;
    int x1, y1, x2, y2;
{
    pw_vector(pw, x1, y1, x2, y2, PIX_XOR, 1);
}

/*
 * draw a rectangle on the bitmap
 */
draw_rectangle(x1, y1, x2, y2)
    int x1, y1, x2, y2;
{
    pw_vector(pw, x1, y1, x2, y1, PIX_SRC, 1);
    pw_vector(pw, x2, y1, x2, y2, PIX_SRC, 1);
    pw_vector(pw, x2, y2, x1, y2, PIX_SRC, 1);
    pw_vector(pw, x1, y2, x1, y1, PIX_SRC, 1);
}

/*
 * highlight the selected region on the drawing area by throwing up a
 * XORed rectangle
 */
select_region(pw, x1, y1, x2, y2)
    struct pixwin *pw;
    int x1, y1, x2, y2;
{
    pw_vector(pw, x1, y1, x2, y1, PIX_XOR, 1);
    pw_vector(pw, x2, y1, x2, y2, PIX_XOR, 1);
    pw_vector(pw, x2, y2, x1, y2, PIX_XOR, 1);
    pw_vector(pw, x1, y2, x1, y1, PIX_XOR, 1);
}

getpoints(x, y)
{
    double wx, wy;

    get_world(x, y, &wx, &wy);
    sprintf(buf, "(%.6g, %.6g)", wx, wy);
    panel_set_value(locate_item, buf);
}

static int sx, sy;
static int old_x, old_y;
static int xs, ys;

int ty, no;

int action_flag = 0;

set_action(act)
    int act;
{
    if ((action_flag = act) == 0) {
	window_set(canvas, WIN_CURSOR, cursor_save, 0);
	if (rectflag) {
	    select_region(pw, sx, sy, old_x, old_y);
	    rectflag = 0;
	}
	if (rubber_flag) {
	    pw_vector(pw, sx, sy, old_x, old_y, PIX_XOR, 1);
	    rubber_flag = 0;
	}
	if (mbox_flag) {
	    select_region(pw, sx, sy, xs, ys);
	    mbox_flag = 0;
	}
	if (mline_flag) {
	    select_line(pw, sx, sy, xs, ys);
	    mline_flag = 0;
	}
    }
}

void my_proc(canvas, event)
    Canvas canvas;
    Event *event;
{
    static int x, y, boxno, lineno;
    static double wx1, wx2, wy1, wy2;
    static double wx, wy, dx, dy;
    extern Panel_item strings_x_item;
    extern Panel_item strings_y_item;

    if (event_is_up(event))
	return;
    if (event_is_ascii(event)) {
	switch (event_action(event)) {
	case 1:		/* ^A */
	    if (activeset()) {
		defaultgraph();
		defaulttics((int) panel_get_value(autoscale_type_item));
		updateparms();
		drawgraph();
	    } else
		errwin("No active sets!");
	    break;
	case 3:		/* ^C */
	    window_set(compose_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 7:		/* ^G */
	    window_set(define_world_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 16:		/* ^P */
	    create_readp_popup();
	    break;
	case 18:		/* ^R */
	    create_readdata_popup();
	    break;
	case 19:		/* ^S */
	    window_set(setops_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 20:		/* ^T */
	    window_set(define_tics_frame, WIN_SHOW, TRUE, 0);
	    break;
	case 23:		/* ^W */
	    create_writep_popup();
	    break;
	case 24:		/* ^X */
	    bailout();
	    break;
	case 26:		/* ^Z */
	    set_action(0);
	    set_action(ZOOM_1ST);
	    window_set(canvas, WIN_CURSOR, cursor, 0);
	    break;
	}
	/*
	 * get_world(x, y, &wx, &wy); writestr(wx,wy,0, strbuf);
	 */
	return;
    }
    x = event_x(event);
    y = event_y(event);
    switch (event_id(event)) {
    case MS_RIGHT:		/* cancel all actions */
	set_action(0);
	break;
    case MS_LEFT:
	switch (action_flag) {
	case DEL_OBJECT:
	    set_action(0);
	    get_world(x, y, &wx, &wy);
	    find_item(wx, wy, &ty, &no);
	    window_set(canvas, WIN_CURSOR, cursor_save, 0);
	    if (ty >= 0) {
		switch (ty) {
		case BOX:
		    kill_box(no);
		    drawgraph();
		    break;
		case LINE:
		    kill_line(no);
		    drawgraph();
		    break;
		}
	    }
	    break;
	case MOVE_OBJECT_1ST:
	    set_action(MOVE_OBJECT_2ND);
	    get_world(x, y, &wx, &wy);
	    find_item(wx, wy, &ty, &no);
	    if (ty < 0) {
		window_set(canvas, WIN_CURSOR, cursor_save, 0);
	    } else {
		switch (ty) {
		case BOX:
		    get_device(boxes[no].x1, boxes[no].y1, &sx, &sy);
		    get_device(boxes[no].x2, boxes[no].y2, &xs, &ys);
		    select_region(pw, sx, sy, xs, ys);
		    mbox_flag = 1;
		    break;
		case LINE:
		    get_device(lines[no].x1, lines[no].y1, &sx, &sy);
		    get_device(lines[no].x2, lines[no].y2, &xs, &ys);
		    select_line(pw, sx, sy, xs, ys);
		    mline_flag = 1;
		    break;
		}
	    }
	    break;
	case MOVE_OBJECT_2ND:
	    dx = sx - x;
	    dy = sy - y;

	    window_set(canvas, WIN_CURSOR, cursor_save, 0);
	    set_action(0);
	    sx = x;
	    sy = y;
	    xs = xs - dx;
	    ys = ys - dy;
	    get_world(sx, sy, &wx1, &wy1);
	    get_world(xs, ys, &wx2, &wy2);
	    switch (ty) {
	    case BOX:
		boxes[no].x1 = wx1;
		boxes[no].x2 = wx2;
		boxes[no].y1 = wy1;
		boxes[no].y2 = wy2;
		break;
	    case LINE:
		lines[no].x1 = wx1;
		lines[no].x2 = wx2;
		lines[no].y1 = wy1;
		lines[no].y2 = wy2;
		break;
	    }
	    drawgraph();
	    break;
	case MAKE_BOX_1ST:
	    set_action(MAKE_BOX_2ND);
	    rectflag = 1;
	    sx = x;
	    sy = y;
	    select_region(pw, sx, sy, x, y);
	    break;
	case MAKE_BOX_2ND:

	    set_action(0);
	    get_world(sx, sy, &wx1, &wy1);
	    get_world(x, y, &wx2, &wy2);
	    if ((boxno = next_box()) >= 0) {
		boxes[boxno].x1 = wx1;
		boxes[boxno].x2 = wx2;
		boxes[boxno].y1 = wy1;
		boxes[boxno].y2 = wy2;
		boxes[boxno].color = box_color;
		boxes[boxno].style = 1;
		setcolor(boxes[boxno].color);
		setclipping(FALSE);
		move2(boxes[boxno].x1, boxes[boxno].y1);
		draw2(boxes[boxno].x1, boxes[boxno].y2);
		draw2(boxes[boxno].x2, boxes[boxno].y2);
		draw2(boxes[boxno].x2, boxes[boxno].y1);
		draw2(boxes[boxno].x1, boxes[boxno].y1);
		setcolor(1);
		setclipping(TRUE);
	    }
	    break;
	case MAKE_LINE_1ST:
	    set_action(MAKE_LINE_2ND);
	    rubber_flag = 1;
	    sx = x;
	    sy = y;
	    pw_vector(pw, sx, sy, x, y, PIX_XOR, 1);
	    break;
	case MAKE_LINE_2ND:
	    set_action(0);
	    if ((lineno = next_line()) >= 0) {
		get_world(sx, sy, &wx1, &wy1);
		get_world(x, y, &wx2, &wy2);
		lines[lineno].x1 = wx1;
		lines[lineno].x2 = wx2;
		lines[lineno].y1 = wy1;
		lines[lineno].y2 = wy2;
		lines[lineno].color = line_color;
		lines[lineno].arrow = line_arrow;
		setcolor(lines[lineno].color);
		draw_arrow(wx1, wy1, wx2, wy2, lines[lineno].arrow);
		setcolor(1);
	    }
	    break;
	case STR_LOC:
	    get_world(x, y, &wx, &wy);
	    sprintf(buf, "%.6g", wx);
	    panel_set_value(strings_x_item, buf);
	    sprintf(buf, "%.6g", wy);
	    panel_set_value(strings_y_item, buf);
	    set_action(0);
	    break;
	case FIND_POINT:
	    {
		int setno, loc;
		extern Panel_item locate_point_item;

		get_world(x, y, &wx, &wy);
		findpoint(wx, wy, &wx, &wy, &setno, &loc);
		sprintf(buf, "Set %d, loc %d, (%lf, %lf)", setno, loc, wx, wy);
		panel_set_value(locate_point_item, buf);
		set_action(FIND_POINT);
	    }
	    break;
	case DEL_POINT:
	    {
		int setno, loc;

		get_world(x, y, &wx, &wy);
		findpoint(wx, wy, &wx, &wy, &setno, &loc);
		if (setno >= 0) {
		    del_point(setno, loc);
		    update_status(setno);
		}
		set_action(0);
	    }
	    break;
	case LEG_LOC:
	    get_world(x, y, &wx, &wy);
	    sprintf(buf, "%.6g", wx);
	    panel_set_value(legend_x_panel, buf);
	    sprintf(buf, "%.6g", wy);
	    panel_set_value(legend_y_panel, buf);
	    set_action(0);
	    break;
	case ZOOM_1ST:
	    set_action(ZOOM_2ND);
	    rectflag = 1;
	    sx = x;
	    sy = y;
	    select_region(pw, x, y, x, y);
	    break;
	case ZOOM_2ND:
	    set_action(0);
	    select_region(pw, sx, sy, old_x, old_y);
	    get_world(sx, sy, &wx1, &wy1);
	    get_world(old_x, old_y, &wx2, &wy2);
	    if (wx1 > wx2)
		fswap(&wx1, &wx2);
	    if (wy1 > wy2)
		fswap(&wy1, &wy2);
	    xg1 = wx1;
	    xg2 = wx2;
	    yg1 = wy1;
	    yg2 = wy2;
	    defaulttics((int) panel_get_value(autoscale_type_item));
	    drawgraph();
	    break;
	case VIEW_1ST:
	    set_action(VIEW_2ND);
	    rectflag = 1;
	    sx = x;
	    sy = y;
	    select_region(pw, x, y, x, y);
	    break;
	case VIEW_2ND:
	    {
		double vx1, vx2, vy1, vy2;

		set_action(0);
		select_region(pw, sx, sy, old_x, old_y);
		get_world(sx, sy, &wx1, &wy1);
		get_world(old_x, old_y, &wx2, &wy2);
		get_view(wx1, wy1, &vx1, &vy1);
		get_view(wx2, wy2, &vx2, &vy2);
		if (vx1 > vx2)
		    fswap(&vx1, &vx2);
		if (vy1 > vy2)
		    fswap(&vy1, &vy2);
		xv1 = vx1;
		xv2 = vx2;
		yv1 = vy1;
		yv2 = vy2;
		drawgraph();
	    }
	    break;
	}
	break;
    case MS_MIDDLE:		/* not used? */
	break;
    case LOC_MOVE:
    case LOC_WINENTER:
	if (go_locateflag)
	    getpoints(x, y);
	break;
    case LOC_WINEXIT:
	break;
    default:
	break;
    }
    switch (action_flag) {
    case MOVE_OBJECT_2ND:
	dx = sx - x;
	dy = sy - y;

	switch (ty) {
	case BOX:
	    select_region(pw, sx, sy, xs, ys);
	    sx = x;
	    sy = y;
	    xs = xs - dx;
	    ys = ys - dy;
	    select_region(pw, sx, sy, xs, ys);
	    break;
	case LINE:
	    select_line(pw, sx, sy, xs, ys);
	    sx = x;
	    sy = y;
	    xs = xs - dx;
	    ys = ys - dy;
	    select_line(pw, sx, sy, xs, ys);
	    break;
	}
	break;
    case STR_LOC:
	get_world(x, y, &wx, &wy);
	sprintf(buf, "%.6g", wx);
	panel_set_value(strings_x_item, buf);
	sprintf(buf, "%.6g", wy);
	panel_set_value(strings_y_item, buf);
	break;
    case LEG_LOC:
	get_world(x, y, &wx, &wy);
	sprintf(buf, "%.6g", wx);
	panel_set_value(legend_x_panel, buf);
	sprintf(buf, "%.6g", wy);
	panel_set_value(legend_y_panel, buf);
	break;
    }
    if (rectflag) {
	select_region(pw, sx, sy, old_x, old_y);
	select_region(pw, sx, sy, x, y);
    }
    if (rubber_flag) {
	pw_vector(pw, sx, sy, old_x, old_y, PIX_XOR, 1);
	pw_vector(pw, sx, sy, x, y, PIX_XOR, 1);
    }
    old_x = x;
    old_y = y;
}

static void my_redraw_proc(c, w, h)
    Canvas c;
    int w, h;
{
    win_h = h;
    win_w = w;
    drawgraph();
}

static void do_hardcopy()
{
    hardcopyflag = TRUE;
    drawgraph();
}

FILE *resfp = stdout;

static void results_file_proc()
{
    if (get_msg(resfile, buf, "Results to", "File:")) {
	if (resfp != stdout) {
	    fclose(resfp);
	}
	if (strcmp(resfile, buf)) {
	    strcpy(resfile, buf);
	    if (!fexists(resfile)) {
		resfp = fopen(resfile, "w");
		if (resfp == NULL) {
		    errwin("Can't open results file, using stdout");
		    resfp = stdout;
		}
	    } else {
		errwin("Writing results to stdout");
		resfp = stdout;
	    }
	}
    }
}

do_main_loop(argc, argv)
    int argc;
    char **argv;
{
    extern char *version;
    int i;

    winfont = pf_open("/usr/lib/fonts/fixedwidthfonts/cour.b.12");
    putenv("DEFAULT_FONT=/usr/lib/fonts/fixedwidthfonts/cour.b.12");
    inwin = TRUE;
    main_frame = window_create(NULL, FRAME,
			       FRAME_ARGS, argc, argv,
			       FRAME_ICON, &frame_icon,
			       WIN_WIDTH, WINDOWW,
			       WIN_HEIGHT, WINDOWH,
			       WIN_X, 0,
			       WIN_Y, 0,
			       WIN_ERROR_MSG, "Couldn't create main_frame",
			       FRAME_LABEL, version, 0);
    main_panel = window_create(main_frame, PANEL, 0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Files", 0, winfont),
		      PANEL_EVENT_PROC, do_files_pulldown,
		      0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		 panel_button_image(main_panel, "Draw Options", 0, winfont),
		      PANEL_EVENT_PROC, do_draw_pulldown,
		      0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Compose", 0, winfont),
		      PANEL_EVENT_PROC, do_compose_pulldown,
		      0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Re-draw", 0, winfont),
		      PANEL_NOTIFY_PROC, forcedrawgraph,
		      0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Hardcopy", 0, winfont),
		      PANEL_NOTIFY_PROC, do_hardcopy,
		      0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Devices", 0, winfont),
		      PANEL_EVENT_PROC, do_devices_pulldown,
		      0);
/* soon to be added - PJT
    devices_item = panel_create_item(main_panel, PANEL_CYCLE,
					 PANEL_LABEL_STRING, "Device:",
					     PANEL_CHOICE_STRINGS,
			       "HP 7550 8.5x11 landscape",
			       "HP 7550 8.5x11 portrait",
			       "HP 7550 11x17 landscape",
			       "HP 7550 11x17 portrait",
			       "PostScript landscape",
			       "PostScript portrait",
			       "HPGL to LaserJet landscape",
			       "HPGL to Laserjet portrait",
			       "Generic",
			       "Rasterfile", 0, 0);
*/
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Status", 0, winfont),
		      PANEL_NOTIFY_PROC, do_status_proc,
		      0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Defaults", 0, winfont),
		      PANEL_EVENT_PROC, do_defaults_pulldown,
		      0);
    locate_item = panel_create_item(main_panel, PANEL_TEXT,
				    PANEL_LABEL_STRING, "(X,Y):",
				    PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    panel_create_item(main_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(main_panel, "Quit", 0, winfont),
		      PANEL_NOTIFY_PROC, quit_main_proc,
		      0);
    window_fit_height(main_panel);
    canvas = window_create(main_frame, CANVAS,
			   CANVAS_AUTO_SHRINK, TRUE,
			   CANVAS_WIDTH, WINDOWW,
			   CANVAS_HEIGHT, WINDOWH,
			   CANVAS_RESIZE_PROC, my_redraw_proc,
			   WIN_EVENT_PROC, my_proc,
			   WIN_CONSUME_PICK_EVENT, WIN_IN_TRANSIT_EVENTS,
			   WIN_CONSUME_KBD_EVENT, WIN_ASCII_EVENTS,
			   WIN_CONSUME_KBD_EVENT, WIN_RIGHT_KEYS,
			   0);
    files_menu = menu_create(MENU_STRINGS,
			     "Read data file",
			     "Read parameter file",
			     "Write parameter file",
			     0, 0);
    draw_menu = menu_create(MENU_STRINGS,
			    "Define world/view",
			    "",
			    "Tics and labels",
			    "Symbols & legends",
			    "Strings & things",
			    "",
			    "Autoscale",
			    "",
			    "Zoom",
			    "",
			    "Flip X-Y",
			    0, 0);
    compose_menu = menu_create(MENU_STRINGS,
			       "Transformations",
			       "Setops",
			       "Edit points",
			       0, 0);
    devices_menu = menu_create(MENU_STRINGS,
			       "HP 7550 8.5x11 landscape",
			       "HP 7550 8.5x11 portrait",
			       "HP 7550 11x17 landscape",
			       "HP 7550 11x17 portrait",
			       "Generic",
			       "PostScript landscape",
			       "PostScript portrait",
			       "HPGL to LaserJet landscape",
			       "HPGL to Laserjet portrait",
			       "Rasterfile landscape",
			       "Rasterfile portrait",
			       0, 0);
    defaults_menu = menu_create(MENU_STRINGS,
				"Results file",
				0, 0);
    pw = canvas_pixwin(canvas);

    win_h = (int) window_get(canvas, CANVAS_HEIGHT);
    win_w = (int) window_get(canvas, CANVAS_WIDTH);

    define_world_popup();
    define_tics_popup();
    define_symbols_popup();
    define_strings_popup();

    define_points_popup();
    define_compose_popup();
    define_setops_popup();
    define_status_popup();

    cursor_save = cursor_copy(window_get(canvas, WIN_CURSOR));
    cursor = cursor_create(CURSOR_SHOW_CROSSHAIRS, TRUE,
			   CURSOR_OP, PIX_SRC ^ PIX_DST,
			   CURSOR_CROSSHAIR_THICKNESS, 1,
			   CURSOR_CROSSHAIR_LENGTH, CURSOR_TO_EDGE,
			   CURSOR_CROSSHAIR_GAP, 0, 0);
    cursor_move = cursor_create(CURSOR_IMAGE, &move_cursor,
				CURSOR_XHOT, 0,
				CURSOR_YHOT, 0,
				CURSOR_OP, PIX_SRC ^ PIX_DST,
				0);
    cursor_del = cursor_create(CURSOR_IMAGE, &del_cursor,
			       CURSOR_XHOT, 0,
			       CURSOR_YHOT, 0,
			       CURSOR_OP, PIX_SRC ^ PIX_DST,
			       0);
    cursor_wait = cursor_create(CURSOR_IMAGE, &wait_cursor,
				CURSOR_XHOT, 0,
				CURSOR_YHOT, 0,
				CURSOR_OP, PIX_SRC ^ PIX_DST,
				0);
    updateparms();
    updatetics();
    for (i = 0; i < maxplot; i++) {
	update_status(i);
    }
    window_main_loop(main_frame);
}

static void updateparms()
{

    sprintf(buf, "%.5g", xg1);
    panel_set_value(define_world_xg1, buf);
    sprintf(buf, "%.5g", xg2);
    panel_set_value(define_world_xg2, buf);
    sprintf(buf, "%.5g", yg1);
    panel_set_value(define_world_yg1, buf);
    sprintf(buf, "%.5g", yg2);
    panel_set_value(define_world_yg2, buf);
    sprintf(buf, "%.5g", xv1);
    panel_set_value(define_view_xv1, buf);
    sprintf(buf, "%.5g", xv2);
    panel_set_value(define_view_xv2, buf);
    sprintf(buf, "%.5g", yv1);
    panel_set_value(define_view_yv1, buf);
    sprintf(buf, "%.5g", yv2);
    panel_set_value(define_view_yv2, buf);

    if (xt1 > 0)
	sprintf(buf, "%.5g", xt1);
    else
	strcpy(buf, "UNDEFINED");
    panel_set_value(define_tics_xmajor, buf);
    if (xt2 > 0)
	sprintf(buf, "%.5g", xt2);
    else
	strcpy(buf, "UNDEFINED");
    panel_set_value(define_tics_xminor, buf);
    if (yt1 > 0)
	sprintf(buf, "%.5g", yt1);
    else
	strcpy(buf, "UNDEFINED");
    panel_set_value(define_tics_ymajor, buf);
    if (yt2 > 0)
	sprintf(buf, "%.5g", yt2);
    else
	strcpy(buf, "UNDEFINED");
    panel_set_value(define_tics_yminor, buf);

    sprintf(buf, "%d", xform);
    panel_set_value(define_tics_xform, buf);
    sprintf(buf, "%d", yform);
    panel_set_value(define_tics_yform, buf);
    sprintf(buf, "%d", xticangle);
    panel_set_value(define_xtics_angle, buf);

    panel_set_value(title_item, title);
    panel_set_value(subtitle_item, stitle);
    panel_set_value(xaxis_item, xlabel);
    panel_set_value(yaxis_item, ylabel);
}

stufftext(s)
    char *s;
{
    fprintf(resfp, s);
    fflush(resfp);
}

static void do_status_proc(item, event)
    Panel_item item;
    Event *event;
{
    window_set(status_frame, WIN_SHOW, TRUE, 0);
}

static void status_done_proc()
{
    window_set(status_frame, WIN_SHOW, FALSE, 0);
}

update_status(setno)
    int setno;
{
    double x1, y1, x2, y2, xbar, ybar, xsd, ysd, *getx(), *gety();

    getsetminmax(setno, &x1, &x2, &y1, &y2);
    if (isactive(setno)) {
	stasum(getx(setno), getsetlength(setno), &xbar, &xsd, 0);
	stasum(gety(setno), getsetlength(setno), &ybar, &ysd, 0);
    } else {
	xbar = 0.0;
	ybar = 0.0;
	xsd = 0.0;
	ysd = 0.0;
    }
    sprintf(buf, "%2d %4d %3s  X  %8.5g %8.5g %8.5g %8.5g   %s", setno, getsetlength(setno), onoff(isactive(setno)), x1, x2, xbar, xsd, getcomment(setno));
    panel_set_value(status_items_x[setno], buf);
    sprintf(buf, "             Y  %8.5g %8.5g %8.5g %8.5g", y1, y2, ybar, ysd);
    panel_set_value(status_items_y[setno], buf);
}

static void update_stuff_status()
{
    double x1, y1, x2, y2, xbar, ybar, xsd, ysd, *getx(), *gety();
    int i;

    sprintf(buf, "\nStatus of sets\n");
    stufftext(buf);
    for (i = 0; i < maxplot; i++) {
	getsetminmax(i, &x1, &x2, &y1, &y2);
	if (isactive(i)) {
	    stasum(getx(i), getsetlength(i), &xbar, &xsd, 0);
	    stasum(gety(i), getsetlength(i), &ybar, &ysd, 0);
	} else {
	    xbar = 0.0;
	    ybar = 0.0;
	    xsd = 0.0;
	    ysd = 0.0;
	}
	sprintf(buf, "%2d %4d %3s  X  %8.5g %8.5g %8.5g %8.5g   %s\n", i, getsetlength(i), onoff(isactive(i)), x1, x2, xbar, xsd, getcomment(i));
	stufftext(buf);
	sprintf(buf, "             Y  %8.5g %8.5g %8.5g %8.5g\n", y1, y2, ybar, ysd);
	stufftext(buf);
    }
}

static void define_status_popup()
{
    int i;

    status_frame = window_create(main_frame, FRAME,
				 WIN_Y, 50,
				 FRAME_LABEL, "Status",
				 FRAME_SHOW_LABEL, TRUE,
			      WIN_ERROR_MSG, "Couldn't create status_frame",
				 0);
    status_panel = window_create(status_frame, PANEL, 0);

    sprintf(buf, "set# n  stat        min     max     mean   std. dev.   comment");
    header_item1 = panel_create_item(status_panel, PANEL_TEXT,
				     PANEL_VALUE_FONT, winfont,
				     PANEL_LABEL_STRING, buf,
				     PANEL_ITEM_X, ATTR_COL(1),
				     PANEL_ITEM_Y, ATTR_ROW(1),
				     PANEL_VALUE_DISPLAY_LENGTH, 0, 0);
    for (i = 0; i < maxplot; i++) {
	status_items_x[i] = panel_create_item(status_panel, PANEL_TEXT,
					      PANEL_VALUE_FONT, winfont,
					      PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(2 + 2 * i),
					 PANEL_VALUE_DISPLAY_LENGTH, 80, 0);
	status_items_y[i] = panel_create_item(status_panel, PANEL_TEXT,
					      PANEL_VALUE_FONT, winfont,
					      PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(3 + 2 * i),
					 PANEL_VALUE_DISPLAY_LENGTH, 80, 0);
    }
    panel_create_item(status_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	       panel_button_image(status_panel, "Write status", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(33),
		      PANEL_NOTIFY_PROC, update_stuff_status,
		      0);
    panel_create_item(status_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(status_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(20),
		      PANEL_ITEM_Y, ATTR_ROW(33),
		      PANEL_NOTIFY_PROC, status_done_proc,
		      0);
    window_fit(status_panel);
    window_fit(status_frame);
}

static void save_session(item, event)
    Panel_item item;
    Event *event;
{
    char fname[80];

    strcpy(fname, panel_get_value(session_fname_item));
}

chdir_proc()
{
    char pathn[80];

    strcpy(pathn, panel_get_value(readdata_dir_item));
    if (strlen(pathn)) {
	if (!(chdir(pathn) == 0)) {
	    sprintf(buf, "Error in changing to directory %s", pathn);
	    errwin(buf);
	}
    }
}

static void cancel_ok(item, event)
    Panel_item item;
    Event *event;
{
    window_return(panel_get(item, PANEL_CLIENT_DATA));
}

static Panel_setting eotproc_noreturn(item, event)
    Panel_item item;
    Event *event;
{
    switch (event_id(event)) {
    case '\t':
	return PANEL_INSERT;
    case '\r':
    case '\n':
	chdir_proc();
	return PANEL_NONE;
    default:
	return PANEL_INSERT;
    }
}

static Panel_setting eotproc(item, event)
    Panel_item item;
    Event *event;
{
    switch (event_id(event)) {
    case '\t':
	return PANEL_INSERT;
    case '\r':
    case '\n':
	window_return(TRUE);
	return PANEL_NONE;

    default:
	return PANEL_INSERT;
    }
}

int get_msg(oldmsg, newmsg, header, prompt)
    char *oldmsg, *newmsg, *header, *prompt;
{
    int status;
    Frame getmsg_frame;
    Panel getmsg_panel;
    Panel_item msg_item;
    Panel_item message_item;

    getmsg_frame = window_create(0, FRAME,
				 WIN_Y, 50,
				 WIN_X, 50,
				 FRAME_SHOW_LABEL, FALSE,
				 FRAME_NO_CONFIRM, TRUE,
			      WIN_ERROR_MSG, "Couldn't create getmsg_frame",
				 0);
    getmsg_panel = window_create(getmsg_frame, PANEL, 0);
    message_item = panel_create_item(getmsg_panel, PANEL_MESSAGE,
				     PANEL_ITEM_X, ATTR_COL(1),
				     PANEL_ITEM_Y, ATTR_ROW(0),
				     PANEL_LABEL_STRING, header, 0);
    msg_item = panel_create_item(getmsg_panel, PANEL_TEXT,
				 PANEL_LABEL_STRING, prompt,
				 PANEL_ITEM_X, ATTR_COL(1),
				 PANEL_ITEM_Y, ATTR_ROW(1),
				 PANEL_NOTIFY_STRING, "\r\t\n",
				 PANEL_NOTIFY_PROC, eotproc,
				 PANEL_VALUE_DISPLAY_LENGTH, 60, 0);
    panel_create_item(getmsg_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(getmsg_panel, "Cancel", 6, 0),
		      PANEL_CLIENT_DATA, FALSE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);

    panel_create_item(getmsg_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(10),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(getmsg_panel, " OK ", 6, 0),
		      PANEL_CLIENT_DATA, TRUE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);
    window_fit(getmsg_panel);
    window_fit(getmsg_frame);
    panel_set_value(msg_item, oldmsg);
    status = (int) window_loop(getmsg_frame);
    strcpy(newmsg, (char *) panel_get_value(msg_item));
    window_destroy(getmsg_frame);
    return status;
}

static void create_readp_popup()
{
    int status;
    char fname[80];
    int i;
    Frame readp_frame;
    Panel readp_panel;
    Panel_item readp_fname_item;
    Panel_item message_item;

    readp_frame = window_create(0, FRAME,
				WIN_Y, 50,
				WIN_X, 50,
				FRAME_SHOW_LABEL, FALSE,
				FRAME_NO_CONFIRM, TRUE,
				WIN_ERROR_MSG, "Couldn't create readp_frame",
				0);
    readp_panel = window_create(readp_frame, PANEL, 0);
    message_item = panel_create_item(readp_panel, PANEL_MESSAGE,
				     PANEL_ITEM_X, ATTR_COL(1),
				     PANEL_ITEM_Y, ATTR_ROW(0),
			      PANEL_LABEL_STRING, "Read parameter file", 0);
    readp_fname_item = panel_create_item(readp_panel, PANEL_TEXT,
					 PANEL_LABEL_STRING, "File: ",
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(1),
					 PANEL_NOTIFY_STRING, "\r\t\n",
					 PANEL_NOTIFY_PROC, eotproc,
					 PANEL_VALUE_DISPLAY_LENGTH, 60, 0);
    panel_create_item(readp_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(readp_panel, "Cancel", 6, 0),
		      PANEL_CLIENT_DATA, FALSE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);

    panel_create_item(readp_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(10),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(readp_panel, " OK ", 6, 0),
		      PANEL_CLIENT_DATA, TRUE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);
    window_fit(readp_panel);
    window_fit(readp_frame);
    status = (int) window_loop(readp_frame);
    window_destroy(readp_frame);
    if (status) {
	strcpy(fname, panel_get_value(readp_fname_item));
	if (strlen(fname)) {
	    getparms(fname);
	    updateparms();
	    updatetics();
	    for (i = 0; i < maxplot; i++) {
		updatesymbols(i);
		updatelegends(i);
	    }
	    updatestrings(0);
	    drawgraph();
	}
    }
}

static void create_writep_popup()
{
    int status;
    char fname[80];
    Frame writep_frame;
    Panel writep_panel;
    Panel writep_fname_item;
    Panel_item message_item;

    writep_frame = window_create(0, FRAME,
				 WIN_Y, 50,
				 WIN_X, 50,
				 FRAME_SHOW_LABEL, FALSE,
				 FRAME_NO_CONFIRM, TRUE,
			      WIN_ERROR_MSG, "Couldn't create writep_frame",
				 0);
    writep_panel = window_create(writep_frame, PANEL, 0);
    message_item = panel_create_item(writep_panel, PANEL_MESSAGE,
				     PANEL_ITEM_X, ATTR_COL(1),
				     PANEL_ITEM_Y, ATTR_ROW(0),
			     PANEL_LABEL_STRING, "Write parameter file", 0);
    writep_fname_item = panel_create_item(writep_panel, PANEL_TEXT,
					  PANEL_LABEL_STRING, "File: ",
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(1),
					  PANEL_NOTIFY_STRING, "\r\t\n",
					  PANEL_NOTIFY_PROC, eotproc,
					  PANEL_VALUE_DISPLAY_LENGTH, 60, 0);
    panel_create_item(writep_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(writep_panel, "Cancel", 6, 0),
		      PANEL_CLIENT_DATA, FALSE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);

    panel_create_item(writep_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(10),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(writep_panel, " OK ", 6, 0),
		      PANEL_CLIENT_DATA, TRUE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);
    window_fit(writep_panel);
    window_fit(writep_frame);
    status = (int) window_loop(writep_frame);
    window_destroy(writep_frame);
    if (status) {
	strcpy(fname, panel_get_value(writep_fname_item));
	if (strlen(fname))
	    putparms(fname);
    }
}

static void create_readdata_popup()
{
    char fname[80];
    unsigned value;
    int i, status;

    readdata_frame = window_create(main_frame, FRAME,
				   WIN_Y, 50,
				   WIN_X, 0,
			    WIN_ERROR_MSG, "Couldn't create readdata_frame",
				   FRAME_NO_CONFIRM, TRUE,
				   0);
    readdata_panel = window_create(readdata_frame, PANEL, 0);

    toggle_readdata_item = panel_create_item(readdata_panel, PANEL_CYCLE,
					 PANEL_LABEL_STRING, "File format:",
					     PANEL_CHOICE_STRINGS,
					     "Data is X Y",
					     "Data is X Y1 Y2 Y3 ...",
					     "Pipe X Y",
					     "IHL format",
					     0,
					     PANEL_ITEM_X, ATTR_COL(1),
					     PANEL_ITEM_Y, ATTR_ROW(2),
					     0);
    readdata_fname_item = panel_create_item(readdata_panel, PANEL_TEXT,
				     PANEL_LABEL_STRING, "Data file name: ",
					    PANEL_ITEM_X, ATTR_COL(1),
					    PANEL_ITEM_Y, ATTR_ROW(1),
					    PANEL_NOTIFY_STRING, "\r\t\n",
					    PANEL_NOTIFY_PROC, eotproc,
					 PANEL_VALUE_DISPLAY_LENGTH, 60, 0);
    readdata_dir_item = panel_create_item(readdata_panel, PANEL_TEXT,
				  PANEL_LABEL_STRING, "Current directory: ",
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(3),
					PANEL_NOTIFY_PROC, eotproc_noreturn,
					  PANEL_NOTIFY_STRING, "\r\t\n",
					  PANEL_VALUE_DISPLAY_LENGTH, 60, 0);
    panel_create_item(readdata_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(readdata_panel, "Cancel", 6, 0),
		      PANEL_CLIENT_DATA, FALSE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);

    panel_create_item(readdata_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(10),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(readdata_panel, " OK ", 6, 0),
		      PANEL_CLIENT_DATA, TRUE,
		      PANEL_NOTIFY_PROC, cancel_ok, 0);
    getwd(buf);
    panel_set_value(readdata_dir_item, buf);
    window_fit(readdata_panel);
    window_fit(readdata_frame);
    status = (int) window_loop(readdata_frame);
    window_destroy(readdata_frame);
    if (status) {
	value = (int) panel_get_value(toggle_readdata_item);
	strcpy(fname, panel_get_value(readdata_fname_item));
	if (fname[0]) {
	    if (getdata(fname, value)) {
		for (i = 0; i < maxplot; i++) {
		    update_status(i);
		}
		drawgraph();
	    } else {
		sprintf(buf, "Error opening file %s", fname);
		errwin(buf);
	    }
	} else
	    errwin("Define file name");
    }
}

static void define_world_proc(item, event)
    Panel_item item;
    Event *event;
{
    char val[80];
    int ier;
    double x = (xg2 - xg1), y = (yg2 - yg1), a = xg1, b = yg1, c = xg2, d = yg2;
    int errpos;
    extern double result;

    strcpy(val, panel_get_value(define_world_xg1));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    xg1 = result;
    strcpy(val, panel_get_value(define_world_yg1));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    yg1 = result;
    strcpy(val, panel_get_value(define_world_xg2));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    xg2 = result;
    strcpy(val, panel_get_value(define_world_yg2));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    yg2 = result;
    drawgraph();
}

static void define_view_proc(item, event)
    Panel_item item;
    Event *event;
{
    char val[80];
    double tmpx1, tmpx2;
    double tmpy1, tmpy2;
    int ierr = 0;

    strcpy(val, panel_get_value(define_view_xv1));
    tmpx1 = atof(val);
    strcpy(val, panel_get_value(define_view_xv2));
    tmpx2 = atof(val);
    strcpy(val, panel_get_value(define_view_yv1));
    tmpy1 = atof(val);
    strcpy(val, panel_get_value(define_view_yv2));
    tmpy2 = atof(val);
    if (!fbounds(tmpx1, 0.0, 1.0, "View xmin"))
	ierr = 1;
    else if (!fbounds(tmpx2, tmpx1, 1.0, "View xmax"))
	ierr = 1;
    else if (!fbounds(tmpy1, 0.0, 1.0, "View ymin"))
	ierr = 1;
    else if (!fbounds(tmpy2, tmpy1, 1.0, "View ymax"))
	ierr = 1;
    if (!ierr) {
	xv1 = tmpx1;
	xv2 = tmpx2;
	yv1 = tmpy1;
	yv2 = tmpy2;
	drawgraph();
    }
}

static void define_viewm_proc(item, event)
    Panel_item item;
    Event *event;
{
    set_action(0);
    set_action(VIEW_1ST);
    window_set(canvas, WIN_CURSOR, cursor, 0);
}

static double scrollper = 0.95;

static void gwindleft_proc(item, event)
    Panel_item item;
    Event *event;
{
    double dx = scrollper * (xg2 - xg1);

    xg1 = xg1 - dx;
    xg2 = xg2 - dx;
    drawgraph();
}

static void gwindright_proc(item, event)
    Panel_item item;
    Event *event;
{
    double dx = scrollper * (xg2 - xg1);

    xg1 = xg1 + dx;
    xg2 = xg2 + dx;
    drawgraph();
}

static void gwinddown_proc(item, event)
    Panel_item item;
    Event *event;
{
    double dy = scrollper * (yg2 - yg1);

    yg1 = yg1 - dy;
    yg2 = yg2 - dy;
    drawgraph();
}

static void gwindup_proc(item, event)
    Panel_item item;
    Event *event;
{
    double dy = scrollper * (yg2 - yg1);

    yg1 = yg1 + dy;
    yg2 = yg2 + dy;
    drawgraph();
}

static void scroll_proc(item, value, event)
    Panel_item item;
    Event *event;
    int value;
{
    scrollper = value / 100.0;
}

static void update_proc(item, event)
    Panel_item item;
    Event *event;
{
    updateparms();
}

static void autoscale_proc(item, event)
    Panel_item item;
    Event *event;
{
    if (activeset()) {
	defaultgraph();
	defaulttics((int) panel_get_value(autoscale_type_item));
	updateparms();
	drawgraph();
    } else
	errwin("No active sets!");
}

static void autoscale_set_proc(item, event)
    Panel_item item;
    Event *event;
{
    int value;

    value = (int) panel_get_value(autoscale_set_item);
    if (isactive(value)) {
	defaultsetgraph(value);
	defaulttics((int) panel_get_value(autoscale_type_item));
	updateparms();
	drawgraph();
    } else
	errwin("No active sets!");
}

static void do_zoom_proc(item, event)
    Panel_item item;
    Event *event;
{
    set_action(0);
    set_action(ZOOM_1ST);
    window_set(canvas, WIN_CURSOR, cursor, 0);
}

static void define_world_done_proc()
{
    /*
     * window_set(define_world_frame,FRAME_NO_CONFIRM,TRUE,0);
     * window_destroy(define_world_frame);
     */
    window_set(define_world_frame, WIN_SHOW, FALSE, 0);
}

static void define_world_popup()
{
    define_world_frame = window_create(main_frame, FRAME,
				       FRAME_LABEL, "World/view",
				       FRAME_SHOW_LABEL, TRUE,
				       WIN_Y, 50,
			WIN_ERROR_MSG, "Couldn't create define_world_frame",
				       0);
    define_world_panel = window_create(define_world_frame, PANEL,
				       0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_LABEL_IMAGE,
	 panel_button_image(define_world_panel, "Define world", 0, winfont),
		      PANEL_NOTIFY_PROC, define_world_proc,
		      0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(15),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_LABEL_IMAGE,
		 panel_button_image(define_world_panel, "Zoom", 0, winfont),
		      PANEL_NOTIFY_PROC, do_zoom_proc,
		      0);
    define_world_xg1 = panel_create_item(define_world_panel, PANEL_TEXT,
					 PANEL_LABEL_STRING, "World Xmin: ",
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(2),
					 PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    define_world_xg2 = panel_create_item(define_world_panel, PANEL_TEXT,
					 PANEL_LABEL_STRING, "World Xmax: ",
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(3),
					 PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    define_world_yg1 = panel_create_item(define_world_panel, PANEL_TEXT,
					 PANEL_LABEL_STRING, "World Ymin: ",
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(4),
					 PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    define_world_yg2 = panel_create_item(define_world_panel, PANEL_TEXT,
					 PANEL_LABEL_STRING, "World Ymax: ",
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(5),
					 PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(7),
		      PANEL_LABEL_IMAGE,
	  panel_button_image(define_world_panel, "Define view", 0, winfont),
		      PANEL_NOTIFY_PROC, define_view_proc,
		      0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(define_world_panel, "Define view by mouse", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(8),
		      PANEL_NOTIFY_PROC, define_viewm_proc,
		      0);
    define_view_xv1 = panel_create_item(define_world_panel, PANEL_TEXT,
					PANEL_LABEL_STRING, "View Xmin: ",
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(9),
					PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    define_view_xv2 = panel_create_item(define_world_panel, PANEL_TEXT,
					PANEL_LABEL_STRING, "View Xmax: ",
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(10),
					PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    define_view_yv1 = panel_create_item(define_world_panel, PANEL_TEXT,
					PANEL_LABEL_STRING, "View Ymin: ",
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(11),
					PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    define_view_yv2 = panel_create_item(define_world_panel, PANEL_TEXT,
					PANEL_LABEL_STRING, "View Ymax: ",
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(12),
					PANEL_VALUE_DISPLAY_LENGTH, 15, 0);

    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(14),
		      PANEL_LABEL_IMAGE,
	   panel_button_image(define_world_panel, "Left", 0, (Pixfont *) 0),
		      PANEL_NOTIFY_PROC, gwindleft_proc,
		      0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(7),
		      PANEL_ITEM_Y, ATTR_ROW(14),
		      PANEL_LABEL_IMAGE,
	  panel_button_image(define_world_panel, "Right", 0, (Pixfont *) 0),
		      PANEL_NOTIFY_PROC, gwindright_proc,
		      0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(5),
		      PANEL_ITEM_Y, ATTR_ROW(13),
		      PANEL_LABEL_IMAGE,
	     panel_button_image(define_world_panel, "Up", 0, (Pixfont *) 0),
		      PANEL_NOTIFY_PROC, gwindup_proc,
		      0);
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(5),
		      PANEL_ITEM_Y, ATTR_ROW(15),
		      PANEL_LABEL_IMAGE,
	   panel_button_image(define_world_panel, "Down", 0, (Pixfont *) 0),
		      PANEL_NOTIFY_PROC, gwinddown_proc,
		      0);
    panel_create_item(define_world_panel, PANEL_SLIDER,
		      PANEL_ITEM_X, ATTR_COL(18),
		      PANEL_ITEM_Y, ATTR_ROW(14),
		      PANEL_LABEL_STRING, "Scroll %",
		      PANEL_VALUE, 95,
		      PANEL_MIN_VALUE, 1,
		      PANEL_MAX_VALUE, 100,
		      PANEL_NOTIFY_PROC, scroll_proc,
		      0);

    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(define_world_panel, "Update world and view", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(17),
		      PANEL_NOTIFY_PROC, update_proc,
		      0);

    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	    panel_button_image(define_world_panel, "Autoscale", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(19),
		      PANEL_NOTIFY_PROC, autoscale_proc,
		      0);
    define_select_set_panel3(define_world_panel, autoscale_set_item, ATTR_COL(25), ATTR_ROW(20));
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
     panel_button_image(define_world_panel, "Autoscale on set", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(20),
		      PANEL_NOTIFY_PROC, autoscale_set_proc,
		      0);
    autoscale_type_item = panel_create_item(define_world_panel, PANEL_CYCLE,
					    PANEL_ITEM_X, ATTR_COL(1),
					    PANEL_ITEM_Y, ATTR_ROW(22),
				      PANEL_LABEL_STRING, "Autoscale mode:",
					    PANEL_CHOICE_STRINGS,
					    "Adjustable spacing",
					    "Fixed spacing",
					    0,
					    0);
    auto_redraw_item = panel_create_item(define_world_panel, PANEL_CYCLE,
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(23),
					 PANEL_LABEL_STRING, "Draw mode:",
					 PANEL_CHOICE_STRINGS,
					 "Auto re-draw",
					 "No auto redraw",
					 0,
					 0);
/* some day...

    graph_type_item = panel_create_item(define_world_panel, PANEL_CYCLE,
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(24),
					PANEL_LABEL_STRING, "Graph type:",
					PANEL_CHOICE_STRINGS,
					"X-Y",
					"Bar graph",
					"Stacked bar",
					"Adjacent bar",
					"Polar",
					0,
					0);
*/
    panel_create_item(define_world_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		 panel_button_image(define_world_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(25),
		      PANEL_NOTIFY_PROC, define_world_done_proc,
		      0);
    window_fit(define_world_panel);
    window_fit(define_world_frame);
}

static void updatetics()
{
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 0, xticflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 1, xticlflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 2, fformx, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 3, xgridflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 4, xticslog, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 5, xzflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 6, xztflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 7, xticinoutflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 8, xticopflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 9, xabsflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 10, xtopflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 11, boxflag, 0);
    panel_set(toggle_xtics_item, PANEL_TOGGLE_VALUE, 12, boxon, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 0, yticflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 1, yticlflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 2, fformy, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 3, ygridflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 4, yticslog, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 5, yzflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 6, yztflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 7, yticinoutflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 8, yticopflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 9, yabsflag, 0);
    panel_set(toggle_ytics_item, PANEL_TOGGLE_VALUE, 10, ytopflag, 0);
}


static void define_tics_proc(item, event)
    Panel_item item;
    Event *event;
{
    char val[80];
    int ier;
    extern double result;
    double x = (xg2 - xg1), y = (yg2 - yg1), a = xg1, b = yg1, c = xg2, d = yg2;
    int errpos;

    strcpy(val, panel_get_value(define_tics_xmajor));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    xt1 = result;
    strcpy(val, panel_get_value(define_tics_xminor));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    xt2 = result;
    strcpy(val, panel_get_value(define_tics_ymajor));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    yt1 = result;
    strcpy(val, panel_get_value(define_tics_yminor));
    fixupstr(val);
    scanner(val, &x, &y, &a, &b, &c, &d, 0, &errpos);
    if (errpos) {
	return;
    }
    yt2 = result;
    strcpy(val, panel_get_value(define_tics_xform));
    xform = atoi(val);
    strcpy(val, panel_get_value(define_tics_yform));
    yform = atoi(val);
    strcpy(val, panel_get_value(define_xtics_angle));
    xticangle = atoi(val);
    xticflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 0);
    yticflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 0);
    xticlflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 1);
    yticlflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 1);
    fformx = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 2);
    fformy = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 2);
    xgridflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 3);
    ygridflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 3);
    xticslog = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 4);
    yticslog = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 4);
    if (xticslog) {
	if (xt2 > 9.0 || xt2 < 0.0) {
	    xt2 = 9.0;
	    sprintf(buf, "%.5g", xt2);
	    panel_set_value(define_tics_xminor, buf);
	}
    }
    if (yticslog) {
	if (yt2 > 9.0 || yt2 < 0.0) {
	    yt2 = 9.0;
	    sprintf(buf, "%.5g", yt2);
	    panel_set_value(define_tics_yminor, buf);
	}
    }
    xzflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 5);
    yzflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 5);
    xztflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 6);
    yztflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 6);
    boxflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 11);
    boxon = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 12);
    xticinoutflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 7);
    yticinoutflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 7);
    xticopflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 8);
    yticopflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 8);
    xabsflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 9);
    yabsflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 9);
    xtopflag = (int) panel_get(toggle_xtics_item, PANEL_TOGGLE_VALUE, 10);
    ytopflag = (int) panel_get(toggle_ytics_item, PANEL_TOGGLE_VALUE, 10);
    drawgraph();
}

static void define_labels_proc(item, event)
    Panel_item item;
    Event *event;
{
    strcpy(title, panel_get_value(title_item));
    strcpy(stitle, panel_get_value(subtitle_item));
    strcpy(xlabel, panel_get_value(xaxis_item));
    strcpy(ylabel, panel_get_value(yaxis_item));
    drawgraph();
}

static void do_defaults_proc(item, event)
    Panel_item item;
    Event *event;
{
    extern int curfont;

    curfont = (int) panel_get_value(defaults_font_item);
    drawgraph();
}

static void toggle_tics_proc(item, value, event)
    Panel_item item;
    unsigned int value;
    Event *event;
{
}

static void define_tics_done_proc()
{
    window_set(define_tics_frame, WIN_SHOW, FALSE, 0);
}

static void define_tics_popup()
{
    define_tics_frame = window_create(main_frame, FRAME,
				      FRAME_LABEL, "Tics and labels",
				      FRAME_SHOW_LABEL, TRUE,
				      WIN_Y, 50,
			 WIN_ERROR_MSG, "Couldn't create define_tics_frame",
				      0);
    define_tics_panel = window_create(define_tics_frame, PANEL,
    /* WIN_VERTICAL_SCROLLBAR,scrollbar_create(0), */
				      0);
    panel_create_item(define_tics_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(8),
		      PANEL_LABEL_IMAGE,
	   panel_button_image(define_tics_panel, "Define tics", 0, winfont),
		      PANEL_NOTIFY_PROC, define_tics_proc,
		      0);
    panel_create_item(define_tics_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	   panel_button_image(define_tics_panel, "Update tics", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(20),
		      PANEL_ITEM_Y, ATTR_ROW(8),
		      PANEL_NOTIFY_PROC, update_proc,
		      0);
    panel_create_item(define_tics_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(35),
		      PANEL_ITEM_Y, ATTR_ROW(8),
		      PANEL_LABEL_IMAGE,
		  panel_button_image(define_tics_panel, "Done", 0, winfont),
		      PANEL_NOTIFY_PROC, define_tics_done_proc,
		      0);
    define_tics_xmajor = panel_create_item(define_tics_panel, PANEL_TEXT,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(9),
				  PANEL_LABEL_STRING, "X-axis major tics: ",
					 PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    define_tics_xminor = panel_create_item(define_tics_panel, PANEL_TEXT,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(10),
				  PANEL_LABEL_STRING, "X-axis minor tics: ",
					 PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    define_tics_ymajor = panel_create_item(define_tics_panel, PANEL_TEXT,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(11),
				  PANEL_LABEL_STRING, "Y-axis major tics: ",
					 PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    define_tics_yminor = panel_create_item(define_tics_panel, PANEL_TEXT,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(12),
				  PANEL_LABEL_STRING, "Y-axis minor tics: ",
					 PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    define_tics_xform = panel_create_item(define_tics_panel, PANEL_TEXT,
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(13),
			    PANEL_LABEL_STRING, "X-axis labels precision: ",
					  PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    define_tics_yform = panel_create_item(define_tics_panel, PANEL_TEXT,
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(14),
			    PANEL_LABEL_STRING, "Y-axis labels precision: ",
					  PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    define_xtics_angle = panel_create_item(define_tics_panel, PANEL_TEXT,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(15),
				PANEL_LABEL_STRING, "X-axis labels angle: ",
					 PANEL_VALUE_DISPLAY_LENGTH, 25, 0);
    toggle_xtics_item = panel_create_item(define_tics_panel, PANEL_TOGGLE,
					  PANEL_ITEM_X, ATTR_COL(0),
					  PANEL_ITEM_Y, ATTR_ROW(17),
				      PANEL_LABEL_STRING, "X-axis Toggles:",
					  PANEL_LAYOUT, PANEL_VERTICAL,
					  PANEL_CHOICE_STRINGS,
					  "Show X-axis tics",
					  "Show X-axis tic labels",
					  "X-axis labels decimal",
					  "X-axis grid",
					  "X-axis logarithmic",
					  "Draw the line Y=0",
					  "Tics for the line Y=0",
					  "X-axis tics out",
					  "X-axis tics top",
					  "X-axis labels absolute",
					  "X-axis labels on top",
					  "Box around plot closed",
					  "Box around plot",
					  0,
					PANEL_NOTIFY_PROC, toggle_tics_proc,
					  0);
    toggle_ytics_item = panel_create_item(define_tics_panel, PANEL_TOGGLE,
					  PANEL_ITEM_X, ATTR_COL(30),
					  PANEL_ITEM_Y, ATTR_ROW(17),
				      PANEL_LABEL_STRING, "Y-axis Toggles:",
					  PANEL_LAYOUT, PANEL_VERTICAL,
					  PANEL_CHOICE_STRINGS,
					  "Show Y-axis tics",
					  "Show Y-axis tic labels",
					  "Y-axis labels decimal",
					  "Y-axis grid",
					  "Y-axis logarithmic",
					  "Draw the line X=0",
					  "Tics for the line X=0",
					  "Y-axis tics out",
					  "Y-axis tics right",
					  "Y-axis labels absolute",
					  "Y-axis labels on right",
					  0,
					PANEL_NOTIFY_PROC, toggle_tics_proc,
					  0);

    panel_create_item(define_tics_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(0),
		      PANEL_LABEL_IMAGE,
	 panel_button_image(define_tics_panel, "Define labels", 0, winfont),
		      PANEL_NOTIFY_PROC, define_labels_proc,
		      0);
    title_item = panel_create_item(define_tics_panel, PANEL_TEXT,
				   PANEL_LABEL_STRING, "Title: ",
				   PANEL_ITEM_X, ATTR_COL(1),
				   PANEL_ITEM_Y, ATTR_ROW(1),
				   PANEL_VALUE_DISPLAY_LENGTH, 40, 0);
    subtitle_item = panel_create_item(define_tics_panel, PANEL_TEXT,
				      PANEL_LABEL_STRING, "Subtitle: ",
				      PANEL_ITEM_X, ATTR_COL(1),
				      PANEL_ITEM_Y, ATTR_ROW(2),
				      PANEL_VALUE_DISPLAY_LENGTH, 40, 0);
    xaxis_item = panel_create_item(define_tics_panel, PANEL_TEXT,
				   PANEL_LABEL_STRING, "X-axis label: ",
				   PANEL_ITEM_X, ATTR_COL(1),
				   PANEL_ITEM_Y, ATTR_ROW(3),
				   PANEL_VALUE_DISPLAY_LENGTH, 40, 0);
    yaxis_item = panel_create_item(define_tics_panel, PANEL_TEXT,
				   PANEL_LABEL_STRING, "Y-axis label: ",
				   PANEL_ITEM_X, ATTR_COL(1),
				   PANEL_ITEM_Y, ATTR_ROW(4),
				   PANEL_VALUE_DISPLAY_LENGTH, 40, 0);
    panel_create_item(define_tics_panel, PANEL_BUTTON,
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_LABEL_IMAGE,
		      panel_button_image(define_tics_panel, "Establish default font for labels", 0, winfont),
		      PANEL_NOTIFY_PROC, do_defaults_proc,
		      0);
    defaults_font_item = panel_create_item(define_tics_panel, PANEL_CYCLE,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(6),
					PANEL_LABEL_STRING, "Default font:",
					   PANEL_CHOICE_STRINGS,
					   "Romanc",
					   "Romand",
					   "Romans",
					   "Romant",
					   "Italicc",
					   "Italict",
					   "Scriptc",
					   "Scripts",
					   "Greekc",
					   "Greeks",
					   0,
					   0);
    window_fit(define_tics_panel);
    window_fit(define_tics_frame);
    updatetics();
}

static void undefine_errbar_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setn;

    setn = (int) panel_get_value(error_from_set);
    clearseterrbar(setn);
    drawgraph();
}

static void define_errbar_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setfrom, setto, errbar_xy;

    setfrom = (int) panel_get_value(error_from_set);
    setto = (int) panel_get_value(error_to_set);
    errbar_xy = (int) panel_get_value(error_xy_item);
    makeseterrbar(setfrom, setto);
    seterrbarxy(setfrom, errbar_xy);
    update_status(setfrom);
    drawgraph();
}

static void errbar_length_proc(item, value, event)
    Panel_item item;
    Event *event;
    int value;
{
    errbarper = value / 50.0;
}


static void define_symbols_proc(item, event)
    Panel_item item;
    Event *event;
{
    int sym, line, pen, i;

    for (i = 0; i < maxplot; i++) {

	sym = (int) panel_get_value(toggle_symbols_item[i]);
	pen = (int) panel_get_value(toggle_pen_item[i]);
	line = (int) panel_get_value(toggle_lines_item[i]);
	setplotsym(i, sym);
	setlinesym(i, line);
	setplotcolor(i, pen);
    }
    drawgraph();
}

updatesymbols(value)
    unsigned int value;
{
    panel_set_value(toggle_symbols_item[value], getsetplotsym(value));
    panel_set_value(toggle_pen_item[value], getsetcolor(value));
    panel_set_value(toggle_lines_item[value], getsetlinesym(value));
}

static void toggle_symbols_proc(item, value, event)
    Panel_item item;
    unsigned int value;
    Event *event;
{
}

static void toggle_pen_proc(item, value, event)
    Panel_item item;
    unsigned int value;
    Event *event;
{
}

static void toggle_lines_proc(item, value, event)
    Panel_item item;
    unsigned int value;
    Event *event;
{
}

/* legends */

static int firstrun = TRUE;

extern plotstr legstr[];

updatelegends(value)
    unsigned int value;
{
    char buf[80];

    panel_set_value(toggle_legends_item, legendflag);
    sprintf(buf, "%lf", legx);
    panel_set_value(legend_x_panel, buf);
    sprintf(buf, "%lf", legy);
    panel_set_value(legend_y_panel, buf);
    panel_set_value(legends_gap_item, lgap - 1);
    panel_set_value(legends_len_item, llen - 1);
    panel_set_value(legend_str_panel[value], legstr[value].s);
}

static void define_legends_proc(item, event)
    Panel_item item;
    Event *event;
{
    char val[80];
    int i;

    legendflag = (int) panel_get_value(toggle_legends_item);
    lgap = (int) panel_get_value(legends_gap_item) + 1;
    llen = (int) panel_get_value(legends_len_item) + 1;
    strcpy(val, panel_get_value(legend_x_panel));
    legx = atof(val);
    strcpy(val, panel_get_value(legend_y_panel));
    legy = atof(val);
    for (i = 0; i < maxplot; i++) {
	strcpy(legstr[i].s, panel_get_value(legend_str_panel[i]));
    }
    drawgraph();
}

static void legend_loc_proc()
{
    set_action(0);
    set_action(LEG_LOC);
}

static void toggle_legends_proc(item, value, event)
    Panel_item item;
    unsigned int value;
    Event *event;
{
}

static void define_symbols_done_proc()
{
    window_set(define_symbols_frame, WIN_SHOW, FALSE, 0);
}

static void define_symbols_popup()
{
    int i;

    if (firstrun && !legendflag) {
	for (i = 0; i < MAXPLOT; i++)
	    legstr[i].s[0] = '\0';
	firstrun = FALSE;
    }
    define_symbols_frame = window_create(main_frame, FRAME,
					 FRAME_LABEL, "Symbols and legends",
					 FRAME_SHOW_LABEL, TRUE,
		      WIN_ERROR_MSG, "Couldn't create define_symbols_frame",
					 WIN_Y, 50,
					 0);
    define_symbols_panel = window_create(define_symbols_frame, PANEL, 0);

    panel_create_item(define_symbols_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
     panel_button_image(define_symbols_panel, "Define symbols", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_NOTIFY_PROC, define_symbols_proc,
		      0);
    for (i = 0; i < maxplot; i++) {
	sprintf(buf, "Set %2d  Sym:", i);
	toggle_symbols_item[i] = panel_create_item(define_symbols_panel, PANEL_CYCLE,
						   PANEL_LABEL_STRING, buf,
						   PANEL_CHOICE_STRINGS,
						   "No symbol",
						   "Dot",
						   "Square",
						   "Triangle",
						   "Diamond",
						   "Open star",
						   "Plus",
						   "X",
						   "Star",
						   "Solid circle",
						   "Solid box",
						   "Solid triangle",
						   "Solid triangle left",
						   "Solid triangle",
						   "Solid triangle",
						   "Solid star",
						   "Impulse",
						   "Line from bot to top",
						   "Histogram",
						   "Stair step",
						   "Bar",
						   "Location in set",
						   "Set #",
						   "Set #, location",
						   "Range of set",
						   0,
						   PANEL_ITEM_X, ATTR_COL(1),
					      PANEL_ITEM_Y, ATTR_ROW(2 + i),
				     PANEL_NOTIFY_PROC, toggle_symbols_proc,
						   0);
	toggle_pen_item[i] = panel_create_item(define_symbols_panel, PANEL_CYCLE,
					       PANEL_LABEL_STRING, "Pen:",
					       PANEL_CHOICE_STRINGS,
					       "No pen",
					       "Pen 1",
					       "Pen 2",
					       "Pen 3",
					       "Pen 4",
					       "Pen 5",
					       "Pen 6",
					       "Pen 7",
					       "Pen 8", 0,
					       PANEL_ITEM_X, ATTR_COL(38),
					       PANEL_ITEM_Y, ATTR_ROW(2 + i),
					 PANEL_NOTIFY_PROC, toggle_pen_proc,
					       0);
	toggle_lines_item[i] = panel_create_item(define_symbols_panel, PANEL_CYCLE,
						 PANEL_LABEL_STRING, "Line:",
						 PANEL_CHOICE_STRINGS,
						 "No line",
						 "Solid",
						 "Dotted",
						 "Dashed",
						 "Long Dashed",
						 "Dot-dashed", 0,
						 PANEL_ITEM_X, ATTR_COL(58),
					      PANEL_ITEM_Y, ATTR_ROW(2 + i),
				       PANEL_NOTIFY_PROC, toggle_lines_proc,
						 0);
    }
    panel_create_item(define_symbols_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
      panel_button_image(define_symbols_panel, "Define legend", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(18),
		      PANEL_NOTIFY_PROC, define_legends_proc,
		      0);
    for (i = 0; i < maxplot; i++) {
	sprintf(buf, "Set %2d  legend:", i);
	legend_str_panel[i] = panel_create_item(define_symbols_panel, PANEL_TEXT,
						PANEL_ITEM_X, ATTR_COL(1),
					     PANEL_ITEM_Y, ATTR_ROW(19 + i),
						PANEL_LABEL_STRING, buf,
					 PANEL_VALUE_DISPLAY_LENGTH, 50, 0);
    }
    legends_gap_item = panel_create_item(define_symbols_panel, PANEL_CYCLE,
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(34),
					 PANEL_LABEL_STRING, "Lgap:",
					 PANEL_CHOICE_STRINGS,
					 "1",
					 "2",
					 "3",
					 "4",
					 0, 0);
    legends_len_item = panel_create_item(define_symbols_panel, PANEL_CYCLE,
					 PANEL_ITEM_X, ATTR_COL(12),
					 PANEL_ITEM_Y, ATTR_ROW(34),
					 PANEL_LABEL_STRING, "Llen:",
					 PANEL_CHOICE_STRINGS,
					 "1",
					 "2",
					 "3",
					 "4",
					 "5",
					 "6",
					 "7",
					 "8",
					 0, 0);
    legend_x_panel = panel_create_item(define_symbols_panel, PANEL_TEXT,
				       PANEL_ITEM_X, ATTR_COL(1),
				       PANEL_ITEM_Y, ATTR_ROW(35),
				       PANEL_LABEL_STRING, "X location:",
				       PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    legend_y_panel = panel_create_item(define_symbols_panel, PANEL_TEXT,
				       PANEL_ITEM_X, ATTR_COL(1),
				       PANEL_ITEM_Y, ATTR_ROW(36),
				       PANEL_LABEL_STRING, "Y location:",
				       PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    toggle_legends_item = panel_create_item(define_symbols_panel, PANEL_CYCLE,
					    PANEL_LABEL_STRING, "Legends:",
					    PANEL_CHOICE_STRINGS,
					    "OFF",
					    "ON",
					    0,
					    PANEL_ITEM_X, ATTR_COL(1),
					    PANEL_ITEM_Y, ATTR_ROW(37),
				     PANEL_NOTIFY_PROC, toggle_legends_proc,
					    0);
    panel_create_item(define_symbols_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(define_symbols_panel, "Define location with mouse", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(38),
		      PANEL_NOTIFY_PROC, legend_loc_proc,
		      0);

    panel_create_item(define_symbols_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	       panel_button_image(define_symbols_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(40),
		      PANEL_NOTIFY_PROC, define_symbols_done_proc,
		      0);
    panel_create_item(define_symbols_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
    panel_button_image(define_symbols_panel, "Define error bar", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(30),
		      PANEL_ITEM_Y, ATTR_ROW(35),
		      PANEL_NOTIFY_PROC, define_errbar_proc,
		      0);
    error_from_set = panel_create_item(define_symbols_panel,
				       PANEL_CYCLE,
				 PANEL_LABEL_STRING, "Error bars from set:",
				       PANEL_CHOICE_STRINGS,
		       "Set 0", "Set 1", "Set 2", "Set 3", "Set 4", "Set 5",
		     "Set 6", "Set 7", "Set 8", "Set 9", "Set 10", "Set 11",
				       "Set 12", "Set 13", "Set 14", 0,
				       PANEL_ITEM_X, ATTR_COL(30),
				       PANEL_ITEM_Y, ATTR_ROW(36),
				       0);
    error_to_set = panel_create_item(define_symbols_panel,
				     PANEL_CYCLE,
				     PANEL_LABEL_STRING, "To set:",
				     PANEL_CHOICE_STRINGS,
		       "Set 0", "Set 1", "Set 2", "Set 3", "Set 4", "Set 5",
		     "Set 6", "Set 7", "Set 8", "Set 9", "Set 10", "Set 11",
				     "Set 12", "Set 13", "Set 14", 0,
				     PANEL_ITEM_X, ATTR_COL(60),
				     PANEL_ITEM_Y, ATTR_ROW(36),
				     0);
    error_xy_item = panel_create_item(define_symbols_panel, PANEL_CYCLE,
				      PANEL_LABEL_STRING, "Error bars on:",
				      PANEL_CHOICE_STRINGS,
				      "Y top and bottom",
				      "X left and right",
				      "Y top only",
				      "X left only",
				      "Y bottom only",
				      "X right only",
				      0,
				      PANEL_ITEM_X, ATTR_COL(30),
				      PANEL_ITEM_Y, ATTR_ROW(37),
				      0);
    panel_create_item(define_symbols_panel, PANEL_SLIDER,
		      PANEL_ITEM_X, ATTR_COL(30),
		      PANEL_ITEM_Y, ATTR_ROW(38),
		      PANEL_LABEL_STRING, "Error bar length %",
		      PANEL_VALUE, 50,
		      PANEL_MIN_VALUE, 1,
		      PANEL_MAX_VALUE, 100,
		      PANEL_NOTIFY_PROC, errbar_length_proc,
		      0);
    panel_create_item(define_symbols_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(define_symbols_panel, "Undefine error bar", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(30),
		      PANEL_ITEM_Y, ATTR_ROW(39),
		      PANEL_NOTIFY_PROC, undefine_errbar_proc,
		      0);
    window_fit(define_symbols_panel);
    window_fit(define_symbols_frame);
    for (i = 0; i < maxplot; i++) {
	updatesymbols(i);
	updatelegends(i);
    }
}
