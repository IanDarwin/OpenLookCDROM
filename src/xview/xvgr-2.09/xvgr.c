/* $Id: xvgr.c,v 1.54 92/08/22 19:37:32 pturner Exp Locker: pturner $
 *
 *  Initialize XView and do_main_loop()
 */

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sys/param.h>

#include <X11/Xlib.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/font.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/cms.h>
#include <xview/sel_attrs.h>
#include <xview/textsw.h>

/* experimenting with drag 'n drop */
#ifdef DND

#include <xview/dragdrop.h>
Xv_drop_site drop_site;

#define MY_DROP_SITE 1
Selection_requestor sel;

#endif

#include "globals.h"

/* paint window of canvas */
Xv_Window paint_window;

/* color map */
Cms cms;
extern Xv_singlecolor cmscolors[];
extern unsigned long colors[];
unsigned long *mcolors;
extern Colormap mycmap;

/* display */
extern Display *disp;

/* X drawable for canvas */
extern Window xwin;

/* toggle the locator on & off */
extern int go_locateflag;	/* defined in events.c */

/* an icon */
unsigned short closed_bits[] = {
#include "icon.h"
};

/* and an icon mask */
unsigned short mask_bits[] = {
#include "icon_mask.h"
};

Server_image closed_image, mask_image;
Icon icon;

Frame main_frame = (Frame) 0;	/* main frame and panel */
Panel main_panel;
Canvas canvas;			/* canvas to draw graph and associated pixwin */

Panel_item calc_item;		/* for calculator on top */
Panel_item locate_item;		/* locator on main_panel */
Panel_item stack_depth_item;	/* stack depth on main_panel */
Panel_item curw_item;		/* current world when cycling */

static Menu files_menu;		/* pulldown menus on main panel */
static Menu draw_menu;		/* view pulldown */
static Menu compose_menu;	/* edit pulldown */

/*
 * forward decls for callbacks
 */
void do_hardcopy();
void doforce_redraw();
void do_zoom();
void do_select_point();
void do_clear_point();
void my_proc();
void my_resize_proc();
void refresh();
void gwindNW_proc();
void gwindup_proc();
void gwindshrink_proc();
void gwindexpand_proc();
void gwindNE_proc();
void gwindleft_proc();
void gwindright_proc();
void gwindSW_proc();
void gwinddown_proc();
void gwindSE_proc();
void autoscale_proc();
void autoticks_proc();
void push_world();
void pop_world();
void push_and_zoom();
void cycle_world_stack();
void do_packsets();

/*
 * for graphs pullright
 */
void create_gactive_frame();
void create_gcopy_frame();
void create_gswap_frame();
void create_gkill_frame();
void create_gfocus_frame();
void create_gshow_frame();
void create_gtype_frame();

/*
 * for transformations pullright
 */
void create_eval_frame();
void create_load_frame();
void create_histo_frame();
void create_fourier_frame();
void create_run_frame();
void create_reg_frame();
void create_nonl_frame();
void create_diff_frame();
void create_int_frame();
void create_xcor_frame();
void create_spline_frame();
void create_samp_frame();
void create_digf_frame();
void create_lconv_frame();
void create_leval_frame();

/*
 * for Set operations pullright
 */
void create_activate_frame();
void create_deactivate_frame();
void create_reactivate_frame();
void create_change_frame();
void create_copy_frame();
void create_setlength_frame();
void create_move_frame();
void create_swap_frame();
void create_drop_frame();
void create_join_frame();
void create_split_frame();
void create_kill_frame();
void create_sort_frame();
void create_write_frame();
void create_saveall_frame();
void create_reverse_frame();
void create_coalesce_frame();
void do_flush();
void do_kill_nearest();
void do_copy_nearest();
void do_move_nearest();
void do_reverse_nearest();
void do_deactivate_nearest();
void do_join_nearest();
void do_delete_nearest();

/*
 * for Region operations pullright
 */
void create_define_frame();
void create_clear_frame();
void create_extract_frame();
void create_delete_frame();
void create_evalregion_frame();
void create_area_frame();

/*
 * for Strings & things pullright
 */
void define_objects_popup();
void define_strings_popup();
void define_lines_popup();
void define_boxes_popup();
void do_boxes_proc();
void do_lines_proc();
void do_move_proc();
void do_delete_object_proc();
void strings_loc_proc();
void strings_ang_proc();
void strings_edit_proc();
void edit_objects_proc();
void do_clear_lines();
void do_clear_boxes();
void do_clear_text();

/*
 * for Locator pullright
 */
void create_locator_frame();

void do_flipxy();
void do_invertx();
void do_inverty();

/*
 * for point operations pullright
 */
void do_find_points();
void do_track_points();
void do_delete_points();
void create_add_frame();
void do_move_points();
void do_movex_points();
void do_movey_points();
void create_goto_frame();

void update_worldview();	/* update world, view windows if any changes
				 * made during mouse controlled zoom */
void drawgraph();		/* calls plotone(), called any time the graph
				 * needs to be re-drawn not used as a canvas
				 * redraw proc */

void define_symbols_popup();	/* define set symbols popup */
void define_legend_popup();	/* define graph legend popup */
void define_points_popup();	/* edit points, defined in ptswin.c */
void create_printer_setup();
void create_about_grtool();
void create_props_frame();	/* set properties */
void create_page_frame();	/* set page properties */
void create_misc_frame();	/* set misc. items */
void create_frame_frame();
void create_draw_frame();
void create_view_frame();
void create_autos_frame();
void create_arrange_frame();
void create_default_frame();
void create_label_frame();
void create_graph_frame();
void define_strings_popup();	/* strings popup, routines in strwin.c */
void define_setops_popup();	/* set operations, routines in setwin.c */
void define_status_popup();	/* status window - information about sets */
void create_monitor_frame();	/* monitor - just testing */
void create_wparam_frame();	/* write parameter files */
void create_file_popup();	/* read XY data */
void create_block_frame();	/* read block data */
void create_eblock_frame();	/* read block data */
void create_writes_frame();	/* write sets */
void create_com_frame();	/* */
void create_parmsr_frame();	/* read parameters */
void create_world_frame();	/* define the world dimensions, defined in
				 * worldwin.c */
void create_ticks_frame();
void define_colors_popup();	/* defined in colorwin.c */
void create_editp_frame();

void update_set_status();

/*
 * init display
 */
void initialize_screen(argc, argv)
    int *argc;
    char **argv;
{
    if (xv_init(XV_INIT_ARGC_PTR_ARGV, argc, argv, NULL) == NULL) {
	fprintf(stderr, "xv_init failed, server problems?\n");
	exit(1);
    }
    invert = defaults_get_boolean("xvgr.invertdraw", "Xvgr.InvertDraw", 0);
    backingstore = defaults_get_boolean("xvgr.backingstore", "Xvgr.Backingstore", 0);
    allow_refresh = defaults_get_boolean("xvgr.allowrefresh", "Xvgr.AllowRefresh", 1);
    redraw_now = defaults_get_integer("xvgr.initialdraw", "Xvgr.InitialDraw", 0);
    revflag = defaults_get_boolean("xvgr.reversevideo", "Xvgr.ReverseVideo", 0);
    maxplot = defaults_get_integer("xvgr.maxsets", "Xvgr.MaxSets", MAXPLOT);
    maxgraph = defaults_get_integer("xvgr.maxgraphs", "Xvgr.MaxGraphs", MAXGRAPH);
    maxcolors = defaults_get_integer("xvgr.maxcolors", "Xvgr.MaxColors", MAXCOLORS);
    verify_action = defaults_get_boolean("xvgr.verifyaction", "Xvgr.VerifyAction", 0);
    allow_dc = defaults_get_boolean("xvgr.allowdoubleclick", "Xvgr.AllowDoubleClick", 1);
    autoscale_onread = defaults_get_boolean("xvgr.autoscaleonread", "Xvgr.AutoscaleOnRead", 0);
}

/*
 * show the current state of potentially long running commands
 * in the footer
 */
void set_right_footer(s)
    char *s;
{
    char *str;
    struct tm tm, *localtime();
    long time_value;
    char name[256];

    if (inwin) {
	if (s) {
	    xv_set(main_frame, FRAME_RIGHT_FOOTER, s, NULL);
	} else {
	    (void) time(&time_value);
	    tm = *localtime(&time_value);
	    str = asctime(&tm);
	    strcpy(name, disp->display_name);
	    strcat(name, " | ");
	    strcat(name, str);
	    xv_set(main_frame, FRAME_RIGHT_FOOTER, name, NULL);
	}
    }
}

void set_left_footer(s)
    char *s;
{
    if (inwin) {
	xv_set(main_frame, FRAME_LEFT_FOOTER, s, NULL);
    }
}

/*
 * for ^X received by the canvas event proc
 */
void bailout()
{
    if (xv_destroy_safe(main_frame) == XV_OK) {
	exit(0);
    }
}

/*
 * exit by button on front panel
 */
/*ARGSUSED*/
static void quit_main_proc(item, event)
    Panel_item item;
    Event *event;
{
    bailout();
}

/*
 * set action to SEL_POINT for selecting the locator reference point
 */
static void do_select_point()
{
    set_action(0);
    set_action(SEL_POINT);
    g[cg].pointset = TRUE;
}

/*
 * clear the locator reference point
 */
static void do_clear_point()
{
    g[cg].pointset = FALSE;
    g[cg].pt_type = 0;
    g[cg].dsx = g[cg].dsy = 0.0;
}

/*
 * total wipeout of graphs, sets, annotation
 */
void clear_all()
{
    wipeout(1);
}

/*
 * set current directory labels
 */
void set_curdir()
{
    char d[MAXPATHLEN];
    extern Panel_item block_dir_msg_item;
    extern Panel_item comr_dir_msg_item;
    extern Panel_item comw_dir_msg_item;
    extern Panel_item files_dir_msg_item;
    extern Panel_item parmsr_dir_msg_item;
    extern Panel_item parmsw_dir_msg_item;
    extern Panel_item write_dir_msg_item;
    extern Panel_item saveall_dir_msg_item;

    getcwd(d, MAXPATHLEN);
    xv_set(main_frame, XV_LABEL, d, NULL);
    if (files_dir_msg_item) {
	xv_set(files_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (block_dir_msg_item) {
	xv_set(block_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (comr_dir_msg_item) {
	xv_set(comr_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (comw_dir_msg_item) {
	xv_set(comw_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (parmsr_dir_msg_item) {
	xv_set(parmsr_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (parmsw_dir_msg_item) {
	xv_set(parmsw_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (write_dir_msg_item) {
	xv_set(write_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
    if (saveall_dir_msg_item) {
	xv_set(saveall_dir_msg_item, PANEL_LABEL_STRING, d, NULL);
    }
}

/*
 * evaluate an expression for the calculator
 */
/*ARGSUSED*/
static void do_calc_proc(item, event)
    Panel_item item;
    Event *event;
{
    static double a = 0.0;
    static double b = 0.0;
    static double c = 0.0;
    static double d = 0.0;
    static double x = 0.0;
    static double y = 0.0;
    static int errpos;
    static char val[128];
    extern double result;

    errpos = 0;
    strcpy(val, (char *) xv_get(calc_item, PANEL_VALUE));
    fixupstr(val);
    scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
    if (errpos) {
	xv_set(calc_item, PANEL_VALUE, "error", NULL);
    } else {
	sprintf(val, "%.5g", result);
	xv_set(calc_item, PANEL_VALUE, val, NULL);
    }
}

/*
 * main panel
 */
void do_main_loop()
{
    int i;
    Menu m1, m2, m3, m4, m5;
    char name[256];

    getcwd(name, 255);

    main_frame = (Frame) xv_create(NULL, FRAME,
				   FRAME_SHOW_FOOTER, TRUE,
				   FRAME_LEFT_FOOTER, "Locator",
				   FRAME_RIGHT_FOOTER, "Idle",
				   XV_WIDTH, 800,
				   XV_HEIGHT, 700,
				WIN_ERROR_MSG, "Couldn't create main_frame",
				   XV_LABEL, name, NULL);
/*
				   XV_LABEL, "XVgr", NULL);
*/
    main_panel = (Panel) xv_create(main_frame, PANEL,
				   XV_HELP_DATA, "xvgr:main_panel",
				   WIN_ROW_GAP, 1,
				   NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Re-draw",
		     PANEL_NOTIFY_PROC, doforce_redraw,
		     XV_HELP_DATA, "xvgr:redraw",
		     XV_X, xv_col(main_panel, 25),
		     XV_Y, xv_row(main_panel, 0),
		     NULL);
    locate_item = (Panel_item) xv_create(main_panel, PANEL_MESSAGE,
					 XV_X, xv_col(main_panel, 0),
					 XV_Y, xv_row(main_panel, 1) + 5,
					 XV_HELP_DATA, "xvgr:locator",
					 PANEL_LABEL_STRING, "[X,Y]",
					 NULL);
    calc_item = (Panel_item) xv_create(main_panel, PANEL_TEXT,
				       PANEL_LABEL_STRING, "Calc: ",
				       XV_HELP_DATA, "xvgr:calc",
				       XV_X, xv_col(main_panel, 0),
				       XV_Y, xv_row(main_panel, 2) + 5,
				       PANEL_NOTIFY_STRING, "\r\t\n",
				       PANEL_NOTIFY_PROC, do_calc_proc,
				       PANEL_VALUE_DISPLAY_LENGTH, 40,
				       NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 35),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:zoom",
		     PANEL_LABEL_STRING, "Z",
		     PANEL_NOTIFY_PROC, do_zoom,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 39),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:out",
		     PANEL_LABEL_STRING, "O",
		     PANEL_NOTIFY_PROC, gwindexpand_proc,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 43),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:in",
		     PANEL_LABEL_STRING, "I",
		     PANEL_NOTIFY_PROC, gwindshrink_proc,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 47),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:autoscale",
		     PANEL_LABEL_STRING, "AS",
		     PANEL_NOTIFY_PROC, autoscale_proc,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 53),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:autotick",
		     PANEL_LABEL_STRING, "AT",
		     PANEL_NOTIFY_PROC, autoticks_proc,
		     NULL);
    stack_depth_item = (Panel_item) xv_create(main_panel, PANEL_MESSAGE,
					      XV_X, xv_col(main_panel, 72),
					      XV_Y, xv_row(main_panel, 0),
					    XV_HELP_DATA, "xvgr:stackdepth",
					      PANEL_LABEL_STRING, "SD:0",
					      NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 60),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:pushworld",
		     PANEL_LABEL_STRING, "PU",
		     PANEL_NOTIFY_PROC, push_world,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 66),
		     XV_Y, xv_row(main_panel, 0),
		     XV_HELP_DATA, "xvgr:popworld",
		     PANEL_LABEL_STRING, "PO",
		     PANEL_NOTIFY_PROC, pop_world,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 60),
		     XV_Y, xv_row(main_panel, 1),
		     XV_HELP_DATA, "xvgr:pushnzoom",
		     PANEL_LABEL_STRING, "PZ",
		     PANEL_NOTIFY_PROC, push_and_zoom,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 66),
		     XV_Y, xv_row(main_panel, 1),
		     XV_HELP_DATA, "xvgr:cycle",
		     PANEL_LABEL_STRING, "CY",
		     PANEL_NOTIFY_PROC, cycle_world_stack,
		     NULL);
    curw_item = (Panel_item) xv_create(main_panel, PANEL_MESSAGE,
				       XV_X, xv_col(main_panel, 72),
				       XV_Y, xv_row(main_panel, 1),
				       XV_HELP_DATA, "xvgr:currentcycle",
				       PANEL_LABEL_STRING, "CW:0",
				       NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 35),
		     XV_Y, xv_row(main_panel, 1),
		     XV_HELP_DATA, "xvgr:left",
		     PANEL_LABEL_STRING, "L",
		     PANEL_NOTIFY_PROC, gwindleft_proc,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 39),
		     XV_Y, xv_row(main_panel, 1),
		     XV_HELP_DATA, "xvgr:right",
		     PANEL_LABEL_STRING, "R",
		     PANEL_NOTIFY_PROC, gwindright_proc,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 43),
		     XV_Y, xv_row(main_panel, 1),
		     XV_HELP_DATA, "xvgr:up",
		     PANEL_LABEL_STRING, "U",
		     PANEL_NOTIFY_PROC, gwindup_proc,
		     NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     XV_X, xv_col(main_panel, 47),
		     XV_Y, xv_row(main_panel, 1),
		     XV_HELP_DATA, "xvgr:down",
		     PANEL_LABEL_STRING, "D",
		     PANEL_NOTIFY_PROC, gwinddown_proc,
		     NULL);

    window_fit_height(main_panel);

/*
 * initialize canvas and set colors
 */

    disp = (Display *) xv_get(main_frame, XV_DISPLAY);
    if (monomode) {
	use_colors = 1;
    } else {
	use_colors = DisplayPlanes(disp, DefaultScreen(disp));
    }
    if (use_colors > 4) {
	for (i = 0; i < MAXGRAPH; i++) {
	    if (g[i].parmsread != TRUE) {
		setdefaultcolors(i);
	    }
	}
    }
    if (use_colors > 4) {
	/* one more allocated than needed */
	cms = (Cms) xv_create(NULL, CMS,
			      CMS_NAME, "palette",
			      CMS_TYPE, XV_DYNAMIC_CMS,
			      CMS_SIZE, maxcolors + 1,
			      CMS_COLORS, cmscolors,
			      NULL);
	canvas = (Canvas) xv_create(main_frame, CANVAS,
/*
				    XV_WIDTH, 800,
				    XV_HEIGHT, 700,
*/
				    CANVAS_WIDTH, 800,
				    CANVAS_HEIGHT, 700,
				    CANVAS_AUTO_EXPAND, TRUE,
				    CANVAS_AUTO_SHRINK, TRUE,
				    CANVAS_AUTO_CLEAR, TRUE,
				    CANVAS_RETAINED, TRUE,
				    WIN_RETAINED, TRUE,
				    XV_SHOW, TRUE,
				    WIN_CMS, cms,
				    NULL);
	mcolors = (unsigned long *) xv_get(canvas, WIN_X_COLOR_INDICES);
	for (i = 0; i < maxcolors; i++) {
	    colors[i] = mcolors[i];
	}
    } else {
	canvas = (Canvas) xv_create(main_frame, CANVAS,
/*
				    XV_WIDTH, 800,
				    XV_HEIGHT, 700,
*/
				    CANVAS_WIDTH, 800,
				    CANVAS_HEIGHT, 700,
				    CANVAS_AUTO_EXPAND, TRUE,
				    CANVAS_AUTO_SHRINK, TRUE,
				    CANVAS_AUTO_CLEAR, TRUE,
				    CANVAS_RETAINED, TRUE,
				    WIN_RETAINED, TRUE,
				    XV_SHOW, TRUE,
				    NULL);
    }
    colors[0] = WhitePixel(disp, DefaultScreen(disp));
    colors[1] = BlackPixel(disp, DefaultScreen(disp));
    if (revflag) {
	iswap(&colors[0], &colors[1]);
    }
    xv_set(canvas_paint_window(canvas), WIN_CONSUME_EVENTS,
	   WIN_ASCII_EVENTS,
	   LOC_DRAG,
	   LOC_MOVE,
	   LOC_WINENTER,
	   LOC_WINEXIT,
	   WIN_MOUSE_BUTTONS,
	   NULL,
#ifndef DND
	   WIN_IGNORE_EVENTS,
	   WIN_UP_EVENTS,
	   NULL,
#endif
	   WIN_EVENT_PROC, my_proc,
	   NULL);

    paint_window = canvas_paint_window(canvas);
    xwin = (Window) xv_get(paint_window, XV_XID);

/*
 * create pulldown menus
 */
    files_menu = (Menu) menu_create(
				    MENU_GEN_PIN_WINDOW, main_frame, "Files",
				    MENU_ITEM, MENU_STRING,
				    "Read sets...",
				    XV_HELP_DATA, "xvgr:read_sets",
				 MENU_NOTIFY_PROC, create_file_popup, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Read parameters...",
				    XV_HELP_DATA, "xvgr:read_parms",
				MENU_NOTIFY_PROC, create_parmsr_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Read block data...",
				    XV_HELP_DATA, "xvgr:read_blocks",
				 MENU_NOTIFY_PROC, create_block_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Save all...",
				    XV_HELP_DATA, "xvgr:save_all",
			       MENU_NOTIFY_PROC, create_saveall_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Write sets...",
				    XV_HELP_DATA, "xvgr:write_sets",
				 MENU_NOTIFY_PROC, create_write_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Write parameters...",
				    XV_HELP_DATA, "xvgr:write_parms",
				MENU_NOTIFY_PROC, create_wparam_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Clear all...",
				    XV_HELP_DATA, "xvgr:clear_all",
				    MENU_NOTIFY_PROC, clear_all, NULL,
				    MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Print",
				    XV_HELP_DATA, "xvgr:print",
				    MENU_NOTIFY_PROC, do_hardcopy, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Printer setup...",
				    XV_HELP_DATA, "xvgr:printer_setup",
			       MENU_NOTIFY_PROC, create_printer_setup, NULL,
				    MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Command interpreter...",
				    XV_HELP_DATA, "xvgr:commands",
				    MENU_NOTIFY_PROC, create_com_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Status...",
				    XV_HELP_DATA, "xvgr:status",
				MENU_NOTIFY_PROC, define_status_popup, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Results...",
				    XV_HELP_DATA, "xvgr:monitor",
			       MENU_NOTIFY_PROC, create_monitor_frame, NULL,
				    MENU_ITEM, MENU_STRING,
				    "About...",
				    XV_HELP_DATA, "xvgr:about",
				MENU_NOTIFY_PROC, create_about_grtool, NULL,
				    MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				    MENU_ITEM, MENU_STRING,
				    "Exit",
				    XV_HELP_DATA, "xvgr:exit",
				    MENU_NOTIFY_PROC, quit_main_proc, NULL,
				    NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "File",
		     PANEL_ITEM_MENU, files_menu,
		     XV_HELP_DATA, "xvgr:files_menu",
		     XV_X, xv_col(main_panel, 0),
		     XV_Y, xv_row(main_panel, 0),
		     NULL);
/*
 * create view pulldown menu
 */
    m1 = (Menu) xv_create(NULL, MENU,
			  MENU_GEN_PIN_WINDOW, main_frame, "Graph ops",
			  MENU_ITEM,
			  MENU_STRING, "Activate...",
			  XV_HELP_DATA, "xvgr:graph_activate",
			  MENU_NOTIFY_PROC, create_gactive_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Copy...",
			  XV_HELP_DATA, "xvgr:graph_copy",
			  MENU_NOTIFY_PROC, create_gcopy_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Swap...",
			  XV_HELP_DATA, "xvgr:graph_swap",
			  MENU_NOTIFY_PROC, create_gswap_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Kill...",
			  XV_HELP_DATA, "xvgr:graph_kill",
			  MENU_NOTIFY_PROC, create_gkill_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Focus...",
			  XV_HELP_DATA, "xvgr:graph_focus",
			  MENU_NOTIFY_PROC, create_gfocus_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Show...",
			  XV_HELP_DATA, "xvgr:graph_show",
			  MENU_NOTIFY_PROC, create_gshow_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Set graph type...",
			  XV_HELP_DATA, "xvgr:graph_type",
			  MENU_NOTIFY_PROC, create_gtype_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Arrange graphs...",
			  XV_HELP_DATA, "xvgr:graph_arrange",
			  MENU_NOTIFY_PROC, create_arrange_frame, NULL,
			  NULL);
    m2 = (Menu) xv_create(NULL, MENU,
			MENU_GEN_PIN_WINDOW, main_frame, "Strings & things",
			  MENU_ITEM,
			  MENU_STRING, "Text",
			  XV_HELP_DATA, "xvgr:objects_text",
			  MENU_NOTIFY_PROC, strings_loc_proc, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Text at angle",
			  XV_HELP_DATA, "xvgr:objects_text_at_angle",
			  MENU_NOTIFY_PROC, strings_ang_proc, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Edit Text",
			  XV_HELP_DATA, "xvgr:objects_edit_text",
			  MENU_NOTIFY_PROC, strings_edit_proc, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Text props...",
			  XV_HELP_DATA, "xvgr:objects_text_props",
			  MENU_NOTIFY_PROC, define_strings_popup, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Line",
			  XV_HELP_DATA, "xvgr:objects_line",
			  MENU_NOTIFY_PROC, do_lines_proc, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Line props...",
			  XV_HELP_DATA, "xvgr:objects_line_props",
			  MENU_NOTIFY_PROC, define_lines_popup, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Box",
			  XV_HELP_DATA, "xvgr:objects_box",
			  MENU_NOTIFY_PROC, do_boxes_proc, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Box props...",
			  XV_HELP_DATA, "xvgr:objects_box_props",
			  MENU_NOTIFY_PROC, define_boxes_popup, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Move object",
			  XV_HELP_DATA, "xvgr:objects_move",
			  MENU_NOTIFY_PROC, do_move_proc, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Delete object",
			  XV_HELP_DATA, "xvgr:objects_delete",
			  MENU_NOTIFY_PROC, do_delete_object_proc, NULL,
/*
			  MENU_ITEM,
			  MENU_STRING, "Edit object",
			  XV_HELP_DATA, "xvgr:objects_edit",
			  MENU_NOTIFY_PROC, edit_objects_proc, NULL,
*/
			  MENU_ITEM,
			  MENU_STRING, "Clear all text",
			  XV_HELP_DATA, "xvgr:objects_clear_text",
			  MENU_NOTIFY_PROC, do_clear_text, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Clear all lines",
			  XV_HELP_DATA, "xvgr:objects_clear_lines",
			  MENU_NOTIFY_PROC, do_clear_lines, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Clear all boxes",
			  XV_HELP_DATA, "xvgr:objects_clear_boxes",
			  MENU_NOTIFY_PROC, do_clear_boxes, NULL,
			  NULL);

    m3 = (Menu) xv_create(NULL, MENU,
			  MENU_ITEM,
			  MENU_STRING, "Set fixed point",
			  XV_HELP_DATA, "xvgr:fixedpoint_set",
			  MENU_NOTIFY_PROC, do_select_point, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Clear fixed point",
			  XV_HELP_DATA, "xvgr:fixedpoint_clear",
			  MENU_NOTIFY_PROC, do_clear_point, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Props...",
			  XV_HELP_DATA, "xvgr:fixedpoint_props",
			  MENU_NOTIFY_PROC, create_locator_frame, NULL,
			  NULL);

    draw_menu = (Menu) menu_create(
				   MENU_GEN_PIN_WINDOW, main_frame, "View",
				   MENU_ITEM, MENU_STRING,
				   "Graphs",
				   XV_HELP_DATA, "xvgr:graphs_pullright",
				   MENU_PULLRIGHT, m1,
				   NULL,
				   MENU_ITEM, MENU_STRING,
				   "Define world...",
				   XV_HELP_DATA, "xvgr:world",
				 MENU_NOTIFY_PROC, create_world_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Define view...",
				   XV_HELP_DATA, "xvgr:view",
				   MENU_NOTIFY_PROC, create_view_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Autoscale...",
				   XV_HELP_DATA, "xvgr:autoscale",
				 MENU_NOTIFY_PROC, create_autos_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Draw options...",
				   XV_HELP_DATA, "xvgr:draw_options",
				   MENU_NOTIFY_PROC, create_draw_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Toggles...",
				   XV_HELP_DATA, "xvgr:props",
				   MENU_NOTIFY_PROC, create_props_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Page layout...",
				   XV_HELP_DATA, "xvgr:page",
				   MENU_NOTIFY_PROC, create_page_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Misc....",
				   XV_HELP_DATA, "xvgr:misc",
				   MENU_NOTIFY_PROC, create_misc_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Title/subtitle...",
				   XV_HELP_DATA, "xvgr:title",
				 MENU_NOTIFY_PROC, create_label_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Ticks/tick labels...",
				   XV_HELP_DATA, "xvgr:ticks",
				 MENU_NOTIFY_PROC, create_ticks_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Symbols...",
				   XV_HELP_DATA, "xvgr:symbols",
			       MENU_NOTIFY_PROC, define_symbols_popup, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Legends...",
				   XV_HELP_DATA, "xvgr:legends",
				MENU_NOTIFY_PROC, define_legend_popup, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Frame...",
				   XV_HELP_DATA, "xvgr:frame",
				 MENU_NOTIFY_PROC, create_frame_frame, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Strings & things",
				   XV_HELP_DATA, "xvgr:objects",
				   MENU_PULLRIGHT, m2,
				   NULL,
				   MENU_ITEM, MENU_STRING,
				   "", MENU_INACTIVE, TRUE, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Flip XY",
				   XV_HELP_DATA, "xvgr:flipxy",
				   MENU_NOTIFY_PROC, do_flipxy, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Invert X",
				   XV_HELP_DATA, "xvgr:invertx",
				   MENU_NOTIFY_PROC, do_invertx, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Invert Y",
				   XV_HELP_DATA, "xvgr:inverty",
				   MENU_NOTIFY_PROC, do_inverty, NULL,
				   MENU_ITEM, MENU_STRING,
				   "Locator",
				   XV_HELP_DATA, "xvgr:locator",
				   MENU_PULLRIGHT, m3,
				   NULL,
				   NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "View",
		     PANEL_ITEM_MENU, draw_menu,
		     XV_HELP_DATA, "xvgr:view_menu",
		     XV_X, xv_col(main_panel, 8),
		     XV_Y, xv_row(main_panel, 0),
		     NULL);
/*
 * create edit pulldown
 */
    m1 = (Menu) xv_create(NULL, MENU,
			  MENU_GEN_PIN_WINDOW, main_frame, "Transformations",
			  MENU_ITEM,
			  MENU_STRING, "Evaluate expressions...",
			  XV_HELP_DATA, "xvgr:evaluate",
			  MENU_NOTIFY_PROC, create_eval_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Load values...",
			  XV_HELP_DATA, "xvgr:load",
			  MENU_NOTIFY_PROC, create_load_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Load and evaluate...",
			  XV_HELP_DATA, "xvgr:load_and_eval",
			  MENU_NOTIFY_PROC, create_leval_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Regression...",
			  XV_HELP_DATA, "xvgr:regress",
			  MENU_NOTIFY_PROC, create_reg_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Non-linear curve fitting...",
			  XV_HELP_DATA, "xvgr:nonl",
			  MENU_NOTIFY_PROC, create_nonl_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Running averages...",
			  XV_HELP_DATA, "xvgr:running",
			  MENU_NOTIFY_PROC, create_run_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Histograms...",
			  XV_HELP_DATA, "xvgr:histograms",
			  MENU_NOTIFY_PROC, create_histo_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Fourier transforms...",
			  XV_HELP_DATA, "xvgr:fourier",
			  MENU_NOTIFY_PROC, create_fourier_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Integration...",
			  XV_HELP_DATA, "xvgr:integrate",
			  MENU_NOTIFY_PROC, create_int_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Differentiate...",
			  XV_HELP_DATA, "xvgr:differentiate",
			  MENU_NOTIFY_PROC, create_diff_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Cross/Auto correlation...",
			  XV_HELP_DATA, "xvgr:xcor",
			  MENU_NOTIFY_PROC, create_xcor_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Splines...",
			  XV_HELP_DATA, "xvgr:splines",
			  MENU_NOTIFY_PROC, create_spline_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Sample...",
			  XV_HELP_DATA, "xvgr:sample",
			  MENU_NOTIFY_PROC, create_samp_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Digital filter...",
			  XV_HELP_DATA, "xvgr:digf",
			  MENU_NOTIFY_PROC, create_digf_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Linear convolution...",
			  XV_HELP_DATA, "xvgr:linc",
			  MENU_NOTIFY_PROC, create_lconv_frame, NULL,
			  NULL);

    m5 = (Menu) xv_create(NULL, MENU,
		     MENU_GEN_PIN_WINDOW, main_frame, "Pick set operations",
			  MENU_ITEM, MENU_STRING,
			  "", MENU_INACTIVE, TRUE, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Kill nearest set",
			  XV_HELP_DATA, "xvgr:pick_killnearest",
			  MENU_NOTIFY_PROC, do_kill_nearest, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Copy nearest set",
			  XV_HELP_DATA, "xvgr:pick_copynearest",
			  MENU_NOTIFY_PROC, do_copy_nearest, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Move nearest set",
			  XV_HELP_DATA, "xvgr:pick_movenearest",
			  MENU_NOTIFY_PROC, do_move_nearest, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Reverse nearest set",
			  XV_HELP_DATA, "xvgr:pick_reversenearest",
			  MENU_NOTIFY_PROC, do_reverse_nearest, NULL,
			  MENU_ITEM,
			  MENU_STRING, "De-activate nearest set",
			  XV_HELP_DATA, "xvgr:pick_deactivatenearest",
			  MENU_NOTIFY_PROC, do_deactivate_nearest, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Join nearest sets",
			  XV_HELP_DATA, "xvgr:pick_joinnearest",
			  MENU_NOTIFY_PROC, do_join_nearest, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Delete range in nearest set",
			  XV_HELP_DATA, "xvgr:pick_deletenearest",
			  MENU_NOTIFY_PROC, do_delete_nearest, NULL,
			  NULL);

    m2 = (Menu) xv_create(NULL, MENU,
			  MENU_GEN_PIN_WINDOW, main_frame, "Set operations",
			  MENU_ITEM, MENU_STRING,
			  "Pick operations",
			  XV_HELP_DATA, "xvgr:set_pickops",
			  MENU_PULLRIGHT, m5, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Activate...",
			  XV_HELP_DATA, "xvgr:set_activate",
			  MENU_NOTIFY_PROC, create_activate_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "De-activate...",
			  XV_HELP_DATA, "xvgr:set_deactivate",
			  MENU_NOTIFY_PROC, create_deactivate_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Re-activate...",
			  XV_HELP_DATA, "xvgr:set_reactivate",
			  MENU_NOTIFY_PROC, create_reactivate_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Set length...",
			  XV_HELP_DATA, "xvgr:set_length",
			  MENU_NOTIFY_PROC, create_setlength_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Change set type...",
			  XV_HELP_DATA, "xvgr:set_change",
			  MENU_NOTIFY_PROC, create_change_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Copy...",
			  XV_HELP_DATA, "xvgr:set_copy",
			  MENU_NOTIFY_PROC, create_copy_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Move...",
			  XV_HELP_DATA, "xvgr:set_move",
			  MENU_NOTIFY_PROC, create_move_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Swap...",
			  XV_HELP_DATA, "xvgr:set_swap",
			  MENU_NOTIFY_PROC, create_swap_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Drop points...",
			  XV_HELP_DATA, "xvgr:set_drop",
			  MENU_NOTIFY_PROC, create_drop_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Join sets...",
			  XV_HELP_DATA, "xvgr:set_join",
			  MENU_NOTIFY_PROC, create_join_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Split...",
			  XV_HELP_DATA, "xvgr:set_split",
			  MENU_NOTIFY_PROC, create_split_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Kill...",
			  XV_HELP_DATA, "xvgr:set_kill",
			  MENU_NOTIFY_PROC, create_kill_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Kill all",
			  XV_HELP_DATA, "xvgr:set_killall",
			  MENU_NOTIFY_PROC, do_flush, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Sort...",
			  XV_HELP_DATA, "xvgr:set_sort",
			  MENU_NOTIFY_PROC, create_sort_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Write sets...",
			  XV_HELP_DATA, "xvgr:set_write",
			  MENU_NOTIFY_PROC, create_write_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Reverse order...",
			  XV_HELP_DATA, "xvgr:set_reverse",
			  MENU_NOTIFY_PROC, create_reverse_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Coalesce sets...",
			  XV_HELP_DATA, "xvgr:set_coalesce",
			  MENU_NOTIFY_PROC, create_coalesce_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Pack sets",
			  XV_HELP_DATA, "xvgr:set_pack",
			  MENU_NOTIFY_PROC, do_packsets, NULL,
			  NULL);

    m3 = (Menu) xv_create(NULL, MENU,
		       MENU_GEN_PIN_WINDOW, main_frame, "Region operations",
			  MENU_ITEM,
			  MENU_STRING, "Define region...",
			  XV_HELP_DATA, "xvgr:region_define",
			  MENU_NOTIFY_PROC, create_define_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Evaluate in region...",
			  XV_HELP_DATA, "xvgr:region_eval",
			  MENU_NOTIFY_PROC, create_evalregion_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Clear region...",
			  XV_HELP_DATA, "xvgr:region_clear",
			  MENU_NOTIFY_PROC, create_clear_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Extract points...",
			  XV_HELP_DATA, "xvgr:region_extract",
			  MENU_NOTIFY_PROC, create_extract_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Delete points...",
			  XV_HELP_DATA, "xvgr:region_delete",
			  MENU_NOTIFY_PROC, create_delete_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Area/perimeter...",
			  XV_HELP_DATA, "xvgr:region_area",
			  MENU_NOTIFY_PROC, create_area_frame, NULL,
			  NULL);

    m4 = (Menu) xv_create(NULL, MENU,
			MENU_GEN_PIN_WINDOW, main_frame, "Point operations",
			  MENU_ITEM,
			  MENU_STRING, "Find points",
			  XV_HELP_DATA, "xvgr:points_find",
			  MENU_NOTIFY_PROC, do_find_points, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Tracker",
			  XV_HELP_DATA, "xvgr:points_tracker",
			  MENU_NOTIFY_PROC, do_track_points, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Delete",
			  XV_HELP_DATA, "xvgr:points_delete",
			  MENU_NOTIFY_PROC, do_delete_points, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Add...",
			  XV_HELP_DATA, "xvgr:points_add",
			  MENU_NOTIFY_PROC, create_add_frame, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Move",
			  XV_HELP_DATA, "xvgr:points_move",
			  MENU_NOTIFY_PROC, do_move_points, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Move along X",
			  XV_HELP_DATA, "xvgr:points_movex",
			  MENU_NOTIFY_PROC, do_movex_points, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Move along Y",
			  XV_HELP_DATA, "xvgr:points_movey",
			  MENU_NOTIFY_PROC, do_movey_points, NULL,
			  MENU_ITEM,
			  MENU_STRING, "Goto...",
			  XV_HELP_DATA, "xvgr:points_goto",
			  MENU_NOTIFY_PROC, create_goto_frame, NULL,
			  NULL);
    compose_menu = (Menu) menu_create(
				    MENU_GEN_PIN_WINDOW, main_frame, "Edit",
				      MENU_ITEM, MENU_STRING,
				      "Transformations",
				  XV_HELP_DATA, "xvgr:edit_transformations",
				      MENU_PULLRIGHT, m1,
				      NULL,
				      MENU_ITEM, MENU_STRING,
				      "Set operations",
				      XV_HELP_DATA, "xvgr:edit_setops",
				      MENU_PULLRIGHT, m2,
				      NULL,
				      MENU_ITEM, MENU_STRING,
				      "Region operations",
				      XV_HELP_DATA, "xvgr:edit_regionops",
				      MENU_PULLRIGHT, m3,
				      NULL,
				      MENU_ITEM, MENU_STRING,
				      "Edit sets...",
				      XV_HELP_DATA, "xvgr:edit_sets",
				 MENU_NOTIFY_PROC, create_editp_frame, NULL,
				      MENU_ITEM, MENU_STRING,
				      "Point operations",
				      XV_HELP_DATA, "xvgr:edit_points",
				      MENU_PULLRIGHT, m4,
				      NULL,
				      MENU_ITEM, MENU_STRING,
				      "Block data...",
				      XV_HELP_DATA, "xvgr:edit_block",
				MENU_NOTIFY_PROC, create_eblock_frame, NULL,
				      NULL);
    (void) xv_create(main_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Edit",
		     XV_HELP_DATA, "xvgr:edit_menu",
		     PANEL_ITEM_MENU, compose_menu,
		     XV_X, xv_col(main_panel, 17),
		     XV_Y, xv_row(main_panel, 0),
		     NULL);

/*
 * init the icon
 */
    closed_image = (Server_image) xv_create(NULL, SERVER_IMAGE,
					    XV_WIDTH, 64,
					    XV_HEIGHT, 64,
					    SERVER_IMAGE_BITS, closed_bits,
					    NULL);
    mask_image = (Server_image) xv_create(NULL, SERVER_IMAGE,
					  XV_WIDTH, 64,
					  XV_HEIGHT, 64,
					  SERVER_IMAGE_BITS, mask_bits,
					  NULL);

/*
 * set the icon
 */
    icon = (Icon) xv_create(NULL, ICON,
			    ICON_IMAGE, closed_image,
			    ICON_MASK_IMAGE, mask_image,
			    ICON_TRANSPARENT, TRUE,
			    NULL);
    xv_set(main_frame, FRAME_ICON, icon, NULL);

#ifdef DND
    drop_site = xv_create(canvas_paint_window(canvas), DROP_SITE_ITEM,
			  DROP_SITE_ID, MY_DROP_SITE,
			  DROP_SITE_EVENT_MASK, DND_ENTERLEAVE, 0);
    sel = xv_create(canvas, SELECTION_REQUESTOR, 0);
#endif

/*
 * initialize the cursors
 */
    init_cursors();

/*
 * define the resize and repaint procs
 */
    (void) xv_set(canvas,
		  CANVAS_RESIZE_PROC, my_resize_proc,
		  CANVAS_REPAINT_PROC, refresh, NULL);
/*
 * the following xv_set maps the window so that in a pipe,
 * graphics can be seen
 */
    xv_set(main_frame, XV_SHOW, TRUE, NULL);

/*
 * update all popups
 */
    update_all(cg);

    xv_main_loop(main_frame);
}
