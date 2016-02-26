/*
 * $Id: tool.c,v 2.3 1994/08/29 17:35:04 billr Exp $
 */
/*
 * calentool - day/week/month/year-at-a-glance calendar for XView/Open Look
 * 
 * Original suntool source Copyright (C) 1987, Sun Microsystems, Inc.
 * 	All Rights Reserved
 * Permission is hereby granted to use and modify this program in source
 * or binary form as long as it is not sold for profit and this copyright
 * notice remains intact.
 * Original author: Philip Heller, Sun Microsystems, Inc.
 * 
 * All additional software, enhancements and modifications are
 * Copyright 1988, 1989, 1991, 1994 by Tektronix, Inc. - All Rights Reserved.
 * 
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Tektronix, Inc. not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.
 * 
 * TEKTRONIX INCORPORATED MAKES NO REPRESENTATIONS ABOUT THE
 * SUITABILITY OF THIS SOFTWARE FOR ANY PURPOSE.  IT IS PROVIDED "AS IS"
 * WITHOUT EXPRESS OR IMPLIED WARRANTY.  TEKTRONIX INCORPORATED
 * DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  IN NO
 * EVENT SHALL TEKTRONIX INCORPORATED BE LIABLE FOR ANY SPECIAL,
 * INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
 * LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
 * OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 * 
 * Author: Bill Randle, Tektronix, Inc. <billr@saab.cna.tek.com>
 */ 
#include <stdio.h>	/* for NULL */

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>

#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/openmenu.h>
#include <xview/defaults.h>
#include <xview/font.h>
#include <xview/notice.h>
#include <xview/svrimage.h>
#include <xview/cms.h>
#include <xview/xv_xrect.h>
#include <sys/file.h>
#include "ct.h"
#include "xv_ct.h"

#ifndef NO_SUN_MOON
static short moon_icon_data[] = {
#include "moony.icon"
};
static short sun_icon_data[] = {
#include "sunny.icon"
};
#endif

extern int monday_first;
extern struct tm current;
extern Frame frame;
extern int working_msg;
extern int n_slots;
extern int user_font;
extern char *progname;
Canvas canvas;
Xv_window cpwindow;
Panel panel;
Panel_item todaybutton_pi, working_pi;
Panel_item monthmenu_pi, yearmenu_pi, weekbutton_pi, daybutton_pi;
Panel_item previous_pi, next_pi, current_pi, filebutton_pi;
Panel_item clock_pi;
Panel_item runl_pi;
Panel_item donebutton_pi;
#ifndef NO_PRINTER
Panel_item printbutton_pi;
#endif
#ifndef NO_SUN_MOON
Panel_item moonbutton_pi, sunbutton_pi;
#endif
Menu next_menu, previous_menu;
Menu day_menu, week_menu, month_menu, year_menu;
Menu day_sel_menu, current_menu;
Menu done_menu;
#ifndef NO_PRINTER
Menu print_menu;
Xv_opaque print_menu_proc();
Menu print_menu_gen();
#endif
Xv_opaque day_menu_proc(), week_menu_proc(), month_menu_proc();
Xv_opaque previous_menu_proc(), next_menu_proc(), current_menu_proc();
Xv_opaque year_menu_proc(), day_sel_menu_done_proc(), done_menu_proc();
Menu day_menu_gen(), week_menu_gen(), month_menu_gen();
Menu next_prev_menu_gen(), current_menu_gen();
Menu year_menu_gen();
Xv_Font font, bigfont, sfont, menufont;
Frame fframe = 0;
Frame attr_frame = 0;
Panel_item repeat_pi, remind_pi, everyx_pi, whichwk_pi, marked_pi;
Panel_item advw_pi;
extern Cms cms;
#ifndef NO_SUN_MOON
Frame sframe = 0, mframe = 0;
#endif
Frame fileframe = 0;
Panel_item filename_pi, file_ro_pi, file_ac_pi;
#ifndef NO_PRINTER
Panel_item prcmd_pi;
Panel_item prfile_pi;
Panel_item prfname_pi;
int print_to_file = 0;
Frame prframe = 0;
extern char psfile[];
#endif
#ifndef NO_SUN_MOON
Canvas scanvas, mcanvas;
Xv_window scanvas_paint_window, mcanvas_paint_window;
Panel_item sdate_pi, mdate_pi;
GC sgc;
XWMHints *s_hints = NULL;
XWMHints *m_hints = NULL;
XSizeHints sm_sizehints;
XClassHint moon_WMClassHints = {
        "moon", "Calentool" };
XClassHint sun_WMClassHints = {
        "sun", "Calentool" };
#endif
Frame prompt_frame = 0;
Frame date_frame = 0;
Panel_item setdate_pi;
int weekbutton_notify();
int todaybutton_notify(), daybutton_notify();
int filebutton_notify();
int donebutton_notify();
#ifndef NO_SUN_MOON
int moonbutton_notify(), sunbutton_notify();
void write_moon_data(), write_sun_data();
#endif
#ifndef NO_PRINTER
int printbutton_notify();
void print_menu_event();
#endif
void mainsw_inputevent();
void fdone_proc(), fkeep_proc();
int fappt_notify();
void attr_accept(), attr_abort();
void dismiss_event_proc();
#ifndef NO_SUN_MOON
void sdone_proc(), mdone_proc();
void sframe_done(), mframe_done();
#endif
void fileframe_done(), file_accept(), file_reset();
void file_save();
void file_orig();
void dtframe_done(), dtdone_proc();
void dtreset_proc();
#ifndef NO_PRINTER
void prframe_done(), prdone_proc(), prchoice_proc();
void prreset_proc();
#endif
void set_frame_pos();
Notify_value check_close();
int monthlength();
char year_str[NR_YEARS][5]; /* holds strings for year menu */
extern Xv_Cursor day_cursor, wait_cursor;
extern int day_is_open, mainsw_state;
extern char clockstr[];
extern struct appt_entry future[];
extern int findex;
extern struct dayslot *slots;
#ifndef NO_SUN_MOON
Server_image moon_icon_pr, sun_icon_pr;
#endif
extern char printer[];
extern int day_first;
extern int locked;

#ifdef __STDC__
void add_years_to_menu (void);
#else
void add_years_to_menu ();
#endif

/* Create and init control panel */
void
create_panel()
{
	int	width;
	int dummy_x;
	Panel_item dummy;

	/* Create the control panel. */
	panel = xv_create(frame, PANEL,
			  XV_FONT, menufont,
			  WIN_CONSUME_EVENTS,
			  WIN_UP_EVENTS, KEY_LEFT(7), 0,
			  XV_HELP_DATA, "calentool:MainPanel",
			  0);

	(void)xv_set(panel,
		XV_X, xv_col(frame, 0),
		XV_Y, xv_row(frame, 0),
		XV_HEIGHT, 72,
		0);

	/* Create the panel items and their menus */
	done_menu = xv_create(0, MENU,
		XV_FONT, menufont,
		MENU_NOTIFY_PROC, done_menu_proc,
		MENU_ITEM,
			MENU_STRING, "Close to Icon",
			MENU_VALUE, 1,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Quit Tool",
			MENU_VALUE, 2,
			NULL,
		0);

	donebutton_pi = xv_create(panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Done",
			PANEL_ITEM_MENU, done_menu,
			XV_X, xv_col(panel, 0),
			XV_Y, 2+xv_row(panel, 0),
			XV_HELP_DATA, "calentool:DoneButton",
                        0);

	if (monday_first)
		day_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_NOTIFY_PROC, day_menu_proc,
			MENU_GEN_PROC, day_menu_gen,
			MENU_STRINGS,
#ifdef FRENCH
				"Lundi", "Mardi", "Mercredi",
				"Jeudi", "Vendredi", "Samedi",
				"Dimanche",
#else
# ifdef SWEDISH
				"Mandag", "Tisdag", "Onsdag",
				"Torsdag", "Fredag", "Lordag",
				"Sondag", 
# else
				"Monday", "Tuesday", "Wednesday",
				"Thursday", "Friday", "Saturday",
				"Sunday",
# endif
#endif
				NULL,
			0);
	else
		day_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_NOTIFY_PROC, day_menu_proc,
			MENU_GEN_PROC, day_menu_gen,
			MENU_STRINGS,
#ifdef FRENCH
				"Dimanche", "Lundi", "Mardi",
				"Mercredi", "Jeudi", "Vendredi",
				"Samedi",
#else
# ifdef SWEDISH
				"Sondag", "Mandag", "Tisdag", "Onsdag",
				"Torsdag", "Fredag", "Lordag",
# else
				"Sunday", "Monday", "Tuesday",
				"Wednesday", "Thursday", "Friday",
				"Saturday",
# endif
#endif
				NULL,
			0);

	daybutton_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Day",
			PANEL_ITEM_MENU, day_menu,
                        PANEL_NOTIFY_PROC, daybutton_notify,
			XV_Y, xv_row(panel, 0)+2,
			XV_HELP_DATA, "calentool:DayButton",
                        0);
	
	/* next set of buttons are 5 pixels apart */
	xv_set(panel, PANEL_ITEM_X_GAP, 5, 0);

	week_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_NOTIFY_PROC, week_menu_proc,
			MENU_GEN_PROC, week_menu_gen,
			MENU_ITEM,
				MENU_STRING, "1st",
				MENU_VALUE, 1,
				0,
			MENU_ITEM,
				MENU_STRING, "2nd",
				MENU_VALUE, 2,
				0,
			MENU_ITEM,
				MENU_STRING, "3rd",
				MENU_VALUE, 3,
				0,
			MENU_ITEM,
				MENU_STRING, "4th",
				MENU_VALUE, 4,
				0,
			MENU_ITEM,
				MENU_STRING, "5th",
				MENU_VALUE, 5,
				0,
			MENU_ITEM,
				MENU_STRING, "Last",
				MENU_VALUE, 6,
				0,
			0);

	weekbutton_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Week",
			PANEL_ITEM_MENU, week_menu,
			PANEL_NOTIFY_PROC, weekbutton_notify,
			XV_HELP_DATA, "calentool:WeekButton",
                        0);

	month_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_NOTIFY_PROC, month_menu_proc,
			MENU_GEN_PROC, month_menu_gen,
			MENU_STRINGS,
#ifdef FRENCH
				"Janvier", "Fevrier", "Mars",
				"Avril", "Mai", "Juin",
				"Juillet", "Aout", "Septembre",
				"Octobre", "Novembre", "Decembre",
#else
# ifdef SWEDISH
				"Januari", "Februari", "Mars", "April", "Mai",
				"Juni", "Juli", "Augusti", "September",
				"Oktober", "November", "December",
# else
				"January", "February", "March",
				"April", "May", "June",
				"July", "August", "September",
				"October", "November", "December",
# endif
#endif
				NULL,
			0);

	monthmenu_pi = xv_create(panel, PANEL_BUTTON,  
                        PANEL_LABEL_STRING, "Month",
			PANEL_ITEM_MENU, month_menu,
			XV_HELP_DATA, "calentool:MonthButton",
                        0);

	year_menu = menu_create(0);	/* years filled in later */
	add_years_to_menu();

	yearmenu_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Year",
			PANEL_ITEM_MENU, year_menu,
			XV_HELP_DATA, "calentool:YearButton",
                        0);

	/* next set of buttons are 10 pixels apart */
	xv_set(panel, PANEL_ITEM_X_GAP, 10, 0);

	todaybutton_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Today",
                        PANEL_NOTIFY_PROC, todaybutton_notify,
			XV_HELP_DATA, "calentool:TodayButton",
                        0);

	/* next set of buttons are 15 pixels apart */
	xv_set(panel, PANEL_ITEM_X_GAP, 15, 0);

	filebutton_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "File...",
                        PANEL_NOTIFY_PROC, filebutton_notify,
			XV_HELP_DATA, "calentool:FileButton",
                        0);

#ifndef NO_PRINTER
	print_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_NOTIFY_PROC, print_menu_proc,
			MENU_GEN_PROC, print_menu_gen,
			MENU_ITEM,
				MENU_STRING, "Print Postscript",
				MENU_VALUE, 1,
				0,
			MENU_ITEM,
				MENU_STRING, "Print Raster",
				MENU_VALUE, 2,
				0,
			MENU_ITEM,
				MENU_STRING, "Change Printer...",
				MENU_VALUE, 3,
				0,
#ifdef RASTER_ONLY
			XV_KEY_DATA, MENU_KEY, 2,
#else
			XV_KEY_DATA, MENU_KEY, 1,
#endif
			0);

	printbutton_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Print",
                        PANEL_ITEM_MENU, print_menu,
			XV_SHOW, FALSE,
			XV_HELP_DATA, "calentool:PrintButton",
                        0);
#endif

	/* create a placeholder button 5 pixels over to get its x position */
	xv_set(panel, PANEL_ITEM_X_GAP, 5, 0);

	dummy = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Dumy",
			XV_SHOW, FALSE,
                        0);
	dummy_x = xv_get(panel, PANEL_ITEM_X);
	xv_destroy(dummy);

	previous_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_NOTIFY_PROC, previous_menu_proc,
			MENU_GEN_PROC, next_prev_menu_gen,
			MENU_ITEM,
				MENU_STRING, "Yesterday",
				MENU_VALUE, 1,
				0,
			MENU_ITEM,
				MENU_STRING, "Last Week",
				MENU_VALUE, 2,
				0,
			MENU_ITEM,
				MENU_STRING, "Last Month",
				MENU_VALUE, 3,
				0,
			MENU_ITEM,
				MENU_STRING, "Last Year",
				MENU_VALUE, 4,
				0,
			0);
	
	previous_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Previous",
			PANEL_ITEM_MENU, previous_menu,
			XV_X, xv_col(panel, 2),
			XV_Y, xv_row(panel, 1),
			XV_HELP_DATA, "calentool:PreviousButton",
			0);

	/* next set of buttons are 10 pixels apart */
	xv_set(panel, PANEL_ITEM_X_GAP, 10, 0);

	current_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_ACTION_PROC, current_menu_proc,
			MENU_GEN_PROC, current_menu_gen,
			MENU_ITEM,
				MENU_STRING, "Current Day",
				MENU_VALUE, 1,
				0,
			MENU_ITEM,
				MENU_STRING, "Current Week",
				MENU_VALUE, 2,
				0,
			MENU_ITEM,
				MENU_STRING, "Current Month",
				MENU_VALUE, 3,
				0,
			MENU_ITEM,
				MENU_STRING, "Current Year",
				MENU_VALUE, 4,
				0,
			MENU_ITEM,
				MENU_STRING, "Change Date...",
				MENU_VALUE, 5,
				0,
			0);

	current_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Current",
			PANEL_ITEM_MENU, current_menu,
			XV_HELP_DATA, "calentool:CurrentButton",
			0);

	next_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_ACTION_PROC, next_menu_proc,
			MENU_GEN_PROC, next_prev_menu_gen,
			MENU_ITEM,
				MENU_STRING, "Tomorrow",
				MENU_VALUE, 1,
				0,
			MENU_ITEM,
				MENU_STRING, "Next Week",
				MENU_VALUE, 2,
				0,
			MENU_ITEM,
				MENU_STRING, "Next Month",
				MENU_VALUE, 3,
				0,
			MENU_ITEM,
				MENU_STRING, "Next Year",
				MENU_VALUE, 4,
				0,
			0);
	
	next_pi = xv_create(panel, PANEL_BUTTON,
                        PANEL_LABEL_STRING, "Next",
			PANEL_ITEM_MENU, next_menu,
			XV_HELP_DATA, "calentool:NextButton",
			0);

#ifndef NO_SUN_MOON
	/* next set of buttons are 5 pixels apart */
	xv_set(panel, PANEL_ITEM_X_GAP, 5, 0);

	sun_icon_pr = xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		64,
		XV_HEIGHT,		64,
		SERVER_IMAGE_BITS,	sun_icon_data,
		0);
	sunbutton_pi = xv_create(panel, PANEL_MESSAGE,
                        PANEL_LABEL_IMAGE, sun_icon_pr,
                        PANEL_EVENT_PROC, sunbutton_notify,
			XV_X, dummy_x,
			XV_Y, xv_row(panel, 0),
			XV_SHOW, FALSE,
			XV_HELP_DATA, "calentool:SunButton",
                        0);

	moon_icon_pr = xv_create(XV_NULL, SERVER_IMAGE,
		XV_WIDTH,		64,
		XV_HEIGHT,		64,
		SERVER_IMAGE_BITS,	moon_icon_data,
		0);
	moonbutton_pi = xv_create(panel, PANEL_MESSAGE,
                        PANEL_LABEL_IMAGE, moon_icon_pr,
                        PANEL_EVENT_PROC, moonbutton_notify,
			XV_Y, xv_row(panel, 0),
			XV_SHOW, FALSE,
			XV_HELP_DATA, "calentool:MoonButton",
                        0);
#endif

	/* approximate real panel width: icon_x + (icon_w + gap)*2 */
	width = dummy_x + (64+5)*2;

	get_today();	/* get current date and time */
	clock_pi = xv_create(panel, PANEL_MESSAGE,
			PANEL_LABEL_STRING, clockstr,
			PANEL_LABEL_FONT, font,
			XV_X, dummy_x-10-(strlen(clockstr)*
				xv_get(font, FONT_DEFAULT_CHAR_WIDTH)),
			XV_Y, xv_row(panel, 1),
			0);

	working_pi = xv_create(panel, PANEL_MESSAGE,
			PANEL_LABEL_STRING, "Working!",
			PANEL_LABEL_FONT, menufont,
			PANEL_LABEL_BOLD, TRUE,
			XV_SHOW, FALSE,
			XV_X, dummy_x-20-(strlen("Working!")*
				xv_get(font, FONT_DEFAULT_CHAR_WIDTH)),
			XV_Y, xv_row(panel, 2)-4,
			0);

	/* resize the base frame, if necessary */
	if (width > (int)xv_get(frame, XV_WIDTH))
		xv_set(frame, XV_WIDTH, width, 0);

	/*
	 * menu strings for right MB menu in the canvas
	 * (day display).
	 */
	day_sel_menu = xv_create(0, MENU,
			XV_FONT, menufont,
			MENU_TITLE_ITEM, "Appointment",
			MENU_ITEM,
				MENU_STRING, "Properties...",
				MENU_VALUE, MMODIFY,
				0,
			MENU_ITEM,
				MENU_STRING, "Cut",
				MENU_VALUE, MCUT,
				0,
			MENU_ITEM,
				MENU_STRING, "Paste",
				MENU_VALUE, MPASTE,
				0,
			MENU_ITEM,
				MENU_STRING, "Copy",
				MENU_VALUE, MCOPY,
				0,
			MENU_ITEM,
				MENU_STRING, "Delete",
				MENU_VALUE, MDELETE,
				0,
			MENU_ITEM,
				MENU_STRING, "Undelete",
				MENU_VALUE, MUNDELETE,
				0,
			MENU_ACTION_PROC, day_sel_menu_done_proc,
			XV_HELP_DATA, "calentool:AppointmentMenu",
			0);

	/*
	 * interpose on panel events to check for L7 (open/close)
	 */
	notify_interpose_event_func(panel, check_close, NOTIFY_SAFE);
}

/*
 * Add year strings to year panel menu
 */
void
add_years_to_menu()
{
	int n, year;

	n = 1;
	for (year=START_YEAR; year<START_YEAR+NR_YEARS; year++,n++) {
		sprintf(year_str[n-1], "%4d", year+1900);
		xv_set(year_menu, MENU_ITEM,
			MENU_STRING, year_str[n-1],
			MENU_VALUE, n,
			0, 0);
	}
	xv_set(year_menu, MENU_NOTIFY_PROC, year_menu_proc,
			MENU_GEN_PROC, year_menu_gen,
			0);
}

/* turn sun and moon buttons on or off */
void
sun_moon_buttons(state)
int state;
{
#ifndef NO_SUN_MOON
	if (state) {
		if (!mframe)
			xv_set(moonbutton_pi, XV_SHOW, TRUE, 0);
		if (!sframe)
			xv_set(sunbutton_pi, XV_SHOW, TRUE, 0);
	} else {
		xv_set(moonbutton_pi, XV_SHOW, FALSE, 0);
		xv_set(sunbutton_pi, XV_SHOW, FALSE, 0);
		/* remove moon window, if it exists */
		if (mframe) {
			xv_destroy_safe(mframe);
			mframe = 0;
		}
		/* remove sun window, if it exists */
		if (sframe) {
			xv_destroy_safe(sframe);
			sframe = 0;
		}
	}
#endif
}

/* turn print button on or off */
void print_button(state)
int state;
{
#ifndef NO_PRINTER
/* if no printer specified then never show Print button */
	if (state)
		xv_set(printbutton_pi, XV_SHOW, TRUE, 0);
	else
		xv_set(printbutton_pi, XV_SHOW, FALSE, 0);
#endif	/* NO_PRINTER */
}

void working(state)
int state;
{
	/* turn "Working!" message on or off */
	if (working_msg) {
		if (state) {
			xv_set(working_pi, XV_SHOW, TRUE, 0);
		} else {
			xv_set(working_pi, XV_SHOW, FALSE, 0);
		}
	}
}

/* repaint the canvas on redisplay */
void repaint_canvas(canvas, pw, dpy, xwin, xrects)
Canvas          canvas;
Xv_Window       pw;
Display         *dpy;
Window          xwin;
Xv_xrectlist    *xrects;
{
	 switch(mainsw_state) {
		case DISPLAYING_DAY:
			draw_day();
			break;
		case DISPLAYING_WEEK:
			draw_week();
			break;
		case DISPLAYING_MONTH:
			draw_month();
			break;
		case DISPLAYING_YEAR:
			draw_year();
			break;
	}
}

/* Create and init main subwindow. */
void
create_main_window()
{
	canvas = xv_create(frame, CANVAS,
			CANVAS_FIXED_IMAGE, TRUE,
			CANVAS_REPAINT_PROC, repaint_canvas,
			WIN_BELOW, panel,
			WIN_ERROR_MSG, "Can't create main window.",
			XV_HELP_DATA, "calentool:DayDisplay",
			WIN_CMS, cms,
			0);
	/* make sure CMS got set before setting the colors */
	/* note that the colors are indices into the Cms structure
	   created in calentool.c [setcms()]
	 */
	xv_set(canvas, WIN_FOREGROUND_COLOR, CMS_CONTROL_COLORS+1,
			WIN_BACKGROUND_COLOR, CMS_CONTROL_COLORS+0,
			0);
	cpwindow = canvas_paint_window(canvas);
	/* create another gc for the main window */
	gc_val.function = GXclear;
	gc_val.fill_style = FillSolid;
	gc_val.background = backgr;
	gc_val.foreground = foregr;
	drawable = (Drawable)xv_get(cpwindow, XV_XID);
	gcc = XCreateGC(mydisplay, drawable,
		GCBackground|GCForeground|GCFunction|GCFont|GCFillStyle,
		&gc_val);
	gc_val.fill_style = FillOpaqueStippled;
	gc_val.function = GXcopy;
	gccs = XCreateGC(mydisplay, drawable,
		GCBackground|GCForeground|GCFunction|GCFont|GCFillStyle,
		&gc_val);

	/* and another gc for the sun/mooon window */
	gc_val.function = GXcopy;
	gc_val.fill_style = FillSolid;
#ifndef NO_SUN_MOON
	sgc = XCreateGC(mydisplay, drawable,
		GCBackground|GCForeground|GCFunction|GCFont|GCFillStyle,
		&gc_val);
#endif

	/* set the events to trap in the canvas paint window */
	xv_set(cpwindow, 
		WIN_CONSUME_EVENTS,
			WIN_NO_EVENTS,
			WIN_ASCII_EVENTS,
			WIN_META_EVENTS,
			WIN_UP_EVENTS,
			WIN_TOP_KEYS,
			WIN_LEFT_KEYS,
			WIN_MOUSE_BUTTONS,
			LOC_WINENTER,
			LOC_WINEXIT,
			LOC_DRAG,
			WIN_IN_TRANSIT_EVENTS,
			WIN_REPAINT,
			0,
		WIN_EVENT_PROC, mainsw_inputevent,
		0);

	
	xv_set(xv_get(canvas, CANVAS_NTH_PAINT_WINDOW, 0),
		WIN_CURSOR, day_cursor, 0);
	day_is_open = FALSE;
}

void
get_fonts()
{
	char *fontname;
	char buf[128];

	/* Open normal font file */
	if (defaults_exists("font.name", NULL))
		fontname = defaults_get_string("font.name", NULL, NULL);
	else {
		sprintf(buf, "%s.font.normal", progname);
		fontname = defaults_get_string(buf,
			"Calentool.font.normal",
			"-*-fixed-medium-r-normal-*-13-120-*-*-c-*-iso8859-1");
	}
	font = xv_find (0, FONT,
		FONT_NAME,	fontname,
		0);
	if (!font) {
		fprintf(stderr, "%s: can't open font <%s>\n", progname, fontname);
		exit(1);
	}

	/* Open big font file */
	sprintf(buf, "%s.font.large", progname);
	fontname = defaults_get_string(buf,
		"Calentool.font.large",
		"-*-fixed-medium-r-normal-*-*-230-*-*-c-*-iso8859-1");
	bigfont = xv_find (0, FONT,
		FONT_NAME,	fontname,
		0);
	if (!bigfont) {
		fprintf(stderr, "%s: can't open font <%s>\n", progname, fontname);
		exit(1);
	}

	/* Open small font file */
	sprintf(buf, "%s.font.small", progname);
	fontname = defaults_get_string(buf,
		"Calentool.font.small",
		"-*-fixed-medium-r-normal-*-8-80-*-*-c-*-iso8859-1");
	sfont = xv_find (0, FONT,
		FONT_NAME,	fontname,
		0);
	if (!sfont) {
		fprintf(stderr, "%s: can't open font <%s>\n", progname, fontname);
		exit(1);
	}

	/* Open menu font file */
	sprintf(buf, "%s.font.menu", progname);
	fontname = defaults_get_string(buf,
		"Calentool.font.menu",
		"-*-fixed-medium-r-normal-*-13-120-*-*-c-*-iso8859-1");
	menufont = xv_find (0, FONT,
		FONT_NAME,	fontname,
		0);
	if (!menufont) {
		fprintf(stderr, "%s: can't open font <%s>\n", progname, fontname);
		exit(1);
	}
}

/*
 * create popup window for future appts display
 * called when we draw a day display.
 */
void
create_future_popup()
{
	int i, p_width;
	char *fappt_str, *format_appt();
	static Panel fpanel, fcpanel;
	static Panel_item fdone_pi, fkeep_pi;
	static int cp_width, cp_x;
	XSizeHints sizehints;

	if (!fframe) {
		/* create new frame and control panel */
		fframe = xv_create(frame, FRAME_CMD,
				XV_LABEL, "Future Appointments",
				FRAME_SHOW_LABEL, TRUE,
				FRAME_DONE_PROC, fdone_proc,
				WIN_ERROR_MSG, "Can't create future frame.",
				FRAME_INHERIT_COLORS, TRUE,
				FRAME_CLOSED, FALSE,
				0);

		fcpanel = xv_get(fframe, FRAME_CMD_PANEL);
	
		fkeep_pi = xv_create(fcpanel, PANEL_BUTTON, PANEL_NOTIFY_PROC,
				fkeep_proc, PANEL_LABEL_STRING , "Keep",
				PANEL_LABEL_FONT, font,
				XV_SHOW, TRUE,
				XV_X, xv_col(fcpanel, 10),
				XV_Y, xv_row(fcpanel, 0),
				XV_HELP_DATA, "calentool:FutureKeepButton",
				0);
	
		fdone_pi = xv_create(fcpanel, PANEL_BUTTON,
				PANEL_NOTIFY_PROC, fdone_proc,
				PANEL_LABEL_FONT, font,
				PANEL_LABEL_STRING, "Dismiss ",
				XV_SHOW, TRUE,
				XV_X, xv_col(fcpanel, 20),
				XV_Y, xv_row(fcpanel, 0),
				XV_HELP_DATA, "calentool:FutureDismissButton",
				0);
	
		(void) xv_create(fcpanel, PANEL_MESSAGE,
				PANEL_LABEL_FONT, font,
				PANEL_LABEL_STRING, "Select an appointment to view that day.",
				XV_SHOW, TRUE,
				PANEL_LABEL_X, xv_col(fcpanel, 5),
				PANEL_LABEL_Y, xv_row(fcpanel, 1),
				PANEL_LABEL_BOLD, TRUE,
				0);

		xv_set(fcpanel, PANEL_DEFAULT_ITEM, fkeep_pi,
				XV_HELP_DATA, "calentool:FuturePanel",
				0);
		window_fit(fcpanel);
		cp_width = (int) xv_get(fcpanel, XV_WIDTH);
		cp_x = (int) xv_get(fcpanel, XV_X);
	} else {
		/* existing frame, so just delete and recreate the
		 * message panel and its items
		 */
		xv_destroy_safe(fpanel);
	}
	fpanel = xv_create(fframe, PANEL, WIN_BELOW, fcpanel,
			XV_FONT, font, XV_X, cp_x, 0);
	/* create a panel message item for each future appt */
	for (i=0; i<findex; i++) {
		fappt_str = format_appt(&future[i]);
		(void) xv_create(fpanel, PANEL_MESSAGE,
				PANEL_NOTIFY_PROC, fappt_notify,
				XV_SHOW, TRUE,
				XV_KEY_DATA, PANEL_ITEM_KEY, i,
				PANEL_LABEL_X, xv_col(fpanel, 1),
				PANEL_LABEL_Y, xv_row(fpanel, i),
				PANEL_LABEL_FONT, font,
				PANEL_LABEL_STRING, fappt_str,
				0);
	}
	window_fit(fpanel);
	/* find out which panel is wider and use it for frame width */
	p_width = (int) xv_get(fpanel, XV_WIDTH);
	if (p_width > cp_width) {
		/* reset control panel size */
		xv_set(fcpanel, XV_WIDTH, p_width, 0);
		/* move buttons */
		/*****
		xv_set(fdone_pi, XV_X, xv_col(fcpanel, -7)+p_width,
				XV_SHOW, TRUE,
				0);
		xv_set(fkeep_pi, XV_X, xv_col(fcpanel, -17)+p_width,
				XV_SHOW, TRUE,
				0);
		*****/
	} else {
		xv_set(fpanel, XV_WIDTH, cp_width, 0);
		/* move buttons */
		/****
		xv_set(fdone_pi, XV_X, xv_col(fcpanel, -7)+cp_width,
				XV_SHOW, TRUE,
				0);
		xv_set(fkeep_pi, XV_X, xv_col(fcpanel, -17)+cp_width,
				XV_SHOW, TRUE,
				0);
		*****/
	}
	window_fit(fframe);
	set_frame_pos(fframe, NULL, "future", &sizehints);
	xv_set(fframe,
		XV_X, sizehints.x, XV_Y, sizehints.y,
		XV_SHOW, TRUE,
		0);
}

/*
 * create a popup to modify or set attributes for a given
 * appointment.
 */
void
create_attr_frame()
{
	Panel attr_panel;
	Panel_item def;
	void everyx_notify();

	/* create new frame and control panel */
	attr_frame = xv_create(frame, FRAME_CMD,
			XV_LABEL, "Appointment Properties",
			FRAME_SHOW_LABEL, TRUE,
			WIN_ERROR_MSG, "Can't create properties frame.",
			FRAME_INHERIT_COLORS, TRUE,
			FRAME_DONE_PROC, attr_abort,
			FRAME_CLOSED, FALSE,
			WIN_EVENT_PROC, dismiss_event_proc,
			0);

	attr_panel = xv_get(attr_frame, FRAME_CMD_PANEL);
	xv_set(attr_panel, XV_FONT, font,
			XV_HELP_DATA, "calentool:AppointmentProps",
			0);
	
	everyx_pi = xv_create(attr_panel, PANEL_TOGGLE,
			XV_SHOW, TRUE,
			PANEL_DISPLAY_LEVEL, PANEL_ALL,
			PANEL_NOTIFY_PROC, everyx_notify,
			PANEL_LABEL_FONT, font,
			/*
			PANEL_CHOICE_FONT, font,
			*/
			PANEL_LABEL_STRING, "Repeat appointment:",
			PANEL_CHOICE_STRINGS, "Mon-Fri", "Every Day",
			"Selected Week", "Every Month", "Every Year", 0,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 1)-5,
			XV_HELP_DATA, "calentool:AppointmentPropsType",
			0);

	whichwk_pi = xv_create(attr_panel, PANEL_TOGGLE,
			XV_SHOW, TRUE,
			PANEL_DISPLAY_LEVEL, PANEL_ALL,
			PANEL_LABEL_FONT, font,
			/*
			PANEL_CHOICE_FONT, font,
			*/
			PANEL_LABEL_STRING,
			"Indicate which week(s) in the month:",
			PANEL_CHOICE_STRINGS, "1st", "2nd",
			"3rd", "4th", "5th", "Last", "All", 0,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 2)-5,
			XV_HELP_DATA, "calentool:AppointmentPropsWeek",
			0);

	repeat_pi = xv_create(attr_panel, PANEL_NUMERIC_TEXT,
			XV_SHOW, FALSE,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING,
			"Repeat at specified interval of days:",
			PANEL_VALUE, 0, PANEL_VALUE_STORED_LENGTH, 4,
			PANEL_VALUE_DISPLAY_LENGTH, 4,
			PANEL_MIN_VALUE, 0,
			PANEL_MAX_VALUE, 366,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 2),
			PANEL_BLINK_CARET, TRUE,
			XV_HELP_DATA, "calentool:AppointmentPropsInterval",
			0);
	
	runl_pi = xv_create(attr_panel, PANEL_NUMERIC_TEXT,
			XV_SHOW, TRUE,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING,
			"Repeat specified number of times (0=forever):",
			PANEL_VALUE, 0, PANEL_VALUE_STORED_LENGTH, 4,
			PANEL_VALUE_DISPLAY_LENGTH, 4,
			PANEL_MIN_VALUE, 0,
			PANEL_MAX_VALUE, 366,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 3),
			PANEL_BLINK_CARET, TRUE,
			XV_HELP_DATA, "calentool:AppointmentPropsDuration",
			0);

	remind_pi = xv_create(attr_panel, PANEL_NUMERIC_TEXT,
			XV_SHOW, TRUE,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING,
			"Warn in advance by specified number of days:",
			PANEL_VALUE, 0, PANEL_VALUE_STORED_LENGTH, 4,
			PANEL_VALUE_DISPLAY_LENGTH, 4,
			PANEL_MIN_VALUE, 0,
			PANEL_MAX_VALUE, 366,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 4),
			PANEL_BLINK_CARET, TRUE,
			XV_HELP_DATA, "calentool:AppointmentPropsWarn",
			0);

	advw_pi = xv_create(attr_panel, PANEL_NUMERIC_TEXT,
			XV_SHOW, TRUE,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING,
			"Remind a specified number of minutes before:",
			PANEL_VALUE, 0, PANEL_VALUE_STORED_LENGTH, 4,
			PANEL_VALUE_DISPLAY_LENGTH, 4,
			PANEL_MIN_VALUE, 0,
			PANEL_MAX_VALUE, 120,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 5),
			PANEL_BLINK_CARET, TRUE,
			XV_HELP_DATA, "calentool:AppointmentPropsRemind",
			0);

	/* This panel item is currently only supported
	 * for note appointment entries.
	 */
	marked_pi = xv_create(attr_panel, PANEL_CHOICE,
			XV_SHOW, FALSE,
			PANEL_DISPLAY_LEVEL, PANEL_ALL,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Note Indication:",
			PANEL_CHOICE_STRINGS,
				"Do not show in month/year display",
				"Show in month/year display",
				0,
			XV_X, xv_col(attr_panel, 1),
			XV_Y, xv_row(attr_panel, 6),
			XV_HELP_DATA, "calentool:AppointmentPropsNote",
			0);

	def = xv_create(attr_panel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, attr_accept,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Apply",
			XV_SHOW, TRUE,
			XV_X, xv_col(attr_panel, 30),
			XV_Y, xv_row(attr_panel, 7),
			XV_HELP_DATA, "calentool:AppointmentPropsApply",
			0);

	(void) xv_create(attr_panel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, attr_abort,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Reset",
			XV_SHOW, TRUE,
			XV_X, xv_col(attr_panel, 50),
			XV_Y, xv_row(attr_panel, 7),
			XV_HELP_DATA, "calentool:AppointmentPropsReset",
			0);

	xv_set(attr_panel, PANEL_DEFAULT_ITEM, def, 0);
	window_fit(attr_panel);
	window_fit(attr_frame);
}

#ifndef NO_SUN_MOON
/*
 * create popup for sun data frame
 */
void
sun_data_frame()
{
	Panel spanel;

	/* create new frame and canvas */
	if (!sframe) {
		sframe = xv_create(frame, FRAME_CMD,
				XV_LABEL, "Solar Data",
				FRAME_SHOW_LABEL, TRUE,
				FRAME_DONE_PROC, sframe_done,
				WIN_ERROR_MSG, "Can't create sun data frame.",
				FRAME_INHERIT_COLORS, TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				FRAME_CLOSED, FALSE,
				WIN_EVENT_PROC, dismiss_event_proc,
				0);

		spanel = xv_get(sframe, FRAME_CMD_PANEL);
		xv_set(spanel,
			XV_FONT, font,
			XV_WIDTH, 55*xv_get(font, FONT_DEFAULT_CHAR_WIDTH),
			0);
	
#if 0
		(void) xv_create(spanel, PANEL_BUTTON,
				PANEL_NOTIFY_PROC, sdone_proc,
				PANEL_LABEL_FONT, font,
				PANEL_LABEL_STRING, "Done",
				XV_SHOW, TRUE,
				XV_X, xv_col(spanel, 40),
				XV_Y, xv_row(spanel, 0),
				0);
#endif

		sdate_pi = xv_create(spanel, PANEL_MESSAGE,
				XV_X, xv_col(spanel, 8),
				XV_Y, xv_row(spanel, 0),
				/* string filled in later */
				PANEL_LABEL_STRING, "",
				PANEL_LABEL_FONT, font,
				0);
	
		window_fit_height(spanel);
		scanvas = xv_create(sframe, CANVAS, WIN_BELOW, spanel,
				XV_X, 0,
				XV_HEIGHT, 17*
					xv_get(font, FONT_DEFAULT_CHAR_HEIGHT),
				XV_WIDTH, 55*
					xv_get(font, FONT_DEFAULT_CHAR_WIDTH),
				WIN_ERROR_MSG, "Can't create sun data canvas.",
				WIN_CMS, cms,
				CANVAS_REPAINT_PROC, write_sun_data,
				0);

		/* make sure CMS got set before setting the colors */
		xv_set(scanvas, WIN_FOREGROUND_COLOR, CMS_CONTROL_COLORS+1,
				WIN_BACKGROUND_COLOR, CMS_CONTROL_COLORS+0,
				0);

		/* set the events to trap in the canvas paint window */
		scanvas_paint_window = canvas_paint_window(scanvas);
		xv_set(scanvas_paint_window, 
			WIN_CONSUME_EVENTS,
				WIN_NO_EVENTS,
				WIN_REPAINT,
				0,
			0);
	
		window_fit(sframe);
	        /* set this applications WM_CLASS attributes */
		if (s_hints == NULL) {
			if ((s_hints = XGetWMHints(mydisplay, xv_get(sframe, XV_XID))) == NULL)
				s_hints = XAllocWMHints();
			s_hints->input = True;
			s_hints->flags = InputHint;
		}
		/* do this to make sure positioning hints get set */
		/* (doing XV_X and XV_Y on the frame doesn't work) */
		set_frame_pos(sframe, mframe, "sun", &sm_sizehints);
		XSetWMProperties(mydisplay, xv_get(sframe, XV_XID), NULL, NULL,
			NULL, 0, &sm_sizehints, s_hints, &sun_WMClassHints);
		xv_set(sframe, XV_X, sm_sizehints.x, XV_Y, sm_sizehints.y, NULL);
	}
	xv_set(sframe,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		XV_SHOW, TRUE,
		0);
}

/*
 * create popup for moon data frame
 */
void
moon_data_frame()
{
	Panel mpanel;

	/* create new frame and canvas */
	if (!mframe) {
		mframe = xv_create(frame, FRAME_CMD,
				XV_LABEL, "Lunar Data",
				FRAME_SHOW_LABEL, TRUE,
				FRAME_DONE_PROC, mframe_done,
				WIN_ERROR_MSG, "Can't create moon data frame.",
				FRAME_INHERIT_COLORS, TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				FRAME_CLOSED, FALSE,
				WIN_EVENT_PROC, dismiss_event_proc,
				0);

		mpanel = xv_get(mframe, FRAME_CMD_PANEL);
		xv_set(mpanel,
			XV_FONT, font,
			XV_WIDTH, 71*xv_get(font, FONT_DEFAULT_CHAR_WIDTH),
			0);
	
#if 0
		(void) xv_create(mpanel, PANEL_BUTTON,
				PANEL_NOTIFY_PROC, mdone_proc,
				PANEL_LABEL_FONT, font,
				PANEL_LABEL_STRING, "Done",
				XV_SHOW, TRUE,
				XV_X, xv_col(mpanel, 55),
				XV_Y, xv_row(mpanel, 0),
				0);
#endif

		mdate_pi = xv_create(mpanel, PANEL_MESSAGE,
				XV_X, xv_col(mpanel, 8),
				XV_Y, xv_row(mpanel, 0),
				/* string filled in later */
				PANEL_LABEL_STRING, "",
				PANEL_LABEL_FONT, font,
				0);
	
		window_fit_height(mpanel);
		mcanvas = xv_create(mframe, CANVAS, WIN_BELOW, mpanel,
				XV_X, 0,
				XV_HEIGHT, 17*
					xv_get(font, FONT_DEFAULT_CHAR_HEIGHT),
				XV_WIDTH, 71*
					xv_get(font, FONT_DEFAULT_CHAR_WIDTH),
				CANVAS_REPAINT_PROC, write_moon_data,
				WIN_CMS, cms,
				WIN_ERROR_MSG, "Can't create moon data canvas.",
				0);

		/* make sure CMS got set before setting the colors */
		xv_set(mcanvas, WIN_FOREGROUND_COLOR, CMS_CONTROL_COLORS+1,
				WIN_BACKGROUND_COLOR, CMS_CONTROL_COLORS+0,
				0);

		/* set the events to trap in the canvas paint window */
		mcanvas_paint_window = canvas_paint_window(mcanvas);
		xv_set(mcanvas_paint_window, 
			WIN_CONSUME_EVENTS,
				WIN_NO_EVENTS,
				WIN_REPAINT,
				0,
			0);
	
		window_fit(mcanvas);
		window_fit(mframe);
	        /* set this applications WM_CLASS attributes */
		if (m_hints == NULL) {
			if ((m_hints = XGetWMHints(mydisplay, xv_get(mframe, XV_XID))) == NULL)
				m_hints = XAllocWMHints();
			m_hints->input = True;
			m_hints->flags = InputHint;
		}
		/* do this to make sure positioning hints get set */
		set_frame_pos(mframe, sframe, "moon", &sm_sizehints);
		XSetWMProperties(mydisplay, xv_get(mframe, XV_XID), NULL, NULL,
			NULL, 0, &sm_sizehints, m_hints, &moon_WMClassHints);
		xv_set(mframe, XV_X, sm_sizehints.x, XV_Y, sm_sizehints.y, NULL);
	}
	xv_set(mframe,
		FRAME_CMD_PUSHPIN_IN, TRUE,
		XV_SHOW, TRUE,
		0);
}
#endif	/* NO_SUN_MOON */

void
set_frame_pos(pframe, below, name, sizehints)
Frame pframe;
Frame below;
char *name;
XSizeHints *sizehints;
{
	Rect rect, rect1;
	char *def;
	unsigned int result, w, h;
	int xpos, ypos;
	char buff[128], buff1[128];

	sprintf(buff, "%s.%s.geometry", progname, name);
	sprintf(buff1, "Calentool.%s.geometry", name);
	if (defaults_exists(buff, buff1)) {
		/* use user supplied default */
		def = defaults_get_string(buff, buff1, NULL);
		result = XGeometry(mydisplay, screen, def, "+0+0",
				6, 1, 1, 0, 0,
				&xpos, &ypos, &w, &h);
	} else {
		/* try another form */
		sprintf(buff, "%s.%s.geometry", name, name);
		if (defaults_exists(buff, NULL)) {
			/* use user supplied default */
			def = defaults_get_string(buff, buff1, NULL);
			result = XGeometry(mydisplay, screen, def, "+0+0",
					6, 1, 1, 0, 0,
					&xpos, &ypos, &w, &h);
		} else {
			/* figure out a good place to put it */
			frame_get_rect(frame, &rect);
			frame_get_rect(pframe, &rect1);
			/* find out if the base frame is left or right justified */
			if (rect.r_left <= (DisplayWidth(mydisplay, screen)-rect.r_left+rect.r_width)) {
				/* position this frame to the right */
				xpos = rect.r_left+rect.r_width;
				/* keep it all on screen */
				if (xpos > (DisplayWidth(mydisplay, screen)-rect1.r_width))
					xpos = DisplayWidth(mydisplay, screen)-rect1.r_width;
			} else {
				/* position this frame to the left */
				xpos = rect.r_left-rect1.r_width;
				/* keep it all on screen */
				if (xpos < 0)
					xpos = 0;
			}
			if (below && xv_get(below, XV_SHOW)) {
				/* position it below the "below" frame */
				frame_get_rect(below, &rect1);
				ypos = rect.r_top + rect1.r_height;
			} else {
				ypos = rect.r_top;
			}
		}
	}
	sizehints->x = xpos;
	sizehints->y = ypos;
	sizehints->flags = PPosition;
}

/*
 * create a popup to allow selecting a different appointment file
 */
void
create_file_frame()
{
	Panel filepanel;
	XSizeHints sizehints;

	/* create new frame and control panel */
	fileframe = xv_create(frame, FRAME_CMD,
			XV_LABEL, "File Selection",
			FRAME_SHOW_LABEL, TRUE,
			FRAME_DONE_PROC, fileframe_done,
			WIN_ERROR_MSG, "Can't create file frame.",
			FRAME_INHERIT_COLORS, TRUE,
			FRAME_CMD_PUSHPIN_IN, TRUE,
			FRAME_CLOSED, FALSE,
			0);

	filepanel = xv_get(fileframe, FRAME_CMD_PANEL);
	xv_set(filepanel, XV_FONT, menufont, 0);
	
	file_ro_pi = xv_create(filepanel, PANEL_CHOICE, PANEL_CHOICE_STRINGS,
			"Read Only", "Read/Write", 0,
			PANEL_LABEL_FONT, font,
			/*
			PANEL_CHOICE_FONT, font,
			*/
			XV_SHOW, TRUE,
			XV_X, xv_col(filepanel, 45),
			XV_Y, xv_row(filepanel, 1),
			0);

	file_ac_pi = xv_create(filepanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, file_accept,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Apply", 
			XV_SHOW, TRUE,
			XV_X, xv_col(filepanel, 12),
			XV_Y, xv_row(filepanel, 2),
			0);

	(void) xv_create(filepanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, file_orig, 
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Original",
			XV_SHOW, TRUE,
			XV_X, xv_col(filepanel, 22),
			XV_Y, xv_row(filepanel, 2),
			0);

	(void) xv_create(filepanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, file_reset,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Reset",
			XV_SHOW, TRUE,
			XV_X, xv_col(filepanel, 34),
			XV_Y, xv_row(filepanel, 2),
			0);

	(void) xv_create(filepanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, file_save,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Save",
			XV_SHOW, TRUE,
			XV_X, xv_col(filepanel, 43),
			XV_Y, xv_row(filepanel, 2),
			0);

	(void) xv_create(filepanel, PANEL_MESSAGE,
			XV_SHOW, TRUE,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Enter the name of the desired appointment file",
			PANEL_LABEL_X, xv_col(filepanel, 6),
			PANEL_LABEL_Y, xv_row(filepanel, 0),
			PANEL_LABEL_BOLD, TRUE,
			0);

	filename_pi = xv_create(filepanel, PANEL_TEXT,
			XV_SHOW, TRUE,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Filename:",
			PANEL_LABEL_BOLD, TRUE,
			PANEL_VALUE, 0, PANEL_VALUE_STORED_LENGTH, 128,
			PANEL_VALUE_DISPLAY_LENGTH, 30,
			XV_X, xv_col(filepanel, 1),
			XV_Y, xv_row(filepanel, 1),
			/*PANEL_BLINK_CARET, TRUE,*/
			0);

	xv_set(filepanel, PANEL_DEFAULT_ITEM, file_ac_pi, 0);
	window_fit(filepanel);
	window_fit(fileframe);
	set_frame_pos(fileframe, NULL, "fileSelection", &sizehints);
	xv_set(fileframe, XV_X, sizehints.x, XV_Y, sizehints.y, NULL);
}


#ifndef NO_PRINTER
/* create popup to change the printer */
void
create_print_frame()
{
	Panel prpanel;
	Panel_item def;
	XSizeHints sizehints;

	prframe = xv_create(frame, FRAME_CMD,
			XV_SHOW, FALSE,
			XV_LABEL, "Change Printer",
			FRAME_SHOW_LABEL, TRUE,
			FRAME_DONE_PROC, prframe_done,
			WIN_ERROR_MSG, "Can't create printer frame.",
			FRAME_INHERIT_COLORS, TRUE,
			FRAME_CLOSED, FALSE,
			FRAME_CMD_PUSHPIN_IN, FALSE,
			0);

	prpanel = xv_get(prframe, FRAME_CMD_PANEL);
	xv_set(prpanel, XV_FONT, font, 0);
	
	def = xv_create(prpanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, prdone_proc,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Apply",
			XV_SHOW, TRUE,
			XV_X, xv_col(prpanel, 12),
			XV_Y, xv_row(prpanel, 3),
			0);

	(void) xv_create(prpanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, prreset_proc,
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_STRING, "Reset",
			XV_SHOW, TRUE,
			XV_X, xv_col(prpanel, 24),
			XV_Y, xv_row(prpanel, 3),
			0);

	prfile_pi = xv_create(prpanel, PANEL_CHOICE,
			XV_SHOW, TRUE,
			XV_X, xv_col(prpanel, 1),
			XV_Y, xv_row(prpanel, 0)+5,
			PANEL_LABEL_STRING, "Destination:",
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_BOLD, TRUE,
			/*
			PANEL_CHOICE_FONT, font,
			*/
			PANEL_CHOICE_STRINGS, "Printer", "File Only", 0,
			PANEL_NOTIFY_PROC, prchoice_proc,
			0);

	prcmd_pi = xv_create(prpanel, PANEL_TEXT,
			XV_SHOW, FALSE,
			XV_X, xv_col(prpanel, 1),
			XV_Y, xv_row(prpanel, 1)+5,
			PANEL_LABEL_STRING, "Print command:",
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_BOLD, TRUE,
			PANEL_VALUE_STORED_LENGTH, 128,
			PANEL_VALUE_DISPLAY_LENGTH, 30,
			0);
	
	prfname_pi = xv_create(prpanel, PANEL_TEXT,
			XV_SHOW, FALSE,
			XV_X, xv_col(prpanel, 1),
			XV_Y, xv_row(prpanel, 1)+5,
			PANEL_LABEL_STRING, "Filename:",
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_BOLD, TRUE,
			PANEL_VALUE_STORED_LENGTH, 128,
			PANEL_VALUE_DISPLAY_LENGTH, 30,
			0);
	
	xv_set(prcmd_pi, PANEL_VALUE, printer, 0);
	xv_set(prfname_pi, PANEL_VALUE, psfile, 0);
	xv_set(prfile_pi, PANEL_VALUE, print_to_file, 0);
	if (print_to_file)
		xv_set(prfname_pi, XV_SHOW, TRUE, 0);
	else
		xv_set(prcmd_pi, XV_SHOW, TRUE, 0);
	xv_set(prpanel, PANEL_DEFAULT_ITEM, def, 0);

	window_fit(prpanel);
	window_fit(prframe);
	set_frame_pos(prframe, NULL, "printerSelection", &sizehints);
	xv_set(prframe, XV_X, sizehints.x, XV_Y, sizehints.y, NULL);
}
#endif	/* NO_PRINTER */

/* create popup to change the date */
void
create_date_frame()
{
	Panel dtpanel;
	Panel_item def;
	char date[9];
	XSizeHints sizehints;

	date_frame = xv_create(frame, FRAME_CMD,
			XV_SHOW, FALSE,
			XV_LABEL, "Change Current Date",
			FRAME_SHOW_LABEL, TRUE,
			FRAME_DONE_PROC, dtframe_done,
			WIN_ERROR_MSG, "Can't create date frame.",
			FRAME_INHERIT_COLORS, TRUE,
			FRAME_CLOSED, FALSE,
			0);

	dtpanel = xv_get(date_frame, FRAME_CMD_PANEL);
	xv_set(dtpanel, XV_FONT, font, 0);
	
	def = xv_create(dtpanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, dtdone_proc,
			PANEL_LABEL_STRING, "Apply",
			XV_SHOW, TRUE,
			XV_X, xv_col(dtpanel, 12),
			XV_Y, xv_row(dtpanel, 1),
			0);

	(void) xv_create(dtpanel, PANEL_BUTTON,
			PANEL_NOTIFY_PROC, dtreset_proc,
			PANEL_LABEL_STRING, "Reset",
			XV_SHOW, TRUE,
			XV_X, xv_col(dtpanel, 24),
			XV_Y, xv_row(dtpanel, 1),
			0);

	setdate_pi = xv_create(dtpanel, PANEL_TEXT,
			XV_SHOW, TRUE,
			XV_X, xv_col(dtpanel, 1),
			XV_Y, xv_row(dtpanel, 0)+5,
			PANEL_LABEL_STRING, (day_first == 1 ?
				"Enter date (D, D/M, or D/M/Y):"
				: (day_first == 2 ?
				"Enter date (D, M/D, Y/M/D):"
				: "Enter date (D, M/D, or M/D/Y):")),
			PANEL_LABEL_FONT, font,
			PANEL_LABEL_BOLD, TRUE,
			PANEL_VALUE_STORED_LENGTH, 10,
			PANEL_VALUE_DISPLAY_LENGTH, 10,
			0);
	
	switch (day_first) {
	    case 1:
		sprintf(date, "%d/%d/%02d", current.tm_mday, current.tm_mon+1, current.tm_year);
		break;
	    case 2:
		sprintf(date, "%02d-%02d-%02d", current.tm_year, current.tm_mon+1, current.tm_mday);
		break;
	    case 0:
	    default:
		sprintf(date, "%d/%d/%02d", current.tm_mon+1, current.tm_mday, current.tm_year);
		break;
	}
	xv_set(setdate_pi, PANEL_VALUE, date, 0);
	xv_set(dtpanel, PANEL_DEFAULT_ITEM, def, 0);
	window_fit(dtpanel);
	window_fit(date_frame);
	set_frame_pos(date_frame, NULL, "dateSelection", &sizehints);
	xv_set(date_frame, XV_X, sizehints.x, XV_Y, sizehints.y, NULL);
}

/* replace cursors with hourglass symbol to show we're waiting */
void
lock_cursors()
{
	xv_set(frame, FRAME_BUSY, TRUE, 0);
	locked = 1;
}

/* restore cursors */
void
unlock_cursors()
{
	xv_set(frame, FRAME_BUSY, FALSE, 0);
	locked = 0;
}
