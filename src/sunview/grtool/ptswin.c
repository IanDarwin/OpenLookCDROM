/*
	ptswin.c - edit points

        $Header: ptswin.c,v 1.7 89/08/27 11:56:03 pturner Locked $
*/

#include <stdio.h>
#include <math.h>
#include <sys/types.h>
#include <sunwindow/attr.h>
#include <sunwindow/defaults.h>
#include <suntool/frame.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include <pixrect/pixrect.h>
#include "globals.h"
#include "defines.h"

extern int findpointflag;	/* use mouse to locate a point in a set */
extern int movepointflag;	/* use mouse to move a point */
extern int delpointflag;	/* use mouse to delete a point in a set */
extern int addpointflag;	/* use mouse to add a point to a set */
extern int extractpointflag;	/* use mouse to extract points from one set
				 * and add to another set */

#define define_select_set_panel(panel,x,y,panelname,panel_item) \
				panel_item=panel_create_item( panel,\
				PANEL_CYCLE,\
				PANEL_ITEM_X,x,\
				PANEL_ITEM_Y,y,\
				PANEL_LABEL_STRING,panelname,\
	 			PANEL_CHOICE_STRINGS,\
			"Set 0", "Set 1", "Set 2", "Set 3", "Set 4", "Set 5",\
			"Set 6", "Set 7", "Set 8", "Set 9", "Set 10", "Set 11",\
			"Set 12", "Set 13", "Set 14", 0, 0 );

void drawgraph();
void define_points_popup();

extern Pixfont *winfont;
extern Panel_item help_item;

extern Window main_frame, main_panel;
Frame points_frame;
static Panel points_panel;
Panel_item locate_point_item;

static void do_find_proc();
static void do_move_proc();
static void do_del_proc();
static void do_extract_proc();

findpoint(x, y, xs, ys, setno, loc)
    double x, y;
    double *xs, *ys;
    int *setno, *loc;
{
    double dx = xg2 - xg1, dy = yg2 - yg1, *getx(), *gety(), *xtmp, *ytmp, tmp, tmin = 1.0e307;
    int i, j, len;

    *setno = -1;
    for (i = 0; i < maxplot; i++) {
	if (isactive(i)) {
	    xtmp = getx(i);
	    ytmp = gety(i);
	    len = getsetlength(i);
	    for (j = 0; j < len; j++) {
		if ((tmp = hypot((x - xtmp[j]) / dx, (y - ytmp[j]) / dy)) < tmin) {
		    *setno = i;
		    *loc = j + 1;
		    *xs = xtmp[j];
		    *ys = ytmp[j];
		    tmin = tmp;
		}
	    }
	}
    }
}

del_point(setno, pt)
    int setno, pt;
{
    int i;
    double *x, *y, *getx(), *gety();

    x = getx(setno);
    y = gety(setno);
    for (i = pt; i < getsetlength(setno); i++) {
	x[i - 1] = x[i];
	y[i - 1] = y[i];
    }
    setlength(setno, getsetlength(setno) - 1);
    updatesetminmax(setno);
    drawgraph();
}

static void do_find_proc(item, event)
    Panel_item item;
    Event *event;
{
    set_action(0);
    /*panel_set_value(help_item, "Position cursor near point and press left mouse button");*/
    set_action(FIND_POINT);
}

static void do_move_proc(item, event)
    Panel_item item;
    Event *event;
{
}

static void do_del_proc(item, event)
    Panel_item item;
    Event *event;
{
    set_action(0);
    set_action(DEL_POINT);
}

static int inbounds(x, y)
    double x, y;
{
    if ((x >= xg1 && x <= xg2) && (y >= yg1 && y <= yg2)) {
	return (1);
    }
    return 0;
}

static void do_clip_to_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i, j, n;
    double *getx(), *gety(), *x, *y;

    for (i = 0; i < maxplot; i++) {

	if (isactive(i)&&(iserrbar(i)<0)) {
	    n = 0;
	    x = getx(i);
	    y = gety(i);
	    for (j = 0; j < getsetlength(i); j++) {
		if (inbounds(x[j], y[j])) {
		    x[n] = x[j];
		    y[n] = y[j];
		    n++;
		}
	    }
	    if (n == 0)
		killset(i);
	    else {
		setlength(i, n);
		updatesetminmax(i);
	    }
	    update_status(i);
	}
    }
    drawgraph();
}

static void do_clip_in_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i, j, n;
    double *getx(), *gety(), *x, *y;

    for (i = 0; i < maxplot; i++) {

	if (isactive(i)&&(iserrbar(i)<0)) {
	    n = 0;
	    x = getx(i);
	    y = gety(i);
	    for (j = 0; j < getsetlength(i); j++) {
		if (!inbounds(x[j], y[j])) {
		    x[n] = x[j];
		    y[n] = y[j];
		    n++;
		}
	    }
	    if (n == 0)
		killset(i);
	    else {
		setlength(i, n);
		updatesetminmax(i);
	    }
	    update_status(i);
	}
    }
    drawgraph();
}

static void do_add_proc(item, event)
    Panel_item item;
    Event *event;
{
}

static void do_extract_proc(item, event)
    Panel_item item;
    Event *event;
{
}

static void points_done_proc()
{
    window_set(points_frame, WIN_SHOW, FALSE, 0);
}

void define_points_popup()
{
    points_frame = window_create(main_frame, FRAME,
				 WIN_Y, 50,
				 FRAME_LABEL, "Edit points",
				 FRAME_SHOW_LABEL, TRUE,
			      WIN_ERROR_MSG, "Couldn't create points_frame",
				 0);
    points_panel = window_create(points_frame, PANEL,
				 0);

    panel_create_item(points_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		 panel_button_image(points_panel, "Find point", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_NOTIFY_PROC, do_find_proc,
		      0);
    locate_point_item = panel_create_item(points_panel, PANEL_TEXT,
			      PANEL_LABEL_STRING, "Set, location, (X, Y): ",
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(2),
					  PANEL_VALUE_DISPLAY_LENGTH, 40, 0);

/*    panel_create_item(points_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		 panel_button_image(points_panel, "Move point", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(4),
		      PANEL_NOTIFY_PROC, do_move_proc,
		      0);*/

    panel_create_item(points_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	       panel_button_image(points_panel, "Delete point", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_NOTIFY_PROC, do_del_proc,
		      0);

    panel_create_item(points_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
      panel_button_image(points_panel, "Clip points to window", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(7),
		      PANEL_NOTIFY_PROC, do_clip_to_proc,
		      0);

    panel_create_item(points_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
      panel_button_image(points_panel, "Clip points in window", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(9),
		      PANEL_NOTIFY_PROC, do_clip_in_proc,
		      0);

    /*
     * panel_create_item(points_panel, PANEL_BUTTON, PANEL_LABEL_IMAGE,
     * panel_button_image(points_panel, "Add point", 0, winfont),
     * PANEL_ITEM_X, ATTR_COL(1), PANEL_ITEM_Y, ATTR_ROW(6),
     * PANEL_NOTIFY_PROC, do_add_proc, 0);
     * 
     * panel_create_item(points_panel, PANEL_BUTTON, PANEL_LABEL_IMAGE,
     * panel_button_image(points_panel, "Extract points", 0, winfont),
     * PANEL_ITEM_X, ATTR_COL(1), PANEL_ITEM_Y, ATTR_ROW(7),
     * PANEL_NOTIFY_PROC, do_extract_proc, 0);
     */

    panel_create_item(points_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(points_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(11),
		      PANEL_NOTIFY_PROC, points_done_proc,
		      0);
    window_fit(points_panel);
    window_fit(points_frame);
}
