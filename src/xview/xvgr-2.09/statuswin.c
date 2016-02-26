/* $Id: statuswin.c,v 1.20 1992/09/05 04:35:12 pturner Exp pturner $
 *
 * status popup and about
 *
 */

#include <stdio.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/scrollbar.h>
#include <xview/font.h>
#include "patchlevel.h"
#include "globals.h"

#define SPAGESIZE 10
#define NPAGES (MAXPLOT/SPAGESIZE)

static int npages = NPAGES;

#define getdx(gno, setn)	getcol(gno, setn, 2)
#define getdy(gno, setn)	getcol(gno, setn, 3)

Frame status_frame = (Frame) 0;
static Frame status_panel;
static Panel_item select_status_item;
static int curpage = 0;
static int clear_status = 0;
static int show_dxdy = 0;

FILE *resfp = stdout;

/*
 * data is written to a canvas
 */
static Xv_Font fixedfont;
static Canvas scanvas;
static Display *sdpy;
static XFontStruct *sfont;
static GC sgc;
static XGCValues sgcval;
static Window sxwin;
static Xv_Window swin;
static int fh, fw;
void status_item_proc();

static char header[256];

static void status_done_proc()
{
    xv_set(status_frame, WIN_SHOW, FALSE, 0);
}

void setstatuslabel(s)
    char *s;
{
    XDrawString(sdpy, sxwin, sgc, 4, fh, s, strlen(s));
}

void update_status(gno, itemtype, itemno)
    int gno, itemtype, itemno;
{
    int i;
    void update_set_status();
    void update_graph_status();
    void update_region_status();

    if (status_frame) {
	XClearWindow(sdpy, sxwin);
	setstatuslabel(header);
	switch (itemtype) {
	case SETS:
	    if (itemno < 0) {
		for (i = curpage * SPAGESIZE; i < SPAGESIZE * (curpage + 1); i++) {
		    update_set_status(gno, i);
		}
	    }
	    break;
	case GRAPHS:
	    if (itemno < 0) {
		for (i = 0; i < MAXGRAPH; i++) {
		    update_graph_status(i);
		}
	    }
	    break;
	case REGIONS:
	    if (itemno < 0) {
		for (i = 0; i < MAXREGION; i++) {
		    update_region_status(i);
		}
	    }
	    break;
	}
    }
}

void update_graph_status(gno)
    int gno;
{
    char *graph_types();

    if (gno >= 0 && gno < MAXGRAPH) {
	if (status_frame && cur_statusitem == GRAPHS) {
	    if (gno == cg) {
		sprintf(buf, "  %2d    %3s    %3s    %6s    %d [Current graph]", gno, on_or_off(g[gno].active), yes_or_no((!g[gno].hidden)), graph_types(g[gno].type, 0), g[gno].maxplot);
	    } else {
		sprintf(buf, "  %2d    %3s    %3s    %6s    %d", gno, on_or_off(g[gno].active), yes_or_no((!g[gno].hidden)), graph_types(g[gno].type, 0), g[gno].maxplot);
	    }
	    XDrawString(sdpy, sxwin, sgc, 4, 3 * fh + 2 * (gno % SPAGESIZE) * fh, buf, strlen(buf));
	}
    }
}

void update_region_status(rno)
    int rno;
{
    char *region_types();

    if (rno >= 0 && rno < MAXREGION) {
	if (status_frame && cur_statusitem == REGIONS) {
	    sprintf(buf, "  %2d    %3s   %6s", rno, on_or_off(rg[rno].active), region_types(rg[rno].type, 0));
	    XDrawString(sdpy, sxwin, sgc, 4, 3 * fh + 2 * (rno % SPAGESIZE) * fh, buf, strlen(buf));
	}
    }
}

void update_set_status(gno, setno)
    int gno, setno;
{
    double x1, y1, x2, y2, xbar, ybar, xsd, ysd, dx1, dx2, dy1, dy2, dxbar, dybar, dxsd, dysd;
    int ix1, ix2;
    int iy1, iy2;
    char st[15], buf1[128], buf2[128];

    strcpy(st, "XY");
    if (setno >= curpage * SPAGESIZE && setno < (curpage + 1) * SPAGESIZE) {
	if (status_frame && cur_statusitem == SETS && gno == cg) {
	    if (isactive(gno, setno)) {
		minmax(getx(gno, setno), getsetlength(gno, setno), &x1, &x2, &ix1, &ix2);
		minmax(gety(gno, setno), getsetlength(gno, setno), &y1, &y2, &iy1, &iy2);
		xbar = 0.0;
		ybar = 0.0;
		xsd = 0.0;
		ysd = 0.0;
		dxbar = 0.0;
		dybar = 0.0;
		dxsd = 0.0;
		dysd = 0.0;
		stasum(getx(gno, setno), getsetlength(gno, setno), &xbar, &xsd, 0);
		stasum(gety(gno, setno), getsetlength(gno, setno), &ybar, &ysd, 0);
		switch (dataset_type(gno, setno)) {
		case XY:
		    strcpy(st, "XY");
		    break;
		case XYZ:
		    strcpy(st, "XY Z");
		    stasum(getdx(gno, setno), getsetlength(gno, setno), &dxbar, &dxsd, 0);
		    break;
		case XYDX:
		    strcpy(st, "XY DX");
		    break;
		case XYDY:
		    strcpy(st, "XY DY");
		    break;
		case XYDXDX:
		    strcpy(st, "XY DXDX");
		    break;
		case XYDYDY:
		    strcpy(st, "XY DYDY");
		    break;
		case XYDXDY:
		    strcpy(st, "XY DXDY");
		    break;
		case XYZW:
		    strcpy(st, "XY ZW");
		    break;
		case XYRT:
		    strcpy(st, "XY R");
		    break;
		case XYX2Y2:
		    strcpy(st, "XY X2Y2");
		    break;
		case XYSEG:
		    strcpy(st, "XY SEG");
		    break;
		case XYBOX:
		    strcpy(st, "XY BOX");
		    break;
		case XYARC:
		    strcpy(st, "XY ARC");
		    break;
		case XYYY:
		    strcpy(st, "XY Y1 Y2");
		    break;
		case XYXX:
		    strcpy(st, "XY X1 X2");
		    break;
		case XYHILO:
		    strcpy(st, "XY HILO");
		    break;
		}
		sprintf(buf1, " %2d %4d %3s  %7s | X %8.5g %5d %8.5g %5d %8.5g %8.5g   %s", setno, getsetlength(gno, setno), on_or_off(g[gno].p[setno].active), st, x1, ix1, x2, ix2, xbar, xsd, getcomment(gno, setno));
		sprintf(buf2, "                      | Y %8.5g %5d %8.5g %5d %8.5g %8.5g", y1, iy1, y2, iy2, ybar, ysd);
            } else if (g[gno].p[setno].deact) {
                sprintf(buf1, " %2d    De-activated", setno);
                strcpy(buf2, " ");
            } else {
                sprintf(buf1, " %2d    Undefined", setno);
                strcpy(buf2, " ");
            }
	    XDrawString(sdpy, sxwin, sgc, 4, 3 * fh + 3 * (setno % SPAGESIZE) * fh, buf1, strlen(buf1));
	    XDrawString(sdpy, sxwin, sgc, 4, 3 * fh + (3 * (setno % SPAGESIZE) + 1) * fh, buf2, strlen(buf2));
	}
    }
}

void update_status_popup()
{
    status_item_proc();
/*
    update_status(cg, cur_statusitem, -1);
*/
}

static void page_status_proc()
{
    curpage = (curpage + 1) % npages;
    update_status(cg, cur_statusitem, -1);
}

static void home_status_proc()
{
    curpage = 0;
    update_status(cg, cur_statusitem, -1);
}

static void end_status_proc()
{
    curpage = npages - 1;
    update_status(cg, cur_statusitem, -1);
}

void status_item_proc()
{
    int cd;

    if (status_frame) {
	cd = (int) xv_get(select_status_item, PANEL_VALUE);
	switch (cd) {
	case 0:
	    cur_statusitem = SETS;
	    sprintf(header, " set# n  stat  type   | X/Y   min    at      max    at     mean    std. dev.  comment");
	    break;
	case 1:
	    cur_statusitem = GRAPHS;
	    sprintf(header, " Graph # Active  Show  Type  Max sets");
	    break;
	case 2:
	    cur_statusitem = REGIONS;
	    sprintf(header, " Region # Active  Type");
	    break;
	}
	clear_status = 1;
	update_status(cg, cur_statusitem, -1);
    }
}

/*
 * write the status to the results file
 */
static void update_stuff_status()
{
    double x1, y1, x2, y2, xbar, ybar, xsd, ysd;
    int i;

    sprintf(buf, "\nStatus of sets for graph %d (current)\n", cg);
    stufftext(buf);
    for (i = 0; i < g[cg].maxplot; i++) {
	getsetminmax(cg, i, &x1, &x2, &y1, &y2);
	if (isactive(cg, i)) {
	    stasum(getx(cg, i), getsetlength(cg, i), &xbar, &xsd, 0);
	    stasum(gety(cg, i), getsetlength(cg, i), &ybar, &ysd, 0);
	    sprintf(buf, " %2d %4d %3s  X  %8.5g %8.5g %8.5g %8.5g   %s\n", i, getsetlength(cg, i), on_or_off(isactive(cg, i)), x1, x2, xbar, xsd, getcomment(cg, i));
	    stufftext(buf);
	    sprintf(buf, "              Y  %8.5g %8.5g %8.5g %8.5g\n", y1, y2, ybar, ysd);
	    stufftext(buf);
	}
    }
    for (i = 0; i < MAXGRAPH; i++) {
    }
    for (i = 0; i < MAXREGION; i++) {
    }
}

/*
 * build the status popup
 */
void define_status_popup()
{
    extern Frame main_frame;
    int pagetest;

    if (status_frame) {
	xv_set(status_frame, WIN_SHOW, TRUE, NULL);
	pagetest = g[cg].maxplot % SPAGESIZE;
	npages = g[cg].maxplot / SPAGESIZE;
	update_status_popup();
	return;
    }
    pagetest = g[cg].maxplot % SPAGESIZE;
    npages = g[cg].maxplot / SPAGESIZE;
    status_frame = xv_create(main_frame, FRAME,
			     WIN_Y, 50,
			     XV_LABEL, "Status",
			     FRAME_SHOW_LABEL, TRUE,
			     WIN_ERROR_MSG, "Couldn't create status_frame",
			     0);
    status_panel = xv_create(status_frame, PANEL,
			     WIN_ROW_GAP, 1,
			     0);
    xv_create(status_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Done",
	      XV_X, xv_col(status_panel, 0),
	      XV_Y, xv_row(status_panel, 0),
	      PANEL_NOTIFY_PROC, status_done_proc,
	      0);
    xv_create(status_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Update",
	      XV_X, xv_col(status_panel, 7),
	      XV_Y, xv_row(status_panel, 0),
	      PANEL_NOTIFY_PROC, update_status_popup,
	      0);
    xv_create(status_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Write",
	      XV_X, xv_col(status_panel, 16),
	      XV_Y, xv_row(status_panel, 0),
	      PANEL_NOTIFY_PROC, update_stuff_status,
	      0);
    xv_create(status_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Page",
	      XV_X, xv_col(status_panel, 7),
	      XV_Y, xv_row(status_panel, 1),
	      PANEL_NOTIFY_PROC, page_status_proc,
	      0);
    xv_create(status_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Home",
	      XV_X, xv_col(status_panel, 16),
	      XV_Y, xv_row(status_panel, 1),
	      PANEL_NOTIFY_PROC, home_status_proc,
	      0);
    xv_create(status_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "End",
	      XV_X, xv_col(status_panel, 25),
	      XV_Y, xv_row(status_panel, 1),
	      PANEL_NOTIFY_PROC, end_status_proc,
	      0);
    select_status_item = xv_create(status_panel, PANEL_CHOICE,
				   PANEL_LABEL_STRING, "Display status of",
				   PANEL_NOTIFY_PROC, status_item_proc,
				   PANEL_CHOICE_STRINGS,
				   "Sets", "Graphs", "Regions",
				   NULL,
				   XV_X, xv_col(status_panel, 30),
				   XV_Y, xv_row(status_panel, 0),
				   0);
    window_fit_height(status_panel);
    scanvas = xv_create(status_frame, CANVAS, NULL);
    xv_set(scanvas,
	   CANVAS_AUTO_SHRINK, TRUE,
	   CANVAS_AUTO_EXPAND, TRUE,
	   CANVAS_WIDTH, 800,
	   CANVAS_HEIGHT, 450,
	   XV_WIDTH, 700,
	   XV_HEIGHT, 450,
	   CANVAS_REPAINT_PROC, update_status_popup,
	   0);

    sdpy = (Display *) xv_get(status_frame, XV_DISPLAY);
    fixedfont = (Xv_Font) xv_find(main_frame, FONT, FONT_FAMILY, FONT_FAMILY_DEFAULT_FIXEDWIDTH, NULL);
    fw = (int) xv_get(fixedfont, FONT_DEFAULT_CHAR_WIDTH);
    fh = (int) xv_get(fixedfont, FONT_DEFAULT_CHAR_HEIGHT);
    sfont = (XFontStruct *) xv_get(fixedfont, FONT_INFO);
    sgcval.font = sfont->fid;
    sgcval.foreground = BlackPixel(sdpy, DefaultScreen(sdpy));
    sgcval.background = WhitePixel(sdpy, DefaultScreen(sdpy));
    sgcval.graphics_exposures = False;
    sgc = XCreateGC(sdpy, RootWindow(sdpy, DefaultScreen(sdpy)), GCForeground | GCBackground | GCFont | GCGraphicsExposures, &sgcval);
    swin = (Xv_Window) xv_get(scanvas, CANVAS_NTH_PAINT_WINDOW, 0, NULL);
    sxwin = (Window) xv_get(swin, XV_XID);
    window_fit(status_frame);
    xv_set(status_frame, WIN_SHOW, TRUE, NULL);
    update_status_popup();
}

/*
 * say a few things about xvgr, number of graphs, set size
 */
Frame about_frame = (Frame) 0;
static Panel about_panel;
static Panel_item about_item[8];

void about_done_proc()
{
    xv_set(about_frame, WIN_SHOW, FALSE, 0);
}

void create_about_grtool()
{
    extern Frame main_frame;

    if (about_frame) {
	xv_set(about_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    about_frame = xv_create(main_frame, FRAME,
			    WIN_Y, 50,
			    WIN_X, 50,
			    XV_LABEL, "About xvgr",
			    FRAME_SHOW_LABEL, TRUE,
			    WIN_ERROR_MSG, "Couldn't create about_frame",
			    0);
    about_panel = xv_create(about_frame, PANEL,
			    WIN_ROW_GAP, 1,
			    0);
    sprintf(buf, " %s - Patch level %d", version, PATCHLEVEL);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 0),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Max number of sets = %d", maxplot);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 1),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Max scratch array length = %d", MAXARR);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 2),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "This version does not do grids");
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 3),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Max number of graphs = %d", maxgraph);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 4),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Max number of lines = %d", MAXLINES);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 5),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Max number of boxes = %d", MAXBOXES);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 6),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Maximum number of strings = %d", MAXSTR);
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 7),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "This version acquired from ftp.ccalmr.ogi.edu [129.95.72.34]");
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 8),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "Comments and bug reports to pturner@amb4.ccalmr.ogi.edu");
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 9),
	      PANEL_LABEL_STRING, buf, 0);
    sprintf(buf, "All mail will be read, but time contraints prevent answers");
    xv_create(about_panel, PANEL_MESSAGE,
	      XV_X, xv_col(about_panel, 1),
	      XV_Y, xv_row(about_panel, 10),
	      PANEL_LABEL_STRING, buf, 0);
    xv_create(about_panel, PANEL_BUTTON,
	      XV_X, xv_col(about_panel, 10),
	      XV_Y, xv_row(about_panel, 12),
	      PANEL_LABEL_STRING, "Done",
	      PANEL_NOTIFY_PROC, about_done_proc, 0);
    window_fit(about_panel);
    window_fit(about_frame);
    xv_set(about_frame, WIN_SHOW, TRUE, 0);
}
