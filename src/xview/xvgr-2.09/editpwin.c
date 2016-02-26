/* $Id: editpwin.c,v 1.5 91/12/08 15:33:43 pturner Exp Locker: pturner $
 *
 * edit a set
 *
 */

#include <stdio.h>
#include <math.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

extern Frame main_frame;

Frame editp_frame = (Frame) NULL;
Panel editp_panel;

/*
 * Panel item declarations
 */
Panel_item editp_table[10][4];
Panel_item editp_set_choice_item;
Panel_item editp_prec_choice_item;
Panel_item editp_form_choice_item;

static int curpage = 0;
static int curline = 0;		/* not used yet */
static int currow = 0;		/* not used yet */
static int curcol = 0;		/* not used yet */
static int curset = 0;
static double *ex, *ey, *edx, *edy;

/*
 * Event and Notify proc declarations
 */
static int editp_Done_notify_proc();
static int editp_accept_notify_proc();
static int editp_update_notify_proc();
static void editp_pageup_notify_proc();
static void editp_pagedown_notify_proc();
static void editp_home_notify_proc();
static void editp_end_notify_proc();
static Panel_setting do_entry_proc();
static int editp_set_proc();
static void do_update_editp_proc();
void update_editp_proc();

#define EDIT_ROW 101
#define EDIT_COL 102
#define MAX_ROWS 10
#define MAX_COLS 2

static void load_edit_set(gno, setno)
    int gno, setno;
{
    int i;
    double *x, *y;
    int len = getsetlength(gno, setno);

    if (ex != NULL) {
	cfree(ex);
    }
    if (ey != NULL) {
	cfree(ey);
    }
    if (edx != NULL) {
	cfree(edx);
    }
    if (edy != NULL) {
	cfree(edy);
    }
    if (!isactive(gno, setno)) {
	return;
    }
    ex = (double *) calloc(len, sizeof(double));
    ey = (double *) calloc(len, sizeof(double));
/*
	edx = (double *) calloc(len, sizeof(double));
	edy = (double *) calloc(len, sizeof(double));
*/
    x = getx(gno, setno);
    y = gety(gno, setno);
    for (i = 0; i < len; i++) {
	ex[i] = x[i];
	ey[i] = y[i];
    }
}

static void accept_edit_set(gno, setno)
    int gno, setno;
{
    int i;
    double *x, *y;
    int len = getsetlength(gno, setno);

    if (isactive(cg, curset) && len != 0) {
	x = getx(gno, setno);
	y = gety(gno, setno);
	for (i = 0; i < len; i++) {
	    x[i] = ex[i];
	    y[i] = ey[i];
	}
	updatesetminmax(gno, setno);
	update_set_status(gno, setno);
    } else {
	errwin("Set not active");
    }
}


/*
 * Create the point editor
 */
void create_editp_frame()
{
    int i, j;

    if (editp_frame) {
	do_update_editp_proc();
	xv_set(editp_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    editp_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "Edit sets",
				    NULL);
    editp_panel = (Panel) xv_create(editp_frame, PANEL,
				    NULL);
    editp_set_choice_item = (Panel_item) xv_create(editp_panel, PANEL_CHOICE_STACK,
						   PANEL_CHOICE_NCOLS, 3,
					    PANEL_LABEL_STRING, "Edit set:",
						   PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
		 "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
		 "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
						   NULL,
					  PANEL_NOTIFY_PROC, editp_set_proc,
					       XV_X, xv_col(editp_panel, 0),
					       XV_Y, xv_row(editp_panel, 0),
						   NULL);
    (void) xv_create(editp_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Page down",
		     PANEL_NOTIFY_PROC, editp_pagedown_notify_proc,
		     XV_X, xv_col(editp_panel, 0),
		     XV_Y, xv_row(editp_panel, 1),
		     NULL);
    (void) xv_create(editp_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Page up",
		     PANEL_NOTIFY_PROC, editp_pageup_notify_proc,
		     XV_X, xv_col(editp_panel, 15),
		     XV_Y, xv_row(editp_panel, 1),
		     NULL);
    (void) xv_create(editp_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Home",
		     PANEL_NOTIFY_PROC, editp_home_notify_proc,
		     XV_X, xv_col(editp_panel, 0),
		     XV_Y, xv_row(editp_panel, 2),
		     NULL);
    (void) xv_create(editp_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "End",
		     PANEL_NOTIFY_PROC, editp_end_notify_proc,
		     XV_X, xv_col(editp_panel, 15),
		     XV_Y, xv_row(editp_panel, 2),
		     NULL);
    editp_prec_choice_item = (Panel_item) xv_create(editp_panel, PANEL_CHOICE_STACK,
					   PANEL_LABEL_STRING, "Precision:",
						    PANEL_CHOICE_STRINGS,
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						    NULL,
					      XV_X, xv_col(editp_panel, 30),
					       XV_Y, xv_row(editp_panel, 0),
						    NULL);
    editp_form_choice_item = (Panel_item) xv_create(editp_panel, PANEL_CHOICE_STACK,
					      PANEL_LABEL_STRING, "Format:",
						    PANEL_CHOICE_STRINGS,
					"General", "Decimal", "Exponential",
						    NULL,
					      XV_X, xv_col(editp_panel, 30),
					       XV_Y, xv_row(editp_panel, 1),
						    NULL);
    (void) xv_create(editp_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "#",
		     XV_X, xv_col(editp_panel, 0),
		     XV_Y, xv_row(editp_panel, 3),
		     NULL);
    (void) xv_create(editp_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "X",
		     XV_X, xv_col(editp_panel, 6),
		     XV_Y, xv_row(editp_panel, 3),
		     NULL);
    (void) xv_create(editp_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Y",
		     XV_X, xv_col(editp_panel, 22),
		     XV_Y, xv_row(editp_panel, 3),
		     NULL);
    for (j = 0; j < MAX_COLS; j++) {
	for (i = 0; i < MAX_ROWS; i++) {
	    if (j == 0) {
		sprintf(buf, "%5d", i + 1);
	    } else {
		buf[0] = 0;
	    }
	    editp_table[i][j] = (Panel_item) xv_create(editp_panel, PANEL_TEXT,
					      PANEL_NOTIFY_STRING, "\r\t\n",
					   PANEL_NOTIFY_PROC, do_entry_proc,
					     PANEL_VALUE_DISPLAY_LENGTH, 14,
						    PANEL_LABEL_STRING, buf,
			XV_X, xv_col(editp_panel, (j > 0) ? j * 16 + 5 : 0),
					   XV_Y, xv_row(editp_panel, i + 4),
						   XV_KEY_DATA, EDIT_ROW, i,
						   XV_KEY_DATA, EDIT_COL, j,
						       NULL);
	}
    }
    (void) xv_create(editp_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, editp_Done_notify_proc,
		     XV_X, xv_col(editp_panel, 15),
		     XV_Y, xv_row(editp_panel, MAX_ROWS + 4),
		     NULL);
    (void) xv_create(editp_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, editp_accept_notify_proc,
		     XV_X, xv_col(editp_panel, 5),
		     XV_Y, xv_row(editp_panel, MAX_ROWS + 4),
		     NULL);
    window_fit(editp_panel);
    window_fit(editp_frame);
    load_edit_set(cg, curset);
    do_update_editp_proc();
    xv_set(editp_frame, WIN_SHOW, TRUE, 0);
}				/* end create_editp_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int editp_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(editp_frame, WIN_SHOW, FALSE, 0);
}

/*ARGSUSED*/
static int editp_set_proc()
{
    curset = (int) xv_get(editp_set_choice_item, PANEL_VALUE);
    if (isactive(cg, curset)) {
	load_edit_set(cg, curset);
    } else {
	errwin("Warning: Set not active");
    }
    do_update_editp_proc();
}

/*ARGSUSED*/
static Panel_setting do_entry_proc(item, event)
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
    int irow, icol;
    extern double result;

    errpos = 0;
    if (!isactive(cg, curset)) {
	errwin("Warning: Set not active");
	return PANEL_NONE;
    }
    irow = (int) xv_get(item, XV_KEY_DATA, EDIT_ROW) + curpage * MAX_ROWS;
    icol = (int) xv_get(item, XV_KEY_DATA, EDIT_COL);
    strcpy(val, (char *) xv_get(item, PANEL_VALUE));
    if (strlen(val) == 0) {
	return PANEL_NONE;
    }
    fixupstr(val);
    scanner(val, &x, &y, 1, &a, &b, &c, &d, 1, 0, 0, &errpos);
    if (errpos) {
	xv_set(item, PANEL_VALUE, "error", NULL);
    } else {
	sprintf(val, "%.5g", result);
	xv_set(item, PANEL_VALUE, val, NULL);
	switch (icol) {
	case 0:
	    if (ex != NULL) {
		ex[irow] = result;
	    }
	    break;
	case 1:
	    if (ey != NULL) {
		ey[irow] = result;
	    }
	    break;
	case 2:
	    if (edx != NULL) {
		edx[irow] = result;
	    }
	    break;
	case 3:
	    if (edy != NULL) {
		edy[irow] = result;
	    }
	    break;

	}
    }
    return PANEL_NEXT;
}

static void do_update_editp_proc()
{
    update_editp_proc(cg, curset);
}

/*ARGSUSED*/
void update_editp_proc(gno, setno)
    int gno, setno;
{
    int i, len = getsetlength(gno, curset);
    char val[128];

    if (editp_frame) {
	if (setno == -1) {
	    load_edit_set(gno, curset);
	    if (!isactive(gno, curset)) {
		len = 0;
	    }
	}
	if (curpage * MAX_ROWS > len) {
	    curpage = 0;
	}
	for (i = 0; i < MAX_ROWS; i++) {
	    sprintf(val, "%5d", i + MAX_ROWS * curpage + 1);
	    xv_set(editp_table[i][0], PANEL_LABEL_STRING, val, NULL);
	    if (len > i + MAX_ROWS * curpage) {
		sprintf(val, "%.5g", ex[i + MAX_ROWS * curpage]);
		xv_set(editp_table[i][0], PANEL_VALUE, val, NULL);
		sprintf(val, "%.5g", ey[i + MAX_ROWS * curpage]);
		xv_set(editp_table[i][1], PANEL_VALUE, val, NULL);
	    } else {
		val[0] = 0;
		xv_set(editp_table[i][0], PANEL_VALUE, val, NULL);
		xv_set(editp_table[i][1], PANEL_VALUE, val, NULL);
	    }
	}
    }
}

/*ARGSUSED*/
static void editp_pageup_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    if (!isactive(cg, curset)) {
	errwin("Warning: Set not active");
	return;
    }
    curpage--;
    if (curpage < 0) {
	curpage = getsetlength(cg, curset) / (MAX_ROWS + 1);
    }
    do_update_editp_proc();
}

/*ARGSUSED*/
static void editp_pagedown_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    if (!isactive(cg, curset)) {
	errwin("Warning: Set not active");
	return;
    }
    if (curpage * MAX_ROWS < getsetlength(cg, curset)) {
	curpage++;
	if (curpage * MAX_ROWS == getsetlength(cg, curset)) {
	    curpage = 0;
	}
    }
    do_update_editp_proc();
}

/*ARGSUSED*/
static void editp_home_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    if (!isactive(cg, curset)) {
	errwin("Warning: Set not active");
	return;
    }
    curpage = 0;
    do_update_editp_proc();
}

/*ARGSUSED*/
static void editp_end_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    if (!isactive(cg, curset)) {
	errwin("Warning: Set not active");
	return;
    }
    curpage = getsetlength(cg, curset) / MAX_ROWS;
    do_update_editp_proc();
}

/*ARGSUSED*/
static int editp_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    if (!isactive(cg, curset)) {
	errwin("Warning: Set not active");
	return;
    }
    accept_edit_set(cg, curset);
}
