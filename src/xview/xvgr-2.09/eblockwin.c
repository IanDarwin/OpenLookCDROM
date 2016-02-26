/* $Id: eblockwin.c,v 1.5 92/02/29 20:58:11 pturner Exp Locker: pturner $
 *
 * Edit block data Panel
 *
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

extern double *blockdata[];
extern int blocklen;
extern int blockncols;

static char ncolsbuf[128];

static int block_curtype = XY;

extern Frame main_frame;

Frame eblock_frame = (Frame) 0;
Panel eblock_panel;

/*
 * Panel item declarations
 */
static Panel_item eblock_ncols_item;
static Panel_item eblock_type_choice_item;
static Panel_item eblock_x_choice_item;
static Panel_item eblock_y_choice_item;
static Panel_item eblock_e1_choice_item;
static Panel_item eblock_e2_choice_item;
static Panel_item eblock_e3_choice_item;
static Panel_item eblock_e4_choice_item;
static Panel_item eblock_e5_choice_item;
static Panel_item eblock_set_choice_item;
static Panel_item eblock_graph_choice_item;

/*
 * Event and Notify proc declarations
 */
static int eblock_Done_notify_proc();
static void eblock_type_notify_proc();
static int eblock_accept_notify_proc();
static void update_eblock();

/*
 * Create the files Frame and the files Panel
 */
void create_eblock_frame()
{
    if (blockncols == 0) {
	errwin("Need to read block data first");
	return;
    }
    if (eblock_frame) {
	update_eblock();
	xv_set(eblock_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    eblock_frame = (Frame) xv_create(main_frame, FRAME,
				     FRAME_LABEL, "Block data",
				     NULL);
    eblock_panel = (Panel) xv_create(eblock_frame, PANEL,
				     PANEL_LAYOUT, PANEL_VERTICAL,
				     NULL);
    eblock_ncols_item = (Panel_item) xv_create(eblock_panel, PANEL_MESSAGE,
					       PANEL_LABEL_STRING, "tmp",
					       NULL);
    eblock_type_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
				     PANEL_LABEL_STRING, "Create set type:",
					   XV_HELP_DATA, "xvgr:eblock_type",
				 PANEL_NOTIFY_PROC, eblock_type_notify_proc,
						     PANEL_CHOICE_STRINGS,
						     "X Y",
						     "X Y DX",
						     "X Y DY",
						     "X Y DX1 DX2",
						     "X Y DY1 DY2",
						     "X Y DX DY",
						     "X Y Z",
						     "X HI LO OPEN CLOSE",
						     "X Y RADIUS",
						     NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						     NULL);
    eblock_x_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "X from column:",
					      XV_HELP_DATA, "xvgr:eblock_x",
						  PANEL_CHOICE_NCOLS, 4,
						  PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
	   "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
	   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
						  NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						  NULL);
    eblock_y_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "Y from column:",
					      XV_HELP_DATA, "xvgr:eblock_y",
						  PANEL_CHOICE_NCOLS, 4,
						  PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
	   "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
	   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
						  NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						  NULL);
    eblock_e1_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "E1 from column:",
					     XV_HELP_DATA, "xvgr:eblock_e1",
						   PANEL_CHOICE_NCOLS, 4,
						   PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
	   "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
	   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
						   NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						   NULL);
    eblock_e2_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "E2 from column:",
					     XV_HELP_DATA, "xvgr:eblock_e2",
						   PANEL_CHOICE_NCOLS, 4,
						   PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
	   "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
	   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
						   NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						   NULL);
    eblock_e3_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
				      PANEL_LABEL_STRING, "E3 from column:",
					     XV_HELP_DATA, "xvgr:eblock_e3",
						   PANEL_CHOICE_NCOLS, 4,
						   PANEL_CHOICE_STRINGS,
				"1", "2", "3", "4", "5", "6", "7", "8", "9",
	   "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
	   "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
						   NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						   NULL);
    eblock_set_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
						    PANEL_CHOICE_NCOLS, 3,
					 PANEL_LABEL_STRING, "Load to set:",
					    XV_HELP_DATA, "xvgr:eblock_set",
						    PANEL_CHOICE_STRINGS,
		   "Next", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
		 "10", "11", "12", "13", "14", "15", "16", "17", "18", "19",
		 "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
						    NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						    NULL);
    eblock_graph_choice_item = (Panel_item) xv_create(eblock_panel, PANEL_CHOICE_STACK,
					    PANEL_LABEL_STRING, "In graph:",
					  XV_HELP_DATA, "xvgr:eblock_graph",
						      PANEL_CHOICE_STRINGS,
						      "Current",
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						      NULL,
				    PANEL_VALUE_X, xv_col(eblock_panel, 18),
						      NULL);

    (void) xv_create(eblock_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, eblock_accept_notify_proc,
		     XV_HELP_DATA, "xvgr:eblock_accept",
		     XV_X, xv_col(eblock_panel, 5),
		     XV_Y, xv_row(eblock_panel, 11),
		     NULL);
    (void) xv_create(eblock_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, eblock_Done_notify_proc,
		     XV_HELP_DATA, "xvgr:eblock_done",
		     XV_X, xv_col(eblock_panel, 17),
		     XV_Y, xv_row(eblock_panel, 11),
		     NULL);
    window_fit(eblock_panel);
    window_fit(eblock_frame);
    update_eblock();
    xv_set(eblock_frame, WIN_SHOW, TRUE, 0);
}				/* end create_eblock_panel */

static int set_types[] = {XY, NXY, IHL, BIN, XYDX, XYDY, XYDXDX, XYDYDY, XYDXDY, XYZ, XYHILO, -1};

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int eblock_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(eblock_frame, WIN_SHOW, FALSE, 0);
    return XV_OK;
}

static void update_eblock()
{
    if (!eblock_frame) {
	return;
    }
    if (blockncols == 0) {
	errwin("Need to read block data first");
	return;
    }
    sprintf(ncolsbuf, "%d columns of length = %d", blockncols, blocklen);
    xv_set(eblock_ncols_item, PANEL_LABEL_STRING, ncolsbuf, NULL);
    switch (block_curtype) {
    case XY:
	xv_set(eblock_e1_choice_item, XV_SHOW, FALSE, 0);
	xv_set(eblock_e2_choice_item, XV_SHOW, FALSE, 0);
	xv_set(eblock_e3_choice_item, XV_SHOW, FALSE, 0);
	panel_paint(eblock_e1_choice_item, PANEL_CLEAR);
	panel_paint(eblock_e2_choice_item, PANEL_CLEAR);
	panel_paint(eblock_e3_choice_item, PANEL_CLEAR);
	break;
    case XYRT:
    case XYDX:
    case XYDY:
    case XYZ:
	xv_set(eblock_e1_choice_item, XV_SHOW, TRUE, 0);
	xv_set(eblock_e2_choice_item, XV_SHOW, FALSE, 0);
	xv_set(eblock_e3_choice_item, XV_SHOW, FALSE, 0);
	panel_paint(eblock_e2_choice_item, PANEL_CLEAR);
	panel_paint(eblock_e3_choice_item, PANEL_CLEAR);
	break;
    case XYDXDX:
    case XYDYDY:
    case XYDXDY:
	xv_set(eblock_e1_choice_item, XV_SHOW, TRUE, 0);
	xv_set(eblock_e2_choice_item, XV_SHOW, TRUE, 0);
	xv_set(eblock_e3_choice_item, XV_SHOW, FALSE, 0);
	panel_paint(eblock_e3_choice_item, PANEL_CLEAR);
	break;
    case XYHILO:
	xv_set(eblock_e1_choice_item, XV_SHOW, TRUE, 0);
	xv_set(eblock_e2_choice_item, XV_SHOW, TRUE, 0);
	xv_set(eblock_e3_choice_item, XV_SHOW, TRUE, 0);
	break;
    }
}

/*ARGSUSED*/
static void eblock_type_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int d1 = (int) xv_get(eblock_type_choice_item, PANEL_VALUE);

    switch (d1) {
    case 0:
	block_curtype = XY;
	break;
    case 1:
	block_curtype = XYDX;
	break;
    case 2:
	block_curtype = XYDY;
	break;
    case 3:
	block_curtype = XYDXDX;
	break;
    case 4:
	block_curtype = XYDYDY;
	break;
    case 5:
	block_curtype = XYDXDY;
	break;
    case 6:
	block_curtype = XYZ;
	break;
    case 7:
	block_curtype = XYHILO;
	break;
    case 8:
	block_curtype = XYRT;
	break;
    }
    update_eblock();
}

/*ARGSUSED*/
static int eblock_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0;
    char buf[256];
    int setno, graphno;
    int d1, d2, cx, cy, c1, c2, c3;
    double *tx, *ty, *t2, *t3, *t4;

    d1 = (int) xv_get(eblock_type_choice_item, PANEL_VALUE);
    cx = (int) xv_get(eblock_x_choice_item, PANEL_VALUE);
    cy = (int) xv_get(eblock_y_choice_item, PANEL_VALUE);
    if (cx >= blockncols) {
	errwin("Column for X exceeds the number of columns in block data");
	return;
    }
    if (cy >= blockncols) {
	errwin("Column for Y exceeds the number of columns in block data");
	return;
    }
    switch (block_curtype) {
    case XY:
	break;
    case XYRT:
    case XYDX:
    case XYDY:
    case XYZ:
	c1 = (int) xv_get(eblock_e1_choice_item, PANEL_VALUE);
	if (c1 >= blockncols) {
	    errwin("Column for E1 exceeds the number of columns in block data");
	    return;
	}
	break;
    case XYDXDX:
    case XYDYDY:
    case XYDXDY:
	c1 = (int) xv_get(eblock_e1_choice_item, PANEL_VALUE);
	c2 = (int) xv_get(eblock_e2_choice_item, PANEL_VALUE);
	if (c1 >= blockncols) {
	    errwin("Column for E1 exceeds the number of columns in block data");
	    return;
	}
	if (c2 >= blockncols) {
	    errwin("Column for E2 exceeds the number of columns in block data");
	    return;
	}
	break;
    case XYHILO:
	c1 = (int) xv_get(eblock_e1_choice_item, PANEL_VALUE);
	c2 = (int) xv_get(eblock_e2_choice_item, PANEL_VALUE);
	c3 = (int) xv_get(eblock_e3_choice_item, PANEL_VALUE);
	if (c1 >= blockncols) {
	    errwin("Column for E1 exceeds the number of columns in block data");
	    return;
	}
	if (c2 >= blockncols) {
	    errwin("Column for E2 exceeds the number of columns in block data");
	    return;
	}
	if (c3 >= blockncols) {
	    errwin("Column for E3 exceeds the number of columns in block data");
	    return;
	}
	break;
    }

    setno = (int) xv_get(eblock_set_choice_item, PANEL_VALUE) - 1;
    graphno = (int) xv_get(eblock_graph_choice_item, PANEL_VALUE) - 1;

    if (graphno == -1) {
	graphno = cg;
    }
    if (setno == -1) {
	setno = nextset(graphno);
    }
    if (setno == -1) {
	return;
    }
    if (g[graphno].active == OFF) {
	set_graph_active(graphno);
    }
    activateset(graphno, setno);
    settype(graphno, setno, block_curtype);

    tx = (double *) calloc(blocklen, sizeof(double));
    ty = (double *) calloc(blocklen, sizeof(double));
    for (i = 0; i < blocklen; i++) {
	tx[i] = blockdata[cx][i];
	ty[i] = blockdata[cy][i];
    }
    setcol(graphno, tx, setno, blocklen, 0);
    setcol(graphno, ty, setno, blocklen, 1);

    switch (block_curtype) {
    case XY:
	sprintf(buf, "Cols %d %d", cx + 1, cy + 1);
	break;
    case XYRT:
    case XYDX:
    case XYDY:
    case XYZ:
	sprintf(buf, "Cols %d %d %d", cx + 1, cy + 1, c1 + 1);
	t2 = (double *) calloc(blocklen, sizeof(double));
	for (i = 0; i < blocklen; i++) {
	    t2[i] = blockdata[c1][i];
	}
	setcol(graphno, t2, setno, blocklen, 2);
	break;
    case XYDXDX:
    case XYDYDY:
    case XYDXDY:
	sprintf(buf, "Cols %d %d %d %d", cx + 1, cy + 1, c1 + 1, c2 + 1);
	t2 = (double *) calloc(blocklen, sizeof(double));
	t3 = (double *) calloc(blocklen, sizeof(double));
	for (i = 0; i < blocklen; i++) {
	    t2[i] = blockdata[c1][i];
	    t3[i] = blockdata[c2][i];
	}
	setcol(graphno, t2, setno, blocklen, 2);
	setcol(graphno, t3, setno, blocklen, 3);
	break;
    case XYHILO:
	sprintf(buf, "Cols %d %d %d %d %d", cx + 1, cy + 1, c1 + 1, c2 + 1, c3 + 1);
	t2 = (double *) calloc(blocklen, sizeof(double));
	t3 = (double *) calloc(blocklen, sizeof(double));
	t4 = (double *) calloc(blocklen, sizeof(double));
	for (i = 0; i < blocklen; i++) {
	    t2[i] = blockdata[c1][i];
	    t3[i] = blockdata[c2][i];
	    t4[i] = blockdata[c3][i];
	}
	setcol(graphno, t2, setno, blocklen, 2);
	setcol(graphno, t3, setno, blocklen, 3);
	setcol(graphno, t4, setno, blocklen, 4);
	break;
    }

    setcomment(graphno, setno, buf);
    updatesetminmax(graphno, setno);
    update_status_popup();
    drawgraph();
    return XV_OK;
}
