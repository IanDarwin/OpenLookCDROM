/* $Id: nonlwin.c,v 1.5 92/06/27 05:39:34 pturner Exp Locker: pturner $
 *
 * non linear curve fitting
 *
 */

#include <stdio.h>
#include <math.h>
#include <xview/xview.h>
#include <xview/canvas.h>
#include <xview/panel.h>
#include <xview/cursor.h>
#include <xview/font.h>
#include "globals.h"

/*
info = 0  improper input parameters.
info = 1  algorithm estimates that the relative error in the sum of squares is at most tol.
info = 2  algorithm estimates that the relative error between x and the solution is at most tol.
info = 3  conditions for info = 1 and info = 2 both hold.
info = 4  fvec is orthogonal to the columns of the jacobian to machine precision.
info = 5  number of calls to fcn has reached or exceeded 200*(n+1).
info = 6  tol is too small. no further reduction in the sum of squares is possible.
info = 7  tol is too small. no further improvement in the approximate solution x is possible.
*/
char *info_strings[] = {
    "Improper input parameters.",
    "Relative error in the sum of squares is at most tol.",
    "Relative error between A and the solution is at most tol.",
    "Relative error in the sum of squares and A and the solution is at most tol.",
    "Fvec is orthogonal to the columns of the jacobian to machine precision.",
    "Number of calls to fcn has reached or exceeded 200*(n+1).",
    "Tol is too small. No further reduction in the sum of squares is possible.",
    "Tol is too small. No further improvement in the approximate solution A is possible."
};

extern double nonl_parms[];

void drawgraph();
void generic_done_proc();

void create_nonl_frame();
void create_nonleval_frame();
void create_nonlread_frame();
void create_nonlwrite_frame();

#define FRAME_KEY        101
#define MAXPARM 10

/* ARGSUSED */

static Frame nonl_frame = (Frame) 0;
static Panel nonl_panel;
static Panel_item nonl_formula_item;
static Panel_item nonl_set_item;
static Panel_item nonl_load_item;
static Panel_item nonl_loadgraph_item;
static Panel_item nonl_initial_item[MAXPARM];
static Panel_item nonl_computed_item[MAXPARM];
static Panel_item nonl_tol_item;
static Panel_item nonl_info_item;
static Panel_item nonl_nparm_item;
static void do_nonl_proc();

void create_nonl_frame()
{
    extern Frame main_frame;
    int i;

    if (nonl_frame) {
	xv_set(nonl_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    nonl_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Non-linear curve fitting",
				   NULL);
    nonl_panel = (Panel) xv_create(nonl_frame, PANEL,
				   XV_HELP_DATA, "xvgr:nonl_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel3(nonl_panel, nonl_set_item);
    xv_set(nonl_set_item, PANEL_VALUE_X, xv_col(nonl_panel, 20), NULL);
    nonl_load_item = (Panel_item) xv_create(nonl_panel, PANEL_CYCLE,
					    XV_HELP_DATA, "xvgr:nonl_load",
					    PANEL_LABEL_STRING, "Load:",
					    PANEL_CHOICE_STRINGS,
					    "Fitted values",
					    "Residuals",
					    "None",
					    NULL,
				      PANEL_VALUE_X, xv_col(nonl_panel, 20),
					    NULL);
    nonl_loadgraph_item = (Panel_item) xv_create(nonl_panel, PANEL_CHOICE_STACK,
				    XV_HELP_DATA, "xvgr:nonl_resulttograph",
					    PANEL_LABEL_STRING, "To graph:",
						 PANEL_CHOICE_STRINGS,
						 "Current",
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						 NULL,
				      PANEL_VALUE_X, xv_col(nonl_panel, 20),
						 NULL);
    nonl_formula_item = (Panel_item) xv_create(nonl_panel, PANEL_TEXT,
					  XV_HELP_DATA, "xvgr:nonl_formula",
					    PANEL_LABEL_STRING, "Function:",
					     PANEL_VALUE_DISPLAY_LENGTH, 35,
					       PANEL_VALUE, "y = ",
				      PANEL_VALUE_X, xv_col(nonl_panel, 15),
					       0);
    nonl_nparm_item = (Panel_item) xv_create(nonl_panel, PANEL_NUMERIC_TEXT,
					     XV_HELP_DATA, "xvgr:nonl_nparm",
				     PANEL_LABEL_STRING, "# of parameters:",
					     PANEL_VALUE_DISPLAY_LENGTH, 4,
					     PANEL_MIN_VALUE, 1,
					     PANEL_MAX_VALUE, 10,
				      PANEL_VALUE_X, xv_col(nonl_panel, 20),
					     NULL);
    nonl_tol_item = (Panel_item) xv_create(nonl_panel, PANEL_TEXT,
					   XV_HELP_DATA, "xvgr:nonl_tol",
					   PANEL_LABEL_STRING, "Tolerance:",
					   PANEL_VALUE_DISPLAY_LENGTH, 20,
					   PANEL_VALUE, "1e-7",
				      PANEL_VALUE_X, xv_col(nonl_panel, 20),
					   0);

    (void) xv_create(nonl_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Initial guess",
		     XV_X, xv_col(nonl_panel, 5),
		     XV_Y, xv_row(nonl_panel, 7),
		     NULL);
    (void) xv_create(nonl_panel, PANEL_MESSAGE,
		     PANEL_LABEL_STRING, "Computed values",
		     XV_X, xv_col(nonl_panel, 25),
		     XV_Y, xv_row(nonl_panel, 7),
		     NULL);

    for (i = 0; i < MAXPARM; i++) {
	sprintf(buf, "A%1d: ", i);
	nonl_initial_item[i] = (Panel_item) xv_create(nonl_panel, PANEL_TEXT,
					 XV_HELP_DATA, "xvgr:nonl_initparm",
						    PANEL_LABEL_STRING, buf,
					     PANEL_VALUE_DISPLAY_LENGTH, 15,
						      PANEL_VALUE, "0.0",
						XV_X, xv_col(nonl_panel, 1),
					    XV_Y, xv_row(nonl_panel, 8 + i),
						      0);
    }
    for (i = 0; i < MAXPARM; i++) {
	nonl_computed_item[i] = (Panel_item) xv_create(nonl_panel, PANEL_TEXT,
					 XV_HELP_DATA, "xvgr:nonl_compparm",
					     PANEL_VALUE_DISPLAY_LENGTH, 15,
					       XV_X, xv_col(nonl_panel, 25),
					    XV_Y, xv_row(nonl_panel, 8 + i),
						       0);
    }
    nonl_info_item = (Panel_item) xv_create(nonl_panel, PANEL_MESSAGE,
					    PANEL_LABEL_STRING, "Info: None",
					    XV_X, xv_col(nonl_panel, 1),
					    XV_Y, xv_row(nonl_panel, 19),
					    NULL);
    (void) xv_create(nonl_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:nonl_apply",
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_nonl_proc,
		     XV_X, xv_col(nonl_panel, 5),
		     XV_Y, xv_row(nonl_panel, 20),
		     0);
/*
    (void) xv_create(nonl_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:nonl_step",
	      PANEL_LABEL_STRING, "Step",
	      PANEL_NOTIFY_PROC, do_nonl_proc,
	      XV_X, xv_col(nonl_panel, 5),
	      XV_Y, xv_row(nonl_panel, 5),
	      0);
*/
    (void) xv_create(nonl_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:nonl_eval",
		     PANEL_LABEL_STRING, "Eval...",
		     PANEL_NOTIFY_PROC, create_nonleval_frame,
		     XV_X, xv_col(nonl_panel, 15),
		     XV_Y, xv_row(nonl_panel, 20),
		     0);
    (void) xv_create(nonl_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:nonl_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, nonl_frame,
		     XV_X, xv_col(nonl_panel, 25),
		     XV_Y, xv_row(nonl_panel, 20),
		     NULL);
    window_fit(nonl_panel);
    window_fit(nonl_frame);
    xv_set(nonl_frame, WIN_SHOW, TRUE, NULL);
}

void update_nonl()
{
}

/* ARGSUSED */
static void do_nonl_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i, setno, loadto, graphto, npar, info;
    double tol = 1e-7, a[MAXPARM];
    char fstr[256];

    setno = (int) xv_get(nonl_set_item, PANEL_VALUE);
    loadto = (int) xv_get(nonl_load_item, PANEL_VALUE);
    graphto = (int) xv_get(nonl_loadgraph_item, PANEL_VALUE) - 1;
    if (graphto < 0) {
	graphto = cg;
    }
    npar = (int) xv_get(nonl_nparm_item, PANEL_VALUE);
    strcpy(fstr, (char *) xv_get(nonl_formula_item, PANEL_VALUE));
    for (i = 0; i < MAXPARM; i++) {
	a[i] = 0.0;
	strcpy(buf, (char *) xv_get(nonl_initial_item[i], PANEL_VALUE));
	sscanf(buf, "%lf", &a[i]);
    }
    lmfit(fstr, getsetlength(cg, setno), getx(cg, setno), gety(cg, setno), npar, a, tol, &info);
    for (i = 0; i < MAXPARM; i++) {
	sprintf(buf, "%lf", a[i]);
	xv_set(nonl_computed_item[i], PANEL_VALUE, buf, NULL);
	nonl_parms[i] = a[i];
    }
    if (info > 0 && info < 4) {
	switch (loadto) {
	case 0:
	    do_compute(setno, 1, graphto, fstr);
	    break;
	case 1:
	    do_compute(setno, 1, graphto, fstr);
	    break;
	case 2:
	    break;
	}
    }
    if (info >= 0 && info <= 7) {
	xv_set(nonl_info_item, PANEL_LABEL_STRING, info_strings[info], NULL);
    }
}

void create_nonleval_frame()
{
}
