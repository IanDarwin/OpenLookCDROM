/* $Id: compwin.c,v 1.16 92/07/19 08:13:49 pturner Exp Locker: pturner $
 *
 * transformations, curve fitting, etc.
 *
 * formerly, this was all one big popup, now it is several.
 * All are created as needed
 *
 * TODO change to style of setops for popups
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

void drawgraph();		/* calls plotone(), called any time the graph
				 * needs to be re-drawn */

void create_eval_frame();
void create_load_frame();
void create_histo_frame();
void create_fourier_frame();
void create_run_frame();
void create_reg_frame();
void create_diff_frame();
void create_int_frame();
void create_xcor_frame();
void create_spline_frame();
void create_samp_frame();
void create_digf_frame();
void create_lconv_frame();
void create_leval_frame();

/*
 * generic quit proc for all of the frames defined below
 */
#define FRAME_KEY        101

/* ARGSUSED */
void generic_done_proc(item, event)
    Panel_item item;
    Event *event;
{
    Frame frame = (Frame) xv_get(item, XV_KEY_DATA, FRAME_KEY);

    xv_set(frame, WIN_SHOW, FALSE, 0);
}

static Frame eval_frame = (Frame) 0;
static Panel eval_panel;
static Panel_item compute_formula_item;
static Panel_item compute_set_item;
static Panel_item compute_load_item;
static Panel_item compute_loadgraph_item;
static void do_compute_proc();

/*
 * allow a carriage return to activate formula
 * interpreter
 */
static Panel_setting eotproc_compose(item, event)
    Panel_item item;
    Event *event;
{
    switch (event_id(event)) {
    case '\r':
    case '\n':
	do_compute_proc(item, event);
	return PANEL_NONE;
    default:
	return (panel_text_notify(item, event));
    }
}

void create_eval_frame()
{
    extern Frame main_frame;

    if (eval_frame) {
	xv_set(eval_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    eval_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Evaluate",
				   NULL);
    eval_panel = (Panel) xv_create(eval_frame, PANEL,
				   XV_HELP_DATA, "xvgr:eval_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel3(eval_panel, compute_set_item);
    xv_set(compute_set_item, PANEL_VALUE_X, xv_col(eval_panel, 15), NULL);
    compute_load_item = xv_create(eval_panel, PANEL_CYCLE,
				  XV_HELP_DATA, "xvgr:eval_resulttoset",
				  PANEL_LABEL_STRING, "Result To:",
				  PANEL_CHOICE_STRINGS,
				  "Same set",
				  "New set", NULL,
				  PANEL_VALUE_X, xv_col(eval_panel, 15),
				  NULL);
    compute_loadgraph_item = (Panel_item) xv_create(eval_panel, PANEL_CHOICE_STACK,
				    XV_HELP_DATA, "xvgr:eval_resulttograph",
					    PANEL_LABEL_STRING, "In graph:",
						    PANEL_CHOICE_STRINGS,
						    "Current",
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						    NULL,
				      PANEL_VALUE_X, xv_col(eval_panel, 15),
						    NULL);

    compute_formula_item = xv_create(eval_panel, PANEL_TEXT,
				     XV_HELP_DATA, "xvgr:eval_formula",
				     PANEL_LABEL_STRING, "Formula:",
				     PANEL_NOTIFY_STRING, "\n\r",
				     PANEL_NOTIFY_PROC, eotproc_compose,
				     PANEL_VALUE_DISPLAY_LENGTH, 40,
				     XV_X, xv_col(eval_panel, 1),
				     XV_Y, xv_row(eval_panel, 4),
				     0);
    xv_create(eval_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:eval_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_compute_proc,
	      XV_X, xv_col(eval_panel, 5),
	      XV_Y, xv_row(eval_panel, 5),
	      0);
    (void) xv_create(eval_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:eval_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, eval_frame,
		     XV_X, xv_col(eval_panel, 15),
		     XV_Y, xv_row(eval_panel, 5),
		     NULL);
    window_fit(eval_panel);
    window_fit(eval_frame);
    xv_set(eval_frame, WIN_SHOW, TRUE, NULL);
}

/* load a set */
static Frame load_frame = (Frame) 0;
static Panel load_panel;
static Panel_item load_set_item;
static Panel_item load_start_item;
static Panel_item load_step_item;
static Panel_item load_to_item;

void create_load_frame()
{
    extern Frame main_frame;
    static void do_load_proc();

    if (load_frame) {
	xv_set(load_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    load_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Load",
				   NULL);
    load_panel = (Panel) xv_create(load_frame, PANEL,
				   XV_HELP_DATA, "xvgr:load_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel3(load_panel, load_set_item);
    xv_set(load_set_item, PANEL_VALUE_X, xv_col(load_panel, 14), NULL);
    load_to_item = xv_create(load_panel, PANEL_CYCLE,
			     XV_HELP_DATA, "xvgr:load_to",
			     PANEL_LABEL_STRING, "To:",
			     PANEL_CHOICE_STRINGS,
			     "X",
			     "Y",
			     "Scratch array A",
			     "Scratch array B",
			     "Scratch array C",
			     "Scratch array D", NULL,
			     PANEL_VALUE_X, xv_col(load_panel, 14),
			     NULL);
    load_start_item = xv_create(load_panel, PANEL_TEXT,
				XV_HELP_DATA, "xvgr:load_start",
				PANEL_LAYOUT, PANEL_HORIZONTAL,
				PANEL_LABEL_STRING, "Start:",
				PANEL_VALUE_DISPLAY_LENGTH, 15,
				PANEL_VALUE_X, xv_col(load_panel, 14),
				NULL);
    load_step_item = xv_create(load_panel, PANEL_TEXT,
			       XV_HELP_DATA, "xvgr:load_step",
			       PANEL_LAYOUT, PANEL_HORIZONTAL,
			       PANEL_LABEL_STRING, "Step:",
			       PANEL_VALUE_DISPLAY_LENGTH, 15,
			       PANEL_VALUE_X, xv_col(load_panel, 14),
			       NULL);
    xv_create(load_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:load_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_load_proc,
	      XV_X, xv_col(load_panel, 5),
	      XV_Y, xv_row(load_panel, 5),
	      NULL);

    (void) xv_create(load_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:load_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, load_frame,
		     XV_X, xv_col(load_panel, 15),
		     XV_Y, xv_row(load_panel, 5),
		     NULL);

    window_fit(load_panel);
    window_fit(load_frame);
    xv_set(load_frame, WIN_SHOW, TRUE, NULL);
}

/* histograms */
static Frame histo_frame = (Frame) 0;
static Panel histo_panel;
static Panel_item histo_binw_item;
static Panel_item histo_hxmin_item;
static Panel_item histo_hxmax_item;
static Panel_item histo_type_item;
static Panel_item toggle_set_histo_item;

void create_histo_frame()
{
    extern Frame main_frame;
    static void do_histo_proc();

    if (histo_frame) {
	xv_set(histo_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    histo_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "Histogram",
				    NULL);
    histo_panel = (Panel) xv_create(histo_frame, PANEL,
				    XV_HELP_DATA, "xvgr:histo_panel",
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    define_select_set_panel2(histo_panel, toggle_set_histo_item, "Select set:");
    xv_set(toggle_set_histo_item, PANEL_VALUE_X, xv_col(histo_panel, 15), NULL);
    histo_binw_item = xv_create(histo_panel, PANEL_TEXT,
				XV_HELP_DATA, "xvgr:histo_binwidth",
				PANEL_LABEL_STRING, "Bin width:",
				PANEL_VALUE_DISPLAY_LENGTH, 15,
				PANEL_VALUE_X, xv_col(histo_panel, 15),
				NULL);
    histo_hxmin_item = xv_create(histo_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:histo_minx",
				 PANEL_LABEL_STRING, "Minimum X:",
				 PANEL_VALUE_DISPLAY_LENGTH, 5,
				 PANEL_VALUE_X, xv_col(histo_panel, 15),
				 NULL);
    histo_hxmax_item = xv_create(histo_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:histo_maxx",
				 PANEL_LABEL_STRING, "Maximum X:",
				 PANEL_VALUE_DISPLAY_LENGTH, 15,
				 PANEL_VALUE_X, xv_col(histo_panel, 15),
				 NULL);
    histo_type_item = xv_create(histo_panel, PANEL_CYCLE,
				XV_HELP_DATA, "xvgr:histo_load",
				PANEL_LABEL_STRING, "Load:",
				PANEL_CHOICE_STRINGS,
				"Histogram",
				"Cumulative Histo",
				NULL,
				PANEL_VALUE_X, xv_col(histo_panel, 15),
				NULL);
    xv_create(histo_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:histo_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_histo_proc,
	      XV_X, xv_col(histo_panel, 5),
	      XV_Y, xv_row(histo_panel, 5),
	      NULL);

    (void) xv_create(histo_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:histo_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, histo_frame,
		     XV_X, xv_col(histo_panel, 15),
		     XV_Y, xv_row(histo_panel, 5),
		     NULL);
    window_fit(histo_panel);
    window_fit(histo_frame);
    xv_set(histo_frame, WIN_SHOW, TRUE, NULL);
}

/* DFTs */
static Frame fourier_frame = (Frame) 0;
static Panel fourier_panel;
static Panel_item toggle_set_fourier_item;
static Panel_item toggle_loadx_fourier_item;
static Panel_item toggle_window_fourier_item;
static Panel_item toggle_load_fourier_item;
static Panel_item toggle_inv_fourier_item;
static Panel_item toggle_type_fourier_item;

void create_fourier_frame()
{
    extern Frame main_frame;
    static void do_fourier_proc();
    static void do_fft_proc();
    static void do_window_proc();

    if (fourier_frame) {
	xv_set(fourier_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    fourier_frame = (Frame) xv_create(main_frame, FRAME,
				      FRAME_LABEL, "Fourier",
				      NULL);
    fourier_panel = (Panel) xv_create(fourier_frame, PANEL,
				      XV_HELP_DATA, "xvgr:fourier_panel",
				      PANEL_LAYOUT, PANEL_VERTICAL,
				      NULL);
    define_select_set_panel2(fourier_panel, toggle_set_fourier_item, "Select set:");
    xv_set(toggle_set_fourier_item, PANEL_VALUE_X, xv_col(fourier_panel, 17), NULL);
    toggle_window_fourier_item = xv_create(fourier_panel, PANEL_CYCLE,
					 XV_HELP_DATA, "xvgr:fourier_window",
					 PANEL_LABEL_STRING, "Data window:",
					 PANEL_CHOICE_STRINGS,
					 "None (Rectangular)",
					 "Triangular",
					 "Hanning",
					 "Welch",
					 "Hamming",
					 "Blackman",
					 "Parzen",
					 NULL,
				   PANEL_VALUE_X, xv_col(fourier_panel, 17),
					 NULL);
    toggle_load_fourier_item = xv_create(fourier_panel, PANEL_CYCLE,
					 XV_HELP_DATA, "xvgr:fourier_load",
					 PANEL_LABEL_STRING, "Load:",
					 PANEL_CHOICE_STRINGS,
					 "Magnitude",
					 "Phase",
					 "Coefficients",
					 NULL,
				   PANEL_VALUE_X, xv_col(fourier_panel, 17),
					 NULL);
    toggle_loadx_fourier_item = xv_create(fourier_panel, PANEL_CYCLE,
					  XV_HELP_DATA, "xvgr:fourier_xeq",
					  PANEL_LABEL_STRING, "X = ",
					  PANEL_CHOICE_STRINGS,
					  "Index",
					  "Frequency",
					  "Period",
					  NULL,
				   PANEL_VALUE_X, xv_col(fourier_panel, 17),
					  NULL);
    toggle_inv_fourier_item = xv_create(fourier_panel, PANEL_CYCLE,
					XV_HELP_DATA, "xvgr:fourier_perform",
					PANEL_LABEL_STRING, "Perform:",
					PANEL_CHOICE_STRINGS,
					"Transform",
					"Inverse transform",
					NULL,
				   PANEL_VALUE_X, xv_col(fourier_panel, 17),
					NULL);
    toggle_type_fourier_item = xv_create(fourier_panel, PANEL_CYCLE,
				      XV_HELP_DATA, "xvgr:fourier_datatype",
					 PANEL_LABEL_STRING, "Type:",
					 PANEL_CHOICE_STRINGS,
					 "Real",
					 "Complex",
					 NULL,
				   PANEL_VALUE_X, xv_col(fourier_panel, 17),
					 NULL);

    xv_create(fourier_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:fourier_applydft",
	      PANEL_LABEL_STRING, "Apply DFT",
	      PANEL_NOTIFY_PROC, do_fourier_proc,
	      XV_X, xv_col(fourier_panel, 4),
	      XV_Y, xv_row(fourier_panel, 7),
	      0);
    xv_create(fourier_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:fourier_applyfft",
	      PANEL_LABEL_STRING, "Apply FFT",
	      PANEL_NOTIFY_PROC, do_fft_proc,
	      XV_X, xv_col(fourier_panel, 16),
	      XV_Y, xv_row(fourier_panel, 7),
	      0);
    xv_create(fourier_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:fourier_applywindow",
	      PANEL_LABEL_STRING, "Window only",
	      PANEL_NOTIFY_PROC, do_window_proc,
	      XV_X, xv_col(fourier_panel, 27),
	      XV_Y, xv_row(fourier_panel, 7),
	      0);
    xv_create(fourier_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:fourier_done",
	      PANEL_LABEL_STRING, "Done",
	      PANEL_NOTIFY_PROC, generic_done_proc,
	      XV_KEY_DATA, FRAME_KEY, fourier_frame,
	      XV_X, xv_col(fourier_panel, 40),
	      XV_Y, xv_row(fourier_panel, 7),
	      NULL);

    window_fit(fourier_panel);
    window_fit(fourier_frame);
    xv_set(fourier_frame, WIN_SHOW, TRUE, NULL);
}

/* running averages */
static Frame run_frame = (Frame) 0;
static Panel run_panel;
static Panel_item toggle_ravglen_item;
static Panel_item toggle_run_item;
static Panel_item toggle_set_runavg_item;

void create_run_frame()
{
    extern Frame main_frame;
    static void do_runavg_proc();

    if (run_frame) {
	xv_set(run_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    run_frame = (Frame) xv_create(main_frame, FRAME,
				  FRAME_LABEL, "Running etc.",
				  NULL);
    run_panel = (Panel) xv_create(run_frame, PANEL,
				  XV_HELP_DATA, "xvgr:run_panel",
				  PANEL_LAYOUT, PANEL_VERTICAL,
				  NULL);
    define_select_set_panel2(run_panel, toggle_set_runavg_item, "Select set:");
    xv_set(toggle_set_runavg_item, PANEL_VALUE_X, xv_col(run_panel, 15), NULL);
    toggle_run_item = xv_create(run_panel, PANEL_CYCLE,
				XV_HELP_DATA, "xvgr:run_toggle",
				PANEL_CHOICE_STRINGS,
				"Average",
				"Median",
				"Minimum",
				"Maximum",
				"Std. dev.", NULL,
				PANEL_VALUE_X, xv_col(run_panel, 15),
				NULL);
    toggle_ravglen_item = xv_create(run_panel, PANEL_TEXT,
				    XV_HELP_DATA, "xvgr:run_length",
				    PANEL_LABEL_STRING, "Length:",
				    PANEL_VALUE_DISPLAY_LENGTH, 10,
				    PANEL_VALUE_X, xv_col(run_panel, 15),
				    NULL);

    (void) xv_create(run_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:run_apply",
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_runavg_proc,
		     XV_X, xv_col(run_panel, 5),
		     XV_Y, xv_row(run_panel, 4),
		     NULL);
    (void) xv_create(run_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:run_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, run_frame,
		     XV_X, xv_col(run_panel, 15),
		     XV_Y, xv_row(run_panel, 4),
		     NULL);
    window_fit(run_panel);
    window_fit(run_frame);
    xv_set(run_frame, WIN_SHOW, TRUE, NULL);
}

/* items for linear regression */
static Frame reg_frame = (Frame) 0;
static Panel reg_panel;
static Panel_item toggle_degree_item;	/* degree of fit */
static Panel_item toggle_resid_item;	/* load residuals, fitted values or
					 * nothing */
static Panel_item toggle_set_regress_item;	/* which set to use */

void create_reg_frame()
{
    extern Frame main_frame;
    static void do_regress_proc();

    if (reg_frame) {
	xv_set(reg_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    reg_frame = (Frame) xv_create(main_frame, FRAME,
				  FRAME_LABEL, "Regression",
				  NULL);
    reg_panel = (Panel) xv_create(reg_frame, PANEL,
				  XV_HELP_DATA, "xvgr:reg_panel",
				  PANEL_LAYOUT, PANEL_VERTICAL,
				  NULL);
    define_select_set_panel2(reg_panel, toggle_set_regress_item, "Select set:");
    xv_set(toggle_set_regress_item, PANEL_VALUE_X, xv_col(reg_panel, 15), NULL);
    toggle_degree_item = xv_create(reg_panel, PANEL_CYCLE,
				   XV_HELP_DATA, "xvgr:reg_degree",
				   PANEL_LABEL_STRING, "Degree:",
				   PANEL_CHOICE_STRINGS,
				   "Linear",
				   "Quadratic",
				   "Cubic",
				   "4th degree",
				   "5th degree", 
				   "6th degree", 
				   "7th degree", 
				   "8th degree", 
				   "9th degree", 
				   "10th degree", 
				    NULL,
				   PANEL_VALUE_X, xv_col(reg_panel, 15),
				   NULL);
    toggle_resid_item = xv_create(reg_panel, PANEL_CYCLE,
				  XV_HELP_DATA, "xvgr:reg_load",
				  PANEL_LABEL_STRING, "Load:",
				  PANEL_CHOICE_STRINGS,
				  "Fitted values",
				  "Residuals", NULL,
				  PANEL_VALUE_X, xv_col(reg_panel, 15),
				  NULL);

    (void) xv_create(reg_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:reg_apply",
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_regress_proc,
		     XV_X, xv_col(reg_panel, 5),
		     XV_Y, xv_row(reg_panel, 4),
		     NULL);

    (void) xv_create(reg_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:reg_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, reg_frame,
		     XV_X, xv_col(reg_panel, 15),
		     XV_Y, xv_row(reg_panel, 4),
		     NULL);
    window_fit(reg_panel);
    window_fit(reg_frame);
    xv_set(reg_frame, WIN_SHOW, TRUE, NULL);
}

/* finite differencing */
static Frame diff_frame = (Frame) 0;
static Panel diff_panel;
static Panel_item toggle_set_differ_item;
static Panel_item toggle_differ_type_item;

void create_diff_frame()
{
    extern Frame main_frame;
    static void do_differ_proc();

    if (diff_frame) {
	xv_set(diff_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    diff_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Differentiate",
				   NULL);
    diff_panel = (Panel) xv_create(diff_frame, PANEL,
				   XV_HELP_DATA, "xvgr:diff_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel2(diff_panel, toggle_set_differ_item, "Select set:");
    xv_set(toggle_set_differ_item, PANEL_VALUE_X, xv_col(diff_panel, 15), NULL);
    toggle_differ_type_item = xv_create(diff_panel, PANEL_CYCLE,
					XV_HELP_DATA, "xvgr:diff_method",
					PANEL_LABEL_STRING, "Method:",
					PANEL_CHOICE_STRINGS,
					"Forward difference",
					"Backward difference",
					"Centered difference",
					NULL,
				      PANEL_VALUE_X, xv_col(diff_panel, 15),
					NULL);

    xv_create(diff_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:diff_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_differ_proc,
	      XV_X, xv_col(diff_panel, 5),
	      XV_Y, xv_row(diff_panel, 3),
	      NULL);

    (void) xv_create(diff_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:diff_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, diff_frame,
		     XV_X, xv_col(diff_panel, 15),
		     XV_Y, xv_row(diff_panel, 3),
		     NULL);
    window_fit(diff_panel);
    window_fit(diff_frame);
    xv_set(diff_frame, WIN_SHOW, TRUE, NULL);
}

/* numerical integration */
static Frame int_frame = (Frame) 0;
static Panel int_panel;
static Panel_item toggle_set_int_item;
static Panel_item toggle_int_type_item;
static Panel_item int_sum_item;

void create_int_frame()
{
    extern Frame main_frame;
    static void do_int_proc();

    if (int_frame) {
	xv_set(int_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    int_frame = (Frame) xv_create(main_frame, FRAME,
				  FRAME_LABEL, "Integration",
				  NULL);
    int_panel = (Panel) xv_create(int_frame, PANEL,
				  XV_HELP_DATA, "xvgr:int_panel",
				  PANEL_LAYOUT, PANEL_VERTICAL,
				  NULL);
    define_select_set_panel2(int_panel, toggle_set_int_item, "Select set:");
    xv_set(toggle_set_int_item, PANEL_VALUE_X, xv_col(int_panel, 15), NULL);
    toggle_int_type_item = xv_create(int_panel, PANEL_CYCLE,
				     XV_HELP_DATA, "xvgr:int_load",
				     PANEL_LABEL_STRING, "Load:",
				     PANEL_CHOICE_STRINGS,
				     "Cumulative sum",
				     "Sum only",
				     NULL,
				     PANEL_VALUE_X, xv_col(int_panel, 15),
				     NULL);
    int_sum_item = xv_create(int_panel, PANEL_TEXT,
			     XV_HELP_DATA, "xvgr:int_sum",
			     PANEL_LABEL_STRING, "Sum:",
			     PANEL_VALUE_DISPLAY_LENGTH, 15,
			     PANEL_VALUE_X, xv_col(int_panel, 15),
			     NULL);

    xv_create(int_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:int_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_int_proc,
	      XV_X, xv_col(int_panel, 5),
	      XV_Y, xv_row(int_panel, 4),
	      NULL);

    (void) xv_create(int_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     XV_HELP_DATA, "xvgr:int_done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, int_frame,
		     XV_X, xv_col(int_panel, 15),
		     XV_Y, xv_row(int_panel, 4),
		     NULL);
    window_fit(int_panel);
    window_fit(int_frame);
    xv_set(int_frame, WIN_SHOW, TRUE, NULL);
}

/* cross correlation */
static Frame xcor_frame = (Frame) 0;
static Panel xcor_panel;
static Panel_item toggle_set_xcor1_item;
static Panel_item toggle_set_xcor2_item;
static Panel_item toggle_xcor_type_item;
static Panel_item xcor_lag_item;

void create_xcor_frame()
{
    extern Frame main_frame;
    static void do_xcor_proc();

    if (xcor_frame) {
	xv_set(xcor_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    xcor_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "X correlation",
				   NULL);
    xcor_panel = (Panel) xv_create(xcor_frame, PANEL,
				   XV_HELP_DATA, "xvgr:xcor_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel2(xcor_panel, toggle_set_xcor1_item, "Select set:");
    xv_set(toggle_set_xcor1_item, PANEL_VALUE_X, xv_col(xcor_panel, 15), NULL);
    define_select_set_panel2(xcor_panel, toggle_set_xcor2_item, "Select set:");
    xv_set(toggle_set_xcor2_item, PANEL_VALUE_X, xv_col(xcor_panel, 15), NULL);
    toggle_xcor_type_item = xv_create(xcor_panel, PANEL_CYCLE,
				      XV_HELP_DATA, "xvgr:xcor_bias",
				      PANEL_LABEL_STRING, "Bias:",
				      PANEL_CHOICE_STRINGS,
				      "Biased estimate",
				      "Unbiased estimate",
				      NULL,
				      PANEL_VALUE_X, xv_col(xcor_panel, 15),
				      NULL);
    xcor_lag_item = xv_create(xcor_panel, PANEL_TEXT,
			      XV_HELP_DATA, "xvgr:xcor_lag",
			      PANEL_LABEL_STRING, "Lag:",
			      PANEL_VALUE_DISPLAY_LENGTH, 15,
			      PANEL_VALUE_X, xv_col(xcor_panel, 15),
			      NULL);
    xv_create(xcor_panel, PANEL_BUTTON,
	      XV_HELP_DATA, "xvgr:xcor_apply",
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_xcor_proc,
	      XV_X, xv_col(xcor_panel, 5),
	      XV_Y, xv_row(xcor_panel, 5),
	      NULL);

    (void) xv_create(xcor_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, xcor_frame,
		     XV_X, xv_col(xcor_panel, 15),
		     XV_Y, xv_row(xcor_panel, 5),
		     NULL);
    window_fit(xcor_panel);
    window_fit(xcor_frame);
    xv_set(xcor_frame, WIN_SHOW, TRUE, NULL);
}

/* splines */
static Frame spline_frame = (Frame) 0;
static Panel spline_panel;
Panel_item toggle_spline_item;
Panel_item spline_start_item;
Panel_item spline_stop_item;
Panel_item spline_step_item;

void create_spline_frame()
{
    extern Frame main_frame;
    static void do_spline_proc();

    if (spline_frame) {
	xv_set(spline_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    spline_frame = (Frame) xv_create(main_frame, FRAME,
				     FRAME_LABEL, "Splines",
				     NULL);
    spline_panel = (Panel) xv_create(spline_frame, PANEL,
				     XV_HELP_DATA, "xvgr:spline_panel",
				     PANEL_LAYOUT, PANEL_VERTICAL,
				     NULL);
    define_select_set_panel2(spline_panel, toggle_spline_item, "Select set:");
    xv_set(toggle_spline_item, PANEL_VALUE_X, xv_col(spline_panel, 15), NULL);
    spline_start_item = xv_create(spline_panel, PANEL_TEXT,
				  XV_HELP_DATA, "xvgr:spline_start",
				  PANEL_LABEL_STRING, "Start:",
				  PANEL_VALUE_DISPLAY_LENGTH, 15,
				  PANEL_VALUE_X, xv_col(spline_panel, 15),
				  NULL);
    spline_stop_item = xv_create(spline_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:spline_stop",
				 PANEL_LABEL_STRING, "Stop:",
				 PANEL_VALUE_DISPLAY_LENGTH, 15,
				 PANEL_VALUE_X, xv_col(spline_panel, 15),
				 NULL);
    spline_step_item = xv_create(spline_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:spline_nsteps",
				 PANEL_LABEL_STRING, "N steps:",
				 PANEL_VALUE_DISPLAY_LENGTH, 15,
				 PANEL_VALUE_X, xv_col(spline_panel, 15),
				 NULL);

    (void) xv_create(spline_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:spline_apply",
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_spline_proc,
		     XV_X, xv_col(spline_panel, 5),
		     XV_Y, xv_row(spline_panel, 5),
		     NULL);

    (void) xv_create(spline_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:spline_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, spline_frame,
		     XV_X, xv_col(spline_panel, 15),
		     XV_Y, xv_row(spline_panel, 5),
		     NULL);
    window_fit(spline_panel);
    window_fit(spline_frame);
    xv_set(spline_frame, WIN_SHOW, TRUE, NULL);
}

/* sample a set */
static Frame samp_frame = (Frame) 0;
static Panel samp_panel;
static Panel_item toggle_set_sample_item;
static Panel_item sample_start_item;
static Panel_item sample_step_item;
static Panel_item sample_type_item;
static Panel_item sample_expr_item;

/*
 * service the type cycle for sample
 */
static void do_sample_type_proc()
{
    int itype;

    itype = (int) xv_get(sample_type_item, PANEL_VALUE);
    xv_set(sample_start_item, XV_SHOW, !itype, 0);
    xv_set(sample_step_item, XV_SHOW, !itype, 0);
    xv_set(sample_expr_item, XV_SHOW, itype, 0);
    panel_paint(sample_start_item, PANEL_CLEAR);
    panel_paint(sample_step_item, PANEL_CLEAR);
    panel_paint(sample_expr_item, PANEL_CLEAR);
}

void create_samp_frame()
{
    extern Frame main_frame;
    static void do_sample_proc();
    static void do_sample_type_proc();

    if (samp_frame) {
	xv_set(samp_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    samp_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Sample",
				   NULL);
    samp_panel = (Panel) xv_create(samp_frame, PANEL,
				   XV_HELP_DATA, "xvgr:samp_panel",
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel2(samp_panel, toggle_set_sample_item, "Select set:");
    xv_set(toggle_set_sample_item, PANEL_VALUE_X, xv_col(samp_panel, 13), NULL);
    sample_start_item = xv_create(samp_panel, PANEL_TEXT,
				  XV_HELP_DATA, "xvgr:samp_start",
				  PANEL_LABEL_STRING, "Start:",
				  PANEL_VALUE_DISPLAY_LENGTH, 15,
				  PANEL_VALUE_X, xv_col(samp_panel, 13),
				  NULL);
    sample_step_item = xv_create(samp_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:samp_step",
				 PANEL_LABEL_STRING, "Step:",
				 PANEL_VALUE_DISPLAY_LENGTH, 15,
				 PANEL_VALUE_X, xv_col(samp_panel, 13),
				 NULL);
    sample_expr_item = xv_create(samp_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:samp_expr",
				 PANEL_LABEL_STRING, "Expr:",
				 PANEL_VALUE_DISPLAY_LENGTH, 25,
				 PANEL_VALUE_X, xv_col(samp_panel, 13),
				 NULL);
    sample_type_item = xv_create(samp_panel, PANEL_CYCLE,
				 XV_HELP_DATA, "xvgr:samp_type",
				 PANEL_LABEL_STRING, "Type:",
				 PANEL_CHOICE_STRINGS,
				 "Start/step",
				 "Logical expression",
				 NULL,
				 PANEL_NOTIFY_PROC, do_sample_type_proc,
				 PANEL_VALUE_X, xv_col(samp_panel, 13),
				 NULL);
    (void) xv_create(samp_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:samp_apply",
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_sample_proc,
		     XV_X, xv_col(samp_panel, 5),
		     XV_Y, xv_row(samp_panel, 6),
		     NULL);

    (void) xv_create(samp_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:samp_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, samp_frame,
		     XV_X, xv_col(samp_panel, 15),
		     XV_Y, xv_row(samp_panel, 6),
		     NULL);
    window_fit(samp_panel);
    window_fit(samp_frame);
    do_sample_type_proc();
    xv_set(samp_frame, WIN_SHOW, TRUE, NULL);
}

/* apply a digital filter in set 2 to set 1 */
static Frame digf_frame = (Frame) 0;
static Panel digf_panel;
static Panel_item toggle_set_digf1_item;
static Panel_item toggle_set_digf2_item;

void create_digf_frame()
{
    extern Frame main_frame;
    static void do_digfilter_proc();

    if (digf_frame) {
	xv_set(digf_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    digf_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Digital filter",
				   NULL);
    digf_panel = (Panel) xv_create(digf_frame, PANEL,
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    define_select_set_panel2(digf_panel, toggle_set_digf1_item, "Select set:");
    xv_set(toggle_set_digf1_item, PANEL_VALUE_X, xv_col(digf_panel, 18), NULL);
    define_select_set_panel2(digf_panel, toggle_set_digf2_item, "Weights from set:");
    xv_set(toggle_set_digf2_item, PANEL_VALUE_X, xv_col(digf_panel, 18), NULL);

    (void) xv_create(digf_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_digfilter_proc,
		     XV_X, xv_col(digf_panel, 5),
		     XV_Y, xv_row(digf_panel, 3),
		     NULL);

    (void) xv_create(digf_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, digf_frame,
		     XV_X, xv_col(digf_panel, 15),
		     XV_Y, xv_row(digf_panel, 3),
		     NULL);
    window_fit(digf_panel);
    window_fit(digf_frame);
    xv_set(digf_frame, WIN_SHOW, TRUE, NULL);
}

/* linear convolution */
static Frame lconv_frame = (Frame) 0;
static Panel lconv_panel;
static Panel_item toggle_set_linc1_item;
static Panel_item toggle_set_linc2_item;

void create_lconv_frame()
{
    extern Frame main_frame;
    static void do_linearc_proc();

    if (lconv_frame) {
	xv_set(lconv_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    lconv_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "Linear convolution",
				    NULL);
    lconv_panel = (Panel) xv_create(lconv_frame, PANEL,
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    define_select_set_panel2(lconv_panel, toggle_set_linc1_item, "Select set:");
    xv_set(toggle_set_linc1_item, PANEL_VALUE_X, xv_col(lconv_panel, 20), NULL);
    define_select_set_panel2(lconv_panel, toggle_set_linc2_item, "Convolve with set:");
    xv_set(toggle_set_linc2_item, PANEL_VALUE_X, xv_col(lconv_panel, 20), NULL);
    (void) xv_create(lconv_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Apply",
		     PANEL_NOTIFY_PROC, do_linearc_proc,
		     XV_X, xv_col(lconv_panel, 5),
		     XV_Y, xv_row(lconv_panel, 3),
		     NULL);

    (void) xv_create(lconv_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, lconv_frame,
		     XV_X, xv_col(lconv_panel, 15),
		     XV_Y, xv_row(lconv_panel, 3),
		     NULL);
    window_fit(lconv_panel);
    window_fit(lconv_frame);
    xv_set(lconv_frame, WIN_SHOW, TRUE, NULL);
}

/* evaluate a formula - load the next set */
static Frame leval_frame = (Frame) 0;
static Panel leval_panel;
static Panel_item compute_formulax_item;
static Panel_item compute_formulay_item;
static Panel_item load_start2_item;
static Panel_item load_stop2_item;
static Panel_item load_npts_item;
static Panel_item load_to2_item;

void create_leval_frame()
{
    extern Frame main_frame;
    static void do_compute_proc2();

    if (leval_frame) {
	xv_set(leval_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    leval_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "Load & eval",
				    NULL);
    leval_panel = (Panel) xv_create(leval_frame, PANEL,
				    XV_HELP_DATA, "xvgr:leval_panel",
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    compute_formulax_item = xv_create(leval_panel, PANEL_TEXT,
				      XV_HELP_DATA, "xvgr:leval_fx",
				      PANEL_LABEL_STRING, "Fx: ",
				      PANEL_VALUE_DISPLAY_LENGTH, 25,
				      PANEL_VALUE_X, xv_col(leval_panel, 15),
				      NULL);
    compute_formulay_item = xv_create(leval_panel, PANEL_TEXT,
				      XV_HELP_DATA, "xvgr:leval_fy",
				      PANEL_LABEL_STRING, "Fy: ",
				      PANEL_VALUE_DISPLAY_LENGTH, 25,
				      PANEL_VALUE_X, xv_col(leval_panel, 15),
				      NULL);
    load_to2_item = xv_create(leval_panel, PANEL_CYCLE,
			      XV_HELP_DATA, "xvgr:leval_to",
			      PANEL_LABEL_STRING, "To:",
			      PANEL_CHOICE_STRINGS,
			      "X",
			      "Y",
			      "Scratch array A",
			      "Scratch array B",
			      "Scratch array C",
			      "Scratch array D", NULL,
			      PANEL_VALUE_X, xv_col(leval_panel, 15),
			      NULL);
    load_start2_item = xv_create(leval_panel, PANEL_TEXT,
				 XV_HELP_DATA, "xvgr:leval_start",
				 PANEL_LABEL_STRING, "Start:",
				 PANEL_VALUE_DISPLAY_LENGTH, 15,
				 PANEL_VALUE_X, xv_col(leval_panel, 15),
				 NULL);
    load_stop2_item = xv_create(leval_panel, PANEL_TEXT,
				XV_HELP_DATA, "xvgr:leval_stop",
				PANEL_LABEL_STRING, "Stop:",
				PANEL_VALUE_DISPLAY_LENGTH, 15,
				PANEL_VALUE_X, xv_col(leval_panel, 15),
				NULL);
    load_npts_item = xv_create(leval_panel, PANEL_TEXT,
			       XV_HELP_DATA, "xvgr:leval_npts",
			       PANEL_LABEL_STRING, "# of pts:",
			       PANEL_VALUE_DISPLAY_LENGTH, 15,
			       PANEL_VALUE_X, xv_col(leval_panel, 15),
			       NULL);
    xv_create(leval_panel, PANEL_BUTTON,
	      PANEL_LABEL_STRING, "Apply",
	      PANEL_NOTIFY_PROC, do_compute_proc2,
	      XV_X, xv_col(leval_panel, 5),
	      XV_Y, xv_row(leval_panel, 6),
	      NULL);

    (void) xv_create(leval_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, generic_done_proc,
		     XV_KEY_DATA, FRAME_KEY, leval_frame,
		     XV_X, xv_col(leval_panel, 15),
		     XV_Y, xv_row(leval_panel, 6),
		     NULL);
    window_fit(leval_panel);
    window_fit(leval_frame);
    xv_set(leval_frame, WIN_SHOW, TRUE, NULL);
}

/*
 * evaluate a formula
 */
/* ARGSUSED */
static void do_compute_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, loadto, graphto;
    char fstr[256];

    setno = (int) xv_get(compute_set_item, PANEL_VALUE);
    if (setno == MAXPLOT) {
	setno = -1;
    }
    loadto = (int) xv_get(compute_load_item, PANEL_VALUE);
    graphto = (int) xv_get(compute_loadgraph_item, PANEL_VALUE) - 1;
    strcpy(fstr, (char *) xv_get(compute_formula_item, PANEL_VALUE));
    do_compute(setno, loadto, graphto, fstr);
}

/*
 * load a set
 */
/* ARGSUSED */
static void do_load_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, toval;
    char startstr[256], stepstr[256];

    setno = (int) xv_get(load_set_item, PANEL_VALUE);
    if (setno == MAXPLOT) {
	setno = -1;
    }
    toval = (int) xv_get(load_to_item, PANEL_VALUE) + 1;
    strcpy(stepstr, (char *) xv_get(load_step_item, PANEL_VALUE));
    strcpy(startstr, (char *) xv_get(load_start_item, PANEL_VALUE));
    do_load(setno, toval, startstr, stepstr);
}

/*
 * evaluate a formula loading the next set
 */
/* ARGSUSED */
static void do_compute_proc2(item, event)
    Panel_item item;
    Event *event;
{
    int npts, toval;
    char fstrx[256], fstry[256];
    char startstr[256], stopstr[256];

    npts = atoi((char *) xv_get(load_npts_item, PANEL_VALUE));
    strcpy(fstrx, (char *) xv_get(compute_formulax_item, PANEL_VALUE));
    strcpy(fstry, (char *) xv_get(compute_formulay_item, PANEL_VALUE));
    strcpy(startstr, (char *) xv_get(load_start2_item, PANEL_VALUE));
    strcpy(stopstr, (char *) xv_get(load_stop2_item, PANEL_VALUE));
    toval = (int) xv_get(load_to2_item, PANEL_VALUE) + 1;
    do_compute2(fstrx, fstry, startstr, stopstr, npts, toval);
}

/*
 * apply a digital filter
 */
/* ARGSUSED */
static void do_digfilter_proc(item, event)
    Panel_item item;
    Event *event;
{
    int set1, set2, digfiltset;

    set1 = (int) xv_get(toggle_set_digf1_item, PANEL_VALUE);
    set2 = (int) xv_get(toggle_set_digf2_item, PANEL_VALUE);
    do_digfilter(set1, set2);
}

/*
 * linear convolution
 */
/* ARGSUSED */
static void do_linearc_proc(item, event)
    Panel_item item;
    Event *event;
{
    int set1, set2, linearcset, i, itmp;
    double *xtmp;

    set1 = (int) xv_get(toggle_set_linc1_item, PANEL_VALUE);
    set2 = (int) xv_get(toggle_set_linc2_item, PANEL_VALUE);
    do_linearc(set1, set2);
}

/*
 * cross correlation
 */
/* ARGSUSED */
static void do_xcor_proc(item, event)
    Panel_item item;
    Event *event;
{
    int set1, set2, itype, lag;

    set1 = (int) xv_get(toggle_set_xcor1_item, PANEL_VALUE);
    set2 = (int) xv_get(toggle_set_xcor2_item, PANEL_VALUE);
    itype = (int) xv_get(toggle_xcor_type_item, PANEL_VALUE);
    lag = atoi((char *) xv_get(xcor_lag_item, PANEL_VALUE));
    do_xcor(set1, set2, itype, lag);
}

/*
 * splines
 */
/* ARGSUSED */
static void do_spline_proc(item, event)
    Panel_item item;
    Event *event;
{
    int set, n;
    char startstr[256], stopstr[256];

    set = (int) xv_get(toggle_spline_item, PANEL_VALUE);
    strcpy(startstr, (char *) xv_get(spline_start_item, PANEL_VALUE));
    strcpy(stopstr, (char *) xv_get(spline_stop_item, PANEL_VALUE));
    n = atoi((char *) xv_get(spline_step_item, PANEL_VALUE));
    do_spline(set, startstr, stopstr, n);
}

/*
 * numerical integration
 */
/* ARGSUSED */
static void do_int_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, itype;
    double sum, do_int();

    setno = (int) xv_get(toggle_set_int_item, PANEL_VALUE);
    itype = (int) xv_get(toggle_int_type_item, PANEL_VALUE);
    sum = do_int(setno, itype);
    sprintf(buf, "%lf", sum);
    xv_set(int_sum_item, PANEL_VALUE, buf, NULL);
}

/*
 * finite differences
 */
/* ARGSUSED */
static void do_differ_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, itype;

    setno = (int) xv_get(toggle_set_differ_item, PANEL_VALUE);
    itype = (int) xv_get(toggle_differ_type_item, PANEL_VALUE);
    do_differ(setno, itype);
}

/*
 * regression
 */
/* ARGSUSED */
static void do_regress_proc(item, event)
    Panel_item item;
    Event *event;
{
    int fitset, setno, ideg, iresid, i;
    double *yd, *yf;

    setno = (int) xv_get(toggle_set_regress_item, PANEL_VALUE);
    ideg = (int) xv_get(toggle_degree_item, PANEL_VALUE) + 1;
    iresid = (int) xv_get(toggle_resid_item, PANEL_VALUE);
    do_regress(setno, ideg, iresid);
    drawgraph();
}

/*
 * running averages, medians, min, max, std. deviation
 */
/* ARGSUSED */
static void do_runavg_proc(item, event)
    Panel_item item;
    Event *event;
{
    int runset, runlen, runtype, setno;

    setno = (int) xv_get(toggle_set_runavg_item, PANEL_VALUE);
    runlen = atoi((char *) xv_get(toggle_ravglen_item, PANEL_VALUE));
    runtype = (int) xv_get(toggle_run_item, PANEL_VALUE);
    do_runavg(setno, runlen, runtype);
    drawgraph();
}

/*
 * DFT
 */
/* ARGSUSED */
static void do_fourier_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, load, loadx, invflag, type, wind;

    setno = (int) xv_get(toggle_set_fourier_item, PANEL_VALUE);
    wind = (int) xv_get(toggle_window_fourier_item, PANEL_VALUE);
    load = (int) xv_get(toggle_load_fourier_item, PANEL_VALUE);
    loadx = (int) xv_get(toggle_loadx_fourier_item, PANEL_VALUE);
    invflag = (int) xv_get(toggle_inv_fourier_item, PANEL_VALUE);
    type = (int) xv_get(toggle_type_fourier_item, PANEL_VALUE);
    do_fourier(0, setno, load, loadx, invflag, type, wind);
    drawgraph();
}

/*
 * DFT by FFT
 */
/* ARGSUSED */
static void do_fft_proc(item, event)
    Panel_item item;
    Event *event;
{
    int load, loadx, invflag;
    int setno, type, wind;

    setno = (int) xv_get(toggle_set_fourier_item, PANEL_VALUE);
    wind = (int) xv_get(toggle_window_fourier_item, PANEL_VALUE);
    load = (int) xv_get(toggle_load_fourier_item, PANEL_VALUE);
    loadx = (int) xv_get(toggle_loadx_fourier_item, PANEL_VALUE);
    invflag = (int) xv_get(toggle_inv_fourier_item, PANEL_VALUE);
    type = (int) xv_get(toggle_type_fourier_item, PANEL_VALUE);
    do_fourier(1, setno, load, loadx, invflag, type, wind);
    drawgraph();
}

/*
 * Apply data window only
 */
/* ARGSUSED */
static void do_window_proc(item, event)
    Panel_item item;
    Event *event;
{
    int load, loadx, invflag;
    int setno, type, wind;

    setno = (int) xv_get(toggle_set_fourier_item, PANEL_VALUE);
    wind = (int) xv_get(toggle_window_fourier_item, PANEL_VALUE);
    type = (int) xv_get(toggle_type_fourier_item, PANEL_VALUE);
    do_window(setno, type, wind);
    drawgraph();
}

/*
 * histograms
 */
/* ARGSUSED */
static void do_histo_proc(item, event)
    Panel_item item;
    Event *event;
{
    int hset, hist_type;
    double binw, xmin, xmax;

    hset = (int) xv_get(toggle_set_histo_item, PANEL_VALUE);
    binw = atof((char *) xv_get(histo_binw_item, PANEL_VALUE));
    xmin = atof((char *) xv_get(histo_hxmin_item, PANEL_VALUE));
    xmax = atof((char *) xv_get(histo_hxmax_item, PANEL_VALUE));
    hist_type = (int) xv_get(histo_type_item, PANEL_VALUE);
    do_histo(hset, binw, xmin, xmax, hist_type);
}

/*
 * sample a set, by start/step or logical expression
 */
/* ARGSUSED */
static void do_sample_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, typeno;
    char exprstr[256];
    int startno, stepno;

    setno = (int) xv_get(toggle_set_sample_item, PANEL_VALUE);
    typeno = (int) xv_get(sample_type_item, PANEL_VALUE);
    strcpy(exprstr, (char *) xv_get(sample_expr_item, PANEL_VALUE));
    startno = atoi((char *) xv_get(sample_start_item, PANEL_VALUE));
    stepno = atoi((char *) xv_get(sample_step_item, PANEL_VALUE));
    do_sample(setno, typeno, exprstr, startno, stepno);
}
