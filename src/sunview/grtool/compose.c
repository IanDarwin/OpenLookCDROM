
/*
	compose.c - transformations, curve fitting, etc.

	$Header: compose.c,v 1.4 89/08/27 11:55:45 pturner Locked $
*/

static char buf[256];		/* string used here and there */

#include <stdio.h>
#include <math.h>
#include <suntool/sunview.h>
#include <suntool/canvas.h>
#include <suntool/panel.h>
#include "globals.h"
#include "defines.h"

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

#define define_select_set_panel4(panel,panel_item,x,y) \
				panel_item=panel_create_item( panel,\
				PANEL_CYCLE,\
				PANEL_LABEL_STRING,"Select set:",\
	 			PANEL_CHOICE_STRINGS,\
			"Set 0", "Set 1", "Set 2", "Set 3", "Set 4", "Set 5",\
			"Set 6", "Set 7", "Set 8", "Set 9", "Set 10", "Set 11",\
			"Set 12", "Set 13", "Set 14", "All sets", 0,\
	    		PANEL_ITEM_X,x,\
	    		PANEL_ITEM_Y,y,\
			0 );

void drawgraph();		/* calls plotone(), called any time the graph
				 * needs to be re-drawn */

extern Frame main_frame;
extern Cursor cursor, cursor_save;	/* cursor for zoom */
extern Cursor cursor_wait;
extern Canvas canvas;
extern Pixwin *pw;
extern Pixfont *winfont;	/* font to use for all of grtool */

void define_compose_popup();	/* transformations, etc. */
Frame compose_frame;
static Panel compose_panel;
static Panel_item toggle_degree_item;
static Panel_item toggle_resid_item;	/* load residuals, fitted values or
					 * nothing */
static Panel_item toggle_set_regress_item;	/* which set to use */
static Panel_item toggle_set_differ_item;	/* which set to use */
static Panel_item toggle_differ_type_item;
static Panel_item toggle_set_int_item;	/* which set to use */
static Panel_item toggle_int_type_item;
static Panel_item int_sum_item;
static Panel_item toggle_ravglen_item;
static Panel_item toggle_set_runavg_item;
static Panel_item histo_binw_item;
static Panel_item histo_hxmin_item;
static Panel_item histo_hxmax_item;
static Panel_item toggle_set_histo_item;
static Panel_item toggle_set_fourier_item;
static Panel_item compute_formula_item;
static Panel_item compute_set_item;
static Panel_item load_set_item;
static Panel_item load_start_item;
static Panel_item load_step_item;
static Panel_item load_to_item;
static Panel_item toggle_set_xcor1_item;
static Panel_item toggle_set_xcor2_item;
static Panel_item toggle_xcor_type_item;
static Panel_item xcor_lag_item;

static void do_compute_proc();

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

forwarddiff(x, y, resx, resy, n)
    double x[], y[], resx[], resy[];
int n;

{
    int i;
    double h;

    for (i = 1; i < n; i++) {
	resx[i - 1] = x[i - 1];
	h = x[i - 1] - x[i];
	resy[i - 1] = (y[i - 1] - y[i]) / h;
    }
}

backwarddiff(x, y, resx, resy, n)
    double x[], y[], resx[], resy[];
int n;

{
    int i;
    double h;

    for (i = 0; i < n - 1; i++) {
	resx[i] = x[i];
	h = x[i + 1] - x[i];
	resy[i] = (y[i + 1] - y[i]) / h;
    }
}

centereddiff(x, y, resx, resy, n)
    double x[], y[], resx[], resy[];
int n;

{
    int i;
    double h1, h2;

    for (i = 1; i < n - 1; i++) {
	resx[i - 1] = x[i];
	h1 = x[i] - x[i - 1];
	h2 = x[i + 1] - x[i];
	resy[i] = (y[i + 1] - y[i - 1]) / (h1 + h2);
    }
}

double trapint(x, y, resx, resy, n)
    double x[], y[], resx[], resy[];
int n;

{
    int i;
    double sum = 0.0;
    double h;

    for (i = 1; i < n; i++) {
	h = (x[i] - x[i - 1]);
	if (resx != NULL)
	    resx[i - 1] = (x[i - 1] + x[i]) * 0.5;
	sum = sum + h * (y[i - 1] + y[i]) * 0.5;
	if (resy != NULL)
	    resy[i - 1] = sum;
    }
    return sum;
}

static void do_xcor_proc(item, event)
    Panel_item item;
    Event *event;
{
    int set1, set2, xcorset, itype, lag, i, ierr;
    double *getx(), *gety(), *xtmp;

    set1 = (int) panel_get_value(toggle_set_xcor1_item);
    set2 = (int) panel_get_value(toggle_set_xcor2_item);
    itype = (int) panel_get_value(toggle_xcor_type_item);
    lag = atoi((char *) panel_get_value(xcor_lag_item));
    if (lag == 0 || (getsetlength(set1) - 1 < lag)) {
	return;
    }
    if (!(isactive(set1) || isactive(set2))) {
	return;
    }
    if ((getsetlength(set1) < 3) || (getsetlength(set2) < 3)) {
	return;
    }
    xcorset = nextset();
    if (xcorset != (-1)) {
	activateset(xcorset);
	if (set1 != set2)
	    sprintf(buf, "X-correlation of set % and %d at lag %d", set1, set2, lag);
	else
	    sprintf(buf, "Autocorrelation of set %d at lag %d", set1, lag);
	ierr = crosscorr(gety(set1), gety(set2), getsetlength(set1), lag, itype, getx(xcorset), gety(xcorset));
	xtmp = getx(xcorset);
	for (i = 0; i < lag; i++) {
	    xtmp[i] = i;
	}
	setlength(xcorset, lag);
	setcomment(xcorset, buf);
	updatesetminmax(xcorset);
	update_status(xcorset);
	drawgraph();
    }
}


static void do_int_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, intset, itype;
    double *getx(), *gety();
    double sum;

    setno = (int) panel_get_value(toggle_set_int_item);
    itype = (int) panel_get_value(toggle_int_type_item);
    if (!isactive(setno)) {
	return;
    }
    if (getsetlength(setno) < 3) {
	return;
    }
    if (itype == 0) {
	intset = nextset();
	if (intset != (-1)) {
	    activateset(intset);
	    sprintf(buf, "Cumulative sum of set %d", setno);
	    sum = trapint(getx(setno), gety(setno), getx(intset), gety(intset), getsetlength(setno));
	    setlength(intset, getsetlength(setno) - 1);
	    setcomment(intset, buf);
	    updatesetminmax(intset);
	    sprintf(buf, "%lf", sum);
	    panel_set_value(int_sum_item, buf);
	    update_status(intset);
	    drawgraph();
	}
    } else {
	sum = trapint(getx(setno), gety(setno), NULL, NULL, getsetlength(setno));
	sprintf(buf, "%lf", sum);
	panel_set_value(int_sum_item, buf);
    }
}

static void do_differ_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, diffset, itype;
    double *getx(), *gety();

    setno = (int) panel_get_value(toggle_set_differ_item);
    itype = (int) panel_get_value(toggle_differ_type_item);
    if (!isactive(setno)) {
	return;
    }
    if (getsetlength(setno) < 3) {
	return;
    }
    diffset = nextset();
    if (diffset != (-1)) {
	activateset(diffset);
	switch (itype) {
	case 0:
	    sprintf(buf, "Forward difference of set %d", setno);
	    forwarddiff(getx(setno), gety(setno), getx(diffset), gety(diffset), getsetlength(setno));
	    setlength(diffset, getsetlength(setno) - 1);
	    break;
	case 1:
	    backwarddiff(getx(setno), gety(setno), getx(diffset), gety(diffset), getsetlength(setno));
	    sprintf(buf, "Backward difference of set %d", setno);
	    setlength(diffset, getsetlength(setno) - 1);
	    break;
	case 2:
	    centereddiff(getx(setno), gety(setno), getx(diffset), gety(diffset), getsetlength(setno));
	    sprintf(buf, "Centered difference of set %d", setno);
	    setlength(diffset, getsetlength(setno) - 2);
	    break;
	}
	setcomment(diffset, buf);
	updatesetminmax(diffset);
	update_status(diffset);
	drawgraph();
    }
}

static void do_regress_proc(item, event)
    Panel_item item;
    Event *event;
{
    int fitset, setno, ideg, iresid, i;
    double *getx(), *gety(), *yd, *yf;

    setno = (int) panel_get_value(toggle_set_regress_item);
    ideg = (int) panel_get_value(toggle_degree_item) + 1;
    iresid = (int) panel_get_value(toggle_resid_item);
    if (!isactive(setno)) {
	return;
    }
    fitset = nextset();
    if (fitset != (-1)) {
	activateset(fitset);
	setlength(fitset, getsetlength(setno));
	copyx(setno, fitset);
	fitcurve(getx(setno), gety(setno), getsetlength(setno), ideg, gety(fitset));
	if (iresid) {
	    yf = gety(fitset);
	    yd = gety(setno);
	    for (i = 0; i < getsetlength(fitset); i++) {
		yf[i] = yd[i] - yf[i];
	    }
	}
	sprintf(buf, "%d deg fit of set %d", ideg, setno);
	setcomment(fitset, buf);
	updatesetminmax(fitset);
	update_status(fitset);
    }
    drawgraph();
}

static void do_runavg_proc(item, event)
    Panel_item item;
    Event *event;
{
    int runset, runlen, setno;
    double *getx(), *gety();

    setno = (int) panel_get_value(toggle_set_runavg_item);
    runlen = atoi(panel_get_value(toggle_ravglen_item));
    if (!isactive(setno)) {
	errwin("Set not active");
	return;
    }
    if (runlen >= getsetlength(setno)) {
	errwin("Length of running average too long");
	return;
    }
    if (runlen < 2) {
	errwin("Length of running average too short");
	return;
    }
    runset = nextset();
    if (runset != (-1)) {
	activateset(runset);
	setlength(runset, getsetlength(setno) - runlen + 1);
	runavg(getx(setno), gety(setno), getx(runset), gety(runset), getsetlength(setno), runlen);
	sprintf(buf, "%d-pt. avg. on set %d ", runlen, setno);
	setcomment(runset, buf);
	updatesetminmax(runset);
	update_status(runset);
    }
    drawgraph();
}

static void do_fourier_proc(item, event)
    Panel_item item;
    Event *event;
{
    double *getx(), *gety();
    int i, specset, setno;
    double *y, *xx, *yy;

    setno = (int) panel_get_value(toggle_set_fourier_item);
    if (!isactive(setno)) {
	errwin("Set not active");
	return;
    }
    if (getsetlength(setno) < 2) {
	errwin("Set length < 2");
	return;
    }
    specset = nextset();
    if (specset != -1) {
	window_set(canvas, WIN_CURSOR, cursor_wait, 0);
	window_set(compose_panel, WIN_CURSOR, cursor_wait, 0);
	y = gety(setno);
	activateset(specset);
	xx = getx(specset);
	yy = gety(specset);
	setlength(specset, fouriercoef(y, xx, yy, getsetlength(setno)));
	for (i = 0; i < getsetlength(specset); i++) {
	    /*
	     * xx[i]=hypot(xx[i],yy[i]); yy[i]=atan2(-yy[i],xx[i]);
	     */
	    yy[i] = hypot(xx[i], yy[i]);
	    xx[i] = i;
	}
	sprintf(buf, "DFT of set %d", setno);
	setcomment(specset, buf);
	updatesetminmax(specset);
	update_status(specset);
	window_set(canvas, WIN_CURSOR, cursor_save, 0);
	window_set(compose_panel, WIN_CURSOR, cursor_save, 0);
    }
    drawgraph();
}

static void do_fft_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i;
    double *getx(), *gety();
    double *xx, *yy;
    int i2, setno, specset;

    setno = (int) panel_get_value(toggle_set_fourier_item);
    if (!isactive(setno)) {
	errwin("Set not active");
	return;
    }
    if (getsetlength(setno) < 2) {
	errwin("Set length < 2");
	return;
    }
    if ((i2 = ilog2(getsetlength(setno))) <= 0) {
	errwin("Set length not a power of 2");
	return;
    }
    specset = nextset();
    if (specset != -1) {
	activateset(specset);
	xx = getx(specset);
	yy = gety(specset);
	setlength(specset, getsetlength(setno));
	copyx(setno, specset);
	copyy(setno, specset);
	fft(xx, yy, getsetlength(specset), i2, 1);
	for (i = 0; i < getsetlength(specset); i++) {
	    /*
	     * xx[i]=hypot(xx[i],yy[i]); yy[i]=atan2(-yy[i],xx[i]);
	     */
	    yy[i] = hypot(xx[i], yy[i]);
	    xx[i] = i;
	}
	sprintf(buf, "FFT of set %d", setno);
	setcomment(specset, buf);
	updatesetminmax(specset);
	update_status(specset);
	drawgraph();
    }
}

static void do_histo_proc(item, event)
    Panel_item item;
    Event *event;
{
    int hset;
    double binw, xmin, xmax;

    hset = (int) panel_get_value(toggle_set_histo_item);
    if (!isactive(hset)) {
	errwin("Set not active");
	return;
    }
    if (getsetlength(hset) <= 0) {
	errwin("Set length = 0");
	return;
    }
    binw = atof(panel_get_value(histo_binw_item));
    xmin = atof(panel_get_value(histo_hxmin_item));
    xmax = atof(panel_get_value(histo_hxmax_item));
    histogram(hset, binw, xmin, xmax);
    drawgraph();
}

histogram(setno, bins, xmin, xmax)
    int setno;
    double bins, xmin, xmax;
{
    int binmax, n1, n, i, j, nbins, hset;
    double spread, xi, *x, *y;
    int *ind;

    if ((hset = nextset()) == -1) {
	return;
    }
    ind = (int *) calloc(maxarr, sizeof(int));
    for (i = 0; i < maxarr; i++)
	ind[i] = 0;
    n = getsetlength(setno);
    spread = xmax - xmin;
    nbins = (int) (spread / bins);
    j = 0;
    y = gety(setno);
    for (i = 0; i < n; i++) {
	xi = y[i];
	j = (int) ((xi - xmin) / bins);
	ind[j] = ind[j] + 1;
    }
    n1 = 0;
    binmax = 0;
    x = getx(hset);
    y = gety(hset);
    for (i = 0; i < nbins; i++) {
	x[i] = i * bins + bins / 2.0 + xmin;
	y[i] = ind[i];
	n1 = n1 + ind[i];
	if (ind[i] > binmax) {
	    binmax = ind[i];
	}
    }
    activateset(hset);
    setlength(hset, nbins);
    setplotsym(hset, HISTOSYM);
    updatesetminmax(hset);
    sprintf(buf, "Histogram from set # %d", setno);
    setcomment(hset, buf);
    update_status(hset);
    free(ind);
}

static void do_compute_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i, idraw = 0, setno;
    char fstr[80];

    setno = (int) panel_get_value(compute_set_item);
    strcpy(fstr, panel_get_value(compute_formula_item));
    if (setno == maxplot) {
	for (i = 0; i < maxplot; i++) {
	    if (isactive(i)) {
		formula(i, fstr);
		idraw = 1;
	    }
	}
    } else if (isactive(setno)) {
	formula(setno, fstr);
	idraw = 1;
    }
    if (idraw)
	drawgraph();
}

static void do_load_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i, ier = 0, idraw = 0, setno, toval, getsetno(), getint(), isactive();
    double start, step, tmp;
    double x,y,a,b,c,d;
    extern double result;

    setno = (int) panel_get_value(load_set_item);
    toval = (int) panel_get_value(load_to_item) + 1;
    strcpy(buf, panel_get_value(load_start_item));
    fixupstr(buf);
    scanner(buf, &x,&y,&a,&b,&c,&d,0,&ier);
    if (ier) 
	return;
    start = result;
    strcpy(buf, panel_get_value(load_step_item));
    fixupstr(buf);
    scanner(buf, &x,&y,&a,&b,&c,&d,0,&ier);
    if (ier) 
	return;
    step = result;
    if (setno == maxplot) {
	for (i = 0; i < maxplot; i++) {
	    if (isactive(i)) {
		loadset(i, toval, start, step);
		idraw = 1;
	    }
	}
    } else if (isactive(setno)) {
	loadset(setno, toval, start, step);
	idraw = 1;
    }
    if (idraw)
	drawgraph();
}

static void compose_done_proc()
{
    window_set(compose_frame, WIN_SHOW, FALSE, 0);
}

void define_compose_popup()
{
    compose_frame = window_create(main_frame, FRAME,
				  WIN_Y, 50,
				  FRAME_LABEL, "Tranformations",
				  FRAME_SHOW_LABEL, TRUE,
			     WIN_ERROR_MSG, "Couldn't create compose_frame",
				  0);
    compose_panel = window_create(compose_frame, PANEL, 0);
    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	  panel_button_image(compose_panel, "Evaluate formula", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_NOTIFY_PROC, do_compute_proc,
		      0);
    define_select_set_panel4(compose_panel, compute_set_item, ATTR_COL(1), ATTR_ROW(2));
    compute_formula_item = panel_create_item(compose_panel, PANEL_TEXT,
					     PANEL_LABEL_STRING, "Formula:",
					     PANEL_ITEM_X, ATTR_COL(1),
					     PANEL_ITEM_Y, ATTR_ROW(3),
					     PANEL_NOTIFY_STRING, "\n\r",
					 PANEL_NOTIFY_PROC, eotproc_compose,
					 PANEL_VALUE_DISPLAY_LENGTH, 40, 0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(compose_panel, "Load", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_NOTIFY_PROC, do_load_proc,
		      0);
    define_select_set_panel4(compose_panel, load_set_item, ATTR_COL(1), ATTR_ROW(6));
    load_to_item = panel_create_item(compose_panel, PANEL_CYCLE,
				     PANEL_ITEM_X, ATTR_COL(1),
				     PANEL_ITEM_Y, ATTR_ROW(7),
				     PANEL_LABEL_STRING, "To:",
				     PANEL_CHOICE_STRINGS,
				     "X",
				     "Y",
				     "Scratch array A",
				     "Scratch array B",
				     "Scratch array C",
				     "Scratch array D", 0,
				     0);
    load_start_item = panel_create_item(compose_panel, PANEL_TEXT,
				    PANEL_LABEL_STRING, "Load start value:",
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(8),
					PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    load_step_item = panel_create_item(compose_panel, PANEL_TEXT,
				       PANEL_ITEM_X, ATTR_COL(1),
				       PANEL_ITEM_Y, ATTR_ROW(9),
				     PANEL_LABEL_STRING, "Load step value:",
				       PANEL_VALUE_DISPLAY_LENGTH, 15, 0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	     panel_button_image(compose_panel, "Differentiate", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(30),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_NOTIFY_PROC, do_differ_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_differ_item, ATTR_COL(30), ATTR_ROW(6));
    toggle_differ_type_item = panel_create_item(compose_panel, PANEL_CYCLE,
					      PANEL_LABEL_STRING, "Method:",
						PANEL_CHOICE_STRINGS,
						"Forward difference",
						"Backward difference",
						"Centered difference",
						0,
						PANEL_ITEM_X, ATTR_COL(30),
						PANEL_ITEM_Y, ATTR_ROW(7),
						0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		 panel_button_image(compose_panel, "Integrate", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(30),
		      PANEL_ITEM_Y, ATTR_ROW(9),
		      PANEL_NOTIFY_PROC, do_int_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_int_item, ATTR_COL(30), ATTR_ROW(10));
    toggle_int_type_item = panel_create_item(compose_panel, PANEL_CYCLE,
					     PANEL_LABEL_STRING, "Load:",
					     PANEL_CHOICE_STRINGS,
					     "Cumulative sum",
					     "Sum only",
					     0,
					     PANEL_ITEM_X, ATTR_COL(30),
					     PANEL_ITEM_Y, ATTR_ROW(11),
					     0);
    int_sum_item = panel_create_item(compose_panel, PANEL_TEXT,
				     PANEL_ITEM_X, ATTR_COL(30),
				     PANEL_ITEM_Y, ATTR_ROW(12),
				     PANEL_LABEL_STRING, "Sum:",
				     PANEL_VALUE_DISPLAY_LENGTH, 15, 0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		    panel_button_image(compose_panel, "X-corr", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(30),
		      PANEL_ITEM_Y, ATTR_ROW(14),
		      PANEL_NOTIFY_PROC, do_xcor_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_xcor1_item, ATTR_COL(30), ATTR_ROW(15));
    define_select_set_panel3(compose_panel, toggle_set_xcor2_item, ATTR_COL(30), ATTR_ROW(16));
    toggle_xcor_type_item = panel_create_item(compose_panel, PANEL_CYCLE,
					      PANEL_LABEL_STRING, "Bias:",
					      PANEL_CHOICE_STRINGS,
					      "Biased estimate",
					      "Unbiased estimate",
					      0,
					      PANEL_ITEM_X, ATTR_COL(30),
					      PANEL_ITEM_Y, ATTR_ROW(17),
					      0);
    xcor_lag_item = panel_create_item(compose_panel, PANEL_TEXT,
				      PANEL_ITEM_X, ATTR_COL(30),
				      PANEL_ITEM_Y, ATTR_ROW(18),
				      PANEL_LABEL_STRING, "Lag:",
				      PANEL_VALUE_DISPLAY_LENGTH, 15, 0);


    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(compose_panel, "Histo", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(11),
		      PANEL_NOTIFY_PROC, do_histo_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_histo_item, ATTR_COL(1), ATTR_ROW(12));
    histo_binw_item = panel_create_item(compose_panel, PANEL_TEXT,
					PANEL_ITEM_X, ATTR_COL(1),
					PANEL_ITEM_Y, ATTR_ROW(13),
					PANEL_LABEL_STRING, "Bin width:",
					PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    histo_hxmin_item = panel_create_item(compose_panel, PANEL_TEXT,
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(14),
					 PANEL_LABEL_STRING, "Minimum X:",
					 PANEL_VALUE_DISPLAY_LENGTH, 15, 0);
    histo_hxmax_item = panel_create_item(compose_panel, PANEL_TEXT,
					 PANEL_ITEM_X, ATTR_COL(1),
					 PANEL_ITEM_Y, ATTR_ROW(15),
					 PANEL_LABEL_STRING, "Maximum X:",
					 PANEL_VALUE_DISPLAY_LENGTH, 15, 0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(compose_panel, "DFT", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(17),
		      PANEL_NOTIFY_PROC, do_fourier_proc,
		      0);
    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(compose_panel, "FFT", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(7),
		      PANEL_ITEM_Y, ATTR_ROW(17),
		      PANEL_NOTIFY_PROC, do_fft_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_fourier_item, ATTR_COL(1), ATTR_ROW(18));

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
	      panel_button_image(compose_panel, "Running avg.", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(20),
		      PANEL_NOTIFY_PROC, do_runavg_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_runavg_item, ATTR_COL(1), ATTR_ROW(21));
    toggle_ravglen_item = panel_create_item(compose_panel, PANEL_TEXT,
			   PANEL_LABEL_STRING, "Length of running average:",
					    PANEL_ITEM_X, ATTR_COL(1),
					    PANEL_ITEM_Y, ATTR_ROW(22),
					 PANEL_VALUE_DISPLAY_LENGTH, 20, 0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		   panel_button_image(compose_panel, "Regress", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(24),
		      PANEL_NOTIFY_PROC, do_regress_proc,
		      0);
    define_select_set_panel3(compose_panel, toggle_set_regress_item, ATTR_COL(1), ATTR_ROW(25));
    toggle_degree_item = panel_create_item(compose_panel, PANEL_CYCLE,
				PANEL_LABEL_STRING, "Select degree of fit:",
					   PANEL_CHOICE_STRINGS,
					   "Linear regression",
					   "Quadratic",
					   "Cubic",
					   "4th degree",
					   "5th degree", 0,
					   PANEL_ITEM_X, ATTR_COL(1),
					   PANEL_ITEM_Y, ATTR_ROW(26),
					   0);
    toggle_resid_item = panel_create_item(compose_panel, PANEL_CYCLE,
					  PANEL_LABEL_STRING, "Load:",
					  PANEL_CHOICE_STRINGS,
					  "Fitted values",
					  "Residuals", 0,
					  PANEL_ITEM_X, ATTR_COL(1),
					  PANEL_ITEM_Y, ATTR_ROW(27),
					  0);

    panel_create_item(compose_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(compose_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(29),
		      PANEL_NOTIFY_PROC, compose_done_proc,
		      0);
    window_fit(compose_panel);
    window_fit(compose_frame);
}
