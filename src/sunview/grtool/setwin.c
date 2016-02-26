
/*
	setops - operations on sets

	$Header: setwin.c,v 1.6 89/09/02 10:07:53 pturner Locked $
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
#include "defines.h"
#include "globals.h"

static char format[64] = "%lf %lf";

#define PIX_XOR PIX_SRC^PIX_DST

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

void define_setops_popup();

extern Pixfont *winfont;

extern Window main_frame, main_panel;
Frame setops_frame;
static Panel setops_panel;
static Panel_item activate_set_item;
static Panel_item set_length_item;
static Panel_item length_to_set_item;
static Panel_item copy_from_item;
static Panel_item copy_to_item;
static Panel_item drop_points_item;
static Panel_item drop_start_item;
static Panel_item drop_end_item;
static Panel_item join_from_item;
static Panel_item join_to_item;
static Panel_item kill_set_item;
static Panel_item sort_set_item;
static Panel_item sort_xy_item;
static Panel_item sort_up_down_item;
static Panel_item write_sets_item;
static Panel_item write_sets_format_item;
static Panel_item write_sets_file_item;

static void do_activate_proc();
static void do_setlength_proc();
static void setops_done_proc();
static void do_copy_proc();
static void do_drop_points_proc();
static void do_join_sets_proc();
static void do_kill_proc();
static void do_flush_proc();
static void do_sort_proc();
static void do_write_sets_proc();

static char errbuf[100];

void drawgraph();

static void do_activate_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno;

    setno = (int) panel_get_value(activate_set_item);
    if (isactive(setno)) {
	sprintf(errbuf, "Set %d already active", setno);
	errwin(errbuf);
	return;
    }
    activateset(setno);
    updatesetminmax(setno);
    update_status(setno);
}

static void do_setlength_proc(item, event)
    Panel_item item;
    Event *event;
{
    int setno, len;
    char buf[16];

    setno = (int) panel_get_value(set_length_item);
    if (!isactive(setno)) {
	sprintf(errbuf, "Set %d not active", setno);
	errwin(errbuf);
	return;
    }
    strcpy(buf, (char *) panel_get_value(length_to_set_item));
    len = atoi(buf);
    if (len <= 0 || len>maxarr) {
	sprintf(errbuf, "Improper set length = %d", len);
	errwin(errbuf);
	return;
    }
    setlength(setno, len);
    updatesetminmax(setno);
    update_status(setno);
    drawgraph();
}

static void setops_done_proc()
{
    window_set(setops_frame, WIN_SHOW, FALSE, 0);
}

void define_setops_popup()
{
    setops_frame = window_create(main_frame, FRAME,
				 WIN_Y, 50,
				 FRAME_LABEL, "Set operations",
				 FRAME_SHOW_LABEL, TRUE,
			      WIN_ERROR_MSG, open_err_msg,
				 0);
    setops_panel = window_create(setops_frame, PANEL, 0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(1), "Activate:", activate_set_item);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		   panel_button_image(setops_panel, "Activate", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(1),
		      PANEL_NOTIFY_PROC, do_activate_proc,
		      0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(2), "Set length of:", set_length_item);
    length_to_set_item = panel_create_item(setops_panel, PANEL_TEXT,
					   PANEL_LABEL_STRING, "Length: ",
					   PANEL_ITEM_X, ATTR_COL(45),
					   PANEL_ITEM_Y, ATTR_ROW(2),
					 PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		 panel_button_image(setops_panel, "Set length", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(2),
		      PANEL_NOTIFY_PROC, do_setlength_proc,
		      0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(3), "Copy from:", copy_from_item);
    define_select_set_panel(setops_panel, ATTR_COL(35), ATTR_ROW(3), "To:", copy_to_item);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Copy", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(3),
		      PANEL_NOTIFY_PROC, do_copy_proc,
		      0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(4), "Drop points from:", drop_points_item);
    drop_start_item = panel_create_item(setops_panel, PANEL_TEXT,
					PANEL_LABEL_STRING, "Start drop:",
					PANEL_ITEM_X, ATTR_COL(45),
					PANEL_ITEM_Y, ATTR_ROW(4),
					PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    drop_end_item = panel_create_item(setops_panel, PANEL_TEXT,
				      PANEL_LABEL_STRING, "End drop: ",
				      PANEL_ITEM_X, ATTR_COL(65),
				      PANEL_ITEM_Y, ATTR_ROW(4),
				      PANEL_VALUE_DISPLAY_LENGTH, 10, 0);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Drop", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(4),
		      PANEL_NOTIFY_PROC, do_drop_points_proc,
		      0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(5), "Join:", join_from_item);
    define_select_set_panel(setops_panel, ATTR_COL(35), ATTR_ROW(5), "To:", join_to_item);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Join", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(5),
		      PANEL_NOTIFY_PROC, do_join_sets_proc,
		      0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(6), "Kill:", kill_set_item);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Kill", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(6),
		      PANEL_NOTIFY_PROC, do_kill_proc,
		      0);

    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
      panel_button_image(setops_panel, "Flush all active sets", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(7),
		      PANEL_NOTIFY_PROC, do_flush_proc,
		      0);

    define_select_set_panel(setops_panel, ATTR_COL(15), ATTR_ROW(8), "Sort:", sort_set_item);
    sort_xy_item = panel_create_item(setops_panel, PANEL_CYCLE,
				     PANEL_LABEL_STRING, "Sort on:",
				     PANEL_CHOICE_STRINGS,
				     "X",
				     "Y", 0,
				     PANEL_ITEM_X, ATTR_COL(35),
				     PANEL_ITEM_Y, ATTR_ROW(8),
				     0);
    sort_up_down_item = panel_create_item(setops_panel, PANEL_CYCLE,
					  PANEL_LABEL_STRING, "Order:",
					  PANEL_CHOICE_STRINGS,
					  "Ascending",
					  "Descending", 0,
					  PANEL_ITEM_X, ATTR_COL(55),
					  PANEL_ITEM_Y, ATTR_ROW(8),
					  0);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Sort", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(8),
		      PANEL_NOTIFY_PROC, do_sort_proc,
		      0);

    write_sets_item = panel_create_item(setops_panel,
					PANEL_CYCLE,
					PANEL_ITEM_X, ATTR_COL(15),
					PANEL_ITEM_Y, ATTR_ROW(9),
					PANEL_LABEL_STRING, "Write:",
					PANEL_CHOICE_STRINGS,
		"All", "Set 0", "Set 1", "Set 2", "Set 3", "Set 4", "Set 5",
		     "Set 6", "Set 7", "Set 8", "Set 9", "Set 10", "Set 11",
					"Set 12", "Set 13", "Set 14", 0, 0);
    write_sets_format_item = panel_create_item(setops_panel, PANEL_TEXT,
					     PANEL_LABEL_STRING, "Format: ",
					       PANEL_ITEM_X, ATTR_COL(35),
					       PANEL_ITEM_Y, ATTR_ROW(9),
					 PANEL_VALUE_DISPLAY_LENGTH, 20, 0);
    write_sets_file_item = panel_create_item(setops_panel, PANEL_TEXT,
				      PANEL_LABEL_STRING, "Write to file: ",
					     PANEL_ITEM_X, ATTR_COL(15),
					     PANEL_ITEM_Y, ATTR_ROW(10),
					 PANEL_VALUE_DISPLAY_LENGTH, 40, 0);
    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Write", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(9),
		      PANEL_NOTIFY_PROC, do_write_sets_proc,
		      0);

    panel_create_item(setops_panel, PANEL_BUTTON,
		      PANEL_LABEL_IMAGE,
		      panel_button_image(setops_panel, "Done", 0, winfont),
		      PANEL_ITEM_X, ATTR_COL(1),
		      PANEL_ITEM_Y, ATTR_ROW(12),
		      PANEL_NOTIFY_PROC, setops_done_proc,
		      0);
    panel_set_value(write_sets_format_item, format);
    window_fit(setops_panel);
    window_fit(setops_frame);
}

/*
	setops - combine, copy sets
*/

static void do_copy_proc(item, event)
    Panel_item item;
    Event *event;
{
    int j1, j2, i;
    char buf[32];
    double *x1, *x2, *y1, *y2, *getx(), *gety();

    j1 = (int) panel_get_value(copy_from_item);
    if (!isactive(j1)) {
	sprintf(errbuf, "Set %d not active", j1);
	errwin(errbuf);
	return;
    }
    j2 = (int) panel_get_value(copy_to_item);
    if (!isactive(j2)) {
	sprintf(errbuf, "Set %d not active", j2);
	errwin(errbuf);
	return;
    }
    x1 = getx(j1);
    y1 = gety(j1);
    x2 = getx(j2);
    y2 = gety(j2);
    for (i = 0; i < getsetlength(j1); i++) {
	x2[i] = x1[i];
	y2[i] = y1[i];
    }
    setlength(j2, getsetlength(j1));
    sprintf(buf, "copy of set %d", j1);
    setcomment(j2, buf);
    updatesetminmax(j2);
    update_status(j2);
    drawgraph();
}

static void do_drop_points_proc()
{
    int i, dist, startno, endno, setno;
    double *x, *y, *getx(), *gety();

    setno = (int) panel_get_value(drop_points_item);
    if (!isactive(setno)) {
	sprintf(errbuf, "Set %d not active", setno);
	errwin(errbuf);
	return;
    }
    startno = atoi((char *) panel_get_value(drop_start_item)) - 1;
    endno = atoi((char *) panel_get_value(drop_end_item)) - 1;
    dist = endno - startno + 1;
    if (startno<0) {
	errwin("Start # < 1");
	return;
    }
    if (endno>=getsetlength(setno)) {
	errwin("Ending # > set length");
	return;
    }
    if (startno>endno) {
	errwin("Starting # > ending #");
	return;
    }
    if (dist == getsetlength(setno)) {
	errwin("# of points to drop = set length, use kill");
	return;
    }
    x = getx(setno);
    y = gety(setno);
    for (i = endno + 1; i < getsetlength(setno); i++) {
	x[i - dist] = x[i];
	y[i - dist] = y[i];
    }
    setlength(setno, getsetlength(setno) - dist);
    updatesetminmax(setno);
    update_status(setno);
    drawgraph();
}

static void do_join_sets_proc()
{
    int j1, j2, jlen, i, it;
    double *x1, *x2, *y1, *y2, *getx(), *gety();

    j1 = (int) panel_get_value(join_from_item);
    if (!isactive(j1)) {
	sprintf(errbuf, "Set %d not active", j1);
	errwin(errbuf);
	return;
    }
    j2 = (int) panel_get_value(join_to_item);
    if (!isactive(j2)) {
	sprintf(errbuf, "Set %d not active", j2);
	errwin(errbuf);
	return;
    }
    if ((jlen = getsetlength(j1) + getsetlength(j2)) <= maxarr) {
	x1 = getx(j1);
	x2 = getx(j2);
	y1 = gety(j1);
	y2 = gety(j2);
	it = getsetlength(j2);
	for (i = 0; i < getsetlength(j1); i++) {
	    x2[it + i] = x1[i];
	    y2[it + i] = y1[i];
	}
	setlength(j2, jlen);
	updatesetminmax(j2);
	update_status(j2);
        drawgraph();
    } else
	errwin("Sets too big to join");
}

static void do_kill_proc()
{
    int setno;

    setno = (int) panel_get_value(kill_set_item);
    if (!isactive(setno)) {
	sprintf(errbuf, "Set %d already dead", setno);
	errwin(errbuf);
	return;
    } else {
	killset(setno);
	update_status(setno);
        drawgraph();
    }
}

static void do_flush_proc()
{
    int i, j;
    double *x, *y, *getx(), *gety();

    if (yesno("Flush all active sets, are you sure? ","","YES","NO")) {
	for (i = 0; i < maxplot; i++) {
	    if (isactive(i)) {
		killset(i);
		/*
		 * setplotsym(i,0); setlinesym(i,0);
		 */
		setlength(i, 0);
		x = getx(i);
		y = gety(i);
		for (j = 0; j < maxarr; j++) {
		    x[j] = 0.0;
		    y[j] = 0.0;
		}
		setcomment(i, "\0");
		update_status(i);
	    }
	}
        drawgraph();
    }
}

/*
	sort.c - sort sets
*/

static void do_sort_proc()
{

    int d, i, j;
    int lo = 0;
    int up;
    double t1, t2, *tmp1, *tmp2, *getx(), *gety();
    int setno, sorton, stype;

    setno = (int) panel_get_value(sort_set_item);
    if (!isactive(setno)) {
	sprintf(errbuf, "Set %d not active", setno);
	errwin(errbuf);
	return;
    }
    up = getsetlength(setno) - 1;
    sorton = (int) panel_get_value(sort_xy_item);
    stype = (int) panel_get_value(sort_up_down_item);
    if (!sorton) {		/* sort on x */
	tmp1 = getx(setno);
	tmp2 = gety(setno);
    } else {
	tmp1 = gety(setno);
	tmp2 = getx(setno);
    }
    for (d = up - lo + 1; d > 1;) {
	if (d < 5)
	    d = 1;
	else
	    d = (5 * d - 1) / 11;
	for (i = up - d; i >= lo; i--) {
	    t1 = tmp1[i];
	    t2 = tmp2[i];
	    if (!stype) {
		for (j = i + d; j <= up && (t1 > tmp1[j]); j += d) {
		    tmp1[j - d] = tmp1[j];
		    tmp2[j - d] = tmp2[j];
		}
		tmp1[j - d] = t1;
		tmp2[j - d] = t2;
	    } else {
		for (j = i + d; j <= up && (t1 < tmp1[j]); j += d) {
		    tmp1[j - d] = tmp1[j];
		    tmp2[j - d] = tmp2[j];
		}
		tmp1[j - d] = t1;
		tmp2[j - d] = t2;
	    }
	}
    }
    drawgraph();
}

/*
	write.c - write a set or sets to a file
			called from setops.c
*/


static void do_write_sets_proc()
{
    int i, j, n;
    char fn[80];
    FILE *cp;
    double *getx(), *gety();
    double *x, *y;
    int setno;

    setno = (int) panel_get_value(write_sets_item);
    setno--;
    fn[0] = '\0';
    strcpy(fn, (char *) panel_get_value(write_sets_file_item));
    strcpy(format, (char *) panel_get_value(write_sets_format_item));
    if (!fn[0])
	return;
    if (fexists(fn))
	return;
    if ((cp = fopen(fn, "w")) == NULL) {
	char s[64];

	sprintf(s, "Unable to open file %s", fn);
	errwin(s);
    }
    if (setno < 0) {
	if (!activeset()) {
	    errwin("No active sets");
	    return;
	} else {
	    for (j = 0; j < maxplot; j++) {
		if (isactive(j)) {
		    x = getx(j);
		    y = gety(j);
		    n = getsetlength(j);
		    for (i = 0; i < n; i++) {
			fprintf(cp, format, x[i], y[i]);
			fputc('\n', cp);
		    }
		    fprintf(cp, "&\n");
		}
	    }
	    fclose(cp);
	}
    } else {
	if (!isactive(setno)) {
	    sprintf(errbuf, "Set %d not active", setno);
	    errwin(errbuf);
	    return;
	} else {
	    x = getx(setno);
	    y = gety(setno);
	    n = getsetlength(setno);
	    for (i = 0; i < n; i++) {
		fprintf(cp, format, x[i], y[i]);
		fputc('\n', cp);
	    }
	    fclose(cp);
	}
    }
}

/*
	partit.c - split sets
*/
/*
extern int maxplot,maxarr;

evenparts()
{
	int setno,i,j,itmp,tmpset,psets,lpart;
	char s[80];
	double *x,*y,*getx(),*gety();
	if (!nsets) {
		errwin("No active sets");
		return;
	}
	setno=getsetno(LINES-5,0,"Even partition of set # : ");
	if (setno==(-1)) return;
	if (getsetlength(setno)<3) {
		errwin("Set length < 3");
		return;
	}
	if (!isactive(setno)) {
		sprintf(s,"Set %d not active",setno);
		errwin(s);
		return;
	}
	getint(LINES-4,0,"Length of partitions : ",&itmp);
	if (ibounds(itmp,2,maxarr/2,"length of partition")){
		lpart=itmp;
	}
	else return;
	if (lpart>=getsetlength(setno)) {
		errwin("partition greater than or equal to set length");
		return;
	}
	psets=getsetlength(setno)/lpart;
	if (psets>(maxplot-nsets+1)) {
		errwin("Insufficient sets for partition");
		return;
	}
	if (!(getsetlength(setno) % lpart)) {
		mvprintw("Creating %d sets of length %d",psets,lpart);
		for (i=1;i<psets;i++) {
			if (!((tmpset=nextset())>=0)) {
				return;
			}
			x=getx(tmpset);
			y=gety(tmpset);
			for (j=0;j<lpart;j++) {
				x[j]=x[i*lpart+j];
				y[j]=y[i*lpart+j];
			}
			setlength(tmpset,lpart);
			activateset(tmpset);
			sprintf(s,"partition %d of set %d",i+1,setno);
			setcomment(tmpset,s);
			updatesetminmax(tmpset);
		}
		setlength(setno,lpart);
		updatesetminmax(setno);
	}
	else {
		errwin("partition must divide set length evenly");
	}
}
*/
