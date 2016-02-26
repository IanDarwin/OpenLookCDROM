/* $Id: fileswin.c,v 1.23 92/08/15 15:55:03 pturner Exp Locker: pturner $
 *
 * files Panel
 *
 * major problems here with the file selection dialog, I'm using
 * popen("ls etc. to get the files in the current directory TODO fix
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

static char dirbuf[MAXPATHLEN];
char *getcwd();

extern Frame main_frame;

Frame files_frame = (Frame) 0;
Panel files_panel;

static int step_read = 0;

/*
 * Panel item declarations
 */
static Panel_item files_type_choice_item;
static Panel_item files_source_choice_item;
static Panel_item files_list_list_item;
Panel_item files_dir_msg_item;
static Panel_item files_file_text_item;
static Panel_item files_graph_choice_item;
static Panel_item files_step_choice_item;

/*
 * Event and Notify proc declarations
 */
static int files_Done_notify_proc();
static void files_type_notify_proc();
static void files_source_notify_proc();
static void files_list_notify_proc();
static Panel_setting files_file_notify_proc();
static void files_graph_notify_proc();
static int files_accept_notify_proc();
static int files_skip_notify_proc();
static int files_next_notify_proc();
static void update_files();

/*
 * Create the files Frame and the files Panel
 */
void create_file_popup()
{
    FILE *p;
    int i = 0;

    getcwd(dirbuf, MAXPATHLEN);

    if (files_frame) {
	xv_set(files_dir_msg_item, PANEL_LABEL_STRING, dirbuf, NULL);
	xv_set(files_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    files_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "files",
				    NULL);
    files_panel = (Panel) xv_create(files_frame, PANEL,
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    files_dir_msg_item = (Panel_item) xv_create(files_panel, PANEL_MESSAGE,
						PANEL_LABEL_STRING, dirbuf,
						NULL);
    files_list_list_item = (Panel_item) xv_create(files_panel, PANEL_LIST,
					     PANEL_LABEL_STRING, "Contents",
					    XV_HELP_DATA, "xvgr:files_list",
				  PANEL_NOTIFY_PROC, files_list_notify_proc,
						  PANEL_LIST_DISPLAY_ROWS, 5,
						  NULL);
    files_type_choice_item = (Panel_item) xv_create(files_panel, PANEL_CHOICE_STACK,
					   PANEL_LABEL_STRING, "File type:",
					    XV_HELP_DATA, "xvgr:files_type",
						    PANEL_CHOICE_STRINGS,
						    "X Y",
						    "X Y1 Y2 ...",
						    "IHL",
						    "Binary",
						    "X Y DX",
						    "X Y DY",
						    "X Y DX1 DX2",
						    "X Y DY1 DY2",
						    "X Y DX DY",
						    "X Y Z",
						    "X HI LO OPEN CLOSE",
						    "X Y RADIUS",
						    NULL,
				     PANEL_VALUE_X, xv_col(files_panel, 18),
						    NULL);
    files_source_choice_item = (Panel_item) xv_create(files_panel, PANEL_CHOICE,
					   PANEL_LABEL_STRING, "Read from:",
					    XV_HELP_DATA, "xvgr:files_from",
						      PANEL_CHOICE_STRINGS,
						      "Disk",
						      "Pipe",
						      NULL,
				     PANEL_VALUE_X, xv_col(files_panel, 18),
						      NULL);
    files_graph_choice_item = (Panel_item) xv_create(files_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "Read to graph:",
					   XV_HELP_DATA, "xvgr:files_graph",
						     PANEL_CHOICE_STRINGS,
						     "Current",
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						     NULL,
				     PANEL_VALUE_X, xv_col(files_panel, 18),
						     NULL);
    files_file_text_item = (Panel_item) xv_create(files_panel, PANEL_TEXT,
				  PANEL_NOTIFY_PROC, files_file_notify_proc,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 20,
						PANEL_LABEL_STRING, "File:",
					    XV_HELP_DATA, "xvgr:files_file",
				      PANEL_VALUE_X, xv_col(files_panel, 8),
						  NULL);
/*
    files_step_choice_item = (Panel_item) xv_create(files_panel, PANEL_CHECK_BOX,
					    PANEL_LABEL_STRING, "Step read",
					XV_HELP_DATA, "xvgr:files_stepread",
				     PANEL_VALUE_X, xv_col(files_panel, 18),
						    NULL);
*/
    (void) xv_create(files_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, files_accept_notify_proc,
		     XV_HELP_DATA, "xvgr:files_accept",
		     XV_X, xv_col(files_panel, 2),
		     XV_Y, xv_row(files_panel, 10),
		     NULL);
/*
    (void) xv_create(files_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Next",
		     PANEL_NOTIFY_PROC, files_next_notify_proc,
		     XV_HELP_DATA, "xvgr:files_next",
		     XV_X, xv_col(files_panel, 12),
		     XV_Y, xv_row(files_panel, 10),
		     NULL);
    (void) xv_create(files_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Skip",
		     PANEL_NOTIFY_PROC, files_skip_notify_proc,
		     XV_HELP_DATA, "xvgr:files_skip",
		     XV_X, xv_col(files_panel, 19),
		     XV_Y, xv_row(files_panel, 10),
		     NULL);
*/
    (void) xv_create(files_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, files_Done_notify_proc,
		     XV_HELP_DATA, "xvgr:files_done",
		     XV_X, xv_col(files_panel, 15),
		     XV_Y, xv_row(files_panel, 10),
		     NULL);
    window_fit(files_panel);
    window_fit(files_frame);
    update_files(NULL);
    xv_set(files_frame, WIN_SHOW, TRUE, 0);
}				/* end create_files_panel */

static int set_types[] = {XY, NXY, IHL, BIN, XYDX, XYDY, XYDXDX, XYDYDY, XYDXDY, XYZ, XYHILO, XYRT, -1};

/*
 * update the list with the current contents
 */
void update_file_list(mask, dirbuf, list_item, dir_item)
    char *mask, *dirbuf;
    Panel_item list_item, dir_item;
{
    int d1, d2, i = 0;
    char buf[MAXPATHLEN + 17];
    char **dlist, **make_dir_list();
    int nitems, nadd;
    int n = (int) xv_get(list_item, PANEL_LIST_NROWS);

    dlist = make_dir_list(mask, &nitems);
    if (dlist != NULL) {
	getcwd(dirbuf, MAXPATHLEN);
	xv_set(dir_item, PANEL_LABEL_STRING, dirbuf, NULL);
	xv_set(list_item, XV_SHOW, FALSE, NULL);
	for (i = 0; i < n; i++) {
	    xv_set(list_item, PANEL_LIST_DELETE, 0, 0);
	}
	nadd = 0;
	for (i = 0; i < nitems; i++) {
	    strcpy(buf, dlist[i]);
	    if (isdir(buf)) {
		strcat(buf, "/");
		xv_set(list_item, PANEL_LIST_INSERT, nadd,
		       PANEL_LIST_STRING, nadd, buf, NULL);
		nadd++;
	    }
	}
	for (i = 0; i < nitems; i++) {
	    strcpy(buf, dlist[i]);
	    if (!isdir(buf)) {
		xv_set(list_item, PANEL_LIST_INSERT, nadd,
		       PANEL_LIST_STRING, nadd, buf, NULL);
		nadd++;
	    }
	}
	xv_set(list_item, XV_SHOW, TRUE, NULL);
    } else {
	errwin("Can't open directory");
    }
}

/*
 * update the state of the files popup
 */
static void update_files(mask)
    char *mask;
{
    int d1, d2, i = 0;

    update_file_list(mask, dirbuf,
		     files_list_list_item,
		     files_dir_msg_item);
    while (curtype != set_types[i] && set_types[i] >= 0) {
	i++;
    }
    if (set_types[i] == -1) {
	curtype = XY;
	d1 = 0;
    } else {
	d1 = i;
    }
    xv_set(files_type_choice_item, PANEL_VALUE, d1, NULL);
    switch (cursource) {
    case DISK:
	d2 = 0;
	break;
    case PIPE:
	d2 = 1;
	break;
    default:
	d2 = 0;
	cursource = DISK;
	break;
    }
    xv_set(files_source_choice_item, PANEL_VALUE, d2, NULL);
}

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int files_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(files_frame, WIN_SHOW, FALSE, 0);
    return XV_OK;
}

/*ARGSUSED*/
static void files_list_notify_proc(item, s, cd, op, e)
    Panel_item item;
    char *s;
    caddr_t cd;
    Panel_list_op op;
    Event *e;
{
    static char buf[256];
    char stmp[MAXPATHLEN];
    FILE *p;
    XEvent *event = e->ie_xevent;

    if (s == NULL) {
	return;
    }
    strcpy(stmp, s);
    if (event_is_down(e) && op == PANEL_LIST_OP_SELECT) {
	if (double_click(event)) {
	    if (strcmp(buf, s)) {
		return;		/* string is different */
	    } else {
/* check to see if this is a directory */
		if (isdir(stmp)) {
		    if (!my_chdir(stmp)) {
			update_files(NULL);
			xv_set(files_file_text_item, PANEL_VALUE, "", NULL);
		    }
		} else {
		    if ((p = fopen(buf, "r")) == NULL) {
			char tmpbuf[256];

			sprintf(tmpbuf, "Unable to open file %s", stmp);
			errwin(tmpbuf);
		    } else {
			fclose(p);
			xv_set(files_file_text_item, PANEL_VALUE, stmp, NULL);
			files_accept_notify_proc();
		    }
		}
	    }
	    strcpy(buf, stmp);
	} else {
	    strcpy(buf, stmp);
	    if ((p = fopen(buf, "r")) == NULL) {
		sprintf(buf, "Unable to open file %s", stmp);
		errwin(buf);
	    } else {
		fclose(p);
		xv_set(files_file_text_item, PANEL_VALUE, stmp, NULL);
	    }
	}
    }
}

/*ARGSUSED*/
static Panel_setting files_file_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0, d2;
    char buf[MAXPATHLEN], buf2[MAXPATHLEN];

    strcpy(buf, (char *) xv_get(files_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    d2 = (int) xv_get(files_source_choice_item, PANEL_VALUE);
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!d2 && !my_chdir(buf)) {
	update_files(NULL);
    } else {
	if (d2) {
	    files_accept_notify_proc();
	} else {
	    update_files(buf);
	    if ((int) xv_get(files_list_list_item, PANEL_LIST_NROWS) == 1) {
		xv_set(files_file_text_item, PANEL_VALUE, buf, 0);
		files_accept_notify_proc(item, event);
	    }
	}
    }
    return PANEL_NONE;
}

static char filesfname[MAXPATHLEN];
static int readtograph;

/*ARGSUSED*/
static int files_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0;
    char buf[MAXPATHLEN];
    int graphno;
    int d1, d2;

    strcpy(buf, (char *) xv_get(files_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    d2 = (int) xv_get(files_source_choice_item, PANEL_VALUE);
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!d2 && !my_chdir(buf)) {
	update_files(NULL);
    } else {
	d1 = (int) xv_get(files_type_choice_item, PANEL_VALUE);

	switch (d1) {
	case 0:
	    curtype = XY;
	    break;
	case 1:
	    curtype = NXY;
	    break;
	case 2:
	    curtype = IHL;
	    break;
	case 3:
	    curtype = BIN;
	    break;
	case 4:
	    curtype = XYDX;
	    break;
	case 5:
	    curtype = XYDY;
	    break;
	case 6:
	    curtype = XYDXDX;
	    break;
	case 7:
	    curtype = XYDYDY;
	    break;
	case 8:
	    curtype = XYDXDY;
	    break;
	case 9:
	    curtype = XYZ;
	    break;
	case 10:
	    curtype = XYHILO;
	    break;
	case 11:
	    curtype = XYRT;
	    break;
	}
	switch (d2) {
	case 0:
	    cursource = DISK;
	    break;
	case 1:
	    cursource = PIPE;
	    break;
	}
	strcpy(filesfname, (char *) xv_get(files_file_text_item, PANEL_VALUE));
	graphno = xv_get(files_graph_choice_item, PANEL_VALUE) - 1;
	if (graphno == -1) {
	    graphno = cg;
	}
	if (g[graphno].active == OFF) {
	    set_graph_active(graphno);
	}
	/*
	 * step_read = (int) xv_get(files_file_text_item, PANEL_VALUE); if
	 * (step_read) { if (getdata_step(graphno, filesfname, cursource,
	 * curtype)) { drawgraph(); } } else { if (getdata(graphno,
	 * filesfname, cursource, curtype)) { drawgraph(); } }
	 */
	sprintf(buf, "Opening %s", filesfname);
	xv_set(files_dir_msg_item, PANEL_LABEL_STRING, buf, NULL);
	set_wait_cursor(1);
	if (getdata(graphno, filesfname, cursource, curtype)) {
	    if (autoscale_onread) {
		autoscale_proc();
	    }
	    drawgraph();
	}
	unset_wait_cursor(1);
	getcwd(dirbuf, MAXPATHLEN);
	xv_set(files_dir_msg_item, PANEL_LABEL_STRING, dirbuf, NULL);
    }
    return XV_OK;
}

/*
 * eventually will allow sets to be read one at a time
 */
static int files_skip_notify_proc()
{
}

static int files_next_notify_proc()
{
}
