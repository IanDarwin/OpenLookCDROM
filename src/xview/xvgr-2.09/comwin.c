/* $Id: comwin.c,v 1.16 92/07/19 10:15:21 pturner Exp Locker: pturner $
 *
 * command line interpreter
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

/* all declared in pars.yacc */
extern int gotbatch;
extern int gotparams;
extern int gotread;
extern int readsrc, readtype;
extern char batchfile[];
extern char paramfile[];
extern char readfile[];
extern double result;

extern Frame main_frame;

Frame com_frame = (Frame) 0;
Panel com_panel;

/*
 * Panel item declarations
 */
Panel_item com_list_list_item;
Panel_item com_command_text_item;

/*
 * Event and Notify proc declarations
 */
static int com_Done_notify_proc();
static void com_list_notify_proc();
void create_comw_frame();
void create_comr_frame();
static int com_clear_notify_proc();
static int com_replay_notify_proc();
static Panel_setting com_command_notify_proc();
static void update_comr();

/*
 * Create the com Frame and the com Panel
 */
void create_com_frame()
{

    if (com_frame) {
	xv_set(com_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    com_frame = (Frame) xv_create(main_frame, FRAME,
				  FRAME_LABEL, "Commands",
				  NULL);
    com_panel = (Panel) xv_create(com_frame, PANEL,
				  XV_HELP_DATA, "xvgr:com_panel",
				  PANEL_LAYOUT, PANEL_VERTICAL,
				  NULL);
    com_list_list_item = (Panel_item) xv_create(com_panel, PANEL_LIST,
					   XV_HELP_DATA, "xvgr:com_history",
					      PANEL_LABEL_STRING, "History",
				    PANEL_NOTIFY_PROC, com_list_notify_proc,
						PANEL_LIST_DISPLAY_ROWS, 5,
						PANEL_LIST_WIDTH, 300,
						NULL);
    (void) xv_create(com_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:com_save",
		     PANEL_LABEL_STRING, "Save...",
		     PANEL_NOTIFY_PROC, create_comw_frame,
		     XV_X, xv_col(com_panel, 0), XV_Y, xv_row(com_panel, 5),
		     NULL);
    (void) xv_create(com_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:com_read",
		     PANEL_LABEL_STRING, "Read...",
		     PANEL_NOTIFY_PROC, create_comr_frame,
		     XV_X, xv_col(com_panel, 9), XV_Y, xv_row(com_panel, 5),
		     NULL);
    (void) xv_create(com_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:com_clear",
		     PANEL_LABEL_STRING, "Clear",
		     PANEL_NOTIFY_PROC, com_clear_notify_proc,
		     XV_X, xv_col(com_panel, 18), XV_Y, xv_row(com_panel, 5),
		     NULL);
    (void) xv_create(com_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:com_replay",
		     PANEL_LABEL_STRING, "Replay",
		     PANEL_NOTIFY_PROC, com_replay_notify_proc,
		     XV_X, xv_col(com_panel, 27), XV_Y, xv_row(com_panel, 5),
		     NULL);
    (void) xv_create(com_panel, PANEL_BUTTON,
		     XV_HELP_DATA, "xvgr:com_done",
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, com_Done_notify_proc,
		     XV_X, xv_col(com_panel, 20), XV_Y, xv_row(com_panel, 7),
		     NULL);
    com_command_text_item = (Panel_item) xv_create(com_panel, PANEL_TEXT,
					   XV_HELP_DATA, "xvgr:com_command",
				 PANEL_NOTIFY_PROC, com_command_notify_proc,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 35,
					     PANEL_LABEL_STRING, "Command:",
		     XV_X, xv_col(com_panel, 1), XV_Y, xv_row(com_panel, 6),
						   NULL);
    window_fit(com_panel);
    window_fit(com_frame);
    xv_set(com_frame, WIN_SHOW, TRUE, NULL);
}				/* end create_com_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int com_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(com_frame, WIN_SHOW, FALSE, NULL);
}

/*ARGSUSED*/
static void com_list_notify_proc(item, s, cd, op, e)
    Panel_item item;
    char *s;
    caddr_t cd;
    Panel_list_op op;
    Event *e;
{
    static char buf[256];
    char stmp[256];
    XEvent *event = e->ie_xevent;

    if (s == NULL) {
	return;
    }
    strcpy(stmp, s);
    if (event_is_down(e) && op == PANEL_LIST_OP_SELECT) {
	if (double_click(event)) {
	    if (strcmp(buf, stmp)) {
		return;		/* string is different */
	    } else {
		xv_set(com_command_text_item, PANEL_VALUE, stmp, NULL);
		com_command_notify_proc();
	    }
	    strcpy(buf, stmp);
	} else {
	    xv_set(com_command_text_item, PANEL_VALUE, stmp, NULL);
	    strcpy(buf, stmp);
	}
    }
}

/*ARGSUSED*/
static int com_clear_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i;
    int n = (int) xv_get(com_list_list_item, PANEL_LIST_NROWS);

    xv_set(com_list_list_item, XV_SHOW, FALSE, NULL);
    for (i = 0; i < n; i++) {
	xv_set(com_list_list_item, PANEL_LIST_DELETE, 0, NULL);
    }
    xv_set(com_list_list_item, XV_SHOW, TRUE, NULL);
    return XV_OK;
}

/*ARGSUSED*/
static Panel_setting com_command_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    static int errpos;
    char buf[256];
    int n;

    strcpy(buf, (char *) xv_get(com_command_text_item, PANEL_VALUE));
    if (strlen(buf) != 0) {
	n = (int) xv_get(com_list_list_item, PANEL_LIST_NROWS);
	xv_set(com_list_list_item, PANEL_LIST_INSERT, n, PANEL_LIST_STRING, n, buf, NULL);
	xv_set(com_list_list_item, PANEL_LIST_SELECT, n, TRUE, NULL);
	errpos = 0;
	fixupstr(buf);
	scanner(buf, getx(cg, curset), gety(cg, curset), getsetlength(cg, curset), ax, bx, cx, dx, MAXARR, 0, curset, &errpos);

/*
 * the yacc grammar is not re-entrant so run these separately
*/
	if (gotbatch && batchfile[0]) {
	    runbatch(batchfile);
	    gotbatch = 0;
	} else if (gotparams && paramfile[0]) {
	    if (!getparms(cg, paramfile)) {
	    }
	    gotparams = 0;
	} else if (gotread && readfile[0]) {
	    if (getdata(cg, readfile, readsrc, readtype)) {
		drawgraph();
	    }
	    gotread = 0;
	}
	xv_set(com_command_text_item, PANEL_VALUE, "", NULL);
    }
    return PANEL_NONE;
}

/*ARGSUSED*/
static int com_replay_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    static int errpos, errcount;
    char buf[256];
    int i, n = (int) xv_get(com_list_list_item, PANEL_LIST_NROWS);

    errcount = 0;
    for (i = 0; i < n; i++) {
	xv_set(com_list_list_item, PANEL_LIST_SELECT, i, TRUE, NULL);
	strcpy(buf, (char *) xv_get(com_list_list_item, PANEL_LIST_STRING, i));
	errpos = 0;
	fixupstr(buf);
	scanner(buf, getx(cg, curset), gety(cg, curset), getsetlength(cg, curset), ax, bx, cx, dx, MAXARR, 0, curset, &errpos);
	if (errpos) {
	    errcount++;
	}
	if (errcount > 3) {
	    if (yesno("Lots of errors, abort?", "Press YES or NO", "YES", "NO")) {
		break;
	    } else {
		errcount = 0;
	    }
	}
/*
 * the yacc grammar is not re-entrant so run these separately
*/
	if (gotbatch && batchfile[0]) {
	    runbatch(batchfile);
	    gotbatch = 0;
	} else if (gotparams && paramfile[0]) {
	    if (!getparms(cg, paramfile)) {
	    }
	    gotparams = 0;
	} else if (gotread && readfile[0]) {
	    if (getdata(cg, readfile, readsrc, readtype)) {
		drawgraph();
	    }
	    gotread = 0;
	}
    }
    return XV_OK;
}

Frame comr_frame = (Frame) 0;
Panel comr_panel;

/*
 * Panel item declarations
 */
static Panel_item comr_list_list_item;
static Panel_item comr_file_text_item;
Panel_item comr_dir_msg_item;

static char dirbuf[MAXPATHLEN];

/*
 * Event and Notify proc declarations
 */
static int comr_Done_notify_proc();
static void comr_list_notify_proc();
static Panel_setting comr_file_notify_proc();
static int comr_accept_notify_proc();

void create_comr_frame()
{
    FILE *p;
    int i = 0;
    char buf[256];

    getcwd(dirbuf, MAXPATHLEN);
    if (comr_frame) {
	xv_set(comr_dir_msg_item, PANEL_LABEL_STRING, dirbuf, NULL);
	xv_set(comr_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    comr_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Read commands",
				   NULL);
    comr_panel = (Panel) xv_create(comr_frame, PANEL,
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    comr_dir_msg_item = (Panel_item) xv_create(comr_panel, PANEL_MESSAGE,
					       PANEL_LABEL_STRING, dirbuf,
					       NULL);
    comr_list_list_item = (Panel_item) xv_create(comr_panel, PANEL_LIST,
					  PANEL_LABEL_STRING, "File filter",
				   PANEL_NOTIFY_PROC, comr_list_notify_proc,
						 PANEL_LIST_DISPLAY_ROWS, 5,
		   XV_X, xv_col(comr_panel, 0), XV_Y, xv_row(comr_panel, 1),
						 NULL);
    comr_file_text_item = (Panel_item) xv_create(comr_panel, PANEL_TEXT,
				   PANEL_NOTIFY_PROC, comr_file_notify_proc,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 20,
						 PANEL_LABEL_STRING, "File:",
						 XV_X, xv_col(comr_panel, 0),
						 NULL);
    (void) xv_create(comr_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, comr_Done_notify_proc,
		  XV_X, xv_col(comr_panel, 15), XV_Y, xv_row(comr_panel, 7),
		     NULL);
    (void) xv_create(comr_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, comr_accept_notify_proc,
		   XV_X, xv_col(comr_panel, 5), XV_Y, xv_row(comr_panel, 7),
		     NULL);
    window_fit(comr_panel);
    window_fit(comr_frame);
    update_comr(NULL);
    xv_set(comr_frame, WIN_SHOW, TRUE, NULL);
}


/*
 * Update proc
 */
static void update_comr(mask)
    char *mask;
{
    update_file_list(mask, dirbuf,
		     comr_list_list_item,
		     comr_dir_msg_item);
}

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int comr_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(comr_frame, WIN_SHOW, FALSE, NULL);
    return XV_OK;
}

/*ARGSUSED*/
static void comr_list_notify_proc(item, s, cd, op, e)
    Panel_item item;
    char *s;
    caddr_t cd;
    Panel_list_op op;
    Event *e;
{
    static char buf[MAXPATHLEN];
    char stmp[MAXPATHLEN];
    FILE *p;
    XEvent *event = e->ie_xevent;

    if (s == NULL) {
	printf("s == NULL\n");
	return;
    }
    strcpy(stmp, s);
    if (event_is_down(e) && op == PANEL_LIST_OP_SELECT) {
	if (double_click(event)) {
	    if (strcmp(buf, stmp)) {
		return;		/* string is different */
	    } else {
/* check to see if this is a directory */
		if (isdir(stmp)) {
		    if (!my_chdir(stmp)) {
			update_comr(NULL);
			xv_set(comr_file_text_item, PANEL_VALUE, "", 0);
		    }
		} else {
		    if ((p = fopen(buf, "r")) == NULL) {
			char tmpbuf[256];

			sprintf(tmpbuf, "Unable to open file %s", stmp);
			errwin(tmpbuf);
		    } else {
			fclose(p);
			xv_set(comr_file_text_item, PANEL_VALUE, stmp, 0);
			comr_accept_notify_proc();
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
		xv_set(comr_file_text_item, PANEL_VALUE, stmp, 0);
	    }
	}
    }
}

/*ARGSUSED*/
static Panel_setting comr_file_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    char buf[MAXPATHLEN];

    strcpy(buf, (char *) xv_get(comr_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!my_chdir(buf)) {
	update_comr(NULL);
    } else {
	update_comr(buf);
	if ((int) xv_get(comr_list_list_item, PANEL_LIST_NROWS) == 1) {
	    xv_set(comr_file_text_item, PANEL_VALUE, buf, 0);
	}
    }
    return PANEL_NONE;
}

/*ARGSUSED*/
static int comr_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0;
    char buf[256];
    FILE *p;
    char fname[256];
    void getcommands();

    strcpy(buf, (char *) xv_get(comr_file_text_item, PANEL_VALUE));
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!my_chdir(buf)) {
	update_comr(NULL);
    } else {
	getcommands(buf);
    }
    return XV_OK;
}

void getcommands(s)
    char *s;
{
    char buf[256];
    int i;
    FILE *fp = fopen(s, "r");
    int n = (int) xv_get(com_list_list_item, PANEL_LIST_NROWS);

    if (fp == NULL) {
	errwin("Unable to open file");
    }
    xv_set(com_list_list_item, XV_SHOW, FALSE, NULL);
    for (i = 0; i < n; i++) {
	xv_set(com_list_list_item, PANEL_LIST_DELETE, 0, NULL);
    }
    i = 0;
    while (fgets(buf, 255, fp) != NULL) {
	if (buf[0] != '#') {
	    buf[strlen(buf) - 1] = 0;
	    xv_set(com_list_list_item, PANEL_LIST_INSERT, i, PANEL_LIST_STRING, i, buf, NULL);
	    i++;
	}
    }
    fclose(fp);
    xv_set(com_list_list_item, XV_SHOW, TRUE, NULL);
    xv_set(comr_frame, WIN_SHOW, FALSE, NULL);
}

Frame comw_frame = (Frame) 0;
Panel comw_panel;
Panel_item comw_dir_msg_item;

/*
 * Panel item declarations
 */
Panel_item comw_text_item;
Panel_item comw_choice_item;

/*
 * Event and Notify proc declarations
 */
static comw_Done_notify_proc();
static comw_apply_notify_proc();

/*
 * Create the wparam Frame and the wparam Panel
 */
void create_comw_frame()
{
    getcwd(dirbuf, MAXPATHLEN);
    if (comw_frame) {
	xv_set(comw_dir_msg_item, PANEL_LABEL_STRING, dirbuf, NULL);
	xv_set(comw_frame, WIN_SHOW, TRUE, NULL);
	return;
    }
    comw_frame = (Frame) xv_create(main_frame, FRAME,
				   FRAME_LABEL, "Save commands",
				   NULL);
    comw_panel = (Panel) xv_create(comw_frame, PANEL,
				   PANEL_LAYOUT, PANEL_VERTICAL,
				   NULL);
    comw_dir_msg_item = (Panel_item) xv_create(comw_panel, PANEL_MESSAGE,
					       PANEL_LABEL_STRING, dirbuf,
					       NULL);
    comw_text_item = (Panel_item) xv_create(comw_panel, PANEL_TEXT,
					    PANEL_LAYOUT, PANEL_HORIZONTAL,
					    PANEL_VALUE_DISPLAY_LENGTH, 30,
				   PANEL_LABEL_STRING, "Write commands to:",
				      PANEL_VALUE_X, xv_col(comw_panel, 20),
					    NULL);

    (void) xv_create(comw_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, comw_Done_notify_proc,
		     XV_X, xv_col(comw_panel, 20),
		     XV_Y, xv_row(comw_panel, 2),
		     NULL);
    (void) xv_create(comw_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, comw_apply_notify_proc,
		     XV_X, xv_col(comw_panel, 0),
		     XV_Y, xv_row(comw_panel, 2),
		     NULL);
    window_fit(comw_panel);
    window_fit(comw_frame);
    xv_set(comw_frame, WIN_SHOW, TRUE, NULL);
}

static comw_Done_notify_proc()
{
    xv_set(comw_frame, WIN_SHOW, FALSE, NULL);
}

static int comw_apply_notify_proc()
{
    int i;
    char s[MAXPATHLEN];

    strcpy(s, (char *) xv_getstr(comw_text_item));
    if (s[0] == '~') {
	expand_tilde(s);
    }
    if (isdir(s)) {
	if (my_chdir(s)) {
	    errwin("Can't change directory");
	}
    } else if (!fexists(s)) {
	FILE *pp = fopen(s, "w");

	if (pp != NULL) {
	    int n = (int) xv_get(com_list_list_item, PANEL_LIST_NROWS);

	    fprintf(pp, "# Xvgr release 2 command dump\n");
	    for (i = 0; i < n; i++) {
		strcpy(s, (char *) xv_get(com_list_list_item, PANEL_LIST_STRING, i));
		fprintf(pp, "%s\n", s);
	    }
	    fclose(pp);
	} else {
	    errwin("Unable to open file");
	}
    }
}
