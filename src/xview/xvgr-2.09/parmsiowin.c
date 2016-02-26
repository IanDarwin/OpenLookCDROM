/* $Id: parmsiowin.c,v 1.15 92/07/19 10:15:20 pturner Exp Locker: pturner $
 *
 * read and write parameters popups
 *
 * has the same problem as the files popup, use of popen("ls etc.
 * to get list of files
 *
 */

#include <stdio.h>
#include <sys/param.h>
#include <xview/xview.h>
#include <xview/panel.h>

#include "globals.h"

extern Frame main_frame;

Frame parmsr_frame = (Frame) 0;
Panel parmsr_panel;

/*
 * Panel item declarations
 */
static Panel_item parmsr_list_list_item;
static Panel_item parmsr_file_text_item;
static Panel_item parmsr_graph_choice_item;

/*
 * Event and Notify proc declarations
 */
static int parmsr_Done_notify_proc();
static void parmsr_list_notify_proc();
static Panel_setting parmsr_file_notify_proc();
static void parmsr_graph_notify_proc();
static int parmsr_accept_notify_proc();
static void update_parmsr();
Panel_item parmsr_dir_msg_item;
Panel_item parmsw_dir_msg_item;

static char dirbuf[MAXPATHLEN];

/*
 * Create the parmsr Frame and the parmsr Panel
 */
void create_parmsr_frame()
{
    FILE *p;
    int i = 0;
    char buf[256];

    getcwd(dirbuf, MAXPATHLEN);
    if (parmsr_frame) {
	xv_set(parmsr_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    parmsr_frame = (Frame) xv_create(main_frame, FRAME,
				     FRAME_LABEL, "Read parameters",
				     NULL);
    parmsr_panel = (Panel) xv_create(parmsr_frame, PANEL,
				     PANEL_LAYOUT, PANEL_VERTICAL,
				     NULL);
    parmsr_dir_msg_item = (Panel_item) xv_create(parmsr_panel, PANEL_MESSAGE,
						 PANEL_LABEL_STRING, dirbuf,
						 NULL);
    parmsr_list_list_item = (Panel_item) xv_create(parmsr_panel, PANEL_LIST,
					     PANEL_LABEL_STRING, "Contents",
				 PANEL_NOTIFY_PROC, parmsr_list_notify_proc,
						 PANEL_LIST_DISPLAY_ROWS, 5,
	       XV_X, xv_col(parmsr_panel, 0), XV_Y, xv_row(parmsr_panel, 1),
						   NULL);
    parmsr_graph_choice_item = (Panel_item) xv_create(parmsr_panel, PANEL_CHOICE_STACK,
				       PANEL_LABEL_STRING, "Read to graph:",
						      PANEL_CHOICE_STRINGS,
						      "Default", "Current",
			   "0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
						      NULL,
				    PANEL_VALUE_X, xv_col(parmsr_panel, 18),
						      NULL);
    parmsr_file_text_item = (Panel_item) xv_create(parmsr_panel, PANEL_TEXT,
				 PANEL_NOTIFY_PROC, parmsr_file_notify_proc,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 20,
						PANEL_LABEL_STRING, "File:",
					      XV_X, xv_col(parmsr_panel, 0),
						   NULL);
    (void) xv_create(parmsr_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, parmsr_Done_notify_proc,
	      XV_X, xv_col(parmsr_panel, 15), XV_Y, xv_row(parmsr_panel, 8),
		     NULL);
    (void) xv_create(parmsr_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, parmsr_accept_notify_proc,
	       XV_X, xv_col(parmsr_panel, 5), XV_Y, xv_row(parmsr_panel, 8),
		     NULL);
    window_fit(parmsr_panel);
    window_fit(parmsr_frame);
    update_parmsr(NULL);
    if (plfile[0]) {
	xv_set(parmsr_file_text_item, PANEL_VALUE, plfile, NULL);
    }
    xv_set(parmsr_frame, WIN_SHOW, TRUE, NULL);
}				/* end create_parmsr_panel */

static void update_parmsr(mask)
    char *mask;
{
    int d1, d2, i = 0;

    update_file_list(mask, dirbuf,
		     parmsr_list_list_item,
		     parmsr_dir_msg_item);
}

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int parmsr_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(parmsr_frame, WIN_SHOW, FALSE, NULL);
    return XV_OK;
}

/*ARGSUSED*/
static void parmsr_list_notify_proc(item, s, cd, op, e)
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
			update_parmsr(NULL);
			xv_set(parmsr_file_text_item, PANEL_VALUE, "", NULL);
		    }
		} else {
		    if ((p = fopen(buf, "r")) == NULL) {
			char tmpbuf[256];

			sprintf(tmpbuf, "Unable to open file %s", stmp);
			errwin(tmpbuf);
		    } else {
			fclose(p);
			xv_set(parmsr_file_text_item, PANEL_VALUE, stmp, NULL);
			parmsr_accept_notify_proc();
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
		xv_set(parmsr_file_text_item, PANEL_VALUE, stmp, NULL);
	    }
	}
    }
}

/*ARGSUSED*/
static Panel_setting parmsr_file_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0;
    char buf[MAXPATHLEN];

    strcpy(buf, (char *) xv_get(parmsr_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!my_chdir(buf)) {
	update_parmsr(NULL);
    } else {
	update_parmsr(buf);
	if ((int) xv_get(parmsr_list_list_item, PANEL_LIST_NROWS) == 1) {
	    xv_set(parmsr_file_text_item, PANEL_VALUE, buf, 0);
	    parmsr_accept_notify_proc();
	}
    }
    return PANEL_NONE;
}

static char fname[MAXPATHLEN];

/*ARGSUSED*/
static int parmsr_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0;
    char buf[MAXPATHLEN];
    int graphno;

    strcpy(buf, (char *) xv_get(parmsr_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!my_chdir(buf)) {
	update_parmsr(NULL);
    } else {
	strcpy(fname, (char *) xv_get(parmsr_file_text_item, PANEL_VALUE));
	graphno = xv_get(parmsr_graph_choice_item, PANEL_VALUE) - 1;
	if (graphno == -2 || graphno == -1) {
	    graphno = cg;
	}
	getparms(graphno, fname);
	drawgraph();
    }
    return XV_OK;
}

/*
 * write paramsters popup
 */
Frame wparam_frame = (Frame) 0;
Panel wparam_panel;

/*
 * Panel item declarations
 */
Panel_item wparam_text_item;
Panel_item wparam_choice_item;
Panel_item wparam_props_item;

/*
 * Event and Notify proc declarations
 */
static wparam_Done_notify_proc();
static wparam_apply_notify_proc();

/*
 * Create the wparam Frame and the wparam Panel
 */
void create_wparam_frame()
{
    getcwd(dirbuf, MAXPATHLEN);
    if (wparam_frame) {
	xv_set(wparam_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    wparam_frame = (Frame) xv_create(main_frame, FRAME,
				     FRAME_LABEL, "Write parameters",
				     NULL);
    wparam_panel = (Panel) xv_create(wparam_frame, PANEL,
				     PANEL_LAYOUT, PANEL_VERTICAL,
				     NULL);
    parmsw_dir_msg_item = (Panel_item) xv_create(wparam_panel, PANEL_MESSAGE,
						 PANEL_LABEL_STRING, dirbuf,
						 NULL);

    wparam_choice_item = (Panel_item) xv_create(wparam_panel, PANEL_CHOICE_STACK,
					  PANEL_LABEL_STRING, "From graph:",
						PANEL_CHOICE_STRINGS,
					 "Current", "0", "1", "2", "3", "4",
				      "5", "6", "7", "8", "9", "All active",
						NULL,
				    PANEL_VALUE_X, xv_col(wparam_panel, 23),
						NULL);

/*
    wparam_props_item = (Panel_item) xv_create(wparam_panel, PANEL_CHECK_BOX,
				     PANEL_LABEL_STRING, "Objects to save:",
					       PANEL_CHOOSE_ONE, FALSE,
					       PANEL_CHOICE_STRINGS,
			  "Graphs", "Axes", "Regions", "Sets", "Annotation",
					       NULL,
				    PANEL_VALUE_X, xv_col(wparam_panel, 23),
					       PANEL_VALUE, 31,
					       NULL);
*/
    wparam_text_item = (Panel_item) xv_create(wparam_panel, PANEL_TEXT,
					      PANEL_LAYOUT, PANEL_HORIZONTAL,
					      PANEL_VALUE_DISPLAY_LENGTH, 30,
				 PANEL_LABEL_STRING, "Write parameters to:",
				    PANEL_VALUE_X, xv_col(wparam_panel, 23),
					      NULL);

    (void) xv_create(wparam_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, wparam_Done_notify_proc,
		     XV_X, xv_col(wparam_panel, 15),
		     XV_Y, xv_row(wparam_panel, 4),
		     NULL);
    (void) xv_create(wparam_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, wparam_apply_notify_proc,
		     XV_X, xv_col(wparam_panel, 5),
		     XV_Y, xv_row(wparam_panel, 4),
		     NULL);
    window_fit(wparam_panel);
    window_fit(wparam_frame);
    xv_set(wparam_frame, WIN_SHOW, TRUE, 0);
}

static wparam_Done_notify_proc()
{
    xv_set(wparam_frame, WIN_SHOW, FALSE, 0);
}

static int wparam_apply_notify_proc()
{
    int wparamno = (int) xv_get(wparam_choice_item, PANEL_VALUE);
    char s[256];

    wparamno--;
    strcpy(s, (char *) xv_getstr(wparam_text_item));
    if (!fexists(s)) {
	FILE *pp = fopen(s, "w");

	if (pp != NULL) {
	    if (wparamno == -1) {
		wparamno = cg;
		putparms(wparamno, pp, 0);
		fclose(pp);
	    } else if (wparamno == MAXGRAPH) {
		putparms(-1, pp, 0);
		fclose(pp);
	    } else {
		putparms(wparamno, pp, 0);
		fclose(pp);
	    }
	} else {
	    errwin("Unable to open file");
	}
    }
}
