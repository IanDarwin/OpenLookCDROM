/* $Id: blockwin.c,v 1.9 92/07/19 10:15:18 pturner Exp Locker: pturner $
 *
 * block data Panel
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

extern double *blockdata[];
extern int blocklen;
extern int blockncols;

static char dirbuf[MAXPATHLEN];
char *getcwd();

static int blocksrc = DISK;

extern Frame main_frame;

Frame block_frame = (Frame) 0;
Panel block_panel;

/*
 * Panel item declarations
 */
static Panel_item block_source_choice_item;
static Panel_item block_list_list_item;
Panel_item block_dir_msg_item;
static Panel_item block_file_text_item;

/*
 * Event and Notify proc declarations
 */
static int block_Done_notify_proc();
static void block_type_notify_proc();
static void block_source_notify_proc();
static void block_list_notify_proc();
static Panel_setting block_file_notify_proc();
static void block_graph_notify_proc();
static int block_accept_notify_proc();
static void update_files();

/*
 * Create the files Frame and the files Panel
 */
void create_block_frame()
{
    FILE *p;
    int i = 0;

    getcwd(dirbuf, MAXPATHLEN);

    if (block_frame) {
	xv_set(block_dir_msg_item, PANEL_LABEL_STRING, dirbuf, NULL);
	xv_set(block_frame, WIN_SHOW, TRUE, 0);
	return;
    }
    block_frame = (Frame) xv_create(main_frame, FRAME,
				    FRAME_LABEL, "Block data",
				    NULL);
    block_panel = (Panel) xv_create(block_frame, PANEL,
				    PANEL_LAYOUT, PANEL_VERTICAL,
				    NULL);
    block_dir_msg_item = (Panel_item) xv_create(block_panel, PANEL_MESSAGE,
						PANEL_LABEL_STRING, dirbuf,
						NULL);
    block_list_list_item = (Panel_item) xv_create(block_panel, PANEL_LIST,
					     PANEL_LABEL_STRING, "Contents",
					    XV_HELP_DATA, "xvgr:block_list",
				  PANEL_NOTIFY_PROC, block_list_notify_proc,
						  PANEL_LIST_DISPLAY_ROWS, 5,
						  NULL);
    block_source_choice_item = (Panel_item) xv_create(block_panel, PANEL_CHOICE,
					   PANEL_LABEL_STRING, "Read from:",
					    XV_HELP_DATA, "xvgr:block_from",
						      PANEL_CHOICE_STRINGS,
						      "Disk",
						      "Pipe",
						      NULL,
				     PANEL_VALUE_X, xv_col(block_panel, 18),
						      NULL);
    block_file_text_item = (Panel_item) xv_create(block_panel, PANEL_TEXT,
				  PANEL_NOTIFY_PROC, block_file_notify_proc,
					     PANEL_LAYOUT, PANEL_HORIZONTAL,
					     PANEL_VALUE_DISPLAY_LENGTH, 20,
						PANEL_LABEL_STRING, "File:",
					    XV_HELP_DATA, "xvgr:block_file",
				      PANEL_VALUE_X, xv_col(block_panel, 8),
						  NULL);
    (void) xv_create(block_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Accept",
		     PANEL_NOTIFY_PROC, block_accept_notify_proc,
		     XV_HELP_DATA, "xvgr:block_accept",
		     XV_X, xv_col(block_panel, 5),
		     XV_Y, xv_row(block_panel, 8),
		     NULL);
    (void) xv_create(block_panel, PANEL_BUTTON,
		     PANEL_LABEL_STRING, "Done",
		     PANEL_NOTIFY_PROC, block_Done_notify_proc,
		     XV_HELP_DATA, "xvgr:block_done",
		     XV_X, xv_col(block_panel, 17),
		     XV_Y, xv_row(block_panel, 8),
		     NULL);
    window_fit(block_panel);
    window_fit(block_frame);
    update_files(NULL);
    xv_set(block_frame, WIN_SHOW, TRUE, 0);
}				/* end create_block_panel */

/*
 * Notify and event procs
 */

/*ARGSUSED*/
static int block_Done_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    xv_set(block_frame, WIN_SHOW, FALSE, 0);
    return XV_OK;
}

static void update_files(mask)
    char *mask;
{
    int d2, i = 0;
    update_file_list(mask, dirbuf, 
		block_list_list_item, 
		block_dir_msg_item);
    switch (blocksrc) {
    case DISK:
	d2 = 0;
	break;
    case PIPE:
	d2 = 1;
	break;
    default:
	d2 = 0;
	blocksrc = DISK;
	break;
    }
    xv_set(block_source_choice_item, PANEL_VALUE, d2, NULL);
}

/*ARGSUSED*/
static void block_list_notify_proc(item, s, cd, op, e)
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
			xv_set(block_file_text_item, PANEL_VALUE, "", NULL);
		    }
		} else {
		    if ((p = fopen(buf, "r")) == NULL) {
			char tmpbuf[256];

			sprintf(tmpbuf, "Unable to open file %s", stmp);
			errwin(tmpbuf);
		    } else {
			fclose(p);
			xv_set(block_file_text_item, PANEL_VALUE, stmp, NULL);
			block_accept_notify_proc();
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
		xv_set(block_file_text_item, PANEL_VALUE, stmp, NULL);
	    }
	}
    }
}

/*ARGSUSED*/
static Panel_setting block_file_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0, d2;
    char buf[MAXPATHLEN];

    strcpy(buf, (char *) xv_get(block_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    d2 = (int) xv_get(block_source_choice_item, PANEL_VALUE);
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!d2 && !my_chdir(buf)) {
	update_files(NULL);
    } else {
	if (d2) {
	    block_accept_notify_proc();
	} else {
	    update_files(buf);
	    if ((int) xv_get(block_list_list_item, PANEL_LIST_NROWS) == 1) {
		xv_set(block_file_text_item, PANEL_VALUE, buf, 0);
	    }
	}
    }
    return PANEL_NONE;
}

static char filesfname[MAXPATHLEN];

/*ARGSUSED*/
static int block_accept_notify_proc(item, event)
    Panel_item item;
    Event *event;
{
    int i = 0;
    char buf[MAXPATHLEN];
    int graphno;
    int d1, d2;

    strcpy(buf, (char *) xv_get(block_file_text_item, PANEL_VALUE));
    if (strlen(buf) == 0) {
	return PANEL_NONE;
    }
    d2 = (int) xv_get(block_source_choice_item, PANEL_VALUE);
    if (buf[0] == '~') {
	expand_tilde(buf);
    }
    if (!d2 && !my_chdir(buf)) {
	update_files(NULL);
    } else {
	switch (d2) {
	case 0:
	    blocksrc = DISK;
	    break;
	case 1:
	    blocksrc = PIPE;
	    break;
	}
	strcpy(filesfname, (char *) xv_get(block_file_text_item, PANEL_VALUE));
	if (getdata(cg, filesfname, blocksrc, 5)) {
	    if (blocklen == 0) {
		errwin("Block data length = 0");
	    } else if (blockncols == 0) {
		errwin("Number of columns in block data = 0");
	    } else {
		xv_set(block_frame, WIN_SHOW, FALSE, 0);
		create_eblock_frame();
	    }
	}
    }
    return XV_OK;
}
