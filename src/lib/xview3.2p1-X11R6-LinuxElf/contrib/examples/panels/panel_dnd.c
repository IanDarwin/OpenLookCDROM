/*
 * panel_dnd.c - provides text fields, a textsw, and
 * several drop targets to demonstate ways to receive
 * and illustrate drag and drop operations.
 */

#include <malloc.h>
#include <xview/xview.h>
#include <xview/dragdrop.h>
#include <xview/panel.h>
#include <xview/svrimage.h>
#include <xview/textsw.h>

static unsigned short normal_bitmap[] = {
#include "normal.icon"
};

static unsigned short busy_bitmap[] = {
#include "busy.icon"
};

static unsigned short normal2_bitmap[] = {
#include "normal2.icon"
};

static unsigned short busy2_bitmap[] = {
#include "busy2.icon"
};

static char * dnd_codes[7] = {
    "OK",
    "Error",
    "Illegal Target",
    "Timeout",
    "Unable to obtain selection",
    "Dropped on root window",
    "*** Unknown return code"
};

Frame frame;
Panel panel;
Panel_item drop_target[3];
Drag_drop dnd;


/*ARGSUSED*/
static void
hide_drop_targets(item, event)
    Panel_item item;
    Event *event;
{
    int	    i;
    int	    show;

    show = !xv_get(drop_target[0], XV_SHOW);
    for (i=0; i<=2; i++)
	xv_set(drop_target[i],
	       XV_SHOW, show,
	       0);
    xv_set(item,
	PANEL_LABEL_STRING, show ? "Hide drop targets" : "Show drop targets",
	0);
}


/*ARGSUSED*/
static void
inactivate_drop_targets(item, event)
    Panel_item item;
    Event *event;
{
    int	    i;
    int	    inactive;

    inactive = !xv_get(drop_target[0], PANEL_INACTIVE);
    for (i=0; i<=2; i++)
	xv_set(drop_target[i],
	       PANEL_INACTIVE, inactive,
	       0);
    xv_set(item,
	PANEL_LABEL_STRING, inactive ? "Activate drop targets" :
	    "Inactivate drop targets",
	0);
}


static void
get_primary_selection(sel_req)
    Selection_requestor sel_req;
{
    long	    length;
    int		    format;
    char	   *sel_string;
    char	   *string;

    xv_set(sel_req, SEL_TYPE, XA_STRING, 0);
    string = (char *) xv_get(sel_req, SEL_DATA, &length, &format);
    if (length != SEL_ERROR) {
	/* Create a NULL-terminated version of 'string' */
	sel_string = (char *) xv_calloc(1, length+1);
	strncpy(sel_string, string, length);
	/* Print out primary selection string */
	printf("Primary selection= \"%s\"\n", sel_string);
    } else
	printf("*** Unable to get primary selection.\n");
}


static int
drop_target_notify_proc(item, value, event)
    Panel_item	    item;
    unsigned int    value;
    Event	   *event;
{
    Selection_requestor sel_req;

    printf("(drop_target_notify_proc) %s action= ",
	xv_get(item, PANEL_LABEL_STRING));
    sel_req = xv_get(item, PANEL_DROP_SEL_REQ);
    switch (event_action(event)) {
      case ACTION_DRAG_COPY:
	printf("ACTION_DRAG_COPY,\n");
	get_primary_selection(sel_req);
	break;
      case ACTION_DRAG_MOVE:
	printf("ACTION_DRAG_MOVE\n");
	get_primary_selection(sel_req);
	break;
      case LOC_DRAG:
	printf("LOC_DRAG, result= %s\n", dnd_codes[MIN(value, 6)]);
	break;
      default:
	printf("%d\n", event_action(event));
	break;
    }
    return XV_OK;
}


main(argc, argv)
    int argc;
    char **argv;
{
    Server_image    busy_glyph[2];
    Server_image    normal_glyph[2];
    Panel	    panel2;

    xv_init(XV_INIT_ARGS, argc, argv, 0);

    frame = xv_create(NULL, FRAME,
	FRAME_LABEL, "Drag and Drop Test",
	0);
    panel = xv_create(frame, PANEL,
	PANEL_LAYOUT, PANEL_VERTICAL,
	0);
    xv_create(panel, PANEL_BUTTON,
	PANEL_LABEL_STRING, "Inactivate Drop Targets",
	PANEL_NOTIFY_PROC, inactivate_drop_targets,
	0);
    xv_create(panel, PANEL_BUTTON,
	PANEL_LABEL_STRING, "Hide Drop Targets",
	PANEL_NOTIFY_PROC, hide_drop_targets,
	0);
    xv_create(panel, PANEL_TEXT,
	PANEL_LABEL_STRING, "Text field #1:",
	PANEL_VALUE, "Hello world!",
	PANEL_VALUE_DISPLAY_LENGTH, 20,
	0);
    xv_create(panel, PANEL_TEXT,
	PANEL_LABEL_STRING, "Text field #2:",
	PANEL_VALUE_DISPLAY_LENGTH, 20,
	0);
    dnd = xv_create(panel, DRAGDROP, 0);
    xv_create(dnd, SELECTION_ITEM,
	SEL_DATA, "dnd selection data",
	0);
    normal_glyph[0] = xv_create(NULL, SERVER_IMAGE,
	XV_HEIGHT, 64,
	XV_WIDTH, 64,
	SERVER_IMAGE_DEPTH, 1,
	SERVER_IMAGE_BITS, normal_bitmap,
	0);
    busy_glyph[0] = xv_create(NULL, SERVER_IMAGE,
	XV_HEIGHT, 64,
	XV_WIDTH, 64,
	SERVER_IMAGE_DEPTH, 1,
	SERVER_IMAGE_BITS, busy_bitmap,
	0),
    normal_glyph[1] = xv_create(NULL, SERVER_IMAGE,
	XV_HEIGHT, 64,
	XV_WIDTH, 64,
	SERVER_IMAGE_DEPTH, 1,
	SERVER_IMAGE_BITS, normal2_bitmap,
	0);
    busy_glyph[1] = xv_create(NULL, SERVER_IMAGE,
	XV_HEIGHT, 64,
	XV_WIDTH, 64,
	SERVER_IMAGE_DEPTH, 1,
	SERVER_IMAGE_BITS, busy2_bitmap,
	0),
    drop_target[0] = xv_create(panel, PANEL_DROP_TARGET,
	PANEL_LABEL_STRING, "Full Drop Target:",
	PANEL_NOTIFY_PROC, drop_target_notify_proc,
	PANEL_DROP_DND, dnd,
	PANEL_DROP_FULL, TRUE,
	PANEL_DROP_GLYPH, normal_glyph[0],
	PANEL_DROP_BUSY_GLYPH, busy_glyph[0],
	0);
    xv_create(panel, PANEL_DROP_TARGET,
	PANEL_LABEL_STRING, "Full Drop Target #2:",
	PANEL_NOTIFY_PROC, drop_target_notify_proc,
	PANEL_DROP_DND, dnd,
	PANEL_DROP_FULL, TRUE,
	PANEL_DROP_GLYPH, normal_glyph[1],
	PANEL_DROP_BUSY_GLYPH, busy_glyph[1],
	0);
    drop_target[1] = xv_create(panel, PANEL_DROP_TARGET,
	PANEL_LABEL_STRING, "Default Empty Drop Target:",
	PANEL_NOTIFY_PROC, drop_target_notify_proc,
	PANEL_DROP_SITE_DEFAULT, TRUE,
	0);
    if (xv_get(drop_target[1], PANEL_DROP_SITE_DEFAULT) != TRUE) {
	printf("PANEL_DROP_SITE_DEFAULT failed\n");
	exit(1);
    }
    drop_target[2] = xv_create(panel, PANEL_DROP_TARGET,
	PANEL_LABEL_STRING, "Default Full Drop Target:",
	PANEL_NOTIFY_PROC, drop_target_notify_proc,
	PANEL_DROP_DND, dnd,
	PANEL_DROP_FULL, TRUE,
	0);
    window_fit(panel);  

    xv_create(frame, TEXTSW,
	WIN_ROWS, 4,
	WIN_COLUMNS, 10,
	0);
    panel2 = xv_create(frame, PANEL,
	PANEL_LAYOUT, PANEL_VERTICAL,
	0);
    xv_create(panel2, PANEL_MESSAGE,
	PANEL_LABEL_STRING, "New panel",
	0);
    xv_create(panel2, PANEL_TEXT,
	PANEL_LABEL_STRING, "Text field:",
	PANEL_VALUE_DISPLAY_LENGTH, 20,
	0);
    window_fit(panel2);

    window_fit(frame);

    xv_main_loop(frame);
    exit(0);
}



