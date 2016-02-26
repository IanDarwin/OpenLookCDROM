#ifndef lint
static char    *RCSid = "$Header: /n/homeserver/i/gounares/pu/apex/src/apex/RCS/apex_shell.c,v 1.1 93/01/06 03:27:21 gounares Exp Locker: gounares $";

#endif

/*
 * $Log:	apex_shell.c,v $
 * Revision 1.1  93/01/06  03:27:21  gounares
 * Initial revision
 * 
 */

/*
 * shell.c
 * 
 * written by Alexander Gounares
 * 
 * 10/21/92
 */

/*
 * Copyright 1993 Alexander Gounares
 * 
 * This source is covered by the 
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>
GNU General Public License Version 2
<!@ 1 COPYING 1 #Link to the GNU General Public License
#when run this description will be passed to the shell
cat COPYING
 @!>

 * 
 * see the apeX manual for more details
 */

#include <stdio.h>
#include <strings.h>
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/termsw.h>
#include <xview/panel.h>
#include <xview/notify.h>
#include <xview/notice.h>
#include <xview/svrimage.h>
#include <xview/icon.h>
#include <xview/textsw.h>
#include "apex_shell_bmp"
#include "apex_shell.h"
#include "alloc.h"
#include "apex_options.h"

struct _Shell {
	Frame           frame;
	Panel           panel;
	Panel_item      rcs_choice;
	Panel_item      file_text;
	Panel_item      apply_button;
	Panel_item      dismiss_button;
	Termsw          tty;
};

typedef struct _Shell Shell;
extern int      CLIENT_DATA_KEY;

#define MAX_PATTERN_SIZE 1024

/* check in popup shell */

typedef struct _CIP {
	Frame           frame;
	Panel           panel1;
	Panel_item      version_choice;
	Panel_item      version_num;
	Panel_item      lock_check;
	Textsw          textsw;
	Panel           panel2;
	Panel_item      apply_button;
}               CIP;

typedef struct _COP {
	Frame           frame;
	Panel           panel;
	Panel_item      version_choice;
	Panel_item      version_text;
	Panel_item      lock_check;
	Panel_item      apply_button;
}               COP;

static void     create_shell();

static Shell    main_shell;	       /* we can have only one shell per
				        * process */

static char    *shell_help[] =
{
	"the Shell",
	"B", "Overview",
	"b", "    The apeX Shell consists of a normal Openwin cmdtool with",
	"b", "an integrated RCS interface.",
	"b", " ",
	"B", "Check In / Check Out",
	"b", "    Files matching the glob patterns listed in the 'Files:'",
	"b", "field can be either checked in or out from RCS.  Hitting the",
	"b", "'Apply...' button will bring up the relevant popups for each",
	"b", "operation.",
	0};

static char    *cip_help[] =
{
	"RCS Check-In",
	"B", "Automatic / New",
	"b", "    Automatic means that RCS will be told to assign the next",
	"b", "highest version number to the files being checked in.",
	"b", " ",
	"b", "    If this is a new release, select 'New' and specify the",
	"b", "release number in the 'Release Number' field",
	"B", "CAUTION:",
	"b", "    RCS will fail if the release number is lower than the",
	"b", "    current release number",
	"B", "Locked Checkout",
	"b", "    Selecting this option will tell RCS to immediately check",
	"b", "out and lock the files being checked in.  This will allow you",
	"b", "to continue your work uninterupted.",
	"B", "Check In Description",
	"b", "    Use the text field to enter your log description for this",
	"b", "checkin.   Each file being checked in will get this log",
	"b", "description.",
	0};

/*
 * shell_dismiss_proc -- called when the dismiss button is hit
 */
static void 
shell_dismiss_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	Shell          *pshell = (Shell *) xv_get(item, PANEL_CLIENT_DATA);

	xv_set(pshell->frame, XV_SHOW, FALSE, NULL);
}

/*
 * cop_version_proc -- toggle the version text item
 */
static void 
cop_version_proc(item, value, event)
    Panel_item      item;
    int             value;
    Event          *event;
{
	COP            *pcop = (COP *) xv_get(item, PANEL_CLIENT_DATA);

	if (!value)
		xv_set(pcop->version_text, PANEL_INACTIVE, TRUE, NULL);
	else
		xv_set(pcop->version_text, PANEL_INACTIVE, FALSE, NULL);
}

/*
 * cop_apply_proc -- actually do the check out
 */
static void 
cop_apply_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	COP            *pcop = (COP *) xv_get(item, XV_KEY_DATA, CLIENT_DATA_KEY);
	Shell          *pshell = (Shell *) xv_get(item, PANEL_CLIENT_DATA);
	char            szBuf[MAX_PATTERN_SIZE + 50],
	                szArgs[50],
	                c;
	int             length;


	c = ((int) xv_get(pcop->lock_check, PANEL_VALUE)) ? 'l' : 'r';

	szArgs[0] = '\0';

	if ((int) xv_get(pcop->version_choice, PANEL_VALUE))
		sprintf(szArgs, "v %s", xv_get(pcop->version_text, PANEL_VALUE));

	sprintf(szBuf, "%s -%c%s %s\n", get_checkout(), c, szArgs,
		xv_get(pshell->file_text, PANEL_VALUE));

	length = strlen(szBuf);

	ttysw_input(pshell->tty, szBuf, length);
}

/*
 * create_cop -- create a Check Out Window
 */
static COP     *
create_cop(pshell)
    Shell          *pshell;
{
	COP            *pcop = (COP *) acalloc(1, sizeof(COP));

	pcop->frame = (Frame) xv_create(pshell->frame, FRAME_CMD,
		FRAME_LABEL, "apeX Check Out options",
		NULL);

	pcop->panel = (Panel) xv_get(pcop->frame, FRAME_CMD_PANEL);

	xv_set(pcop->panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	pcop->version_choice = (Panel_item) xv_create(pcop->panel, PANEL_CHOICE,
		PANEL_CHOICE_STRINGS, "Latest Version", "Specific Version", NULL,
		PANEL_CLIENT_DATA, pcop,
		PANEL_NOTIFY_PROC, cop_version_proc,
		NULL);

	pcop->version_text = (Panel_item) xv_create(pcop->panel, PANEL_TEXT,
		PANEL_LABEL_STRING, "Version Number:",
		PANEL_VALUE_DISPLAY_LENGTH, 15,
		PANEL_INACTIVE, TRUE,
		NULL);

	pcop->lock_check = (Panel_item) xv_create(pcop->panel, PANEL_CHECK_BOX,
		PANEL_LABEL_STRING, "Locked Checkout",
		PANEL_CHOICE_STRINGS, "", NULL,
		PANEL_VALUE, 1,
		NULL);

	pcop->apply_button = (Panel_item) xv_create(pcop->panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Apply",
		PANEL_CLIENT_DATA, pshell,
		XV_KEY_DATA, CLIENT_DATA_KEY, pcop,
		PANEL_NOTIFY_PROC, cop_apply_proc,
		XV_X, 100,
		NULL);


	window_fit(pcop->panel);
	window_fit(pcop->frame);

	return pcop;
}

/*
 * called when the frame is done...need to do this to clear the editor
 */
static void 
pcip_done_proc(frame)
    Frame           frame;
{
	CIP            *pcip = (CIP *) xv_get(frame, XV_KEY_DATA, CLIENT_DATA_KEY);

	textsw_reset(pcip->textsw, 0, 0);
	xv_set(frame, XV_SHOW, FALSE, NULL);

	xv_destroy_safe(pcip->frame);
	free(pcip);
}

/*
 * cip_apply_proc -- actually do the check in -- call the program apex_ci
 */
static void 
cip_apply_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	CIP            *pcip = (CIP *) xv_get(item, XV_KEY_DATA, CLIENT_DATA_KEY);
	Shell          *pshell = (Shell *) xv_get(item, PANEL_CLIENT_DATA);
	char           *szBuf,
	                szArgs[10],
	                c;
	int             length,
	                l;
	char           *szTmpFile = tmpnam(NULL);
	FILE           *fp;

	if (!szTmpFile) {
		notice_prompt(pcip->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS, "Cannot create temporary file using tmpnam",
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	length = (l = (int) xv_get(pcip->textsw, TEXTSW_LENGTH)) +
		MAX_PATTERN_SIZE + 50;
	szBuf = (char *) acalloc(length, sizeof(char));

	xv_get(pcip->textsw, TEXTSW_CONTENTS, 0, szBuf, l);

	length = strlen(szBuf);

	/*
	 * if( length < 3 || !(szBuf[length -1] == '\n' && szBuf[length - 2]
	 * == '.' && szBuf[length - 3] == '\n' ) ) { strcat(szBuf, "\n.\n");
	 * length +=3; }
	 */
	if ((fp = fopen(szTmpFile, "w")) == NULL) {
		char            szBuf2[1024];

		sprintf(szBuf2, "Cannot open temporary file '%s'", szTmpFile);
		notice_prompt(pcip->frame, event,
			NOTICE_FOCUS_XY, event_x(event), event_y(event),
			NOTICE_MESSAGE_STRINGS, szBuf2,
			NULL,
			NOTICE_BUTTON_YES, "OK",
			NULL);
		return;
	}
	write(fileno(fp), szBuf, length);

	fclose(fp);

	c = ((int) xv_get(pcip->lock_check, PANEL_VALUE)) ? 'l' : 'r';

	szArgs[0] = '\0';

	if ((int) xv_get(pcip->version_choice, PANEL_VALUE) == 1)
		sprintf(szArgs, "v %d", (int) xv_get(pcip->version_num, PANEL_VALUE));

	sprintf(szBuf, "%s -%c%s %s < %s\n",
		get_checkin(), c, szArgs,
		(char *) xv_get(pshell->file_text, PANEL_VALUE),
		szTmpFile);

	length = strlen(szBuf);

	ttysw_input(pshell->tty, szBuf, length);

}

/*
 * cip_version_proc -- toggle the the version number item state
 */
static void 
cip_version_proc(item, value, event)
    Panel_item      item;
    int             value;
    Event          *event;
{
	CIP            *pcip = (CIP *) xv_get(item, PANEL_CLIENT_DATA);

	if (!value)
		xv_set(pcip->version_num, PANEL_INACTIVE, TRUE, NULL);
	else
		xv_set(pcip->version_num, PANEL_INACTIVE, FALSE, NULL);
}

/*
 * create a Check In Popup
 */
static CIP     *
create_cip(pshell)
    Shell          *pshell;
{
	CIP            *pcip = (CIP *) acalloc(1, sizeof(CIP));

	pcip->frame = (Frame) xv_create(pshell->frame, FRAME_CMD,
		FRAME_LABEL, "apeX Check-In Description",
		XV_KEY_DATA, CLIENT_DATA_KEY, pcip,
		FRAME_DONE_PROC, pcip_done_proc,
		NULL);

	pcip->panel1 = (Panel) xv_get(pcip->frame, FRAME_CMD_PANEL);

	pcip->version_choice = (Panel_item) xv_create(pcip->panel1, PANEL_CHOICE,
		PANEL_CHOICE_STRINGS, "Automatic", "New", NULL,
		PANEL_NOTIFY_PROC, cip_version_proc,
		PANEL_CLIENT_DATA, pcip,
		NULL);

	pcip->version_num = (Panel_item) xv_create(pcip->panel1, PANEL_NUMERIC_TEXT,
		PANEL_LABEL_STRING, "Release Number:",
		PANEL_MIN_VALUE, 2,
		PANEL_VALUE_DISPLAY_LENGTH, 4,
		PANEL_INACTIVE, TRUE,
		NULL);

	pcip->lock_check = (Panel_item) xv_create(pcip->panel1, PANEL_CHECK_BOX,
		PANEL_LABEL_STRING, "Locked Checkout",
		PANEL_CHOICE_STRINGS, "", NULL,
		PANEL_VALUE, 1,
		NULL);

	window_fit_height(pcip->panel1);

	pcip->textsw = (Textsw) xv_create(pcip->frame, TEXTSW,
		XV_X, 0,
		TEXTSW_TAB_WIDTH, get_tabsize(),
		WIN_COLUMNS, 80,
		WIN_ROWS, 10,
		WIN_BELOW, pcip->panel1,
		NULL);

	pcip->panel2 = (Panel) xv_create(pcip->frame, PANEL,
		WIN_BELOW, pcip->textsw,
		XV_X, 0,
		XV_HEIGHT, 30,
		XV_WIDTH, xv_get(pcip->textsw, XV_WIDTH),
		NULL);

	pcip->apply_button = (Panel_item) xv_create(pcip->panel2, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Apply",
		XV_X, 250,
		PANEL_CLIENT_DATA, pshell,
		XV_KEY_DATA, CLIENT_DATA_KEY, pcip,
		PANEL_NOTIFY_PROC, cip_apply_proc,
		NULL);

	xv_set(pcip->panel1, XV_WIDTH, xv_get(pcip->textsw, XV_WIDTH), NULL);

	set_help_button(pcip->frame, pcip->panel1, cip_help);

	window_fit(pcip->frame);
	return pcip;
}

/*
 * apply_proc -- called when the apply button on the _shell_ is called
 */
static void 
rcs_apply_proc(item, event)
    Panel_item      item;
    Event          *event;
{
	if ((int) xv_get(main_shell.rcs_choice, PANEL_VALUE) == 0) {
		/* check in choice */
		CIP            *pcip = create_cip(&main_shell);

		xv_set(pcip->frame, XV_SHOW, TRUE, NULL);
	} else {
		COP            *pcop = create_cop(&main_shell);

		xv_set(pcop->frame, XV_SHOW, TRUE, NULL);
	}
}

/*
 * create the shell
 */
static void
create_shell()
{
	static Server_image shell_image;
	static Icon     shell_icon;

	Notify_value    editor_done();

	if (!shell_image) {
		shell_image = (Server_image) xv_create(NULL, SERVER_IMAGE,
			SERVER_IMAGE_X_BITS, apex_shell_bmp_bits,
			XV_HEIGHT, apex_shell_bmp_height,
			XV_WIDTH, apex_shell_bmp_width,
			SERVER_IMAGE_DEPTH, 1,
			NULL);
	}
	if (!shell_icon) {
		shell_icon = (Icon) xv_create(NULL, ICON,
			ICON_IMAGE, shell_image,
			ICON_MASK_IMAGE, shell_image,
			ICON_TRANSPARENT, TRUE,
			NULL);
	}
	main_shell.frame = (Frame) xv_create(NULL, FRAME,
		FRAME_LABEL, "apeX Shell",
		FRAME_ICON, shell_icon,
		NULL);

	main_shell.panel = (Panel) xv_create(main_shell.frame, PANEL,
		XV_HEIGHT, 30,
		NULL);

	main_shell.rcs_choice = (Panel_item) xv_create(main_shell.panel,
		PANEL_CHOICE,
		PANEL_CHOICE_STRINGS, "Check In", "Check Out", NULL,
		NULL);

	main_shell.file_text = (Panel_item) xv_create(main_shell.panel,
		PANEL_TEXT,
		PANEL_LABEL_STRING, "Files:",
		PANEL_VALUE, get_pattern(),
		PANEL_VALUE_DISPLAY_LENGTH, 24,
		PANEL_VALUE_STORED_LENGTH, MAX_PATTERN_SIZE,
		NULL);

	main_shell.apply_button = (Panel_item) xv_create(main_shell.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Apply...",
		PANEL_NOTIFY_PROC, rcs_apply_proc,
		NULL);

	main_shell.dismiss_button = (Panel_item) xv_create(main_shell.panel,
		PANEL_BUTTON,
		PANEL_LABEL_STRING, "Dismiss",
		PANEL_NOTIFY_PROC, shell_dismiss_proc,
		PANEL_CLIENT_DATA, &main_shell,
		NULL);

	window_fit_height(main_shell.panel);

	main_shell.tty = (Termsw) xv_create(main_shell.frame, TERMSW,
		WIN_ROWS, 24,
		WIN_COLUMNS, 80,
		WIN_BELOW, main_shell.panel,
		NULL);

	xv_set(main_shell.panel, XV_WIDTH, xv_get(main_shell.tty, XV_WIDTH), NULL);

	window_fit(main_shell.frame);

	set_help_button(main_shell.frame, main_shell.panel, shell_help);

	notify_interpose_destroy_func(main_shell.frame, editor_done);
}

/*
 * show_shell
 */
void
show_shell()
{
	if (!main_shell.frame) {
		create_shell();
	}
	xv_set(main_shell.frame, XV_SHOW, TRUE, NULL);
}

/*
 * get_shell_frame - return the frame of the main_shell
 */
Frame
get_shell_frame()
{
	return main_shell.frame;
}

/*
 * reset_shell -- reset the shell stuff to NULL
 */
void
reset_shell()
{
	static Shell    z;

	main_shell = z;
}

/*
 * get_shell -- return the tty handle
 */
Termsw
get_shell()
{
	return main_shell.tty;
}
