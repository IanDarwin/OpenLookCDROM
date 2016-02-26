/* routines associated with displaying simple
 * information and sending mail to the author. A lot of this
 * is based on code from Ftptool.
 */

#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/cursor.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/xv_xrect.h>
#include <xview/cms.h>
#include <xview/textsw.h>
#include <xview/server.h>
#include <xview/seln.h>
#include <xview/notice.h>
#include <xview/notify.h>
#include <xview/xv_error.h>

#include "robot.h"
#include "patchlevel.h"

#define TO_LEN  64
#define MAILER "/usr/ucb/mail"

#define	THE_WIDTH	600
#define PANEL_HEIGHT	27
Frame	about_frame;
Frame	mail_frame;
Panel	about_panel;
Textsw	about_text;
Panel	mail_panel, mail_panel2;
Textsw	mail_text;
Panel_item	mail_add;

char	about_message[] =
"Robot is a graph plotting and data analysis program.\n\
It has emphasis on astronomy but most of the functions should\n\
apply to other fields too.\n\
\n\
Robot is under near continual development and comments,\n\
bug reports etc. are invited.\n\
\n\
To obtain on-line help press the button marked Help\n\
(at least on a Sun) with the cursor positioned above\n\
the button or menu item of interest.\n\
If this doesn't work it's probably because the file robot.info\n\
isn't located in a directory listed in the HELPPATH environment variable.\n\
\n\
There is also a manual and several demonstration scripts.\n\
\n\
The demonstration scripts are located in the directory named Demos\n\
if you have the full robot distribution.\n\
The scripts can be run in several different ways:\n\
(i) Press the button labelled \"File...\" and select a file.\n\
(ii) On the line labelled \"Robot Command:\" type:\n\
file Demos/xxxx.rob\n\
where Demos/xxxx.rob is the name of a file (this assumes Demos\n\
is the directory where the file is located).\n\
(iii) Using the OpenWindows file manager you can \"drag and drop\"\n\
a file onto the drop target (the square on the right).\n\
(iv) When you start robot you can give the name of the file to run\n\
on the command line.\n\
\n\
The latest version of Robot can be obtained by anonymous ftp\n\
from ftp.astro.psu.edu in the directory pub/astrod.\n\
\n\
\n\
I would appreciate receiving reprints of any papers that make use of this program.\n\
\n\
Robin Corbet\n\
525 Davey Laboratory\n\
University Park\n\
PA 16802\n\
U.S.A.\n\
\n\n\
corbet@astro.psu.edu\
"
;

void
show_mail()
{
	xv_set(mail_frame, XV_SHOW, TRUE, NULL);
	xv_set(mail_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);

}

void
cancel_mail()
{
	textsw_reset(mail_text, 0, 0);
	xv_set(mail_frame, XV_SHOW, FALSE, NULL);
	xv_set(mail_frame, FRAME_CMD_PUSHPIN_IN, FALSE, NULL);

}
void
send_mail()
{
    FILE *pp;
    char addr[TO_LEN + 1], buf[BUFSIZ+1];
    int last_was_NL;
    Textsw_index cur_pos, next_pos;

    static char *fb_cmd = NULL;
    strcpy(addr, (char *) xv_get(mail_add, PANEL_VALUE));

    if ((int)xv_get(mail_text, TEXTSW_LENGTH) == 0) {
		totext2_("ERROR: NO TEXT IN MESSAGE.");
		return;
    }

 
   if (fb_cmd == NULL)
		fb_cmd = (char *)malloc((unsigned int)(strlen(MAILER) + TO_LEN + 1));
    
    sprintf(fb_cmd, "%s %s", MAILER, addr);
    if ((pp = popen(fb_cmd, "w")) == NULL) {
		totext2_("POPEN ERROR; COULDN'T SEND FEEDBACK MESSAGE!");
		return;
    }
 
    fprintf(pp, "~s Robot %-1.2f \n\n", VERSION);
 
    fprintf(pp, "\n");

    next_pos = 0;
    cur_pos = next_pos - BUFSIZ;
    while (next_pos == cur_pos + BUFSIZ) {
		cur_pos = next_pos;
		next_pos = (Textsw_index)xv_get(mail_text,
			TEXTSW_CONTENTS, cur_pos, buf, BUFSIZ);
    if ((next_pos - cur_pos) != 0) {
        buf[next_pos - cur_pos] = '\0';
        fprintf(pp, "%s", buf);
        last_was_NL = (buf[next_pos-cur_pos-1] == '\n');
    }
    }
    /*
     *  Force last char out to be a newline
     */
    if (!last_was_NL)
		putc('\n', pp);
       
    if(pclose(pp) != 0) {
    	totext2_("ERROR: MAIL FAILED -- MESSAGE NOT SENT!");
	return;
    }
    cancel_mail();
}

/* A window to give information */
void
about_frame_create(frame)
Frame	frame;
{
	about_frame = xv_create(frame, FRAME_CMD,
				FRAME_LABEL, "About Robot",
				FRAME_SHOW_FOOTER,	FALSE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
	about_panel = xv_get(about_frame, FRAME_CMD_PANEL);
	xv_set(about_panel, 
		XV_WIDTH, THE_WIDTH,
		XV_HEIGHT, PANEL_HEIGHT,
		XV_X,	0,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);


	about_text = (Textsw) xv_create(about_frame, TEXTSW,
					 XV_WIDTH, THE_WIDTH,
					 WIN_BELOW, about_panel,
				       	 XV_X,	0,
					 XV_HEIGHT, 120,
					 NULL);
	textsw_insert(about_text, about_message, strlen(about_message));
	textsw_normalize_view(about_text, 0);

	xv_create(about_panel, PANEL_BUTTON,
			XV_HELP_DATA, "robot:send_mail",
			PANEL_LABEL_STRING, "Send Mail...",
			PANEL_NOTIFY_PROC, show_mail,
			NULL);
if(not_open_look){
	
	   xv_set(about_panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	   xv_create(about_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}

	window_fit(about_frame);






/* and make the mailer window */

	mail_frame = xv_create(frame, FRAME_CMD,
				FRAME_LABEL, "Send Mail to Author",
				FRAME_SHOW_FOOTER,	FALSE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
	mail_panel = xv_get(mail_frame, FRAME_CMD_PANEL);
	xv_set(mail_panel, 
		XV_WIDTH, THE_WIDTH,
		XV_HEIGHT, 70,
		PANEL_LAYOUT, PANEL_VERTICAL,
		NULL);

	xv_create(mail_panel, PANEL_MESSAGE,
		PANEL_LABEL_STRING, 
		"Comments welcome, please check mail address below is correct",
		NULL);
	mail_add = xv_create(mail_panel, PANEL_TEXT,
			XV_X,	0,
			WIN_BELOW,	mail_panel,
			PANEL_VALUE, "corbet@astro.psu.edu",
			PANEL_LABEL_STRING, "To:",
			PANEL_VALUE_DISPLAY_LENGTH, 35,
			NULL);

	mail_text = (Textsw) xv_create(mail_frame, TEXTSW,
					 XV_WIDTH, THE_WIDTH,
					 XV_HEIGHT, 120,
					 XV_X, 0,
					 WIN_BELOW, mail_panel,
					 NULL);
	mail_panel2 = xv_create(mail_frame, PANEL,
		XV_WIDTH, THE_WIDTH,
		XV_HEIGHT, PANEL_HEIGHT,
		WIN_BELOW, mail_text,
		XV_X, 0,
		NULL);

	xv_create(mail_panel2, PANEL_BUTTON,
			XV_HELP_DATA, "robot:send",
			PANEL_LABEL_STRING, "Send",
			PANEL_NOTIFY_PROC, send_mail,
			XV_WIDTH, THE_WIDTH,
			NULL);
	xv_create(mail_panel2, PANEL_BUTTON,
			XV_HELP_DATA, "robot:cancel_mail",
			PANEL_LABEL_STRING, "Cancel",
			PANEL_NOTIFY_PROC, cancel_mail,
			NULL);

	window_fit(mail_frame);


}
void
show_about()
{
	xv_set(about_frame, XV_SHOW, TRUE, NULL);
	xv_set(about_frame, FRAME_CMD_PUSHPIN_IN, TRUE, NULL);

}



