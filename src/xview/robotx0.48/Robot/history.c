/* command history */

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

/* this presently only works if HISTORY_DEPTH and LIST_LENGTH are the
 * same value */
#define HISTORY_DEPTH	12  /* specifies the number of previous
	 		     * commands retained */
#define LIST_LENGTH	12 /* number of lines displayed */
Frame	history_frame;
Panel_item	history_list;
Panel_item	history_text;



void
history_ok()
{
	strcpy(inst, (char *) xv_get(history_text, PANEL_VALUE));

	to_robot();
}



void
set_text(item, string, client_data, op, event)
	Panel_item      item;
	char           *string;
	Xv_opaque       client_data;
	Panel_list_op   op;
	Event          *event;
{
	static char     last_string[80] = "";
	static struct timeval then = {0, 0};
	static struct timeval now = {0, 0};

	Bool historyok;
	historyok = FALSE;

	if (op == PANEL_LIST_OP_SELECT) {
		if strne
		    ((char *)xv_get(history_text, PANEL_VALUE), string)
			xv_set(history_text, PANEL_VALUE, string, NULL);
		now = event_time(event);
		if (double_click(last_string, &then, string, &now)){
			history_ok();
 		}
		strcpy(last_string, string);
		then = now;
		
	}
}

void
make_history_frame(frame)
Frame	frame;
{
	history_frame = xv_create(frame, FRAME_CMD,
			FRAME_LABEL, "History of Interactive Commands",
			FRAME_CMD_PUSHPIN_IN, TRUE,
			NULL);
	history_panel = xv_get(history_frame, FRAME_CMD_PANEL);
	xv_set(history_panel, PANEL_LAYOUT, PANEL_VERTICAL, NULL);

	history_list = xv_create(history_panel, PANEL_LIST,
			PANEL_LIST_DISPLAY_ROWS,	LIST_LENGTH,
			PANEL_LIST_WIDTH,		250,
			PANEL_NOTIFY_PROC,	set_text,
			XV_HELP_DATA,	"robot:history_list",
			NULL);

	history_text = xv_create(history_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "Command:",
			PANEL_VALUE_DISPLAY_LENGTH, 25,
			PANEL_NOTIFY_PROC,	history_ok,
			XV_HELP_DATA,	"robot:history_text",
			NULL);


	xv_set(history_panel, PANEL_DEFAULT_ITEM,
	 xv_create(history_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING,	"Send Command",
			PANEL_NOTIFY_PROC,	history_ok,
			XV_HELP_DATA,	"robot:history_ok",
			NULL),
				NULL);

if(not_open_look){
	
	   xv_set(history_panel,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);
	   xv_create(history_panel, PANEL_BUTTON,
		XV_HELP_DATA,		"robot:dismiss",
		PANEL_LABEL_STRING,	DISMISS,
		PANEL_NOTIFY_PROC,	hide_me,
		NULL);
}

	window_fit(history_panel);
	window_fit(history_frame);
}

void
make_history(text)
char           *text;
{
int             nrows, i;
/* don't save blank strings */
	if(streq(text, ""))return;

	if strne((char *)xv_get(history_text, PANEL_VALUE), text)
		xv_set(history_text, PANEL_VALUE, text, NULL); 
	nrows = xv_get(history_list, PANEL_LIST_NROWS);
	if (nrows < LIST_LENGTH )
		xv_set(history_list, PANEL_LIST_STRING, nrows, text,
		       PANEL_LIST_SELECT, nrows, TRUE,
		       NULL);
	else {			/* need to shift models along */
/* hide list during redraw to try to make it less annoying! */
		xv_set(history_list, XV_SHOW, FALSE, NULL);
		for (i = 0; i <= (LIST_LENGTH - 2); i++) {
			if(strne((char *)xv_get(history_list, 
					PANEL_LIST_STRING, i),
			(char *) xv_get(history_list, PANEL_LIST_STRING, i+1)))
		xv_set(history_list, PANEL_LIST_STRING, i,
			       xv_get(history_list, PANEL_LIST_STRING, i + 1), NULL);
		}
		if(strne(text, (char *) xv_get(history_list, PANEL_LIST_STRING, HISTORY_DEPTH - 1)))
		xv_set(history_list, PANEL_LIST_STRING, HISTORY_DEPTH - 1, text,
		       PANEL_LIST_SELECT, HISTORY_DEPTH - 1, TRUE,
		       NULL);
		xv_set(history_list, XV_SHOW, TRUE, NULL);
	}
}
void
show_history()
{
	xv_set(history_frame,	XV_SHOW, TRUE,
				FRAME_CMD_PUSHPIN_IN, TRUE,
				NULL);
}


/* analog of C shell repeat command feature
 * this could probably do with some tidying up! */
char           *
repeat_history(text)
	char            text[];
/* it's assummed that the first character is a '^' or '!' */
{
	int             nrows, i, len, editlen;
	int             index;
	char            htext1[ILENGTH], htext2[ILENGTH], htext3[ILENGTH],
	                htext4[ILENGTH];



	nrows = xv_get(history_list, PANEL_LIST_NROWS);
		/* does a previous command exist? */
		if (nrows < 1) {
			totext2_("Error: no previous instruction");
			return ("");
		}

	/* allow "editing" of previous instruction */
	if (text[0] == '^' && text[1] != '^') {
		/* reset some text */
		strcpy(htext1, "");
		strcpy(htext2, "");
		strcpy(htext3, "");
		strcpy(htext4, "");

		/* find the next '^' */
		len = strlen(text);
		index = -1;
		for (i = 1; i < len; i++) {
			if (text[i] == '^') {
				index = i;
				strncpy(htext1, text + 1, index - 1);
				htext1[index - 1] = NULL;
				break;
			}
		}
		if(index < 0){
			strcpy(htext1, text+1);
			index = len-1;
		}
		strcpy(htext2, text + index + 1);
		if (htext2[strlen(htext2) - 1] == '^') {
			htext2[strlen(htext2) - 1] = NULL;
		}
		/* now try to find it in the previous string */
		sprintf(htext3, "%s",
		(char *) xv_get(history_list, PANEL_LIST_STRING, nrows - 1));
		editlen = strlen(htext1);
		len = strlen(htext3);

		for (i = 0; i < len; i++) {
			if (strncmp(htext3 + i, htext1, editlen) == 0) {
				/* think we found the string */
				if (i > 0) {
					strncpy(htext4, htext3, i);
					htext4[i] = NULL;
				}
				else{
					strcpy(htext4, "");
				}
				strcat(htext4, htext2);
				strcat(htext4, htext3 + i + editlen);
				return (htext4);
			}
		}
		totext2_("Error: pattern not matched");
		return ("");


	}
	/* it's not an edit instruction so it should be a repeat command */
	else if (text[0] != '^' && text[0] != '!') {
		totext2_("WARNING - BUG IN CHECK_HISTORY");
		return("IDLE");
	} else {
		/*
		 * if the instruction is a ^^ or a !! repeat the last
		 * instruction this is thus similar to both IRAF and the C
		 * shell but breaks the robot convention of having a "!" as a
		 * comment
		 */
		if (streqn(text, "!!") || streqn(text, "^^"))
			return (char *) xv_get(history_list, PANEL_LIST_STRING, nrows - 1);

		len = strlen(text) - 1;

		for (i = nrows - 1; i >= 0; i--) {
			if (strncmp(text + 1,
				    (char *) (xv_get(history_list, PANEL_LIST_STRING, i)), len) == 0) {
				return (char *) xv_get(history_list, PANEL_LIST_STRING, i);
			}
		}
		/* got to end of loop without a match */
		totext2_("Modifier failed");
		totext2_("(No matching pattern)");
		return (text);
	}
}
