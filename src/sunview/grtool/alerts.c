/*
	alerts.c - yesno(), errwin()

	$Header: alerts.c,v 1.3 89/09/07 22:16:08 pturner Locked $
*/

#include <stdio.h>
#include <suntool/sunview.h>
#include <suntool/panel.h>
#include <pixrect/pixrect.h>
#include <suntool/alert.h>

extern Frame main_frame;

extern int inwin;	/* true if in window system */

/* alerts */
int yesno(msg1,msg2,msgyes,msgno)
    char *msg1, *msg2;
    char *msgyes,*msgno;
{
    char buf[300];
    int result;
    Event event;		/* unused */
    int beep = 0;
    if (!inwin) {
	fprintf(stderr,"%s\n",msg1);
	fprintf(stderr,"%s\n","abort? (y/n)");
	gets(buf);
	if (buf[0]=='y') {
	    return 1;
	}
	else {
	    return 0;
	}
    }

    result = alert_prompt(main_frame, &event,
			  ALERT_MESSAGE_STRINGS,
			  msg1, msg2,
			  0,
			  ALERT_NO_BEEPING, (beep) ? 0 : 1,
			  ALERT_BUTTON_YES, msgyes,
			  ALERT_BUTTON_NO, msgno,
			  ALERT_TRIGGER, ACTION_STOP,	/* allow either YES or
							 * NO answer */
			  0);
    switch (result) {
    case ALERT_YES:
	return 1;
	break;
    case ALERT_NO:
	return 0;
	break;
    case ALERT_TRIGGERED:	/* result of ACTION_STOP trigger */
	break;
    case ALERT_FAILED:		/* not likely to happen unless out of memory */
	sprintf(buf, "%s  Press \"Continue\" to proceed.", msg1);
	result = confirm_ok(buf);
	break;
    }
}

errwin(msg)
    char *msg;
{
    char buf[300];
    int result;
    Event event;		/* unused */
    char *contine_msg = "Press \"Continue\" to proceed.";
    int beep = 0;
    if (!inwin) {
	fprintf(stderr,"%s\n",msg);
	return;
    }

    result = alert_prompt(main_frame, &event,
			  ALERT_MESSAGE_STRINGS,
			  msg,
			  contine_msg,
			  0,
			  ALERT_NO_BEEPING, (beep) ? 0 : 1,
			  ALERT_BUTTON_YES, "Continue",
			  ALERT_TRIGGER, ACTION_STOP,	/* allow either YES or
							 * NO answer */
			  0);
    switch (result) {
    case ALERT_YES:
    case ALERT_TRIGGERED:	/* result of ACTION_STOP trigger */
	break;
    case ALERT_FAILED:		/* not likely to happen unless out of memory */
	sprintf(buf, "%s  Press \"Continue\" to proceed.", msg);
	result = confirm_ok(buf);
	break;
    }
}

/* confirmer routines to be used if alert fails for any reason */

static Frame init_confirmer();
static int confirm();
static void yes_no_ok();

int confirm_yes(message)
    char *message;
{
    return confirm(message, FALSE);
}

int confirm_ok(message)
    char *message;
{
    return confirm(message, TRUE);
}

static int confirm(message, ok_only)
    char *message;
    int ok_only;
{
    Frame confirmer;
    int answer;

    /* create the confirmer */
    confirmer = init_confirmer(message, ok_only);
    /* make the user answer */
    answer = (int) window_loop(confirmer);
    /* destroy the confirmer */
    window_set(confirmer, FRAME_NO_CONFIRM, TRUE, 0);
    window_destroy(confirmer);
    return answer;
}

static Frame
 init_confirmer(message, ok_only)
    char *message;
    int ok_only;
{
    Frame confirmer;
    Panel panel;
    Panel_item message_item;
    int left, top, width, height;
    Rect *r;
    struct pixrect *pr;

    confirmer = window_create(0, FRAME, FRAME_SHOW_LABEL, FALSE, 0);
    panel = window_create(confirmer, PANEL, 0);
    message_item = panel_create_item(panel, PANEL_MESSAGE,
				     PANEL_LABEL_STRING, message, 0);

    if (ok_only) {
	pr = panel_button_image(panel, "Continue", 8, 0);
	width = pr->pr_width;
    } else {
	pr = panel_button_image(panel, "Cancel", 8, 0);
	width = 2 * pr->pr_width + 10;
    }

    /* center the yes/no or ok buttons under the message */
    r = (Rect *) panel_get(message_item, PANEL_ITEM_RECT);
    left = (r->r_width - width) / 2;
    if (left < 0)
	left = 0;
    top = rect_bottom(r) + 5;

    if (ok_only) {
	panel_create_item(panel, PANEL_BUTTON,
			  PANEL_ITEM_X, left, PANEL_ITEM_Y, top,
			  PANEL_LABEL_IMAGE, pr,
			  PANEL_CLIENT_DATA, 1,
			  PANEL_NOTIFY_PROC, yes_no_ok,
			  0);
    } else {
	panel_create_item(panel, PANEL_BUTTON,
			  PANEL_ITEM_X, left, PANEL_ITEM_Y, top,
			  PANEL_LABEL_IMAGE, pr,
			  PANEL_CLIENT_DATA, 0,
			  PANEL_NOTIFY_PROC, yes_no_ok,
			  0);
	panel_create_item(panel, PANEL_BUTTON,
	      PANEL_LABEL_IMAGE, panel_button_image(panel, "Confirm", 8, 0),
			  PANEL_CLIENT_DATA, 1,
			  PANEL_NOTIFY_PROC, yes_no_ok,
			  0);
    }

    window_fit(panel);
    window_fit(confirmer);

    /* center the confirmer frame on the screen */
    r = (Rect *) window_get(confirmer, WIN_SCREEN_RECT);
    width = (int) window_get(confirmer, WIN_WIDTH);
    height = (int) window_get(confirmer, WIN_HEIGHT);
    left = (r->r_width - width) / 2;
    top = (r->r_height - height) / 2;
    if (left < 0)
	left = 0;
    if (top < 0)
	top = 0;
    window_set(confirmer, WIN_X, left, WIN_Y, top, 0);

    return confirmer;
}

static void yes_no_ok(item, event)
    Panel_item item;
    Event *event;
{
    window_return(panel_get(item, PANEL_CLIENT_DATA));
}
