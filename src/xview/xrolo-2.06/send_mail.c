/************************************************************************/
/*	Copyright 1990 by Chuck Musciano and Harris Corporation		*/
/*									*/
/*	Permission to use, copy, modify, and distribute this software	*/
/*	and its documentation for any purpose and without fee is	*/
/*	hereby granted, provided that the above copyright notice	*/
/*	appear in all copies and that both that copyright notice and	*/
/*	this permission notice appear in supporting documentation, and	*/
/*	that the name of Chuck Musciano and Harris Corporation not be	*/
/*	used in advertising or publicity pertaining to distribution	*/
/*	of the software without specific, written prior permission.	*/
/*	Chuck Musciano and Harris Corporation make no representations	*/
/*	about the suitability of this software for any purpose.  It is	*/
/*	provided "as is" without express or implied warranty.		*/
/*									*/
/*	This code contains data and information that is proprietary	*/
/*	to Casio Corporation.  You may be subject to legal action if	*/
/*	this information is released without explicit permission from	*/
/*	Casio.								*/
/************************************************************************/

/************************************************************************/
/*									*/
/*	send_mail.c	display a little information window		*/
/*									*/
/************************************************************************/

#include	<stdio.h>
#include	<ctype.h>
#include    "patchlevel.h"
#include	<sys/param.h>
#include	<sys/types.h>
#include	<xview/xview.h>
#include	<xview/panel.h>
#include	<xview/textsw.h>
#include    <xview/notice.h>
#include	<X11/Xutil.h>

typedef struct {
	Xv_opaque	mail;
	Xv_opaque	mail_controls;
	Xv_opaque	msg_21;
	Xv_opaque	msg_22;
	Xv_opaque	address;
	Xv_opaque	other_address;
	Xv_opaque	message;
	Xv_opaque	mail_controls1;
	Xv_opaque	accept_send;
	Xv_opaque	cancel_mail;
} xrolo_mail_objects;

int INSTANCE = 101;

extern xrolo_mail_objects	*xrolo_mail_objects_initialize();
extern Xv_opaque	xrolo_mail_mail_create();
extern Xv_opaque	xrolo_mail_mail_controls_create();
extern Xv_opaque	xrolo_mail_msg_21_create();
extern Xv_opaque	xrolo_mail_msg_22_create();
extern Xv_opaque	xrolo_mail_address_create();
extern Xv_opaque	xrolo_mail_other_address_create();
extern Xv_opaque	xrolo_mail_message_create();
extern Xv_opaque	xrolo_mail_mail_controls1_create();
extern Xv_opaque	xrolo_mail_accept_send_create();
extern Xv_opaque	xrolo_mail_cancel_mail_create();

static xrolo_mail_objects	*dialog = NULL;

/*
 * Initialize an instance of object `mail'.
 */
xrolo_mail_objects *
xrolo_mail_objects_initialize(ip, owner)
	xrolo_mail_objects	*ip;
	Xv_opaque	owner;
{
	if (!ip && !(ip = (xrolo_mail_objects *) calloc(1, sizeof (xrolo_mail_objects))))
		return (xrolo_mail_objects *) NULL;
	if (!ip->mail)
		ip->mail = xrolo_mail_mail_create(ip, owner);
	if (!ip->mail_controls)
		ip->mail_controls = xrolo_mail_mail_controls_create(ip, ip->mail);
	if (!ip->msg_21)
		ip->msg_21 = xrolo_mail_msg_21_create(ip, ip->mail_controls);
	if (!ip->msg_22)
		ip->msg_22 = xrolo_mail_msg_22_create(ip, ip->mail_controls);
	if (!ip->address)
		ip->address = xrolo_mail_address_create(ip, ip->mail_controls);
	if (!ip->other_address)
		ip->other_address = xrolo_mail_other_address_create(ip, ip->mail_controls);
	if (!ip->message)
		ip->message = xrolo_mail_message_create(ip, ip->mail);
	if (!ip->mail_controls1)
		ip->mail_controls1 = xrolo_mail_mail_controls1_create(ip, ip->mail);
	if (!ip->accept_send)
		ip->accept_send = xrolo_mail_accept_send_create(ip, ip->mail_controls1);
	if (!ip->cancel_mail)
		ip->cancel_mail = xrolo_mail_cancel_mail_create(ip, ip->mail_controls1);
	return ip;
}

/*
 * Create object `mail' in the specified instance.

 */
Xv_opaque
xrolo_mail_mail_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void	done_mail();
	Xv_opaque	obj;
	
	obj = xv_create(owner, FRAME_CMD,
		XV_KEY_DATA, INSTANCE, ip,
		XV_WIDTH, 506,
		XV_HEIGHT, 338,
		XV_LABEL, "Xrolo: Send mail",
		XV_SHOW, FALSE,
		FRAME_SHOW_FOOTER, FALSE,
		FRAME_SHOW_RESIZE_CORNER, FALSE,
		FRAME_CMD_PUSHPIN_IN, FALSE,
		FRAME_DONE_PROC, done_mail,
		NULL);
	xv_set(xv_get(obj, FRAME_CMD_PANEL), WIN_SHOW, FALSE, NULL);
	return obj;
}

/*
 * Create object `mail_controls' in the specified instance.

 */
Xv_opaque
xrolo_mail_mail_controls_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:mail_controls",
		XV_X, 0,
		XV_Y, 0,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, 92,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `msg_21' in the specified instance.

 */
Xv_opaque
xrolo_mail_msg_21_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:msg_21",
		XV_X, 14,
		XV_Y, 14,
		XV_WIDTH, 478,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "Your feedback is appreciated.  To ensure that your message reaches me, check",
		PANEL_LABEL_BOLD, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `msg_22' in the specified instance.

 */
Xv_opaque
xrolo_mail_msg_22_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_MESSAGE,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:msg_22",
		XV_X, 14,
		XV_Y, 30,
		XV_WIDTH, 395,
		XV_HEIGHT, 13,
		PANEL_LABEL_STRING, "that the \"To\" address is valid for your site before clicking \"Send\".",
		PANEL_LABEL_BOLD, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `address' in the specified instance.

 */
Xv_opaque
xrolo_mail_address_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		handle_address();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_CHOICE, PANEL_DISPLAY_LEVEL, PANEL_CURRENT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:address",
		XV_X, 14,
		XV_Y, 64,
		XV_WIDTH, 278,
		XV_HEIGHT, 23,
		PANEL_VALUE_X, 43,
		PANEL_VALUE_Y, 64,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_CHOICE_NROWS, 1,
		PANEL_LABEL_STRING, "To:",
		PANEL_NOTIFY_PROC, handle_address,
		PANEL_CHOICE_STRINGS,
			"luis@rice.edu",
			"ronbo@vixen.uucp",
			"Other:",
			0,
		NULL);
	return obj;
}

/*
 * Create object `other_address' in the specified instance.

 */
Xv_opaque
xrolo_mail_other_address_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_TEXT,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:other_address",
		XV_X, 105,
		XV_Y, 69,
		XV_WIDTH, 385,
		XV_HEIGHT, 15,
		PANEL_VALUE_X, 114,
		PANEL_VALUE_Y, 69,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		PANEL_VALUE_DISPLAY_LENGTH, 47,
		PANEL_VALUE_STORED_LENGTH, 256,
		PANEL_READ_ONLY, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `message' in the specified instance.

 */
Xv_opaque
xrolo_mail_message_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, TEXTSW,
		XV_KEY_DATA, INSTANCE, ip,
		XV_X, 0,
		XV_Y, 92,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, 188,
		OPENWIN_SHOW_BORDERS, TRUE,
		NULL);
	return obj;
}

/*
 * Create object `mail_controls1' in the specified instance.

 */
Xv_opaque
xrolo_mail_mail_controls1_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:mail_controls1",
		XV_X, 0,
		XV_Y, 280,
		XV_WIDTH, WIN_EXTEND_TO_EDGE,
		XV_HEIGHT, WIN_EXTEND_TO_EDGE,
		WIN_BORDER, FALSE,
		NULL);
	return obj;
}

/*
 * Create object `accept_send' in the specified instance.

 */
Xv_opaque
xrolo_mail_accept_send_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		send_mail();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:accept_send",
		XV_X, 193,
		XV_Y, 18,
		XV_WIDTH, 48,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Send",
		PANEL_NOTIFY_PROC, send_mail,
		NULL);
	return obj;
}

/*
 * Create object `cancel_mail' in the specified instance.

 */
Xv_opaque
xrolo_mail_cancel_mail_create(ip, owner)
	caddr_t		ip;
	Xv_opaque	owner;
{
	extern void		cancel_mail();
	Xv_opaque	obj;
	
	obj = xv_create(owner, PANEL_BUTTON,
		XV_KEY_DATA, INSTANCE, ip,
		XV_HELP_DATA, "xrolo:cancel_mail",
		XV_X, 253,
		XV_Y, 18,
		XV_WIDTH, 59,
		XV_HEIGHT, 20,
		PANEL_LABEL_STRING, "Cancel",
		PANEL_NOTIFY_PROC, cancel_mail,
		NULL);
	return obj;
}

send_email(item, event, cp)
Panel_item	item;
Event		*event;
char *cp;
{
	extern Frame frame;
	if (dialog == NULL) {
		dialog = xrolo_mail_objects_initialize(NULL, frame);
		place_dialog(frame, dialog->mail);
		xv_set(dialog->other_address, WIN_SHOW, FALSE, NULL);
	}
	xv_set(dialog->address, PANEL_CHOICE_STRINGS, cp, NULL, NULL);
	xv_set(dialog->mail, XV_SHOW, TRUE, NULL);
	xv_set(item, PANEL_NOTIFY_STATUS, XV_OK, 0);
}

/************************************************************************/
extern Frame frame;
popup_send_mail(item, event)
Panel_item	item;
Event		*event;
{
	if (dialog == NULL) {
	   dialog = xrolo_mail_objects_initialize(NULL, frame);
	   place_dialog(frame, dialog->mail);
	   xv_set(dialog->other_address, WIN_SHOW, FALSE, NULL);
	   }
	xv_set(dialog->mail, XV_SHOW, TRUE, NULL);
	xv_set(item, PANEL_NOTIFY_STATUS, XV_OK, 0);
}

/************************************************************************/
void	handle_address(item, value, event)

Panel_item	item;
int		value;
Event		*event;

{
	if (value == 2) {
	   xv_set(dialog->other_address, WIN_SHOW, TRUE, NULL);
	   panel_paint(dialog->address, PANEL_NO_CLEAR);
	   panel_paint(dialog->other_address, PANEL_NO_CLEAR);
	   }
	else {
	   xv_set(dialog->other_address, WIN_SHOW, FALSE, NULL);
	   panel_paint(dialog->address, PANEL_NO_CLEAR);
	   }
}

/************************************************************************/
void	done_mail(frame)

Frame		frame;

{
	textsw_reset(dialog->message, 0, 0);
	xv_set(frame, XV_SHOW, FALSE, 0);
}

/************************************************************************/
	void	send_mail(item, event)

Panel_item	item;
Event		*event;

{	char	cmd[1024], *buf;
	int	val, size;
	FILE	*pipe;

	xv_set(item, PANEL_NOTIFY_STATUS, XV_ERROR, 0);
	val = (int) xv_get(dialog->address, PANEL_VALUE);
	if (val != 2)
	   sprintf(cmd, "%s %s", MAILER, xv_get(dialog->address, PANEL_CHOICE_STRING, val));
	else {
	   buf = (char *) xv_get(dialog->other_address, PANEL_VALUE);
	   if (is_empty(buf)) {
	      error("You must specify an alternate address before sending your message");
	      return;
	      }
	   else
	      sprintf(cmd, "%s %s", MAILER, buf);
	   }
	size = (int) xv_get(dialog->message, TEXTSW_LENGTH);
	if (size == 0) {
	   error("Please type a message before sending the mail");
	   return;
	   }
	if ((pipe = popen(cmd, "w")) == NULL) {
	   error("Could not invoke \"%s\" to send mail", cmd);
	   return;
	   }
	lets_get_busy(frame, TRUE, NULL);
	buf = (char *) malloc(size);
	xv_get(dialog->message, TEXTSW_CONTENTS, 0, buf, size);
	fprintf(pipe, "Subject: Xrolo %s patch level %d comment\n\n",
			VERSION, PATCHLEVEL);
	if (fwrite(buf, 1, size, pipe) != size) {
	   error("Could not write message to the mailer");
	   free(buf);
	   return;
	   }
	pclose(pipe);
	free(buf);
	textsw_reset(dialog->message, 0, 0);
	lets_get_busy(frame, FALSE, NULL);
	xv_set(item, PANEL_NOTIFY_STATUS, XV_OK, 0);
}

/************************************************************************/
void	cancel_mail(item, event)

Panel_item	item;
Event		*event;

{
	textsw_reset(dialog->message, 0, 0);
	xv_set(item, PANEL_NOTIFY_STATUS, XV_OK, 0);
}

place_dialog(base, dialog)
Xv_opaque	base;
Xv_opaque	dialog;
{	
	Rect		br, dr, sr;
	XWMHints	*hints;

	sr = *((Rect *) xv_get(base, WIN_SCREEN_RECT));
	frame_get_rect(base, &br);
	frame_get_rect(dialog, &dr);
	if (rect_right(&br) + dr.r_width < sr.r_width) {
	   dr.r_left = rect_right(&br);
	   dr.r_top = br.r_top;
	   }
	else if (dr.r_width <= br.r_left) {
	   dr.r_left = br.r_left - dr.r_width;
	   dr.r_top = br.r_top;
	   }
	else {
	   dr.r_left = br.r_left + 32;
	   dr.r_top = br.r_top + 32;
	   }
	if (dr.r_top + dr.r_height > sr.r_height)
	   dr.r_top = sr.r_height - dr.r_height;
	if (dr.r_top < 0)
	   dr.r_top = 0;
	frame_set_rect(dialog, &dr);

	hints = XGetWMHints(xv_get(dialog, XV_DISPLAY), xv_get(dialog, XV_XID));
	hints->flags |= StateHint;
	hints->initial_state = NormalState;
	XSetWMHints(xv_get(dialog, XV_DISPLAY), xv_get(dialog, XV_XID), hints);
	XFree(hints);
}

error(a, b, c, d, e, f)
char	*a, *b, *c, *d, *e, *f;
{	char	buf[512];

	sprintf(buf, a, b, c, d, e, f);
	notice_prompt(frame, NULL,
		         NOTICE_MESSAGE_STRINGS, buf, 0,
		         NOTICE_BUTTON_YES, "OK",
		      0);
}

int	is_empty(s)
char	*s;
{
	if (s == NULL)
	   return(TRUE);
	for (; *s; s++)
	   if (!isspace(*s))
	      return(FALSE); 
	return(TRUE);
}

static set_busy(base, busy, skip)
Frame	base;
int	busy;
Frame	skip;
{	Frame	subframe;
	int	i;

	for (i = 1; subframe = (Frame) xv_get(base, FRAME_NTH_SUBFRAME, i); i++)
	   if (subframe != skip)
	      if (xv_get(subframe, XV_SHOW))
	         xv_set(subframe, FRAME_BUSY, busy, NULL);
	xv_set(base, FRAME_BUSY, busy, NULL);
}

lets_get_busy(base, busy, skip)
Frame	base;
int	busy;
Frame	skip;
{	static	int	depth = 0;

	if (busy) {
	   if (depth == 0)
	      set_busy(base, TRUE, skip);
	   depth++;
	   }
	else if (depth > 0)
	   if (--depth == 0)
	      set_busy(base, FALSE, NULL);
}
