/*
 *	@(#)x.c	1.36 12/16/94
 *
 *	(c) Copyright 1993-1994 by Mark Grant. All right reserved.
 *	The author assumes no liability for damages resulting from the 
 *	use of this software, even if the damage results from defects in
 *	this software. No warranty is expressed or implied.
 *
 *	This software is being distributed under the GNU Public Licence,
 *	see the file COPYING for more details.
 *
 *			- Mark Grant (mark@unicorn.com) 29/6/94
 *	
 *      Menu item for selecting Mail files added by
 *      	- Keith Paskett (keith.paskett@sdl.usu.edu) 8 Sep 1994
 */

#include <stdlib.h>
#include <unistd.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <dirent.h>
#include <sys/dirent.h>
#include <X11/Xos.h>
#ifndef MAXPATHLEN
#include <sys/param.h>
#endif /* MAXPATHLEN */
#include <xview/xview.h>
#include <xview/frame.h>
#include <xview/panel.h>
#include <xview/canvas.h>
#include <xview/sel_attrs.h>
#include <xview/textsw.h>
#include <xview/notice.h>
#include <xview/icon.h>
#include <xview/font.h>
#include <xview/scrollbar.h>
#include <xview/notify.h>

#include "def.h"
#include "buffers.h"
#include "message.h"
#include "mailrc.h"
#include "gui.h"

#define	GC_KEY	10

static	Frame	main_frame;
static	Panel	top_panel;
static	Panel	main_panel;
static	Panel	display_panel;
static	Panel_item	file_name_item;
static	Panel_item	include_item,deliver_item,header_item;
static	Panel_item	clear_item, addkey_item;
static	Panel_item	log_item,sign_item,encrypt_item;
static	Canvas		list_canvas;
static	Scrollbar	v_scroll;
static	Xv_font		list_font;
static	Icon		icon,newmail_icon;
static	Server_image	icon_image, newmail_icon_image, icon_image_mask;
static	Xv_window	list_window;

#define LIST_DISPLACEMENT	16

static unsigned short icon_bits[] = {
#include "privtool.icon"
};

static unsigned short	newmail_icon_bits[] = {
#include "privtool-new.icon"
};

static unsigned short	icon_bits_mask[] = {
#include "privtool-mask.icon"
};

extern	char	default_mail_file[];

static	Panel_item	send_to_item, send_cc_item;
static	Panel_item	send_subject_item;
static	Textsw	deliver_body_window;
static	Frame	pass_frame;
static	Frame	deliver_frame;
static	Panel	deliver_panel;
static	Panel_item	pass_item;
static	Menu		files_menu[3];
static	Menu_item	files_item[3];
	Menu_item	add_path_to_menu();

static	Window  map_win;
static	Display *dpy;
static	GC	gc,clear_gc;

/* Draw a box around the message in the list to show it's selected */

static	draw_box_round_message(m)

MESSAGE	*m;

{
	int	y,w;
	XPoint	points[5];

	y = m->list_pos * LIST_DISPLACEMENT;
	w = xv_get (list_canvas, XV_WIDTH);

	points[0].x = 0;
	points[0].y = y - LIST_DISPLACEMENT + 4;
	points[1].x = w - 2;
	points[1].y = points[0].y;
	points[2].x = points[1].x;
	points[2].y = points[1].y + LIST_DISPLACEMENT - 1;
	points[3].x = 0;
	points[3].y = points[2].y;
	points[4].x = 0;
	points[4].y = points[0].y;

	XDrawLines(dpy, map_win, gc, points, 5, CoordModeOrigin);
}

/* Display the message description in the list */

void	display_message_description(m)

MESSAGE	*m;

{
	int	y,w;

	y = m->list_pos * LIST_DISPLACEMENT;
	w = xv_get (list_canvas, XV_WIDTH);

	XFillRectangle (dpy, map_win, clear_gc, 0, y - LIST_DISPLACEMENT + 4, 
		w, LIST_DISPLACEMENT);
	XDrawString(dpy,map_win,gc,0,
		y,
		m->description,strlen(m->description));

		if (m->flags & MESS_SELECTED)
			draw_box_round_message(m);
}

/* Display a footer string */

void	set_main_footer(s)

char	*s;

{
	if (main_frame)
		xv_set(main_frame,
			FRAME_LEFT_FOOTER,s,
			NULL);
}

/* Clear the footer string */

void	clear_main_footer()

{
	set_main_footer("");
}

#define NORMAL_ICON	0
#define NEWMAIL_ICON	1

static	int	icon_type;

/* Show the icon for new mail */

void	show_newmail_icon()

{
	if (newmail_icon && icon_type != NEWMAIL_ICON) {

		/* Update the icon image */

		xv_set (icon_image,
			SERVER_IMAGE_BITS, newmail_icon_bits,
			NULL);

		/* Force a repaint */

		if (xv_get (main_frame, FRAME_CLOSED)) {
			xv_set (main_frame,
				WIN_ALARM, 
				NULL);
			xv_set (icon,
				ICON_IMAGE, icon_image,
				NULL);
		}

		icon_type = NEWMAIL_ICON;
	}
}

/* Display the icon for no new mail */

void	show_normal_icon()

{
	if (icon && icon_type != NORMAL_ICON) {

		/* Update the icon image */

		xv_set (icon_image,
			SERVER_IMAGE_BITS, icon_bits,
			NULL);

		/* Force a repaint */

		if (xv_get (main_frame, FRAME_CLOSED)) {
			xv_set (icon,
				ICON_IMAGE, icon_image,
				NULL);
		}

		icon_type = NORMAL_ICON;
	}
}

/* Close the passphrase window */

void	close_passphrase_window()

{
	if (pass_frame)
		xv_set(pass_frame,
			XV_SHOW, FALSE,
			NULL);
}

/* Open the passphrase window */

void	open_passphrase_window(s)

char	*s;

{
	if (pass_frame)
		xv_set(pass_frame,
			XV_SHOW, TRUE,
			FRAME_LABEL, s,
			FRAME_CLOSED, FALSE, /* Just in case */
			NULL);
}

/* MUST be possible to call this function before pass_item is set up */

char	*read_passphrase_string()

{
	if (!pass_item)
		return NULL;

	return (char *)xv_get(pass_item,
		PANEL_VALUE);
}

/* Clear the displayed passphrase string */

void	clear_passphrase_string()

{
	if (pass_item)
		xv_set(pass_item,
			PANEL_VALUE,"",
			NULL);
}

static	Panel	pass_panel;

/* Create the passphrase window */

void	create_passphrase_window()

{
	if (!pass_frame) {
		pass_frame = (Frame) xv_create(main_frame, FRAME,
			XV_SHOW, FALSE,
			NULL);

		pass_panel = (Panel) xv_create(pass_frame, PANEL,
			XV_HEIGHT, 80,
			PANEL_LAYOUT, PANEL_HORIZONTAL,
			NULL);

		pass_item = xv_create (pass_panel, PANEL_TEXT,
			PANEL_LABEL_STRING, "Passphrase :",
			PANEL_VALUE_DISPLAY_LENGTH,20,
			PANEL_MASK_CHAR,'*',
			PANEL_NOTIFY_PROC, got_passphrase,
			NULL);

		(void) xv_create(pass_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Abort",
			PANEL_NOTIFY_PROC, abort_passphrase_proc,
			NULL);

		window_fit(pass_panel);
		window_fit(pass_frame);
	}
}

/* Set the value of the log/encrypt/etc item */

void	update_log_item(f)

int	f;

{
	if (log_item)
		xv_set(log_item,
			PANEL_VALUE, f,
			NULL);
}

int	read_only_notice_proc ()

{
	int	choice = NOTICE_YES;

	choice = notice_prompt(top_panel, NULL,
		NOTICE_MESSAGE_STRINGS, "Mail file is read-only so changes", 
			"Cannot be saved.",
			NULL,
		NOTICE_BUTTON_YES, "Abort",
		NOTICE_BUTTON_NO, "Ignore",
		NULL);

	update_random ();

	return (choice == NOTICE_YES);
}

/* Warn the user that we failed to save correctly */

int	failed_save_notice_proc ()

{
	int	choice = NOTICE_YES;

	choice = notice_prompt(top_panel, NULL,
		NOTICE_MESSAGE_STRINGS, "Error occured while writing to disk !", 
			"Press abort to cancel this operation, or ignore to",
			"ignore the error (which may cause a loss of data !).",
			NULL,
		NOTICE_BUTTON_YES, "Abort",
		NOTICE_BUTTON_NO, "Ignore",
		NULL);

	update_random ();

	return (choice == NOTICE_YES);
}

/* Tell the user that they don't have the correct secret key */

int	no_sec_notice_proc(w)

int	w;

{
	int	choice;
	Panel	p;

	switch (w) {

		case ERROR_DELIVERY:
		p = deliver_panel;
		break;

		case ERROR_READING:
		p = display_panel;
		break;
	}

	choice = notice_prompt(p, NULL,
		NOTICE_MESSAGE_STRINGS, "Error: No secret key found !", 
			NULL,
		NOTICE_BUTTON_YES, "Send Unsigned",
		NOTICE_BUTTON_NO, "Abort",
		NULL);

	update_random();

	return (choice == NOTICE_YES);
}

/* Confirm that we want to read the file despite failure to lock it */

int	confirm_unlocked_read ()

{
	int	choice;

	/* Firewall in case we're called before opening the UI */

	if (!main_panel)
		return FALSE;

	choice = notice_prompt (main_panel, NULL,
		NOTICE_MESSAGE_STRINGS,
			"File cannot be locked for read. Open anyway ?",
			NULL,
		NOTICE_BUTTON_YES, "Open",
		NOTICE_BUTTON_NO, "Abort",
		NULL);

	update_random ();

	return (choice == NOTICE_YES);
}

/* Warn the user that we couldn't find a public key for the recipient */

int	no_key_notice_proc(w)

int	w;

{
	int	choice;
	Panel	p;

	switch (w) {

		case ERROR_DELIVERY:
		p = deliver_panel;
		break;

		case ERROR_READING:
		p = display_panel;
		break;
	}

	choice = notice_prompt(p, NULL,
		NOTICE_MESSAGE_STRINGS, "Error: Recipient has no public key !", 
			NULL,
		NOTICE_BUTTON_YES, "Send Unencrypted",
		NOTICE_BUTTON_NO, "Abort",
		NULL);

	update_random();

	return (choice == NOTICE_YES);
}

/* Warn the user that we got a bad PGP message (bad format, I mean 8-)) */

void	bad_file_notice(w)

int	w;

{
	Panel	p;

	switch (w) {

		case ERROR_DELIVERY:
		p = deliver_panel;
		break;

		case ERROR_READING:
		p = display_panel;
		break;
	}

	(void) notice_prompt(p, NULL,
		NOTICE_MESSAGE_STRINGS, "Error: Bad message format !", NULL,
		NOTICE_BUTTON_YES, "Abort",
		NULL);

	update_random();
}

/* Tell the user that they entered an incorrect passphrase */

int	bad_pass_phrase_notice(w)

int	w;

{
	Panel	p;
	int	choice;

	switch (w) {

		case ERROR_DELIVERY:
		p = deliver_panel;
		break;

		case ERROR_READING:
		p = display_panel;
		break;
	}

	choice = notice_prompt(p, NULL,
		NOTICE_MESSAGE_STRINGS, "Error: Bad Pass Phrase !", NULL,
		NOTICE_BUTTON_YES, "Reenter",
		NOTICE_BUTTON_NO, "Abort",
		NULL);

	update_random();

	return (choice == NOTICE_YES);
}

/* Return the currently selected file name */

char	*read_file_name()

{
	return  (char *)xv_get (file_name_item,PANEL_VALUE);
}

/* Set the currently selected file name */

void	set_file_name (s)

char	*s;

{
	xv_set (file_name_item, 
		PANEL_VALUE, s,
		NULL);
}

/* Update the main list of mail messages */

void	update_message_list()

{
	char	s[128];
	char	b[32];
	int	oldh, newh;
	int	o,h;
	int	w;
	int	v_pos;

	/* Firewall */

	if (!list_canvas)
		return;

	oldh = xv_get (list_canvas, CANVAS_HEIGHT);
	newh = messages.number * LIST_DISPLACEMENT + 4;

	o = oldh / LIST_DISPLACEMENT;
	h = messages.number;

	w = xv_get (list_canvas, XV_WIDTH);
	if (dpy)
		XFillRectangle (dpy, map_win, clear_gc, 0, newh, 
			w, oldh - newh);

	if ((h+15)/16 != (o+15)/16 || (h > o)) {
		xv_set (list_canvas, CANVAS_HEIGHT,
			(newh+15) & ~15,
			NULL);
	}

	if (!deleted.number)
		sprintf (s, "%d messages", messages.number);
	else
		sprintf (s, "%d messages, %d deleted", messages.number,
			deleted.number);

	if (messages.new) {
		sprintf (b, ", %d new", messages.new);
		strcat (s, b);
	}

	if (messages.unread) {
		sprintf (b, ", %d unread", messages.unread);
		strcat (s, b);
	}

#ifdef SHOW_ENCRYPTED
	if (messages.encrypted) {
		sprintf (b, ", %d encrypted", messages.encrypted);
		strcat (s, b);
	}
#endif

	xv_set (main_frame,
		FRAME_RIGHT_FOOTER, s,
		NULL);

	/* Following doesn't seem to work, but all the message list code 
	   needs to be rewritten anyway... */

#if 0
	if (list_window) {
		v_pos = xv_get (v_scroll, SCROLLBAR_VIEW_START);
		h = xv_get (list_window, XV_HEIGHT);

		if (v_pos >= (newh - h)) {
			xv_set (v_scroll, SCROLLBAR_VIEW_START, newh - h, NULL);
		}
	}
#endif
}


#define READ_SIZE	128

/* Read the message out of the edit window into a buffer */

void	read_message_to_deliver(b)

BUFFER	*b;

{
	char	buff[READ_SIZE];
	Textsw_index	next_pos = (Textsw_index) 0;
	Textsw_index	last_pos;

	do {
		last_pos = next_pos;
		next_pos = (Textsw_index) xv_get(deliver_body_window,
			TEXTSW_CONTENTS,next_pos,buff,
			READ_SIZE);
		if (last_pos != next_pos)
			add_to_buffer(b,buff,next_pos-last_pos);
	} while (last_pos != next_pos);
}

/* Get the recipient for the message */

char	*read_recipient()

{
	return (char *)xv_get(send_to_item,PANEL_VALUE);
}

/* Get the subject for the message */

char	*read_subject()

{
	 return (char *)xv_get(send_subject_item,PANEL_VALUE);
}

/* Get the cc: line */

char	*read_cc()

{
	 return (char *)xv_get(send_cc_item,PANEL_VALUE);
}

/* Show the message composition window */

void	show_deliver_frame()

{
	if (deliver_frame)
		xv_set(deliver_frame,
			XV_SHOW,TRUE,
			NULL);
}

/* And close it */

void	close_deliver_window()

{
	if (deliver_frame)
		xv_set(deliver_frame,
			XV_SHOW,FALSE,
			NULL);
}


void	set_focus_to_body_proc()

{
	xv_set(deliver_body_window, WIN_SET_FOCUS, NULL);
}

/* Set the flags as appropriate when the user changes the value */

static	options_proc(item,value,event)

Panel_item	item;
int	value;
Event	*event;

{
	set_deliver_flags(value);
}

/* Setup the deliver window */

x_setup_send_window()

{
	char	*log,*dontlog;

	/* Create the frame if not already done */

	if (!deliver_frame) {
		deliver_frame = (Frame) xv_create(main_frame, FRAME,
			XV_WIDTH, 640,
			XV_HEIGHT, 440,
			FRAME_LABEL, "Compose Window",
			NULL);

		deliver_panel = (Panel) xv_create(deliver_frame, PANEL,
			XV_HEIGHT, 95,
			PANEL_LAYOUT, PANEL_HORIZONTAL,
			NULL);

		include_item = xv_create (deliver_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Include",
			NULL);

		deliver_item = xv_create (deliver_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Deliver",
			PANEL_NOTIFY_PROC, deliver_proc,
			NULL);

		header_item = xv_create (deliver_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Header",
			NULL);

		clear_item = xv_create (deliver_panel, PANEL_BUTTON,
			PANEL_LABEL_STRING, "Include",
			NULL);

		log_item = xv_create (deliver_panel, PANEL_CHECK_BOX,
			PANEL_LABEL_STRING, "Options",
			PANEL_CHOOSE_ONE, FALSE,
			PANEL_CHOICE_STRINGS, "Sign", "Encrypt", "Log", "Raw", NULL,
			PANEL_NOTIFY_PROC, options_proc,
			NULL);

		send_to_item = (Panel_item) xv_create(deliver_panel,
			PANEL_TEXT,
			PANEL_LABEL_STRING,"To :",
			PANEL_VALUE_DISPLAY_LENGTH, 45,
			XV_Y, 30,
			XV_X, 0,
			NULL);

		send_subject_item = (Panel_item) xv_create(deliver_panel,
			PANEL_TEXT,
			PANEL_LABEL_STRING,"Subject :",
			PANEL_VALUE_DISPLAY_LENGTH, 40,
			XV_Y, 50,
			XV_X, 0,
			NULL);

		send_cc_item = (Panel_item) xv_create(deliver_panel,
			PANEL_TEXT,
			PANEL_LABEL_STRING,"Cc :",
			PANEL_VALUE_DISPLAY_LENGTH, 45,
			PANEL_NOTIFY_PROC, set_focus_to_body_proc,
			XV_Y, 70,
			XV_X, 0,
			NULL);

		deliver_body_window = (Textsw) xv_create(deliver_frame, TEXTSW,
			XV_HEIGHT, 288,
			XV_Y, 95,
			TEXTSW_MEMORY_MAXIMUM,1000000,
			NULL);

		window_fit(deliver_body_window);
		window_fit(deliver_frame);
	}
	else

		/* Open frame just in case ! */

		xv_set (deliver_frame,
			FRAME_CLOSED, FALSE,
			NULL);

	/* Default to encryption and signature */

	deliver_flags = DELIVER_SIGN|DELIVER_ENCRYPT;

	/* Clear the bits if told to by mailrc */

	if (find_mailrc("nodefaultsign"))
		deliver_flags &= ~DELIVER_SIGN;
	if (find_mailrc("nodefaultencrypt"))
		deliver_flags &= ~DELIVER_ENCRYPT;

	/* Check mailrc for logging options */

	log = find_mailrc("nodontlogmessages");
	dontlog = find_mailrc("dontlogmessages");

	/* If logging enabled, set log options */

	if (log && !dontlog) {
		deliver_flags |= DELIVER_LOG;
	}

	if (find_mailrc("log-raw")) {
		deliver_flags |= DELIVER_RAW;
	}

	/* Finally, set the panel item */

	update_log_item(deliver_flags);

	/* And empty the text panel */

	textsw_erase(deliver_body_window,0,TEXTSW_INFINITY);

	/* All set ? Ok, show the frame ! */

	show_deliver_frame();
}

/* Set up a compose window */

static	send_message()

{
	int	i;

	setup_send_window();

	xv_set(send_subject_item,
		PANEL_VALUE,"",
		NULL);

	xv_set(send_to_item,
		PANEL_VALUE,"",
		NULL);

	xv_set(send_cc_item,
		PANEL_VALUE,"",
		NULL);
}

static char	attribution_string[] = " said :\n\n";

/* Reply to the sender without including the message */

static	reply_sender_no_include ()

{
	char	subject[256];

	update_random();

	if (!last_message_read)
		return;

	set_reply (last_message_read);

	setup_send_window();

	xv_set (send_cc_item,
		PANEL_VALUE, "",
		NULL);
	
	if (last_message_read->subject) {
		if (strncasecmp(last_message_read->subject,"Re:",3)) {
			sprintf(subject,"Re: %s\n",
				last_message_read->subject);
		}
		else
			strcpy(subject,last_message_read->subject);

		xv_set(send_subject_item,
			PANEL_VALUE, subject,
			NULL);
	}
	else
		xv_set(send_subject_item,
			PANEL_VALUE,"",
			NULL);

	xv_set(send_to_item,
		PANEL_VALUE,last_message_read->email,
		NULL);

	xv_set(deliver_body_window,
		TEXTSW_INSERTION_POINT,0,
		TEXTSW_FIRST_LINE,0,NULL);
}

/* Reply_to sender and include message */

static	reply_to_sender()

{
	int	i;
	byte	*mess;
	byte	*m;
	BUFFER	*b;
	char	subject[256];
	char	*indent;
	int	indent_l = 2;

	update_random();

	if (!last_message_read)
		return;

	set_reply (last_message_read);

	if (!(indent = find_mailrc("indentprefix"))) 
		indent = "> ";
	else
		indent_l = strlen (indent);

	setup_send_window();
	
	xv_set (send_cc_item,
		PANEL_VALUE, "",
		NULL);
	
	if (last_message_read->subject) {
		if (strncasecmp(last_message_read->subject,"Re:",3)) {
			sprintf(subject,"Re: %s\n",
				last_message_read->subject);
		}
		else
			strcpy(subject,last_message_read->subject);

		xv_set(send_subject_item,
			PANEL_VALUE, subject,
			NULL);
	}
	else
		xv_set(send_subject_item,
			PANEL_VALUE,"",
			NULL);

	xv_set(send_to_item,
		PANEL_VALUE,last_message_read->email,
		NULL);

	if (last_message_read->decrypted)
		b = last_message_read->decrypted;
	else
		b = message_contents(last_message_read);

	mess = b->message;
	i = b->length;

	textsw_insert(deliver_body_window,
		last_message_read->sender,
		strlen(last_message_read->sender));

	textsw_insert(deliver_body_window,
		attribution_string,
		strlen(attribution_string));

	while (i > 0) {
		if (indent_l)
			textsw_insert(deliver_body_window,
				indent, indent_l);
		m = mess;
		while (i-- && *m && *m!= '\n')
			m++;

		textsw_insert(deliver_body_window,(char *)mess,
			m - mess + 1);

		mess = m+1;
	}

	xv_set(deliver_body_window,
		TEXTSW_INSERTION_POINT,0,
		TEXTSW_FIRST_LINE,0,NULL);
}

static	Frame	display_frame;

/* Are we displaying a message ? */

int	is_displaying_message()

{
	if (!display_frame)
		return FALSE;

	return xv_get (display_frame,
		XV_SHOW);
}

/* Set the footer on the display window */

void	set_display_footer(s)

char	*s;

{
	if (display_frame)
		xv_set(display_frame,
			FRAME_LEFT_FOOTER,s,
			NULL);
}

static	Frame	header_frame;

/* Close the header window */

void	hide_header_frame()

{
	if (header_frame) {
		xv_set(header_frame,
			XV_SHOW, FALSE,
			NULL);
	}
}

/* Display the header */

static	void	display_header_proc()

{
	static	Textsw	header_window;

	update_random();

	if (!displayed_message)
		return;

	if (!header_frame) {
		header_frame = (Frame) xv_create(display_frame, FRAME,
			XV_WIDTH, 640,
			XV_HEIGHT, 240,
			FRAME_SHOW_FOOTER, FALSE,
			FRAME_LABEL, "Message Header",
			NULL);

		header_window = (Textsw) xv_create(header_frame, TEXTSW,
			TEXTSW_MEMORY_MAXIMUM,1000000,
			NULL);
	}

	xv_set(header_window, TEXTSW_READ_ONLY, FALSE,
			NULL);

	textsw_delete(header_window,
		0, TEXTSW_INFINITY);

	textsw_reset (header_window, 0, 0);

	textsw_insert(header_window,(char *)displayed_message->header->message,
		displayed_message->header->length);

	xv_set(header_window,TEXTSW_FIRST_LINE,0,
		TEXTSW_INSERTION_POINT, 0,
		TEXTSW_READ_ONLY, TRUE,
		NULL);

	xv_set(header_frame,
		XV_SHOW, TRUE,
		NULL);
}

/* Clear the footer on the display window */

void	clear_display_footer()

{
	set_display_footer("");
}

static	Panel_item	sender_item;
static	Panel_item	date_item;
static	Textsw	sig_window;
static	Textsw	body_window;

/* Catch resize events on the display frame */

static	void	display_frame_proc(w,e,a)

Xv_Window	w;
Event		*e;
Notify_arg	a;

{
	int	newh;

	if (event_id(e) != WIN_RESIZE)
		return;

	if (!body_window || !display_frame || !sig_window)
		return;

	newh = (int) xv_get (display_frame, XV_HEIGHT);

	xv_set (body_window, XV_HEIGHT, newh - 82, NULL);
	xv_set (sig_window, 
		XV_HEIGHT, 32,
		XV_Y, newh - 32,
		NULL);
}

/* Show the add key button */

void	show_addkey ()

{
	xv_set (addkey_item,
		PANEL_INACTIVE, FALSE,
		NULL);
}

/* Create the display window */

void	create_display_window()

{
	if (!display_frame) {
		display_frame = (Frame) xv_create(main_frame, FRAME,
			XV_WIDTH, 640,
			XV_HEIGHT, 400,
			FRAME_SHOW_FOOTER, TRUE,
			WIN_EVENT_PROC, display_frame_proc,
			NULL);

		display_panel = (Panel) xv_create(display_frame, PANEL,
			XV_HEIGHT, 50,
			PANEL_LAYOUT, PANEL_HORIZONTAL,
			NULL);

		sender_item = (Panel_item) xv_create(display_panel,
			PANEL_TEXT,
			PANEL_LABEL_STRING,"Sender :",
			PANEL_VALUE_DISPLAY_LENGTH, 40,
			XV_X, 0,
			XV_Y, 5,
			NULL);

		(void) xv_create(display_panel,
			PANEL_BUTTON,
			PANEL_LABEL_STRING,"Header",
			PANEL_NOTIFY_PROC, display_header_proc,
			NULL);

		addkey_item = xv_create (display_panel, 
			PANEL_BUTTON,
			PANEL_LABEL_STRING, "Add Key",
			PANEL_INACTIVE, TRUE,
			NULL);

		date_item = (Panel_item) xv_create(display_panel,
			PANEL_TEXT,
			PANEL_LABEL_STRING,"Date    :",
			PANEL_VALUE_DISPLAY_LENGTH, 35,
			XV_Y, 30,
			XV_X, 0,
			NULL);

		body_window = (Textsw) xv_create(display_frame, TEXTSW,
			XV_HEIGHT, 288,
			TEXTSW_MEMORY_MAXIMUM,1000000,
			NULL);

		sig_window = (Textsw) xv_create(display_frame, TEXTSW,
			XV_HEIGHT, 32,
			NULL);

		window_fit(body_window);
		window_fit(display_frame);
	}
	else {
		/* Open Window just in case */

		xv_set (display_frame,
			FRAME_CLOSED, FALSE,
			NULL);

		/* Clear add key item */

		xv_set (addkey_item,
			PANEL_INACTIVE, TRUE,
			NULL);
	}
}

/* Display the information about the sender */

void	display_sender_info(m)

MESSAGE	*m;

{
	xv_set(sender_item,PANEL_READ_ONLY,FALSE,NULL);
	xv_set(sender_item,PANEL_VALUE,m->sender,NULL);
	xv_set(sender_item,PANEL_READ_ONLY,TRUE,NULL);

	if (m->header_date)
		xv_set(date_item,PANEL_VALUE,m->header_date,NULL);
	else
		xv_set(date_item,PANEL_VALUE,m->date,NULL);

	xv_set(date_item,PANEL_READ_ONLY,TRUE,NULL);
}

/* Clear the display window */

void	clear_display_window()

{
	xv_set(sig_window, TEXTSW_READ_ONLY, FALSE,
			NULL);

	xv_set(body_window, TEXTSW_READ_ONLY, FALSE,
			NULL);

	textsw_delete(sig_window,
		0, TEXTSW_INFINITY);

	textsw_delete(body_window,
		0, TEXTSW_INFINITY);

	textsw_reset (sig_window, 0, 0);
	textsw_reset (body_window, 0, 0);
}

/* Set the display window to read-only */

void	lock_display_window()

{
	xv_set(sig_window,TEXTSW_FIRST_LINE,0,
		TEXTSW_INSERTION_POINT, 0,
		TEXTSW_READ_ONLY, TRUE,
		NULL);

	xv_set(body_window,TEXTSW_FIRST_LINE,0,
		TEXTSW_INSERTION_POINT, 0,
		TEXTSW_READ_ONLY, TRUE,
		NULL);
}

/* Show the display window */

void	show_display_window(m)

MESSAGE	*m;

{
	xv_set(display_frame,
			FRAME_LABEL, m->subject,
			XV_SHOW, TRUE,
			NULL);
}

/* Hide the display window */

void	close_display_window()

{
	if (display_frame)
		xv_set (display_frame,
			XV_SHOW, FALSE,
			NULL);
}

/* Display the message body from the buffer */

void	display_message_body(b)

BUFFER	*b;

{
	textsw_insert(body_window,b->message,b->length);
}

void	display_message_sig(b)

BUFFER	*b;

{
	textsw_insert(sig_window,b->message,b->length);
}

void	delete_message_proc()

{
	MESSAGE	*om,*m;
	int	n = 0;
	int	y;

	update_random();

	/* Start at the head of the list */

	m = messages.start;

	while (m) {

		/* For each message, decrease list position by the number
		   of deleted messages. */

		if (!(m->flags & MESS_DELETED))
			m->list_pos -= n;

		/* Store message pointer in om, then set m to next message */

		om = m;
		m = m->next;

		if (om->flags & MESS_SELECTED) {

			delete_message(om);

			/* Then increment number of deleted messages */

			n++;
		}
		else
			if (n) {
				display_message_description(om);
			}
	}

	/* If any messages were deleted, clear the bottom lines of the list */

	if (n) {
		int	w;
		MESSAGE	*m;

		w = xv_get (list_canvas, XV_WIDTH);

		/* Remember - messages.end may be NULL ! */

		m = messages.end;

		while (m && (m->flags & MESS_DELETED))
			m = m->prev;

		if (m)
			y = m->list_pos;
		else
			y = 0;

		XFillRectangle (dpy, map_win, clear_gc, 0, 
			y*LIST_DISPLACEMENT + 4, 
			w, LIST_DISPLACEMENT*n);
	}

	/* Move current selected message pointer if neccesary */

	if (last_message_read) {
		last_message_read->flags |= MESS_SELECTED;
		display_message_description (last_message_read);

		if (is_displaying_message())
			display_message(last_message_read);
	}
	else
		close_display_window();

	update_message_list();
}

static	list_repaint_proc(canvas,pw,r_a)

Canvas  canvas;
Xv_window       pw;
Rectlist        *r_a;

{
	MESSAGE	*m;
	int32	min_pos, max_pos;

	list_window = pw;

	min_pos = (r_a->rl_bound.r_top / LIST_DISPLACEMENT) - 1;
	max_pos = ((r_a->rl_bound.r_top+r_a->rl_bound.r_height) / 
		LIST_DISPLACEMENT) + 1;

	gc = (GC) xv_get (canvas, XV_KEY_DATA, GC_KEY);
        dpy=(Display *)xv_get(pw,XV_DISPLAY);
        map_win=(Window)xv_get(pw,XV_XID);

	m = messages.start;

	while (m) {
		if (m->list_pos >= min_pos && m->list_pos <= max_pos &&
			!(m->flags & MESS_DELETED)) 
			display_message_description(m);
		m = m->next;
	}
}

static	list_notify_proc(window,event)

Xv_Window	*window;
Event	*event;

{
	MESSAGE	*m,*lm;
	int	n;

	update_random();

	if (event_is_up(event))
	switch (event_action(event)) {

		/* Middle button - select/deselect another message */

		case ACTION_ADJUST:
		n = (event_y(event) / LIST_DISPLACEMENT) + 1;

		m = messages.start;
		while (m && m->list_pos != n) {
			m = m->next;
		}

		if (m) {
			m->flags ^= MESS_SELECTED;
			display_message_description(m);
		}
		break;

		/* Left button - select one message, or display */

		case ACTION_SELECT:
		n = (event_y(event) / LIST_DISPLACEMENT) + 1;

		m = messages.start;
		while (m && m->list_pos != n) {
			m = m->next;
		}

		if (m) {

			/* If already selected, display it */

			if (m->flags & MESS_SELECTED) {
				set_main_footer ("Displaying message...");
				display_message (m);
				clear_main_footer ();
			}
			else {
				/* Clear all selected messages */

				lm = messages.start;

				while (lm) {
					if (lm->flags & MESS_SELECTED) {
						lm->flags &= ~MESS_SELECTED;
						display_message_description (lm);
					}
					lm = lm->next;
				}

				/* Then select this one ! */

				m->flags |= MESS_SELECTED;
				draw_box_round_message(m);
				last_message_read = m;
			}
		}
	}
}

void	update_menu_item()

{
	char	*s;
	int	i;
	char	temp [MAXPATHLEN];

	s = read_file_name();

	if (s && *s) {
		copy_to_nl (s, temp);
		s = temp;
	}
	else
		s = "";

	for (i = 0; i < 3; i++) {
		if (files_item[i]) {
			xv_set (files_item[i], 
				MENU_STRING, strdup(s),
				NULL);
		}
	}
}

void	x_xxxx_message_proc(item,event)

Panel_item	item;
Event		*event;

{
	update_random();

	if (event_is_down(event))
		switch (event_action(event)) {

			case ACTION_MENU:
			case ACTION_SELECT:
			update_menu_item();
			break;

		}
}

void	copy_choice_proc(m,mi)

Menu	m;
Menu_item	mi;

{
	char	*s;

	s = (char *)xv_get (mi, MENU_STRING);

	if (s) {
		update_menu_item ();

		/* Oops, update_menu_item() may change the menu string ! */

		s = (char *)xv_get (mi, MENU_STRING);
		set_file_name (s);
		copy_message_proc(s);
	}
}

void	move_choice_proc(m,mi)

Menu	m;
Menu_item	mi;

{
	char	*s;

	s = (char *)xv_get (mi, MENU_STRING);

	if (s) {
		update_menu_item ();

		/* Oops, update_menu_item() may change the menu string ! */

		s = (char *)xv_get (mi, MENU_STRING);
		set_file_name (s);
		move_message_proc(s);
	}
}

void	load_choice_proc(m,mi)

Menu	m;
Menu_item	mi;

{
	char	*s;

	s = (char *)xv_get (mi, MENU_STRING);
	
	if (s) {
		update_menu_item ();

		/* Oops, update_menu_item() may change the menu string ! */

		s = (char *)xv_get (mi, MENU_STRING);
		set_file_name (s);
		load_file_proc(s);
	}
}

static	void	clear_passphrase_proc()

{
	destroy_passphrase (TRUE);
}

static	void	(*menus_proc[3])() = {

	move_choice_proc,
	copy_choice_proc,
	load_choice_proc

};

void	close_all_windows()

{
	close_display_window ();
	close_deliver_window ();

	xv_set (main_frame,
		FRAME_CLOSED, TRUE,
		NULL);
}


/*
 * file_menu_action_proc - Set the file_name_item based on the menu item
 */
void
file_menu_action_proc(menu, menu_item)
Menu    menu;
Menu_item       menu_item;
{
  char	  folder_ending[MAXPATHLEN];
  char	  *menu_string, *menu_data, *s, *folder_dir, *home;

  menu_string = (char *)xv_get(menu_item, MENU_STRING);
  menu_data = (char *)xv_get(menu, MENU_CLIENT_DATA);
  home = getenv("HOME");
  
  if ((folder_dir = (char *)find_mailrc("folder")) && (*menu_data != '.'))
	  {
	    s = menu_data+strlen(folder_dir) +1;
	    if (*folder_dir != '/')
	      {
		s += strlen(home);
		if (*s == '/')
		    s += 1;
	      }
	      
	    sprintf(folder_ending, "+/%s/%s", s, menu_string);
	    if (s[0] == '\0')
	      {
		sprintf(folder_ending, "+/%s", menu_string);
	      }
	     else
	      {
		sprintf(folder_ending, "+/%s/%s", s, menu_string);
	      }
	  }
	 else
	  {
	    if (menu_data+1 != '\0')
	      {
		sprintf(folder_ending, "%s/%s", menu_data, menu_string);
	      }
	     else
	      {
		strcpy(folder_ending, menu_string);
	      }
	  }

    xv_set(file_name_item,
        PANEL_VALUE,      folder_ending,
        NULL);
}

/*
 * return an allocated char * that points to the last item in a path.
 */
char *
getfilename(path)
char *path;
{
    char *p;

    if (p = rindex(path, '/'))
        p++;
    else
        p = path;
    return strcpy(malloc(strlen(p)+1), p);
}

/* gen_pullright() is called in the following order:
 *   Pullright menu needs to be displayed. (MENU_PULLRIGHT)
 *   Menu is about to be dismissed (MENU_DISPLAY_DONE)
 *      User made a selection (before menu notify function)
 *      After the notify routine has been called.
 * The above order is done whether or not the user makes a
 * menu selection.
 */
Menu
gen_pullright(mi, op)
Menu_item mi;
Menu_generate op;
{
    Menu menu;
    Menu_item new, old = mi;
    char buf[MAXPATHLEN];

    if (op == MENU_DISPLAY) {
        menu = (Menu)xv_get(mi, MENU_PARENT);
        sprintf(buf, "%s/%s",
            (char *)xv_get(menu, MENU_CLIENT_DATA), (char *)xv_get(mi, MENU_STRING));
        /* get old menu and free it -- we're going to build another */
        if (menu = (Menu)xv_get(mi, MENU_PULLRIGHT)) {
            free((char *)xv_get(menu, MENU_CLIENT_DATA));
            xv_destroy(menu);
        }
        if (new = add_path_to_menu(buf)) {
            menu = (Menu)xv_get(new, MENU_PULLRIGHT);
            xv_destroy(new);
            return menu;
        }
    }
    if (!(menu = (Menu)xv_get(mi, MENU_PULLRIGHT)))
            menu = (Menu)xv_create(NULL, MENU,
                MENU_STRINGS, "Couldn't build a menu.", NULL,
                NULL);
    return menu;
}

/*
 * The path passed in is scanned via readdir().  For each file in the
 * path, a menu item is created and inserted into a new menu.  That
 * new menu is made the PULLRIGHT_MENU of a newly created panel item
 * for the path item originally passed it.  Since this routine is
 * recursive, a new menu is created for each subdirectory under the
 * original path.
 */
Menu_item
add_path_to_menu(path)
char *path;
{
    DIR                 *dirp;
    struct dirent       *dp;
    struct stat         s_buf;
    Menu_item           mi;
    Menu                next_menu;
    char                buf[MAXPATHLEN];
    static int          recursion;

    /* don't add a folder to the list if user can't read it */
    if (stat(path, &s_buf) == -1 || !(s_buf.st_mode & S_IREAD))
        return NULL;
    if (s_buf.st_mode & S_IFDIR) {
        int cnt = 0;
        if (!(dirp = opendir(path)))
            /* don't bother adding to list if we can't scan it */
            return NULL;
        if (recursion)
            return (Menu_item)-1;
        recursion++;
        next_menu = (Menu)xv_create(XV_NULL, MENU, NULL);
        while (dp = readdir(dirp))
            if (strcmp(dp->d_name, ".") && strcmp(dp->d_name, "..")) {
                (void) sprintf(buf, "%s/%s", path, dp->d_name);
                mi = add_path_to_menu(buf);
                if (!mi || mi == (Menu_item)-1) {
                    int do_gen_pullright = (mi == (Menu_item)-1);
                    /* unreadable file or dir - deactivate item */
                    mi = (Menu_item)xv_create(XV_NULL, MENUITEM,
                        MENU_STRING,  getfilename(dp->d_name),
                        MENU_RELEASE,
                        MENU_RELEASE_IMAGE,
                        NULL);
                    if (do_gen_pullright)
                        xv_set(mi,
                            MENU_GEN_PULLRIGHT, gen_pullright,
                            NULL);
                    else
                        xv_set(mi, MENU_INACTIVE, TRUE, NULL);
                }
                xv_set(next_menu, MENU_APPEND_ITEM, mi, NULL);
                cnt++;
            }
        closedir(dirp);
        mi = (Menu_item)xv_create(XV_NULL, MENUITEM,
            MENU_STRING,        getfilename(path),
            MENU_RELEASE,
            MENU_RELEASE_IMAGE,
            MENU_NOTIFY_PROC,   file_menu_action_proc,
            NULL);
        if (!cnt) {
            xv_destroy(next_menu);
            /* An empty or unsearchable directory - deactivate item */
            xv_set(mi, MENU_INACTIVE, TRUE, NULL);
        } else {
            xv_set(next_menu,
                MENU_TITLE_ITEM, strcpy(malloc(strlen(path)+1), path),
                MENU_CLIENT_DATA, strcpy(malloc(strlen(path)+1), path),
                NULL);
            xv_set(mi, MENU_PULLRIGHT, next_menu, NULL);
        }
        recursion--;
        return mi;
    }
    return (Menu_item)xv_create(NULL, MENUITEM,
        MENU_STRING,            getfilename(path),
        MENU_RELEASE,
        MENU_RELEASE_IMAGE,
        MENU_NOTIFY_PROC,       file_menu_action_proc,
        NULL);
}


void	setup_ui(level)

int	level;

{
	int	i, slen;
	MESSAGE	*m;
	GC	gc;
	Display	*dpy;
	XGCValues	gcvalues;
	char	app_name[128];
	char    folder_dir[MAXPATHLEN];
	Menu_item	mi;
	char	*files,*f,*fs, *folder;
	Menu	button_menu, sort_menu, undelete_menu, file_menu;
	struct itimerval	timer;

	sprintf(app_name,"%s [ Security Level %d ]",prog_name,level);

	main_frame = (Frame) xv_create(NULL, FRAME,
		FRAME_LABEL, app_name,
		XV_WIDTH, 720,
		XV_HEIGHT, 320,
		FRAME_SHOW_FOOTER, TRUE,
		FRAME_CLOSED, TRUE,
		NULL);

	top_panel = (Panel) xv_create(main_frame, PANEL,
		XV_HEIGHT, 64,
		PANEL_LAYOUT, PANEL_HORIZONTAL,
		NULL);

	button_menu = (Menu) xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Load In-Box",
			MENU_NOTIFY_PROC, inbox_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Save Changes",
			MENU_NOTIFY_PROC, save_changes_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Done",
			MENU_NOTIFY_PROC, done_proc,
			NULL,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "File",
		PANEL_ITEM_MENU, button_menu,
		NULL);

	sort_menu = (Menu) xv_create (NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Time and Date",
			MENU_NOTIFY_PROC, sort_by_time,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Sender",
			MENU_NOTIFY_PROC, sort_by_sender,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Subject",
			MENU_NOTIFY_PROC, sort_by_subject,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Size",
			MENU_NOTIFY_PROC, sort_by_size,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Status",
			MENU_NOTIFY_PROC, sort_by_status,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Message Number",
			MENU_NOTIFY_PROC, sort_by_number,
			NULL,
		NULL);

	button_menu = (Menu) xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Previous",
			MENU_NOTIFY_PROC, prev_message_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Next",
			MENU_NOTIFY_PROC, next_message_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Sort By",
			MENU_PULLRIGHT, sort_menu,
			NULL,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "View",
		PANEL_ITEM_MENU, button_menu,
		NULL);

	undelete_menu = (Menu) xv_create (NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Last",
			MENU_NOTIFY_PROC, undelete_last_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "From List",
			MENU_INACTIVE, TRUE,
			NULL,
		NULL
	);

	button_menu = (Menu) xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Cut",
			MENU_INACTIVE, TRUE,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Copy",
			MENU_INACTIVE, TRUE,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Delete",
			MENU_INACTIVE, TRUE,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Undelete",
			MENU_PULLRIGHT, undelete_menu,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Clear Passphrase",
			MENU_NOTIFY_PROC, clear_passphrase_proc,
			NULL,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Edit",
		PANEL_ITEM_MENU, button_menu,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Compose",
		PANEL_NOTIFY_PROC, send_message,
		NULL);

	button_menu = (Menu) xv_create (NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Save Changes And Quit",
			MENU_NOTIFY_PROC, save_and_quit_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Quit, Ignore Changes",
			MENU_NOTIFY_PROC, quit_proc,
			NULL,
		NULL
	);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Quit",
		PANEL_ITEM_MENU, button_menu,
		NULL);


	if ((folder = (char *)find_mailrc("folder")))
	  {
	    struct stat	  s_buf;
	    if (*folder != '/')
	      {
		char * home;
		home = getenv ("HOME");
		sprintf(folder_dir, "%s/%s", home, folder);
	      }
	     else
	      {
		strcpy(folder_dir, folder);
	      }
	    /* if directory doesn't exist set to cwd */
	    if (stat(folder_dir, &s_buf) == -1 || !(s_buf.st_mode & S_IREAD))
		strcpy(folder_dir, ".");
	  }
	 else
	  {
	    strcpy(folder_dir, ".");
	  }
	  
	mi = add_path_to_menu(folder_dir);
	if (mi)
	  {
	    file_menu = (Menu)xv_get(mi, MENU_PULLRIGHT);
	    /* We no longer need the item since we have the menu from it */
	    xv_destroy(mi);
	    (void) xv_create (top_panel, PANEL_ABBREV_MENU_BUTTON,
		    PANEL_LABEL_STRING, "Mail File:",
		    PANEL_ITEM_MENU, file_menu,
		    NULL);
	  }
	  

	file_name_item = (Panel_item) xv_create (top_panel, PANEL_TEXT,
		PANEL_VALUE_DISPLAY_LENGTH, 30,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Done",
		PANEL_NOTIFY_PROC, done_proc,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Next",
		PANEL_NOTIFY_PROC, next_message_proc,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Delete",
		PANEL_NOTIFY_PROC, delete_message_proc,
		NULL);

	button_menu = (Menu) xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "Print Decrypted/Authenticated",
			MENU_NOTIFY_PROC, print_cooked_proc,
			NULL,
		MENU_ITEM,
			MENU_STRING, "Print Encrypted/Clearsigned",
			MENU_NOTIFY_PROC, print_raw_proc,
			NULL,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Print",
		PANEL_ITEM_MENU, button_menu,
		NULL);

	button_menu = (Menu) xv_create(NULL, MENU,
		MENU_ITEM,
			MENU_STRING, "To Sender",
			MENU_NOTIFY_PROC, reply_sender_no_include,
			NULL,
		MENU_ITEM,
			MENU_STRING, "To Sender, Include",
			MENU_NOTIFY_PROC, reply_to_sender,
			NULL,
		MENU_ITEM,
			MENU_STRING, "To All",
			MENU_INACTIVE, TRUE,
			NULL,
		MENU_ITEM,
			MENU_STRING, "To All, Include",
			MENU_INACTIVE, TRUE,
			NULL,
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Reply",
		PANEL_ITEM_MENU, button_menu,
		NULL);

	files = find_mailrc("filemenu2");

	for (i = 0; i < 3 ; i++)  {
		files_menu[i] = (Menu) xv_create (NULL,
			MENU,
			NULL);

		files_item[i] = (Menu_item) xv_create(NULL,
			MENUITEM,
			MENU_STRING, strdup(""),
			MENU_NOTIFY_PROC, menus_proc[i],
			MENU_RELEASE,
			MENU_RELEASE_IMAGE,
			NULL);

		xv_set(files_menu[i], 
			MENU_APPEND_ITEM, files_item[i],
			NULL);
	}

	if (files) {
		files = strdup(files);

		f = files;
		while (*f) {
			fs = f;
			while (*f && *f != ' ')
				f++;
			if (*f)
				*f++ = 0;

			for (i = 0; i < 3; i++) {
				mi = (Menu_item) xv_create(NULL,
					MENUITEM,
					MENU_STRING, strdup(fs),
					MENU_RELEASE,
					MENU_RELEASE_IMAGE,
					MENU_NOTIFY_PROC, menus_proc[i],
					NULL);

				xv_set(files_menu[i], 
					MENU_APPEND_ITEM, mi,
					NULL);
			}
		}

		free (files);
	}

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Move",
		PANEL_NOTIFY_PROC, x_xxxx_message_proc,
		PANEL_ITEM_MENU, files_menu[0],
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Copy",
		PANEL_NOTIFY_PROC, x_xxxx_message_proc,
		PANEL_ITEM_MENU, files_menu[1],
		NULL);

	(void) xv_create (top_panel, PANEL_BUTTON,
		PANEL_LABEL_STRING, "Load",
		PANEL_NOTIFY_PROC, x_xxxx_message_proc,
		PANEL_ITEM_MENU, files_menu[2],
		NULL);

	list_canvas = (Canvas) xv_create(main_frame,
		CANVAS,
		CANVAS_AUTO_SHRINK, FALSE,
		CANVAS_AUTO_EXPAND, TRUE,
		CANVAS_REPAINT_PROC, list_repaint_proc,
		WIN_RETAINED, FALSE,
		NULL);

	list_font = (Xv_font) xv_create(main_frame,
		FONT,
		FONT_NAME, "courier",
		NULL);


	/* If we failed to open it, get the default font */

	if (!list_font) {
		list_font = (Xv_font) xv_get (main_frame,
			XV_FONT);
	}

	dpy = (Display *) xv_get(main_frame, XV_DISPLAY);

	gcvalues.font = xv_get(list_font, XV_XID);
	gcvalues.foreground = BlackPixel(dpy, DefaultScreen(dpy));
	gcvalues.background = WhitePixel(dpy, DefaultScreen(dpy));
	gcvalues.graphics_exposures = FALSE;
	gcvalues.fill_style = FillSolid;
	gcvalues.function = GXclear;

	clear_gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		GCForeground|GCBackground|GCFont|GCGraphicsExposures|
		GCFillStyle|GCFunction,
		&gcvalues);

	gc = XCreateGC(dpy, RootWindow(dpy, DefaultScreen(dpy)),
		GCForeground|GCBackground|GCFont|GCGraphicsExposures,
		&gcvalues);

	xv_set(list_canvas, XV_KEY_DATA, GC_KEY, gc, NULL);

	/* Pixels per unit should be computed from something
	 * but I'm not sure what.  KP
	 */ 
	v_scroll = (Scrollbar) xv_create(list_canvas,
		SCROLLBAR,
		SCROLLBAR_DIRECTION, SCROLLBAR_VERTICAL,
		SCROLLBAR_SPLITTABLE, FALSE,
		SCROLLBAR_PIXELS_PER_UNIT, 16, 
		NULL);

	xv_set (canvas_paint_window(list_canvas), 
		WIN_EVENT_PROC, list_notify_proc,
		WIN_CONSUME_EVENTS, WIN_MOUSE_BUTTONS, NULL,
		WIN_RETAINED, FALSE,
		NULL);

	newmail_icon_image = (Server_image) xv_create(NULL,SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_BITS, newmail_icon_bits,
		NULL);

	icon_image = (Server_image) xv_create(NULL,SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_BITS, icon_bits,
		NULL);

	icon_image_mask = (Server_image) xv_create(NULL,SERVER_IMAGE,
		XV_WIDTH, 64,
		XV_HEIGHT, 64,
		SERVER_IMAGE_BITS, icon_bits_mask,
		NULL);

	icon = (Icon) xv_create(main_frame, ICON,
		ICON_IMAGE, icon_image,
		ICON_MASK_IMAGE, icon_image_mask,
		ICON_TRANSPARENT, TRUE,
		NULL);

	newmail_icon = (Icon) xv_create(main_frame, ICON,
		ICON_IMAGE, newmail_icon_image,
		ICON_TRANSPARENT, TRUE,
		NULL);

	xv_set(main_frame,
		FRAME_ICON, icon,
		NULL);

	m = messages.start;
	i = 1;
	while (m) {
		m->list_pos = i;
		m->number = i;

		if (m->status == MSTAT_NONE)
			show_newmail_icon();

		set_message_description(m);
		m = m->next;
		i++;
	}

	update_message_list();

	window_fit(main_frame);

	f = find_mailrc("retrieveinterval");
	if (!f) {
		timer.it_value.tv_sec = DEFAULT_CHECK_TIME;
	}
	else {
		timer.it_value.tv_sec = atoi(f);
	}

	timer.it_value.tv_usec = 0;
	timer.it_interval = timer.it_value;

	if (timer.it_value.tv_sec > 0) {
		notify_set_itimer_func(main_frame,
			check_for_new_mail,
			ITIMER_REAL,
			&timer,
			NULL);
	}

	xv_main_loop(main_frame);
}

void	shutdown_ui()

{
	xv_destroy_safe(main_frame);
}

