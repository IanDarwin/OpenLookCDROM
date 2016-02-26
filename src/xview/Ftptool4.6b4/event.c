#include "ftptool.h"

#pragma ident   "@(#)event.c 1.5     93/05/26"

#ifdef USE_PROTOTYPES
void	local_cd_select(void)
#else
void	local_cd_select()
#endif
{
	int 	nitems, selection, row;
	struct dirlist *tmp;

	local_footer_message("");
	/* check that only one item is selected, and it is a directory */
	nitems = xv_get(local_window.list, PANEL_LIST_NROWS);

	selection = 0;
	for (row = 0; row < nitems; row++)
		if (xv_get(local_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(local_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			if (S_ISDIR(tmp->mode)) {
				selection = row;
				break;
			}
		}

	tmp = (struct dirlist *)xv_get(local_window.list,
		PANEL_LIST_CLIENT_DATA, selection);

	xv_set(local_window.list,
		PANEL_LIST_SELECT, selection, FALSE,
		NULL);
	change_local_dir(tmp->name, 0);
}

#ifdef USE_PROTOTYPES
void	local_cd_text(void)
#else
void	local_cd_text()
#endif
{
	char	*dir;

	local_footer_message("");
	dir = (char *)xv_get(local_window.directory, PANEL_VALUE);
	if (*dir == '\0') {
		local_footer_message("Please type in a pathname first.");
		return;
	}
	change_local_dir(dir, 0);
}

#ifdef USE_PROTOTYPES
void	local_cd_dotdot(void)
#else
void	local_cd_dotdot()
#endif
{
	local_footer_message("");
	change_local_dir("..", 0);
}

#ifdef USE_PROTOTYPES
void	remote_cd_select(void)
#else
void	remote_cd_select()
#endif
{
	int 	nitems, selection, row;
	struct dirlist *tmp;

	footer_message("");
	/* check that only one item is selected, and it is a directory */
	nitems = xv_get(base_window.list, PANEL_LIST_NROWS);
	selection = 0;
	for (row = 0; row < nitems; row++)
		if (xv_get(base_window.list, PANEL_LIST_SELECTED, row)) {
			tmp = (struct dirlist *)xv_get(base_window.list,
				PANEL_LIST_CLIENT_DATA, row);
			if (non_unix || S_ISDIR(tmp->mode)) {
				selection = row;
				break;
			}
		}

	tmp = (struct dirlist *)xv_get(base_window.list,
		PANEL_LIST_CLIENT_DATA, selection);

	xv_set(base_window.list,
		PANEL_LIST_SELECT, selection, FALSE,
		NULL);
	which_remote_file = strdup(tmp->name);
	if (which_remote_file == NULL) {
		fprintf(stderr, "Out of memory.\n");
		return;
	}
	dowhat = DOREMOTECD;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	remote_cd_text(void)
#else
void	remote_cd_text()
#endif
{
	footer_message("");
	which_remote_file = (char *)xv_get(base_window.directory, PANEL_VALUE);
	if (*which_remote_file == '\0') {
		footer_message("Please type in a pathname first.");
		return;
	}
	if (!non_unix) {
		/* removed following line to let the remote ftpd interpret */
		/* ~ itself. This also removes environment support for the */
		/* remote machine, but local environment variables probably */
		/* don't mean anything anyway remotely. */
		/*
		which_remote_file = expand_dirname(which_remote_file);
		 */
		which_remote_file = strdup(which_remote_file);
	} else
		which_remote_file = strdup(which_remote_file);
	if (which_remote_file == NULL)
		return;
	dowhat = DOREMOTECDFORCE;
	notify_stop();
}

#ifdef USE_PROTOTYPES
void	remote_cd_dotdot(void)
#else
void	remote_cd_dotdot()
#endif
{
	footer_message("");
	which_remote_file = strdup("..");
	if (which_remote_file == NULL) {
		fprintf(stderr, "Out of memory.\n");
		return;
	}
	dowhat = DOREMOTECD;
	notify_stop();
}

#ifdef USE_PROTOTYPES
Notify_value destroy_func(Notify_client client, Destroy_status status)
#else
Notify_value destroy_func(client, status)
Notify_client	client;
Destroy_status	status;
#endif
{
	int	answer;
	static int triedonce;
#ifdef XVIEW3
	Xv_notice notice;
#endif

	if (status == DESTROY_SAVE_YOURSELF) {
		/* save state. Not a death */
		return (NOTIFY_DONE);
	}
	if (connected) {
#ifdef XVIEW3
		notice = xv_create(base_window.panel, NOTICE,
			NOTICE_MESSAGE_STRINGS,
				"You are still connected.",
				NULL,
			NOTICE_BUTTON_YES,  "Cancel",
			NOTICE_BUTTON_NO,	"Quit anyway",
			NOTICE_STATUS, &answer,
			XV_SHOW, TRUE,
			NULL);
		xv_destroy_safe(notice);
#else
		answer = notice_prompt(base_window.panel, NULL,
			NOTICE_MESSAGE_STRINGS,
				"You are still connected.",
				NULL,
			NOTICE_BUTTON_YES,  "Cancel",
			NOTICE_BUTTON_NO,	"Quit anyway",
			NULL);
#endif
		if (answer == NOTICE_YES)
			return ((Notify_value)notify_veto_destroy(client));
	}
	if (list_changed) {
		if (status == DESTROY_CHECKING) {
#ifdef XVIEW3
			notice = xv_create(base_window.panel, NOTICE,
				NOTICE_MESSAGE_STRINGS,
					"Your host list has changed. You can",
					NULL,
				NOTICE_BUTTON_YES,	"Save changes",
				NOTICE_BUTTON,		"Quit anyway", 2,
				NOTICE_BUTTON,		"Cancel quit", 3,
				NOTICE_STATUS, &answer,
				XV_SHOW, TRUE,
				NULL);
			xv_destroy_safe(notice);
#else
			answer = notice_prompt(base_window.panel, NULL,
				NOTICE_MESSAGE_STRINGS,
					"Your host list has changed. You can",
					NULL,
				NOTICE_BUTTON_YES,	"Save changes",
				NOTICE_BUTTON,		"Quit anyway", 2,
				NOTICE_BUTTON,		"Cancel quit", 3,
				NULL);
#endif
			triedonce = 1;
		} else if (!triedonce) {
#ifdef XVIEW3
			notice = xv_create(base_window.panel, NOTICE,
				NOTICE_MESSAGE_STRINGS,
					"Your host list has changed. You can",
					NULL,
				NOTICE_BUTTON_YES,	"Save changes",
				NOTICE_BUTTON,		"Quit anyway", 2,
				NOTICE_STATUS, &answer,
				XV_SHOW, TRUE,
				NULL);
			xv_destroy_safe(notice);
#else
			answer = notice_prompt(base_window.panel, NULL,
				NOTICE_MESSAGE_STRINGS,
					"Your host list has changed. You can",
					NULL,
				NOTICE_BUTTON_YES,	"Save changes",
				NOTICE_BUTTON,		"Quit anyway", 2,
				NULL);
#endif
		} else {
			/* perhaps something has gone wrong. */
			answer = 2;
		}

		switch (answer) {
		case NOTICE_YES:
			list_changed = 0;
			timestamped = 0;
			write_ftptoolrc();
			break;
		case 2:
			/* avoid saving timestamps, if they are there */
			timestamped = 0;
			break;
		case 3:
			return ((Notify_value)notify_veto_destroy(client));
			break;
		}
	}
	if (timestamped) {
		timestamped = 0;
		write_ftptoolrc();
	}

	quit_ftp();

	system("rm -rf /usr/tmp/sched* /usr/tmp/ftptl*");

	dowhat = DOQUIT;

	switch (status) {
	case DESTROY_CHECKING:
		break;
	case DESTROY_CLEANUP:
		return (notify_next_destroy_func(client, status));
		break;
	case DESTROY_PROCESS_DEATH:
		exit(1);
		break;
	case DESTROY_SAVE_YOURSELF:
		fprintf(stderr,
		    "Impossible DESTROY_SAVE_YOURSELF event!\n");
		break;
	}
	return (NOTIFY_DONE);
}

#ifdef USE_PROTOTYPES
Notify_value sig_func(void)
#else
Notify_value sig_func()
#endif
{
	if (xv_destroy_safe(base_window.frame) == XV_OK) {
		quit_ftp();
		exit(1);
	}
	return (NOTIFY_DONE);
}

#ifdef USE_PROTOTYPES
void cycle_busy_icon(void)
#else
void cycle_busy_icon()
#endif
{
	static int i;

	xv_set(frame_icon,
		ICON_IMAGE, busy_glyphs[i],
		ICON_TRANSPARENT, TRUE,
		NULL);
	i++;
	if (i == nbusyicons)
		i = 0;
}

static struct itimerval busy_itimer = {
	{0, 200000, },
	{0, 200000, }
};

#ifdef USE_PROTOTYPES
void start_busy_cycle(void)
#else
void start_busy_cycle()
#endif
{
	notify_set_itimer_func(base_window.frame, (Notify_func)cycle_busy_icon,
		ITIMER_REAL, &busy_itimer, (struct itimerval *)NULL);
}

#ifdef USE_PROTOTYPES
void end_busy_cycle(void)
#else
void end_busy_cycle()
#endif
{
	notify_set_itimer_func(base_window.frame, NOTIFY_FUNC_NULL, ITIMER_REAL,
		&busy_itimer, (struct itimerval *)NULL);
	xv_set(frame_icon,
		ICON_IMAGE, ftptool_glyph,
		ICON_TRANSPARENT, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void props_event_proc(Panel panel, Event *event)
#else
void props_event_proc(panel, event)
Panel	panel;
Event *event;
#endif
{
	if (event_action(event) == ACTION_PROPS) {
		xv_set(tool_property_window.frame,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void base_event_proc(Xv_Window window, Event *event)
#else
void base_event_proc(window, event)
Xv_Window	window;
Event *event;
#endif
{
	int height;
	int	y;
	int	rows;
	int	rowsize;
	int	width;

#ifdef XVIEW3
	switch (event_action(event)) {
	case ACTION_DRAG_COPY:
	case ACTION_DRAG_MOVE:
		remote_drop(window, event, base_window.sel);
		break;
	default:
		break;
	}
#endif
	switch (event_id(event)) {
	case WIN_RESIZE:
		height = xv_get(base_window.panel, XV_HEIGHT);
		width = xv_get(base_window.panel, XV_WIDTH);
		y = xv_get(base_window.list, PANEL_ITEM_Y);
		rowsize = xv_get(base_window.list, PANEL_LIST_ROW_HEIGHT);
		rows = (height - y - 45) / rowsize;
		if (rows <= 0)
			rows = 1;
		xv_set(base_window.list,
			PANEL_LIST_DISPLAY_ROWS, rows,
			PANEL_LIST_WIDTH, width - 30,
			PANEL_PAINT, PANEL_NONE,
			NULL);
#ifdef XVIEW3
		xv_set(base_window.drop_site,
			DROP_SITE_REGION, xv_get(base_window.list, XV_RECT),
			NULL);
#endif
		panel_paint(base_window.list, PANEL_CLEAR);
		break;
	default:
		break;
	}
}

#ifdef USE_PROTOTYPES
void resize_window(Panel panel, Panel_item list, Panel_item dismiss)
#else
void resize_window(panel, list, dismiss)
Panel	panel;
Panel_item	list;
Panel_item	dismiss;
#endif
{
	int height;
	int	y;
	int	rows;
	int	rowsize;
	int	width;
	Rect	*butrect;

	height = xv_get(panel, XV_HEIGHT);
	width = xv_get(panel, XV_WIDTH);
	y = xv_get(list, PANEL_ITEM_Y);
	rowsize = xv_get(list, PANEL_LIST_ROW_HEIGHT);
	rows = (height - y - 45) / rowsize;
	/* leave room for dismiss button */
	if (!openlook_mode)
		rows -= 1;
	if (rows <= 0)
		rows = 1;
	xv_set(list,
		PANEL_LIST_DISPLAY_ROWS, rows,
		PANEL_LIST_WIDTH, width - 30,
		PANEL_PAINT, PANEL_NONE,
		NULL);
	panel_paint(list, PANEL_CLEAR);
	/* for non-openlook mode */
	butrect = (Rect *)xv_get(dismiss, XV_RECT);
	xv_set(dismiss,
		XV_X, width/2 - butrect->r_width/2,
		XV_Y, height - butrect->r_height,
		NULL);
}

#ifdef USE_PROTOTYPES
void local_event_proc(Xv_Window window, Event *event)
#else
void local_event_proc(window, event)
Xv_Window	window;
Event *event;
#endif
{
#ifdef XVIEW3
	switch (event_action(event)) {
	case ACTION_DRAG_COPY:
	case ACTION_DRAG_MOVE:
		local_drop(window, event, local_window.sel);
		break;
	default:
		break;
	}
#endif
	switch (event_id(event)) {
	case WIN_RESIZE:
		resize_window(local_window.panel, local_window.list,
			local_window.dismiss);
		break;
	default:
		break;
	}
}

#ifdef USE_PROTOTYPES
void schedule_event_proc(Xv_Window window, Event *event)
#else
void schedule_event_proc(window, event)
Xv_Window	window;
Event *event;
#endif
{
	int height;
	int	y;
	int	rows;
	int	rowsize;
	int	width;

	switch (event_id(event)) {
	case WIN_RESIZE:
		height = xv_get(schedule_window.panel, XV_HEIGHT);
		width = xv_get(schedule_window.panel, XV_WIDTH);
		y = xv_get(schedule_window.send_list, PANEL_ITEM_Y);
		rowsize = xv_get(schedule_window.send_list,
		    PANEL_LIST_ROW_HEIGHT);
		rows = (height - y - 45) / rowsize;
		if (rows <= 0)
			rows = 1;
		xv_set(schedule_window.send_list,
			PANEL_LIST_DISPLAY_ROWS, rows,
			PANEL_LIST_WIDTH, width - 30,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		xv_set(schedule_window.receive_list,
			PANEL_LIST_DISPLAY_ROWS, rows,
			PANEL_LIST_WIDTH, width - 30,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		if (xv_get(schedule_window.send_list, XV_SHOW) == TRUE)
			panel_paint(schedule_window.send_list, PANEL_CLEAR);
		else
			panel_paint(schedule_window.receive_list, PANEL_CLEAR);
		break;
	default:
		break;
	}
}

#ifdef USE_PROTOTYPES
void host_event_proc(Xv_Window window, Event *event)
#else
void host_event_proc(window, event)
Xv_Window	window;
Event *event;
#endif
{
	int	width;
	int	height_diff;
	int panelwidth;
	Rect	*connect_rect;
	Rect	*dismiss_rect;
	int	space;
	int	pos;
	int	y;
	int rows;
	int rowsize;
	Panel_item	item;
	Panel_item_type	item_type;

	switch (event_id(event)) {
	case WIN_RESIZE:
		width = xv_get(host_window.frame, XV_WIDTH);
		if (xv_get(host_window.advanced.panel, XV_SHOW) == TRUE)
			panelwidth = width / 2;
		else
			panelwidth = width - 2;
		xv_set(host_window.panel,
			XV_WIDTH, width,
			NULL);
		xv_set(host_window.basic.panel,
			XV_WIDTH, panelwidth,
			NULL);
		xv_set(host_window.advanced.panel,
			XV_WIDTH, panelwidth + 2,
			XV_X, panelwidth - 3,
			NULL);
		PANEL_EACH_ITEM(host_window.basic.panel, item) {
			item_type = (Panel_item_type)xv_get(item,
			    PANEL_ITEM_CLASS);
			if (item_type == PANEL_TEXT_ITEM)
				resize_text_item(host_window.basic.panel, item);
		} PANEL_END_EACH;
		PANEL_EACH_ITEM(host_window.advanced.panel, item) {
			item_type = (Panel_item_type)xv_get(item,
			    PANEL_ITEM_CLASS);
			if (item_type == PANEL_TEXT_ITEM)
				resize_text_item(host_window.advanced.panel,
				    item);
		} PANEL_END_EACH;

		y = xv_get(host_window.basic.panel, XV_HEIGHT);
		y -= 25;

		height_diff = y - xv_get(host_window.basic.plus, XV_Y);

		xv_set(host_window.basic.plus,
			XV_X, panelwidth - xv_get(host_window.basic.plus,
			    XV_WIDTH) - 5,
			XV_Y, y,
			NULL);
		xv_set(host_window.advanced.minus,
			XV_Y, y,
			NULL);

		/* move items around */
		xv_set(host_window.basic.account,
			XV_Y, xv_get(host_window.basic.account, XV_Y) +
			    height_diff,
			NULL);

		xv_set(host_window.basic.password,
			XV_Y, xv_get(host_window.basic.password, XV_Y) +
			    height_diff,
			NULL);

		xv_set(host_window.basic.login,
			XV_Y, xv_get(host_window.basic.login, XV_Y) +
			    height_diff,
			NULL);

		xv_set(host_window.basic.host,
			XV_Y, xv_get(host_window.basic.host, XV_Y) +
			    height_diff,
			NULL);

		rowsize = xv_get(host_window.basic.list, PANEL_LIST_ROW_HEIGHT);
		rows = (xv_get(host_window.basic.list, XV_HEIGHT)
		    - xv_get(host_window.basic.list, PANEL_ITEM_Y) +
		    height_diff - 5) / rowsize;
		xv_set(host_window.basic.list,
			PANEL_LIST_DISPLAY_ROWS, rows,
			PANEL_LIST_WIDTH, panelwidth - 130,
			PANEL_PAINT, PANEL_NONE,
			NULL);
		panel_paint(host_window.basic.list, PANEL_CLEAR);

		connect_rect = (Rect *)xv_get(host_window.basic.connect,
			XV_RECT);
		if (!openlook_mode) {
			dismiss_rect = (Rect *)xv_get(host_window.basic.dismiss,
				XV_RECT);
			space = xv_col(host_window.basic.panel, 1);
			pos = (panelwidth-
				(connect_rect->r_width + space +
				dismiss_rect->r_width)) / 2;
			xv_set(host_window.basic.connect,
				XV_X, pos,
				XV_Y, y,
				NULL);
			xv_set(host_window.basic.dismiss,
				XV_X, pos + connect_rect->r_width + space,
				XV_Y, y,
				NULL);
		} else {
			xv_set(host_window.basic.connect,
				XV_X, panelwidth/2  - connect_rect->r_width / 2,
				XV_Y, y,
				NULL);
			xv_set(host_window.basic.dismiss,
				XV_Y, y,
				NULL);
		}
		break;
	default:
		break;
	}
}

int	fired;

#ifdef USE_PROTOTYPES
void send_noop_command(void)
#else
void send_noop_command()
#endif
{
	if (!keepalive)
		return;
	(void) command("NOOP");
	fired++;
	footer_message("Kept connection alive, time = %d", fired);
}

/* 10 minutes */
static struct itimerval keepalive_itimer = {
	{600, 0, },
	{600, 0, }
};

#ifdef USE_PROTOTYPES
void idle_timer_on(void)
#else
void idle_timer_on()
#endif
{
	fired = 0;
	notify_set_itimer_func(base_window.frame,
	    (Notify_func)send_noop_command, ITIMER_REAL, &keepalive_itimer,
	    (struct itimerval *)NULL);
}

#ifdef USE_PROTOTYPES
void idle_timer_off(void)
#else
void idle_timer_off()
#endif
{
	notify_set_itimer_func(base_window.frame, NOTIFY_FUNC_NULL, ITIMER_REAL,
		&keepalive_itimer, (struct itimerval *)NULL);
}
