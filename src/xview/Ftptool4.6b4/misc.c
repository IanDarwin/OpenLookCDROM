#include "ftptool.h"

#pragma ident   "@(#)misc.c 1.5     93/05/27"

#ifdef USE_PROTOTYPES
void log_char(char ch)
#else
void log_char(ch)
char	ch;
#endif
{
	if (logging) {
		if (textsw_insert(session_window.log, &ch, 1) != 1) {
			logging = 0;
			xv_set(tool_property_window.ftptool.options,
				PANEL_VALUE, logging | keepalive,
				NULL);
		}
	}
}

#ifdef USE_PROTOTYPES
void log_message(char *s)
#else
void log_message(s)
char	*s;
#endif
{
	if (logging) {
		if (textsw_insert(session_window.log, s,
		    strlen(s)) != strlen(s)) {
			logging = 0;
			xv_set(tool_property_window.ftptool.options,
				PANEL_VALUE, logging | keepalive,
				NULL);
		}
	}
}

#ifdef USE_PROTOTYPES
char	*ftp_error(char ch, char *def)
#else
char	*ftp_error(ch, def)
char	ch;
char	*def;
#endif
{
	char	*str;
	char	*nl;

	if ((str = index(response_line, ch)) != NULL) {
		if ((nl = index(str, '\n')) != NULL)
			*nl = '\0';
		return (str+1);
	}
	return (def);
}

#ifdef USE_PROTOTYPES
void footer_message(char *format, ...)
#else
/*VARARGS0*/
void footer_message(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char	*format;
#endif
	va_list	argptr;
	static char	scratch[MAXPATHLEN + 1];

#ifdef USE_PROTOTYPES
	va_start(argptr, format);
#else
	va_start(argptr);
	format = (char *)va_arg(argptr, char *);
#endif
	vsprintf(scratch, format, argptr);
	xv_set(base_window.frame,
		FRAME_LEFT_FOOTER, scratch,
		NULL);
	va_end(argptr);
}

#ifdef USE_PROTOTYPES
void local_footer_message(char *format, ...)
#else
/*VARARGS0*/
void local_footer_message(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char	*format;
#endif
	va_list	argptr;
	static char	scratch[MAXPATHLEN + 1];

#ifdef USE_PROTOTYPES
	va_start(argptr, format);
#else
	va_start(argptr);
	format = (char *)va_arg(argptr, char *);
#endif
	vsprintf(scratch, format, argptr);
	xv_set(local_window.frame,
		FRAME_LEFT_FOOTER, scratch,
		NULL);
	va_end(argptr);
}

#ifdef USE_PROTOTYPES
void right_footer_message(char *format, ...)
#else
/*VARARGS0*/
void right_footer_message(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char	*format;
#endif
	va_list	argptr;
	static char	scratch[MAXPATHLEN + 1];

#ifdef USE_PROTOTYPES
	va_start(argptr, format);
#else
	va_start(argptr);
	format = (char *)va_arg(argptr, char *);
#endif
	vsprintf(scratch, format, argptr);
	xv_set(base_window.frame,
		FRAME_RIGHT_FOOTER, scratch,
		NULL);
	va_end(argptr);
}

#ifdef USE_PROTOTYPES
void local_right_footer_message(char *format, ...)
#else
/*VARARGS0*/
void local_right_footer_message(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char	*format;
#endif
	va_list	argptr;
	static char	scratch[MAXPATHLEN + 1];

#ifdef USE_PROTOTYPES
	va_start(argptr, format);
#else
	va_start(argptr);
	format = (char *)va_arg(argptr, char *);
#endif
	vsprintf(scratch, format, argptr);
	xv_set(local_window.frame,
		FRAME_RIGHT_FOOTER, scratch,
		NULL);
	va_end(argptr);
}

#ifdef USE_PROTOTYPES
void schedule_footer_message(char *format, ...)
#else
/*VARARGS0*/
void schedule_footer_message(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char	*format;
#endif
	va_list	argptr;
	static char	scratch[MAXPATHLEN + 1];

#ifdef USE_PROTOTYPES
	va_start(argptr, format);
#else
	va_start(argptr);
	format = (char *)va_arg(argptr, char *);
#endif
	vsprintf(scratch, format, argptr);
	xv_set(schedule_window.frame,
		FRAME_LEFT_FOOTER, scratch,
		NULL);
	va_end(argptr);
}

#ifdef USE_PROTOTYPES
void status_footer_message(char *format, ...)
#else
/*VARARGS0*/
void status_footer_message(va_alist)
va_dcl
#endif
{
#ifndef USE_PROTOTYPES
	char	*format;
#endif
	va_list	argptr;
	static char	scratch[MAXPATHLEN + 1];

#ifdef USE_PROTOTYPES
	va_start(argptr, format);
#else
	va_start(argptr);
	format = (char *)va_arg(argptr, char *);
#endif
	vsprintf(scratch, format, argptr);
	xv_set(status_window.frame,
		FRAME_LEFT_FOOTER, scratch,
		NULL);
	va_end(argptr);
}

#ifdef USE_PROTOTYPES
void timeout_disconnect(void)
#else
void timeout_disconnect()
#endif
{
	footer_message("Disconnected (timeout).");
	disconnect();
}

#ifdef USE_PROTOTYPES
void close_files(void)
#else
void close_files()
#endif
{
	if (commandfp) {
		fflush(commandfp);
		(void) fclose(commandfp);
	}
	commandfp = NULL;
	if (responsefp) {
		fflush(responsefp);
		(void) fclose(responsefp);
	}
	responsefp = NULL;
}

#ifdef USE_PROTOTYPES
void quit_ftp(void)
#else
void quit_ftp()
#endif
{
	connected = 0;

	if (!timedout && commandfp)
		(void) command("QUIT");
	close_files();
	data = -1;

	/* remove /usr/tmp files */
	system("rm -rf /usr/tmp/ftptl*");
}

#ifdef USE_PROTOTYPES
void disconnect(void)
#else
void disconnect()
#endif
{
	if (timedout > 1)
		fprintf(stderr, "multiple timeouts?\n");

	quit_ftp();

	xv_set(schedule_window.process,
		PANEL_INACTIVE, FALSE,
		NULL);

	xv_set(frame_icon,
		ICON_LABEL, "",
		NULL);
	xv_set(base_window.frame,
		XV_LABEL, header_name,
		NULL);

	non_unix = 0;

	response_line[0] = '\0';

	xv_set(tool_property_window.directory_lists.remote_sort,
		PANEL_INACTIVE, FALSE,
		NULL);


	/* inactivate buttons */
	end_busy_cycle();
	cursor_normal();
	idle_timer_off();

	remote_list_nfiles = 0;
	remote_list_ndirs = 0;
	remote_list_nothers = 0;

	if (other_dir_pattern) {
		free(other_dir_pattern);
		other_dir_pattern = NULL;
	}

	/*
	free_batchlist(receive_window.list, 0);
	 */
	nreceiveitems = 0;

	change_remote_list_menu();

	clear_slist(base_window.list);
	free_dircache(&remote_dircache);
	right_footer_message("");

	/* inactivate buttons */
	xv_set(host_window.basic.connect,
		PANEL_INACTIVE, FALSE,
		PANEL_LABEL_STRING, " Connect  ",
		XV_HELP_DATA, "ftptool:ConnectButton",
		NULL);

	xv_set(base_window.connect,
		PANEL_INACTIVE, FALSE,
		PANEL_LABEL_STRING, " Connect... ",
		XV_HELP_DATA, "ftptool:ConnectButton",
		NULL);

	xv_set(base_window.directory,
		PANEL_INACTIVE, TRUE,
		PANEL_VALUE, "",
		NULL);

	xv_set(base_window.file,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(base_window.view,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(local_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(local_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);

	xv_set(tool_property_window.apply,
		PANEL_INACTIVE, FALSE,
		NULL);

	xv_set(base_window.abort,
		PANEL_INACTIVE, TRUE,
		NULL);


	xv_set(base_window.list,
		XV_SHOW, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void xfer_buttons_inactive(void)
#else
void xfer_buttons_inactive()
#endif
{
	footer_message("");
	local_footer_message("");
	idle_timer_off();
	start_busy_cycle();
	cursor_busy();
	xv_set(base_window.file,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(base_window.view,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(host_window.basic.connect,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(base_window.connect,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(base_window.directory,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(base_window.list,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(local_window.directory,
		PANEL_READ_ONLY, TRUE,
		NULL);
	xv_set(local_window.list,
		PANEL_INACTIVE, TRUE,
		NULL);

	xv_set(tool_property_window.apply,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(tool_property_window.category,
		PANEL_INACTIVE, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void xfer_buttons_active(void)
#else
void xfer_buttons_active()
#endif
{
	if (abort_transfer)
		xv_set(base_window.frame,
			FRAME_BUSY, FALSE,
			NULL);
	cursor_normal();
	end_busy_cycle();
	idle_timer_on();

	xv_set(base_window.file,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(base_window.view,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(host_window.basic.connect,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(base_window.connect,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(base_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(base_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(local_window.directory,
		PANEL_READ_ONLY, FALSE,
		NULL);
	xv_set(local_window.list,
		PANEL_INACTIVE, FALSE,
		NULL);

	xv_set(tool_property_window.apply,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(tool_property_window.category,
		PANEL_INACTIVE, FALSE,
		NULL);

	xv_set(base_window.abort,
		PANEL_INACTIVE, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
void cursor_busy(void)
#else
void cursor_busy()
#endif
{
	Xv_Window	window;

	window = xv_get(base_window.panel, CANVAS_NTH_PAINT_WINDOW, 0);
	xv_set(window,
		WIN_CURSOR, busy_cursor,
		NULL);
	window = xv_get(local_window.panel, CANVAS_NTH_PAINT_WINDOW, 0);
	xv_set(window,
		WIN_CURSOR, busy_cursor,
		NULL);
	window = xv_get(tool_property_window.panel, CANVAS_NTH_PAINT_WINDOW, 0);
	xv_set(window,
		WIN_CURSOR, busy_cursor,
		NULL);
}

#ifdef USE_PROTOTYPES
void cursor_normal(void)
#else
void cursor_normal()
#endif
{
	Xv_Window	window;

	window = xv_get(base_window.panel, CANVAS_NTH_PAINT_WINDOW, 0);
	xv_set(window,
		WIN_CURSOR, normal_cursor,
		NULL);
	window = xv_get(local_window.panel, CANVAS_NTH_PAINT_WINDOW, 0);
	xv_set(window,
		WIN_CURSOR, normal_cursor,
		NULL);
	window = xv_get(tool_property_window.panel, CANVAS_NTH_PAINT_WINDOW, 0);
	xv_set(window,
		WIN_CURSOR, normal_cursor,
		NULL);
}

#ifdef USE_PROTOTYPES
void show_stats(struct file_property_window *file_props,
	struct dirlist *tmp)
#else
void show_stats(file_props, tmp)
struct file_property_window *file_props;
struct dirlist *tmp;
#endif
{
	int		user_perms, group_perms, other_perms;
	char	*type;

	switch (tmp->mode & S_IFMT) {
		case S_IFREG:
			type = "Regular File";
			break;
		case S_IFDIR:
			type = "Directory";
			break;
		case S_IFLNK:
			type = "Symbolic Link";
			break;
		case S_IFCHR:
			type = "Character Device";
			break;
		case S_IFBLK:
			type = "Block Device";
			break;
		case S_IFSOCK:
			type = "Socket";
			break;
		case S_IFIFO:
			type = "Named Pipe";
			break;
		default:
			type = "unknown";
			break;
	}

	user_perms = 0;
	if (tmp->mode & S_IRUSR)
		user_perms |= 1;
	if (tmp->mode & S_IWUSR)
		user_perms |= 2;
	if (tmp->mode & S_IXUSR)
		user_perms |= 4;

	group_perms = 0;
	if (tmp->mode & S_IRGRP)
		group_perms |= 1;
	if (tmp->mode & S_IWGRP)
		group_perms |= 2;
	if (tmp->mode & S_IXGRP)
		group_perms |= 4;

	other_perms = 0;
	if (tmp->mode & S_IROTH)
		other_perms |= 1;
	if (tmp->mode & S_IWOTH)
		other_perms |= 2;
	if (tmp->mode & S_IXOTH)
		other_perms |= 4;

	/* show stats in window */
	xv_set(file_props->filename,
		PANEL_VALUE, tmp->name,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->owner,
		PANEL_LABEL_STRING, tmp->owner,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->group,
		PANEL_LABEL_STRING, tmp->group,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->modtime,
		PANEL_LABEL_STRING, tmp->date,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->type,
		PANEL_LABEL_STRING, type,
		PANEL_INACTIVE, FALSE,
		NULL);
	sprintf(scratch, "%d", tmp->size);
	xv_set(file_props->size,
		PANEL_LABEL_STRING, scratch,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->user_perms,
		PANEL_VALUE, user_perms,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->group_perms,
		PANEL_VALUE, group_perms,
		PANEL_INACTIVE, FALSE,
		NULL);
	xv_set(file_props->other_perms,
		PANEL_VALUE, other_perms,
		PANEL_INACTIVE, FALSE,
		NULL);
}

#ifdef USE_PROTOTYPES
void inactivate_props(struct file_property_window *file_props)
#else
void inactivate_props(file_props)
struct file_property_window *file_props;
#endif
{
	xv_set(file_props->filename,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->owner,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->group,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->modtime,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->type,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->size,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->user_perms,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->group_perms,
		PANEL_INACTIVE, TRUE,
		NULL);
	xv_set(file_props->other_perms,
		PANEL_INACTIVE, TRUE,
		NULL);
}

#ifdef USE_PROTOTYPES
int ask_make_dir(char *s)
#else
int ask_make_dir(s)
char	*s;
#endif
{
	int		answer;
#ifdef XVIEW3
	Xv_notice notice;
#endif

	sprintf(scratch, "%s does not exist.", s);
#ifdef XVIEW3
	notice = xv_create(local_window.panel, NOTICE,
		NOTICE_MESSAGE_STRINGS,
			scratch,
			"You can:",
			NULL,
		NOTICE_BUTTON,	"Cancel", 4,
		NOTICE_BUTTON,	"Try to create it", 3,
		NOTICE_STATUS, &answer,
		XV_SHOW, TRUE,
		NULL);
	xv_destroy_safe(notice);
#else
	answer = notice_prompt(local_window.panel, NULL,
		NOTICE_MESSAGE_STRINGS,
			scratch,
			"You can:",
			NULL,
		NOTICE_BUTTON,	"Cancel", 4,
		NOTICE_BUTTON,	"Try to create it", 3,
		NULL);
#endif
	if (answer == 4)
		return (-1);

	return (make_dirs(s, 1));
}

#ifdef USE_PROTOTYPES
int make_dirs(char *s, int make_last)
#else
int make_dirs(s, make_last)
char	*s;
int		make_last;
#endif
{
	int	rval = 0;
	char	*slash;
	char	*lastslash;
	char	ch;
	char	*dir;

	dir = expand_dirname(s);
	if (dir == NULL)
		return (ENOMEM);
	slash = dir;
	for (;;) {
		lastslash = slash;
		slash = index(lastslash + 1, '/');
		if (slash) {
			ch = *slash;
			*slash = '\0';
			if (mkdir(dir, 0777) == -1) {
				rval = errno;
				if (rval != EEXIST)
					goto out;
				rval = 0;
			}
			*slash = ch;
		} else {
			if (make_last && mkdir(dir, 0777) == -1) {
				if (errno == EEXIST)
					rval = 0;
				else
					rval = errno;
			}
			goto out;
		}
	}
out:
	free(dir);
	return (rval);
}

#ifdef USE_PROTOTYPES
int ask_make_remote_dir(char *s)
#else
int ask_make_remote_dir(s)
char	*s;
#endif
{
	int		answer;
#ifdef XVIEW3
	Xv_notice notice;
#endif

	sprintf(scratch, "%s does not exist.", s);
#ifdef XVIEW3
	notice = xv_create(base_window.panel, NOTICE,
		NOTICE_MESSAGE_STRINGS,
			scratch,
			"You can:",
			NULL,
		NOTICE_BUTTON,	"Cancel", 4,
		NOTICE_BUTTON,	"Try to create it", 3,
		NOTICE_STATUS, &answer,
		XV_SHOW, TRUE,
		NULL);
	xv_destroy_safe(notice);
#else
	answer = notice_prompt(base_window.panel, NULL,
		NOTICE_MESSAGE_STRINGS,
			scratch,
			"You can:",
			NULL,
		NOTICE_BUTTON,	"Cancel", 4,
		NOTICE_BUTTON,	"Try to create it", 3,
		NULL);
#endif
	if (answer == 4)
		return (-1);

	return (make_remote_dirs(s, 1));
}

#ifdef USE_PROTOTYPES
int make_remote_dirs(char *s, int make_last)
#else
int make_remote_dirs(s, make_last)
char	*s;
int		make_last;
#endif
{
	int	rval = 0;
	char	*slash;
	char	*lastslash;
	char	*colon;
	char	ch;
	char	*dir;
	char	*ftperr;

	dir = strdup(s);
	if (dir == NULL)
		return (ENOMEM);
	slash = dir;
	for (;;) {
		lastslash = slash;
		slash = index(lastslash + 1, '/');
		if (slash) {
			ch = *slash;
			*slash = '\0';

			if (command("MKD %s", dir) == ERROR) {
				/* 550 file: File exists */
				colon = index(response_line, ':');
				if (!colon || strncmp(colon + 1, " File", 5)) {
					/* permission denied or file exists */
					sprintf(scratch,
					    "Remote mkdir of %s failed.", dir);
					ftperr = ftp_error(' ', scratch);
					footer_message(ftperr);
					return (1);
				}
			}

			*slash = ch;
		} else {
			if (make_last) {
				if (command("MKD %s", dir) == ERROR) {
					colon = index(response_line, ':');
					if (!colon ||
					    strncmp(colon + 1, " File", 5)) {
						/* permission denied */
						/* file exists */
						sprintf(scratch,
						    "Remote mkdir of %s failed.",
						    dir);
						ftperr = ftp_error(' ',
						    scratch);
						footer_message(ftperr);
						return (1);
					}
				}

			}
			goto out;
		}
	}
out:
	free(dir);
	return (rval);
}

/* geometry = widthxheight+x+y */
#ifdef USE_PROTOTYPES
void set_geometry(char *s, Frame frame, int def_width, int def_height,
	int def_x, int def_y)
#else
void set_geometry(s, frame, def_width, def_height, def_x, def_y)
char	*s;
Frame	frame;
int		def_width;
int		def_height;
int		def_x;
int		def_y;
#endif
{
	int		width;
	int		height;
	int		x;
	int		y;
	Rect	rect;

	if (strlen(s) == 0 || sscanf(s, "%dx%d+%d+%d", &width,
	    &height, &x, &y) != 4) {
		width = def_width;
		height = def_height;
		x = def_x;
		y = def_y;
	}
	if (x < 0 || x >= display_width)
		x = 0;
	if (y < 0 || y >= display_height)
		y = 0;
	rect.r_left = x;
	rect.r_top = y;
	if (width < def_width)
		rect.r_width = def_width;
	else
		rect.r_width = width;
	if (height < def_height)
		rect.r_height = def_height;
	else
		rect.r_height = height;
	frame_set_rect(frame, &rect);
}

#ifdef USE_PROTOTYPES
void save_geometry(char *s, Frame frame)
#else
void save_geometry(s, frame)
char	*s;
Frame	frame;
#endif
{
	Rect	rect;

	frame_get_rect(frame, &rect);
	sprintf(s, "%dx%d+%d+%d", rect.r_width, rect.r_height,
		rect.r_left, rect.r_top);
}

#ifdef USE_PROTOTYPES
void justify_items(Panel panel, int resize)
#else
void justify_items(panel, resize)
Panel	panel;
int	resize;		/* TRUE to resize text fields */
#endif
{
	register Panel_item	item;
	register int		value_x;
	register Panel_item_type class;
	Xv_Font			font = XV_NULL;
	Font_string_dims	font_size;
	int			longest = 0;
	char			*string;

	if (panel == XV_NULL)
		return;

	/*
	 * Compute the longest label excluding all panel buttons
	 */
	PANEL_EACH_ITEM(panel, item) {
		if ((int)xv_get(item, PANEL_SHOW_ITEM) && ((Panel_item_type)
		    xv_get(item, PANEL_ITEM_CLASS) != PANEL_BUTTON_ITEM)) {
			font = (Xv_Font)xv_get(item, PANEL_LABEL_FONT);
			string = (char *)xv_get(item, PANEL_LABEL_STRING);
			xv_get(font, FONT_STRING_DIMS, string, &font_size);
			if (font_size.width > longest)  {
				longest = font_size.width;
			}
		}
	} PANEL_END_EACH;

	value_x = longest + 2 * (int)xv_get(panel, PANEL_ITEM_X_GAP);

	/* Layout each item (except buttons) on the panel */
	PANEL_EACH_ITEM(panel, item) {
		if ((int)xv_get(item, PANEL_SHOW_ITEM) &&
		    ((class = (Panel_item_type)xv_get(item, PANEL_ITEM_CLASS))
		    != PANEL_BUTTON_ITEM)) {
			xv_set(item,
				PANEL_VALUE_X, value_x,
				NULL);
			if (resize && class == PANEL_TEXT_ITEM) {
				resize_text_item(panel, item);
			}
		}
	}
	PANEL_END_EACH;

	return;
}

#ifdef USE_PROTOTYPES
void resize_text_item(Panel panel, Panel_item text_item)
#else
void resize_text_item(panel, text_item)
Panel		panel;
Panel_item	text_item;
#endif
{
	Xv_Font	font;
	int	width;
	int	n;

	if (panel == XV_NULL || text_item == XV_NULL)
		return;

	/*
	 * Set the display width of the fillin field to extend to the
	 * right edge of the panel.
	 */
	width = (int)xv_get(panel, XV_WIDTH) -
		(int)xv_get(text_item, PANEL_VALUE_X) -
		(int)xv_get(panel, PANEL_ITEM_X_GAP);

	font = (Xv_Font)xv_get(panel, XV_FONT);
	n = width / (int)xv_get(font, FONT_DEFAULT_CHAR_WIDTH);

	/*
	 * Make sure it gets no smaller than 5 characters and no larger
	 * than the stored length.
	 */
	if (n < 5)
		n = 5;
	else if (n > (int)xv_get(text_item, PANEL_VALUE_STORED_LENGTH))
		n = (int)xv_get(text_item, PANEL_VALUE_STORED_LENGTH);

	xv_set(text_item,
		PANEL_VALUE_DISPLAY_LENGTH, n,
		NULL);

	return;
}

#ifdef USE_PROTOTYPES
char *linkval(char *string)
#else
char *linkval(string)
char	*string;
#endif
{
	char	*tmp;

	/* string is of the form */
	/* name -> value */
	/* be somewhat sure we find the ->, not just one or the other, */
	/* since those _are_ legal filename characters */
	tmp = string;

	while ((tmp = index(tmp, '-')) != NULL) {
		if (tmp[1] == '>' && tmp[2] == ' ')
			return (strdup(&tmp[3]));
		tmp++; /* skip '-', since we didn't find -> */
	}
	fprintf(stderr, "linkval: malformed link entry\n");
	return (NULL);
}

#ifdef USE_PROTOTYPES
char *linkname(char *string)
#else
char *linkname(string)
char	*string;
#endif
{
	char	*str, *tmp;

	/* string is of the form */
	/* name -> value */
	/* be somewhat sure we find the ->, not just one or the other, */
	/* since those _are_ legal filename characters */
	str = strdup(string);
	if (str == NULL) {
		fprintf(stderr, "linkname: Out of memory.\n");
		return (NULL);
	}
	tmp = str;

	while ((tmp = index(tmp, '-')) != NULL) {
		if (tmp[1] == '>' && tmp[2] == ' ' &&
		    tmp > str && tmp[-1] == ' ') {
			tmp[-1] = '\0';
			return (str);
		}
		tmp++; /* skip '-', since we didn't find -> */
	}
	/*
	fprintf(stderr, "linkval: malformed link entry\n");
	free(str);
	*/
	return (str);
}

#ifdef USE_PROTOTYPES
void add_dismiss(Panel panel, Panel_item first, Panel_item dismiss)
#else
void add_dismiss(panel, first, dismiss)
Panel	panel;
Panel_item	first;
Panel_item	dismiss;
#endif
{
	Rect	*first_rect;
	Rect	*dismiss_rect;
	int	width, space, pos;

	width = xv_get(panel, XV_WIDTH);
	first_rect = (Rect *)xv_get(first, XV_RECT);
	if (openlook_mode) {
		xv_set(dismiss,
			XV_SHOW, FALSE,
			NULL);
		xv_set(first,
			XV_X, width/2 - first_rect->r_width/2,
			XV_SHOW, TRUE,
			NULL);
	} else {
		dismiss_rect = (Rect *)xv_get(dismiss, XV_RECT);
		space = xv_col(panel, 1);
		pos = (width-(first_rect->r_width + space +
		    dismiss_rect->r_width))/2;
		xv_set(first,
			XV_X, pos,
			XV_SHOW, TRUE,
			NULL);
		xv_set(dismiss,
			XV_X, pos + first_rect->r_width + space,
			XV_SHOW, TRUE,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
void update_date(int doscheddefault)
#else
void update_date(doscheddefault)
int	doscheddefault;
#endif
{
	time_t	t;
	struct tm *tm;
	static char date[30];

	t = time((time_t *)NULL);
	tm = localtime(&t);
	current_year = tm->tm_year;
	current_month = tm->tm_mon;
	is_dst = tm->tm_isdst;
	strftime(date, sizeof (date), "%R %b %e %Y", tm);
	xv_set(schedule_window.current_time,
		PANEL_LABEL_STRING, date,
		NULL);
	if (doscheddefault) {
		xv_set(schedule_window.hour,
			PANEL_VALUE, tm->tm_hour,
			NULL);
		xv_set(schedule_window.minute,
			PANEL_VALUE, tm->tm_min,
			NULL);
		xv_set(schedule_window.month,
			PANEL_VALUE, tm->tm_mon,
			NULL);
		xv_set(schedule_window.day,
			PANEL_VALUE, tm->tm_mday,
			NULL);
		xv_set(schedule_window.year,
			PANEL_VALUE, tm->tm_year + 1900,
			NULL);
	}
}

#ifdef USE_PROTOTYPES
Notify_value date_wrapper(void)
#else
Notify_value date_wrapper()
#endif
{
	update_date(0);
	return (NOTIFY_DONE);
}

#ifdef USE_PROTOTYPES
void local_show_items(void)
#else
void local_show_items()
#endif
{
	int	nitems =  (int)xv_get(local_window.list, PANEL_LIST_NROWS);

	if (nitems == 1) {
		local_right_footer_message("1 item, %d selected.",
		    local_list_nfiles + local_list_ndirs + local_list_nothers);
	} else {
		local_right_footer_message("%d items, %d selected.", nitems,
		    local_list_nfiles + local_list_ndirs + local_list_nothers);
	}
}

#ifdef USE_PROTOTYPES
void remote_show_items(void)
#else
void remote_show_items()
#endif
{
	int	nitems = (int)xv_get(base_window.list, PANEL_LIST_NROWS);

	if (nitems == 1) {
		right_footer_message("1 item, %d selected.",
		    remote_list_nfiles + remote_list_ndirs +
			remote_list_nothers);
	} else {
		right_footer_message("%d items, %d selected.", nitems,
		    remote_list_nfiles + remote_list_ndirs +
			remote_list_nothers);
	}
}

#ifdef USE_PROTOTYPES
int ping_server(void)
#else
int ping_server()
#endif
{
	(void) command("NOOP");
	if (code == 421) {
		timedout++;
		return (ETIMEDOUT);
	}
	return (0);
}

#ifdef NEED_STRCASECMP

/* A quick and dirty version of strcasecmp(), which seems to work */

#ifdef USE_PROTOTYPES
int strcasecmp(const char *s1, const char *s2)
#else
int strcasecmp(s1, s2)
char *s1, *s2;
#endif
{
	char	c1, c2;

	while (*s1 && *s2) {
		if (isupper(*s1))
			c1 = tolower(*s1);
		else
			c1 = *s1;
		if (isupper(*s2))
			c2 = tolower(*s2);
		else
			c2 = *s2;
		if (c1 == c2) {
			s1++;
			s2++;
			continue;
		} else if (c1 > c2)
			return (1);
		else
			return (-1);
	}
	if (*s1 == '\0' && *s2 == '\0')
		return (0);

	if (*s1 == '\0')
		return (-1);

	return (1);

}

#endif

#ifdef USE_PROTOTYPES
void caret_to_first(Panel_item item)
#else
void caret_to_first(item)
Panel_item	item;
#endif
{
#ifdef notdef /* ACTION_LINE_START */
	void	(*default_event)();
	Event	event;
	char	*val;

	val = (char *)xv_get(item, PANEL_VALUE);
	if (val == NULL || *val == '\0')
		return;
	default_event = (void (*)())xv_get(item, PANEL_EVENT_PROC);
	event_set_action(&event, ACTION_LINE_START);
	(*default_event)(item, &event);
#endif
}

#ifdef USE_PROTOTYPES
void fix_carets(void)
#else
void fix_carets()
#endif
{
	/* Fix host */
	caret_to_first(host_window.basic.host);

	/* Fix login */
	caret_to_first(host_window.basic.login);

	/* Fix password */
	caret_to_first(host_window.basic.password);

	/* Fix password */
	caret_to_first(host_window.basic.account);

	/* Fix alias */
	caret_to_first(host_window.advanced.alias);

	/* Fix comment */
	caret_to_first(host_window.advanced.comment);

	/* Fix proxy */
	caret_to_first(host_window.advanced.proxy);

	/* Fix remote_auto_cd */
	caret_to_first(host_window.advanced.remote_auto_cd);

	/* Fix local_auto_cd */
	caret_to_first(host_window.advanced.local_auto_cd);

	/* Fix dir_parse */
	caret_to_first(host_window.advanced.dir_parse);
}
